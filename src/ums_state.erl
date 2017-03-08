-module(ums_state).

-export([edges_subscribed/1]).
-export([find_session_cleanup/1]).
-export([install_ets/0]).
-export([install_mnesia/1]).
-export([notify_session_reestablished/1]).
-export([remove_session_cleanup/1]).
-export([schedule_resource_cleanup/1]).
-export([store_session_cleanup/2]).
-export([subscribe_edge/3]).
-export([subscriptions_for_session_id/1]).
-export([unsubscribe_edge/3]).

-include_lib("stdlib/include/ms_transform.hrl").

-record(umss_v1, {
          session_id = error(must_supply_session_id) :: binary(),
          resource = error(must_supply_resource) :: string(),
          edge_server = error(must_supply_name) :: binary()
         }).

edges_subscribed(Resources)
  when is_list(Resources) ->
    MatchHead = #umss_v1{session_id='_', resource='$1', edge_server = '$2'},
    MatchGuards = [{'==', '$1', Resource} || Resource <- Resources],
    Result = '$2',
    F = fun() ->
                mnesia:select(ums_state, [{MatchHead, MatchGuards, [Result]}])
        end,
    mnesia:transaction(F).

subscriptions_for_session_id(SessionId) ->
    MS = ets:fun2ms(
           fun(#umss_v1{session_id=S, resource=R, edge_server=E})
                 when S == SessionId ->
                   {R, E}
           end),
    F = fun() ->
                mnesia:select(ums_state, MS)
        end,
    {atomic, Subscriptions} = mnesia:transaction(F),
    {ok, Subscriptions}.

subscribe_edge(SessionId, Edge, Resources)
  when is_list(Resources) ->
    [ok = subscribe_edge(SessionId, Edge, Resource) || Resource <- Resources],
    ok;
subscribe_edge(SessionId, Edge, Resource) ->
    Subscription = #umss_v1{session_id=SessionId, resource=Resource, edge_server=Edge},
    mnesia:dirty_write(ums_state, Subscription).

unsubscribe_edge(SessionId, Edge, Resource) ->
    Subscription = #umss_v1{session_id=SessionId, resource=Resource, edge_server=Edge},
    mnesia:dirty_delete_object(ums_state, Subscription).

sync(_Edge, _Resources) ->
    ok.

schedule_resource_cleanup(Sid) ->
    lager:debug("Scheduling cleanup for: ~p", [Sid]),
    spawn(fun() ->
                  try
                      store_session_cleanup(Sid, self()),
                      receive
                          {session_reestablished, Sid} ->
                              lager:debug("Session re-established: ~p", [Sid]),
                              ok
                      after application:get_env(ums, subscription_timeout, 5000) ->
                              remove_subscriptions_by_session_id(Sid)
                      end
                  after
                      remove_session_cleanup(Sid)
                  end
          end).

install_ets() ->
    ets:new(session_timeouts, [named_table, public]).

store_session_cleanup(Sid, Pid) ->
    ets:insert(session_timeouts, {Sid, Pid}).

remove_session_cleanup(Sid) ->
    ets:delete(session_timeouts, Sid).

notify_session_reestablished(Sid) ->
    case find_session_cleanup(Sid) of
        {ok, Pid} ->
            Pid ! {session_reestablished, Sid},
            {ok, subscriptions_for_session_id(Sid)};
        {error, not_found} = E ->
            E
    end.

find_session_cleanup(Sid) ->
    case ets:lookup(session_timeouts, Sid) of
        [{Sid, Pid}] ->
            {ok, Pid};
        [] ->
            {error, not_found}
    end.

remove_subscriptions_by_session_id(SessionId) ->
    RemoveBySessionId =
        fun() ->
                try
                    lager:debug("Removing all sessions for: ~p",
                                [{SessionId}]),
                    Res = mnesia:delete(ums_state, SessionId, write),
                    lager:debug("Sessions removed for: ~p. Result: ~p", [SessionId, Res]),
                    Res
                catch
                    E:R ->
                        lager:error("Error cleaning up resources: ~p", [{E,R}])
                end

        end,
    mnesia:transaction(RemoveBySessionId).

install_mnesia(Nodes) ->
    rpc:multicall(Nodes, application, start, [mnesia]),
    mnesia:create_schema(Nodes),
    case mnesia:create_table(ums_state,
                             [{attributes, record_info(fields, umss_v1)},
                              {record_name, umss_v1},
                              {ram_copies, Nodes},
                              {type, bag}]) of
        {atomic, ok} ->
            ok;
        {aborted, {already_exists, ums_state}} ->
            ok
    end,
    mnesia:change_config(extra_db_nodes, Nodes),
    ok.
