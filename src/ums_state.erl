-module(ums_state).

-export([edges_subscribed/1]).
-export([subscribe_edge/2]).
-export([unsubscribe_edge/2]).
-export([install_mnesia/1]).
-export([schedule_resource_cleanup/2]).
-export([store_session_cleanup/2]).
-export([remove_session_cleanup/1]).
-export([find_session_cleanup/1]).
-export([sync/2]).
-export([install_ets/0]).


-record(umss_v1, {
          resource = error(must_supply_resource) :: string(),
          edge_server = error(must_supply_name) :: binary()
         }).


edges_subscribed(Resource)
  when is_binary(Resource) ->
    mnesia:dirty_read(ums_state, Resource);
edges_subscribed(Resources)
  when is_list(Resources) ->
    MatchHead = #umss_v1{resource='$1', edge_server = '$2'},
    MatchGuards = [{'==', '$1', Resource} || Resource <- Resources],
    Result = '$2',
    F = fun() ->
                mnesia:select(ums_state, [{MatchHead, MatchGuards, [Result]}])
        end,
    mnesia:transaction(F).

subscribe_edge(Edge, Resources)
  when is_list(Resources) ->
    [ok = subscribe_edge(Edge, Resource) || Resource <- Resources],
    ok;
subscribe_edge(Edge, Resource) ->
    Subscription = #umss_v1{resource=Resource, edge_server=Edge},
    mnesia:dirty_write(ums_state, Subscription).

unsubscribe_edge(Edge, Resource) ->
    Subscription = #umss_v1{resource=Resource, edge_server=Edge},
    mnesia:dirty_delete_object(ums_state, Subscription).

sync(_Edge, _Resources) ->
    ok.

schedule_resource_cleanup(Sid, _Resources) ->
    spawn(fun() ->
                  try
                      store_session_cleanup(Sid, self()),
                      receive
                          {session_reestablished, Sid} ->
                              ok
                      after application:get_env(ums, subscription_timeout, 5000) ->
                              %% TODO: Cleanup all resources here
                              ok
                      end
                  after
                      remove_session_cleanup(Sid)
                  end
          end).

install_ets() ->
    ets:new(session_timeouts, [named_table, public]).

store_session_cleanup(Sid, Pid) ->
    ets:insert(session_timeouts, {Pid, Sid}).

remove_session_cleanup(Pid) ->
    ets:delete(session_timeouts, Pid).

find_session_cleanup(Sid) ->
    case ets:lookup(session_timeouts, Sid) of
        [[{Sid, Pid}]] ->
            {ok, Pid};
        [] ->
            {error, not_found}
    end.

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
