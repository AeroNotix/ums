-module(ums_state).

-export([init/0]).
-export([edges_subscribed/1]).
-export([find_session_cleanup/1]).
-export([notify_session_reestablished/1]).
-export([remove_session_cleanup/1]).
-export([remove_subscriptions_by_session_id/1]).
-export([schedule_resource_cleanup/1]).
-export([store_session_cleanup/2]).
-export([subscribe_edge/3]).
-export([subscriptions_for_session_id/1]).
-export([unsubscribe_edge/3]).

-include_lib("ums/include/ums.hrl").

-define(state_backend, (application:get_env(ums, state_backend, ums_state_mnesia))).

-callback init() -> ok.
-callback edges_subscribed(Resources :: binary()) -> {ok, binary()} | {error, atom()}.
-callback subscriptions_for_session_id(SessionId :: binary()) -> {ok, binary()} | {error, atom()}.
-callback subscribe_edge(SessionId :: binary(), Edge :: binary(), Resource :: binary()) -> ok | {error, atom()}.
-callback unsubscribe_edge(SessionId :: binary(), Edge :: binary(), Resource :: binary()) -> ok | {error, atom()}.
-callback remove_subscriptions_by_session_id(SessionId :: binary()) -> ok | {error, atom()}.

init() ->
    install_ets(),
    ?state_backend:init().

edges_subscribed(Resources) ->
    ?state_backend:edges_subscribed(Resources).

subscriptions_for_session_id(SessionId) ->
    ?state_backend:subscriptions_for_session_id(SessionId).

subscribe_edge(SessionId, Edge, Resource) ->
    ?state_backend:subscribe_edge(SessionId, Edge, Resource).

unsubscribe_edge(SessionId, Edge, Resource) ->
    ?state_backend:unsubscribe_edge(SessionId, Edge, Resource).

remove_subscriptions_by_session_id(SessionId) ->
    ?state_backend:remove_subscriptions_by_session_id(SessionId).

schedule_resource_cleanup(Sid) ->
    lager:debug("Scheduling cleanup for: ~p", [Sid]),
    spawn(fun() ->
                  try
                      ?state_backend:store_session_cleanup(Sid, self()),
                      receive
                          {session_reestablished, Sid} ->
                              lager:debug("Session re-established: ~p", [Sid]),
                              ok
                      after application:get_env(ums, subscription_timeout, 5000) ->
                              ?state_backend:remove_subscriptions_by_session_id(Sid)
                      end
                  after
                      ?state_backend:remove_session_cleanup(Sid)
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
        {error, not_found} ->
            {ok, []}
    end.

find_session_cleanup(Sid) ->
    case ets:lookup(session_timeouts, Sid) of
        [{Sid, Pid}] ->
            {ok, Pid};
        [] ->
            {error, not_found}
    end.
