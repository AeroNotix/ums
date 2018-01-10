-module(ums_session_manager).

-export([init/0]).
-export([schedule_resource_cleanup/1]).
-export([store_session_cleanup/2]).
-export([remove_session_cleanup/1]).
-export([notify_session_reestablished/1]).

-include_lib("ums/include/ums.hrl").

init() ->
    install_ets().

schedule_resource_cleanup(SessionId) ->
    lager:debug("Scheduling cleanup for: ~p", [SessionId]),
    spawn(fun() ->
                  try
                      ?state_backend:store_session_cleanup(SessionId, self()),
                      receive
                          {session_reestablished, SessionId} ->
                              lager:debug("Session re-established: ~p", [SessionId]),
                              ok
                      after application:get_env(ums, subscription_timeout, 5000) ->
                              ?state_backend:remove_subscriptions_by_session_id(SessionId)
                      end
                  after
                      ?state_backend:remove_session_cleanup(SessionId)
                  end
          end).

install_ets() ->
    ets:new(session_timeouts, [named_table, public]),
    ok.

store_session_cleanup(Sid, Pid) ->
    ets:insert(session_timeouts, {Sid, Pid}).

remove_session_cleanup(Sid) ->
    ets:delete(session_timeouts, Sid).

find_session_cleanup(Sid) ->
    case ets:lookup(session_timeouts, Sid) of
        [{Sid, Pid}] ->
            {ok, Pid};
        [] ->
            {error, not_found}
    end.

notify_session_reestablished(Sid) ->
    case find_session_cleanup(Sid) of
        {ok, Pid} ->
            Pid ! {session_reestablished, Sid},
            {ok, ?state_backend:subscriptions_for_session_id(Sid)};
        {error, not_found} ->
            {ok, []}
    end.
