-module(ums_state_redis).

-include_lib("ums/include/ums.hrl").
-behaviour(ums_state).

-export([edges_subscribed/1]).
-export([init/0]).
-export([remove_subscriptions_by_session_id/1]).
-export([subscribe_edge/3]).
-export([subscriptions_for_session_id/1]).
-export([unsubscribe_edge/3]).

init() ->
    ok.

edges_subscribed(Resources)
  when is_list(Resources) ->
    ok.

remove_subscriptions_by_session_id(_SessionId) ->
    ok.

subscribe_edge(_SessionId, _Edge, Resources)
  when is_list(Resources) ->
    ok.

unsubscribe_edge(_SessionId, _Edge, _Resource) ->
    ok.

subscriptions_for_session_id(_SessionId) ->
    ok.
