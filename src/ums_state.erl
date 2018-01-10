-module(ums_state).

-export([init/0]).
-export([edges_subscribed/1]).
-export([remove_subscriptions_by_session_id/1]).
-export([subscribe_edge/3]).
-export([subscriptions_for_session_id/1]).
-export([unsubscribe_edge/3]).

-include_lib("ums/include/ums.hrl").

-callback init() -> ok.
-callback edges_subscribed(Resources :: binary()) -> {ok, binary()} | {error, atom()}.
-callback subscriptions_for_session_id(SessionId :: binary()) -> {ok, binary()} | {error, atom()}.
-callback subscribe_edge(SessionId :: binary(), Edge :: binary(), Resource :: binary()) -> ok | {error, atom()}.
-callback unsubscribe_edge(SessionId :: binary(), Edge :: binary(), Resource :: binary()) -> ok | {error, atom()}.
-callback remove_subscriptions_by_session_id(SessionId :: binary()) -> ok | {error, atom()}.

init() ->
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
