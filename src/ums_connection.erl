-module(ums_connection).

-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-record(s_v1, {
          session_id :: binary(),
          subscriptions = [] :: list()
         }).

init({_, _}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, R0, _Opts) ->
    Sid = ums_server:new_session_id(),
    R1 = cowboy_req:set_resp_header(<<"x-ums-session-id">>, Sid, R0),
    {ok, R1, #s_v1{session_id=Sid}}.

websocket_handle({ping, _}, Req, S) ->
    {ok, Req, S};
websocket_handle({binary, Raw}, R0, S) ->
    JSON = jsx:decode(Raw, [return_maps]),
    Op = proplists:get_value(<<"op">>, JSON),
    websocket_handle_op(Op, JSON, R0, S).

websocket_handle_op(<<"subscribe">>=Op, #{<<"resource">> := Resource, <<"edge">> := Edge}, _R0, S) ->
    ok = ums_state:subscribe_edge(Edge, Resource),
    websocket_reply_status(ok, Op, S, Edge, Resource);
websocket_handle_op(<<"unsubscribe">>=Op, #{<<"resource">> := Resource, <<"edge">> := Edge}, _R0, S) ->
    ok = ums_state:unsubscribe_edge(Edge, Resource),
    websocket_reply_status(ok, Op, S, Edge, Resource);
websocket_handle_op(<<"sync">>, #{<<"resources">> := Resources, <<"edge">> := Edge}, _R0, S)
  when is_list(Resources) ->
    ok = ums_state:sync(Edge, Resources),
    {reply, {binary, status(ok)}, S}.

websocket_info(_, R0, S) ->
    {ok, R0, S}.

websocket_terminate(_Reason, _R0, #s_v1{session_id=Sid, subscriptions=Subs}) ->
    ums_state:schedule_resource_cleanup(Sid, Subs).

status(ok) ->
    <<"{\"status\": \"ok\"}">>.

websocket_reply_status(ok, Op, S, Edge, Resource) ->
    {reply, {binary, status(ok)}, next_state(Op, S, Edge, Resource)}.

next_state(<<"subscribe">>, S=#s_v1{subscriptions=Subs}, Edge, Resource) ->
    S#s_v1{subscriptions=lists:usort([{Edge, Resource}|Subs])};
next_state(<<"unsubscribe">>, S=#s_v1{subscriptions=Subs}, Edge, Resource) ->
    S#s_v1{subscriptions=lists:delete({Edge, Resource}, Subs)}.
