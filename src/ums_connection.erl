-module(ums_connection).

-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-define(SID_HEADER, <<"x-ums-session-id">>).

-record(s_v1, {
          session_id :: binary(),
          subscriptions = [] :: list()
         }).

init({_, _}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, R0, _Opts) ->
    {Sid, Subscriptions, R2} =
        case cowboy_req:header(?SID_HEADER, R0) of
            {undefined, R1} ->
                Sid0 = ums_server:new_session_id(),
                {Sid0, [], cowboy_req:set_resp_header(?SID_HEADER, Sid0, R1)};
            {SidHeader, R1} ->
                {ok, Subs} = ums_state:notify_session_reestablished(SidHeader),
                lager:debug("Reestablishing session: ~p", [Subs]),
                self() ! inform_client_of_known_subscriptions,
                {SidHeader, Subs, cowboy_req:set_resp_header(?SID_HEADER, SidHeader, R1)}
        end,
    {ok, R2, #s_v1{session_id=Sid, subscriptions=Subscriptions}}.

websocket_handle({ping, _}, Req, S) ->
    {ok, Req, S};
websocket_handle({binary, Raw}, R0, S) ->
    #{<<"op">> := Op} = JSON = jsx:decode(Raw, [return_maps]),
    websocket_handle_op(Op, JSON, R0, S).

websocket_handle_op(<<"subscribe">>=Op, #{<<"resource">> := Resource, <<"edge">> := Edge}, R0, #s_v1{session_id=Sid}=S) ->
    ok = ums_state:subscribe_edge(Sid, Edge, Resource),
    websocket_reply_status(ok, Op, S, R0, Edge, Resource);
websocket_handle_op(<<"unsubscribe">>=Op, #{<<"resource">> := Resource, <<"edge">> := Edge}, R0, #s_v1{session_id=Sid}=S) ->
    ok = ums_state:unsubscribe_edge(Sid, Edge, Resource),
    websocket_reply_status(ok, Op, S, R0, Edge, Resource).

websocket_info(inform_client_of_known_subscriptions, R0, #s_v1{session_id=Sid}=State) ->
    {ok, Subscriptions} = ums_state:subscriptions_for_session_id(Sid),
    lager:debug("Informing client of known sessions: ~p", [{Sid, Subscriptions}]),
    {reply, {binary, subscriptions_to_json(Subscriptions)}, R0, State}.

websocket_terminate(_Reason, _R0, #s_v1{session_id=Sid}) ->
    ums_state:schedule_resource_cleanup(Sid).

status(ok) ->
    <<"{\"status\": \"ok\"}">>.

websocket_reply_status(ok, Op, S, Req, Edge, Resource) ->
    {reply, {binary, status(ok)}, Req, next_state(Op, S, Edge, Resource)}.

next_state(<<"subscribe">>, S=#s_v1{subscriptions=Subs}, Edge, Resource) ->
    S#s_v1{subscriptions=lists:usort([{Resource, Edge}|Subs])};
next_state(<<"unsubscribe">>, S=#s_v1{subscriptions=Subs}, Edge, Resource) ->
    S#s_v1{subscriptions=lists:delete({Resource, Edge}, Subs)}.

subscriptions_to_json(Subscriptions) ->
    jsx:encode(
      [{subscriptions,
        [[{resource, Resource}, {edge, Edge}] || {Resource, Edge} <- Subscriptions]}]
      ).
