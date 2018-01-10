-module(ums_connection).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([websocket_terminate/2]).

-define(SID_HEADER, <<"x-ums-session-id">>).

-record(s_v1, {
          session_reestablished :: boolean(),
          session_id :: binary(),
          subscriptions = [] :: list()
         }).

init(R0, _) ->
    {SessionReestablished, Sid, R1} =
        case cowboy_req:header(?SID_HEADER, R0) of
            undefined ->
                Sid0 = ums_server:new_session_id(),
                {false, Sid0, cowboy_req:set_resp_header(?SID_HEADER, Sid0, R0)};
            SidHeader when is_binary(SidHeader) ->
                {true, SidHeader, cowboy_req:set_resp_header(?SID_HEADER, SidHeader, R0)}
        end,
    {cowboy_websocket, R1, #s_v1{session_reestablished=SessionReestablished,
                                 session_id=Sid}}.

websocket_init(#s_v1{session_reestablished=true, session_id=Sid}=S) ->
    {ok, Subs} = ums_session_manager:notify_session_reestablished(Sid),
    lager:debug("Reestablishing session: ~p", [Subs]),
    self() ! inform_client_of_known_subscriptions,
    {ok, S#s_v1{session_id=Sid, subscriptions=Subs}};
websocket_init(#s_v1{session_reestablished=false}=S) ->
    {ok, S}.

websocket_handle({ping, _}, S) ->
    {ok, S};
websocket_handle({binary, Raw}, S) ->
    #{<<"op">> := Op} = JSON = jsx:decode(Raw, [return_maps]),
    websocket_handle_op(Op, JSON, S).

websocket_handle_op(<<"subscribe">>=Op, #{<<"resource">> := Resource, <<"edge">> := Edge}, #s_v1{session_id=Sid}=S) ->
    ok = ums_state:subscribe_edge(Sid, Edge, Resource),
    websocket_reply_status(ok, Op, S, Edge, Resource);
websocket_handle_op(<<"unsubscribe">>=Op, #{<<"resource">> := Resource, <<"edge">> := Edge}, #s_v1{session_id=Sid}=S) ->
    ok = ums_state:unsubscribe_edge(Sid, Edge, Resource),
    websocket_reply_status(ok, Op, S, Edge, Resource).

websocket_info(inform_client_of_known_subscriptions, #s_v1{session_id=Sid}=State) ->
    {ok, Subscriptions} = ums_state:subscriptions_for_session_id(Sid),
    lager:debug("Informing client of known sessions: ~p", [{Sid, Subscriptions}]),
    {reply, {binary, subscriptions_to_json(Subscriptions)}, State};
websocket_info({debug_message, JSON}, State) ->
    {reply, {binary, jsx:encode(JSON)}, State}.


websocket_terminate(_Reason, #s_v1{session_id=Sid}) ->
    ums_state:schedule_resource_cleanup(Sid).

status(ok) ->
    <<"{\"status\": \"ok\"}">>.

websocket_reply_status(ok, Op, S, Edge, Resource) ->
    {reply, {binary, status(ok)}, next_state(Op, S, Edge, Resource)}.

next_state(<<"subscribe">>, S=#s_v1{subscriptions=Subs}, Edge, Resource) ->
    S#s_v1{subscriptions=lists:usort([{Resource, Edge}|Subs])};
next_state(<<"unsubscribe">>, S=#s_v1{subscriptions=Subs}, Edge, Resource) ->
    S#s_v1{subscriptions=lists:delete({Resource, Edge}, Subs)}.

subscriptions_to_json(Subscriptions) ->
    jsx:encode(
      [{subscriptions,
        [[{resource, Resource}, {edge, Edge}] || {Resource, Edge} <- Subscriptions]}]
      ).
