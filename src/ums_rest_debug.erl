-module(ums_rest_debug).


-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_, R0, _Opts) ->
    {ok, R0, undefined}.

handle(R0, State) ->
    {Method, R1} = cowboy_req:method(R0),
    HasBody = cowboy_req:has_body(R1),
    {ok, R2} = maybe_send_message(Method, HasBody, R1),
    {ok, R2, State}.

maybe_send_message(<<"POST">>, true, R0) ->
    {ok, JSON, R1}
        = ums_cowboy:read_json_body(R0),
    send_message_to_all_websockets(JSON),
    cowboy_req:reply(200, [], <<>>, R1);
maybe_send_message(_, true, R0) ->
    cowboy_req:reply(405, [], <<>>, R0);
maybe_send_message(_, false, R0) ->
    cowboy_req:reply(400, [], <<>>, R0).

terminate(_Reason, _Request, _State) ->
    ok.

send_message_to_all_websockets(JSON) ->
    lager:debug("Sending debug message to websocket: ~p", [JSON]),
    SendToCowboyWebSocket =
        fun(Pid) ->
                PInfo = process_info(Pid),
                case proplists:get_value(current_function, PInfo) == {cowboy_websocket,handler_loop,4} of
                    true ->
                        Pid ! {debug_message, maps:remove(<<"resources">>, JSON)};
                    false ->
                        ok
                end
        end,
    lists:map(SendToCowboyWebSocket, processes()).
