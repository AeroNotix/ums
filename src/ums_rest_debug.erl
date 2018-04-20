-module(ums_rest_debug).


-export([init/2]).
-export([terminate/3]).

init(R0, Opts) ->
    Method = cowboy_req:method(R0),
    HasBody = cowboy_req:has_body(R0),
    R1 = maybe_send_message(Method, HasBody, R0),
    {ok, R1, Opts}.

maybe_send_message(<<"POST">>, true, R0) ->
    {ok, JSON, R1} = ums_cowboy:read_json_body(R0),
    send_message_to_all_websockets(JSON),
    cowboy_req:reply(200, #{}, <<>>, R1);
maybe_send_message(_, true, R0) ->
    cowboy_req:reply(405, #{}, <<>>, R0);
maybe_send_message(_, false, R0) ->
    cowboy_req:reply(400, #{}, <<>>, R0).

terminate(_Reason, _Request, _State) ->
    ok.

send_message_to_all_websockets(JSON) ->
    lager:debug("Sending debug message to websocket: ~p", [JSON]),
    SendToCowboyWebSocket =
        fun(Pid) ->
                case process_info(Pid, current_function) == {current_function, {cowboy_websocket,loop,3}} of
                    true ->
                        Pid ! {debug_message, maps:remove(<<"resources">>, JSON)};
                    false ->
                        ok
                end
        end,
    lists:map(SendToCowboyWebSocket, processes()).
