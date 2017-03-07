-module(ums).

-export([notify/2]).


notify(Resource, Msg) ->
    Edge = ums_state:edge_subscribed(Resource),
    UMSHeaders = [{<<"x-ums-source">>, ums_server:identifier()}],
    JSON = jsx:encode(Msg),
    Request = {Edge, UMSHeaders, "application/json", JSON},
    {ok, {{"HTTP/1.1", ReturnCode, _}, _Head, _Body}} = httpc:request(post, Request, [], []),
    case ReturnCode of
        200 ->
            ok;
        _ ->
            %% Do something here with backing up messages?
            ok
    end.
