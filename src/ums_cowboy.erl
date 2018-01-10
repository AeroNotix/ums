-module(ums_cowboy).

%% Everything is exported and everything is a function because we can
%% mock, properly.
-export([routes/0]).
-export([max_keepalive/0]).
-export([request_timeout/0]).
-export([env/0]).
-export([protoopts/0]).
-export([transopts/0]).
-export([start_listener/0]).
-export([read_json_body/1]).


routes() ->
    DebugEndpoint =
        case application:get_env(ums, debug_endpoint, false) of
            true ->
                [{<<"/debug">>, ums_rest_debug, []}];
            false ->
                []
        end,
    [{<<"/health">>, ums_rest_health, []},
     {<<"/route">>, ums_rest_route, []},
     {<<"/subscribe">>, ums_connection, []}|DebugEndpoint].

max_keepalive() ->
    application:get_env(ums, cowboy_max_keepalive, 150).

request_timeout() ->
    application:get_env(ums, cowboy_max_request_timeout, 60000).

env() ->
    #{dispatch => cowboy_router:compile([{'_', routes()}])}.

protoopts() ->
    #{env => env(),
      max_keepalive => max_keepalive(),
      timeout => request_timeout()}.

transopts() ->
    [{port, application:get_env(ums, client_websocket_port, 5564)},
     {backlog, 1000}].

start_listener() ->
    {ok, _} = cowboy:start_clear(
                http,
                transopts(),
                protoopts()
               ),
    ok.

read_body(Req) ->
    read_body(Req, <<>>).

read_body(Req0, Buf) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req1} ->
            Body = <<Buf/binary, Data/binary>>,
            {ok, Body, Req1};
        {more, Data, Req1} ->
            Body = <<Buf/binary, Data/binary>>,
            read_body(Req1, Body);
        {error, _} = E ->
            E
    end.

read_json_body(Req0) ->
    {ok, JBody, Req1} = read_body(Req0),
    Body = jsx:decode(JBody, [return_maps]),
    {ok, Body, Req1}.
