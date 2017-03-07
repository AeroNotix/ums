-module(ums_cowboy).

%% Everything is exported and everything is a function because we can
%% mock, properly.
-export([routes/0]).
-export([middlewares/0]).
-export([max_keepalive/0]).
-export([request_timeout/0]).
-export([env/0]).
-export([protoopts/0]).
-export([transopts/0]).
-export([acceptor_count/0]).
-export([listener_name/0]).
-export([start_listener/0]).


routes() ->
    [{<<"/subscribe">>, ums_connection, []}].

middlewares() ->
    [cowboy_router,
     cowboy_handler].

max_keepalive() ->
    application:get_env(ums, cowboy_max_keepalive, 150).

request_timeout() ->
    application:get_env(ums, cowboy_max_request_timeout, 60000).

env() ->
    [{dispatch, cowboy_router:compile([{'_', routes()}])}].

protoopts() ->
    [{env, env()},
     {middlewares, middlewares()},
     {max_keepalive, max_keepalive()},
     {timeout, request_timeout()}].

transopts() ->
    [{port, application:get_env(ums, client_websocket_port, 5564)},
     {backlog, 1000}].

acceptor_count() ->
    application:get_env(ums, cowboy_acceptors, 200).

listener_name() ->
    ums_websocket_listener.

start_listener() ->
    {ok, _} = cowboy:start_http(
                listener_name(),
                acceptor_count(),
                transopts(),
                protoopts()
               ),
    ok.
