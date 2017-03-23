-module(ums_app).

-behaviour(application).

-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
    ok = start_cowboy(),
    _ = ums_state:install_ets(),
    ok = start_mnesia(),
    ums_sup:start_link().

start_cowboy() ->
    ok = ums_cowboy:start_listener().

start_mnesia() ->
    ok = ums_state:install_mnesia([node()|werld:expected_nodes()]),
    mnesia:wait_for_tables([ums_state], 60000).

stop(_State) ->
    ok.
