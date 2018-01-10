-module(ums_app).

-behaviour(application).

-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
    ok = ums_cowboy:start_listener(),
    ok = ums_state:init(),
    ok = ums_session_manager:init(),
    ums_sup:start_link().

stop(_State) ->
    ok.
