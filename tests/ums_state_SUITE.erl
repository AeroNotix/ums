-module(ums_state_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    application:ensure_all_started(ums),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [].

all() ->
    [subscribe_edge_and_lookup].

subscribe_edge_and_lookup(_Config) ->
    Sid = <<"foo">>,
    Edge = <<"http://localhost">>,
    Resource = <<"00000-00000-00000-00000-00000">>,
    ok = ums_state:subscribe_edge(Sid, Edge, Resource),
    ok = ums_state:subscribe_edge(Sid, Edge, Resource),
    {ok, [Edge]} = ums_state:edges_subscribed([Resource]).
