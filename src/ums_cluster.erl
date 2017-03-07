-module(ums_cluster).

-behaviour(gen_server).

-export([world/0]).

-export([start_link/0]).
-export([code_change/3]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([init/1]).
-export([terminate/2]).


-record(state, { interval :: non_neg_integer() }).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    Interval = application:get_env(ubic_airos, world_interval, 1000 * 60 * 20),
    self() ! world,
    {ok, #state{ interval = Interval }}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Request, State) ->
    {stop, unknown_cast, State}.

handle_info(world, State) ->
    Interval = State#state.interval,
    try
        net_adm:world()
    catch
        exit:{error, enoent} ->
            lager:info("No .hosts.erlang file found."),
            {error, disconnected}
    after
        schedule_check(Interval)
    end,
    {noreply, State};
handle_info(_, State) ->
    {stop, unknown_info, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

world() ->
    try
        ECS = application:get_env(ums, expected_cluster_size, 1),
        ECS = length(net_adm:world()),
        ok
    catch
        _:_ ->
            timer:sleep(5000),
            world()
    end.

schedule_check(Interval) ->
    erlang:send_after(Interval, self(), world).
