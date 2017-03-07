-module(ums_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).


start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


init([]) ->

    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},
    {ok, {SupFlags, [
                     ?CHILD(ums_cluster, worker)
                    ]}}.
