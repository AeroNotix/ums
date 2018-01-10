-module(ums_rest_health).

-export([init/2]).


init(R0, Opts) ->
    Headers = #{
      <<"content-type">> => <<"text/plain">>
     },
    R1 = cowboy_req:reply(200, Headers, <<>>, R0),
    {ok, R1, Opts}.
