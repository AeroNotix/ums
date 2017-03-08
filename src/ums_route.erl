-module(ums_route).

-export([send/2]).


send(Resources, Message) ->
    lager:debug("Sending message to ~p with ~p", [Resources, Message]),
    JSONMessage = jsx:encode(maps:without(<<"resources">>, Message)),
    case ums_state:edges_subscribed(Resources) of
        {ok, Edges}
          when is_list(Edges) ->
            [case httpc:request(post, {binary_to_list(Edge), [], "application/json", JSONMessage}, [], []) of
                 {ok, {{_, 200, _}, _Headers, _}} ->
                     {ok, Edge, sent};
                 {ok, {{_, StatusCode, _}, _Headers, _}} ->
                     {error, Edge, StatusCode}
             end || Edge <- Edges];
        {error, unknown_resource} = E ->
            E
    end.
