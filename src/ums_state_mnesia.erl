-module(ums_state_mnesia).

-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("ums/include/ums.hrl").

-export([init/0]).
-export([edges_subscribed/1]).
-export([subscriptions_for_session_id/1]).
-export([subscribe_edge/3]).
-export([unsubscribe_edge/3]).
-export([add_node_to_mnesia_cluster/1]).
-export([remove_subscriptions_by_session_id/1]).
-export([table_exists/0]).

-record(umss_v1, {
          session_id,
          resource,
          edge_server
         }).

-behaviour(ums_state).

init() ->
    ok = install_mnesia(),
    ok = mnesia:wait_for_tables([ums_state], infinity),
    lager:error("Mnesia started").

edges_subscribed(Resources)
  when is_list(Resources) ->
    MatchHead = #umss_v1{session_id='_', resource='$1', edge_server='$2'},
    MatchGuards = [{'==', '$1', Resource} || Resource <- Resources],
    Result = '$2',
    F = fun() ->
                mnesia:select(ums_state, [{MatchHead, MatchGuards, [Result]}])
        end,
    case mnesia:transaction(F) of
        [] ->
            {error, unknown_resource};
        {atomic, Edges} when is_list(Edges) ->
            {ok, Edges}
    end.

subscriptions_for_session_id(SessionId) ->
    MS = ets:fun2ms(
           fun(#umss_v1{session_id=S, resource=R, edge_server=E})
                 when S == SessionId ->
                   {R, E}
           end),
    F = fun() ->
                mnesia:select(ums_state, MS)
        end,
    {atomic, Subscriptions} = mnesia:transaction(F),
    {ok, Subscriptions}.

subscribe_edge(SessionId, Edge, Resources)
  when is_list(Resources) ->
    [ok = subscribe_edge(SessionId, Edge, Resource) || Resource <- Resources],
    ok;
subscribe_edge(SessionId, Edge, Resource) ->
    lager:debug("Subscribing edge in session: ~p", [{SessionId, Edge, Resource}]),
    Subscription = #umss_v1{session_id=SessionId, resource=Resource, edge_server=Edge},
    mnesia:dirty_write(ums_state, Subscription).

unsubscribe_edge(SessionId, Edge, Resource) ->
    Subscription = #umss_v1{session_id=SessionId, resource=Resource, edge_server=Edge},
    mnesia:dirty_delete_object(ums_state, Subscription).

install_mnesia() ->
    ExpectedNodes = expected_nodes(),
    Nodes = lists:usort(nodes()),
    case ExpectedNodes == Nodes of
        true ->
            lager:debug("Attempting to lock"),
            InstallOnNodes = lists:filter(fun node_is_alive/1, [node() | Nodes]),
            global:trans({mnesia_create_lock, node()},
                         fun() ->
                                 lager:debug("~p got lock", [node()]),
                                 install_mnesia(InstallOnNodes)
                         end,
                         Nodes);
        false ->
            lager:error("Waiting for cluster to be fully formed before becoming operational"),
            timer:sleep(10000),
            install_mnesia()
    end.

install_mnesia(Nodes) ->
    case check_remote_tables_exist(Nodes) of
        true ->
            ok = mnesia:start(),
            case ask_remote_nodes_to_change_config(Nodes, node()) of
                true ->
                    lager:error("Creating schema after cluster join dance: ~p", [mnesia:create_schema(Nodes)]);
                false ->
                    timer:sleep(10000),
                    lager:error("Waiting before trying to join mnesia cluster again"),
                    install_mnesia()
            end;
        false ->
            lager:debug("Creating schema: ~p", [mnesia:create_schema(Nodes)]),
            rpc:multicall(Nodes, application, start, [mnesia]),
            ok = create_table(ums_state,
                              [{attributes, record_info(fields, umss_v1)},
                               {record_name, umss_v1},
                               {ram_copies, Nodes},
                               {type, bag}])
    end.

%% Stolen from https://github.com/ostinelli/syn/blob/master/src/syn_backbone.erl
create_table(TableName, Options) ->
    CurrentNode = node(),
    %% ensure table exists
    case mnesia:create_table(TableName, Options) of
        {atomic, ok} ->
            lager:debug("~p was successfully created", [TableName]),
            ok;
        {aborted, {already_exists, TableName}} ->
            %% table already exists, try to add current node as copy
            add_table_copy_to_current_node(TableName);
        {aborted, {already_exists, TableName, CurrentNode}} ->
            %% table already exists, try to add current node as copy
            add_table_copy_to_current_node(TableName);
        Other ->
            lager:debug("Error while creating ~p: ~p", [TableName, Other]),
            {error, Other}
    end.


add_table_copy_to_current_node(TableName) ->
    CurrentNode = node(),
    %% wait for table
    mnesia:wait_for_tables([TableName], 10000),
    %% add copy
    case mnesia:add_table_copy(TableName, CurrentNode, ram_copies) of
        {atomic, ok} ->
            lager:debug("Copy of ~p was successfully added to current node", [TableName]),
            ok;
        {aborted, {already_exists, TableName}} ->
            lager:debug("Copy of ~p is already added to current node", [TableName]),
            ok;
        {aborted, {already_exists, TableName, CurrentNode}} ->
            lager:debug("Copy of ~p is already added to current node", [TableName]),
            ok;
        {aborted, Reason} ->
            lager:error("Error while creating copy of ~p: ~p", [TableName, Reason]),
            {error, Reason}
    end.

check_remote_tables_exist(N0) ->
    Nodes = lists:usort(N0 -- [node()]),
    lager:debug("Checking tables exist on: ~p", [Nodes]),
    {Replies, _} = rpc:multicall(Nodes, ums_state, table_exists, []),
    lager:debug("Checked tables exist on: ~p, Replies: ~p", [Nodes, Replies]),
    lists:any(fun(X) -> X end, Replies).

table_exists() ->
    Node = node(),
    lager:debug("Table exists check on ~p", [Node]),
    try
        lists:member(ums_state, mnesia:system_info(tables))
    catch
        exit:{aborted, {node_not_running, Node}} ->
            lager:debug("Table exist check - mnesia not running"),
            false
    end.

add_node_to_mnesia_cluster(Node) ->
    lager:debug("Adding node to mnesia cluster: ~p", [Node]),
    {ok, _} = mnesia:change_config(extra_db_nodes, [Node]),
    {atomic, ok} = mnesia:add_table_copy(ums_state, Node, ram_copies),
    ok.

ask_remote_nodes_to_change_config(N0, ForWhom) ->
    Nodes = lists:usort(N0 -- [node()]),
    try
        lager:debug("Asking remote nodes to change config: ~p", [{Nodes, ForWhom}]),
        {Replies, _} = rpc:multicall(Nodes, ums_state, add_node_to_mnesia_cluster, [ForWhom]),
        lager:debug("Asked remote nodes to change config: ~p", [Replies]),
        lists:all(fun(X) -> ok == X end, Replies)
    catch
        E:R ->
            lager:debug("Unhandled error requesting config change: ~p", [{E,R}]),
            exit(ask_remote_nodes_to_change_config)
    end.

node_is_alive(Node) ->
    net_adm:ping(Node) /= pang.

expected_nodes() ->
    case application:get_env(ums, clustered, false) of
        true ->
            lists:usort(werld:expected_nodes() -- [node()]);
        false ->
            []
    end.


remove_subscriptions_by_session_id(SessionId) ->
    RemoveBySessionId =
        fun() ->
                try
                    lager:debug("Removing all sessions for: ~p",
                                [{SessionId}]),
                    Res = mnesia:delete(ums_state, SessionId, write),
                    lager:debug("Sessions removed for: ~p. Result: ~p", [SessionId, Res]),
                    Res
                catch
                    E:R ->
                        lager:error("Error cleaning up resources: ~p", [{E,R}])
                end

        end,
    mnesia:transaction(RemoveBySessionId).
