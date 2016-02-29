-module(sut).

-include("include/sut.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-export([vagrant_cluster/1]).

-type sut() :: #sut{}.
-export_type([sut/0]).

-compile([export_all]).

cache_dir(#sut{provisioner = vagrant}) ->
    file:make_dir("cache"),
    "cache/";
cache_dir(_Sut) ->
    "/tmp/".

default() ->
    #sut{nodes = ["10.10.10.2", "10.10.10.3", "10.10.10.4"]}.

random(N) ->
    case get(random_seed) of
        undefined ->
            random:seed(erlang:phash2([node()]),
                        erlang:monotonic_time(),
                        erlang:unique_integer());
        _ -> ok
    end,
    random:uniform(N).

random_sut_node(#sut{nodes = Nodes}) ->
    lists:nth(random(length(Nodes)), Nodes).

amqp_connect(Sut) ->
    Node = random_sut_node(Sut),
    {ok, Connection} = amqp_connection:start(#amqp_params_network{host = Node, heartbeat = 20}),
    link(Connection),
    {ok, Channel} = amqp_connection:open_channel(Connection),
    {ok, Connection, Channel}.

-spec run(Cmd :: string()) -> integer().
run(Cmd) ->
    run(Cmd, []).

stringify_args([]) ->
    [];
stringify_args([Int|Rest]) when is_integer(Int) ->
    [integer_to_list(Int)|stringify_args(Rest)];
stringify_args([Something|Rest]) ->
    [Something|stringify_args(Rest)].


-spec run(Cmd :: string(), Env :: [{string(), string()}]) -> integer().
run([[_|_] = Cmd | Args], Env) ->
    Port = open_port({spawn_executable, Cmd},
                     [{env, Env}, stderr_to_stdout, exit_status, {line, 10000}, {args, stringify_args(Args)}]),
    run_loop([], Port);
run(Cmd, Env) ->
    Port = open_port({spawn, Cmd},
                     [{env, Env}, stderr_to_stdout, exit_status, {line, 10000}]),
    run_loop([], Port).

run_loop(Prefix, Port) ->
    receive
        {Port, {data, {Flag, Data}}} when  Flag =:= eol orelse Flag =:= noeol ->
            file:write(standard_io, [Prefix, Data, "\n"]),
            run_loop(Prefix, Port);
        {Port, {data, Data}} ->
            file:write(standard_io, [Prefix, Data]),
            run_loop(Prefix, Port);
        {Port, {exit_status, Status}} ->
            Status;
        {Port, _} ->
            run_loop(Prefix, Port)
    end.

-spec fresh_vagrant_cluster(NumNodes :: non_neg_integer()) -> ok.
fresh_vagrant_cluster(NumNodes) ->
    run("vagrant destroy -f"),
    vagrant_cluster(NumNodes).

-spec vagrant_cluster(NumNodes :: non_neg_integer()) -> ok.
vagrant_cluster(NumNodes) ->
    0 = run("vagrant up --provider virtualbox",
            [{"APT_PROXY_URL", "http://10.10.10.1:3142/"},
             {"VM_COUNT", integer_to_list(NumNodes)}]),
    #sut{nodes = ["10.10.10." ++ integer_to_list(Node) || Node <- lists:seq(2, NumNodes + 1)],
         provisioner = vagrant}.

install_deb(CachedFileName, #sut{provisioner = vagrant} = _Sut) ->
    _ = "
       dpkg -i /vagrant/ " ++ CachedFileName ++ "
       DEBIAN_FRONTEND=noninteractive apt-get -y -f install
    ",
    %% ok = run_root_script(Script, Sut).
    ok.

git_checkout_cluster(Dir, NumNodes) ->
    0 = run([code:priv_dir(mos_like_rabbit_cluster) ++ "/git_checkout_cluster.sh",
             Dir, NumNodes]).

-include_lib("eunit/include/eunit.hrl").

random_sut_nodes_test() ->
    Nodes = ["1", "2", "3"],
    Sut = #sut{nodes = Nodes},
    ?assert(lists:member(random_sut_node(Sut), Nodes)).

run_test() ->
    102 = run("bash -c 'exit $EXIT_CODE'", [{"EXIT_CODE", "102"}]).

lo() ->
    ok.
