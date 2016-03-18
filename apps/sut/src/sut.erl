-module(sut).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(TIMEOUT, 3600 * 1000).

%% gen_server callbacks
-export([terminate/2, init/1, handle_info/2, handle_cast/2, handle_call/3, code_change/3]).

%% API
-export([start/0
        ,default/0
        ,amqp_connect/0
        ,random_node/0
        ,start_users/2
        ,ack_user/1
        ,ctl/2
        ,ctl_list/2
        ,info/1
        ,info/2
        ,network_delay/1
        ,num_nodes/0
        ]).

%% API for querying state
-export([queue_exists/2]).

-type host_only() :: string().
-type host_port() :: {host(), 1..65535}.
-type host() :: host_only() | host_port().
-export_type([host/0, host_only/0, host_port/0]).

-record(state, {nodes = [] :: [host()],
                ctl_path = "" :: string(),
                ctl_env = [] :: [{string(), string()}]}).

-include_lib("sut/include/sut.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start() -> {ok, pid()}.
start() ->
    application:ensure_all_started(amqp_client),
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

%% @doc Initializes SUT to some default configuration - single running
%% broker with basic config.
-spec default() -> ok.
default() ->
    NodeCount = list_to_integer(os:getenv("NODE_COUNT", "3")),
    case os:getenv("RABBITMQ_GIT") of
        Path when is_list(Path) ->
            gen_server:call(?SERVER, {git_checkout_cluster, Path, NodeCount}, 3600 * 1000);
        _ ->
            exit(no_default_deploy_method_selected)
    end.

%% @doc Creates AMQP connection to random node under test
-spec amqp_connect() -> {ok, ConnectionPid :: pid(), ChannelPid :: pid()}.
amqp_connect() ->
    {ok, Node} = random_node(),
    {ok, Connection} = amqp_connection:start(sut_node_to_amqp_network_params(Node)),
    link(Connection),
    {ok, Channel} = amqp_connection:open_channel(Connection),
    {ok, Connection, Channel}.

%% @doc Returns a random (supposed-to-be) healthy node.
-spec random_node() -> {ok, host()}.
random_node() ->
    gen_server:call(?SERVER, random_node).

-spec start_users(Count :: non_neg_integer(),
                  fun(() -> term())) -> ok.
start_users(Count, Fun) ->
    gen_server:call(?SERVER, {start_users, Count, Fun}, ?TIMEOUT).

ctl(NodeNumber, CtlArgs) ->
    {ok, CtlPath, BaseArgs, Env} = gen_server:call(?SERVER, {ctl_run_template, NodeNumber}),
    sut_exec:system([CtlPath | BaseArgs ++ CtlArgs], Env).

ctl_list(NodeNumber, CtlArgs) ->
    {ok, CtlPath, BaseArgs, Env} = gen_server:call(?SERVER, {ctl_run_template, NodeNumber}),
    {0, [<<"Listing", _/binary>> | Items]} = sut_exec:run(CtlPath, BaseArgs ++ CtlArgs, [{env, Env}, line]),
    [list_to_tuple(re:split(Item, <<"\t">>)) || Item <- Items].

ack_user(Acker) ->
    Acker ! {user_ack, self()}.

info(Msg) ->
    ct:pal(Msg).

info(Fmt, Args) ->
    ct:pal(Fmt, Args).

%% Introduce network delay between cluster nodes (in milliseconds).
-spec network_delay(non_neg_integer() | false) -> ok.
network_delay(false) ->
    gen_server:call(?SERVER, reset_network_delay);
network_delay(Delay) ->
    gen_server:call(?SERVER, {network_delay, Delay}).

num_nodes() ->
    gen_server:call(?SERVER, num_nodes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Querying API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec queue_exists(NodeNumber :: non_neg_integer(), QName) -> boolean() when
      QName :: atom() | string() | binary().

queue_exists(NodeNumber, QName) when is_atom(QName) ->
    queue_exists(NodeNumber, atom_to_binary(QName, utf8));
queue_exists(NodeNumber, QName) when is_list(QName) ->
    queue_exists(NodeNumber, list_to_binary(QName));
queue_exists(NodeNumber, QName) ->
    Queues = ctl_list(NodeNumber, ["list_queues"]),
    case proplists:get_value(QName, Queues) of
        undefined -> false;
        _ -> true
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([]) ->
    {ok, #state{}}.

handle_call(num_nodes, _From, #state{nodes = Nodes} = State) ->
    {reply, length(Nodes), State};
handle_call(reset_network_delay, _From, State) ->
    run_helper("slow_loopback.sh", ["stop"]),
    {reply, ok, State};
handle_call({network_delay, Delay}, _From, State) ->
    run_helper("slow_loopback.sh", ["start", Delay]),
    {reply, ok, State};
handle_call({ctl_run_template, NodeNumber}, _From, State) ->
    {reply, prepare_ctl_run_template(NodeNumber, State), State};
handle_call({start_users, Count, Fun}, From, State) ->
    do_start_users(Count, Fun, From),
    {noreply, State};
handle_call(random_node, _From, State) ->
    {reply, {ok, choose_random_node(State)}, State};
handle_call({git_checkout_cluster, Path, NodeNumber}, _From, _State) ->
    sut:info("Starting ~b-node cluster using git checkout of rabbit at ~s", [NodeNumber, Path]),
    NewState = git_checkout_cluster(Path, NodeNumber),
    sut:info("Initialized ~p node cluster", [length(NewState#state.nodes)]),
    {reply, ok, NewState};

handle_call(_Msg, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
run_helper(Helper, Args) ->
    0 = sut_exec:system([code:priv_dir(sut) ++ "/" ++ Helper | Args]).

git_checkout_cluster(Dir, NumNodes) ->
    0 = sut_exec:system([code:priv_dir(sut) ++ "/git_checkout_cluster.sh", Dir, NumNodes]),
    #state{nodes = [ {"127.0.0.1", 17000 + NodeNumber} || NodeNumber <- lists:seq(1, NumNodes) ],
           ctl_path = Dir ++ "/scripts/rabbitmqctl",
           ctl_env = [{"ERL_LIBS", Dir ++ "/deps"}]
          }.

sut_node_to_amqp_network_params({Host, Port}) ->
    #amqp_params_network{host = Host, port = Port, heartbeat = 20};
sut_node_to_amqp_network_params(Node) ->
    #amqp_params_network{host = Node, heartbeat = 20}.

choose_random_node(#state{nodes = Nodes}) ->
    lists:nth(sut_utils:random(length(Nodes)), Nodes).

do_start_users(Count, Fun, From) ->
    spawn_link(fun() ->
                       Acker = self(),
                       Pids = [erlang:spawn_link(fun() -> Fun(Acker) end) || _ <- lists:seq(1, Count)],
                       lists:foreach(fun (Pid) ->
                                             receive
                                                 {user_ack, Pid} ->
                                                     ok
                                             after
                                                 100000 ->
                                                     exit({user_failed_to_start, Pid})
                                             end
                                     end,
                                     Pids),
                       info("Started ~p '~p' users", [Count, Fun]),
                       gen_server:reply(From, ok)
               end),
    ok.

prepare_node_name(NodeNumber, _State) ->
    "test-cluster-node-" ++ integer_to_list(NodeNumber) ++ "@localhost".

prepare_ctl_run_template(NodeNumber, #state{ctl_path = Path, ctl_env = Env} = State) ->
    {ok, Path, ["-n", prepare_node_name(NodeNumber, State)], Env}.
