%% check that `rabbitmqctl list_channels` is reasonably fast in cluster environment
-module(lots_of_channels_SUITE).

-export([all/0
        ,groups/0
        ,init_per_suite/1
        ]).

-export([list_items_speed/1, list_items_sanity/1]).

-include_lib("sut/include/sut.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-define(CHANNELS_PER_USER, 200).
-define(QUEUES_PER_USER, 300).
-define(NUM_USERS, 50).

all() ->
    [{group, list_test, [{userdata, #{cmd => "list_queues", expected_count => ?QUEUES_PER_USER * ?NUM_USERS}}]}
    ,{group, list_test, [{userdata, #{cmd => "list_channels", expected_count => (1 + ?CHANNELS_PER_USER) * ?NUM_USERS}}]}
    ].

groups() ->
    [{list_test, [], [list_items_speed, list_items_sanity]}].

init_per_suite(Config) ->
    sut:start(),
    sut:default(),
    sut:start_users(?NUM_USERS, fun channel_opening_user/1),
    0 = sut:ctl(1, ["status"]),
    Config.

channel_opening_user(Acker) ->
    {ok, Connection, Channel} = sut:amqp_connect(),
    [ amqp_connection:open_channel(Connection) || _ <- lists:seq(1, ?CHANNELS_PER_USER) ],
    [ #'queue.declare_ok'{} = amqp_channel:call(Channel, #'queue.declare'{exclusive = true}) || _ <- lists:seq(1, ?QUEUES_PER_USER)],
    sut:ack_user(Acker),
    Unique = make_ref(),
    receive
        Unique ->
            ok
    end.

%% Perform list_XXX command equivalent separately for every node and
%% using only local data of that node.
list_local("list_queues") ->
    NumNodes = sut:num_nodes(),
    lists:foreach(fun (NodeNum) ->
                          0 = sut:ctl(NodeNum, ["eval", "rabbit_misc:filter_exit_map(fun (C) -> rabbit_amqqueue:info(C) end, [ Q || Q <- rabbit_amqqueue:list_local(<<$/>>) ] ), ok."])
                  end, lists:seq(1, NumNodes)),
    ok;
list_local("list_channels") ->
    NumNodes = sut:num_nodes(),
    lists:foreach(fun (NodeNum) ->
                          0 = sut:ctl(NodeNum, ["eval", "rabbit_misc:filter_exit_map(fun (C) -> rabbit_channel:info(C) end, rabbit_channel:list_local()), ok."])
                  end, lists:seq(1, NumNodes)),
    ok.

userdata(Config) ->
    GroupProps = proplists:get_value(tc_group_properties, Config),
    proplists:get_value(userdata, GroupProps).

list_items_speed(Config) ->
    #{cmd := Cmd} = userdata(Config),
    ct:pal("Testing speed of '~s'", [Cmd]),

    %% Establish baseline
    {TimeEval, _} = timer:tc(fun list_local/1, [Cmd]),

    %% Localhost with no delay should be close to baseline
    sut:network_delay(false),
    {TimeList, _} = timer:tc(fun sut:ctl_list/2, [1, [Cmd]]),

    %% And this currently goes out of hand and needs to be fixed
    sut:network_delay(1),
    {TimeListDelay, _} = timer:tc(fun sut:ctl_list/2, [1, [Cmd]]),

    sut:info("Direct ~p, normal ~p, delayed ~p", [TimeEval, TimeList, TimeListDelay]),

    %% Introducing delay shouldn't cost us that much
    true = TimeList * 2 > TimeListDelay,

    ok.

%% Check that all items were returned by list command
list_items_sanity(Config) ->
    #{cmd := Cmd, expected_count := Expected} = userdata(Config),

    ct:pal("Testing sanity of '~s', expecting ~b items", [Cmd, Expected]),

    List = sut:ctl_list(1, [Cmd]),
    Got = length(List),

    ct:pal("Got ~b items", [Got]),

    Expected = Got,

    timer:sleep(1000000),
    ok.
