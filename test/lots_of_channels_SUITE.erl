%% check that `rabbitmqctl list_channels` is reasonably fast in cluster environment
-module(lots_of_channels_SUITE).

-export([all/0]).

-export([list_channels_should_be_fast_test/1]).

-include_lib("sut/include/sut.hrl").

all() ->
    [list_channels_should_be_fast_test].

channel_opening_user(Acker) ->
    {ok, Connection, _Channel} = sut:amqp_connect(),
    [ amqp_connection:open_channel(Connection) || _ <- lists:seq(1, 200) ],
    sut:ack_user(Acker),
    Unique = make_ref(),
    receive
        Unique ->
            ok
    end.

%% query node directly about it's channels
list_channels_local() ->
    NumNodes = sut:num_nodes(),
    lists:foreach(fun (NodeNum) ->
                          0 = sut:ctl(NodeNum, ["eval", "rabbit_misc:filter_exit_map(fun (C) -> rabbit_channel:info(C) end, rabbit_channel:list_local()), ok."])
                  end, lists:seq(1, NumNodes)),
    ok.

list_channels_ctl() ->
    0 = sut:ctl(1, ["list_channels"]).

list_channels_should_be_fast_test(_Config) ->
    sut:start(),
    sut:default(),
    sut:start_users(50, fun channel_opening_user/1),
    0 = sut:ctl(1, ["status"]),

    %% Establish baseline
    {TimeEval, _} = timer:tc(fun list_channels_local/0),

    %% Localhost with no delay should be close to baseline
    sut:network_delay(false),
    {TimeList, _} = timer:tc(fun list_channels_ctl/0),

    %% And this currently goes out of hand and needs to be fixed
    sut:network_delay(1),
    {TimeListDelay, _} = timer:tc(fun list_channels_ctl/0),

    sut:info("Direct ~p, normal ~p, delayed ~p", [TimeEval, TimeList, TimeListDelay]),

    %% Introducing delay shouldn't cost us that much
    true = TimeList * 2 > TimeListDelay,

    ok.
