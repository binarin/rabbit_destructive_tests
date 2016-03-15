-module(queue_expires_sanity_SUITE).

-export([all/0, init_per_suite/1, init_per_testcase/2, end_per_testcase/2,
         groups/0]).

-export([declaring_and_doing_nothing_afterwards/1
        ,basic_get_resets_timer/1
        ,end_of_consumption_resets_timer/1
        ,connection_close_resets_timer/1
        ,publishing_to_queue_doesnt_reset_timer/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("sut/include/sut.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

groups() ->
    [{reenterable, [parallel],
      [declaring_and_doing_nothing_afterwards
      ,basic_get_resets_timer
      ,end_of_consumption_resets_timer
      ,connection_close_resets_timer
      ,publishing_to_queue_doesnt_reset_timer
      ]}].

all() ->
    [{group, reenterable}].

init_per_suite(Config) ->
    sut:start(),
    sut:default(),
    Config.

init_per_testcase(_, Config) ->
    {ok, Conn, Chan} = sut:amqp_connect(),
    [{conn, Conn}, {chan, Chan}|Config].

end_per_testcase(_, Config) ->
    Conn = ?config(conn, Config),
    unlink(Conn),
    amqp_connection:close(Conn),
    Config.

declaring_and_doing_nothing_afterwards(Config) ->
    QName = declare(Config),
    wait_for_expiration(QName),
    ok.

basic_get_resets_timer(Config) ->
    Chan = ?config(chan, Config),
    QName = declare(Config),
    timer:sleep(5000),
    #'basic.get_empty'{} = amqp_channel:call(Chan, #'basic.get'{queue = QName}),
    wait_for_expiration(QName),
    ok.

end_of_consumption_resets_timer(Config) ->
    QName = declare(Config),
    Chan = ?config(chan, Config),
    #'basic.consume_ok'{consumer_tag = T} =
        amqp_channel:call(Chan, #'basic.consume'{queue = QName}),
    timer:sleep(9000),
    #'basic.cancel_ok'{} =
        amqp_channel:call(Chan, #'basic.cancel'{consumer_tag = T}),
    wait_for_expiration(QName),
    ok.

connection_close_resets_timer(Config) ->
    QName = declare(Config),
    {ok, Conn, Chan} = sut:amqp_connect(),
    #'basic.consume_ok'{} =
        amqp_channel:call(Chan, #'basic.consume'{queue = QName}),
    timer:sleep(5000),
    unlink(Conn),
    amqp_connection:close(Conn),
    wait_for_expiration(QName),
    ok.

publishing_to_queue_doesnt_reset_timer(Config) ->
    QName = declare(Config),
    Chan = ?config(chan, Config),
    timer:sleep(9000),
    amqp_channel:register_confirm_handler(Chan, self()),
    #'confirm.select_ok'{} = amqp_channel:call(Chan, #'confirm.select'{}),
    amqp_channel:cast(Chan, #'basic.publish'{exchange = <<>>, routing_key = QName, mandatory = true},
                      #amqp_msg{payload = <<"Test">>}),
    receive
        #'basic.ack'{} -> ok
    after
        300 -> exit(message_not_delivered_to_queue)
    end,
    timer:sleep(2000),
    false = sut:queue_exists(1, QName),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
wait_for_expiration(QName) ->
    timer:sleep(9000),
    true = sut:queue_exists(1, QName),
    timer:sleep(2000),
    false = sut:queue_exists(1, QName).

declare(Config) ->
    Chan = ?config(chan, Config),
    Declare = #'queue.declare'{arguments = [{<<"x-expires">>, long, 10000}]},
    #'queue.declare_ok'{queue = QName} = amqp_channel:call(Chan, Declare),
    QName.
