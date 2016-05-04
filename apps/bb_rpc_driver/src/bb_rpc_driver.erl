-module(bb_rpc_driver).
-export([new/1, run/4]).

-compile([{parse_transform, lager_transform}]).
-include_lib("amqp_client/include/amqp_client.hrl").

-record(state, {server_conn
               ,client_conn
               ,server_chan
               ,client_chan
               ,queue
               ,reply_queue
               ,correlation_id
               }).

-record(server_state, {chan
                      }).

prepare_server_fabric(Chan) ->
    #'queue.declare_ok'{queue = QName} = amqp_channel:call(Chan, #'queue.declare'{durable = true}),
    lager:debug("Declared server queue ~s", [QName]),
    {QName}.

prepare_client_fabric(Chan) ->
    #'queue.declare_ok'{queue = QName} = amqp_channel:call(Chan, #'queue.declare'{durable = true}),
    lager:debug("Declared client queue ~s", [QName]),
    {QName}.

connect({Host, Port}) ->
    SslOptions = basho_bench_config:get(amqp_ssl_options, none),
    {ok, Conn} = amqp_connection:start(#amqp_params_network{host = Host, port = Port, ssl_options = SslOptions}),
    {ok, Chan} = amqp_connection:open_channel(Conn),
    {Conn, Chan}.

new(_Id) ->
    application:ensure_all_started(amqp_client),
    [_, ClientConnectCreds, ServerConnectCreds] = basho_bench_config:get(amqp_servers, []),
    {ClientConn, ClientChan} = connect(ClientConnectCreds),
    {ServerConn, ServerChan} = connect(ServerConnectCreds),
    {QName} = prepare_server_fabric(ServerChan),
    {ReplyQName} = prepare_client_fabric(ClientChan),
    start_server(ServerConn, ServerChan, QName),
    consume_replies(ClientChan, ReplyQName),
    {ok, #state{client_conn = ClientConn
               ,client_chan = ClientChan
               ,server_conn = ServerConn
               ,server_chan = ServerChan
               ,queue = QName
               ,reply_queue = ReplyQName
               ,correlation_id = 1
               }}.


send_rpc_req(#state{correlation_id = CorrelationId0, reply_queue = QName} = State) ->
    Payload = <<"====================================================================================================",
                "====================================================================================================",
                "====================================================================================================",
                "====================================================================================================",
                "====================================================================================================",
                "====================================================================================================",
                "====================================================================================================",
                "====================================================================================================">>,
    CorrelationId1 = CorrelationId0 + 1,
    Props = #'P_basic'{correlation_id = erlang:integer_to_binary(CorrelationId1), reply_to = QName},
    Publish = #'basic.publish'{routing_key = State#state.queue
                              ,mandatory = true
                              },
    amqp_channel:cast(State#state.client_chan, Publish, #amqp_msg{payload = Payload, props = Props}),
    {erlang:integer_to_binary(CorrelationId1), State#state{correlation_id = CorrelationId1}}.

run(_, _, _, State0) ->
    {RPCId, State1} = send_rpc_req(State0),
    wait_rpc_reply(RPCId, State1),
    {ok, State1}.

start_server(_Conn, Chan, Queue) ->
    spawn_link(fun () ->
                       #'basic.qos_ok'{} = amqp_channel:call(Chan, #'basic.qos'{prefetch_count = 1}),
                       #'basic.consume_ok'{} = amqp_channel:call(Chan, #'basic.consume'{queue = Queue}),
                       server_loop(#server_state{chan = Chan})
               end),
    ok.

server_loop(State) ->
    receive
        #'basic.consume_ok'{} ->
            server_loop(State);
        {#'basic.deliver'{} = Deliver, Msg} ->
            handle_server_deliver(Deliver, Msg, State);
        M ->
            lager:debug("Loop received: ~p", [M]),
            server_loop(State)
    end.

handle_server_deliver(#'basic.deliver'{delivery_tag = DTag},
                      #amqp_msg{props = #'P_basic'{correlation_id = RPCId, reply_to = ReplyTo}},
                      #server_state{chan = Chan} = State) ->
    PropsOut = #'P_basic'{correlation_id = RPCId},
    Payload = <<"====================================================================================================",
                "====================================================================================================",
                "====================================================================================================",
                "====================================================================================================",
                "====================================================================================================",
                "====================================================================================================",
                "====================================================================================================",
                "====================================================================================================">>,
    Publish = #'basic.publish'{routing_key = ReplyTo},
    amqp_channel:cast(Chan, Publish, #amqp_msg{payload = Payload, props = PropsOut}),
    amqp_channel:cast(Chan, #'basic.ack'{delivery_tag = DTag}),
    server_loop(State).

consume_replies(Chan, QName) ->
    #'basic.qos_ok'{} = amqp_channel:call(Chan, #'basic.qos'{prefetch_count = 1}),
    #'basic.consume_ok'{} = amqp_channel:call(Chan, #'basic.consume'{queue = QName}),
    receive
        #'basic.consume_ok'{} ->
            ok
    end,
    ok.

wait_rpc_reply(RPCId, #state{client_chan = Chan}) ->
    receive
        {#'basic.deliver'{delivery_tag = DTag}, #amqp_msg{props = #'P_basic'{correlation_id = RPCId}}} ->
            amqp_channel:cast(Chan, #'basic.ack'{delivery_tag = DTag}),
            ok;
        Any ->
            lager:debug("Any: ~p", [Any])
    end.
