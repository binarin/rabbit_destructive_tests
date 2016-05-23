-module(bb_rpc_driver).

%% worst case:
%% 1 - all queues are declared here
%% 2 - servers connect here
%% 3 - clients connect here

-export([new/1, run/4]).

-compile([{parse_transform, lager_transform}]).
-include_lib("amqp_client/include/amqp_client.hrl").

-record(config, {placement = worst
                ,pattern = explicit_reply_queue
                ,servers = []
                }).

-record(server_state, {chan
                      ,queue
                      ,config
                      }).

-record(client_state, {config
                      ,request_queue
                      ,reply_queue
                      ,chan
                      ,correlation_id
                      }).

new(_Id) ->
    application:ensure_all_started(amqp_client),
    Config = parse_config(),
    {QName} = start_server(Config),
    {ok, init_client(QName, Config)}.

run(_, _, _, State0) ->
    {RPCId, State1} = send_rpc_req(State0),
    wait_rpc_reply(RPCId, State1),
    {ok, State1}.

parse_config() ->
    Servers = basho_bench_config:get(amqp_servers, []),
    Placement = basho_bench_config:get(oslo_queue_placement, worst),
    Pattern = basho_bench_config:get(oslo_pattern, explicit_reply_queue),
    #config{servers = Servers
           ,placement = Placement
           ,pattern = Pattern
           }.

start_server(#config{} = Config) ->
    {QName} = prepare_server_fabric(Config),
    ServerCreds = choose_server_connect_placement(Config#config.placement, Config),
    {_ServerConn, ServerChan} = connect(ServerCreds),
    spawn_server(#server_state{chan = ServerChan
                              ,queue = QName
                              ,config = Config
                              }),
    {QName}.

prepare_server_fabric(#config{placement = Placement} = Config) ->
    DeclareCreds = choose_server_declare_placement(Placement, Config),
    {DeclareConn, DeclareChan} = connect(DeclareCreds),
    #'queue.declare_ok'{queue = QName} = amqp_channel:call(DeclareChan, #'queue.declare'{durable = true}),
    amqp_connection:close(DeclareConn),
    lager:debug("Declared server queue ~s", [QName]),
    {QName}.

choose_server_declare_placement(first_node, #config{servers = Servers}) ->
    [DeclareCreds, _, _] = Servers,
    DeclareCreds;
choose_server_declare_placement(worst, #config{servers = Servers}) ->
    [DeclareCreds, _, _] = Servers,
    DeclareCreds.

choose_server_connect_placement(first_node, #config{servers = Servers}) ->
    [Creds, _, _] = Servers,
    Creds;
choose_server_connect_placement(worst, #config{servers = Servers}) ->
    [_, Creds, _] = Servers,
    Creds.

choose_client_connect_placement(first_node, #config{servers = Servers}) ->
    [Creds, _, _] = Servers,
    Creds;
choose_client_connect_placement(worst, #config{servers = Servers}) ->
    [_, _, Creds] = Servers,
    Creds.

connect({Host, Port}) ->
    SslOptions = basho_bench_config:get(amqp_ssl_options, none),
    {ok, Conn} = amqp_connection:start(#amqp_params_network{host = Host, port = Port, ssl_options = SslOptions}),
    {ok, Chan} = amqp_connection:open_channel(Conn),
    {Conn, Chan}.

spawn_server(#server_state{chan = Chan, queue = Queue} = _State) ->
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
    Payload = payload(),
    Publish = #'basic.publish'{routing_key = ReplyTo},
    amqp_channel:cast(Chan, Publish, #amqp_msg{payload = Payload, props = PropsOut}),
    amqp_channel:cast(Chan, #'basic.ack'{delivery_tag = DTag}),
    server_loop(State).

init_client(QName, Config) ->
    State1 = #client_state{config = Config,
                           correlation_id = 1,
                           request_queue = QName},
    State2 = prepare_client_fabric(State1),
    State3 = client_connect(State2),
    State4 = consume_replies(State3),
    State4.

prepare_client_fabric(#client_state{config = #config{placement = direct_reply_to}} = State) ->
    State#client_state{reply_queue = <<"amq.rabbitmq.reply-to">>};
prepare_client_fabric(#client_state{config = Config} = State) ->
    [DeclareCreds, _, _] = Config#config.servers,
    {Conn, Chan} = connect(DeclareCreds),
    #'queue.declare_ok'{queue = QName} = amqp_channel:call(Chan, #'queue.declare'{durable = true}),
    lager:debug("Declared client queue ~s", [QName]),
    amqp_connection:close(Conn),
    State#client_state{reply_queue = QName}.

client_connect(#client_state{config = Config} = State) ->
    ClientCreds = choose_client_connect_placement(Config#config.placement, Config),
    {_Conn, Chan} = connect(ClientCreds),
    State#client_state{chan = Chan}.

consume_replies(#client_state{chan = Chan, config = #config{placement = direct_reply_to}} = State) ->
    #'basic.consume_ok'{} = amqp_channel:call(Chan, #'basic.consume'{queue = <<"amq.rabbitmq.reply-to">>, no_ack=true}),
    State;
consume_replies(#client_state{chan = Chan, reply_queue = Queue} = State) ->
    #'basic.qos_ok'{} = amqp_channel:call(Chan, #'basic.qos'{prefetch_count = 1}),
    #'basic.consume_ok'{} = amqp_channel:call(Chan, #'basic.consume'{queue = Queue}),
    receive
        #'basic.consume_ok'{} ->
            ok
    end,
    State.

payload() ->
    <<"====================================================================================================",
      "====================================================================================================",
      "====================================================================================================",
      "====================================================================================================",
      "====================================================================================================",
      "====================================================================================================",
      "====================================================================================================",
      "====================================================================================================">>.

send_rpc_req(#client_state{correlation_id = CorrelationId0, request_queue = RequestQueue} = State) ->
    Payload = payload(),
    CorrelationId1 = CorrelationId0 + 1,
    Props = #'P_basic'{correlation_id = erlang:integer_to_binary(CorrelationId1), reply_to = client_reply_to(State)},
    Publish = #'basic.publish'{routing_key = RequestQueue
                              ,mandatory = true
                              },
    amqp_channel:cast(State#client_state.chan, Publish, #amqp_msg{payload = Payload, props = Props}),
    {erlang:integer_to_binary(CorrelationId1), State#client_state{correlation_id = CorrelationId1}}.

client_reply_to(#client_state{reply_queue = Queue}) ->
    Queue.

wait_rpc_reply(RPCId, #client_state{chan = Chan}) ->
    receive
        {#'basic.deliver'{delivery_tag = DTag}, #amqp_msg{props = #'P_basic'{correlation_id = RPCId}}} ->
            amqp_channel:cast(Chan, #'basic.ack'{delivery_tag = DTag}),
            ok;
        Any ->
            lager:debug("Any: ~p", [Any])
    end.
