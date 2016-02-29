-module(mos_like_rpc_client).
-behaviour(gen_fsm).
-compile([{parse_transform, lager_transform}]).

-export([terminate/3, init/1, handle_sync_event/4, handle_info/3, handle_event/3, code_change/4]).

-include_lib("amqp_client/include/amqp_client.hrl").

-define(AMQP_ACK(Rec),
        is_record(Rec, 'basic.ack') orelse
        is_record(Rec, 'basic.nack')).

%% States
-export([initial/2
        ,do_call/2
        ,wait_publish_confirm/2
        ,drop_acks/2
        ,drop_consume_ok/2
        ,wait_rpc_reply/2
        ]).

-export([start_link/1]).

-record(state, {connection, channel, reply_to}).

start_link(Sut) ->
    gen_fsm:start_link(?MODULE, [Sut], []).

init([Sut]) ->
    StateName = initial,
    StateData = Sut,
    {ok, StateName, StateData, 1}.

handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok,
    {next_state, Reply, StateName, StateData}.

%% Forward messages to current state
handle_info({#'basic.deliver'{}, _} = Delivery, StateName, StateData) ->
    ?MODULE:StateName(Delivery, StateData);
handle_info(#'basic.consume_ok'{} = ConsumeOk, StateName, StateData) ->
    ?MODULE:StateName(ConsumeOk, StateData);
handle_info({#'basic.return'{} = Return, _}, StateName, StateData) ->
    ?MODULE:StateName(Return, StateData);
handle_info(AMQPAck, StateName, StateData) when ?AMQP_ACK(AMQPAck) ->
    ?MODULE:StateName(AMQPAck, StateData).

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%% States
initial(timeout, Sut) ->
    {ok, Connection, Channel} = sut:amqp_connect(Sut),
    amqp_channel:register_return_handler(Channel, self()),
    amqp_channel:register_confirm_handler(Channel, self()),
    #'confirm.select_ok'{} = amqp_channel:call(Channel, #'confirm.select'{}),
    #'queue.declare_ok'{queue = ReplyTo} = amqp_channel:call(Channel, #'queue.declare'{auto_delete = true}),
    #'basic.consume_ok'{} = amqp_channel:call(Channel, #'basic.consume'{queue = ReplyTo}),
    lager:debug("RPC client connected to broker: ~p, reply queue: ~s", [Connection, ReplyTo]),
    {next_state, drop_consume_ok, #state{connection = Connection, channel = Channel, reply_to = ReplyTo}, 1}.

drop_consume_ok(#'basic.consume_ok'{}, State) ->
    {next_state, do_call, State, 1}.

do_call(timeout, #state{channel = Channel} = State) ->
    Ref = make_ref(),
    CorrelationId = term_to_binary(Ref),
    Payload = <<"doesn't matter">>,
    Publish = #'basic.publish'{exchange = <<"ha-rpc-exchange">>, mandatory=true},
    Props = #'P_basic'{reply_to = State#state.reply_to, correlation_id = CorrelationId},
    Msg = #amqp_msg{props = Props, payload = Payload},
    amqp_channel:cast(Channel, Publish, Msg),
    lager:debug("Published ~p", [Ref]),
    {next_state, wait_publish_confirm, State}.

wait_publish_confirm(#'basic.return'{reply_text = Reason, exchange = Exchange, routing_key = Key}, State) ->
    lager:debug("RPC message to exchange '~s' with key '~s' lost with reason '~s'", [Exchange, Key, Reason]),
    {next_state, drop_acks, State};
wait_publish_confirm(#'basic.ack'{}, State) ->
    lager:debug("RPC message ack-ed"),
    {next_state, wait_rpc_reply, State};
wait_publish_confirm(#'basic.nack'{}, State) ->
    lager:debug("RPC message nack-ed"),
    {next_state, do_call, State, timeout(State)}.

drop_acks(AMQPAck, State) when ?AMQP_ACK(AMQPAck) ->
    lager:debug("Discarding ~p", [element(1, AMQPAck)]),
    {next_state, do_call, State, timeout(State)}.

wait_rpc_reply({#'basic.deliver'{delivery_tag = Tag}, #amqp_msg{props = #'P_basic'{correlation_id = CorrelationId}}},
               #state{channel = Channel} = State) ->
    Ref = binary_to_term(CorrelationId),
    lager:debug("Got rpc reply to ~p", [Ref]),
    amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}),
    {next_state, do_call, State, timeout(State)}.

%% Helpers
timeout(_State) ->
    5000.
