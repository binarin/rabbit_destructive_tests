-module(mos_like_rpc_server).

-behaviour(gen_server).
-compile([export_all]).
-compile([{parse_transform, lager_transform}]).

-include_lib("amqp_client/include/amqp_client.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {connection
               ,sut
               ,exchange_name
               ,queue_name
               ,channel
               }).

start_link(Sut) ->
    gen_server:start_link(?MODULE, [Sut], []).

init([Sut]) ->
    self() ! init,
    {ok, #state{sut = Sut
               ,exchange_name = <<"ha-rpc-exchange">>
               ,queue_name = <<"ha-rpc-queue">>
               }}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(init, State) ->
    State1 = connect(State),
    State2 = declare_objects(State1),
    State3 = start_consumer(State2),
    {noreply, State3};
handle_info({#'basic.deliver'{delivery_tag = Tag},
             #amqp_msg{props = #'P_basic'{reply_to = ReplyTo, correlation_id = CorrelationId}}},
            #state{channel = Channel} = State) ->
    Payload = <<"hello, my little friend">>,
    Props = #'P_basic'{correlation_id = CorrelationId},
    ReplyMsg = #amqp_msg{props = Props, payload = Payload},
    Publish = #'basic.publish'{exchange = <<"">>, routing_key = ReplyTo},
    lager:debug("Replying to ~s", [ReplyTo]),
    amqp_channel:cast(Channel, Publish, ReplyMsg),
    amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}),
    {noreply, State};
handle_info(Info, State) ->
    lager:debug("Got message ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%%%%%%%%
%% Helpers %%
%%%%%%%%%%%%%
connect(#state{sut = Sut} = State) ->
    {ok, Connection, Channel} = sut:amqp_connect(Sut),
    State#state{connection = Connection, channel = Channel}.

declare_objects(#state{channel = Channel, queue_name = Queue, exchange_name = Exchange} = State) ->
    declare_queue(Channel, Queue),
    declare_exchange(Channel, Exchange),
    #'queue.bind_ok'{} = amqp_channel:call(Channel, #'queue.bind'{queue = Queue, exchange = Exchange}),
    State.

declare_queue(Channel, Queue) ->
    Declare = #'queue.declare'{queue = Queue
                              ,auto_delete = false
                              },
    #'queue.declare_ok'{} = amqp_channel:call(Channel, Declare).

declare_exchange(Channel, Exchange) ->
    Declare = #'exchange.declare'{exchange = Exchange, type = <<"fanout">>},
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, Declare).

start_consumer(#state{channel = Channel, queue_name = Queue} = State) ->
    #'basic.consume_ok'{} = amqp_channel:call(Channel, #'basic.consume'{queue = Queue}),
    receive
        #'basic.consume_ok'{} ->
            ok
    after
        1000 -> exit(no_consume_ok)
    end,
    State.
