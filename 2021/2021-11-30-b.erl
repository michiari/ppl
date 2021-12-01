-module('2021-11-30-b').
-export([start/0, broker/0, producer/2, consumer/1, start_consumers/0]).

start() ->
    start_broker(),
    spawn(?MODULE, start_consumers, []),
    ok.

start_broker() ->
    Broker = spawn(?MODULE, broker, []),
    register(broker, Broker).

start_consumers() ->
    [spawn_link(?MODULE, consumer, [[Topic]]) || Topic <- [erlang, haskell, scheme]],
    handle_errors().

handle_errors() ->
    process_flag(trap_exit, true),
    receive
        {'EXIT', Pid, Msg} ->
            io:format("Process ~p died with message: ~s~n", [Pid, Msg]),
            handle_errors()
    end.

broker() ->
    Topics = #{ erlang => [], haskell => [], scheme => [] },
    Messages = #{ erlang => [], haskell => [], scheme => [] },
    broker_loop(Topics, Messages).

broker_loop(Topics, Messages) ->
    receive
        {subscribe, Topic, Pid} ->
            io:format("Registering ~p for topic ~w.~n", [Pid, Topic]),
            #{Topic := Pids} = Topics,
            broker_loop(Topics#{Topic := [Pid | Pids]}, Messages);
        {produce, Topic, Msg} ->
            io:format("Spreading ~s for topic ~w.~n", [Msg, Topic]),
            #{Topic := Pids} = Topics,
            #{Topic := Msgs} = Messages,
            [P ! {publish, Topic, Msg} || P <- Pids],
            broker_loop(Topics, Messages#{Topic := [Msg | Msgs]})
    end.

consumer(Ts) ->
    [broker ! {subscribe, X, self()} || X <- Ts],
    consumer_loop(Ts).

consumer_loop(Topics) ->
    receive
        {publish, Topic, Message} ->
            io:format("Received new messege ~s about ~w~n", [Message, Topic]),
            consumer_loop(Topics)
    after 10000 ->
            Chance = rand:uniform(100) < 50,
            if
                Chance ->
                    exit("Bye.");
                true ->
                    consumer_loop(Topics)
            end
    end.

producer(Message, Topic) ->
    broker ! {produce, Topic, Message},
    ok.

% Change the program so that every 10,000 ms consumers can send to the broker
% a message of the form
% {update, Topic, self()}
% for each one of the topics they are subscribed to.
% When receiving it, the broker re-sends to the consumer all messages
% it previously received about that topic, with messages of the form
% {repost, Topic, Message}
