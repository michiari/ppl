-module('2020-12-03-b').
-export([start/0, produce/2, broker/0, start_consumers/0, consumer/1]).

%% NOTE: compile with c('2020-12-03-b').

start() ->
    start_broker(),
    start_manager().

start_broker() ->
    Broker = spawn(?MODULE, broker, []),
    register(broker, Broker).

broker() ->
    Subscribers = #{erlang => [], haskell => [], scheme => []},
    Messages = #{erlang => [], haskell => [], scheme => []},
    broker_loop(Subscribers, Messages).

broker_loop(Subs, Msgs) ->
    receive
        {subscribe, Topic, Pid} -> % this message is sent by a consumer to subscribe to a topic
            io:format("Registering ~p for topic ~w~n", [Pid, Topic]),
            #{Topic := Pids} = Subs,
            broker_loop(Subs#{Topic := [Pid | Pids]}, Msgs);
        {post, Topic, Msg} -> % this message is sent by a producer to write about a topic
            io:format("Spreading ~s fot topic ~w~n", [Msg, Topic]),
            case Subs of
                #{Topic := Pids} ->
                    [P ! {publish, Topic, Msg} || P <- Pids],
                    #{Topic := TopicMsgs} = Msgs,
                    broker_loop(Subs, Msgs#{Topic := [Msg | TopicMsgs]});
                _ ->
                    io:format("Bad topic~n", []),
                    broker_loop(Subs, Msgs)
            end;
        {update, Topic, Pid} ->
            io:format("Repost requested by ~p for topic ~w~n", [Pid, Topic]),
            #{Topic := TopicMsgs} = Msgs,
            [Pid ! {repost, Topic, M} || M <- TopicMsgs],
            broker_loop(Subs, Msgs)
    end.

start_manager() ->
    spawn(?MODULE, start_consumers, []).

start_consumers() ->
    [spawn_link(?MODULE, consumer, [[haskell]]) || _ <- lists:seq(1,3)],
    [spawn_link(?MODULE, consumer, [[scheme]]) || _ <- lists:seq(1,3)],
    [spawn_link(?MODULE, consumer, [[erlang]]) || _ <- lists:seq(1,3)],
    process_flag(trap_exit, true),
    handle_errors().

handle_errors() ->
    receive
        {'EXIT', Pid, Msg} ->
            io:format("Process ~p died with message: ~s~n", [Pid, Msg]),
            handle_errors()
    end.

consumer(Topics) ->
    [broker ! {subscribe, T, self()} || T <- Topics],
    consumer_loop(Topics).

consumer_loop(Topics) ->
    receive
        {publish, Topic, Message} -> % this message is sent by the broker when a new message arrives
            io:format("Received message ~s for topic ~w~n", [Message, Topic]),
            consumer_loop(Topics);
        {repost, Topic, Message} ->
            io:format("Received repost ~s for topic ~w~n", [Message, Topic]),
            consumer_loop(Topics)
    after 10000 ->
            Chance = rand:uniform(100) < 20,
            if
                Chance ->
                    exit("Bye.");
                true ->
                    [broker ! {update, T, self()} || T <- Topics]
            end,
            consumer_loop(Topics)
    end.

produce(Topic, Msg) ->
    broker ! {post, Topic, Msg}.
