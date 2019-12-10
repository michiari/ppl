-module(es9ps).
-compile(export_all).

start_broker() ->
    BrokerPid = spawn(?MODULE, broker, []),
    register(broker, BrokerPid).

broker() ->
    Topics = #{erlang => [], haskell => [], scheme => []},
    Messages = #{erlang => [], haskell => [], scheme => []},
    broker_loop(Topics, Messages).

broker_loop(Topics, Messages) ->
    receive
	{subscribe, Topic, Pid} ->
	    io:format("Registering ~p for topic ~w~n", [Pid, Topic]),
	    #{Topic := Pids} = Topics,
	    broker_loop(Topics#{Topic := [Pid | Pids]}, Messages);
	{produce, Topic, Msg} ->
	    io:format("Spreading message ~s for topic ~w~n", [Msg, Topic]),
	    #{Topic := Pids} = Topics,
	    [P ! {publish, Topic, Msg} || P <- Pids],
	    #{Topic := Msgs} = Messages,
	    broker_loop(Topics, Messages#{Topic := [Msg | Msgs]});
	{update, Topic, Pid} ->
	    io:format("Requested repost of topic ~w by process ~p~n",
		      [Topic, Pid]),
	    #{Topic := Msgs} = Messages,
	    repost(Topic, Msgs, Pid),
	    broker_loop(Topics, Messages)
    end.

repost(_, [], _) ->
    ok;
repost(Topic, [Msg | Rest], Pid) ->
    Pid ! {repost, Topic, Msg},
    repost(Topic, Rest, Pid).

consumer(Topics) ->
    [broker ! {subscribe, Topic, self()} || Topic <- Topics],
    consumer_loop(Topics).

consumer_loop(Topics) ->
    receive
	{publish, Topic, Message} ->
	    io:format("Received message ~s for topic ~w~n",
		      [Message, Topic]),
	    consumer_loop(Topics)
    after 10000 ->
	    Chance = rand:uniform(100) =< 5,
	    if
		Chance ->
		    exit("Bye.");
		true ->
		    [broker ! {update, T, self()} || T <- Topics]
	    end,
	    consumer_loop(Topics)
    end.

start_consumers() ->
    [spawn_link(?MODULE, consumer, [[haskell]]) || _ <- [1,2]],
    [spawn_link(?MODULE, consumer, [[erlang]]) || _ <- [1,2]],
    [spawn_link(?MODULE, consumer, [[scheme]]) || _ <- [1,2]],
    handle_exit().

handle_exit() ->
    process_flag(trap_exit, true),
    receive
	{'EXIT', Pid, Msg} ->
	    io:format("Process ~p died with message: ~s~n",
		      [Pid, Msg]),
	    handle_exit()
    end.

producer(Message, Topic) ->
    broker ! {produce, Topic, Message}.

start() ->
    start_broker(),
    spawn(?MODULE, start_consumers, []).
