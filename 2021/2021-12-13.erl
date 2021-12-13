-module('2021-12-13').
-compile(export_all).

start_ring(N) ->
    Pids = start_ring_nodes(N),
    First = hd(Pids),
    First ! {init, First},
    Pids.

start_ring_nodes(0) ->
    [];
start_ring_nodes(1) ->
    NodePid = spawn(?MODULE, ring_node, [last]),
    [NodePid];
start_ring_nodes(N) ->
    [NextPid | Rest] = start_ring_nodes(N-1),
    NodePid = spawn(?MODULE, ring_node, [NextPid]),
    [NodePid, NextPid | Rest].

ring_node(last) ->
    receive
        {init, First} ->
            ring_node_loop(First)
    end;
ring_node(NextPid) ->
    receive
        {init, First} ->
            NextPid ! {init, First},
            ring_node_loop(NextPid)
    end.

ring_node_loop(NextPid) ->
    receive
        {roundtrip, Msg} ->
            io:format("~p: Starting round-trip for message ~s~n", [self(), Msg]),
            NextPid ! {roundtrip, Msg, self()},
            ring_node_loop(NextPid);
        {roundtrip, Msg, StartPid} when StartPid =:= self() ->
            io:format("~p: Round trip finished for message ~s~n", [self(), Msg]),
            ring_node_loop(NextPid);
        {roundtrip, Msg, StartPid} ->
            io:format("~p: Continuing round trip for message ~s~n", [self(), Msg]),
            NextPid ! {roundtrip, Msg, StartPid},
            ring_node_loop(NextPid);
        {nextn, 0, Msg} ->
            io:format("~p: Message received: ~s~n", [self(), Msg]),
            ring_node_loop(NextPid);
        {nextn, N, Msg} ->
            io:format("~p: Sending message ~s to ~wth next node~n", [self(), Msg, N]),
            NextPid ! {nextn, N-1, Msg},
            ring_node_loop(NextPid);
        die ->
            io:format("~p: Bye.~n", [self()]),
            NextPid ! {notifydeath, NextPid, self()};
        {notifydeath, NewNextPid, NextPid} ->
            io:format("~p: RIP ~p~n", [self(), NextPid]),
            ring_node_loop(NewNextPid);
        {notifydeath, NewNextPid, DeadPid} ->
            NextPid ! {notifydeath, NewNextPid, DeadPid},
            ring_node_loop(NextPid)
    end.

do_ringy_thingy() ->
    Nodes = start_ring(5),
    io:format("~w~n", [Nodes]),
    hd(Nodes) ! {roundtrip, "Trip!"},
    timer:sleep(5000),
    lists:nth(4, Nodes) ! {nextn, 7, "You will die."},
    timer:sleep(5000),
    lists:nth(2, Nodes) ! die,
    timer:sleep(1000),
    hd(Nodes) ! {roundtrip, "Trip!"},
    ok.


%% 2017-07-05, Exercise 3
activate({leaf, V}, _) ->
    io:format("Spawning leaf ~w~n", [V]),
    spawn(?MODULE, leafy, [V]);
activate({branch, L, R}, F) ->
    io:format("Spawning internal node~n", []),
    LPid = activate(L, F),
    RPid = activate(R, F),
    spawn(?MODULE, branchy, [F, LPid, RPid, false, none, none]).

leafy(V) ->
    receive
        {ask, P} ->
            io:format("Sending the leaf value ~w~n", [V]),
            P ! {self(), V}
    end.

branchy(F, L, R, Go, ChildValue, Sender) ->
    receive
        {ask, P} ->
            io:format("Forwarding to children~n", []),
            L ! {ask, self()},
            R ! {ask, self()},
            branchy(F, L, R, false, none, P);
        {L, V} ->
            case Go of
                true ->
                    Sender ! {self(), F(V, ChildValue)};
                false ->
                    branchy(F, L, R, true, V, Sender)
            end;
        {R, V} ->
            case Go of
                true ->
                    Sender ! {self(), F(ChildValue, V)};
                false ->
                    branchy(F, L, R, true, V, Sender)
            end
    end.

test() ->
    T1 = {branch, {branch, {leaf, 1}, {leaf, 2}}, {leaf, 3}},
    A1 = activate(T1, fun(X, Y) -> X + Y end), % fun min/2
    A1 ! {ask, self()},
    receive
        {A1, V} -> V
    end.


%% 2017/07/20, Exercise 3

create_dlist(0) ->
    [];
create_dlist(N) ->
    [spawn(?MODULE, dlist, [I, 0]) || I <- lists:seq(1, N)].

set(I, Pids, V) ->
    lists:nth(I+1, Pids) ! {set, V}.

get(I, Pids) ->
    lists:nth(I+1, Pids) ! {get, self()},
    receive
        V -> V
    end.

dlist(I, V) ->
    receive
        {set, NewV} ->
            dlist(I, NewV);
        {get, P} ->
            P ! V,
            dlist(I, V)
    end.

dlist_to_list(DList) ->
    d2l(DList, []).

d2l([], L) ->
    L;
d2l([P | Ps], Acc) ->
    P ! {get, self()},
    receive
        V -> d2l(Ps, Acc ++ [V])
    end.

dmap([], _) ->
    ok;
dmap([P | Ps], F) ->
    P ! {get, self()},
    receive
        V ->
            P ! {set, F(V)},
            dmap(Ps, F)
    end.

test2() ->
    DList = create_dlist(10),
    [set(I, DList, I) || I <- lists:seq(0, 9)],
    io:format("~w~n", [dlist_to_list(DList)]),
    dmap(DList, fun(X) -> X + 1 end),
    dlist_to_list(DList).

%% Define a dfoldl function that does a left fold on a dynamic list.
