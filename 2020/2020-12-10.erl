-module('2020-12-10').
-export([start_ring/1, ring_node/1, do_ringy_thingy/0, leafy/1, branchy/5, test/0, dlist/2, testdl/0]).

%% Ring topology
start_ring(N) ->
    Pids = start_ring_node(N),
    First = hd(Pids),
    First ! {init, First},
    Pids.

start_ring_node(1) ->
    NodePid = spawn(?MODULE, ring_node, [last]),
    [NodePid];
start_ring_node(N) ->
    [NextPid | Rest] = start_ring_node(N-1),
    NodePid = spawn(?MODULE, ring_node, [NextPid]),
    [NodePid, NextPid | Rest].

ring_node(last) ->
    receive
        {init, First} ->
            io:format("First PID received: ~p~n", [First]),
            ring_node_loop(First)
    end;
ring_node(NextPid) ->
    receive
        {init, First} ->
            NextPid ! {init, First},
            io:format("Forwarding first PID.~n", []),
            ring_node_loop(NextPid)
    end.

ring_node_loop(NextPid) ->
    receive
        {roundtrip, Msg} ->
            io:format("~p: starting round trip for message ~s~n", [self(), Msg]),
            NextPid ! {roundtrip, self(), Msg},
            ring_node_loop(NextPid);
        {roundtrip, TargetPid, Msg} when TargetPid =:= self() ->
            io:format("~p: round trip for message ~s finished.~n", [self(), Msg]),
            ring_node_loop(NextPid);
        {roundtrip, TargetPid, Msg} ->
            io:format("~p: continuing round trip of message ~s~n", [self(), Msg]),
            NextPid ! {roundtrip, TargetPid, Msg},
            ring_node_loop(NextPid);
        {nextn, 0, Msg} ->
            io:format("~p: message received ~s.~n", [self(), Msg]),
            ring_node_loop(NextPid);
        {nextn, N, Msg} ->
            io:format("~p: sending message ~s to the ~wth subsequent node.~n", [self(), Msg, N]),
            NextPid ! {nextn, N-1, Msg},
            ring_node_loop(NextPid);
        die ->
            io:format("~p: Bye.~n", [self()]),
            NextPid ! {notifydeath, self(), NextPid};
        {notifydeath, NextPid, NewNextPid} ->
            io:format("~p: RIP ~p.~n", [self(), NextPid]),
            ring_node_loop(NewNextPid);
        {notifydeath, DeadPid, DeadNextPid} ->
            io:format("~p: forwarding death notice.~n", [self()]),
            NextPid ! {notifydeath, DeadPid, DeadNextPid},
            ring_node_loop(NextPid)
    end.

do_ringy_thingy() ->
    Nodes = start_ring(5),
    io:format("~w~n", [Nodes]),
    hd(Nodes) ! {roundtrip, "Round!"},
    lists:nth(3, Nodes) ! {nextn, 10, "Ten!"},
    timer:sleep(5000),
    lists:nth(4, Nodes) ! die,
    timer:sleep(5000),
    hd(Nodes) ! {roundtrip, "Round!"},
    ok.

% Exam 2017-07-05, exercise 3
activate({leaf, V}, _) ->
    io:format("spawning leaf ~w~n", [V]),
    spawn(?MODULE, leafy, [V]);
activate({branch, L, R}, F) ->
    io:format("spawning node~n", []),
    Tl = activate(L, F),
    Tr = activate(R, F),
    spawn(?MODULE, branchy, [F, Tl, Tr, none, false]).

leafy(V) ->
    receive
        {ask, P} ->
            io:format("Sending value ~w to ~p~n", [V, P]),
            P ! {self(), V}
    end.

branchy(F, L, R, Parent, Go) ->
    receive
        {ask, P} ->
            io:format("forwarding to leaves ~p and ~p~n", [L, R]),
            L ! {ask, self()},
            R ! {ask, self()},
            branchy(F, L, R, P, false);
        {L, V} ->
            case Go of
                false ->
                    branchy(F, L, R, Parent, V);
                _ ->
                    Parent ! {self(), F(V, Go)}
            end;
        {R, V} ->
            case Go of
                false ->
                    branchy(F, L, R, Parent, V);
                _ ->
                    Parent ! {self(), F(Go, V)}
            end
    end.

test() ->
    T = {branch, {branch, {leaf, 1}, {leaf, 2}}, {leaf, 3}},
    A = activate(T, fun min/2),%%fun(X, Y) -> X+Y end),
    A ! {ask, self()},
    receive
        {A, V} ->
            io:format("value ~w~n", [V])
    end.


% Exam 2017-07-20, exercise 3
create_dlist(0) ->
    [];
create_dlist(N) when N > 0 ->
    [spawn(?MODULE, dlist, [I, 0]) || I <- lists:seq(1, N)].

dlist(I, V) ->
    receive
        {set, NewV} ->
            dlist(I, NewV);
        {get, P} ->
            P ! V,
            dlist(I, V)
    end.

get(I, Pids) ->
    lists:nth(I, Pids) ! {get, self()},
    receive
        V -> V
    end.

set(I, Pids, V) ->
    lists:nth(I, Pids) ! {set, V}.

dlist_to_list([]) ->
    [];
dlist_to_list(D) ->
    d2l(D, []).


d2l([], L) -> L;
d2l([P | Ps], L) ->
    P ! {get, self()},
    receive
        V -> d2l(Ps, L ++ [V])
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

dfoldl(_, [], Acc) ->
    Acc;
dfoldl(F, [P | Ps], Acc) ->
    P ! {get, self()},
    receive
        V ->
            dfoldl(F, Ps, F(Acc, V))
    end.


testdl() ->
    L = create_dlist(10),
    [set(N, L, N) || N <- lists:seq(1,10)],
    List = dlist_to_list(L),
    io:format("~w~n", [List]),
    dmap(L, fun(X) -> X+1 end),
    io:format("~w~n", [dlist_to_list(L)]),
    io:format("~w~n", [dfoldl(fun(X, Y) -> X + Y end, L, 0)]),
    ok.
