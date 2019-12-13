-module('2019-12-13').
-compile(export_all).

% Make a ring of linked nodes
start_ring(N) ->
    Pids = start_ring_node(N),
    First = hd(Pids),
    First ! {init, First},
    Pids.

start_ring_node(0) ->
    NodePid = spawn(?MODULE, ring_node, [last]),
    [NodePid];
start_ring_node(N) ->
    [NextPid | Rest] = start_ring_node(N-1),
    NodePid = spawn(?MODULE, ring_node, [NextPid]),
    [NodePid, NextPid | Rest].

% Close the ring by sending the PID of the first node all the way to the last node.
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

% Implement the following message handlers:
% - Round trip from any node;
% - Send message from a node to the one N positions ahead;
% - Make a node terminate, and fix the ring by restoring the link
%   between the previous node and the next one.
ring_node_loop(NextPid) ->
    receive
	{roundtrip, Msg} ->
	    io:format("~p: Starting round trip for message ~s.~n", [self(), Msg]),
	    NextPid ! {roundtrip, self(), Msg},
	    ring_node_loop(NextPid);
	{roundtrip, TargetPid, Msg} when TargetPid == self() ->
	    io:format("~p: End of round trip for message ~s.~n", [self(), Msg]),
	    ring_node_loop(NextPid);
	{roundtrip, TargetPid, Msg} ->
	    io:format("~p: Continuing round trip of message ~s.~n", [self(), Msg]),
	    NextPid ! {roundtrip, TargetPid, Msg},
	    ring_node_loop(NextPid);
	{nextn, 0, Msg} ->
	    io:format("~p: Message received: ~s.~n", [self(), Msg]),
	    ring_node_loop(NextPid);
	{nextn, N, Msg} ->
	    io:format("~p: Sending message ~s to ~wth subsequent node.~n", [self(), Msg, N]),
	    NextPid ! {nextn, N-1, Msg},
	    ring_node_loop(NextPid);
	{die} ->
	    io:format("~p: Bye.~n", [self()]),
	    NextPid ! {notifydeath, self(), NextPid};
	{notifydeath, NextPid, NewNextPid} ->
	    io:format("~p: RIP ~p.~n", [self(), NextPid]),
	    ring_node_loop(NewNextPid);
	{notifydeath, DeadPid, NewNextPid} ->
	    NextPid ! {notifydeath, DeadPid, NewNextPid},
	    ring_node_loop(NextPid)
    end.

do_ringy_thingy() ->
    Nodes = start_ring(10),
    io:format("~w~n", [Nodes]),
    hd(Nodes) ! {roundtrip, "Giro!"},
    lists:nth(3, Nodes) ! {nextn, 15, "Quindici!"},
    timer:sleep(5000),
    lists:nth(6, Nodes) ! {die},
    timer:sleep(5000),
    hd(Nodes) ! {roundtrip, "Giro!"},
    ok.


% Exam 2017-07-05
% Define an **activate** function, which takes as input:

% a binary tree stored with tuples, e.g. {branch, {branch, {leaf, 1}, {leaf, 2}}, {leaf, 3}}.
% a binary function F
% then **activate** creates a network of actors having the same structure of the given tree.

% Actors corresponding to leaves run a function called **leafy**,
% and answer messages {ask, P} where P is a process, with
% the pair {self(), V}, where V is the value stored in the leaf, then terminate.

% Actors for the branches run a function called **branchy**,
% if a branch actor receives an {ask, P} request it forward it to both its leaves, and
% when a branch actor receives the answers, they call
% F on the obtained values, then send the result V to P as
% {self(), V} and terminate.

% E.g. running the following code:
% test() ->
%  T1 = {branch,
%		{branch,
%			{leaf, 1},
%				{leaf, 2}},
%		{leaf, 3}},
%  A1 = activate(T1, fun min/2),
%  A1 ! {ask, self()},
%  receive
%  {A1, V} -> V
%  end.
% should return 1.


activate({leaf, V}, _) ->
    io:format("~p: Spawning a leaf ~w~n", [self(), V]),
    spawn(?MODULE, leafy, [V]);
activate({branch, L, R}, F) ->
    io:format("~p: Spawning a branch~n", [self()]),
    Tl = activate(L, F),
    Tr = activate(R, F),
    spawn(?MODULE, branchy, [F, Tl, Tr, none, false, false]).

leafy(V) ->
    receive
	{ask, P} ->
	    io:format("~p: Sending leaf value ~w to ~p.~n", [self(), V, P]),
	    P ! {self(), V};
	 M ->
	    error("Protocol error: ~w~n", [M])
    end.

branchy(F, L, R, none, _, _) ->
    receive
	{ask, P} ->
	    io:format("~p: Farwarding to leaves ~p ~p~n", [self(), L, R]),
	    L ! {ask, self()},
	    R ! {ask, self()},
	    branchy(F, L, R, wait, false, P);
	 M ->
	    error("Protocol error: ~w~n", [M])
    end;
branchy(F, L, R, wait, _, P) ->
    receive
	{L, V} ->
	    branchy(F, L, R, recl, V, P);
	{R, V} ->
	    branchy(F, L, R, recr, V, P);
	 M ->
	    error("Protocol error: ~w~n", [M])
    end;
branchy(F, _, R, recl, Vl, P) ->
    receive
	{R, Vr} ->
	    P ! {self(), F(Vl, Vr)};
       	 M ->
	    error("Protocol error: ~w~n", [M])
    end;
branchy(F, L, _, recr, Vr, P) ->
    receive
	{L, Vl} ->
	    P ! {self(), F(Vl, Vr)};
	M ->
	    error("Protocol error: ~w~n", [M])
end.

main() ->
    T1 = {branch, {branch, {leaf, 1}, {leaf, 2}}, {leaf, 3}},
    A = activate(T1, fun min/2),
    io:format("Asking ~p~n", [A]),
    A ! {ask, self()},
    receive
	{A, V} ->
	    io:format("The value is ~w~n", [V])
    end.


% Exam 2017-07-20
% We want to define a “dynamic list” data structure, where
% each element of the list is an actor storing a value.
% Such value can be read and set in parallel.

% 1) Define create_dlist, which takes a number n and
%    returns a dynamic list of length n initialized to 0.
% 2) Define the function dlist_to_list,
%    which takes a dynamic list and returns a list
%    of the contained values.
% 3) Define a map for dynamic list.
%    Of course this operation has side effects,
%    since it changes the content of the list.

create_dlist(0) ->
    [];
create_dlist(N) ->
    [spawn(?MODULE, dlist, [I, 0]) || I <- lists:seq(1, N)].

dlist(I, V) ->
    receive
	{set, NewV} ->
	    dlist(I, NewV);
	{get, P} ->
	    P ! V,
	    dlist(I, V)
    end.

set(I, L, V) ->
    lists:nth(I+1, L) ! {set, V}.

get(I, L) ->
    lists:nth(I+1, L) ! {get, self()},
    receive
	V ->
	    V
    end.


dlist_to_list([]) ->
    [];
dlist_to_list(Ps) ->
    d2l(Ps, []).


d2l([], L) ->
    L;
d2l([P | Ps], L) ->
    P ! {get, self()},
    receive
	V ->
	    d2l(Ps, L ++ [V])
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

dfoldl([], Acc, _) ->
    Acc;
dfoldl([P | Ps], Acc, F) ->
    P ! {get, self()},
    receive
	V ->
	    dfoldl(Ps, F(Acc, V), F)
    end.


main2() ->
    L = create_dlist(10),
    [set(N, L, N) || N <- lists:seq(0,9)],
    LL = dlist_to_list(L),
    io:format("List: ~w~n", [LL]),
    dmap(L, fun(X) -> X*2 end),
    L2 = dlist_to_list(L),
    io:format("List: ~w~n", [L2]),
io:format("Sum: ~w~n", [dfoldl(L, 0, fun(X, Y) -> X+Y end)]),
    ok.
