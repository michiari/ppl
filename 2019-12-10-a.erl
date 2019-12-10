-module(es9).
%-export([add/2, factorial/1, factorial/2]).
-compile(export_all).

add(A, B) ->
    A + B.

factorial(0) ->
    1;
factorial(N) ->
    N * factorial(N-1).

factorial(N, Acc) when N =< 0 ->
    Acc;
factorial(N, Acc) ->
    factorial(N-1, N*Acc).

greet(male, Name) ->
    io:format("Hello, Mr. ~s~n", [Name]);
greet(female, Name) ->
    io:format("Hello, Ms. ~s~n", [Name]);
greet(_, Name) ->
    io:format("Hello, ~s~n", [Name]).

car([X | _]) ->
    X.

ht([X | T]) ->
    io:format("Head: ~w~nTail: ~w~n", [X, T]).

caar([_, X2 | _]) ->
    X2.

map(_, []) ->
    [];
map(F, [X | Xs]) ->
    [F(X) | map(F, Xs)].

foldr(_, Acc, []) ->
    Acc;
foldr(F, Acc, [X | Xs]) ->
    F(X, foldr(F, Acc, Xs)).

foldl(_, Acc, []) ->
    Acc;
foldl(F, Acc, [X | Xs]) ->
    foldl(F, F(Acc, X), Xs).

filter(_, []) ->
    [];
filter(P, [X | Xs]) ->
    case P(X) of
	true ->
	    [X | filter(P, Xs)];
	false ->
	    filter(P, Xs)
    end.

merge([], L) ->
    L;
merge(L, []) ->
    L;
merge([X | Xs], [Y | Ys]) ->
    if
	X =< Y ->
	    [X | merge(Xs, [Y | Ys])];
	true ->
	    [Y | merge([X | Xs], Ys)]
    end.


merge_sort(L) ->
    spawn(?MODULE, ms_split, [self(), left, L]),
    receive
	{_, LSorted} ->
	    LSorted
    end.

ms_split(Parent, Side, []) ->
    Parent ! {Side, []};
ms_split(Parent, Side, [X]) ->
    Parent ! {Side, [X]};
ms_split(Parent, Side, L) ->
    {Ll, Lr} = lists:split(length(L) div 2, L),
    spawn(?MODULE, ms_split, [self(), left, Ll]),
    spawn(?MODULE, ms_split, [self(), right, Lr]),
    ms_merge(Parent, Side, empty, empty).

ms_merge(Parent, Side, Ll, Lr)
  when (Ll == empty) or (Lr == empty) ->
    receive
	{left, LlSorted} ->
	    ms_merge(Parent, Side, LlSorted, Lr);
	{right, LrSorted} ->
	    ms_merge(Parent, Side, Ll, LrSorted)
    end;
ms_merge(Parent, Side, Ll, Lr) ->
    Parent ! {Side, merge(Ll, Lr)}.
