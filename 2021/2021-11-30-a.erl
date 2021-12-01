-module('2021-11-30-a').
%-export([add/2, factorial/1]).
-compile(export_all).

add(A, B) ->
    A + B.

factorial(0) ->
    1;
factorial(N) ->
    N * factorial(N - 1).

factorial_tail(N, Acc) when N =< 0 ->
    Acc;
factorial_tail(N, Acc) ->
    factorial_tail(N-1, Acc * N).

greet(male, Name) ->
    io:format("Hello, Mr. ~s~n", [Name]);
greet(female, Name) ->
    io:format("Hello, Ms. ~s~n", [Name]);
greet(_, Name) ->
    io:format("Hello, ~s~n", [Name]).

car([X | _]) ->
    X.

cdr([_ | Tail]) ->
    Tail.

cadr([_, Y | _]) ->
    Y.

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
filter(F, [X | Xs]) ->
    case F(X) of
        true -> [X | filter(F, Xs)];
        _    -> filter(F, Xs)
    end.


merge(Xs, []) ->
    Xs;
merge([], Ys) ->
    Ys;
merge(Left = [X | Xs], Right = [Y | Ys]) ->
    if
        X =< Y ->
            [X | merge(Xs, Right)];
        true ->
            [Y | merge(Left, Ys)]
    end.

merge_sort(L) ->
    spawn(?MODULE, ms_split, [self(), anything, L]),
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

ms_merge(Parent, Side, Ll, Lr) when (Ll == empty) or (Lr == empty) ->
    receive
        {left, LlSorted} ->
            ms_merge(Parent, Side, LlSorted, Lr);
        {right, LrSorted} ->
            ms_merge(Parent, Side, Ll, LrSorted)
    end;
ms_merge(Parent, Side, Ll, Lr) ->
    Parent ! {Side, merge(Ll, Lr)}.


