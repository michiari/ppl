-module('2020-12-03-a').
%-export([add/2, factorial/1, factorial/2]).
-compile(export_all).

%% NOTE: compile with c('2020-12-03-a').

add(A, B) ->
    A + B.

factorial(0) ->
    1;
factorial(N) ->
    N * factorial(N-1).

factorial(N, Acc) when N =< 0 ->
    Acc;
factorial(N, Acc) ->
    factorial(N-1, N * Acc).

greet(male, Name) ->
    io:format("Hello, Mr. ~s~n", [Name]);
greet(female, Name) ->
    io:format("Hello, Ms. ~s~n", [Name]);
greet(_, Name) ->
    io:format("Hello, ~s~n", [Name]).

%% Lists
car([X | _]) ->
    X.
cdr([_ | Xs]) ->
    Xs.
caar([_, X | _]) ->
    X.
%% Also hd and tl

%% Higher order functions
map(_, []) ->
    [];
map(F, [X | Xs]) ->
    [F(X) | map(F, Xs)].

foldr(_, Acc, []) ->
    Acc;
foldr(F, Acc, [X | Xs]) ->
    F(X, foldr(F, Acc, Xs)).
% prepared:foldr(fun(X,Acc) -> X * Acc end, 1, [1,2,3,4,5]).

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
%% prepared:filter(fun(X) -> X > 5 end, [1,5,10,6,7,a]).


%% Parallel merge sort
merge([], Xs) ->
    Xs;
merge(Xs, []) ->
    Xs;
merge([X | Xs] = Fst, [Y | Ys] = Snd) ->
    if
        X =< Y ->
            [X | merge(Xs, Snd)];
        true ->
            [Y | merge(Fst, Ys)]
    end.

ms_split(Parent, []) ->
    Parent ! [];
ms_split(Parent, [X]) ->
    Parent ! [X];
ms_split(Parent, L) ->
    {Ll, Lr} = lists:split(length(L) div 2, L),
    spawn(?MODULE, ms_split, [self(), Ll]),
    spawn(?MODULE, ms_split, [self(), Lr]),
    ms_merge(Parent, empty, empty).

ms_merge(Parent, empty, empty) ->
    receive
        Ll ->
            ms_merge(Parent, Ll, empty)
    end;
ms_merge(Parent, Ll, empty) ->
    receive
        Lr ->
            ms_merge(Parent, Ll, Lr)
    end;
ms_merge(Parent, Ll, Lr) ->
    Parent ! merge(Ll, Lr).

merge_sort(L) ->
    spawn(?MODULE, ms_split, [self(), L]),
    receive
        LSorted ->
            LSorted
    end.

% prepared:merge_sort([rand:uniform(100) || _ <- lists:seq(1, 20)]).
