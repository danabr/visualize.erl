-module(sum).

-export([sum/1, generate/1]).

-include("include/analyze.hrl").

sum(List) when is_list(List) ->
    Self = self(),
    spawn(fun() -> sum(List, 0, 0, Self) end),
    receive
        {sum, Sum} -> Sum
    end.

sum([], ToWait, Acc, Parent) ->
    wait(ToWait, Acc, Parent);
sum([X|Xs], ToWait, Acc, Parent) when is_list(X) ->
    Self = self(),
    spawn(fun() -> sum(X, 0, 0, Self) end),
    sum(Xs, ToWait+1, Acc, Parent);
sum([X|Xs], ToWait, Acc, Parent) ->
    sum(Xs, ToWait, X+Acc, Parent).

wait(0, Sum, Parent) ->
    Parent ! {sum, Sum};
wait(N, Acc, Parent) ->
    receive
        {sum, PartSum} ->
            wait(N-1, Acc+PartSum, Parent)
    end.

% Generate a list of max N elements
generate(0) -> [];
generate(N) ->
    Nested = random:uniform(100),
    if
        Nested < 10 ->
            [generate(random:uniform(N))|generate(N-1)];
        true ->
            [random:uniform(N)|generate(N-1)]
    end.
