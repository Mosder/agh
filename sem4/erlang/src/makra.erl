-module(makra).

-export([factorial/1]).

factorial(N) when N =< 0 -> 1;
factorial(N) -> N*factorial(N-1).