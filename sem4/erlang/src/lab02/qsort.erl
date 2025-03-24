-module(qsort).

-export([qs/1, random_elems/3, compare_speeds/3]).

less_than(List, Arg) -> [X || X <- List, X < Arg].

grt_eq_than(List, Arg) -> [X || X <- List, X >= Arg].

qs([]) -> [];
qs([Pivot | Tail]) -> qs(less_than(Tail, Pivot)) ++ [Pivot] ++ qs(grt_eq_than(Tail, Pivot)).

random_elems(N, Min, Max) -> [Min - 1 + rand:uniform(Max-Min+1) || _ <- lists:seq(1, N)].

compare_speeds(List, Fun1, Fun2) ->
  io:format("~w: ~wms~n", [Fun1, element(1, timer:tc(Fun1, [List]))/1000]),
  io:format("~w: ~wm~n", [Fun2, element(1, timer:tc(Fun2, [List]))/1000]).