-module(myLists).

-export([contains/2, duplicate_elements/1, sum_floats/1]).

contains([], _Element) -> false;
contains([H | _T], H) -> true;
contains([_H | T], Element) -> contains(T, Element).

duplicate_elements([]) -> [];
duplicate_elements([H | T]) -> [H, H | duplicate_elements(T)].

sum_floats(List) -> sum_floats(List, 0).

sum_floats([], Acc) -> Acc;
sum_floats([H | T], Acc) when is_float(H) -> sum_floats(T, H + Acc);
sum_floats([_H | T], Acc) -> sum_floats(T, Acc).
