-module(functions).

-export([power/2]).

factorial(0) -> 1;
factorial(N) when is_integer(N), N > 0 ->
  N * factorial(N-1).

exp_taylor(_X, 0) -> 1;
exp_taylor(X, K) when is_integer(K), K > 0 ->
  (power(X, K) / factorial(K)) + exp_taylor(X, K-1).

log_taylor(_X, -1) -> 0;
log_taylor(X, K) when is_integer(K), K >= 0 ->
  ((2/(2*K + 1)) * power((X-1)/(X+1), 2*K + 1)) + log_taylor(X, K-1).

power(Base, Exponent) when (not is_number(Base)) or (not is_number(Exponent)) ->
  "base and exponent must be numbers~n";
power(Base, Exponent) when Base < 0, not is_integer(Exponent) ->
  "non integer exponents are not supported for negative base~n";
power(_Base, 0) -> 1;
power(Base, Exponent) when is_integer(Exponent), Exponent > 0 ->
  Base * power(Base, Exponent - 1);
power(Base, Exponent) when Exponent > 0 ->
  power(Base, trunc(Exponent)) * exp_taylor((Exponent-trunc(Exponent)) * log_taylor(Base, 100), 100);
power(Base, Exponent) when Exponent < 0 ->
  1 / power(Base, -Exponent).
