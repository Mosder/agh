%%%-------------------------------------------------------------------
%%% @author mosder
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Apr 2025 15:12
%%%-------------------------------------------------------------------
-module(pingpong).
-author("mosder").

%% API
-export([start/0, stop/0, play/1, ping_loop/1, pong_loop/0]).

start() ->
  register(ping, spawn(fun() -> ping_loop() end)),
  register(pong, spawn(fun() -> pong_loop() end)).

ping_loop() -> ping_loop(0).
ping_loop(Sum) ->
  timer:sleep(100),
  receive
    stop -> ok;
    0 -> io:format("ping: 0 sum: ~w~n", [Sum]), ping_loop(Sum);
    N -> io:format("ping: ~w sum: ~w~n", [N, Sum+N]), pong ! N-1, ping_loop(Sum+N)
  after 20000 -> ok
  end.

pong_loop() ->
  timer:sleep(100),
  receive
    stop -> ok;
    0 -> io:format("pong: 0~n"), pong_loop();
    N -> io:format("pong: ~w~n", [N]), ping ! N-1, pong_loop()
  after 20000 -> ok
  end.

stop() -> ping ! stop, pong ! stop, ok.

play(N) -> ping ! N, ok.
