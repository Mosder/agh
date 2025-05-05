%%%-------------------------------------------------------------------
%% @doc lab04 public API
%% @end
%%%-------------------------------------------------------------------

-module(lab04_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    lab04_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
