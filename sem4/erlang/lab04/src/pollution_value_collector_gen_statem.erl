%%%-------------------------------------------------------------------
%%% @author mosder
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. May 2025 20:00
%%%-------------------------------------------------------------------
-module(pollution_value_collector_gen_statem).
-author("mosder").

-behaviour(gen_statem).

%% API
-export([start_link/0, stop/0, set_station/1, add_value/3, store_data/0]).

%% gen_statem callbacks
-export([init/1, terminate/3, callback_mode/0, unselected/3, selected/3, store_ready/3]).

-define(SERVER, ?MODULE).

-record(state, {station, values=[]}).
%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
start_link() ->
  gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init([]) ->
  {ok, unselected, []}.

unselected(_Event, {set_station, ID}, _State) ->
  {next_state, selected, #state{station=ID}}.

selected(_Event, {add_value, DateTime, Type, Value}, State) ->
  {next_state, store_ready, #state{station=State#state.station, values=[{DateTime, Type, Value}]}}.

store_ready(_Event, {add_value, DateTime, Type, Value}, State) ->
  {next_state, store_ready, #state{station=State#state.station, values=State#state.values++[{DateTime, Type, Value}]}};
store_ready(_Event, store_data, State) ->
  handle_store(State#state.station, State#state.values),
  {next_state, unselected, []}.

%% @private
%% @doc This function is called by a gen_statem when it needs to find out
%% the callback mode of the callback module.
callback_mode() -> state_functions.

%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _StateName, _State) -> ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
stop() -> gen_statem:stop(pollution_value_collector_gen_statem).

set_station(ID) ->
  gen_statem:cast(pollution_value_collector_gen_statem, {set_station, ID}).

add_value(DateTime, Type, Value) ->
  gen_statem:cast(pollution_value_collector_gen_statem, {add_value, DateTime, Type, Value}).

store_data() ->
  gen_statem:cast(pollution_value_collector_gen_statem, store_data).

handle_store(_ID, []) -> ok;
handle_store(ID, [{DateTime, Type, Value} | T]) ->
  pollution_gen_server:add_value(ID, DateTime, Type, Value),
  handle_store(ID, T).