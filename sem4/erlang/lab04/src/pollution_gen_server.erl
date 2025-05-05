%%%-------------------------------------------------------------------
%%% @author mosder
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(pollution_gen_server).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

-define(SERVER, ?MODULE). 

-record(pollution_gen_server_state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #pollution_gen_server_state{}}.

handle_call(_Request, _From, State = #pollution_gen_server_state{}) ->
    {reply, ok, State}.

handle_cast(_Request, State = #pollution_gen_server_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #pollution_gen_server_state{}) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-record(call, {type, args}).

stop() -> pollutionServer ! {request, stop}, ok.

call(Call = #call{}) ->
    pollutionServer ! {request, self(), Call},
    receive
        {reply, Reply} -> Reply
    end.

add_station(Name, {Lat, Lon}) ->
    call(#call{type=setter, args={add_station, Name, {Lat, Lon}}}).

add_value(ID, DateTime, Type, Value) ->
    call(#call{type=setter, args={add_value, ID, DateTime, Type, Value}}).

remove_value(ID, DateTime, Type) ->
    call(#call{type=setter, args={remove_value, ID, DateTime, Type}}).

get_one_value(ID, DateTime, Type) ->
    call(#call{type=getter, args={get_one_value, ID, DateTime, Type}}).

get_station_min(ID, Type) ->
    call(#call{type=getter, args={get_station_min, ID, Type}}).

get_station_mean(ID, Type) ->
    call(#call{type=getter, args={get_station_mean, ID, Type}}).

get_daily_mean(Type, Date) ->
    call(#call{type=getter, args={get_daily_mean, Type, Date}}).

get_daily_average_data_count(Date) ->
    call(#call{type=getter, args={get_daily_average_data_count, Date}}).
