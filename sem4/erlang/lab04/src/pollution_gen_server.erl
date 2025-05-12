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
-export([
    crash/0,
    stop/0,
    add_station/2,
    add_value/4,
    remove_value/3,
    get_one_value/3,
    get_station_min/2,
    get_station_mean/2,
    get_daily_mean/2,
    get_daily_average_data_count/1
]).

-define(SERVER, ?MODULE). 

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, pollution:create_monitor()}.

handle_call({Function, Args}, _From, State) ->
    {reply, erlang:apply(pollution, Function, Args++[State]), State}.

handle_cast(crash, _State) ->
    crash:crash();
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast({Function, Args}, State) ->
    Output = erlang:apply(pollution, Function, Args++[State]),
    case Output of
        {error, Message} ->
            io:format("~s~n", [Message]),
            {noreply, State};
        Result -> {noreply, Result}
    end.

terminate(Reason, _State) ->
    io:format("Server closed with reason: ~s~n", [Reason]),
    Reason.

%%%===================================================================
%%% Internal functions
%%%===================================================================
crash() -> gen_server:cast(pollution_gen_server, crash).

stop() ->
    gen_server:cast(pollution_gen_server, stop).

add_station(Name, {Lat, Lon}) ->
    gen_server:cast(pollution_gen_server, {add_station, [Name, {Lat, Lon}]}).

add_value(ID, DateTime, Type, Value) ->
    gen_server:cast(pollution_gen_server, {add_value, [ID, DateTime, Type, Value]}).

remove_value(ID, DateTime, Type) ->
    gen_server:cast(pollution_gen_server, {remove_value, [ID, DateTime, Type]}).

get_one_value(ID, DateTime, Type) ->
    gen_server:call(pollution_gen_server, {get_one_value, [ID, DateTime, Type]}).

get_station_min(ID, Type) ->
    gen_server:call(pollution_gen_server, {get_station_min, [ID, Type]}).

get_station_mean(ID, Type) ->
    gen_server:call(pollution_gen_server, {get_station_mean, [ID, Type]}).

get_daily_mean(Type, Date) ->
    gen_server:call(pollution_gen_server, {get_daily_mean, [Type, Date]}).

get_daily_average_data_count(Date) ->
    gen_server:call(pollution_gen_server, {get_daily_average_data_count, [Date]}).
