%%%-------------------------------------------------------------------
%%% @author mosder
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. May 2025 01:41
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("mosder").

%% API
-export([
  start/0,
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

-record(call, {type, args}).

start() -> register(pollutionServer, spawn(fun () -> init() end)).

init() -> loop(pollution:create_monitor()).

loop(Monitor) ->
  receive
    {request, stop} -> ok;
    {request, Pid, Call = #call{}} ->
      Output = case Call#call.args of
                 {add_station, Name, {Lat, Lon}} ->
                   pollution:add_station(Name, {Lat, Lon}, Monitor);
                 {add_value, ID, DateTime, Type, Value} ->
                   pollution:add_value(ID, DateTime, Type, Value, Monitor);
                 {remove_value, ID, DateTime, Type} ->
                   pollution:remove_value(ID, DateTime, Type, Monitor);
                 {get_one_value, ID, DateTime, Type} ->
                   pollution:get_one_value(ID, DateTime, Type, Monitor);
                 {get_station_min, ID, Type} ->
                   pollution:get_station_min(ID, Type, Monitor);
                 {get_station_mean, ID, Type} ->
                   pollution:get_station_mean(ID, Type, Monitor);
                 {get_daily_mean, Type, Date} ->
                   pollution:get_daily_mean(Type, Date, Monitor);
                 {get_daily_average_data_count, Date} ->
                   pollution:get_daily_average_data_count(Date, Monitor)
               end,
      case Output of
        {error, Message} ->
          Pid ! {reply, {error, Message}},
          loop(Monitor);
        Result ->
          case Call#call.type of
            setter ->
              Pid ! {reply, ok},
              loop(Result);
            getter ->
              Pid ! {reply, Result},
              loop(Monitor)
          end
      end
  end.

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
