-module(pollution).

-export([
  create_monitor/0,
  add_station/3,
  add_value/5,
  remove_value/4,
  get_one_value/4,
  get_station_min/3,
  get_station_mean/3,
  get_daily_mean/3,
  get_daily_average_data_count/2
]).

-record(measurement, {datetime, type, value}).
%% #{
%%    stations => #{ Name -> [measurement] },
%%    geo_to_name => #{ {Lat, Lon} -> Name }
%% }
create_monitor() -> #{stations => #{}, geo_to_name => #{}}.

geo_to_name(Lat, Lon, Monitor) -> maps:get({Lat, Lon}, maps:get(geo_to_name, Monitor)).

get_station({Lat, Lon}, Monitor) ->
  case maps:is_key({Lat, Lon}, maps:get(geo_to_name, Monitor)) of
    true -> maps:get(geo_to_name(Lat, Lon, Monitor), maps:get(stations, Monitor));
    false -> -1
  end;
get_station(Name, Monitor) ->
  case maps:is_key(Name, maps:get(stations, Monitor)) of
    true -> maps:get(Name, maps:get(stations, Monitor));
    false -> -1
  end.

get_measurement(Station, DateTime, Type) ->
  case lists:filter(fun(M) -> (M#measurement.datetime == DateTime) and (M#measurement.type == Type) end, Station) of
    [] -> -1;
    [M] -> M
  end.

add_station(Name, {Lat, Lon}, Monitor) ->
  case (get_station(Name, Monitor) == -1) and (get_station({Lat, Lon}, Monitor) == -1) of
    true ->
      Stations = maps:get(stations, Monitor),
      GeoToName = maps:get(geo_to_name, Monitor),
      Monitor#{stations := Stations#{Name => []}, geo_to_name := GeoToName#{{Lat, Lon} => Name}};
    false -> {error, "Station already exists"}
  end.

add_value(ID, DateTime, Type, Value, Monitor) ->
  case get_station(ID, Monitor) of
    -1 -> {error, "Cannot add data to a non-existent station"};
    Station ->
      case get_measurement(Station, DateTime, Type) of
        -1 ->
          NewMeasurement = #measurement{datetime = DateTime, type = Type, value = Value},
          Stations = maps:get(stations, Monitor),
          StationName = case ID of {Lat, Lon} -> geo_to_name(Lat, Lon, Monitor); Name -> Name end,
          Monitor#{stations := Stations#{StationName := Station ++ [NewMeasurement]}};
        _ -> {error, "Cannot add a measurement of the same station, datetime and type"}
      end
  end.

remove_value(ID, DateTime, Type, Monitor) ->
  case get_station(ID, Monitor) of
    -1 -> {error, "Cannot remove a measurement from a non-existent station"};
    Station ->
      case get_measurement(Station, DateTime, Type) of
        -1 -> {error, "Cannot remove a measurement that doesn't exist"};
        Measurement ->
          Stations = maps:get(stations, Monitor),
          StationName = case ID of {Lat, Lon} -> geo_to_name(Lat, Lon, Monitor); Name -> Name end,
          Monitor#{stations := Stations#{StationName := lists:filter(fun(M) -> M /= Measurement end, Station)}}
      end
  end.

get_one_value(ID, DateTime, Type, Monitor) ->
  case get_station(ID, Monitor) of
    -1 -> {error, "Cannot get a value from a non-existent station"};
    Station ->
      case get_measurement(Station, DateTime, Type) of
        -1 -> {error, "Cannot get a value from a non-existent measurement"};
        Measurement -> Measurement#measurement.value
      end
  end.

get_station_min(ID, Type, Monitor) ->
  case get_station(ID, Monitor) of
    -1 -> {error, "Cannot get a minimum from a non-existent station"};
    Station ->
      case lists:filter(fun(M) -> M#measurement.type == Type end, Station) of
        [] -> {error, "Didn't find any measurements of such type in the specified station"};
        List -> lists:min(lists:map(fun(M) -> M#measurement.value end, List))
      end
  end.

get_station_mean(ID, Type, Monitor) ->
  case get_station(ID, Monitor) of
    -1 -> {error, "Cannot get a mean from a non-existent station"};
    Station ->
      case lists:filter(fun(M) -> M#measurement.type == Type end, Station) of
        [] -> {error, "Didn't find any measurements of such type in the specified station"};
        List ->
          ValueList = lists:map(fun(M) -> M#measurement.value end, List),
          lists:sum(ValueList) / length(ValueList)
      end
  end.

get_daily_mean(Type, Date, Monitor) ->
  AllMeasurements = lists:foldl(fun(Ms, List) -> Ms ++ List end, [], [V || _ := V <- maps:get(stations, Monitor)]),
  case lists:filter(fun(M) ->
    (M#measurement.type == Type) and (element(1, M#measurement.datetime) == Date) end, AllMeasurements) of
    [] -> {error, "Didn't find any measurements of such type and datetime"};
    List ->
      ValueList = lists:map(fun(M) -> M#measurement.value end, List),
      lists:sum(ValueList) / length(ValueList)
  end.

get_daily_average_data_count(Date, Monitor) ->
  AllMeasurements = lists:foldl(fun(Ms, List) -> Ms ++ List end, [], [V || _ := V <- maps:get(stations, Monitor)]),
  case lists:filter(fun(M) -> element(1, M#measurement.datetime) == Date end, AllMeasurements) of
    [] -> {error, "Didn't find any measurements on that day"};
    List -> length(List) / length(maps:keys(maps:get(stations, Monitor)))
  end.
