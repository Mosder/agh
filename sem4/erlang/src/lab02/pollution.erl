-module(pollution).

-export([]).

%% rekordy i mapy grrrrrrrrrrrrrrrrrrrrrrrrrr
create_monitor() -> [].

station_exists(Name, Data) -> length([1 || {Name, _. _} <- Data, ])

add_station(Name, {Lat, Lon}, Data) ->
  case exists(Name) or exists(Lat, Lon) ->
    false -> [{}] ++ Data
    _ -> Data
  end.