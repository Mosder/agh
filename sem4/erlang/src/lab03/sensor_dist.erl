%%%-------------------------------------------------------------------
%%% @author mosder
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Apr 2025 16:08
%%%-------------------------------------------------------------------
-module(sensor_dist).
-author("mosder").

%% API
-export([find_closest/2, find_closest_parallel/2, get_rand_locations/1]).

get_rand_locations(Number) ->
  [{random:uniform(10000), random:uniform(10000)} || _ <- lists:seq(1, Number)].

dist({X1, Y1}, {X2, Y2}) ->
  math:sqrt((X1-X2)*(X1-X2) + (Y1-Y2)*(Y1-Y2)).

find_for_person(PersonLocation, SensorsLocation) ->
  lists:min([{dist(PersonLocation, SL), {PersonLocation, SL}} || SL <- SensorsLocation]).

find_closest(PeopleLocations, SensorsLocations) ->
  lists:min([find_for_person(PL, SensorsLocations) || PL <- PeopleLocations]).

find_for_person(PersonLocation, SensorsLocation, ParentPID) ->
  ParentPID ! find_for_person(PersonLocation, SensorsLocation).

find_closest_parallel(PeopleLocations, SensorsLocations) ->
  _ = [spawn(?MODULE, find_for_person, [PL, SensorsLocations, self()]) || PL <- PeopleLocations],
  lists:min([receive Dist -> Dist end || _ <- PeopleLocations]).
