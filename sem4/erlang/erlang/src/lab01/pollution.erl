-module(pollution).

-export([test_data/0, number_of_readings/2, calculate_max/2, calculate_mean/2]).

test_data() ->
  [
    {
      "andrzej",
      {{2025, 3, 16},{3, 3, 3}},
      [{"pm2.5", 103.4}, {"pm10", 69.2137}]
    },
    {
      "maciej",
      {{2025, 2, 14},{6, 9, 12}},
      [{"pm2.5", 11}, {"pm10", 22}, {"co", 50}]
    },
    {
      "marzena",
      {{2025, 3, 16},{12, 21, 33}},
      [{"pm2.5", 107}, {"pm10", 1.11}, {"co", 23.32}, {"no2", 999999}]
    },
    {
      "zbigniew",
      {{2024, 12, 31},{23, 59, 59}},
      [{"pm2.5", 7.27}, {"pm10", 456.999}]
    }
  ].

%% count the number of readings on a certain day
number_of_readings(Readings, Date) -> number_of_readings(Readings, Date, 0).

number_of_readings([], _Date, Acc) -> Acc;
number_of_readings([{_Name, {Date, _Time}, _Data} | T], Date, Acc) -> number_of_readings(T, Date, Acc + 1);
number_of_readings([_H | T], Date, Acc) -> number_of_readings(T, Date, Acc).

%% find value of given type inside data, returns -1 if not found
find_value([], _Type) -> -1;
find_value([{Type, Value} | _T], Type) -> Value;
find_value([_H | T], Type) -> find_value(T, Type).

%% calculate max value of given type in given readings
calculate_max(Readings, Type) -> calculate_max(Readings, Type, -1).

calculate_max([], _Type, -1) -> "Data type not found";
calculate_max([], _Type, Max) -> Max;
calculate_max([{_Name, _DateTime, Data} | T], Type, Max) -> calculate_max(T, Type, max(find_value(Data, Type), Max)).

%% calculate mean value of given type in given readings
calculate_mean(Readings, Type) -> calculate_mean(Readings, Type, 0, 0).

calculate_mean([], _Type, _Sum, 0) -> "Data type not found";
calculate_mean([], _Type, Sum, Count) -> Sum / Count;
calculate_mean([{_Name, _DateTime, Data} | T], Type, Sum, Count) ->
  case find_value(Data, Type) of
    -1 -> calculate_mean(T, Type, Sum, Count);
    Value -> calculate_mean(T, Type, Sum + Value, Count + 1)
  end.
