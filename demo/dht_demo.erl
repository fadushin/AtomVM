-module (dht_demo).

-export([start/0]).

start() ->
    {ok, DHT11} = dht:start(21, dht11),
    loop(DHT11).

loop(DHT11) ->
    take_measurement(DHT11),
    timer:sleep(10000),
    loop(DHT11).

take_measurement(DHT) ->
    case dht:measure(DHT) of
        {ok, Measurement} ->
            {Temp, TempFractional, Hum, HumFractional} = Measurement,
            io:format(
                "Temperature: ~p.~pC  Humidity: ~p.~p%~n", [
                    Temp, TempFractional, Hum, HumFractional]
            );
        Error ->
            io:format("Error taking measurement: ~p~n", [Error])
    end.
