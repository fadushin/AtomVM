-module (dht_example).

-export([start/0]).

start() ->
    {ok, DHT11} = dht:start(21, dht11),
    {ok, DHT22} = dht:start(22, dht22),
    loop(DHT11, DHT22).

loop(DHT11, DHT22) ->
    take_measurement(dht11, DHT11),
    take_measurement(dht22, DHT22),
    timer:sleep(30000),
    loop(DHT11, DHT22).

take_measurement(Device, DHT) ->
    case dht:measure(DHT) of
        {ok, Measurement} ->
            {Temp, TempFractional, Hum, HumFractional} = Measurement,
            io:format("~p Temperature: ~p.~pC  Humidity: ~p.~p%~n", [Device, Temp, TempFractional, Hum, HumFractional]);
        Error ->
            io:format("Error taking measurement on ~p: ~p~n", [Device, Error])
    end.
