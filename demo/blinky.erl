-module (blinky).
-export([start/0]).

start() ->
    GPIO = gpio:open(),
    gpio:set_direction(GPIO, 2, output),
    loop(GPIO, 1).

loop(GPIO, Val) ->
    gpio:set_level(GPIO, 2, Val),
    io:format("Set pin 2 to ~p~n", [Val]),
    timer:sleep(1000),
    loop(GPIO, 1 - Val).
