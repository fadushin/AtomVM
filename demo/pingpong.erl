-module(pingpong).
-export([start/0]).

start() ->
    GPIO = gpio:open(),
    gpio:set_direction(GPIO, 2, output),
    Pong = spawn(fun() -> pong(GPIO) end),
    ping(GPIO, Pong).

ping(GPIO, Pong) ->
    Pong ! {ping, self()},
    receive
        pong ->
            gpio:set_level(GPIO, 2, 1),
            timer:sleep(1000),
            ping(GPIO, Pong)
    end.

pong(GPIO) ->
    receive
        {ping, Pid} ->
            gpio:set_level(GPIO, 2, 0),
            timer:sleep(1000),
            Pid ! pong,
            pong(GPIO)
    end.
