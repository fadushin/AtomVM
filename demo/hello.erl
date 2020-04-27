-module(hello).
-export([start/0]).

start() ->
    erlang:display({esp32_free_heap_size, erlang:system_info(esp32_free_heap_size)}),
    io:format("Hello World!~n").
