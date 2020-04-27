-module(avm_util).

-export([sleep_forever/0, address_to_string/1]).

%%
sleep_forever() ->
    timer:sleep(60000),
    sleep_forever().

address_to_string({A, B, C, D}) ->
    io_lib:format("~p.~p.~p.~p", [A, B, C, D]).
