-module(wifi_demo).
-export([start/0]).

-include("logger.hrl").

start() ->
    erlang:display({esp32_free_heap_size, erlang:system_info(esp32_free_heap_size)}),
    case atomvm:platform() of
        esp32 ->
            start_wifi();
        _ -> ok
    end,
    run().

start_wifi() ->
    case network_fsm:wait_for_sta() of
        {ok, {Address, Netmask, Gateway}} ->
            ?LOG_INFO(
                "IP address: ~s Netmask: ~s Gateway: ~s", [
                    avm_util:address_to_string(Address),
                    avm_util:address_to_string(Netmask),
                    avm_util:address_to_string(Gateway)
                ]
            );
        Error ->
            ?LOG_ERROR("An error occurred starting network: ~p", [Error])
    end.

run() ->
    erlang:display({esp32_free_heap_size, erlang:system_info(esp32_free_heap_size)}),
    avm_util:sleep_forever().
