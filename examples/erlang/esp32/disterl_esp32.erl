-module(disterl_esp32).

-export([start/0]).

-include("estdlib.hrl").
-include("atomvm.hrl").

-define(PIN, 2).

start() ->
    Creds = [
        {ssid, esp:nvs_get_binary(?ATOMVM_NVS_NS, ?ATOMVM_NVS_STA_SSID, <<"myssid">>)},
        {psk,  esp:nvs_get_binary(?ATOMVM_NVS_NS, ?ATOMVM_NVS_STA_PSK, <<"mypsk">>)}
    ],
    case network_fsm:wait_for_sta(Creds, 30000) of
        {ok, {Address, Netmask, Gateway}} ->
            ?IO:format(
                "Acquired IP address: ~p Netmask: ~p Gateway: ~p~n",
                [to_string(Address), to_string(Netmask), to_string(Gateway)]
            ),
            disterl_start();
        Error ->
            ?IO:format("An error occurred starting network: ~p~n", [Error])
    end.

disterl_start() ->
    case disterld:start(<<"mvmota">>) of
        {ok, _Disterld} ->
            ?IO:format("Disterl daemon started.~n"),
            avm_timer:sleep(360000);
        Error ->
            Error
    end.

to_string({{A,B,C,D}, Port}) ->
    ?IO_LIB:format("~p.~p.~p.~p:~p", [A,B,C,D, Port]);
to_string({A,B,C,D}) ->
    ?IO_LIB:format("~p.~p.~p.~p", [A,B,C,D]).
