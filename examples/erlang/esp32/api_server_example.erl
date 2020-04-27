-module(api_server_example).

-export([start/0]).

start() ->
    case atomvm:platform() of
        esp32 ->
            start_wifi();
        _ -> ok
    end,
    run().

start_wifi() ->
    Creds = [
        {ssid, esp:nvs_get_binary(atomvm, sta_ssid, <<"myssid">>)},
        {psk,  esp:nvs_get_binary(atomvm, sta_psk, <<"mypsk">>)}
    ],
    case network_fsm:wait_for_sta(Creds, 30000) of
        {ok, {Address, Netmask, Gateway}} ->
            io:format(
                "Acquired IP address: ~p Netmask: ~p Gateway: ~p~n",
                [Address, Netmask, Gateway]
            );
        Error ->
            io:format("An error occurred starting network: ~p~n", [Error])
    end.

run() ->
    Port = 8080,
    Config = [{["api"], api_handler, {system_info_api_handler, undefined}}],
    io:format("Starting httpd on port ~p with config ~p...~n", [Port, Config]),
    case httpd:start(8080, Config) of
        {ok, Pid} when is_pid(Pid) ->
            io:format("httpd started.~n"),
            sleep_forever();
        Error ->
            io:format("An error occurred: ~p~n", [Error])
    end.

sleep_forever() ->
    timer:sleep(10000),
    sleep_forever().
