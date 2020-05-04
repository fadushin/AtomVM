-module(tcp_server_blink).

-export([start/0]).

-define(PIN, 2).

start() ->
    Creds = [
        {ssid, esp:nvs_get_binary(atomvm, sta_ssid, <<"myssid">>)},
        {psk,  esp:nvs_get_binary(atomvm, sta_psk, <<"mypsk">>)}
    ],
    case network_fsm:wait_for_sta(Creds, 30000) of
        {ok, {Address, Netmask, Gateway}} ->
            io:format(
                "Acquired IP address: ~p Netmask: ~p Gateway: ~p~n",
                [to_string(Address), to_string(Netmask), to_string(Gateway)]
            ),
            tcp_server_start();
        Error ->
            io:format("An error occurred starting network: ~p~n", [Error])
    end.

tcp_server_start() ->
    case gen_tcp:listen(44404, [{active, true}]) of
        {ok, ListenSocket} ->
            io:format("Listening on ~p.~n", [local_address(ListenSocket)]),
            Gpio = gpio:open(),
            gpio:set_direction(Gpio, ?PIN, output),
            spawn(fun() -> accept(ListenSocket, Gpio) end),
            sleep_forever();
        Error ->
            io:format("An error occurred listening: ~p~n", [Error])
    end.

accept(ListenSocket, Gpio) ->
    io:format("Waiting to accept connection...~n"),
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            io:format("Accepted connection.  local: ~p peer: ~p~n", [local_address(Socket), peer_address(Socket)]),
            spawn(fun() -> accept(ListenSocket, Gpio) end),
            echo(Gpio, 0);
        Error ->
            io:format("An error occurred accepting connection: ~p~n", [Error])
    end.

echo(Gpio, PinState) ->
    io:format("Waiting to receive data...~n"),
    gpio:set_level(Gpio, ?PIN, PinState),
    receive
        {tcp_closed, _Socket} ->
            io:format("Connection closed.~n"),
            ok;
        {tcp, Socket, Packet} ->
            io:format("Received packet ~p from ~p.  Echoing back...~n", [Packet, peer_address(Socket)]),
            gen_tcp:send(Socket, Packet),
            echo(Gpio, 1 - PinState)
    end.

local_address(Socket) ->
    to_string(inet:sockname(Socket)).

peer_address(Socket) ->
    to_string(inet:peername(Socket)).

to_string({{A,B,C,D}, Port}) ->
    io_lib:format("~p.~p.~p.~p:~p", [A,B,C,D, Port]);
to_string({A,B,C,D}) ->
    io_lib:format("~p.~p.~p.~p", [A,B,C,D]).

sleep_forever() ->
    timer:sleep(10000),
    sleep_forever().
