-module(system_info_server).

-export([start/0, handle_req/3]).

start() ->
        case atomvm:platform() of
        esp32 ->
            start_wifi();
        _ -> ok
    end,
    run().

%% @private
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

%% @private
run() ->
    Router = [
        {"*", ?MODULE, []}
    ],
    io:format("Starting http server on port 8080...~n"),
    case http_server:start_server(8080, Router) of
        Pid when is_pid(Pid) ->
            io:format("HTTP server started.~n"),
            sleep_forever();
        Error ->
            io:format("An error occurred: ~p~n", [Error])

    end.

handle_req("GET", [], Conn) ->
    TimeString = universaltime_to_bin(erlang:universaltime()),
    Body = [<<"<html><body><h1>">>, TimeString, <<"</h1></body></html>">>],
    http_server:reply(200, Body, Conn);

handle_req("GET", ["system", "info"], Conn) ->
    SysInfo = [
        {atom_count, erlang:system_info(atom_count)},
        {process_count, erlang:system_info(process_count)},
        {port_count, erlang:system_info(port_count)},
        {word_size, erlang:system_info(wordsize)},
        {platform, atomvm:platform()},
        {heap_free, erlang:system_info(esp32_free_heap_size)}
    ],
    Body = json_encoder:encode(SysInfo),
    http_server:reply(200, Body, Conn);

handle_req("GET", ["processes"], Conn) ->
    Procs = lists:reverse(erlang:processes()),
    Body = json_encoder:encode([io_lib:format("~p", [Proc]) || Proc <- Procs]),
    http_server:reply(200, Body, Conn);

handle_req("GET", ["processes", PidString, "info"], Conn) ->
    {Code, ProcInfo} = try_proc_info_list(PidString),
    Body = json_encoder:encode(ProcInfo),
    http_server:reply(Code, Body, Conn);

handle_req(Method, Path, Conn) ->
    io:format("Method: ~p Path: ~p~n", [Method, Path]),
    Body = <<"<html><body><h1>Not Found</h1></body></html>">>,
    http_server:reply(404, Body, Conn).

universaltime_to_bin({{Year, Month, Day}, {H, M, S}}) ->
    [
     erlang:integer_to_binary(Year), $/,
     erlang:integer_to_binary(Month), $/,
     erlang:integer_to_binary(Day), $\s,
     erlang:integer_to_binary(H), $:,
     erlang:integer_to_binary(M), $:,
     erlang:integer_to_binary(S)
    ].

try_proc_info_list(PidString) ->
    try proc_info_list(PidString) of
        Res -> {200, Res}
    catch
        _:_ -> {404, [{error, <<"Process not found.">>}]}
    end.

proc_info_list(PidString) ->
    PidInteger = erlang:list_to_integer(PidString),
    Procs = lists:reverse(erlang:processes()),
    Pid = lists:nth(PidInteger, Procs),
    io:format("pid: ~p~n", [Pid]),
    [
        {pid, io_lib:format("~p", [Pid])},
        erlang:process_info(Pid, heap_size),
        erlang:process_info(Pid, stack_size),
        erlang:process_info(Pid, message_queue_len),
        erlang:process_info(Pid, memory)
    ].

% sys_info(esp32) ->
%     [
%         {esp32_free_heap_size, erlang:system_info(esp32_free_heap_size)},
%         {esp32_chip_info, erlang:system_info(esp32_chip_info)},
%         {esp_idf_version, erlang:system_info(esp_idf_version)}
%     ];
% sys_info(_) -> [].

sleep_forever() ->
    timer:sleep(10000),
    sleep_forever().
