-module(system_info_api_handler).

-export([handle_api_request/4]).

-include("logger.hrl").

%%
%% api_handler implementation
%%

handle_api_request(get, [], _HttpRequest, _HandlerOpts) ->
    ?LOG_INFO("get /", []),
    {ok, "atomvm"};
handle_api_request(get, ["time"], _HttpRequest, _HandlerOpts) ->
    ?LOG_INFO("get /time", []),
    {ok, get_time()};
handle_api_request(get, ["system", "info"], _HttpRequest, _HandlerOpts) ->
    ?LOG_INFO("get /system/info", []),
    SysInfo = [
        {atom_count,    erlang:system_info(atom_count)},
        {process_count, erlang:system_info(process_count)},
        {port_count,    erlang:system_info(port_count)},
        {word_size,     erlang:system_info(wordsize)},
        {platform,      atomvm:platform()},
        {heap_free,     erlang:system_info(esp32_free_heap_size)}
    ],
    {ok, SysInfo};
handle_api_request(get, ["processes"], _HttpRequest, _HandlerOpts) ->
    ?LOG_INFO("get /processes", []),
    Procs = lists:reverse(erlang:processes()),
    {ok, [io_lib:format("~p", [Proc]) || Proc <- Procs]};
handle_api_request(get, ["processes", PidString, "info"], _HttpRequest, _HandlerOpts) ->
    ?LOG_INFO("get /processes/<int>/info", []),
    try
        {ok, proc_info_list(PidString)}
    catch
        _:_ ->
            {error, no_such_process}
    end;
handle_api_request(Method, Path, _HttpRequest, _HandlerOpts) ->
    ?LOG_ERROR("Unsupported method ~p and path ~p", [Method, Path]),
    {error, {unsupported, Method, Path}}.

%%
%% Private operations
%%

%% @private
get_time() ->
    {{Year, Month, Day}, {H, M, S}} = erlang:universaltime(),
    [
        {year,      Year},
        {month,     Month},
        {day,       Day},
        {hour,      H},
        {minute,    M},
        {second,    S}
    ].

%% @private
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

% %% @private
% sys_info(esp32) ->
%     [
%         {esp32_free_heap_size, erlang:system_info(esp32_free_heap_size)},
%         {esp32_chip_info, erlang:system_info(esp32_chip_info)},
%         {esp_idf_version, erlang:system_info(esp_idf_version)}
%     ];
% sys_info(_) -> [].
