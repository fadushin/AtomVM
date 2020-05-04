%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Copyright 2019 by Davide Bettio <davide@uninstall.it>                 %
%                                                                         %
%   This program is free software; you can redistribute it and/or modify  %
%   it under the terms of the GNU Lesser General Public License as        %
%   published by the Free Software Foundation; either version 2 of the    %
%   License, or (at your option) any later version.                       %
%                                                                         %
%   This program is distributed in the hope that it will be useful,       %
%   but WITHOUT ANY WARRANTY; without even the implied warranty of        %
%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         %
%   GNU General Public License for more details.                          %
%                                                                         %
%   You should have received a copy of the GNU General Public License     %
%   along with this program; if not, write to the                         %
%   Free Software Foundation, Inc.,                                       %
%   51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA .        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(http_server).

-export([start_server/2, reply/3, parse_query_string/1]).

start_server(Port, Router) ->
    case gen_tcp:listen(Port, []) of
        {ok, ListenSocket} ->
            spawn(fun() -> accept(ListenSocket, Router) end);
        Error ->
            erlang:display(Error)
    end.

accept(ListenSocket, Router) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, _Socket} ->
            spawn(fun() -> accept(ListenSocket, Router) end),
            loop(Router);
        Error ->
            erlang:display(Error)
    end.

loop(Router) ->
    loop(Router, {waiting_request_line, [], []}).

loop(Router, OldStateAndData) ->
    receive
        {tcp_closed, _Socket} ->
            ok;
        {tcp, Socket, Packet} ->
            io:format("Received request from ~p~n", [inet:peername(Socket)]),
            case parse_http(Packet, OldStateAndData) of
                {got_request, RequestData} ->
                    Conn = [{socket, Socket} | RequestData],
                    Method = proplists:get_value(method, Conn),
                    PathTokens = proplists:get_value(uri, Conn),
                    {ok, Module} = find_route(Method, PathTokens, Router),
                    SplitTokens = split(PathTokens),
                    {ok, _UpdatedConn} = Module:handle_req(Method, SplitTokens, Conn),
                    ok;

                {need_more, StateAndData} ->
                    loop(Router, StateAndData);
                Error ->
                    io:format("Error parsing request: ~p~n", [Error]),
                    ok
            end
    end.

find_route(_Method, _Path, []) ->
    {error, 404};

find_route(Method, Path, [{Target, Mod, _Opts} | T]) ->
    if
        Path == Target ->
            {ok, Mod};

        Target == "*" ->
            {ok, Mod};

        true ->
            find_route(Method, Path, T)
    end.

reply(StatusCode, Reply, Conn) ->
    Socket = proplists:get_value(socket, Conn),
    FullReply = [
        <<"HTTP/1.1 ">>,
        code_to_status_string(StatusCode),
        "\r\nContent-Type: text/html\r\n\r\n",
        Reply
    ],
    gen_tcp:send(Socket, FullReply),
    gen_tcp:close(Socket),
    ClosedConn = [{closed, true} | Conn],
    {ok, ClosedConn}.

code_to_status_string(200) ->
    <<"200 OK">>;
code_to_status_string(Code) ->
    [erlang:integer_to_binary(Code), <<" NotOK">>].

parse_query_string(L) ->
    parse_query_string(L, key, [], []).

parse_query_string([$+ | Tail], State, Acc, Data) ->
    parse_query_string([$\s | Tail], State, Acc, Data);

parse_query_string([$%, Hex1, Hex2 | Tail], State, Acc, Data) ->
    Char = (hex_char_to_n(Hex1) bsl 4) bor hex_char_to_n(Hex2),
    parse_query_string([Char | Tail], State, Acc, Data);

parse_query_string([$& | Tail], value, Acc, [LastKey | Data]) ->
    NewData = [{LastKey, reverse(Acc)} | Data],
    parse_query_string(Tail, key, [], NewData);

parse_query_string([$= | Tail], key, Acc, Data) ->
    parse_query_string(Tail, value, [], [ reverse(Acc) | Data]);

parse_query_string([], value, Acc, [LastKey | Data]) ->
    [{LastKey, reverse(Acc)} | Data ];

parse_query_string([C | Tail], State, Acc, Data) ->
    parse_query_string(Tail, State, [C | Acc], Data).

hex_char_to_n(N) when N >= $0 andalso N =< $9 ->
    N - $0;

hex_char_to_n(N) when N >= $a andalso N =< $f ->
    (N - $a) + 10;

hex_char_to_n(N) when N >= $A andalso N =< $F ->
    (N - $A) + 10.

split(L) ->
    Tokens = split(L, [], []),
    TrimmedTokens =
        case Tokens of
            [[] | T] -> T;
            AlreadyTrimmed -> AlreadyTrimmed
        end,
    [_H | T2] = reverse(TrimmedTokens),
    T2.

split([], TokenAcc, Acc) ->
    [reverse(TokenAcc) | Acc];

split([$/ | T], TokenAcc, Acc) ->
    split(T, [], [reverse(TokenAcc) | Acc]);

split([H | T], TokenAcc, Acc) ->
    split(T, [H | TokenAcc], Acc).

parse_http([], {waiting_body, [], Data}) ->
    {got_request, Data};

parse_http([], {in_body, Acc, Data}) ->
    {got_request, [{body_chunk, reverse(Acc)} | Data]};

parse_http([], StateAndData) ->
    {need_more, StateAndData};

parse_http([$\n | Tail], {{waiting_lf, NextState}, [], Data}) ->
    parse_http(Tail, {NextState, [], Data});

parse_http([C | Tail], {waiting_request_line, [], []}) ->
    parse_http(Tail, {in_method, [C], []});

parse_http([$\s | Tail], {in_method, Acc, []}) ->
    parse_http(Tail, {waiting_uri, [], [{method, reverse(Acc)}]});

parse_http([C | Tail], {in_method, Acc, []}) ->
    parse_http(Tail, {in_method, [C | Acc], []});

parse_http([C | Tail], {waiting_uri, [], Data}) ->
    parse_http(Tail, {in_uri, [C], Data});

parse_http([$\s | Tail], {in_uri, Acc, Data}) ->
    parse_http(Tail, {waiting_http_version, [], [{uri, reverse(Acc)} | Data]});

parse_http([C | Tail], {in_uri, Acc, Data}) ->
    parse_http(Tail, {in_uri, [C | Acc], Data});

parse_http([C | Tail], {waiting_http_version, [], Data}) ->
    parse_http(Tail, {in_http_version, [C], Data});

parse_http([$\r | Tail], {in_http_version, Acc, Data}) ->
    parse_http(Tail, {{waiting_lf, waiting_headers}, [], [{http_version, reverse(Acc)} | Data]});

parse_http([C | Tail], {in_http_version, Acc, Data}) ->
    parse_http(Tail, {in_http_version, [C | Acc], Data});

parse_http([$\r | Tail], {waiting_headers, [], Data}) ->
    parse_http(Tail, {{waiting_lf, waiting_body}, [], Data});

parse_http([C | Tail], {waiting_headers, [], Data}) ->
    parse_http(Tail, {in_header, [C], Data});

parse_http([$\r | Tail], {in_header, Acc, Data}) ->
    parse_http(Tail, {{waiting_lf, waiting_headers}, [], [{header, reverse(Acc)} | Data]});

parse_http([C | Tail], {in_header, Acc, Data}) ->
    parse_http(Tail, {in_header, [C | Acc], Data});

parse_http([C | Tail], {waiting_body, [], Data}) ->
    parse_http(Tail, {in_body, [C], Data});

parse_http([C | Tail], {in_body, Acc, Data}) ->
    parse_http(Tail, {in_body, [C | Acc], Data});

parse_http([C | Tail], {_, _Acc, Data}) ->
    erlang:display({consume, [C]}),
    parse_http(Tail, {consume, [], Data});

parse_http(Input, StateTuple) ->
    erlang:display({cannot_process, Input, StateTuple}).

reverse(L) -> reverse(L, []).

reverse([], Acc) ->
    Acc;

reverse([H | T], Acc) ->
    reverse(T, [H | Acc]).
