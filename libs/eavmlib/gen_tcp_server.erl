%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Copyright 2019 by Fred Dushin <fred@dushin.net>                       %
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

-module(gen_tcp_server).

-export([start/3]).

-include("estdlib.hrl").
-include("logger.hrl").

start(Port, HandlerModule, Args) ->
    case ?GEN_TCP:listen(Port, [{binary, true}]) of
        {ok, ListenSocket} ->
            spawn(fun() -> accept(ListenSocket, HandlerModule, Args) end),
            {ok, ListenSocket};
        Error ->
            Error
    end.

accept(ListenSocket, HandlerModule, Args) ->
    % ok = erlang:process_flag(self(), trace_calls, true),
    case ?GEN_TCP:accept(ListenSocket) of
        {ok, Socket} ->
            spawn(fun() -> accept(ListenSocket, HandlerModule, Args) end),
            loop(HandlerModule, HandlerModule:handle_init(Socket, Args));
        Error ->
            ?LOG_ERROR("Error accepting connection: ~p", [Error])
    end.

loop(HandlerModule, State) ->
    receive
        {tcp_closed, Socket} ->
            ?LOG_INFO("Peer ~p closed socket", [?INET:peername(Socket)]),
            HandlerModule:handle_closed(State);
        {tcp, Socket, Packet} ->
            ?LOG_DEBUG("received packet: ~p", [Packet]),
            case HandlerModule:handle_receive(Socket, Packet, State) of
                {reply, ResponsePacket, ResponseState} when is_binary(ResponsePacket) ->
                    ?LOG_INFO("Sending reply: ~p", [ResponsePacket]),
                    ?GEN_TCP:send(Socket, ResponsePacket),
                    loop(HandlerModule, ResponseState);
                {reply, Responses, ResponseState} when is_list(Responses) ->
                    ?LOG_INFO("Sending replies: ~p", [Responses]),
                    [?GEN_TCP:send(Socket, Response) || Response <- Responses],
                    loop(HandlerModule, ResponseState);
                {noreply, ResponseState} ->
                    ?LOG_INFO("no reply", []),
                    loop(HandlerModule, ResponseState);
                {close, ResponsePacket, _ResponseState} when is_binary(ResponsePacket) ->
                    ?LOG_INFO("Sending reply and then closing socket: ~p", [ResponsePacket]),
                    ?GEN_TCP:send(Socket, ResponsePacket),
                    ?TIMER:sleep(500),
                    ?GEN_TCP:close(Socket);
                {close, Responses, _ResponseState} when is_list(Responses) ->
                    ?LOG_INFO("Sending replies and then closing socket: ~p", [Responses]),
                    [?GEN_TCP:send(Socket, Response) || Response <- Responses],
                    ?TIMER:sleep(500),
                    ?GEN_TCP:close(Socket);
                {close, _ResponseState} ->
                    ?LOG_INFO("Closing socket", []),
                    ?GEN_TCP:close(Socket);
                SomethingElse ->
                    ?LOG_ERROR("Unexpected response from handler ~p: ~p", [HandlerModule, SomethingElse])
            end
    end.
