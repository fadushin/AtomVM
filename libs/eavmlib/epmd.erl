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

-module(epmd).

-export([start/1, start/2]).
-export([handle_init/2, handle_receive/3, handle_closed/1]).

-include("estdlib.hrl").
-include("logger.hrl").

-record(state,{
    disterl_port,
    options
}).

-define(PORT_PLEASE2_REQ, 122).
-define(PORT2_RESP, 119).
-define(PORT2_ERROR, 16#FF).
-define(NORMAL_NODE_TYPE, 77).
-define(TCP_PROTOCOL, 0).
-define(LOWEST_VERSION, 5).
-define(HIGHEST_VERSION, 5).

-define(NODE_NAME, <<"atomvm">>).
-define(EPMD_PORT, 4369).

%%
%% public API
%%

start(DisterlPort) ->
    start(DisterlPort, []).

start(DisterlPort, Options) ->
    case gen_tcp_server:start(?EPMD_PORT, ?MODULE, {DisterlPort, Options}) of
        {ok, _Server} = Result ->
            ?LOG_INFO("epmd started on port ~p", [?EPMD_PORT]),
            Result;
        Error ->
            ?LOG_ERROR("epmd failed to start: ~p", [Error]),
            Error
    end.

%%
%% gen_tcp_server implementation
%%

%% @hidden
handle_init(_Socket, {DisterlPort, Options}) ->
    #state{disterl_port=DisterlPort, options=Options}.

%% @hidden
handle_receive(_Socket, Packet, State) ->
    ByteSize = erlang:byte_size(Packet),
    case ByteSize < 2 of
        true ->
            ?LOG_ERROR("Missing length prefix: ~p", [Packet]),
            {close, State};
        _ ->
            <<Length:16, Request/binary>> = Packet,
            %% TODO is there a reasonable upper bound on what to allow?
            case Length > 512 of
                true ->
                    ?LOG_ERROR("Message too large: ~p", [erlang:byte_size(Packet)]),
                    {close, State};
                _ ->
                    ?LOG_DEBUG("Handling request ~p", [Request]),
                    handle_request(Length, Request, State)
            end
    end.

%% @hidden
handle_closed(_State) ->
    ok.

%%
%% internal functions
%%

%% @private
handle_request(Length, <<?PORT_PLEASE2_REQ:8, NodeName/binary>>, State) ->
    ?LOG_DEBUG("PORT_PLEASE2_REQ: NodeName=~p", [NodeName]),
    case erlang:byte_size(NodeName) =:= Length - 1 of
        true ->
            {close, find_node(NodeName, State), State};
        false ->
            ?LOG_ERROR("Bad size prefix: length: ~p nize: ~p", [Length, erlang:byte_size(NodeName)]),
            {close, <<?PORT2_RESP:8, ?PORT2_ERROR:8>>, State}
    end;
handle_request(Length, Request, State) ->
    ?LOG_ERROR("Bad request.  length=~p request=~p", [Length, Request]),
    {close, State}.


find_node(?NODE_NAME, #state{disterl_port=DisterlPort} = _State) ->
    ?LOG_INFO("'PORT_PLEASE2_REQ' found ~p at port ~p", [?NODE_NAME, DisterlPort]),
    Elen = 0,
    <<
        ?PORT2_RESP:8, 0:8, DisterlPort:16, 
        ?NORMAL_NODE_TYPE:8, ?TCP_PROTOCOL:8, 
        ?HIGHEST_VERSION:16, ?LOWEST_VERSION:16,
        6:16, ?NODE_NAME/binary, Elen:16
    >>;
find_node(OtherNodeName, _State) ->
    ?LOG_WARNING("'PORT_PLEASE2_REQ' NodeName ~p not_found", [OtherNodeName]),
    <<?PORT2_RESP:8, ?PORT2_ERROR:8>>.
