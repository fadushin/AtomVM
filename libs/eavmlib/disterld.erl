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

-module(disterld).

-export([start/1, start/2]).
-export([handle_init/2, handle_receive/3, handle_closed/1]).

-include("estdlib.hrl").
-include("logger.hrl").

-record(state,{
    mstate = accept,
    cookie,
    ch_b,
    address
}).

-define(DISTERLD_PORT, 7556).
-define(SEND_NAME, 110).
-define(SEND_CHALLENGE_REPLY, 114).
-define(SEND_CHALLENGE_ACK, 97).
-define(PASS_THROUGH, 112).

-define(DFLAG_EXTENDED_REFERENCES, 16#4).
-define(DFLAG_UTF8_ATOMS, 16#10000).
-define(DFLAG_EXTENDED_PIDS_PORTS, 16#100).
-define(DFLAG_NEW_FUN_TAGS, 16#80).

-define(ATOM_VM_FLAGS,
    ?DFLAG_EXTENDED_REFERENCES
    bor ?DFLAG_UTF8_ATOMS
    bor ?DFLAG_EXTENDED_PIDS_PORTS
    bor ?DFLAG_NEW_FUN_TAGS
).

-define(NODE_NAME, <<"atomvm">>).

%%
%% public API
%%

start(Cookie) when is_binary(Cookie) ->
    start(Cookie, ?DISTERLD_PORT).

start(Cookie, DisterlPort) ->
    case epmd:start(DisterlPort, [{debug, true}]) of
        {ok, _Epmd} ->
            rex:start(),
            gen_tcp_server:start(DisterlPort, ?MODULE, Cookie),
            ?LOG_INFO("disterld started on port ~p", [DisterlPort]);
        Error ->
            Error
    end.

%%
%% gen_tcp_server implementation
%%

%% @hidden
handle_init(_Socket, Cookie) ->
    #state{cookie=Cookie}.

%% @hidden
handle_receive(Socket, Packet, #state{mstate=MState} = State) ->
    LenBytes = case MState of
        maybe_connected -> 4;
        connected -> 4;
        _ -> 2
    end,
    ByteSize = erlang:byte_size(Packet),
    ?LOG_INFO("LenBytes: ~p ByteSize: ~p", [LenBytes, ByteSize]),
    case ByteSize < LenBytes of
        true ->
            ?LOG_ERROR("Missing length prefix: ~p", [Packet]),
            {close, State};
        _ ->
            LenBits = LenBytes * 8,
            <<Length:LenBits, Request/binary>> = Packet,
            %% TODO is there a reasonable upper bound on what to allow?
            case Length > 1024 of
                true ->
                    ?LOG_ERROR("Message too large: ~p", [erlang:byte_size(Packet)]),
                    {close, State};
                _ ->
                    ?LOG_INFO("Handling request Length=~p Request=~p", [Length, Request]),
                    handle_request(Socket, Length, Request, State)
            end
    end.

%% @hidden
handle_closed(_State) ->
    ok.

%%
%% internal functions
%%

%% @private
handle_request(
    Socket, _Length, 
    <<?SEND_NAME:8, Version0:8, Version1:8, Flags:32, Name/binary>>, 
    #state{mstate=accept} = State
) ->
    ?LOG_INFO("SEND_NAME: Version0=~p, Version1=~p, Flags=~p, Name=~p", [Version0, Version1, Flags, Name]),
    NodeName = ?NODE_NAME,
    %% TODO implement bs_append so that size computation is not needed
    NodeNameSize = erlang:byte_size(NodeName),
    Address = get_address(Socket),
    LongNodeName = <<NodeName:NodeNameSize/binary, $@:8, Address/binary>>,
    ChB = create_challenge(),
    {reply, [
        create_message(create_ok_staus_message()), 
        create_message(
            create_challenge_message(
                Version0, Version1, ChB, LongNodeName
            )
        )], 
        State#state{mstate=wait_challenge_reply, ch_b=ChB}
    };
handle_request(
    _Socket, _Length, 
    <<?SEND_CHALLENGE_REPLY:8, ChA:32, DiA:16/binary>>, 
    #state{mstate=wait_challenge_reply, cookie=Cookie, ch_b=ChB} = State
) ->
    ?LOG_INFO("SEND_CHALLENGE_REPLY:  ChA=~p DiA=~p", [ChA, DiA]),
    case validate_challenge(ChB, Cookie, DiA) of
        true ->
            ?LOG_INFO("Validated challenge from client", []),
            {reply, 
                create_message(
                    create_challenge_ack_message(
                        create_digest(ChA, Cookie)
                    )
                ), 
                State#state{mstate=maybe_connected}
            };
        _ ->
            ?LOG_ERROR("Failed to validated challenge from client", []),
            {close, State}
    end;
handle_request(_Socket, 0, _Request, State) ->
    ?LOG_INFO("KEEP_ALIVE", []),
    %{reply, <<0:32>>, State#state{mstate=connected}};
    {noreply, State#state{mstate=connected}};
handle_request(_Socket, Length, <<?PASS_THROUGH:8, Payload/binary>>=Msg, State) ->
    MsgSize = erlang:byte_size(Msg),
    PayloadSize = erlang:byte_size(Payload),
    ?LOG_INFO("PASS_THROUGH:  Length=~p MsgSize=~p PayloadSize=~p Payload=~p", [Length, MsgSize, PayloadSize, Payload]),
    {ControlMessage, Used} = erlang:binary_to_term(Payload, [used]),
    erlang:display({"ControlMessage=~p Used=~p", [ControlMessage, Used]}),
    case ControlMessage of
        {6, FromPid, Unused, ToName} ->
            MessageBin = binary:part(Payload, Used, PayloadSize - Used),
            Message = erlang:binary_to_term(MessageBin),
            erlang:display({"Message=~p", [Message]}),
            case handle_reg_send(FromPid, ToName, Message) of
                {ok, Response} ->
                    {reply, create_passthrough_reply({2, Unused, FromPid}, Response), State};
                _ ->
                    {close, State}
            end;
        _ ->
            ?LOG_ERROR("Unsupported ControlMessage: ~p", [ControlMessage]),
            {close, State}
    end;
handle_request(_Socket, Length, Request, State) ->
    ?LOG_ERROR("Unsupported request.  Length=~p Request=~p", [Length, Request]),
    {close, State}.

handle_reg_send(_FromPid, ToName, Message) ->
    case ToName of
        rex ->
            case Message of
                {'$gen_call', {_, Ref}, Msg} ->
                    erlang:display({time_to_gen_call_rex, Msg}),
                    Result = ?GEN_SERVER:call(rex, Msg),
                    {ok, {Ref, Result}};
                _ ->
                    {error, unknownMessage}
            end;
        _ ->
            {unknownToName}
    end.

%% @private
create_passthrough_reply(ControlMessage, Message) ->
    ControlMessageBin = erlang:term_to_binary(ControlMessage),
    N = erlang:byte_size(ControlMessageBin),
    MessageBin = erlang:term_to_binary(Message),
    M = erlang:byte_size(MessageBin),
    Len = 1 + N + M,
    <<Len:32, ?PASS_THROUGH:8, ControlMessageBin/binary, MessageBin/binary>>.

%% @private
create_message(Msg) when is_binary(Msg) ->
    Len = erlang:byte_size(Msg),
    <<Len:16, Msg/binary>>.

%% @private
create_ok_staus_message() ->
    OK = <<"ok">>,
    <<115:8, OK/binary>>.

%% @private
create_challenge_message(Version0, Version1, Challenge, Name) ->
    AtomVMFlags = ?ATOM_VM_FLAGS,
    <<?SEND_NAME:8, Version0:8, Version1:8, AtomVMFlags:32, Challenge:32, Name/binary>>.

%% @private
create_challenge_ack_message(Digest) ->
    <<?SEND_CHALLENGE_ACK:8, Digest/binary>>.

%% @private
get_address(Socket) ->
    {Address, _Port} = ?INET:sockname(Socket),
    {A, B, C, D} = Address,
    AddressStr = ?IO_LIB:format("~p.~p.~p.~p", [A, B, C, D]),
    erlang:list_to_binary(AddressStr).

%% @private
create_challenge() ->
    atomvm:random() rem 32768.

%% @private
create_digest(Challenge, Cookie) when is_integer(Challenge) andalso is_binary(Cookie) ->
    ChallengeBin = erlang:integer_to_binary(Challenge),
    CookieLen = erlang:byte_size(Cookie),
    Bin = <<Cookie:CookieLen/binary, ChallengeBin/binary>>,
    Hash = erlang:md5(Bin),
    Hash.

%% @private
validate_challenge(Challenge, Cookie, Digest) ->
    case create_digest(Challenge, Cookie) of
        Digest ->
            true;
        _SomethingElse ->
            false
    end.
