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

-module(rex).

-export([start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-include("estdlib.hrl").
-include("logger.hrl").

-define(SERVER_NAME, ?MODULE).

-record(state,{
    
}).

%%
%% public API
%%

start() ->
    ?GEN_SERVER:start({local, ?SERVER_NAME}, ?MODULE, [], []).


%%
%% gen_server implementation
%%

init(_) ->
    {ok, #state{}}.

handle_call({call, Module, Function, Args, _Leader}, _From, State) ->
    ?LOG_INFO("Calling ~p:~p(~p)", [Module, Function, Args]),
    Result = 
    try
        case Args of
            [] ->
                Module:Function();
            [Arg1] ->
                Module:Function(Arg1);
            [Arg1, Arg2] ->
                Module:Function(Arg1, Arg2);
            [Arg1, Arg2, Arg3] ->
                Module:Function(Arg1, Arg2, Arg3);
            [Arg1, Arg2, Arg3, Arg4] ->
                Module:Function(Arg1, Arg2, Arg3, Arg4);
            [Arg1, Arg2, Arg3, Arg4, Arg5] ->
                Module:Function(Arg1, Arg2, Arg3, Arg4, Arg5);
            _ ->
                {error, {unsupported_arity, length(Args)}}
        end
    catch
        _:_ ->
            {error, {call_failure, Module, Function, Args}}
    end,
    ?LOG_INFO("Result=~p", [Result]),
    {reply, Result, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.