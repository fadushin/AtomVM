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

-define(LOG_INFO(Format, Args),      logger:log({?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE}, info,     {Format, Args})).
-define(LOG_WARNING(Format, Args),   logger:log({?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE}, warning,  {Format, Args})).
-define(LOG_ERROR(Format, Args),     logger:log({?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE}, error,    {Format, Args})).
-define(LOG_DEBUG(Format, Args),     logger:log({?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE}, debug,    {Format, Args})).

% -define(LOG_INFO(Format, Args),      avm_io:format(Format ++ "~n", Args)).
% -define(LOG_WARNING(Format, Args),   avm_io:format(Format ++ "~n", Args)).
% -define(LOG_ERROR(Format, Args),     avm_io:format(Format ++ "~n", Args)).
