-module(api_handler).

-export([handle_http_req/4]).

-include("logger.hrl").

handle_http_req(Method, Path, HttpRequest, {ModOrFun, Opts}) ->
    case ModOrFun of
        Mod when is_atom(Mod) ->
            case Mod:handle_api_request(Method, Path, HttpRequest, Opts) of
                {ok, Reply} ->
                    Body = json_encoder:encode(Reply),
                    {ok, {"application/json", Body}};
                Error ->
                    Error
            end;
        Fun when is_function(Fun) ->
            case Fun(Method, Path, HttpRequest, Opts) of
                {ok, Reply} ->
                    Body = json_encoder:encode(Reply),
                    {ok, {"application/json", Body}};
                Error ->
                    Error
            end;
        SomethingElse ->
            ?LOG_ERROR("Bad Argument.  Expected module or function, but got ~p", [SomethingElse]),
            internal_server_error
    end.
