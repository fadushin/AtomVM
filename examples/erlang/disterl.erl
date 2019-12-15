-module(disterl).

-export([start/0]).

-include("estdlib.hrl").

start() ->
    case disterld:start(<<"mvmota">>) of
        {ok, _Disterld} ->
            ?IO:format("Disterl daemon started.~n"),
            avm_timer:sleep(360000);
        Error ->
            Error
    end.
