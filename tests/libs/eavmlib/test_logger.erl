-module(test_logger).

-export([test/0, do_log/1, counter/1]).

-include("etest.hrl").
-include("logger.hrl").

test() ->
    start_counter(),
    avm_logger:start([{sinks, [{?MODULE, do_log}]}]),

    ?ASSERT_MATCH(get_counter(info), 0),
    ?ASSERT_MATCH(get_counter(warning), 0),
    ?ASSERT_MATCH(get_counter(error), 0),
    ?ASSERT_MATCH(get_counter(debug), 0),

    ok = ?LOG_INFO("This is an info", []),
    ?ASSERT_MATCH(get_counter(info), 1),
    ?ASSERT_MATCH(get_counter(warning), 0),
    ?ASSERT_MATCH(get_counter(error), 0),
    ?ASSERT_MATCH(get_counter(debug), 0),

    ok = ?LOG_WARNING("This is a warning", []),
    ?ASSERT_MATCH(get_counter(info), 1),
    ?ASSERT_MATCH(get_counter(warning), 1),
    ?ASSERT_MATCH(get_counter(error), 0),
    ?ASSERT_MATCH(get_counter(debug), 0),

    ok = ?LOG_ERROR("This is an error", []),
    ?ASSERT_MATCH(get_counter(info), 1),
    ?ASSERT_MATCH(get_counter(warning), 1),
    ?ASSERT_MATCH(get_counter(error), 1),
    ?ASSERT_MATCH(get_counter(debug), 0),

    ok = ?LOG_DEBUG("This is a debug", []),
    ?ASSERT_MATCH(get_counter(info), 1),
    ?ASSERT_MATCH(get_counter(warning), 1),
    ?ASSERT_MATCH(get_counter(error), 1),
    ?ASSERT_MATCH(get_counter(debug), 0),

    avm_logger:set_levels([debug, info]),
    ok = ?LOG_INFO("Another info ~p", [info]),
    ok = ?LOG_WARNING("Another warning ~p", [warning]),
    ok = ?LOG_ERROR("Another error ~p", [error]),
    ok = ?LOG_DEBUG("Another debug ~p", [debug]),
    ?ASSERT_MATCH(get_counter(info), 2),
    ?ASSERT_MATCH(get_counter(warning), 1),
    ?ASSERT_MATCH(get_counter(error), 1),
    ?ASSERT_MATCH(get_counter(debug), 1),

    % avm_logger:set_sinks([{logger, console_log}]),
    % ok = ?LOG_INFO("Some sample ~p logging to print to the console.", [info]),

    ok.


do_log({_Location, _Time, _Pid, Level, _Msg} = _LogRequest) ->
    increment_counter(Level).


-record(state, {
    counters = [
        {info, 0},
        {warning, 0},
        {error, 0},
        {debug, 0}
    ]
}).

start_counter() ->
    Pid = spawn(?MODULE, counter, [#state{}]),
    erlang:register(counter, Pid).

increment_counter(Level) ->
    Pid = erlang:whereis(counter),
    Pid ! {increment, Level}.

get_counter(Level) ->
    timer:sleep(50),
    Pid = erlang:whereis(counter),
    Ref = erlang:make_ref(),
    Pid ! {self(), Ref, get_counter, Level},
    receive
        {Ref, Counter} -> Counter
    end.

counter(#state{counters=Counters} = State) ->
    NewState = receive
        {increment, Level} ->
            Value = proplists:get_value(Level, Counters),
            State#state{counters=[{Level, Value + 1} | lists:keydelete(Level, 1, Counters)]};
        {Pid, Ref, get_counter, Level} ->
            Pid ! {Ref, proplists:get_value(Level, Counters)},
            State
    end,
    counter(NewState).
