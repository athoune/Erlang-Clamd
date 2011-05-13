-module(clamd_test_stream).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

stream_test() ->
    {ok, Pid} = clamd_stream:start_link("localhost", 3310),
    io:format("~w~n", [Pid]),
    ok = clamd_stream:chunk("X5O!P%@AP[4\\PZX54(P^)7CC)7}"),
    ok = clamd_stream:chunk("$EICAR-STANDARD-ANTIVIRUS-TEST-FILE!$H+H*"),
    clamd_stream:finish().
