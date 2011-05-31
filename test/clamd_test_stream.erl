-module(clamd_test_stream).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

multi_stream_test() ->
    {ok, First} = clamd_stream:start("localhost", 3310, nil),
    ok = clamd:chunk_stream(First, "X5O!P%@AP[4\\PZX54(P^)7CC)7}"),
    {ok, Second} = clamd_stream:start("localhost", 3310, nil),
    ok = clamd:chunk_stream(Second, "X5O!P%@AP[4\\PZX54(P^)7CC)7}"),
    ok = clamd:chunk_stream(First, "$EICAR-STANDARD-ANTIVIRUS-TEST-FILE!$H+H*"),
    ok = clamd:chunk_stream(Second, "$EICAR-STANDARD"),
    ok = clamd:chunk_stream(Second, "-ANTIVIRUS-TEST-FILE!$H+H*"),
    {ok, virus, _R1} = clamd:close_stream(First),
    {ok, virus, _R2} = clamd:close_stream(Second).
