-module(clamd_virus).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

ping_test() ->
    application:start(clamd),
    {ok, _} = clamd:ping().
stream_test() ->
    % application:start(clamd),
    {ok, Pid} = clamd:open_stream(),
    clamd_stream:chunk(Pid, "X5O!P%@AP[4\\PZX54(P^)7CC)7}"),
    clamd_stream:chunk(Pid, "$EICAR-STANDARD-ANTIVIRUS-TEST-FILE!$H+H*"),
    clamd_stream:finish(Pid).
    