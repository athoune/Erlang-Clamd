-module(clamd_virus).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

ping_test() ->
    application:start(clamd),
    {ok, _} = clamd:ping().
stream_test() ->
    % application:start(clamd),
    clamd:stream("X5O!P%@AP[4\\PZX54(P^)7CC)7}"),
    clamd:stream("$EICAR-STANDARD-ANTIVIRUS-TEST-FILE!$H+H*"),
    clamd:stream("").
eicar_test() ->
    clamd:stream("EICAR"),
    clamd:stream("").
    