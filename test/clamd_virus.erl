-module(clamd_virus).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

ping_test() ->
    application:start(clamd),
    {ok, _} = clamd:ping().

stream_test() ->
    {ok, virus, _} = clamd:stream([
     "X5O!P%@AP[4\\PZX54(P^)7CC)7}",
     "$EICAR-STANDARD-ANTIVIRUS-TEST-FILE!$H+H*"]).

file_test() ->
    {ok, virus, _} = clamd:stream(clamd:file_wrapper("../test/test.virus")).

scan_test() ->
    {ok, Cwd} = file:get_cwd(),
    F = filename:split(Cwd),
    F1 = lists:sublist(F, length(F) -1),
    {ok, no_virus} = clamd:scan(Cwd),
    {ok, virus, "Eicar-Test-Signature", _} = clamd:scan(filename:join(F1 ++ ["test", "test.virus"])).
