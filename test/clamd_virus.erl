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

pool_test() ->
    clamd:transaction(fun(Worker) ->
        clamd:start_stream(Worker),
        clamd:chunk_stream(Worker, "X5O!P%@AP[4\\PZX54(P^)7CC)7}"),
        clamd:chunk_stream(Worker, "$EICAR-STANDARD-ANTIVIRUS-TEST-FILE!$H+H*"),
        {ok, virus, "Eicar-Test-Signature"} = clamd:end_stream(Worker)
    end).

file_test() ->
    {ok, virus, _} = clamd:stream(clamd:file_wrapper("../test/test.virus")).

scan_test() ->
    {ok, Cwd} = file:get_cwd(),
    F = filename:split(Cwd),
    F1 = lists:sublist(F, length(F) -1),
    {ok, no_virus} = clamd:scan(Cwd),
    {ok, virus, "Eicar-Test-Signature", _} = clamd:scan(filename:join(F1 ++ ["test", "test.virus"])).
