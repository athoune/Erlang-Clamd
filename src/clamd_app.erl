-module(clamd_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(StartType, StartArgs) ->
	application:start(yamerl),
	io:fwrite("_StartType   ~p~n  StartArgs   ~p~n   ",[StartType, StartArgs]),
    clamd_sup:start_link().

stop(_State) ->
    ok.
