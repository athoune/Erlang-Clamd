-module(clamd_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(StartType, StartArgs) ->
	application:start(yamerl),
    clamd_sup:start_link().

stop(_State) ->
    ok.
