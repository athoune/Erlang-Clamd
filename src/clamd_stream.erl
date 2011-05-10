-module(clamd_stream).

-export([init/1]).

-record(state, {socket, host, port}).

init([Host, Port]) ->
    {ok, Socket} = gen_tcp:connect(Host, Port, [list, {packet, raw}, {active, false}]),
    {ok, #state{socket=Socket, host=Host, port=Port}}.
    