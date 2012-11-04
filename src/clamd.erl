-module(clamd).

-behaviour(gen_server).
-behaviour(poolboy_worker).


%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2,
handle_info/2, terminate/2, code_change/3]).

%% poolboy callback
-export([start_link/1]).

%% public API
-export([
    ping/0,
    stats/0,
    version/0,
    scan/1,
    stream/1,
    file_wrapper/1
    ]).

-record(state, {socket, host, port}).

%%====================================================================
%% api callbacks
%%====================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, ["localhost", 3310], []).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%.s %--------------------------------------------------------------------
init([Host, Port]) ->
    {ok, #state{
            socket = nil,
            host = Host,
            port = Port
        }
    }.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({ping}, _From, State) ->
    {ok, #state{socket=Socket} = New_State} = connect(State),
    {reply, clamd_protocol:ask(Socket, "PING"), New_State};
handle_call({stats}, _From, State) ->
    {ok, #state{socket=Socket} = New_State} = connect(State),
    {reply, clamd_protocol:ask(Socket, "STATS"), New_State};
handle_call({version}, _From, State) ->
    {ok, #state{socket=Socket} = New_State} = connect(State),
    {reply, clamd_protocol:ask(Socket, "VERSION"), New_State};
handle_call({start_stream}, _From, State) ->
    {ok, #state{socket=Socket} = New_State} = connect(State),
    ok = clamd_protocol:start_stream(Socket),
    {reply, ok, New_State};
handle_call({chunk_stream, Chunk}, _From, #state{socket=Socket}=State) ->
    clamd_protocol:chunk_stream(Socket, Chunk),
    {reply, ok, State};
handle_call({end_stream}, _From, #state{socket=Socket}=State) ->
    R = clamd_protocol:end_stream(Socket),
    {reply, R, State};
handle_call({scan, Path}, _From, State) ->
    {ok, #state{socket=Socket} = New_State} = connect(State),
    {reply, clamd_protocol:scan(Socket, Path), New_State};
handle_call(Msg, _From, State) ->
    io:format("call : ~p~n", [Msg]),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #state{socket=Socket}) ->
    gen_tcp:close(Socket).

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% API functions
%%--------------------------------------------------------------------


ping() ->
    poolboy:transaction(clamd_pool, fun(Worker) ->
                gen_server:call(Worker, {ping})
        end).

stats() ->
    poolboy:transaction(clamd_pool, fun(Worker) ->
                gen_server:call(Worker, {stats})
        end).

version() ->
    poolboy:transaction(clamd_pool, fun(Worker) ->
                gen_server:call(Worker, {version})
        end).

scan(Path) ->
    poolboy:transaction(clamd_pool, fun(Worker) ->
                gen_server:call(Worker, {scan, Path})
        end).

stream(Chunks) when is_list(Chunks) ->
    poolboy:transaction(clamd_pool, fun(Worker) ->
                ok = gen_server:call(Worker, {start_stream}),
                lists:foreach(fun(T) ->
                            ok = gen_server:call(Worker, {chunk_stream, T})
                    end, Chunks),
                gen_server:call(Worker, {end_stream})
        end);
stream({Reader, State}) ->
    poolboy:transaction(clamd_pool, fun(Worker) ->
                ok = gen_server:call(Worker, {start_stream}),
                read_chunk(Worker, Reader, State),
                gen_server:call(Worker, {end_stream})
        end).

read_chunk(Worker, Reader, State) ->
    case Reader(State) of
        eof -> ok;
        {ok, Chunk, NewState} ->
            ok = gen_server:call(Worker, {chunk_stream, Chunk}),
            read_chunk(Worker, Reader, NewState)
    end.

file_wrapper(Path) ->
    F = fun(State) ->
            case State of
                {open, Path} ->
                    {ok, Fd} = file:open(Path, [read]),
                    {ok, Chunk} = file:read(Fd, 128),
                    {ok, Chunk, {reading, Fd}};
                {reading, Fd} ->
                    R = file:read(Fd, 128),
                    case R of
                        eof -> eof;
                        {ok, Chunk} ->
                            {ok, Chunk, {reading, Fd}}
                    end
            end
    end,
    {F, {open, Path}}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

connect(#state{host=Host, port=Port} = State) ->
    case gen_tcp:connect(Host, Port, [list, {packet, raw}, {active, false}]) of
        {ok, Socket} ->
            {ok, State#state{
                socket = Socket,
                host = Host,
                port = Port
            }};
        {error, Reason} -> {stop, Reason}
    end.
