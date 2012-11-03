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
    open_stream/0,
    chunk_stream/2,
    close_stream/1,
    message/1,
    response/1]).

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
%%--------------------------------------------------------------------
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
    {reply, ask(Socket, "PING"), New_State};
handle_call({stats}, _From, State) ->
    {ok, #state{socket=Socket} = New_State} = connect(State),
    {reply, ask(Socket, "STATS"), New_State};
handle_call({version}, _From, State) ->
    {ok, #state{socket=Socket} = New_State} = connect(State),
    {reply, ask(Socket, "VERSION"), New_State};
handle_call({open_stream}, _From, #state{
        host= Host, port=Port} = State) ->
    R = clamd_stream:start(Host, Port, self()),
    {reply, R, State};
% handle_call({scan, _Path}, _From, #state{socket=Socket} = State) ->
%     {reply, ok, State};
handle_call({finished}, _From, State) ->
    {reply, ok, State};
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
                io:format("ping transaction ~p~n", [Worker]),
                gen_server:call(Worker, {ping})
        end).

stats() ->
    gen_server:call(?MODULE, {stats}).

version() ->
    gen_server:call(?MODULE, {version}).

scan(Path) ->
    gen_server:call(?MODULE, {scan, Path}).

open_stream() ->
    gen_server:call(?MODULE, {open_stream}).

chunk_stream(Pid, Chunk) ->
    gen_server:call(Pid, {chunk, Chunk}).

close_stream(Pid) ->
    R = gen_server:call(Pid, {finish}),
    Pid ! 'EXIT',
    R.

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

% https://wiki.clamav.net/Main/UpgradeNotes095

message(Action) ->
    "z" ++ Action ++ [0].

% Ask something to clamd and retuen response
ask(Socket, Action) ->
    gen_tcp:send(Socket, message(Action)),
    response(Socket).

response(Socket) ->
    response(Socket, []).
response(Socket, Acc) ->
    case gen_tcp:recv(Socket, 1) of
        {ok, Packet} ->
            case Packet of
                [0] -> {ok, Acc};
                _ -> response(Socket, Acc ++ Packet)
            end;
        {error, Reason} -> {error, Reason}
    end.

