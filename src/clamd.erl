-module(clamd_clamscan).

-behaviour(gen_server).

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, 
handle_info/2, terminate/2, code_change/3]).

%% public API
-export([ping/0]).

-record(state, {socket}).

%%====================================================================
%% api callbacks
%%====================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, ["localhost", 3310], []).

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
    case gen_tcp:connect(Host, Port, [binary, {packet, raw}, {active, false}]) of
        {ok, Socket} ->
            % ping(),
            {ok, #state{
                socket = Socket
            }};
        {error, Reason} -> {stop, Reason}
    end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({ping}, _From, #state{socket=Socket} = State) ->
    gen_tcp:send(Socket, "zPING" ++ 0),
    case gen_tcp:recv(Socket, 5) of
        {ok, <<"PONG">>} ->
            {reply, ok, State};
        {ok, Packet} ->
            io:format("~p", [Packet]),
            {reply, error, State};
        {error, Reason} ->
            io:format("error: ~w~n", [Reason]),
            {reply, error, State}
    end;
handle_call({scan, _Path}, _From, #state{socket=Socket} = State) ->
    gen_tcp:send(Socket, ""),
    {reply, ok, State};
handle_call({stream, _Path}, _From, State) ->
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
%%% Internal functions
%%--------------------------------------------------------------------

ping() ->
    gen_server:call(?MODULE, {ping}).