-module(clamd_stream).

-behaviour(gen_server).

%% gen_server callbacks
-export([start_link/2, init/1, handle_call/3, handle_cast/2, 
handle_info/2, terminate/2, code_change/3]).

%% API
-export([
    chunk/1,
    finish/0]).

-record(state, {socket, host, port}).

start_link(Host, Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Host, Port], []).

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
    {ok, Socket} = gen_tcp:connect(Host, Port, [list, {packet, raw}, {active, false}]),
    io:format("Socket : ~w~n", [Socket]),
    gen_tcp:send(Socket, clamd:message("INSTREAM")),
    io:format("INSTREAM~n"),
    {ok, #state{socket=Socket, host=Host, port=Port}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({chunk, Bucket}, _From, #state{socket=Socket} = State) ->
    Size = length(Bucket),
    gen_tcp:send(Socket, <<Size:32/big>>),
    gen_tcp:send(Socket, Bucket),
    {reply, ok, State};
handle_call({finish}, _From, #state{socket=Socket} = State) ->
    gen_tcp:send(Socket,[0,0,0,0]),
    {ok, R} = clamd:response(Socket),
    {reply, {ok, R}, State};
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

%% API

chunk(Chunk) ->
    gen_server:call(?MODULE, {chunk, Chunk}).

finish() ->
    gen_server:call(?MODULE, {finish}).
