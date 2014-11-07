
-module(clamd_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

% start_child(Value, LeaseTime) ->
%     supervisor:start_child(?SERVER, [Value, LeaseTime]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Path = case application:get_env(clamd, path) of 
        undefined ->
            "etc";
        {ok, Value} ->
            Value
    end, 
	Pools = case yamerl_constr:file(Path++"clamd.yaml") of
		[Conf] ->
			element(2,lists:nth(1,Conf));
		X ->
			throw(io_lib:fwrite("Error parsing ecloud.yaml ~p",[X]))
	end,
    PoolSpecs = lists:map(fun([{"name",Name}, {"pool_size",SizeArgs}, {"hostname",Hostname} = Host, {"port",Port} = Port1]) ->
    	[{"size",SizeNum},{"max_overflow",MaxOverflowNum}] = hd(SizeArgs),
    	PoolArgs = [{name, {local, list_to_atom(Name)}},
            		{worker_module, clamd}] ++ [{size,SizeNum},{max_overflow,MaxOverflowNum}],
            		WorkerArgs = [{hostname, Hostname}, {port, Port}],
            		poolboy:child_spec(Name, PoolArgs, WorkerArgs)
    end,Pools),
    {ok, {{one_for_one, 10, 10}, PoolSpecs}}.

