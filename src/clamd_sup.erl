
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
    Value = case application:get_env(clamd, vals) of 
        undefined ->
            case yamerl_constr:file("etc/clamd.yaml") of
                [Conf] ->
                    Conf;
                X ->
                    throw(io_lib:fwrite("Error parsing clamd.yaml ~p",[X]))
            end;
        {ok, [Conf]} ->
            Conf
    end, 
    Pools = element(2,lists:nth(1,Value)),
    PoolSpecs = lists:map(fun([{"name",Name}, {"pool_size",SizeArgs}, {"hostname",Hostname} = Host, {"port",Port} = Port1]) ->
    	[{"size",SizeNum},{"max_overflow",MaxOverflowNum}] = hd(SizeArgs),
    	PoolArgs = [{name, {local, list_to_atom(Name)}},
            		{worker_module, clamd}] ++ [{size,SizeNum},{max_overflow,MaxOverflowNum}],
            		WorkerArgs = [{hostname, Hostname}, {port, Port}],
            		poolboy:child_spec(Name, PoolArgs, WorkerArgs)
    end,Pools),
    {ok, {{one_for_one, 10, 10}, PoolSpecs}}.

