
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
	Value = case application:get_env(clamd, settings) of 
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
    {ok, { {one_for_one, 10, 10}, [
                poolboy:child_spec(clamd_pool, [
                        {name, {local, clamd_pool}},
                        {worker_module, clamd},
                        {size, element(2,lists:keyfind("worker_size", 1, Value))},
                        {max_overflow, element(2,lists:keyfind("max_overflow", 1, Value))}], 
                        [element(2,lists:keyfind("host", 1, Value)), 
                        element(2,lists:keyfind("port", 1, Value))])
            ]} }.

