-module(erel_manager_sup).
-include_lib("erel_manager/include/erel_manager.hrl").
-behaviour(esupervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    esupervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->
  #one_for_one{
    children = [
      #one_for_one{
        id = erel_manager_group_sup,
        registered = erel_manager_group_sup
      },
      ?endp_worker(erel_manager_host, erel_manager_host, undefined)
    ]
  }.
