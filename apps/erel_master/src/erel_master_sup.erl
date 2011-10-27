-module(erel_master_sup).
-include_lib("erel_master/include/erel_master.hrl").
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
      #worker{
        id = erel_net_manager
      },
      #worker{
        id = erel_master_releases
      },
      #one_for_one{
        id = erel_master_group_sup,
        registered = erel_master_group_sup
      },
      #one_for_one {
        id = erel_master_file_receiver_sup,
        registered = erel_master_file_receiver_sup
      },
      ?endp_worker(erel_master_host, erel_master_host, undefined)
    ]
  }.
