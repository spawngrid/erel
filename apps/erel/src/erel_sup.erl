-module(erel_sup).
-include_lib("esupervisor/include/esupervisor.hrl").
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
    {ok, AppName} = application:get_key(erel, description),
    {ok, AvailableEndpoints} = application:get_env(erel, available_endpoints),
    {ok, Endpoints} = application:get_env(erel, endpoints),
    EndpointSups = [ #worker{ 
                        id = proplists:get_value(Endpoint, AvailableEndpoints) 
                       } || Endpoint <- Endpoints ],
    case init:script_id() of
        {AppName, _Version} ->
            #one_for_one{
              children = [
                          #worker{
                             id = erel_net_manager
                            },
                          #one_for_one{
                             id = erel_instance_sup,
                             registered = erel_instance_sup
                          },
                          #worker{
                                   id = erel_release_manager
                            }
                          | 
                          EndpointSups
                         ]
            };
        _ ->
            #one_for_one{}
    end.

