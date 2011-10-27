-module(erel_manager).
-export([group_join/2, group_part/2, group_transfer/4]).

-include_lib("erel_manager/include/erel_manager.hrl").

group_join(Group, Hostname) ->
  Children = supervisor:which_children(erel_manager_sup), 
  {_, Pid, _, _} = lists:keyfind(erel_manager_host, 1, Children),
  gen_server:call(Pid, {join, Hostname, Group}).

group_part(Group, Hostname) ->
  Children = supervisor:which_children(erel_manager_sup), 
  {_, Pid, _, _} = lists:keyfind(erel_manager_host, 1, Children),
  gen_server:call(Pid, {part, Hostname, Group}).

group_transfer(Id, Group, Path, Attributes) ->
  Groups = supervisor:which_children(erel_manager_group_sup),
  case lists:keyfind(Group, 1, Groups) of
    false -> %% this manager hasn't joined the group
      {ok, Pid} = supervisor:start_child(erel_manager_group_sup, esupervisor:spec(?endp_worker(endpoint(), Group, erel_manager_group, Group))),
      ?DBG("Started group handler for the group '~s', pid ~p",[Group, Pid]);
    {_, Pid, _, _} -> %% it already did
      ?DBG("Already started group handler for the group '~s', pid ~p", [Group, Pid])
  end,
  gen_server:call(Pid, {transfer, Id, Path, Attributes}, infinity).
 
%% Internal
endpoint() ->
  {ok, Endpoint} = application:get_env(erel_manager, endpoint),
  Endpoint.
