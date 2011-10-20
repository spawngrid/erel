-module(erel_master_host).
-export([binding/1, init/2, handle_message/2, handle_cast/2, handle_call/3]).

-include_lib("erel_master/include/erel_master.hrl").

-record(state, { endpoint :: pid(), hostname :: string() }).

binding(_) ->
  #endp_binding{ name = erel, type = topic, topic = "erel.host"}.

init(Endpoint, _) ->
  Hostname = erel_master:hostname(),
  gen_server:cast(Endpoint, announce),
  ?INFO("Joined as a host '~s'", [Hostname]),
  {ok, #state{ endpoint = Endpoint , hostname = Hostname}}.

handle_message({join, Hostname, Group}, #state{ hostname = Hostname } = State) ->
  ?INFO("Group join request received, group name '~s'", [Group]),
  Groups = supervisor:which_children(erel_master_group_sup),
  case lists:keyfind(Group, 1, Groups) of
    false -> %% this master hasn't joined the group
      {ok, Pid} = supervisor:start_child(erel_master_group_sup, esupervisor:spec(?endp_worker(Group, erel_master_group, Group))),
      ?DBG("Started group handler for the group '~s', pid ~p",[Group, Pid]);
    {_, Pid, _, _} -> %% it already did
      ?DBG("Already started group handler for the group '~s', pid ~p", [Group, Pid])
  end,
  {ok, State};

handle_message({join, Hostname, Group}, #state{} = State) ->
  ?INFO("Group join request received for the group '~s' for the other host '~s', ignoring", [Group, Hostname]),
  {ok, State};

handle_message({host, Hostname, Attributes}, #state{} = State) ->
  ?INFO("Host '~s' joined with following attributes: ~p", [Hostname, Attributes]),
  {ok, State};

handle_message(_, #state{} = State) ->
  {ok, State}.

handle_cast(announce, #state{ endpoint = Endpoint, hostname = Hostname } = State) ->
  erel_endp:cast(Endpoint, erel, "erel.host", {host, Hostname, host_attributes()}),
  {noreply, State}.

handle_call(_, _, State) ->
  {noreply, State}.

%% internal

host_attributes() ->
  [{os_type, os:type()},
   {os_version, os:version()},
   {wordsize, memsup:get_os_wordsize()},
   {memory, proplists:get_value(system_total_memory, memsup:get_system_memory_data())},
   {cores, erlang:system_info(logical_processors_online)}
  ].
