-module(erel_manager_host).
-export([binding/1, init/2, handle_message/2, handle_cast/2, handle_call/3]).

-include_lib("erel_manager/include/erel_manager.hrl").

-record(state, { endpoint :: pid() }).

binding(_) ->
  #endp_binding{ name = erel, type = topic, topic = "erel.host" }.

init(Endpoint, _) ->
  {ok, #state{ endpoint = Endpoint }}.

handle_message(_, State) ->
  {ok, State}.

handle_cast(_, State) ->
  {noreply, State}.

handle_call({join, Hostname, Group}, _From, #state{ endpoint = Endpoint } = State) ->
  Groups = supervisor:which_children(erel_manager_group_sup),
  case lists:keyfind(Group, 1, Groups) of
    false -> %% this manager hasn't joined the group
      {ok, Pid} = supervisor:start_child(erel_manager_group_sup, esupervisor:spec(?endp_worker(Group, erel_manager_group, Group))),
      ?DBG("Started group handler for the group '~s', pid ~p",[Group, Pid]);
    {_, Pid, _, _} -> %% it already did
      ?DBG("Already started group handler for the group '~s', pid ~p", [Group, Pid])
  end,
  ?DBG("Broadcasting request for host '~s' to join group '~s'", [Hostname, Group]),
  erel_endp:cast(Endpoint, erel, "erel.host", {join, Hostname, Group}),
  {reply, ok, State}.
