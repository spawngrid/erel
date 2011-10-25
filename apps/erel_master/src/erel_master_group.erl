-module(erel_master_group).
-export([binding/1, init/2, handle_message/2, handle_cast/2, handle_call/3]).

-include_lib("erel_master/include/erel_master.hrl").

-record(state, { endpoint :: pid(), hostname :: string(), group :: string(), topic :: string() }).

binding(Group) ->
  #endp_binding{ name = erel, type = topic, topic = group_topic(Group) }.

init(Endpoint, Group) ->
  Hostname = erel_master:hostname(),
  erel_endp:cast(Endpoint, erel, group_topic(Group), {announce, Hostname}),
  ?INFO("Announced host '~s' to the group '~s'", [Hostname, Group]),
  {ok, #state{ endpoint = Endpoint, hostname = Hostname, group = Group, topic = group_topic(Group) }}.

handle_message(ping, #state{ endpoint = Endpoint, hostname = Hostname, group = Group, topic = Topic } = State) ->
  ?INFO("Group ('~s') ping received, replying", [Group]),
  erel_endp:cast(Endpoint, erel, Topic, {pong, Hostname}),
  {noreply, State};

handle_message({announce, Hostname}, #state{ hostname = Hostname } = State) -> %% our own announce message, ignore
  {ok, State};
handle_message({announce, Hostname}, #state{} = State) -> %% some other host's announce message, ignore
  {ok, State};

handle_message({deploy, Attributes, Crc, Chunks}, #state{} = State) ->
  ?DBG("New deployment with crc32 of ~p", [Crc]),
  case supervisor:start_child(erel_master_file_receiver_sup, 
    esupervisor:spec(#worker{ id = Crc, modules = [erel_master_file_receiver],
      start_func = { erel_master_file_receiver, start_link, [self(), Crc, Chunks] }, 
      restart = transient })) of 
    {ok, Pid} ->
       ?DBG("Started file receiver for crc32 of ~p, pid ~p", [Crc, Pid]);
    {error, {already_started, Pid}} ->
       ?DBG("File receiver for crc32 of ~p was already started, pid ~p",[Crc, Pid])
   end,
  {ok, State}; 

handle_message({chunk, Crc, Chunk, Chunks, ChunkSize, Part}=Msg, #state{} = State) ->
  Receivers = supervisor:which_children(erel_master_file_receiver_sup),
  case lists:keyfind(Crc, 1, Receivers) of 
    false -> %% missing receiver
      {ok, Pid} = supervisor:start_child(erel_master_file_receiver_sup, 
        esupervisor:spec(#worker{ id = Crc, modules = [erel_master_file_receiver],
            start_func = { erel_master_file_receiver, start_link, [self(), Crc, Chunks] }, 
      restart = transient })),
     ?DBG("Started file receiver for crc32 of ~p, pid ~p", [Crc, Pid]),
     gen_server:cast(Pid, Msg);
    {_, Pid, _, _} -> %% found it
      gen_server:cast(Pid, Msg)
  end,
  {ok, State};

handle_message(Message, #state{ group = Group } = State) ->
  ?WARNING("Unexpected message received in the group '~s': ~p",[Group, Message]),
  {ok, State}.

handle_cast(_, State) ->
  {noreply, State}.

handle_call(_, _, State) ->
  {noreply, State}.

%% internal

group_topic(Group) ->
  "erel.group." ++ Group.
