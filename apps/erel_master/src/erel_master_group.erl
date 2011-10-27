-module(erel_master_group).
-export([binding/1, init/2, handle_message/2, handle_cast/2, handle_call/3]).

-include_lib("erel_master/include/erel_master.hrl").

-record(state, { endpoint :: pid(), hostname :: string(), group :: string(), topic :: string(),
                 received :: list({term(), string()}) }).

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
  {ok, State};

handle_message({announce, Hostname}, #state{ hostname = Hostname } = State) -> %% our own announce message, ignore
  {ok, State};
handle_message({announce, Hostname}, #state{} = State) -> %% some other host's announce message, ignore
  {ok, State};

handle_message({transfer, Id, Attributes, Crc, Chunks}, #state{} = State) ->
  ?DBG("New transfer with id ~p and crc32 of ~p", [Id, Crc]),
  case supervisor:start_child(erel_master_file_receiver_sup, 
    esupervisor:spec(#worker{ id = Id, modules = [erel_master_file_receiver],
      start_func = { erel_master_file_receiver, start_link, [self(), Id, Crc, Chunks] }, 
      restart = transient })) of 
    {ok, Pid} ->
       ?DBG("Started file receiver for id ~p, crc32 of ~p, pid ~p", [Id, Crc, Pid]);
    {error, already_present} -> %% stale name to be removed
      supervisor:delete_child(erel_master_file_receiver_sup, Id),
      handle_message({transfer, Id, Attributes, Crc, Chunks}, State);
    {error, {already_started, Pid}} ->
       ?DBG("File receiver for id ~p, crc32 of ~p was already started, pid ~p",[Id, Crc, Pid])
   end,
  {ok, State}; 

handle_message({chunk, Id, Crc, Chunk, Chunks, ChunkSize, Part}=Msg, #state{} = State) ->
  Receivers = supervisor:which_children(erel_master_file_receiver_sup),
  case lists:keyfind(Id, 1, Receivers) of 
    false -> %% missing receiver
      {ok, Pid} = supervisor:start_child(erel_master_file_receiver_sup, 
        esupervisor:spec(#worker{ id = Id, modules = [erel_master_file_receiver],
            start_func = { erel_master_file_receiver, start_link, [self(), Id, Crc, Chunks] }, 
      restart = transient })),
     ?DBG("Started file receiver for id ~p, crc32 of ~p, pid ~p", [Id, Crc, Pid]),
     gen_server:cast(Pid, Msg);
    {_, undefined, _, _} -> %% stale spec
       supervisor:delete_child(erel_master_file_receiver_sup, Id),
       handle_message(Msg, State);
    {_, Pid, _, _} -> %% found it
      gen_server:cast(Pid, Msg)
  end,
  {ok, State};

handle_message(list_releases, #state{ endpoint = Endpoint, topic = Topic, hostname = Hostname } = State) ->
  erel_endp:cast(Endpoint, erel, Topic, {list_releases, [], Hostname}),
  {ok, State};

handle_message({provision_release, Release}, #state{ endpoint = Endpoint, topic = Topic, hostname = Hostname, received = Received } = State) ->
  Path = proplists:get_value(Release, Received),
  erel_master_releases:provision(Release, Path),
  ?INFO("Release '~s' has been provisioned", [Release]),
  erel_endp:cast(Endpoint, erel, Topic, {{provision_release, Release}, Hostname}),
  {ok, State};

handle_message(Message, #state{ group = Group } = State) ->
  {ok, State}.

handle_cast({received, Id, Crc, Filename}, #state{ endpoint = Endpoint, topic = Topic,
                                                   received = Received, hostname = Hostname } = State) ->
  ?DBG("File with crc32 of ~p has been fully received and saved to ~s", [Crc, Filename]),
  erel_endp:cast(Endpoint, erel, Topic, {{received, Id}, Hostname}), 
  {noreply, State#state{ received = [{Id, Filename}|Received] }};

handle_cast(_, State) ->
  {noreply, State}.

handle_call(_, _, State) ->
  {noreply, State}.

%% internal

group_topic(Group) ->
  "erel.group." ++ Group.
