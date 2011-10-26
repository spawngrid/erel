-module(erel_manager_group).
-export([binding/1, init/2, handle_message/2, handle_cast/2, handle_call/3]).

-include_lib("erel_manager/include/erel_manager.hrl").

-record(state, { endpoint :: pid(), group :: string() }).

binding(Group) ->
  #endp_binding{ name = erel, type = topic, topic = group_topic(Group) }.

init(Endpoint, Group) ->
  ?DBG("Joined the group '~s'", [Group]),
  {ok, #state{ endpoint = Endpoint, group = Group }}.

handle_message(_Message, #state{} = State) ->
  {ok, State}.

handle_cast(_, State) ->
  {noreply, State}.

handle_call({transfer, Id, Path, Attributes}, From, #state{ group = Group, endpoint = Endpoint } = State) ->
  ?DBG("Transfering ~s to group '~s'", [Path, Group]),
  spawn_link(fun () -> transfer(From, Endpoint, Id, Group, Path, Attributes) end),
  {noreply, State};

handle_call(_, _, State) ->
  {noreply, State}.

%% internal

group_topic(Group) ->
  "erel.group." ++ Group.

-define(CHUNK_SIZE, (128*1024)).
transfer(From, Endpoint, Id, Group, Path, Attributes) ->
  ?DBG("Creating zip file"),
  {ok, Zip} = create_zip(Path),
  Crc = erlang:crc32(Zip),
  Size = byte_size(Zip),
  Chunks = Size div ?CHUNK_SIZE,
  Remainder = Size rem ?CHUNK_SIZE,
  ?DBG("Sending ~p chunks of the file with crc32 ~p (total size ~p)", [Chunks + 1, Crc, Size]),
  erel_endp:cast(Endpoint, erel, group_topic(Group), {transfer, Id, Attributes, Crc, Chunks + 1}),
  lists:foldl(fun(Chunk, Offset) -> 
        Part = binary:part(Zip, Offset, ?CHUNK_SIZE),
        erel_endp:cast(Endpoint, erel, group_topic(Group), {chunk, Id, Crc, Chunk, Chunks + 1, ?CHUNK_SIZE, Part}),
        Offset + ?CHUNK_SIZE
    end, 0, lists:seq(1, Chunks)),
  Part = binary:part(Zip, Size - Remainder, Remainder),
  erel_endp:cast(Endpoint, erel, group_topic(Group), {chunk, Id, Crc, Chunks + 1, Chunks + 1, Remainder, Part}),
  gen_server:reply(From, ok).

create_zip(Path) ->
    {ok, {_, Binary}} = zip:create("erel.zip", ["."], [memory, {compress, all}, {cwd, Path}]),
    {ok, Binary}.
