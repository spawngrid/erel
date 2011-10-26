-module(erel_master_file_receiver).
-behaviour(gen_server).

-include_lib("erel_master/include/erel_master.hrl").

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, { group_handler :: pid(), crc :: integer(), chunks :: integer(),
    expected_chunk = 1 :: integer(), out_of_order_chunks = [] :: list({integer(), binary()}) }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(GroupHandler, Crc, Chunks) ->
        gen_server:start_link(?MODULE, {GroupHandler, Crc, Chunks}, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init({GroupHandler, Crc, Chunks}) ->
        {ok, #state{
              group_handler = GroupHandler, crc = Crc, 
              chunks = Chunks
            }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
        {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({chunk, Crc, Chunk, Chunks, ChunkSize, Part}, 
  #state{ group_handler = GroupHandler,
          crc = Crc, 
          chunks = Chunks,
          out_of_order_chunks = OOO,
          expected_chunk = ExpectedChunk } = State) when ExpectedChunk == Chunk andalso  
                                                        ChunkSize == size(Part) ->
  write_chunk(Crc, Part),
  case Chunks == Chunk of
    true ->
      ?INFO("Finished receiving file"),
      gen_server:cast(GroupHandler, {received, Crc, filename(Crc)}),
      {stop, normal, State#state{ expected_chunk = 1 }};
    false ->
      ?DBG("Received chunk ~p",[Chunk]),
      %% write pending out of order chunks
      OOOSorted = lists:usort(fun ({C1, _},{C2,_}) -> C1 =< C2 end, OOO),
      ToWrite = lists:reverse(element(2, 
                lists:foldl(fun ({N, _}=C,{I,Acc}) when N-1 == I -> {N, [C|Acc]}; (_, Acc) -> Acc end, {Chunk, []}, OOOSorted))),
      length(ToWrite) > 0 andalso ?DBG("Writing pending chunks ~w",[lists:map(fun({N,_}) -> N end, ToWrite)]),
      [ write_chunk(Crc, ChunkPart) || {_, ChunkPart} <- ToWrite ], 
      NewExpectation = case ToWrite of 
        [] -> ExpectedChunk + 1;
        [_|_] -> {N, _} = hd(lists:reverse(ToWrite)), N+1
      end,
      case NewExpectation - 1 of
        Chunks ->
          ?INFO("Finished receiving file"),
          gen_server:cast(GroupHandler, {received, Crc, filename(Crc)}),
          {stop, normal, State#state{ expected_chunk = 1 }};
        _ ->
          {noreply, State#state{ expected_chunk = NewExpectation, out_of_order_chunks = OOO -- ToWrite}}
      end
  end;

handle_cast({chunk, Crc, Chunk, ExpectedChunks, ChunkSize, Part}, 
  #state{ crc = Crc, expected_chunk = ExpectedChunk,
          chunks = ExpectedChunks,
          out_of_order_chunks = Chunks } = State) when ChunkSize == size(Part) ->
  ?DBG("Received an out of order chunk ~p, was expecting ~p",[Chunk, ExpectedChunk]),
  case Chunks of
    [] ->
      EarliestChunk = none;
    [_|_] ->
      EarliestChunk = lists:min(lists:map(fun ({C,_}) -> C end, Chunks))
  end, 
  case (EarliestChunk == ExpectedChunk) of
    true ->
      ?DBG("Found a predecessor chunk for ~p, writing both",[Chunk]),
      write_chunk(Crc, proplists:get_value(EarliestChunk, Chunks)),
      write_chunk(Crc, Part),
      {noreply, State#state{ out_of_order_chunks = lists:keydelete(EarliestChunk, 1, Chunks) }};
    false -> 
      {noreply, State#state{ out_of_order_chunks = [{Chunk, Part}|Chunks] }}
  end;


handle_cast(Msg, State) ->
        ?DBG("Invalid message received: ~p",[Msg]),
        {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
        {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
        ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
write_chunk(Crc, Binary) ->
  Filename = filename(Crc),
  filelib:ensure_dir(Filename),
  {ok, File} = file:open(Filename, [append, binary]),
  ok = file:write(File, Binary),
  file:close(File),
  ok.

filename(Crc) ->
  CrcS = integer_to_list(Crc),
  case application:get_env(erel_master, dir) of
    {ok, Dir} ->
      ok;
    _ ->
      {ok, Dir} = file:get_cwd()
  end,
  filename:absname(filename:join([Dir, "files", CrcS, "file.zip"])).
 
