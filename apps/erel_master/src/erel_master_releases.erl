-module(erel_master_releases).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([provision/2, releases/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, { erels = [] :: list({string(), atom()})}).

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
start_link() ->
        gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

provision(Release, Path) ->
  gen_server:call(?SERVER, {provision, Release, Path}, infinity).

releases() ->
  gen_server:call(?SERVER, releases).

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
init([]) ->
        {ok, #state{}}.

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
handle_call({provision, Release, Path}, From, #state{ erels = Erels } = State) ->
  Dir = filename:join([erel_master:directory(), "releases", Release]),
  filelib:ensure_dir(Dir ++ "/"),
  ok = erl_tar:extract(Path, [{cwd, Dir}]),
  file:delete(Path),
  %% start erel
  {ok, [Releases]} = file:consult(filename:join([Dir, "releases", "RELEASES"])),
  Rel = {release, "erel", Version, _Erts, _Deps, permanent} = lists:keyfind("erel", 2, Releases),

  Erl = filename:join([Dir, "erts-" ++ erel_release:erts_version(Dir), "bin",
          "erl"]),

  Boot = filename:join([Dir, "releases", Version, "erel"]),
  NodeName = binary_to_list(ossp_uuid:make(v4, text)) ++ "@" ++ erel_net_manager:hostname(),
  NodeAtom = list_to_atom(NodeName),
  Cookie = erel_net_manager:cookie(NodeAtom),
  erlang:set_cookie(NodeAtom, Cookie),
  net_kernel:monitor_nodes(true),
  Port = open_port({spawn_executable, Erl},[{args, ["-detached","-boot", Boot,
          "-name", NodeName, "-eval", "erlang:set_cookie('" ++
          atom_to_list(node()) ++ "','" ++ atom_to_list(Cookie) ++ "'),"
          "pong=net_adm:ping('" ++ atom_to_list(node()) ++"')."]}]),
  port_connect(Port, whereis(application_controller)),
  unlink(Port),
  receive 
     {nodeup, NodeAtom} -> net_kernel:monitor_nodes(false),
       {reply, ok, State#state{ erels = [{Release, NodeAtom}|Erels] }}
  after 1000*60 ->
       {reply, {error, timeout}, State}
  end;

handle_call(releases, _From, #state{ erels = Erels } = State) ->
  Reply = lists:flatten([ gen_server:call({erel_release_manager, Node}, releases) || {_, Node} <- Erels ]),
  {reply, Reply, State}.

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
handle_cast(_Msg, State) ->
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




