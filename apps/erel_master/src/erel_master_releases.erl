-module(erel_master_releases).

-behaviour(gen_server).
-include_lib("erel_master/include/erel_master.hrl").
%% API
-export([start_link/0]).
-export([provision/2, releases/0, start/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, { nodes = [] :: list({string(), atom()}), releases = [] :: list({string(), term()})}).

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

start(Release) ->
  gen_server:call(?SERVER, {start, Release}, infinity).

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
handle_call({provision, Release, Path}, From, #state{ releases = Releases } = State) ->
  Dir = filename:join([erel_master:directory(), "releases", Release]),
  filelib:ensure_dir(Dir ++ "/"),
  ok = erl_tar:extract(Path, [{cwd, Dir}]),
  file:delete(Path),
  %% extract version
  {ok, [Rels]} = file:consult(filename:join([Dir, "releases", "RELEASES"])),
  {release, Release, Version, _, _, _} = lists:keyfind(Release, 2, Rels),
  {reply, ok, State#state{ releases = [{Release, Version}|Releases] }};

handle_call(releases, _From, #state{ releases = Releases } = State) ->
  {reply, Releases, State};

handle_call({start, Release}, _From, #state{ releases = Releases, nodes = Nodes } = State) ->
  Version = proplists:get_value(Release, Releases),
  Dir = filename:join([erel_master:directory(), "releases", Release]),
  BinDir = filename:join([Dir, "erts-" ++ erel_release:erts_version(Dir), "bin"]), 
  Erl = filename:join([BinDir, "erlexec"]),

  Boot = filename:join([Dir, "releases", Version, Release]),
  NodeName = binary_to_list(ossp_uuid:make(v4, text)) ++ "@" ++ erel_net_manager:hostname(),
  NodeAtom = list_to_atom(NodeName),
  Cookie = erel_net_manager:cookie(NodeAtom),
  erlang:set_cookie(NodeAtom, Cookie),
  net_kernel:monitor_nodes(true),
  Port = open_port({spawn_executable, Erl},[
          {env, [{"ROOTDIR",Dir}, {"BINDIR", BinDir}, {"EMU","beam"}]},
          {args, ["-detached","-boot", Boot,
          "-name", NodeName, "-eval", "erlang:set_cookie('" ++
          atom_to_list(node()) ++ "','" ++ atom_to_list(Cookie) ++ "'),"
          "pong=net_adm:ping('" ++ atom_to_list(node()) ++"')."]}]),
  port_connect(Port, whereis(application_controller)),
  unlink(Port),
  receive 
     {nodeup, NodeAtom} -> net_kernel:monitor_nodes(false),
       {reply, ok, State#state{ nodes = [{Release, NodeAtom}|Nodes] }}
  after 1000*60 ->
       {reply, {error, timeout}, State}
  end.

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




