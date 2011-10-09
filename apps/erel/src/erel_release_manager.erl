-module(erel_release_manager).

-behaviour(gen_server).

%% API
-export([start_link/0,
         instantiate/4,
         releases/1, releases/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {
          root,
          releases = [],
          instantiated = [],
          ports = []
         }).

%%%===================================================================
%%% API
%%%===================================================================

instantiate(Node, Release, Version, Options) ->
    gen_server:call({?SERVER, Node}, {instantiate, Release, Version, Options}).
                                            
releases(Node) ->
    gen_server:call({?SERVER, Node}, releases).

releases() ->
    gen_server:call(?SERVER, releases).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
    {ok, [[Root]]} = init:get_argument(root),
    sort_releases(Root),

    Versions = filelib:wildcard("*", filename:join([Root, "releases"])),
    Releases = [ {filename:basename(Release, ".rel"), RelVersion} || RelVersion <- Versions, 
                                                                     Release <- filelib:wildcard("*.rel", filename:join([Root, "releases", RelVersion])) ],
    

    {ok, #state{
       root = Root,
       releases = Releases
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
handle_call({instantiate, Release, Version, Options}, _From, #state{ root = Root, releases = Releases, instantiated = Instantiated, ports = Ports } = State) ->
    case lists:member({Release, Version}, Releases) of
        true ->
            Port = instantiate_release(Root, Release, Version, Options),
            {reply, ok, State#state{ instantiated = [{{Release, Version}, Port}|Instantiated],
                                     ports = [{Port, {Release, Version}}|Ports]
                                   }};
        false ->
            {reply, {error, notfound}, State}
    end;

handle_call(releases, _From, #state{ releases = Releases } = State) ->
    {reply, Releases, State}.

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
sort_releases(Root) ->
    RelDir = filename:join([Root, "releases"]),
    [ begin
          RelFilename = filename:join([RelDir, Release]),
          {ok, [{release, {Name, Version}, {erts, _}, _}]} = file:consult(RelFilename),
          case filename:dirname(Release) =:= Version of
              true ->
                  ignore;
              false -> %% move
                  ok = filelib:ensure_dir(filename:join([RelDir, Version]) ++ "/"), %% make sure appropriate directory exists
                  RelFiles = filelib:wildcard(Name ++ ".*", filename:join([RelDir, filename:dirname(Release)])), %% release files
                  [ file:rename(filename:join([RelDir, filename:dirname(Release), RelFile]), 
                                filename:join([RelDir, Version, RelFile])) || RelFile <- RelFiles ]
          end
      end || Release <- filelib:wildcard("*/*.rel", RelDir) ],
    ok.

-define(with_option(X, Name, Args),  case proplists:get_value(X, Options) of
                                         undefined ->
                                             [];
                                         Name ->
                                             Args
                                                 
                                     end).

instantiate_release(Root, Release, Version, Options) ->
    Erl = filename:join([Root, "bin", "erl"]),
    Args = ?with_option(<<"shortname">>, Shortname, ["-sname", binary_to_list(Shortname)]),
    open_port({spawn_executable, Erl},[{args, ["-detached","-boot",filename:join([Root, "releases", Version, Release])|
                                               Args]},
                                       exit_status,
                                       hide]).
                                               
                                       
              
