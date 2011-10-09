-module(erel_net_manager).

-behaviour(gen_server).

%% API
-export([start_link/0, hostname/0, cookie/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {
          name,
          hostname,
          net_kernel,
          cookies = []
         }).

%%%===================================================================
%%% API
%%%===================================================================

cookie(Node) ->
    gen_server:call(?SERVER, {cookie, Node}).
    
hostname() ->
    gen_server:call(?SERVER, hostname).

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
    
    erlang:process_flag(trap_exit, true),

    {ok, Hostname0} = inet:gethostname(),
    Hostname = list_to_atom(Hostname0),

    case application:get_env(shortname) of
        undefined ->
            Name = binary_to_atom(ossp_uuid:make(v4, text), latin1);
        {ok, Name} ->
            ok
    end,

    os:cmd(filename:join([Root, "bin", "epmd"]) ++ " -daemon"), %% ensure epmd is running
    {ok, Pid} = net_kernel:start([Name, shortnames]),
    NetKernel = erlang:monitor(process, Pid),

    {ok, #state{
       name = Name,
       hostname = Hostname,
       net_kernel = NetKernel
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
handle_call({cookie, Node}, _From, #state{ cookies = Cookies } = State) ->
    case proplists:get_value(Node, Cookies) of
        undefined ->
            Cookie = binary_to_atom(ossp_uuid:make(v4, text), latin1),
            {reply, Cookie, State#state{ cookies = [{Node, Cookie}|Cookies] }};
        Cookie ->
            {reply, Cookie, State}
    end;

handle_call(hostname, _From, #state{ hostname = Hostname } = State) ->
    {reply, Hostname, State}.

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
handle_info({'DOWN', NetKernel, process, _Pid, _Info}, 
            #state{ net_kernel = NetKernel } = State) ->
    {stop, net_kernel_stop, State};
handle_info(_, State) ->
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
terminate(_Reason, #state{ net_kernel = NetKernel } = _State) ->
    erlang:demonitor(NetKernel),
    net_kernel:stop(),
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
