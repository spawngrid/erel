-module(erel_net_manager).

-behaviour(gen_server).

%% API
-export([start_link/0, cookie/1, hostname/0, name/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {
    hostname :: string(),
    name :: atom(),
    net_kernel :: pid(),
    cookies = [] :: [{atom(), atom()}]
  }).

%%%===================================================================
%%% API
%%%===================================================================

cookie(Node) ->
    gen_server:call(?SERVER, {cookie, Node}).

hostname() ->
    gen_server:call(?SERVER, {info, hostname}).

name() ->
    gen_server:call(?SERVER, {info, name}).


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

    case node() of
      'nonode@nohost' ->
        {Host, Domain} = {inet_db:gethostname(),inet_db:res_option(domain)},
        Hostname = Host ++ "." ++ Domain,
        Name = list_to_atom(binary_to_list(ossp_uuid:make(v4, text)) ++ "@" ++ Hostname),
        os:cmd(filename:join([Root, "bin", "epmd"]) ++ " -daemon"), %% ensure epmd is running
        {ok, Pid} = net_kernel:start([Name, longnames]),
        NetKernel = erlang:monitor(process, Pid);
      Name ->
        Hostname = lists:nth(2, string:tokens(atom_to_list(Name), "@")),
        NetKernel = undefined
    end,


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
handle_call({info, hostname}, _From, #state{ hostname = Hostname } = State) ->
  {reply, Hostname, State};
handle_call({info, name}, _From, #state{ name = Name } = State) ->
  {reply, Name, State};

handle_call({cookie, Node}, _From, #state{ cookies = Cookies } = State) ->
    case proplists:get_value(Node, Cookies) of
        undefined ->
            Cookie = binary_to_atom(ossp_uuid:make(v4, text), latin1),
            {reply, Cookie, State#state{ cookies = [{Node, Cookie}|Cookies] }};
        Cookie ->
            {reply, Cookie, State}
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
terminate(_Reason, #state{ net_kernel = undefined } = _State) ->
    ok;
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
