-module(erel_endpoint_http).

-behaviour(gen_server).

%% API
-export([start_link/0, 
         add_node/2, nodes/0, get_webmachine_pid/1,
         routes/0]).

%% Internal
-export([route_table/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {
          http_port,
          routes = [],
          nodes = [],
          webmachine
         }).

%%%===================================================================
%%% API
%%%===================================================================
add_node(Server, Node) ->
    gen_server:call({?SERVER, Server}, {add_node, Node}).

nodes() ->
    gen_server:call(?SERVER, nodes).

get_webmachine_pid(Server) ->
    gen_server:call({?SERVER, Server}, get_webmachine_pid).

routes() ->
    gen_server:call(?SERVER, routes).
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
    Routes = ?MODULE:route_table(),
    {ok, HttpPort} = application:get_env(erel, http_port),
    gen_server:cast(self(), init),
    {ok, #state{ routes = Routes, http_port = HttpPort }}.

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
handle_call({add_node, Node}, _From, #state{ nodes = Nodes } = State) ->
    monitor_node(Node, true),
    {reply, ok, State#state{ nodes = [Node|Nodes] }};
handle_call(nodes, _From, #state{ nodes = Nodes } = State) ->
    {reply, [node()|Nodes], State};

handle_call(get_webmachine_pid, _From, #state{ webmachine = Pid } = State) ->
    {reply, Pid, State};

handle_call(routes, _From, #state{ routes = Routes } = State) ->
    {reply, Routes, State}.

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
handle_cast(init, #state{ http_port = HttpPort, routes = Routes } = State) ->
    WebConfig = [
                 {ip, "0.0.0.0"},
                 {port, HttpPort},
                 {dispatch, Routes}],

    case (catch gen_tcp:listen(HttpPort, [{reuseaddr, true}])) of
        {error, eaddrinuse} ->
            {ok, {{200, _}, _Headers, Body}} = lhttpc:request("http://127.0.0.1:" ++ integer_to_list(HttpPort) ++ "/?node=" ++ atom_to_list(node()), 'GET', [], 5000),
            NodeInfo = jsx:json_to_term(Body),
            Node = binary_to_atom(proplists:get_value(<<"node">>, NodeInfo), latin1),
            erlang:set_cookie(Node,
                              binary_to_atom(proplists:get_value(<<"cookie">>, NodeInfo), latin1)),
            erel_endpoint_http:add_node(Node, node()),
            Pid = erel_endpoint_http:get_webmachine_pid(Node);
        {ok, LSock} ->
            gen_tcp:close(LSock),
            {ok, Pid} = webmachine_mochiweb:start(WebConfig)
    end,
    link(Pid),
    {noreply, State#state{ webmachine = Pid }}.

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
handle_info({nodedown, Node}, #state{ nodes = Nodes } = State) ->
    {noreply, State#state{ nodes = Nodes -- [Node] }}.

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
code_change(_OldVsn, #state{ webmachine = Pid } = State, _Extra) ->
    {ok, HttpPort} = application:get_env(erel, http_port),

    Routes = ?MODULE:route_table(),

    unlink(Pid),

    Node = node(),
    case node(Pid) of
        Node ->
            exit(Pid, normal);
        _ ->
            ignore
    end,

    gen_server:cast(self(), init),
    
    {ok, State#state{ routes = Routes, http_port = HttpPort } }.

%%%===================================================================
%%% Internal functions
%%%===================================================================
route_table() ->
    [
     {[], erel_http_node, []},
     {["releases"], erel_http_release, []},
     {["instances"], erel_http_instance, []},
%     {["instances", release, version], erel_http_instance, [release, version]},
     {["instances", host, release, version], erel_http_instance, [host, release, version]}

    ].
