-module(erel_manager_quorum).
-export([start/3, start/5, stop/2]).
-export([binding/1, init/2, handle_message/2, handle_cast/2, handle_call/3]).

-include_lib("erel_manager/include/erel_manager.hrl").

-record(state, { endpoint :: pid(),  expected = [] :: list(string()), cb :: fun(() -> any()),
        reply :: term(), req :: none | term(), payloads = [] :: list({string(), term()})  }).

start(Topic, Hosts, Fun) ->
  start(ping, pong, Topic, Hosts, Fun).

start(Req, Reply, Topic, Hosts, Fun) ->
  {ok, Endpoint} = application:get_env(erel_manager, endpoint),  
  supervisor:start_child(erel_manager_quorum_sup, 
    esupervisor:spec(?endp_worker(Endpoint, {Topic, Hosts}, ?MODULE, {Req, Reply,Topic, Hosts, Fun}))).

stop(Topic, Hosts) ->
  supervisor:terminate_child(erel_manager_quorum_sup, {Topic, Hosts}),
  supervisor:delete_child(erel_manager_quorum_sup, {Topic, Hosts}).

binding({_Req, _Reply, Topic, _Expected, _Fun}) ->
  #endp_binding{ name = erel, type = topic, topic = Topic }.

init(Endpoint, {Req, Reply, Topic, Expected, Fun}) ->
  length(Expected) == 0 andalso Fun(undefined),
  Req =/= none andalso erel_endp:cast(Endpoint, erel, Topic, Req),
  {ok, #state{ endpoint = Endpoint, expected = Expected, cb = Fun, reply = Reply, req = Req }}.

handle_message({announce, Hostname}, #state{ reply = Reply, req = ping } = State) ->
  handle_message({Reply, Hostname}, State);

handle_message({Reply, Hostname}, #state{ reply = Reply } = State) ->
  handle_message({Reply, undefined, Hostname}, State);

handle_message({Reply, Payload, Hostname}, #state{ expected = Expected, cb = Cb, reply = Reply, payloads = Payloads } = State) ->
  Expected1 = Expected -- [Hostname],
  ?DBG("Host ~s has responded with ~p(~p), left to respond: ~p",[Hostname, Reply, Payload, Expected1]),
  case Expected1 of 
    [] ->
      ?DBG("Quorum has been reached"), 
      spawn(fun () -> Cb([{Hostname, Payload}|Payloads]) end),
      {ok, State#state{ expected = Expected1, cb = fun() -> ok end }};
    _ ->
      {ok, State#state{ expected = Expected1, payloads = [{Hostname, Payload}|Payloads] }}
  end;

handle_message(_, #state{} = State) ->
  {ok, State}.


handle_cast(_, State) ->
  {noreply, State}.

handle_call(_, _, State) ->
  {noreply, State}.

%% 
