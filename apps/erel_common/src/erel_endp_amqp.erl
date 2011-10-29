-module(erel_endp_amqp).
-behaviour(gen_bunny).

-include_lib("erel_master/include/erel_master.hrl").
-include_lib("gen_bunny/include/gen_bunny.hrl").

-export([init/1,
    handle_message/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2]).

-export([start_link/2]).

-record(state, { handler :: atom(),
                 hstate :: term(),
                 vsn :: binary()  }).

start_link(Handler, Arg) ->
  [Host, Port, 
   Username, Password, 
   VHost] = [ begin {ok, Value} = application:get_env(Config), Value end
   || Config <- [amqp_host, amqp_port, amqp_username, amqp_password, amqp_vhost]],

  ConnectInfo = {network, Host, Port, {list_to_binary(Username), list_to_binary(Password)}, list_to_binary(VHost)},
  Declare = binding_to_declare(Handler:binding(Arg)),

  gen_server:start_link(gen_bunny, [?MODULE, ConnectInfo,
      Declare, [Handler, Arg]], []). 

init([Handler, Arg]) ->
  {ok, HState} = Handler:init(self(), Arg),
  {ok, Vsn} = application:get_key(erel_common, vsn),
  {ok, #state{ handler = Handler, hstate = HState, vsn = list_to_binary(Vsn) }}.

handle_message(#amqp_msg{ props = Props,
                          payload = Message}, 
                        #state{ vsn = Vsn,
                                handler = Handler, 
                                hstate = HState } = State) ->
  case lists:keyfind(<<"erel_common_version">>, 1, Props#'P_basic'.headers) of
    {_, _, Vsn} ->
      {ok, HState1} = Handler:handle_message(binary_to_term(Message), HState),
      {noreply, State#state{ hstate = HState1 }};
    {_, _, OtherVsn} ->
      ?ERROR("Incompatible erl_commons version (~p) of the message received (message_id=~p), ignoring", [OtherVsn, Props#'P_basic'.message_id]),
      {noreply, State}
  end. 

handle_call(Call, From, #state{ handler = Handler, hstate = HState } = State) ->
  case Handler:handle_call(Call, From, HState) of
    {reply, Reply, HState1} ->
      {reply, Reply, State#state{ hstate = HState1 }};
    {noreply, HState1} ->
      {noreply, State#state{ hstate = HState1 }}
  end.

handle_cast({cast, Exchange, Topic, Message}, #state{ vsn = Vsn } = State) ->
  BasicPublish = #'basic.publish'{
    exchange = atom_to_binary(Exchange, latin1), 
    routing_key = list_to_binary(Topic)
  },
  Self = self(), 
  #amqp_msg{} = Msg0 = bunny_util:new_message(term_to_binary(Message)),
  Msg = Msg0#amqp_msg{ props = #'P_basic'{ headers = [{<<"erel_common_version">>, longstr, Vsn}] }},
  spawn_link(fun () ->
        amqp_channel:cast(gen_bunny:get_channel(Self), BasicPublish,
          Msg) end),
  {noreply, State};

handle_cast(Msg, #state{ handler = Handler, hstate = HState} = State) ->
  {noreply, HState1} = Handler:handle_cast(Msg, HState),
  {noreply, State#state{ hstate = HState1 }}.

handle_info(Info, State) ->
  {noreply, State}.

terminate(Reason, _State) ->
  ok.

%%
binding_to_declare(#endp_binding{ name = Name, type = Type, topic = Topic}) ->
  {#'exchange.declare'{exchange = atom_to_binary(Name, latin1),  type =
      binding_type_to_type(Type), auto_delete = true }, #'queue.declare'{ queue = <<>>, auto_delete = true, exclusive = true}, list_to_binary(Topic)}.
binding_type_to_type(topic) ->
  <<"topic">>;
binding_type_to_type(direct) ->
  <<"direct">>.
