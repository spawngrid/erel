-module(erel_manager_fsm).
-include_lib("erel_manager/include/erel_manager.hrl").

-behaviour(gen_fsm).

%% API
-export([start_link/1]).

%% gen_fsm callbacks
-export([init/1,
    started/2,
    ready/2,
    group_join/2,
    deployment/2,
    handle_event/3,
    handle_sync_event/4,
    handle_info/3,
    terminate/3,
    code_change/4]).

-define(SERVER, ?MODULE).

-record(state, { config :: list(term()), 
    groups = [] :: list({string(), list(string())}),
    joined_groups = [] :: list({string(), list(string())}),
    releases = [] :: list({atom(), string()}),
    deployments = [] :: list({atom(), list(string())}) }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Config) ->
  gen_fsm:start_link({local, ?SERVER}, ?MODULE, Config, []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init(Config) ->
  gen_fsm:send_event(self(), run),
  {ok, started, #state{ config = sort_config(Config) }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
started(run, #state{ config = [] } = State) -> % config has been interpreted
  gen_fsm:send_event(self(), run),
  {next_state, ready, State};
started(run, #state{ config = [Instruction|Config] } = State) ->
  State1 = interpret(Instruction, State),
  gen_fsm:send_event(self(), run),
  {next_state, started, State1#state{ config = Config }}.

ready(run, #state{ groups = [_|_] } = State) -> % schedule joining a group
  gen_fsm:send_event(self(), run),
  {next_state, group_join, State};
ready(run, #state{ deployments = [] } = State) -> % all deployments have been done
  {stop, normal, State};
ready(run, #state{ deployments = [_|_] } = State) -> % schedule a deployment
  gen_fsm:send_event(self(), run),
  {next_state, deployment, State}.

group_join(run, #state{ groups = [{_Group, Hosts}|_] } = State) -> % join host to a group
  Self = self(),
  Fun = fun () -> gen_fsm:send_event(Self, group_quorum_reached) end,
  erel_manager_quorum:start("erel.host", Hosts, Fun),
  {next_state, group_join, State};
group_join(group_quorum_reached, #state{ groups = [{Group, Hosts}|_] } = State) ->
  Self = self(),
  Fun = fun () -> gen_fsm:send_event(Self, group_joined) end,
  ?INFO("Quorum for group '~s' has been reached", [Group]),
  erel_manager_quorum:stop("erel.host", Hosts),
  erel_manager_quorum:start("erel.group." ++ Group, Hosts, Fun),
  [ erel_manager:group_join(Group, Host) || Host <- Hosts ],
  {next_state, group_join, State};
group_join(group_joined, #state{ groups = [{Group, Hosts}|Groups], joined_groups = JoinedGroups } = State) ->
  ?INFO("All required hosts for group '~s' have joined the group topic", [Group]),
  erel_manager_quorum:stop("erel.group." ++ Group, Hosts),
  gen_fsm:send_event(self(), run),
  {next_state, ready, State#state{ groups = Groups, joined_groups = [{Group, Hosts}|JoinedGroups] }}.

deployment(run, #state{ deployments = [{_Release, []}|Deployments] } = State) -> % deployment is complete
  gen_fsm:send_event(self(), run),
  {next_state, ready, State#state{ deployments = Deployments }};
deployment(run, #state{ deployments = [{Release, [Group|Groups]}|Deployments], 
                        joined_groups = JoinedGroups, releases = Releases } = State) -> % deploy
  Self = self(),
  Id = ossp_uuid:make(v4, text),
  Fun = fun () -> gen_fsm:send_event(Self, {transfer_completed, Group}) end,
  erel_manager_quorum:start(none, {received, Id}, "erel.group." ++ Group, proplists:get_value(Group, JoinedGroups), Fun),
  erel_manager:group_transfer(Id, Group, proplists:get_value(Release, Releases), []),
  {next_state, deployment, State#state{ deployments = [{Release, [Group|Groups]}|Deployments] }};
deployment({transfer_completed, Group}, #state{ deployments = [{Release, Groups}|Deployments] } = State) ->
  ?INFO("Transfer to all hosts in group '~s' has been completed", [Group]),
  gen_fsm:send_event(self(), run),
  {next_state, deployment, State#state{ deployments = [{Release, Groups -- [Group]}|Deployments] }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
        {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
        Reply = ok,
        {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
        {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
        ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
        {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

sort_config(Terms) ->
  lists:usort(fun(C1,C2) when is_tuple(C1), is_tuple(C2) ->
                directive_priority(element(1, C1)) =< directive_priority(element(1,C2))
            end, Terms).
% where
  directive_priority(release) -> 2;
  directive_priority(deploy) -> 3;
  directive_priority(group) -> 1;
  directive_priority(_) -> 100.

interpret({group, Name, Hosts}, #state{ groups = Groups } = State) ->
  State#state{ groups = [{Name, Hosts}|Groups]};
interpret({deploy, Name, Groups}, #state{ deployments = Deployments } = State) ->
  State#state{ deployments = [{Name, Groups}|Deployments]};
interpret({release, Name, Path}, #state{ releases = Releases } = State) ->
  State#state{ releases = [{Name, Path}|Releases]}.



