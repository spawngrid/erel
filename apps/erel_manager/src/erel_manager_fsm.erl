-module(erel_manager_fsm).
-include_lib("erel_manager/include/erel_manager.hrl").

-behaviour(gen_fsm).

%% API
-export([start_link/2]).

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

-type release_name() :: string().
-type release_version() :: string().
-type release() :: {release_name(), release_version()}.

-record(state, { 
    type :: provision,
    config :: list(term()), 
    groups = [] :: list({string(), list(string())}),
    joined_groups = [] :: list({string(), list(string())}),
    releases = [] :: list({release(), string()}),
    deployments = [] :: list({release(), list(string())}) }).

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
start_link(Type, Config) ->
  gen_fsm:start_link({local, ?SERVER}, ?MODULE, {Type, Config}, []).

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
init({Type, Config}) ->
  gen_fsm:send_event(self(), run),
  {ok, started, #state{ type = Type, config = sort_config(Config) }}.

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
  ?DBG("Preparing groups"),
  gen_fsm:send_event(self(), run),
  {next_state, group_join, State};
ready(run, #state{ deployments = [], type = provision } = State) -> % all deployments have been done
  ?INFO("No more deployments pending"),
  {stop, normal, State};
ready(run, #state{ deployments = [{Rel, Groups}|_], type = provision } = State) -> % schedule a deployment
  ?INFO("Scheduling deployment of '~s' to ~p groups", [Rel, Groups]),
  gen_fsm:send_event(self(), run),
  {next_state, deployment, State}.

group_join(run, #state{ groups = [{Group, Hosts}|_] } = State) -> % join host to a group
  Self = self(),
  Fun = fun (ActualHosts, _) ->
          gen_fsm:send_event(Self, {group_quorum_reached, ActualHosts}) 
        end,
  ?INFO("Waiting for hosts ~p to join the group '~s'",[Hosts, Group]),
  erel_manager_quorum:start("erel.host", Hosts, Fun),
  {next_state, group_join, State};
group_join({group_quorum_reached, ActualHosts}, #state{ groups = [{Group, Hosts}|Groups] } = State) ->
  Self = self(),
  Fun = fun (JoinedHosts, _) -> gen_fsm:send_event(Self, {group_joined, JoinedHosts}) end,
  ?INFO("Quorum for group '~s' has been reached", [Group]),
  erel_manager_quorum:start("erel.group." ++ Group, ActualHosts, Fun),
  [ erel_manager:group_join(Group, Host) || Host <- ActualHosts ],
  {next_state, group_join, State#state{ groups = [{Group, ActualHosts}|Groups] }};
group_join({group_joined, JoinedHosts}, #state{ groups = [{Group, Hosts}|Groups], joined_groups = JoinedGroups } = State) ->
  ?INFO("All required hosts for group '~s' have joined the group topic", [Group]),
  gen_fsm:send_event(self(), run),
  {next_state, ready, State#state{ groups = Groups, joined_groups = [{Group, JoinedHosts}|JoinedGroups] }}.

deployment(run, #state{ deployments = [{Release, []}|Deployments] } = State) -> % deployment is complete
  ?INFO("Release '~s' deployment has been completed",[Release]),
  gen_fsm:send_event(self(), run),
  {next_state, ready, State#state{ deployments = Deployments }};
deployment(run, #state{ deployments = [{Release, [Group|Groups]}|Deployments], 
                        joined_groups = JoinedGroups } = State) -> % check releases 
  Self = self(),
  Hosts = proplists:get_value(Group, JoinedGroups),
  Fun = fun (Hosts1, Releases) -> gen_fsm:send_event(Self, {releases, Releases, Hosts1}) end,
  ?DBG("Requesting lists of releases from the hosts ~p in group '~s'",[Hosts,Group]),
  erel_manager_quorum:start(list_releases, list_releases, "erel.group." ++ Group, Hosts,  Fun),
  {next_state, deployment, State};
deployment({releases, Listed, Hosts}, #state{ releases = Releases, deployments =
        [{RelName, [Group|Groups]}|_], joined_groups = JoinedGroups } = State) ->
  %% find out which hosts need the release
  {_, RelVer, _} = lists:keyfind(RelName, 1, Releases),
  RHosts = lists:map(fun({Host, _}) -> Host end, lists:filter(fun({_, Rels}) -> not lists:member({RelName, RelVer}, Rels) end, Listed)),
  Self = self(),
  SubGroup = Group ++ "." ++ RelName ++ "-" ++ RelVer,
  length(RHosts) == 0 andalso ?INFO("No hosts need any new deployments"),
  length(RHosts) == 0 orelse ?INFO("Inviting ~p to the deployment group '~s'",
      [RHosts, SubGroup]),
  Fun = fun (RHosts1,_) -> gen_fsm:send_event(Self, {deployment_group_joined, SubGroup,
                  RHosts1}) end,
  erel_manager_quorum:start("erel.group." ++ SubGroup, RHosts, Fun),
  [ erel_manager:group_join(SubGroup, Host) || Host <- RHosts ],
  {next_state, deployment, State};
deployment({deployment_group_joined, SubGroup, Hosts}, #state{ releases = Releases, deployments = [{Release, _}|_] } = State) ->
  ?INFO("Deployment group '~s' has been joined by all required nodes", [SubGroup]),
  Self = self(),
  Fun = fun (Hosts1, _) -> gen_fsm:send_event(Self, {transfer_completed, SubGroup, Hosts1}) end,
  erel_manager_quorum:start(none, {received, Release}, "erel.group." ++ SubGroup, Hosts, Fun),
  {_, _, Path} = lists:keyfind(Release, 1, Releases),
  length(Hosts) > 0 andalso erel_manager:group_transfer(Release, SubGroup, Path,[]),
  {next_state, deployment, State};
deployment({transfer_completed, Group, Hosts}, #state{ deployments = [{Release, [_|Groups]}|_Deployments] } = State) ->
  ?INFO("Transfer to all hosts in group '~s' has been completed", [Group]),
  Self = self(),
  Fun = fun (Hosts1, _) -> gen_fsm:send_event(Self, {provisioning_completed, Group, Hosts1}) end,
  erel_manager_quorum:start({provision_release, Release}, {provision_release, Release}, "erel.group." ++ Group, Hosts, Fun), 
  {next_state, deployment, State#state{}};
deployment({provisioning_completed, Group, Hosts}, #state{} =State) ->
  ?INFO("Provisioning on all hosts in group '~s' has been completed", [Group]),
  Self = self(),
  Fun = fun(Hosts1, _) -> gen_fsm:send_event(Self, {deployment_group_emptied, Group, Hosts1}) end,
  erel_manager_quorum:start(none, leave, "erel.group." ++ Group, Hosts, Fun), 
  [ erel_manager:group_part(Group, Host) || Host <- Hosts ],
  {next_state, deployment, State};
deployment({deployment_group_emptied, Group, Hosts}, #state{ deployments = [{Release, [_|Groups]}|Deployments]} =State) ->
  ?INFO("Deployment group '~s' has been emptied",[Group]),
  gen_fsm:send_event(self(), run),
  {next_state, deployment, State#state{ deployments = [{Release, Groups}|Deployments] }}.


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
                directive_priority(element(1, C1)) < directive_priority(element(1,C2))
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
  %% figure out version
  {ok, Config} = application:get_env(erel_manager, config),
  RealPath = filename:join([filename:dirname(Config), Path]),
  {ok, [Rels]} = file:consult(filename:join([RealPath, "releases", "RELEASES"])),
  case lists:keyfind(Name, 2, Rels) of
    false ->
      ?ERROR("Release '~s' can't be found in ~s", [Name, filename:join([RealPath, "releases", "RELEASES"])]),
      State; %% should we return state here or make fsm stop?
    {release, Name, Version, _, _, _} ->
      ?INFO("Release '~s' has been located, version ~s", [Name, Version]),
      State#state{ releases = [{Name, Version, RealPath}|Releases]}
  end.



