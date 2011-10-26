-module(erel_manager_main).
-export([start/0]).

-include_lib("erel_manager/include/erel_manager.hrl").

start() ->
  case application:get_env(erel_manager, config) of
    {ok, Config} ->
      {ok, Terms} = file:consult(Config),
      lists:foldl(fun (Term, Acc) -> interpret(Term, Acc) end, [], sort(Terms));
    undefined ->
      io:format("You should specify -erel_manager config ConfigFile~n"),
      init:stop()
  end.

%% internal

sort(Terms) ->
  lists:usort(fun(C1,C2) when is_tuple(C1), is_tuple(C2) ->
                directive_priority(element(1, C1)) =< directive_priority(element(1,C2))
            end, Terms).
% where
  directive_priority(release) -> 0;
  directive_priority(deploy) -> 1;
  directive_priority(group) -> 2;
  directive_priority(_) -> 100.

interpret({group, Name, Hosts}, Props) ->
  Fun = fun() ->
      ?INFO("Quorum for group '~s' has been reached", [Name]),
      erel_manager_quorum:stop("erel.host", Hosts),
      group_join(Name, Hosts, proplists:get_value(commands, Props, []))
  end,
  erel_manager_quorum:start("erel.host", Hosts, Fun),
  Props;
interpret({release, Name, Path}, Props) ->
  [{{release, Name}, Path}|Props];
interpret({deploy, Name, Groups}, Props) ->
  Path = proplists:get_value({release, Name}, Props),
  Fun = fun () -> [ erel_manager:group_deploy(Group, Path, []) || Group <- Groups ] end,
  Commands = proplists:get_value(commands, Props, []),
  [{commands, [Fun|Commands]}|Props];
interpret(_, Props) ->
  Props.

group_join(Name, Hosts, Cmds) ->
  Fun = fun() ->
      ?INFO("All required hosts for group '~s' have joined the group topic", [Name]),
      erel_manager_quorum:stop("erel.group." ++ Name, Hosts),
      [ Cmd() || Cmd <- Cmds ]
  end,
  erel_manager_quorum:start("erel.group." ++ Name, Hosts, Fun),
  [ erel_manager:group_join(Name, Host) || Host <- Hosts ].
