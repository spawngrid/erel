-module(erel_manager_main).
-export([provision/0]).

-include_lib("erel_manager/include/erel_manager.hrl").

provision() ->
  case application:get_env(erel_manager, config) of
    {ok, ConfigFile} ->
      {ok, Config} = file:consult(ConfigFile),
      {ok, Pid} = supervisor:start_child(erel_manager_sup, esupervisor:spec(#worker{ id = erel_manager_fsm, restart = transient,
                                                   start_func = {erel_manager_fsm, start_link, [Config]}})),
      Ref = monitor(process, Pid),
      receive {'DOWN', Ref, Type, Object, Info} -> init:stop() end;
    undefined ->
      io:format("You should specify -erel_manager config ConfigFile~n"),
      init:stop()
  end.
