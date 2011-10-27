-module(erel_manager_main).
-export([start/0]).

-include_lib("erel_manager/include/erel_manager.hrl").

start() ->
  case application:get_env(erel_manager, config) of
    {ok, ConfigFile} ->
      {ok, Config} = file:consult(ConfigFile),
      supervisor:start_child(erel_manager_sup, esupervisor:spec(#worker{ id = erel_manager_fsm,
                                                   start_func = {erel_manager_fsm, start_link, [Config]}}));
    undefined ->
      io:format("You should specify -erel_manager config ConfigFile~n"),
      init:stop()
  end.
