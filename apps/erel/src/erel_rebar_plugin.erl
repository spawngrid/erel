-module(erel_rebar_plugin).

-export([post_generate/2]).

post_generate(Config, ReltoolFile) ->
  {ok, Terms} = file:consult(ReltoolFile),
  case Terms of 
    [{application, _, _}] ->
      ok;
    _ ->
      ErelDir = filename:absname(rebar_config:get(Config, erel_dir, [])),
      TargetDir = filename:join([filename:dirname(ReltoolFile), proplists:get_value(target_dir, Terms)]),
      erel_release_manager:inject_erel(ErelDir, TargetDir),
      ok
  end.
