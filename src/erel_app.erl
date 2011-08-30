-module(erel_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Pid} = erel_sup:start_link(),
    io:format(user, "++ Controller node name: ~p~n", [node()]),
    {ok, Pid}.

stop(_State) ->
    ok.
