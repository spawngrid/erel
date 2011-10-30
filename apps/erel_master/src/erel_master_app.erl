-module(erel_master_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	case application:get_env(erel_master, debug) of
		{ok, true} ->
			ignore;
		undefined -> % switch debugging off
			alog_control:replace_flows([{flow, nodebug, {mod, ['_']},{'<', debug}, [alog_tty], true}])
	end,

	erel_master_sup:start_link().

stop(_State) ->
	ok.
