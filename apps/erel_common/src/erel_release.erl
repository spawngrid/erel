-module(erel_release).
-export([erts_version/1]).

erts_version(Root) ->
    {ok, Bin} = file:read_file(filename:join([Root, "releases",
                "start_erl.data"])),
    [ErtsVsn, _] = binary:split(Bin, <<" ">>),
    binary_to_list(ErtsVsn).
