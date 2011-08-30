-module(erel_http_node).
-compile(export_all).
-include_lib("webmachine/include/webmachine.hrl").

-record(state, {
          flavour
         }).

init(Flavour) ->
    {ok, #state{ flavour = Flavour }}.

content_types_provided(ReqData, State) ->
    {[{"application/json", to_json}], ReqData, State}.

to_json(ReqData, #state{ flavour = [] } = State) ->
    {ok, Vsn} = application:get_key(erel, vsn),
    case wrq:get_qs_value("node",ReqData) of
        undefined -> 
            CookieInfo = [];
        NodeName ->
            NodeAtom = list_to_atom(NodeName),
            Cookie = erel_net_manager:cookie(NodeAtom),
            erlang:set_cookie(NodeAtom, Cookie),
            CookieInfo = [{<<"cookie">>, atom_to_binary(Cookie, latin1)}]
    end,

    Node = [
            {<<"node">>, atom_to_binary(node(), latin1)},
            {<<"version">>, list_to_binary(Vsn)}|
            CookieInfo
           ],
    {binary_to_list(jsx:term_to_json(Node)), ReqData, State}.
