-module(erel_http_release).
-compile(export_all).
-include_lib("webmachine/include/webmachine.hrl").

-record(state, {
          node,
          flavour
         }).

init(Flavour) ->
    {ok, #state{ flavour = Flavour }}.

content_types_provided(ReqData, State) ->
    {[{"application/json", to_json}], ReqData, State}.


to_json(ReqData, #state{ flavour = [] } = State) ->
    Nodes = erel_endpoint_http:nodes(),
    Releases0 = compact_proplist(
                 [ {atom_to_binary(Node, latin1), 
                    {list_to_binary(Release), list_to_binary(Version)}} || Node <- Nodes, {Release, Version} <- erel_release_manager:releases(Node)]),
    Releases = [ {Node, compact_proplist(Releases) } || {Node, Releases} <- Releases0 ],
    {binary_to_list(jsx:term_to_json(Releases)), ReqData, State}.


%%% Internal

compact_proplist(PropList) ->
    dict:to_list(
      lists:foldl(fun ({K,V}, Dict) -> 
                          dict:append(K, V, Dict) 
                  end, dict:new(), PropList)).
