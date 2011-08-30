-module(erel_http_instance).
-compile(export_all).
-include_lib("webmachine/include/webmachine.hrl").

-record(state, {
          flavour
         }).

init(Flavour) ->
    {ok, #state{ flavour = Flavour }}.

content_types_provided(ReqData, State) ->
    {[{"application/json", to_json}], ReqData, State}.

content_types_accepted(ReqData, State) ->
    {[{"application/json", accept_json}], ReqData, State}.


allowed_methods(ReqData, #state{ flavour = [host, release, version] } = State) ->
    {['GET','POST','HEAD'], ReqData, State};
allowed_methods(ReqData, State) ->
    {['GET','HEAD'], ReqData, State}.

post_is_create(ReqData, #state{ flavour = [host, release, version] } = State) ->
    {true, ReqData, State};
post_is_create(ReqData, State) ->
    {false, ReqData, State}.

create_path(ReqData, #state{ flavour = [host, release, version] } = State) ->
    PathInfo = wrq:path_info(ReqData),
    Host = dict:fetch(host, PathInfo),
    Release = dict:fetch(release, PathInfo),
    Version = dict:fetch(version, PathInfo),
    {string:join([Host, Release, Version], "/"), ReqData, State};
create_path(ReqData, State) ->
    {undefined, ReqData, State}.

accept_json(ReqData, #state{ flavour = [host, release, version] } = State) ->
    PathInfo = wrq:path_info(ReqData),
    Host = dict:fetch(host, PathInfo),
    Release = dict:fetch(release, PathInfo),
    Version = dict:fetch(version, PathInfo),
    BinOpts = jsx:json_to_term(wrq:req_body(ReqData)),

    erel_release_manager:instantiate(list_to_atom(Host), 
                                     Release, Version, BinOpts),
    {true, ReqData, State}.

to_json(ReqData, #state{ flavour = [host, release, version] } = State) ->
    {"{}", ReqData, State}.
