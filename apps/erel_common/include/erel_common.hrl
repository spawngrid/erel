-define(endp_worker(Endpoint,Id, Name, Arg), #worker{ id = Id, start_func = {Endpoint, start_link, [Name, Arg]}, modules = [Name] }).
-define(endp_worker(Id, Name, Arg), #worker{ id = Id, start_func = {proplists:get_value(endpoint, application:get_all_env()), start_link, [Name, Arg]}, modules = [Name] }).

-record(endp_binding, {name :: atom(), type :: topic | direct, topic :: string()}).
