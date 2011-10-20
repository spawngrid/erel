-module(erel_master).
-export([hostname/0]).

hostname() ->
  case application:get_env(erel_master, hostname) of
    {ok, HostnameA} when is_atom(HostnameA)  ->
      atom_to_list(HostnameA);
    _ ->
      {Host, Domain} = {inet_db:gethostname(),inet_db:res_option(domain)},
      Host ++ "." ++ Domain
  end.
