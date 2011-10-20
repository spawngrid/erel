-module(erel_endp).
-export([cast/4]).

cast(Server, Exchange, Topic, Message) ->
  gen_server:cast(Server, {cast, Exchange, Topic, Message}). 
