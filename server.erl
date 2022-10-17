-module(server).
-export([start/1]).

start(N) ->
    erlang:display("Start server"),
    spawn(fun() -> init(N) end).

init(N) ->
    erlang:display("Init server"),
    Store = store:new(N),
    Validator = validator:start(),
    server(Validator, Store).
    
server(Validator, Store) ->
    receive 
        {open, Client} ->
	    Client ! {transaction, Validator, Store},
            server(Validator, Store);
        stop ->
            Validator ! stop,
            store:stop(Store)
    end.
