-module(validator).
-export([start/0]).

start() ->
    spawn_link(fun() -> init() end).

init()->
    validator().

validator() ->
    receive
        {validate, Ref, _Reads, Writes, Client, Handler} ->
            Tag = make_ref(),
            send_write_checks(Tag, Writes, Handler),
            case check_writes(length(Writes), Tag) of
                ok ->
                    update(Writes),
                    Client ! {Ref, ok};
                abort ->
                    Client ! {Ref, abort}
            end,
            validator();
        stop ->
            ok;
        _Old ->
            validator()
    end.
    
update(Writes) ->
    lists:foreach(fun({_, Entry, Value}) -> 
                    Entry ! {write, Value}
                  end, 
                  Writes).

send_write_checks(Tag, Writes, Handler) ->
    Self = self(),
    lists:foreach(fun({_, Entry, _}) ->
		    Entry ! {check, Self, Tag, Handler}
                  end, 
                  Writes).

check_writes(0, _) ->
    ok;
check_writes(N, Tag) ->
    receive
        {Tag, ok} ->
            check_writes(N-1, Tag);
        {Tag, abort} ->
            abort
    end.
