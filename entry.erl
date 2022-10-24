-module(entry).
-export([new/1]).

new(Value) ->
    spawn_link(fun() -> init(Value) end).

init(Value) ->
    entry(Value, make_ref(), sets:new()).

entry(Value, Time, ActiveHandlers) ->
    receive
        {read, Ref, From} ->
            UpdatedActiveHandlers = sets:add_element(From, ActiveHandlers),
            ResponseMsg = {Ref, self(), Value, Time},
            From ! ResponseMsg,
            entry(Value, Time, UpdatedActiveHandlers);
        {write, New} ->
            entry(New, make_ref(), ActiveHandlers);
        {check, From, Tag, Handler} ->
            UpdatedActiveHandlers = sets:del_element(Handler, ActiveHandlers),
            case sets:size(UpdatedActiveHandlers) of
                0 ->
                    From ! {Tag, ok};
                _ ->
                    From ! {Tag, abort}
            end,
            entry(Value, Time, UpdatedActiveHandlers);
        {removehandler, From} ->
            entry(Value, Time, sets:del_element(From, ActiveHandlers));
        stop ->
            ok
    end.
