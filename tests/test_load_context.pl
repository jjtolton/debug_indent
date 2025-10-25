test_context :-
    prolog_load_context(module, M),
    prolog_load_context(term_position, P),
    format("Module: ~w, Position: ~w~n", [M, P]).

?- test_context.
