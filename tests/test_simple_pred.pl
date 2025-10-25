:- use_module('../debug_indent.pl').

:- start_debug.

simple_add(X, Y, Z) :- Z is X + Y.

:- end_debug.

test_simple :-
    reset_trace_depth,
    simple_add(2, 3, R),
    format("Result: ~w~n", [R]).

?- test_simple.
