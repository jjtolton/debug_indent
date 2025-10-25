:- module(test_simple, [test_simple/0]).
:- use_module('../debug_indent.pl').

:- start_debug.

simple_add(X, Y, Z) :-
    Z is X + Y.

:- end_debug.

test_simple :-
    reset_trace_depth,
    simple_add(2, 3, R),
    write('Result: '), write(R), nl.
