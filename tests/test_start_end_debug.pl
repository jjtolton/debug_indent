:- use_module('../debug_indent.pl').

not_instrumented(X) :-
    Y is X + 1,
    Z is Y * 2,
    write('Not instrumented: '), write(Z), nl.

:- start_debug.

instrumented_add(X, Y, Z) :-
    Z is X + Y.

instrumented_multiply(X, Y, Z) :-
    Z is X * Y.

instrumented_factorial(0, 1).
instrumented_factorial(N, F) :-
    N > 0,
    N1 is N - 1,
    instrumented_factorial(N1, F1),
    F is N * F1.

:- end_debug.

also_not_instrumented(X) :-
    Y is X * 3,
    write('Also not instrumented: '), write(Y), nl.

test_not_instrumented :-
    reset_trace_depth,
    not_instrumented(5),
    nl.

test_instrumented_add :-
    reset_trace_depth,
    instrumented_add(3, 4, Z),
    write('Result: '), write(Z), nl,
    nl.

test_instrumented_factorial :-
    reset_trace_depth,
    instrumented_factorial(3, F),
    write('Result: '), write(F), nl,
    nl.

test_also_not_instrumented :-
    reset_trace_depth,
    also_not_instrumented(5),
    nl.

test_all :-
    write('=== Testing not instrumented predicate ==='), nl,
    test_not_instrumented,
    write('=== Testing instrumented add ==='), nl,
    test_instrumented_add,
    write('=== Testing instrumented factorial ==='), nl,
    test_instrumented_factorial,
    write('=== Testing also not instrumented predicate ==='), nl,
    test_also_not_instrumented.

?- test_all.
