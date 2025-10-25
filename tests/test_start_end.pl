:- module(test_start_end, [test_basic/0, test_factorial/0]).
:- use_module('../debug_indent.pl').

not_instrumented(X, Y) :-
    Y is X + 1.

:- start_debug.

instrumented_add(X, Y, Z) :-
    Z is X + Y.

instrumented_factorial(0, 1).
instrumented_factorial(N, F) :-
    N > 0,
    N1 is N - 1,
    instrumented_factorial(N1, F1),
    F is N * F1.

:- end_debug.

also_not_instrumented(X, Y) :-
    Y is X * 2.

test_basic :-
    reset_trace_depth,
    write('=== Testing not instrumented ==='), nl,
    not_instrumented(5, R1),
    format("Result: ~w~n~n", [R1]),

    write('=== Testing instrumented add ==='), nl,
    instrumented_add(2, 3, R2),
    format("Result: ~w~n~n", [R2]),

    write('=== Testing also not instrumented ==='), nl,
    also_not_instrumented(5, R3),
    format("Result: ~w~n~n", [R3]).

test_factorial :-
    reset_trace_depth,
    write('=== Testing instrumented factorial ==='), nl,
    instrumented_factorial(3, F),
    format("Result: ~w~n", [F]).

?- test_basic.
?- test_factorial.
