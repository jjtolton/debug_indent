:- use_module('../debug_indent.pl').

% Not instrumented
before(X) :- X > 0.

:- start_debug.

% Auto-instrumented
factorial(0, 1).
factorial(N, F) :-
    N > 0,
    N1 is N - 1,
    factorial(N1, F1),
    F is N * F1.

:- end_debug.

% Not instrumented
after(X) :- X < 10.

test :-
    reset_trace_depth,
    factorial(3, F),
    format("Factorial result: ~w~n", [F]).

?- test.
