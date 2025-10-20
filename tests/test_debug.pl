:- module(test_debug, [test_factorial/0, test_member/0]).
:- use_module(debug_indent).

factorial(0, 1) :- !.
factorial(N, F) :-
    $$ N > 0,
    $$ N1 is N - 1,
    $$ factorial(N1, F1),
    $$ F is N * F1.

test_factorial :-
    reset_trace_depth,
    $$ factorial(3, F),
    format("Result: ~w~n", [F]).

member_check(X, [X|_]).
member_check(X, [_|T]) :-
    $$ member_check(X, T).

test_member :-
    reset_trace_depth,
    $$ member_check(3, [1, 2, 3, 4]),
    format("Found!~n", []).

?- test_factorial.
