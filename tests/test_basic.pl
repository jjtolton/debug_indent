:- module(test_basic, [test_add/0]).
:- use_module(debug_indent).

add(X, Y, Z) :-
    $$ Z is X + Y.

test_add :-
    reset_trace_depth,
    $$ add(2, 3, R),
    format("Result: ~w~n", [R]).

?- test_add.
