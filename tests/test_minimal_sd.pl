:- module(test_minimal_sd, []).
:- use_module('../debug_indent.pl').

:- start_debug.

simple_pred(X) :- X > 0.

:- end_debug.
