:- module(test_one_pred, []).
:- use_module('../debug_indent.pl').

:- start_debug.

foo(X) :- X > 0.

:- end_debug.
