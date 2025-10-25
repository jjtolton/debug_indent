:- use_module('../debug_indent.pl').

before_pred :- write('before'), nl.

:- start_debug.
:- end_debug.

after_pred :- write('after'), nl.

test :-
    before_pred,
    after_pred.

?- test.
