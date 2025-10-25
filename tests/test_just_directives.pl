:- use_module('../debug_indent.pl').

test_directives :-
    write('Testing directives'), nl.

:- start_debug.
:- end_debug.

?- test_directives.
