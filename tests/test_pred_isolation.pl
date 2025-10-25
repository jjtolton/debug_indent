:- use_module('../debug_indent.pl').

test_instrument_simple :-
    write('Testing instrument on simple goal...'), nl,
    Body0 = (Z is X + Y),
    instrument_body(Body0, Body1),
    format("Body0: ~w~n", [Body0]),
    format("Body1: ~w~n", [Body1]),
    nl.

test_expand_instrumented :-
    write('Testing expand on instrumented goal...'), nl,
    Body1 = ($$ (Z is X + Y)),
    expand_body(Body1, test_module, 10, Body2),
    format("Body1: ~w~n", [Body1]),
    format("Body2: ~w~n", [Body2]),
    nl.

?- test_instrument_simple.
?- test_expand_instrumented.
