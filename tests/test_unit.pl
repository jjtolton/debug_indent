:- use_module('../debug_indent.pl').

test_contains_debug_op :-
    write('Testing contains_debug_op/1...'), nl,

    \+ contains_debug_op(foo(X)),
    write('  ✓ Simple goal has no debug op'), nl,

    contains_debug_op($$ foo(X)),
    write('  ✓ Detects $$ operator'), nl,

    contains_debug_op((foo(X), $$ bar(Y))),
    write('  ✓ Detects $$ in conjunction'), nl,

    \+ contains_debug_op((foo(X), bar(Y))),
    write('  ✓ No false positives in conjunction'), nl,

    nl.

test_instrument_body :-
    write('Testing instrument_body/2...'), nl,

    instrument_body(foo(X), Result1),
    Result1 = ($$ foo(X)),
    write('  ✓ Simple goal gets instrumented'), nl,

    instrument_body((foo(X), bar(Y)), Result2),
    Result2 = (($$ foo(X)), ($$ bar(Y))),
    write('  ✓ Conjunction gets instrumented'), nl,

    instrument_body(\+ foo(X), Result3),
    Result3 = (\+ ($$ foo(X))),
    write('  ✓ Negation gets instrumented'), nl,

    nl.

test_expand_body :-
    write('Testing expand_body/4...'), nl,

    expand_body($$ foo(X), test, 10, Result1),
    Result1 = '$$-'(foo(X), test, 10),
    write('  ✓ Expands $$ to $$-/3'), nl,

    expand_body(($$ foo(X), $$ bar(Y)), test, 20, Result2),
    Result2 = ('$$-'(foo(X), test, 20), '$$-'(bar(Y), test, 20)),
    write('  ✓ Expands conjunction of $$ ops'), nl,

    nl.

run_all_tests :-
    write('=== Running Unit Tests ==='), nl, nl,
    test_contains_debug_op,
    test_instrument_body,
    test_expand_body,
    write('=== All Tests Passed! ==='), nl.

?- run_all_tests.
