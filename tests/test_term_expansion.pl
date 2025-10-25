:- module(test_term_expansion, []).

user:term_expansion((Head :- Body), (Head :- Body)) :-
    format("~n=== TERM EXPANSION ===~n", []),
    format("Head: ~q~n", [Head]),
    format("Body: ~q~n", [Body]),

    prolog_load_context(module, Module),
    format("Module: ~q~n", [Module]),

    prolog_load_context(term_position, TermPosition),
    format("TermPosition: ~q~n", [TermPosition]),

    (   prolog_load_context(source, Source)
    ->  format("Source: ~q~n", [Source])
    ;   format("Source: not available~n", [])
    ),

    (   prolog_load_context(file, File)
    ->  format("File: ~q~n", [File])
    ;   format("File: not available~n", [])
    ),

    (   prolog_load_context(stream, Stream)
    ->  format("Stream: ~q~n", [Stream])
    ;   format("Stream: not available~n", [])
    ),

    format("===================~n~n", []),
    fail.

test_predicate(X) :-
    X > 0,
    Y is X + 1,
    Y < 10.

another_test(A, B) :-
    A = B.
