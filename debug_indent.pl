:- module(debug_indent, [
    op(900, fx, $$),
    ($$)/1,
    ('$$-')/3,
    reset_trace_depth/0
]).

:- use_module(library(format), [portray_clause/1]).

:- meta_predicate($$(0)).
:- meta_predicate('$$-'(0, +, +)).

:- dynamic('$trace_depth'/1).
:- dynamic('$trace_counter'/1).

'$trace_depth'(0).
'$trace_counter'(0).

%% to keep the compiler happy
'$$'(_).

user:contains_debug_op(Var) :-
    var(Var), !, fail.

user:contains_debug_op($$ _) :- !.

user:contains_debug_op((A, B)) :- !,
    (user:contains_debug_op(A) ; user:contains_debug_op(B)).

user:contains_debug_op((A ; B)) :- !,
    (user:contains_debug_op(A) ; user:contains_debug_op(B)).

user:contains_debug_op((A -> B)) :- !,
    (user:contains_debug_op(A) ; user:contains_debug_op(B)).

user:contains_debug_op(\+ A) :- !,
    user:contains_debug_op(A).

user:contains_debug_op(_) :- fail.

user:expand_body(Var, _, _, Var) :-
    var(Var), !.

user:expand_body($$(Goal), Module, LineNo, '$$-'(Goal, Module, LineNo)) :- !.

user:expand_body((A, B), Module, LineNo, (A1, B1)) :- !,
    user:expand_body(A, Module, LineNo, A1),
    user:expand_body(B, Module, LineNo, B1).

user:expand_body((A ; B), Module, LineNo, (A1 ; B1)) :- !,
    user:expand_body(A, Module, LineNo, A1),
    user:expand_body(B, Module, LineNo, B1).

user:expand_body((A -> B), Module, LineNo, (A1 -> B1)) :- !,
    user:expand_body(A, Module, LineNo, A1),
    user:expand_body(B, Module, LineNo, B1).

user:expand_body(\+ A, Module, LineNo, \+ A1) :- !,
    user:expand_body(A, Module, LineNo, A1).

user:expand_body(Goal, _Module, _LineNo, Goal).

user:term_expansion((Head :- Body0), (Head :- Body)) :-
    user:contains_debug_op(Body0),
    format("~n[TERM EXPANSION] Expanding: ~q~n", [Head]),
    prolog_load_context(module, Module),
    prolog_load_context(term_position, position_and_lines_read(_CharPos, LineNo)),
    format("[TERM EXPANSION] Module: ~q, LineNo: ~q~n", [Module, LineNo]),
    user:expand_body(Body0, Module, LineNo, Body),
    format("[TERM EXPANSION] Body0: ~q~n", [Body0]),
    format("[TERM EXPANSION] Body:  ~q~n~n", [Body]).



reset_trace_depth :-
    retractall('$trace_depth'(_)),
    assertz('$trace_depth'(0)),
    retractall('$trace_counter'(_)),
    assertz('$trace_counter'(0)).

'$write_spaces'(0) :- !.
'$write_spaces'(N) :-
    N > 0,
    put_char(' '),
    N1 is N - 1,
    '$write_spaces'(N1).

'$indent' :-
    '$trace_depth'(D),
    Spaces is D * 2,
    '$write_spaces'(Spaces).

'$next_counter'(N) :-
    '$trace_counter'(N),
    N1 is N + 1,
    retractall('$trace_counter'(_)),
    assertz('$trace_counter'(N1)).

'$inc_depth'(D, D1) :-
    retractall('$trace_depth'(_)),
    D1 is D + 1,
    assertz('$trace_depth'(D1)).

'$dec_depth'(D) :-
    retractall('$trace_depth'(_)),
    assertz('$trace_depth'(D)).

'$$'(G_0) :-
    '$trace_depth'(D),
    '$next_counter'(N),
    '$indent',
    format("~w] call: ", [N]),
    portray_clause(G_0),
    '$inc_depth'(D, _D1),
    catch(
          (
              G_0,
              '$dec_depth'(D),
              '$indent',
              format("~w] exit: ", [N]),
              portray_clause(G_0)
          ),
        Ex,
          (   '$dec_depth'(D),
              '$indent',
              format("~w] exception: ", [N]),
              portray_clause(Ex:G_0),
              throw(Ex)
          )
    ).

'$$-'(G_0, Module, LineNo) :-
    '$trace_depth'(D),
    '$next_counter'(N),
    '$indent',
    format("~w] call (~w:~w): ", [N, Module, LineNo]),
    portray_clause(G_0),
    '$inc_depth'(D, _D1),
    catch(
          (
              G_0,
              '$dec_depth'(D),
              '$indent',
              format("~w] exit (~w:~w): ", [N, Module, LineNo]),
              portray_clause(G_0)
          ),
        Ex,
          (   '$dec_depth'(D),
              '$indent',
              format("~w] exception (~w:~w): ", [N, Module, LineNo]),
              portray_clause(Ex:G_0),
              throw(Ex)
          )
    ).


