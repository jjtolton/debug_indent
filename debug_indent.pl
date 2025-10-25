:- module(debug_indent, [
    op(900, fx, $$),
    ($$)/1,
    ('$$-')/3,
    reset_trace_depth/0,
    contains_debug_op/1,
    is_instrumented/1,
    instrument_body/2,
    expand_body/4,
    auto_instrument_and_expand/2,
    expand_debug_ops/2
]).

:- use_module(library(format), [portray_clause/1]).
:- use_module(library(iso_ext), [bb_put/2, bb_get/2]).

:- meta_predicate($$(0)).
:- meta_predicate('$$-'(0, +, +)).

:- discontiguous(($$)/1).
:- discontiguous(debug_started/0).
:- discontiguous(debug_ended/0).

term_expansion((:- establish_debug_mode), established) :-
    (   bb_get(auto_debug_mode, _), !
    ;   bb_put(auto_debug_mode, off),
        bb_put(trace_depth, 0),
        bb_put(trace_counter, 0)
    ).

:- establish_debug_mode.

%% to keep the compiler happy
'$$'(_).

contains_debug_op(Var) :-
    var(Var), !, fail.

contains_debug_op($$ _) :- !.

contains_debug_op((A, B)) :- !,
    (contains_debug_op(A) ; contains_debug_op(B)).

contains_debug_op((A ; B)) :- !,
    (contains_debug_op(A) ; contains_debug_op(B)).

contains_debug_op((A -> B)) :- !,
    (contains_debug_op(A) ; contains_debug_op(B)).

contains_debug_op(\+ A) :- !,
    contains_debug_op(A).

contains_debug_op(_) :- fail.

is_instrumented(Var) :-
    var(Var), !, fail.

is_instrumented($$ _) :- !.

is_instrumented('$$-'(_, _, _)) :- !.

is_instrumented((A, B)) :- !,
    (is_instrumented(A) ; is_instrumented(B)).

is_instrumented((A ; B)) :- !,
    (is_instrumented(A) ; is_instrumented(B)).

is_instrumented((A -> B)) :- !,
    (is_instrumented(A) ; is_instrumented(B)).

is_instrumented(\+ A) :- !,
    is_instrumented(A).

is_instrumented(_) :- fail.

instrument_body(Var, Var) :- var(Var), !.

instrument_body((A, B), (A1, B1)) :- !,
    instrument_body(A, A1),
    instrument_body(B, B1).

instrument_body((A ; B), (A1 ; B1)) :- !,
    instrument_body(A, A1),
    instrument_body(B, B1).

instrument_body((A -> B), (A1 -> B1)) :- !,
    instrument_body(A, A1),
    instrument_body(B, B1).

instrument_body(\+ A, \+ A1) :- !,
    instrument_body(A, A1).

instrument_body(Goal, $$ Goal).

expand_body(Var, _, _, Var) :-
    var(Var), !.

expand_body($$(Goal), Module, LineNo, '$$-'(Goal, Module, LineNo)) :- !.

expand_body((A, B), Module, LineNo, (A1, B1)) :- !,
    expand_body(A, Module, LineNo, A1),
    expand_body(B, Module, LineNo, B1).

expand_body((A ; B), Module, LineNo, (A1 ; B1)) :- !,
    expand_body(A, Module, LineNo, A1),
    expand_body(B, Module, LineNo, B1).

expand_body((A -> B), Module, LineNo, (A1 -> B1)) :- !,
    expand_body(A, Module, LineNo, A1),
    expand_body(B, Module, LineNo, B1).

expand_body(\+ A, Module, LineNo, \+ A1) :- !,
    expand_body(A, Module, LineNo, A1).

expand_body(Goal, _Module, _LineNo, Goal).

auto_instrument_and_expand(Body0, Body) :-
    prolog_load_context(module, Module),
    prolog_load_context(term_position, position_and_lines_read(_CharPos, LineNo)),
    instrument_body(Body0, Body1),
    expand_body(Body1, Module, LineNo, Body).

expand_debug_ops(Body0, Body) :-
    prolog_load_context(module, Module),
    prolog_load_context(term_position, position_and_lines_read(_CharPos, LineNo)),
    expand_body(Body0, Module, LineNo, Body).

% term_expansion((:- start_debug), []) :-
%     bb_put(auto_debug_mode, on).

% term_expansion((:- end_debug), []) :-
%     bb_put(auto_debug_mode, off).

term_expansion((:- start_debug), debug_started) :-
    bb_put(auto_debug_mode, on).

term_expansion((:- end_debug), debug_ended) :-
    bb_put(auto_debug_mode, off).

term_expansion((Head :- Body0), (Head :- Body)) :-
    bb_get(auto_debug_mode, on),
    \+ is_instrumented(Body0),
    !,
    auto_instrument_and_expand(Body0, Body).

term_expansion((Head :- Body0), (Head :- Body)) :-
    contains_debug_op(Body0),
    expand_debug_ops(Body0, Body).



reset_trace_depth :-
    bb_put(trace_depth, 0),
    bb_put(trace_counter, 0).

'$write_spaces'(0) :- !.
'$write_spaces'(N) :-
    N > 0,
    put_char(' '),
    N1 is N - 1,
    '$write_spaces'(N1).

'$indent' :-
    bb_get(trace_depth, D),
    Spaces is D * 2,
    '$write_spaces'(Spaces).

'$next_counter'(N) :-
    bb_get(trace_counter, N),
    N1 is N + 1,
    bb_put(trace_counter, N1).

'$inc_depth'(D, D1) :-
    D1 is D + 1,
    bb_put(trace_depth, D1).

'$dec_depth'(D) :-
    bb_put(trace_depth, D).

'$$-'(G_0, Module, LineNo) :-
    bb_get(trace_depth, D),
    '$next_counter'(N),
    '$indent',
    portray_clause(trace(id:N, call, Module:LineNo, G_0)),
    '$inc_depth'(D, _D1),
    catch(
          (
              G_0,
              '$dec_depth'(D),
              '$indent',
              portray_clause(trace(id:N, exit, Module:LineNo, G_0))
          ),
        Ex,
          (   '$dec_depth'(D),
              '$indent',
              portray_clause(trace(id:N, exception, Module:LineNo, Ex, G_0)),
              throw(Ex)
          )
    ).


