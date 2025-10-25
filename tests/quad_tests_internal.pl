:- use_module('../quads/quads.pl').
:- use_module(debug_indent).

pkg_name('../debug_indent.pl').



'basic'
?- pkg_name(Pkg), Pkg:contains_debug_op(($$ (_X=1))).
%@   error(existence_error(procedure,'../debug_indent.pl':contains_debug_op/1),contains_debug_op/1).

