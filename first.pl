%main predicate: declares ==> infix, reads in file name, calls file
% parsing predicates, calls make_one_step
main:-
	 op(1200, xfx, ==>),
	 write("enter file name in double quotes: "),
	 read(FileName),
         open(FileName,read,Str),
         read_rules(Str,_Rules),
         close(Str),
	 make_one_step.

%read_rules: asserts each rule from the infile while !at end of stream
read_rules(Stream,[]):-
         at_end_of_stream(Stream).

read_rules(Stream,[X|L]):-
         \+  at_end_of_stream(Stream),
         read(Stream,X),
	 assertz(X),
	 read_rules(Stream,L).

% asserts the basic one step evaluation rule and calls make_one_step_op
% for each member of the list in Predicates
make_one_step :- assert(eval_onestep(X,Y) :- ==>(X,Y)),
	         predicates(L),
	         member(Pred, L),
	         make_onestep_op(Pred).

% takes a tuple from the predicate list of form (name, arity) and
% contructs terms using Name and Ar.
% First definition asserts op(X,Y) evals to op(Z,Y) when X evals to Z
% Second definition asserts op(X,Y) evals to op(X,Z) when Y evals to Z

set_args(Term, Coords, [], Arity) :- N is Arity+1, N = Coords.
set_args(Term, Coords, [X|Xs], Arity) :-
	arg(Coords, Term, X),
	N is Coords + 1,
	set_args(Term, N, Xs, Arity).

assert_evals(Term1, Term2, L):-
	%how do i get every combination of items from the var list?
	findall((X,Y), (member(X,L), member(Y,L)), Z),
	member(Pair, Z),
	arg(1, Pair, Coord1),
	arg(2, Pair, Coord2),
	assert(eval_onestep(Term1, Term2) :- eval_onestep(Coord1, Coord2)).

 make_onestep_op(Pred):-
	arg(1, Pred, Name),
	arg(2,Pred, Ar),
	functor(Term1, Name, Ar),
	set_args(Term1, 1, L, Ar),
	functor(Term2, Name, Ar),
	set_args(Term2, 1, L, Ar),
	assert_evals(Term1, Term2, L).

% make_one_step_op(Pred):-
	%arg(1, Pred, Name),
	%arg(2,Pred, Ar),
	%functor(Term1, Name, Ar),
	%arg(1, Term1, X),
	%arg(2, Term1, Y),
	%functor(Term2, Name, Ar),
	%arg(1, Term2, X),
	%arg(2, Term2, Z),
	%assert(eval_onestep(Term1, Term2):-eval_onestep(Y,Z)).

%evaluate: recursively evaluates term to normal form
eval(X,Y) :-
	eval_onestep(X,Z),
	eval(Z,Y).
%if no evaluation rules apply, the term evaluates to itself
eval(X,X).



