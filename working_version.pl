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

set_args(Name, Arity, Arity) :- set_arg(Name, Arity, Arity).
set_args(Name, Arity, Coord) :- set_arg(Name, Arity, Coord),
	NewCoord is Coord+1,
	set_arg(Name, Arity, NewCoord).

set_arg(Name, Arity, Coord) :- functor(Term1, Name, Arity),
	functor(Term2, Name, Arity),
	identify_vars(Term1, Term2, Arity, 1, Coord),
	arg(Coord, Term1, T1),
	arg(Coord, Term2, T2),
	assert(eval_onestep(Term1, Term2) :- eval_onestep(T1, T2)).

identify_vars(_, _, Arity, Arity, Arity).
identify_vars(Term1, Term2, Arity, Arity, _):-arg(Arity, Term1, X),
	arg(Arity, Term2, X).
identify_vars(Term1, Term2, Arity, Coord, Coord):- Next is Coord +1,
	identify_vars(Term1, Term2, Arity, Next, Coord).
identify_vars(Term1, Term2, Arity, Current, Coord) :- arg(Current, Term1, X),
	arg(Current, Term2, X),
	Next is Current+1,
	identify_vars(Term1, Term2, Arity, Next, Coord).

 make_onestep_op(Pred):-
	arg(1, Pred, Name),
	arg(2,Pred, Ar),
	set_args(Name, Ar, 1 ).

%evaluate: recursively evaluates term to normal form
eval(X,Y) :-
	eval_onestep(X,Z),
	eval(Z,Y).
%if no evaluation rules apply, the term evaluates to itself
eval(X,X).



