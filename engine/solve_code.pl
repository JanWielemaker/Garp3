% This file contains some old code from solve.pl If we ever move away
% from bitvectors, this stuff saves a lot of recoding...






/*
get_derivable(L, R, Derivable, Rel):-
	memberchk(relation(L, Rel, R), Derivable),
	!.
get_derivable(L, R, Derivable, Rel):-
	memberchk(relation(R, Rel1, L), Derivable),
	inverse(Rel1, Rel),
	!.


incompatible_rels(>, <).
incompatible_rels(>, =<).
incompatible_rels(>, =).
incompatible_rels(>=, <).
incompatible_rels(=, <).
incompatible_rels(=, >).
incompatible_rels(<=, >).
incompatible_rels(<, >).
incompatible_rels(<, >=).
incompatible_rels(<, =).
*/


/*
 * append_relation(+Relation, +Cin, -Cout, -Appended, +AnalyseSimple)
 * append_relation_all(+RelationList, .. idem ..).
 * append a relation/relationlist to the cio structure
 * Relation in intern representaion
 * Appended: relations appended (i.e. not already derivable)
 * AnalyseSimple: true -> replace a with b if a=b is derived
 *		  false -> don't replace
 *


 FL nov 07 INeq overhaul
% FL december 2004:
% In the influence resolution procedure in garp 2.0 the list of adders
% (influences to add) is passed on  when appending a relation.
% This way, AnalyseSimpleEquality and Zero Analysis can
% change pointers/bits, without invalidating the adders.

% other calls do not yet use these arguments so this extra clause
% is added to catch old style calls.
append_relation(R, Cin, Cout, Ap, As):-
    append_relation(R, Cin, Cout, Ap, As, [],[]).

% fail if relation is invalid (i.e. a > a, or a+b > a+b)
% FL june 07: new position of this check: before simplification
append_relation(Intern, Cin, _, _, _, _, _):-
	illegal_relation(Intern),
	(
	  etrace(solve_derived_invalid, _, inequality)
	-> % only do the rest of the work if tracer is on...
	  cio_q(Cin, Q),
	  etrace(solve_invalid_input, [Intern, Q], inequality)
	;
	  true
	),
	!,
	fail.

% FL december 2004: Solve does simplification of results:
% x + y = x + z  ->  y = z,
% but not simplification of the input... This is done here, before
% the actual appendrelation2 call.
% Last two arguments are Adders passed on for bit change updates.
% (see: analyse zero equality)
append_relation(R, Cin, Cout, Ap, As, Addersin, Addersout):-
     simplify_relations([R], [R1]),
    !,
    append_relation2(R1, Cin, Cout, Ap, As, Addersin, Addersout).



FL june 07: old position of illegal_rel check
% fail if relation is invalid (i.e. a > a, or a+b > a+b)
% FL december 2004: new name & 2 extra arguments, see above.
commmented out
append_relation2(Intern, Cin, _, _, _, _, _):-
	illegal_relation(Intern),
	(
	  etrace(solve_derived_invalid, _, inequality)
	-> % only do the rest of the work if tracer is on...
	  cio_q(Cin, Q),
	  etrace(solve_invalid_input, [Intern, Q], inequality)
	;
	  true
	),
	!,
	fail.


FL nov 07 INeq overhaul
% relation is already derivable
% FL december 2004: new name & 2 extra arguments, see above.

append_relation2(Intern, Cio, Cio, [], _, Ad, Ad):-
	cio_d(Cio, Derivable),
	is_derivable(Intern, Derivable),
	% etrace(solve_derivable_rel, _, add_relation), gives too many extra statements in e.g. Inf Res.
	!.

% append relation (fail if inconsistent)
% FL december 2004: new name (+2) & 2 extra arguments, see above.

append_relation2(AppendOne, Cin, Cout, [AppendOne], DoAnalyseSimple, Adders, NAdders):-
	cio_r_nr_d_nd_q_nq_c_nc(Cin, Cnew,
		Relations, SortedFinalAllRelations,
		Derivable, SortedFinalAllNewDerivable,
		Q, NQ,
		C, NC),
	append([AppendOne], Derivable, KnownDerivable),
	% all relations we don't want to derive again
	solve([[]/AppendOne], Relations, Relations, KnownDerivable, NewDerived, Q),
	% fails is inconsistent
	append(KnownDerivable, NewDerived, AllNewDerivable),
	% all relations we have derived now
	append([AppendOne], NewDerived, AllNew),
	% all new relations
	append([AppendOne], Relations, AllRelations),
	% all relations used as a base
	%remove_weaker(AllNew, AllNew, StrongAllNew),
	remove_weaker2(AllNew, AllNew, StrongAllNew),
	% remove a>=b if a>b is found
	remove_weaker(StrongAllNew, AllRelations, StrongAllRelations),
	%remove_weaker2(AllRelations, AllRelations, StrongAllRelations),
	% also in base
	remove_weaker(StrongAllNew, AllNewDerivable, StrongAllNewDerivable),
	%remove_weaker2(AllNewDerivable, AllNewDerivable, StrongAllNewDerivable),
	% also in all derived
	flag(analyse_simple_found_contradiction, _, false),
	analyse_simple_equality(DoAnalyseSimple, StrongAllNew, Q, Q1,
		StrongAllNew, SimpleAllNew,
		StrongAllRelations, SimpleAllRelations,
		StrongAllNewDerivable, SimpleAllNewDerivable,
		C, C1, Adders, Adders1), % Note that adderlists are passed on
	% replace a with b if a=b
	analyse_zero_equality(DoAnalyseSimple, SimpleAllNew, FinalAllNew,
	    SimpleAllNewDerivable, FinalAllNewDerivable,
	    SimpleAllRelations, FinalAllRelations,
	    Q1, NQ, C1, NC, Adders1, Adders2), % Note that adderlists are passed on
	% FL december 2004 new in garp 2.0: zero analysis = analyse simple equality using:
	% x = 0, y = 0 -> x=y pattern
	sort(FinalAllRelations, SortedFinalAllRelations),
	% remove duplicates
	sort(FinalAllNew, SortedFinalAllNew),
	sort(FinalAllNewDerivable, SortedFinalAllNewDerivable),
	!,
	inspect_correspondence(SortedFinalAllNew, Cnew, Cout, DoAnalyseSimple, Adders2, NAdders).
	% inspect if and iff statements (correspondence)


	%
FL december 2004: 2 extra arguments, see above.
append_relation_all(Append, Cin, Cout, AppendNotDerivable, DoAnalyseSimple):-
    append_relation_all(Append, Cin, Cout, AppendNotDerivable, DoAnalyseSimple, [], []).


% fail if invalid input
% FL june 07 new position of this check: before simplification
append_relation_all(InternL, Cin, _, _, _, _, _):-
	member(Intern, InternL),
	illegal_relation(Intern),
	(
	  etrace(solve_derived_invalid, _, inequality)
	-> % only do the rest of the work if tracer is on...
	  cio_q(Cin, Q),
	  etrace(solve_invalid_input, [Intern, Q], inequality)
	;
	  true
	),
	!,
	fail.


% FL december 2004: Solve does simplification of results:
% x + y = x + z  ->  y = z,
% but not simplification of the input... This is done here, before
% the actual appendrelation2 call.
% Last two arguments are Adders passed on for bit change updates.
% (see: analyse zero equality)
append_relation_all(Append1, Cin, Cout, AppendNotDerivable, DoAnalyseSimple, Adin, Adout):-
	simplify_relations(Append1, Append),
	!,
	append_relation_all2(Append, Cin, Cout, AppendNotDerivable, DoAnalyseSimple, Adin, Adout).

 old position of this check...
% fail if invalid input

% FL december 2004: new name & 2 extra arguments, see above.
append_relation_all2(InternL, Cin, _, _, _, _, _):-
	member(Intern, InternL),
	illegal_relation(Intern),
	(
	  etrace(solve_derived_invalid, _, inequality)
	-> % only do the rest of the work if tracer is on...
	  cio_q(Cin, Q),
	  etrace(solve_invalid_input, [Intern, Q], inequality)
	;
	  true
	),
	!,
	fail.

% append relations (fail if inconsistent)
% Adderlist is passed on for bit change updates: see analyse zero equality
% results in 2 extra arguments

append_relation_all2(Append, Cin, Cout, AppendNotDerivable, DoAnalyseSimple, A, NA):-
	cio_r_nr_d_nd_q_nq_c_nc(Cin, Cnew,
		Relations, SortedFinalAllRelations,
		Derivable, SortedFinalAllNewDerivable,
		Q, NQ,
		C, NC),
	remove_weaker(Append, Append, StrongAppend),
	remove_derivable(StrongAppend, Derivable, AppendNotDerivable),
	!,
(	AppendNotDerivable == [], Cin = Cout, A = NA
	;
	append(AppendNotDerivable, Derivable, KnownDerivable),
	% all relations we don't want to derive again
	append(AppendNotDerivable, Relations, KnownRelations),
	add_empty_parents(AppendNotDerivable, AppendNotDerivable2),
	solve(AppendNotDerivable2, KnownRelations, KnownRelations,
			KnownDerivable, NewDerived, Q),
	% fails is inconsistent% add relation, succeeds if result is consistent,
% has no term twice and can be simplified
% if inconsistent: fail solve
% if no simplification, or already derivable, or evident:
	append(KnownDerivable, NewDerived, AllNewDerivable),
	% all relations we have derived now
	append(AppendNotDerivable, NewDerived, AllNew),
	% all new relations
	append(AppendNotDerivable, Relations, AllRelations),
	% all relations used as a base
	%remove_weaker(AllNew, AllNew, StrongAllNew),
	remove_weaker2(AllNew, AllNew, StrongAllNew),
	% remove a>=b if a>b is found
	remove_weaker(StrongAllNew, AllRelations, StrongAllRelations),
	%remove_weaker2(AllRelations, AllRelations, StrongAllRelations),
	% also in base
	remove_weaker(StrongAllNew, AllNewDerivable, StrongAllNewDerivable),
	%remove_weaker2(AllNewDerivable, AllNewDerivable, StrongAllNewDerivable),
	% also in all derived
	analyse_simple_equality(DoAnalyseSimple, StrongAllNew, Q, NQ,
		StrongAllNew, FinalAllNew,
		StrongAllRelations, FinalAllRelations,
		StrongAllNewDerivable, FinalAllNewDerivable,
		C, NC, A, NA1), % Note that adderlists are passed on
	% replace a with b if a=b
	% analyse zero equality is not done here, because it is done in the
	% single version of append relation,
    % because it is exhaustive it is most efficient to do it once in a while.
	sort(FinalAllRelations, SortedFinalAllRelations),
	% remove duplicates
	sort(FinalAllNew, SortedFinalAllNew),
	sort(FinalAllNewDerivable, SortedFinalAllNewDerivable),
	!,
	inspect_correspondence(SortedFinalAllNew, Cnew, Cout, DoAnalyseSimple, NA1, NA)
	% inspect if and iff statements
 ),
	!.



 % remove_derivable(+ToInspect, +KnownRelations, -NotDerivable)
 %
 % remove all derivable relations (given Arg2) from Arg1
 %

*/

 /*
remove_derivable([], _, []).

remove_derivable([H|T], D, NT):-
	is_derivable(H, D),
	!,
	remove_derivable(T, D, NT).

remove_derivable([H|T], D, [H|NT]):-
	remove_derivable(T, D, NT).

*/





/* --------------------------------------------------------------------
 * solve(+NewRelations, +ToInspect, +AllKnown, +AllDerivable, -New)
 *
 * for each relation in NewRelations:
 * add first of NewRelations with all of ToInspect (= AllKnown on first call)
 * if a valid, not evident and not already known relation is found:
 * add it to NewRelations
 * Return only relations between two single quantities or constants
 *
 */


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% New approach to inequality reasoning: First the relations are divided
% into contexts with shared, connected variables, thus minimizing
% derivations between otherwise unconnected variables.
%
% A redo of add_relation would be good because now the contexts have to
% be determined again and again.
%
% (caching is not possible: analyse zero etc. change pointers so they
% should be adapted too, which is an equal amount of work.)
%

/*

solve(NR, TO, K, D, New, Q):-
	split_contexts(NR, TO, K, D, Contexts),
	solve_contexts(Contexts, New, Q).

%done
solve_contexts([], [], _Q).

% empty new relations >> skip (nb: does not save much,
% only some flag calls
solve_contexts([context([], _TO, _K, _D, _)|T], New, Q):-
	!,
	solve_contexts(T, New, Q).

% solve context
solve_contexts([context(NR, TO, K, D, _)|T], New, Q):-
	solve_core(NR, TO, K, D, N, Q),
	solve_contexts(T, NT, Q),
	append(N, NT, New).

split_contexts(NR, TO, K, D, Contexts):-
	context_tag(D, TD),
	% construct first set of contexts using a tagged version of D which is a superset of NR, TO and K
	pointer_contexts(TD, NR, TO, K, PointerContexts1),
	% merge contexts with common elements
	merge_contexts(PointerContexts1, Contexts).

context_tag([], []).
context_tag([Rel|T], [Rel/CP|NT]):-
	determine_pointers(Rel, CP),
	context_tag(T, NT).

%%	%%%%%%%
% take each element of D and put it in existing or new pointer context
%
pointer_contexts([], [], [], [], []).

% make new context with H as single element of D and its pointers as
% the new bitvector describing the context (contextpointers CP
pointer_contexts([H/CP|T], NRin, TOin, Kin, [context(CNR, CTO, CK, [H], CP)|ContextT]):-
	insert_nr(H, NRin, NRout, CNR),
	insert_to_k(H, TOin, TOout, CTO),
	insert_to_k(H, Kin, Kout, CK),
	pointer_contexts(T, NRout, TOout, Kout, ContextT).

%done
insert_nr(_Rel, [], [], []).
%fit
insert_nr(Rel, [List/Rel|T], T, [List/Rel]):-
	!. %relation fits in context, done
%	insert_nr(Rel, T, NRout, NT).

% no fit
insert_nr(Rel, [H|T], [H|NRoutT], NR):-
	insert_nr(Rel, T, NRoutT, NR).

%done
insert_to_k(_Rel, [], [], []).
%fit
insert_to_k(Rel, [Rel|T], T, [Rel]):-
	!. % relation fits in context, done
%	insert_to_k(Rel, T, NRout, NT).


% no fit
insert_to_k(Rel, [H|T], [H|NRoutT], NR):-
	insert_to_k(Rel, T, NRoutT, NR).


%%%%%%%%%%%%%%%%
%
merge_contexts([], []).

% merge possible: extract all contexts from tail that have common
% elements with current context
merge_contexts([context(NR1, TO1, K1, D1, CP1)|T], RM):-
	select(context(NR2, TO2, K2, D2, CP2), T, Rest),
	map_intersection(CP1, CP2, bitvector(Common)),
	Common =\= 0,
	!, % common elements: merge contexts
	map_union(CP1, CP2, CP),
	append(NR1, NR2, NR),
	append(TO1, TO2, TO),
	append(K1, K2, K),
	append(D1, D2, D),
	% try to find more possible merges for this context
	merge_contexts([context(NR, TO, K, D, CP)|Rest], RM).

% no merge possible
merge_contexts([H|T], [H|TM]):-
	merge_contexts(T, TM).

*/

% %	%%%%%%%%END NEWWWWW %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5



/*
cache_solve_result(_NR, _K, _D, _New):-
	flag(solve_caching, F, F),
	F\= 0,
	!.

cache_solve_result(NR, K, D, New):-
	%hash_term(NR/K, Hash), % should be enough as an identifier
	%assertz(solve_cache(Hash, NR/K/D/New)).
	assertz(solve_cache(NR/K, D/New)).

get_cached_solve_result(_NR, _K, _D, _New):-
	flag(solve_caching, F, F),
	F\= 0,
	!,
	fail.

get_cached_solve_result(NR, K, D, New):-
	%hash_term(NR/K, Hash),
	%solve_cache(Hash, NR/K/D/New).
	solve_cache(NR/K, D/New).
*/
% ----------- resolve influences and proportional relations ------------




/*** FL december-2003: old resolve_adders from garp 1.7 ***
*     with partial closed world assumption
*

resolve_adders(Adders, Cin, Cout):-
	cio_q_nq(Cin, Cnew, Q, NQ),
	adders_quantities(Adders, NewAdders, Q, NQ),
	!,
	resolve_adders(NewAdders, Cnew, Cout, true).
	% fails if contradiction

% no progress see if we can set some uninfluenced influence to zero
% we must be conservative:
% set a quantity to zero that is  proportionalto / an influence on some
% other quantity, if possible. Then try to find new results.
% it is difficult to predict what will happen, because the quantity may
% have all kinds of relations to other quantities, for instance:
%
%  derivative(a) > derivative(b)	% not very likely, but possible
%  prop_pos(c, a)			% c is proportional to a
%  prop_pos(b, c)			% b is proportional to c
%  assume a to be zero
%  derive: derivative(b) > zero
%  resolve: c = zero
%  resolve: b = zero -> contradiction
%
% so if the assumption fails, try another one, but don't fail the
% complete predicate

resolve_adders(Adders, Cin, Cout, false):-
	cio_q(Cin, Q),
	member(Value/DQ, Q), 			% a value or derivative
	list_map([DQ], DM),
	\+ memberchk(adder(DM, _), Adders), 	% not influenced itself
	member(adder(_, L), Adders), 		% it influences something
	(	memberchk(pos(DM), L);
		memberchk(neg(DM), L)
	),
	cio_d(Cin, Der), 			% it's sign is not known
	\+ get_sign(pos(DM), Der, _),
	list_map([], ZeroMap), 			% it can be assumed
	append_relation(relation(DM, =, ZeroMap), Cin, Cnew, _, false),
	tracer(resolve, 'assuming %w is zero', [Value]),
	resolve_adders(Adders, Cnew, Cout, true),
	% now we are sure the assumption is safe !
	!.

resolve_adders(_, Cio, Cio, false).

resolve_adders(Adders, Cin, Cout, true):-	% first time, or progress
	resolve(Adders, Cin, Cnew, Unresolved, Progress),
	resolve_adders(Unresolved, Cnew, Cout, Progress).

*** FL december-2003: end of old resolve_adders,  new garp 2.0 starts here ***/





/* --- resolve_addders_2/4: old garp 1.7.2  ----

% no progress, discard adders and derive unknown for these quantities.
resolve_adders(_, Cio, Cio, false).

% first time, or progress
resolve_adders(Adders, Cin, Cout, true):-
	resolve(Adders, Cin, Cnew, [], Unresolved, Progress),
	resolve_adders(Unresolved, Cnew, Cout, Progress).

*** FL May 2004: garp 2.0 version (with tracer adders in normal representation) */

/*** FL december-2003: GARP 1.7 resolve clauses not weighing influences... ***

% resolve: for each adder: determine signs of terms
% if ambiguous (at least one positive and one negative term): discard adder
% if positive, zero, or negative sum: add this result
% if not all terms are known: put adder on Unresolved list

resolve([], Cin, Cin, [], false).

resolve([ adder(Q, AddList) | Tail ], Cin, Cout, NewUnresolved, Progress) :-
	(@tracer->>tell(resolve) -> %gp3 0.3
		cio_q(Cin, QL),
		print_sum(Q, QL),
		write(' is influenced by / proportional to: '),
		forall(member(One, AddList), write_addone(One, QL)),
		nl,
		@tracer->>told,
		 !
		;
		true
	),
	cio_d(Cin, Derivable),
	sign_addlist(AddList, Derivable, first, NewAddList, Result),
	(@tracer->>tell(resolve) -> %gp3 0.3
		print_sum(Q, QL),
		write(' evaluated to be influenced by / proportional to: '),
		forall(member(One, NewAddList), write_addone(One, QL)),
		nl,
		write('which is '), write_ln(Result),
		@tracer->>told,
		!
		;
		true
	),
	!,
	add_adder_result(Q, Result, Cin, Cnew, Progress, RestProgress,
		NewUnresolved, Unresolved, adder(Q, NewAddList)),
	% note this may fail
	resolve(Tail, Cnew, Cout, Unresolved, RestProgress).


write_addone(X, _):- var(X), !.
write_addone(pos(One), QL):- write('positive '), print_sum(One, QL), tab(2), !.
write_addone(neg(One), QL):- write('negative '), print_sum(One, QL), tab(2), !.
write_addone(One, _):- write(One), tab(2), !.

% incomplete information: add new adder to unresolved of tail,
% Let progress be progress of tail

add_adder_result(_, incomplete, Cin, Cin, Progress, Progress, [Adder|Unresolved],
	Unresolved, Adder):- !.

% valid new result: let progress be true if not already derivable

add_adder_result(Q, Sign, Cin, Cout, Progress, TailProgress,
		Unresolved, Unresolved, _):-
	zero_relation_sign(Relation, Sign),
	list_map([], Empty),
	right_order(relation(Q, Relation, Empty), Irel),
	append_relation(Irel, Cin, Cout, Added, false),
	(Added = [],
	  Progress=TailProgress;
	  Progress=true),
	!.

% ambiguous influence on Q: return all possibilities (upon backtracking)

add_adder_result(Q, ambiguous, Cin, Cout, Progress, TailProgress,
		Unresolved, Unresolved, _):-
	list_map([], Empty),
	member(Relation, [ <, =, > ]),
	right_order(relation(Q, Relation, Empty), Irel),
	once((
		append_relation(Irel, Cin, Cout, Added, false),
		(	Added = [],
			Progress=TailProgress;
			Progress=true
		)
	     )).

right_order(relation(L, <, R), relation(R, >, L)) :- !.
right_order(relation(L, =<, R), relation(R, >=, L)) :- !.
right_order(R, R):- !.



% determine sum of influences/proportional relations

sign_addlist([], _, Result, [], Result):- !.

% if already ambiguous, don't mind the tail

sign_addlist(_, _, ambiguous, [], ambiguous):- !.

% get sign, determine sum, do tail (let head of newtail be sign of head)

sign_addlist([H|T], Relations, PrevSign, [H|NewTail], Result):-
	memberchk(H, [ pos, zero, neg ]),
	!,
	sign_add(PrevSign, H, NewSign),
	sign_addlist(T, Relations, NewSign, NewTail, Result).

sign_addlist([H|T], Relations, PrevSign, [Sign|NewTail], Result):-
	get_sign(H, Relations, Sign),
	!,
	sign_add(PrevSign, Sign, NewSign),
	sign_addlist(T, Relations, NewSign, NewTail, Result).

% could not get sign: do tail (let Head of newtail be Head)
% if previous + tail is ambiguous, sign is not important
% otherwise return incomplete

sign_addlist([H|T], Relations, PrevSign, [H|NewTail], Result):-
	sign_addlist(T, Relations, PrevSign, NewTail, NewResult),
	sign_incomplete(NewResult, Result).


*** FL december-2003: New GARP 2.0 resolve clauses and subs below replace above ******/





/*****	endnew FL june 07 *****/






/***
*   FL december-2003:  New GARP 2.0 code ends here,
*   sign_incomplete & sign_add unused by garp 2.0
***

sign_incomplete(ambiguous, ambiguous):- ! .
% if already 'ambiguous' then unknown sign makes no difference
sign_incomplete(_, incomplete):- ! .
% we can't establish the correct sign

sign_add(first, S, S). 	% first sign
sign_add(zero, zero, zero).
sign_add(pos, pos, pos).
sign_add(neg, neg, neg).
sign_add(pos, zero, pos).
sign_add(zero, pos, pos).
sign_add(neg, zero, neg).
sign_add(zero, neg, neg).
sign_add(pos, neg, ambiguous).
sign_add(neg, pos, ambiguous).
*/
% sign_add(pos_zero, pos, pos).
% sign_add(pos_zero, zero, pos_zero).
% sign_add(pos_zero, neg, ambiguous).   % ????????
% sign_add(neg_zero, neg, neg).
% sign_add(neg_zero, zero, neg_zero).
% sign_add(neg_zero, pos, ambiguous).   % ????????

% sign_add(pos, pos_zero, pos).
% sign_add(zero, pos_zero,  pos_zero).
% sign_add(neg, pos_zero,  ambiguous).  % ????????
% sign_add(neg, neg_zero,  neg).
% sign_add(zero, neg_zero,  neg_zero).
% sign_add(pos, neg_zero,  ambiguous).  % ????????

/***
*   FL december-2003: get_sign still used in garp 2.0 (by other clauses then sign_add!)
***/



/*
%done.
derive_unknown_2ndorder([], [], Results, Results).

derive_unknown_2ndorder([_|UnknownAddersTail], [adder(TrQ, _)|UnknownTrAddersTail], ResultsIn, ResultsOut):-
	TrQ = second_derivative(Q),
	derive_unknown_2ndorder(UnknownAddersTail, UnknownTrAddersTail, [Q/unknown|ResultsIn], ResultsOut).

*/


/*
%% --------------------------------------------------------------------
%%	ANALYSE SOLVE RESULTS: ANALYSE SIMPLE / ZERO EQUALITY ETC.
%% --------------------------------------------------------------------
*/

% analyse results of solve
% if simple equality (between two parameters or a parameter and a constant,
% (not zero))
% replace quantity pointer of one of these with the other in all relations

% don't analyse when resolving influences/proportional relations

% FL: in GARP 2.0 two extra arguments: adders in /out
% these also contain pointers and should also be updated.
% This one list should be left intact during the changes: it actually is two lists.
% (programmers frivolity not to need 4 extra arguments)

% New may 07: FIX to incorporate
/*
analyse_simple_equality(false, _, Q, Q, N, N, Relations, Relations,
	Derivable, Derivable, C, C, A, A).

analyse_simple_equality(true, [], Q, Q, N, N, Relations, Relations,
	Derivable, Derivable, C, C, A, A).

analyse_simple_equality(true, [ relation(Left, =, Right) | Tail ], Q, NQ,
 N, NN, Relations, NewRelations, Derivable, NewDerivable, C, NC, A, NA) :-
	map_list(Left, [ ILeft ]),
	map_list(Right, [ IRight ]),
	% NEW fl may 07: do not replace zero pointers... analyze zero takes care of that...
         ILeft \== 0,
	 IRight \== 0,
	% should update later and remove analyze zero procedure:
	% if x = 0 or 0 = x is derived, it is always the zero pointer that is inserted
	% and in additions the zero pointer is left out. eg. x = 0, x + y = z -> y = z instead of y + 0 = z
	% like in process zero pointer
	replace_index_relations(ILeft, IRight, N, TN),
	replace_index_relations(ILeft, IRight, Relations, TRelations),
	replace_index_relations(ILeft, IRight, Derivable, TDerivable),
	replace_index_relations(ILeft, IRight, Tail, NTail),
	replace_index_relations_for_correspondence(ILeft, IRight, C, TC),
	% fails if that would produce a 'double quantity'
	% sets flag if contradiction found
	replace_index_in_adders(ILeft, IRight, A, TA), % Note the adder pointers are updated
	!,
	replace_index(ILeft, IRight, Q, TQ),
	analyse_simple_equality(true, NTail, TQ, NQ, TN, NN, TRelations,
		NewRelations, TDerivable, NewDerivable, TC, NC, TA, NA).

analyse_simple_equality(true, [ _ | Tail ], Q, NQ, N, NN, Relations,
		NewRelations, Derivable, NewDerivable, C, NC, A, NA) :-
	flag(analyse_simple_found_contradiction, false, false),
    analyse_simple_equality(true, Tail, Q, NQ, N, NN, Relations,
		NewRelations, Derivable, NewDerivable, C, NC, A, NA).
*/


/*
replace_index_relations_for_correspondence(_, _, [], [], _).
replace_index_relations_for_correspondence(I1, I2,
	[ correspondence(L1, L2)/BV1/BV2 | T ], [correspondence(NL1, NL2)/NBV1/NBV2 | NT], Remove):-
	replace_index_relations_cor(I1, I2, L1, NL1),
	replace_index_relations_cor(I1, I2, L2, NL2),
	replace_context_pointer(Remove, _, BV1, NBV1),
	replace_context_pointer(Remove, _, BV2, NBV2),
	!,
	replace_index_relations_for_correspondence(I1, I2, T, NT, Remove).
replace_index_relations_for_correspondence(I1, I2,
	[ if_correspondence(L1, L2)/BV1/BV2 | T ], [if_correspondence(NL1, NL2)/NBV1/NBV2 | NT], Remove):-
	replace_index_relations_cor(I1, I2, L1, NL1),
	replace_index_relations_cor(I1, I2, L2, NL2),
	replace_context_pointer(Remove, _, BV1, NBV1),
	replace_context_pointer(Remove, _, BV2, NBV2),
	!,
	replace_index_relations_for_correspondence(I1, I2, T, NT, Remove).
*/





































/*** FL december-2003: END NEW ***/


/*----------------- analyse zero equality -------------------------*/

/* FL januari 2004 : analyse_zero_equality  UPDATE FL mar 07: bitversion
*
* in Analyse-simple-equality, y is replaced by x if x=y,
* to minimize the set of relations even further this is also done if:
* q = zero & p = zero because p = q is now derivable in theory.
*
* To do this efficiently all pointers to quantities equal to zero are put in a
* list. Then all these pointers are replaced by the pointer 0,
* Note that pointers start counting at 1 so 0 is always free.
* A lot of empty relations will come up like:  0 = 0, etc. these are removed
* Some sums will contain a zero: x + 0 > y, these extra zeros are removed: x > y
* Then zero = 0 is added to the relationsset.
*
* quantities are equal to zero if: q = zero, or if q >= zero & zero >= q,
*
* FL march 2004: The latter being unnecessary now that solve can deduct q=zero
* in this case.
*
* Note that zero is mapped to the empty list: [].
*
* When applying the zeroset (changing pointers) relations are checked
* on contradictions also. eg. 0>0, In that case the operation fails. (message displayed)
*
* This operation is done exhaustively on all known relations, not only on new relations,
* arguments:
* - the list with pointers equal to zero is easily obtained
* - some call's do not analyse zero equality, but the smaller the relationset,
*   the faster solve works.
* - double quantity equalities will not be affected.
*
* the operation is only performed when new relations contain a zero equality
* this lowers the number of times this operation is carried out & saves time.
* FL mar 07: NB it is done now always unless no relation in any set has a zero equality
*
* In Adders and Correspondences indexes are replaced also,
* empty relations are not removed here. In correspondences they can do no harm.
*
*
*
* List notation is used, c-predicates for map operations being inadequate:
* Todo: analyse-zero-equality using map operations and new c predicates.
* Not neccesary however for time complexity is not a problem in practice
*
*/
/*
% analyse simple equality set to false: return input

analyse_zero_equality(false, New, New, Derivable, Derivable,
                      Base, Base, QList, QList, Corr, Corr, Adders, Adders).
*/

/* Switch removed, do it always, FL
% analyse zero equality set to false in switch: return input
analyse_zero_equality(_, New, New, Derivable, Derivable,
                      Base, Base, QList, QList, Corr, Corr, Adders, Adders):-
                      flag(no_analyse_zero, Flag, Flag),
                      algorithm_assumption_flag(Flag, true, no_analyse_zero),
                      !.
*/

/*
% relations do not contain a zero equality: return input bitvector version

analyse_zero_equality(true, New, New, Derivable, Derivable,
                      Base, Base, QList, QList, Corr, Corr, Adders, Adders):-
    % smallest
    % map_list_for_relation_set(New, New1),
    %(select(relation(bitvector(1), =, bitvector(0)), New, NewR) ; New = NewR),
    \+ contains_equality_to_zero(New),
    % next
    % map_list_for_relation_set(Base, Base1),
    %(select(relation(bitvector(1), =, bitvector(0)), Base, BaseR) ; Base = BaseR),
    \+ contains_equality_to_zero(Base),
    % biggest set last
    % map_list_for_relation_set(Derivable, Derivable1),
    %(select(relation(bitvector(1), =, bitvector(0)), Derivable, DerivableR) ; Derivable = DerivableR),
    \+ contains_equality_to_zero(Derivable),
    !.


% analyse zero equality bitvector version

analyse_zero_equality(true, NewIn, NewOut, DerivableIn, DerivableOut,
                      BaseIn, BaseOut, QListIn, QListOut, CorrIn, CorrOut, AddersIn, AddersOut):-
    % find all zero indexes
     %
%    map_list_for_relation_set(NewIn, New),
%    map_list_for_relation_set(DerivableIn, Derivable),
%    map_list_for_relation_set(BaseIn, Base),
%    map_list_for_correspondences(CorrIn, Corr),
%    map_list_for_adders(AddersIn, Adders),
%    get_zero_set(Derivable, ZerosL),
%    delete(ZerosL, 0, ZerosetL), % after the first zero-analysis, 0 is always part of the list

    get_bitwise_zero_set(DerivableIn, Zeros, BitZeros),


 %   (Zeros = ZerosL ; stop),


    delete(Zeros, 0, Zeroset), % after the first zero-analysis, 0 is always part of the list
    % delete bitwise 0 (NB bitmap 1 corresponds to [0])
    NotZero is \1,
    BitZeroset is BitZeros /\ NotZero,
    InvBitZeroset is \BitZeros,

%     (Zeroset = ZerosetL ; stop),

%    apply_zero_set(Derivable, Zeroset, Derivable1),
%    apply_zero_set(New, Zeroset, New1),
%    apply_zero_set(Base, Zeroset, Base1),
%    apply_zero_set_correspondences(Corr, Zeroset, Corr1),
%    apply_zero_set_adders(Adders, Zeroset, Adders1),
        % convert back to map representation
%    list_map_for_relation_set(New1, NewOutL),
%    list_map_for_relation_set(Derivable1, DerivableOutL),
%    list_map_for_relation_set(Base1, BaseOutL),
%    list_map_for_correspondences(Corr1, CorrOutL),
%    list_map_for_adders(Adders1, AddersOutL),

        % change all zero indexes to 0,
        % remove extra 0's & evident relations,
        % add [] = [0],
        % find contradiction?  x>x then fail
    apply_bitwise_zero_set(DerivableIn, InvBitZeroset, DerivableOut),
    apply_bitwise_zero_set(NewIn, InvBitZeroset, NewOut),
    apply_bitwise_zero_set(BaseIn, InvBitZeroset, BaseOut),
    apply_bitwise_zero_set_correspondences(CorrIn, InvBitZeroset, CorrOut),
    apply_bitwise_zero_set_adders(AddersIn, BitZeroset, InvBitZeroset, AddersOut),


%    (NewOutL == NewOut ; stop),
%    ( DerivableOut == DerivableOutL ; stop),
%    ( BaseOut == BaseOutL ; stop),
%    ( CorrOut == CorrOutL ; stop),
%    ( AddersOut == AddersOutL ; stop),


    % update Qlist
    replace_indexes_with_zeros(Zeroset, QListIn, QListOut).



*/



%*-------------- Analyse zero equality tools: ----------------------*
/*
% set of relations has at least one relation to zero. memberchk version with bitvectors...:)

contains_equality_to_zero(List):-
	memberchk(relation(bitvector(1), =, bitvector(_)), List),!.

contains_equality_to_zero(List):-
	memberchk(relation(bitvector(_), =, bitvector(1)), List),!.


% get_bitwise_zero_set(+Relations, -Zeroset, -Bitwisezeros)
% FL mar 07: bitwise version
% find all quantities / pointers equal to zero in a relationset:
% intern-list representation, use q = [] or q>=[] /\ []>=q
% NOW: q = bitvector(0) etc.  NB [] = bitvector(0), [0] is bitvector(1),

% done
get_bitwise_zero_set([], [], 0).

% X equal to zero, add to zeroset
get_bitwise_zero_set([relation(bitvector(X), =, bitvector(1))|Tail], [Xindex|ZTail], BitZeros):-
	% X is single item: e.g. list: [3], not addition like [3, 5]
	1 is popcount(X),
	!,
	map_list(bitvector(X), [Xindex]),
	get_bitwise_zero_set(Tail, ZTail, BZTail),
	% take the union of tail and X
	BitZeros is BZTail \/ X.

% X equal to zero, add to zeroset
get_bitwise_zero_set([relation(bitvector(1), =, bitvector(X))|Tail], [Xindex|ZTail], BitZeros):-
	% X is single item: e.g. list: [3], not addition like [3, 5]
	1 is popcount(X),
	!,
	map_list(bitvector(X), [Xindex]),
	get_bitwise_zero_set(Tail, ZTail, BZTail),
	% take the union of tail and X
	BitZeros is BZTail \/ X.

% X equal or greater to zero if also inverse then add to zeroset
get_bitwise_zero_set([relation(bitvector(X), >=, bitvector(1))|Tail], [Xindex|ZTail], BitZeros):-
	% X is single item: e.g. list: [3], not addition like [3, 5]
	1 is popcount(X),
	bitwise_combine_to_zero(right, X, Tail),
	!,
	map_list(bitvector(X), [Xindex]),
	get_bitwise_zero_set(Tail, ZTail, BZTail),
	% take the union of tail and X
	BitZeros is BZTail \/ X.


% X equal or greater to zero if also inverse then add to zeroset
get_bitwise_zero_set([relation(bitvector(1), >=, bitvector(X))|Tail], [Xindex|ZTail], BitZeros):-
	% X is single item: e.g. list: [3], not addition like [3, 5]
	1 is popcount(X),
	bitwise_combine_to_zero(left, X, Tail),
	!,
	map_list(bitvector(X), [Xindex]),
	get_bitwise_zero_set(Tail, ZTail, BZTail),
	% take the union of tail and X
	BitZeros is BZTail \/ X.

% no zero equality, next relation
get_bitwise_zero_set([_|Tail], ZTail, BitZeros):-
    get_bitwise_zero_set(Tail, ZTail, BitZeros).


bitwise_combine_to_zero(right, X, Relations):-
    memberchk(relation(bitvector(1), >=, bitvector(X)), Relations).

bitwise_combine_to_zero(left, X, Relations):-
    memberchk(relation(bitvector(X), >=, bitvector(1)), Relations).



% apply zeros for Qlist, use analyse simple equality predicate. NB still List operated!
replace_indexes_with_zeros([], In, In).

replace_indexes_with_zeros([H|T], In, Out):-
    replace_index(H, 0, In, New),
    replace_indexes_with_zeros(T, New, Out).


% apply_zero_set(+RelationsIn, +Zeroset, -RelationsOut)
% apply zeros to set of relations in intern-list representation
% append the relation 0 = zero, which is removed because evident.

apply_bitwise_zero_set(RelationsIn, IZeroset, RelationsOut):-
    apply_bitwise_zeros(RelationsIn, IZeroset, RelationsOut). % Relations1),    % fails in case of contradiction
    %append([relation(bitvector(1), =, bitvector(0))], Relations1, RelationsOut).


% apply_zeros(+RelationsIn, +Zeroset, -RelationsOut)
% apply zeros to set of relations in intern-bit representation,
% fails with error report in case of a contradiction

apply_bitwise_zeros([], _, []).

% valid new relation after zero application, return, continue with rest

apply_bitwise_zeros([relation(X, Rel, Y)|Tail], IZeros, Out):-
    % change indexes in zeroset to zero, leave one zero if no other indexes.
    change_bitwise_indexes_to_one_zero(X, IZeros, X1),
    change_bitwise_indexes_to_one_zero(Y, IZeros, Y1),
    bitwise_check_relation(X1, Rel, Y1, Result), % (0=0 (empty)) or x>x (contradiction) or x=x evident or x?y(normal)
    !,
    determine_b_c_r_result(Result, relation(X1, Rel, Y1), NewTail, Out),
    apply_bitwise_zeros(Tail, IZeros, NewTail).

% keep relation
determine_b_c_r_result(normal, relation(X, Rel, Y), NewTail, [relation(X, Rel, Y)|NewTail]).

% discard relation
determine_b_c_r_result(evident, _, Out, Out).

% fail on contradiction
determine_b_c_r_result(contradiction, _, _, _):-
	etrace(solve_zero_found_invalid, _, add_relation),
	!,
	fail.

% change_indexes_to_one_zero(+BitmapIn, +Zerosmap, -BitmapOut)
% remove all elements in zeros from list, leave one zero if no other
% element/pointer is present
% eg. [4, 5], zeros=5 -> [4]
% eg. [5], zeros=5 -> [0]
% eg. [], zeros=5 -> []
change_bitwise_indexes_to_one_zero(bitvector(0), _InvZeromap, bitvector(0)):-
	!.
change_bitwise_indexes_to_one_zero(bitvector(In), InvZeromap, bitvector(Out)):-
	Out1 is In /\ InvZeromap, % subtract the zeroset
	(
	  Out1 == 0 % []
	->
	  Out = 1  %-> [0]
	;
	  Out1 = Out % -> [X, Y, Z]
	).


% check relation for information value

bitwise_check_relation(X, Rel, X, Result):-
    !,
    (Rel == >
    ->
    Result = contradiction % x > x is impossible
    ;
    Result = evident).  % x = x or x >= x is evident.

bitwise_check_relation(X, Rel, Y, Result):-
    %list is empty or contains only 0's
    only_bitwise_zeros_in_list(X),
    only_bitwise_zeros_in_list(Y),
    !,
    (Rel == >
    ->
    Result = contradiction  % zero > zero is impossible
    ;
    Result = evident).    % zero = zero is evident empty/no information value

% case else: normal
bitwise_check_relation(_X, _Rel, _Y, normal).



% succeed if list is empty or contains only 0's
% bitwise check is very easy... :)
only_bitwise_zeros_in_list(bitvector(0)).

only_bitwise_zeros_in_list(bitvector(1)).


% apply for correspondences & adders


% apply_zero_set_correspondences(+CorrespondencesIn, +Zeroset, -CorrespondencesOut)
% Pointers in correspondences (intern-list representation) need to be updated.
% all relations are returned:
% an if_correspondence([relation( 0 > 0), ...) will not fire
% an if_correspondence([relation( 0 = 0), ...) will fire
% so information is preserved, however, correspondences containing no
% information are formed. They can do no harm though.

apply_bitwise_zero_set_correspondences([], _IZeroset, []).

apply_bitwise_zero_set_correspondences([H|T], IZeroset, [NH|NT]):-
    H =.. [C, Relations1, Relations2],
    put_all_bitwise_zero_indexes(Relations1, IZeroset, NR1),
    put_all_bitwise_zero_indexes(Relations2, IZeroset, NR2),
    NH =.. [C, NR1, NR2],
    apply_bitwise_zero_set_correspondences(T, IZeroset, NT).


% apply_zero_set_adders(+AddersIn, +Zeroset, +AddersOut)
% Pointers in adders (intern-list representation) need to be updated also.

apply_bitwise_zero_set_adders([], _Zeroset, _InvZeroset, []).

apply_bitwise_zero_set_adders([H|T], Zeroset, InvZeroset, [NH|NT]):-
    H =.. [adder, Q, List],
    replace_bitwise_zero_indexes(Q, InvZeroset, NQ),
    put_all_bitwise_zero_indexes_addlist(List, Zeroset, NList),
    NH =.. [adder, NQ, NList],
    apply_bitwise_zero_set_adders(T, Zeroset, InvZeroset, NT).


%replace all zero indexes with 0 in a relation.
put_all_bitwise_zero_indexes([], _, []).

put_all_bitwise_zero_indexes([relation(X, Rel, Y)|Tail], IZList,[relation(X1, Rel, Y1)|NTail]):-
    replace_bitwise_zero_indexes(X, IZList, X1),
    replace_bitwise_zero_indexes(Y, IZList, Y1),
    !,
    put_all_bitwise_zero_indexes(Tail, IZList, NTail).

% do not replace empty: [] with zero [0]
replace_bitwise_zero_indexes(bitvector(0), _Zeromap, bitvector(0)):-
	!.
replace_bitwise_zero_indexes(bitvector(In), InvZeromap, bitvector(Out)):-
	%InverseZeros is \Zeromap,
	Out1 is In /\ InvZeromap, %subtract the zeroset.
	(
	  Out1 == 0 % []
	->
	  Out = 1  %-> [0]
	;
	  Out1 = Out % -> [X, Y, Z]
	).


% replace all zero indexes with 0 in an addlist.
% eg. [pos(X), neg(Y)]

put_all_bitwise_zero_indexes_addlist([], _, []).

put_all_bitwise_zero_indexes_addlist([H|T], ZMap,[NH|NT]):-
    H =.. [X, bitvector(Q)],
    Res is Q /\ ZMap,
    (
      Res == 0 % Q is not in Zmap
    ->
      NQ = Q
    ;
      NQ = 1  % bitvector 1 is the zero pointer: [0]
    ),
    !,
    NH =.. [X, bitvector(NQ)], % X could be: noforce, saves evaluation later on...
    put_all_bitwise_zero_indexes_addlist(T, ZMap, NT).


*/



/*** FL januari 2004: NEW GARP 2.0 SPECIFIC ENDS HERE ***/



/* correspondence relations
   inspected when a correspondence relation is found, or
   when a new relation is derived
 */

/* FL 7-3-2004: Old version: has a...
* Bug: when analyse simple encounters an x=y relation, it replaces all y's with
* x and removes the x=y (now x=x) relation. If this x=y relation is the condition part of
* a correspondence, this results in an empty list. The empty list should mean:
* no conditions -> fire correspondence. But the empty list fails the is_derivable
* check. (common_select fails on the empty list).
* an empty list as consequence is of no concern; append_relation_all is a list
* operation and has no problem with empty lists.

inspect_correspondence(Relations, Cin, Cout):-
	inspect_correspondence(Relations, Cin, Cout, []).

inspect_correspondence(Relations, Cin, Cout, Previous):-
	cio_c_nc_d(Cin, Cnew, Correspondence, RCorrespondence, Derivable),
(	common_select(Correspondence,
		correspondence(L1, L2),
		RCorrespondence)
;	common_select(Correspondence,
		correspondence(L2, L1),
		RCorrespondence)
;	common_select(	Correspondence,
		if_correspondence(L1, L2),
		RCorrespondence)
),
	common_select(L1, One, RL1),
	is_derivable(One, Relations),
	check_derivable(RL1, Derivable),
	append(Previous, L2, ToAdd),
	!,
	inspect_correspondence(Relations, Cnew, Cout, ToAdd).

inspect_correspondence(_, Cio, Cio, []):- !.
inspect_correspondence(_, Cin, Cout, ToAdd):-
	append_relation_all(ToAdd, Cin, Cout, _, true).

*** New version below: ***/



/*
% fails on empty list
determine_pointers_list([H], [H/BV], BV):-
	!,
	determine_pointers(H, BV).

determine_pointers_list([H|T], [H/BV|NT], NBV):-
	determine_pointers(H, BV),
	determine_pointers_list(T, NT, TBV),
	map_union(BV, TBV, NBV).
*/


/*
check_context_derivable_set([], _).
check_context_derivable_set([Rel|T], Contexts):-
	check_context_derivable(Rel, Contexts),
	check_context_derivable_set(T, Contexts).

*/

/*
	% FL may 07, passing on adders now for pointer updates...
inspect_correspondence(Relations, Cin, Cout, DoAnalyseSimple):-
	inspect_correspondence(Relations, Cin, Cout, [], DoAnalyseSimple, [], []).

inspect_correspondence(Relations, Cin, Cout, DoAnalyseSimple, AddersIn, AddersOut):-
	inspect_correspondence(Relations, Cin, Cout, [], DoAnalyseSimple, AddersIn, AddersOut).

inspect_correspondence(Relations, Cin, Cout, Previous, DoAnalyseSimple, AddersIn, AddersOut):-
	cio_c_nc_d(Cin, Cnew, Correspondence, RCorrespondence, Derivable),
	determine_pointers_list(Relations, _, RBV),   % if nothing in common,  it cannot fire

	( common_select(Correspondence,
		correspondence(L1, L2)/BV/_,
		RCorrespondence)
	;	common_select(Correspondence,
		correspondence(L2, L1)/BV/_,
		RCorrespondence)
	;	common_select(	Correspondence,
		if_correspondence(L1, L2)/BV/_,
		RCorrespondence)
	),

	(
	L1 = [] % An empty list means no conditions: fire correspondence.

	;

	map_intersection(BV, RBV, bitvector(Common)), % could this be more strict?
	Common =\= 0,

	common_select(L1, One, RL1),
	is_derivable(One, Relations),
	check_derivable(RL1, Derivable)
	),
	append(Previous, L2, ToAdd),
	!,
	inspect_correspondence(Relations, Cnew, Cout, ToAdd, DoAnalyseSimple, AddersIn, AddersOut).

inspect_correspondence(_, Cio, Cio, [], _, Adders, Adders):- !.
inspect_correspondence(_, Cin, Cout, ToAdd, DoAnalyseSimple, AddersIn, AddersOut):-
	append_relation_all(ToAdd, Cin, Cout, _, DoAnalyseSimple, AddersIn, AddersOut).

control_corr([]).
control_corr([H/_/_|T]):-
	H=..[_, L1, L2],
	all_canonical(L1),
	all_canonical(L2),
	control_corr(T).
all_canonical(_):-!.
all_canonical([]).
all_canonical([H|T]):-
	(
	  canonical(H)
	;
	  stop
	),
	!,
	all_canonical(T).

canonical(relation(L, =, R)):-
	!,
	sort([L, R], [L, R]).
canonical(_).

*/
/*
% FL nov 07, new context version: Only check relevant correspondences.
% Only use relevant derivable.

% FL may 07, passing on adders now for pointer updates...
inspect_correspondence(Relations, Cin, Cout, DoAnalyseSimple):-
	inspect_correspondence(Relations, Cin, Cout, DoAnalyseSimple, [], []).

inspect_correspondence([], Cio, Cio, _DoAnalyseSimple, Adders, Adders).

inspect_correspondence([Relation|T], Cin, Cout, DoAnalyseSimple, AddersIn, AddersOut):-
	cio_c(Cin, Corr),
	determine_pointers(Relation, BV),
	relevant_correspondences(BV, Corr, Relevant),
	Relevant \= [],
	!,
	cio_r(Cin, Contexts),
	(relevant_contexts(BV, Contexts, [context(_, Derivable, _, _)], _); stop),
	!,
	inspect_correspondence2(Relation, Relevant, Derivable, Cin, Cnew, [], DoAnalyseSimple, AddersIn, AddersNew),
	inspect_correspondence(T, Cnew, Cout, DoAnalyseSimple, AddersNew, AddersOut).

%no relevant correspondences, skip
inspect_correspondence([_Relation|T], Cin, Cout, DoAnalyseSimple, AddersIn, AddersOut):-
	inspect_correspondence(T, Cin, Cout, DoAnalyseSimple, AddersIn, AddersOut).



% inspected all: but nothing to append
inspect_correspondence2(_Relation, [], _D, Cio, Cio, [], _DoAnalyseSimple, Adders, Adders):-
	!.

% inspected all: append consequential relations
inspect_correspondence2(_Relation, [], _D, Cin, Cout, ToAdd, DoAnalyseSimple, AddersIn, AddersOut):-
	append_relation_all(ToAdd, Cin, Cout, _, DoAnalyseSimple, AddersIn, AddersOut).

% check a relevant correspondence. > fires
inspect_correspondence2(Relation, [R/_/_|T], Derivable, Cin, Cout, Previous, DoAnalyseSimple, AddersIn, AddersOut):-
	(
	  R = correspondence(L1, L2)
	;
	  R = correspondence(L2, L1)
	;
	  R = if_correspondence(L1, L2)
	),
	(
	common_select(L1, One, RL1),
	is_derivable(One, [Relation]),
	check_derivable(RL1, Derivable)
	;       % new:
	L1 = [] % An empty list means no conditions: fire correspondence.
	),
	!,
	append(Previous, L2, ToAdd),
	cio_c_nc(Cin, Cnew, C, NC),
	delete(C, R, NC),
	inspect_correspondence2(Relation, T, Derivable, Cnew, Cout, ToAdd, DoAnalyseSimple, AddersIn, AddersOut).

% check a relevant correspondence. > does not fire
inspect_correspondence2(Relation, [_R|T], Derivable, Cin, Cout, ToAdd, DoAnalyseSimple, AddersIn, AddersOut):-
	inspect_correspondence2(Relation, T, Derivable, Cin, Cout, ToAdd, DoAnalyseSimple, AddersIn, AddersOut).



relevant_correspondences(_BV, [], []).

relevant_correspondences(BV, [C/CBV/CCV|CorrT], [C/CBV/CCV|Relevant]):-
	map_intersection(BV, CBV, bitvector(Common)), % could this be more strict?
	Common =\= 0,
	!,
	relevant_correspondences(BV, CorrT, Relevant).

relevant_correspondences(BV, [_|CorrT], Relevant):-
	relevant_correspondences(BV, CorrT, Relevant).


*/



/*
% strictly_relevant_contexts(+RelPointers, +RelationContexts, -RelevantContexts):-
% returns contexts that are supersets of the pointerset.

strictly_relevant_contexts(_RP, [], [], []).

% relevant_context
strictly_relevant_contexts(RP, [context(R, D, CP)|RT], [context(R, D, CP)|CT], OT):-
	map_union(RP, CP, CP), %RP is subset of CP
	!,
	strictly_relevant_contexts(RP, RT, CT, OT).

% other context
strictly_relevant_contexts(RP, [H|RT], CT, [H|OT]):-
	strictly_relevant_contexts(RP, RT, CT, OT).

*/



/*
inspect_a_correspondence(correspondence(L1, L2)/BV/_, Cin, Cout):-
	cio_r(Cin, R),
	%strictly_relevant_contexts(BV, R, Contexts, _),
	relevant_contexts(BV, R, Contexts, _),
	(
	  (
	    check_context_derivable_set(L1, Contexts),
	    !,
	    append_relation_all(L2, Cin, Cout, _, true)
	  )
	;
	  (
	    check_context_derivable_set(L2, Contexts),
	    !,
	    append_relation_all(L1, Cin, Cout, _, true)
	  )
	).


inspect_a_correspondence(if_correspondence(L1, L2)/BV/_, Cin, Cout):-
	cio_r(Cin, R),
	%strictly_relevant_contexts(BV, R, Contexts, _),
	relevant_contexts(BV, R, Contexts, _),
	check_context_derivable_set(L1, Contexts),
	!,
	append_relation_all(L2, Cin, Cout, _, true).

inspect_a_correspondence(_, Cio, Cio).

inspect_some_correspondences([], Cio, Cio).
inspect_some_correspondences([H|T], Cin, Cout):-
	inspect_a_correspondence(H, Cin, Cnew),
	inspect_some_correspondences(T, Cnew, Cout).

*/
/*


inspect_a_correspondence(correspondence(L1, L2)/BV/_, Cin, Cout):-
	cio_d(Cin, Derivable),
	check_derivable(L1, Derivable),
	!,
	append_relation_all(L2, Cin, Cout, _, true).

inspect_a_correspondence(correspondence(L1, L2)/BV/_, Cin, Cout):-
	cio_d(Cin, Derivable),
	check_derivable(L2, Derivable),
	!,
	append_relation_all(L1, Cin, Cout, _, true).

inspect_a_correspondence(if_correspondence(L1, L2)/BV/_, Cin, Cout):-
	cio_d(Cin, Derivable),
	check_derivable(L1, Derivable),
	!,
	append_relation_all(L2, Cin, Cout, _, true).

inspect_a_correspondence(_, Cio, Cio).

*/


/*************** old listversion analyse zero preds..
%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%
%
%
%
%



% get_zero_set(+Relations, -Zeroset)
% find all quantities / pointers equal to zero in a relationset:
% intern-list representation, use q = [] or q>=[] /\ []>=q

% done
get_zero_set([], []).

% X equal to zero, add to zeroset
get_zero_set([relation([X], =, [])|Tail], [X|ZTail]):-
    !,
    get_zero_set(Tail, ZTail).

% X equal to zero, add to zeroset
get_zero_set([relation([], =, [X])|Tail], [X|ZTail]):-
    !,
    get_zero_set(Tail, ZTail).

% X equal or greater to zero if also inverse then add to zeroset
get_zero_set([relation([X], >=, [])|Tail], [X|ZTail]):-
    combine_to_zero(right, X, Tail),
    !,
    get_zero_set(Tail, ZTail).

% X equal or greater to zero if also inverse then add to zeroset
get_zero_set([relation([], >=, [X])|Tail], [X|ZTail]):-
    combine_to_zero(left, X, Tail),
    !,
    get_zero_set(Tail, ZTail).

% no zero equality, next relation
get_zero_set([_|Tail], ZTail):-
    get_zero_set(Tail, ZTail).


combine_to_zero(right, X, Relations):-
    memberchk(relation([], >=, [X]), Relations).

combine_to_zero(left, X, Relations):-
    memberchk(relation([X], >=, []), Relations).
% apply_zero_set(+RelationsIn, +Zeroset, -RelationsOut)
% apply zeros to set of relations in intern-list representation
% append the relation 0 = zero, which is removed because evident.

apply_zero_set(RelationsIn, Zeroset, RelationsOut):-
    apply_zeros(RelationsIn, Zeroset, Relations1),    % fails in case of contradiction
    append([relation([0], =, [])], Relations1, RelationsOut).


% apply_zeros(+RelationsIn, +Zeroset, -RelationsOut)
% apply zeros to set of relations in intern-list representation,
% fails with error report in case of a contradiction

apply_zeros([], _, []).

% valid new relation after zero application, return, continue with rest

apply_zeros([relation(X, Rel, Y)|Tail], Zeros, [relation(X1, Rel, Y1)|NewTail]):-
    % change indexes in zeroset to zero, leave one zero if no other indexes.
    change_indexes_to_one_zero(X, Zeros, X1, false),
    change_indexes_to_one_zero(Y, Zeros, Y1, false),
    \+ empty_relation(X1, Rel, Y1, _Result),
    !,
    %\+ invalid_relation(relation(X1, Rel, Y1)), % X > X (contradiction) fail (FL July 2004: superfluous empty relation checks for contradiction)
    %!,
    apply_zeros(Tail, Zeros, NewTail).

% empty relation after zero application, discard, continue with rest

apply_zeros([relation(X, Rel, Y)|Tail], Zeros, NewTail):-
    % change indexes in zeroset to zero, leave one zero if no other indexes.
    change_indexes_to_one_zero(X, Zeros, X1, false),
    change_indexes_to_one_zero(Y, Zeros, Y1, false),
    empty_relation(X1, Rel, Y1, Result),% 0 = 0 (empty) or x>x (contradiction) or x=x evident
    memberchk(Result, [empty, evident]),
    !,
    apply_zeros(Tail, Zeros, NewTail).


% change_indexes_to_one_zero(+ListIn, +Zeros, -ListOut, +false)
% remove all elements in zeros from list, leave one zero if no other
% element/pointer is present (indicater set to true).
% eg. [4, 5], zeros=5 -> [4]
% eg. [5], zeros=5 -> [0]

% done
change_indexes_to_one_zero([], _, [], _).

% no other pointers unequal to zero return [0]
change_indexes_to_one_zero([X], Zeros, [0], false):-
    memberchk(X, Zeros),!.

% other pointers found before, return empty
change_indexes_to_one_zero([X], Zeros, [], true):-
    memberchk(X, Zeros),!.

% nonzero pointer, return it.
change_indexes_to_one_zero([X], _, [X], _).

% zero pointer in front, remove from list.
change_indexes_to_one_zero([X, Y|Tail], Zeros, NewTail, NonZeroInList):-
    memberchk(X, Zeros),!,
    change_indexes_to_one_zero([Y|Tail], Zeros, NewTail, NonZeroInList).

% nonzero pointer, return it, set indicator, to true
change_indexes_to_one_zero([X, Y|Tail], Zeros, [X|NewTail], _NonZeroInList):-
    change_indexes_to_one_zero([Y|Tail], Zeros, NewTail, true).




% check relation for information value

empty_relation(X, Rel, X, Result):-
    !,
    (Rel == >
    ->
    Result = contradiction, % x > x is impossible
    etrace(solve_zero_found_invalid, _, add_relation)
    ;
    Result = evident).  % x = x or x >= x is evident.

empty_relation(X, Rel, Y, Result):-
    %list is empty or contains only 0's
    only_zeros_in_list(X),
    only_zeros_in_list(Y),
    !,
    (Rel == >
    ->
    Result = contradiction,  % zero > zero is impossible
    etrace(solve_zero_found_invalid, _, add_relation)
    ;
    Result = empty).    % zero = zero is evident empty/no information value




% succeed if list is empty or contains only 0's
only_zeros_in_list([]).

only_zeros_in_list([0|T]):-
    only_zeros_in_list(T).


% * apply for correspondences & adders *


% apply_zero_set_correspondences(+CorrespondencesIn, +Zeroset, -CorrespondencesOut)
% Pointers in correspondences (intern-list representation) need to be updated.
% all relations are returned:
% an if_correspondence([relation( 0 > 0), ...) will not fire
% an if_correspondence([relation( 0 = 0), ...) will fire
% so information is preserved, however, correspondences containing no
% information are formed. They can do no harm though.

apply_zero_set_correspondences([], _Zeroset, []).

apply_zero_set_correspondences([H|T], Zeroset, [NH|NT]):-
    H =.. [C, Relations1, Relations2],
    put_all_zero_indexes(Relations1, Zeroset, NR1),
    put_all_zero_indexes(Relations2, Zeroset, NR2),
    NH =.. [C, NR1, NR2],
    apply_zero_set_correspondences(T, Zeroset, NT).


% apply_zero_set_adders(+AddersIn, +Zeroset, +AddersOut)
% Pointers in adders (intern-list representation) need to be updated also.

apply_zero_set_adders([], _Zeroset, []).

apply_zero_set_adders([H|T], Zeroset, [NH|NT]):-
    H =.. [adder, Q, List],
    replace_zero_indexes(Q, Zeroset, NQ),
    put_all_zero_indexes_addlist(List, Zeroset, NList),
    NH =.. [adder, NQ, NList],
    apply_zero_set_adders(T, Zeroset, NT).


%replace all zero indexes with 0 in a relation.
put_all_zero_indexes([], _, []).

put_all_zero_indexes([relation(X, Rel, Y)|Tail], ZList,[relation(X1, Rel, Y1)|NTail]):-
    replace_zero_indexes(X, ZList, X1),
    replace_zero_indexes(Y, ZList, Y1),
    !,
    put_all_zero_indexes(Tail, ZList, NTail).

replace_zero_indexes([], _, []).

replace_zero_indexes([I|T], ZList, [0|NT]):-
    memberchk(I, ZList),
    !,
    replace_zero_indexes(T, ZList, NT).

replace_zero_indexes([H|T], ZList, [H|NT]):-
    replace_zero_indexes(T, ZList, NT).


% replace all zero indexes with 0 in an addlist.
% eg. [pos(X), neg(Y)]

put_all_zero_indexes_addlist([], _, []).

put_all_zero_indexes_addlist([H|T], ZList,[NH|NT]):-
    H =.. [X, [Q]],
    memberchk(Q, ZList),
    !,
    NH =.. [X, [0]], % X could be: noforce, saves evaluation later on...
    put_all_zero_indexes_addlist(T, ZList, NT).

put_all_zero_indexes_addlist([H|T], ZList,[H|NT]):-
    put_all_zero_indexes_addlist(T, ZList, NT).


%*--------------special list to map and back tools for sets--------*%

% list_map_for_relation_set(+InternRelationsIn, -InternListRelationsOut)
% From LIST to Map
% convert bitmap representation to list (map_list), for a whole
% set of relations (intern representation),
% making them intern-list representation

list_map_for_relation_set([], []).

list_map_for_relation_set([relation(X, Rel, Y)|Tail], [relation(X1, Rel, Y1)|NewTail]):-
    list_map(X, X1),
    list_map(Y, Y1),
    list_map_for_relation_set(Tail, NewTail).


% map_list_for_relation_set(+InternListRelationsOut, -InternRelationsIn)
% From MAP to LIST
% convert list representation to bitmap (list_map), for a whole
% set of relations (intern-list representation),
% making them intern representation

map_list_for_relation_set([], []).

map_list_for_relation_set([relation(X, Rel, Y)|Tail], [relation(X1, Rel, Y1)|NewTail]):-
    map_list(X, X1),
    map_list(Y, Y1),
    map_list_for_relation_set(Tail, NewTail).


% From LIST to Map for correspondences

list_map_for_correspondences([], []).

list_map_for_correspondences([H|T], [NH|NT]):-
    H =.. [C, Relations1, Relations2],
    list_map_for_relation_set(Relations1, NR1),
    list_map_for_relation_set(Relations2, NR2),
    NH =.. [C, NR1, NR2],
    list_map_for_correspondences(T, NT).


% From LIST to Map for adders

list_map_for_adders([], []).

list_map_for_adders([H|T], [NH|NT]):-
    H =.. [adder, Q, List],
    list_map(Q, NQ),
    list_map_addlist(List, NList),
    NH =.. [adder, NQ, NList],
    list_map_for_adders(T, NT).

list_map_addlist([], []).

list_map_addlist([H|T], [NH|NT]):-
    H =.. [X, Q],
    list_map(Q, NQ),
    NH =.. [X, NQ],
    list_map_addlist(T, NT).


% From MAP to LIST for correspondences

map_list_for_correspondences([], []).

map_list_for_correspondences([H|T], [NH|NT]):-
    H =.. [C, Relations1, Relations2],
    map_list_for_relation_set(Relations1, NR1),
    map_list_for_relation_set(Relations2, NR2),
    NH =.. [C, NR1, NR2],
    map_list_for_correspondences(T, NT).


% From MAP to LIST for adders

map_list_for_adders([], []).

map_list_for_adders([H|T], [NH|NT]):-
    H =.. [adder, Q, List],
    map_list(Q, NQ),
    map_list_addlist(List, NList),
    NH =.. [adder, NQ, NList],
    map_list_for_adders(T, NT).

map_list_addlist([], []).

map_list_addlist([H|T], [NH|NT]):-
    H =.. [X, Q],
    map_list(Q, NQ),
    NH =.. [X, NQ],
    map_list_addlist(T, NT).

%*-------- End listmap maplist tools ---------*%


% End listversion analyse zero preds...................*/
