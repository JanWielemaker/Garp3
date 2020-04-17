/*  File:    solve
    Purpose: Inequality reasoning
    Author:  Martin Reinders & Bert Bredeweg & Floris Linnebank
    Date:    August 1989
    Part-of:  GARP (version 2.0)
    Modified: 30 July 2004

    Copyright (c) 2004, University of Amsterdam. All rights reserved.

*/


%         FL nov 07: inequality reasoning redo:
%
%         - canonical form also for = relations, this means there is only one possible way to write
%         a relation internally -> faster checking possible
%
%         - division of base in relevant contexts with derivable relations attached:
%         base:
%	  [context([Relations...], [Derivable], ContextPointer), context(_, _), ..., ...]
%	  this way solve can work on interdependent sets of relations not combining relations without
%	  a chance of success
%
%         derivable works like before, for old calls:
%         - [AllDerivable..., ...]
%
%         - unified analyse simple and analyse zero relations
%         less code, less work.
%
%         Idea: immediate removal of weaker relations in solvecore: (not done yet)
%         therefore always only single relation about 2 variables present.
%         would allow for faster retrieval (memberchk vs member) of relations about same variables
%         could be expensive though and it does not save the remove_weaker work done later because
%         solve_core does not return the new derivable set (but it could in theory)
%
%         IDEA: caching of contexts: simply retrieve derivable, given a set of relations.
%         Does not work (yet), it would need a translation into a generic form to have enough matches.
%         This translation is probably too expensive especially since many models
%         have quite low core inequality reasoning costs (<10%)
%


%%%%%%%%%%%%% append_relation: %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Add a relation to the mathematical model:
% fail if contradictory,
% succeed with empty assumptions if it is a derivable or known relation,
% succeed with assumptions if there are new relations (unknown / underivable)
%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% other calls do not yet use the extra 2 arguments with AddersInOut so this extra clause
% is added to catch old style calls.
% CAN BE REMOVED IF OLD CALLS ARE UPDATED... (ABOUT 10 CHANGES)
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

% this extra call allows append_relation_all to hook in at exactly the
% right point
append_relation(R, Cin, Cout, Ap, As, Addersin, Addersout):-
	append_relation1(R, Cin, Cout, Ap, As, Addersin, Addersout, [], [], true).


% FL december 2004: Solve does simplification of results:
% x + y = x + z  ->  y = z,
% but not simplification of the input... This is done here, before
% the actual appendrelation2 call.
% Last two arguments are Adders passed on for bit change updates.
% (see: analyse zero equality)
append_relation1(R, Cin, Cout, Ap, As, Addersin, Addersout, MoreRel, NewMore, CheckMultFlag):-
	simplify_relations([R], [R1]),
	determine_pointers(R1, RP),
	cio_r(Cin, Relations),
	relevant_contexts(RP, Relations, Contexts, OtherContexts),
	!,
	append_relation2(R1, RP, Contexts, OtherContexts, Cin, Cout, Ap, As, Addersin, Addersout, MoreRel, NewMore, CheckMultFlag).


% append_relation2(+Relation, +InvolvedPointers, +Cin, -Cout, -AppendedRels, +AnalyseSimpleTrueFail, +AddersIn, -AddersOut)
%
% 4 options:
% - relation is new context but evident -> return true
% - relation is new context -> return new context
% - relation is already derivable -> return true
% - relation fits in single context -> solve single context
% - relation connects multiple contexts -> solve combined context
%
% fails if inconsistent (solve_core will do this)

% empty relevant contexts...>> but relation is evident -> return true
append_relation2(AppendOne, _RP, [], _, Cio, Cio, [], _, Adders, Adders, MR, MR, _CheckMultFlag):-
	check_context_derivable(AppendOne, []),
	!.

% empty relevant contexts...>> relation is new context -> return new context (actually solve new empty context)
append_relation2(AppendOne, RP, [], OtherContexts, Cin, Cout, [AppendOne], DoAS, Adders, NAdders, MR, NMR, CheckMultFlag):-
	!,
	append_relation_core(AppendOne, RP, context([], [], RP), OtherContexts, Cin, Cout, [AppendOne], DoAS, Adders, NAdders, MR, NMR, CheckMultFlag).

% - relation is already derivable -> return true
append_relation2(AppendOne, _RP, Relevant, _, Cio, Cio, [], _, Adders, Adders, MR, MR, _CheckMultFlag):-
	check_context_derivable(AppendOne, Relevant),
	!.

% - relation fits or extends single context -> solve single context
append_relation2(AppendOne, RP, [context(R, D, CP)], OtherContexts, Cin, Cout, [AppendOne], DoAS, Adders, NAdders, MR, NMR, CheckMultFlag):-
	!,
	map_union(RP, CP, NewCP), % by definition not unique
	append_relation_core(AppendOne, RP, context(R, D, NewCP), OtherContexts, Cin, Cout, [AppendOne], DoAS, Adders, NAdders, MR, NMR, CheckMultFlag).

% - relation connects multiple contexts -> solve combined context
append_relation2(AppendOne, RP, Contexts, OtherContexts, Cin, Cout, [AppendOne], DoAS, Adders, NAdders, MR, NMR, CheckMultFlag):-
	combine_contexts(Contexts, CombiContext),
	!,
	append_relation_core(AppendOne, RP, CombiContext, OtherContexts, Cin, Cout, [AppendOne], DoAS, Adders, NAdders, MR, NMR, CheckMultFlag).



%  solve single context
append_relation_core(AppendOne, _RP, context(Relations, Derivable, CP), OtherContexts, Cin, Cout, [AppendOne], DoAS, Adders, NAdders, MR, NMR, CheckMultFlag):-
	!,
	cio_r_nr_d_nd_q_nq_c_nc(Cin, Cnew,
				_, NewContexts,
				AllDerivable, NewAllDerivable,
				Q, NQ,
				C, NC),
	append([AppendOne], Derivable, Known),	% all relations we don't want to derive again
	solve_core([[]/AppendOne], Relations, Relations, Known, FreshDerived, Q), % fails is inconsistent
	% reconstruct: relations and derivable, construct allnew:
	apply_cannonical_form(FreshDerived, NewDerived),
	append(Known, NewDerived, NewDerivable1),
	append([AppendOne], NewDerived, AllNew1),
	append([AppendOne], Relations, NewRelations1),
	% remove a>=b if a>b is found:
	remove_weaker2(AllNew1, AllNew1, AllNew2),
	remove_weaker(AllNew2, NewRelations1, NewRelations2),
	remove_weaker(AllNew2, NewDerivable1, NewDerivable2),
	% reconstruct allderivable by adding allnew:
	append(AllNew2, AllDerivable, NewAllDerivable1),
	remove_weaker(AllNew2, NewAllDerivable1, NewAllDerivable2),
	% replace a with b if a=b
	flag(analyse_simple_found_contradiction, _, false),
	analyse_binairy_equality(DoAS, CP, _NCPold, AllNew2, Q, NQ,
				 AllNew2, AllNew3,
				 NewRelations2, NewRelations3,
				 NewDerivable2, NewDerivable3,
				 NewAllDerivable2, NewAllDerivable3,
				 MR, NMR,
				 C, NC, Adders, Adders1),
	sort(AllNew3, AllNew4),
	sort(NewRelations3, NewRelations),
	sort(NewDerivable3, NewDerivable),
	sort(NewAllDerivable3, NewAllDerivable),
	(
	  NewRelations \= []
	->
	  determine_pointers_set(NewRelations, NCP),
	  % FL feb 2010, analyse bin eq previously updated pointer context
	  % sometimes making pointers not included if relation was not binary, now just recalculated.
	  NewContexts = [context(NewRelations, NewDerivable, NCP)|OtherContexts]
	;
	  NewContexts = OtherContexts % analyse binary has removed complete context...
	),
	(
	  C \= NC  % if Correspondences have changed, they should be checked with the 0 = 0 relation, to fire any correspondences with the 0 = 0 relation now as a condition after the pointer updates.
	->
	  list_map([0], Z),
	  AllNew = [relation(Z, =, Z)|AllNew4]
	;
	  AllNew4 = AllNew
	),
	!,
	inspect_correspondence(AllNew, Cnew, Cnew1, DoAS, Adders1, Adders2),
	inspect_multiplications(CheckMultFlag, Cnew1, Cout, DoAS, Adders2, NAdders).%check_multiplications



%%%%%%%%%%%%%%% append_relation_all: %%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	implements append_relation for a set of relations
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% other calls do not yet use these arguments so this extra clause
% is added to catch old style calls.
% CAN BE REMOVED IF OLD CALLS ARE UPDATED... (ABOUT 10 CHANGES)
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

append_relation_all([H|T], Cin, Cout, AppendNotDerivable, DoAnalyseSimple, Adin, Adout):-
	append_relation_all2([H|T], Cin, Cout, [], AppendNotDerivable, DoAnalyseSimple, Adin, Adout).

% use predicates of single append relation to do all
%
append_relation_all2([], Cio, Cio, AppNotDer, AppNotDer, _DoAnalyseSimple, Add, Add).

append_relation_all2([H|T], Cin, Cout, AppendNotDerivableIn, AppendNotDerivableOut, DoAnalyseSimple, Adin, Adout):-
	append_relation1(H, Cin, Cio, AppendNotDerivable, DoAnalyseSimple, Adin, Add, T, NT, fail), %pass on Tail for pointer updates
	 % fail indicates this call should not do multiplications checking. this is because append all is used by mult,
	 % causes unnecessary rechecking of mult context during addition of its results
	!,
	append(AppendNotDerivableIn, AppendNotDerivable, NewAppendNotDerivable),
	append_relation_all2(NT, Cio, Cout, NewAppendNotDerivable, AppendNotDerivableOut, DoAnalyseSimple, Add, Adout).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	helper preds append relation
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% determine bitvector for a set of relations
%
determine_pointers_set(RelationList, BitVector):-
	determine_pointers_set1(RelationList, BVList),
	merge_bitvectors(BVList, bitvector(0), BitVector).

determine_pointers_set1([], []).

determine_pointers_set1([Rel|T], [BV|NT]):-
	determine_pointers(Rel, BV),
	determine_pointers_set1(T, NT).

merge_bitvectors([], BV, BV).
merge_bitvectors([BV|T], In, Out):-
	map_union(BV, In, New),
	merge_bitvectors(T, New, Out).



% determine bitvector for a relation
%
determine_pointers(relation(R, _, L), BitVector):-
	map_union(R, L, All), % combine
	NotZero is \1,	      % remove zero pointer
	map_intersection(All, bitvector(NotZero), BitVector).


% check_context_derivable(Relation, SetOfContexts
% succeeds when first context with relation or implied relation is found
% NB fails on empty list!!!

% evident relation
check_context_derivable(Rel, _):-
	evident(Rel),
	!.

% success
check_context_derivable(Rel, [context(_R, D, _CP)|_]):-
	is_context_derivable(Rel, D),
	!.

%try next context
check_context_derivable(Rel, [_|T]):-
	check_context_derivable(Rel, T).
	%NB fails on empty list!!!


% is_context_derivable(Relation, Derivable)
% NB evidence checking allready done!!!!
% inverse for equality relation not necessary anymore: canonical form used

is_context_derivable(relation(Left, Relation, Right), Derivable):-
	memberchk(relation(Left, Relation1, Right), Derivable), % only one relation can be present for each Left/Right pair
	(
	  Relation = Relation1
	;
	  implies_relation(Relation1, Relation)
	),
	!.

% extra clause for the case that:
% = relation implies  >= relation, and left and right are switched.
is_context_derivable(relation(Left, >=, Right), Derivable):-
	sort([Left, Right], [NLeft, NRight]), % cannonical form for = relations
	Left = NRight, % they were indeed switched! If not then previous clause should have succeeded allready.
	Right = NLeft, % (saves unnecessary memberchecks)
	memberchk(relation(NLeft, =, NRight), Derivable),
	!.

%  is_derivable/2
%
%  1 + Relation, intern representation (<, =< reversed to >, >=)
%  2 + Relations, known to be derivable (idem)
%
%  is 1 a member of 2? (or directly implied by 2?, e.g. a > b -> a >= b

is_derivable(Relation, _):- evident(Relation), !.
is_derivable(Relation, Derivable):-
	is_context_derivable(Relation, Derivable).


% during solve_core the set of derivable may become weak: more then one
% relation known per variable pair.
% therefore an extensive is_derivable check is needed.
is_derivable_weakset(Relation, _):- evident(Relation), !.

is_derivable_weakset(relation(Left, Relation, Right), Derivable):-
	memberchk(relation(Left, Relation, Right), Derivable), % direct hit
	!.

%special clauses for >=
is_derivable_weakset(relation(Left, >=, Right), Derivable):-
	memberchk(relation(Left, >, Right), Derivable),
	!.

is_derivable_weakset(relation(Left, >=, Right), Derivable):-
	sort([Left, Right], [NLeft, NRight]), % cannonical form for = relations
	memberchk(relation(NLeft, =, NRight), Derivable),
	!.



% check_derivable:
% is a list of relations (intern representation) completely derivable?

check_derivable([], _).
check_derivable([H|T], D):-
	is_derivable(H, D),
	!,
	check_derivable(T, D).


% tautologies
evident(relation(X, =, X)).
evident(relation(X, >=, X)).


%  implies_relation(+op1, -op2)
%  a op1 b  ->  a op2 b
implies_relation(>, >=).
implies_relation(=, >=).



% relevant_contexts(+RelPointers, +RelationContexts, -RelevantContexts)
% returns all contexts with a pointer overlap
relevant_contexts(_RP, [], [], []).

% relevant_context
relevant_contexts(RP, [context(R, D, CP)|RT], [context(R, D, CP)|CT], OT):-
	map_intersection(RP, CP, bitvector(Common)),
	Common \== 0, % FL nov 07 used to be =\= but arithmetic is not necessary here (and slower!)
	!,
	relevant_contexts(RP, RT, CT, OT).

% other context
relevant_contexts(RP, [H|RT], CT, [H|OT]):-
	relevant_contexts(RP, RT, CT, OT).

% combine multiple contexts into one.
% (should always be list of 2 or more)
%

combine_contexts([context(Relations, Derivable, CP)|Tail], New):-
	combine_contexts2(Tail, Relations, Derivable, CP, New).

combine_contexts2([], Relations, Derivable, CP, context(Relations, Derivable, CP)).
combine_contexts2([context(Relations1, Derivable1, CP1)|T], Relations2, Derivable2, CP2, New):-
	append(Relations1, Relations2, Relations3),
	append(Derivable1, Derivable2, Derivable3),
	(map_union_unique(CP1, CP2, CP3); etrace(error_combine_connected_context, nil, nil)),
	% unique is extra check... CP's  should never have an intersection allready
	!,
	combine_contexts2(T, Relations3, Derivable3, CP3, New).



% remove_weaker(+Newrelations, +KnownRelations, -KnownStronger)
% Arg1 is a list of new (derived) relations
% Arg2 is a list of known (derived) relations
% remove from Arg2 each relation a >= b if Arg1 contains a > b/ a = b

remove_weaker([], Rs, Rs).

remove_weaker([relation(L, >, R)| Tail], Rs, NRs):-
	memberchk(relation(L, >=, R), Rs),
	% memberchk is logically superfluous,
	% but, while written in C, the test is faster
	common_select(Rs, relation(L, >=, R), TRs), !,
	%remove_weaker([relation(L, >, R)|Tail], TRs, NRs). %why check again for this relation!? only one weaker should be present
	remove_weaker(Tail, TRs, NRs).

remove_weaker([relation(L, =, R)|Tail], Rs, NRs):-
	memberchk(relation(L, >=, R), Rs),
	common_select(Rs, relation(L, >=, R), TRs), !,
	%remove_weaker([relation(L, =, R)|Tail], TRs, NRs). %why check again for this relation!? only one weaker should be present
	remove_weaker(Tail, TRs, NRs).

remove_weaker([ relation(R, =, L) | Tail], Rs, NRs):-
	memberchk(relation(L, >=, R), Rs),
	common_select(Rs, relation(L, >=, R), TRs), !,
	%remove_weaker([relation(L, =, R)|Tail], TRs, NRs). %why check again for this relation!? only one weaker should be present
	remove_weaker(Tail, TRs, NRs).

remove_weaker([_|Tail], Rs, NRs):-
	remove_weaker(Tail, Rs, NRs).


% a secondversion, aims to do this quicker for 2 identical lists...
% the idea is: take weak relation, search for strong.
%
% remove from Arg2 each relation a >= b if Arg2 contains a > b/ a = b

remove_weaker2(List, Strong, Strongest):-
	memberchk(relation(L, >=, R), List), % weak relation present.
	(
	  memberchk(relation(L, >, R), Strong)
        ;
	  canonical_form(relation(L, =, R), Rel),
	  memberchk(Rel, Strong)
	), %and strong present
	!,
	member_and_tail(relation(L, >=, R), List, Rest), %select rest from the inputlist
	select(relation(L, >=, R), Strong, Stronger), % remove weak relation from stronglist
	%continue with rest,
	remove_weaker2(Rest, Stronger, Strongest).

% done
remove_weaker2(_, Strongest, Strongest).



% simplification should not be necessary and makes trace less insightfull
illegal_relation(relation(Left, Relation, Right)):-
	invalid_relation(relation(Left, Relation, Right)),
	!.

invalid_relation(relation(M, >, M)).


% simplify_relation(+Left, +Right, -NLeft, -NRight)
% If two sides of an (in)equality relation in intern representation contain the same quantities,
% then these can be taken out:
% a + b = a + c -> b = c
% Sometimes leaving zero on one side:
% a + b = b -> a = 0

% simplify list of relations
simplify_relations([], []).

simplify_relations([relation(Left, Rel, Right)|Tail], [relation(NLeft, Rel, NRight)|NewTail]):-
    simplify_relation(Left, Right, Left1, Right1),
    canonical_form(Left1, Right1, Rel, NLeft, NRight),
    !,
    simplify_relations(Tail, NewTail).

% simplify one relation
simplify_relation(Left, Right, NLeft, NRight):-
    map_without_intersection(Left, Right, NLeft1, NRight1),
    %new FL may 07: intercept [] pointer for zero, replace with: [0]
    zero_pointer(relation(NLeft1, dummy, NRight1), relation(NLeft, dummy, NRight)),
    !.

simplify_relation(Left, Right, Left, Right).



%%%%%%%%%%%%%%%%%%%%%%Inequality Reasoning / Solve : Core %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% The core solve process works by combining new relations with:
%  1) derivable relations with the same variables
%  2) base relations
%  3) new derived relations
% This is still much less then simply combining with all derivable
% relations.
%
% Since new relations are added to the derivable set there may be more
% then one relation about each pair of variables. e.g.:
% A>B is known/derivable, A=B is added.
% --> both are in the derivable set,
% later they will be combined (to form the contradiction in this case)
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



solve_core(NR, TO, K, D, New, Q):-
	@tracer->>trace_on(inequality),
	!,
	flag(max_depth, MaxDepth, MaxDepth),
	flag(solve_extra_combinations, SEC, SEC),
	solve_trace(NR, TO, K, D, New1, Q, MaxDepth, SEC),
	add_empty_parents(New1, New1P),	% Some relations can only be derived by combining derivable relations... (3)
	append(New1, D, ND),
	solve_trace(New1P, New1, New1, ND, New2, Q, MaxDepth, SEC), % so these are found here.
	append(New1, New2, New).

solve_core(NR, TO, K, D, New, _):-
	flag(max_depth, MaxDepth, MaxDepth),
	flag(solve_extra_combinations, SEC, SEC),
	solve_normal(NR, TO, K, D, New1, MaxDepth, SEC),
	add_empty_parents(New1, New1P),% Some relations can only be derived by combining derivable relations...
	append(New1, D, ND),
	solve_normal(New1P, New1, New1, ND, New2, MaxDepth, SEC), % so these are found here.
	append(New1, New2, New).


% done, return result
solve_normal([], _, _, _, [], _, _):- !.

% combine relation with derivable relation on same variables (1)
%
% only done if switch is on, because it can take up to 50% extra time...
%
solve_normal([Parents/Relation1|TailNew], KnownRelations, Known, Derivable, New, MaxDepth, true):-
	limit_parents(Parents, MaxDepth),
	get_derivable_same_var(Relation1, Derivable, Relation2),
	\+ Relation2 = Relation1,
	add_relation(Relation1, Relation2, Relation3a),  % transitivity
	zero_pointer(Relation3a, Relation3),             % FL NEW: is x = [] is derived; replace: [] with [0]
	\+ Relation3 = Relation1,			 % not same as input
	\+ is_derivable_weakset(Relation3, Derivable),	         % not derivable
	\+ evident(Relation3),				 % not evident
	!,
	\+ invalid_relation(Relation3),			 % fail if contradiction
	return_simple_only(Relation3, NNew, New),
	!,
	solve_normal([Parents/Relation1, [Relation1, Relation2|Parents]/ Relation3
		| TailNew], KnownRelations, Known, [Relation3|Derivable], NNew, MaxDepth, true).

% combine with base relation (2)
% add relation, succeeds if result is consistent,
% has no term twice and can be simplified
% if inconsistent: fail solve
% if no simplification, or already derivable, or evident: next clause
solve_normal([Parents/Relation1|TailNew], KnownRelations, Known, Derivable, New, MaxDepth, SEC):-
	limit_parents(Parents, MaxDepth),
	member_and_tail(Relation2, KnownRelations, TailKnown),
	add_relation(Relation1, Relation2, Relation3a),  % transitivity
	zero_pointer(Relation3a, Relation3),             % FL NEW: is x = [] is derived; replace: [] with [0]
	\+ memberchk(Relation2, Parents),		 % not circular
	\+ is_derivable_weakset(Relation3, Derivable),	 % not derivable
	\+ evident(Relation3),				 % not evident
	!,
	\+ invalid_relation(Relation3),			 % fail if contradiction
	return_simple_only(Relation3, NNew, New),
	!,
	solve_normal([Parents/Relation1, [Relation1, Relation2|Parents]/ Relation3
		| TailNew], TailKnown, Known, [Relation3|Derivable], NNew, MaxDepth, SEC).

% tried all for first relation, try tail
solve_normal([_|TailNew], _, Known, Derivable, New, MaxDepth, SEC):-
	!,
	solve_normal(TailNew, Known, Known, Derivable, New, MaxDepth, SEC).



% ------------------- same when tracing inequality reasoning ----------

% done, return result
solve_trace([], _, _, _, [], _, _MaxDepth, _):- !.

% combine relation with derivable relation on same variables
%
% only done if switch is on, because it can take up to 50% extra time...
%
solve_trace([Parents/Relation1|TailNew], KnownRelations, Known, Derivable, New, Q, MaxDepth, true):-
	flag(equal_intervals, Flag, Flag), % seems to eat resources combined with this option, fix needed in future
	Flag = fail,
	limit_parents(Parents, MaxDepth),
	get_derivable_same_var(Relation1, Derivable, Relation2),
	\+ Relation2 = Relation1,
	add_relation(Relation1, Relation2, Relation3a),  % transitivity
	zero_pointer(Relation3a, Relation3),             % FL NEW: is x = [] is derived; replace: [] with [0]
	\+ memberchk(Relation2, Parents),		 % not circular
	\+ is_derivable_weakset(Relation3, Derivable),	 % not derivable
	\+ evident(Relation3),				 % not evident
	!,
	(invalid_relation(Relation3) ->			 % fail if contradiction
		etrace(solve_derived_invalid, _, inequality),
	        @tracer->>tell(inequality),
	        show_antecedents(Parents, Relation1, Relation2, Relation3, 'contradiction.', Q),
	        @tracer->>told,
		fail
		;
		true
	),
	return_simple_only(Relation3, NNew, New),
	!,
	(simple_relation(Relation3) ->
	  etrace(solve_found_valid, _, inequality),
	  @tracer->>tell(inequality),
	  show_antecedents(Parents, Relation1, Relation2, Relation3, 'derived.', Q),
	  @tracer->>told
	  ;
	  true
	),
	append(Parents, [Relation1, Relation2], NewParents),
	solve_trace([Parents/Relation1, NewParents/ Relation3 | TailNew],
			 KnownRelations, Known, [Relation3|Derivable], NNew, Q, MaxDepth, true).

% add relation, succeeds if result is consistent,
% has no term twice and can be simplified
% if inconsistent: fail solve
% if no simplification, or already derivable, or evident: next clause
solve_trace([Parents/Relation1|TailNew], KnownRelations, Known, Derivable, New, Q, MaxDepth, SEC):-
	limit_parents(Parents, MaxDepth),
	member_and_tail(Relation2, KnownRelations, TailKnown),
	add_relation(Relation1, Relation2, Relation3a),  % transitivity
	zero_pointer(Relation3a, Relation3),             % FL NEW: if x = [] is derived; replace: [] with [0]
	\+ memberchk(Relation2, Parents),		 % not circular
	\+ is_derivable_weakset(Relation3, Derivable),	 % not derivable
	\+ evident(Relation3),				 % not evident
	!,
	(invalid_relation(Relation3) ->			 % fail if contradiction
		etrace(solve_derived_invalid, _, inequality),
	        @tracer->>tell(inequality),
	        show_antecedents(Parents, Relation1, Relation2, Relation3, 'contradiction.', Q),
	        @tracer->>told,
		fail
		;
		true
	),
	return_simple_only(Relation3, NNew, New),
	!,
	(simple_relation(Relation3) ->
	  etrace(solve_found_valid, _, inequality),
	  @tracer->>tell(inequality),
	  show_antecedents(Parents, Relation1, Relation2, Relation3, 'derived.', Q),
	  @tracer->>told
	  ;
	  true
	),
	append(Parents, [Relation1, Relation2], NewParents),
	solve_trace([Parents/Relation1, NewParents/ Relation3 | TailNew],
			 TailKnown, Known, [Relation3|Derivable], NNew, Q, MaxDepth, SEC).

% tried all for first relation, try tail
solve_trace([_|TailNew], _, Known, Derivable, New, Q, MaxDepth, SEC):-
	!,
	solve_trace(TailNew, Known, Known, Derivable, New, Q, MaxDepth, SEC).


% a lot of speed can be gained in some cases
% if the number of parents is constrained.  (FL June 07)
%
% if many calculus relations exist in the model,
% then many new but uninformative calculus relations can be derived.
%
% after X steps the full partial ordering is probably allready traversed
% so the rest of the derivations is only about uninformative new combinations
% of quantities.

% uninitialised maxdepth flag -> 0
limit_parents(_Parents, 0):-
	!.
% fails if number of parents is over limit
limit_parents(Parents, MaxDepth):-
	length(Parents, Depth),
	Depth < MaxDepth,
	!.


% member_and_tail
% return member and everything behind it
member_and_tail(H, [H|T], T).
member_and_tail(H, [_|T], NT):- member_and_tail(H, T, NT).



% return only parameter > landmark, parameter > parameter, etc.

return_simple_only(Relation, New, [Relation|New]) :-
	simple_relation(Relation),
	!.

return_simple_only(_, New, New).


simple_relation(relation(L, _, R)):-
	map_count(L, C1),
	C1 < 2,
	map_count(R, C2),
	C2 < 2.



add_empty_parents([], []).
add_empty_parents([H|T], [[]/H|NT]):- add_empty_parents(T, NT).


get_derivable_same_var(relation(L, _Relin, R), Derivable, relation(L, Rel, R)):-
	memberchk(relation(L, _Rel, R), Derivable), % at least one %traced, tiny bit faster with this pre check
	member(relation(L, Rel, R), Derivable).

get_derivable_same_var(relation(L, _Relin, R), Derivable, relation(R, Rel, L)):-
	memberchk(relation(R, _Rel, L), Derivable), % at least one
	member(relation(R, Rel, L), Derivable).

/*
 * add_relation(+R1, +R2, -R3)
 * add relation between two sums, represented as bitmaps
 *
 * succeeds only if both left sides or right sides don't have a common term
 * and the result can be simplified
 *
 * This scheme implements transitivity and reasoning about sums
 *
 * some examples:
 *
 * a > b		/\ b > c		-> a > c
 * a + b > c		/\ a = d		-> d + b > c
 *	(this result is not returned, but may lead to another result)
 * d = 0		/\ d + b > c		-> b > c
 *
 * note a lot of relations can be derived in many ways
 *
 * to prevent some superfluous derivations, if a quantity is equal to
 * another quantity, it is replaced with that quantity (if possible)
 * see append_relations(_all) / analyse_simple_equality
 *
 */

% Patch 3-3-2004 by FL: this case wasn't captured, because it cannot be
% derived using transitivity. This technique of combining weaker relations
% to stronger ones is mentioned in simmons '89,
add_relation(relation(X, >=, Y), relation(Y, >=, X), relation(X, =, Y)):-
	!.


% transitivity:
add_relation(relation(Le1, Rel1, Ri1), relation(Le2, Rel2, Ri2), relation(Le3, Rel3, Ri3)):-
	add_compatible(Rel1, Rel2, Rel3),
	map_union_unique(Le1, Le2, L),	  % union left sides (no term twice)
	map_union_unique(Ri1, Ri2, R),
	map_without_intersection(L, R, Le3, Ri3),  % simplify
	!.

% if first clause failed, and one relation is '=', then reverse one relation
add_relation(relation(Le1, Rel1, Ri1), relation(Ri2, =, Le2),
		relation(Le3, Rel3, Ri3)):-
	!,	% do not use clause 3
	add_compatible(Rel1, =, Rel3),
	map_union_unique(Le1, Le2, L),
	map_union_unique(Ri1, Ri2, R),
	map_without_intersection(L, R, Le3, Ri3),
	!.

add_relation(relation(Ri1, =, Le1), relation(Le2, Rel2, Ri2),
		relation(Le3, Rel3, Ri3)):-
	add_compatible(=, Rel2, Rel3),
	map_union_unique(Le1, Le2, L),
	map_union_unique(Ri1, Ri2, R),
	map_without_intersection(L, R, Le3, Ri3),
	!.


% < =< were inversed
add_compatible(=, =, =).
add_compatible(=, >, >).
add_compatible(=, >=, >=).
add_compatible(>, =, >).
add_compatible(>, >=, >).
add_compatible(>, >, >).
add_compatible(>=, =, >=).
add_compatible(>=, >=, >=).
add_compatible(>=, >, >).

inverse(<, >).
inverse(=<, >=).
inverse(=, =).
inverse(>=, =<).
inverse(>, <).


% substitue [] with zeropointer [0]   (FL june 07)
% and remove excess zeropointers from additions
% if x = [] is derived; replace: [] with [0] if x = [ 0, y ] is
% derived; replace: [0, y ] with y
%
zero_pointer(relation(bitvector(Left), Rel, bitvector(Right)), relation(bitvector(NewLeft), Rel, bitvector(NewRight))):-
	process_zero_pointer(Left, NewLeft),
	process_zero_pointer(Right, NewRight).

% [] becomes [0] (NB zeropointer: [0] is the integer/bitvector: 1)
process_zero_pointer(0, 1):-!.
% addition with zero [0, 5, 6] becomes [5, 6]
process_zero_pointer(Q, NQ):-
	Y is Q /\ 1,
	Y == 1, % there is a zero in the quantity (nb this check first: cheap)
	X is popcount(Q),
	X > 1, % there is an addition
	!,
	NQ is Q /\ \1, % subtract the zero (AND operation with NOT zero) (could also be simple: - 1)
	!.
% else no change
process_zero_pointer(Q, Q).




% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%                ANALYSE SOLVE RESULTS: ANALYSE BINAIRY EQUALITY
%
%
% analyse results of solve
% if simple equality (between two parameters or a parameter and a constant,
% (not zero))
% replace quantity pointer of one of these with the other in all relations
%
% don't analyse when resolving influences/proportional relations
%
% Two extra arguments: adders in /out these also contain pointers and should also be updated.
% This one list should be left structurally intact during the changes:
% it actually is two lists. (not to need 4 extra arguments)
%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

analyse_binairy_equality(false, CP, CP, _, Q, Q, N, N, R, R, D, D, AllD, AllD, MR, MR, C, C, A, A).

analyse_binairy_equality(true, CP, CP, [], Q, Q, N, N, R, R, D, D, AllD, AllD, MR, MR, C, C, A, A).


analyse_binairy_equality(true, CP, NCP, [relation(Right, =, Left)|Tail], Q, NQ, N, NN, R, NR, D, ND, AllD, NAllD, MR, NMR, C, NC, A, NA):-
	map_list(Left, [ ILeft ]),
	map_list(Right, [ IRight ]),
	list_map([ILeft, IRight], M),
	% if 0 = x is derived, it is always the zero pointer that is inserted (NB cannonical form!)
	% NB Right and Left swapped to keep lowest pointer (and 0)
	% and in additions the zero pointer is left out. eg. x = 0, x + y = z -> y = z instead of y + 0 = z
	% like in process zero pointer
	replace_index_relations(M, ILeft, IRight, N, N1),
	replace_index_relations(M, ILeft, IRight, R, R1),
	replace_index_relations(M, ILeft, IRight, D, D1),
	replace_index_relations(M, ILeft, IRight, AllD, AllD1),
	replace_index_relations(M, ILeft, IRight, Tail, Tail1),
	replace_index_relations(M, ILeft, IRight, MR, MR1),
	replace_index_relations_for_correspondence(M, ILeft, IRight, C, C1),
	% fails if that would produce a 'double quantity'
	% sets flag if contradiction found
	replace_index_in_adders(ILeft, IRight, A, A1), % Note the adder pointers are updated
	!,
	replace_index(ILeft, IRight, Q, Q1),
	replace_context_pointer(Left, Right, CP, CP1),
	analyse_binairy_equality(true, CP1, NCP, Tail1, Q1, NQ, N1, NN, R1, NR, D1, ND, AllD1, NAllD, MR1, NMR, C1, NC, A1, NA).

analyse_binairy_equality(true, CP, NCP, [_|Tail], Q, NQ, N, NN, R, NR, D, ND, AllD, NAllD, MR, NMR, C, NC, A, NA) :-
	flag(analyse_simple_found_contradiction, false, false),
    analyse_binairy_equality(true, CP, NCP, Tail, Q, NQ, N, NN, R, NR, D, ND, AllD, NAllD, MR, NMR, C, NC, A, NA).


replace_context_pointer(Remove, _, CP, NCP):-
	map_difference(CP, Remove, NCP).


% replace index relations fails if F & S are in a sum together
% or if an inconsistent relation is found: x > x
replace_index_relations(_, _, _, [], []).

% fail if F & S are already in a sum together

replace_index_relations(M, _F, _S, [ relation(Left, _, Right) | _ ], _) :-
	%list_map([F, S], M),
	(	map_intersection(M, Left, M)
		;
		map_intersection(M, Right, M)
	),
	!,
	fail.

replace_index_relations(M, F, S, [ relation(Left, Rel, Right) | Tail ], Result) :-
	m_replace_two(F, S, Left, Right, NLeft, NRight),
	!,	% fails if F not in Left OR Right
	zero_pointer(relation(NLeft, Rel, NRight), Relation),
	use_if_not_evident(Relation, NewTail, Result),
	replace_index_relations(M, F, S, Tail, NewTail).

replace_index_relations(M, F, S, [ Relation | Tail ], [ Relation | NewTail ]) :-
	replace_index_relations(M, F, S, Tail, NewTail).


% Patch FL July 2004 Solve failed to find an obvious contradiction:
% probably all parents used already in this particular case
% form was:
% 2 = 1, 1 > 2
% analyse simple constructs the contradictive relation: 1 > 1
% this fails here to make append relation fail thus providing an extra
% chance to capture inconsistencies

use_if_not_evident(relation(M, >, M), Result, Result):-
    !,
    flag(analyse_simple_found_contradiction, _, true),
    etrace(solve_simple_found_invalid, _, add_relation),
    !,
    fail.

use_if_not_evident(Rel, Result, Result):-
	evident(Rel), !.

use_if_not_evident(Rel, Result, [Rel|Result]).


% suceed for fist, try second or succeed
m_replace_two(First, Second, Org1, Org2, New1, New2) :-
	map_replace(First, Second, Org1, New1),
	(map_replace(First, Second, Org2, New2) ; Org2 = New2),
	!.

% failed for first, try second or fail
m_replace_two(First, Second, Org1, Org2, Org1, New2) :-
	map_replace(First, Second, Org2, New2).


replace_index_relations_for_correspondence(_, _, _, [], []).
replace_index_relations_for_correspondence(M, I1, I2, [correspondence(L1, L2)|T], NewList):-
	replace_index_relations_cor(M, I1, I2, L1, NL1, Change1),
	replace_index_relations_cor(M, I1, I2, L2, NL2, Change2),
	!,
	(
	  no_full_tautology_or_contradiction_correspondence(Change1, Change2, NL1, NL2, undirected)
	->
	  NewList = [correspondence(NL1, NL2)|NT]
	;
	  NewList = NT %correspondence has become useless
	),
	!,
	replace_index_relations_for_correspondence(M, I1, I2, T, NT).
replace_index_relations_for_correspondence(M, I1, I2, [if_correspondence(L1, L2)|T], NewList):-
	replace_index_relations_cor(M, I1, I2, L1, NL1, Change1),
	replace_index_relations_cor(M, I1, I2, L2, NL2, Change2),
	!,
	(
	  no_full_tautology_or_contradiction_correspondence(Change1, Change2, NL1, NL2, directed)
	->
	  NewList = [if_correspondence(NL1, NL2)|NT]
	;
	  NewList = NT %correspondence has become useless
	),
	!,
	replace_index_relations_for_correspondence(M, I1, I2, T, NT).



% FL July 2004
% correspondences can have contradictory relations which just indicates there not
% active in this state, therefore no use_if_not_evident is used here.

replace_index_relations_cor(_, _, _, [], [], false).

% fail if F & S are already in a sum together

replace_index_relations_cor(M, _F, _S, [ relation(Left, _, Right) | _ ], _, false) :-
	%list_map([F, S], M),
	(	map_intersection(M, Left, M)
		;
		map_intersection(M, Right, M)
	),
	!,
	fail.

replace_index_relations_cor(M, F, S, [ relation(Left, Rel, Right) | Tail ], [relation(NLeft, Rel, NRight)|NewTail], true) :-
	m_replace_two(F, S, Left, Right, NLeft, NRight),
	!,	% fails if F not in Left OR Right
	replace_index_relations_cor(M, F, S, Tail, NewTail, _).

replace_index_relations_cor(M, F, S, [ Relation | Tail ], [ Relation | NewTail ], Change) :-
	replace_index_relations_cor(M, F, S, Tail, NewTail, Change).



% no changes: still relevant correspondence.
no_full_tautology_or_contradiction_correspondence(false, false, _, _, _):-
	!.

% directed, contradictory conditions: irrelevant
no_full_tautology_or_contradiction_correspondence(_, _, L1, _, directed):-
	contradiction_in_list(L1),
	!,
	fail.

% directed, tautologous consequenses: irrelevant
no_full_tautology_or_contradiction_correspondence(_, _, _, L2, directed):-
	full_tautology_list(L2),
	!,
	fail.

% undirected, only tautologys: irrelevant
no_full_tautology_or_contradiction_correspondence(_, _, L1, L2, undirected):-
	full_tautology_list(L1),
	full_tautology_list(L2),
	!,
	fail.

% undirected, contradiction in both lists: irrelevant
no_full_tautology_or_contradiction_correspondence(_, _, L1, L2, undirected):-
	contradiction_in_list(L1),
	contradiction_in_list(L2),
	!,
	fail.

% still relevant correspondence.
no_full_tautology_or_contradiction_correspondence(_, _, _, _, _).

contradiction_in_list([]):-!, fail.
contradiction_in_list([H|_]):-
	invalid_relation(H),!.
contradiction_in_list([_|T]):-
	contradiction_in_list(T).

full_tautology_list([]).
full_tautology_list([H|T]):-
	evident(H),
	full_tautology_list(T).


% FL december-2003: GARP 2.0 new: replace index for adder lists (influence resolution)
% Note that the structure of the list (length, positioning of items is
% left intact

replace_index_in_adders(_, _, [], []).

replace_index_in_adders(ILeft, IRight, [Adder|Tail], [NAdder|NTail]):-
    replace_index_single_adder(ILeft, IRight, Adder, NAdder),
    replace_index_in_adders(ILeft, IRight, Tail, NTail).


replace_index_single_adder(ILeft, IRight, adder(Q, Addlist), adder(NQ, NAddlist)):-
    map_replace(ILeft, IRight, Q, NQ),
    !,
    replace_index_addlist(ILeft, IRight, Addlist, NAddlist).

% map replace failed for Q
replace_index_single_adder(ILeft, IRight, adder(Q, Addlist), adder(Q, NAddlist)):-
    replace_index_addlist(ILeft, IRight, Addlist, NAddlist).


replace_index_addlist(_, _, [], []).

replace_index_addlist(ILeft, IRight, [H|T], [NH|NT]):-
    H =.. [Type, Q],
    map_replace(ILeft, IRight, Q, NQ),
    !,
    NH =.. [Type, NQ],
    replace_index_addlist(ILeft, IRight, T, NT).

% map replace failed for Q
replace_index_addlist(ILeft, IRight, [H|T], [H|NT]):-
    replace_index_addlist(ILeft, IRight, T, NT).



% replace_index/4
% update quantity list:
% replace all occurences of I1 by I2
replace_index(_, _, [], []).

replace_index(I1, I2, [ H/I1 | Tail ], [ H/I2 | NewTail ]) :-
	!,
	replace_index(I1, I2, Tail, NewTail).

replace_index(I1, I2, [ H | Tail ], [ H | NewTail ]) :-
	replace_index(I1, I2, Tail, NewTail).




%%%%%%%%%%%%%%%%%%  Correspondence Inspection     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	inspect_correspondence:
%	checks all correspondences if they fire because of a given set of new relations
%
%	inspect_a_correspondence:
%	checks if a new correpondence fires given the set of derivable relations
%
%	inspect_some_correspondences:
%	implements inspect_a_correspondence for a set of new correspondences
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* To remove
 FL may 07, passing on adders now for pointer updates...
inspect_correspondence(Relations, Cin, Cout, DoAnalyseSimple):-
	inspect_correspondence2(Relations, Cin, Cout, [], DoAnalyseSimple, [], []).
*/

inspect_correspondence(Relations, Cin, Cout, DoAnalyseSimple, AddersIn, AddersOut):-
	inspect_correspondence2(Relations, Cin, Cout, [], DoAnalyseSimple, AddersIn, AddersOut).

inspect_correspondence2([], Cio, Cio, [], _DoAnalyseSimple, Adders, Adders):-
	!.
inspect_correspondence2(Relations, Cin, Cout, Previous, DoAnalyseSimple, AddersIn, AddersOut):-
	cio_c_nc_d(Cin, Cnew, Correspondence, RCorrespondence, Derivable),
	(
	  select(correspondence(L1, L2), Correspondence, RCorrespondence)
	;
	  select(correspondence(L2, L1), Correspondence, RCorrespondence)
	;
	  select(if_correspondence(L1, L2), Correspondence, RCorrespondence)
	),
	(
	  L1 = [] % An empty list means no conditions: fire correspondence.
	;
	  select(One, L1, RL1),
	  is_derivable(One, Relations),
	  check_derivable(RL1, Derivable)
	),
	append(Previous, L2, ToAdd),
	!,
	inspect_correspondence2(Relations, Cnew, Cout, ToAdd, DoAnalyseSimple, AddersIn, AddersOut).

inspect_correspondence2(_, Cio, Cio, [], _, Adders, Adders):- !.
inspect_correspondence2(_, Cin, Cout, ToAdd, DoAnalyseSimple, AddersIn, AddersOut):-
	append_relation_all(ToAdd, Cin, Cout, _, DoAnalyseSimple, AddersIn, AddersOut).



inspect_a_correspondence(correspondence(L1, L2), Cin, Cout):-
	cio_d(Cin, D),
	%strictly_relevant_contexts(BV, R, Contexts, _),
	%relevant_contexts(BV, R, Contexts, _),
	(
	  (
	    check_derivable(L1, D),
	    !,
	    append_relation_all(L2, Cin, Cout, _, true)
	  )
	;
	  (
	    check_derivable(L2, D),
	    !,
	    append_relation_all(L1, Cin, Cout, _, true)
	  )
	).


inspect_a_correspondence(if_correspondence(L1, L2), Cin, Cout):-
	cio_d(Cin, D),
	%strictly_relevant_contexts(BV, R, Contexts, _),
	%relevant_contexts(BV, R, Contexts, _),
	check_derivable(L1, D),
	!,
	append_relation_all(L2, Cin, Cout, _, true).

inspect_a_correspondence(_, Cio, Cio).



inspect_some_correspondences([], Cio, Cio).
inspect_some_correspondences([H|T], Cin, Cout):-
	inspect_a_correspondence(H, Cin, Cnew),
	correct_changed_pointers(Cin, Cnew, T, NT), % pointers can change, leading to invalid correspondences in tail
	inspect_some_correspondences(NT, Cnew, Cout).


%no pointer changes
correct_changed_pointers(Cio, Cio, Corr, Corr):-!.

%no pointer changes
correct_changed_pointers(Cold, Cnew, Corr, Corr):-
	cio_q(Cold, Q),
	cio_q(Cnew, Q),
	!.

%pointers changed! update correspondences
correct_changed_pointers(Cold, Cnew, CorrIn, CorrOut):-
	cio_q(Cold, Q),
	cio_q(Cnew, NQ),
	determine_changed_pointers(Q, NQ, Changes),
	switch_changed_pointers(Changes, CorrIn, CorrOut).

%done
determine_changed_pointers([], [], []).

% unchanged pointer
determine_changed_pointers([Q/P|T], [Q/P|NT], Changes):-
	!,
	determine_changed_pointers(T, NT, Changes).

% unchanged pointer, strange position
determine_changed_pointers([Q/P|T], NQ, Changes):-
	member(Q/P, NQ),
	!,
	determine_changed_pointers(T, NQ, Changes).

% changed pointer
determine_changed_pointers([Q/P|T], [Q/NP|NT], [changed(P, NP)|Changes]):-
	!,
	determine_changed_pointers(T, NT, Changes).

% changed pointer, strange position
determine_changed_pointers([Q/P|T], NQ, [changed(P, NP)|Changes]):-
	member(Q/NP, NQ),
	!,
	determine_changed_pointers(T, NQ, Changes).

% pointer dissapeared? should not happen
determine_changed_pointers([_Q/_P|T], NQ, Changes):-
	etrace(pointer_error, determine_changed_pointers, general),
	!,
	determine_changed_pointers(T, NQ, Changes).

% done
switch_changed_pointers([], Corr, Corr).
% change
switch_changed_pointers([changed(Old, New)|T], CorrIn, CorrOut):-
	list_map([Old, New], M),
	replace_index_relations_for_correspondence(M, Old, New, CorrIn, CorrNew),
	switch_changed_pointers(T, CorrNew, CorrOut).

