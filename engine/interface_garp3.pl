
/*
interface_garp3.pl
Some functions formerly defined in interface.pl, but still needed in one way or the other.

Based on interface.pl, part of the Garp 2.01 standalone legacy version. Named authors: Martin Reinders & Bert Bredeweg & Jan Wielemaker & Floris Linnebank
Only the parts that are needed are here. Output will only be visible in debug mode, in the engine thread window.

PART OF Garp3. SEE COPYRIGHT NOTICE.

runs in namespace  'engine'
*/

/*
Tracer update feb 2007: all tracerstatements from the engine are routed
through this file to make adjustment easy. Lot of new code: etracer/3 at
bottom of file (at this moment the bottom of course...)
*/



/*
TRACER
The tracer has been changed to @tracer, using a trace window.
We did not change any old (garp) tracer calls, but just redefined the calls here.

New trace lines should use the methods of @tracer:
@tracer->trace_on(+option, ...), succeeds if option is on
@tracer->trace(+string, +option,...): shows string in the tracer if any one of the given option is on. This can even be used while ->>tell is in action. Does not interfere (but will be output first, before the output of ->>tell).
@tracer->>tell(+option,...): open stdout for tracing. Anything written to stdout will be saved for tracing (when any one of the given options is on). When @tracer->>told is called this will stop, and the output will be shown in the tracer.
    This call fails if all given options are off (like @tracer->trace_on). So you can use
    if
	@tracer->>tell(specification)
    then
    (
	writeln(blahblah)
	etc.
	@tracer->>told
    ).
*/

trace_on(Option):-
	@tracer->>trace_on(Option).
%%

tracer(Which, Atom, List) :-
	%we even copy the assumptions stuff from the old code ...
	%we do not use @tracer->>tell, so this one can be used while ->>tell is in force
	if
		atomic(Which)
	then
	(
		@tracer->>trace_on(Which),
		Option = Which,
		(
			Which == assumptions,
			current_assumption(_,Tab)
		;
			Tab = 0
		)
	)
	else
	(
		member(Option,Which),
		@tracer->>trace_on(Option),!,
		(
			member(assumptions,Which),
			current_assumption(_, Tab)
		;
			Tab = 0
		)
	),

	%create the string:
	TraceLine *= string,
	TraceLine->>insert_character('\t',times:=Tab),
	swritef(String,Atom, List),
	TraceLine->>append(String),
	@tracer->>trace(TraceLine,Option),
	!.

tracer(_, _, _).


/*
xml_comment/3 is plain undefined. Had something to do with a www-version of Garp. Code just not deleted.
We could search and destroy all calls to xml_comment, but we choose not do (portability + contract reasons)
*/
xml_comment(_,_,_).


/******************** NOT EDITED CODE BELOW (JUST COPIED FROM INTERFACE.PL) (now edited! feb 2007 FL)****************/
% show a term with single argument which is a list
% show each term of the same kind indented + 4
show_term_arg_list(I, Term) :-
	Term =.. [ Functor, List ],
	is_list(List),
	!,
	tab(I),
	(List == [] ->
	  write(Term)
	  ;
	  write(Functor),
	  write_ln('([ '),
	  NI is I + 4,
	  show_arg_list(NI, List),
	  tab(I),
	  atom_length(Functor, L),
	  tab(L),
	  write('  ])')
	).

% term is a system structure

show_term_arg_list(I, S) :-
	system_structure_slots(S, _, _, _, _),
	!,
	show_system_structure(I, S).

% write term anyway if not a term with a single list argument

show_term_arg_list(I, Term) :-
	tab(I),
	write(Term).

% show elements of list

show_arg_list(_, []).
show_arg_list(Indent, [H|T]) :-
	show_term_arg_list(Indent, H),
	(T \= [] ->
		write_ln(', '),
		show_arg_list(Indent, T)
		;
		nl
	).

show_to_list(_, []).
show_to_list(Indent, [to(Causes, Conditions, Results, ToStates, Status)|T]) :-
	tab(Indent),
	write_ln('to('),
	NI is (Indent + 4),
	show_term_arg_list(NI, Causes),
	write_ln(', '),
	show_term_arg_list(NI, Conditions),
	write_ln(', '),
	show_term_arg_list(NI, Results),
	write_ln(', '),
	show_term_arg_list(NI, ToStates),
	write_ln(', '),
	tab(NI),
	write(Status),
	write(')'),
	(T \= [] -> write_ln(', '), show_to_list(Indent, T) ; true).



/*  Show_antecedents.. moved here from solve.pl FL feb 2007
% new FL May 2004 garp 2.0:
Old with pointer bug: pointers are often not unique, the wrong quantity is displayed
New approach with substitutions list: see print_relation in interface.pl

show_antecedents([ P1, P2 | T ], R1, R2, R3, W, Q):-
	print_relation(P1, Q),
	write(' & '),
	print_relation(P2, Q),
	write_ln(' ->'),
	show_antecedents(T, R1, R2, R3, W, Q),
	!.

show_antecedents([], R1, R2, R3, W, Q):-
	print_relation(R1, Q),
	write(' & '),
	print_relation(R2, Q),
	write_ln(' ->'),
	print_relation(R3, Q),
	write(' '),
	write_ln(W),
	!.
New Below */


/* FL 1-3-07 OLD version with substitution list, new below
% s_a/6 to s_a/7: add empty substitutions list
show_antecedents(A, R1, R2, R3, W, Q):-
    show_antecedents(A, R1, R2, R3, W, Q, []).

% show_antecedents/7 with incoming substitutions
show_antecedents([ P1, P2 | T ], R1, R2, R3, W, Q, SIn):-
	print_relation(P1, Q, S1),
	write('   &   '),
	print_relation(P2, Q, S2),
	write_ln('   ->'),
	append(S1, S2, S3),
	append(S3, SIn, SOut),
	show_antecedents(T, R1, R2, R3, W, Q, SOut),
	!.

% show_antecedents/7 with incoming substitutions
show_antecedents([], R1, R2, R3, W, Q, SIn):-
	print_relation(R1, Q, S1),
	write('   &   '),
	print_relation(R2, Q, S2),
	write_ln('   ->'),
	print_relation(R3, Q, S3),
	write(' '),
	write_ln(W),
	append(S1, S2, S4),
	append(S3, S4, S5),
	append(SIn, S5, S6),
	list_to_set(S6, Substitutions),
	print_substitution_list(Substitutions), nl,
	!.

% end new FL May 2004 */

% FL 1-3-07 Third approach:
% print size3=size14=derivative38 > zero=flow4 (etc.)
%
%

show_antecedents([ P1, P2 | T ], R1, R2, R3, W, Q):-
	e_indent(5, I),
	write(I),
	print_relation(P1, Q),
	write(I),
	write_ln('  &'),
	write(I),
	print_relation(P2, Q),
	write(I),
	write_ln(' ==>'),
	show_antecedents(T, R1, R2, R3, W, Q),
	!.

show_antecedents([], R1, R2, R3, W, Q):-
	e_indent(5, I),
	write(I),
	print_relation(R1, Q),
	write(I),
	write_ln('  &'),
	write(I),
	print_relation(R2, Q),
	write(I),
	write(' ==>'),
	write_conclusion(W),
	nl,
	write(I),
	print_relation(R3, Q),
	!.

write_conclusion('contradiction.'):-
	write(' contradiction:').


write_conclusion('derived.'):-
	write(' derived:').


% transition_names: New version 18-2-2004 by FL:
% old '1a' style transition notation had only room for 256 transitions,
% new version '1t1, 1t2, 1t3, etc.' notation has no bounds

/*** Old version: ***
transition_names([], []).
transition_names([ FromState/ToNr/_/_ |T1 ], [ H|T2 ]) :-
	ToChar is 96 + ToNr,
	name(ToName, [ToChar]),
	concat(FromState, ToName, H),
	transition_names(T1, T2).
*** New Below: '1t1' notation ***/

transition_names([], []).
transition_names([ FromState/ToNr/_/_ |T1 ], [ Name|T2 ]) :-
    concat_atom([FromState, ToNr], t, Name),
	transition_names(T1, T2).

% end new FL


% unused? FL feb 07
parameter_visible_item(Par, Item) :-
	parameter(Par, _, _, SE, Inst, _, _),
	term_to_atom(SE, SEatom),
	concat_atom([Inst, '(', SEatom, ')' ], Item),
	!.





/* FL 2-3-07: NO more substitutions list... new below
% New FL July 2004, garp 2.0
% print_derivable_relations( +Der, +Q )
% to print a batch of relations it is nicer to print all relations
% and then all substitutions

print_derivable_relations( List, Q ):-
    print_derivable(List, Q), nl,
    write('-- substitutions: '), nl,
    sort_substitutions(Q, SQ),
    print_substitutions_set(SQ).

print_derivable(L, Q):-
	forall(member(X, L), (tab(2), print_relation(X, Q, _),nl)).

print_substitutions_set(Q):-
    forall(member(X, Q), (tab(2), print_substitution_list_items([X]),nl)).

sort_substitutions(In, Out):-
    add_key_quantities(In, Keyed),
    keysort(Keyed, Sorted),
    add_key_quantities(Out, Sorted).

add_key_quantities([], []).

add_key_quantities([H/I|T], [I-H/I|NT]):-
    add_key_quantities(T, NT).

% END NEW print derivable relations code */

print_derivable_relations( List, Q ):-
	forall(member(R, List), (tab(2), print_relation(R, Q, no))).

/*FL 2-3-07: NO more substitutions list... new below
% New Code, May 2004, do not print terms recursively
% but print the sum of indexes followed by an interpretation list
% eg. 1 + 5 (value(Q1)/1, value.. )etc.

% print relation & subsitution at once
% L is list of intern relations, Cin is current cio structure (see intern.pl & selector.pl)
print_relations(L, Cin) :-
	cio_q(Cin, Q),
	forall(member(X, L), (print_relation(X, Q, Subst), print_substitution_list(Subst), nl)).
*/

% new FL 2-3-07, no substitutionslist,
% but print every possible qty for a vector
print_relations(L, Cin) :-
	cio_q(Cin, Q),
	forall(member(X, L), print_relation(X, Q)).


/* Old: FL 1-3-07 prints only one quantity per bitvector
% print_relation(+Relation, +Q, -Substitutions)
% print Relation, using Q, return substitutions list for use in solve_trace (see solve.pl)
print_relation(relation(Left, Rel, Right), Q, Subst):-
        print_sum(Left, Q, Subst1),
        tab(1),
        write(Rel),
        tab(1),
        print_sum(Right, Q, Subst2),
        smerge(Subst1, Subst2, Subst).
*/

% New FL 7-3-07 sometimes print zero=...=... sometimes not
% default for old calls: yes
print_relation(X, Y):-
	print_relation(X, Y, yes).

% New FL 1-3-07 prints every possible quantity for each bitvector
% print_relation(+Relation, +Q)
% print Relation, using Q, followed by substitutions
print_relation(relation(Left, Rel, Right), Q, ZeroYN):-
        print_sumall(Left, Q, ZeroYN),
        tab(1),
        write(Rel),
        tab(1),
        print_sumall(Right, Q, ZeroYN), nl.

print_sumall(M, Q, ZeroYN):-
	map_list(M, L),
	printsumall2(L, Q, ZeroYN).

% zero = empty list = pointer 0
printsumall2([], _Q, no):-
	write(zero), !.
	% forall(member(Qty/0, Q), (write('='), write_qty(Qty))).

% zero = empty list = pointer 0
printsumall2([], Q, yes):-
	write(zero), !,
	forall(member(Qty/0, Q), (write('='), write_qty(Qty))).

% other: just to catch additions (lists longer then one item), 3rd call
printsumall2([H|T], Q, _):-
	%get the first:
	common_select(Q, Qty1/H, QR),
	write_qty(Qty1),
	%equal to the rest:
	forall(member(QtyR/H, QR), (write('='), write_qty(QtyR))),
	%Get additions:
	printsumall3(T, Q).

% done
printsumall3([], _).

% wrote head, add tail
printsumall3([H|T], Q):-
	write(' + '),
	%get the first:
	common_select(Q, Qty1/H, QR),
	write_qty(Qty1),
	%equal to the rest:
	forall(member(QtyR/H, QR), (write('='), write_qty(QtyR))),
	printsumall3(T, Q).

% translate into user notation and write:
write_qty(Qty):-
	translate_qty(Qty, Qty1),
	write(Qty1).

translate_qty(value(Qty), Qty):-
	!.

translate_qty(derivative(Qty), d(Qty)):-
	!.
% FL New mar 07: 2nd order derivative checking
translate_qty(second_derivative(Qty), dd(Qty)):-
	!.

% FL new jan 2012
translate_qty(third_derivative(Qty), ddd(Qty)):-
	!.

translate_qty(X, X).




translate_2ndorder_qty(value(Qty), Qty):-
	!.
translate_2ndorder_qty(derivative(Qty), d(Qty)):-
	!.
translate_2ndorder_qty(second_derivative(Qty), dd(Qty)):-
%translate_2ndorder_qty(second_derivative(Qty), d(Atom)):-
	%format(atom(Atom), 'd"(~w)', [Qty]),
	!.
translate_2ndorder_qty(X, X).



write_signs([Sign]):-!,
	write(Sign).
write_signs([Sign|Tail]):-
	write(Sign),
	write(', '),
	write_signs(Tail).



%unused now? FL 1-3-07
print_sum(L, Q, S) :-
	map_list(L, T),
	print_terms(T, Q, S).

%unused now? FL 1-3-07
print_terms([], _, []) :-
	write(zero), !.

%unused now? FL 1-3-07
print_terms(Sum, Q, Substitutions):-
	is_list(Sum),
	findall(Quantity/Index, (member(Index, Sum), member(Quantity/Index, Q)), Substitutions),
	print_index_sum(Sum).

%unused now? FL 1-3-07
% write indexes as sum of pointers
print_index_sum([X]):-
    write('X'),
    write(X).


%unused now? FL 1-3-07
print_index_sum([X, Y|Tail]):-
    write('X'),
    write(X),
    write(' + '),
    print_index_sum([Y|Tail]).

% print the list of substitutions with a space and normal brackets
print_substitution_list([]):-
    !.

print_substitution_list(Interpretation):-
    write('-- substitutions: '),
    print_substitution_list_items(Interpretation),
    write_ln(' . ').


print_substitution_list_items([Q/X]):-
    write(Q),
    write('/X'),
    write(X).

print_substitution_list_items([Q/X, Y|Tail]):-
    write(Q),
    write('/X'),
    write(X),
    write(', '),
    print_substitution_list_items([Y|Tail]).



print_equalities(Q) :-
	flag(q_cnt, H, H),	% highest quantity number
	succ(H, HH),
        print_equalities(1, HH, Q).

print_equalities(N, N, _) :- !.
print_equalities(N, H, Q) :-
        common_select(Q, _/N, NQ),
        memberchk(_/N, NQ), !,
        write('Equal quantities: '),
        forall(member(V/N, Q), (write(V), tab(2))),
        nl,
        succ(N, NN),
        print_equalities(NN, H, Q).

print_equalities(N, H, Q) :-
	succ(N, NN),
	print_equalities(NN, H, Q).

% END NEW print relations code


show_system_structure(Indent, Structure) :-
	system_structure_slots(Structure, Name, Isa, Cond, Giv),
	NI is Indent + 4,
	tab(Indent),
	write('system_structures('),
	write(Name),
	write_ln(', '),
	show_term_arg_list(NI, isa(Isa)),
	write_ln(', '),
	show_term_arg_list(NI, conditions(Cond)),
	write_ln(', '),
	show_term_arg_list(NI, givens(Giv)),
	write(')'),
	!.

/*% Old:
show_state(Name, Items) :-
	state(Name, SMD),
	state_status(Name, Status),
	state_to(Name, ToList),
	state_from(Name, From),
	write('state('),
	write(Name),
	write(', '),
	write(Status),
	(memberchk(from, Items) ->
		write_ln(', '),
		tab(4),
		write(From)
		;
		true),
	(memberchk(to, Items) ->
		write_ln(', '),
		tab(4),
		(ToList == [] -> write('[]') ;
			write_ln('['),
			show_to_list(8, ToList),
			write(']')
		)
		;
		true
	),
	(member(Some, Items),
	  memberchk(Some, [ system_elements,
				parameters,
				par_values,
				par_relations,
				system_structure_names,
				system_structures,
				input_model ]) ->
		write_ln(', '),
		show_smd(4, SMD, Items)
		;
		true
	),
	write_ln(').'),
	!.

*/

/* OLD
% show system model description
show_smd(Indent, SMD, Items) :-
	smd_slots2(SMD, Name, SE, P, V, R, SS, IS),
	tab(Indent),
	write('smd('),
	write(Name),
	write_ln(', '),
	NI is Indent + 4,
	(memberchk(system_elements, Items) ->
		show_term_arg_list(NI, SE),
		write_ln(', ')
		;
		true
	),
	(memberchk(parameters, Items) ->
		show_term_arg_list(NI, P),
		write_ln(', ')
		;
		true
	),
	(memberchk(par_values, Items) ->
		show_term_arg_list(NI, V),
		write_ln(', ')
		;
		true
	),
	(memberchk(par_relations, Items) ->
		show_term_arg_list(NI, R),
		write_ln(', ')
		;
		true
	),
	(memberchk(system_structure_names, Items),
	  \+ memberchk(system_structures, Items) ->
		SS = system_structures(SSL),
		findall(SName,
			(member(Str, SSL),
			  system_structure_slots(Str, SName, _, _, _)
			), SNames),
		show_term_arg_list(NI, system_structures(SNames)),
		write_ln(', ')
		;
	  memberchk(system_structures, Items) ->
		show_term_arg_list(NI, SS),
		write_ln(', ')
		;
		true
	),
	(IS = nil -> tab(Indent), write('_)')
		;
		(memberchk(input_model, Items) ->
			show_smd(NI, IS, [system_elements, parameters,
				par_values, par_relations, system_structures]),
			write(')')
			;
			tab(NI),
			write('_)')
		)
	).
*/


% NEW FL feb 2007
show_state(Name):-
	state(Name, SMD),
	state_status(Name, Status),
	% state_to(Name, _ToList),
	% state_from(Name, _From),
	write('State: '),
	write(Name),
	write(', Status: '),
	write(Status),
	nl, nl,
	show_smd(SMD),
	!.


% New FL feb 2007
show_smd(SMDIn):-
	sortSMD(SMDIn, SMD),
	smd_slots(SMD, _Name, SE, P, V, R, SS, _IS),
	write_ln('Entities:'),
	forall(member(instance(E, S), SE), (tab(2), write_entity(instance(E, S)),nl)), nl,
        write_ln('Configurations:'),
	forall(member(has_attribute(X, Y, Z), SE), (tab(2), write_configuration(has_attribute(X, Y, Z)),nl)), nl,
	write_ln('Quantities:'),
	forall(member(Q, P), (tab(2), write_quantity(Q),nl)), nl,
	write_ln('Values:'),
	forall(member(Val, V), (tab(2), write_value2(Val), nl)), nl,
	write_ln('Dependencies:'),
	forall(member(Rel, R), (tab(2), write_relation(Rel), nl)), nl,
	write_ln('MFs:'),
	forall(member(MF, SS), (tab(2), write_mf(MF), nl)).

sortSMD(SMDIn, SMD):-
	smd_slots(SMDIn, _, SE1, P1, V1, R1, SS1, _),
	msort(SE1, SE),
	msort(P1, P),
	msort(V1, V),
	rel_type_sort(R1, R),
	msort(SS1, SS),
	smd_slots(SMD, _, SE, P, V, R, SS, _).

rel_type_sort(In, Out):-
	split_rel_type(In, Ineq, DIneq, IP, Corr),
	msort(Ineq, Ineq1),
	msort(DIneq, DIneq1),
	msort(IP, IP1),
	msort(Corr, Corr1),
	append(Ineq1, DIneq1, Front),
	append(IP1, Corr1, Back),
	append(Front, Back, Out),
	!.

split_rel_type([], [], [], [], []).
split_rel_type([Rel|Tail], [Rel|Ineq], DIneq, IP, Corr):-
	pure_inequality_type(Rel),
	!,
	split_rel_type(Tail, Ineq, DIneq, IP, Corr).
split_rel_type([Rel|Tail], Ineq, [Rel|DIneq], IP, Corr):-
	d_inequality_type(Rel),
	!,
	split_rel_type(Tail, Ineq, DIneq, IP, Corr).
split_rel_type([Rel|Tail], Ineq, DIneq, [Rel|IP], Corr):-
	influence_proportional_type(Rel),
	!,
	split_rel_type(Tail, Ineq, DIneq, IP, Corr).
split_rel_type([Rel|Tail], Ineq, DIneq, IP, [Rel|Corr]):-
	% correspondence_type(Rel),
	% FL no check, should be corr anyway, but catches other things also without error. (more stable)
	split_rel_type(Tail, Ineq, DIneq, IP, Corr).

write_entity(instance(Entity, Type)):-
	writef('%w, of type: %w', [Entity, Type]).

write_configuration(has_attribute(X, Y, Z)):-
	writef('%w -- %w -- %w', [X, Y, Z]).

write_quantity(Q):-
	parameter(Q, _, Qty, Ent, Instance, _, Qspace),
	writef('%w(%w) (internal: %w), quantityspace: %w', [Qty, Ent, Instance, Qspace]).

write_value(value(Instance, _, Interval, Derivative)):-
	nonvar(Derivative),
	!,
	(var(Interval) -> Interval = ? ; true),
	writef('%w = %w, d(%w) = %w', [Instance, Interval, Instance, Derivative]).

write_value(value(Instance, _, Interval, _)):-
	(var(Interval) -> Interval = ? ; true),
	writef('%w = %w', [Instance, Interval]).

% write_value2: in state listing, always print derivative...
write_value2(value(Instance, _, Interval, Derivative)):-
	(var(Interval) -> Interval = ? ; true),
	(var(Derivative) -> Derivative = ? ; true),
	writef('%w = %w, d(%w) = %w', [Instance, Interval, Instance, Derivative]).


write_ass_list([], _).
write_ass_list([Ass|T], Indent):-
	translate_rel(Ass, List),  % FL (assumptions are always relations, quite sure..)
	write(Indent),
	writef('%w %w %w\n', List),
	write_ass_list(T, Indent).


write_relation(Relation):-
	translate_rel(Relation, List),
	writef('%w %w %w', List).


write_relations([], _).

write_relations([H|T], I):-
	translate_rel(H, List),
	write(I),
	writef('%w %w %w\n', List),
	write_relations(T, I).


write_mf(MF):-
	system_structure_slots(MF, Name, Isa, _, _),
	writef('%w, isa: ', [Name]),
	write_commalist(Isa).

write_commalist([X]):-
	!,
	write(X).

write_commalist([H|T]):-
	writef('%w, ', [H]),
	write_commalist(T).

write_nl_list([X], Indent):-
	!,
	writef('%w%w\n', [Indent, X]).

write_nl_list([H|T], Indent):-
	writef('%w%w,\n', [Indent, H]),
	write_nl_list(T, Indent).

% New FL feb 07
%

show_terminations([]).
show_terminations([To|Tail]):-
	To =.. [_, cause(List)|_],
	show_terminations_list(List, 1),
	nl,
	show_terminations(Tail).

show_terminations_list([], _).

show_terminations_list([H|T], L):-
	e_indent(L, I),
	write(I),
	writef('%w\n', [H]),
	show_terminations_list(T, L).

%control indent here
% indent levels as spaces, because tab(x) assumes write mode, not usable
% in trace call.
e_indent(1, '   ').
e_indent(2, '      ').
e_indent(3, '         ').
e_indent(4, '            ').
e_indent(5, '               ').


% control new lines here
write_adding_nl.

write_extra_adding_nl.

write_sec_nl.

write_end_nl.

write_main_nl:-
	nl,nl.

write_conclusion_nl.

write_item_nl.

% E(ngine)TRACE
% Tracer update feb 2007: all tracerstatements from the engine are
% routed through this file to make adjustment easy.
%
% For every single event an etrace clause with a unique identifyer is
% made to make the statement. Variables and traceroptions are passed on.
% (options not used now).
%
% Is Options a list? should be, but it is not now. a loop using member
% could be used. execute when one of given options is on.
% Anser: PCE cannot handle lists... thus etrace options should be a
% single argument, and where more items (like critical ones) are needed
% they should be hardcoded. unless we want to work with
% T =.. [trace, String, List]
% but that seems bad because it is only needed in a few cases and would
% be to expensive to use in general.
%
% Identifiers start with filename for easy crossref during programming.
%
% Added extra trace category 'warning' which is always printed and
% used for errors and warnings. (colour: general)


etrace(nl, Options):-
	is_list(Options),
	!,
	T =.. [trace, '\n' | Options],
	@tracer->>T.

etrace(nlnl, Options):-
	is_list(Options),
	!,
	T =.. [trace, '\n\n' | Options],
	@tracer->>T.

etrace(implied_nl, _Options):-
	%is_list(Options),
	!.
	% T =.. [trace,		!. %,	@tracer->>trace('\n', Options).

etrace(nl, Options):-
	!,
	@tracer->>trace('\n', Options).

etrace(nlnl, Options):-
	!,
	@tracer->>trace('\n\n', Options).

etrace(implied_nl, _Options):-
	!. %,	@tracer->>trace('\n', Options).

etrace(blockstart, Options):-
	is_list(Options),
	!,
	T =.. [trace, '\n--------------------------------------------------------------------------\n'|Options],
	@tracer->>T.

etrace(blockstart, Option):-
	!,
	@tracer->>trace('\n--------------------------------------------------------------------------\n', Option).

etrace(blockstop, Options):-
	!,
	@tracer->>trace('--------------------------------------------------------------------------\n\n', Options).

etrace(branchepoint, Options):-
	!,
	etrace(blockstart, Options),
	@tracer->>trace('Branching in statesearch, multiple possibililities from here on.\n', Options, general), %tempflo: critical
	etrace(blockstop, Options).

etrace(pathstart, Options):-
	!,
	etrace(blockstart, [Options, general]),
	@tracer->>trace('Statesearch going into branch.\n', Options, general), %tempflo: critical
	etrace(blockstop, Options).


etrace(pathstop, Options):-
	!,
	etrace(blockstart, Options),
	@tracer->>trace('Statesearch ended for this branch, backtracking to next branch if possible.\n', Options, general), %tempflo: critical
	etrace(blockstop, Options),
	@tracer->>finish. % to flush text on tracewindow.

etrace(X, _):-
	!,
	(
	@tracer->>tell(warning)
	->
	writef('\n\n*** Error: unkown tracecall to etrace2: %w\n\n', [X]),
	@tracer->>told
	;
	true
	).



etrace(_, _, _):-
	flag(tracerActive, X, X),
	X \== true,
	!.



/* TOP refered etrace/3 preds */


etrace(class_add_rel, [Relation], _Options):-
	!,
	(
	 @tracer->>tell(add_relation)
	->
	  write_item_nl,
	 e_indent(3,I),
	 translate_rel(Relation, List),
	 writef('%wadd: %w %w %w\n', [I|List]),
	 @tracer->>told
	;
	 true
	).

etrace(class_add_rel_fail, [Relation], _Options):-
	!,
	(
	 @tracer->>tell(add_relation)
	->
	 write_item_nl,
	 e_indent(3,I),
	 translate_rel(Relation, List),
	 writef('%w# inconsistent: %w %w %w\n', [I|List]),
	 @tracer->>told
	;
	 true
	).

etrace(class_add_cor, [Relation], _Options):-
	!,
	(
	 @tracer->>tell(add_relation)
	->
	  write_item_nl,
	 e_indent(3,I),
	 translate_cor(Relation, List),
	 writef('%wadd: %w\n', [I|List]),
	 @tracer->>told
	;
	 true
	).

etrace(class_add_cor_fail, [Relation], _Options):-
	!,
	(
	 @tracer->>tell(add_relation)
	->
	  write_item_nl,
	 e_indent(3,I),
	 translate_cor(Relation, List),
	 writef('%w# inconsistent: %w\n', [I|List]),
	 @tracer->>told
	;
	 true
	).




etrace(reclass_match_state, State, _Options):-
	!,
	format(string(Msg), 'matching with existing state: ~w\n', [State]),
	@tracer->>trace(Msg, transition).

etrace(reclass_match_mf, _, _Options):-
	!,
	@tracer->>trace('   matching MFs \n', transition).
	% @tracer->>trace('entities OK, \nmatching MFs \n', transition).

etrace(reclass_match_se, _, _Options):-
	!,
        @tracer->>trace('   matching entities\n', transition).
        % @tracer->>trace('values OK, \nmatching entities\n', transition).

etrace(reclass_match_val, _, _Options):-
	!,
	@tracer->>trace('   matching values\n', transition).

etrace(reclass_match_rel, _, _Options):-
	!,
	@tracer->>trace('   matching relations\n', transition).
	% @tracer->>trace('MFs OK, \nmatching relations\n', transition).

etrace(reclass_match_continuity, _, _Options):-
	!,
	@tracer->>trace('   testing continuity constraints\n', transition).
	% @tracer->>trace('relations OK, \ntesting continuity constraints\n', transition).

etrace(reclass_match_rel_der, _, _Options):-
	!,
	@tracer->>trace('Checking derivative continuity with existing state\n', transition).


etrace(reclass_recheck_mf, SName, _Options):-
	!,
        (
	  @tracer->>tell(respecification)
	->
	    write_main_nl,
            writef('re-Checking MF: %w\n', [SName]),
	  @tracer->>told
	;
          true
        ).

etrace(reclass_recheck_mf_ok, SName, _Options):-
	!,
        (
	  @tracer->>tell(respecification, general)  %tempflo: critical
	->
            writef('re-Accept MF: %w\n', [SName]),
	  @tracer->>told
	;
          true
        ).

etrace(reclass_recheck_mf_contradiction, SName, _Options):-
	!,
        (
	  @tracer->>tell(respecification, general)  %tempflo: critical
	->
            writef('# consequenses of MF: %w inconsistent: inconsistent state\n', [SName]),
	  @tracer->>told,
	  etrace(pathstop, respecification)
	;
          true
        ).

% POSTPONE??? algorithm should be checked to see what really goes on...
etrace(reclass_recheck_delay, SName, _Options):-
	!,
        (
	  @tracer->>tell(respecification, general)  %tempflo: critical
	->
            writef('Postpone decision on MF: %w, derivative assumptions required\n', [SName]),
	  @tracer->>told
	;
          true
        ).

etrace(reclass_recheck_remove_static, SName, _Options):-
	!,
        (
	  @tracer->>tell(respecification, general)  %tempflo: critical
	->
            writef('re-Reject MF: %w, removing entity consequenses\n', [SName]),
	  @tracer->>told
	;
          true
        ).

etrace(reclass_recheck_remove_proces_agent, SName, _Options):-
	!,
        (
	  @tracer->>tell(respecification, general)  %tempflo: critical
	->
            writef('re-Reject MF: %w\n', [SName]),
	  @tracer->>told
	;
          true
        ).

etrace(reclass_recheck_fail_GAV, SName, _Options):-
	!,
        (
	  @tracer->>tell(respecification, general)  %tempflo: critical
	->
	  writef('Cannot re-check MF: %w\n', [SName]),
	  @tracer->>told
	;
          true
        ),
        (
	  @tracer->>tell(respecification)  %tempflo: critical ???
	->
	  e_indent(1, I),
	  writef('%wReason: Generate All Values mechanism\n', [I]),
	  writef('%wResult: MF will be checked later in normal MF-search\n', [I]),
	  @tracer->>told
	;
          true
        ).




etrace(class_check_hyp, SName, _Options):-
	!,
	(
	  @tracer->>tell(specification)
        ->
	  write_main_nl,
	  writef('Check candidate MF: %w\n', [SName]),
	  @tracer->>told
	;
	  true
	).

etrace(class_reject_mf, SName, _Options):-
	!,
	(
	  @tracer->>tell(specification, general) %tempflo: critical
	->
	  write_conclusion_nl,
	  writef('Reject MF: %w\n', [SName]),
	  @tracer->>told
	;
	  true
	).

etrace(class_reject_mf_conseq, Sname, _Options):-
	!,
	(
	  @tracer->>tell(specification, general) %tempflo: critical
	->
	  write_conclusion_nl,
	  writef('# %w (or a parent) has inconsistent consequenses, inconsistent state\n', [Sname]),
	  @tracer->>told,
	  etrace(pathstop, specification)
	;
	  true
	).

etrace(class_reject_mf_conseq_step_one, Sname, _Options):-
	!,
	(
	  @tracer->>tell(specification, general) %tempflo: critical
	->
	  write_conclusion_nl,
	  writef('# %w (or a parent) has inconsistent consequenses, inconsistent state\n', [Sname])
	;
	  true
	),
	(
	  @tracer->>tell(specification) %tempflo: not! critical
	->
	  nl,nl
	;
	  true
	),
	(
	  @tracer->>tell(specification, general) %tempflo: critical
	->
	  writef('Determining alternative branches in statesearch:\n', []),
	  @tracer->>told
	;
	  true
	).

etrace(class_reject_mf_conseq_step_two, _Sname, _Options):-
	!,
	(
	  @tracer->>tell(specification)
	->
	  write_conclusion_nl,
	  writef('\nDone determining alternative branches in statesearch\n', []),
	  @tracer->>told,
	  etrace(pathstop, specification)
	;
	  true
	).

etrace(class_accept_mf, Sname, _Options):-
	!,
	(
	  @tracer->>tell(specification, general) %tempflo: critical
	->
	  write_conclusion_nl,
	  writef('Accept MF: %w\n', [Sname]),
	  @tracer->>told
	;
	  true
	),
	(
	  @tracer->>tell(specification)
	->
	  write_main_nl,
	  writef('Searching children for accepted MF: %w\n', [Sname]),
	  @tracer->>told
	;
	  true
	).

etrace(class_hyp_child, [Child, _Hyp], _Options):-
	!,
	(
	  @tracer->>tell(specification)
	->
	  e_indent(1, I),
	  writef('%wcandidate: %w\n', [I, Child]),
	  @tracer->>told
	;
	  true
	).

etrace(class_search_hyp_ent, List, _Options):-
	!,
	(
	  @tracer->>tell(specification)
	->
	  write_main_nl,
	  writef('Searching candidate MFs for entity: %w, of type: %w\n', List),
	  @tracer->>told
	;
	  true
	).

etrace(class_isa_candidate, List, _Options):-
	!,
	(
	  @tracer->>tell(specification)
	->
	  write_item_nl,
	  e_indent(1,I),
	  writef('%wcandidate: %w\n', [I|List]),
	  @tracer->>told
	;
	  true
	).

etrace(class_search_hyp_mf, List, _Options):-
	!,
	(
	  @tracer->>tell(specification)
	->
	  write_main_nl,
	  writef('Searching candidate MFs for MF: %w\n', List),
	  @tracer->>told
	;
	  true
	).


etrace(class_add_qty_known, List, _Options):-
	!,
	(
	  @tracer->>tell(specification)
	->
	  write_item_nl,
	  e_indent(3,I),
	  writef('%wadd: %w(%w), quantityspace: %w, internal: %w (already known)\n', [I|List]),
	  @tracer->>told
	;
	  true
	).


etrace(class_add_qty, List, _Options):-
	!,
	(
	  @tracer->>tell(specification)
	->
	  write_item_nl,
	  e_indent(3,I),
	  writef('%wadd: %w(%w), quantityspace: %w, internal: %w\n', [I|List]),
	  @tracer->>told
	;
	  true
	).


etrace(class_no_reasoning_assumptions, [], _Options):-
	!,
	(
	  @tracer->>tell(assumptions)
	->
	  % e_indent(1, I),
	  writef('\nNo Reasoning Assumptions will be made. Simulation preference is turned off\n', []),
	  @tracer->>told
	;
	  true
	).
etrace(class_no_conditional_derivatives, [], _Options):-
	!,
	(
	  @tracer->>tell(assumptions)
	->
	  e_indent(1, I),
	  writef('Simulation Preference "Allow reasoning assumptions on conditional derivatives" is turned off.\n%wChecking for conditional derivative dependencies:\n', [I]),
	  @tracer->>told
	;
	  true
	).

etrace(class_no_conditional_derivatives_MF_reject, [AssList, Hyp], _Options):-
	!,
	(
	  @tracer->>tell(assumptions)
	->
	  e_indent(1, I),
	  e_indent(2, I2),
	  writef('\nMF %w will not be assumed. \n', [Hyp]),
	  writef('%wConditional derivative dependencies:\n', [I]),
	  write_relations(AssList, I2),
	  @tracer->>told
	;
	  true
	),
       (
	  @tracer->>tell(assumptions, general)  %tempflo: critical
	->
            writef('Reject MF: %w, No derivative assumptions allowed\n', [Hyp]),
	  @tracer->>told
	;
          true
        ).





/*	solve.pl  */
etrace(solve_ir_search, _, _Options):-
	!,
	@tracer->>trace('Searching all influences and proportionalities...\n', resolve).

etrace(solve_cw_start, _, _Options):-
	!,
	(
	  @tracer->>tell(resolve)
	->
	  write_main_nl,
	  %write('Applying closed world assumption:\n'), %tempflo: critical no
	  write('Setting unknown influences to zero:\n'),
	  @tracer->>told
	;
	  true
	).

etrace(second_order_proportionality_propagation, _, _Options):-
	!,
	(
	  @tracer->>tell(resolve)
	->
	  write_main_nl,
	  write('Allowing second order derivative propagation over proportionalities: making sign-equality assumption.\n'),
	  @tracer->>told
	;
	  true
	).

etrace(no_second_order_proportionality_propagation, _, _Options):-
	!,
	(
	  @tracer->>tell(resolve)
	->
	  write_main_nl,
	  write('Not allowing second order derivative propagation over proportionalities: No sign-equality assumption.\n'),
	  @tracer->>told
	;
	  true
	).

etrace(third_order_proportionality_propagation, _, _Options):-
	!,
	(
	  @tracer->>tell(resolve)
	->
	  write_main_nl,
	  write('Allowing third order derivative propagation over proportionalities: making sign-equality assumption on this level.\n'),
	  @tracer->>told
	;
	  true
	).

etrace(no_third_order_proportionality_propagation, _, _Options):-
	!,
	(
	  @tracer->>tell(resolve)
	->
	  write_main_nl,
	  write('Not allowing third order derivative propagation over proportionalities: No sign-equality assumption on this level.\n'),
	  @tracer->>told
	;
	  true
	).


etrace(solve_cw_second_order_start, _, _Options):-
	!,
	(
	  @tracer->>tell(resolve)
	->
	  write_main_nl,
	  %write('Applying closed world assumption:\n'), %tempflo: critical no
	  write('Setting unknown second order influences to zero:\n'),
	  @tracer->>told
	;
	  true
	).

etrace(solve_cw_third_order_start, _, _Options):-
	!,
	(
	  @tracer->>tell(resolve)
	->
	  write_main_nl,
	  %write('Applying closed world assumption:\n'), %tempflo: critical no
	  write('Setting unknown third order influences to zero:\n'),
	  @tracer->>told
	;
	  true
	).

etrace(solve_cw_zero, [Qty], Options):-
	!,
	(
	  @tracer->>tell(Options, general)
	->
	  translate_qty(Qty, Q),
	  writef('   assuming %w is zero\n', [Q]), %tempflo: critical YES
	  @tracer->>told
	;
	  true
	).

etrace(solve_add_ir_res_fail, append_relation(_Relation, _, _, _, _, _, _), Options):-
	!,
	@tracer->>trace('# Can not add resulting effect (derivative allready known or constrained?), inconsistent state\n', Options, general), %tempflo: critical
	etrace(pathstop, resolve).


etrace(solve_resolve_2ndorder_start, _, _Options):-
	!,
	@tracer->>trace('\n\nDetermining 2nd order derivatives:\n', resolve).

etrace(solve_resolve_2ndorder_done, _, _Options):-
	!,
	@tracer->>trace('\n\nFinished determining 2nd order derivatives\n', resolve).

etrace(solve_resolve_3rdorder_start, _, _Options):-
	!,
	@tracer->>trace('\n\nDetermining 3rd order derivatives:\n', resolve).

etrace(solve_resolve_3rdorder_done, _, _Options):-
	!,
	@tracer->>trace('\n\nFinished determining 3rd order derivatives\n', resolve).

etrace(solve_found_invalid, _, _Options):-
	!,
	@tracer->>trace('# inconsistent relation\n', add_relation).

etrace(solve_simple_found_invalid, _, _Options):-
	!,
	@tracer->>trace('         # inconsistent relation found by analyze simple relation\n', add_relation).

etrace(solve_zero_found_invalid, _, _Options):-
	!,
	@tracer->>trace('         # inconsistent relation found by analyze zero relation\n', add_relation).

etrace(solve_derivable_rel, _, _Options):-
	!,
	@tracer->>trace('derivable relation\n', add_relation).

etrace(solve_added_rel, _, _Options):-
	!.  %,@tracer->>trace('added relation\n', add_relation). too obvious. its either: derivable, inconsistent, or added.


etrace(division_by_zero, [Relation, Divisor], _Options):-
	!,
	(
	  @tracer->>tell(general, inequality)
	->
	  e_indent(3,I1),
	  writef('%w# Division by zero in relation: %w by quantity: %w\n', [I1, Relation, Divisor]),
	  @tracer->>told
	;
	  true
	).


etrace(solve_derived_invalid, _, _Options):-
	!,
	(
	  @tracer->>tell(inequality)
	->
	  e_indent(4,I1),
	  writef('%w# inconsistent relation derived:\n', [I1]),
	  @tracer->>told
	;
	  true
	).

etrace(solve_found_valid, _, _Options):-
	!,
	(
	  @tracer->>tell(inequality)
	->
	  e_indent(4,I1),
	  writef('%wnew fact derived:\n', [I1]),
	  @tracer->>told
	;
	  true
	).

etrace(solve_invalid_input, [Rel, Q], inequality):-
	!,
	(
	  @tracer->>tell(inequality)
	->
	  e_indent(5, I),
	  write(I),
	  print_relation(Rel, Q),
	  @tracer->>told
	;
	  true
	).


etrace(solve_2ndorder_ir_no_der, [TrQ, _TrList, _QList, _Derivable], _Options):-
	!,
      (
	@tracer->>tell(resolve)
      ->
	write_main_nl,
	translate_qty(TrQ, EQ),
	writef('Derivative of %w is unknown, not calculating 2nd order derivative.\n', [EQ]),
	@tracer->>told,
	!
      ;
	true
      ).

etrace(solve_3rdorder_ir_no_der, [TrQ, _TrList, _QList, _Derivable], _Options):-
	!,
      (
	@tracer->>tell(resolve)
      ->
	write_main_nl,
	translate_qty(TrQ, EQ),
	writef('Second derivative of %w is unknown, not calculating 3rd order derivative.\n', [EQ]),
	@tracer->>told,
	!
      ;
	true
      ).

etrace(solve_ir_set, [TrQ, TrList, _QList, _Derivable], _Options):-
	!,
      (
	@tracer->>tell(resolve)
      ->
	e_indent(2, I2),
	write_main_nl,
	translate_qty(TrQ, d(EQ)),
	writef('Effects on %w are: \n', [EQ]),
        % finding values is at this point hard, large procedure...
        % leave it for now
        % solution: give simple explanation...
	forall(member(One, TrList), write_addone(One, EQ, I2)),
	@tracer->>told,
	!
      ;
	true
      ).

etrace(solve_ca_filter, [TrQ1, TrQ2], _Options):-
	!,
      (
	@tracer->>tell(resolve)
      ->
	e_indent(1, I2),
	write_main_nl,
	writef('Not applying Compare Derivatives on d(%w): \n', [TrQ1]),
        writef('%wQuantity has a P* or P/ relation from d(%w).\n', [I2, TrQ2]),
	@tracer->>told,
	!
      ;
	true
      ).


etrace(solve_ca_set, [TrQ1, TrQ2], _Options):-
	!,
      (
	@tracer->>tell(resolve)
      ->
	write_main_nl,
	writef('Compare Derivatives: d(%w) and d(%w): \n', [TrQ1, TrQ2]),
	@tracer->>told,
	!
      ;
	true
      ).

etrace(solve_ir_translate_p_star_bad_pattern, [TrPstar, TrPslash, TrQ], resolve):-
	!,
      (
	@tracer->>tell(resolve, general)
      ->
	e_indent(2, I2),
	write_main_nl,
	write('\n*** Warning: possible incorrect P* and P/ use: ***\n'),
	translate_qty(TrQ, d(EQ)),
	forall(member(One, TrPstar), write_addone(One, EQ, I2)),
	forall(member(One, TrPslash), write_addone(One, EQ, I2)),
	writef('%wTypical multiplication uses two P* relations.\n', [I2]),
	writef('%wTypical division uses a P* and a P/ relation.', [I2]),
	@tracer->>told,
	!
      ;
	true
      ).

etrace(solve_ir_translate_p_star, [TrQ, TrList], resolve):-
	!,
      (
	@tracer->>tell(resolve)
      ->
	e_indent(2, I2),
	write_main_nl,
	translate_qty(TrQ, d(EQ)),
	writef('Determining multiplication / division effects on %w: \n', [EQ]),
	forall(member(One, TrList), write_addone(One, EQ, I2)),
	@tracer->>told,
	!
      ;
	true
      ).
/*
etrace(solve_ir_translate_p_star_p_slash, [TrQ, TrList], resolve):-
	!,
      (
	@tracer->>tell(resolve)
      ->
	e_indent(2, I2),
	write_main_nl,
	translate_qty(TrQ, d(EQ)),
	writef('Determining division effects on %w: \n', [EQ]),
	forall(member(One, TrList), write_addone(One, EQ, I2)),
	@tracer->>told,
	!
      ;
	true
      ).
*/


etrace(solve_2ndorder_ir_set, [TrQ, TrList, _QList, _Derivable], _Options):-
	!,
      (
	@tracer->>tell(resolve)
      ->
	e_indent(2, I2),
	write_main_nl,
	translate_qty(TrQ, EQ),
	writef('Effects on %w are: \n', [EQ]),
	forall(member(One, TrList), write_addone_2nd_order(One, EQ, I2)),
	@tracer->>told,
	!
      ;
	true
      ).

etrace(solve_3rdorder_ir_set, [TrQ, TrList, _QList, _Derivable], _Options):-
	!,
      (
	@tracer->>tell(resolve)
      ->
	e_indent(2, I2),
	write_main_nl,
	translate_qty(TrQ, EQ),
	writef('Effects on %w are: \n', [EQ]),
	forall(member(One, TrList), write_addone_3rd_order(One, EQ, I2)),
	@tracer->>told,
	!
      ;
	true
      ).

etrace(solve_ir_unres, [UnknownList], _Options):-
	!,
      (
	@tracer->>tell(resolve)
      ->
	e_indent(1, I1),
	e_indent(2, I2),
	writef('%wUnknown (still):\n', [I1]),
	write_unknow_inf(UnknownList, I2),
	writef('%wEffects unresolved\n', [I1]),
	@tracer->>told,
	!
      ;
	true
      ).

etrace(solve_2ndorder_ir_unres, [UnknownList], _Options):-
	!,
      (
	@tracer->>tell(resolve)
      ->
	e_indent(1, I1),
	e_indent(2, I2),
	writef('%wUnknown (still):\n', [I1]),
	write_2ndorder_unknow_inf(UnknownList, I2),
	writef('%wEffects unresolved\n', [I1]),
	@tracer->>told,
	!
      ;
	true
      ).


etrace(solve_3rdorder_ir_unres, [UnknownList], _Options):-
	!,
      (
	@tracer->>tell(resolve)
      ->
	e_indent(1, I1),
	e_indent(2, I2),
	writef('%wUnknown (still):\n', [I1]),
	write_2ndorder_unknow_inf(UnknownList, I2),
	writef('%wEffects unresolved\n', [I1]),
	@tracer->>told,
	!
      ;
	true
      ).

%single resolved influence
etrace(solve_ir_single_res, [TrInf, Sign], _Options):-
	!,
      (
	@tracer->>tell(resolve)
      ->
	e_indent(2, I2),
	process_single_influence(TrInf, Sign, DI, EF),
	write(I2),
        writef('%w is %w\n', [DI, EF]),
	@tracer->>told,
	!
      ;
	true
      ).

etrace(solve_2ndorder_ir_single_res, [TrInf, Sign], _Options):-
	!,
      (
	@tracer->>tell(resolve)
      ->
	e_indent(2, I2),
	process_single_influence(TrInf, Sign, DI, EF),
	write(I2),
        writef('%w is %w\n', [DI, EF]),
	@tracer->>told,
	!
      ;
	true
      ).

etrace(solve_3rdorder_ir_single_res, [TrInf, Sign], _Options):-
	!,
      (
	@tracer->>tell(resolve)
      ->
	e_indent(2, I2),
	process_single_influence(TrInf, Sign, DI, EF),
	write(I2),
        writef('%w is %w\n', [DI, EF]),
	@tracer->>told,
	!
      ;
	true
      ).

%multiple resolved influence with sign addition
etrace(solve_ir_add, [EffectsList], _Options):-
	!,
      (
	@tracer->>tell(resolve)
      ->
	e_indent(2, I2),
	forall(member(A/S/E, EffectsList), process_sign_add_item(A, S, E, I2)),
	@tracer->>told,
	!
      ;
	true
      ).

%multiple with loop
etrace(solve_ir_loop, [TrQ, Sign], _Options):-
	!,
      (
	@tracer->>tell(resolve)
      ->
	e_indent(2, I2),
	writef('%wCausal loop detected. Assuming ambiguity and rechecking node later.\n', [I2]),
	%forall(member(A/S/E, EffectsList), process_sign_add_item(A, S, E, I2)),
	@tracer->>told,
	!
      ;
	true
      ),
      (
        @tracer->>tell(resolve, general) %tempflo: critical
      ->
        translate_qty(TrQ, Q),
        writef('Resulting effect on %w is ambiguous. possibilities are: ', [Q]),
        write_signs(Sign),
	nl,
	@tracer->>told
      ;
	true
      ),
      etrace(branchepoint, resolve).   %tempflo: critical but branchepoint is critical in general... :).


%multiple with loop at bottleneck
etrace(solve_ir_loop_bottleneck, [EffectsList], _Options):-
	!,
      (
	@tracer->>tell(resolve)
      ->
	e_indent(2, I2),
	writef('%wAt bottleneck in causal loop. Propagating known effects and rechecking node later.\n', [I2]),
	forall(member(A/S/E, EffectsList), process_sign_add_item(A, S, E, I2)),
	@tracer->>told,
	!
      ;
	true
      ).




%multiple resolved influence with sign addition
etrace(solve_2ndorder_ir_add, [EffectsList], _Options):-
	!,
      (
	@tracer->>tell(resolve)
      ->
	e_indent(2, I2),
	forall(member(A/S/E, EffectsList), process_sign_add_item_2ndorder(A, S, E, I2)),
	@tracer->>told,
	!
      ;
	true
      ).

etrace(solve_ir_res, [TrQ, Sign], _Options):-
	!,
      (
	@tracer->>tell(resolve)
      ->
	translate_qty(TrQ, Q),
	translate_sign(Sign, Effect, Value),
	writef('%w resulting effect: %w = %w\n', [Effect, Q, Value]),
	@tracer->>told,
	!
      ;
	true
      ).

etrace(solve_2ndorder_ir_res, [TrQ, Sign], _Options):-
	!,
      (
	@tracer->>tell(resolve)
      ->
	translate_qty(TrQ, Q),
	translate_sign(Sign, Effect, Value),
	writef('%w resulting effect: %w = %w\n', [Effect, Q, Value]),
	@tracer->>told,
	!
      ;
	true
      ).

etrace(solve_ir_bal, [Influences, Pos, Neg], _Options):-
	!,
      (
	@tracer->>tell(resolve)
      ->
	e_indent(1, I1),
	e_indent(2, I2),
	write(I1),
	write('Balancing multiple effects: \n'),
	construct_tracer_balance(Influences, PList, NList),
	concat_atom(PList, ' + ', Pos1),
	(Pos1 == '' -> Pos = 'zero' ; Pos1 = Pos),
	concat_atom(NList, ' + ', Neg1),
	(Neg1 == '' -> Neg = 'zero' ; Neg1 = Neg),
	write(I2),
	writef('testing relation between: %w and %w\n', [Pos, Neg]),
	@tracer->>told,
	!
      ;
	true
      ).

etrace(solve_ca_unequal, [], _Options):-
	!,
      (
	@tracer->>tell(resolve)
      ->
	e_indent(2, I),
	write(I),
	write('Derivative signs are unequal.\n'),
	write(I),
	write('Compare Derivatives unnecessary.\n'),
	@tracer->>told,
	!
      ;
	true
      ).

etrace(solve_ca_bal, [Influences1, Influences2, Pos, Neg], _Options):-
	!,
      (
	@tracer->>tell(resolve)
      ->
	e_indent(1, I1),
	e_indent(2, I2),
	write(I1),
	write('Balancing comparative effects: \n'),
	construct_tracer_balance(Influences1, PList1, NList1),
        construct_tracer_balance(Influences2, NList2, PList2),
        append(PList1, PList2, PList),
        append(NList1, NList2, NList),
	concat_atom(PList, ' + ', Pos1),
	(Pos1 == '' -> Pos = 'zero' ; Pos1 = Pos),
	concat_atom(NList, ' + ', Neg1),
	(Neg1 == '' -> Neg = 'zero' ; Neg1 = Neg),
	write(I2),
	writef('testing relation between: %w and %w\n', [Pos, Neg]),
	@tracer->>told,
	!
      ;
	true
      ).

etrace(solve_2ndorder_ir_bal, [Influences, Pos, Neg], _Options):-
	!,
      (
	@tracer->>tell(resolve)
      ->
	e_indent(1, I1),
	e_indent(2, I2),
	write(I1),
	write('Balancing multiple effects: \n'),
	construct_2ndorder_tracer_balance(Influences, PList, NList),
	concat_atom(PList, ' + ', Pos1),
	(Pos1 == '' -> Pos = 'zero' ; Pos1 = Pos),
	concat_atom(NList, ' + ', Neg1),
	(Neg1 == '' -> Neg = 'zero' ; Neg1 = Neg),
	write(I2),
	writef('testing relation between: %w and %w\n', [Pos, Neg]),
	@tracer->>told,
	!
      ;
	true
      ).

etrace(solve_ir_bal_res, [TrQ, Pos, Neg, Sign], _Options):-
	!,
      (
	@tracer->>tell(resolve)
      ->
	e_indent(2, I2),
        translate_qty(TrQ, Q),
        translate_sign(Sign, Effect, Value),
	zero_relation_sign(S, Sign),
	write(I2),
	writef('balance result: %w %w %w\n', [Pos, S, Neg]),
        writef('%w resulting effect: %w = %w\n', [Effect, Q, Value]), %tempflo: critical??? no...
        @tracer->>told
      ;
        true
      ).

etrace(solve_ca_bal_res, [Q1, Q2, Relation, Pos, Neg], _Options):-
	!,
      (
	@tracer->>tell(resolve)
      ->
	e_indent(1, I1),
	e_indent(2, I2),
	write(I2),
        writef('balance result: %w %w %w\n', [Pos, Relation, Neg]),
        write(I1),
        writef('Adding comparative analysis result: d(%w) %w d(%w)\n', [Q1, Relation, Q2]),
        @tracer->>told
      ;
        true
      ).

etrace(solve_ca_bal_no_res, [Q1, Q2, Pos, Neg], _Options):-
	!,
      (
	@tracer->>tell(resolve)
      ->
	e_indent(1, I1),
	e_indent(2, I2),
	write(I2),
	writef('balance result: %w =?= %w\n', [Pos, Neg]),
        write(I1),
        writef('Undecided comparative analysis between d(%w) and d(%w)\n', [Q1, Q2]),
	@tracer->>told
      ;
        true
      ).

etrace(solve_ca_done, [_Q1, _Q2], _Options):-
	!,
      (
	@tracer->>tell(resolve)
      ->
	write('Comparison of derivatives done for this quantity pair.'),
	@tracer->>told
      ;
        true
      ).


etrace(solve_2ndorder_ir_bal_res_assume_zero, [TrQ, Pos, Neg, zero], _Options):-
	!,
      (
	@tracer->>tell(resolve)
      ->
	e_indent(2, I2),
	write(I2),
	writef('balance result: %w =?= %w\n', [Pos, Neg]),
        write('Unknown resulting effect\n'),
	@tracer->>told
      ;
        true
      ),
      (
        @tracer->>tell(general) %tempflo: critical
      ->
        translate_qty(TrQ, Q),
	writef('Assuming: %w = %w\n', [Q, zero]),
        @tracer->>told
      ;
        true
      ).

etrace(solve_2ndorder_ir_bal_res, [TrQ, Pos, Neg, Sign], _Options):-
	!,
      (
	@tracer->>tell(resolve)
      ->
	e_indent(2, I2),
        translate_qty(TrQ, Q),
        translate_sign(Sign, Effect, Value),
	zero_relation_sign_2ndorder(S, Sign),
	write(I2),
	writef('balance result: %w %w %w\n', [Pos, S, Neg]),
        writef('%w resulting effect: %w = %w\n', [Effect, Q, Value]), %tempflo: critical??? no...
        @tracer->>told
      ;
        true
      ).

etrace(solve_ir_bal_unres, [TrQ, Pos, Neg, Sign], _Options):-
	!,
      (
        @tracer->>tell(resolve)
      ->
	translate_qty(TrQ, Q),
	e_indent(1, I1),
	write(I1),
	writef('Relation between: %w and %w is unknown\n', [Pos, Neg]),
        @tracer->>told
      ;
	true
      ),
      (
        @tracer->>tell(resolve, general) %tempflo: critical
      ->
	translate_qty(TrQ, Q),
        writef('Resulting effect on %w is ambiguous. possibilities are: ', [Q]),
        write_signs(Sign),
	nl,
	@tracer->>told
      ;
	true
      ),
      etrace(branchepoint, resolve).   %tempflo: critical but branchepoint is critical in general... :).


etrace(solve_ir_amb_nth, [TrQ, N, Sign], Options):-
	!,
      (
	@tracer->>tell(Options, general)  %tempflo: critical
      ->
        translate_qty(TrQ, Q),
	writef('Taking possibility nr: %w, for ambiguous causal effect: %w = %w', [N, Q, Sign]), nl,
	@tracer->>told,
	!
      ;
	true
      ).


etrace(solve_2ndorder_wrong_assumed_derivative_termination, [TrQ, Sign], _Options):-
	!,
      (
	@tracer->>tell(resolve, general)  %tempflo: critical
      ->
        translate_qty(TrQ, Q),
	writef('\n# State is result of wrongly assumed derivative termination.\n', []),
	writef('Derivative terminated in wrong direction according to current information: \n    %w = %w', [Q, Sign]), nl,
	@tracer->>told,
	etrace(pathstop, resolve),
	!
      ;
	true
      ).




etrace(solve_append_bal_error, _, _Options):-
	!,
	@tracer->>trace('\n\n*** Error: cannot append influence resolution balance\n\n', warning).

etrace(addone_var_error, _, _Options):-
	!,
	@tracer->>trace('\n\n*** Error: variable in list of influences\n\n', warning).

etrace(addone_warning, _, _Options):-
	!,
	@tracer->>trace('\n\n*** Warning: unrecognised item in list of influences\n\n', warning).


/*     initquantity.pl */

etrace(initquantity_error, [G, I, S], _Options):-
	!,
	(
	  @tracer->>tell(warning)
	->
	  writef('\n\n*** Error: no quantity space specified for parameter %w instance %w space %w\n', [G, I, S]),
	  @tracer->>told
	;
	  true
	).




/*	methods.pl */

etrace(methods_start_sim, List, _Options):-
	!,
	etrace(blockstart, general),
	(
	  @tracer->>tell(general)
	->
	  e_indent(1, I),
	  write('Start simulation for scenario:\n'),
	  writef('%w%w\n', [I|List]),  %tempflo: critical
	  @tracer->>told
	;
	  true
	),
	etrace(blockstop, general).

etrace(methods_det_term, List, _Options):-
	!,
        % for 2nd order continuity constraints we need to know where we are in the procedure
	flag(trace_fase, _, termination),
	etrace(blockstart, [termination, general]),
	(
	  @tracer->>tell(termination, general)
	->
	  writef('Determining terminations for state:  %w \n', List),  %tempflo: critical
	  @tracer->>told
	;
	  true
	),
	etrace(blockstop, termination).

etrace(methods_show_terminations, [State, []], termination):-
	!,
	etrace(blockstart, termination),
	format(string(Msg), 'No terminations for state: ~w\n', [State]),
	%tempflo: critical??? no: ordered terminations are... yes, want to know where we are...
	@tracer->>trace(Msg, termination, general),
	etrace(blockstop, termination).

etrace(methods_show_terminations, [State, To], termination):-
	!,
	(
	  @tracer->>trace_on(termination, general)
	->
	  etrace(blockstart, termination),
	  @tracer->>tell(termination, general),
	    writef('Terminations for state: %w\n\n', [State]), %tempflo: critical??? no, too much text, lets see...
	    show_terminations(To),
          @tracer->>told,
	  etrace(blockstop, termination)
        ;
          true
	),
	etrace(nl, [termination, general]). %tempflo: critical

etrace(methods_det_ord, List, _Options):-
	!,
        % for 2nd order continuity constraints we need to know where we are in the procedure
	flag(trace_fase, _, ordering),
	etrace(blockstart, [ordering, general]),
	format(string(Msg), 'Determining ordering for state: ~w\n', List), %tempflo: critical
	@tracer->>trace(Msg, ordering, general),
	etrace(blockstop, ordering).

etrace(methods_show_ord, [State, To], _Options):-
	!,
	(
	  @tracer->>trace_on(ordering)
	->
	  etrace(blockstart, ordering),
	  format(string(Msg), 'DONE: Determining ordering for state: ~w\n', [State]),
	  @tracer->>trace(Msg, ordering),
	  etrace(blockstop, ordering)
	;
	  true
	),
	(
	  @tracer->>trace_on(general, ordering)
	->
	  etrace(blockstart, ordering),
	  @tracer->>tell(ordering, general),
	    write('Ordered terminations:\n\n'), %, [State]),
	    %tempflo: critical ???
	    show_terminations(To),
          @tracer->>told,
	  etrace(blockstop, ordering)
        ;
          true
	).


etrace(methods_do_trans, List, _Options):-
	!,
        % for 2nd order continuity constraints we need to know where we are in the procedure
	flag(trace_fase, _, transition),
	etrace(blockstart, [transition, general]),
	format(string(Msg), 'Working on transition: ~w, for state: ~w\n', List), % critical event
	@tracer->>trace(Msg, transition, general),
	etrace(blockstop, transition),
	@tracer->>finish. % to flush text on tracewindow.

etrace(methods_skip_trans, [I, _Sub, Which, State], _Options):-
	(
	  @tracer->>tell(transition, general) % critical
	->
	  writef('\nFastest Path Heuristic active. Skipping transition: %w, for state: %w\n', [I, State]),
	  @tracer->>told
	;
	  true
	),
	(
	  @tracer->>tell(transition) % not criical
	->
	  writef('Reason: changes are a subset of those in transition: %w\n', [Which]),
	  @tracer->>told
	;
	  true
	),
	@tracer->>finish. % to flush text on tracewindow.

etrace(methods_skip_trans_assumed, [I, _Sub, _Which, State], _Options):-
	(
	  @tracer->>tell(transition, general) % critical
	->
	  writef('\nFastest Path Heuristic active. Skipping transition: %w, for state: %w\n', [I, State]),
	  @tracer->>told
	;
	  true
	),
	(
	  @tracer->>tell(transition) % not criical
	->
	  writef('Reason: assumed transitions.\n', []),
	  @tracer->>told
	;
	  true
	),
	@tracer->>finish. % to flush text on tracewindow.


etrace(methods_value_clash, [V1, V2], _Options):-
	!,
	(
	  @tracer->>tell(warning) % warning
	->
	  e_indent(1, I),
	  write('\n# inconsistent values, no merge possible: \n'),
	  write(I),
	  write_value(V1),
	  nl,
	  write(I),
	  write_value(V2),
	  nl,
	  @tracer->>told
	;
	  true
	).


etrace(methods_set_exo_der_branchepoint, [Par, Type, _, Position, Origin, _, Of], _Options):-
	!,
	(
	  @tracer->>tell(add_relation) % tempflo cat?
	->
	  e_indent(1, I1),
	  translate_qty(Par, Q),
	  translate_exo_type(Type, T),
	  translate_qspace_position(I1, Position, P),
	  translate_origin(I1, Origin, O),
	  writef('\nExogenous derivative: %w, of type: %w\n', [Q, T]),
          writef('%w%w%wCan be set to %w values.', [O, P, I1, Of]),
	  @tracer->>told
	;
	  true
	),
	etrace(branchepoint, add_relation).


etrace(methods_set_exo_der_intobranche, [Par, Type, Rel, Position, Origin, Nr, Of], _Options):-
	!,
	etrace(pathstart, add_relation),
	(
	  @tracer->>tell(add_relation, general) % tempflo critical?
	->
	  e_indent(1, I1),
	  translate_qty(Par, Q),
	  translate_exo_type(Type, T),
	  translate_qspace_position(I1, Position, P),
	  translate_origin(I1, Origin, O),
	  translate_rel(Rel, RList),
	  nl,
	  writef('Taking option %w of %w for exogenous derivative: %w, of type: %w\n', [Nr, Of, Q, T]),
          writef('%w%w%wadd: %w %w %w', [O, P, I1 |RList]),
	  @tracer->>told
	;
	  true
	).


etrace(methods_set_exo_der, [Par, Type, Rel, Position, Origin, _, _], _Options):-
	!,
	(
	  @tracer->>tell(add_relation) % tempflo cat?
	->
	  e_indent(1, I1),
	  translate_qty(Par, Q),
	  translate_exo_type(Type, T),
	  translate_qspace_position(I1, Position, P),
	  translate_origin(I1, Origin, O),
	  translate_rel(Rel, RList),
	  nl,
	  writef('Set exogenous derivative: %w, of type: %w\n', [Q, T]),
          writef('%w%w%wadd: %w %w %w', [O, P, I1 |RList]),
	  @tracer->>told
	;
	  true
	).


etrace(methods_set_exo_2nd_der, [Rel, Par], _Options):-
	!,
	(
	  @tracer->>tell(add_relation) % tempflo cat?
	->
	  e_indent(1, I1),
	  translate_qty(Par, Q),
	  translate_rel(Rel, RList),
          writef('%wSet 2nd order derivative for %w\n%wadd: %w %w %w', [I1, Q, I1 |RList]),
	  @tracer->>told
	;
	  true
	).

etrace(methods_set_exo_3rd_der, [Rel, Par], _Options):-
	!,
	(
	  @tracer->>tell(add_relation) % tempflo cat?
	->
	  e_indent(1, I1),
	  translate_qty(Par, Q),
	  translate_rel(Rel, RList),
          writef('%wSet 3rd order derivative for %w\n%wadd: %w %w %w', [I1, Q, I1 |RList]),
	  @tracer->>told
	;
	  true
	).

etrace(methods_set_exo_der_fail,[_Par, _Type, _, _, _, _, _], _Options):-
	!,
	(
	  @tracer->>tell(add_relation, general) % tempflo critical (cat?)
	->
	  % e_indent(1, I1),
	  % translate_qty(Par, Q),
	  write('# Inconsistent: failed to set exogenous derivative.\n'),
	  @tracer->>told
	;
	  true
	),
	etrace(pathstop, add_relation). % critical by itself


% 1 item list: 2nd-der
etrace(methods_set_exo_der_fail,[_Par], _Options):-
	!,
	(
	  @tracer->>tell(add_relation, general) % tempflo critical (cat?)
	->
	  % e_indent(1, I1),
	  % translate_qty(Par, Q),
	  write('# Inconsistent: failed to set exogenous 2nd order derivative.\n'),
	  @tracer->>told
	;
	  true
	),
	etrace(pathstop, add_relation). % critical by itself


etrace(methods_valuebranchpoint, [Rel, Par, Space, L], _Options):-
	!,
	(
	  @tracer->>tell(add_relation) % tempflo cat?
	->
	  e_indent(1, I1),
	  translate_qty(Par, Q),
	  writef('\nQuantity: %w, is determined by the calculation: \n%w', [Q, I1]),
	  write_relation(Rel),
          writef('\n%wResult is ambiguous. \nTrying all %w values of quantityspace: ', [I1, L]),
	  write_commalist(Space),
	  @tracer->>told
	;
	  true
	),
	etrace(branchepoint, add_relation).


etrace(methods_into_valuebranch, [Rel, Par, Value, Nr, Of], _Options):-
	!,
	etrace(pathstart, add_relation),
	(
	  @tracer->>tell(add_relation, general) % tempflo critical?
	->
	  e_indent(1, I1),
	  %translate_qty(Par, Q),
	  nl,
	  writef('Taking option %w of %w for ambiguous calculation: ', [Nr, Of]),
	  write_relation(Rel),
          writef('\n%w%w = %w \n', [I1, Par, Value]),
	  @tracer->>told
	;
	  true
	).

etrace(methods_valuebranchpoint_unresolved, [Rel, Par], _Options):-
	!,
	(
	  @tracer->>tell(add_relation) % tempflo cat?
	->
	  e_indent(1, I1),
	  translate_qty(Par, Q),
	  writef('\nQuantity: %w, is determined by the calculation: \n%w', [Q, I1]),
	  write_relation(Rel),
          writef('\n%wOne or more calculus quantities are still unknown. \nNo resultvalue branching for %w at this point.', [I1, Q]),
	  @tracer->>told
	;
	  true
	).

etrace(methods_value_branching_start, _, _Options):-
	!,
	etrace(blockstart, add_relation),
	@tracer->>trace('Branching resultvalues for ambiguous calculus relations:\n', add_relation, general), %tempflo: critical
	etrace(blockstop, add_relation).

etrace(methods_value_branching_done, _, _Options):-
	!,
	etrace(blockstart, add_relation),
	@tracer->>trace('DONE: Branching resultvalues for ambiguous calculus relations.\n', add_relation),
	etrace(blockstop, add_relation).


etrace(methods_sinus_zero_in_middle_error, Par, general):-
	!,
	(
	  @tracer->>tell(warning)
	->
	  translate_qty(Par, Q),
	  writef('\n\n *** Error: Derivative of Exogenous Sinusoidal: %w cannot be stable when not in extreme value of quantity space.\n\n', [Q]),
	  @tracer->>told
	;
	  true
	).

etrace(methods_det_exo_start, _, _Options):-
	!,
	etrace(blockstart, add_relation),
	@tracer->>trace('Setting derivatives for exogenous quantities:\n', add_relation, general), %tempflo: critical
	etrace(blockstop, add_relation).

etrace(methods_det_exo_done, _, _Options):-
	!,
	etrace(blockstart, add_relation),
	@tracer->>trace('DONE: Setting derivatives for exogenous quantities.\n', add_relation),
	etrace(blockstop, add_relation).












/*	class.pl */

etrace(class_reverse_hyp_order, List, _Options):-
	!,
	(
	  @tracer->>tell(specification, general) %tempflo: critical, can cause bug.
	->
	  write_main_nl,
	  writef('Conditions of candidate MF: %w \nare consequences of: %w. Order reversed\n', List),
	  @tracer->>told
	;
	  true
	).



etrace(class_add_entities, Instances, _Options):-
	!,
	(
	  @tracer->>tell(specification)
	->
	  e_indent(2,I1),
	  e_indent(3,I2),
	  write_adding_nl,
	  writef('%wadding entities:\n', [I1]),
	  forall(member(instance(E, S), Instances), writef('%wadd: %w, of type: %w\n', [I2,E, S])),
	  @tracer->>told
	;
	  true
	).

etrace(class_add_conf, Instances, _Options):-
	!,
	(
	  @tracer->>tell(specification)
	->
	  e_indent(2,I1),
	  e_indent(3,I2),
	  write_adding_nl,
	  writef('%wadding configurations:\n', [I1]),
	  forall(member(has_attribute(X, Y, Z), Instances), writef('%wadd: %w -- %w -- %w\n', [I2, X, Y, Z])),
	  @tracer->>told
	;
	  true
	).

etrace(class_add_quantities, _, _Options):-
	!,
	(
	  @tracer->>tell(specification)
	->
	  e_indent(2,I1),
	  write_adding_nl,
	  writef('%wadding quantities:\n', [I1]),
	  @tracer->>told
	;
	  true
	).


etrace(class_add_qty_error, List, _Options):-
	!,
	format(string(Msg), '\n\n*** Error: can not add quantity: ~w\n\n', List),
	@tracer->>trace(Msg, specification, warning).

etrace(class_add_implied_rel, _, _Options):-
	!,
	(
	  @tracer->>tell(add_relation)
	->
	  e_indent(3,I),
	  write_extra_adding_nl,
	  writef('%w & adding dependencies implied by quantity:\n', [I]),
	  @tracer->>told
	;
	  true
	).

etrace(class_add_descibing_rel, _, _Options):-
	!,
	(
	  @tracer->>tell(add_relation)
	->
	  e_indent(3,I),
	  write_extra_adding_nl,
	  writef('%w & adding dependencies describing/defining quantity value:\n', [I]),
	  @tracer->>told
	;
	  true
	).

etrace(class_add_relations, _, _Options):-
	!,
	(
	  @tracer->>tell(add_relation)
	->
	  e_indent(2,I),
	  write_adding_nl,
	  writef('%wadding dependencies:\n', [I]),
	  @tracer->>told
	;
	  true
	).

etrace(class_add_cor_error, [List], _Options):-
	!,
	format(string(Msg), '\n\n*** Error: cannot add correspondence: ~w\n\n', List),
	@tracer->>trace(Msg, warning).


etrace(class_inv_corr_error, [S1, S2], _Options):-
	!,
	(
	 @tracer->>tell(warning)
	->
	 writef('\n\n*** Error:	inverted correspondence impossible, %w incompatible with %w quantityspace\n\n', [S1, S2]),
	 @tracer->>told
	;
	 true
	).

etrace(class_add_val_error, [Q, I, S], _Options):-
	!,
	(
	 @tracer->>tell(warning)
	->
	 writef('\n\n*** Error: no relations for quantity: %w, interval: %w and space: %w\n\n', [Q, I, S]),
	 @tracer->>told
	;
	 true
	).


etrace(class_add_values, _, _Options):-
	!,
	(
	  @tracer->>tell(specification)
	->
	  e_indent(2,I),
	  write_adding_nl,
	  writef('%wadding quantity values:\n', [I]),
	  @tracer->>told
	;
	  true
	).

etrace(class_add_val, List, _Options):-
	!,
	(
	  @tracer->>tell(specification)
	->
	  write_item_nl,
	  e_indent(3,I),
	  writef('%wadd: value %w equal to %w\n', [I|List]),
	  @tracer->>told
	;
	  true
	).

etrace(class_add_val_der, List, _Options):-
	!,
	(
	  @tracer->>tell(specification)
	->
	  write_item_nl,
	  e_indent(3,I),
	  writef('%wadd: value %w equal to %w, derivative equal to %w\n', [I|List]),
	  @tracer->>told
	;
	  true
	).

etrace(class_val_not_ok, List, _Options):-
	!,
	(
	  @tracer->>tell(specification)
	->
	  %write_end_nl,
	  e_indent(2,I),
	  writef('%w# quantity value inconsistent: %w = %w\n', [I|List]),
	  @tracer->>told
	;
	  true
	).

etrace(class_check_cond_par_not_found, [H], _Options):-
	!,
	(
	  @tracer->>tell(specification)
	->
	  parameter(H, _, Q, E, _, _, S),
	  e_indent(2,I),
	  writef('%w# quantity not found: %w(%w), quantityspace: %w\n', [I, Q, E, S]),
	  @tracer->>told
	;
	  true
	).


etrace(class_add_modelfragments, _, _Options):-
	!,
	(
	  @tracer->>tell(specification)
	->
	  e_indent(2,I),
	  write_adding_nl,
	  writef('%wadding MFs:\n', [I]),
	  @tracer->>told
	;
	  true
	).


etrace(class_add_mf, MF, _Options):-
	!,
	(
	  @tracer->>tell(specification)
	->
	  write_item_nl,
	  e_indent(3,I),
	  writef('%wadd: %w\n', [I, MF]),
	  @tracer->>told
	;
	  true
	).

etrace(class_check_rel, [Rel], Options):-
	!,
	(
	  @tracer->>tell(Options)
	->
	  write_sec_nl,
	  e_indent(2,I),
	  translate_rel(Rel, List),
	  writef('%wchecking: %w %w %w\n', [I|List]),
	  @tracer->>told
	;
	  true
	).

etrace(class_invalid_rel, [Rel], Options):-
	!,
	(
	  @tracer->>tell(Options)
	->
	  %  write_end_nl,
	  e_indent(2,I),
	  translate_rel(Rel, List),
	  writef('%w# inconsistent: %w %w %w\n', [I|List]),
	  @tracer->>told
	;
	  true
	).

etrace(class_isa_ok, [H], specification):-
	!,
	(
	  @tracer->>tell(specification)
	->
	  e_indent(1,I),
	  e_indent(2,I2),
	  write_end_nl,
	  writef('%wisa-conditions OK\n%wtype: %w\n', [I, I2, H]),
	  @tracer->>told
	;
	  true
	).

etrace(class_isa_uncertain, [H], _Options):-
	!,
	(
	  @tracer->>tell(specification)
	->
	  e_indent(1,I),
	  e_indent(2,I2),
	  write_end_nl,
	  writef('%wisa-conditions uncertain\n%wparent: %w, will be checked first/simultaneously\n', [I, I2, H]),
	  @tracer->>told
	;
	  true
	).

etrace(class_isa_ok_parent, [H], _Options):-
	!,
	(
	  @tracer->>tell(specification)
	->
	  e_indent(1,I),
	  e_indent(2,I2),
	  write_end_nl,
	  writef('%wisa-conditions OK\n%wparent: %w, is active\n', [I, I2, H]),
	  @tracer->>told
	;
	  true
	).

etrace(class_isa_not_ok_parent, [H], _Options):-
	!,
	(
	  @tracer->>tell(specification)
	->
	  e_indent(1,I),
	  e_indent(2,I2),
	  write_end_nl,
	  writef('%w# isa-conditions NOT OK\n%wparent: %w, was rejected\n',  [I, I2, H]),
	  @tracer->>told
	;
	  true
	).

etrace(class_isa_uncertain_parent_found, [H], _Options):-
	!,
	(
	  @tracer->>tell(specification)
	->
	  e_indent(1,I),
	  e_indent(2,I2),
	  write_end_nl,
	  writef('%wisa-conditions uncertain\n%wparent: %w found. (new candidate)\n',  [I, I2, H]),
	  writef('%wparent will be checked first/simultaneously\n', [I2]),
	  @tracer->>told
	;
	  true
	).

etrace(class_isa_parent_not_yet, [Sname], _Options):-
	!,
	(
	  @tracer->>tell(specification, general)
	->
	  e_indent(1,I),
	  %e_indent(2,I2),
	  write_end_nl,
	  writef('%wisa-conditions uncertain, parent must be found first\n', [I]),
	  % tempflo this not critical? good text? see algorithm?
	  % -> with isa bugfix (#43) should be ok, and this clause never used
	  writef('Postpone decision on MF:  %w',  [Sname]), %tempflo: critical
	  @tracer->>told
	;
	  true
	).


etrace(class_conditions_check, _, _Options):-
	!,
	(
	  @tracer->>tell(specification)
	->
	  e_indent(1,I),
	  write_sec_nl,
	  writef('%wchecking conditions\n', [I]),
	  @tracer->>told
	;
	  true
	).

etrace(class_conditions_ok, [_Name], _Options):-
	!,
	(
	  @tracer->>tell(specification)
	->
	  e_indent(1,I),
	  write_end_nl,
	  writef('%wconditions OK\n', [I]), %, for MF: %w\n', [I, Name]),
	  @tracer->>told
	;
	  true
	).

etrace(class_conditions_not_ok, [_Name], _Options):-
	!,
	(
	  @tracer->>tell(specification)
	->
	  e_indent(1,I),
	  write_end_nl,
	  writef('%w# conditions NOT OK\n', [I]), %, for MF: %w\n', [I, Name]),
	  @tracer->>told
	;
	  true
	).

etrace(class_consequenses_check, _, _Options):-
	!,
	(
	  @tracer->>tell(specification)
	->
	  e_indent(1,I),
	  write_sec_nl,
	  writef('%wchecking consequenses\n', [I]),
	  @tracer->>told
	;
	  true
	).

etrace(class_consequenses_ok, [_Name], _Options):-
	!,
	(
	  @tracer->>tell(specification)
	->
	  e_indent(1,I),
	  write_end_nl,
	  writef('%wconsequenses OK\n', [I]), %, for MF: %w\n', [I, Name]),
	  @tracer->>told
	;
	  true
	).

etrace(class_consequenses_not_ok, [_Name], _Options):-
	!,
	(
	  @tracer->>tell(specification)
	->
	  e_indent(1,I),
	  write_end_nl,
	  writef('%w# consequenses NOT OK, Please verify model integrity.\n', [I]), %, for MF: %w\n', [I, Name]),
	  @tracer->>told
	;
	  true
	).

etrace(class_need_ass, [PLevel], _Options):-
	!,
	@tracer->>trace('\n\n', assumptions), % new lines not critical...
        (
	  @tracer->>tell(assumptions, general)  %tempflo: critical
	->
	  writef('Need to make reasoning assumption, currently %w assumptions were made (level %w)\n', [PLevel, PLevel]),
	  @tracer->>told
	;
          true
        ).

etrace(class_possible_ass, [PLevel], _Options):-
	!,
        (
	  @tracer->>tell(assumptions)  %tempflo: critical??
	->
          writef('Possible reasoning assumptions with %w assumptions already made. (level %w):\n\n', [PLevel, PLevel]),
	  @tracer->>told
	;
          true
        ).


etrace(class_list_ass, [_Nr, MF, RAs], _Options):-
	!,
        (
	  @tracer->>tell(assumptions)
	->
          e_indent(1, I),
	  writef('%w, with reasoning assumptions:\n', [MF]),
	  write_ass_list(RAs, I),
	  nl,
	  @tracer->>told
	;
          true
        ).


etrace(class_mf_needs_ass_step_one, [RAs], _Options):-
	!,
        (
	  @tracer->>tell(assumptions, specification)
	->
          e_indent(1, I1),
          e_indent(2, I2),
	  write(I1),
	  write('conditions OK. reasoning assumptions needed. unknown (but consistent) facts:\n'),
	  write_ass_list(RAs, I2),
	  @tracer->>told
	;
          true
        ).

etrace(class_mf_needs_ass_step_two, [MF], _Options):-
	!,
        (
	  @tracer->>tell(assumptions, general) %tempflo: critical
	->
	  writef('Postpone decision on MF: %w\n', [MF]),
	  @tracer->>told
	;
          true
        ).

etrace(class_mf_needs_ass, [MF, RAs], _Options):-
	!,
        (
	  @tracer->>tell(assumptions)
	->
          e_indent(1, I1),
          e_indent(2, I2),
	  write(I1),
	  write('reasoning assumptions needed. unknown (but consistent) facts:\n'),
	  write_ass_list(RAs, I2),
	  @tracer->>told
	;
          true
        ),
        (
	  @tracer->>tell(assumptions, general) %tempflo: critical
	->
	  writef('Postpone decision on MF: %w\n', [MF]),
	  @tracer->>told
	;
          true
        ).


etrace(class_test_ass, [RAs, MF, Level], _Options):-
	!,
	etrace(pathstart, assumptions), %tempflo cat? specification?
        (
	  @tracer->>tell(assumptions, general) %tempflo: critical?? no, accept reject is... yes.. should be clear
	->
            write_main_nl,
	    e_indent(1, I1),
	    e_indent(2, I2),
	    writef('Trying MF: %w, and parents, if present\n', [MF]), % tempflo  weg? and parents etc
	    writef('%wat level %w, with reasoning assumptions:\n', [I1, Level]),
	    write_ass_list(RAs, I2),
	  @tracer->>told
	;
          true
        ).


etrace(class_backtrack, [Level], _Options):-
	!,
        (
	  @tracer->>tell(assumptions, general) %tempflo: critical     ???
	->
	  %nl?
	  % Previous is Level - 1,
          % writef('Returning to point were %w reasoning assumptions were made. \n(backtracking on level: %w)\n', [Previous, Level]),
	  writef('Taking different alternative for reasoning assumption %w. \n(backtracking on level: %w)\n', [Level, Level]),
	  @tracer->>told
	;
          true
        ).

etrace(class_re_check_ass, [MF, _RAs], _Options):-
	!,
        (
	  @tracer->>tell(assumptions)
	->
	    write_main_nl,
            writef('Reconsidering assumable MF: %w \n', [MF]),
	    % write('with previous reasoning assumptions:\n'),
	    % write_ass_list(RAs),
	  @tracer->>told
	;
          true
        ).

etrace(class_ass_still, [MF], _Options):-
	!,
        (
	  @tracer->>tell(assumptions)
	->
	  write_conclusion_nl,
          writef('Still assumable MF: %w \n', [MF]),
	  @tracer->>told
	;
          true
        ).

etrace(class_still_valid_or_assumable, [_MF], _Options):-
	!,
        (
	  @tracer->>tell(assumptions)
	->
	  write_conclusion_nl,
          writef('Not an alternative branch, consistent with current state\n', []),
	  @tracer->>told
	;
          true
        ).

etrace(class_is_alternative_branch, [_MF], _Options):-
	!,
        (
	  @tracer->>tell(assumptions)
	->
	  write_conclusion_nl,
          writef('Assumable MF is alternative branch, inconsistent with current state\n', []),
	  @tracer->>told
	;
          true
        ).

etrace(class_re_check_ass_der, [_MF, RAs], _Options):- % FL class_hypothesis will place 'Accept' statement
	!,
        (
	  @tracer->>tell(assumptions, general) %tempflo: critical
	->
            e_indent(1, I1),
            e_indent(2, I2),
	    write(I1),
	    write('previously unknown facts / reasoning assumptions are now derivable:\n'),
	    write_ass_list(RAs, I2),
	  @tracer->>told
	;
          true
        ).

etrace(class_re_check_ass_inc, [MF, RAs], _Options):-
	!,
        (
	  @tracer->>tell(assumptions) %tempflo: critical ???
	->
            e_indent(1, I1),
            e_indent(2, I2),
	    write(I1),
            write('now inconsistent facts / reasoning assumptions:\n'),
	    write_ass_list(RAs, I2),
	  @tracer->>told
	;
          true
        ),
        (
	  @tracer->>tell(assumptions, general) %tempflo: critical
	->
	    writef('Reject MF: %w \n', [MF]),
	  @tracer->>told
	;
          true
        ).










/*	reclass.pl */

etrace(reclass_scn_start, _, _Options):-
	!,
	etrace(blockstart, specification),
	@tracer->>trace('Adding scenario content:\n', specification, general), %tempflo: critical
	etrace(blockstop, specification).

etrace(reclass_scn_done, _, _Options):-
	!,
	etrace(blockstart, specification),
	@tracer->>trace('DONE: Adding scenario content.\n', specification), %tempflo: critical ???
	etrace(blockstop, specification).

etrace(reclass_class_start, _, _Options):-
	!,
	etrace(blockstart, specification),
	@tracer->>trace('Searching for new MFs:\n', specification, general), %tempflo: critical
	etrace(blockstop, specification).

etrace(reclass_class_done, _, _Options):-
	!,
	etrace(blockstart, specification),
	@tracer->>trace('DONE: Searching for new MFs.\n', specification), %tempflo: critical ???
	etrace(blockstop, specification).

etrace(reclass_ir_start, _, _Options):-
	!,
	etrace(blockstart, resolve),
	@tracer->>trace('Start influence resolution:\n', resolve, general), %tempflo: critical
	etrace(blockstop, resolve).

etrace(reclass_ir_done, _, _Options):-
	!,
	etrace(blockstart, resolve),
	@tracer->>trace('DONE: Influence resolution.\n', resolve), %tempflo: critical ???
	etrace(blockstop, resolve).

etrace(reclass_reclass_start, _, _Options):-
	!,
	etrace(blockstart, respecification),
	@tracer->>trace('Checking MFs active in previous state:\n', respecification, general), %tempflo: critical
	etrace(blockstop, respecification).

etrace(reclass_reclass_done, _, _Options):-
	!,
	etrace(blockstart, respecification),
	@tracer->>trace('DONE: Checking MFs active in previous state.\n', respecification), %tempflo: critical ???
	etrace(blockstop, respecification).



etrace(reclass_apply, To, _Options):-
	!,
        (
	  @tracer->>trace_on(transition, general)
	->
          e_indent(1, I),
          etrace(blockstart, transition),
	  @tracer->>tell(transition, general), %tempflo: critical???
	  write_ln('Applying transition content of terminations:'),
	  write_nl_list(To, I),
	  @tracer->>told,
	  % for 2nd order continuity constraints we need to know where we are in the procedure
	  flag(trace_fase, _, transition),
	  etrace(blockstop, transition)
        ;
          true
        ).

etrace(reclass_apply_done, _, _Options):-
	!,
	etrace(blockstart, transition),
	@tracer->>trace('DONE: Applying transition content.\n', transition), %tempflo: critical ???
	etrace(blockstop, transition).

etrace(reclass_print_der, Cout2, _Options):-
	!,
	(
	  @tracer->>tell(derivable)
	->
          cio_q_d(Cout2, Q, Der),
	  nl,
	  write_ln('Derivable relations:'),
          print_derivable_relations( Der, Q ),
          nl,
          @tracer->>told
        ;
          true
	).


etrace(reclass_trans_to, List, _Options):-
	!,
	etrace(blockstart, transition),
	format(string(Msg), 'Equal existing state found. Transition to state: ~w\n', List),
	@tracer->>trace(Msg, transition, general), %tempflo: critical
	etrace(blockstop, transition),
	@tracer->>finish. % to flush text on tracewindow.

etrace(reclass_make_store, _, _Options):-
	!,
	@tracer->>trace('making store\n', transition).

etrace(reclass_landmark_der, NewLandmarkRels, _Options):-
	!,
	format(string(Msg), 'Landmark relations derived: ~w\n', NewLandmarkRels),
	@tracer->>trace(Msg, transition).


etrace(reclass_show_newstate, NextState, _Options):-
	!,
	(
	  @tracer->>trace_on(transition, general)
	->
	  etrace(blockstart, transition),
	  @tracer->>tell(transition, general), %tempflo: critical
	  writef('No equal existing state found. Transition to new state: %w\n', [NextState]),
	  @tracer->>told
	;
	  true
	),
	@tracer->>finish, % to flush text on tracewindow.
	(
	  @tracer->>trace_on(transition)
	->
	  etrace(nl, transition),
	  @tracer->>tell(transition),
	  state(NextState, SMD), % show_state(NextState), But who needs, the number again and status: interpreted...
	  show_smd(SMD),         % show_state(NextState), that adds nothing...
          @tracer->>told
        ;
          true
	),
	etrace(blockstop, transition).
	% @tracer->>finish. % to flush text on tracewindow. %,
	% etrace(nl, general). %tempflo: critical   newline after state...

etrace(reclass_match_or_add, _, _Options):-
	!,
	etrace(blockstart, transition), %tempflo: critical
	@tracer->>trace('State description finished, comparing state with existing states: \n', transition, general),
	etrace(blockstop, transition). % match or add state


etrace(reclass_equal_state_error, _, _Options):-
	!,
	@tracer->>trace('\n\n*** Error: equal existing states detected\n\n', warning).



etrace(reclass_recheck_unknown_type, H, _Options):- % tempflo: id should end on error
	!,
	format(string(Msg), '\n\n*** Error: re_check_conditions: unknown condition type: ~w\n\n', [H]),
	@tracer->>trace(Msg, warning).

% belongs to old rule based transition style?
etrace(reclass_term_rule_con, State, _Options):-
	!,
	format(string(Msg), 'checking termination-rule continuity for state: ~w\n', [State]),
	@tracer->>trace(Msg, transition).

% belongs to old rule based transition style?
etrace(reclass_term_der_con, [Q, Rel, Der], _Options):-
	!,
	format(string(Msg), 'checking derivative continuity for: ~w ~w ~w\n', [Q, Rel, Der]),
	@tracer->>trace(Msg, transition).

% belongs to old rule based transition style?
etrace(reclass_skip_con, [LMap, R, RMap, Q], _Options):-
	!,
	(
	  @tracer->>tell(transition)
	->
	  (write('Skipping continuity for: '),
           print_relation(relation(LMap, R, RMap), Q),
	   nl),
           @tracer->>told
	;
          true
        ).

etrace(reclass_hyp_child, [Child, Name], _Options):-
	!,
	(
	  @tracer->>tell(respecification)
	->
	  e_indent(1, I),
	  nl,
	  writef('%wcandidate: %w\n', [I, Child]),
	  writef('%wparent: %w\n', [I, Name]),
	  @tracer->>told
	;
	  true
	).

etrace(reclass_det_prev_der_warning, _, _Options):-
	!,
	@tracer->>trace('\n\n*** Warning: determine_previous_derivative/3: continuity relation not found.\n\n', warning).

etrace(reclass_search_children, _, _Options):-
	!,
	etrace(blockstart, respecification), %tempflo: critical
	@tracer->>trace('Searching for child MFs: (candidate children of still active MFs)\n', respecification, general),
	etrace(blockstop, respecification).

etrace(reclass_search_children_done, _, _Options):-
	!,
	etrace(blockstart, respecification), %tempflo: critical ???
	@tracer->>trace('DONE: Searching for children candidate MFs\n', respecification),
	etrace(blockstop, respecification).

etrace(reclass_cio_empty, _, _Options):-
	!,
	etrace(blockstart, general), %tempflo: critical
	@tracer->>trace('State search stopped, inconsistent model: has contradictory consequenses\n', general),
	etrace(blockstop, general).

etrace(reclass_scn_fail, _, _Options):-
	!,
	etrace(blockstart, general), %tempflo: critical
	@tracer->>trace('State search stopped, inconsistent scenario\n', general),
	etrace(blockstop, general).

etrace(reclass_apply_fail, _, _Options):-
	!,
	etrace(blockstart, general), %tempflo: critical
	@tracer->>trace('State search stopped, inconsistent transition content\n', general),
	etrace(blockstop, general).




%%% Transitions.pl

etrace(transitions_epsilon_continuity, [Par, Der, Rel], _Options):-
      !,
      flag(trace_fase, Option, Option),
      (
        Option == ordering % in ordering fase extra continuity constraints are not traced
      ->
        true
      ;
      (	  % in termination and transition fase extra continuity constraints ARE traced
	@tracer->>tell(Option)
      ->
	writef('\nEpsilon continuity constraint for d(%w): \n', [Par]),
	writef('   reason: d(%w) was %w and transition is immediate\n', [Par, Der]),
	write('   constraint: '),
	write_relation(Rel),
	nl,
	@tracer->>told
      ;
        true
      )
      ).

/*
etrace(transitions_2ndorder_continuity, [Par, Der, SoDer, Rel], _Options):-
      !,
      flag(trace_fase, Option, Option),
      (
        Option == ordering % in ordering fase extra continuity constraints are not traced
      ->
        true
      ;
      (	  % in termination and transition fase extra continuity constraints ARE traced
	@tracer->>tell(Option)
      ->
	translate_2ndorder_val2rel(SoDer, SodRel),
        writef('\n2nd order continuity constraint for d(%w): \n', [Par]),
	writef('   reason: d(%w) = %w and dd(%w) %w zero\n', [Par, Der, Par, SodRel]),
	write('   constraint: '),
	write_relation(Rel),
	nl,
	@tracer->>told
      ;
        true
      )
      ).

*/
  etrace(transitions_2ndorder_continuity, [Par, Der, SoDer, Rel], _Options):-
      !,
      (

	@tracer->>tell(resolve, transition)
      ->
	translate_2ndorder_val2rel(SoDer, SodRel),
        writef('\n2nd order continuity constraint for d(%w): \n', [Par]),
	writef('   reasons: \n', []),
	writef('       d(%w)  was: %w\n', [Par, Der]),
        writef('       dd(%w) was: %w zero\n', [Par, SodRel]),
        writef('       causal model has not changed for quantity\n', []),
	write('   constraint: '),
	write_relation(Rel),
	nl,
	@tracer->>told
      ;
        true
      ).

etrace(transitions_3rdorder_continuity, [Par, Der, SoDer, TODer, Rel], _Options):-
      !,
      (

	@tracer->>tell(resolve, transition)
      ->
	translate_2ndorder_val2rel(SoDer, SodRel),
	translate_2ndorder_val2rel(TODer, TodRel),
        writef('\n3rd order continuity constraint for d(%w): \n', [Par]),
	writef('   reasons: \n', []),
	writef('       d(%w)  was: %w\n', [Par, Der]),
        writef('       dd(%w) was: %w zero\n', [Par, SodRel]),
        writef('       ddd(%w) was: %w zero\n', [Par, TodRel]),
        writef('       causal model has not changed for quantity\n', []),
	write('   constraint: '),
	write_relation(Rel),
	nl,
	@tracer->>told
      ;
        true
      ).


etrace(hod_postfilter, [Par, Der, SoDer, TODer, Sign], _Options):-
      !,
      (

	@tracer->>tell(resolve, transition)
      ->
	translate_2ndorder_val2rel(SoDer, SodRel),
	translate_2ndorder_val2rel(TODer, TodRel),
        writef('\n3rd order derivative continuity constraint for d(%w): \n', [Par]),
	writef('   reasons: \n', []),
	writef('       d(%w)  was: %w\n', [Par, Der]),
        writef('       dd(%w) was: %w zero\n', [Par, SodRel]),
        writef('       ddd(%w) was: %w zero\n', [Par, TodRel]),
        writef('       causal model has not changed for quantity\n', []),
	writef('   constraint: d(%w) cannot be: %w', [Par, Sign]),
	nl,
	@tracer->>told
      ;
        true
      ).

etrace(transitions_term_exo_sinus, [Par, Val, Der, To], _Options):-
      !,
      (
	@tracer->>tell(termination)
      ->
	exogenous_name_and_direction(To, sinus, N, D),
	write('\nTermination '),
	write_termination(N),
	write(':\n  '),
	writef('   d(%w) = %w  >>>>  d(%w) = %w\n', [Par, Der, Par, D]),
        qspace(Par, P, _, _),
	parameter(P, _, _, _, _, _, Space),
	writef('   reason: %w = %w which is an extreme value of quantityspace %w \n', [Par, Val, Space]),
        % writef('   because %w = %w (quantityspace: %w)\n', [Par, Val, Space]),
	@tracer->>told
      ;
        true
      ).


etrace(transitions_term_exo_random, [Par, _Val, Der, To], _Options):-
      !,
      (
	@tracer->>tell(termination)
      ->
	exogenous_name_and_direction(To, random, N, D),
	write('\nTermination '),
	write_termination(N),
	write(':\n  '),
	writef('   d(%w) = %w  >>>>  d(%w) = %w\n', [Par, Der, Par, D]),
	@tracer->>told
      ;
        true
      ).

etrace(transitions_term_exo_pos_parabola, [Par, Val, Der, To], _Options):-
      !,
      (
	@tracer->>tell(termination)
      ->
	exogenous_name_and_direction(To, pos_parabola , N, D),
	write('\nTermination '),
	write_termination(N),
	write(':\n  '),
	writef('   d(%w) = %w  >>>>  d(%w) = %w\n', [Par, Der, Par, D]),
        qspace(Par, P, _, _),
	parameter(P, _, _, _, _, _, Space),
	writef('   reason: %w = %w which is the top value of quantityspace %w \n', [Par, Val, Space]),
        % writef('   because %w = %w (quantityspace: %w)\n', [Par, Val, Space]),
	@tracer->>told
      ;
        true
      ).

etrace(transitions_term_exo_neg_parabola, [Par, Val, Der, To], _Options):-
      !,
      (
	@tracer->>tell(termination)
      ->
	exogenous_name_and_direction(To, neg_parabola , N, D),
	write('\nTermination '),
	write_termination(N),
	write(':\n  '),
	writef('   d(%w) = %w  >>>>  d(%w) = %w\n', [Par, Der, Par, D]),
        qspace(Par, P, _, _),
	parameter(P, _, _, _, _, _, Space),
	writef('   reason: %w = %w which is the bottom value of quantityspace %w \n', [Par, Val, Space]),
        % writef('   because %w = %w (quantityspace: %w)\n', [Par, Val, Space]),
	@tracer->>told
      ;
        true
      ).


etrace(transitions_m_e_start, _, _Options):-
	!,
	etrace(nlnl, ordering),
	@tracer->>trace('Checking mutually exclusive terminations:\n', ordering, general).  %tempflo: critical

etrace(transitions_m_e_done, _, _Options):-
	!,
	@tracer->>trace('\nFinished mutually exclusive terminations check\n', ordering).

etrace(transitions_m_e, [C1, C2], _Options):-
      !,
      (
	@tracer->>tell(ordering)
      ->
	write_order_inc_together(C1, C2, 'mutually exclusive terminations'),
        @tracer->>told
      ;
	true
      ).





etrace(transitions_corr_ord, _, _Options):-
	!,
	etrace(nlnl, ordering),
	@tracer->>trace('Checking correspondences:\n', ordering, general).  %tempflo: critical

etrace(transitions_corr_ord_done, _, _Options):-
	!,
	@tracer->>trace('\nFinished correspondence check\n', ordering).


etrace(transitions_math_ord, _, _Options):-
	!,
	etrace(nlnl, ordering),
	@tracer->>trace('Checking mathematical consistency of binary termination subsets:\n', ordering, general).  %tempflo: critical

etrace(transitions_math_ord_done, _, _Options):-
	!,
	@tracer->>trace('\nFinished binairy mathematical consistency check\n', ordering).

etrace(transitions_complex_math_ord, _, _Options):-
	!,
	etrace(nlnl, ordering),
	@tracer->>trace('Checking mathematical consistency of termination subsets using constant & complex equations:\n', ordering, general).  %tempflo: critical

etrace(transitions_complex_math_ord_done, _, _Options):-
	!,
	@tracer->>trace('\nFinished complex mathematical consistency check\n', ordering).

etrace(transitions_crossproduct, _, _Options):-
	!,
	etrace(nlnl, ordering),
	@tracer->>trace('Determining all combinations of terminations:\n', ordering, general).  %tempflo: critical


etrace(transitions_crossproduct_done, [Crossproduct], _Options):-
	!,
      (
	@tracer->>tell(ordering)
      ->
	show_terminations(Crossproduct),
	write('\nFinished determining all combinations of terminations\n'),
        @tracer->>told
      ;
	true
      ).


etrace(transitions_corr_rem_fut, Causes, _Options):-
      !,
      (
	@tracer->>tell(ordering)
      ->
	write_order_remove(Causes, 'active correspondence in future value'),
        @tracer->>told
      ;
	true
      ).

etrace(transitions_corr_rem_act, Causes, _Options):-
      !,
      (
	@tracer->>tell(ordering)
      ->
	write_order_remove(Causes, 'active correspondence in current value'),
        @tracer->>told
      ;
	true
      ).


etrace(transitions_corr_inc_alone_fut, [Causes1, Causes2], _Options):-
      !,
      (
	@tracer->>tell(ordering) -> %changed gp3 0.3
	write_order_inc_alone(Causes1, Causes2, 'active correspondence in future value'),
        @tracer->>told
      ;
	true
      ).


etrace(transitions_corr_inc_alone_act, [Causes1, Causes2], _Options):-
      !,
      (
	@tracer->>tell(ordering)
      ->
	write_order_inc_alone(Causes1, Causes2, 'active correspondence in current value'),
        @tracer->>told
      ;
	true
      ).

etrace(transitions_corr_inc_together, [Causes1, Causes2], _Options):-
      !,
      (
	@tracer->>tell(ordering)
      ->
	write_order_inc_together(Causes1, Causes2, 'active correspondence in future value'),
        @tracer->>told
      ;
	true
      ).

etrace(transitions_eq_corr_rem, Causes, _Options):-
      !,
      (
	@tracer->>tell(ordering)
      ->
	write_order_remove(Causes, 'full correspondence for quantities in dependency'),
        @tracer->>told
      ;
	true
      ).

etrace(transitions_solve_context, [ToTest, ParNames, ValNames, ExtraRels], _Options):-
      !,
      (
        @tracer->>tell(ordering)
      ->
        e_indent(1, I1),
        e_indent(2, I2),
        split_terminations(ToTest, TNames, TConditions),
	write('\n-- Determining mathematical consistency for terminations:\n'),
	write_nl_list(TNames, I2),
	writef('\n%wcurrent values:\n', [I1]),
	write_context_values(ParNames, ValNames, I2),
	writef('\n%wcurrent dependencies:\n', [I1]),
	write_context_conditions(TConditions, I2),
	writef('\n%wconstant dependencies:\n', [I1]),
	write_c_relations(ExtraRels, I2),
        @tracer->>told
      ;
	true
      ).

etrace(transitions_solve_context_done, ToTest, _Options):-
              !,
      (
        @tracer->>tell(ordering)
      ->
        e_indent(2, I2),
        split_terminations(ToTest, TNames, _TConditions),
	write('\n~~ Finished determining mathematical consistency for terminations:\n'),
	write_nl_list(TNames, I2),
	@tracer->>told
      ;
	true
      ).


etrace(transitions_solve_context_test, Causes, _Options):-
      !,
      (
        @tracer->>tell(ordering)
      ->
        e_indent(1, I1),
        e_indent(2, I2),
        writef('\n%wtesting combination:\n', [I1]),
	write_nl_list(Causes, I2),
        @tracer->>told
      ;
	true
      ).


etrace(transitions_solve_context_consistent, _, _Options):-
	!,
	@tracer->>trace('   consistent combination\n', ordering).


etrace(transitions_solve_context_inconsistent, _, _Options):-
	!,
	@tracer->>trace(' # inconsistent combination\n', ordering).













etrace(transitions_epsilon_start, _, _Options):-
	!,
	etrace(nlnl, ordering),
	@tracer->>trace('Applying epsilon ordering:\n', ordering, general). %tempflo: critical


etrace(transitions_epsilon_done, _, _Options):-
	!,
	@tracer->>trace('\nFinished epsilon ordering\n', ordering).


etrace(transitions_epsilon_remove, Causes, _Options):-
        !,
	(
	  @tracer->>tell(ordering)
	->
	  e_indent(1, I1),
	  e_indent(2, I2),
	  writef('%wremoving large epsilon: \n', [I1]),
	  write_nl_list(Causes, I2),
	  nl,
	  @tracer->>told
	;
	  true
	).

etrace(transitions_epsilon_merge_start, _, _Options):-
	 !,
	etrace(nlnl, ordering),
	@tracer->>trace('Applying epsilon merging:\n', ordering, general). %tempflo: critical


etrace(transitions_epsilon_merge_done, _, _Options):-
	!,
	@tracer->>trace('\nFinished epsilon merging\n', ordering).


etrace(transitions_epsilon_merge_remove, Causes, _Options):-
        !,
	(
	  @tracer->>tell(ordering)
	->
	  e_indent(1, I1),
	  e_indent(2, I2),
	  writef('\n%wremoving terminations combination: \n', [I1] ),
	  write_nl_list(Causes, I2),
	  writef('%wreason:\n%wimmediate superset combination present\n', [I1, I2]), % explain this better! tempflo
	  @tracer->>told
	;
	  true
	).


etrace(transitions_split_epsilon, [ToSmallOut, ToAssumedSmall, ToAssumedLarge, ToLargeOut], _Options):-
        !,
	(
	  @tracer->>tell(ordering)
	->
	  % e_indent(1, I1),
	  % e_indent(2, I2),
	  write('\n(1a) Immediate terminations:\n\n'),
	  show_terminations(ToSmallOut),
	  write('\n(1b) Immediate terminations (Assumed):\n\n'),
	  show_terminations(ToAssumedSmall),
	  write('\n(2a) NON-Immediate terminations:\n\n'),
	  show_terminations(ToLargeOut),
          write('\n(2b) NON-Immediate terminations (Assumed):\n\n'),
	  show_terminations(ToAssumedLarge),
          @tracer->>told
	;
	  true
	).

etrace(transtions_trying_immediate, _, _Options):-
        !,
	(
	  @tracer->>tell(ordering)
	->
	  write('\nCompleting ordering procedure for the set of Immediate terminations (1a & 1b):\n'),
          @tracer->>told
	;
	  true
	).

etrace(transtions_empty_immediate, _, _Options):-
        !,
	(
	  @tracer->>tell(ordering)
	->
	  write('\nNo Immediate terminations (1a) found.\nCompleting ordering procedure for the set of NON-Immediate terminations (2a & 2b) and Assumed Immediate Terminations (1b):\n'),
          @tracer->>told
	;
	  true
	).

etrace(transtions_empty_immediate_assumed_immediate, _, _Options):-
        !,
	(
	  @tracer->>tell(ordering)
	->
	  write('\nNo Immediate terminations (1a) found.\nCompleting ordering procedure for the other sets of terminations (1b, 2a, 2b)\n'),
          @tracer->>told
	;
	  true
	).
etrace(transitions_ordering_assumed_immediate, _, _Options):-
        !,
	(
	  @tracer->>tell(ordering)
	->
	  write('\nCompleting ordering procedure for the set of Assumed Immediate terminations (1b):\n'),
          @tracer->>told
	;
	  true
	).


etrace(transitions_ordering_non_immediate, _, _Options):-
        !,
	(
	  @tracer->>tell(ordering)
	->
	  write('\nCompleting ordering procedure for the set of NON-Immediate terminations (2a)\n'),
         @tracer->>told
	;
	  true
	).


etrace(transitions_ordering_assumed_non_immediate, _, _Options):-
        !,
	(
	  @tracer->>tell(ordering)
	->
	  write('\nCompleting ordering procedure for the set of Assumed NON-Immediate terminations (2b)\n'),
         @tracer->>told
	;
	  true
	).


etrace(transtions_trying_non_immediate, _, _Options):-
        !,
	(
	  @tracer->>tell(ordering)
	->
	  write('\nNo valid Immediate terminations set found (1a).\nCompleting ordering procedure for the other sets of terminations (1b, 2a, 2b):\n'),
          @tracer->>told
	;
	  true
	).


etrace(transtions_ordering_empty, _, _Options):-
        !,
	(
	  @tracer->>tell(ordering)
	->
	  e_indent(1, I1),
	  writef('%wNo terminations to be ordered\n', [I1]),
          @tracer->>told
	;
	  true
	).



% GENERAL WARNING CLAUSE
etrace(ID, _, _):-
	format(string(Msg), '\n\n*** Warning: Unknown tracecall by engine: ~w\n\n', [ID]),
	@tracer->>trace(Msg, warning).







write_order_remove(Causes, Reason):-
	e_indent(1, I1),
	e_indent(2, I2),
	writef('\n%wremove termination: \n', [I1]),
	write_nl_list(Causes, I2),
	writef('%wreason:\n%w%w\n', [I1, I2,Reason]).

write_order_inc_alone(Causes1, Causes2, Reason):-
	e_indent(1, I1),
	e_indent(2, I2),
	writef('\n%wtermination: \n', [I1]),
	write_nl_list(Causes1, I2),
	writef('%wshould co-occur with: \n', [I1]),
	write_nl_list(Causes2, I2),
	writef('%wreason:\n%w%w\n', [I1, I2, Reason]).

write_order_inc_together(Causes1, Causes2, Reason):-
	e_indent(1, I1),
	e_indent(2, I2),
	writef('\n%wtermination: \n', [I1]),
	write_nl_list(Causes1, I2),
	writef('%wshould NOT co-occur with: \n', [I1]),
	write_nl_list(Causes2, I2),
	writef('%wreason:\n%w%w\n', [I1, I2, Reason]).


exogenous_name_and_direction(To, Type, Name, Direction):-
	To =.. [_, cause([T1])|_],
	T1 =.. [N, _],
	concat_atom([H|T], '_', N),
	concat_atom([H,Type|T], '_', Name),
	last(T, L),
	!,
	exo_dir(L, Direction).

exo_dir(up, plus).
exo_dir(down, min).
exo_dir(stable, zero).


% value_termination_etrace(+Quantity, +Value, +Der, +Termintion, +NextV,
% +Assumptions, +RelationOld, +RelationNew) once a value termination is
% found for Value, output tracer information Meets is the q-space
% condition, if Assumptions is a nonempty list the Der was assumed if
% RelationOld (&New) is empty no equivalent inequality was present in
% SMD if nonempty then equivalentinequality also terminates

% tracer off
value_termination_etrace(_, _, _, _, _, _, _, _):-
    \+ trace_on(termination),
    !.

% no assumptions made
% no equivalent inequality present
value_termination_etrace(Q, V, D, T, NV, [], [], []):-
    @tracer->>tell(termination), %gp3 0.3
    !,
    write('\nTermination '),
    write_termination(T),
    write(':\n   '),
    writef('%w = %w  >>>>  %w = %w\n', [Q, V, Q, NV]),
    qspace(Q, P, _, _),
    parameter(P, _, _, _, _, _, Space),
    writef('   reason: d(%w) = %w, and %w is next value on quantityspace %w \n', [Q, D, NV, Space]),
    % writef('   reason: d(%w) = %w (quantityspace: %w)\n', [Q, D, Space]),
    % writef('-- no equivalent inequality found\n\n', []),
    @tracer->>told.

% no assumptions made
% equivalent inequality present
value_termination_etrace(Q, V, D, T, NV, [], [par_relations([R1])], [par_relations([R2])]):-
    @tracer->>tell(termination), %gp3 0.3
    !,
    write('\nTermination '),
    write_termination(T),
    write(':\n   '),
    writef('%w = %w  >>>>  %w = %w\n', [Q, V, Q, NV]),
    qspace(Q, P, _, _),
    parameter(P, _, _, _, _, _, Space),
    writef('   reason: d(%w) = %w, and %w is next value quantityspace %w \n', [Q, D, NV, Space]),
    % writef('   reason: d(%w) = %w (quantityspace: %w)\n', [Q, D, Space]),
    write('Added equivalent (dependency) termination '),
    write_dependency_termination(T),
    write(':\n  '),
    write_relation(R1),
    write('  >>>>  '),
    write_relation(R2),
    nl,
    @tracer->>told.

% assumptions made
% no equivalent inequality present
value_termination_etrace(Q, V, D, T, NV, [_|_], [], []):-
    @tracer->>tell(termination), %gp3 0.3
    !,
    write('\nTermination '),
    write_termination(T),
    write(':\n   '),
    writef('%w = %w  >>>>  %w = %w\n', [Q, V, Q, NV]),
    qspace(Q, P, _, _),
    parameter(P, _, _, _, _, _, Space),
    writef('   reason: d(%w) = %w can be assumed, and %w is next value on quantityspace %w \n', [Q, D, NV, Space]), % FL NEW WORD NEEDED!
    % writef('   reason: d(%w) = %w can be assumed (quantityspace %w)\n', [Q, D, Space]),
    % writef('-- no equivalent inequality found\n\n', []),
    @tracer->>told.



% assumptions made
% equivalent inequality present
value_termination_etrace(Q, V, D, T, NV, [_|_], [par_relations([R1])], [par_relations([R2])]):-
    @tracer->>tell(termination), %gp3 0.3
    !,
    write('\nTermination '),
    write_termination(T),
    write(':\n   '),
    writef('%w = %w  >>>>  %w = %w\n', [Q, V, Q, NV]),
    qspace(Q, P, _, _),
    parameter(P, _, _, _, _, _, Space),
    writef('   reason: d(%w) = %w can be assumed, and %w is next value quantityspace %w \n', [Q, D, NV, Space]), % FL NEW WORD NEEDED!
    % writef('   reason: d(%w) = %w can be assumed (quantityspace %w)\n', [Q, D, Space]),
    write('Added equivalent (dependency) termination '),
    write_dependency_termination(T),
    write(':\n  '),
    write_relation(R1),
    write('  >>>>  '),
    write_relation(R2),
    nl,
    @tracer->>told.



% A derivative termination!
%
% can be driven by 2nd or 3rd order derivative,
% indicated by SODTOD var
% DD is driving HOD (higher order derivative)
% so if SODTOD is tod DD = DDD
%
derivative_termination_etrace(Q, D, DD, ND, T, SODTOD):-
    @tracer->>tell(termination), %gp3 0.3
    !,
    write('\nTermination '),
    write_termination(T),
    write(':\n   '),
    writef('d(%w) = %w  >>>>  d(%w) = %w\n', [Q, D, Q, ND]),
    (
      SODTOD = sod
    ->
      writef('   reason: dd(%w) = %w, \n', [Q, DD])
    ;
      SODTOD = tod,
      writef('   reason: dd(%w) = %w, and\n', [Q, zero]),
      writef('           ddd(%w) = %w, \n', [Q, DD])
    ),
    @tracer->>told.

% tracer off
derivative_termination_etrace(_, _, _, _, _, _).


% An assumed derivative termination!
assumed_derivative_termination_etrace(Q, D, DD, ND, T):-
    @tracer->>tell(termination), %gp3 0.3
    !,
    write('\nTermination '),
    write_termination(T),
    write(':\n   '),
    writef('d(%w) = %w  >>>>  d(%w) = %w\n', [Q, D, Q, ND]),
    writef('   reason: ambiguous derivative, dd(%w) = %w can be assumed\n', [Q, DD]),
    @tracer->>told.

% tracer off
assumed_derivative_termination_etrace(_, _, _, _, _).

write_termination(X):-
	concat_atom(List, '_', X),
	concat_atom(List, ' ', Term),
	write(Term).

write_dependency_termination(Cause):-
	memberchk(Cause, [to_point_above, assumed_to_point_above]),
	!,
	write('from smaller to equal').

write_dependency_termination(Cause):-
	memberchk(Cause, [to_point_below, assumed_to_point_below]),
	!,
	write('from greater to equal').

write_dependency_termination(Cause):-
	memberchk(Cause, [to_interval_above, assumed_to_interval_above]),
	!,
	write('from equal to greater').

write_dependency_termination(Cause):-
	memberchk(Cause, [to_interval_below, assumed_to_interval_below]),
	!,
	write('from equal to smaller').





% equality_termination_tracer(+OldRelation, +D_Relation, +NewRelation, +KnowledgeStatus)
% output tracer information for OldRelation
% D_relation is conditions, Status is the reliability of this information it is:
% - known: the derivative relation is derivable therefore termination is mandatory
% - weak_known: only a weak >= or =< derivative relation is known: termination can be assumed
% - unknown: no derivative information: termination can be assumed

% tracer off
equality_termination_tracer(_, _, _, _, _):-
	\+ trace_on(termination),
	!.

% equal derivatives derivable or known
equality_termination_tracer(Relation, D_Relation, _, _, known_equal):-
	@tracer->>tell(termination), %gp3 0.3
	!,
	write('\nNo termination for:\n   '),
	write_relation(Relation),
	write('\n   reason: '),
	write_relation(D_Relation),
	write(' is known or derivable.\n'),
	@tracer->>told.

% known d-relation
equality_termination_tracer(OldRelation, D_Relation, NewRelation, T, known):-
	@tracer->>tell(termination), %gp3 0.3
	!,
	write('\nTermination '),
	write_termination(T),
	write(': \n   '),
	write_relation(OldRelation),
	write('  >>>>  '),
	write_relation(NewRelation),
	nl,
	write('   reason: '),
	write_relation(D_Relation),
	write(' is known or derivable.\n'),
	@tracer->>told.


% known weak d-relation
equality_termination_tracer(OldRelation, D_Relation, NewRelation, T, weak_known):-
	@tracer->>tell(termination), %gp3 0.3
	!,
	write('\nTermination assumed '),
	write_termination(T),
	write(': \n   '),
	write_relation(OldRelation),
	write('  >>>>  '),
	write_relation(NewRelation),
	nl,
	write('   '), % write('  because '),
	write_relation(D_Relation),
	write(' is known or derivable.\n'),
	@tracer->>told.

% unknown d-relation
equality_termination_tracer(OldRelation, D_Relation, NewRelation, T, unknown):-
	@tracer->>tell(termination), %gp3 0.3
	!,
	write('\nTermination assumed '),
	write_termination(T),
	write(': \n   '),
	write_relation(OldRelation),
	write('  >>>>  '),
	write_relation(NewRelation),
	nl,
	write('   '), % write('because '),
	write_relation(D_Relation),
	write(' is consistently assumable.\n'),
	@tracer->>told.




% fl Moved write_addone here from solve.pl

write_addone(X, _, _):- var(X), etrace(addone_var_error, _, warning),!.
write_addone(pos(value(One)), V, I):- writef('%w%w -- I+ --> %w\n', [I, One, V]),!.
write_addone(neg(value(One)), V, I):- writef('%w%w  -- I- --> %w\n', [I, One, V]),!.
write_addone(pos(derivative(One)), V, I):- writef('%w%w  -- P+ --> %w\n', [I, One, V]),!.
write_addone(neg(derivative(One)), V, I):- writef('%w%w  -- P- --> %w\n', [I, One, V]),!.
write_addone(pos_neg_mult(derivative(One)), V, I):- writef('%w%w  -- P* --> %w\n', [I, One, V]),!.
write_addone(pos_neg_diw(derivative(One)), V, I):- writef('%w%w  -- P/ --> %w\n', [I, One, V]),!.
write_addone(One, _, _):- write(One), tab(2), etrace(addone_warning, _, warning), !.


%
write_addone_2nd_order(X, _, _):- var(X), etrace(addone_var_error, _, general),!.
write_addone_2nd_order(pos(derivative(One)), V, I):- writef('%wd(%w)  -- I''+ --> %w\n', [I, One, V]),!.
write_addone_2nd_order(neg(derivative(One)), V, I):- writef('%wd(%w)  -- I''- --> %w\n', [I, One, V]),!.
write_addone_2nd_order(pos(second_derivative(One)), V, I):- writef('%wdd(%w)  -- P''+ --> %w\n', [I, One, V]),!.
write_addone_2nd_order(neg(second_derivative(One)), V, I):- writef('%wdd(%w)  -- P''- --> %w\n', [I, One, V]),!.
write_addone_2nd_order(pos_neg_mult(second_derivative(One)), V, I):- writef('%wdd(%w)  -- P''* --> %w\n', [I, One, V]),!.
write_addone_2nd_order(pos_neg_diw(second_derivative(One)), V, I):- writef('%wdd(%w)  -- P''/ --> %w\n', [I, One, V]),!.
write_addone_2nd_order(One, _, _):- write(One), tab(2), etrace(addone_warning, _, general), !.

write_addone_3rd_order(X, _, _):- var(X), etrace(addone_var_error, _, general),!.
write_addone_3rd_order(pos(second_derivative(One)), V, I):- writef('%wdd(%w)  -- I''''+ --> %w\n', [I, One, V]),!.
write_addone_3rd_order(neg(second_derivative(One)), V, I):- writef('%wdd(%w)  -- I''''- --> %w\n', [I, One, V]),!.
write_addone_3rd_order(pos(third_derivative(One)), V, I):- writef('%wddd(%w)  -- P''''+ --> %w\n', [I, One, V]),!.
write_addone_3rd_order(neg(third_derivative(One)), V, I):- writef('%wddd(%w)  -- P''''- --> %w\n', [I, One, V]),!.
write_addone_3rd_order(pos_neg_mult(third_derivative(One)), V, I):- writef('%wddd(%w)  -- P''''* --> %w\n', [I, One, V]),!.
write_addone_3rd_order(pos_neg_diw(third_derivative(One)), V, I):- writef('%wddd(%w)  -- P''''/ --> %w\n', [I, One, V]),!.
write_addone_3rd_order(One, _, _):- write(One), tab(2), etrace(addone_warning, _, general), !.



translate_sign(pos, 'Positive', plus).
translate_sign(neg, 'Negative', min).
translate_sign(zero, 'Zero/nil', zero). % tempflo nil is ok word?
%last two for second order derivatives
translate_sign(pos_zero, 'Positive or Zero', plus/zero).
translate_sign(neg_zero, 'Negative or Zero', min/zero).
translate_sign(unknown, 'Unknown', unknown).

process_single_influence(pos(Inf), Sign, I, E):-
	translate_qty(Inf, I),
	describe_sign(Sign, E).

process_single_influence(neg(Inf), Sign, I, E):-
	translate_qty(Inf, I),
	inverse_sign(Sign, S),
	describe_sign(S, E).

process_sign_add_item(Qty, Sign, Effect, Indent):-
	translate_qty(Qty, Q),
	describe_sign(Sign, S),
	describe_effect(Effect, E),
	writef('%w%w is %w. Effect is %w\n', [Indent, Q, S, E]).

process_sign_add_item_2ndorder(Qty, Sign, Effect, Indent):-
	translate_qty(Qty, Q),
	describe_sign(Sign, S),
	describe_effect(Effect, E),
	writef('%w%w is %w. Effect is %w\n', [Indent, Q, S, E]).

construct_tracer_balance([], [], []).

construct_tracer_balance([pos(Inf)|T], [I|PT], NT):-
	translate_qty(Inf,In),
	term_to_atom(In, I),
	construct_tracer_balance(T, PT, NT).

construct_tracer_balance([neg(Inf)|T], PT, [I|NT]):-
	translate_qty(Inf,In),
	term_to_atom(In, I),
	construct_tracer_balance(T, PT, NT).

construct_2ndorder_tracer_balance([], [], []).

construct_2ndorder_tracer_balance([pos(Inf)|T], [I|PT], NT):-
	translate_2ndorder_qty(Inf,In),
	term_to_atom(In, I),
	construct_2ndorder_tracer_balance(T, PT, NT).

construct_2ndorder_tracer_balance([neg(Inf)|T], PT, [I|NT]):-
	translate_2ndorder_qty(Inf,In),
	term_to_atom(In, I),
	construct_2ndorder_tracer_balance(T, PT, NT).


describe_sign(neg, 'below zero').
describe_sign(pos, 'above zero').
describe_sign(zero, 'equal to zero').
%last two for second order derivatives
describe_sign(pos_zero, 'above or equal to zero').
describe_sign(neg_zero, 'below or equal to zero').


describe_effect(neg, 'negative').
describe_effect(pos, 'positive').
describe_effect(zero, 'zero/nil').



write_unknow_inf([], _Indent).

write_unknow_inf([H|T], I):-
	H=.. [_,Qty],
	translate_qty(Qty, Q),
	writef('%w%w\n', [I, Q]),
	write_unknow_inf(T, I).


write_2ndorder_unknow_inf([], _Indent).

write_2ndorder_unknow_inf([H|T], I):-
	H=.. [_,Qty],
	translate_qty(Qty, Q),
	writef('%w%w\n', [I, Q]),
	write_2ndorder_unknow_inf(T, I).


/*
write_unknow_inf([I]):-
	I =.. [_,Qty],
	translate_qty(Qty, Q),
	write(Q),
	!.

write_unknow_inf([H|T]):-
	H=.. [_,Qty],
	translate_qty(Qty, Q),
	writef('%w and ', [Q]),
	write_unknow_inf(T).
*/

write_context_values([], [], _Indent).
write_context_values([Q|QT],[V|VT], I):-
	writef('%w%w = %w\n', [I, Q, V]),
	write_context_values(QT, VT, I).

write_context_conditions([], _Indent).
write_context_conditions([par_relations(Rels)|T], I):-
	write_c_relations(Rels, I),
	%nl,
	write_context_conditions(T, I).
% other conditions not important(now?)
write_context_conditions([_S|T], I):-
	% writef('skip %w\n', [S]),
	% nl,
	write_context_conditions(T, I).

write_c_relations([], _Indent).
write_c_relations([H|T], I):-
	write_c_relation(H, I),
	write_c_relations(T, I).

write_c_relation(Rel, I):-
	translate_rel(Rel, List),
	writef('%w%w %w %w\n', [I|List]),true.

% format terminations for tracer: tempflo unused?
format_terminations([], []).

format_terminations([H/_/_|T], [NH|NT]):-
    H =.. [to, cause(NH)|_],
    format_terminations(T, NT).


% proces terminations for tracer:
split_terminations([], [], []).

split_terminations([H/_/_|T], Causes, Conditions):-
    H =.. [to, cause(Ca), conditions(Co)|_],
    split_terminations(T, CAT, COT),
    smerge(Co, COT, Conditions),
    smerge(Ca, CAT, Causes).



/*
translate_2ndorder_val2rel(pos, 'greater then').
translate_2ndorder_val2rel(neg, 'smaller then').
translate_2ndorder_val2rel(zero, 'equal to').
translate_2ndorder_val2rel(pos_zero, 'greater or equal to').
translate_2ndorder_val2rel(neg_zero, 'smaller or equal to ').
translate_2ndorder_val2rel(unknown, '?').
*/
translate_2ndorder_val2rel(pos, >).
translate_2ndorder_val2rel(neg, <).
translate_2ndorder_val2rel(zero, =).
translate_2ndorder_val2rel(pos_zero, >=).
translate_2ndorder_val2rel(neg_zero, =<).
translate_2ndorder_val2rel(unknown, =?=).



translate_exo_type(exogenous_sinus, 'Sinusoidal').
translate_exo_type(exogenous_free, 'Random').
translate_exo_type(exogenous_decreasing, 'Decreasing').
translate_exo_type(exogenous_increasing, 'Increasing').
translate_exo_type(exogenous_steady, 'Steady').
translate_exo_type(exogenous_pos_parabola, 'Parabola (Positive)').
translate_exo_type(exogenous_neg_parabola, 'Parabola (Negative)').

translate_qspace_position(Indent, middle, Atom):-
	atom_concat(Indent, 'Not at extreme value of quantityspace\n', Atom),!.
translate_qspace_position(Indent, extreme, Atom):-
	atom_concat(Indent, 'At extreme value of quantityspace\n', Atom),!.
translate_qspace_position(Indent, highestpoint, Atom):-
	atom_concat(Indent, 'At highest point of quantityspace\n', Atom),!.
translate_qspace_position(Indent, lowestpoint, Atom):-
	atom_concat(Indent, 'At lowest point of quantityspace\n', Atom),!.
%nil
translate_qspace_position(_, _, ''):-!.

/*
translate_origin(Indent, input, Atom):-
	atom_concat(Indent, 'State direct from scenario \n', Atom),!.
*/
%nil
translate_origin(_, _, ''):-!.



%  FL not in use???
translate_relations([], []).

translate_relations([H|T], [NH|NT]):-
	translate_rel(H, NH),
	translate_relations(T, NT).


translate_rel(In, [Msg, '', '']):-
	correspondence_type(In),
	!,
	translate_cor(In, [Msg]).

% extra step needed for additions and subtractions
translate_rel(In, [L, Rel, R]):-
	translate_r(In, [X1, Y, Z1], ValDer),
	translate_part_r(X1, X, CalcX, ValDer),
	translate_part_r(Z1, Z, CalcZ, ValDer),
	order_calculation(X, CalcX, Y, Z, CalcZ, L, Rel, R).

% no further translation of influences and proportionalities
translate_part_r(X, X, _, infprop):-
	!.

translate_part_r(min(X, Y), Str, calc, value):-
	translate_part_r(X, X1, _, value),
	translate_part_r(Y, Y1, _, value),
	format(string(Str), '~w ~w ~w', [X1, -, Y1]), !.

translate_part_r(plus(X, Y), Str, calc, value):-
	translate_part_r(X, X1, _, value),
	translate_part_r(Y, Y1, _, value),
	format(string(Str), '~w ~w ~w', [X1, +, Y1]), !.

translate_part_r(min(X, Y), Str, calc, derivative):-
	translate_part_r(X, X1, _, derivative),
	translate_part_r(Y, Y1, _, derivative),
	format(string(Str), '~w ~w ~w', [X1, -, Y1]), !.

translate_part_r(plus(X, Y), Str, calc, derivative):-
	translate_part_r(X, X1, _, derivative),
	translate_part_r(Y, Y1, _, derivative),
	format(string(Str), '~w ~w ~w', [X1, +, Y1]), !.

% no calculation
translate_part_r(zero, zero, single, derivative):-!.
% no calculation
translate_part_r(X, d(X), single, derivative).

% no calculation
translate_part_r(X, X, single, value).

% no calculation
translate_part_r(zero, zero, single, secondderivative):-!.
% no calculation
translate_part_r(X, dd(X), single, secondderivative).

% no calculation
translate_part_r(zero, zero, single, thirdderivative):-!.
% no calculation
translate_part_r(X, ddd(X), single, thirdderivative).


% write down calculations with a single item on the left
% single on the left: keep order
order_calculation(L, single, Rel, R, _, L, Rel, R):-
	!.
% single on the right with equality
% and not equal to zero: reverse order
order_calculation(L, calc, =, R, single, R, =, L):-
	R \= zero,
	!.

% both calculation: keep order
order_calculation(L, _, Rel, R, _, L, Rel, R).


% normal inequalities
translate_r(greater(X, Y), [X, >, Y], value):-	!.
translate_r(greater_or_equal(X, Y), [X, >=, Y], value):-	!.
translate_r(equal(X, Y), [X, =, Y], value):-	!.
translate_r(smaller_or_equal(X, Y), [X, <=, Y], value):-	!.
translate_r(smaller(X, Y), [X, <, Y], value):-	!.
translate_r(min(X, Y), [X, -, Y], value):-	!.
translate_r(plus(X, Y), [X, -, Y], value):-	!.

% derivatives:
translate_r(d_greater(X, Y), [X, >, Y], derivative):-	!.
translate_r(d_greater_or_equal(X, Y), [X, >=, Y], derivative):-	!.
translate_r(d_equal(X, Y), [X, =, Y], derivative):-	!.
translate_r(d_smaller_or_equal(X, Y), [X, <=, Y], derivative):-	!.
translate_r(d_smaller(X, Y), [X, <, Y], derivative):-	!.

% I & P
translate_r(inf_pos_by(X, Y), [Y,'-- I+ -->',X], infprop):-	!.
translate_r(inf_neg_by(X, Y), [Y,'-- I- -->',X], infprop):-	!.
translate_r(prop_pos(X, Y), [Y,'-- P+ -->',X], infprop):-	!.
translate_r(prop_neg(X, Y), [Y,'-- P- -->',X], infprop):-	!.
translate_r(prop_mult(X, Y), [Y,'-- P* -->',X], infprop):-	!.
translate_r(prop_diw(X, Y), [Y,'-- P/ -->',X], infprop):-	!.

% second_derivatives:
translate_r(dd_greater(X, Y), [X, >, Y], secondderivative):-	!.
translate_r(dd_greater_or_equal(X, Y), [X, >=, Y], secondderivative):-	!.
translate_r(dd_equal(X, Y), [X, =, Y], secondderivative):-	!.
translate_r(dd_smaller_or_equal(X, Y), [X, <=, Y], secondderivative):-	!.
translate_r(dd_smaller(X, Y), [X, <, Y], secondderivative):-	!.

% third_derivatives:
translate_r(ddd_greater(X, Y), [X, >, Y], thirdderivative):-	!.
translate_r(ddd_greater_or_equal(X, Y), [X, >=, Y], thirdderivative):-	!.
translate_r(ddd_equal(X, Y), [X, =, Y], thirdderivative):-	!.
translate_r(ddd_smaller_or_equal(X, Y), [X, <=, Y], thirdderivative):-	!.
translate_r(ddd_smaller(X, Y), [X, <, Y], thirdderivative):-	!.


translate_cor(v_correspondence(X1,Y1,X2,Y2), [Msg]):-
	format(string(Msg), '~w = ~w <--V--> ~w = ~w', [X1, Y1, X2, Y2]).

translate_cor(dir_v_correspondence(X1,Y1,X2,Y2), [Msg]):-
	format(string(Msg), '~w = ~w --V--> ~w = ~w', [X1, Y1, X2, Y2]).

translate_cor(q_correspondence(X1,X2), [Msg]):-
	format(string(Msg), '~w <--Q--> ~w', [X1, X2]).

translate_cor(dir_q_correspondence(X1,X2), [Msg]):-
	format(string(Msg), '~w --Q--> ~w', [X1, X2]).

translate_cor(mirror_q_correspondence(X1,X2), [Msg]):-
	format(string(Msg), '~w <--Q-inv--> ~w', [X1, X2]).

translate_cor(dir_mirror_q_correspondence(X1,X2), [Msg]):-
	format(string(Msg), '~w --Q-inv--> ~w', [X1, X2]).

translate_cor(dv_correspondence(X1,Y1,X2,Y2), [Msg]):-
	format(string(Msg), 'd(~w) = ~w <--dV--> d(~w) = ~w', [X1, Y1, X2, Y2]).

translate_cor(dir_dv_correspondence(X1,Y1,X2,Y2), [Msg]):-
	format(string(Msg), 'd(~w) = ~w --dV--> d(~w) = ~w', [X1, Y1, X2, Y2]).

translate_cor(dq_correspondence(X1,X2), [Msg]):-
	format(string(Msg), 'd(~w) <--dQ--> d(~w)', [X1, X2]).

translate_cor(dir_dq_correspondence(X1,X2), [Msg]):-
	format(string(Msg), 'd(~w) --dQ--> d(~w)', [X1, X2]).

translate_cor(mirror_dq_correspondence(X1,X2), [Msg]):-
	format(string(Msg), 'd(~w) <--dQ-inv--> d(~w)', [X1, X2]).

translate_cor(dir_mirror_dq_correspondence(X1,X2), [Msg]):-
	format(string(Msg), 'd(~w) --dQ-inv--> d(~w)', [X1, X2]).

translate_cor(full_correspondence(X1,X2), [Msg]):-
	format(string(Msg), '~w <--FQ--> ~w', [X1, X2]).

translate_cor(dir_full_correspondence(X1,X2), [Msg]):-
	format(string(Msg), '~w --FQ--> ~w', [X1, X2]).

translate_cor(if(List1,List2), [Msg]):-
	translate_if_list(List1, L1),
	concat_atom(L1, ' & ', X1),
	translate_if_list(List2, L2),
	concat_atom(L2, ' & ', X2),
	format(string(Msg), 'if ~w, then ~w', [X1, X2]).

translate_cor(iff(List1,List2), [Msg]):-
	translate_if_list(List1, L1),
	concat_atom(L1, ' & ', X1),
	translate_if_list(List2, L2),
	concat_atom(L2, ' & ', X2),
	format(string(Msg), 'if ~w, then ~w, and vice versa', [X1, X2]).

translate_if_list([], []).
translate_if_list([H|T], [NH|NT]):-
	translate_rel(H, List),
	list_to_atom(List, AtomList),
	concat_atom(AtomList, ' ', NH),
	translate_if_list(T, NT).

list_to_atom([], []).
list_to_atom([H|T], [NH|NT]):-
	term_to_atom(H, NH),
	list_to_atom(T, NT).

/* all relations: source types.pl


(greater(_, _)).
(greater_or_equal(_, _)).
(equal(_, _)).
(smaller_or_equal(_, _)).
(smaller(_, _)).
(d_greater(_, _)).
(d_greater_or_equal(_, _)).
(d_equal(_, _)).
(d_smaller_or_equal(_, _)).
(d_smaller(_, _)).

(inf_pos_by(_, _)).
(inf_neg_by(_, _)).
(prop_pos(_, _)).
(prop_neg(_, _)).
(v_correspondence(_, _, _, _)).
(dir_v_correspondence(_, _, _, _)).
(q_correspondence(_, _)).
(dir_q_correspondence(_, _)).
(mirror_q_correspondence(_, _)).
(dir_mirror_q_correspondence(_, _)).
(dv_correspondence(_, _, _, _)).
(dir_dv_correspondence(_, _, _, _)).
(dq_correspondence(_, _)).
(dir_dq_correspondence(_, _)).
(mirror_dq_correspondence(_, _)).
(dir_mirror_dq_correspondence(_, _)).
(full_correspondence(_, _)).
(dir_full_correspondence(_, _)).
(if(_, _)).
(iff(_, _)).


*/
