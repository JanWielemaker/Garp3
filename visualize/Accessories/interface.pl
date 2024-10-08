% A simple Interface to query GARP models, which can
% list all instances of the different GARP primitives.
%
% Written by Anders Bouwer, 16 June 1999
%
% Last updated 1 Sept 2005

% This file is used by VisiGarp to access GARP content

% global variable for the language used
language(dutch).
% language(english).



% Different approach: GARP specific - assumes that you know
% the implementation of GARP constructs

% extract(+TypeOfPrimitive,-Names)
% succeeds iff Names is the list of instances of TypeOfPrimitive.
extract(system_structures, Names):-
	findall(Name, engine:system_structures(Name, _Isa, _Conditions, _Givens), Names).


% compare_states_by_type(N1, N2, Type, N1minN2, N2minN1, N1and2)
%
% compares states N1 and N2 on Type, resulting in what's
% in N1 but not in N2, in N2 but not in N1, and what's in both
%
compare_states_by_type(N1, N2, Type, N1minN2, N2minN1, N1andN2):-
	% get description of state N1
	index_state(N1, Type, L1),
        index_state(N2, Type, L2),

	% compare state descriptions
        % the cut is (currently) necessary to prevent
        % backtracking and finding doubles by difference_types
%	my_difference(L1, L2, N1minN2),
%	my_difference(L2, L1, N2minN1),
%	my_difference(L1, N1minN2, N1andN2),!.
	difference_types([Type/L1], [Type/L2], N1minN2),
	difference_types([Type/L2], [Type/L1], N2minN1),
	difference_types([Type/L1], N1minN2, N1andN2),!.




conc([], L, L).

conc([X|L1], L2, [X|L3]):-
	conc(L1, L2, L3).


my_concat(A1-Z1, Z1-Z2, A1-Z2).


% differences(+L1, +L2, -L1minusL2, -L2minusL1, -L1andL2)
% succeeds iff:
% - L1minusL2 contains all elements from L1 which don't occur in L2;
% - L2minusL1 contains all elements from L2 which don't occur in L1;
% - L1andL2 contains all elements from L1 which also occur in L2.
%
differences(L1, L2, L1minL2, L2minL1, L1andL2):-
	my_difference(L1, L2, L1minL2),
	my_difference(L2, L1, L2minL1),
	my_difference(L1, L1minL2, L1andL2).


% my_difference(+L1, +L2, -L1minusL2)
% succeeds iff:
% - L1minusL2 contains all elements from L1 which don't occur in L2.
% slightly different from built-in predicate difference, which
% does not delete all occurrences
my_difference([], _L2 ,[]).


my_difference([X|L1], L2, L):-
%        nonvar(X),
%       member(X, L2), % this also matches with variables in L2
        % this only matches when both variables have the same name
	member(Y, L2),
        X == Y,!,
	my_difference(L1, L2, L).



/*
%my_difference([X|L1], L2, [X|L]):-
%        var(X),!,
%	my_difference(L1, L2, L).
*/


my_difference([X|L1], L2, [X|L]):-
	my_difference(L1, L2, L).


my_var_difference(L1, L2, L):-
	% delete all items which occur both in L1 and L2
	% this version also matches variables occurring in
	% elements of L1 and L2
	findall(X, (member(X, L1), not(member(X, L2))), L).




var_difference(L1,L2, LDiff):-
        atomize_list(L1, S1),
        atomize_list(L2, S2),
	my_difference(S1, S2, LDiff).
	% termify_list(SDiff,LDiff).


termify_list([],[]).

termify_list([X|Rest],[T|TRest]):-
	term_to_atom(T, X),
	termify_list(Rest,TRest).


atomize_list([],[]).

atomize_list([X|Rest],[unk|SRest]):-
        var(X),!,
	% atomize(X, S),
	atomize_list(Rest,SRest).

atomize_list([X|Rest],[X|SRest]):-
	atomize_list(Rest,SRest).




% special treatment of model fragments.
% matching only the names of model fragments is enough,
% otherwise it will try to match the complete instantiation
% of a model fragment, with all its irrelevant details, and
% hence almost never find corresponding
% model fragments
%
difference_types([system_structures/L1|R1],[],[system_structures/SL1|Rest]):-
        simplify_mf_list(L1, SL1),
	difference_types(R1, [], Rest),!.


difference_types([system_structures/L1|R1],[system_structures/L2|R2],[system_structures/D|Rest]):-
        simplify_mf_list(L1, SL1),
	simplify_mf_list(L2, SL2),
        my_difference(SL1, SL2, D),
	difference_types(R1, R2, Rest),!.


difference_types([], [], []).

difference_types(L, [], L).

difference_types([], _L, []).


% this presupposes that both lists are built the same way, with the
% same types in the same order.
difference_types([Type/L1|R1],[Type/L2|R2],[Type/D|Rest]):-
	my_difference(L1, L2, D),
	% var_difference(L1, L2, D),
	difference_types(R1, R2, Rest).


% If one of the lists contains more types than the other,
% the following clauses will skip to the next type
% they both have in common


% skip left for model fragments
difference_types([system_structures/L0|R1],[Type/L2|R2],[system_structures/SL0|Rest]):-
        Type \== system_structures,
        simplify_mf_list(L0, SL0),
        difference_types(R1,[Type/L2|R2],Rest),!.

% skip left
difference_types([MissingType/L0|R1],[Type/L2|R2],[MissingType/L0|Rest]):-
        MissingType \== Type,
        difference_types(R1,[Type/L2|R2],Rest),!.

% skip right
difference_types([Type/L1|R1],[MissingType/_L0|R2], Rest):-
        MissingType \== Type,
        difference_types([Type/L1|R1], R2, Rest).




simplify_mf_list([], []).

simplify_mf_list([system_structures(Name, _Isa, _Cond, _Res)|Rest],
                 [Name|SimplifiedRest]):-
        simplify_mf_list(Rest, SimplifiedRest).




% index_state(Nr, Type, FlatList)
%
% Given a state Nr and a Type, e.g., par_relations,
% this predicate returns a (flattened, if necessary) list of the
% par_relations in that state.
%
% input_system
index_state(Nr, input_system, FlatList):-
	engine:state(Nr, smd(input_system(L), _SE, _P, _PV, _PR, _SS, _ISC)),
	nonvar(L),
	flatten(L, FlatList).

% system_elements
index_state(Nr, system_elements, L):-
	engine:state(Nr, smd(_IS, system_elements(L), _P, _PV, _PR, _SS, _ISC)).

% parameters
index_state(Nr, parameters, L):-
	engine:state(Nr, smd(_IS, _SE, parameters(L), _PV, _PR, _SS, _ISC)).

% par_values
index_state(Nr, par_values, L):-
	engine:state(Nr, smd(_IS, _SE, _P, par_values(L), _PR, _SS, _ISC)).

% par_relations
index_state(Nr, par_relations, L):-
	engine:state(Nr, smd(_IS, _SE, _P, _PV, par_relations(L), _SS, _ISC)).

% system_structures
index_state(Nr, system_structures, L):-
	engine:state(Nr, smd(_IS, _SE, _P, _PV, _PR, system_structures(L), _ISC)).


/*
% Old, slow (but very general) version, which
% does not require any particular ordering
% of the types of information.
%
index_state(Nr, Type, FlatList):-
	engine:state(Nr, SMD),
	SMD =.. [smd|_ArgList],
	member(Term, _ArgList),
	nonvar(Term),
	Term =.. [Type|L],
	nonvar(L),
	flatten(L, FlatList).
*/


% index_state(Nr, causal_effects, FlatList):-
%
% this clause is intended to add extra detail to the GARP
% state information, namely the effect status of the causal
% par-relations.
%
index_state(Nr, causal_effects, FlatList):-
	engine:state(Nr, _SMD),
	findall(effect_status(CausalRel, Direction, EffectStatus),
                effect_status(Nr, CausalRel, Direction, EffectStatus),
                FlatList).


% index_state(Nr, corr_effects, FlatList):-
%
% this clause is intended to add extra detail to the GARP
% state information, namely the effect status of the correspondence
% par-relations.
%
index_state(Nr, corr_effects, FlatList):-
	engine:state(Nr, _SMD),
	findall(corr_effect_status(CorrRel, EffectStatus),
                corr_effect_status(Nr, CorrRel, EffectStatus),
                FlatList).




% for input system (N = 0) there is no separate state-predicate
% in the saved.svst files, but the input system information is
% stored in all other states, so we can just look in state nr.1.
% This is not true - the information there is incomplete!
% Therefore, only look up the name from there, and search for
% the rest in the information recorded by GARP.
%
% Remember, we assume the state information is structured somewhat
% like this:
%
% state(N, smd(_InputSystemName, _SystemElemements, _Parameters,
%	     _ParValues, _ParRelations, _SysStructures,
%	     smd(input_system(Name),
%		 SystemElements, Parameters, ParValues, ParRelations,
%		 SystemStructures,
%		 _X))).
%


%gp3 0.2: rewrite of state 0 version. We use recorded(bound_input_system) to get the bound version of the
%input system (needed because garp does not generate internal naming for parameters etc in input system
%gp3 1.4: we no longer use recorded(bound_input_system), but the since gp3 1.4 asserted scenario_state
%which uses the *right* binding

index_state(0, Type, FlatList):-
	engine:state(1, SMD),
	SMD = smd(InputSystemName, _SystemElemements, _Parameters,
		  _ParValues, _ParRelations, _SysStructures,
		  _INCOMPLETE_SMD_INPUT),
       % recorded(bound_input_system, SMD_INPUT),
       engine:scenario_state(SMD_INPUT),
	SMD_INPUT =.. [smd, InputSystemName|ArgList],
	member(Term, ArgList),
	nonvar(Term),
	Term =.. [Type|L],
	nonvar(L),
	flatten(L, FlatList).

/*
old code:
index_state(0, Type, FlatList):-
	engine:state(1, SMD),
	SMD = smd(InputSystemName, _SystemElemements, _Parameters,
		  _ParValues, _ParRelations, _SysStructures,
		  _INCOMPLETE_SMD_INPUT),
        recorded(input_system_index, SMD_INPUT/_X),
	SMD_INPUT =.. [smd, InputSystemName|ArgList],
	member(Term, ArgList),
	nonvar(Term),
	Term =.. [Type|L],
	nonvar(L),
	flatten(L, FlatList).
*/


index_pred_state(Nr, Type, Pred):-
	index_state(Nr, Type, FlatList),
	member(Pred, FlatList).


% this is meant to find library system structures which are
% not bound to a certain state yet.
%
index_pred_state(-1, system_structures, SystemStructureTerm):-
	recorded(library_index, SystemStructureTerm/_).




% Find individual state transitions:

% to improve graph generation, extract_transitions reverses
% the list, so that the low numbers come up first.
% this is a very ad hoc solution - not very nice!
%
extract_transitions(NrFrom, NrTo):-
	findall(X/Y,
		extract_transition(X, Y),
		ListOfTransitions),
	reverse(ListOfTransitions, RevListOfTransitions), !,
	member(NrFrom/NrTo, RevListOfTransitions).


extract_transition(NrFrom, NrTo):-
	engine:state_from(NrTo, FromList),
	member(NrFrom, FromList).

%%%%%% Optimized version

record_transitions(state_graph):-
        retractall(rec_transition(_A, _B)),
	findall(X/Y,
		extract_transition(X, Y),
		ListOfTransitions),
	sort(ListOfTransitions, SortedListOfTransitions), !,
	forall(member(NrFrom/NrTo, SortedListOfTransitions),
             assertz(rec_transition(NrFrom, NrTo))
        ),
        record_real_transitions(state_graph).


% similar, but without transitions from input
record_real_transitions(state_graph):-
        retractall(rec_real_transition(_A, _B)),
	forall(
             (rec_transition(X, Y),
              X \= input
             ),
             assertz(rec_real_transition(X, Y))
        ).


% Find the details of a state transition:
% Old version, just returns prolog code strings
%
find_transition_details(StrFrom, StrTo, Cause, Conditions, Results, Status):-
	term_to_atom(NrFrom, StrFrom),
	term_to_atom(NrTo, StrTo),
	engine:state_to(NrFrom, ToList),
	transition_list_member(NrTo, ToList,
		     Cause, Conditions, Results, Status).



% Find the details of a state transition:
% New version, extracts only (some? of) the interesting bits
%
find_transition_detailsB(StrFrom, StrTo,
		Quantity, ChangeDirection, QualValFrom, QualValTo):-
	term_to_atom(NrFrom, StrFrom),
	term_to_atom(NrTo, StrTo),
	engine:state_to(NrFrom, ToList),
	transition_list_memberB(NrTo, ToList,
		Quantity, ChangeDirection, QualValFrom, QualValTo).






% NrTo is contained in the statelist of the first element
%
% new version
transition_list_memberB(NrTo, [to(CauseTerm, ConditionsTerm, ResultsTerm,
			   to_state(StateList), _Status)|_RestToList],
		           Quantity, ChangeDirection, ValueFrom, ValueTo):-
		member(NrTo, StateList),

		CauseTerm = cause(CauseList),
                CauseList = [TransitionTerm|_Rest],
		% Is _Rest always empty, or can there be multiple causes?

                TransitionTerm =.. [ChangeDirection, Quantity],

                ConditionsTerm = conditions(ConditionsList),
                member(par_values(CondParValList), ConditionsList),
                member(value(Quantity, _Type, ValueFromTerm, _Der), CondParValList),

                ResultsTerm = results(ResultsList),
                member(par_values(ResParValList), ResultsList),
%                member(par_relations(ResParRelList), ResultsList),
                member(value(Quantity, _Type2, ValueToTerm, _Der2), ResParValList),

		term_to_atom(ValueFromTerm, ValueFrom),
		term_to_atom(ValueToTerm, ValueTo).


% or NrTo is contained in the rest of the list
%
transition_list_memberB(NrTo, [_Head|RestToList],
	           Quantity, ChangeDirection, ValueFrom, ValueTo):-
                transition_list_memberB(NrTo, RestToList,
	           Quantity, ChangeDirection, ValueFrom, ValueTo).






% NrTo is contained in the statelist of the first element
%
% old version
transition_list_member(NrTo, [to(cause(CausesList),
				 conditions(ConditionsList),
				 results(ResultsList),
				 to_state(StateList), Status)|_RestToList],
		             CausesList, ConditionsList, ResultsList, Status):-
		member(NrTo, StateList).


% or NrTo is contained in the rest of the list
%
transition_list_member(NrTo, [_Head|RestToList],
		     Cause, Conditions, Results, Status):-
		transition_list_member(NrTo, RestToList,
		     Cause, Conditions, Results, Status).


% build_str_list(TermList, StrList)
% not necessary anymore
%
% changes a list of terms into a list of strings
%
build_str_list([], []).

build_str_list([Term|RestTermList], [Str|RestStrList]):-
	atomize(Term, Atom),
	% append a newline to each string
	sformat(Str, '~w~n', Atom),
	build_str_list(RestTermList, RestStrList).


% str_list_to_str(StrList, Str)
% not necessary anymore
%
% changes a list of strings into a single string, with
% newlines between elements
%
str_list_to_str([], '').

str_list_to_str([Str|Rest], String):-
	str_list_to_str(Rest, RestStr),
	string_concat(Str, RestStr, String).


%!	state_quantity_value(?State, ?QuantityValues)
%
%	Refacfored from class `quantityValuesDialog`

state_quantity_value(State, qv{predicate:Pred,
			       name:Name,
			       entity:Entity,
			       value:Val,
			       quantity_space:QS,
			       derivative: #{1: Der, 2: SOD, 3: TOD}}) :-
	find_quantity_details(State,Name,Pred,_Type,Entity,_CVal,ValStruct,QS,Der),
	find_2nd_derivative(State,Name, SOD, TOD), % SOD = 2nd der, TOD = 3rd der
	%strip the parameter name of the value struct (if nescessary) [visigarp code]
	term_to_atom(ValTerm,ValStruct),
	strip_atomize(ValTerm,Val).


/* for illustration purposes, this is what a transition predicate looks like:

state_to(4, [to(cause([to_interval_above(number_of1)]),
      conditions([quantity_spaces([meets(number_of1,
      [point(medium(number_of1)), interval(high)])]),
      par_values([value(number_of1, _G2095, medium(number_of1),
      plus)])]),
      results([par_relations([d_greater_or_equal(number_of1, zero)]),
      par_values([value(number_of1, _G2095 , high, _G2125)])]),
      to_state([5]), closed)]).

*/


% For use with the XPCE autograph program:

% find_quantity_details(N, QName, QPred, Type, Entity, RealVal, QualValue)
%
% succeeds iff QPred, Type, Entity, RealValue and QualValue can be found
% corresponding to QName
%
find_quantity_details(N, QuantityName, QPred, Type, Entity, RealVal, QualValue, QSpace, Derivative):-
	find_quantity_entity(N, QuantityName, QPred, Type, QSpace, Entity),
	find_quantity_value(N, QuantityName, RealVal, QualValue, Derivative).


% find_quantity_entity(N, QuantityName, QuantityPred, Type, QSpace, Entity)
%
% succeeds iff there is a predicate
%     QuantityPred(Entity, QuantityName, Type, QSpace) in state N
%
find_quantity_entity(N, QuantityName, QuantityPred, Type, QSpace, Entity):-
	index_pred_state(N, parameters, ParDefTerm),
        ParDefTerm=..[QuantityPred, Entity, QuantityName, Type, QSpace].



% find_quantity_value(N, QuantityName, RealValue, QualValue, Derivative)
%
% succeeds iff there is a predicate
%     value(QuantityName, RealValue, QualValue, Derivative) in state N
%
find_quantity_value(N, QuantityName, RealValue, QualValue, Derivative):-
	index_pred_state(N, par_values, ParValueTerm),
        ParValueTerm = value(_QuantityName0, _RealValue0, _QualValue0, _Derivative0),
        ParValueTerm=..ParValueTermList,
        fill_with_unknown(ParValueTermList, FilledList),
        value(QuantityName, RealValue, QualValue, Derivative) =..FilledList.




%FL April 08. extract second order derivatives for printing...
find_2nd_derivative(State,Name, SOD, TOD):-
	engine:state(State, SMD),
	engine:get_store(SMD, Store),
	engine:store_sod(Store, HOD),
	memberchk(hod_info(Name, _Der, SOD, TOD, _, _, _), HOD),
	!.

find_2nd_derivative(_, _, unknown, unknown).



dd_text(pos, '+').
dd_text(neg, '-').
dd_text(zero, '0').
dd_text(pos_zero, '+/0').
dd_text(neg_zero, '-/0').
dd_text(unknown, '?').
dd_text(nil, '?').


par_2nd_der_history(_Parameter, _IndexCounter, [], []).

par_2nd_der_history(Parameter, Index, [State|Rest], [Index/SOD/TOD|DDList]):-
	find_2nd_derivative(State, Parameter, SOD, TOD),
	NextIndex is Index + 1,
	par_2nd_der_history(Parameter, NextIndex, Rest, DDList).





% find_entity_details(N, EntityName, ...)
%
% succeeds iff ...
%
%find_entity_details(N, EntityName, ...):-
%	index_pred_state(N,




% find_entity_with_quantity(N, EntityName)
%
% succeeds iff Entity is an entity with at least one quantity in state N
%
find_entity_with_quantity(N, EntityName):-
        find_instance(N, Entity, _Type),
        entity_has_quantity(N, Entity),
        atomize(Entity, EntityName).

entity_has_quantity(N, Entity):-
        find_quantity_entity(N, _Q, _QType, _T, _QS, Entity),!.


% show_system_element(N, SystemElementName)
%
% succeeds for every system element named SystemElementName
% in state N
%
show_system_element(N, Name):-
        find_system_element(N, TermName),
        not(atomic(TermName)),
        term_to_atom(TermName, Name).

show_system_element(N, Name):-
        find_system_element(N, Name),
        atomic(Name).


% find_system_element(N, SystemElementName)
%
% succeeds for every system structure named SystemElementName
% in state N
%
find_system_element(N, Name):-
	index_pred_state(N, system_elements, Name).



% find_instance(N, Entity, Type)
%
% succeeds for every system element named Entity of type Type
% in state N
%
find_instance(N, Entity, Type):-
	index_pred_state(N, system_elements, InstanceDef),
	InstanceDef = instance(Entity, Type).


% find_entity_attribute(N, EntityA, Rel, EntityB)
%
% succeeds for every attribute in system elements which
% consists of a relationship Rel between EntityA and EntityB
% in state N
%
find_entity_attribute(N, EntityA, Rel, EntityB):-
	index_pred_state(N, system_elements, AttributeDef),
	AttributeDef = has_attribute(EntityA, Rel, EntityB),
	% this is necessary to prevent returning other attributes
        find_instance(N, EntityA, _TypeA),
	find_instance(N, EntityB, _TypeB).


% find_other_attribute(N, EntityA, Rel, OtherB)
%
% succeeds for every attribute in system elements which
% consists of a relationship Rel between EntityA and something
% called OtherB (which is not an instance) in state N
%
find_other_attribute(N, EntityA, Rel, OtherB):-
	index_pred_state(N, system_elements, AttributeDef),
	AttributeDef = has_attribute(EntityA, Rel, OtherB),
	find_instance(N, EntityA, _TypeA),
	not(find_instance(N, OtherB, _TypeB)).





% show_system_structure(N, SystemStructureName)
%
% succeeds for every system structure named SystemStructureName
% in state N
%
show_system_structure(N, Name):-
        find_system_structure(N, TermName),
        not(atomic(TermName)),
        term_to_atom(TermName, Name).

show_system_structure(N, Name):-
        find_system_structure(N, Name),
        atomic(Name).


% find_system_structure(N, SystemStructureName)
%
% succeeds for every system structure named SystemStructureName
% in state N
%
find_system_structure(N, Name):-
	index_pred_state(N, system_structures,
                 system_structures(Name, _Isa, _Conditions, _Givens)).



% find_system_structure_supertype(N, Name, Supertype)
%
% succeeds for every supertype (always only one?) of the partial model
% called Name, as present in state N
%
find_system_structure_supertype(N, Name, Supertype):-
	index_pred_state(N, system_structures,
	     system_structures(Name, isa(SupertypeList), _Cond, _Givens)),
	member(SupertypeTerm, SupertypeList),
	term_to_atom(SupertypeTerm, Supertype).





% find_system_structure_detail_type(N, Name, ConditionsOrGivens, Type)
%
% finds the types of conditions/givens present in system structure
% called Name as present in state N.  Because conditions and givens
% are structured the same way, this predicate can be used for both
%
find_system_structure_detail_type(N, Name, conditions, ConditionType):-
	find_system_structure_condition_type(N, Name, ConditionType).

find_system_structure_detail_type(N, Name, givens, GivenType):-
	find_system_structure_given_type(N, Name, GivenType).





% find_system_structure_detail(N, Name, ConditionsOrGivens, Type, Detail)
%
% succeeds for every element in the conditions/givens of the partial model
% called Name as present in state N.  Because conditions and givens
% are structured the same way, this predicate can be used for both
%
find_system_structure_detail(N, Name, conditions, ConditionType, Condition):-
	find_system_structure_condition(N, Name, ConditionType, Condition).

find_system_structure_detail(N, Name, givens, GivenType, Given):-
	find_system_structure_given(N, Name, GivenType, Given).



% find_system_structure_condition_type(N, Name, ConditionType)
%
% succeeds for every Condition type in the conditions of the partial model
% called Name, as present in state N. It can be of any of the types
% system elements, parameters, parameter values, parameter relations,
% or system structures. Because we don't want this predicate to succeed
% when the ConditionList is empty, it should contain at least one element
%
find_system_structure_condition_type(N, Name, ConditionType):-
	index_pred_state(N, system_structures,
                system_structures(Name2, _Isa, conditions(CondList), _Givens)),
	term_to_atom(Name, NameAtom),
	term_to_atom(Name2, NameAtom2),
	NameAtom2 = NameAtom,
	member(ConditionListTerm, CondList),
	ConditionListTerm =.. [ConditionType, [_AtLeastOneElement|_Rest]].



% find_system_structure_condition(N, Name, ConditionType, Condition)
%
% succeeds for every Condition element in the conditions of the partial model
% called Name, as present in state N. It can be of any of the types
% system elements, parameters, parameter values, parameter relations,
% or system structures
%
find_system_structure_condition(N, Name, ConditionType, Condition):-
	index_pred_state(N, system_structures,
                system_structures(Name2, _Isa, conditions(CondList), _Givens)),
	term_to_atom(Name, NameAtom),
	term_to_atom(Name2, NameAtom2),
	NameAtom2 = NameAtom,
	member(ConditionListTerm, CondList),
	ConditionListTerm =.. [ConditionType, ConditionListOfTypeX],
	member(ConditionTerm, ConditionListOfTypeX),
	term_to_atom(ConditionTerm, Condition).



% find_system_structure_given_type(N, Name, GivenType)
%
% succeeds for every Given type in the givens of the partial model
% called Name, as present in state N. It can be of any of the types
% system elements, parameters, parameter values, parameter relations,
% or system structures (although the last type should not be used in
% givens in good GARP models). Because we don't want this predicate to succeed
% when the ConditionList is empty, it should contain at least one element
%
find_system_structure_given_type(N, Name, GivenType):-
	index_pred_state(N, system_structures,
                system_structures(Name2, _Isa, _CondList, givens(GivenList))),
	term_to_atom(Name, NameAtom),
	term_to_atom(Name2, NameAtom2),
	NameAtom2 = NameAtom,
	member(GivenListTerm, GivenList),
	GivenListTerm =.. [GivenType, [_AtLeastOneElement|_Rest]].



% find_system_structure_given(N, Name, GivenType, Given)
%
% succeeds for every Given element in the givens of the partial model
% called Name, as present in state N. It can be of any of the types
% system elements, parameters, parameter values, parameter relations,
% or system structures (although the last type should not be used in
% givens in good GARP models)
%
find_system_structure_given(N, Name, GivenType, Given):-
	index_pred_state(N, system_structures,
                system_structures(Name2, _Isa, _CondList, givens(GivenList))),
	term_to_atom(Name, NameAtom),
	term_to_atom(Name2, NameAtom2),
	NameAtom2 = NameAtom,
	member(GivenListTerm, GivenList),
	GivenListTerm =.. [GivenType, GivenListOfTypeX],
	member(GivenTerm, GivenListOfTypeX),
	term_to_atom(GivenTerm, Given).




% find_process_details(N, Process, CondListOfParRelations, GivenListOfParRelations)
%
% finds a process in state N, together with the conditions (usually one
% inequality) that triggered it, and the par_relations that result from
% it (and usually loop back to it).
%
find_process_details(N, Process, CondListOfParRelations, GivenListOfParRelations):-
	% should this be a transitive relation,
	% e.g., should it find fast_growth, when
	% fast_growth isa growth isa process?
	term_to_atom(ProcessTerm, Process),

	find_system_structure_supertype(N, ProcessTerm, process),

	index_pred_state(N, system_structures,
                system_structures(ProcessTerm2, _Isa, conditions(CondList), givens(GivenList))),
	term_to_atom(ProcessTerm, NameAtom),
	term_to_atom(ProcessTerm2, NameAtom2),
	NameAtom2 = NameAtom,
	member(CondListTerm, CondList),
	CondListTerm =.. [par_relations, CondListOfParRelations],

	member(GivenListTerm, GivenList),
	GivenListTerm =.. [par_relations, GivenListOfParRelations].




% finds all system structures in state N relevant to Entity (a system element)
% and sorts them in a list called SystemStructures
%
find_system_structures_for_entity(N, Entity, SystemStructures):-
	findall(SystemStructure,
	find_system_structure_for_entity(N, Entity, SystemStructure),
		UnsortedSystemStructures),
	sort(UnsortedSystemStructures, SystemStructures).



% finds any system structure in state N relevant to Entity (a system element)
%
find_system_structure_for_entity(N, Entity, SystemStructure):-
        find_instance(N, Entity, _T),
	find_system_structure(N, SystemStructure),
	find_system_structure_detail(N, SystemStructure, _CorG,
				     system_elements, _D),
	term_to_atom(_DA, _D2),
	% this can go wrong when you start using entities called 'instance',
	% 'has_attribute', or other reserved phrases
	_DA2 =.. _DL,
	member(Entity, _DL2).


% this may be useful for finding entities which are relevant to a
% particular system structure:
%
% index_pred_state(1,system_structures,B),
% B =.. _BL,
% member(_M, _BL),
% _M=conditions(_CL),
% member(system_elements(_SE), _CL),
% member(instance(I, _T), _SE).









% fill_with_unknown(InputList, FilledList)
%
% succeeds iff FilledList is a list containing the same elements
% already contained in InputList, but with all variables replaced
% by 'unknown' atoms, and with all terms replaced by atoms
%
fill_with_unknown([],[]).

fill_with_unknown([X|RestIn],[?|RestOut]):-
        var(X),
        fill_with_unknown(RestIn, RestOut).

fill_with_unknown([X|RestIn],[X|RestOut]):-
        nonvar(X),
        atomic(X),
        fill_with_unknown(RestIn, RestOut).

fill_with_unknown([X|RestIn],[Y|RestOut]):-
        nonvar(X),
        not(atomic(X)),
        term_to_atom(X,Y),
        fill_with_unknown(RestIn, RestOut).


% find_relation(N, Rel, A, B)
%
% succeeds iff there is a predicate Rel(A,B) in state N,
% e.g., find_relation(2, greater, A, B) may give: A=dead1 B=zero.
%
find_relation(N, RelationName, A, B):-
	index_pred_state(N, par_relations, RelationTerm),
        RelationTerm=..[RelationName, A, B].

% This one is exactly the same, but is necessary while
% autograph requires three free variables in the `generator-term',
% e.g., find_rel(2, greater, RelName, A, B) will succeed
% like the previous version, with RelName=greater.
%
find_rel(N, RelationName, RelationName, A, B):-
	index_pred_state(N, par_relations, RelationTerm),
        RelationTerm=..[RelationName, A, B].

% show_rel(N, RelationName, VisRelationName, X, Y)
% can handle nested variables by using the visualize predicate
%
show_rel(N, RelationName, VisRelationName, X, Y, Dir):-
        find_rel(N, RelationName, RelationName, A, B),
        visualize(RelationName, A, B, VisRelationName, X, Y, Dir).



% show_causal_model(N, Rel, A, B, Dir)
%
% succeeds for every visualization of the corresponding
% predicate in state N.
%
% this clause is for recall, when not instantiated
%
show_causal_model(N, VisualizedRelName, X, Y, Dir):-
        var(VisualizedRelName),
        causal_model(N, RelName, A, B),
        visualize(RelName, A, B, VisualizedRelName, X, Y, Dir).


% this clause is for recognition, when instantiated
% the order of the conditions is switched to
% increase efficiency
show_causal_model(N, VisualizedRelName, X, Y, Dir):-
        nonvar(VisualizedRelName),
        visualize(RelName, A, B, VisualizedRelName, X, Y, Dir),
        causal_model(N, RelName, A, B).


% causal_model(N, Rel, A, B)
%
% succeeds for every predicate Rel(A,B) in state N,
% for which Rel=inf_pos_by/inf_neg_by/prop_pos/prop_neg
%
% this clause is for recall, when uninstantiated
%
causal_model(N, RelationName, A, B):-
        var(RelationName),
	index_pred_state(N, par_relations, RelationTerm),
        RelationTerm=..[RelationName, A, B],
        causal_relation(RelationName).

% this clause is for recognition, when instantiated
%
causal_model(N, RelationName, A, B):-
        nonvar(RelationName),
        causal_relation(RelationName),
        RelationTerm=..[RelationName, A, B],
	index_pred_state(N, par_relations, RelationTerm).


%
% The following relations are considered part of the
% the causal model
%
causal_relation(inf_pos_by).
causal_relation(inf_neg_by).
causal_relation(prop_pos).
causal_relation(prop_neg).
causal_relation(prop_mult).
causal_relation(prop_diw).

%
% The following relations are not really part of the
% causal model, but for some purposes you may want to
% include them anyway.
%
% causal_relation(R):- math_relation(R, _VR).
%causal_relation(greater).
%causal_relation(greater_or_equal).
%causal_relation(equal).
%causal_relation(less_or_equal).
%causal_relation(smaller_or_equal).
%causal_relation(smaller).
%causal_relation(less).



% show_math_model(N, Rel, A, B, Dir)
%
% succeeds for every visualization of the corresponding
% predicate in state N.
%
show_math_model(N, VisualizedRelName, X, Y, Dir):-
        math_model(N, RelName, A, B),
        visualize(RelName, A, B, VisualizedRelName, X, Y, Dir).



% math_model(N, Rel, A, B)
%
% succeeds for every predicate Rel(A,B) in state N,
% for which Rel is a math_relation
%
math_model(N, RelationName, A, B):-
	index_pred_state(N, par_relations, RelationTerm),
        math_relation(RelationName, _),
        RelationTerm=..[RelationName, A, B].


% show_ineq_model(N, Rel, A, B, Dir)
%
% succeeds for every visualization of the corresponding
% predicate in state N.
%
show_ineq_model(N, VisualizedRelName, X, Y, Dir):-
        ineq_model(N, RelName, A, B),
        % this does not always work when X or Y is known while A or B is not.
        % visualize(RelName, A, B, VisualizedRelName, X, Y, Dir),
        % therefore, C and D are used as intermediate variables
        visualize(RelName, A, B, VisualizedRelName, C, D, Dir),
        X = C,
        Y = D.



% ineq_model(N, Rel, A, B)
%
% succeeds for every predicate Rel(A,B) in state N,
% for which Rel is an ineq_relation
%
ineq_model(N, RelationName, A, B):-
        ineq_relation(RelationName, _),
        RelationTerm=..[RelationName, A, B],
	index_pred_state(N, par_relations, RelationTerm).



% ineq_relation(GarpRelation, VisualizedRelation)
%
ineq_relation(smaller, '<').
ineq_relation(smaller_or_equal, '<=').
ineq_relation(equal, '=').
ineq_relation(greater_or_equal, '>=').
ineq_relation(greater, '>').

ineq_relation(d_smaller, 'd<').
ineq_relation(d_smaller_or_equal, 'd<=').
ineq_relation(d_equal, 'd=').
ineq_relation(d_greater_or_equal, 'd>=').
ineq_relation(d_greater, 'd>').

%
% The following relations are considered part of the
% the mathematical model
%
math_relation(R, VR):-
        ineq_relation(R, VR).

math_relation(plus, '+').
math_relation(min, '-').
math_relation(mult, 'x').
math_relation(diw, '/').

% the following are similar to equal relations and are
% therefore included under the mathematical relations
math_relation(v_correspondence, 'V').
math_relation(dir_v_correspondence, 'V^').
math_relation(q_correspondence, 'Q').
math_relation(dir_q_correspondence, 'Q^').

% new dependency types, 22 may 2005
math_relation(dv_correspondence, 'dV').
math_relation(dir_dv_correspondence, 'dV^').
math_relation(dq_correspondence, 'dQ').
math_relation(dir_dq_correspondence, 'dQ^').

math_relation(mirror_q_correspondence, 'mQ').
math_relation(dir_mirror_q_correspondence, 'mQ^').
math_relation(mirror_dq_correspondence, 'mdQ').
math_relation(dir_mirror_dq_correspondence, 'mdQ^').

math_relation(full_correspondence, 'F').
math_relation(dir_full_correspondence, 'F^').

% the following don't seem to exist but are included
% anyway...
%
math_relation(less, '<').
math_relation(less_or_equal, '<=').
math_relation(d_less, 'd<').
math_relation(d_less_or_equal, 'd<=').



% the following relations deal with derivatives:
%
derivative_relation(d_equal, =).
derivative_relation(d_greater, >).
derivative_relation(d_greater_or_equal, >=).
derivative_relation(d_smaller, <).
derivative_relation(d_smaller_or_equal, <=).

derivative_correspondence_relation(dv_correspondence, 'dV').
derivative_correspondence_relation(dir_dv_correspondence, 'dV^').

% inverse math relation
%
inverse_math_relation('=', '=').
inverse_math_relation('<', '>').
inverse_math_relation('<=', '>=').
inverse_math_relation('>', '<').
inverse_math_relation('>=', '<=').

inverse_math_relation('d=', 'd=').
inverse_math_relation('d<', 'd>').
inverse_math_relation('d<=', 'd>=').
inverse_math_relation('d>', 'd<').
inverse_math_relation('d>=', 'd<=').



% show_binary_model(N, Rel, A, B, Dir, Term)
%
% succeeds for every visualization of the corresponding
% predicate in state N.
%

%gp3 1.4 added Term the original term given by the engine. Used in find_in_design stuff

show_binary_model(N, VisualizedRelName, X, Y, Dir, Term):-
        binary_model(N, RelName, A, B, Term),
        visualize(RelName, A, B, VisualizedRelName, X, Y, Dir).


% binary_model(N, RelName, A, B,Term)
%
% succeeds for every binary predicate Rel(A,B) in state N,
% and also for correspondences, which are in essence binary, but
% have four arguments in the GARP format
%
%gp3 1.4 added Term the original term given by the engine. Used in find_in_design stuff

binary_model(N, RelationName, A, B, RelationTerm):-
	index_pred_state(N, par_relations, RelationTerm),
        binary_relation(RelationName),
        RelationTerm=..[RelationName, A, B].

% the correspondence format: Rel(Par1, Val1, Par2, Val2)
% is transformed into: Rel, Val1(Par1), Val2(Par2)
%
binary_model(N, RelationName, A, B, RelationTerm):-
	index_pred_state(N, par_relations, RelationTerm),
        binary_relation(RelationName),
        RelationTerm=..[RelationName, Par1, Val1, Par2, Val2],
        strip_atomize(Val1, SVal1),
        strip_atomize(Val2, SVal2),
        A =.. [SVal1, Par1],
        B =.. [SVal2, Par2].



binary_relation(Rel):-
        causal_relation(Rel).

% math dependencies are not necessarily binary - righthand side may be nested!:
% lefthand side too: e.g., equal(max_plus(height18), max_plus(height19))
%
% and the V, V^, dV, dV^ correspondences formally have 4 arguments!
%
% this is solved by atomizing a combination of quantity and (der) value
%
binary_relation(Rel):-
        math_relation(Rel, _VisRel).

%
% visualize(RelationName, A, B, VisualRelationName, X, Y, Dir)
%
% determines how RelationName(A,B) will be visualized, e.g.,
% visualize(inf_pos_by, number_of1, born1, 'I+', born1, number_of1, second)
% will ensure a visualization like `born1 --(I+)--> number_of1'
%
visualize(inf_pos_by, B, A, 'I+', A, B, second):-!.
visualize(inf_neg_by, B, A, 'I-', A, B, second):-!.
visualize(prop_pos, B, A, 'P+', A, B, second):-!.
visualize(prop_neg, B, A, 'P-', A, B, second):-!.
visualize(prop_mult, B, A, 'P*', A, B, second):-!.
visualize(prop_diw, B, A, 'P/', A, B, second):-!.


% these two are undirected - no arrow!
% the right part may be nested. simple solution: reduce to atom
visualize(equal, A, B, '=', X, Y, none):-!,
         term_to_atom(A, X),
         term_to_atom(B, Y).

visualize(d_equal, A, B, 'd=', X, Y, none):-!,
         term_to_atom(A, X),
         term_to_atom(B, Y).

visualize(smaller, A, B, '<', A, B, second):-!.
%visualize(less, A, B, '<', A, B, second):-!.
visualize(greater, A, B, '>', A, B, second):-!.
visualize(smaller_or_equal, A, B, '<=', A, B, second):-!.
%visualize(less_or_equal, A, B, '<=', A, B, second):-!.
visualize(greater_or_equal, A, B, '>=', A, B, second):-!.

visualize(d_smaller, A, B, 'd<', A, B, second):-!.
%visualize(d_less, A, B, 'd<', A, B, second):-!.
visualize(d_greater, A, B, 'd>', A, B, second):-!.
visualize(d_smaller_or_equal, A, B, 'd<=', A, B, second):-!.
%visualize(d_less_or_equal, A, B, 'd<=', A, B, second):-!.
visualize(d_greater_or_equal, A, B, 'd>=', A, B, second):-!.

% these are undirected - both sides should have an arrow!
% changed 01/09/2005. AB, before, they had no arrow
visualize(q_correspondence, B, A, 'Q', A, B, both):-!.
visualize(dq_correspondence, B, A, 'dQ', A, B, both):-!.
visualize(mirror_q_correspondence, B, A, 'mQ', A, B, both):-!.
visualize(mirror_dq_correspondence, B, A, 'mdQ', A, B, both):-!.
visualize(full_correspondence, B, A, 'F', A, B, both):-!.

% this one actually had four arguments:
% v_correspondence(Par1, Val1, Par2, Val2)
% but this has been condensed into the nested format of two arguments:
% Val1(Par1), Val2(Par2)!
visualize(v_correspondence, B, A, 'V', X, Y, both):-!,
         term_to_atom(A, X),
         term_to_atom(B, Y).

% this one actually had four arguments:
% dv_correspondence(Par1, Val1, Par2, Val2)
% but this has been condensed into the nested format of two arguments:
% Val1(Par1), Val2(Par2)!
visualize(dv_correspondence, B, A, 'dV', X, Y, both):-!,
         term_to_atom(A, X),
         term_to_atom(B, Y).



% this means that parameter B can be determined only when the
% corresponding value(s) of parameter A are known
visualize(dir_q_correspondence, B, A, 'Q^', A, B, second):-!.
visualize(dir_mirror_q_correspondence, B, A, 'mQ^', A, B, second):-!.
visualize(dir_dq_correspondence, B, A, 'dQ^', A, B, second):-!.
visualize(dir_mirror_dq_correspondence, B, A, 'mdQ^', A, B, second):-!.
visualize(dir_full_correspondence, B, A, 'F^', A, B, second):-!.


% this one actually has four arguments!
% dir_v_correspondence(Par1, Val1, Par2, Val2)
% dir_v_correspondence(substance_flow14, zero, flow_area12, zero)
% means that the substance_flow through a pipe will be zero if the
% flow area of the pipe is zero (and not the other way around)
visualize(dir_v_correspondence, B, A, 'V^', X, Y, second):-!,
         term_to_atom(A, X),
         term_to_atom(B, Y).

visualize(dir_dv_correspondence, B, A, 'dV^', X, Y, second):-!,
         term_to_atom(A, X),
         term_to_atom(B, Y).

visualize(MathRelName, A, B, MathRelSymbol, A, B, second):-
	 math_relation(MathRelName, MathRelSymbol).

% visualize derivative values as +, -, 0, or ? (? already taken care of)
%
visualize(plus, '+'):-!.
visualize(min, '-'):-!.
visualize(zero, '0'):-!.
visualize(X, X).

% transform derivative into a string
%
derivative_str(plus, increase):-!.
derivative_str(min, decrease):-!.
derivative_str(zero, be_steady):-!.
derivative_str(_X, be_unknown):-!.

% visualize_formula(+Formula, -String)
%
% given a Formula, it produces a String
% which visualizes the formula
%
% Arg is atomic, or a string with single quotes,
% representing something atomic
visualize_formula(Arg, Str):-
        atomic(Arg),
        term_to_atom(Term, Arg),
        atomic(Term), !,
        atomize(Arg, Str).

% Arg is a string with double quotes,
visualize_formula(Arg, Str):-
        pce_expansion:is_string(Arg),
        term_to_atom(Term, Arg),
        visualize_formula(Term, Str),!.

% Arg is a string representing some structure
visualize_formula(Arg, Str):-
        atomic(Arg),
        term_to_atom(Term, Arg),
        visualize_formula(Term, Str),!.

% visualize formula in readable format
% do something special for derivative relations
visualize_formula(Formula, Str):-
        Formula =.. [Rel, Arg1, Arg2],
        is_quantity(Arg1),
        derivative_relation(Rel, VRel),!, % gives <=> instead of d<=>
        d_visualize_formula(Arg1, V1),
        d_visualize_formula(Arg2, V2),
        swritef(Str, '%w %w %w ', [V1, VRel, V2]),!.


% visualize formula in readable format
visualize_formula(Formula, Str):-
        Formula =.. [Rel, Arg1, Arg2],
        is_quantity(Arg1),!,
        visualize(Rel, Arg1, Arg2, VRel, VArg1, VArg2, _Dir),
        visualize_formula(VArg1, V1),
        visualize_formula(VArg2, V2),
        swritef(Str, '%w %w %w ', [V1, VRel, V2]),!.

% visualize formula in readable format
visualize_formula(Formula, Str):-
        Formula =.. [Rel, Arg1, Arg2],
        % Arg1 is not a quantity!
        visualize(Rel, Arg1, Arg2, VRel, VArg1, VArg2, _Dir),
        nq_visualize_formula(VArg1, V1),
        nq_visualize_formula(VArg2, V2),
        swritef(Str, '%w %w %w ', [V1, VRel, V2]),!.


% strip off the quantity name from values
visualize_formula(Formula, Value):-
        Formula =.. [Value, _Quantity],!.

visualize_formula(Formula, Str):-
        atomize(Formula, Str).

visualize_formula(Arg, Arg):-
        writef('Not able to visualize Arg: %w. \n', [Arg]).


% the LHS of the formula was not a quantity,
% we're now working on the RHS

% Arg is atomic, or a string with single quotes,
% representing something atomic
nq_visualize_formula(Arg, Str):-
        atomic(Arg),
        term_to_atom(Term, Arg),
        atomic(Term), !,
        atomize(Arg, Str).

% Arg is a string with double quotes,
nq_visualize_formula(Arg, Str):-
        pce_expansion:is_string(Arg),
        % writef('Arg is a string: %w. \n', [Arg]),
        term_to_atom(Term, Arg),
        nq_visualize_formula(Term, Str),!.

% Arg is a string representing some structure
nq_visualize_formula(Arg, Str):-
        atomic(Arg),
        term_to_atom(Term, Arg),
        nq_visualize_formula(Term, Str),!.

% the LHS of the formula was not a quantity,
% so don't strip off the quantity name from values
nq_visualize_formula(Arg, Arg):-
        Arg =.. [_Value, _Quantity],!.

nq_visualize_formula(Formula, Str):-
        Formula =.. [Rel, Arg1, Arg2],
        visualize(Rel, Arg1, Arg2, VRel, VArg1, VArg2, _Dir),
        nq_visualize_formula(VArg1, V1),
        nq_visualize_formula(VArg2, V2),
        swritef(Str, '%w %w %w ', [V1, VRel, V2]),!.

nq_visualize_formula(Arg, Str):-
        writef('Not able to nq_visualize Arg: %w. \n', [Arg]),
        atomize(Arg, Str),!.


% For derivative formulas, do something special
% d(quantity1) = DerValue / d(quantity2) / d-formula
d_visualize_formula(Arg, d(Arg)):-
        is_quantity(Arg),!.

% Arg is atomic, or a string with single quotes,
% representing something atomic
d_visualize_formula(Arg, Str):-
        atomic(Arg),
        term_to_atom(Term, Arg),
        atomic(Term), !,
        atomize(Arg, Str).

% Arg is a string with double quotes,
d_visualize_formula(Arg, Str):-
        pce_expansion:is_string(Arg),
        % writef('Arg is a string: %w. \n', [Arg]),
        term_to_atom(Term, Arg),
        d_visualize_formula(Term, Str),!.

% Arg is a string representing some structure
d_visualize_formula(Arg, Str):-
        atomic(Arg),
        term_to_atom(Term, Arg),
        d_visualize_formula(Term, Str),!.

d_visualize_formula(Formula, Str):-
        Formula =.. [Rel, Arg1, Arg2],
        visualize(Rel, Arg1, Arg2, VRel, VArg1, VArg2, _Dir),
        d_visualize_formula(VArg1, V1),
        d_visualize_formula(VArg2, V2),
        swritef(Str, '%w %w %w ', [V1, VRel, V2]),!.

d_visualize_formula(Arg, Str):-
        writef('Not able to d_visualize Arg: %w. \n', [Arg]),
        atomize(Arg, Str),!.


% visualize_effect_status_change(from(Dir1, Status1),
%                                       to(Dir2, Status2), EffStr),
% creates EffStr which textually explains what happens
% when a causal effect changes in status
%
% from Pos/Neg effect to inactive/submissive/balanced
visualize_effect_status_change(from(Dir1, effect),
                                       to(_Dir2, Status2), EffStr):-
        visualize_effect_dir(Dir1, DirStr),
        visualize_effect_status(Status2, StatusStr),
        swritef(EffStr, '%w effect becomes %w ', [DirStr, StatusStr]),!.

% from inactive/submissive/balanced to Pos/Neg effect
visualize_effect_status_change(from(_Dir1, Status1),
                                       to(Dir2, effect), EffStr):-
        visualize_effect_dir(Dir2, DirStr),
        visualize_effect_status(Status1, StatusStr),
        swritef(EffStr, '%w becomes %w effect ', [StatusStr, DirStr]),!.

% from inactive to submissive/balanced
visualize_effect_status_change(from(none, none),
                                       to(Dir2, Status2), EffStr):-
        visualize_effect_dir(Dir2, DirStr2),
        visualize_effect_status(none, StatusStr1),
        visualize_effect_status(Status2, StatusStr2),
        swritef(EffStr, '%w becomes %w %w ', [StatusStr1, DirStr2, StatusStr2]),!.

% from submissive to inactive/balanced
visualize_effect_status_change(from(Dir1, submissive),
                                       to(_Dir2, Status2), EffStr):-
        visualize_effect_dir(Dir1, DirStr1),
        visualize_effect_status(submissive, StatusStr1),
        visualize_effect_status(Status2, StatusStr2),
        swritef(EffStr, '%w %w becomes %w ', [DirStr1, StatusStr1, StatusStr2]),!.


% from balanced to inactive/submissive
visualize_effect_status_change(from(Dir1, balanced),
                                       to(_Dir2, Status2), EffStr):-
        visualize_effect_dir(Dir1, DirStr1),
        visualize_effect_status(balanced, StatusStr1),
        visualize_effect_status(Status2, StatusStr2),
        swritef(EffStr, '%w %w becomes %w ', [DirStr1, StatusStr1, StatusStr2]),!.


% rest case - should not fire!
visualize_effect_status_change(from(Dir1, Status1),
                                       to(Dir2, Status2), EffStr):-
        visualize_effect_dir(Dir1, DirStr1),
        visualize_effect_dir(Dir2, DirStr2),
        visualize_effect_status(Status1, StatusStr1),
        visualize_effect_status(Status2, StatusStr2),
        swritef(EffStr, '%s %s becomes %s %s ',
                   [DirStr1, StatusStr1, DirStr2, StatusStr2]),!.




visualize_effect_dir(pos, positive).
visualize_effect_dir(neg, negative).

visualize_effect_status(none, inactive).
visualize_effect_status(balanced, balanced).
visualize_effect_status(submissive, 'submissive effect').
visualize_effect_status(effect, effect).

% no effect:
% Str = inactive
visualize_effect_dir_status(_Dir, none, inactive).

% effect with Status in a particular direction Dir:
% Str = positive/negative effect/submissive/balanced
visualize_effect_dir_status(Dir, Status, Str):-
        visualize_effect_dir(Dir, DirStr),
        visualize_effect_status(Status, StatusStr),
        swritef(Str, '%w %w ',
                   [DirStr, StatusStr]),!.


% Visualization of structure events
%
% Entity
visualize_system_elements(instance(E, T), Str):-
        swritef(Str, 'E: %w of type %w', [E, T]).
% Relation
visualize_system_elements(has_attribute(E1, R, E2), Str):-
        is_entity(E2), !,
        swritef(Str, 'R: %w %w %w', [E1, R, E2]).
% Attribute
visualize_system_elements(has_attribute(E1, R, A), Str):-
        % A is not an entity
        swritef(Str, 'A: %w %w %w', [E1, R, A]).
% Quantity
visualize_quantity(QuantityTerm, Str):-
        QuantityTerm =.. [_QType, E, Q, _T, QS],
        swritef(Str, 'Q: %w(%w) with domain %w', [Q, E, QS]).



% is_entity(E)
%
% succeeds when P is an entity in one of the states (not 0)
is_entity(E):-
        index_pred_state(N, system_elements, instance(E, _T)),
        N \== 0,!.


% is_quantity(P)
%
% succeeds when P is a quantity in one of the states (not 0)
is_quantity(P):-
        find_quantity_entity(N, P, _GenericName, _QSType, _QS, _E),
        N \== 0,!.



% list the names of all input models in the input system file
list_input_systems(Names):-
	findall(Name,
	engine:smd( input_system( Name ), _, _, _, _, _),
	Names).




















