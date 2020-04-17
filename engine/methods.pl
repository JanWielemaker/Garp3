/*  File:     Methods.pl
    Purpose:  Basic reasoning methods for GARP
    Author:   Martin Reinders & Bert Bredeweg & Floris Linnebank
    Date:     August 1989
    Part-of:  GARP (version 2.0)
    Modified: 30 July 2004

    Copyright (c) 2004, University of Amsterdam. All rights reserved.

*/

% new input model

new_input_model(Name):-
    clean_up,
    initialise_quantity_spaces,
    SMD =.. [ smd, Name, SE, P, V, R, SS ],
    call(SMD),
    % nest input smd (augmented with dummy argument) in overall smd
    ISMD =.. [smd, Name, SE, P, V, R, SS, nil ],
    smd_slots(NSMD, Name, [], [], [], [], [], ISMD),
    Name = input_system( RealName ),
    etrace(methods_start_sim, [RealName], general),
    successor_states(
        to(cause([]), conditions([]), results([]), to_state([]), _),
        input, NSMD, _).
/**/

/*
 * abolish global predicaties state & qspace
 * reset gensym
 *
 */

clean_up:-
    % In DL Wouter posted a bug [438] about number increasing over simulations. Which is exactly what you expect without resetting the number generator. [FL]
    %once((reset_gensym ; true)),
    % But this statement was commented out for years: Reset gensym IS evil.
    % It is a global reset and will most probably cause problems in other number generation uses in the prolog instance (e.g. in OWL generation). [FL]
    % Solution: specifically reset the bases that were previously used:
    reset_simulation_gensym, %uses current asserted info to find bases, must be done before before retracting. [FL]

    retractall(state(_, _)),
    retractall(state_status(_, _)),
    retractall(state_to(_, _)),
    retractall(state_from(_, _)),
    retractall(qspace(_, _, _, _)),
    retractall(state_values(_,_)),% new FL june 07
    flag(state_number, _, 0).

make_new_state(Name, Status, FromStates, ToList, SMD):-
    flag(state_number, Old, Old),
    Name is Old + 1,
    flag(state_number, _, Name),
    assertz(state(Name, SMD) ),
    assertz(state_status(Name, Status)),
    assertz(state_from(Name, FromStates)),
    assertz(state_to(Name, ToList)),
    % new for faster matching: statevalues separate. FL june 07
    smd_slots(SMD, _, _, _, V, _, _, _),
    assertz(state_values(Name, V)),
    !.


/* --------------------------- TERMINATION --------------------------- */

terminate_all :-
    forall(state_status(State, interpreted), termination(State) ).


/*** FL 10-3-2004: Old rule based version USED TO BE ACTIVE UNDER SWITCH ***

% find terminations for a state

termination(State):-
    flag(use_old_transition_rules, Flag, Flag), % New
    algorithm_assumption_flag(Flag, true, use_old_transition_rules),      % New

    state(State, SMD),
    retract(state_status(State, interpreted)),
    retract(state_to(State, [])),

        % Have GARP produce XML comments when running via the internet...
        (  current_prolog_flag( bbwww_garp, true )
        -> xml_comment( 2, ['determining terminations for state ', State], garpinfo)
        ;  etrace(methods_det_term, [State], general)
        ),

    findall(
        to( cause(Causes),
            conditions(Cond),
            results(Results),
            to_state([]),
            terminated),
        (   rule(termination, Causes, condition(Cond),
                        result(Results) ),
            check_rule_conditions(Cond, SMD, [])
        ),
        To),
    assert(state_status(State, terminated)),
    assert(state_to(State, To)),
    etrace(methods_show_terminations, [State, To], termination),
    !.

%*** GARP 2.0 Version below using procedures & inequality reasoning: ***/

% find terminations for a state

termination(State):-
    %flag(use_old_transition_rules, Flag, Flag),
    %algorithm_assumption_flag(Flag, fail, use_old_transition_rules),
    state(State, SMD), retract(state_status(State, interpreted)),
    retract(state_to(State, [])),

        % Have GARP produce XML comments when running via the internet...
        (  current_prolog_flag( bbwww_garp, true )
        -> xml_comment( 2, ['determining terminations for state ', State], garpinfo)
        ;  etrace(methods_det_term, [State], general)
        ),

    findall(To, single_termination(SMD, To), ToList1),

    % extra rule interpreter for domain specific terminations
    flag(extra_termination_interpreter, ETIFlag, ETIFlag),
    (
        algorithm_assumption_flag(ETIFlag, true, extra_termination_interpreter)
        ->
        findall(to( cause(Causes),
                conditions(Cond),
                results(Results),
                to_state([]),
                terminated),
                (
                rule(termination, Causes, condition(Cond),
                     result(Results) ),
                check_rule_conditions(Cond, SMD, [])
                ),
                ToList2)
        ;
        ToList2 = []
    ),

    append(ToList1, ToList2, ToList),
    assert(state_status(State, terminated)),
    assert(state_to(State, ToList)),
    etrace(methods_show_terminations, [State, ToList], termination),
    !.

% END NEW


/* ------------------------ PRECEDENCE ----------------------------- */

order_all :-
    forall(state_status(State, terminated), precedence(State) ).


/*** FL March 2004: old rule based precedence: USED TO BE ACTIVE UNDER SWITCH***

precedence(State):-
    flag(use_old_transition_rules, Flag, Flag), % New
algorithm_assumption_flag(Flag, true, use_old_transition_rules), % New
state(State, SMD), retract(state_status(State, terminated) ),
retract(state_to(State, Terminated) ),

        % Have GARP produce XML comments when running via the internet...
        (  current_prolog_flag( bbwww_garp, true )
        -> xml_comment( 2, ['determining ordering for state ', State], garpinfo)
        ;  etrace(methods_det_ord, [State], ordering)
        ),

    apply_order_rules(Terminated, Ordered, SMD),
    merge_possible_terminations(Ordered, ToFinal1),
    set_to_list_status_ordered(ToFinal1, ToFinal),
    asserta(state_status(State, ordered) ),
    asserta(state_to(State, ToFinal) ),
    etrace(methods_show_ord, [State, ToFinal], ordering),
    !.

%*** New garp 2.0 version with procedures & inequality reasoning below: ***/

precedence(State):-
    %flag(use_old_transition_rules, Flag, Flag),
    %algorithm_assumption_flag(Flag, fail, use_old_transition_rules),
    state(State, SMD), retract(state_status(State, terminated) ),
    retract(state_to(State, Terminated) ),

        % Have GARP produce XML comments when running via the internet...
        (  current_prolog_flag( bbwww_garp, true )
        -> xml_comment( 2, ['determining ordering for state ', State], garpinfo)
        ;  etrace(methods_det_ord, [State], general)
        ),

    do_precedence(SMD, Terminated, ToFinal),

    asserta(state_status(State, ordered) ),
    asserta(state_to(State, ToFinal) ),
    etrace(methods_show_ord, [State, ToFinal], ordering),
    !.

% End New


% when all order rules have been applied set status of each termination
% to ordered.

set_to_list_status_ordered([], []).
set_to_list_status_ordered([to(Cause, Cond, Result, ToStates, terminated)|T1],
           [to(Cause, Cond, Result, ToStates, ordered)|T2]) :-
set_to_list_status_ordered(T1, T2).

apply_order_rules(To1, To2, SMD):-
    rule(precedence, Cause, condition(Conditions), action(Actions)),
    check_rule_conditions(Conditions, SMD, To1),
    tracer(ordering, 'apply: %w', [Cause]),
    apply_order_actions(Cause, Actions, To1, NTo1),
    !,
    apply_order_rules(NTo1, To2, SMD).
apply_order_rules(To, To, _).


apply_order_actions(_, [], To, To).

apply_order_actions(Cause, [ remove(L) | T ], To, NTo):-
    remove_from_tolist(L, To, TTo),     % what to do with Cause?
    (@tracer->>tell(ordering) -> %gp3 0.3
        write_ln('removing: '),
        pr_l_tab(4, L),
        @tracer->>told;
        true
    ),
    apply_order_actions(Cause, T, TTo, NTo).

apply_order_actions(Cause, [ merge(L) | T ], To, NTo):-
    merge_tolist_select(Cause, L, To, TTo, New),
    (trace_on(ordering) ->
        write('merging :'),
        pr_l_tab(4, L)
        ;
        true
    ),
    apply_order_actions(Cause, T, [New|TTo], NTo).

remove_from_tolist([], To, To).
remove_from_tolist([H|T], To, NTo):-
    common_select(To, to(cause(Causes), _, _, _, _Status), TTo),
    member(H, Causes),
    !,
    remove_from_tolist(T, TTo, NTo).

% merge a termination 'Cause' with another termination

merge_tolist_select(Cause, [], To, To,
    to(cause([Cause]), conditions([]), results([]),     to_state([]), _Status) ):- !.

merge_tolist_select(Cause, [H|T], To, NTo,
        to( cause(Causes),
            conditions(Conditions),
            results(Results),
            to_state(States), Status) ) :-
    merge_tolist_select(Cause, T, To, TTo,
        to( cause(Causes2),
            conditions(Conditions2),
            results(Results2),
            to_state(States2), Status)),
    common_select(TTo,
        to( cause(Causes1),
            conditions(Conditions1),
            results(Results1),
            to_state(States1), Status),
        NTo),
    memberchk(H, Causes1),
    smerge(Causes1, Causes2, Causes),
    merge_rule_conditions_givens(Conditions1, Conditions2, Conditions),
    merge_rule_conditions_givens(Results1, Results2, Results),
    smerge(States1, States2, States),
    !.

% merge a list of terminations

merge_tolist([], to(cause([]),
            conditions([]),
            results([]),
            to_state([]), _Status) ):- !.

merge_tolist([
    to( cause(Causes1),
        conditions(Conditions1),
        results(Results1),
        to_state(States1), Status)
    |Tail],
    to( cause(Causes),
        conditions(Conditions),
        results(Results),
        to_state(States), Status) )    :-
    merge_tolist(Tail,
    to( cause(Causes2),
        conditions(Conditions2),
        results(Results2),
        to_state(States2), Status)),
    smerge(Causes1, Causes2, Causes),
    merge_rule_conditions_givens(Conditions1, Conditions2, Conditions),
    merge_rule_conditions_givens(Results1, Results2, Results),
    smerge(States1, States2, States),
    !.


merge_possible_terminations(ToList, Final):-
    findall(To,
        (   a_subset(Subset, ToList),
            merge_tolist(Subset, To)
        ),
        Final).

/* ----------------------- (UNDO) TRANSITION --------------------------- */

close_all:-
    forall(state_status(State, ordered), transition(State) ).

% no terminations for this state

transition(State):-
    state_to(State, []),
    retract(state_status(State, ordered) ),
    assert(state_status(State, closed) ),
    !.

transition(State):-
    transition(State, _),
    fail.

transition(_).

transition(State, Which):-
    flag(fast_path, Flag, Flag),
    algorithm_assumption_flag(Flag, fail, fast_path),
    state(State, SMD),
    state_to(State, ToList),
    !,
    nth1(Which, ToList, to(Cause, Cond, Result, _, ordered)),

        % Have GARP produce XML comments when running via the internet...
        (  current_prolog_flag( bbwww_garp, true )
        -> xml_comment( 2, ['working on transition ', Which, ' of state ', State], garpinfo)
        ;  etrace(methods_do_trans, [Which, State], general)
        ),
    successor_states(to(Cause, Cond, Result, _, ordered), State, SMD, NewTo),
    once((  retract(state_to(State, ToOld  )),
        replace_nth1(Which, ToOld, NewTo, ToNew),
        asserta(state_to(State, ToNew) )
         )),
    (memberchk(to(_, _, _, _, ordered), ToNew) ->
        true
        ;
        retract(state_status(State, _) ),
        assert(state_status(State, closed) )
    ).
% heuristic version:
% test transition with most changes: if valid, leave others
% if mutual exclusive sets exist: no solution for that yet..
% try largest terminations of each set is not easy
transition(State, Which):-
    flag(fast_path, Flag, Flag),
    algorithm_assumption_flag(Flag, true, fast_path),
    state(State, SMD),
    state_to(State, ToList), %(nb length sorted done by ordering decreasing order)
    !,
	nth1(Which, ToList, to(Cause, Cond, Result, _, ordered)), % take each element of this list at a time

	% to check if this to() has not been closed already because of being subset of another succesful to()
	% we get the current tolist (it can be modified by the procedure close_subset_to() before backtracking)
	state_to(State, CurrentTo),
	memberchk(to(Cause, Cond, Result, _, ordered), CurrentTo),


        % Have GARP produce XML comments when running via the internet...
        (  current_prolog_flag( bbwww_garp, true )
        -> xml_comment( 2, ['working on transition ', Which, ' of state ', State], garpinfo)
        ;  etrace(methods_do_trans, [Which, State], general)
        ),

    successor_states(to(Cause, Cond, Result, _, ordered), State, SMD, NewTo),

    once((  retract(state_to(State, ToOld  )),
	    replace_nth1(Which, ToOld, NewTo, ToNew),
	    asserta(state_to(State, ToNew) )
         )),

   (
      NewTo = to(_, _, _, to_state([_|_]), _)
    ->  % succesful statesearch (valid state found): close all other terminations that are a subset
      close_subset_to(State, Which, Cause)
    ;
      true
    ),

    state_to(State, ToLatest),
    (memberchk(to(_, _, _, _, ordered), ToLatest) ->
        true
        ;
        retract(state_status(State, _) ),
        assert(state_status(State, closed) )
    ).


close_subset_to(State, Which, Cause):-
	retract(state_to(State, To)),
	% true / fail switch for agressive fastpath on assumed terminations (FL mar 2012: set to fail!)
	close_subset_to_items(1, To, Cause, ToNew, Which, State, fail),
	asserta(state_to(State, ToNew)),
	!.


close_subset_to_items(_,[], _, [], _, _, _).

% agressive fastpath, assumed termination; close
% FL mar 2012: aggressive approach seems no longer valid now that there
% are more assumed terminations that may be mutually exclusive...
close_subset_to_items(I, [To|T], cause(Super), [NTo|NT], Which, State, true):-
	To = to(cause(Sub), Cond, Result, TS , ordered),
	is_assumed_termination(Sub),
	!,
	NTo = to(cause(Sub), Cond, Result, TS , closed),
	etrace(methods_skip_trans_assumed, [I, Sub, Which, State], general), %TODO update this call! FLjan2012???
	I2 is I + 1,
	close_subset_to_items(I2, T, cause(Super), NT, Which, State, true).


%subset, close
close_subset_to_items(I, [To|T], cause(Super), [NTo|NT], Which, State, AF):-
	To = to(cause(Sub), Cond, Result, TS , ordered),
	subset(Sub, Super),
	!,
	NTo = to(cause(Sub), Cond, Result, TS , closed),
	etrace(methods_skip_trans, [I, Sub, Which, State], general),
	I2 is I + 1,
	close_subset_to_items(I2, T, cause(Super), NT, Which, State, AF).

%no subset, next
close_subset_to_items(I, [To|T], cause(Super), [To|NT], Which, State, AF):-
	I2 is I + 1,
	close_subset_to_items(I2, T, cause(Super), NT, Which, State, AF).


/*
is_assumed_termination(List):-
	memberchk(First, List),
	%just check first in list...
	First =.. [Name, _],
	concat_atom([assumed|_], '_', Name),
	!.
*/

is_assumed_termination(List):-
	memberchk(First, List),
	%just check first in list...
	assumed_small_epsilon_set(Small),
	memberchk(First, Small),
	!.
is_assumed_termination(List):-
	memberchk(First, List),
	%just check first in list...
	assumed_large_epsilon_set(Large),
	memberchk(First, Large),
	!.












undo_transition(State):-
    state_to(State, []),
    retract(state_status(State, closed) ),
    asserta(state_status(State, ordered) ),
    !.

undo_transition(State):-
    undo_transition(State, _),
    fail.

undo_transition(_).

undo_transition(State, Which):-
    state_to(State, ToList),
    !,
    nth1(Which, ToList, to(Cause, Cond, Result, to_state(ToStates), closed)),
    writef('undo transition %w of state %w to states:\n', [Which, State]),
    pr_l_tab(4, ToStates),
    once((  retract(state_to(State, OldTo) ),
        replace_nth1(Which, OldTo,
            to(Cause, Cond, Result, to_state([]), ordered), NewTo),
        asserta(state_to(State, NewTo) )
         )),
    undo_successor_states(ToStates, State).


successor_states(to(Cause, Cond, Res, to_state([]), ordered), From, SMD,
              to(Cause, Cond, Res, to_state(States), closed)):-
    findall(State, depth(to(Cause, Cond, Res, to_state([]), ordered), SMD, State),
        States),
    forall(member(St, States), append_from(St, From)),
    !.


undo_successor_states([], _).
undo_successor_states([H|T], From):-
    state_from(H, FromList),
    common_select(FromList, From, []), % only predecessor: retract state
    !,
    undo_transition(H),         % and all its successors
    retract(state(H, _)),
    retract(state_status(H, _)),
    retract(state_to(H, _)),
    retract(state_from(H, _)),
    retract(state_values(H, _)), % new FL june 07
    undo_successor_states(T, From).

undo_successor_states([H|T], From):-
    state_from(H, FromList),
    common_select(FromList, From, NewFromList),     % more predecessors
    retract(state_from(H, _)),
    asserta(state_from(H, NewFromList)),
    !,
    undo_successor_states(T, From).

% merge state From into from list of successor State

append_from(State, From):-
    retract(state_from(State, Fr)),
    smerge([From], Fr, NFr),
    asserta(state_from(State, NFr)),
    !.

/* -------------------------- remove from smd ----------------------- */

remove_from_smd([], ResultSMD, ResultSMD).

remove_from_smd([ parameters(PList) | Tail ], SMD1, ResultSMD):-
    !,
    smd_slots(SMD1, Name, SE, P, V, R, SS, IS),
    remove_all(PList, P, NP),
    smd_slots(SMD2, Name, SE, NP, V, R, SS, IS),
    remove_from_smd(Tail, SMD2, ResultSMD).

remove_from_smd([ par_values(VList) | Tail ], SMD1, ResultSMD):-
    !,
    smd_slots(SMD1, Name, SE, P, V, R, SS, IS),
    remove_all(VList, V, NV),
    smd_slots(SMD2, Name, SE, P, NV, R, SS, IS),
    remove_from_smd(Tail, SMD2, ResultSMD).

remove_from_smd([ par_relations(RList) | Tail ], SMD1, ResultSMD):-
    !,
    smd_slots(SMD1, Name, SE, P, V, R, SS, IS),
    % get alternative notations for RList, e.g. A > B = B < A
    findall(AR,
        (member(Re, RList), alternative_relation(Re, AR) ), ARS),
    append(ARS, RList, ARList),
    remove_all(ARList, R, NR),
    smd_slots(SMD2, Name, SE, P, V, NR, SS, IS),
    remove_from_smd(Tail, SMD2, ResultSMD).

% system elements are special: if we want to remove
% intance(water1, substance) (a condition of a termination rule)
% then we actually have to remove each instance relation that
% mentions water1 (the most likely is instance(water1, water)).

remove_from_smd([ system_elements(SEList) | Tail ], SMD1, ResultSMD):-
    !,
    smd_slots(SMD1, Name, SE, P, V, R, SS, IS),
    remove_system_elements(SEList, SE, NSE),
    smd_slots(SMD2, Name, NSE, P, V, R, SS, IS),
    remove_from_smd(Tail, SMD2, ResultSMD).

remove_from_smd([ system_structures(SSList) | Tail ], SMD1, ResultSMD):-
    !,
    smd_slots(SMD1, Name, SE, P, V, R, SS, IS),
    remove_all(SSList, SS, NSS),
    smd_slots(SMD2, Name, SE, P, V, R, NSS, IS),
    remove_from_smd(Tail, SMD2, ResultSMD).

% allow action tests etc.

remove_from_smd([ _ | Tail ], SMD, ResultSMD):-
    remove_from_smd(Tail, SMD, ResultSMD).

/* --------------------- add to smd ------------------------------- */

add_to_smd([], ResultSMD, ResultSMD).

add_to_smd([ parameters(PList) | Tail ], SMD1, ResultSMD):-
    !,
    smd_slots(SMD1, Name, SE, P, V, R, SS, IS),
    smerge(PList, P, NP),
    smd_slots(SMD2, Name, SE, NP, V, R, SS, IS),
    add_to_smd(Tail, SMD2, ResultSMD).

add_to_smd([ par_values(VList) | Tail ], SMD1, ResultSMD):-
    !,
    smd_slots(SMD1, Name, SE, P, V, R, SS, IS),
    merge_values(VList, V, NV),
    smd_slots(SMD2, Name, SE, P, NV, R, SS, IS),
    add_to_smd(Tail, SMD2, ResultSMD).

add_to_smd([ par_relations(RList) | Tail ], SMD1, ResultSMD):-
    !,
    smd_slots(SMD1, Name, SE, P, V, R, SS, IS),
    smerge(RList, R, NR),
    smd_slots(SMD2, Name, SE, P, V, NR, SS, IS),
    add_to_smd(Tail, SMD2, ResultSMD).

add_to_smd([ system_elements(EList) | Tail ], SMD1, ResultSMD):-
    !,
    smd_slots(SMD1, Name, SE, P, V, R, SS, IS),
    smerge(EList, SE, NSE),
    smd_slots(SMD2, Name, NSE, P, V, R, SS, IS),
    add_to_smd(Tail, SMD2, ResultSMD).

add_to_smd([ system_structures(SList) | Tail ], SMD1, ResultSMD):-
    !,
    smd_slots(SMD1, Name, SE, P, V, R, SS, IS),
    smerge(SList, SS, NSS),
    smd_slots(SMD2, Name, SE, P, V, R, NSS, IS),
    add_to_smd(Tail, SMD2, ResultSMD).

% allow action tests etc.

add_to_smd([ _ | Tail ], SMD, ResultSMD):-
    add_to_smd(Tail, SMD, ResultSMD).


/* -------------------- merge rule conditions/results --------------- */

% merge two sets of results or conditions

% values must match, like add to smd ..

merge_rule_conditions_givens([ par_values(V1) | Tail ], L2, Result):-
    common_select(L2, par_values(V2), RL2),
    !,
    merge_values(V1, V2, NV),
    merge_rule_conditions_givens(Tail, [par_values(NV)|RL2], Result).

% anything else

% functor(List) in both sets

merge_rule_conditions_givens([S1|T1], L2, R):-
    S1 =.. [Functor, P1 ],
    common_select(L2, S2, T2),
    S2 =.. [Functor, P2],
    !,
    smerge(P1, P2, P3),
    S3 =.. [Functor, P3],
    merge_rule_conditions_givens(T1, [S3|T2], R).

% functor(List) in first set only

merge_rule_conditions_givens([S1|T1], L2, R):-
    merge_rule_conditions_givens(T1, [S1|L2], R).

merge_rule_conditions_givens([], R, R) :- !.

/* ----------------- apply transition rules ---------------------- */

% at the moment we only apply transition rules on values: one rule for
% each value

apply_transition_rules(values, [], [], []).
apply_transition_rules(values, [OnFirst|Rest], Conditions, Givens):-
    rule(transition, _, condition(ThisConditions), givens(ThisGivens)),
    ThisConditions = [ par_values([ OnFirst ]) ],
        % not really very much to chose from
    apply_transition_rules(values, Rest, ConditionsRest, GivensRest),
    append(ThisGivens, GivensRest, Givens),
    append(ThisConditions, ConditionsRest, Conditions).


remove_all([], L, L).

remove_all([H|T], L, NL):-
    common_select(L, H, L1),
    !,
    remove_all(T, L1, NL).

remove_all([_|T], L, NL):-
    remove_all(T, L, NL).

% check rule conditions for termination rules

check_rule_conditions([], _, _):- !.

check_rule_conditions([ termination(TList) | Tail ], SMD, To):-
    !,
    check_termination_conditions(TList, To),
    check_rule_conditions(Tail, SMD, To).

check_rule_conditions([ system_elements(EList) | Tail ], SMD, To):-
    !,
    smd_slots(SMD, _, Elements, _, _, _, _, _),
    super_class_sub_set(EList, Elements),
    % Elist specifies superconcept relations on a sub set of Elements
    check_rule_conditions(Tail, SMD, To).

check_rule_conditions([ parameters(PList) | Tail ], SMD, To):-
    !,
    smd_slots(SMD, _, _, Parameters, _, _, _, _),
    sub_set(PList, Parameters),
    check_rule_conditions(Tail, SMD, To).

check_rule_conditions([ par_values(VList) | Tail ], SMD, To):-
    !,
    smd_slots(SMD, _, _, _, Values, _, _, _),
    value_sub_set(VList, Values),
    check_rule_conditions(Tail, SMD,  To).

check_rule_conditions([ par_relations(RList) | Tail ], SMD, To):-
    !,
    smd_slots(SMD, _, _, _, _, Relations, _, _),

        % REPAIR: 23/08/2001 by BB (adding VList)
    smd_slots(SMD, _, _, _, VList, _, _, _),
    relations_sub_set(RList, Relations, VList),

    check_rule_conditions(Tail, SMD, To).

check_rule_conditions([ quantity_spaces(QList) | Tail ], SMD, To):-
    !,
    check_quantity_spaces(QList),
    check_rule_conditions(Tail, SMD, To).

check_rule_conditions([ H | _ ], _, _):-
    writef('*** Error: can not apply rule condition %w\n', [H]),
    fail.


% condition specifies a termination rule

% REPAIR: 18/04/2001 by BB
% The old version did not try all possibilities. If the 2nd
% clause failed, it removed the 1st 'to' from the 'Termination' list.
% After that, those removed terminations are lost, although they
% may have been crucial for one of the conditions still present
% in the 'Tail' of the '[H|T]' conditions of the rule that is
% being evaluated.
%
% The new version 'removes' only the item from the 'Termination'
% list that matches the 'Header' of the '[H|T]' conditions of the
% rule that is being evaluated (using 'select', a system predicate).
% All the other (remaining) terminations are used to evaluate the
% next condition in the 'Tail' of the '[H|T]' of the rule.
%
% OLD:
% check_termination_conditions([], _) :- !.
% check_termination_conditions([H|T], [to(cause(Causes), _, _, _, _)|R]):-
%   member(H, Causes),
%   check_termination_conditions(T, R).
% check_termination_conditions(L, [_|R]):-
%   check_termination_conditions(L, R).
%
% NEW:
check_termination_conditions([], _) :- !.
check_termination_conditions( [H|T], Terminations ) :-
        select( to(cause(Causes), _, _, _, _), Terminations, Rest ),
        member( H, Causes ),
        check_termination_conditions( T, Rest ).


/* rewrite quantityspace list so that each interval i becomes interval(i) */

add_interval_structure([], []).

add_interval_structure([point(X)|T], [point(X)|NT]):-
    !,
    add_interval_structure(T, NT).

add_interval_structure([I|T], [interval(I)|NT]):-
    add_interval_structure(T, NT).

% quantity space conditions

check_quantity_spaces([]).
check_quantity_spaces([meets(Par, L)|T]):-
    !,
    qspace(Par, _, Space, _),
    add_interval_structure(Space, NSpace),
    part_list(L, NSpace),
    check_quantity_spaces(T).


check_quantity_spaces([interval(Par, I)|T]):-
    !,
    qspace(Par, _, Space, _),
    memberchk(I, Space),
    I \= point(_),          % shouldn't be possible anyway
    check_quantity_spaces(T).
check_quantity_spaces([point(Par, I)|T]):-
    !,
    qspace(Par, _, Space, _),
    memberchk(point(I), Space),
    check_quantity_spaces(T).

check_quantity_spaces([H|_]):-
        writef('**** Error: can not apply %w\n', [H]),
        fail.


value_sub_set([], _).
value_sub_set([value(Nm, _Q, V, D)|T], L):-
    V == unknown,
    common_select(L, value(Nm, _QX, VX, DX), NL),
    var(VX), nonvar(DX),
    D = DX,
    value_sub_set(T, NL).
value_sub_set([value(Nm, _Q, V, D)|T], L):-
    common_select(L, value(Nm, _QX, VX, DX), NL),
    nonvar(VX), nonvar(DX),
    V = VX, D = DX,
    value_sub_set(T, NL).


/* FL 3-3-04, Old relations_sub_set before adding new correspondence primitives of garp 2.0***

relations_sub_set([], _, _).


% any form of correspondence between two parameters
% will produce multiple solutions if more than one
% correspondence relation is specified

relations_sub_set([correspondence(P1, V1, P2, V2)|T], L, VList):-
    !,
    (   common_select(L, q_correspondence(P1, P2), NL)
    ;
        common_select(L, q_correspondence(P2, P1), NL)
    ;
        common_select(L, dir_q_correspondence(P1, P2), NL)
    ;
        common_select(L, dir_q_correspondence(P2, P1), NL)
    ;
        common_select(L, v_correspondence(P1, V1, P2, V2), NL)
    ;
        common_select(L, v_correspondence(P2, V2, P1, V1), NL)
    ;
        common_select(L, dir_v_correspondence(P1, V1, P2, V2), NL)
    ;
        common_select(L, dir_v_correspondence(P2, V2, P1, V1), NL)
    ),
    relations_sub_set(T, NL, VList ).

relations_sub_set([H|T], L, VList ):-
    (   common_select(L, H, NL)
    ;
        alternative_relation(H, IH),
        common_select(L, IH, NL)
        ;
                try_value_consistency( H, L, VList, NL )
    ),
    relations_sub_set(T, NL, VList).
********* GARP 2.0 version of  relations_sub_set below: ***/

/* new correspondence primitives reference:

- dv_correspondence(_, _, _, _)
- dq_correspondence(_, _)
- dir_dv_correspondence(_, _, _, _)
- dir_dq_correspondence(_, _)
- full_correspondence(_, _)
- dir_full_correspondence(_, _)
- mirror_q_correspondence(_, _)
- mirror_dq_correspondence(_, _)

NB!
Since the rule interpreter that uses this procedure aspects only
correspondence between parameters, all derivative correspondences
are deactivated in this procedure.
*/

relations_sub_set([], _, _).

% any form of correspondence between two parameters
% will produce multiple solutions if more than one
% correspondence relation is specified

relations_sub_set([correspondence(P1, V1, P2, V2)|T], L, VList):-
    !,
    (
    member(Corr, [  q_correspondence(P1, P2),
                    q_correspondence(P2, P1),
                    dir_q_correspondence(P1, P2),
                    dir_q_correspondence(P2, P1),
                    v_correspondence(P1, V1, P2, V2),
                    v_correspondence(P2, V2, P1, V1),
                    dir_v_correspondence(P1, V1, P2, V2),
                    dir_v_correspondence(P2, V2, P1, V1),
%                   dq_correspondence(P1, P2),
%                   dq_correspondence(P2, P1),
%                   dir_dq_correspondence(P1, P2),
%                   dir_dq_correspondence(P2, P1),
%                   dv_correspondence(P1, V1, P2, V2),
%                   dv_correspondence(P2, V2, P1, V1),
%                   dir_dv_correspondence(P1, V1, P2, V2),
%                   dir_dv_correspondence(P2, V2, P1, V1),
                    full_correspondence(P1, P2),
                    full_correspondence(P2, P1),
%                   mirror_q_correspondence(P1, P2),
%                   mirror_q_correspondence(P2, P1),
%                   dir_mirror_q_correspondence(P1, P2),
%                   dir_mirror_q_correspondence(P2, P1),
%                   mirror_dq_correspondence(P1, P2),
%                   mirror_dq_correspondence(P2, P1),
%                   dir_mirror_dq_correspondence(P1, P2),
%                   dir_mirror_dq_correspondence(P2, P1),
                    dir_full_correspondence(P1, P2),
                    dir_full_correspondence(P2, P1)
                 ]),
    common_select(L, Corr, NL)
    )
    ;
    (
    member(Corr, [  mirror_q_correspondence(P1, P2),
                    mirror_q_correspondence(P2, P1),
                    dir_mirror_q_correspondence(P1, P2),
                    dir_mirror_q_correspondence(P2, P1)
%                   mirror_dq_correspondence(P1, P2),
%                   mirror_dq_correspondence(P2, P1),
%                   dir_mirror_dq_correspondence(P1, P2),
%                   dir_mirror_dq_correspondence(P2, P1)
                 ]),
    common_select(L, Corr, NL),
    mirrorable_qspaces(P1, P2, List),
    memberchk(V1/V2, List)
    ),
    relations_sub_set(T, NL, VList ).

relations_sub_set([H|T], L, VList ):-
    (   common_select(L, H, NL)
    ;
        alternative_relation(H, IH),
        common_select(L, IH, NL)
        ;
                try_value_consistency( H, L, VList, NL )
    ),
    relations_sub_set(T, NL, VList).



% Part of REPAIR: 23/08/2001 by BB

/*
All the rules for the transition step require a 100% match with what is
present in the state. So if a 5 valued quantity has it lowest value and a
constraint requires that the quantity is lower then its highest value, this
constraint will usually not succeed. Although consistent, this fact is
not directly present in the state. Here a procedure is added that does
allow rules to evaluate such constraints. If the constraint cannot be
match directly, but is consistent with the current state: proceed as if
rule is valid. However, the procedure is limited in that it only works
for values constraints (not for constraints on derivatives) and that it
does not do any transitivity reasoning. For the time being that seems
to be sufficient... */

% ALL NEW

try_value_consistency( H, _L, VList, _NL ):-
          % must be a in equality relation between a quantity (LHS) and a
          % value (RHS) no variables, only instances of quantities and values
          % the constraint value must exist in the Qspace (and should
          % be a point value)
          H =.. [ Rel, LHS, RHS ],
          % should we include equal and inequalities for derivatives...
          member( Rel, [greater, smaller, greater_or_equal, smaller_or_equal] ),
          nonvar( LHS ), nonvar( RHS ),
          memberchk( value( LHS, _SE, V, _D), VList ),
          qspace( LHS, _, QSpace, _ ),
          memberchk( point( RHS ), QSpace ),
          nonvar( V ),

	 (
	    @tracer->>tell(ordering,termination,transition)
	  ->
            write('Trying consistency check: '),
            write_ln( H ),
            @tracer->>told,
            !
         ;
            true
	 ),

          % separate QSpace wrt current value
          separate( V, QSpace, Lower, Higher ),
          % make constraint
          Constraint =.. [ Rel, RHS ],
          % test consistency
          value_consistency( Constraint, V, Lower, Higher ).


% Separate the values from a Qspace into Lower or Higher
% then the value the quantity currently has.
% Stop when Qspace becomes empty
separate( _, [], [], [] ):- !.
% When value matches, put rest QSpace in Higher list
% (match a point value)
separate( V, [point( V )|Higher], [], Higher ):- !.
% (now it should be an interval, but we don't check...)
separate( V, [V|Higher], [], Higher ):- !.
% When no match, put H1 (V in Qspace) in Lower list
separate( V, [H1|T1], [H1|T2], List ):-
        separate( V, T1, T2, List ).

value_consistency( equal( V ), V, _, _ ):- !.
value_consistency( greater( V ), _, Lower, _ ):-
        ( memberchk( point( V ), Lower ) ; memberchk( V, Lower ) ).
value_consistency( smaller( V ), _, _, Greater ):-
        ( memberchk( point(V), Greater ) ; memberchk( V, Greater ) ).
value_consistency( greater_or_equal( V ), V, _, _ ):- !.
value_consistency( greater_or_equal( V ), _, Lower, _ ):-
        ( memberchk( point( V ), Lower ) ; memberchk( V, Lower ) ).
value_consistency( smaller_or_equal( V ), V, _, _ ):- !.
value_consistency( smaller_or_equal( V ), _, _, Higher ):-
        ( memberchk( point(V), Higher ) ; memberchk( V, Higher ) ).

% End of REPAIR: 23/08/2001 by BB


% corresponding values for a parameter:
% values must be at same place in qspaces

get_corresponding_values([H1|_], [H2|_], V1, V2):-
    once((H1 = point(V1) ; H1 = V1)),
    once((H2 = point(V2) ; H2 = V2)).

get_corresponding_values([_|T1], [_|T2], V1, V2):-
    get_corresponding_values(T1, T2, V1, V2).

% alternative ways to specify relations:

alternative_relation(v_correspondence(P1, V1, P2, V2),
              v_correspondence(P2, V2, P1, V1)).
alternative_relation(q_correspondence(L, R), q_correspondence(R, L)).
alternative_relation(smaller(L, R), greater(R, L)).
alternative_relation(greater(L, R), smaller(R, L)).
alternative_relation(equal(L, R), equal(R, L)).
alternative_relation(smaller_or_equal(L, R), greater_or_equal(R, L)).
alternative_relation(greater_or_equal(L, R), smaller_or_equal(R, L)).
% FL 3-3-04: added derivative relations...
alternative_relation(d_smaller(L, R), d_greater(R, L)).
alternative_relation(d_greater(L, R), d_smaller(R, L)).
alternative_relation(d_equal(L, R), d_equal(R, L)).
alternative_relation(d_smaller_or_equal(L, R), d_greater_or_equal(R, L)).
alternative_relation(d_greater_or_equal(L, R), d_smaller_or_equal(R, L)).
% FL 3-3-04: NEW correspondence primitives added:
alternative_relation(dv_correspondence(P1, V1, P2, V2),
              dv_correspondence(P2, V2, P1, V1)).
alternative_relation(dq_correspondence(L, R), dq_correspondence(R, L)).
alternative_relation(full_correspondence(L, R), full_correspondence(R, L)).
alternative_relation(mirror_q_correspondence(L, R), mirror_q_correspondence(R, L)).
alternative_relation(mirror_dq_correspondence(L, R), mirror_dq_correspondence(R, L)).

/* correspondence primitives reference:
- dv_correspondence(_, _, _, _)
- dq_correspondence(_, _)
- dir_dv_correspondence(_, _, _, _)
- dir_dq_correspondence(_, _)
- full_correspondence(_, _)
- dir_full_correspondence(_, _)
- mirror_q_correspondence(_, _)
- dir_mirror_q_correspondence(_, _)
- mirror_dq_correspondence(_, _)
- dir_mirror_dq_correspondence(_, _)
*/


% merge a list of values into another list of values
% note that if   value(Instance, Q1, V1, D1) does not match with
% value(Instance, Q2, V2, D2), the merge is not possible

merge_values([], L2, L2).

merge_values([value(Instance, Q1, V1, D1) | T1], L2,
          [value(Instance, Q1, V1, D1) | T3]):-
    common_select(L2, value(Instance, Q2, V2, D2), T2),
    !,
    (
      value(Instance, Q1, V1, D1) = value(Instance, Q2, V2, D2)
    ->
      merge_values(T1, T2, T3)
    ;
      etrace(methods_value_clash,
	     [value(Instance, Q1, V1, D1), value(Instance, Q2, V2, D2) ],
	     general),
      fail
    ).

merge_values([V1|T1], L2, [V1|T3]):-
    merge_values(T1, L2, T3).

% remove system elements from smd: remove same class and/or sub class

remove_system_elements([], NL, NL).

remove_system_elements([H|T], L, NL):-
    isa_instance_select(H, L, L1),  % probably succeeds once
    !,                  % but retry for safety
    remove_system_elements([H|T], L1, NL).

remove_system_elements([H|T], L, NL):-
    isa_attribute_select(H, L, L1),     % probably succeeds once
    !,                  % but retry for safety
    remove_system_elements([H|T], L1, NL).

remove_system_elements([H|T], L, NL):-
    common_select(L, H, L1),    % probably succeeds once
    !,                  % but retry for safety
    remove_system_elements([H|T], L1, NL).

remove_system_elements([_|T], L, NL):-
    remove_system_elements(T, L, NL).


% ------------------------Determine Exogenous Derivatives----------------------------
%
% exogenous variables get derivatives before influence resolution:
% - check if parameter is in givens of exogenous MF,
% - then use instance information from Cio not from MF to determine most
% specific class.
%
% 7 types:
% - continuous: derivative always plus on the way up, always min on the way down,
% except in highest/lowest points,  terminations arrange stopping and turning around.
% if way up/down is not defined coming from the input system, then generate all
% possibilities: up/down, zero in endpoints.
% - free: if not defined, generate all possibilities in lowest point
% - increasing: derivative always plus except in highest if point
% - steady: derivative always zero
% - decreasing: derivative always min except in lowest if point
% - parabola pos: up, turn around in highest value, down, stop in lowest
% if point.
% - parabola neg: down, turn around in lowest value, up, stop in
% highest if it is apoint.
%
% 2nd derivatives are only supplied with
% exogenous quantities if they are absolutely certain. (steady
% endstate).  (after careful consideration)
%


determine_exogenous_derivatives(Cin, SS, Cout):-
    etrace(methods_det_exo_start, _, add_relation),
    cio_oe(Cin, SE),
    determine_exogenous_derivatives(SS, SE, Cin, Cout),
    etrace(methods_det_exo_done, _, add_relation).


% for every model fragment check if exogenous,
% and act accordingly upon parameters
determine_exogenous_derivatives([], _, Cin, Cin).

% exogenous
determine_exogenous_derivatives([SS|T], SE, Cin, Cout):-
    is_exogenous_mf(SS, SE, Type, Par),
    !,
    value_low_middle_high(Par, Cin, VLMH),
    determine_exogenous_derivative(Par, Type, Cin, Cnew, VLMH),
    determine_exogenous_derivatives(T, SE, Cnew, Cout).

% not exogenous
determine_exogenous_derivatives([_|T], SE, Cin, Cout):-
   determine_exogenous_derivatives(T, SE, Cin, Cout).

% value_low_middle_high(+Par, +Cin, -VLMH)
% determine if par is in lowest, highest or a middle value.
% return values: low(interval/point), middle, high(interval/point)
value_low_middle_high(Par, Cin, VLMH):-
    cio_d(Cin, Derivable),
    qspace(Par, _, Space, _),
    get_derivable_value(value(Par), Interval, Space, Cin, _,
            Derivable),
    low_middle_high(Space, Interval, VLMH).


low_middle_high([Low|_], Low, low(interval)):-!.

low_middle_high([point(Low)|_], Low, low(point)):-!.

low_middle_high(Space, High, high(interval)):-
    last(Space, High),
    !.

low_middle_high(Space, High, high(point)):-
    last(Space, point(High)),
    !.

low_middle_high(_, _, middle).


% determine_exogenous_derivative(+Par, +Type, +Cin, -Cnew, VLMH)
% set derivatives:
% continuous & free can be backtrackable, others not.

% exogenous_sinus: sign known,
determine_exogenous_derivative(Par, exogenous_sinus, Cin, Cout, VLMH):-
    cio_q(Cin, Q),
    cio_d(Cin, D),
    memberchk(derivative(Par)/I, Q),
    list_map([I], Map),
    get_quantity_sign(Map, D, Sign),
    !,
    (
    (Sign = zero, VLMH = middle)
    ->
      (etrace(methods_sinus_zero_in_middle_error, Par, general), fail)
    % in middle values sinus derivative may not be zero. Correspondences can wrongfully set derivative
    ;
      true
    ),
    (  %Set 2nd order der: neg -> dd pos,    pos -> dd neg
       fail %NO MORE UNCERTAIN 2ND DER FOR EXO!!!
%      memberchk(Sign/DRel, [neg/dd_greater(Par, zero), pos/dd_smaller(Par, zero)])
    ->
      intern_representation(DRel, DRelation, Cin, Cnew2),
      etrace(methods_set_exo_2nd_der, [DRel, Par], resolve),
      exogenous_try_append_relation([Par], DRelation, Cnew2, Cout, _, true)
    ;
      Cin = Cout
    ).


% exogenous_sinus: in middle value: plus and min.
determine_exogenous_derivative(Par, exogenous_sinus, Cin, Cout, middle):-
    List = [Par, exogenous_sinus, Rel, middle, input, Nr, 2],
    etrace(methods_set_exo_der_branchepoint, List, general),
    !,
    member(Rel, [d_smaller(Par, zero), %if going down, dd is plus (stopping)
		      d_greater(Par, zero)]),  %if going up, dd is min (stopping)
    nth1(Nr, [d_smaller(Par, zero), d_greater(Par, zero)], Rel),
    etrace(methods_set_exo_der_intobranche, List, resolve),
    intern_representation(Rel, Relation, Cin, Cnew),
    exogenous_try_append_relation(List, Relation, Cnew, Cout, _, true).

% exogenous_sinus: in highest point: stable / down.
determine_exogenous_derivative(Par, exogenous_sinus, Cin, Cout, high(point)):-
    !,
    List = [Par, exogenous_sinus, Rel, extreme, input, Nr, 2],
    etrace(methods_set_exo_der_branchepoint, List, general),
    !,
    member(Rel, [d_smaller(Par, zero),
		 d_equal(Par, zero)]),
    nth1(Nr, [d_smaller(Par, zero), d_equal(Par, zero), d_greater(Par, zero)], Rel),
    etrace(methods_set_exo_der_intobranche, List, resolve),
    intern_representation(Rel, Relation, Cin, Cnew),
    exogenous_try_append_relation(List, Relation, Cnew, Cout, _, true).

% exogenous_sinus: in lowest point: stable / up.
determine_exogenous_derivative(Par, exogenous_sinus, Cin, Cout, low(point)):-
    !,
    List = [Par, exogenous_sinus, Rel, extreme, input, Nr, 2],
    etrace(methods_set_exo_der_branchepoint, List, general),
    !,
    member(Rel, [ d_equal(Par, zero),
		  d_greater(Par, zero)]),
    nth1(Nr, [d_smaller(Par, zero), d_equal(Par, zero), d_greater(Par, zero)], Rel),
    etrace(methods_set_exo_der_intobranche, List, resolve),
    intern_representation(Rel, Relation, Cin, Cnew),
    exogenous_try_append_relation(List, Relation, Cnew, Cout, _, true).

% exogenous_sinus: in extreme interval: all.
determine_exogenous_derivative(Par, exogenous_sinus, Cin, Cout, VLMH):-
    memberchk(VLMH, [high(interval), low(interval)]),
    !,
    List = [Par, exogenous_sinus, Rel, extreme, input, Nr, 3],
    etrace(methods_set_exo_der_branchepoint, List, general),
    !,
    member(Rel, [d_smaller(Par, zero),
		      d_equal(Par, zero),
		      d_greater(Par, zero)]),
    nth1(Nr, [d_smaller(Par, zero), d_equal(Par, zero), d_greater(Par, zero)], Rel),
    etrace(methods_set_exo_der_intobranche, List, resolve),
    intern_representation(Rel, Relation, Cin, Cnew),
    exogenous_try_append_relation(List, Relation, Cnew, Cout, _, true).


% exogenous_free: sign already known
determine_exogenous_derivative(Par, exogenous_free, Cin, Cin, _):-
    cio_q(Cin, Q),
    cio_d(Cin, D),
    memberchk(derivative(Par)/I, Q),
    list_map([I], Map),
    get_quantity_sign(Map, D, _Sign),
    !.


% exogenous_free: sign unknown -> new from inputsystem:
% return all possibilities,
determine_exogenous_derivative(Par, exogenous_free, Cin, Cout, _):-
    List = [Par, exogenous_free, Rel, nil, input, Nr, 3],
    etrace(methods_set_exo_der_branchepoint, List, general),
    !,
    member(Rel, [d_smaller(Par, zero), %if going down, dd is plus (stopping)
		      d_equal(Par, zero),
		      d_greater(Par, zero)]),  %if going up, dd is min (stopping)
    nth1(Nr, [d_smaller(Par, zero), d_equal(Par, zero), d_greater(Par, zero)], Rel),
    intern_representation(Rel, Relation, Cin, Cnew),
    etrace(methods_set_exo_der_intobranche, List, resolve),
    exogenous_try_append_relation(List, Relation, Cnew, Cout, _, true).

% 22-2-2007 FL: reinserted clause,
% thus changed from old semantics 20-8-2004 FL:  exogenous increasing should always be dPlus,
% exogenous_increasing: arrived at highest point, dd stable
determine_exogenous_derivative(Par, exogenous_increasing, Cin, Cout, high(point)):-
    List = [Par, exogenous_increasing, d_equal(Par, zero), highestpoint, nil, nil, nil],
    intern_representation(d_equal(Par, zero), Relation, Cin, Cnew),
    !,
    etrace(methods_set_exo_der, List, resolve),
    exogenous_try_append_relation(List, Relation, Cnew, Cnew1, _, true),
    intern_representation(dd_equal(Par, zero), DRelation, Cnew1, Cnew2),
    etrace(methods_set_exo_2nd_der, [dd_equal(Par, zero), Par], resolve),
    exogenous_try_append_relation([Par], DRelation, Cnew2, Cout, _, true),
    !.


% exogenous_increasing: set to plus,
determine_exogenous_derivative(Par, exogenous_increasing, Cin, Cout, _):-
    List = [Par, exogenous_increasing, d_greater(Par, zero), nil, nil, nil, nil],
    intern_representation(d_greater(Par, zero), Relation, Cin, Cnew),
    etrace(methods_set_exo_der, List, resolve),
    exogenous_try_append_relation(List, Relation, Cnew, Cout, _, true),
    !.

% exogenous_steady: already known zero Set dd zero
determine_exogenous_derivative(Par, exogenous_steady, Cin, Cout, _):-
    cio_q(Cin, Q),
    cio_d(Cin, D),
    memberchk(derivative(Par)/I, Q),
    list_map([I], Map),
    get_quantity_sign(Map, D, Sign),
    Sign = zero,
    !,
    intern_representation(dd_equal(Par, zero), DRelation, Cin, Cnew2),
    etrace(methods_set_exo_2nd_der, [dd_equal(Par, zero), Par], resolve),
    exogenous_try_append_relation([Par], DRelation, Cnew2, Cnew3, _, true),
    %3rd also zero
    intern_representation(ddd_equal(Par, zero), DDRelation, Cnew3, Cnew4),
    etrace(methods_set_exo_3rd_der, [ddd_equal(Par, zero), Par], resolve),
    exogenous_try_append_relation([Par], DDRelation, Cnew4, Cout, _, true).

% exogenous_steady: set to zero, dd zero also
determine_exogenous_derivative(Par, exogenous_steady, Cin, Cout, _):-
    List = [Par, exogenous_steady, d_equal(Par, zero), nil, nil, nil, nil],
    intern_representation(d_equal(Par, zero), Relation, Cin, Cnew),
    etrace(methods_set_exo_der, List, resolve),
    exogenous_try_append_relation(List, Relation, Cnew, Cnew1, _, true),
    %2nd der
    intern_representation(dd_equal(Par, zero), DRelation, Cnew1, Cnew2),
    etrace(methods_set_exo_2nd_der, [dd_equal(Par, zero), Par], resolve),
    exogenous_try_append_relation([Par], DRelation, Cnew2, Cnew3, _, true),
    %3rd der
    intern_representation(ddd_equal(Par, zero), DDRelation, Cnew3, Cnew4),
    etrace(methods_set_exo_3rd_der, [ddd_equal(Par, zero), Par], resolve),
    exogenous_try_append_relation([Par], DDRelation, Cnew4, Cout, _, true),
    !.

% 22-2-2007 FL: reinserted clause,
% thus changed from old semantics of 20-8-2004 : decr. should always be dMin
% exogenous_decreasing: arrived at lowest point, dd to zero
determine_exogenous_derivative(Par, exogenous_decreasing, Cin, Cout, low(point)):-
    intern_representation(d_equal(Par, zero), Relation, Cin, Cnew),
    List = [Par, exogenous_decreasing, d_equal(Par, zero), lowestpoint, nil, nil, nil],
    etrace(methods_set_exo_der, List, resolve),
    exogenous_try_append_relation(List, Relation, Cnew, Cnew1, _, true),
    intern_representation(dd_equal(Par, zero), DRelation, Cnew1, Cnew2),
    etrace(methods_set_exo_2nd_der, [dd_equal(Par, zero), Par], resolve),
    exogenous_try_append_relation([Par], DRelation, Cnew2, Cout, _, true),
    !.


% exogenous_decreasing: set to min,
determine_exogenous_derivative(Par, exogenous_decreasing, Cin, Cout, _):-
    intern_representation(d_smaller(Par, zero), Relation, Cin, Cnew),
    List = [Par, exogenous_decreasing, d_smaller(Par, zero), nil, nil, nil, nil],
    etrace(methods_set_exo_der, List, resolve),
    exogenous_try_append_relation(List, Relation, Cnew, Cout, _, true),
    !.

/*****	FL NEW TYPES: june 07 *****/
%
/* ******* pos parabola ******* */
%
% exogenous_pos_parabola: sign already known,
determine_exogenous_derivative(Par, exogenous_pos_parabola, Cin, Cin, _):-
    cio_q(Cin, Q),
    cio_d(Cin, D),
    memberchk(derivative(Par)/I, Q),
    list_map([I], Map),
    get_quantity_sign(Map, D, _Sign),
    !.

% exogenous_pos_parabola: sign unknown -> endpoint ...
% At lowest point: set to zero, dd zero also
determine_exogenous_derivative(Par, exogenous_pos_parabola, Cin, Cout, low(point)):-
    cio_q(Cin, Q),
    cio_d(Cin, D),
    memberchk(derivative(Par)/I, Q),
    list_map([I], Map),
    list_map([0], ZMap),
    is_derivable(relation(ZMap, >=, Map), D), %on the way down
    !,
    List = [Par, exogenous_pos_parabola, d_equal(Par, zero), nil, nil, nil, nil],
    intern_representation(d_equal(Par, zero), Relation, Cin, Cnew),
    etrace(methods_set_exo_der, List, resolve),
    exogenous_try_append_relation(List, Relation, Cnew, Cnew1, _, true),
    intern_representation(dd_equal(Par, zero), DRelation, Cnew1, Cnew2),
    etrace(methods_set_exo_2nd_der, [dd_equal(Par, zero), Par], resolve),
    exogenous_try_append_relation([Par], DRelation, Cnew2, Cout, _, true),
    !.

% exogenous_pos_parabola: sign unknown -> returning down ...
% set to min,
determine_exogenous_derivative(Par, exogenous_pos_parabola, Cin, Cout, _):-
    cio_q(Cin, Q),
    cio_d(Cin, D),
    memberchk(derivative(Par)/I, Q),
    list_map([I], Map),
    list_map([0], ZMap),
    is_derivable(relation(ZMap, >=, Map), D), %on the way down
    !,
    List = [Par, exogenous_pos_parabola, d_smaller(Par, zero), nil, nil, nil, nil],
    intern_representation(d_smaller(Par, zero), Relation, Cin, Cnew),
    etrace(methods_set_exo_der, List, resolve),
    exogenous_try_append_relation(List, Relation, Cnew, Cout, _, true),
    !.


% exogenous_pos_parabola: sign unknown -> other cases: on the way up
% set to plus,
determine_exogenous_derivative(Par, exogenous_pos_parabola, Cin, Cout, _):-
    List = [Par, exogenous_pos_parabola, d_greater(Par, zero), nil, nil, nil, nil],
    intern_representation(d_greater(Par, zero), Relation, Cin, Cnew),
    etrace(methods_set_exo_der, List, resolve),
    exogenous_try_append_relation(List, Relation, Cnew, Cout, _, true),
    !.

/*	***** neg parabola ***** */

% exogenous_neg_parabola: sign already known,
determine_exogenous_derivative(Par, exogenous_neg_parabola, Cin, Cin, _):-
    cio_q(Cin, Q),
    cio_d(Cin, D),
    memberchk(derivative(Par)/I, Q),
    list_map([I], Map),
    get_quantity_sign(Map, D, _Sign),
    !.

% exogenous_neg_parabola: sign unknown -> upper endpoint..
% set to steady, dd also
determine_exogenous_derivative(Par, exogenous_neg_parabola, Cin, Cout, high(point)):-
    cio_q(Cin, Q),
    cio_d(Cin, D),
    memberchk(derivative(Par)/I, Q),
    list_map([I], Map),
    list_map([0], ZMap),
    is_derivable(relation(Map, >=, ZMap), D), % on the return up
    !,
    List = [Par, exogenous_neg_parabola, d_equal(Par, zero), nil, nil, nil, nil],
    intern_representation(d_equal(Par, zero), Relation, Cin, Cnew),
    etrace(methods_set_exo_der, List, resolve),
    exogenous_try_append_relation(List, Relation, Cnew, Cnew1, _, true),
    intern_representation(dd_equal(Par, zero), DRelation, Cnew1, Cnew2),
    etrace(methods_set_exo_2nd_der, [dd_equal(Par, zero), Par], resolve),
    exogenous_try_append_relation([Par], DRelation, Cnew2, Cout, _, true),
    !.

% exogenous_neg_parabola: sign unknown -> returning up...
% set to plus,
determine_exogenous_derivative(Par, exogenous_neg_parabola, Cin, Cout, _):-
    cio_q(Cin, Q),
    cio_d(Cin, D),
    memberchk(derivative(Par)/I, Q),
    list_map([I], Map),
    list_map([0], ZMap),
    is_derivable(relation(Map, >=, ZMap), D),
    !,
    List = [Par, exogenous_neg_parabola, d_smaller(Par, zero), nil, nil, nil, nil],
    intern_representation(d_greater(Par, zero), Relation, Cin, Cnew),
    etrace(methods_set_exo_der, List, resolve),
    exogenous_try_append_relation(List, Relation, Cnew, Cout, _, true),
    !.

% exogenous_neg_parabola: sign unknown -> inputsystem or going down:
% set to min,
determine_exogenous_derivative(Par, exogenous_neg_parabola, Cin, Cout, _):-
     List = [Par, exogenous_neg_parabola, d_smaller(Par, zero), nil, nil, nil, nil],
     intern_representation(d_smaller(Par, zero), Relation, Cin, Cnew),
     etrace(methods_set_exo_der, List, resolve),
     exogenous_try_append_relation(List, Relation, Cnew, Cout, _, true),
     !.


% extra layer for tracer
exogenous_try_append_relation(_, Relation, Cnew, Cout, _, true):-
    append_relation(Relation, Cnew, Cout, _, true),
    !.
exogenous_try_append_relation(List, _, _, _, _, _):-
    etrace(methods_set_exo_der_fail, List, general),
    fail.


% a parameter appears in givens of a MF which requires
% exogenous assumption. Return type of exogenous variable
is_exogenous_mf(SS, SElist, Type, Par):-
    system_structure_slots(SS, _, _, C, G),
    memberchk(system_elements(SEfromSS), C),
    member(instance(Assumption, Class), SEfromSS),
    isa_instance(Class, exogenous),
    !, % SS is exogenous, now determine specific type from seList, MF may be general
    specific_exogenous_type(Assumption, SElist, Type),
    memberchk(parameters([P]), G), % exogenous MF should introduce only one parameter
    P =..[_, _, Par, _, _].

% exogenous_sinus
specific_exogenous_type(Assumption, SElist, exogenous_sinus):-
    member(instance(Assumption, Class), SElist),
    isa_instance(Class, exogenous_sinus),
    !.

% exogenous_free
specific_exogenous_type(Assumption, SElist, exogenous_free):-
    member(instance(Assumption, Class), SElist),
    isa_instance(Class, exogenous_free),
    !.

% exogenous_increasing
specific_exogenous_type(Assumption, SElist, exogenous_increasing):-
    member(instance(Assumption, Class), SElist),
    isa_instance(Class, exogenous_increasing),
    !.

% exogenous_steady
specific_exogenous_type(Assumption, SElist, exogenous_steady):-
    member(instance(Assumption, Class), SElist),
    isa_instance(Class, exogenous_steady),
    !.

% exogenous_decreasing
specific_exogenous_type(Assumption, SElist, exogenous_decreasing):-
    member(instance(Assumption, Class), SElist),
    isa_instance(Class, exogenous_decreasing),
    !.

/*	FL NEW TYPES: june 07 */
% exogenous_pos_parabola
specific_exogenous_type(Assumption, SElist, exogenous_pos_parabola):-
    member(instance(Assumption, Class), SElist),
    isa_instance(Class, exogenous_pos_parabola),
    !.

% exogenous_neg_parabola
specific_exogenous_type(Assumption, SElist, exogenous_neg_parabola):-
    member(instance(Assumption, Class), SElist),
    isa_instance(Class, exogenous_neg_parabola),
    !.



%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%	%%%%%%%%%%%%%%%%%%%% VALUE BRANCHING %%%%%%%%%%%%%%%%%%%%%%
%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%	If a value X is unknown after class algorithm
%	And it is defined by a calculus X = A - B or A + B
%	(where A and B can also be calculi)
%	Then branch over the different values of A in its qspace
%


value_branching(Cio, _, Cio):-
	flag(value_branching, Flag, Flag),
	algorithm_assumption_flag(Flag, fail, value_branching),
	!.

value_branching(Cin, InputRels, Cout):-
	flag(value_branching, Flag, Flag),
	algorithm_assumption_flag(Flag, true, value_branching),
	% find candidates: user representation
	cio_sf(Cin, SS),
	get_derivable_values(Cin, Cio1),
	cio_v(Cio1, V),
	collect_calculus_relations(InputRels, SS, V, Rels),
	% sorting is imperfect but improves when done a few times.
	sort_branching_candidates(Rels, [], Candidates1),
	sort_branching_candidates(Candidates1, [], Candidates2),
	sort_branching_candidates(Candidates2, [], Candidates),
	!, % backtracking from here:
	(
	  Candidates \= []
	->
	  etrace(methods_value_branching_start, [], specification),
	  value_branching2(Candidates, Cin, Cout, true),
	  etrace(methods_value_branching_done, [], specification)
	;
	  Cin = Cout
	).




/*	***** Proces Branching Candidates ***** */
%
%	because candidates can rely on other candidates
%	unresolved candidates (not all calculus quantities known)
%	are retried as long as there is progress
%

%no progress
value_branching2(_, Cio, Cio, false).

% first time or progress
value_branching2(Candidates, Cin, Cout, true):-
	value_branching3(Candidates, Cin, Cio, [], Unresolved, Progress),
	value_branching2(Unresolved, Cio, Cout, Progress).


% done, tailprogress = false
value_branching3([], Cio, Cio, UnResolved, UnResolved, false).

% check if X is unknown, and all in Add are known,
value_branching3([b_c(Rel, X, Add)|T], Cin, Cout, UnResolved, TailUnResolved, Progress):-
	get_derivable_values(Cin, Ciotemp),
	cio_v(Ciotemp, V),

	%check X
	memberchk(value(X, _, VA, _), V),
	(var(VA) -> XUnKnown = true ; XUnKnown = false),

	%check Rest
	(all_known(Add, V) -> RestKnown = true ; RestKnown = false),

	value_branching4(XUnKnown, RestKnown, [b_c(Rel, X, Add)|T], Cin, Cout, UnResolved, TailUnResolved, Progress).


% X is known,
% remove branching candidate
% Progress = TailProgress
value_branching4(false, _, [_|T], Cin, Cout, UnResolved, TailUnResolved, Progress):-
	!,
	value_branching3(T, Cin, Cout, UnResolved, TailUnResolved, Progress).

% X is unknown, Calculus quantities not all known yet...
% Put on branching candidate on unresolved
% Progress = TailProgress
value_branching4(true, false, [b_c(Rel, X, Add)|T], Cin, Cout, UnResolved, [b_c(Rel, X, Add)|TailUnResolved], Progress):-
	!,
	etrace(methods_valuebranchpoint_unresolved, [Rel, X], specification),
	value_branching3(T, Cin, Cout, UnResolved, TailUnResolved, Progress).

% X is unknown, Calculus quantities all known...
% Branch over all values of X
% Progress is true
value_branching4(true, true, [b_c(Rel, X, _Add)|T], Cin, Cout, UnResolved, TailUnResolved, true):-
	% find possible values for X
	qspace(X, _, Space, _),
	remove_point_indications(Space, FlatSpace),

	%tracerstuff
	length(FlatSpace, L),
	etrace(methods_valuebranchpoint, [Rel, X, FlatSpace, L], specification),

	!, % backtracking from here
	% branch over these values
	member(VX, FlatSpace),

	% tracer
	nth1(Nr, FlatSpace, VX),
	etrace(methods_into_valuebranch, [Rel, X, VX, Nr, L], specification),

	relations_interval(value(X), VX, Space, Relations),
	add_givens([par_relations(Relations)], Cin, Cio2),
	value_branching3(T, Cio2, Cout, UnResolved, TailUnResolved, _).



remove_point_indications([], []).
remove_point_indications([point(X)|T], [X|PT]):-
	!,
	remove_point_indications(T, PT).
remove_point_indications([X|T], [X|PT]):-
	remove_point_indications(T, PT).


all_known([], _).
all_known([H|T], V):-
	memberchk(value(H, _, VA, _), V),
	nonvar(VA),
	all_known(T, V).






/*	***** sort branching candidates ***** */
%
%	if a calculus depends on other calculi
%	then do those first..
%
%	sorting is not correct if loops exist..
%


sort_branching_candidates([], Candidates, Candidates).
sort_branching_candidates([b_c(Rel, X, Add)|T], SoFarIn, Candidates):-
	insert_branching_candidate(Add, b_c(Rel, X, Add), SoFarIn, SoFarOut),
	sort_branching_candidates(T, SoFarOut, Candidates).

% checked al calculus quantities put in front of rest
insert_branching_candidate([], BC, Rest, [BC|Rest]).

% calculus Qty is result of calculus itself.
insert_branching_candidate([Qty|T], b_c(Rel, X, Add), SoFar, NewSoFar):-
	memberchk(b_c(_, Qty, _), SoFar),
	!,
	split_bc_sofar(Qty, SoFar, SoFarFront, SoFarBack),
	insert_branching_candidate(T, b_c(Rel, X, Add), SoFarBack, NewSoFarBack),
	append(SoFarFront, NewSoFarBack, NewSoFar).

% calculus Qty is not result of calculus itself. ignore item
insert_branching_candidate([_|AddListTail], BC, SoFar, NewSoFar):-
	insert_branching_candidate(AddListTail, BC, SoFar, NewSoFar).

% split_adders_sofar(Qty, Input, InputFront, InputBack)
% find the adder in SoFar that influences Qty,
% return Front and Back of List,
% Front includes the adder that influences Qty
% fails if no adder influences Qty, but with a memberchk in front of the call
% that should never happen.

% found, done
split_bc_sofar(Qty, [b_c(Rel, Qty, List)|Tail], [b_c(Rel, Qty, List)], Tail):- !.

split_bc_sofar(QtyA, [b_c(Rel, Qty, List)|Tail], [b_c(Rel, Qty, List)|Front], Back):-
	split_bc_sofar(QtyA, Tail, Front, Back).


%%%%%%%%%%%%% collect value branching candidates
%
%	collects all caluculi where
%	one qty value X, is equal to the addition or subtraction
%	of some other values
%	X must be unknown still.
%	the other values may or may not be known
%

collect_calculus_relations(InputRels, SS, V, Rels):-
	collect_calculus_rels(InputRels, V, InRels),
	collect_ss_calculus_relations(SS, V, SSRels),
	append(InRels, SSRels, Rels1),
	sort(Rels1, Rels).


collect_ss_calculus_relations([], _V, []).
collect_ss_calculus_relations([H|T], V, Rels):-
	system_structure_slots(H, _, _, Conditions, Givens),
	memberchk(par_relations(ConditionRels), Conditions),
	memberchk(par_relations(ResultRels), Givens),
	collect_calculus_rels(ConditionRels, V, CRels),
	collect_calculus_rels(ResultRels, V, RRels),
	collect_ss_calculus_relations(T, V, TailRels),
	append(CRels, RRels, Rels1),
	append(Rels1, TailRels, Rels).


% collect branching candidates
collect_calculus_rels([], _V, []).
collect_calculus_rels([H|T], V, [b_c(H, X, CalcQtys)|TailR]):-
	is_calculus_rel(H, V, X, CalcQtys),
	!,
	collect_calculus_rels(T, V, TailR).

collect_calculus_rels([_|T], V, TailR):-
	collect_calculus_rels(T, V, TailR).


is_calculus_rel(equal(A, plus(B, C)), V, A, Qtys):-
	!,
	memberchk(value(A, _, VA, _), V),
	var(VA),
	qtys_in_calculus(B, BQ),
	qtys_in_calculus(C, CQ),
	append(BQ, CQ, Qtys1),
	sort(Qtys1, Qtys).
is_calculus_rel(equal(A, min(B, C)), V, A, Qtys):-
	!,
	memberchk(value(A, _, VA, _), V),
	var(VA),
	qtys_in_calculus(B, BQ),
	qtys_in_calculus(C, CQ),
	append(BQ, CQ, Qtys1),
	sort(Qtys1, Qtys).
is_calculus_rel(equal(plus(B, C), A), V, A, Qtys):-
	!,
	memberchk(value(A, _, VA, _), V),
	var(VA),
	qtys_in_calculus(B, BQ),
	qtys_in_calculus(C, CQ),
	append(BQ, CQ, Qtys1),
	sort(Qtys1, Qtys).
is_calculus_rel(equal(min(B, C), A), V, A, Qtys):-
	!,
	memberchk(value(A, _, VA, _), V),
	var(VA),
	qtys_in_calculus(B, BQ),
	qtys_in_calculus(C, CQ),
	append(BQ, CQ, Qtys1),
	sort(Qtys1, Qtys).

qtys_in_calculus(plus(B, C), Qtys):-
	qtys_in_calculus(B, BQ),
	qtys_in_calculus(C, CQ),
	append(BQ, CQ, Qtys),
	!.
qtys_in_calculus(min(B, C), Qtys):-
	qtys_in_calculus(B, BQ),
	qtys_in_calculus(C, CQ),
	append(BQ, CQ, Qtys),
	!.
qtys_in_calculus(X, [X]):-
	atom(X).

