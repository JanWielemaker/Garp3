/*  File:     Reclass.pl
    Purpose:  Reclassify parts of old SMD
    Author:   Martin Reinders & Bert Bredeweg & Floris Linnebank
    Date:     August 1989
    Part-of:  GARP (version 2.0)
    Modified: 30 July 2004

    Copyright (c) 2004, University of Amsterdam. All rights reserved.
*/

% a findall call to depth/3 in methods.pl starts the depth first search
% to successor states for a certain input scenario or 'transition scenario'
% (results of a transition)


% first clause is for initial input model

depth(  To, SMD, NextState):-
    To = to(cause([]),
        conditions([]),
        results([]),
        to_state([]),
        _),
    !,
    smd_slots(SMD, Name, SEA, PA, VA, RA, SSA, InputSMD),
    smd_slots(InputSMD, IName, SEI, PI, VI, RI, SSI, _),
    append(SEA, SEI, SE),
    append(PA, PI, P),
    append(VA, VI, V),
    append(RA, RI, R),
    append(SSA, SSI, SS),
    clean_up_depth,
    cio_empty(Cin),
    etrace(reclass_scn_start, _, specification),
    (
      add_givens([ system_elements(SE), parameters(P), par_values(V),
		   par_relations(R) ], Cin, Cnew)
    ->
      etrace(reclass_scn_done, _, specification)
    ;
      etrace(reclass_scn_fail, _, specification),
      fail %inconsistent scenario
    ),
    assertz(scenario_state(InputSMD)), %gp3 1.4: save inputsmd
    !,
    depth_split(Name/IName/RI/SEI/[], [], [], [], SS, Cnew, NextState).


/*** garp 1.7 version: rule based etc. USED TO BE ACTIVE under algorithm option switch:

% transition

depth(  To, SMD_old, NextState):-
    flag(use_old_transition_rules, Flag, Flag), %new FL
    algorithm_assumption_flag(Flag, true, use_old_transition_rules),      %new FL

    To = to(cause(Causes),
        conditions(ConditionsALL),
        results(ResultsALL),
        to_state(_States), _),

    etrace(reclass_apply, Causes, transition),

        % REPAIR: simplify_transition 21/08/2001 by BB (see below for explanation)
        % remove SE items present in both Conditions and Givens from both lists
        simplify_transition( ConditionsALL, Conditions, ResultsALL, Results ),

    % remove termination conditions from total set and input model

    remove_from_smd(Conditions, SMD_old, SMD_old_I),
    smd_slots(SMD_old_I, Name, SE_old, P_old, V_old, _, SS_old, Input_old),
    % get old input model system elements (before removing conditions)
    smd_slots(Input_old, _, Input_SE_org, _, _, _, _, _),
    remove_from_smd(Conditions, Input_old, Input_old_I),
    smd_slots(Input_old_I, IName, Input_SE_old, _, _, Input_R_old, _, _),

    % apply transition rules on values

    apply_transition_rules(values, V_old, _, TransitionResults),

    clean_up_depth,
    cio_empty(Cin),

    % just a way to remove results that don't specify parameters, values etc
    smd_slots(Dummy, _, [], [], [], [], [], _),
    add_to_smd(Results, Dummy, Dummy_I),
    smd_slots(Dummy_I, _, SE_res, P_res, V_res, R_res, _, _),

    % remove systemelement attribute relations for system elements
    % mentioned in original input model, but not in remaining set
    append(SE_old, SE_res, Remains),
    % remaining system elements overall model + givens
    check_input_se(SE_old, Input_SE_org, Remains, NSE_old),
    % remove attribute relations not mentioning system elements
    % in Remains from overall list
    check_input_se(Input_SE_old, Input_SE_org, Remains, NewInputSE),
    % idem for input model list

    % tell classifier what we want to now:
    % all parameters & system elements
    % all results
    % all transition results
    !,
    (
      do_once((   add_givens([    parameters(P_old),
                    parameters(P_res),
                    system_elements(NSE_old),
                    system_elements(SE_res),
                    par_values(V_res),
                    par_relations(R_res),
                    par_relations(Input_R_old)
                    | TransitionResults ],
                Cin, Cin_I)
          ))
    ->
      etrace(reclass_apply_done, _, transition)
    ;
      etrace(reclass_apply_fail, _, transition),
      fail
    ),
    etrace(reclass_reclass_start, _, respecification),
    reclass(SS_old, SS_new, Cin_I, Cin_II),
    etrace(reclass_reclass_done, _, respecification),
    !,
    % make a new input model system element set
    smerge(SE_res, NewInputSE, NInput_SE),
    % append se results to input se (without conditions)

    depth_split(Name/IName/Input_R_old/NInput_SE/R_res, [],
            SS_new, Cin_II, NextState).

END Old version ***//*** Below is garp 2.0 version DEFAULT under algorithm option switch: ***/



% transition

/* FL: March 2004 Notes

- this new version uses the old to() structure
but with slightly different semantics,

in the terminations conditions are only filled with statements that need
to be replaced: only changing statements (values & relations),

Maybe later domain specific rules may be added, these having all conditions
as before. Therefore the procedure is changed very little.

Also in complex ordering procedures certain statements will have to be kept
stable and therefore will appear in both conditions and results.

- Continuity is done in a more consequent way:
derivatives AND derivative RELATIONS are given 1 degree of freedom:
thus ensuring continuity without blocking any movement.

- relations between landmarks derived from the previous state and thus
defining the particular world being modeled in this branch are added
to the input model (see thesis on www)
Question is whether they should be 'quiet' constraints in the Cio or explicit
ones appearing in the SMD. Adding these is controlled by an algorithm option
switch and they are added to the smd.

- Exogenous quantities get a special treatment, their derivatives change explicitly
through terminations.e

*/

depth(  To, SMD_old, NextState):-
    % flag(use_old_transition_rules, Flag, Flag),
    % algorithm_assumption_flag(Flag, fail, use_old_transition_rules),

    To = to(cause(Causes),
        conditions(ConditionsALL),
        results(ResultsALL),
        to_state(_States), _),

    etrace(reclass_apply, Causes, transition),

        % REPAIR: simplify_transition 21/08/2001 by BB (see below for explanation)
        % remove SE items present in both Conditions and Givens from both lists
    simplify_transition( ConditionsALL, Conditions, ResultsALL, Results ),

    % remove termination conditions from total set and input model

    remove_from_smd(Conditions, SMD_old, SMD_old_I),
    smd_slots(SMD_old_I, Name, SE_old, P_old, V_old, R_old, SS_old, Input_old),
    % get old input model system elements (before removing conditions)
    smd_slots(Input_old, _, Input_SE_org, _, _, _, _, _),
    remove_from_smd(Conditions, Input_old, Input_old_I),
    smd_slots(Input_old_I, IName, Input_SE_old, _, _, Input_R_oldC, _, _),

    clean_up_depth,
    cio_empty(Cin),

    % just a way to remove results that don't specify parameters, values etc
    smd_slots(Dummy, _, [], [], [], [], [], _),
    add_to_smd(Results, Dummy, Dummy_I),
    smd_slots(Dummy_I, _, SE_res, P_res, V_res1, R_res, _, _),

% new garp 2.0 specific FL May 2004
    % continuity for not changing values:
    % (not needed for relations, they are discarded)
    % but exogenous free & continuous hold their derivative!
    % fl new june 07: also parabola pos and neg hold their derivative
    get_store(SMD_old, Store),
    store_ec(Store, EC),
    store_ef(Store, EF),
    store_pp(Store, PP),
    store_np(Store, NP),
    append(PP, NP, EX1),
    append(EC, EF, EX2),
    append(EX1, EX2, EX),
    split_off_exogenous(V_old, EX, V_oldNormal, V_oldExogenous), % cut them off
    store_cd(Store, CD), % constant derivatives should not be let free...
    %NEW: FL april 2007: second order derivatives (from previous state), constrain derivatives
    store_sod(Store, SOD),
    %store_ov(Store, OV), %old values from precursor of previous state %Not necessary for postfilter!
    determine_epsilon_type(Causes, Etype),
    do_value_continuity(V_oldNormal, CD, SOD, V_oldNormalNV, V_oldNR, Etype),
    append(V_oldNormalNV, V_oldExogenous, V_oldNV), % put them back
    append([par_values(V_oldNV)], [par_relations(V_oldNR)], TransitionResults),

    % continuity for the input system:
    % only relations (values are discarded)
    store_cr(Store, CR),
    subtract(Input_R_oldC, CR, NotConstant),
    subtract(Input_R_oldC, NotConstant, Constant),
    do_relation_continuity(NotConstant, ContinuousNC, Etype),
    append(Constant, ContinuousNC, Input_R_old),

    % undo continuity in results for free exogenous parameters:
    % they should change derivative through terminations
    % new fl june 07: idem for parabolas (and sinus!) (FL aug 07 NOT TRUE!! only free... )
    undo_continuity_exogenous(V_res1, EF, R_res, V_res),
% end new garp 2.0 specific


% patch: april 2005 FL:
% in a model with assumed modelfragments discontiuous changes of relations are
% possible because relations are discarded in the transitionstep.
% They are checked added for continuity in this patch: not to change too much
% only simple quantity- relations are processed.

    patch_relation_continuity(R_old, R_patch),

% endpatch

    % remove systemelement attribute relations for system elements
    % mentioned in original input model, but not in remaining set
    append(SE_old, SE_res, Remains),
    % remaining system elements overall model + givens
    check_input_se(SE_old, Input_SE_org, Remains, NSE_old),
    % remove attribute relations not mentioning system elements
    % in Remains from overall list
    check_input_se(Input_SE_old, Input_SE_org, Remains, NewInputSE),
    % idem for input model list

    % tell classifier what we want to now:
    % all parameters & system elements
    % all results
    % all transition results
    !,
    (
      do_once((   add_givens([    parameters(P_old),
                    parameters(P_res),
                    system_elements(NSE_old),
                    system_elements(SE_res),
                    par_values(V_res),
                    par_relations(R_res),
                    par_relations(Input_R_old),
                    par_relations(R_patch) % patch april 2005
                    | TransitionResults ],
                Cin, Cin_I)
          ))
    ->
      etrace(reclass_apply_done, _, transition)
    ;
      etrace(reclass_apply_fail, _, transition),
      fail
    ),
    etrace(reclass_reclass_start, _, respecification),
    reclass(SS_old, SS_new, Cin_I, Cin_II),
    etrace(reclass_reclass_done, _, respecification),
    !,
    % make a new input model system element set
    smerge(SE_res, NewInputSE, NInput_SE),
    % append se results to input se (without conditions)

    constrain_assumed_derivative_terminations(Causes, SODConstraints),
    !,
    depth_split(Name/IName/Input_R_old/NInput_SE/R_res, SOD, SODConstraints,
	    V_old, SS_new, Cin_II, NextState).


% if an ambiguous derivative is assumed to change then in the next state
% the derivative should not have a definitive change back because of
% 2nd order derivative information. If the 2nd der in the new state
% is such the assumed termination was wrong and should fail.
%
constrain_assumed_derivative_terminations(Causes, SODconstraints):-
	findall(C, (member(X, Causes), constrain_a_d_t(X, C)), SODconstraints).

constrain_a_d_t(assumed_derivative_up_to_stable(Par), pos/Par):-!.
constrain_a_d_t(assumed_derivative_down_to_stable(Par), neg/Par):-!.
constrain_a_d_t(assumed_derivative_stable_to_up(Par), neg/Par):-!.
constrain_a_d_t(assumed_derivative_stable_to_down(Par), pos/Par):-!.


% end new depth/3 garp 2.0 version


% depth/3 finishes with a call for depth_split/4
% This represents a split in the depth first search, (garp 1.7.2 version)
% either existing states subsume the 'scenario' up to that point,
% or it leads to new states.
% problem is that when a 'scenario' has a set of successor states that is
% partially known then the new states in this set will not be found
%
% In garp 2.0 this split is different:
% all successor states for the 'scenario' are specified and only then are
% these matched with existing states, thus resulting in transitions to new
% and existing states. This way it is ensured that all possible successors
% are found. Also continuity is implemented more strict by matching fully
% specified states.
% The old subsumption style approach can still be used: algorithm option switch

% depth split - find one or more existing states OR specify new state(s)

/* FL Aug 07 Subsumption option is now permanently removed,
% garp 1.7: Subsumption
depth_split(_, _Sod, SS, Cin, NextState):-
    flag(no_subsumption_specify_all, Flag, Flag), % new
    algorithm_assumption_flag(Flag, fail, no_subsumption_specify_all),        % new

    % if there is an existing state, don't backtrack

    % instantiate values from inequality relations
    get_derivable_values(Cin, Cnew),

    % get all parameters that still apply to some system element
    cio_se(Cnew, SE),   % system elements
    % system elements here in 'new' list
    % (in old list after specification)
    cio_p(Cnew, P),     % all parameters
    parameters_apply_to_system_elements(P, SE, UsableP),

    % get all values corresponding to a parameter that's still there
    cio_v_nv(Cnew, CnewI, V, UsableV),
    collect_corresponding_values(UsableP, V, UsableV),

    % system structure values are not bound to values in value list
    % because backtracking on assumptions isn't prolog backtracking
    bind_system_structure_values(SS, CnewI),

    % new garp 2.0 specific
    determine_exogenous_derivatives(CnewI, SS, CnewII),
    % endnew

    findall(State,
         existing_state(SS, CnewII, State),
         States),
    States \= [],
    !,
    member(NextState, States),  % return one by one
    etrace(reclass_trans_to, [NextState], transition).



% garp 1.7: specify new state(s)

depth_split(Name/InputName/InputRelations/InputElements/ResultRelations, OldSod,
        SS, Cin, NextState) :-

    flag(no_subsumption_specify_all, Flag1, Flag1), %new
    algorithm_assumption_flag(Flag1, fail, no_subsumption_specify_all), %new

    % get children for system structures left from the previous state
    % put them on hypotheses list
    etrace(reclass_search_children, _, specification),
    get_child_hypotheses(SS, Cin, Cin1),
    etrace(reclass_search_children_done, _, specification),

    % finish specification
    etrace(reclass_class_start, _, specification),
    do_action(class(Cin1, Cout1), cio_result(Cout1)),
    etrace(reclass_class_done, _, specification),

    % no illegal given found
    \+ (cio_empty(Cout1), etrace(reclass_cio_empty,_,general)),

    % system structures found now and from previous state
    cio_sf(Cout1, SS2),
    append(SS, SS2, Structures),

    bind_system_structure_values(Structures, Cout1),

    % write derivable relations to screen for debugging
    etrace(reclass_print_der, Cout1, derivable),

    % new garp 2.0 specific,
    determine_exogenous_derivatives(Cout1, Structures, Cout1b),
    % endnew

    % inspect influences and proportional relations
    etrace(reclass_ir_start, _, specification),
    resolve_influence_proportional(Cout1b, Cout2, OldSod, SecondOrderDerivatives),
    etrace(reclass_ir_done, _, specification),

    % write derivable relations to screen for debugging
    etrace(reclass_print_der, Cout2, derivable),

    % instantiate values if not done (from derivable relations)
    get_derivable_values(Cout2, Cout3),

    % get elements and relations in system structures
    collect_smd_elements_from_system_structures(Structures, _SE_ss, P_ss, _, R_ss),

    % get all parameters that still apply to some system element
    % or should it be: still occur in some system structure?

    cio_oe(Cout3, SE),  % all system elements (in 'old' list) (FL: that is the 'used' list)
    cio_p(Cout3, P),    % all parameters
    parameters_apply_to_system_elements(P, SE, UsableP1),

    % new FL July 2004: remove parameters not mentioned in SS, in QPT a quantity associated with a process disappears when the process stops, see thesis on www
    % under algorithm option switch control
    parameters_match_system_structures(UsableP1, P_ss, UsableP),

    % get all inequality relations from termination rules
    % between parameters which are not in R_ss (system structures) and
    % still aply to a parameter in UsableP: add to input model

    inequality_between_values(ResultRelations, ResultValRel),
    without(ResultValRel, R_ss, NewInputRelations_I),
    smerge(NewInputRelations_I, InputRelations, NewInputRelations_II),
    without(P, UsableP, DiscardP),  % parameters we don't use
    relations_not_apply_to_parameters(NewInputRelations_II,
                DiscardP, NewInputRelations_IIIA),

    % get all values corresponding to a parameter that's still there
    cio_v(Cout3, V),
    collect_corresponding_values(UsableP, V, UsableV),

    % create new smd

    % new garp 2.0 specific
    % Store important aspects of a state seperately (not user inspectable)
    % in a procedure friendly way: list of constants, list of exogenous quantities, etc.
    etrace(reclass_make_store, _, transition),
    make_smd_cio_store(ResultRelations, R_ss, Structures, NewInputRelations_IIIA, Cout3, SecondOrderDerivatives,
		       Store, NewLandmarkRels),
    flag(use_landmarks, Flag2, Flagno_subsumption_specify_all2),
    (
    algorithm_assumption_flag(Flag2, true, use_landmarks) ->
    (smerge(NewInputRelations_IIIA, NewLandmarkRels, NewInputRelations_III),
    etrace(reclass_landmark_der, [NewLandmarkRels], transition));
    NewInputRelations_IIIA = NewInputRelations_III
    ),
    % endnew

    smd_slots(NewInput, InputName, InputElements, [], [],
            NewInputRelations_III, [], Store), % FL: replaced: '_'  with 'Store' to pass cio to state for termination reference
    smd_slots(NewSMD_I, Name, SE, UsableP, UsableV, R_ss, Structures,
            NewInput),
    add_to_smd([par_relations(NewInputRelations_III), system_elements(
        InputElements)], NewSMD_I, NewSMD),

    make_new_state(NextState, interpreted, [], [], NewSMD),
    etrace(reclass_show_newstate, NextState, transition).


 FL Aug 07 END OLD SUBSUMPTIONCODE */

% garp 2.0:
% depth split without subsumption
% specify state(s): then match with existing, or add as new

/* FL May 2004 Notes:

A newly specified succesor matches an existing smd (state) iff:

- all modelfragments are equal
- all system_elements are equal
- all values match (no variable-instantiated matching though, both (un)bound)
  NB if all values match all parameters are equal,
- the relations match:
    a) landmark relations have to be equal.
       (subject to introduction through derivation)
    b) binairy parameter-parameter relations have to be equal
       (subject to change through transitions)
    c) derivative relations (continuity) have to be consistent
       (old ones added to new specificated cio, thus using solve engine)
    All other relations cannot be different because either the MF's impose them,
    or they follow from the input system and: only b) and c) category relations
    can change, others remain present and constant.
*/

depth_split(Name/InputName/InputRelations/InputElements/ResultRelations, OldSod, SodConstraints,
        V_old, SS, Cin, NextState) :-

    % flag(no_subsumption_specify_all, Flag1, Flag1),
    % algorithm_assumption_flag(Flag1, true, no_subsumption_specify_all),

    % get children for system structures left from the previous state
    % put them on hypotheses list
    etrace(reclass_search_children, _, specification),
    get_child_hypotheses(SS, Cin, Cin1),
    etrace(reclass_search_children_done, _, specification),

    % finish specification
    etrace(reclass_class_start, _, specification),
    do_action(class(Cin1, Cout1), cio_result(Cout1)),
    etrace(reclass_class_done, _, specification),

    % no illegal given found
    %\+ cio_empty(Cout1),
    \+ (cio_empty(Cout1), etrace(reclass_cio_empty,_,general)),

    value_branching(Cout1, InputRelations, Cout1a),

    % system structures found now and from previous state
    cio_sf(Cout1a, SS2),
    append(SS, SS2, Structures),

    bind_system_structure_values(Structures, Cout1a),

    % write derivable relations to screen for debugging
    etrace(reclass_print_der, Cout1a, derivable),

    % new garp 2.0 specific
    determine_exogenous_derivatives(Cout1a, Structures, Cout1b),
    % endnew

    % inspect influences and proportional relationsresolve_influence_proportional
    etrace(reclass_ir_start, _, specification),
    resolve_influence_proportional(Cout1b, Cout2, OldSod, SodConstraints, V_old, SecondOrderDerivatives, AmbiguousDer),
    etrace(reclass_ir_done, _, specification),

    % write derivable relations to screen for debugging
    etrace(reclass_print_der, Cout2, derivable),

    % instantiate values if not done (from derivable relations)
    get_derivable_values(Cout2, Cout3),

    % get elements and relations in system structures
    collect_smd_elements_from_system_structures(Structures, _SE_ss, P_ss, _, R_ss),

    % get all parameters that still apply to some system element
    % or should it be: still occur in some system structure?

    cio_oe(Cout3, SE),  % all system elements (in 'old' list)
    cio_p(Cout3, P),    % all parameters
    parameters_apply_to_system_elements(P, SE, UsableP1),

    % get all inequality relations from termination rules
    % between parameters which are not in R_ss (system structures) and
    % still aply to a parameter in UsableP: add to input model

    %new FL July 2004: remove parameters not mentioned in SS, in QPT a quantity associated with a process disappears when the process stops, see thesis on www.
    %under switch control
    parameters_match_system_structures(UsableP1, P_ss, UsableP),

    inequality_between_values(ResultRelations, ResultValRel),
    without(ResultValRel, R_ss, NewInputRelations_I),
    smerge(NewInputRelations_I, InputRelations, NewInputRelations_II),
    without(P, UsableP, DiscardP),  % parameters we don't use
    relations_not_apply_to_parameters(NewInputRelations_II,
                DiscardP, NewInputRelations_IIIA),

    % get all values corresponding to a parameter that's still there

    cio_v(Cout3, V),
    collect_corresponding_values(UsableP, V, UsableV),

    % create new smd

    % new garp 2.0 specific
    % Store important aspects of a state seperately (not user inspectable)
    % in a procedure friendly way: list of constants, list of exogenous quantities, etc.
    % tempflo: etrace(reclass_make_store, _, transition),
    make_smd_cio_store(ResultRelations, R_ss, Structures, NewInputRelations_IIIA, Cout3, SecondOrderDerivatives,
		       AmbiguousDer, Store, NewLandmarkRels),
    flag(use_landmarks, Flag2, Flag2),
    (
    algorithm_assumption_flag(Flag2, true, use_landmarks) ->
    (smerge(NewInputRelations_IIIA, NewLandmarkRels, NewInputRelations_III),
     etrace(reclass_landmark_der, [NewLandmarkRels], transition));
    NewInputRelations_IIIA = NewInputRelations_III
    ),
    % endnew

    smd_slots(NewInput, InputName, InputElements, [], [],
            NewInputRelations_III, [], Store), %FL: replaced: '_'  with 'Store' to pass cio to state for termination reference
    smd_slots(NewSMD_I, Name, SE, UsableP, UsableV, R_ss, Structures,
            NewInput),
    add_to_smd([par_relations(NewInputRelations_III), system_elements(
        InputElements)], NewSMD_I, NewSMD),

    % specification finshed: match to existing or save as new:
    etrace(reclass_match_or_add, _, transition),
    match_or_add_state(NextState, NewSMD, Cout3).


% FL July 2004: remove parameters not mentioned in SS
% under algorithm option switch control,
% In QPT quantities associated with processes disappear when processes stop (disappear),
% this option allows for both modeling styles:
% describing or not describing inactive processes
% all parameters/quantities are simply matched with the active model fragments,
% if not mentioned in an active model fragment the parameter is removed

% option switch off
parameters_match_system_structures(P, _, P):-
    flag(remove_inactive_quantities, RIQ, RIQ),
    algorithm_assumption_flag(RIQ, fail, remove_inactive_quantities),
    !.

% option switch on
parameters_match_system_structures(PIn, P_ss, POut):-
    flag(remove_inactive_quantities, RIQ, RIQ),
    algorithm_assumption_flag(RIQ, true, remove_inactive_quantities),
    parameters_match_system_structures2(PIn, P_ss, POut).

% done
parameters_match_system_structures2([], _, []).

% parameter is mentioned in ss: keep
parameters_match_system_structures2([H|T], P_ss, [H|NT]):-
    memberchk(H, P_ss),
    !,
    parameters_match_system_structures2(T, P_ss, NT).

%parameter not in ss: discard
parameters_match_system_structures2([_|T], P_ss, NT):-
    parameters_match_system_structures2(T, P_ss, NT).


% match_or_add_state(-StateNumber, +SpecifiedSMD, +AccompanyingCIO)
% Try to match the smd with existing states, if no match is found
% the state is saved as new

% match or add: transition to existing state
match_or_add_state(NextState, SMD, Cio):-
    findall(State,
         match_state_smd(SMD, State, Cio),
         States),
    States \= [],
    length(States, L),
    (L > 1 -> etrace(reclass_equal_state_error, _, general) ; true),
    !,
    States = [NextState],   % return single matching state
    etrace(reclass_trans_to, [NextState], transition).

% match or add: transition to new state
match_or_add_state(NextState, SMD, _):-
    make_new_state(NextState, interpreted, [], [], SMD),
    etrace(reclass_show_newstate, NextState, transition).


% match_state_smd(+SMD, -State, +Cio)
% match smd's, succeed if
% equal system elements
% equal modelfragments
% equal values (and therefore parameters):
%   both values  must be bound or both must be variables,
%   same for derivatives
% equal relations:
%   landmark-, binairy parameter-parameter- and continuity relations are selected
%   and all treated seperately, any other relations are not checked, they follow from
%   modelfragments and or stem from the inputscenario and are constant.

% FL 23-4-2007
% small speedfix: value check is most common cause of no-match
% therefore we do this first.
%
/* OLD new below, first checking statevalues through small predicate...
match_state_smd(NewSMD, State, NewCio):-
    state(State, OldSMD),
    etrace(reclass_match_state, State, transition),
    smd_slots(OldSMD, _, Old_SE, _, Old_V, Old_R, Old_SS, _),
    smd_slots(NewSMD, _, New_SE, _, New_V, New_R, New_SS, _),
    etrace(reclass_match_val, _, transition),
    match_values(Old_V, New_V),
    etrace(reclass_match_mf, _, transition),
    match_lists(Old_SS, New_SS),
    etrace(reclass_match_se, _, transition),
    match_lists(Old_SE, New_SE),
    % last (most work)
    etrace(reclass_match_rel, _, transition),
    get_store(OldSMD, Store),
    store_cio(Store, OldCio),
    store_con(Store, Continuity),
    match_relations(Old_R, Old_V, New_R, NewCio, OldCio, Continuity),
    !.
*/

% new FL june 07 first checking state values by picking up smaller predicate
match_state_smd(NewSMD, State, NewCio):-
    smd_slots(NewSMD, _, New_SE, _, New_V, New_R, New_SS, _),
    state_values(State, Values),
    etrace(reclass_match_state, State, transition),
    etrace(reclass_match_val, _, transition),
    match_values(Values, New_V),
    %at this point,  the values are ok, get big predicate with all info
    state(State, OldSMD),
    smd_slots(OldSMD, _, Old_SE, _, Old_V, Old_R, Old_SS, _),
    %match_values(Old_V, New_V), % not needed anymore
    etrace(reclass_match_se, _, transition),
    match_lists(Old_SE, New_SE),
    etrace(reclass_match_mf, _, transition),
    match_lists(Old_SS, New_SS),
    % last (most work)
    etrace(reclass_match_rel, _, transition),
    get_store(OldSMD, Store),
    store_cio(Store, OldCio),
    store_con(Store, Continuity),
    match_relations(Old_R, Old_V, New_R, NewCio, OldCio, Continuity),
    !.


% match_lists(+List1, +List2)
% checks if two lists have equal members (could also be done using sort/3 and =/2)
% should be in pl-lib

% equal lists: simple match (also for [], [] stopclause)
match_lists(List, List):-
    !.

match_lists([H|T], List2):-
    select(H, List2, New),
    match_lists(T, New),
    !.

/*
% match lists for relations using the alternative relation procedure
% e.g. smaller(x, y) matches greater(y, x)

match_relation_lists(List, List):-
    !.

match_relation_lists([H|T], List2):-
    select(H, List2, New),
    match_relation_lists(T, New),
    !.

match_relation_lists([H|T], List2):-
    alternative_relation(H, NH),
    select(NH, List2, New),
    match_relation_lists(T, New),
    !.
*/

% match lists for valuestatements: value(Par, Q, Val, Der)
% values & derivatives must be both unbound or both bound and equal.
% an unknown value does not match a known value
match_values(List, List):-
    !.

match_values([value(Par, _, Val1, Der1)|T], List2):-
    select(value(Par, _, Val2, Der2), List2, New),
    bound_unbound(Val1, Val2),
    bound_unbound(Der1, Der2),
    !,
    match_values(T, New),
    !.

% both defined
bound_unbound(A, B):-
    nonvar(A),
    !,
    nonvar(B),
    A = B.

% both variables A=B follows by definition
bound_unbound(A, B):-
    var(A),
    !,
    var(B).




% match_relations(+Old_R, +Values, +New_R, +NewCio, +OldCio, +Continuity)
% checks for every different type of relation in SMD's if they represent the same set.
% values have been matched earlier
% relations should be present, maybe in alternative form or directly derivable (x = 0, y = 0, then x=y)
% Continuity is the set of derivative relations stored seperately in the existing state,
% see make_smd_cio_store
% these continuity/derivative relations need only be consistent. This is not the strictest
% possible solution, but sufficient in general. A stricter implementation could produce
% indistinguishable states.

match_relations(Old_R, Values, New_R, NewCio, OldCio, OldContinuity):-
    split_lm_par_der_relations(Old_R, Values, LMRelsO, ParRelsO, DerRelsO),
    split_lm_par_der_relations(New_R, Values, LMRelsN, ParRelsN, _),
    match_relation_lists(LMRelsO, LMRelsN, OldCio, NewCio),
    !,
    match_relation_lists(ParRelsO, ParRelsN, OldCio, NewCio),
    !,
    etrace(reclass_match_continuity, _, transition),
    add_givens([par_relations(OldContinuity), par_relations(DerRelsO)], NewCio, _).
    % should be consistent for continuity


% match lists for relations,
% alternative relation is used:
% e.g. smaller(x, y) matches greater(y, x)
% still differing items should be derivable in opposite cio

% equal lists (also stopclause for [],[],_,_)
match_relation_lists(List, List, _, _):-
    !.

% more items in list2 left: check if derivable
match_relation_lists([], [H|T], Cio1, _):-
    !,
    % all leftover members of list2 are derivable in Cio1
    forall(member(X, [H|T]), (   intern_representation(X, Intern, Cio1, CioTest),
                                cio_d(CioTest, Der),
                                is_derivable(Intern, Der)
                             )).

% match head to element in list2
match_relation_lists([H|T], List2, Cio1, Cio2):-
    member(H, List2),
    !,
    subtract(List2, [H], New), % if multiple occurences take all
    match_relation_lists(T, New, Cio1, Cio2).

% match alternative relation of head with list2
match_relation_lists([H|T], List2, Cio1, Cio2):-
    alternative_relation(H, NH),
    member(NH, List2),
    !,
    subtract(List2, [H], New), % if multiple occurences take all
    match_relation_lists(T, New, Cio1, Cio2).

% unknown item should be derivable in other state's cio
match_relation_lists([H|T], List2, Cio1, Cio2):-
    intern_representation(H, Intern, Cio2, CioTest),
    cio_d(CioTest, Der),
    is_derivable(Intern, Der),
    match_relation_lists(T, List2, Cio1, Cio2).


% split relationset into:
% landmark relations
% binairy par/par relations
% binairy derivative relations (continuity)
% remove other relations: par/landmark relations are captured by matching values,
% non binairy relations, involving addition / subtraction are not
% terminating in garp 2.0 thus follow from the input system (stable)
% or the matching model fragments.

split_lm_par_der_relations([], _, [], [], []).

% derivative relation
split_lm_par_der_relations([Rel|Tail], Values, LMRels, ParRels, [Rel|DerRels]):-
    derivative_equality_type(Rel, Par1, Par2),
    (memberchk(value(Par1, _, _, _), Values) ; Par1 = zero),
    (memberchk(value(Par2, _, _, _), Values) ; Par2 = zero),
    !,
    split_lm_par_der_relations(Tail, Values, LMRels, ParRels, DerRels).

% par-par relation
split_lm_par_der_relations([Rel|Tail], Values, LMRels, [Rel|ParRels], DerRels):-
    par_equality_type(Rel, Par1, Par2),
    memberchk(value(Par1, _, _, _), Values),
    memberchk(value(Par2, _, _, _), Values),
    !,
    split_lm_par_der_relations(Tail, Values, LMRels, ParRels, DerRels).

% landmark relation
split_lm_par_der_relations([Rel|Tail], Values, [Rel|LMRels], ParRels, DerRels):-
    par_equality_type(Rel, Point1, Point2),
    is_landmark_extern(Point1, Values),
    is_landmark_extern(Point2, Values),
    !,
    split_lm_par_der_relations(Tail, Values, LMRels, ParRels, DerRels).

% other: remove
split_lm_par_der_relations([_|Tail], Values, LMRels, ParRels, DerRels):-
    split_lm_par_der_relations(Tail, Values, LMRels, ParRels, DerRels).


% a point is a landmark if it is an unary predicate with a parameter as argument,
% or zero of course. normal representation.
is_landmark_extern(zero, _).

is_landmark_extern(one, _).

is_landmark_extern(minusone, _).

is_landmark_extern(Point, Values):-
    Point =.. [_, Par],
    memberchk(value(Par, _, _, _), Values),
    !.

%these clauses really belong in types.pl

% par-par equality, return left & right
par_equality_type(equal(Par1, Par2), Par1, Par2).
par_equality_type(greater(Par1, Par2), Par1, Par2).
par_equality_type(smaller(Par1, Par2), Par1, Par2).
par_equality_type(greater_or_equal(Par1, Par2), Par1, Par2).
par_equality_type(smaller_or_equal(Par1, Par2), Par1, Par2).

% derivative equality, return left & right
derivative_equality_type(d_equal(Par1, Par2), Par1, Par2).
derivative_equality_type(d_greater(Par1, Par2), Par1, Par2).
derivative_equality_type(d_smaller(Par1, Par2), Par1, Par2).
derivative_equality_type(d_greater_or_equal(Par1, Par2), Par1, Par2).
derivative_equality_type(d_smaller_or_equal(Par1, Par2), Par1, Par2).

% END NEW garp 2.0 code for specify then match approach for depth_split/4



%-------------- make_smd_cio_store: -------------------------------------

/*

FL: September 2004,

In GARP 2.0 a number of features derivable from the user level representation
SMD are stored seperately with a state for easy referencing,
For instance, constant parameters and inequalities can be found in the model
fragments, but to do this every time is not efficient, therefore a list of
these is stored. The same goes for exogenous parameters.

In addition to such shorthand lists, the internal mathematical model is also
saved (in fact the whole cio structure)

For easy state matching (see garp 2.0 depth_split) the derivative relations
that are in fact continuity constraints are also seperately stored

Lastly, after state specification, newly derived landmarkrelations are gathered.
These can be said to describe the specific possible world subject to simulation.
The user can choose to add these extra constraints to the state description (smd)

The store is only for use by the garp simulator. In principle it contains no more
information than the smd, only easier acces.

NEW FL april 07: store second order derivatives: sod
new FL june 07: store exogenous pos and neg parabola
new FL mar 08: store ambiguous derivatives

The store is a structure of the following form:

store(
   cd - constant derivatives
   cv - constant values
   cr - constant relations
   ec - exogenous_sinus (formerly continuous (is ambiguous name))
   ef - exogenous_free
   ei - exogenous_increasing
   es - exogenous_steady
   ed - exogenous_decreasing
   pp - exogenous_pos_parabola
   np - exogenous_neg_parabola
   qt - QuantityTable
  con - Continuity relations extern
  cio - Cio
  lmr - LandMarkRelationsExtern
  sod - Second order derivatives
  amb - Ambiguous Derivatives List
     )

*/

% make_smd_cio_store(+ResultRelations, +ParRelations, +Structures,
%		     +InputRelations, +Cio,
%		     +SecondOrderDerivatives, -Store,
%		     -NewLandmarkRelations)
%
% Compose store, return it and new landmarkrelations

make_smd_cio_store(ResultRelations, ParRelations, Structures, InputRelations, Cio, SecondOrderDerivatives,
		   AmbiguousDer, Store, NewLandmarkRelations):-
    % construct store:
    store_empty(StoreIn),
    % add list of quantities:
    store_quantities(StoreIn, Cio, Snew3),
    % add cio:
    store_cio_ncio(Snew3, Snew4, _, Cio),
    % add constants & exogenous lists:
    store_assumptions(Snew4, Structures, Snew8),
    % extract newly derived landmark relations:
    smerge(ParRelations, InputRelations, AllRelations),
    get_smd_landmark_relations(AllRelations, KnownLMR),
    get_cio_landmark_relations(Cio, DerivableLMR),
    relevant_landmark_relations(DerivableLMR, RelevantLMR),
    new_landmark_relations(RelevantLMR, KnownLMR, NewLandmarkRelations),
    smerge(DerivableLMR, KnownLMR, LandmarkRelations),
    % add landmark relations:
    store_lmr_nlmr(Snew8, Snew9, [], LandmarkRelations),
    % add derivative relations:
    continuity_results(ResultRelations, Continuity),
    store_con_ncon(Snew9, Snew10, _, Continuity),
    store_sod_nsod(Snew10, Snew11, ExoSD, AllSecondOrderDerivatives),
    append(ExoSD, SecondOrderDerivatives, AllSecondOrderDerivatives),
    store_amb_namb(Snew11, Store, _, AmbiguousDer),
    !.


% store_assumptions(+StoreIn, +Structures, -StoreOut)
% extract different exogenous parameters from model fragments
% extract constant parameters, derivatives and relations
% store results

store_assumptions(StoreIn, Structures, StoreOut):-
    store_cio(StoreIn, Cio),
    cio_oe(Cio, SE),
    exogenous_from_structures(Structures, SE, Continuous1, Free1, Inc1, Steady1, Dec1, Pos1, Neg1),
    constant_from_structures(Structures, Values1, Relations1, Derivatives1),
    list_to_set(Continuous1, Continuous),
    list_to_set(Free1, Free),
    list_to_set(Inc1, Inc),
    list_to_set(Steady1, Steady),
    list_to_set(Dec1, Dec),
    list_to_set(Pos1, Pos),
    list_to_set(Neg1, Neg),
    list_to_set(Values1, Values),
    list_to_set(Derivatives1, Derivatives),
    list_to_set(Relations1, Relations),
    store_new_dvrcfisdpn(StoreIn, StoreIn1, Derivatives, Values, Relations, Continuous, Free, Inc, Steady, Dec, Pos, Neg),
    determine_exo_2nd_der(StoreIn1, StoreOut, Inc, Steady, Dec,
			  Pos, Neg, Free, Continuous).


%------- Exogenous ---------------

% exogenous_from_structures(+MF's, +SysElements, -ContinuousExogenous, -etc)
% collect all different types of exogenous parameters from the model fragments

% done
exogenous_from_structures([], _, [], [], [], [], [], [], []).

% exogenous defining modelfragment
exogenous_from_structures([H|T], SE, Continuous, Free, Inc, Steady, Dec, Pos, Neg):-
    % H is an exogenous modelfragment defining Par to be of Type (see methods.pl)
    is_exogenous_mf(H, SE, Type, Par),
    % do tail
    exogenous_from_structures(T, SE, CT, FT, IT, ST, DT, PT, NT),
    % put Par in correct list
    insert_exogenous_type(Type, Par, CT, FT, IT, ST, DT, PT, NT,
                          Continuous, Free, Inc, Steady, Dec, Pos, Neg).

% non exogenous defining modelfragment
exogenous_from_structures([_|T], SE, Continuous, Free, Inc, Steady, Dec, Pos, Neg):-
    exogenous_from_structures(T, SE, Continuous, Free, Inc, Steady, Dec, Pos, Neg).


%update list of correct type
insert_exogenous_type(exogenous_sinus, Par, C, F, I, S, D, P, N, [Par|C], F, I, S, D, P, N).
insert_exogenous_type(exogenous_free, Par, C, F, I, S, D, P, N, C, [Par|F], I, S, D, P, N).
insert_exogenous_type(exogenous_increasing, Par, C, F, I, S, D, P, N, C, F, [Par|I], S, D, P, N).
insert_exogenous_type(exogenous_steady, Par, C, F, I, S, D, P, N, C, F, I, [Par|S], D, P, N).
insert_exogenous_type(exogenous_decreasing, Par, C, F, I, S, D, P, N, C, F, I, S, [Par|D], P, N).
insert_exogenous_type(exogenous_pos_parabola, Par, C, F, I, S, D, P, N, C, F, I, S, D, [Par|P], N).
insert_exogenous_type(exogenous_neg_parabola, Par, C, F, I, S, D, P, N, C, F, I, S, D, P, [Par|N]).


%------- Constants ----------------

% constant_from_structures(+MF's, -ConstantValues, -ConstantRelations,
%                          -ConstantDerivatives)
% collect all constant values, relations and derivatives from system structures
% return parameter names of constant values/derivatives and full constant relations

% done
constant_from_structures([], [], [], []).

% constant model fragment
constant_from_structures([H|T], Values, Relations, Derivatives):-
    %H contains a constant label:
    constant_model_fragment(H, Givens, UnBoundGivens),
    !,
    memberchk(par_values(UBValues), UnBoundGivens),
    % get parameternames for values and derivatives in givens from original mf. (otherwise later derived information can illegally become constant)
    parameternames_constant_val_der(UBValues, GV, GD),
    % get given par_relations from givens
    given_relations(Givens, GR),
    % do tail:
    constant_from_structures(T, VT, RT, DT),
    append(GV, VT, Values),
    append(GR, RT, Relations),
    append(GD, DT, Derivatives).

% non constant model fragment
constant_from_structures([_|T], Values, Relations, Derivatives):-
    constant_from_structures(T, Values, Relations, Derivatives).


% there is an assumption of class constant present, return (original) givens
constant_model_fragment(MF, Givens, UnBoundGivens):-
    system_structure_slots(MF, _, _, Conditions, Givens),
    memberchk(system_elements(SE), Conditions),
    member(instance(_, Class), SE),
    isa_instance(Class, constant),
    !,
    revalidate_system_structure(MF, NMF),
    system_structure_slots(NMF, _, _, _, UnBoundGivens).


% select all relation statements from givens
% this clause assumes only one set of par_relations present in givens
given_relations(Givens, Relations):-
    memberchk(par_relations(Relations), Givens).


% parameternames_constant_val_der(+UnboundValuestatements, -ConstantParNames, -ConstantDerivativeParNames)
% For every value in UnboundValuestatements:
% if value is present or derivative is present and zero then add parametername to constant values list
% if derivative is present, add parametername to constant derivatives list.
parameternames_constant_val_der([], [], []).

% Val = var, Der = var: none constant
parameternames_constant_val_der([value(_, _, Val, Der)|T], VT, DT):-
    var(Der),
    var(Val),
    !,
    parameternames_constant_val_der(T, VT, DT).

% Der and Val defined: constant value & constant derivative
parameternames_constant_val_der([value(Par, _, Val, Der)|T], [Par|VT], [Par|DT]):-
    nonvar(Der),
    nonvar(Val),
    !,
    parameternames_constant_val_der(T, VT, DT).

% special: Der = zero: constant value & constant derivative
parameternames_constant_val_der([value(Par, _, _, Der)|T], [Par|VT], [Par|DT]):-
    nonvar(Der),
    Der = zero,
    !,
    parameternames_constant_val_der(T, VT, DT).

% Der = defined plus or min: constant derivative
parameternames_constant_val_der([value(Par, _, _, Der)|T], VT, [Par|DT]):-
    nonvar(Der),
    !,
    parameternames_constant_val_der(T, VT, DT).

% Val = defined: constant value
parameternames_constant_val_der([value(Par, _, Val, _)|T], [Par|VT], DT):-
    nonvar(Val),
    !,
    parameternames_constant_val_der(T, VT, DT).


/*****	Some Exogenous have 2nd order derivatives...  *****/
%
%

determine_exo_2nd_der(StoreIn, StoreOut, Inc, Steady, Dec,
		      Pos, Neg, Free, Continuous):-
	store_cio(StoreIn, Cio),
	cio_q_d(Cio, QList, Derivable),
	det_exo_2nd_der(Inc, QList, Derivable, DDI),
	det_exo_2nd_der(Steady, QList, Derivable, DDS),
	det_exo_2nd_der(Dec, QList, Derivable, DDD),
	det_exo_2nd_der(Pos, QList, Derivable, DDP),
	det_exo_2nd_der(Neg, QList, Derivable, DDN),
	det_exo_2nd_der(Free, QList, Derivable, DDF),
	det_exo_2nd_der(Continuous, QList, Derivable, DDC),
	append(DDI, DDS, DD1),
	append(DDD, DD1, DD2),
	append(DDP, DD2, DD3),
	append(DDN, DD3, DD4),
	append(DDF, DD4, DD5),
	append(DDC, DD5, ExoSecondDer),
	store_sod_nsod(StoreIn, StoreOut, _, ExoSecondDer).

det_exo_2nd_der([], _, _, []).
det_exo_2nd_der([Par|Tail], QList, Derivable, [hod_info(Par, nil, DDer, nil, [], [], [])|SOD]):- %TODO add Der and 3rd Der flexibly (now nil)
	get_second_derivative_sign(second_derivative(Par), QList, Derivable, DDer),
	!,
	det_exo_2nd_der(Tail, QList, Derivable, SOD).

% no sod for par
det_exo_2nd_der([_Par|Tail], QList, Derivable, SOD):-
	det_exo_2nd_der(Tail, QList, Derivable, SOD).

% get second derivative
get_second_derivative_sign(Qty, QList, Derivable, Der):-
    memberchk(Qty/I, QList),
    list_map([I], DerQty),
    list_map([0], Zero),
    get_sd_sign(DerQty, Zero, Derivable, Der),
    !.

get_sd_sign(Zero, Zero, _Relations, zero):- !.

get_sd_sign(Quantity, Zero, Relations, Sign):-
	member(relation(Quantity, Relation, Zero), Relations),
	sd_sign(Relation, Sign),
	!.

get_sd_sign(Quantity, Zero, Relations, Sign):-
	member(relation(Zero, Relation, Quantity), Relations),
	inverse(Relation, Inverse),
	sd_sign(Inverse, Sign),
	!.

sd_sign(>, pos).
sd_sign(>=, pos_zero).
sd_sign(=, zero).
sd_sign(<, neg).
sd_sign(=<, neg_zero).

/*-------------------continuity results ---------------*/

% matching fully specified states requires checking consistency of
% the mathematical models. Most relations follow from model fragments,
% but continuity constraints (derivative relations) can differ and are only
% found in the result relations and not in the smd therefore, extract these
% for testing purposes

% continuity_results(+Resultrelations, -DerivativeRelations)
% extract all relations about derivatives...
continuity_results([], []).

% derivative relations
continuity_results([Rel|Tail], [Rel|NewTail]):-
    derivative_equality_type(Rel, _, _),
    !,
    continuity_results(Tail, NewTail).

% other
continuity_results([_|Tail], NewTail):-
    continuity_results(Tail, NewTail).

%--------Quantity table --------------------------------
% store quantities in intern representation
store_quantities(StoreIn, Cio, StoreOut):-
    store_qt_nqt(StoreIn, StoreOut, _, QT),
    cio_q(Cio, Q),
    map_quantities(Q, QT).


map_quantities([], []).

map_quantities([Q/I|T], [Q/M|QT]):-
    list_map([I], M),
    map_quantities(T, QT).


%---------------- landmark relations ------------------

% get_smd_landmark_relations(+AllRelations, -LandmarkRelations)
% get normal representation landmarkrelations from smd

% done
get_smd_landmark_relations([], []).

% H is lmrel
get_smd_landmark_relations([H|T], [H|NT]):-
    % parse and check if every argument = landmark
    is_normal_landmark_relation(H),
    !,
    get_smd_landmark_relations(T, NT).

% skip other
get_smd_landmark_relations([_|T],  NT):-
    get_smd_landmark_relations(T, NT).


% parse Relation & check for landmarks (ALL arguments)
is_normal_landmark_relation(Relation):-
    parse_normal_relation(Relation, Parameters),
    forall(member(P, Parameters), is_landmark(value(P))).

parse_normal_relation(greater(Left, Right), Parameters):-
    !,
    normal_expression(Left, PL),
    normal_expression(Right, PR),
    append(PL, PR, Parameters), !.

parse_normal_relation(smaller(Left, Right), Parameters):-
    !,
    normal_expression(Left, PL),
    normal_expression(Right, PR),
    append(PL, PR, Parameters), !.

parse_normal_relation(equal(Left, Right), Parameters):-
    !,
    normal_expression(Left, PL),
    normal_expression(Right, PR),
    append(PL, PR, Parameters), !.

parse_normal_relation(smaller_or_equal(Left, Right), Parameters):-
    !,
    normal_expression(Left, PL),
    normal_expression(Right, PR),
    append(PL, PR, Parameters), !.

parse_normal_relation(greater_or_equal(Left, Right), Parameters):-
    !,
    normal_expression(Left, PL),
    normal_expression(Right, PR),
    append(PL, PR, Parameters), !.

normal_expression(plus(L, R), P):-
    !,
    normal_expression(L, PL),
    normal_expression(R, PR),
    append(PL, PR, P), !.

normal_expression(min(L, R), P):-
    !,
    normal_expression(L, PL),
    normal_expression(R, PR),
    append(PL, PR, P), !.

normal_expression(P, [P]).

% get_cio_landmark_relations(+Cio, -DerivableLMRels)
% newly derived landmarkrelations will of course appear in the
% internal mathematical model,
% get derivable landmarkrelations from cio, translate to normal

get_cio_landmark_relations(Cio, LandmarkRelations):-
    cio_q(Cio, Q),
    cio_d(Cio, Derivable),
    % determine landmarks
    findall(X/I, (member(X/I, Q), is_landmark(X)), Landmarks),
    findall(Relation, possible_lmr(Derivable, Landmarks, Relation), LandmarkRelations1),
    %  lm equalities:
    get_landmark_equalities(Landmarks, LandmarkRelations2),
    smerge(LandmarkRelations1, LandmarkRelations2, LandmarkRelations).

possible_lmr(Derivable, Landmarks, LMR):-
	member(Internal, Derivable),
	Internal = relation(Map1, Symbol, Map2),
	map_list(Map1, [I1]),
	map_list(Map2, [I2]),
	(member(value(X1)/I1, Landmarks) ; (I1 == 0, X1 = zero)),
	(member(value(X2)/I2, Landmarks) ; (I2 == 0, X2 = zero)),
	translate_extern(Symbol, Rel),
	LMR =.. [Rel, X1, X2].


translate_extern(=, equal).
translate_extern(>=, greater_or_equal).
translate_extern(>, greater).

/*
% Old: mathematically complete, but not exhaustive... A > B, A = C
% includes: C > B which is not given here...
% get_cio_landmark_relations(+Cio, -DerivableLMRels) newly derived
% landmarkrelations will of course appear in the internal mathematical
% model, get derivable landmarkrelations from cio, translate to normal

get_cio_landmark_relations(Cio, LandmarkRelations):-
    cio_q(Cio, Q),
    cio_d(Cio, Derivable),
    %  determine landmarks
    findall(X/I, (member(X/I, Q), is_landmark(X)), Landmarks),
    list_map([0], Zero),
    %  lm inequalities:
    get_landmark_relations(Derivable, Zero, Landmarks,
    LandmarkRelations1), % lm equalities:
    get_landmark_equalities(Landmarks, LandmarkRelations2),
    smerge(LandmarkRelations1, LandmarkRelations2, LandmarkRelations).
*/

is_landmark(value(X)):-
    X =.. [_, Par],
    qspace(Par, _, List, _),
    memberchk(point(X), List).

% get_landmark_relations(+Derivable, +ZeroMap, +LandMarkSet, -LMRelations)
% get all landmark inequalities from derivable
% (NB equalities are not in derivable because of analyse simple equality they just share equal pointers)

% done
get_landmark_relations([], _, _, []).

% lm relation
get_landmark_relations([relation(Left, Rel, Right)|TDerivable], Zero, Landmarks, [LandmarkRelation|Relations]):-
    is_landmark_intern(Left, Zero, Landmarks),
    is_landmark_intern(Right, Zero, Landmarks),
    !,
    extern_representation(relation(Left, Rel, Right), Landmarks, LandmarkRelation),
    get_landmark_relations(TDerivable, Zero, Landmarks, Relations).

% other
get_landmark_relations([_|TDerivable], Zero, Landmarks, Relations):-
   get_landmark_relations(TDerivable, Zero, Landmarks, Relations).


% is_landmark_intern(+Argument, +ZeroMap, +LandMarkSet)
% check if Argument is a landmark

% zero is a Landmark
is_landmark_intern(Zero, Zero, _Landmarks):-
    !.

% Map is in Landmarkset
is_landmark_intern(Map, _Zero, Landmarks):-
    map_list(Map, [I]),
    memberchk(_/I, Landmarks).


% get_landmark_equalities(+LandMarkQuantityTable, -Equalities)
% if 2 landmarks are equal (equal(LM1, LM2)
% analyse simple removes the relation and gives LM's equal pointers:
% derive the equality again from:  LM1/i & LM2/i element of Q, -> LM1 = LM2

% done
get_landmark_equalities([], []).

% equal pointers for 2 landmarks
get_landmark_equalities([value(X)/I|RestLandmarks], [equal(Y, X)|RestEqualities]):-
    memberchk(value(Y)/I, RestLandmarks),
    Y \== X,
    !,
    get_landmark_equalities(RestLandmarks, RestEqualities).

% next
get_landmark_equalities([_|RestLandmarks], RestEqualities):-
    get_landmark_equalities(RestLandmarks, RestEqualities).


% extern_representation(+Intern, +Q, -Extern)
% convert landmarkrelations from intern to extern, Q should be landmarks only
% relation should be about values not derivatives (logical with landmarks)

extern_representation(relation(Left, >, Right), Q, greater(ELeft, ERight)):-
    map_list(Left, LList),
    map_list(Right, RList),
    extern_sum(LList, Q, ELeft),
    extern_sum(RList, Q, ERight).

extern_representation(relation(Left, >=, Right), Q, greater_or_equal(ELeft, ERight)):-
    map_list(Left, LList),
    map_list(Right, RList),
    extern_sum(LList, Q, ELeft),
    extern_sum(RList, Q, ERight).

extern_representation(relation(Left, =, Right), Q, equal(ELeft, ERight)):-
    map_list(Left, LList),
    map_list(Right, RList),
    extern_sum(LList, Q, ELeft),
    extern_sum(RList, Q, ERight).


extern_sum([0], _Q, zero):-!.

extern_sum([X], Q, Element):-
    memberchk(value(Element)/X, Q).
    % although more landmarks may have this Index (indicating a landmark equality)
    % this is no problem, one is enough, the others will be derivable.

/* FL June 2004 should there be additions here?
extern_sum([X, Y|Rest], Q, plus(EX, EYRest)):-
    extern_sum([X], Q, EX),
    extern_sum([Y|Rest], Q, EYRest).
*/


% relevant_landmark_relations(+LandmarkRelations, -RelevantLMR)
% removes all lm relations to zero and within one q space
% these are supplied by q-spaces or derived from these and are not
% informative and supplied in every state.

% done
relevant_landmark_relations([], []).

% trivial: remove
relevant_landmark_relations([Relation|Tail], NewTail):-
    Relation =.. [_, Point1, Point2],
    trivial_landmark_rel(Point1, Point2),
    !,
    relevant_landmark_relations(Tail, NewTail).

relevant_landmark_relations([Relation|Tail], [Relation|NewTail]):-
    relevant_landmark_relations(Tail, NewTail).

% landmarks relation to zero
trivial_landmark_rel(_, zero).

trivial_landmark_rel(zero, _).

%same q-space
trivial_landmark_rel(Point1, Point2):-
    Point1 =.. [_, Par],
    Point2 =.. [_, Par].


% new_landmark_relations(+LandmarkRels, +AllRelations, -NewLandmarkRels)
% To find newly derived landmarkrelations in a state, subtract all
% smd relations from the set of known and derived landmarkrelations.
% DerivedLM - ALLKnown = NewLM,
% use alternative relation

% done
new_landmark_relations([], _, []).

% known element
new_landmark_relations([LMR|Tail], AllKnown, NewTail):-
    memberchk(LMR, AllKnown),
    !,
    new_landmark_relations(Tail, AllKnown, NewTail).

% known element
new_landmark_relations([LMR|Tail], AllKnown, NewTail):-
    alternative_relation(LMR, ALMR),
    memberchk(ALMR, AllKnown),
    !,
    new_landmark_relations(Tail, AllKnown, NewTail).

% new element
new_landmark_relations([LMR|Tail], AllKnown, [LMR|NewTail]):-
    new_landmark_relations(Tail, AllKnown, NewTail).

% END NEW garp 2.0 make store code

% BEGIN REPAIR: simplify_transition 21/08/2001 by BB

/* Facts present in the condition of a rule are removed from a state
and replaced by the givens of that rule, when determining a transition
between states. However, sometimes conditions are needed in a rule that
should not be removed at all, but which are required to check if the
rule applies. So they have to written in the conditions as well as in
the givens. In principle this not a problem, but when constraints on system
elements are used problems may turn up, because isa-level detail may get
lost. Suppose we have (super) a-b-c-d-e (sub), and in a state we have an
instance of e (e1). Now suppose in a rule instances of at least b are
required. So, e1 is found as consistent. In the givens of the rule the
instance of b (e1) is set back. So in the next state we have lost e1
as being a suptype of e and we have gained e1 as a subtype of b. A model
fragment that requires a subtype of for instance 'd' to be present will
now not be valid anymore... Although in principle correct, this is not
what is needed in the case of replacing the condition we used in the rule.

The procedure below simply removes such statements from both the 'condition'
and the 'givens' list. So, if statements on system elements appear both in the
conditions and in the givens, assume that the model builder wanted replacement,
and remove both so that it is neither removed from the old state or wrongly
replaced.

Notice: in a next version of GARP rules should have conditions that are
used ONLY as conditions AND conditions that must be replaced. This not only
matches better what model builders want, it will also speed up the transition
step. */

% Remove SE items present in both Conditions and Givens from both lists
simplify_transition( ConStart, ConRemain, ResultStart, ResultRemain ):-
        % Get SE from Condition list, create an empty list when no SE present
        % and give the input (Start) to the output Rest)
        ( select( system_elements( SE_C_Large ), ConStart, ConRest ),
            !
            ;
            SE_C_Large = [], ConStart = ConRest ),
        % Get SE from Result list, create an empty list when no SE present
        % and give the input (Start) to the output Rest)
        ( select( system_elements( SE_R_Large ), ResultStart, ResultRest ),
            !
            ;
            SE_R_Large = [], ResultStart = ResultRest ),

        % Do the simplification:
        % Remove SE items present in both Conditions and Givens from both lists
        do_simplify_transition( SE_C_Large, SE_C_Remain, SE_R_Large, SE_R_Remain ),

        % Put remaining SE lists back in Conditions and Givens
        % NB: the SE are placed first in the list (not last, as before)
        % but this should not make any difference (order is not important)
        select( system_elements( SE_C_Remain ), ConRemain, ConRest ),
        select( system_elements( SE_R_Remain ), ResultRemain, ResultRest ).

% Stop when both empty, Conditions empty or Results (Givens) empty
do_simplify_transition( [], [], [], [] ):- !.
do_simplify_transition( [], [], X, X ):- !.
do_simplify_transition( X, X, [], [] ):- !.
% when SE both in Conditions and Givens, remove from both lists
do_simplify_transition( [HC|RestC], RemainC, StartG, RemainG ):-
        % Unclear: should we test for variables, and prevent
        % unification with variables? To be decided...
        select( HC, StartG, RestStartG ),
        % no backtracking allowed
        (  @tracer->>tell(transition) %gp3 0.3
        -> write('Simplifying transition, removing: '),
           write_ln( HC ), @tracer->>told,
           !; true ),
        !,
        do_simplify_transition( RestC, RemainC, RestStartG, RemainG ).
% when SE not both in Conditions and Givens, keep them in both lists
do_simplify_transition( [HC|RestC], [HC|RemainC], [HG|RestStartG], [HG|RemainG] ):-
        do_simplify_transition( RestC, RemainC, RestStartG, RemainG ).

% END REPAIR: simplify_transition 21/08/2001 by BB


% remove attribute relations mentioning a removed system element
check_input_se([], _, _, []).

check_input_se([has_attribute(E1, _R, _E2)|T],
        Before, After, NT):-
    memberchk(instance(E1, _), Before),
    \+ memberchk(instance(E1, _), After),
    !,
    check_input_se(T, Before, After, NT).

check_input_se([has_attribute(_E1, _R, E2)|T],
        Before, After, NT):-
    memberchk(instance(E2, _), Before),
    \+ memberchk(instance(E2, _), After),
    !,
    check_input_se(T, Before, After, NT).

check_input_se([H|T], Before, After, [H|NT]):-
    check_input_se(T, Before, After, NT).

bind_system_structure_values([], _).

bind_system_structure_values([H|T], Cio):-
    cio_v(Cio, V),
    system_structure_slots(H, _, _, Cond, Giv),
    append(Cond, Giv, All),
    bind_system_structure_values2(All, V),
    bind_system_structure_values(T, Cio).

bind_system_structure_values2([], _).

bind_system_structure_values2([par_values(VL)|T], V):-
    !,
    sub_set(VL, V),
    !,
    bind_system_structure_values2(T, V).

bind_system_structure_values2([_|T], V):-
    bind_system_structure_values2(T, V).


% sift inequalities from other relations

inequality_between_values([], []).
inequality_between_values([H|T], [H|NT]):-
    inequality_between_values_type(H),
    !,
    inequality_between_values(T, NT).
inequality_between_values([_|T], NT):-
    inequality_between_values(T, NT).

inequality_between_values_type(equal(_, _)).
inequality_between_values_type(greater(_, _)).
inequality_between_values_type(smaller(_, _)).
inequality_between_values_type(greater_or_equal(_, _)).
inequality_between_values_type(smaller_or_equal(_, _)).

parameters_apply_to_system_elements([], _, []).

% parameter system elements still exist
parameters_apply_to_system_elements([H|T], AllSE, [H|NT]):-
    parameter(H, _HH, _G, SE, _I, _T, _S),
    forall(comma_member(Element, SE), member(instance(Element, _), AllSE)),
    !,
    parameters_apply_to_system_elements(T, AllSE, NT).

% parameter not usable anymore
parameters_apply_to_system_elements([_|T], AllSE, NT):-
    parameters_apply_to_system_elements(T, AllSE, NT).

% sift relations that apply to parameters which do not exist anymore
% from others

relations_not_apply_to_parameters([], _, []).
relations_not_apply_to_parameters([H|T], P, [H|NT]):-
    parameters_not_in_relation(P, H),
    !,
    relations_not_apply_to_parameters(T, P, NT).

relations_not_apply_to_parameters([_|T], P, NT):-
    relations_not_apply_to_parameters(T, P, NT).

parameters_not_in_relation(Pars, Atom):-
    atomic(Atom),
    !,
    \+ (member(APar, Pars), parameter(APar, _, _, _, Atom, _, _)).

parameters_not_in_relation(Pars, R):-
    R =.. [_|Args],
    !,
    forall(member(Arg, Args), parameters_not_in_relation(Pars, Arg)).

% don't mind var, numbers (if they slip in somehow)
parameters_not_in_relation(_, _).

/* --------------------- reclass ----------------------------------- */

reclass([], [], Cio, Cio).

% re-examine a system structrue

reclass([H|T], [NH|NT], Cin, Cout):-
    % replace values in H with values in original system structure
    do_revalidate(revalidate_system_structure(H, NH)),
    system_structure_slots(NH, SName, Isa, Conditions, Givens),
    etrace(reclass_recheck_mf, SName, respecification),
    cio_ss_nss(Cin, Cnew, SS, NSS),
    append(SS, [SName], NSS),   % put system structure in list
        % couldn't be a parent of itself anyway
	%gp3 0.1: we added Garp3 names (static, process, agent, with process moving to front)
	%to head of the list of possible topnodes
	%but we left the old names as well for legacy etc
    append(SS, [static,process,agent,description_view, composition_view,
        decomposition_view, view, qualitative_state], SSI),
    sub_set(Isa, SSI),  % parents are already still there
    flag(derivative_assumptions_cause_fail, _, false),
    re_check_conditions(Conditions, Cnew, Cnew1),
    % if this fails, remove H and all ss that depend on it
    !,
    (   add_givens(Givens, Cnew1, Cnew2),
        etrace(reclass_recheck_mf_ok, SName, respecification)
    ;
        etrace(reclass_recheck_mf_contradiction, SName, respecification),
        fail
    ),
    !,
    reclass(T, NT, Cnew2, Cout).

% NEW 16-8-2004 FL:
% A MF should not make derivative assumptions,
% (these cannot be found contradictive because derivatives are calculated only much later)
% if they do re_check_conditions fails and sets this flag,
% the MF is later considered in the normal Class procedure.
% 2 clauses: for view / process

% do not assume derivative: view
reclass([H|T], N, Cin, Cout):-
    flag(derivative_assumptions_cause_fail, Flag, Flag),
    Flag == true,
    system_structure_slots(H, SName, Isa, _Conditions, Givens),
    isa_view(Isa),
    !,
    etrace(reclass_recheck_delay, SName, respecification),
    etrace(reclass_recheck_remove_static, SName, respecification),
    remove_system_element_givens(Givens, Cin, Cnew),
    !,
    reclass(T, N, Cnew, Cout).

% do not assume derivative: process
reclass([H|T], N, Cin, Cout):-
    flag(derivative_assumptions_cause_fail, Flag, Flag),
    Flag == true,
    !,
    system_structure_slots(H, SName, _Isa, _Conditions, _Givens),
    etrace(reclass_recheck_delay, SName, respecification),
    etrace(reclass_recheck_remove_proces_agent, SName, respecification),
    !,
    reclass(T, N, Cin, Cout).

% NEW 16-8-2004 FL:
% A  generate all values MF cannot be checked here,
% revalidate_system_structures fails and sets this flag,
% MF is considered in normal Class procedure.
% 2 clauses: for view / process
reclass([H|T], N, Cin, Cout):-
    flag(generate_all_values_causes_fail, Flag, Flag),
    Flag == true,
    !,
    system_structure_slots(H, SName, Isa, _Conditions, Givens),
    isa_view(Isa),
    !,
    etrace(reclass_recheck_remove_static, SName, respecification),
    remove_system_element_givens(Givens, Cin, Cnew),
    !,
    reclass(T, N, Cnew, Cout).

% NEW 16-8-2004 FL:
% A  generate all values MF cannot be checked here,
% revalidate_system_structures fails and sets this flag,
% MF is considered in normal Class procedure.
% 2 clauses: for view / process
reclass([H|T], N, Cin, Cout):-
    flag(generate_all_values_causes_fail, Flag, Flag),
    Flag == true,
    !,
    system_structure_slots(H, SName, _Isa, _Conditions, _Givens),
    etrace(reclass_recheck_remove_proces_agent, SName, respecification),
    !,
    reclass(T, N, Cin, Cout).

% note a problem may arise if H does not have any
% system element/structure conditions or isa conditions;
% if its child fails to exist, it has no right to be here
% currently such ss do not occur
% same problem with assumption in class

% remove view (and system element givens)

reclass([H|T], N, Cin, Cout):-
    system_structure_slots(H, SName, Isa, _Conditions, Givens),
    isa_view(Isa),
    !,
    etrace(reclass_recheck_remove_static, SName, respecification),
    remove_system_element_givens(Givens, Cin, Cnew),
    cio_re_nre(Cnew, Cnew2, Re, [SName|Re]),
    reclass(T, N, Cnew2, Cout).

% remove process/qualitative state (system element givens remain)

reclass([H|T], N, Cin, Cout):-
    system_structure_slots(H, SName, _Isa, _Conditions, _Givens),
    etrace(reclass_recheck_remove_proces_agent, SName, respecification),
    cio_re_nre(Cin, Cnew, Re, [SName|Re]),
    !,
    reclass(T, N, Cnew, Cout).


do_revalidate(revalidate_system_structure(H, NH)):-
    flag(generate_all_values_causes_fail, _, false),
    call(revalidate_system_structure(H, NH)),
    !.

do_revalidate(revalidate_system_structure(S, _)):-
    system_structure_slots(S, SName, _, Conditions, _),
    memberchk(system_elements(SE), Conditions),
    member(instance(_, I), SE),
    isa_instance(I, generate_all_values),
    !,
    etrace(reclass_recheck_fail_GAV, SName, respecification),
    flag(generate_all_values_causes_fail, _, true),
    !,
    fail.



% is system structure a view?
% it is, if it is not a process or qualitative state

isa_view(Isa):-
    \+ memberchk(process, Isa),
    \+ memberchk(qualitative_state, Isa),
    !.

% remove all system elements in a list of givens from SE list in CIO
% (note that during reclassification 'Old' SE list is empty)

remove_system_element_givens([], Cio, Cio).

remove_system_element_givens([system_elements(SEList)|Tail], Cin, Cout):-
    !,
    cio_se_nse(Cin, Cnew, SE, NSE),
    difference(SE, SEList, NSE),
    remove_system_element_givens(Tail, Cnew, Cout).

remove_system_element_givens([_|Tail], Cin, Cout):-
    remove_system_element_givens(Tail, Cin, Cout).


% system structures still there
re_check_conditions(Conditions, Cin, Cout):-
    common_select(Conditions, system_structures(L), Rest),
    !,
    cio_ss(Cin, SS),
    sub_set(L, SS),
    re_check_conditions(Rest, Cin, Cout).


% system elements still there
re_check_conditions(Conditions, Cin, Cout):-
    common_select(Conditions, system_elements(L), Rest),
    !,
    cio_se(Cin, SE),    % at this point all system elements
    cio_oe(Cin, OE), % at this point all attribute relations
    append(SE, OE, ElementsAttributes),
    super_class_sub_set(L, ElementsAttributes),
    re_check_conditions(Rest, Cin, Cout).

% parameters (maybe not necessary at all)
re_check_conditions([parameters(PList)|TConditions], Cin, Cout):-
    !,
    cio_p(Cin, P),
    check_parameter_conditions(PList, P),   % match
    re_check_conditions(TConditions, Cin, Cout).

% values: must match
re_check_conditions([par_values(VList)|TConditions], Cin, Cout):-
    !,
    check_par_value_conditions(VList, Cin, Cnew, AsumpV),
    warn_if_not_empty(AsumpV),

%   FL July 2004:
%   Patch:  Reclass should not assume derivatives,
%   for they cannot be constrained by terminations
    no_derivative_assumptions(AsumpV, VList, [] ),
    !,
%   endpatch

    re_check_conditions(TConditions, Cnew, Cout).

re_check_conditions([par_relations(RList)|TConditions], Cin, Cout):-
    !,
    check_par_relation_conditions(RList, Cin, Cnew, AsumpR),
    warn_if_not_empty(AsumpR),

%   FL July 2004:
%   Patch:  Reclass should not assume derivatives,
%   for they cannot be constrained by terminations
    no_derivative_assumptions(AsumpR, [], RList),
    !,
%   endpatch

    re_check_conditions(TConditions, Cnew, Cout).

re_check_conditions([H|T], C, C):-
    % writef('*** Error re_check_conditions: unknown %w\n', [H]),
    etrace(reclass_recheck_unknown_type, H, reclass_recheck_unknown_type),
    re_check_conditions(T, C, C).

re_check_conditions([], C, C).


% FL: UNUSED???
warn_if_not_empty([]):- !.
warn_if_not_empty(_). %:- etrace(warning_re-check_conditions_must_make_assumptions, [], general).


% no_derivative_assumptions(+Assumptions, +Values, +Relations)
% FL July 2004;
% Reclass should not assume derivatives, for these are freed up (continuity)
% and only calculated later in influence resolution
% for correct check original relations are used not internal relations
% Predicate succeeds if no Assumption maps to a derivative or derivative relation
% in Values and Derivatives

/* Permanently switched off: allowing is always bad at this point
no_derivative_assumptions(_, _, _):-
    flag(allow_d_assumptions_in_reclassifying_mfs, ADA, ADA),
    algorithm_assumption_flag(ADA, true, allow_d_assumptions_in_reclassifying_mfs),
    !.
*/

no_derivative_assumptions(A, V, R):-
    %flag(allow_d_assumptions_in_reclassifying_mfs, ADA, ADA),
    %algorithm_assumption_flag(ADA, fail, allow_d_assumptions_in_reclassifying_mfs),
    %!,
    flag(derivative_assumptions_cause_fail, _, false),
    no_derivative_assumptions2(A, V, R).

% no_derivative_assumptions2(+Assumptions, +Values, +Relations),
% actual checking procedure:
% if any value or relation is in the assumptions then fail, else succeed
% first all values are checked then all relations.

% no assumptions, succeed
no_derivative_assumptions2([], _, _):- !.

% assumption = derivative in value statement, fail
no_derivative_assumptions2(Assumptions, [Value|_], _):-
    value(Value, Instance, _, _, Derivative),
    interval_relations(derivative(Instance),
                        Derivative, derivative, [DRelation]),
    memberchk(DRelation, Assumptions),
    !,
    flag(derivative_assumptions_cause_fail, _, true),
    fail.

% next value statement
no_derivative_assumptions2(Assumptions, [_|VT], Relations):-
    no_derivative_assumptions2(Assumptions, VT, Relations).

% assumption derivative relation in Relations, fail
% (no values left)
no_derivative_assumptions2(Assumptions, [], [Relation|_]):-
    d_inequality_type(Relation),
    memberchk(Relation, Assumptions),
    !,
    flag(derivative_assumptions_cause_fail, _, true),
    fail.

% next relation
no_derivative_assumptions2(Assumptions, [], [_|RT]):-
    no_derivative_assumptions2(Assumptions, [], RT).

% no more values and relations, done, succeed
no_derivative_assumptions2(_, [], []).

% END PATCH FL July 2004


% revalidate a system structure
% when transition rules are applied, values may have changed
% these values are also bound to the value structures in a system structure
% we must retreive the original value structures and check if they still
% hold (which is done by re_check_conditions)

revalidate_system_structure(S, NS):-
    system_structure_slots(S, SName, Isa, Conditions, Results),
    unbind_value_slots(Conditions, NewConditions),
    unbind_value_slots(Results, NewResults),
    system_structure_slots(NS, SName, Isa, NewConditions, NewResults),
    call(NS).   % reinstantiate value slots

unbind_value_slots([], []).
unbind_value_slots([par_values(_)|T], [par_values(_)|NT]):-
    !,
    unbind_value_slots(T, NT).
    % normally only one slot for conditions or values, but more are possible

unbind_value_slots([H|T], [H|NT]):-
    unbind_value_slots(T, NT).


% FIND AN EXISTING STATE
% SystemStructures must form a subset
% all relations must hold
% repair BB 28/04/2001
% 'depth_split' and 'existing_state' did not fully check the
% consistency of the constraints on the derivatives specified
% in the result of the termination rules. The problem only
% happens when looking for existing successor states. And
% concerns constraints that allow multiple values of the derivatives
% (thus d_greater_or_equal and d_smaller_or_equal). The constraints
% that do require ONE particular value to be present in the next
% state (thus =, <, or >) are taken care of by 'get_derivable_values'
% and are therefore not a problem, although they are re-checked in
% this repair (but this could be left out).
%
% <= and >= cannot be solved by taking one specific value.
% Instead these relations have to be 'consistent' with the existing
% state. This is also true for other relations that may have been
% specified in the givens of the 'rules'. However, in general there
% is no need to have such relations, because when important they should
% be captured by model fragments (i.e. system structures). Therefore,
% a repair only dealing with derivatives (enforcing continuity), seems
% sufficient.
% OLD:
%existing_state(SystemStructures, Cin, State):-
%   state(State, SMD),
%   state_status(State, Status),
%   memberchk(Status, [interpreted, terminated, ordered, closed ]),
%   smd_slots(SMD, _, SEE, _P, V, R, SS, _),
%   sub_set(SystemStructures, SS),
%   cio_se(Cin, SE),
%   cio_oe(Cin, OE),
%   append(SE, OE, AE),
%   super_class_sub_set(AE, SEE),
%   cio_v(Cin, Values),
%   forall(member(Value, Values), memberchk(Value, V)),
%   add_givens([par_relations(R) ], Cin, _).
% NEW:
existing_state(SystemStructures, Cin, State):-
    state(State, SMD),
    state_status(State, Status),
    memberchk(Status, [interpreted, terminated, ordered, closed ]),
    % not open, but that should not occur anyway
    smd_slots(SMD, _, SEE, _P, V, R, SS, _),
    sub_set(SystemStructures, SS),
    % system elements
    cio_se(Cin, SE),  % here instances
    cio_oe(Cin, OE),  % here attributes
    append(SE, OE, AE),
    super_class_sub_set(AE, SEE),
    cio_v(Cin, Values),
    forall(member(Value, Values), memberchk(Value, V)),


        % ---- start - part of repair BB 28/04/2001
        cio_q( Cin, NewQ ),
        cio_p_v_r( Cin, _, _, NewR ),
        etrace(reclass_term_rule_con, State, transition),
	check_termination_consistency( NewR, NewQ, V ),
        % ---- end - part of repair BB 28/04/2001

    add_givens([par_relations(R) ], Cin, _).

% ONLY NEW:
% ---- start - part of repair BB 28/04/2001
%
% satisfy, fail or skip each of the constraints. in the case of
% failure (and no skip), target state is not a valid successor
%
% includes: no relations -> no constraints -> succeed & no backtracking
check_termination_consistency( [], _, _ ):- !.
check_termination_consistency( [relation(Left, Rel, Right)|Rest], NewQs, I ):-
        % find internal reference for relation
        map_list( Left, LeftRef),
        map_list( Right, RightRef ),
        !, % satisfy, fail or skip constraint, but no backtracking
        check_termination_consistency( LeftRef, Rel, RightRef, NewQs, I ),
        % check remaining set
        check_termination_consistency( Rest, NewQs, I ).


% we assume we only have derivative constraints wrt to zero
% so: find  quantity on LHS (zero at RHS) (or the opposite => 2nd clause)
check_termination_consistency( [LeftRef], Rel, [], NewQs, I ):-
        % find related quantity derivative constraint (if available)
        memberchk( derivative(Q)/LeftRef, NewQs ),
        % find same quantity in target existing state
        member( value(Q, _, _, Der), I ),
        etrace(reclass_term_der_con, [Q, Rel, Der], transition),
        !,
        valid_termination_constraint( Rel, Der ),
        !.% FL May 2004: Cut added, transitions were found multiple times

check_termination_consistency( [], Rel , [RightRef], NewQs, I ):-
        memberchk( derivative(Q)/RightRef, NewQs ),
        member( value(Q, _, _, Der), I ),
        etrace(reclass_term_der_con, [Q, Rel, Der], transition),
        !,
        valid_termination_constraint( Der, Rel ),
        !.% FL May 2004: Cut added, transitions were found multiple times

/* FL: May 2004 Old version: tracer returns intern relations -> not clear
New version below which translates back to derivatives

% skip all other constraints...
check_termination_consistency( LR, R, RR, _, _ ):-
        tracer( transition, 'Skipping continuity for: %w %w %w', [LR, R, RR] ).

New Version: does not really work, translation never finds correct quantities
(equal pointers for many quantities) Not a problem, Garp 2.0 does not use
existing_state and subroutines anymore... */

% skip all other constraints...
check_termination_consistency( LR, R, RR, Q, _ ):-
        list_map(LR, LMap),
        list_map(RR, RMap),
	etrace(reclass_skip_con, [LMap, R, RMap, Q], transition).

% END New Tracer repair FL.


% when quantity on LHS: Q-Rel-dV, eg.: Q >= zero
valid_termination_constraint( >=, plus ).
valid_termination_constraint( >=, zero ).
valid_termination_constraint( >, plus ).
valid_termination_constraint( =, zero ).

% when quantity on RHS: dV-Rel-Q, eg.:Q =< zero
valid_termination_constraint( min, >= ).
valid_termination_constraint( zero, >= ).
valid_termination_constraint( min, > ).
valid_termination_constraint( zero, = ).

% ---- end - part of repair BB 28/04/2001


get_inequality_relations([], []).
get_inequality_relations([H|T], [H|NT]):-
    inequality_type(H),
    !,
    get_inequality_relations(T, NT).

get_inequality_relations([_|T], NT):-
    get_inequality_relations(T, NT).

% when a set of ss has been reclassified, we must add any possible
% children to the hypotheses list
% this is done before classification, thus oe and os are empty,
% use ss and se

get_child_hypotheses([], Cio, Cio).
get_child_hypotheses([ S | Tail ], Cin, Cout):-
    system_structure_slots(S, Name, _Isa, _Conditions, _Givens),
    cio_ss_se_hy_nhy(Cin, Cresult, SS, SE, Rhyp, NHyp),
    % get children
    findall(system_structures(Child, Isa, Conditions, Givens),
     (system_structures(Child, Isa, Conditions, Givens),
      isa_list(Isa, IsaList),
      member(Name, IsaList),
      system_structure_conditions(Conditions, SSConditions),
      % its system structure conditions
      sub_set(SSConditions, SS),                   %
      system_element_conditions(Conditions, SEConditions),
      % its system element conditions BTSELECT
      super_class_sub_set(SEConditions, SE),
      % select se conditions (including attributes), BTSELECT
      is_new_system_structure(Child, SS, [], Rhyp, [], []),
      etrace(reclass_hyp_child, [Child, Name], specification)
     ),
     Children),
    append(Rhyp, Children, NHyp),
    !,
    get_child_hypotheses(Tail, Cresult, Cout).


% split_off_exogenous(+Values, +ExogenousParameters, -OtherValues, -ExogenousValues)
% split a list of valuestatements according to parameters in EXPars

split_off_exogenous([], _, [], []).

% value is not exogenous
split_off_exogenous([value(Par, Q, Val, Der)|T], Parameters, [value(Par, Q, Val, Der)|OT], ET):-
    \+ memberchk(Par, Parameters),
    !,
    split_off_exogenous(T, Parameters, OT, ET).

% value is  exogenous
split_off_exogenous([value(Par, Q, Val, Der)|T], Parameters, OT, [value(Par, Q, Val, Der)|ET]):-
    split_off_exogenous(T, Parameters, OT, ET).


% undo_continuity_exogenous(+ResultValuesIn, +ExogenousParameters, +ResultRelationsIn,
%                           -ResultValuesOut)
% undo continuity in results for exogenous parameters,
% they should change derivative through terminations

undo_continuity_exogenous([], _, _, []).

% value is not exogenous
undo_continuity_exogenous([value(P, Q, V, D)|T], Pars, Relations, [value(P, Q, V, D)|NT]):-
    \+ memberchk(P, Pars),
    !,
    undo_continuity_exogenous(T, Pars, Relations, NT).

% value is  exogenous, but derivative is not removed (solo exogenous termination)
undo_continuity_exogenous([value(P, Q, V, Der)|T], Pars, Relations, [value(P, Q, V, Der)|NT]):-
    nonvar(Der),
    !,
    undo_continuity_exogenous(T, Pars, Relations, NT).

% value is  exogenous
undo_continuity_exogenous([value(P, Q, V, _)|T], Pars, Relations, [value(P, Q, V, Der)|NT]):-
    determine_previous_derivative(P, Relations, Der),
    undo_continuity_exogenous(T, Pars, Relations, NT).


% from result relations, previous derivative can be derived:
% >= zero means plus,
% <= zero means min,
% no relation means zero. but this will never happen because such a value should not terminate

% >= zero
determine_previous_derivative(P, Relations, plus):-
    memberchk(d_greater_or_equal(P, zero), Relations),
    !.

% =< zero
determine_previous_derivative(P, Relations, min):-
    memberchk(d_smaller_or_equal(P, zero), Relations),
    !.

% no relation found ???
determine_previous_derivative(_, _, _):-
    etrace(reclass_det_prev_der_warning, _, general).
    % write('*** Warning, determine_previous_derivative/3: continuity relation not found'),
    % nl,
    % sleep(2).


