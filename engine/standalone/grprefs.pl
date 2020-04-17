/*  File:    grprefs.pl
    Purpose: algorithm option switches definitions
             semantics of influences and proportional relations
    Author:  Floris Linnebank & Bert Bredeweg
    Date:    September 2004
    Part-of:  GARP (version 2.0)
    Modified: 12 September 2004

    Copyright (c) 2004, University of Amsterdam. All rights reserved.

*/

/*************** INITIALISATION ****************************************/

% The init_algorithm_assumptions/0 (nb. options used to be called assumptions)
% intialises all algorithm option switches to default pattern,
% and applies overrides from model
%
% NB true = on, fail = off,
% All assumptions must be set! (flag starts on 0)

% NB To change the default settings this procedured can be edited by the user
% Only change: fail to true, or true to fail, as desired. 
% (Do not confuse fail and false, the latter is wrong here) 
% A helpfile: algorithm_help can be found in the help directory (garp/program_files/help)
% all options are explained there, as well as associating overrides with a model.

init_algorithm_assumptions:-
    flag(cw_assumption, _, fail),         % Closed World assumption in Influence Resolution
    flag(epsilon_ordering, _, true),      % Epsilon ordering in transitions: remove on basis of precedence
    flag(epsilon_merging, _, true),       % Epsilon merging in transitions: merge immediate transitions
    flag(order_using_equalities, _, true),
    flag(order_using_correspondences, _, true),
    flag(full_branching_value_terminations, _, fail),
    flag(full_branching_equality_terminations, _, fail),
    flag(no_subsumption_specify_all, _, true),
    flag(free_maxmin_derivative, _, true),
    flag(free_zero_derivative, _, fail),
    flag(use_landmarks, _, fail),
    flag(use_old_transition_rules, _, fail),
    flag(terminate_weak_relations, _, fail),
    flag(remove_corresponding_equality, _, fail),
    flag(extra_termination_interpreter, _, fail),
    flag(remove_inactive_quantities, _, fail),
    flag(apply_continuity_d_inequalities, _, true),
    flag(allow_d_assumptions_in_reclassifying_mfs, _, fail),
    flag(no_analyse_zero, _, fail),

    % at least one algorithm_option_switch must be present not to give errormessage:
    % assert dummy
    assert(algorithm_option_switch(dummy, dummy)),

    % apply all overide statements in model
    override_defaults.



/* ---------------  algorithm option switches menu  --------------------- */

% Added 18-2-2004 by FL: algorithm option switches menu
% In the Garp 2.0 engine, some options in the algorithm can be
% switched on and off, flags are used with the values true and fail,
% these values match the generic toggle_menu structure:

% NB assumptions/options must be initialised using init_algorithm_assumptions/0

% first some error trapping:
% check if all flags are initialised
assumptions_menu:-
    flag(cw_assumption, CW, CW),
    flag(epsilon_ordering, EO, EO),
    flag(epsilon_merging, EMR, EMR),
    flag(order_using_equalities, EQO, EQO),
    flag(full_branching_equality_terminations, FBEQ, FBEQ),
    flag(full_branching_value_terminations, FBV, FBV),
    flag(terminate_weak_relations, TWR, TWR),
    flag(use_landmarks, LM, LM),
    flag(remove_corresponding_equality, RCE, RCE),
    flag(free_maxmin_derivative, FMD, FMD),
    flag(free_zero_derivative, FZD, FZD),
    flag(no_subsumption_specify_all, NS, NS),
    flag(extra_termination_interpreter, ETI, ETI),
    flag(remove_inactive_quantities, RIQ, RIQ),
    flag(apply_continuity_d_inequalities, ACI, ACI),
    flag(allow_d_assumptions_in_reclassifying_mfs, ADA, ADA),
    flag(no_analyse_zero, NAZ, NAZ),
    memberchk(0, [CW, EO, EMR, EQO, FBEQ, FBV, TWR, LM, NAZ, /*MCE, */ RIQ, ACI, ADA, RCE, FMD, FZD, NS, ETI]),
    % an uninitialised assumption
    !,
    nl,
    write('Algorithm assumptions not initialised, press y to load default values, or any other to cancel: '),nl,
    write('> '),
    get0(C),
    (char_code(y, C) -> init_algorithm_assumptions ; true).

% make an algorithm options menu:
% The descriptions can be freely changed here,
% BUT: descriptions occur twice here! AND: often the second list does not match anymore
% requiring a copy paste action from the first and editing it (add NEW to every variable)
% symptom of this problem is that the menu seems to work, but on a second call of the menu
% it can be seen that changes have not been made. (THEREFORE: EXPERTS ONLY)

assumptions_menu:-
    flag(cw_assumption, CW, CW),
    flag(epsilon_ordering, EO, EO),
    flag(epsilon_merging, EMR, EMR),
    flag(order_using_equalities, EQO, EQO),
    flag(order_using_correspondences, COR, COR),
    flag(full_branching_equality_terminations, FBEQ, FBEQ),
    flag(full_branching_value_terminations, FBV, FBV),
    flag(terminate_weak_relations, TWR, TWR),
    flag(use_landmarks, LM, LM),
    flag(use_old_transition_rules, OTR, OTR),
    flag(remove_corresponding_equality, RCE, RCE),
    flag(free_maxmin_derivative, FMD, FMD),
    flag(free_zero_derivative, FZD, FZD),
    flag(no_subsumption_specify_all, NS, NS),
    flag(extra_termination_interpreter, ETI, ETI),
    flag(remove_inactive_quantities, RIQ, RIQ),
    flag(apply_continuity_d_inequalities, ACI, ACI),
    flag(allow_d_assumptions_in_reclassifying_mfs, ADA, ADA),
    flag(no_analyse_zero, NAZ, NAZ),
    InitSelections = [  CW/'Apply closed world assumption in influence resolution                          ', % NB spaces make for nice single column menu
                        LM/'Derive landmark relations',
                        FBEQ/'Assume inequality terminations despite unknown derivatives',
                        FBV/'Assume value terminations despite unknown derivatives',
                        TWR/'Generate terminations for >= & =<',
                        ETI/'Use extra termination rules',
                        COR/'Use correspondences in ordering',
                        RCE/'Remove terminations to unequal for full corresponding quantities',
                        EQO/'Use constants in ordering',
                        EO/'Apply epsilon ordering',
                        EMR/'Apply epsilon merging of immediate terminations',
                        NS/'Specify and Match instead of Subsumption',
                        OTR/'Use only old rule based transition procedure',
                        FMD/'No quantity space derivative constraints extreme values',
                        FZD/'No quantity space derivative constraints zero as extreme value',
                        RIQ/'Remove inactive quantities',
                        ACI/'Apply continuity on derivative inequalities',
                        ADA/'Allow assumptions on derivatives in reclassifying MFs',
                        NAZ/'No application of analyse zero equality technique'
                         ],
    toggle_menu('toggle algorithm assumptions: ',
        InitSelections,
        (c)/'cancel (main menu)',
        d/'done',
        Selections),
    Selections = [  NewCW/'Apply closed world assumption in influence resolution                          ', % NB spaces make for nice single column menu
                    NewLM/'Derive landmark relations',
                    NewFBEQ/'Assume inequality terminations despite unknown derivatives',
                    NewFBV/'Assume value terminations despite unknown derivatives',
                    NewTWR/'Generate terminations for >= & =<',
                    NewETI/'Use extra termination rules',
                    NewCOR/'Use correspondences in ordering',
                    NewRCE/'Remove terminations to unequal for full corresponding quantities',
                    NewEQO/'Use constants in ordering',
                    NewEO/'Apply epsilon ordering',
                    NewEMR/'Apply epsilon merging of immediate terminations',
                    NewNS/'Specify and Match instead of Subsumption',
                    NewOTR/'Use only old rule based transition procedure',
                    NewFMD/'No quantity space derivative constraints extreme values',
                    NewFZD/'No quantity space derivative constraints zero as extreme value',
                    NewRIQ/'Remove inactive quantities',
                    NewACI/'Apply continuity on derivative inequalities',
                    NewADA/'Allow assumptions on derivatives in reclassifying MFs',
                    NewNAZ/'No application of analyse zero equality technique'
                         ],
    flag(cw_assumption, _, NewCW),
    flag(epsilon_ordering, _, NewEO),
    flag(epsilon_merging, _, NewEMR),
    flag(order_using_equalities, _, NewEQO),
    flag(order_using_correspondences, _, NewCOR),
    flag(full_branching_equality_terminations, _, NewFBEQ),
    flag(full_branching_value_terminations, _, NewFBV),
    flag(use_landmarks, _, NewLM),
    flag(use_old_transition_rules, _, NewOTR),
    flag(terminate_weak_relations, _, NewTWR),
    flag(remove_corresponding_equality, _, NewRCE),
    flag(no_subsumption_specify_all, _, NewNS),
    flag(free_maxmin_derivative, _, NewFMD),
    flag(free_zero_derivative, _, NewFZD),
    flag(remove_inactive_quantities, _, NewRIQ),
    flag(apply_continuity_d_inequalities, _, NewACI),
    flag(allow_d_assumptions_in_reclassifying_mfs, _, NewADA),
    flag(no_analyse_zero, _, NewNAZ),
    flag(extra_termination_interpreter, _, NewETI).


/************** SUPPLEMENTAL OPTION ROUTINES ************************/

% Algorithm assumption switches can be set in a model by
% declaring statements like:
% algorithm_option_switch(cw_assumption, off). (see algorithm help: 'ah')
% these statements can be present in the isa or rules file which are simply consulted.
%(The library and input files are consulted AND checked by garp.)
override_defaults:-
    forall(algorithm_option_switch(Assumption, OnOff), set_switch(Assumption, OnOff)),
    !.

%dummy switch for cases when no overrides are present
set_switch(dummy, dummy):-!.

%illegal assumption name
set_switch(Assumption, OnOff):-
    flag(Assumption, Flag, Flag), %flag must allready be set to indicate a correct name...
    Flag == 0,
    writef('*** ERROR: illegal algorithm option override: algorithm_option_switch( %w, %w )', [Assumption, OnOff]),
    nl,
    !,
    fail.

set_switch(Assumption, on):-
    flag(Assumption, _, true),
    !.

set_switch(Assumption, off):-
    flag(Assumption, _, fail),
    !.

%illegal assumption value
set_switch(Assumption, OnOff):-
    \+ memberchk(OnOff, [on, off]), % name is correct, but override value is not.
    writef('*** ERROR: illegal algorithm option override: algorithm_option_switch( %w, %w )', [Assumption, OnOff]),
    nl,
    !,
    fail.


% algorithm_assumption_flag(+Flag, +DesiredValue)
%
% use this predicate to test if flag is on or off:
% uninitialised flag will raise error
algorithm_assumption_flag(Flag, _):-
    Flag == 0,
    write_ln('*** ERROR: algorithm option uninitialised.'),
    !, fail.

algorithm_assumption_flag(true, true).
algorithm_assumption_flag(fail, fail).




