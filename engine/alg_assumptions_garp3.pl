/*
alg_assumptions.garp3.pl
Code about algorithm assumptions (= options)
Some code based on legacy file grprefs.pl (see standalone + readme).

That file by Floris Linnebank & Bert Bredeweg and Copyright (c) 2004, University of Amsterdam. All rights reserved.

Now:
PART OF Garp3. SEE COPYRIGHT NOTICE.

runs in namespace  'engine'
*/

/********* BELOW NOT EDITED, JUST COPIED ************************/
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

%gp3 0.3: When run from a model, *all* of these flags will be overruled because
%we export all runpref values to the isa file using algorithm_option_switch
%defaults for garp3 models are set in the init of the garp3 model.
%
%so the defaults below are only used when a legacy model (that was not exported by garp3, modeldefinition version 4 or higher) is loaded.

init_algorithm_assumptions:-
    flag(cw_assumption, _, fail),         % Closed World assumption in Influence Resolution
    flag(cw_assumption_second_order, _, fail),         % Closed World assumption in Influence Resolution For Second Order Derivatives
    flag(cw_assumption_third_order, _, fail),         % Closed World assumption in Influence Resolution For Third Order Derivatives
    flag(epsilon_ordering, _, true),      % Epsilon ordering in transitions: remove on basis of precedence
    flag(epsilon_merging, _, true),       % Epsilon merging in transitions: merge immediate transitions
    flag(order_using_equalities, _, true),
    flag(order_using_correspondences, _, true),
    flag(full_branching_value_terminations, _, fail),
    flag(full_branching_equality_terminations, _, fail),
    % flag(no_subsumption_specify_all, _, true),
    flag(free_maxmin_derivative, _, true),
    flag(free_zero_derivative, _, fail),
    flag(use_landmarks, _, fail),
    % flag(use_old_transition_rules, _, fail),
    flag(terminate_weak_relations, _, fail),
    flag(remove_corresponding_equality, _, fail),
    flag(extra_termination_interpreter, _, fail),
    flag(remove_inactive_quantities, _, fail),
    flag(apply_continuity_d_inequalities, _, true),
    % flag(allow_d_assumptions_in_reclassifying_mfs, _, fail),
    % flag(no_analyse_zero, _, fail),
    % FL New Mar 2007: treat points on the same qspace as equal for different qtys
    % FL may 07: NB SWITCH implementation postponed (versionpatch needed)
    flag(equal_qspace_points, _, fail),
    flag(value_branching, _, fail),
    flag(epsilon_derivative_constraints, _, true),
    flag(reasoning_assumptions, _, true),
    flag(assume_conditional_derivatives, _, true),
    flag(second_order_derivatives, _, true),
    flag(derivative_terminations, _, true),
    flag(second_order_continuity, _, true),
    flag(second_order_proportionality_propagation, _, fail),
    flag(order_epsilon_last, _, fail),
    flag(max_depth, _, 0),
    flag(equal_intervals, _, fail),
    flag(fast_path, _, fail),
    flag(solve_extra_combinations, _, true),
    flag(full_branching_derivative_terminations, _, fail),
    flag(third_order_derivatives, _, fail),
    flag(third_order_proportionality_propagation, _, fail),

    flag(comparative_analysis, _, fail),
    flag(comparative_analysis_on_proportionalities, _, fail),
    flag(comparative_analysis_equal_target_quantity_type, _, fail),
    flag(comparative_analysis_equal_source_quantity_type, _, fail),
    flag(comparative_analysis_similar_target_entity_type, _, fail),
    flag(comparative_analysis_similar_source_entity_type, _, fail),
    flag(comparative_analysis_equal_causal_dependency_sign, _, fail),


    % at least one algorithm_option_switch must be present not to give errormessage:
    % assert dummy
    assert(algorithm_option_switch(dummy, dummy)),

    % apply all overide statements in model
    override_defaults.



/************** SUPPLEMENTAL OPTION ROUTINES ************************/

% Algorithm assumption switches can be set in a model by
% declaring statements like:
% algorithm_option_switch(cw_assumption, off). (see algorithm help: 'ah')
% these statements can be present in the isa or rules file which are simply consulted.
%(The library and input files are consulted AND checked by garp.)
override_defaults:-
    forall(algorithm_option_switch(Assumption, OnOff), (set_switch(Assumption, OnOff))),
    !.

%dummy switch for cases when no overrides are present
set_switch(dummy, dummy):-!.

set_switch(max_depth, Value):- % a custom predicate for max_depth which uses numbers instead of fail/true
	Value >= 0,
	Value < 100,
	flag(max_depth, _, Value),
	!.

%illegal assumption name
set_switch(Assumption, OnOff):-
    flag(Assumption, Flag, Flag), %flag must allready be set to indicate a correct name...
    Flag == 0,
    tracer(warning,'\n\n\n*** ERROR: illegal algorithm option override: wrong name\nalgorithm_option_switch( %w, %w )\n\n\n', [Assumption, OnOff]), %gp3 0.3 changed to tracer
    nl,
    fail.    % go ahead and flag the value anyway... FL
%    !.
%    fail.

set_switch(Assumption, on):-
    flag(Assumption, _, true),
    !.

set_switch(Assumption, off):-
    flag(Assumption, _, fail),
    !.

%illegal assumption value
set_switch(Assumption, OnOff):-
    \+ memberchk(OnOff, [on, off]), % name is correct, but override value is not.
    tracer(warning,'\n\n\n*** ERROR: illegal algorithm option override: wrong value\nalgorithm_option_switch( %w, %w )\n\n\n', [Assumption, OnOff]), %gp3 0.3 changed to tracer
    nl,
    !.
%    fail.  % continue this is not bad enough to cause a full failure of the procedure FL.


% algorithm_assumption_flag(+Flag, +DesiredValue)
%
% use this predicate to test if flag is on or off:
% uninitialised flag will raise error
algorithm_assumption_flag(Flag, _, Option):-
    Flag == 0,
    tracer(warning,'\n\n\n*** ERROR: algorithm option uninitialised: %w\n\n\n',[Option]),
    !.
%, fail.

algorithm_assumption_flag(true, true, _).
algorithm_assumption_flag(fail, fail, _).




