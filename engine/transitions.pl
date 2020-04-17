/*  File: transitions
    Date: May 2004
    Author: Floris Linnebank & Bert Bredeweg
    Purpose: transition algorithms
    Part-of:  GARP (version 2.0)
    Modified: 8 August 2004

    Copyright (c) 2004, University of Amsterdam. All rights reserved.

*/

% GARP 2.0 transition algorithms by FL: March-September 2004

/*** Transitions in GARP 2.0 **************************************************

Up untill GARP 1.7 the transition algorithm used a rule interpreter:

- Firstly terminations were sought each resulting in a to/5 predicate or
structure.
to(
    cause(Causes),          names of transitions
    conditions(Cond),       conditions, to be replaced by:
	results(Res),           results, the changes
	to_state([]),           a list of successor states (empty at first)
	terminated              the status of the state terminated, ordered, closed
    )

- Then ordering rules were used to group and select terminations
- In the closing procedure continuity rules were used to enforce continuity in
parameters which are not involved in the transition.

Problems:
- all conditions are replaced by results not only those which change
- rule format is inflexible and inexpressive,
Advantage: users could adjust rules and define domain specific rules.

The new transition procedure in GARP 2.0 does not use rules.
It does use the same to/5 predicate be it with new semantics though:
- conditions are only the statements that must be replaced,
- results only specify changing statements (or statements that
are deliberately kept stable)
The same Terminate, Order, Close outline is kept. But specific procedures are
used for different types of terminations, and different types of orderings.

Most significant changes:
- the cio mathematical model with it's is_derivable and solve procedures is
used for more powerfull deductions
- using the solve deductions aside from strict merges also partial merges are
made when a termination should merges with either X or Y but cannot occur
alone. And also impossible combinations are derived. This way a lot of work
is saved in closing transitions
- exogenous and constants are introduced as new concepts (see elsewhere and thesis on www)
- A more strict approach to statements about derivatives is adopted:
in closing they will be weakened one step (eg. > becomes >= ) because the
second derivative is outside of GARP's horizon


FL NEW june 07

As Garp is now working with second order derivatives, the normal
derivatives can terminate.
Also derivatives are more strictly constrained in immediate transitions.
Ordering of value and derivative terminations is still under
construction.


******************************************************************************/


/****************************************************************************
***____________________________ TERMINATIONS _____________________________***
***                                                                       ***
****************************************************************************/

% Terminations are sought for by a findall call in methods.pl

% value terminations:
single_termination(SMD, To):-
    smd_slots(SMD, _Name, _SE, _P, VList, RList, _SS, _IS),
    % constants are in a list in store structure (see selector.pl)
    % remove constants, they do not terminate:
    get_store(SMD, Store),
    store_cv(Store, Constant),
    subtract_constant_values(VList, Constant, Rest),
    % take a value statement and see if it terminates, backtrackable
    member(Value, Rest),
    value_termination(Value, RList, Store, To).

% derivative terminations:
% new FL june 07
% Strategy: only generate derivative terminations for quantities
% with 2 or more influencing variables. Because in these cases an
% unknown inequality may drive the change. thus a medium for transition
% is needed. In case of 1 influencing variable there is no need for a
% medium because a value change (I) or another derivative change (P)
% will trigger it.
single_termination(SMD, To):-
    flag(derivative_terminations, Flag, Flag),
    algorithm_assumption_flag(Flag, true, derivative_terminations),
    smd_slots(SMD, _Name, _SE, _P, VList, RList, _SS, _IS),
    % constants are in a list in store structure (see selector.pl)
    % remove constants, they do not terminate:
    get_store(SMD, Store),
    store_cv(Store, Constant),
    subtract_constant_values(VList, Constant, Rest),
    store_sod(Store, SOD),
    % take a 2nd order derivative statement and see if the qty derivative terminates, backtrackable
    member(hod_info(Par, _Der, SODer, TODer, [_,_|_], _TODInfluences, _), SOD), % 3rd element is 2nd Der, 4th element is 3rd Der
                                                                                 % (not always multiple influenced! so check with [_,_|_] structure)
    memberchk(value(Par, Q, Val, Der), Rest),
    (
      memberchk(SODer, [pos, neg]) % only definite 2nd order derivatives drive change...
%    ;
    % FL Feb 2012: for the postfilter a 3rd der termination is not necessary
    % multiple influenced higher order derivatives are not split out (ambiguous)
    % so they need not be terminating by themselves. just freeing them is enough
    %(in the continuity constraints... Then the SOD will change and after that the 1st Der...
    % not immediately the 1st Der from a zero SOD state as in the commented out termination below
%      memberchk(TODer, [pos, neg]), % or a 3rd derivative termination
%      SODer = zero,
%      Der = zero
    ),
    derivative_termination(value(Par, Q, Val, Der), SODer, TODer, RList, Store, To).


% ambiguous derivative terminations:
% new FL mar 08
% ambiguous derivatives can cause dead ends in simulations where in reality they may be changing
% so for these cases terminations can be assumed.
% however, if they are already terminating because of SOD, they should not also be assumed...
single_termination(SMD, To):-
    flag(full_branching_derivative_terminations, Flag, Flag),
    algorithm_assumption_flag(Flag, true, full_branching_derivative_terminations),
    smd_slots(SMD, _Name, _SE, _P, VList, RList, _SS, _IS),
    % constants are in a list in store structure (see selector.pl)
    % remove constants, they do not terminate:
    get_store(SMD, Store),
    store_cv(Store, Constant),
    subtract_constant_values(VList, Constant, Rest),
    store_amb(Store, AMB),
    store_sod(Store, SOD),
    % take an ambiguous derivative and terminate it if it doesnt terminate because of 2nd order derivative info. backtrackable
    member(derivative(Par), AMB),
    not_hod_termination(Par, SOD, DDer),
    memberchk(value(Par, Q, Val, Der), Rest),
    assumed_derivative_termination(value(Par, Q, Val, Der), DDer, RList, Store, To).




% simple equality terminations:
single_termination(SMD, To):-
    smd_slots(SMD, _Name, _SE, _P, VList, RList, _SS, _IS),
    % remove constants, they do not terminate:
    get_store(SMD, Store),
    store_cr(Store, Constant),
    subtract(RList, Constant, Rest),
    % take an inequality statement and see if it terminates, backtrackable
    member(Relation, Rest),
    simple_equality_termination(Relation, VList, Store, To).

% exogenous terminations continuous: (sinusoidal)
single_termination(SMD, To):-
    smd_slots(SMD, _Name, _SE, _P, VList, RList, _SS, _IS),
    get_store(SMD, Store),
    store_ec(Store, EC), % list of continuous exogenous parameters
    store_cd(Store, CD), % constant derivatives cannot terminate:
    subtract(EC, CD, EX),% remove those
    member(Par, EX),
    memberchk(value(Par, Q, Val, Der), VList),
    nonvar(Val), % no termination if value is unknown
    nonvar(Der), % Derivative should always be known for exogenous, but checking never hurts
    qspace(Par, _, [First|Rest], _),
    % FL 22-2-2007: In single point/interval q-space, Rest = []
    (Rest \= [] -> common_last(Rest, Last); First = Last),
    in_extreme_value(Val, First, Last, NewDirection), % no termination if not in extreme values of qspace
    exogenous_termination(NewDirection, value(Par, Q, Val, Der), RList, Store, To),
    etrace(transitions_term_exo_sinus, [Par, Val, Der, To], termination).

% exogenous terminations free:
single_termination(SMD, To):-
    smd_slots(SMD, _Name, _SE, _P, VList, RList, _SS, _IS),
    get_store(SMD, Store),
    store_ef(Store, EF), % list of free exogenous parameters
    store_cd(Store, CD), % constant derivatives cannot terminate:
    subtract(EF, CD, EX),% remove those
    member(Par, EX),
    memberchk(value(Par, Q, Val, Der), VList),
    nonvar(Val), % no termination if value is unknown
    nonvar(Der), % Derivative should always be known for exogenous, but checking never hurts
    qspace(Par, _, [First|Rest], _),
    % FL 22-2-2007: In single point/interval q-space, Rest = []
    (Rest \= [] -> common_last(Rest, Last); First = Last),
    in_extreme_point(Val, First, Last, NewDirection),  %only restrictive if in extreme point.
    exogenous_termination(NewDirection, value(Par, Q, Val, Der), RList, Store, To),
    etrace(transitions_term_exo_random, [Par, Val, Der, To], termination).

% exogenous terminations parabola positive:
single_termination(SMD, To):-
    smd_slots(SMD, _Name, _SE, _P, VList, RList, _SS, _IS),
    get_store(SMD, Store),
    store_pp(Store, PP), % list of continuous exogenous parameters
    store_cd(Store, CD), % constant derivatives cannot terminate:
    subtract(PP, CD, EX),% remove those
    member(Par, EX),
    memberchk(value(Par, Q, Val, Der), VList),
    nonvar(Val), % no termination if value is unknown
    nonvar(Der), % Derivative should always be known for exogenous, but checking never hurts
    qspace(Par, _, SpaceList, _),
    common_last(SpaceList, Top),
    in_top_value(Val, Top, NewDirection), % no termination if not in top value of qspace
    exogenous_termination(NewDirection, value(Par, Q, Val, Der), RList, Store, To),
    etrace(transitions_term_exo_pos_parabola, [Par, Val, Der, To], termination).

% exogenous terminations parabola negative:
single_termination(SMD, To):-
    smd_slots(SMD, _Name, _SE, _P, VList, RList, _SS, _IS),
    get_store(SMD, Store),
    store_np(Store, NP), % list of continuous exogenous parameters
    store_cd(Store, CD), % constant derivatives cannot terminate:
    subtract(NP, CD, EX),% remove those
    member(Par, EX),
    memberchk(value(Par, Q, Val, Der), VList),
    nonvar(Val), % no termination if value is unknown
    nonvar(Der), % Derivative should always be known for exogenous, but checking never hurts
    qspace(Par, _, [Bottom|_], _),
    in_bottom_value(Val, Bottom, NewDirection), % no termination if not in top value of qspace
    exogenous_termination(NewDirection, value(Par, Q, Val, Der), RList, Store, To),
    etrace(transitions_term_exo_pos_parabola, [Par, Val, Der, To], termination).




%subtract_constant_values(+Values, +ConstantParameters, -NotConstantValues)
subtract_constant_values([], _, []).

% constant: subtract
subtract_constant_values([value(Par, _, _, _)|T], Constant, NT):-
    memberchk(Par, Constant),
    !,
    subtract_constant_values(T, Constant, NT).

% constant: keep
subtract_constant_values([value(Par, Q, Val, Der)|T], Constant, [value(Par, Q, Val, Der)|NT]):-
    subtract_constant_values(T, Constant, NT).


%looks for sod termination conditions then fails...
% if these conditions hold a normal derivative termination
% will have fired...
not_hod_termination(Q, HODList, _):-
	memberchk(hod_info(Q, D, SOD, TOD, _, _, _), HODList),
	(
	  sod_termination_possible(D, SOD)
	;
	  tod_termination_possible(D, SOD, TOD)
	),
	!,
	fail.

% ok. return Second derivative
not_hod_termination(Q, HODList, SOD):-
	(memberchk(hod_info(Q, _D, SOD, _TOD, _, _, _), HODList)
	;
	SOD = unknown),
	!.

sod_termination_possible(neg, pos).
sod_termination_possible(zero, pos).
sod_termination_possible(pos, neg).
sod_termination_possible(zero, neg).

tod_termination_possible(zero, zero, pos).
tod_termination_possible(zero, zero, neg).



/*__________________________Exogenous Terminations__________________________*/

% Exogenous terminations concern derivatives! the continuous type only changes
% derivative when ariving in an extreme value, the free type can change derivative
% at any time.

% continuous exogenous variable only terminates in extreme values
in_extreme_value(Val, Val, _, up).
in_extreme_value(Val, _, Val, down).
in_extreme_value(Val, point(Val), _, up).
in_extreme_value(Val, _, point(Val), down).

% free exogenous variable needs direction when in extreme value which is a point
% and q-space constraints are active for that point
in_extreme_point(Val, point(Val), _, up):-
    active_qspace_constraints(Val),
    !.
in_extreme_point(Val, _, point(Val), down):-
    active_qspace_constraints(Val),
    !.
in_extreme_point(_, _, _, any). %not in extreme constrained point return variable for free direction

% parabola exogenous variable only terminates in extreme values
in_top_value(Val, Val, down).
in_top_value(Val, point(Val), down).

in_bottom_value(Val, Val, up).
in_bottom_value(Val, point(Val), up).



% see algorithm_help for switches and qspace constraints
active_qspace_constraints(zero):-
    flag(free_zero_derivative, Flag, Flag),
    algorithm_assumption_flag(Flag, fail, free_zero_derivative),
    !.

active_qspace_constraints(_):-
    flag(free_maxmin_derivative, Flag, Flag),
    algorithm_assumption_flag(Flag, fail, free_maxmin_derivative),
    !.

% exogenous_termination(+Direction, +Value, +RList, +Store, -To)
% exogenous variables terminate their derivatives
% directions:
% down: plus2zero, or zero2min,
% up: min2zero, or zero2plus,
% any: all above

% plus2zero
exogenous_termination(NewDirection, value(Par, Q, Val, plus), _RList, _Store, to(
        cause([exogenous_up_to_stable(Par)]),
        conditions([par_values([value(Par, Q, Val, plus)])]),
	results([par_values([value(Par, Q, Val, zero)])]),
	to_state([]),
	terminated
        )):-
    memberchk(NewDirection, [down, any]).

% zero2min
exogenous_termination(NewDirection, value(Par, Q, Val, zero), _RList, _Store, to(
        cause([exogenous_stable_to_down(Par)]),
        conditions([par_values([value(Par, Q, Val, zero)])]),
	results([par_values([value(Par, Q, Val, min)])]),
	to_state([]),
	terminated
        )):-
    memberchk(NewDirection, [down, any]).

% min2zero
exogenous_termination(NewDirection, value(Par, Q, Val, min), _RList, _Store, to(
        cause([exogenous_down_to_stable(Par)]),
        conditions([par_values([value(Par, Q, Val, min)])]),
	results([par_values([value(Par, Q, Val, zero)])]),
	to_state([]),
	terminated
        )):-
    memberchk(NewDirection, [up, any]).

% zero2plus
exogenous_termination(NewDirection, value(Par, Q, Val, zero), _RList, _Store, to(
        cause([exogenous_stable_to_up(Par)]),
        conditions([par_values([value(Par, Q, Val, zero)])]),
	results([par_values([value(Par, Q, Val, plus)])]),
	to_state([]),
	terminated
        )):-
    memberchk(NewDirection, [up, any]).


/*_____________________________Value Terminations___________________________*/

% NB a value may have an equivalent inequality statement:
% value(X, _, zero, _ ) = equal(X, zero)
% these terminate together here, hence value & relation change

% value & relation change to point above:
value_termination(value(Par, Q, Interval, Der), RList, Store, to(
        cause([Termination]),
        conditions([par_values([value(Par, Q, Interval, Der)])|OldR]),
	results([par_values(Values), par_relations(Relations)|NewR]),
	to_state([]),
	terminated
        )):-
	nonvar(Interval),
    % derivative must be plus or within constraints of algorithm option switch settings
    check_derivative_condition(Der, Par, Store, plus, Assumptions),
    % there is a next point
    check_quantity_spaces([meets( Par, [ interval( Interval ), point( Point ) ] )]),
    % change inequality if present
    value_relation_change(Der, Par, RList, Point, OldR, NewR),
    % apply continuity
    store_sod(Store, SOD),
    do_value_continuity([value(Par, Q, Point, Der)], [], SOD, Values, CRelations, large),
    append(Assumptions, CRelations, Relations),
    % determine name: (assumed or not)
    (
      Assumptions = []
    ->
      Termination = to_point_above(Par),
      Trace = to_point_above
    ;
      Termination = assumed_to_point_above(Par),
      Trace = assumed_to_point_above
    ),
    % supply tracer with information
    value_termination_etrace(Par, Interval, plus, Trace, Point,
                             Assumptions, OldR, NewR).


% value & relation change to point below
value_termination(value(Par, Q, Interval, Der), RList, Store, to(
        cause([Termination]),
        conditions([par_values([value(Par, Q, Interval, Der)])|OldR]),
	results([par_values(Values), par_relations(Relations)|NewR]),
	to_state([]),
	terminated
        )):-
	nonvar(Interval),
    check_derivative_condition(Der, Par, Store, min, Assumptions),
    check_quantity_spaces([meets( Par, [ point( Point ), interval( Interval ) ] )]),
    value_relation_change(Der, Par, RList, Point, OldR, NewR),
    store_sod(Store, SOD),
    do_value_continuity([value(Par, Q, Point, Der)], [], SOD, Values, CRelations, large),
    append(Assumptions, CRelations, Relations),
    (
      Assumptions = []
    ->
      Termination = to_point_below(Par),
      Trace = to_point_below
    ;
      Termination = assumed_to_point_below(Par),
      Trace = assumed_to_point_below
    ),
    value_termination_etrace(Par, Interval, min, Trace, Point,
                             Assumptions, OldR, NewR).


% value & relation change to interval above
value_termination(value(Par, Q, Point, Der), RList, Store, to(
        cause([Termination]),
        conditions([par_values([value(Par, Q, Point, Der)])|OldR]),
	results([par_values(Values), par_relations(Relations)|NewR]),
	to_state([]),
	terminated
        )):-
	nonvar(Point),
    check_derivative_condition(Der, Par, Store, plus, Assumptions),
    check_quantity_spaces([meets( Par, [ point( Point ), interval( Interval ) ] )]),
    value_relation_change(Der, Par, RList, Point, OldR, NewR),
    store_sod(Store, SOD),
    do_value_continuity([value(Par, Q, Interval, Der)], [], SOD, Values, CRelations, small),
    append(Assumptions, CRelations, Relations),
    (
      Assumptions = []
    ->
      Termination = to_interval_above(Par),
      Trace = to_interval_above
    ;
      Termination = assumed_to_interval_above(Par),
      Trace = assumed_to_interval_above
    ),
    value_termination_etrace(Par, Point, plus, Trace, Interval,
                             Assumptions, OldR, NewR).


% value & relation change to interval below
value_termination(value(Par, Q, Point, Der), RList, Store, to(
        cause([Termination]),
        conditions([par_values([value(Par, Q, Point, Der)])|OldR]),
	results([par_values(Values), par_relations(Relations)|NewR]),
	to_state([]),
	terminated
        )):-
	nonvar(Point),
    check_derivative_condition(Der, Par, Store, min, Assumptions),
    check_quantity_spaces([meets( Par, [ interval( Interval ), point( Point ) ] )]),
    value_relation_change(Der, Par, RList, Point, OldR, NewR),
    store_sod(Store, SOD),
    do_value_continuity([value(Par, Q, Interval, Der)], [], SOD, Values, CRelations, small),
    append(Assumptions, CRelations, Relations),
    (
      Assumptions = []
    ->
      Termination = to_interval_below(Par),
      Trace = to_interval_below
    ;
      Termination = assumed_to_interval_below(Par),
      Trace = assumed_to_interval_below
    ),
    value_termination_etrace(Par, Point, min, Trace, Interval,
                             Assumptions, OldR, NewR).


% value_relation_change(+Der, +Par, +RList, +Point, -OldRelation, -NewRelation)
% returns changing relations if present in SMD

% NB relations_sub_set even succeeds when relation not present, but consistent
% with value because of try value consistency patch,
% also if alternative_relation is used we will not know, and try to remove the
% wrong relation. Therefore this is not used and all different possibilities have their clause

% to point above (1)
value_relation_change(plus, Par, RList, Point, [par_relations([smaller(Par, Point)])],
                                               [par_relations([equal( Par, Point )])]):-
	memberchk(smaller(Par, Point), RList),!.

% to point above (2)
value_relation_change(plus, Par, RList, Point, [par_relations([greater(Point, Par)])],
                                               [par_relations([equal( Par, Point )])]):-
	memberchk(greater(Point, Par), RList),!.

% to point below (1)
value_relation_change(min, Par, RList, Point, [par_relations([greater(Par, Point)])],
                                              [par_relations([equal( Par, Point )])]):-
	memberchk(greater(Par, Point), RList), !.

% to point below (2)
value_relation_change(min, Par, RList, Point, [par_relations([smaller(Point, Par)])],
                                              [par_relations([equal( Par, Point )])]):-
	memberchk(smaller(Point, Par), RList), !.

% to interval above
value_relation_change(plus, Par, RList, Point, [par_relations([equal(Par, Point)])],
                                               [par_relations([greater( Par, Point )])]):-
	memberchk(equal(Par, Point), RList), !.

% to interval below
value_relation_change(min, Par, RList, Point, [par_relations([equal(Par, Point)])],
                                              [par_relations([smaller( Par, Point )])]):-
	memberchk(equal(Par, Point), RList), !.

% no value relation present.
value_relation_change(_, _, _, _, [], []).


% check_derivative_condition(+Der, +Par, +Store, +plus, -Assumedrelation)
% determine if the derivative of a value is suitable
% for the calling termination,
% return assumptions (with continuity applied)

% derivative is defined plus
check_derivative_condition(Der, _, _, plus, []):-
    nonvar(Der),
    Der = plus,
    !.

% derivative is defined min
check_derivative_condition(Der, _, _, min, []):-
    nonvar(Der),
    Der = min,
    !.

% derivative is defined zero. no termination should fire
check_derivative_condition(Der, _, _, _, _):-
    nonvar(Der),
    Der = zero,
    !, fail.

% derivative is undefined but can be assumed plus
check_derivative_condition(Der, Par, Store, plus, [d_greater_or_equal(Par, zero)]):-
    var(Der),
    flag(full_branching_value_terminations, Flag, Flag),
    algorithm_assumption_flag(Flag, true, full_branching_value_terminations),
    store_cio(Store, Cio),
    store_qt(Store, Q),
    memberchk(derivative(Par)/Pmap, Q),
    list_map([0], Zero),
    cio_d(Cio, Derivable),
    !, %do once,
    \+ is_derivable(relation(Zero, >=, Pmap), Derivable),
    Der = plus.

% derivative is undefined but can be assumed min
check_derivative_condition(Der, Par, Store, min, [d_smaller_or_equal(Par, zero)]):-
    var(Der),
    flag(full_branching_value_terminations, Flag, Flag),
    algorithm_assumption_flag(Flag, true, full_branching_value_terminations),
    store_cio(Store, Cio),
    store_qt(Store, Q),
    memberchk(derivative(Par)/Pmap, Q),
    list_map([0], Zero),
    cio_d(Cio, Derivable),
    !,  %do once,
    \+ is_derivable(relation(Pmap, >=, Zero), Derivable),
    Der = min.

% derivative is undefined, and even no information in Cio: can be assumed plus
check_derivative_condition(Der, Par, _Store, plus, [d_greater_or_equal(Par, zero)]):-
    var(Der),
    flag(full_branching_value_terminations, Flag, Flag),
    algorithm_assumption_flag(Flag, true, full_branching_value_terminations),
    Der = plus.

% derivative is undefined, and even no information in Cio: can be assumed min
check_derivative_condition(Der, Par, _Store, min, [d_smaller_or_equal(Par, zero)]):-
    var(Der),
    flag(full_branching_value_terminations, Flag, Flag),
    algorithm_assumption_flag(Flag, true, full_branching_value_terminations),
    Der = min.



/*_____________________Derivative Terminations__________________________*/

% NB a derivative may have an equivalent inequality statement:
% value(X, _, _, zero) = d_equal(X, zero)
% these terminate together here, hence derivative & relation change
% (copied text from value terminations, derivative relation changes
% not active yet.)
%
% FL jan 2012: added 3rd order derivative terminations...

% derivative & relation change to point above: plus
derivative_termination(value(Par, _Q, _Interval, Der), pos, _TODer, _RList, _Store, to(
        cause([derivative_down_to_stable(Par)]),
%        conditions([par_values([value(Par, Q, Interval, Der)])|OldR]),
        conditions([]),
%	results([par_values(Values), par_relations(Relations)|NewR]),
	results([par_relations([d_equal(Par, zero)])]),
	to_state([]),
	terminated
        )):-
	nonvar(Der),
	Der = min,
    % change inequality if present
    %derivative_relation_change(pos, Par, RList, OldR, _NewR),
    % supply tracer with information
    derivative_termination_etrace(Par, Der, plus, zero, derivative_down_to_stable, sod).

% derivative & relation change to point below
derivative_termination(value(Par, _Q, _Interval, Der), neg, _TODer, _RList, _Store, to(
        cause([derivative_up_to_stable(Par)]),
        %conditions([par_values([value(Par, Q, Interval, Der)])|OldR]),
	%results([par_values(Values), par_relations(Relations)|NewR]),
	conditions([]),
	results([par_relations([d_equal(Par, zero)])]),
	to_state([]),
	terminated
        )):-
	nonvar(Der),
	Der = plus,
    %derivative_relation_change(neg, Par, RList, OldR, _NewR),
    % supply tracer with information
    derivative_termination_etrace(Par, Der, min, zero, derivative_up_to_stable, sod).

% derivative & relation change to interval above
derivative_termination(value(Par, _Q, _Point, Der), SODer, _TODer, _RList, _Store, to(
        cause([derivative_stable_to_up(Par)]),
        conditions([]),
	results([par_relations([d_greater(Par, zero)])]),
	to_state([]),
	terminated
        )):-
	nonvar(Der),
	Der = zero,
%	(
%	  SODer = pos
%	->
%	  SODTOD = sod
%	;
%	  SODer = zero,
%	  TODer = pos,
%	  SODTOD = tod
%	),
	SODer = pos,  % TOD terminations not used after all...
	SODTOD = sod,

    % derivative_relation_change(Der, Par, RList, OldR, _NewR).
    % supply tracer with information
    derivative_termination_etrace(Par, Der, plus, plus, derivative_stable_to_up, SODTOD).

% derivative & relation change to interval below
derivative_termination(value(Par, _Q, _Point, Der), SODer, _TODer, _RList, _Store, to(
        cause([derivative_stable_to_down(Par)]),
        %conditions([par_values([value(Par, Q, Point, Der)])|OldR]),
	%results([par_values(Values), par_relations(Relations)|NewR]),
	conditions([]),
	results([par_relations([d_smaller(Par, zero)])]),
	to_state([]),
	terminated
        )):-
	nonvar(Der),
	Der = zero,
%	(
%	  SODer = neg
%	->
%	  SODTOD = sod
%	;
%	  SODer = zero,
%	  TODer = neg,
%	  SODTOD = tod
%	),
	SODer = neg,  % TOD terminations not used after all...
	SODTOD = sod,

    %derivative_relation_change(Der, Par, RList, OldR, _NewR).
    % supply tracer with information
    derivative_termination_etrace(Par, Der, min, min, derivative_stable_to_down, SODTOD).


/*
% derivative_relation_change(+Der, +Par, +RList, +Point, -OldRelation, -NewRelation)
% returns changing relations if present in SMD

% NB relations_sub_set even succeeds when relation not present, but consistent
% with value because of try value consistency patch,
% also if alternative_relation is used we will not know, and try to remove the
% wrong relation. Therefore this is not used and all different possibilities have their clause

% to point above (1)
derivative_relation_change(pos, Par, RList, [par_relations([d_smaller(Par, zero)])],
                                               [par_relations([d_equal( Par, zero )])]):-
	memberchk(d_smaller(Par, zero), RList),!.

% to point above (2)
derivative_relation_change(pos, Par, RList, [par_relations([d_greater(zero, Par)])],
                                               [par_relations([d_equal( Par, zero )])]):-
	memberchk(d_greater(zero, Par), RList),!.

% to point below (1)
derivative_relation_change(neg, Par, RList, [par_relations([d_greater(Par, zero)])],
                                              [par_relations([d_equal( Par, zero )])]):-
	memberchk(d_greater(Par, zero), RList), !.

% to point below (2)
derivative_relation_change(neg, Par, RList, [par_relations([d_smaller(zero, Par)])],
                                              [par_relations([d_equal( Par, zero )])]):-
	memberchk(d_smaller(zero, Par), RList), !.

% to interval above
derivative_relation_change(pos, Par, RList, [par_relations([d_equal(Par, zero)])],
                                               [par_relations([d_greater( Par, zero )])]):-
	memberchk(d_equal(Par, zero), RList), !.

% to interval below
derivative_relation_change(neg, Par, RList, [par_relations([d_equal(Par, zero)])],
                                              [par_relations([d_smaller( Par, zero )])]):-
	memberchk(d_equal(Par, zero), RList), !.

% no value relation present.
derivative_relation_change(_, _, _, [], []).


*/

% New FL mar 08
% assumed der terminations.
% terminate if sod is not contradictive
% SecondOrderDerivative can be [pos, neg, zero, pos_zero, neg_zero, unknown]
%

% derivative & relation change to point above: plus
assumed_derivative_termination(value(Par, _Q, _Interval, Der), Sod, _RList, _Store, to(
        cause([assumed_derivative_down_to_stable(Par)]),
%        conditions([par_values([value(Par, Q, Interval, Der)])|OldR]),
        conditions([]),
%	results([par_values(Values), par_relations(Relations)|NewR]),
	results([par_relations([d_equal(Par, zero)])]),
	to_state([]),
	terminated
        )):-
	nonvar(Der),
	Der = min,
	memberchk(Sod, [pos, pos_zero, unknown]), % should pos be part of the list? shouldnt this cause a definitive termination? how about single influence cases?? FL
    % change inequality if present
    %derivative_relation_change(pos, Par, RList, OldR, _NewR),
    % supply tracer with information
    assumed_derivative_termination_etrace(Par, Der, plus, zero, assumed_derivative_down_to_stable).

% derivative & relation change to point below
assumed_derivative_termination(value(Par, _Q, _Interval, Der), Sod, _RList, _Store, to(
        cause([assumed_derivative_up_to_stable(Par)]),
        %conditions([par_values([value(Par, Q, Interval, Der)])|OldR]),
	%results([par_values(Values), par_relations(Relations)|NewR]),
	conditions([]),
	results([par_relations([d_equal(Par, zero)])]),
	to_state([]),
	terminated
        )):-
	nonvar(Der),
	Der = plus,
	memberchk(Sod, [neg, neg_zero, unknown]), % should pos be part of the list? shouldnt this cause a definitive termination? how about single influence cases?? FL
    %derivative_relation_change(neg, Par, RList, OldR, _NewR),
    % supply tracer with information
    assumed_derivative_termination_etrace(Par, Der, min, zero, assumed_derivative_up_to_stable).

% derivative & relation change to interval above
assumed_derivative_termination(value(Par, _Q, _Point, Der), Sod, _RList, _Store, to(
        cause([assumed_derivative_stable_to_up(Par)]),
        conditions([]),
	results([par_relations([d_greater(Par, zero)])]),
	to_state([]),
	terminated
        )):-
	nonvar(Der),
	Der = zero,
	memberchk(Sod, [pos, pos_zero, unknown]), % should pos be part of the list? shouldnt this cause a definitive termination? how about single influence cases?? FL
    % derivative_relation_change(Der, Par, RList, OldR, _NewR).
    % supply tracer with information
    assumed_derivative_termination_etrace(Par, Der, plus, plus, assumed_derivative_stable_to_up).

% derivative & relation change to interval below
assumed_derivative_termination(value(Par, _Q, _Point, Der), Sod, _RList, _Store, to(
        cause([assumed_derivative_stable_to_down(Par)]),
        %conditions([par_values([value(Par, Q, Point, Der)])|OldR]),
	%results([par_values(Values), par_relations(Relations)|NewR]),
	conditions([]),
	results([par_relations([d_smaller(Par, zero)])]),
	to_state([]),
	terminated
        )):-
	nonvar(Der),
	Der = zero,
	memberchk(Sod, [neg, neg_zero, unknown]), % should pos be part of the list? shouldnt this cause a definitive termination? how about single influence cases?? FL
    %derivative_relation_change(Der, Par, RList, OldR, _NewR).
    % supply tracer with information
    assumed_derivative_termination_etrace(Par, Der, min, min, assumed_derivative_stable_to_down).




/*_____________________Simple Equality Terminations__________________________*/

/* FL March 2004, Notes:

par-par relation terminations:

two terminating styles:
(under algorithm assumption switch: full branching equality terminations)
- only when definite: assume steady behaviour in case dX, dY relation unknown
- when possible within constraints: full branching

also under algorithm option switch: terminate weak relations: >= & =<

NB par-landmark relations are taken care of by valueterminations

(todo?complex par relations under switch)

*/


% finds terminations for simple equalities
simple_equality_termination(Relation, VList, Store, To):-
    Relation =.. [RType, Par1, Par2],
    % no correspondences, if's, d_relations, etc.
    memberchk(RType, [greater, smaller, equal, greater_or_equal, smaller_or_equal]),
    % it is composed of parameters, not nested relations or landmarks (e.g. point(Par1), or zero)
    memberchk(value(Par1, _Q1, _Val1, _Der1), VList),
    memberchk(value(Par2, _Q2, _Val2, _Der2), VList),
    % values are known ( ???? FL, should this be a constraint?: NO)
%    nonvar(Val1), % if derivative relation is known, values are not adding information.
%    nonvar(Val2),
    get_derivative_relation(Par1, Par2, Store, DRel),
    equality_termination(RType, DRel, Par1, Par2, Relation, To).


% equality_termination(+RelType, +DerivativeRel, +Par1, +Par2, +Relation, -To)
%
% constructs termination for Relation, if DerivativeRel is ok according to
% algorithm assumptions
%
% Relation can be a complex relation. were dRel is the composite dRel
% & P1, P2 are the complex terms: plus(X, Y), etc.


% no termination in any case if derivatives known to be equal.
equality_termination(_, =, Par1, Par2, Relation, _):-
    !,
    equality_termination_tracer(Relation, d_equal(Par1, Par2), _, _, known_equal),
    fail.

% termination Leftup
% add continuous version of derivative relation
equality_termination(RType, >, Par1, Par2, Relation, to(cause([Cause]),
                                                        conditions([Cond]),
                                                        results([Res]),
                                                        to_state([]),
                                                        terminated)):-
    % find new relation:
    relation_up(RType, NewRType),
    % construct termination name:
    atom_concat(from_, RType, C1),
    atom_concat(C1, '_to_', C2),
    atom_concat(C2, NewRType, C3),
    Cause =.. [C3, Par1, Par2],
    % construct conditions:
    Cond = par_relations([Relation]),
    % construct Results:
    NewRelation =.. [NewRType, Par1, Par2],
    Res = par_relations([NewRelation, d_greater_or_equal(Par1, Par2)]),
    % supply tracer with information:
    equality_termination_tracer(Relation, d_greater(Par1, Par2), NewRelation, C3, known).

% termination leftdown
% add continuous version of derivative relation
equality_termination(RType, <, Par1, Par2, Relation, to(cause([Cause]),
                                                        conditions([Cond]),
                                                        results([Res]),
                                                        to_state([]),
                                                        terminated)):-
    relation_down(RType, NewRType),
    atom_concat(from_, RType, C1),
    atom_concat(C1, '_to_', C2),
    atom_concat(C2, NewRType, C3),
    Cause =.. [C3, Par1, Par2],
    Cond = par_relations([Relation]),
    NewRelation =.. [NewRType, Par1, Par2],
    Res = par_relations([NewRelation, d_smaller_or_equal(Par1, Par2)]),
    equality_termination_tracer(Relation, d_smaller(Par1, Par2), NewRelation, C3, known).

% termination Leftup assumed for weak derivative relation
% add continuous version of assumed derivative relation: nothing
equality_termination(RType, >=, Par1, Par2, Relation, to(cause([Cause]),
                                                        conditions([Cond]),
                                                        results([Res]),
                                                        to_state([]),
                                                        terminated)):-
    flag(full_branching_equality_terminations, Flag, Flag),
    algorithm_assumption_flag(Flag, true, full_branching_equality_terminations),
    relation_up(RType, NewRType),
    atom_concat(assumed_from_, RType, C1),
    atom_concat(C1, '_to_', C2),
    atom_concat(C2, NewRType, C3),
    Cause =.. [C3, Par1, Par2],
    Cond = par_relations([Relation]),
    NewRelation =.. [NewRType, Par1, Par2],
    Res = par_relations([NewRelation, d_greater_or_equal(Par1, Par2)]),
    equality_termination_tracer(Relation, d_greater_or_equal(Par1, Par2), NewRelation, C3, weak_known).

% termination leftdown assumed for weak derivative relation
% add continuous version of assumed derivative relation: nothing
equality_termination(RType, =<, Par1, Par2, Relation, to(cause([Cause]),
                                                        conditions([Cond]),
                                                        results([Res]),
                                                        to_state([]),
                                                        terminated)):-
    flag(full_branching_equality_terminations, Flag, Flag),
    algorithm_assumption_flag(Flag, true, full_branching_equality_terminations),
    relation_down(RType, NewRType),
    atom_concat(assumed_from_, RType, C1),
    atom_concat(C1, '_to_', C2),
    atom_concat(C2, NewRType, C3),
    Cause =.. [C3, Par1, Par2],
    Cond = par_relations([Relation]),
    NewRelation =.. [NewRType, Par1, Par2],
    Res = par_relations([NewRelation, d_smaller_or_equal(Par1, Par2)]),
    equality_termination_tracer(Relation, d_smaller_or_equal(Par1, Par2), NewRelation, C3, weak_known).

% termination Leftup assumed for unknown derivative relation
% add continuous version of assumed derivative relation
equality_termination(RType, unknown, Par1, Par2, Relation, to(cause([Cause]),
                                                        conditions([Cond]),
                                                        results([Res]),
                                                        to_state([]),
                                                        terminated)):-
    flag(full_branching_equality_terminations, Flag, Flag),
    algorithm_assumption_flag(Flag, true, full_branching_equality_terminations),
    relation_up(RType, NewRType),
    atom_concat(assumed_from_, RType, C1),
    atom_concat(C1, '_to_', C2),
    atom_concat(C2, NewRType, C3),
    Cause =.. [C3, Par1, Par2],
    Cond = par_relations([Relation]),
    NewRelation =.. [NewRType, Par1, Par2],
    Res = par_relations([NewRelation, d_greater_or_equal(Par1, Par2)]),
    equality_termination_tracer(Relation, d_greater(Par1, Par2), NewRelation, C3, unknown).

% termination leftdown assumed for unknown derivative relation
% add continuous version of assumed derivative relation
equality_termination(RType, unknown, Par1, Par2, Relation, to(cause([Cause]),
                                                        conditions([Cond]),
                                                        results([Res]),
                                                        to_state([]),
                                                        terminated)):-
    flag(full_branching_equality_terminations, Flag, Flag),
    algorithm_assumption_flag(Flag, true, full_branching_equality_terminations),
    relation_down(RType, NewRType),
    atom_concat(assumed_from_, RType, C1),
    atom_concat(C1, '_to_', C2),
    atom_concat(C2, NewRType, C3),
    Cause =.. [C3, Par1, Par2],
    Cond = par_relations([Relation]),
    NewRelation =.. [NewRType, Par1, Par2],
    Res = par_relations([NewRelation, d_smaller_or_equal(Par1, Par2)]),
    equality_termination_tracer(Relation, d_smaller(Par1, Par2), NewRelation, C3, unknown).


relation_up(smaller, equal).
relation_up(equal, greater).
relation_up(smaller_or_equal, equal):-
    flag(terminate_weak_relations, Flag, Flag),
    algorithm_assumption_flag(Flag, true, terminate_weak_relations).

relation_up(smaller_or_equal, greater):-
    flag(terminate_weak_relations, Flag, Flag),
    algorithm_assumption_flag(Flag, true, terminate_weak_relations).

relation_up(greater_or_equal, greater):-
    flag(terminate_weak_relations, Flag, Flag),
    algorithm_assumption_flag(Flag, true, terminate_weak_relations).

relation_down(greater, equal).
relation_down(equal, smaller).
relation_down(smaller_or_equal, smaller):-
    flag(terminate_weak_relations, Flag, Flag),
    algorithm_assumption_flag(Flag, true, terminate_weak_relations).

relation_down(greater_or_equal, equal):-
    flag(terminate_weak_relations, Flag, Flag),
    algorithm_assumption_flag(Flag, true, terminate_weak_relations).

relation_down(greater_or_equal, smaller):-
    flag(terminate_weak_relations, Flag, Flag),
    algorithm_assumption_flag(Flag, true, terminate_weak_relations).




% check which parameter is moving where
get_derivative_relation(Par1, Par2, Store, DRel):-
    store_cio(Store, Cio),
    store_qt(Store, Q),
    memberchk(derivative(Par1)/P1, Q),
    memberchk(derivative(Par2)/P2, Q),
    !,
    cio_d(Cio, Derivable),
    % NB derivable in store = in intern LIST representation
    % because only is_derivable is used
    try_derivative_relation(P1, P2, Derivable, DRel).

% if either derivative is not in Q-list, derivative relation must be unknown
get_derivative_relation(Par1, Par2, Store, unknown):-
    store_qt(Store, Q),
    (
    \+ memberchk(derivative(Par1)/_P1, Q)
    ;
    \+ memberchk(derivative(Par2)/_P2, Q)
    ),
    !.


% equal
try_derivative_relation(P1, P2, Derivable, =):-
    is_derivable_through_zero(relation(P1, =, P2), Derivable),
    !.

% equal
try_derivative_relation(P1, P2, Derivable, =):-
    is_derivable_through_zero(relation(P1, =, P2), Derivable),
    !.

% dP1 greater
try_derivative_relation(P1, P2, Derivable, >):-
    is_derivable_through_zero(relation(P1, >, P2), Derivable),
    !.

% dP1 smaller
try_derivative_relation(P1, P2, Derivable, <):-
    is_derivable_through_zero(relation(P2, >, P1), Derivable),
    !.


% dP1 greater or equal (can be present together with strong case?)
try_derivative_relation(P1, P2, Derivable, >=):-
    is_derivable_through_zero(relation(P1, >=, P2), Derivable),
    !.

% dP1 smaller or equal (can be present together with strong case?)
try_derivative_relation(P1, P2, Derivable, =<):-
    is_derivable_through_zero(relation(P2, >=, P1), Derivable),
    !.

% last: relation unknown
try_derivative_relation(_P1, _P2, _Derivable, unknown).


% transitivity through zero is not allowed in solve,
% testable for derivatives though:
% e.g. dX >= 0, dY < 0 then, dX > dY

is_derivable_through_zero(relation(P1, Rel, P2), Derivable):-
    is_derivable(relation(P1, Rel, P2), Derivable),
    !.

is_derivable_through_zero(relation(P1, Rel, P2), Derivable):-
    list_map([0], E),
    is_derivable(relation(P1, Rel1, E), Derivable),
    is_derivable(relation(E, Rel2, P2), Derivable),
    add_compatible(Rel1, Rel2, Rel).




/****************************************************************************
***_____________________________ PRECEDENCE ______________________________***
***                                                                       ***
****************************************************************************/

/**** Notes: FL April 2004 ****

% old:
%	apply_order_rules(Terminated, Ordered, SMD),
%	merge_possible_terminations(Ordered, ToFinal1),
%	set_to_list_status_ordered(ToFinal1, ToFinal),

new:

incoming terminations are indexed, and a list of valid and invalid subcombinations
is composed during the procedure to constrain the crossproduct, but still be able to work
with unique single terminations. (smaller, less complexity). See constrained crossproduct

algorithm:
1: merge doubles
2: indexing
3: mutually exclusive for equality terminations
4: mutually exclusive for value terminations
5: mutually exclusive for exogenous terminations
6: correspondences values: removes / merges (via constraints)
7: correspondence remove for equalities
8: correspondences for exogenous terminations
9: terminating equality combined with value termination constraints: partial merges, fatal combinations
10: complex constraints
11: constrained crossproduct
12: epsilon

- 7 is placed under algorithm assumption flag control
- 10 is placed under algorithm assumption flag control
- 12 is placed under algorithm assumption flag control

6, 8 could also be placed under an algorithm assumption flag?

NB NEW FL May 2007 epsilon first or last under switch control

NEW FL nov 2011: phased out epsilon last. Was conceptually not correct
anyhow

NEW FL nov 2011: phased out extra termination rules interpreter.

NEW FL Okt 2011 new procedure for epsilon regarding assumed
immediate terminations:
Terminate ambiguous derivatives will be default active.
This gives us assumed immediate terminations: XI.
Possibilities:
- There are non-assumed immediate terminations: I
These I’s will happen all (merge immidiate) This set is combined with
the crossproduct of all XI's (also the empty set), because XI's
do not have to happen, it depends on what specific state is
actually captured in the generic state description (which is not
specific enough to distinguish the options, thats why the assumed
terminations are there in the first place.
- There are no non-assumed immediates:
   - There are non immediate transitions (Y) (assumed or non-assumed
   doesn't matter here). Then the crossproduct of Y may happen alongside
   the crossproduct of XI. This is because the uncertain XI's may not
   take precedence over other terminations.
   - There are no non-immediate transitions: crossproduct of XI happens

******/

do_precedence(_SMD, [], []):-
	!,
	etrace(transtions_ordering_empty, _, ordering).

do_precedence(SMD, ToIn, ToOut):-
	% merge doubles calls old routine, with trace statements present
	% not necessary anymore... doubles should only occur in rulebased terminations, not used anymore.)
	% merge_double_terminations(ToIn, ToA),

	% test if epsilon ordering is done:
	flag(epsilon_ordering, EpsilonFlag, EpsilonFlag),
	(
	  algorithm_assumption_flag(EpsilonFlag, true, epsilon_ordering)
	->
	  % Default: use epsilon ordering
	  etrace(transitions_epsilon_start, _, ordering),
	  do_precedenceEpsilon(SMD, ToIn, ToOut)
	;
	  %with switch off: dont use epsilon ordering
	  do_precedenceNoEpsilon(SMD, ToIn, ToOut)
	).


do_precedenceEpsilon(SMD, ToIn, ToOut):-
	split_epsilon(ToIn, ToImmediate, ToAssumedImmediate, ToAssumedNonImmediate, ToNonImmediate),
	!,
	etrace(transitions_split_epsilon, [ToImmediate, ToAssumedImmediate, ToAssumedNonImmediate, ToNonImmediate], ordering),
	(
	  ToImmediate \= []
	->
	  % There are Immediate Terminations!
	  etrace(transtions_trying_immediate, _, ordering),
	  % Order these:
	  do_precedence_core(SMD, ToImmediate, TICrossproduct),
	  % and merge them:
	  do_epsilon_merges(TICrossproduct, TIMerged),
	  % then order the Assumed Immediate Terminations
	  do_precedence_core(SMD, ToAssumedImmediate, TAICrossproduct),
	  % And combine:
	  % Non-assumed terminations combined with all possible assumed terminations combination:
	  % NB TIMerged is set and can be more then one if mutual exclusive terminations are present
	  findall(Combination, (
			       member(AssumedCombination, TAICrossproduct),
			       member(ImmediateCombination, TIMerged),
			       merge_tolist([AssumedCombination, ImmediateCombination], Combination)
			       ),
		  Combinations),
	  % also an option: TIMerged sets without any assumed termination:
	  append(TIMerged, Combinations, ResultSet)
	;
	  etrace(transtions_empty_immediate, _, ordering),
	% There are no non-assumed Immediate Terminations:
	% We've decided that assumed immediate terminations may in fact be
	% non-immediate, since the underrepresented state where they come from
	% might 'incorporate' an implicite non immediate transition to the
	% state which has the immediate termination. Eg. an unknown derivative:
	% this might describe two states: steady and increasing. The implicit
	% termination from steady to increasing cannot be assumed to be
	% immediate as well! We have not modelled the process by which the
	% derivative becomes non zero. %
	  % Therefore: Assumed Immediate Terminations, and then (assumed) Non-Immediate terminations are combined into one set:
	  append(ToAssumedNonImmediate, ToNonImmediate, ToNonImmediateAll),
	  append(ToAssumedImmediate, ToNonImmediateAll, ToCombined),
	  do_precedence_core(SMD, ToCombined, ResultSet)
	),
	% If a derivative termination is present in at least one of the sets of terminations.
	% Then sets without this one must be constrained for this derivative
	constrain_terminated_derivatives(ResultSet, ToIn, Final2),

	set_to_list_status_ordered(Final2, Final3),

	% order to list (needed for transition heuristic: fastpath with only succesful supersets of terminations)
	sort_to_list(Final3, ToOut),
	!.



do_precedenceNoEpsilon(SMD, ToIn, ToOut):-
	do_precedence_core(SMD, ToIn, Crossproduct),
	% If a derivative termination is present in at least one of the sets of terminations.
	% Then sets without this one must be constrained for this derivative
	constrain_terminated_derivatives(Crossproduct, ToIn, Final2),
	set_to_list_status_ordered(Final2, Final3),
	% order to list (needed for transition heuristic: fastpath with only succesful supersets of terminations)
	sort_to_list(Final3, ToOut),
	!.



do_precedence_core(SMD, ToIn, Crossproduct):-
    % add index & parameter structure
    index_tolist(ToIn, 0, To2),

    % seperate different termination types for seperate treatment (Other to could be domain specific)
    split_to_list(To2, ValueTo, EqTo, ExoTo, DerTo, OtherTo),
    append(ExoTo, DerTo, DerExoTo),

    % add constraints for mutually exclusive value terminations
    % only relevant in full branching value terminations
    etrace(transitions_m_e_start, _, ordering),
    constrain_m_exclusive_v(ValueTo, [], Combi1),

    % add constraints for mutually exclusive equality terminations
    % only relevant in full branching eq-terminations or terminate weak (>=) relations
    constrain_m_exclusive_eq(EqTo, Combi1, Combi2),

    % add constraints for mutually exclusive exogenous terminations
    constrain_m_exclusive_der_exo(DerExoTo, Combi2, Combi3),

    flag(order_using_correspondences, CFlag, CFlag),
    (
      algorithm_assumption_flag(CFlag, true, order_using_correspondences)
    ->
      (
        etrace(transitions_corr_ord, _, ordering),
        % correspondence merge (via constraints) / remove value terminations:
        correspondence_merge_remove_values(SMD, ValueTo, ValueTo1, Combi3, Combi4),

        % correspondence remove equality terminations  NB. algorithm option switch
                 % todo the merge part, not working now (+ tracer for that)
        correspondence_merge_remove_equalities(SMD, EqTo, EqTo1, Combi4, Combi5),

        % correspondence merge (via constraints) / remove derivative terminations:
        correspondence_merge_remove_derivative(SMD, DerExoTo, DerExoTo1, Combi5, Combi6),
        etrace(transitions_corr_ord_done, _, ordering)
      )
    ;
      (
        Combi3 = Combi6,
        ValueTo = ValueTo1,
        EqTo = EqTo1,
        DerExoTo = DerExoTo1
      )
    ),

    % determine the involved parameters for terminations
    parameters_involved_to(ValueTo1, ValueTo2),
    parameters_involved_to(EqTo1, EqTo2),

    % equality terminations combined with relevant value terminations give extra constraints when combinations are solved
    etrace(transitions_math_ord, _, ordering),
    equality_termination_constraints(ValueTo2, EqTo2, SMD, Combi6, Combi7, Contexts),

    etrace(transitions_math_ord_done, _, ordering),

    % more combinationconstraints from complex contexts
    flag(order_using_equalities, Flag, Flag),
    (
      algorithm_assumption_flag(Flag, true, order_using_equalities)
    ->
      etrace(transitions_complex_math_ord, _, ordering),
      complex_equality_constraints(ValueTo2, EqTo2, SMD, Combi7, Combi8, Contexts),
      etrace(transitions_complex_math_ord_done, _, ordering)
    ;
      Combi8 = Combi7
    ),

    % append all
    append(ValueTo2, EqTo2, ToVE),
    append(DerExoTo1, OtherTo, ToEO),
    append(ToVE, ToEO, ToAll),

    % make crossproduct
    etrace(transitions_crossproduct, _, ordering),
    combination_constrained_crossproduct(ToAll, Combi8, Crossproduct),
    etrace(transitions_crossproduct_done, [Crossproduct], ordering).



/* OLD VERSION before okt 2011 FL

do_precedence(_SMD, [], []):-
	!,
	etrace(transtions_ordering_empty, _, ordering).

do_precedence(SMD, ToIn, ToOut):-
    % merge doubles
    % (FL feb 07: calls old routine, with trace statements present: todo update tracer,
    % not so important though... doubles should only occur in rulebased terminations, never used nowadays.)
    merge_double_terminations(ToIn, ToA),

    flag(order_epsilon_last, EFlag, EFlag),
    % EFlag = true, % temp NB FL Switch still needed
    % if flag is true the To set is split into immediate / non-immediate
    % if the first set comes out empty the second is tried.
    (
      algorithm_assumption_flag(EFlag, fail, order_epsilon_last)
    ->
      %epsilon first: do that and split, (and calls do_precedence3 to finish)
      do_precedence1(SMD, ToA, ToOut)
    ;
      %algorithm_assumption_flag(EFlag, true, order_epsilon_last), % unnecessary check...
      %epsilon last (old style)
      do_precedence_core(SMD, ToA, ToOut, true, true, [])
    ).

do_precedence1(SMD, ToA, ToOut):-
    %Efirst: apply epsilon first.
    split_epsilon(ToA, ToImmediate, ToAssumedImmediate, ToAssumedNonImmediate, ToNonImmediate, EpsilonFlag),
    do_precedence2(SMD, ToImmediate, ToAssumedImmediate, ToAssumedNonImmediate, ToNonImmediate, ToOut, EpsilonFlag).


do_precedence2(SMD, [], [], [], TNI, Tout, fail):-
	%epsilon is off (all T put in large set)
	!,
	do_precedence_core(SMD, TNI, Tout, fail, fail, []).

do_precedence2(SMD, TI, _TAI, _TANI, _TNI, Tout, true):-
	% immediate present:
	TI \= [],
	etrace(transtions_trying_immediate, _, ordering),
	!, % NEW: trying non immediate after immediates got empty is not good, this should be an end state...
	do_precedence_core(SMD, TI, Tout, fail, true, []),
	% Tout \= [],  % NEW: trying non immediate after immediates got empty is not good, this should be an end state...
	!.


do_precedence2(SMD, TI, TAI, TANI, TNI, Tout, true):-
	% determine if this is a rerun after immediate produced empty set (NEW: this never happens anymore see above)
	(
	  TI \= []
	->
	  %immedate present but produced empty ordered set
	  etrace(transtions_trying_non_immediate, _, ordering)
	;
	  etrace(transtions_empty_immediate, _, ordering)
	),
	% try all three and concat results
	do_precedence3(SMD, TAI, ToutAI, fail, true, [], transitions_ordering_assumed_immediate),
	do_precedence3(SMD, TNI, ToutNI, fail, fail, TAI, transitions_ordering_non_immediate),
	do_precedence3(SMD, TANI, ToutANI, fail, fail, [], transitions_ordering_assumed_non_immediate),
	% NB non immediate terminations should not change derivatives for parameters
	% with an assumed derivative termination present.
	% Therefore TAI is passed on as a constraintset.
	append(ToutAI, ToutANI, Tout1),
	append(ToutNI, Tout1, Tout). %NB assumed terminations in the back: for pruning purposes

do_precedence3(_SMD, [], [], _EL, _DEM, _ConstraintT, _):-
	!.

do_precedence3(SMD, Tin, Tout, EL, DEM, ConstraintT, TraceMessage):-
	etrace(TraceMessage, _, ordering),
	do_precedence_core(SMD, Tin, Tout, EL, DEM, ConstraintT),
	!.



do_precedence_core(SMD, To1, ToOut, EpsilonLast, DoEpsilonMerges, AssumedImmediatePresent):-
    % add index & parameter structure
    index_tolist(To1, 0, To2),

    % seperate different termination types for seperate treatment (Other to could be domain specific)
    split_to_list(To2, ValueTo, EqTo, ExoTo, DerTo, OtherTo),
    append(ExoTo, DerTo, DerExoTo),

    % add constraints for mutually exclusive value terminations
    % only relevant in full branching value terminations
    etrace(transitions_m_e_start, _, ordering),
    constrain_m_exclusive_v(ValueTo, [], Combi1),

    % add constraints for mutually exclusive equality terminations
    % only relevant in full branching eq-terminations or terminate weak (>=) relations
    constrain_m_exclusive_eq(EqTo, Combi1, Combi2),

    % add constraints for mutually exclusive exogenous terminations
    constrain_m_exclusive_der_exo(DerExoTo, Combi2, Combi3),

    flag(order_using_correspondences, CFlag, CFlag),
    (algorithm_assumption_flag(CFlag, true, order_using_correspondences)
    ->
    (
        etrace(transitions_corr_ord, _, ordering),
        % correspondence merge (via constraints) / remove value terminations:
        correspondence_merge_remove_values(SMD, ValueTo, ValueTo1, Combi3, Combi4),

        % correspondence remove equality terminations  NB. algorithm option switch
                 % todo the merge part, not working now (+ tracer for that)
        correspondence_merge_remove_equalities(SMD, EqTo, EqTo1, Combi4, Combi5),

        % correspondence merge (via constraints) / remove derivative terminations:
        correspondence_merge_remove_derivative(SMD, DerExoTo, DerExoTo1, Combi5, Combi6),
        etrace(transitions_corr_ord_done, _, ordering)
        )
    ;
        (
        Combi3 = Combi6,
        ValueTo = ValueTo1,
        EqTo = EqTo1,
        DerExoTo = DerExoTo1
        )
    ),

    % determine the involved parameters for terminations
    parameters_involved_to(ValueTo1, ValueTo2),
    parameters_involved_to(EqTo1, EqTo2),

    % equality terminations combined with relevant value terminations give extra constraints when combinations are solved
    etrace(transitions_math_ord, _, ordering),
    equality_termination_constraints(ValueTo2, EqTo2, SMD, Combi6, Combi7, Contexts),
    etrace(transitions_math_ord_done, _, ordering),

    % more combinationconstraints from complex contexts
    flag(order_using_equalities, Flag, Flag),
    (algorithm_assumption_flag(Flag, true, order_using_equalities)
    ->
    etrace(transitions_complex_math_ord, _, ordering),
    complex_equality_constraints(ValueTo2, EqTo2, SMD, Combi7, Combi8, Contexts),
    etrace(transitions_complex_math_ord_done, _, ordering)
    ;
    Combi8 = Combi7),

    % append all
    append(ValueTo2, EqTo2, ToVE),
    append(DerExoTo1, OtherTo, ToEO),
    append(ToVE, ToEO, ToAll),

    % make crossproduct
    etrace(transitions_crossproduct, _, ordering),
    combination_constrained_crossproduct(ToAll, Combi8, Crossproduct),
    etrace(transitions_crossproduct_done, [Crossproduct], ordering),

    % FL July 2004:
    % If constraints implicitly rule out every small epsilon termination,
    % this is only seen when determining the crossproduct. if epsilon is applied earlier,
    % then possibly large epsilon terminations are removed at that point(unjustified),
    % and we end up without terminations!
    % therefore epsilon is applied after the crossproduct:
    % NB general trace statements under do_epsilon clause because of flag...
    (
      EpsilonLast == true
    ->
      do_epsilon(Crossproduct, Final1)
    ;
      %epsilon was done first
      Crossproduct = Final1
    ),

    % FL September 2004:
    % immediate causes should all happen together: immediate = immediate which
    % leaves no room for one after the other.
    % This is a soft merge however, because mutual exclusive terminations can be present
    (
      DoEpsilonMerges == true
    ->
      do_epsilon_merges(Final1, Final2)
    ;
      DoEpsilonMerges == fail,
      % no epsilon merges needed because this is a set of non-immediate terminations
      Final1 = Final2
    ),

    % NEW FL june 07 Derivative terminations must be checked.
    % If case a derivative termination is present in at least one of the sets of terminations
    % Then sets without this one must be constrained for this derivative
    constrain_terminated_derivatives(DerExoTo, AssumedImmediatePresent, Final2, Final3),

    set_to_list_status_ordered(Final3, Final4),

    % order to list (needed for transition heuristic: fastpath with only succesful supersets of terminations)
    sort_to_list(Final4, ToOut),
    !.
*/


% sort in decreasing order to the number of terminations in a to()
sort_to_list(ToIn, ToOut):-
	lenght_key_to(ToIn, Keyed),
	keysort(Keyed, SortedKeyed),
	remove_key(SortedKeyed, Sorted),
	reverse(Sorted, ToOut).

lenght_key_to([], []).
lenght_key_to([to(cause(L), C, R, T, S)|Tail], [Len-to(cause(L), C, R, T, S)|NT]):-
	length(L, Len),
	lenght_key_to(Tail, NT).

remove_key([], []).
remove_key([_-To|T], [To|NT]):-
	remove_key(T, NT).


/***______________________ constrain terminated derivatives _______________***/

% NEW Description: nov 2011
%
% For every derivative termination (or exogenous about
% derivative) see if it occurs somewhere in the crossproduct If so then
% get a constraint to hold the derivative in its original position
%
% Then for every terminations combination from the crossproduct check
% which derivative terminations are present add the constraints of all
% that are not present: these derivatives should not change
%
% But: mutual exclusive terminations (exogenous), should not cripple
% their brothers by setting each others	derivative at the old value...
% so in these cases the presence of this other termination is also
% checked and this leads to an exception, not placing the constraint


% Find all derivative terminations (or exogenous about derivative)that
% occur somewhere in the crossproduct
%
% For each of these get a constraint to hold the derivative in its original position
%
% Then for every terminations combination from the crossproduct check
% which derivative terminations are present add the constraints of all
% that are not present: these derivatives should not change
%
% But: mutual exclusive terminations (exogenous), should not cripple
% their brothers by setting each others	derivative at the old value...
% so in these cases the presence of this other termination is also
% checked and this leads to an exception, not placing the constraint
%
%
% OLD Description:
% For every derivative termination (or exogenous about
% derivative) see if it occurs somewhere in the crossproduct If so then
% get a constraint to hold the derivative in its original position
%
% Then for every terminations combination from the crossproduct check
% which derivative terminations are present add the constraints of all
% that are not present: these derivatives should not change
%
% But: mutual exclusive terminations (exogenous), should not cripple
% their brothers by setting each others	derivative at the old value...
% so in these cases the presence of this other termination is also
% checked and this leads to an exception, not placing the constraint
%
% In case of non immediate terminations, Assumed immediate derivative
% terminations can be present which were ordered in a previous cycle
% (2 cycles needed because assumed derivatives should not remove
% definite terminations. But they derivatives of these Par's should be
% locked. (AssumedDerTo is the imported constraintset).
%
%

constrain_terminated_derivatives(CrossproductIn, AllTerminations, ConstrainedCrossproductOut):-
	collect_terminated_derivatives(AllTerminations, CrossproductIn, ConstraintList),
	apply_terminated_derivative_constraints(CrossproductIn, ConstraintList, ConstrainedCrossproductOut),
	!.

% done
collect_terminated_derivatives([], _To, []).

% the termination is about derivative
% and it occurs in the crossproduct
collect_terminated_derivatives([to(cause([DerCause]), _, _, _, _)|T], Set, [DerCause/Constraint/Exception|CT]):-
	DerCause =.. [Type, Par],
	original_derivative_constraint(Type, Par, Constraint, Exception),
	member(to(cause(Causes), _, _, _, _), Set),
	memberchk(DerCause, Causes),
	!,
	collect_terminated_derivatives(T, Set, CT).


% no combination with this derivative termination: next
collect_terminated_derivatives([_|T], Set, CT):-
	collect_terminated_derivatives(T, Set, CT).


/*
collect_assumed_der_to_constraints([], []).
collect_assumed_der_to_constraints([to(cause([DerCause]), _, _, _, _)|T], [DerCause/Constraint/Exception|CT]):-
	DerCause =.. [Type, Par],
	original_derivative_constraint(Type, Par, Constraint, Exception),
	collect_assumed_der_to_constraints(T, CT).
*/


% NB this table assumes that no combinations of der / exo / assder
% terminations will occur.

original_derivative_constraint(derivative_up_to_stable, Par, d_greater(Par, zero), nil).
original_derivative_constraint(assumed_derivative_up_to_stable, Par, d_greater(Par, zero), nil).
original_derivative_constraint(exogenous_up_to_stable, Par, d_greater(Par, zero), nil).

original_derivative_constraint(derivative_down_to_stable, Par, d_smaller(Par, zero), nil).
original_derivative_constraint(assumed_derivative_down_to_stable, Par, d_smaller(Par, zero), nil).
original_derivative_constraint(exogenous_down_to_stable, Par, d_smaller(Par, zero), nil).

original_derivative_constraint(derivative_stable_to_down, Par, d_equal(Par, zero), derivative_stable_to_up(Par)).
original_derivative_constraint(assumed_derivative_stable_to_down, Par, d_equal(Par, zero), assumed_derivative_stable_to_up(Par)).
original_derivative_constraint(exogenous_stable_to_down, Par, d_equal(Par, zero), exogenous_stable_to_up(Par)).

original_derivative_constraint(derivative_stable_to_up, Par, d_equal(Par, zero), derivative_stable_to_down(Par)).
original_derivative_constraint(assumed_derivative_stable_to_up, Par, d_equal(Par, zero), assumed_derivative_stable_to_down(Par)).
original_derivative_constraint(exogenous_stable_to_up, Par, d_equal(Par, zero), exogenous_stable_to_down(Par)).



% empty constraints: no changes
apply_terminated_derivative_constraints(To, [], To):-
	!.

% done
apply_terminated_derivative_constraints([], _, []).
% for each set: check and apply the constraintslist
apply_terminated_derivative_constraints([H|T], Constraints, [NH|NT]):-
	apply_td_constraints(Constraints, H, NH),
	apply_terminated_derivative_constraints(T, Constraints, NT).


%no more constraints
apply_td_constraints([], To, To).
% derivative termination present in combination, no extra constraints
apply_td_constraints([Cause/_Constraint/_Exception|CT],
		     to(cause(Causes), Conditions, ResultsIn, To, Status),
		     to(cause(Causes), Conditions, Results, To, Status)):-
	memberchk(Cause, Causes),
	!,
        apply_td_constraints(CT, to(cause(Causes), Conditions, ResultsIn, To, Status), to(cause(Causes), Conditions, Results, To, Status)).

% exception termination present in combination, no extra constraints
apply_td_constraints([_Cause/_Constraint/Exception|CT],
		     to(cause(Causes), Conditions, ResultsIn, To, Status),
		     to(cause(Causes), Conditions, Results, To, Status)):-
	memberchk(Exception, Causes),
	!,
        apply_td_constraints(CT, to(cause(Causes), Conditions, ResultsIn, To, Status), to(cause(Causes), Conditions, Results, To, Status)).

% not present, add, constraint to results
apply_td_constraints([_Cause/Constraint/_Exception|CT],
		     to(cause(Causes), Conditions, results(ResultsIn), To, Status),
		     to(cause(Causes), Conditions, results(Results), To, Status)):-
	insert_constraint(ResultsIn, Constraint, Results1), % add the constraint to the par_relations if present. otherwise add this section
	apply_td_constraints(CT, to(cause(Causes), Conditions, results(Results1), To, Status), to(cause(Causes), Conditions, results(Results), To, Status)).



%no par_relations found (end of list): append to end
insert_constraint([], Constraint, [par_relations([Constraint])]).
% par_relation results: append and done
insert_constraint([par_relations(Rels)|T], Constraint, [par_relations(NewRels)|T]):-
	!, %parrelations present, append
	append(Rels, [Constraint], NewRels).
% other results: look deeper
insert_constraint([H|T], Constraint, [H|NT]):-
	insert_constraint(T, Constraint, NT).


/***______________________ merge double terminations ______________________***/

% NB double terminations should never occur unless domain specific rule interpreter
% supplies these, which could be considered bad model building.
% while possible: select two different terminations with equal causes -> merge
% To's not indexed yet... (easy).

% FL Nov 2011: obsolete?

% double found
merge_double_terminations(ToIn, ToOut):-
    select(to(cause(Causes1), _, _, _, _), ToIn, Rest),
member(to(cause(Causes2), _, _, _, _), Rest), member(Cause, Causes1),
member(Cause, Causes2), !, apply_order_actions(merge_similar(Cause),
[merge([Cause, Cause])], ToIn, NewTo), !,
merge_double_terminations(NewTo, ToOut).

% done.
merge_double_terminations(To, To).


/***__________ AUXILLARY PREDICATES used by multiple procedures ___________***/

% construct combination constraint table structure:
% add to all terminations:
% indexnumber (for easy referencing),
% parameters involved (for easy checking of parameter context).

index_tolist([], _, []).

index_tolist([To|Tail], I, [To/J/[]|NewTail]):-
    J is I + 1,
    index_tolist(Tail, J, NewTail).

% for a list of terminations determine all parameters involved
parameters_involved_to([], []).

parameters_involved_to([To/I/_|Tail], [To/I/PSet|NewTail]):-
    To = to(cause(Causes),_,_,_,_),
    p_involved_in_termination(Causes, P),
    list_to_set(P, PSet),
    parameters_involved_to(Tail, NewTail).

p_involved_in_termination([], []).

p_involved_in_termination([H|T], Par):-
    H =.. [_|P],
    p_involved_in_termination(T, PT),
    append(P, PT, Par).


% for a list of equalities determine all parameters involved
% return in Eq/nil/PSet structure for compatibility with predicates
% for To()/I/P structure
parameters_involved_eq([], []).

parameters_involved_eq([Equality|Tail], [Equality/nil/Parameters|NewTail]):-
    p_involved_in_equality(Equality, Parameters),
    parameters_involved_eq(Tail, NewTail).


p_involved_in_equality(Equality, Parameters):-
    Equality =.. [_|Body],
    p_involved_in_eq(Body, List),
    list_to_set(List, Parameters). % one never nows what a modelbuilder writes down.

% returns involved parameters or landmarks
p_involved_in_eq([], []).

% head is addition or subtraction
p_involved_in_eq([H|T], Parameters):-
    H =.. [X,Left,Right],
    memberchk(X, [plus, min]),
    !, % complex term
    p_involved_in_eq([Left], LPar),
    p_involved_in_eq([Right], RPar),
    p_involved_in_eq(T, TPar),
    append(LPar, RPar, HPar),
    append(HPar, TPar, Parameters).

% head is not complex, must be parameter
p_involved_in_eq([H|T], [H|Parameters]):-
    p_involved_in_eq(T, Parameters).


%make structures: relation/[pars]
parameters_in_relations([], []).

parameters_in_relations([Rel|T], [Rel/Pars|NT]):-
    inequality_type(Rel),
    !,
    pars_in_equality(Rel, Pars),
    parameters_in_relations(T, NT).

% remove other relations
parameters_in_relations([_|T], NT):-
    parameters_in_relations(T, NT).

pars_in_equality(Equality, Parameters):-
    Equality =.. [_|Body],
    pars_in_eq(Body, List),
    list_to_set(List, Parameters). % one never nows what a modelbuilder writes down.

% returns involved parameters, or parameters in landmarks
pars_in_eq([], []).

% head is addition or subtraction
pars_in_eq([H|T], Parameters):-
    H =.. [X,Left,Right],
    memberchk(X, [plus, min]),
    !, % complex term
    pars_in_eq([Left], LPar),
    pars_in_eq([Right], RPar),
    pars_in_eq(T, TPar),
    append(LPar, RPar, HPar),
    append(HPar, TPar, Parameters).

% head is not complex, must be parameter or landmark:
% landmark
pars_in_eq([H|T], [P|Parameters]):-
    H =.. [_,P],
    !, % landmark
    pars_in_eq(T, Parameters).

% parameter
pars_in_eq([H|T], [H|Parameters]):-
    pars_in_eq(T, Parameters).


% relevant_relations(+All, +Pars, -Relevant)
% return relations about and only about all parameters.
% All should be a list of structures: relation/list
% Pars are the relevant parameters, list contains all parameters in relation
relevant_relations([], _, []).

/*
% relevant relation, keep
relevant_relations([H/List|T], Pars, [H|NT]):-
    match_lists(Pars, List),
    !,
    relevant_relations(T, Pars, NT).
*/
% relevant relation, keep
relevant_relations([H/List|T], Pars, [H|NT]):-
    subset(List, Pars),
    !,
    relevant_relations(T, Pars, NT).

% irrelevant relation, skip
relevant_relations([_|T], Pars, NT):-
    relevant_relations(T, Pars, NT).

% add to combinations table: add a set of constraints to the table
% empty list: no action
add_to_combinations_table(Combi, [], Combi).

% constraints in list from one source: add list
add_to_combinations_table(CombiIn, [H|T], [Set|CombiIn]):-
    list_to_set([H|T], Set).

% seperate value-, equality- and other terminations:

split_to_list([], [], [], [], [], []).

% value termination
split_to_list([To|Tail], [To|VT], EQTo, ExoTo, DerTo, OTO):-
    is_value_termination(To),
    !,
    split_to_list(Tail, VT, EQTo, ExoTo, DerTo, OTO).

% equality termination
split_to_list([To|Tail], VT, [To|EQTo], ExoTo, DerTo, OTO):-
    is_equality_termination(To),
    !,
    split_to_list(Tail, VT, EQTo, ExoTo, DerTo, OTO).

% exogenous termination
split_to_list([To|Tail], VT, EQTo, [To|ExoTo], DerTo, OTO):-
    is_exogenous_termination(To),
    !,
    split_to_list(Tail, VT, EQTo, ExoTo, DerTo, OTO).

% derivative termination
split_to_list([To|Tail], VT, EQTo, ExoTo, [To|DerTo], OTO):-
    is_derivative_termination(To),
    !,
    split_to_list(Tail, VT, EQTo, ExoTo, DerTo, OTO).

% other termination
split_to_list([To|Tail], VT, EQTo, ExoTo, DerTo, [To|OTO]):-
    split_to_list(Tail, VT, EQTo, ExoTo, DerTo, OTO).


% To is a value change:
is_value_termination(to(cause(Causes), _, _, _, _)/_/_):-
    member(C, Causes),
    memberchk(C, [to_point_above(_),
                  to_point_below(_),
                  to_interval_above(_),
                  to_interval_below(_),
                  assumed_to_point_above(_),
                  assumed_to_point_below(_),
                  assumed_to_interval_above(_),
                  assumed_to_interval_below(_)
                 ]),
    !.

% To is an equality change:
is_equality_termination(to(cause(Causes), _, _, _, _)/_/_):-
    member(C, Causes),
    memberchk(C, [from_equal_to_greater(_, _),
                  from_equal_to_smaller(_, _),
                  from_greater_to_equal(_, _),
                  from_smaller_to_equal(_, _),
                  from_smaller_or_equal_to_smaller(_, _),
                  from_smaller_or_equal_to_equal(_, _),
                  from_smaller_or_equal_to_greater(_, _),
                  from_greater_or_equal_to_greater(_, _),
                  from_greater_or_equal_to_equal(_, _),
                  from_greater_or_equal_to_smaller(_, _),
                  assumed_from_equal_to_greater(_, _),
                  assumed_from_equal_to_smaller(_, _),
                  assumed_from_greater_to_equal(_, _),
                  assumed_from_smaller_to_equal(_, _),
                  assumed_from_smaller_or_equal_to_smaller(_, _),
                  assumed_from_smaller_or_equal_to_equal(_, _),
                  assumed_from_smaller_or_equal_to_greater(_, _),
                  assumed_from_greater_or_equal_to_greater(_, _),
                  assumed_from_greater_or_equal_to_equal(_, _),
                  assumed_from_greater_or_equal_to_smaller(_, _)
                  ]),
    !.

% To is an exogenous change:
is_exogenous_termination(to(cause(Causes), _, _, _, _)/_/_):-
    member(C, Causes),
    memberchk(C, [exogenous_stable_to_down(Par),
                  exogenous_stable_to_up(Par),
                  exogenous_down_to_stable(Par),
                  exogenous_up_to_stable(Par)
                 ]),
    !.

% To is an derivative change:
is_derivative_termination(to(cause(Causes), _, _, _, _)/_/_):-
    member(C, Causes),
    memberchk(C, [derivative_stable_to_down(Par),
                  derivative_stable_to_up(Par),
                  derivative_down_to_stable(Par),
                  derivative_up_to_stable(Par),
		  assumed_derivative_stable_to_down(Par),
                  assumed_derivative_stable_to_up(Par),
                  assumed_derivative_down_to_stable(Par),
                  assumed_derivative_up_to_stable(Par)
                 ]),
    !.


% after a merge to() has list as index: replace by new index I
% replace old indices in combinations table by I
update_index_after_merge(I, ToIn, ToOut, CombiIn, CombiOut):-
     update_indexes_to(ToIn, I, ToOut, OldIndices),
     subtract(OldIndices, [I], OldI),
     update_indexes_combinations(CombiIn, OldI, I, CombiOut).

% for every merged to() (which has a list as index), replace this
% index by J and return old indices

update_indexes_to([], _J, [], []).

update_indexes_to([To/I/_|Tail], J, [To/J/_|NewTail], Indices):-
    is_list(I),
    !,
    update_indexes_to(Tail, J, NewTail, Ind1),
    append(I, Ind1, Ind2),
    list_to_set(Ind2, Indices).

update_indexes_to([To/I/_|Tail], J, [To/I/_|NewTail], Indices):-
    \+ is_list(I),
    !,
    update_indexes_to(Tail, J, NewTail, Indices).


% for every combination mentioning a removed index (in ISet), replace this
% by new index J

update_indexes_combinations([], _ISet, _J, []).

% an old index is in combinationlist: subtract set (and J if present) & add J
update_indexes_combinations([List/Consistency|Tail], ISet, J, [[J|NewList]/Consistency|NewTail]):-
    member(I, List),
    member(I, ISet),
    !,
    subtract(List, [J|ISet], NewList),
    update_indexes_combinations(Tail, ISet, J, NewTail).

% no old index in combilist: next
update_indexes_combinations([H|Tail], ISet, J, [H|NewTail]):-
   update_indexes_combinations(Tail, ISet, J, NewTail).


% after removing several terminations, their indexes can still be in the
% combinations table: any combination only mentioning removed indices
% provides no information: remove

remove_empty_combinations([], [], _).

% empty: remove
remove_empty_combinations([List/_|Tail], NewTail, RemovedIndexes):-
    forall(member(I, List), memberchk(I, RemovedIndexes)),
    !,
    remove_empty_combinations(Tail, NewTail, RemovedIndexes).

% non empty: keep
remove_empty_combinations([List/C|Tail], [List/C|NewTail], RemovedIndexes):-
    remove_empty_combinations(Tail, NewTail, RemovedIndexes).


% because of indexed structure a seperate procedure is needed for merges and
% removes, the original procedures are in methods.pl the only difference is that
% the indexed version can handle the To/Index/Parameters structure

indexed_apply_order_actions(_, [], To, To).

indexed_apply_order_actions(Cause, [ remove(L) | T ], To, NTo):-
	indexed_remove_from_tolist(L, To, TTo),		% what to do with Cause?
	indexed_apply_order_actions(Cause, T, TTo, NTo).

indexed_apply_order_actions(Cause, [ merge(L) | T ], To, NTo):-
	indexed_merge_tolist_select(Cause, L, To, TTo, New),
	indexed_apply_order_actions(Cause, T, [New|TTo], NTo).

indexed_remove_from_tolist([], To, To).
indexed_remove_from_tolist([H|T], To, NTo):-
	common_select(To, to(cause(Causes), _, _, _, _Status)/_/_, TTo),
	member(H, Causes),
	!,
	indexed_remove_from_tolist(T, TTo, NTo).


indexed_merge_tolist_select(Cause, [], To, To,
	to(cause([Cause]), conditions([]), results([]), to_state([]), _Status)/[]/_ ):- !.

indexed_merge_tolist_select(Cause, [H|T], To, NTo,
		to(	cause(Causes),
			conditions(Conditions),
			results(Results),
			to_state(States), Status)/[I1|I2]/_ )	:-
	indexed_merge_tolist_select(Cause, T, To, TTo,
		to(	cause(Causes2),
			conditions(Conditions2),
			results(Results2),
			to_state(States2), Status)/I2/_),
	common_select(TTo,
		to(	cause(Causes1),
			conditions(Conditions1),
			results(Results1),
			to_state(States1), Status)/I1/_,
		NTo),
	memberchk(H, Causes1),
	smerge(Causes1, Causes2, Causes),
	merge_rule_conditions_givens(Conditions1, Conditions2, Conditions),
	merge_rule_conditions_givens(Results1, Results2, Results),
	smerge(States1, States2, States),
	!.


% get_complex_relations(+Relations, +Parameters, -ComplexRelations)
% find all relations involving addition or subtraction
% (non binairy, involving three or more quantities)

get_complex_relations([], _, []).

get_complex_relations([H|T], Values, [H|NT]):-
    is_complex_relation(H, Values),
    !,
    get_complex_relations(T, Values, NT).

get_complex_relations([_|T], Values, NT):-
    get_complex_relations(T, Values, NT).

is_complex_relation(Relation, Values):-
    Relation =.. [RType, Left, Right],
    memberchk(RType, [greater, smaller, equal, greater_or_equal, smaller_or_equal]),
    only_parameters(Left, Values),
    only_parameters(Right, Values),
    ((
    is_complex(Left)
    ;
    is_complex(Right)
    )),!.

/*
% for now no iff's
is_complex_relation(Relation):-
    Relation =.. [RType|_],
    memberchk(RType, [if, iff]).
*/

is_complex(plus(_,_)).
is_complex(min(_,_)).


% an expression is a parameter or addition/subtraction of parameters.
only_parameters(Par, Values):-
    memberchk(value(Par, _, _, _), Values).

only_parameters(plus(Left, Right), Values):-
    only_parameters(Left, Values),
    only_parameters(Right, Values).

only_parameters(min(Left, Right), Values):-
    only_parameters(Left, Values),
    only_parameters(Right, Values).


/***____________ CONSTRAIN MUTUALLY EXCLUSIVE TERMINATIONS ________________***/

% constrain_m_exclusive_?(+Terminations, +CombiTableIn, -CombiTableOut)
% for every termination check if there are others that cannot happen together,
% add these indexes to combined database
% a table is used as information source


% add constraints for mutually exclusive value terminations
% only relevant in full branching value terminations

constrain_m_exclusive_v([], Combi, Combi).

constrain_m_exclusive_v([to(cause(Causes), _, _, _, _)/I/_|Tail], CombiIn, CombiOut):-
    findall([[I, Index]/inconsistent], m_e_v_termination(Causes, Tail, Index), Indices),
    list_to_set(Indices, Indices1),
    append(Indices1, CombiIn, NewCombi),
    constrain_m_exclusive_v(Tail, NewCombi, CombiOut).

m_e_v_termination(Causes1, Tail, Index):-
     member(C1, Causes1),
     member(to(cause(Causes2), _, _, _, _)/Index/_, Tail),
     member(C2, Causes2),
     m_e_v_t(C1, C2),
     etrace(transitions_m_e, [Causes1, Causes2], ordering).

m_e_v_t(assumed_to_point_above(Par), assumed_to_point_below(Par)).
m_e_v_t(assumed_to_interval_above(Par), assumed_to_interval_below(Par)).
m_e_v_t(assumed_to_point_below(Par), assumed_to_point_above(Par)).
m_e_v_t(assumed_to_interval_below(Par), assumed_to_interval_above(Par)).


% add constraints for mutually exclusive equality terminations
% only relevant in full branching eq-terminations or terminate weak (>=) relations

constrain_m_exclusive_eq([], Combi, Combi).

constrain_m_exclusive_eq([to(cause(Causes), _, _, _, _)/I/_|Tail], CombiIn, CombiOut):-
    findall([[I, Index]/inconsistent], m_e_eq_termination(Causes, Tail, Index), Indices),
    list_to_set(Indices, Indices1),
    append(Indices1, CombiIn, NewCombi),
    constrain_m_exclusive_eq(Tail, NewCombi, CombiOut).

m_e_eq_termination(Causes1, Tail, Index):-
     member(C1, Causes1),
     member(to(cause(Causes2), _, _, _, _)/Index/_, Tail),
     member(C2, Causes2),
     m_e_eq_t(C1, C2),
     etrace(transitions_m_e, [Causes1, Causes2], ordering).


% = --> > X <
m_e_eq_t(assumed_from_equal_to_greater(P1, P2), assumed_from_equal_to_smaller(P1, P2)).
m_e_eq_t(assumed_from_equal_to_greater(P1, P2), assumed_from_equal_to_greater(P2, P1)). % not necessary???

m_e_eq_t(assumed_from_equal_to_smaller(P1, P2), assumed_from_equal_to_greater(P1, P2)).
m_e_eq_t(assumed_from_equal_to_smaller(P1, P2), assumed_from_equal_to_smaller(P2, P1)). % not necessary???

% =< --> > X = X <
m_e_eq_t(assumed_from_smaller_or_equal_to_smaller(P1, P2), assumed_from_smaller_or_equal_to_equal(P1, P2)).
m_e_eq_t(assumed_from_smaller_or_equal_to_smaller(P1, P2), assumed_from_smaller_or_equal_to_greater(P1, P2)).
m_e_eq_t(assumed_from_smaller_or_equal_to_smaller(P1, P2), assumed_from_greater_or_equal_to_equal(P2, P1)). % not necessary???
m_e_eq_t(assumed_from_smaller_or_equal_to_smaller(P1, P2), assumed_from_greater_or_equal_to_smaller(P2, P1)). % not necessary???

m_e_eq_t(assumed_from_smaller_or_equal_to_equal(P1, P2), assumed_from_smaller_or_equal_to_smaller(P1, P2)).
m_e_eq_t(assumed_from_smaller_or_equal_to_equal(P1, P2), assumed_from_smaller_or_equal_to_greater(P1, P2)).
m_e_eq_t(assumed_from_smaller_or_equal_to_equal(P1, P2), assumed_from_greater_or_equal_to_smaller(P2, P1)). % not necessary???
m_e_eq_t(assumed_from_smaller_or_equal_to_equal(P1, P2), assumed_from_greater_or_equal_to_greater(P2, P1)). % not necessary???

m_e_eq_t(assumed_from_smaller_or_equal_to_greater(P1, P2), assumed_from_smaller_or_equal_to_equal(P1, P2)).
m_e_eq_t(assumed_from_smaller_or_equal_to_greater(P1, P2), assumed_from_smaller_or_equal_to_smaller(P1, P2)).
m_e_eq_t(assumed_from_smaller_or_equal_to_greater(P1, P2), assumed_from_greater_or_equal_to_equal(P2, P1)). % not necessary???
m_e_eq_t(assumed_from_smaller_or_equal_to_greater(P1, P2), assumed_from_greater_or_equal_to_greater(P2, P1)). % not necessary???

% >= --> > X = X <
m_e_eq_t(assumed_from_greater_or_equal_to_smaller(P1, P2), assumed_from_greater_or_equal_to_equal(P1, P2)).
m_e_eq_t(assumed_from_greater_or_equal_to_smaller(P1, P2), assumed_from_greater_or_equal_to_greater(P1, P2)).
m_e_eq_t(assumed_from_greater_or_equal_to_smaller(P1, P2), assumed_from_smaller_or_equal_to_equal(P2, P1)). % not necessary???
m_e_eq_t(assumed_from_greater_or_equal_to_smaller(P1, P2), assumed_from_smaller_or_equal_to_smaller(P2, P1)). % not necessary???

m_e_eq_t(assumed_from_greater_or_equal_to_equal(P1, P2), assumed_from_greater_or_equal_to_smaller(P1, P2)).
m_e_eq_t(assumed_from_greater_or_equal_to_equal(P1, P2), assumed_from_greater_or_equal_to_greater(P1, P2)).
m_e_eq_t(assumed_from_greater_or_equal_to_equal(P1, P2), assumed_from_smaller_or_equal_to_smaller(P2, P1)). % not necessary???
m_e_eq_t(assumed_from_greater_or_equal_to_equal(P1, P2), assumed_from_smaller_or_equal_to_greater(P2, P1)). % not necessary???

m_e_eq_t(assumed_from_greater_or_equal_to_greater(P1, P2), assumed_from_greater_or_equal_to_equal(P1, P2)).
m_e_eq_t(assumed_from_greater_or_equal_to_greater(P1, P2), assumed_from_greater_or_equal_to_smaller(P1, P2)).
m_e_eq_t(assumed_from_greater_or_equal_to_greater(P1, P2), assumed_from_smaller_or_equal_to_equal(P2, P1)). % not necessary???
m_e_eq_t(assumed_from_greater_or_equal_to_greater(P1, P2), assumed_from_smaller_or_equal_to_greater(P2, P1)). % not necessary???

% add constraints for mutually exclusive exo AND der terminations
constrain_m_exclusive_der_exo([], Combi, Combi).

constrain_m_exclusive_der_exo([to(cause(Causes), _, _, _, _)/I/_|Tail], CombiIn, CombiOut):-
    findall([[I, Index]/inconsistent], m_e_der_exo_termination(Causes, Tail, Index), Indices),
    list_to_set(Indices, Indices1),
    append(Indices1, CombiIn, NewCombi),
    constrain_m_exclusive_der_exo(Tail, NewCombi, CombiOut).

m_e_der_exo_termination(Causes1, Tail, Index):-
     member(C1, Causes1),
     member(to(cause(Causes2), _, _, _, _)/Index/_, Tail),
     member(C2, Causes2),
     m_e_der_exo_t(C1, C2),
     etrace(transitions_m_e, [Causes1, Causes2], ordering).

m_e_der_exo_t(Cause1, Cause2):-
	exo_der_direction(Cause1, Par, One),
	exo_der_direction(Cause2, Par, Other),
	incompatible_directions(One, Other).

exo_der_direction(exogenous_stable_to_down(Par), Par, down).
exo_der_direction(derivative_stable_to_down(Par), Par, down).
exo_der_direction(assumed_derivative_stable_to_down(Par), Par, down).
exo_der_direction(exogenous_stable_to_up(Par), Par, up).
exo_der_direction(derivative_stable_to_up(Par), Par, up).
exo_der_direction(assumed_derivative_stable_to_up(Par), Par, up).

incompatible_directions(up, down).
incompatible_directions(down, up).


/*
% add constraints for mutually exclusive exogenous terminations
% NB incoming to can be other than only exogenous
% NOT USED NOW, COMBINED PROCEDURE FOR EXO AND DER IS USED.
constrain_m_exclusive_exogenous([], Combi, Combi).

constrain_m_exclusive_exogenous([to(cause(Causes), _, _, _, _)/I/_|Tail], CombiIn, CombiOut):-
    findall([[I, Index]/inconsistent], m_e_exo_termination(Causes, Tail, Index), Indices),
    list_to_set(Indices, Indices1),
    append(Indices1, CombiIn, NewCombi),
    constrain_m_exclusive_exogenous(Tail, NewCombi, CombiOut).

m_e_exo_termination(Causes1, Tail, Index):-
     member(C1, Causes1),
     member(to(cause(Causes2), _, _, _, _)/Index/_, Tail),
     member(C2, Causes2),
     m_e_exo_t(C1, C2),
     etrace(transitions_m_e, [Causes1, Causes2], ordering).

m_e_exo_t(exogenous_stable_to_down(Par), exogenous_stable_to_up(Par)).
m_e_exo_t(exogenous_stable_to_up(Par), exogenous_stable_to_down(Par)).



% add constraints for mutually exclusive derivative terminations
% NB incoming to can be other than only derivative.. (is that true??)
% NOT USED NOW, COMBINED PROCEDURE FOR EXO AND DER IS USED.
constrain_m_exclusive_der([], Combi, Combi).

constrain_m_exclusive_der([to(cause(Causes), _, _, _, _)/I/_|Tail], CombiIn, CombiOut):-
    findall([[I, Index]/inconsistent], m_e_der_termination(Causes, Tail, Index), Indices),
    list_to_set(Indices, Indices1),
    append(Indices1, CombiIn, NewCombi),
    constrain_m_exclusive_der(Tail, NewCombi, CombiOut).

m_e_der_termination(Causes1, Tail, Index):-
     member(C1, Causes1),
     member(to(cause(Causes2), _, _, _, _)/Index/_, Tail),
     member(C2, Causes2),
     m_e_der_t(C1, C2),
     etrace(transitions_m_e, [Causes1, Causes2], ordering).

m_e_der_t(derivative_stable_to_down(Par), derivative_stable_to_up(Par)).
m_e_der_t(derivative_stable_to_up(Par), derivative_stable_to_down(Par)).
*/

/***_____________ CORRESPONDENCE MERGE REMOVE VALUES ______________________***/

% every correspondence type is interpreted as a set of directed corresponding
% value pairs. Each pair is checked seperately. In 5 cases combination
% constraints can be derived. See thesis (floris linnebank, common sense
% reasoning) on www for details

correspondence_merge_remove_values(SMD, ValueToIn, ValueToOut, CombiIn, CombiOut):-
    smd_slots(SMD, _, _, _, Values, Relations, _, _),
    collect_correspondences(Relations, Correspondences),
    % (ValueToIn \= [] -> etrace(transitions_corr_ord, _, ordering); true),  % tempflo, shouldnt the whole operation be dependant on VTi being non empty?! NB start and stop corr-check tracestatement in do_precedence
    % findall directed pairs of corresponding values
    findall(pair(Par1, Val1, Par2, Val2),
            (member(Cor, Correspondences),
             corresponding_value_pair(Cor, Par1, Val1, Par2, Val2)),
            Pairs1),
    list_to_set(Pairs1, Pairs),
    c_pair_merge_remove(Pairs, Values, ValueToIn, ValueToOut, CombiIn, CombiOut).


% collect_correspondences(+RelationsIn, -Correspondences)
% get all correspondences from a set of relations for use in merging

collect_correspondences([], []).

collect_correspondences([Relation|RestIn], [Relation|RestOut]):-
    % correspondence_type is too broad, we want only value correspondences
    memberchk(Relation, [v_correspondence(_, _, _, _),
                         q_correspondence(_, _),
                         dir_v_correspondence(_, _, _, _),
                         dir_q_correspondence(_, _),
                         full_correspondence(_, _),
                         dir_full_correspondence(_, _),
                         mirror_q_correspondence(_, _),
                         dir_mirror_q_correspondence(_, _)
                         ]), % what about if & iff's...
    !,
    collect_correspondences(RestIn, RestOut).

% another relation: discard
collect_correspondences([_|RestIn], RestOut):-
    collect_correspondences(RestIn, RestOut).


% For every corresponding value pair check if any constraints on
% involved terminations are needed or terminations need to be removed.
c_pair_merge_remove([], _, To, To, Combi, Combi).

% stop immediately if valueTo is empty...
c_pair_merge_remove([_|_], _, [], [], Combi, Combi).


% pair is inactive, next
% no value terminations towards conditional value
% neither is conditional value present
c_pair_merge_remove([pair(Par1, Val1, _, _)|Pairs], Values, ToIn, ToOut, CombiIn, CombiOut):-
    \+ member(value(Par1, _, Val1, _), Values),
    \+ termination_applicable_to_value(results, Par1, Val1, ToIn, _),
    !,
    c_pair_merge_remove(Pairs, Values, ToIn, ToOut, CombiIn, CombiOut).

% Category 1:
% pair is active,
% but no terminations away from conditional value
% (NB. coming terminations cannot be present Par1 has Val1)
% remove any terminations away from the consequent value
% (NB. coming terminations cannot be present Par2 must have Val2)
c_pair_merge_remove([pair(Par1, Val1, Par2, Val2)|Pairs], Values, ToIn, ToOut, CombiIn, CombiOut):-
    member(value(Par1, _, Val1, _), Values),
    \+ termination_applicable_to_value(conditions, Par1, Val1, ToIn, _),
    !,
    remove_terminations(Par2, Val2, conditions, ToIn, NewTo),
    c_pair_merge_remove(Pairs, Values, NewTo, ToOut, CombiIn, CombiOut).

% Category 2:
% pair is active,
% termination(s) away from conditional value
% (NB. coming terminations cannot be present Par1 has Val1)
% constrain any terminations away from the consequent value
% they must only happen together with termination(s) away from Val1
% (NB. coming terminations cannot be present Par2 must have Val2)
c_pair_merge_remove([pair(Par1, Val1, Par2, Val2)|Pairs], Values, ToIn, ToOut, CombiIn, CombiOut):-
    member(value(Par1, _, Val1, _), Values),
    findall(C1, termination_applicable_to_value(conditions, Par1, Val1, ToIn, C1), Causes),
    Causes = [_|_], % at least one
    must_happen_together_with(Causes, Par2, Val2, conditions, ToIn, CombiIn, NewCombi),
    !,
    c_pair_merge_remove(Pairs, Values, ToIn, ToOut, NewCombi, CombiOut).

% Category 3:
% pair becomes active,
% 1 termination T1 coming to conditional value (can only be one...)
% Par2 is already at consequential Val2
% constrain any terminations away from the consequent value
% they must only happen without termination T1 towards Val1
c_pair_merge_remove([pair(Par1, Val1, Par2, Val2)|Pairs], Values, ToIn, ToOut, CombiIn, CombiOut):-
    member(value(Par2, _, Val2, _), Values),
    termination_applicable_to_value(results, Par1, Val1, ToIn, Cause/_/Index),
    !,
    must_occur_apart(Cause/_/Index, Par2, Val2, conditions, ToIn, CombiIn, NewCombi),
    c_pair_merge_remove(Pairs, Values, ToIn, ToOut, NewCombi, CombiOut).

% Category 4:
% pair becomes active,
% 1 termination T1 coming to conditional value (can only be one...)
% Par2 is not at consequential Val2
% No termination for Par2 to Val2
% remove termination T1 towards Val1
c_pair_merge_remove([pair(Par1, Val1, Par2, Val2)|Pairs], Values, ToIn, ToOut, CombiIn, CombiOut):-
    \+ member(value(Par2, _, Val2, _), Values),
    termination_applicable_to_value(results, Par1, Val1, ToIn, Cause/_/_),
    \+ termination_applicable_to_value(results, Par2, Val2, ToIn, _),
    !,
    etrace(transitions_corr_rem_fut, [Cause], ordering),
    indexed_apply_order_actions(remove_correspondence, [remove([Cause])], ToIn, NewTo),
    c_pair_merge_remove(Pairs, Values, NewTo, ToOut, CombiIn, CombiOut).

% Category 5:
% pair becomes active,
% 1 termination T1 coming to conditional value (can only be one...)
% Par2 is not at consequential Val2
% 1 termination for Par2 to Val2 (can only be one...)
% terminations need each other
c_pair_merge_remove([pair(Par1, Val1, Par2, Val2)|Pairs], Values, ToIn, ToOut, CombiIn, CombiOut):-
    termination_applicable_to_value(results, Par1, Val1, ToIn, C1/_/Index1),
    termination_applicable_to_value(results, Par2, Val2, ToIn, C2/_/Index2),
    !,
    (Index1 = Index2 -> NewCombi = CombiIn ; % indices equal: already merged
    etrace(transitions_corr_inc_alone_fut, [[C1], [C2]], ordering),
    add_to_combinations_table(CombiIn, [[Index1]/inconsistent,
                                        [Index1, Index2]/consistent], NewCombi)),
    c_pair_merge_remove(Pairs, Values, ToIn, ToOut, NewCombi, CombiOut).


% remove_terminations(Par, Val, conditions/results, ToIn, ToOut)
% remove any terminations present with Par/Val in conditions or results
% Category 1
remove_terminations(Par, Val, ComingGoing, ToIn, ToOut):-
    findall(C1, termination_applicable_to_value(ComingGoing, Par, Val, ToIn, C1), Causes),
    remove_terminations(Causes, ToIn, ToOut).

remove_terminations([], To, To).

remove_terminations([Cause/_/_|CauseTail], ToIn, ToOut):-
    etrace(transitions_corr_rem_act, [Cause], ordering),
    indexed_apply_order_actions(remove_correspondence, [remove([Cause])], ToIn, NewTo),
    remove_terminations(CauseTail, NewTo, ToOut).



% must_happen_together_with(Causes, Par, Val, conditions/results, ToIn, CombiIn, CombiOut)
% any terminations present with Par/Val in conditions or results must happen together
% with termination(s) in causes, these are free themselves
% Category 2

% two terminations to combine with
must_happen_together_with([C1/_/I1|C2/_/I2], Par, Val, ComingGoing, ToIn, CombiIn, CombiOut):-
    findall(C, termination_applicable_to_value(ComingGoing, Par, Val, ToIn, C), Causes),
    must_happen_together_with(C1/_/I1, Causes, CombiIn, NewCombi),
    must_happen_together_with(C2/_/I2, Causes, NewCombi, CombiOut).

% one termination to combine with
must_happen_together_with([Cause/_/Index], Par, Val, ComingGoing, ToIn, CombiIn, CombiOut):-
    findall(C1, termination_applicable_to_value(ComingGoing, Par, Val, ToIn, C1), Causes),
    must_happen_together_with(Cause/_/Index, Causes, CombiIn, CombiOut).


must_happen_together_with(_, [], Combi, Combi).

%identical index: already merged
must_happen_together_with(Cause/_/Index, [_/_/Index|CauseTail], CombiIn, CombiOut):-
    !,
    must_happen_together_with(Cause/_/Index, CauseTail, CombiIn, CombiOut).

% I must happen with Index, not alone
must_happen_together_with(Cause/_/Index, [C/_/I|CauseTail], CombiIn, CombiOut):-
    etrace(transitions_corr_inc_alone_act, [[C], [Cause]], ordering),
    add_to_combinations_table(CombiIn, [[I]/inconsistent, [I, Index]/consistent], NewCombi),
    must_happen_together_with(Cause/_/Index, CauseTail, NewCombi, CombiOut).




% must_happen_apart(Index, Par, Val, conditions/results, ToIn, CombiIn, CombiOut)
% any terminations present with Par/Val in conditions or results must not happen together
% with Index termination
% Category 3
must_occur_apart(Cause/_/Index, Par, Val, ComingGoing, ToIn, CombiIn, CombiOut):-
    findall(C1, termination_applicable_to_value(ComingGoing, Par, Val, ToIn, C1), Causes),
    must_occur_apart(Cause/_/Index, Causes, CombiIn, CombiOut).


must_occur_apart(_, [], Combi, Combi).

% I must occur apart from Index, together is fatal
must_occur_apart(Cause/_/Index, [C/_/I|CauseTail], CombiIn, CombiOut):-
    etrace(transitions_corr_inc_together, [[C], [Cause]], ordering),
    add_to_combinations_table(CombiIn, [[I, Index]/inconsistent], NewCombi),
    must_occur_apart(Cause/_/Index, CauseTail, NewCombi, CombiOut).



% termination_applicable_to_value(+cond/res, +Par, +Val, +ToIn, -Cause/Causes)
% find terminations that act upon a certain Value.

% upon backtracking return all terminations that have Par and Val as conditions and change those

termination_applicable_to_value(conditions, Par, Val, ToIn, Cause/Causes/I):-
    member(to(cause(Causes), conditions(Cond), results(Res), _ToState, _Status)/I/_P, ToIn),
    member(par_values(CValues), Cond),
    member(value(Par, _, Val, _), CValues),
    member(par_values(RValues), Res),
    (memberchk(value(Par, _, V, _), RValues)-> % must be changing (or disapearing??)!
    V \= Val ; true
    ),
    member(Cause, Causes),
    memberchk(Cause, [to_point_above(Par),
		      to_point_below(Par),
		      to_interval_above(Par),
		      to_interval_below(Par),
		      assumed_to_point_above(Par),
		      assumed_to_point_below(Par),
		      assumed_to_interval_above(Par),
		      assumed_to_interval_below(Par)
		     ]).


% upon backtracking return all terminations that have Par and Val as result

termination_applicable_to_value(results, Par, Val, ToIn, Cause/Causes/I):-
    member(to(cause(Causes), _Cond, results(Res), _ToState, _Status)/I/_P, ToIn),
    member(par_values(Values), Res),
    member(value(Par, _, Val, _), Values),
    member(Cause, Causes),
    memberchk(Cause, [to_point_above(Par),
		      to_point_below(Par),
		      to_interval_above(Par),
		      to_interval_below(Par),
		      assumed_to_point_above(Par),
		      assumed_to_point_below(Par),
		      assumed_to_interval_above(Par),
		      assumed_to_interval_below(Par)
		     ]).


% corresponding_value_pair(+correspondence, +par1, +val1, -par2, -val2)
% upon backtracking generates all corresponding pairs,
% with implicative direction: 1 -> 2. undirected correspondence yields 2 pairs

corresponding_value_pair(C, Par1, Val1, Par2, Val2):-
    member(C, [v_correspondence(Par1, Val1, Par2, Val2),
                  v_correspondence(Par2, Val2, Par1, Val1),
                  dir_v_correspondence(Par2, Val2, Par1, Val1)
                  ]).

corresponding_value_pair(C, Par1, Val1, Par2, Val2):-
    member(C, [   q_correspondence(Par1, Par2),
                  q_correspondence(Par2, Par1),
                  dir_q_correspondence(Par2, Par1),
                  full_correspondence(Par1, Par2),
                  full_correspondence(Par2, Par1),
                  dir_full_correspondence(Par2, Par1)
                  ]),
    normal_corresponding_qspaces(Par1, Par2, List),
    member(Val1/Val2, List).

corresponding_value_pair(C, Par1, Val1, Par2, Val2):-
    member(C, [mirror_q_correspondence(Par1, Par2),
                  mirror_q_correspondence(Par2, Par1),
                  dir_mirror_q_correspondence(Par2, Par1)
                  ]),
    mirrorable_qspaces(Par1, Par2, List),
    member(Val1/Val2, List).


% normal_corresponding_qspaces(+Par1, +Par2, -Pairs)
% Pairs is a list of interval or point  pairs:
% e.g.: 2x mzp qspace would render: [min/min, zero/zero, plus/plus]
normal_corresponding_qspaces(P1, P2, Pairs):-
	qspace(P1, _, S1, _),
	qspace(P2, _, S2, _),
    add_corresponding_pair(S1, S2, Pairs).

% take 2 qspaces, return all corresponding values.
add_corresponding_pair([], [], []).

%match 2 points
add_corresponding_pair([point(H1)|T1], [point(H2)|T2], [H1/H2|PTail]):-
    !,
    add_corresponding_pair(T1, T2, PTail).

%match 2 intervals
add_corresponding_pair([H1|T1], [H2|T2], [H1/H2|PTail]):-
    add_corresponding_pair(T1, T2, PTail).




/***_____________ CORRESPONDENCE MERGE REMOVE EQUALITIES __________________***/

/* IDEA
for two full corresponding quantities it is not so logical to become unequal in
an interval, because they will need to be equal to move together to a point again,
therefore these terminations are removed. Under algorithm assumption flag control.

merging equal equality terminations when linked by correspondences was needed to
filter out double transitions to the same state, but this problem is caught with
the complex constraints in the model examined. Furthermore the equality termination
procedure cannot handle merged  inequalities yet, so this (still
imperfect) procedure for merging is now switched off...

*/

correspondence_merge_remove_equalities(SMD, EqToIn, EqToOut, CombiIn, CombiOut):-
    smd_slots(SMD, _, _, _, _, Relations, _, _),
    collect_full_corresponding_pairs(Relations, PList),
    list_to_set(PList, Pairs),
    flag(remove_corresponding_equality, RFlag, RFlag),
    (
      algorithm_assumption_flag(RFlag, true, remove_corresponding_equality)
    ->
      (correspondence_remove_equalities(EqToIn, Pairs, NewEQTo, RemovedIndexes),
      remove_empty_combinations(CombiIn, NewCombi, RemovedIndexes))
    ;
      (EqToIn = NewEQTo, CombiIn = NewCombi)
    ),
    !,
    % permanently switched off. problem with equality point. cant handle larger contexts from merged eq terminations
    %flag(merge_corresponding_equality, _, fail),
    %flag(merge_corresponding_equality, MFlag, MFlag),
    (
      fail %algorithm_assumption_flag(MFlag, true, merge_corresponding_equality)
    ->
      correspondence_merge_equalities(NewEQTo, Pairs, EqToOut, NewCombi, CombiOut)
    ;
      (EqToOut = NewEQTo, CombiOut = NewCombi)
    ),
    !.



% for every Equality termination to unequal, check if Parameters are in full corresponding pairs list,
% if so: remove
correspondence_remove_equalities([], _Pairs, [], []).

correspondence_remove_equalities([Termination|EqToIn], Pairs, EqToOut, [I|Removed]):-
    Termination = to(cause(Causes), _,_,_,_)/I/_,
    member(Cause, Causes),
    memberchk(Cause,  [from_equal_to_greater(Par1, Par2),
                      from_equal_to_smaller(Par1, Par2),
                      from_smaller_or_equal_to_smaller(Par1, Par2),
                      from_smaller_or_equal_to_greater(Par1, Par2), % <= to > ? must have been =
                      from_greater_or_equal_to_greater(Par1, Par2),
                      from_greater_or_equal_to_smaller(Par1, Par2),  % <= to > ? must have been =
                      assumed_from_equal_to_greater(Par1, Par2),
                      assumed_from_equal_to_smaller(Par1, Par2),
                      assumed_from_smaller_or_equal_to_smaller(Par1, Par2),
                      assumed_from_smaller_or_equal_to_greater(Par1, Par2), % <= to > ? must have been =
                      assumed_from_greater_or_equal_to_greater(Par1, Par2),
                      assumed_from_greater_or_equal_to_smaller(Par1, Par2)  % <= to > ? must have been =
                      ]),
    sort([Par1, Par2], Pair),
    memberchk(pair(Pair), Pairs),
    !,
    etrace(transitions_eq_corr_rem, Causes, ordering),
    correspondence_remove_equalities(EqToIn, Pairs, EqToOut, Removed).

correspondence_remove_equalities([To|EqToIn], Pairs, [To|EqToOut], Removed):-
   correspondence_remove_equalities(EqToIn, Pairs, EqToOut, Removed).



% find every two equal eq-terminations that should be merged,
% then process every one of these merges,
% NB all full merges, because every termination is unique (only merge similar done).
correspondence_merge_equalities(EQToIn, Pairs, EqToOut, CombiIn, CombiOut):-
    findall(Merge, corresponding_equal_equality_termination(EQToIn, Pairs, Merge), Merges1),
    list_to_set(Merges1, Merges),
    c_merge_equalities(Merges, EQToIn, EqToOut, CombiIn, CombiOut).


% two eq-terminations are equal in nature and have full
% correspondences linking them together,
corresponding_equal_equality_termination(EQTo, Pairs,
                                        merge(Causes)/
                                        Parameters/
                                        Indices):-
    select(to(cause(Causes1), _,_,_,_)/I1/_, EQTo, Rest),
    member(to(cause(Causes2), _,_,_,_)/I2/_, Rest),
    member(Cause1, Causes1),
    member(Cause2, Causes2),
    memberchk(Cause1, [from_equal_to_greater(_, _),
                  from_equal_to_smaller(_, _),
                  from_greater_to_equal(_, _),
                  from_smaller_to_equal(_, _),
                  from_smaller_or_equal_to_smaller(_, _),
                  from_smaller_or_equal_to_equal(_, _),
                  from_smaller_or_equal_to_greater(_, _),
                  from_greater_or_equal_to_greater(_, _),
                  from_greater_or_equal_to_equal(_, _),
                  from_greater_or_equal_to_smaller(_, _)
                  ]),
    memberchk(Cause2, [from_equal_to_greater(_, _),
                  from_equal_to_smaller(_, _),
                  from_greater_to_equal(_, _),
                  from_smaller_to_equal(_, _),
                  from_smaller_or_equal_to_smaller(_, _),
                  from_smaller_or_equal_to_equal(_, _),
                  from_smaller_or_equal_to_greater(_, _),
                  from_greater_or_equal_to_greater(_, _),
                  from_greater_or_equal_to_equal(_, _),
                  from_greater_or_equal_to_smaller(_, _)
                  ]),
    Cause1 =.. [Type, Par1, Par2],  % No tosmallerAB = togreaterBA yet (alternative termination)
    Cause2 =.. [Type, Par3, Par4],
    sort([Par1, Par3], Pair1),
    memberchk(pair(Pair1), Pairs),
    sort([Par2, Par4], Pair2),
    memberchk(pair(Pair2), Pairs),
    sort([Cause1, Cause2], Causes),
    sort([Par1, Par2, Par3, Par4], Parameters),
    sort([I1, I2], Indices).


% process merges: making a full merge for every pair,
% Can there be partial merges needed in the combination table? (full branching?)
c_merge_equalities([], EqTo, EqTo, Combi, Combi).

c_merge_equalities([merge(Causes)/P/[I1, _]|Merges], EqToIn, EqToOut, CombiIn, CombiOut):-
    % merge & use first index as index for merged set.
    Reason =.. [merge_correspondence | P],
    indexed_apply_order_actions(Reason, [merge(Causes)], EqToIn, NewEqTo1),
    update_index_after_merge(I1, NewEqTo1, NewEqTo, CombiIn, NewCombi),
    c_merge_equalities(Merges, NewEqTo, EqToOut, NewCombi, CombiOut).


% collect_full_corresponding_pairs(+RelationsIn, -Correspondences)
% get all corresponding parameter pairs from a set of relations

collect_full_corresponding_pairs([], []).

collect_full_corresponding_pairs([Relation|RestIn], [pair(Pair)|RestOut]):-
    % correspondence_type is too broad, we want only value correspondences
    memberchk(Relation, [
                         full_correspondence(Par1, Par2),
                         dir_full_correspondence(Par1, Par2)
                         ]),
    !,
    sort([Par1, Par2], Pair),
    collect_full_corresponding_pairs(RestIn, RestOut).

% another relation: discard
collect_full_corresponding_pairs([_|RestIn], RestOut):-
    collect_full_corresponding_pairs(RestIn, RestOut).

/***_____________ CORRESPONDENCE MERGE REMOVE Derivative __________________***/
/*
Exogenous terminations work upon derivatives, correspondences can be
about derivatives too, therefore exogenous terminations should respect
correspondence information

FL NEW june 07: derivative terminations obviously work on derivatives
too so they are combined with the exogenous terminations and checked
here.

since other terminations have free derivatives only correpondences between
exogenous parameters are relevant.

used same algorithm as for value correspondences.
bloody big piece of code for something that rarely occurs.
*/



% New version uses exogenous AND pars that are in derivative termiations
% first determine exogenous corresponding derivative pairs
% then determine merges & removes for every pair
correspondence_merge_remove_derivative(SMD, ToIn, ToOut, CombiIn, CombiOut):-
    smd_slots(SMD, _, _, _, Values, Relations, _, _),
    collect_applicable_d_correspondences(Relations, ToIn, ParDerPairs),
    list_to_set(ParDerPairs, Pairs),
    %tag_from_to_terminations(ToIn, ToTagged),
    derivative_merge_remove(Pairs, Values, ToIn, ToOut, CombiIn, CombiOut).
    %remove_from_to_tags(ToTaggedOut, ToOut).


%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
collect_applicable_d_correspondences(Relations, ToIn, ParDerPairs):-
	extract_applicable_parameters(ToIn, ApplicableParameters),
	collect_exogenous_d_correspondences(Relations, ApplicableParameters, ParDerPairs).

extract_applicable_parameters([], []).
extract_applicable_parameters([to(cause(Causes), _, _, _, _)/_/_|T], Parameters):-
	findall(Par, par_in_causes(Par, Causes), Pars),
	extract_applicable_parameters(T, PT),
	append(Pars, PT, Parameters).

par_in_causes(Par, Causes):-
	member(Cause, Causes),
	Cause =.. [_, Par].

% for every relation:
% determine corresponding pairs(Par1, Der1, Par2, Der2)
% where relation is a d_correspondence (or full),
% and it is only about exogenous parameters.

% no relevant parameters: stop
collect_exogenous_d_correspondences(_, [], []):-!.

collect_exogenous_d_correspondences([], _, []).

collect_exogenous_d_correspondences([Rel|T], EX, Pairs):-
    findall(pair(Par1, Der1, Par2, Der2),
            exogenous_corresponding_der_pair(Rel, EX, Par1, Der1, Par2, Der2),
            Pairs1),
    collect_exogenous_d_correspondences(T, EX, Pairs2),
    append(Pairs1, Pairs2, Pairs).


% exogenous_corresponding_der_pair(+relation, +exogenous, +par1, +der1, -der2, -der2)
% upon backtracking generates all corresponding pairs,
% with implicative direction: 1 -> 2. undirected correspondence yields 2 pairs

exogenous_corresponding_der_pair(C, EX, Par1, Der1, Par2, Der2):-
    member(C, [dv_correspondence(Par1, Der1, Par2, Der2),
               dv_correspondence(Par2, Der2, Par1, Der1),
               dir_dv_correspondence(Par2, Der2, Par1, Der1)
               ]),
    memberchk(Par1, EX),
    memberchk(Par2, EX).

exogenous_corresponding_der_pair(C, EX, Par1, Der1, Par2, Der2):-
    member(C, [   dq_correspondence(Par1, Par2),
                  dq_correspondence(Par2, Par1),
                  dir_dq_correspondence(Par2, Par1),
                  full_correspondence(Par1, Par2),
                  full_correspondence(Par2, Par1),
                  dir_full_correspondence(Par2, Par1)
                  ]),
    memberchk(Par1, EX),
    memberchk(Par2, EX),
    derivative_qspace(List),
    member(Der1/Der2, List).

exogenous_corresponding_der_pair(C, EX, Par1, Der1, Par2, Der2):-
    member(C, [ mirror_dq_correspondence(Par1, Par2),
                mirror_dq_correspondence(Par2, Par1)
                ]),
    memberchk(Par1, EX),
    memberchk(Par2, EX),
    mirrored_derivative_qspace(List),
    member(Der1/Der2, List).


% q spaces for all derivatives equal: mzp
derivative_qspace([plus/plus, zero/zero, min/min]).
mirrored_derivative_qspace([plus/min, zero/zero, plus/min]).

/*
%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tag_from_to_terminations([], []).
% nb assume single cause... no merges yet for these terminations...
% empty conditions: derivative termination
tag_from_to_terminations([to(cause([Cause]),
			    conditions(Conditions),
			    results(Results),
			    ToState,
			    Status)/I/P|Tail],
			 [Par/From/To/to(cause([Cause]),
			    conditions(Conditions),
			    results(Results),
			    ToState,
			    Status)/I/P|TaggedTail]):-
	Cause =.. [Type, Par],
	derivative_from_to(Type, From, To),
	tag_from_to_terminations(Tail, TaggedTail).


derivative_from_to(derivative_up_to_stable, plus, zero).
derivative_from_to(exogenous_up_to_stable, plus, zero).
derivative_from_to(derivative_down_to_stable, min, zero).
derivative_from_to(exogenous_down_to_stable, min, zero).
derivative_from_to(derivative_stable_to_down, zero, min).
derivative_from_to(exogenous_stable_to_down, zero, min).
derivative_from_to(derivative_stable_to_up, zero, plus).
derivative_from_to(exogenous_stable_to_up, zero, plus).

remove_from_to_tags([], []).
% nb assume single cause... no merges yet for these terminations...
% empty conditions: derivative termination
remove_from_to_tags([_Par/_From/_To/to(cause([Cause]),
			    conditions(Conditions),
			    results(Results),
			    ToState,
			    Status)/I/P|TaggedTail],
			 [to(cause([Cause]),
			    conditions(Conditions),
			    results(Results),
			    ToState,
			    Status)/I/P|Tail]):-
	remove_from_to_tags(TaggedTail, Tail).

*/

% For every corresponding derivative pair check if any constraints on
% involved terminations are needed or terminations need to be removed.
derivative_merge_remove([], _, To, To, Combi, Combi).

% stop immediately if exoTo is empty...
derivative_merge_remove([_|_], _, [], [], Combi, Combi).


% pair is inactive, next
% no derivative terminations towards conditional derivative
% neither is conditional derivative present
derivative_merge_remove([pair(Par1, Der1, _, _)|Pairs], Values, ToIn, ToOut, CombiIn, CombiOut):-
    \+ member(value(Par1, _, _, Der1), Values),
    \+ termination_applicable_to_derivative(results, Par1, Der1, ToIn, _),
    !,
    derivative_merge_remove(Pairs, Values, ToIn, ToOut, CombiIn, CombiOut).

% Category 1:
% pair is active,
% but no terminations away from conditional derivative
% (NB. coming terminations cannot be present Par1 has Der1)
% remove any terminations away from the consequent derivative
% (NB. coming terminations cannot be present Par2 must have Der2)
%
% NB! This needs a thinkover! derivatives can also change without
% a termination... therefore a remove seems too crude

derivative_merge_remove([pair(Par1, Der1, Par2, Der2)|Pairs], Values, ToIn, ToOut, CombiIn, CombiOut):-
    member(value(Par1, _, _, Der1), Values),
    \+ termination_applicable_to_derivative(conditions, Par1, Der1, ToIn, _),
    !,
    remove_derivative_terminations(Par2, Der2, conditions, ToIn, NewTo),
    derivative_merge_remove(Pairs, Values, NewTo, ToOut, CombiIn, CombiOut).


% Category 2:
% pair is active,
% termination(s) away from conditional derivative
% (NB. coming terminations cannot be present Par1 has Der1)
% constrain any terminations away from the consequent derivative
% they must only happen together with termination(s) away from Der1
% (NB. coming terminations cannot be present Par2 must have Der2)
derivative_merge_remove([pair(Par1, Der1, Par2, Der2)|Pairs], Values, ToIn, ToOut, CombiIn, CombiOut):-
    member(value(Par1, _, _, Der1), Values),
    findall(C1, termination_applicable_to_derivative(conditions, Par1, Der1, ToIn, C1), Causes),
    Causes = [_|_], % at least one
    must_happen_together_with_dversion(Causes, Par2, Der2, conditions, ToIn, CombiIn, NewCombi),
    !,
    derivative_merge_remove(Pairs, Values, ToIn, ToOut, NewCombi, CombiOut).

% Category 3:
% pair becomes active,
% 1 termination T1 coming to conditional derivative (can only be one...)
% Par2 is already at consequential Der2
% constrain any terminations away from the consequent derivative
% they must only happen without termination T1 towards Der1
derivative_merge_remove([pair(Par1, Der1, Par2, Der2)|Pairs], Values, ToIn, ToOut, CombiIn, CombiOut):-
    member(value(Par2, _, _, Der2), Values),
    termination_applicable_to_derivative(results, Par1, Der1, ToIn, Cause/_/Index),
    !,
    must_occur_apart_dversion(Cause/_/Index, Par2, Der2, conditions, ToIn, CombiIn, NewCombi),
    derivative_merge_remove(Pairs, Values, ToIn, ToOut, NewCombi, CombiOut).

% Category 4:
% pair becomes active,
% 1 termination T1 coming to conditional derivative (can only be one...)
% Par2 is not at consequential Der2
% No termination for Par2 to Der2
% remove termination T1 towards Der1
derivative_merge_remove([pair(Par1, Der1, Par2, Der2)|Pairs], Values, ToIn, ToOut, CombiIn, CombiOut):-
    \+ member(value(Par2, _, _, Der2), Values),
    termination_applicable_to_derivative(results, Par1, Der1, ToIn, Cause/_/_),
    \+ termination_applicable_to_derivative(results, Par2, Der2, ToIn, _),
    !,
    etrace(transitions_corr_rem_fut, [Cause], ordering),
    indexed_apply_order_actions(remove_correspondence, [remove([Cause])], ToIn, NewTo),
    derivative_merge_remove(Pairs, Values, NewTo, ToOut, CombiIn, CombiOut).

% Category 5:
% pair becomes active,
% 1 termination T1 coming to conditional derivative (can only be one...)
% Par2 is not at consequential Der2
% 1 termination for Par2 to Der2 (can only be one...)
% terminations need each other
derivative_merge_remove([pair(Par1, Der1, Par2, Der2)|Pairs], Values, ToIn, ToOut, CombiIn, CombiOut):-
    termination_applicable_to_derivative(results, Par1, Der1, ToIn, C1/_/Index1),
    termination_applicable_to_derivative(results, Par2, Der2, ToIn, C2/_/Index2),
    !,
    (Index1 = Index2 -> NewCombi = CombiIn ; % indices equal: already merged
    etrace(transitions_corr_inc_alone_fut, [[C1], [C2]], ordering),
    add_to_combinations_table(CombiIn, [[Index1]/inconsistent,
                                        [Index1, Index2]/consistent], NewCombi)),
    derivative_merge_remove(Pairs, Values, ToIn, ToOut, NewCombi, CombiOut).



% remove_terminations(Par, Val, conditions/results, ToIn, ToOut)
% remove any terminations present with Par/Val in conditions or results
% Category 1
% NB unused now? because derivatives may also change without a
% termination thus not justifying a remove?
remove_derivative_terminations(Par, Val, ComingGoing, ToIn, ToOut):-
    findall(C1, termination_applicable_to_derivative(ComingGoing, Par, Val, ToIn, C1), Causes),
    remove_derivative_terminations(Causes, ToIn, ToOut).

remove_derivative_terminations([], To, To).

remove_derivative_terminations([Cause/_/_|CauseTail], ToIn, ToOut):-
    etrace(transitions_corr_rem_act, [Cause], ordering),
    indexed_apply_order_actions(remove_correspondence, [remove([Cause])], ToIn, NewTo),
    remove_derivative_terminations(CauseTail, NewTo, ToOut).


% must_happen_together_with_dversion(Causes, Par, Der, conditions/results, ToIn, CombiIn, CombiOut)
% any terminations present with Par/Der in conditions or results must happen together
% with termination(s) in causes, these are free themselves
% Category 2

% two terminations to combine with
must_happen_together_with_dversion([C1/_/I1|C2/_/I2], Par, Der, ComingGoing, ToIn, CombiIn, CombiOut):-
    findall(C, termination_applicable_to_derivative(ComingGoing, Par, Der, ToIn, C), Causes),
    must_happen_together_with(C1/_/I1, Causes, CombiIn, NewCombi),
    must_happen_together_with(C2/_/I2, Causes, NewCombi, CombiOut).

% one termination to combine with
must_happen_together_with_dversion([Cause/_/Index], Par, Der, ComingGoing, ToIn, CombiIn, CombiOut):-
    findall(C1, termination_applicable_to_derivative(ComingGoing, Par, Der, ToIn, C1), Causes),
    must_happen_together_with(Cause/_/Index, Causes, CombiIn, CombiOut).


% must_happen_apart_dversion(Index, Par, Val, conditions/results, ToIn, CombiIn, CombiOut)
% any terminations present with Par/Val in conditions or results must not happen together
% with Index termination
% Category 3
must_occur_apart_dversion(Cause/_/Index, Par, Der, ComingGoing, ToIn, CombiIn, CombiOut):-
    findall(C1, termination_applicable_to_derivative(ComingGoing, Par, Der, ToIn, C1), Causes),
    must_occur_apart(Cause/_/Index, Causes, CombiIn, CombiOut).



% termination_applicable_to_derivative(+cond/res, +Par, +Der, +ToIn, -Cause/Causes)
% find terminations that act upon a certain Derivative.
% upon backtracking return all terminations that have Par and Der as conditions and change those
termination_applicable_to_derivative(conditions, Par, Der, ToIn, Cause/Causes/I):-
    member(to(cause(Causes), _Cond, _Res, _ToState, _Status)/I/_, ToIn),
    memberchk(Cause, Causes),
    Cause =.. [Type, Par],
    derivative_from_to(Type, Der, _).

% upon backtracking return all terminations that have Par and Der as result
termination_applicable_to_derivative(results, Par, Der, ToIn, Cause/Causes/I):-
    member(to(cause(Causes), _Cond, _Res, _ToState, _Status)/I/_, ToIn),
    memberchk(Cause, Causes),
    Cause =.. [Type, Par],
    derivative_from_to(Type, _, Der).



derivative_from_to(derivative_up_to_stable, plus, zero).
derivative_from_to(assumed_derivative_up_to_stable, plus, zero).
derivative_from_to(exogenous_up_to_stable, plus, zero).
derivative_from_to(derivative_down_to_stable, min, zero).
derivative_from_to(assumed_derivative_down_to_stable, min, zero).
derivative_from_to(exogenous_down_to_stable, min, zero).
derivative_from_to(derivative_stable_to_down, zero, min).
derivative_from_to(assumed_derivative_stable_to_down, zero, min).
derivative_from_to(exogenous_stable_to_down, zero, min).
derivative_from_to(derivative_stable_to_up, zero, plus).
derivative_from_to(assumed_derivative_stable_to_up, zero, plus).
derivative_from_to(exogenous_stable_to_up, zero, plus).



























/*
% OLD version uses only exogenous
% first determine exogenous corresponding derivative pairs
% then determine merges & removes for every pair
correspondence_merge_remove_exogenous(SMD, ToIn, ToOut, CombiIn, CombiOut):-
    smd_slots(SMD, _, _, _, Values, Relations, _, _),
    get_store(SMD, Store),
    store_ef(Store, EF),
    store_ec(Store, EC),s
    % other exogenous types do not have terminations
    smerge(EF, EC, EX),
    %
    collect_exogenous_d_correspondences(Relations, EX, ParDerPairs),
    list_to_set(ParDerPairs, Pairs),
    exogenous_merge_remove(Pairs, Values, ToIn, ToOut, CombiIn, CombiOut).
*/
/*
% New version uses exogenous AND pars that are in derivative termiations
% first determine exogenous corresponding derivative pairs
% then determine merges & removes for every pair
correspondence_merge_remove_derivative(SMD, ToIn, ToOut, CombiIn, CombiOut):-
    smd_slots(SMD, _, _, _, Values, Relations, _, _),
%    get_store(SMD, Store),
%    store_ef(Store, EF),
%    store_ec(Store, EC),
    % other exogenous types do not have terminations
%    smerge(EF, EC, EX),
    %
    collect_applicable_d_correspondences(Relations, ToIn, ParDerPairs),
    list_to_set(ParDerPairs, Pairs),
    exogenous_merge_remove(Pairs, Values, ToIn, ToOut, CombiIn, CombiOut).


% For every corresponding derivative pair check if any constraints on
% involved terminations are needed or terminations need to be removed.
exogenous_merge_remove([], _, To, To, Combi, Combi).

% stop immediately if exoTo is empty...
exogenous_merge_remove([_|_], _, [], [], Combi, Combi).


% pair is inactive, next
% no derivative terminations towards conditional derivative
% neither is conditional derivative present
exogenous_merge_remove([pair(Par1, Der1, _, _)|Pairs], Values, ToIn, ToOut, CombiIn, CombiOut):-
    \+ member(value(Par1, _, _, Der1), Values),
    \+ termination_applicable_to_derivative(results, Par1, Der1, ToIn, _),
    !,
    exogenous_merge_remove(Pairs, Values, ToIn, ToOut, CombiIn, CombiOut).

% Category 1:
% pair is active,
% but no terminations away from conditional derivative
% (NB. coming terminations cannot be present Par1 has Der1)
% remove any terminations away from the consequent derivative
% (NB. coming terminations cannot be present Par2 must have Der2)
exogenous_merge_remove([pair(Par1, Der1, Par2, Der2)|Pairs], Values, ToIn, ToOut, CombiIn, CombiOut):-
    member(value(Par1, _, _, Der1), Values),
    \+ termination_applicable_to_derivative(conditions, Par1, Der1, ToIn, _),
    !,
    remove_terminations(Par2, Der2, conditions, ToIn, NewTo),
    exogenous_merge_remove(Pairs, Values, NewTo, ToOut, CombiIn, CombiOut).

% Category 2:
% pair is active,
% termination(s) away from conditional derivative
% (NB. coming terminations cannot be present Par1 has Der1)
% constrain any terminations away from the consequent derivative
% they must only happen together with termination(s) away from Der1
% (NB. coming terminations cannot be present Par2 must have Der2)
exogenous_merge_remove([pair(Par1, Der1, Par2, Der2)|Pairs], Values, ToIn, ToOut, CombiIn, CombiOut):-
    member(value(Par1, _, _, Der1), Values),
    findall(C1, termination_applicable_to_derivative(conditions, Par1, Der1, ToIn, C1), Causes),
    Causes = [_|_], % at least one
    must_happen_together_with_exo(Causes, Par2, Der2, conditions, ToIn, CombiIn, NewCombi),
    !,
    exogenous_merge_remove(Pairs, Values, ToIn, ToOut, NewCombi, CombiOut).

% Category 3:
% pair becomes active,
% 1 termination T1 coming to conditional derivative (can only be one...)
% Par2 is already at consequential Der2
% constrain any terminations away from the consequent derivative
% they must only happen without termination T1 towards Der1
exogenous_merge_remove([pair(Par1, Der1, Par2, Der2)|Pairs], Values, ToIn, ToOut, CombiIn, CombiOut):-
    member(value(Par2, _, _, Der2), Values),
    termination_applicable_to_derivative(results, Par1, Der1, ToIn, Cause/_/Index),
    !,
    must_occur_apart_exo(Cause/_/Index, Par2, Der2, conditions, ToIn, CombiIn, NewCombi),
    exogenous_merge_remove(Pairs, Values, ToIn, ToOut, NewCombi, CombiOut).

% Category 4:
% pair becomes active,
% 1 termination T1 coming to conditional derivative (can only be one...)
% Par2 is not at consequential Der2
% No termination for Par2 to Der2
% remove termination T1 towards Der1
exogenous_merge_remove([pair(Par1, Der1, Par2, Der2)|Pairs], Values, ToIn, ToOut, CombiIn, CombiOut):-
    \+ member(value(Par2, _, _, Der2), Values),
    termination_applicable_to_derivative(results, Par1, Der1, ToIn, Cause/_/_),
    \+ termination_applicable_to_derivative(results, Par2, Der2, ToIn, _),
    !,
    etrace(transitions_corr_rem_fut, [Cause], ordering),
    indexed_apply_order_actions(remove_correspondence, [remove([Cause])], ToIn, NewTo),
    exogenous_merge_remove(Pairs, Values, NewTo, ToOut, CombiIn, CombiOut).

% Category 5:
% pair becomes active,
% 1 termination T1 coming to conditional derivative (can only be one...)
% Par2 is not at consequential Der2
% 1 termination for Par2 to Der2 (can only be one...)
% terminations need each other
exogenous_merge_remove([pair(Par1, Der1, Par2, Der2)|Pairs], Values, ToIn, ToOut, CombiIn, CombiOut):-
    termination_applicable_to_derivative(results, Par1, Der1, ToIn, C1/_/Index1),
    termination_applicable_to_derivative(results, Par2, Der2, ToIn, C2/_/Index2),
    !,
    (Index1 = Index2 -> NewCombi = CombiIn ; % indices equal: already merged
    etrace(transitions_corr_inc_alone_fut, [[C1], [C2]], ordering),
    add_to_combinations_table(CombiIn, [[Index1]/inconsistent,
                                        [Index1, Index2]/consistent], NewCombi)),
    exogenous_merge_remove(Pairs, Values, ToIn, ToOut, NewCombi, CombiOut).


% must_happen_together_with_exo(Causes, Par, Der, conditions/results, ToIn, CombiIn, CombiOut)
% any terminations present with Par/Der in conditions or results must happen together
% with termination(s) in causes, these are free themselves
% Category 2

% two terminations to combine with
must_happen_together_with_exo([C1/_/I1|C2/_/I2], Par, Der, ComingGoing, ToIn, CombiIn, CombiOut):-
    findall(C, termination_applicable_to_derivative(ComingGoing, Par, Der, ToIn, C), Causes),
    must_happen_together_with(C1/_/I1, Causes, CombiIn, NewCombi),
    must_happen_together_with(C2/_/I2, Causes, NewCombi, CombiOut).

% one termination to combine with
must_happen_together_with_exo([Cause/_/Index], Par, Der, ComingGoing, ToIn, CombiIn, CombiOut):-
    findall(C1, termination_applicable_to_derivative(ComingGoing, Par, Der, ToIn, C1), Causes),
    must_happen_together_with(Cause/_/Index, Causes, CombiIn, CombiOut).


% must_happen_apart(Index, Par, Val, conditions/results, ToIn, CombiIn, CombiOut)
% any terminations present with Par/Val in conditions or results must not happen together
% with Index termination
% Category 3
must_occur_apart_exo(Cause/_/Index, Par, Der, ComingGoing, ToIn, CombiIn, CombiOut):-
    findall(C1, termination_applicable_to_derivative(ComingGoing, Par, Der, ToIn, C1), Causes),
    must_occur_apart(Cause/_/Index, Causes, CombiIn, CombiOut).


% termination_applicable_to_derivative(+cond/res, +Par, +Der, +ToIn, -Cause/Causes)
% find terminations that act upon a certain Derivative.
% upon backtracking return all terminations that have Par and Der as conditions and change those
termination_applicable_to_derivative(conditions, Par, Der, ToIn, Cause/Causes/I):-
    member(to(cause(Causes), conditions(Cond), _Res, _ToState, _Status)/I/_, ToIn),
    member(par_values(CValues), Cond),
    member(value(Par, _, _, Der), CValues),
    member(Cause, Causes),
    exogenous_terminations_set(List, Par),
    memberchk(Cause, List).

% upon backtracking return all terminations that have Par and Der as result
termination_applicable_to_derivative(results, Par, Der, ToIn, Cause/Causes/I):-
    member(to(cause(Causes), _Cond, results(Res), _ToState, _Status)/I/_, ToIn),
    member(par_values(Values), Res),
    member(value(Par, _, _, Der), Values),
    member(Cause, Causes),
    exogenous_terminations_set(List, Par),
    memberchk(Cause, List).



collect_applicable_d_correspondences(Relations, ToIn, ParDerPairs):-
	extract_applicable_parameters(ToIn, ApplicableParameters),
	collect_exogenous_d_correspondences(Relations, ApplicableParameters, ParDerPairs).

extract_applicable_parameters([], []).
extract_applicable_parameters([to(cause(Causes), _, _, _, _)/_/_|T], Parameters):-
	findall(Par, par_in_causes(Par, Causes), Pars),
	extract_applicable_parameters(T, PT),
	append(Pars, PT, Parameters).

par_in_causes(Par, Causes):-
	member(Cause, Causes),
	Cause =.. [_, Par].

% for every relation:
% determine corresponding pairs(Par1, Der1, Par2, Der2)
% where relation is a d_correspondence (or full),
% and it is only about exogenous parameters.

% no relevant parameters: stop
collect_exogenous_d_correspondences(_, [], []):-!.

collect_exogenous_d_correspondences([], _, []).

collect_exogenous_d_correspondences([Rel|T], EX, Pairs):-
    findall(pair(Par1, Der1, Par2, Der2),
            exogenous_corresponding_der_pair(Rel, EX, Par1, Der1, Par2, Der2),
            Pairs1),
    collect_exogenous_d_correspondences(T, EX, Pairs2),
    append(Pairs1, Pairs2, Pairs).


% exogenous_corresponding_der_pair(+relation, +exogenous, +par1, +der1, -der2, -der2)
% upon backtracking generates all corresponding pairs,
% with implicative direction: 1 -> 2. undirected correspondence yields 2 pairs

exogenous_corresponding_der_pair(C, EX, Par1, Der1, Par2, Der2):-
    member(C, [dv_correspondence(Par1, Der1, Par2, Der2),
               dv_correspondence(Par2, Der2, Par1, Der1),
               dir_dv_correspondence(Par2, Der2, Par1, Der1)
               ]),
    memberchk(Par1, EX),
    memberchk(Par2, EX).

exogenous_corresponding_der_pair(C, EX, Par1, Der1, Par2, Der2):-
    member(C, [   dq_correspondence(Par1, Par2),
                  dq_correspondence(Par2, Par1),
                  dir_dq_correspondence(Par2, Par1),
                  full_correspondence(Par1, Par2),
                  full_correspondence(Par2, Par1),
                  dir_full_correspondence(Par2, Par1)
                  ]),
    memberchk(Par1, EX),
    memberchk(Par2, EX),
    derivative_qspace(List),
    member(Der1/Der2, List).

exogenous_corresponding_der_pair(C, EX, Par1, Der1, Par2, Der2):-
    member(C, [ mirror_dq_correspondence(Par1, Par2),
                mirror_dq_correspondence(Par2, Par1)
                ]),
    memberchk(Par1, EX),
    memberchk(Par2, EX),
    mirrored_derivative_qspace(List),
    member(Der1/Der2, List).


% q spaces for all derivatives equal: mzp
derivative_qspace([plus/plus, zero/zero, min/min]).
mirrored_derivative_qspace([plus/min, zero/zero, plus/min]).
*/

/***___________________ TERMINATING EQUALITY __________________________***/

/* FL march 2004: notes

idea:
Inequality changes are often related to value changes, for example X = Y
where X = Y = zero, cannot become X>Y if not X or Y leaves zero.

Often no merges or removes can be derived, but constraints can be placed on valid
combinations, to find these:

solve combinations of terminations concerning all pairs of parameters where:
- landmark information about the current or future values of parameters is present,
- an equality termination for the parameters is present

When the equality termination does not fire, its par_relations conditions are added,
The combinations of terminations are generated using a constrained crossproduct, but
only checking for fatal combinations (see: indexed_c_constrained_crossproduct()).

FL, May 2004: Constant (defined by assumption) normal equalities relevant to the context
are added to the solve...
*/


equality_termination_constraints(ValueTo, EqTo, SMD, CombiIn, CombiOut, Contexts):-
    % construct 2-quantity contexts:
    smd_slots(SMD, _, _, Parameters, Values, _, _, _),
    get_store(SMD, Store),
    store_lmr(Store, LM),
    store_cr(Store, Constant1),
    pure_inequality_relations(Constant1, Constant2),
    parameters_in_relations(Constant2, Constant), % make structures: relation/[pars]
    append([equal(zero, zero)], LM, LandMarks),
    get_parameter_contexts(EqTo, Parameters, Values, Pairs1),
    list_to_set(Pairs1, Pairs),
    % process contexts:
    eq_termination_constraints(Pairs, EqTo, LandMarks, Constant, ValueTo, CombiIn, CombiOut, Contexts),
    !.


% for every pair check if to be solved (terminations and landmark info present),
% if so, solve...
% pair structure:  ParNames/PList/ValNames/VList
% Plist & Vlist for add-givens.
% ParNames & ValNames for direct use.

% done, return combination constraints table
eq_termination_constraints([], _EqTo, _LandMarks, _Constant, _ValueTo, Combi, Combi, []).

% relevant pair: solve
eq_termination_constraints([[Par1, Par2]/PList/[Val1, Val2]/VList|Pairs],
                           EqToIn, LandMarks, Constant, ValueToIn, CombiIn, CombiOut, [[Par1, Par2]|Contexts]):-
    % there is at least one equality termination about par's:
    findall(EqT/I/P, (member(EqT/I/P, EqToIn), subset(P, [Par1, Par2])), EqTo),
    \+ EqTo = [],

    % get all related value terminations for both parameters:
    findall(VT1/I1/Pars1, (member(VT1/I1/Pars1, ValueToIn),
                     member(Par1, Pars1)),
%                     memberchk(P1, [Par1, Par2])),
                     VTo1),
    findall(VT2/I2/Pars2, (member(VT2/I2/Pars2, ValueToIn),
                     member(Par2, Pars2)),
%                     memberchk(P2, [Par1, Par2])),
                     VTo2),

    % determine all possible values for both parameters:
    future_values(VTo1, Future1),
    future_values(VTo2, Future2),
    append([Val1], Future1, PossibleV1),
    append([Val2], Future2, PossibleV2),
    list_to_set(PossibleV1, PV1a),
    list_to_set(PossibleV2, PV2a),
    extend_intervals_to_adjacent_points(PV1a, Par1, PV1), % needed because a future interval can be bounded by
    extend_intervals_to_adjacent_points(PV2a, Par2, PV2), % a next point about which a landmark relation is known

    % at least one landmark relation exist for an attainable valueset:
    landmark_relations_present(LandMarks, PV1,  PV2, LMPresent),
    \+ LMPresent = [],
    !, % At this point the pair is relevant: solve & derive consistent/inconsistent combinations

    % determine constrained crossproduct: (with index combinations, using only fatal constraints)
    append(VTo1, VTo2, VTo),
    append(EqTo, VTo, ToTest1),
    list_to_set(ToTest1, ToTest),
    indexed_c_constrained_crossproduct(ToTest, CombiIn, Crossproduct),

    % solve every combination of terminations,
    % combined with the values for the parameter pair:

    % if e-q termination is not in combination then add it's conditions:
    % nl. the original equality.
    condition_relations_toset(EqTo, EqConditions),

    % zero equality is trivial for solve
    delete(LMPresent, equal(zero, zero), LMInfo),
    relevant_relations(Constant, [Par1, Par2] , ConstantRelations),
    append(ConstantRelations, LMInfo, ExtraRels),

    etrace(transitions_solve_context, [ToTest, [Par1, Par2], [Val1, Val2], ExtraRels], ordering),

    % actual solve call:
    test_crossproduct(Crossproduct, PList, VList, ExtraRels, EqConditions, TestResults),

    % if no inconsistency, then no extra information in results
    process_results(TestResults, Combinations),

    % store results:
    add_to_combinations_table(CombiIn, Combinations, NewCombi),

    etrace(transitions_solve_context_done, ToTest, ordering),

    % next context:
    eq_termination_constraints(Pairs, EqToIn, LandMarks, Constant, ValueToIn, NewCombi, CombiOut, Contexts).


% irrelevant pair: next
eq_termination_constraints([_|Pairs], EqToIn, LandMarks, Constant, ValueToIn, CombiIn, CombiOut, Contexts):-
    eq_termination_constraints(Pairs, EqToIn, LandMarks, Constant, ValueToIn, CombiIn, CombiOut, Contexts).


% get all  values to where terminations lead
future_values([], []).

future_values([To/_/_|ValueTo], Values):-
    To = to(_,_,results(Results), _,_),
    memberchk(par_values(ValueResults), Results),
    findall(Val, member(value(_, _, Val, _), ValueResults), Values1),
    future_values(ValueTo, Values2),
    append(Values1, Values2, Values).


% get all landmark relations about values X & Values Y
landmark_relations_present(LandMarks, ValuesX,  ValuesY, LMPresent):-
    findall(LMRel, (member(LMRel, LandMarks),
                    relevant_landmark_relation(LMRel, ValuesX, ValuesY)
                    ),
                    LMPresent).

/*
% relations of landmarks to zero give no information ? (e.g. max(X) > zero)
% Yes they do znm: both normal, cannot go to zero & max if equal.
% therefore this clause is wrong:
relevant_landmark_relation(LMRel, _ValuesX, _ValuesY):-
    LMRel =.. [R, X, Y],
    memberchk(R, [greater, smaller]),
    (X = zero
    ;
    Y = zero),
    !, fail.
*/

% Relation between landmarks is about values X and Y have or can take on.
relevant_landmark_relation(LMRel, ValuesX, ValuesY):-
    LMRel =.. [_, X, Y],
    memberchk(X, ValuesX),
    memberchk(Y, ValuesY),
    !.

% Relation between landmarks is about values X and Y have or can take on.
relevant_landmark_relation(LMRel, ValuesX, ValuesY):-
    LMRel =.. [_, X, Y],
    memberchk(X, ValuesY),
    memberchk(Y, ValuesX),
    !.

extend_intervals_to_adjacent_points(In, Par, Out):-
	split_points_intervals(In, Pin, Intervals),
	qspace(Par, _X, PointIntervalList, _Z),
	collect_adjacent_points(Intervals, PointIntervalList, Pad),
	smerge(Pin, Pad, Points),
	append(Points, Intervals, Out).


split_points_intervals([], [], []).

%variable! see as point (no action)
split_points_intervals([Var|T], [Var|PT], IT):-
	var(Var),
	!,
	split_points_intervals(T, PT, IT).


%point zero
split_points_intervals([zero|T], [zero|PT], IT):-
	!,
	split_points_intervals(T, PT, IT).

%point one
split_points_intervals([one|T], [one|PT], IT):-
	!,
	split_points_intervals(T, PT, IT).

%point minusone
split_points_intervals([minusone|T], [minusone|PT], IT):-
	!,
	split_points_intervals(T, PT, IT).

%interval
split_points_intervals([I|T], PT, [I|IT]):-
	atom(I),
	!,
	split_points_intervals(T, PT, IT).
%point
split_points_intervals([P|T], [P|PT], IT):-
	split_points_intervals(T, PT, IT).


collect_adjacent_points([], _, []).

collect_adjacent_points([I|T], PointIntervalList, Out):-
	c_a_p(I, PointIntervalList, AP),
	collect_adjacent_points(T, PointIntervalList, PT),
	smerge(PT, AP, Out).

%point before and after I, done
c_a_p(I, [point(BP), I, point(AP)|_], [BP, AP]):-
	!.
%point before I, done
c_a_p(I, [point(BP), I], [BP]):-
	!.
%point after I, done
c_a_p(I, [I, point(AP)|_], [AP]):-
	!.
%else: next
c_a_p(I, [_|T], Points):-
	c_a_p(I, T, Points).



/***____________________ COMPLEX EQUALITY CONSTRAINTS _____________________***/

/* IDEA:
a complex (larger than binairy, like A = B - C), equality does not terminate and
is assumed to remain active in the next state. If for example an equality termination
for B>C to B=C is present it cannot take place without A becoming zero.

For every complex constraint in the SMD:
- find the involved paramaters and their present values
- find all value terminations associated with these parameters
(NB. because of merge_correspondence these can bring along terminations about
parameters outside the context, these will not play any role though because
their present value is not included in the process.)
- find all binairy equality terminations falling within the context
(no outside parameters here)
? SHOULD OTHER (DOMAIN SPECIFIC) TERMINATIONS BE INCLUDED ? FL: No, no telling what problems can arise, low chance of gain.
- solve all combinations of this termination context including landmark information
and par_relation conditions of eq-terminations in the context, but not in the
specific combination tested.

The combinations of terminations are generated using a constrained crossproduct,
but only checking for fatal combinations (see: indexed_c_constrained_crossproduct()).

FL may 2004: added constant relations (defined by assumptions)
*/

complex_equality_constraints(ValueTo, EqTo, SMD, CombiIn, CombiOut, UsedContexts):-
    % construct quantity contexts:
    smd_slots(SMD, _, _, Parameters, Values, Relations, _, _),
    get_store(SMD, Store),
    store_lmr(Store, LandMarks1),
    parameters_in_relations(LandMarks1, LandMarks),   % relation/pars structures
    store_cr(Store, Constant1),
    pure_inequality_relations(Constant1, Constant), %filter out all other types
    get_complex_relations(Relations, Values, Complex1),
    smerge(Constant, Complex1, Complex),
    parameters_involved_eq(Complex, ComplexParameters), % complex/nil/parameters structures
    get_parameter_contexts(ComplexParameters, Parameters, Values, Contexts1),
    list_to_set(Contexts1, Contexts),
    % process contexts:
    complex_eq_constraints(Contexts, ComplexParameters, EqTo, ValueTo, LandMarks, CombiIn, CombiOut, UsedContexts),
    !.


% for every parameter context:
% - select all complex equalities (may be more then one !/?)
% - select all valueterminations involved
% - select all eq-terminations involved
% if no termination present, -> next context
% - make crossproduct, solve, add results
%
% context structure:  ParNames/PList/ValNames/VList
% Plist & Vlist for add-givens.
% ParNames & ValNames for direct use.
% ComplexParameters:  Complex/nil/ParNames structures

% done, return combination constraints table
complex_eq_constraints([], _Complex, _EqTo, _ValueTo, _LandMarks, Combi, Combi, _).

% relevant context: solve
complex_eq_constraints([Context|Tail], ComplexParameters, EqToIn, ValueToIn, LandMarks, CombiIn, CombiOut, UsedContexts):-
    Context = ParNames/PList/ValNames/VList,
    \+ memberchk(ParNames, UsedContexts),
    findall(Equality, (member(Equality/nil/P, ComplexParameters), subset(P, ParNames)), Complex),

    % get all related value terminations
    findall(VT/I1/Pars, (member(VT/I1/Pars, ValueToIn),
                     member(P1, Pars),
                     memberchk(P1, ParNames)),
                     VTo),
    list_to_set(VTo, ValueTo),

    % get all relevant eq-terminations (within the context)
    findall(EQT/I2/Pars, (member(EQT/I2/Pars, EqToIn),
                     subset(Pars, ParNames)),
                     EQTolist),
    list_to_set(EQTolist, EqTo),

    % not both empty, therefore context is relevant:
    \+ (EqTo = [], ValueTo = []),
    !,

    % determine constrained crossproduct (with index combinations, using only fatal constraints)
    append(EqTo, ValueTo, ToTest),
    indexed_c_constrained_crossproduct(ToTest, CombiIn, Crossproduct),

    % solve every combination of terminations,
    % combined with the values for the parameter pair:
    % if e-q termination is not in combination then add it's par-relation conditions:
    % the original equality.
    condition_relations_toset(EqTo, EqConditions),
    parameters_in_relations(Complex, TaggedComplex),
    append(TaggedComplex, LandMarks, TaggedRelations),
    relevant_relations(TaggedRelations, ParNames , Constant),
    etrace(transitions_solve_context, [ToTest, ParNames, ValNames, Constant], ordering),
    test_crossproduct(Crossproduct, PList, VList, Constant, EqConditions, TestResults),
    % if no inconsistency, then no extra information in results
    process_results(TestResults, Combinations),
    add_to_combinations_table(CombiIn, Combinations, NewCombi),
    etrace(transitions_solve_context_done, ToTest,ordering),
    complex_eq_constraints(Tail, ComplexParameters, EqToIn, ValueToIn, LandMarks, NewCombi, CombiOut, UsedContexts).


% irrelevant context: next
complex_eq_constraints([_|Tail], ComplexParameters, EqTo, ValueTo, LandMarks, CombiIn, CombiOut, UsedContexts):-
    complex_eq_constraints(Tail, ComplexParameters, EqTo, ValueTo, LandMarks, CombiIn, CombiOut, UsedContexts).



/***________ Solve to determine constraints: general predicates ___________***/

% get_parameter_contexts(+ToList, +parameterstatemens, +valuestatements, -contexts)
% tolist is termination/index/involved_parameters list
% generate parametercontextstructures, with parameters & parNames
% for every termination (tip-structure) in tolist
% full branching eq terminations will result in double pairs (list_to-set useful)

get_parameter_contexts([], _, _, []).

get_parameter_contexts([_/_/ParList|Tail], Parameters, Values, [Pair|Pairs]):-
        value_pair(ParList, Parameters, Values, Pair),
        !,
        get_parameter_contexts(Tail, Parameters, Values, Pairs).

% non valid ParList: landmarks -> discard eqcontext
get_parameter_contexts([_/_/_|Tail], Parameters, Values, Pairs):-
        get_parameter_contexts(Tail, Parameters, Values, Pairs).


% make parameter context structure for all Pars in List. Parameters/PList/Values/VList
% Plist & VList are the SMD statements for add_givens
% See terminating_equality_constraints, complex_equality_constraints etc.
value_pair([], _Parameters, _Values, []/[]/[]/[]).

value_pair([Par|Tail], Parameters, Values, ParNames/PList/ValNames/VList):-
    member(P, Parameters),
    P =.. [_, _, Par, _, _],
    memberchk(value(Par, Q, Val, Der), Values),
    value_pair(Tail, Parameters, Values, PNTail/PTail/VNTail/VTail),
    VList = [value(Par, Q, Val, Der)|VTail],
    PList = [P|PTail],
    ParNames = [Par|PNTail],
    ValNames = [Val|VNTail].


% from one (or more mutualy exclusive) equality termination(s)
% get all parameter relations from conditions in one list.
condition_relations_toset([], []).

condition_relations_toset([H|T], EqConditions):-
    H = to(_, conditions(Cond), _, _, _)/_/_,
    findall(Rels, member(par_relations(Rels), Cond), RelsList),
    flatten(RelsList, Relations),
    condition_relations_toset(T, EqCT),
    append(Relations, EqCT, All),
    list_to_set(All, EqConditions).

% SAME for single 'to' without to()/I/P structure

condition_relations_to(to(_, conditions(Cond), _, _, _), EqConditions):-
    findall(Rels, member(par_relations(Rels), Cond), RelsList),
    flatten(RelsList, Relations),
    list_to_set(Relations, EqConditions).


% remove_value_conditions(+Conditions, +OldValues, -NewValues)
% remove all values present in Conditions, from set of values

remove_value_conditions([], NewValues, NewValues).

% value conditions
remove_value_conditions([par_values(VList)|Tail], OldValues, NewValues):-
    !,
	remove_all(VList, OldValues, NewOldValues),
    remove_value_conditions(Tail, NewOldValues, NewValues).

% other conditions
remove_value_conditions([_|Tail], OldValues, NewValues):-
    remove_value_conditions(Tail, OldValues, NewValues).


% if all combinations consistent, no extra constraints derived:
% discard results
process_results(TestResults, []):-
    \+ memberchk(_/inconsistent, TestResults),
    !.

% make a lean representation for fast computation of the crossproduct later on
% all inconsistent items must be included, because consistent ones could occur
% in between in the 'subset hierarchy'
% all consistent items superseding inconsistent ones are included, but only the
% smallest neccesary sets.
% sometimes a fatal combination still has also fatal supersets, these can also
% be weeded out. the fatal supersets are useless.
process_results(TestResults, Combinations):-
    split_consistent_inconsistent(TestResults, Consistent, InConsistent),
    sort(Consistent, Sorted),
    insert_consistent(Sorted, InConsistent, Combinations1),
    remove_redundant_fatal_combinations(Combinations1, Combinations).

% split and tag inconsistent with its length
split_consistent_inconsistent([], [], []).

split_consistent_inconsistent([List/consistent|Tail], [L/List/consistent|CTail], InCTail):-
    length(List, L),
    split_consistent_inconsistent(Tail, CTail, InCTail).

split_consistent_inconsistent([List/inconsistent|Tail], CTail, [List/inconsistent|InCTail]):-
    split_consistent_inconsistent(Tail, CTail, InCTail).


% for every consistent Item X: (prefixed with length)
% - insert X if superset of some inconsistent item Z,
% and no consistent item Y is present which is a subset of X and also superset of Z,
% because consistent items are ordered to length, the minimal amount of consistent items is
% used.

insert_consistent([], Combinations, Combinations).

insert_consistent([_/X/consistent|Tail], CombinationsIn, CombinationsOut):-
    member(Z/inconsistent, CombinationsIn),
    subset(Z, X),
    \+  (
        member(Y/consistent, CombinationsIn),
        subset(Y, X),
        subset(Z, Y)
        ),
    !,
    insert_consistent(Tail, [X/consistent|CombinationsIn], CombinationsOut).

% skip any other item
insert_consistent([_|Tail], CombinationsIn, CombinationsOut):-
    insert_consistent(Tail, CombinationsIn, CombinationsOut).

% if a fatal combinations has supersets, these supersets can be removed
% see constrained crossproduct for fatal combinations
remove_redundant_fatal_combinations(CombinationsIn, CombinationsOut):-
    get_fatal_combinations(CombinationsIn, Other, FatalIn),
    remove_redundant_fatal(FatalIn, FatalIn, FatalOut),
    append(Other, FatalOut, CombinationsOut).


remove_redundant_fatal([], _, []).

% redundant, remove.
remove_redundant_fatal([Combi1|Tail], All, NewTail):-
    member(Combi2, All),
    Combi1 \= Combi2,
    subset(Combi2, Combi1), % a subset fatal item is present.
    !,
    remove_redundant_fatal(Tail, All, NewTail).

% not redundant, keep add /inconsistent tag again.
remove_redundant_fatal([Combi1|Tail], All, [Combi1/inconsistent|NewTail]):-
   remove_redundant_fatal(Tail, All, NewTail).


% test_crossproduct(+Crossproduct, +PList, +VList, +Constant, +EqConditions, -TestResults):-
% for all combinations in crossproduct: test resulting minisystem:
% return indices/consistent or indices/inconsistent list.

% save work for empty crossproduct
test_crossproduct([], _, _, _, _, []).

test_crossproduct(Crossproduct, PList, VList, Constant, EqConditions, TestResults):-
	flag(q_cnt, _, 0),
    cio_empty(Cempty),
    do_once((add_givens([parameters(PList), par_relations(Constant)], Cempty, Cin))),
    !,
    test_a_combination(Crossproduct, Cin, VList, EqConditions, TestResults).


% solve results of a single combination of terminations
test_a_combination([], _, _, _, []).

% valid combination
test_a_combination([To/Combi|Crossproduct], Cin, VList, EqConditions, [Combi/consistent|Tested]):-
    To = to(cause(L), conditions(Cond), results(Res), _, _),
    etrace(transitions_solve_context_test, L, ordering),
    %remove condition values from VList
    %bug: remove value conditions couldnt remove values with assumed derivative
    %solution: continuity
    do_value_continuity(VList, [], [], VList1, _, large), % assume large, constraints not used anyway
    remove_value_conditions(Cond, VList1, Values1),
    %bug: remove value conditions placed back derivatives...
    %solution: continuity
    do_value_continuity(Values1, [], [], Values, _, large), % discard continuity constraints, we're not looking for derivative constraints or contradictions in that area
    % add equalities if equality termination is not in combination
    condition_relations_to(To, ToConditions),
    do_relation_continuity(EqConditions, EqC),
    do_relation_continuity(ToConditions, ToC),
    subtract(EqC, ToC, Stable),
    %solve Values & Results
    do_once((add_givens([par_values(Values), par_relations(Stable)|Res], Cin, _))),
    !, %solve succeeded, keep termination
    etrace(transitions_solve_context_consistent, _,ordering),
    test_a_combination(Crossproduct, Cin, VList, EqConditions, Tested).

% previous clause failed: invalid combination
test_a_combination([To/Combi|Crossproduct], Cin, VList, EqConditions, [Combi/inconsistent|Tested]):-
    To = to(cause(_), _, _, _, _),
    etrace(transitions_solve_context_inconsistent, _,ordering),
    test_a_combination(Crossproduct, Cin, VList, EqConditions, Tested).


/***______________________ CONSTRAINED CROSSPRODUCT _______________________***/

/* IDEA:
Generate normal crossproduct of single terminations,
but before merging and returning a combination, check if it satisfies the combination
constraints in the combi-table

The Combinations table has items like: [1,2]/consistent, [1]/inconsistent, [4,5]/inconsistent
some sets are subsets of others, In this case the largest fitting subset has to be found and
checked for consistency, subsets with a different outcome are ignored.

A combination is valid if no inconsistency is derived from the combinations table

Inconsistent items with no consistent superset item are considered fatal combinations.
if one is present, the combination fails immediately, without further searching.

algorithm for checking non fatal combinations:
the combinations are first sorted to length:

for each subset;
1. take the index subset, removing indices,

2. take item of combitable:
smaller of lenght then current level? inconsistent item in relevant set? fail.
subset of indices?
    yes: put on relevant-list, replacing all subset items, if no superset-item present
    no: ignore -> next

3. repeat 2 untill combitable empty

4. check if any inconsistent items in relevant-list
   if so then ignore this subset
   if not then succeed -> valid subset.

FL May 2004:
Value terminations clash with exogenous derivative terminations: (in merge-tolist)
value terminations have a new value, exogenous have the old value but a new derivative
fix by checking for exogenous termination;
if combined with valuetermination:
prepare exogenous result with a variable in the Value slot.

When making a crossproduct of a subset of terminations for use with solve,
only fatal combinations are checked, a future implementation should associate
the terminations context with each set of constraints such that at this point
also sets of constraints can be used (if not having a larger terminations context)

In the final crossproduct the index structure is discarded,
In the intermediate crossproducts the index structure is kept intact
*/


% fast combination constrained crossproduct
% checking for fatal combinations
% sorting to length, looking for contradiction in small sets first,

combination_constrained_crossproduct(ToList, CombiTableIn, Crossproduct):-
	%list_to_set(CombiTableIn, CombiTableSet),
	get_fatal_combinations_sets(CombiTableIn, CombiTableIn1, Fatal),
	sort_sets_to_length(CombiTableIn1, CombiTable),
	!,
	findall(To,
		(	a_subset(IndexedSubset, ToList),
            % if valid combination: remove index, otherwise fail: next
			combination_index_sets(IndexedSubset, CombiTable, Fatal, Subset, _),
            % valuetermination + derivative termination for exogenous par clash;
			prepare_value_merge_exogenous(Subset, PreparedSubset),
			merge_tolist(PreparedSubset, To)
		),
		Crossproduct).

% Same, but returns to()/[i,j ...] structures for use with solve/test combinations
% Furthermore, this crossproduct is used for testing small subsystems, small contexts of
% a few parameters. Using all constraints would be unfair, because some constraints
% require terminations not present in the context. Therefore only fatal combination constraints are used.
indexed_c_constrained_crossproduct(ToList, CombiTableIn, Crossproduct):-
	get_fatal_combinations_sets(CombiTableIn, _, Fatal),
	findall(To/Indices,
		(	a_subset(IndexedSubset, ToList),
			% if valid combination: remove index, otherwise fail: next
			combination_index_sets(IndexedSubset, [], Fatal, Subset, Indices),
			merge_tolist(Subset, To)
		),
		Crossproduct).


% sort sets to increasing list length
% and remove empty sets
sort_sets_to_length(CombiIn, CombiOut):-
    length_key(CombiIn, Combi1),
    keysort(Combi1, Combi2),
    remove_key_and_empty(Combi2, CombiOut).


% prefix a key with the list-length for sorting.
length_key([], []).

length_key([Set|Tail], [L-Set|NewTail]):-
    length(Set, L),
    length_key(Tail, NewTail).

% remove prefix key and remove empty sets
remove_key_and_empty([], []).

remove_key_and_empty([0-[]|Tail], NewTail):-
        remove_key_and_empty(Tail, NewTail).

remove_key_and_empty([_-Set|Tail], [Set|NewTail]):-
        remove_key_and_empty(Tail, NewTail).


% check combinations per set
combination_index_sets(IndexedSubset, CombiTableIn, Fatal, Subset, Indices):-
    take_index_subset(IndexedSubset, Indices, Subset),
    !,
    \+ includes_fatal_combination(Indices, Fatal),
    !,
    check_combitable_sets(CombiTableIn, Indices).


% seperate indices and to()
take_index_subset([], [], []).

take_index_subset([H/I/_|T], [I|IT], [H|NT]):-
    take_index_subset(T, IT, NT).


% fatal combination contained in considered indices
includes_fatal_combination(Indices, Fatal):-
    member(Combination, Fatal),
    subset(Combination, Indices),
    !.


% check_combitable_sets(Combitable, Indices)
% combitable is divided into seperate sets
% these are check in turn because information from different sources can not be mixed.
% no more constraint sets to check: safe
check_combitable_sets([], _).

% new constraintset, check if no inconsistency left..
check_combitable_sets([Set|Table], Indices):-
    check_combitable_set(Set, Indices, []),
    %check_combitable(1000, Set, Indices, [], Relevant),
    %!,
    %\+ member(_/inconsistent, Relevant),
    %!,
    check_combitable_sets(Table, Indices).


% set complete, check if no inconsistency left..
check_combitable_set([], _, Relevant):-
    memberchk(_/inconsistent, Relevant), %  inconsistent combination left
    !,
    fail.

% set complete, no inconsistency left..
check_combitable_set([], _, _).

% relevant item
check_combitable_set([List/C|Tail], Indices, Relevant):-
    subset(List, Indices),
    !,
    replace_subset_items(List/C, Relevant, NewRelevant),
    check_combitable_set(Tail, Indices, NewRelevant).

% irrelevant item
check_combitable_set([_|Tail], Indices, Relevant):-
    check_combitable_set(Tail, Indices, Relevant).


% replace_subset_items(+CurrentItem, +RelevantIn, -RelevantIn)
%
% replace all subset items in RelevantIn if present
% discard current item if superset item found
% add item if all subset items removed & no superset item present

% superset item found, done
replace_subset_items(List1/_, RelevantIn, RelevantIn):-
    member(List2/_, RelevantIn),
    subset(List1, List2),
    !.

% subset item found, remove, try again
replace_subset_items(List1/C1, RelevantIn, RelevantOut):-
    select(List2/_, RelevantIn, NewRelevant),
    subset(List2, List1),
    !,
    replace_subset_items(List1/C1, NewRelevant, RelevantOut).

% no sub- or superset-item found: add to relevant
replace_subset_items(List/C, RelevantIn, RelevantOut):-
    append([List/C], RelevantIn, RelevantOut).



% extract all combinations which have no chance of being
% superseded by a consistent superset item, because none in set
% any combination containing one of these fatal combinations is doomed.
get_fatal_combinations_sets(Table, TableOut, FatalOut):-
    get_fatal_combinations_sets2(Table, TableOut, Fatal),
    process_fatal(Fatal, FatalOut).

get_fatal_combinations_sets2([], [], []).

get_fatal_combinations_sets2([Set|Table], [SetOut|TableOut], Fatal):-
    get_fatal_combinations(Set, SetOut, Fatal1),
    get_fatal_combinations_sets2(Table, TableOut, Fatal2),
    append(Fatal1, Fatal2, Fatal).


% fatal item.
get_fatal_combinations(SetIn, SetOut, [List|Fatal]):-
	select(List/inconsistent, SetIn, NewSet),
	\+ consistent_superset_present(List, NewSet),
	!,
	get_fatal_combinations(NewSet, SetOut, Fatal).

% done, return results.
get_fatal_combinations(Set, Set, []).


% superseding item present
consistent_superset_present(List1, Combi):-
    member(List2/consistent, Combi),
    subset(List1, List2),
	!.


% remove double occurences and fatal combinations with a subset fatal combination present
process_fatal(FatalIn, FatalOut):-
    sort_lists(FatalIn, Fatal1),
    list_to_set(Fatal1, Fatal2),
    process_fatal(Fatal2, Fatal2, FatalOut).

sort_lists([], []).
sort_lists([H|T], [NH|NT]):-
    list_to_set(H, H1),
    sort(H1, NH),
    sort_lists(T, NT).

process_fatal([], _, []).

% redundant, remove.
process_fatal([Combi1|Tail], All, NewTail):-
    member(Combi2, All),
    Combi1 \= Combi2,
    subset(Combi2, Combi1), % a subset fatal item is present.
    !,
    process_fatal(Tail, All, NewTail).

% not redundant, keep.
process_fatal([Combi1|Tail], All, [Combi1|NewTail]):-
   process_fatal(Tail, All, NewTail).


% exogenous and value terminations do not merge nice because of value in valuestatement
% in result is different. value of value termination is important, derivative of exogenous
% termination is important: remove value in exogenous termination.
% see above
prepare_value_merge_exogenous(ToSet, PreparedToSet):-
    findall(To1/Par1, (member(To1, ToSet), exogenous_termination(To1, Par1)), TaggedExogenousSet),
    findall(To1, (member(To1, ToSet), exogenous_termination(To1, _)), ExogenousSet),
    subtract(ToSet, ExogenousSet, OtherSet),
    findall(Par2, (member(To2, OtherSet), value_termination(To2, Par2)), ValueSet),
    prepare_value_merge_exogenous(TaggedExogenousSet, ValueSet, NewExogenousSet),
    append(NewExogenousSet, OtherSet, PreparedToSet).

% for every exogenous termination check if Value termination present if so: adapt
prepare_value_merge_exogenous([], _, []).

% a value termination is present: make value in result into a variable
prepare_value_merge_exogenous([To/Par|Tail], ValueSet, [NewTo|NewTail]):-
    memberchk(Par, ValueSet),
    !,
    To = to(CA, CO, results(Results), TS, ST),
    take_out_value(Results, NewResults, Par),
    NewTo = to(CA, CO, results(NewResults), TS, ST),
    prepare_value_merge_exogenous(Tail, ValueSet, NewTail).

% no value termination present: keep To as is
prepare_value_merge_exogenous([To/_|Tail], ValueSet, [To|NewTail]):-
    prepare_value_merge_exogenous(Tail, ValueSet, NewTail).


% make Val in valuestatement for Par a variable
take_out_value([], [], _Par).

% par_values
take_out_value([par_values(Values)|T], [par_values(NewValues)|NT], Par):-
    !,
    select(value(Par, Q, _, Der), Values, Rest),
    NewValues = [value(Par, Q, _, Der)|Rest],
    take_out_value(T, NT, Par).

% other results
take_out_value([H|T], [H|NT], Par):-
   take_out_value(T, NT, Par).


% to is an exogenous derivative termination
exogenous_termination(to(cause(L), _, _, _, _), Par):-
    member(Cause, L),
    exogenous_terminations_set(EX, Par),
    memberchk(Cause, EX),
    !.


% to is a value termination
value_termination(to(cause(L), _, _, _, _), Par):-
    member(Cause, L),
    value_terminations_set(V, Par),
    memberchk(Cause, V),
    !.


exogenous_terminations_set([exogenous_stable_to_down(Par),
                            exogenous_stable_to_up(Par),
                            exogenous_down_to_stable(Par),
                            exogenous_up_to_stable(Par)
                            ], Par).

value_terminations_set([to_interval_above(Par),
                        to_interval_below(Par),
                        to_point_above(Par),
                        to_point_below(Par),
                        assumed_to_interval_above(Par),
                        assumed_to_interval_below(Par),
                        assumed_to_point_above(Par),
                        assumed_to_point_below(Par)
                        ], Par).



/***______________________________ EPSILON _____________________________***/
% Efirst:

/* FL okt 2011 flag now done in do_precedence predicate, same for etraces...
split_epsilon(To, [], [], [], To, Flag):-
    flag(epsilon_ordering, Flag, Flag),
    algorithm_assumption_flag(Flag, fail, epsilon_ordering).
*/
/*
split_epsilon(ToIn, ToSmallOut, ToAssumedSmall, ToAssumedLarge, ToLargeOut):-
%    flag(extra_termination_interpreter, Flag, Flag),
%algorithm_assumption_flag(Flag, fail, extra_termination_interpreter),
%    !,
    split_epsilon2(ToIn, ToSmallOut, ToAssumedSmall, ToAssumedLarge, ToLargeOut).
    %etrace(transitions_split_epsilon, [ToSmallOut, ToAssumedSmall, ToAssumedLarge, ToLargeOut], ordering),
    %etrace(transitions_epsilon_done, _, ordering).


% use old slow procedure with rules (FL ok 2011: I'm quite sure this
% option to use termination rules is NEVER used by ANYONE, (EVER!)
% kill it?-)
split_epsilon(ToIn, ToSmallOut, ToAssumedSmall, ToAssumedLarge, ToLargeOut):-
    flag(extra_termination_interpreter, Flag, Flag),
    algorithm_assumption_flag(Flag, true, extra_termination_interpreter),
    !,
    split_epsilon3(ToIn, ToSmallOut, ToAssumedSmall, ToAssumedLarge, ToLargeOut).

% Efirst
%
% This procedure was updated.
% If there are no rule based terminations, then
*/


%%	split_epsilon(+ToIn, -ToSmallOut, -ToAssumedSmall,
%	-ToAssumedLarge, -ToLargeOut)
%
% all terminations are single at this point: cause([Cause])
% So it is an efficient single memberchks and no negations.
%

% done
split_epsilon([], [], [], [], []).

% large:
split_epsilon([to(cause([Cause]), C, R, T, S)|Tail], SmallTail, AssumedSmallTail, AssumedLargeTail,
	       [to(cause([Cause]), C, R, T, S)|LargeTail]):-
	epsilon_size(Cause, large),
	!, % found large,
	split_epsilon(Tail, SmallTail, AssumedSmallTail, AssumedLargeTail, LargeTail).

% assumedsmall:
split_epsilon([to(cause([Cause]), C, R, T, S)|Tail], SmallTail,
	       [to(cause([Cause]), C, R, T, S)|AssumedSmallTail], AssumedLargeTail, LargeTail):-
	epsilon_size(Cause, assumed_small),
	!, % found assumed small
	split_epsilon(Tail, SmallTail, AssumedSmallTail, AssumedLargeTail, LargeTail).


% assumedlarge:
split_epsilon([to(cause([Cause]), C, R, T, S)|Tail], SmallTail, AssumedSmallTail,
	       [to(cause([Cause]), C, R, T, S)|AssumedLargeTail], LargeTail):-
	epsilon_size(Cause, assumed_large),
	!, % found assumed large
	split_epsilon(Tail, SmallTail, AssumedSmallTail, AssumedLargeTail, LargeTail).


% small:
split_epsilon([to(cause([Cause]), C, R, T, S)|Tail],
	       [to(cause([Cause]), C, R, T, S)|SmallTail], AssumedSmallTail, AssumedLargeTail, LargeTail):-
	epsilon_size(Cause, small),
	!,
	split_epsilon(Tail, SmallTail, AssumedSmallTail, AssumedLargeTail, LargeTail).




% old procedure for rule based possibly double terminations...
% done
split_epsilon3([], [], [], [], []).

% large: at least one large epsilon termination present.
split_epsilon3([to(cause(Causes), C, R, T, S)|Tail], SmallTail, AssumedSmallTail, AssumedLargeTail,
	       [to(cause(Causes), C, R, T, S)|LargeTail]):-
	member(Large, Causes),
	\+ epsilon_size(Large, small),
	\+ epsilon_size(Large, assumed_small),
	\+ epsilon_size(Large, assumed_large),
	!, % found large,
	split_epsilon3(Tail, SmallTail, AssumedSmallTail, AssumedLargeTail, LargeTail).

% assumedsmall:
split_epsilon3([to(cause(Causes), C, R, T, S)|Tail], SmallTail,
	       [to(cause(Causes), C, R, T, S)|AssumedSmallTail], AssumedLargeTail, LargeTail):-
	member(AS, Causes),
	epsilon_size(AS, assumed_small),
	!, % found assumed small
	split_epsilon3(Tail, SmallTail, AssumedSmallTail, AssumedLargeTail, LargeTail).


% assumedlarge:
split_epsilon3([to(cause(Causes), C, R, T, S)|Tail], SmallTail, AssumedSmallTail,
	       [to(cause(Causes), C, R, T, S)|AssumedLargeTail], LargeTail):-
	member(AS, Causes),
	epsilon_size(AS, assumed_large),
	!, % found assumed large
	split_epsilon3(Tail, SmallTail, AssumedSmallTail, AssumedLargeTail, LargeTail).


% small: all other (only small)
split_epsilon3([to(cause(Causes), C, R, T, S)|Tail],
	       [to(cause(Causes), C, R, T, S)|SmallTail], AssumedSmallTail, AssumedLargeTail, LargeTail):-
	split_epsilon3(Tail, SmallTail, AssumedSmallTail, AssumedLargeTail, LargeTail).








% define all small epsilon here
small_epsilon_set([to_interval_above(_),
		   to_interval_below(_),
		   from_equal_to_greater(_, _),
		   from_equal_to_smaller(_, _),
		   from_smaller_or_equal_to_smaller(_, _),
		   from_smaller_or_equal_to_greater(_, _), % <= to > must have been =
		   from_greater_or_equal_to_greater(_, _),
		   from_greater_or_equal_to_smaller(_, _),  % <= to > must have been =
		   derivative_stable_to_up(_),  % new FL june 07
		   derivative_stable_to_down(_) % new FL june 07
		  ]).

assumed_small_epsilon_set([
			   assumed_to_interval_above(_), % new FL okt 2011 before they had no assumed prefix...
			   assumed_to_interval_below(_), % new FL okt 2011 before they had no assumed prefix...
			   assumed_from_equal_to_greater(_, _), % new FL okt 2011 before they had no assumed prefix...
			   assumed_from_equal_to_smaller(_, _), % new FL okt 2011 before they had no assumed prefix...
			   assumed_from_smaller_or_equal_to_smaller(_, _), % new FL okt 2011 before they had no assumed prefix...
			   assumed_from_smaller_or_equal_to_greater(_, _), % <= to > must have been = % new FL okt 2011 before they had no assumed prefix...
			   assumed_from_greater_or_equal_to_greater(_, _), % new FL okt 2011 before they had no assumed prefix...
			   assumed_from_greater_or_equal_to_smaller(_, _),  % <= to > must have been = % new FL okt 2011 before they had no assumed prefix...
			   assumed_derivative_stable_to_up(_),  % new FL mar 08
			   assumed_derivative_stable_to_down(_), % new FL mar 08
			   exogenous_stable_to_up(_),
			   exogenous_stable_to_down(_)
			  ]).


large_epsilon_set([
		   to_point_above(_),
		   to_point_below(_),
		   from_greater_to_equal(_, _),
		   from_smaller_to_equal(_, _),
		   from_smaller_or_equal_to_equal(_, _),
		   from_greater_or_equal_to_equal(_, _),
		   derivative_up_to_stable(_),  % new FL mar 08
		   derivative_down_to_stable(_)%, % new FL mar 08
		  ]).


assumed_large_epsilon_set([
			   assumed_to_point_above(_), % new FL okt 2011 before they had no assumed prefix...
			   assumed_to_point_below(_), % new FL okt 2011 before they had no assumed prefix...
			   assumed_from_greater_to_equal(_, _), % new FL okt 2011 before they had no assumed prefix...
			   assumed_from_smaller_to_equal(_, _), % new FL okt 2011 before they had no assumed prefix...
			   assumed_from_smaller_or_equal_to_equal(_, _), % new FL okt 2011 before they had no assumed prefix...
			   assumed_from_greater_or_equal_to_equal(_, _), % new FL okt 2011 before they had no assumed prefix...
			   assumed_derivative_up_to_stable(_),  % new FL mar 08
			   assumed_derivative_down_to_stable(_), % new FL mar 08
			   exogenous_up_to_stable(_),
			   exogenous_down_to_stable(_)			  ]).

% test if small or large:
epsilon_size(To, small):-
    small_epsilon_set(Set),
    memberchk(To, Set),
    !.

epsilon_size(To, assumed_small):-
    assumed_small_epsilon_set(Set),
    memberchk(To, Set),
    !.

epsilon_size(To, assumed_large):-
    assumed_large_epsilon_set(Set),
    memberchk(To, Set),
    !.

epsilon_size(To, large):-
    large_epsilon_set(Set),
    memberchk(To, Set),
    !.

/* % FL nov 2011 Obsolete! Epsilon last not used anymore...
%%%%%%%%%%%%%% some epsilon procedures for epsilon last below: %%%%%%%%%
%
%
%
% policy: everything not small is large

% first check if small and large combinations are present,
% if only large or only small, then no epsilon is needed.
% Otherwise all large combinations are removed.
% FL July 2004: non-indexed epsilon is for use after crossproduct
% if any large epsilon is in a combination it is large as a whole,
% otherwise small



% check if epsilon ordering is required
do_epsilon(To, To):-
    flag(epsilon_ordering, Flag, Flag),
    algorithm_assumption_flag(Flag, fail, epsilon_ordering).

do_epsilon(ToIn, ToOut):-
    flag(epsilon_ordering, Flag, Flag),
    algorithm_assumption_flag(Flag, true, epsilon_ordering),
    etrace(transitions_epsilon_start, _, ordering),
    determine_epsilon(ToIn, To1, Smallest, Greatest),
    ( Smallest = Greatest
    ->
    ToIn = ToOut %only one category, no epsilon to be done
    ;
    remove_epsilon(To1, ToOut) %remove every large epsilon combination
    ),
    etrace(transitions_epsilon_done, _, ordering).




% for every combination check if one large epsilon termination is present,
% if so it is tagged large, otherwise it is tagged small.
% return indicators with smallest and largest category.
determine_epsilon([], [], large, small).

% large: at least one large epsilon termination present.
determine_epsilon([to(cause(Causes), C, R, T, S)|Tail],
                  [large/to(cause(Causes), C, R, T, S)|NewTail],
                  Smallest, large):-
    member(Large, Causes),
    \+ epsilon_size(Large, small),
    !, % found large,
    determine_epsilon(Tail, NewTail, Smallest, _).

% small: all other (only small)
determine_epsilon([to(cause(Causes), C, R, T, S)|Tail],
                  [small/to(cause(Causes), C, R, T, S)|NewTail],
                  small, Greatest):-
    determine_epsilon(Tail, NewTail, _, Greatest).


% remove_epsilon(+TaggedTolist, -FilteredTolist)
% for every combi, keep if small, remove if large

remove_epsilon([], []).

% small epsilon: keep To
remove_epsilon([small/To|ToIn], [To|ToOut]):-
    !,
    remove_epsilon(ToIn, ToOut).

% large epsilon: remove
remove_epsilon([large/to(cause(L), _, _, _, _)|ToIn], ToOut):-
	etrace(transitions_epsilon_remove, L, ordering),
    remove_epsilon(ToIn, ToOut).

*/


/* obsolete
% FL July 2004 Indexed version for use before crossproduct
% FL: mar 07 should be removed, it is clearly wrong in any way,
% new approach, do it first, try immediate set,
% if empty try non-immediate set.

% check if epsilon ordering is required
indexed_do_epsilon(To, To):-
    flag(epsilon_ordering, Flag, Flag),
    algorithm_assumption_flag(Flag, fail, epsilon_ordering).

indexed_do_epsilon(ToIn, ToOut):-
    flag(epsilon_ordering, Flag, Flag),
    algorithm_assumption_flag(Flag, true, epsilon_ordering),
    indexed_determine_epsilon(ToIn, To1, Smallest, Greatest),
    ( Smallest = Greatest
    ->
    ToIn = ToOut %only one category, no epsilon to be done
    ;
    indexed_remove_epsilon(To1, ToOut)).%remove every large epsilon combination


% for every combination check if one small epsilon termination is present,
% if so it is tagged small, otherwise it is tagged large.
% return indicators with smallest and largest category.
indexed_determine_epsilon([], [], large, small).

% small: at least one small epsilon termination present.
indexed_determine_epsilon([to(cause(Causes), C, R, T, S)/I/P|Tail],
                  [small/to(cause(Causes), C, R, T, S)/I/P|NewTail],
                  small, Greatest):-
    member(Small, Causes),
    epsilon_size(Small, small),
    !, % found small,
    indexed_determine_epsilon(Tail, NewTail, _, Greatest).

% large: all other (only large)
indexed_determine_epsilon([to(cause(Causes), C, R, T, S)/I/P|Tail],
                  [large/to(cause(Causes), C, R, T, S)/I/P|NewTail],
                  Smallest, large):-
    indexed_determine_epsilon(Tail, NewTail, Smallest, _).


% remove_epsilon(+TaggedTolist, -FilteredTolist)
% for every combi, keep if small, remove if large

indexed_remove_epsilon([], []).

% small epsilon: keep To
indexed_remove_epsilon([small/To/I/P|ToIn], [To/I/P|ToOut]):-
    !,
    indexed_remove_epsilon(ToIn, ToOut).

% large epsilon: remove
indexed_remove_epsilon([large/to(cause(L), _, _, _, _)/_I/_P|ToIn], ToOut):-
	etrace(transitions_epsilon_remove, L, ordering),
	indexed_remove_epsilon(ToIn, ToOut).
*/

/************************** Epsilon merges *********************************/
% immediate terminations should all happen together,
% but only if they are mergible (not contradictive) which is checked by searching
% for a termination which is the superset.
% therefore this algorithm is only a sort of softmerge, when a termination is a subset
% of another termination then it is removed
% (multiple are removed, virtually merging into the superset termination).


% check if epsilon merging is required
do_epsilon_merges(IO, IO):-
    flag(epsilon_merging, Flag, Flag),
    algorithm_assumption_flag(Flag, fail, epsilon_merging).

do_epsilon_merges(ToIn, ToOut):-
    flag(epsilon_merging, Flag, Flag),
    algorithm_assumption_flag(Flag, true, epsilon_merging),
    etrace(transitions_epsilon_merge_start, _, ordering),
    merge_epsilon(ToIn, [], ToOut),
    etrace(transitions_epsilon_merge_done, _, ordering).


% remove every immediate combination with an
% immediate superset combination in the total set

merge_epsilon([], _, []).

% immediate superset present in Used supersets: discard
merge_epsilon([to(cause(Subset), _, _, _, _)|T], Used, NT):-
    \+ Used = [],
    is_small_epsilon(to(cause(Subset), _, _, _, _)),
    member(to(cause(Superset), _, _, _, _), Used),
    % X is subset
    subset(Subset, Superset),
    % immediate superset combination X found
    is_small_epsilon(to(cause(Superset), _, _, _, _)),
    !,
    etrace(transitions_epsilon_merge_remove, Subset, ordering),
    merge_epsilon(T, Used, NT).

% immediate superset present in Tail: discard
merge_epsilon([to(cause(Subset), _, _, _, _)|T], Used, NT):-
    is_small_epsilon(to(cause(Subset), _, _, _, _)),
    member(to(cause(Superset), _, _, _, _), T),
    % X is subset
    subset(Subset, Superset),
    % X is immediate
    is_small_epsilon(to(cause(Superset), _, _, _, _)),
    % immediate superset combination X found
    !,
    etrace(transitions_epsilon_merge_remove, Subset, ordering),
    merge_epsilon(T, Used, NT).

% other: keep
merge_epsilon([H|T], Used, [H|NT]):-
    merge_epsilon(T, [H|Used], NT).

is_small_epsilon(to(cause(Causes), _, _, _, _)):-
    member(C, Causes),
    epsilon_size(C, small),
    !.


/****************************************************************************
***_________________________ CLOSE / CONTINUITY __________________________***
***                                                                       ***
****************************************************************************/

% do_value_continuity(+Values, +Constants, -NewValues, -NewRelations)
% For every value statement:
% free the derivative,
% add constraints for continuity
% skip constant derivatives
% The constant derivatives should only be used in closing, for in the ordering
% procedure instatiated derivatives give merging problems.
% NEW FL april 07:
% extra continuity constraints on derivative follow from
% second order derivative (calculated in previous state)
% SOD is a list of Par/SodValue structures, where the Sod can take
% the values: [pos, neg, zero, pos_zero, neg_zero, unknown]
%
% FL june 07
% extra continuity constraints for immediate transitions:
% derivatives cannot become stable in these transitions
% because this is a non immediate event.
%



% FL april 2012
% NB this used to assume that mixed epsilon is impossible as is
% normal with the epsilon first approach...
%
% But in the new Higher order derivatives approach with flexible assumed
% termionations 'mixed' is possible: assumed immediates can be combined
% with non immediates in which case the transition is non-immediate.
%
% epsilon types are: small, assumed_small, large, assumed_large.
%

determine_epsilon_type(Causes, Etype):-
	findall(Size, (member(C, Causes), epsilon_size(C, Size)), CauseSizes),
	list_to_set(CauseSizes, Sizes),
	flexible_simple_epsilon_size(Sizes, Etype).

% any large: large
flexible_simple_epsilon_size(Sizes, large):-
	memberchk(large, Sizes),
	!.

% any assumed_large: large
flexible_simple_epsilon_size(Sizes, large):-
	memberchk(assumed_large, Sizes),
	!.

% else: small
flexible_simple_epsilon_size(_Sizes, small).


/*
determine_epsilon_type(Causes, Etype):-
	memberchk(X, Causes),
	simple_epsilon_size(X, Etype).

simple_epsilon_size(X, small):-
	epsilon_size(X, assumed_small),
	!.

simple_epsilon_size(X, large):-
	epsilon_size(X, assumed_large),
	!.
simple_epsilon_size(X, Type):-
	epsilon_size(X, Type),
	!.
*/



% backward compatibility
do_value_continuity(V_oldNormal, CD, SOD, V_oldNormalNV, V_oldNR):-
    do_value_continuity2(V_oldNormal, CD, SOD, V_oldNormalNV, V_oldNR, large).

% normal call
do_value_continuity(V_oldNormal, CD, SOD, V_oldNormalNV, V_oldNR, Etype):-
	flag(epsilon_derivative_constraints, Flag, Flag),
	(
	  algorithm_assumption_flag(Flag, true, epsilon_derivative_constraints)
	->
	  Etype = Etype1
	;
	  Etype1 = large
	),
	do_value_continuity2(V_oldNormal, CD, SOD, V_oldNormalNV, V_oldNR, Etype1).


do_value_continuity2([], _, _, [], [], _).

% A constant derivative: keep unaltered
do_value_continuity2([value(Par, Q, Val, Der)|RestIn], Constant, SOD,
                     [value(Par, Q, Val, Der)|RestOut], RelationsOut, Etype):-
    memberchk(Par, Constant),
    !,
    do_value_continuity2(RestIn, Constant, SOD, RestOut, RelationsOut, Etype).

% An undefined derivative: (defined, then plus, zero or min), no constraints.
do_value_continuity2([value(Par, Q, Val, undefined)|RestIn], Constant, SOD,
                     [value(Par, Q, Val, _)|RestOut], RelationsOut, Etype):-
    !,
    do_value_continuity2(RestIn, Constant, SOD, RestOut, RelationsOut, Etype).

/*
    % The derivative has a value and a second order derivative.
    % THESE CONSTRAINTS NEED TO BE CHECKED AFTER THE CAUSAL MODEL WAS SEEN TO NOT HAVE CHANGED!
do_value_continuity2([value(Par, Q, Val, Der)|RestIn], Constant, SOD,
                     [value(Par, Q, Val, NewDer)|RestOut], [Constraint|RelationsOut], Etype):-
    memberchk(Der, [plus, min, zero]),
    memberchk(Par/SODvalue/multiple, SOD), % NB! only multiple influences give extra constraints.
                                           % if single influences are changing or stable their effect
					   % will change accordingly anyway (no reinforcement by 2nd order derivative needed)
    SODvalue \== unknown, % unknown gives no constraints
    !, % 2nd order derivative gives constraints
    do_2ndorder_valuecontinuity(Par, Der, SODvalue, NewDer, Constraint, Etype),
    do_value_continuity2(RestIn, Constant, SOD, RestOut, RelationsOut, Etype).
*/

% The derivative is zero, no constraints.
do_value_continuity2([value(Par, Q, Val, zero)|RestIn], Constant, SOD,
                     [value(Par, Q, Val, _)|RestOut], RelationsOut, Etype):-
    !,
    do_value_continuity2(RestIn, Constant, SOD, RestOut, RelationsOut, Etype).

% Immediate transition, The derivative is plus, in new state: greater than zero.
do_value_continuity2([value(Par, Q, Val, plus)|RestIn], Constant, SOD,
                     [value(Par, Q, Val, _)|RestOut],
                     [d_greater(Par, zero)|RelationsOut], small):-
    !,
    etrace(transitions_epsilon_continuity, [Par, plus, d_greater(Par, zero)], _),
    do_value_continuity2(RestIn, Constant, SOD, RestOut, RelationsOut, small).

% Immediate transition, The derivative is min, in new state: smaller than zero.
do_value_continuity2([value(Par, Q, Val, min)|RestIn], Constant, SOD,
                     [value(Par, Q, Val, _)|RestOut],
                     [d_smaller(Par, zero)|RelationsOut], small):-
    !,
    etrace(transitions_epsilon_continuity, [Par, min, d_smaller(Par, zero)], _),
    do_value_continuity2(RestIn, Constant, SOD, RestOut, RelationsOut, small).

% The derivative is plus, in new state: greater or equal than zero.
do_value_continuity2([value(Par, Q, Val, plus)|RestIn], Constant, SOD,
                     [value(Par, Q, Val, _)|RestOut],
                     [d_greater_or_equal(Par, zero)|RelationsOut], large):-
    !,
    do_value_continuity2(RestIn, Constant, SOD, RestOut, RelationsOut, large).

% The derivative is min, in new state: smaller or equal than zero.
do_value_continuity2([value(Par, Q, Val, min)|RestIn], Constant, SOD,
                     [value(Par, Q, Val, _)|RestOut],
                     [d_smaller_or_equal(Par, zero)|RelationsOut], large):-
    !,
    do_value_continuity2(RestIn, Constant, SOD, RestOut, RelationsOut, large).


% New FL april 2007:
% Constraints / Results Table:
%     dX   +    0    -
% ddX
%  +       >    >*   =<
% +/0      >    >=   =<
%  0       >    =    <
% -/0	  >=   =<    <
%  -      >=    <    <
%
%  NEW FL june 07:
%  a + or - dX may not become 0 in an immediate transition (Etype =
%  small)
%
%  NEW FL jan 2012:
%  a zero second order derivative (SOD) does not hold a zero first order
%  derivative (DER) from changing if the third order derivative (TOD)
%  indicates this. In other words: allow the terminations consistent
%  with kuipers postfilter.%
%



/* 1st column */

% dX: +, ddX: + t/m 0
do_2ndorder_continuity(Par, pos, SodValue, _TOD, d_greater(Par, zero)):-
	memberchk(SodValue, [pos, zero, pos_zero]),
	etrace(transitions_2ndorder_continuity, [Par, plus, SodValue, d_greater(Par, zero)], resolve),
	!.

% dX: +, ddX: -/0 or -
do_2ndorder_continuity(_Par, pos, SodValue, _TOD, nil):-
	memberchk(SodValue, [neg, neg_zero]),
	!. % no extra constraints than normal continuity


/* 2nd column */

% dX: 0, ddX: +
do_2ndorder_continuity(Par, zero, pos, _TOD, d_greater(Par, zero)):-
	etrace(transitions_2ndorder_continuity, [Par, zero, pos, d_greater(Par, zero)],resolve),

	!.

% dX: 0, ddX: +/0
do_2ndorder_continuity(Par, zero, pos_zero, _TOD, d_greater_or_equal(Par, zero)):-
	etrace(transitions_2ndorder_continuity, [Par, zero, pos_zero, d_greater_or_equal(Par, zero)],resolve),
	!.

% dX: 0, ddX: 0
% NEW FL jan 2012: two options:
% TOD is zero or unknown (constraint d_equal to zero)
% TOD is plus or neg: allow (d_greater or smaller then zero resp.)
do_2ndorder_continuity(Par, zero, zero, _TOD, d_equal(Par, zero)):- %TOD var, unknown, or zero (anything other then pos/neg)
	(
	  var(TOD)
	;
	  \+memberchk(TOD, [pos, neg])
	),
	!,
	etrace(transitions_2ndorder_continuity, [Par, zero, zero, d_equal(Par, zero)],resolve),
	!.
do_2ndorder_continuity(Par, zero, zero, pos, d_greater_or_equal(Par, zero)):-
	!,
	etrace(transitions_3rdorder_continuity, [Par, zero, zero, pos, d_greater_or_equal(Par, zero)],resolve),
	!.
do_2ndorder_continuity(Par, zero, zero, neg, d_smaller_or_equal(Par, zero)):-
	etrace(transitions_3rdorder_continuity, [Par, zero, zero, neg, d_smaller_or_equal(Par, zero)],resolve),
	!.

% dX: 0, ddX: -/0
do_2ndorder_continuity(Par, zero, neg_zero, _TOD, d_smaller_or_equal(Par, zero)):-
	etrace(transitions_2ndorder_continuity, [Par, zero, neg_zero, d_smaller_or_equal(Par, zero)],resolve),
	!.

% dX: 0, ddX: -
do_2ndorder_continuity(Par, zero, neg, _TOD, d_smaller(Par, zero)):-
	etrace(transitions_2ndorder_continuity, [Par, zero, neg, d_smaller(Par, zero)],resolve),
	!.

/* 3rd column */

% dX: -, ddX: - t/m 0
do_2ndorder_continuity(Par, neg, SodValue, _TOD, d_smaller(Par, zero)):-
	memberchk(SodValue, [neg, neg_zero, zero]),
	etrace(transitions_2ndorder_continuity, [Par, min, SodValue, d_smaller(Par, zero)],resolve),
	!.

% dX: -, ddX: +/0 or +
do_2ndorder_continuity(_Par, neg, SodValue, _TOD, nil):-
	memberchk(SodValue, [pos, pos_zero]),
	!. % no extra constraints than normal continuity -> no trace


/*
%% 1st column

% dX: +, ddX: + t/m 0
do_2ndorder_valuecontinuity(Par, plus, SodValue, plus, d_greater(Par, zero), _):-
	memberchk(SodValue, [pos, zero, pos_zero]),
	etrace(transitions_2ndorder_continuity, [Par, plus, SodValue, d_greater(Par, zero)], termination),
	%etrace(transitions_2ndorder_continuity, [Par, plus, SodValue, d_greater(Par, zero)], transition),
	!.

% dX: +, ddX: -/0 or -
% Immediate
do_2ndorder_valuecontinuity(Par, plus, SodValue, _, d_greater(Par, zero), small):-
	memberchk(SodValue, [neg, neg_zero]),
	!. % should be traced!

% dX: +, ddX: -/0 or -
do_2ndorder_valuecontinuity(Par, plus, SodValue, _, d_greater_or_equal(Par, zero), large):-
	memberchk(SodValue, [neg, neg_zero]),
	!. % no extra constraints than normal continuity -> no trace

%% 2nd column

% dX: 0, ddX: +
do_2ndorder_valuecontinuity(Par, zero, pos, plus, d_greater(Par, zero), _):-
	etrace(transitions_2ndorder_continuity, [Par, zero, pos, d_greater(Par, zero)],termination),
	%etrace(transitions_2ndorder_continuity, [Par, zero, pos, d_greater(Par, zero)],transition),
	!.

% dX: 0, ddX: +/0
do_2ndorder_valuecontinuity(Par, zero, pos_zero, _, d_greater_or_equal(Par, zero), _):-
	etrace(transitions_2ndorder_continuity, [Par, zero, pos_zero, d_greater_or_equal(Par, zero)],termination),
	%etrace(transitions_2ndorder_continuity, [Par, zero, pos_zero, d_greater_or_equal(Par, zero)],transition),
	!.

% dX: 0, ddX: 0
do_2ndorder_valuecontinuity(Par, zero, zero, zero, d_equal(Par, zero), _):-
	etrace(transitions_2ndorder_continuity, [Par, zero, zero, d_equal(Par, zero)],termination),
	%etrace(transitions_2ndorder_continuity, [Par, zero, zero, d_equal(Par, zero)],transition),
	!.

% dX: 0, ddX: -/0
do_2ndorder_valuecontinuity(Par, zero, neg_zero, _, d_smaller_or_equal(Par, zero), _):-
	etrace(transitions_2ndorder_continuity, [Par, zero, neg_zero, d_smaller_or_equal(Par, zero)],termination),
	%etrace(transitions_2ndorder_continuity, [Par, zero, neg_zero, d_smaller_or_equal(Par, zero)],transition),
	!.

% dX: 0, ddX: -
do_2ndorder_valuecontinuity(Par, zero, neg, min, d_smaller(Par, zero), _):-
	etrace(transitions_2ndorder_continuity, [Par, zero, neg, d_smaller(Par, zero)],termination),
	%etrace(transitions_2ndorder_continuity, [Par, zero, neg, d_smaller(Par, zero)],transition),
	!.

%% 3rd column

% dX: -, ddX: - t/m 0
do_2ndorder_valuecontinuity(Par, min, SodValue, min, d_smaller(Par, zero), _):-
	memberchk(SodValue, [neg, neg_zero, zero]),
	etrace(transitions_2ndorder_continuity, [Par, min, SodValue, d_smaller(Par, zero)],termination),
	%etrace(transitions_2ndorder_continuity, [Par, min, SodValue, d_smaller(Par, zero)],transition),
	!.

% dX: -, ddX: +/0 or +
% Immediate
do_2ndorder_valuecontinuity(Par, min, SodValue, _, d_smaller(Par, zero), small):-
	memberchk(SodValue, [pos, pos_zero]),
	!. % trace needed!

% dX: -, ddX: +/0 or +
% non-immediate
do_2ndorder_valuecontinuity(Par, min, SodValue, _, d_smaller_or_equal(Par, zero), large):-
	memberchk(SodValue, [pos, pos_zero]),
	!. % no extra constraints than normal continuity -> no trace

% ENDNEW FL april 07
*/



% do_relation_continuity(+Relations, -ContinuousRelations)
% For every relation statement:
% if about derivatives:
% ensure continuity (one step of freedom).
do_relation_continuity(RIn, ROut):-
	do_relation_continuity(RIn, ROut, large).

do_relation_continuity(R, R, _Etype):-
    flag(apply_continuity_d_inequalities, ACI, ACI),
    algorithm_assumption_flag(ACI, fail, apply_continuity_d_inequalities),
    !.

do_relation_continuity(RIn, ROut, _Etype):-
    flag(apply_continuity_d_inequalities, ACI, ACI),
    algorithm_assumption_flag(ACI, true, apply_continuity_d_inequalities),
    do_relation_continuity2(RIn, ROut).

do_relation_continuity2([], []).

% not about derivatives:
do_relation_continuity2([Relation|RestIn], [Relation|RestOut]):-
    Relation =.. [Rel|_],
    \+ derivative_relation_continuity(Rel, _),
    !,
    do_relation_continuity2(RestIn, RestOut).

% about derivatives, but next step = freedom -> remove relation:
do_relation_continuity2([Relation|RestIn], RestOut):-
    Relation =.. [Rel, _, _],
    derivative_relation_continuity(Rel, remove),
    !,
    do_relation_continuity2(RestIn, RestOut).

% about derivatives, weaken relation one step:
do_relation_continuity2([RelationIn|RestIn], [RelationOut|RestOut]):-
    RelationIn =.. [Rel, Left, Right],
    derivative_relation_continuity(Rel, NRel),
    NRel \= remove,
    !,
    RelationOut =.. [NRel, Left, Right],
    do_relation_continuity2(RestIn, RestOut).


% weaken relation or remove, fail if not derivative relation.
derivative_relation_continuity(d_greater, d_greater_or_equal).
derivative_relation_continuity(d_smaller, d_smaller_or_equal).
derivative_relation_continuity(d_greater_or_equal, remove).
derivative_relation_continuity(d_smaller_or_equal, remove).
derivative_relation_continuity(d_equal, remove).


%  patch in reclass april 2004:
% normal simple relations are given a continuity constraint, all others are discarded.


patch_relation_continuity([], []).

% not normal (about derivatives, correspondence etc.)-> remove relation:
patch_relation_continuity([Relation|RestIn], RestOut):-
    Relation =.. [Rel|_],
    \+ normal_relation_continuity(Rel, _),
    !,
    patch_relation_continuity(RestIn, RestOut).

% not simple (involves plus or min)-> remove relation:
patch_relation_continuity([Relation|RestIn], RestOut):-
    Relation =.. [_, Left, Right],
    (
    Left =.. [plus|_];
    Left =.. [min|_];
    Right =.. [plus|_];
    Right =.. [min|_]
    ),
    !,
    patch_relation_continuity(RestIn, RestOut).

% normal, simple, but next step = freedom -> remove relation:
patch_relation_continuity([Relation|RestIn], RestOut):-
    Relation =.. [Rel, _, _],
    normal_relation_continuity(Rel, remove),
    !,
    patch_relation_continuity(RestIn, RestOut).

% normal simple, weaken relation one step:
patch_relation_continuity([RelationIn|RestIn], [RelationOut|RestOut]):-
    RelationIn =.. [Rel, Left, Right],
    normal_relation_continuity(Rel, NRel),
    NRel \= remove,
    !,
    RelationOut =.. [NRel, Left, Right],
    patch_relation_continuity(RestIn, RestOut).


% weaken relation or remove, fail if not normal relation.
normal_relation_continuity(greater, greater_or_equal).
normal_relation_continuity(smaller, smaller_or_equal).
normal_relation_continuity(greater_or_equal, remove).
normal_relation_continuity(smaller_or_equal, remove).
normal_relation_continuity(equal, remove).
