/*  File: class
    Date: August 1989
    Author: Martin Reinders & Bert Bredeweg & Floris Linnebank
    Purpose: Specification of system model
    Part-of:  GARP (version 2.0)
    Modified: 30 July 2004

    Specification of a system model can be seen as the classification
    of a collection of system elements as instances of a collection
    of system structures.

    Copyright (c) 2004, University of Amsterdam. All rights reserved.

*/

% dynamic predicate assumption_on_level(Level, AssumptionNumber)
% records the level an assumption can be made on
% it is retracted as soon as the assumption is made or is derivable
% even if that is on a lower level

:- dynamic
           assumption_on_level/2,
	   current_assumption/2.

% ---------------------------------------------------------------------
% do_action: call Action
% if action fails after recording result sofar, backtrack and proceed
% with Continue
% purpose: 'garbage collection'
% ---------------------------------------------------------------------

do_action(Action, _ExpectedResult):-
    call(Action).

do_action(_, ExpectedResult):-
    ExpectedResult =.. [Key, Result],
    recorded_erase(Key, Result/Continue),
    !,
    do_action(Continue, ExpectedResult).

% ---------------------------------------------------------------------
% class/specify
% ---------------------------------------------------------------------

/*
% c prolog : back track do_action if after Level recursive calls
% not set to 1 upon first call of class yet

class(Cio, _):-
    once(retract(backtrack_level(Level, Old))),
    New is Old + 1,
    (New == Level ->
      asserta(backtrack_level(Level, 1))
      ;
      asserta(backtrack_level(Level, New)),
      fail
    ),
    recorda(cio_result, Cout/class(Cio, Cout)),
    !,
    fail.
*/

% swi prolog : back track do_action if running out of global stack

class(Cio, _):-
    need_global(NG),
    statistics(globallimit, G),
    statistics(globalused, U),
    Free is G - U,
    Free < NG,
    recorda(cio_result, Cout/class(Cio, Cout)),
    !,
    fail.

% swi prolog : back track do_action if running out of trail stack
class(Cio, _):-
    need_trail(NT),
    statistics(traillimit, T),
    statistics(trailused, U),
    Free is T - U,
    Free < NT,
    recorda(cio_result, Cout/class(Cio, Cout)),
    !,
    fail.

/*
 *  no uninspected se or ss, no hypotheses, no assumable ss
 */

class(Cio, Cio):-
    cio_se_ss_hy_as(Cio, [], [], [], []),
    !.

/*
 *  if the first hypothesis has parameter conditions that are givens of some
 *  hypothesis in the tail, then put this hypothesis at the end and the other
 *  in front.
 *  a bit of a hack.
 *  bug: if both have parameter conditions that are givens in the other
 *  this will go on for ever
 *
 */

class(Cin, Cout) :-
    cio_hy_nhy(Cin, Cnew, [First|Rest], NewOrder),
    system_structure_slots(First, SName, _, FirstConditions, _),
    memberchk(parameters(FirstParConditions), FirstConditions),
    FirstParConditions \= [],
    common_select(Rest, SomeHypothesis, RestRest),
    system_structure_slots(SomeHypothesis, _, _, _, SomeGivens),
    memberchk(parameters(SomeParGivens), SomeGivens),
    SomeParGivens \= [],
    member(SomeParameter, FirstParConditions),
    member(SomeParameter, SomeParGivens),
    append([SomeHypothesis | RestRest], [First], NewOrder),
    system_structure_slots(SomeHypothesis, S2Name, _, _, _),
    etrace(class_reverse_hyp_order, [SName, S2Name], specification),
    !,
    class(Cnew, Cout).

/*
 *  try to add an hypothesis
 *  1. get all parents too (if not already present)
 *  2. for all parents & hypothesis, starting with the highest parent:
 *     - check conditions (collect necessary assumptions)
 *     - add givens
 *     - if givens can not be appended:
 *   check_conditions_givens returns 'fail': class_hypothesis stops
 *
 *  class_hypothesis finishes the job: put it on assumption list if necessary,
 *  otherwise search children
 *
 *  fails: parents rejected
 *     conditions contradictory
 *
 */

class(Cin, Cout) :-
    cio_ss_oe_os_re_hy_nhy(Cin, Cnew1, SS, _OSE, OSS, Rejected, [HH|TH], NTH),
    system_structure_slots(HH, SName, Isa, _Conditions, _Results),
    etrace(class_check_hyp, SName, specification),
    append(SS, OSS, KnownSS),
    get_parents(Isa, KnownSS, Rejected, ParentStructures, TH, NTH),
    % may fail.
    append(ParentStructures, [HH], All),
    check_conditions_givens(All, Cnew1, Cnew2, Assumptions),
    (
      Assumptions = false/Assumptions1
    ->
      etrace(class_consequenses_not_ok, [SName], specification),
      (
        Assumptions1 = [] % givens contradictory, without assumptions-> inconsistent state
      ->
        etrace(class_reject_mf_conseq_step_one, SName, general),
	find_alternative_branches(Cnew2),
        etrace(class_reject_mf_conseq_step_two, SName, general),
	!, fail
      ;
        fail % givens contradictory, but with assumptions -> reject MF
      )
    ;
      true % givens not contradictory -> continue
    ),
    !,
    (
      Assumptions == []
    ->
      cio_sf_nsf(Cnew2, Cnew3, SF, NSF),
      append(SF, All, NSF)
    ;
      gensym(assumption, Nr),
      Cnew1 = Cnew3
    ),
    !,
    class_hypothesis(assumption(Nr, All, SName, Assumptions), Cnew3, Cout).

/*
 * if previous clause failed (parents rejected, conditions contradictory
 * put the hypothesis on rejected list
 *
 */

class(Cin, Cout) :-
    flag(parent_not_yet_there, yes, no),
    cio_hy_nhy_re_nre(Cin, Ncin, [H|TH], TH, TR, TR),
    system_structure_slots(H, Sname, _, _, _),
    % do not put on rejected list
    etrace(class_isa_parent_not_yet, [Sname], specification),
    !,
    class(Ncin, Cout).

class(Cin, Cout) :-
    cio_hy_nhy_re_nre(Cin, Ncin, [H|TH], TH, TR, [Sname|TR]),
    system_structure_slots(H, Sname, _, _, _),
    % remove from hypotheses, put on rejected list.
    etrace(class_reject_mf, Sname, specification),
    !,
    class(Ncin, Cout).


/*
 *  take an uninspected system element (an instance)
 *  and search all system structures which mention it as a condition
 *  if all other system element or system structure conditions are fullfilled
 *  put it on the hypothesis list
 *
 *  always succeeds
 *
 */

class(Cin, Cout):-
    cio_se_nse_ss_oe_noe_os_hy_nhy_as_re(Cin, Cnin, [H|T], T,
        SS, OSE, [H|OSE], OSS, Hy, NHy, As, Rejected),
    H = instance(Instance, ClassInstance),
    etrace(class_search_hyp_ent, [Instance, ClassInstance], specification),
    findall(
     system_structures(SName, Isa, Conditions, Givens),
     (system_structures(SName, Isa, Conditions, Givens),
      % a system structure from the library
      system_element_conditions(Conditions, SEConditions),
      % its system element conditions
      common_select(SEConditions, instance(Instance, ClassSuper), NSEConditions),
      % H is a member (but backtracking may give another selection)
      isa_instance(ClassInstance, ClassSuper),
      system_structure_conditions(Conditions, SSConditions),
      % its system structure conditions
      sub_set(SSConditions, OSS),
      % select ss conditions, backtrackable
      super_class_sub_set(NSEConditions, OSE),
      % other se conditions (including attributes), backtrackable

      % New: FL 12-2-07: fix for bug [43]
      % check ss (model fragment) isa conditions
      check_ss_isa(Isa, SS, OSS),

      is_new_system_structure(SName, SS, OSS, [H|T], As, Rejected),
      etrace(class_isa_candidate, [SName], specification)
    ),
     SSHypotheses1), % a list of possible ss. All se and ss conditions ok.
    !,
    % new; GARP 2.0, FL July 2004
    % A label can be placed in a fragment to generate model fragments for every value of the given parameter.
    generate_all_values_assumption(SSHypotheses1, SSHypotheses),
    % end new
    append(Hy, SSHypotheses, NHy),
    class(Cnin, Cout).

/*
 *  take an uninspected system structure
 *  and search all system structures which mention it as a condition
 *  if all other system element or system structure conditions are fulfilled
 *  put it on the hypothesis list
 *
 */

class(Cin, Cout):-
    cio_se_ss_nss_oe_os_nos_hy_nhy_as_re(Cin, Cnin, _, [HSS|TSS], TSS,
         OSE, OSS, [HSS|OSS], Hy, NHy, As, Rejected),
    etrace(class_search_hyp_mf, [HSS], specification),
    findall(
     system_structures(SName, Isa, Conditions, Givens),
     (system_structures(SName, Isa, Conditions, Givens),
      % a system structure from the library
      system_structure_conditions(Conditions, SSConditions),
      % its system structure conditions
      common_select(SSConditions, HSS, RSSConditions),
      % HeadSS is a member of conditions, backtrackable
      sub_set(RSSConditions, OSS),
      % all other SSConditions
      system_element_conditions(Conditions, SEConditions),
      % its system element conditions
      super_class_sub_set(SEConditions, OSE),
      % select se conditions (including attributes), backtrackable

      % New: FL 12-2-07: fix for bug [43]
      % check ss (model fragment) isa conditions
      check_ss_isa(Isa, TSS, OSS),

      is_new_system_structure(SName, [HSS|TSS], OSS, Hy, As, Rejected),
      etrace(class_isa_candidate, [SName], specification)
    ),
     SSHypotheses1), % a list of possible ss. All se and ss conditions ok.
    !,
    % new; GARP 2.0, FL July 2004
    % A label can be placed in a fragment to generate model fragments for every value of the given parameter.
    generate_all_values_assumption(SSHypotheses1, SSHypotheses),
    % end new
    append(Hy, SSHypotheses, NHy),
    class(Cnin, Cout).

/*
 * at this point all hypotheses, system structures and system elements have
 * been inspected, try to find assumable system structures that are now
 * derivable or contradictory
 *
 */

class(Cin, Cout):-
    cio_as_nas(Cin, Cnew, As, Nas),


    common_select(As, assumption(Nr, Remained, Hyp, Assumptions), Nas),

    etrace(class_re_check_ass, [Hyp, Assumptions], assumptions),
    once((   check_par_relation_conditions(Assumptions, Cnew, _, Still)
	 ;
	   true
	 )),
    (Still == [] ; var(Still) ; (etrace(class_ass_still, [Hyp], assumptions), fail)),
    % empty: no assumptions needed; variable: check failed, not empty: assumptions still needed.
    !,
    class_not_still_assumable(
        assumption(Nr, Remained, Hyp, Assumptions), Still, Cnew, Cout).


/*
 * make an assumption
 *
 * with each assumption a level is associated
 * if another assumption is contradictory on that level (see
 * class_not_still_assumable), it will be the next assumption upon backtracking
 *
 */

% fail, backtrack do_action and call make_assumption

class(Cin, _):-
    flag(reasoning_assumptions, RA, RA),
    algorithm_assumption_flag(RA, true, reasoning_assumptions),
    once(current_assumption(_, PLevel)),
    Level is PLevel + 1,
%   gensym(assumption_level, Level),
    cio_as_nas(Cin, _, As, _),
    etrace(class_need_ass, [PLevel], assumptions),
    etrace(branchepoint, assumptions), % tempflo cat?
    % note all possible assumptions on this level
    etrace(class_possible_ass, [PLevel], assumptions),
    forall(member(assumption(Nr, _, MF, RAs), As),
	   (assertz(assumption_on_level(Level, Nr)), etrace(class_list_ass, [Nr, MF, RAs], assumptions))),
    recorda(cio_result, Cout/make_assumption(Cin, Cout, _, Level)),
    !,
    fail.

class(Cin, Cin):-
    flag(reasoning_assumptions, X, X),
    algorithm_assumption_flag(X, fail, reasoning_assumptions),
    etrace(class_no_reasoning_assumptions, _, assumptions).

% ---------------------------------------------------------------------
% make assumption / 'backtrack'
% ---------------------------------------------------------------------

/*
 * make assumption with number AssumptionNumber
 * when called for the first time that number is not instantiated and
 * the first assumption is chosen
 * upon backtracking an assumption is chosen which was contradictory
 * with the first assumption (second clause). The recursive call
 * will then instantiate AssumptionNumber
 *
 * The remaining system structures are added (if that fails the
 * predicate fails)
 * class_hypothesis searches for children
 *
 */

make_assumption(Cin, Cout, AssumptionNumber, Level):-
    cio_as_nas(Cin, Cnew1, As, NAs),
    %once(common_select(As,
    %    assumption(AssumptionNumber, Remained, Hyp, Assumptions),
    %    NAs)),
    select_assumption(As, NAs, assumption(AssumptionNumber, Remained, Hyp, Assumptions), FoundOne),
  (
    FoundOne = false
  ->
    Cin = Cout
    % tracer not really needed here, people that use this preference must know...
    % and are whe sure the switch causes the empty list?
    % etrace(class_no_derivative_assumptions, [], assumptions)
  ;
    % prevent assumption from being made on a higher level
    retractall(assumption_on_level(_, AssumptionNumber)),

    %Try assumption
    etrace(class_test_ass, [Assumptions, Hyp, Level], specification),
    asserta(current_assumption(AssumptionNumber, Level)), % placing this here makes this assumption backtrackable...
    %add assumed conditions:
    once(add_par_relation_givens(Assumptions, Cnew1, Cnew2)),
    %add givens:
    (
      add_structures(Remained, Cnew2, Cnew3)
    ->
      % givens ok, continue
      true % asserta(current_assumption(AssumptionNumber, Level))

    ;
      etrace(class_consequenses_not_ok, [_SName], specification),

      %etrace(class_reject_mf, Hyp, specification),
      etrace(class_reject_mf_conseq_step_one, Hyp, general),
      find_alternative_branches(Cnew2),
      etrace(class_reject_mf_conseq_step_two, Hyp, general),

      %etrace(pathstop, specification),
      fail % givens contradictory, but with assumptions -> simply reject MF.
           % (should we try to get it in the rejected list???)
    ),

    % this is the old trace point: before add assumptions seems better... fl feb 07 tempflo
    % assumption definitive...
    % tracer([specification, assumptions], 'assumption level: %w', [Level]),
    % tracer([specification, assumptions], 'assuming %w (and parents)', [Hyp]),
    cio_sf_nsf(Cnew3, Cnew4, SF, NSF),
    append(SF, Remained, NSF),
    do_action(class_hypothesis(assumption(AssumptionNumber, [], Hyp, []),
            Cnew4, Cout), cio_result(Cout))
  ).

/*
 * get an assumption that failed on this assumption level
 * make it the next assumption
 *
 */

make_assumption(Cin, Cout, _, Level):-
    retract(current_assumption(Culprit, Level)),
    cio_as_nas(Cin, Cnew, As, NAs),
    common_select(As, assumption(Culprit, _, _, _), NAs),
    assumption_culprit(Culprit, Level, AssumptionNumber),
    % next assumption not used after another assumption on the same level
    % i.e. as a consequence of that assumption
    assumption_on_level(Level, AssumptionNumber),
    retractall(assumption_culprit(Culprit, Level, _)),
    etrace(class_backtrack, [Level], assumptions), % Tempflo: cat? specification also?
    !,
    make_assumption(Cnew, Cout, AssumptionNumber, Level).


% In case consequenses of a firing model fragment are inconsistent
% all assumable	mfs must be checked to see if alternative branches
% in the search tree exist
%
% each assumption that is contradictory with this state is
% an alternative
find_alternative_branches(Cin):-
	once(current_assumption(_, Level)), % on level 0 backtracking is not possible anyway
	Level > 0,
    cio_as_nas(Cin, Cnew, As, Nas),
    common_select(As, assumption(Nr, _Remained, Hyp, Assumptions), Nas),
    etrace(class_re_check_ass, [Hyp, Assumptions], assumptions),
    once((   check_par_relation_conditions(Assumptions, Cnew, _, _Still),
	     etrace(class_still_valid_or_assumable, [Hyp], assumptions)
	 ;
	 % check failed: mark as culprit ( = alternative)
	 etrace(class_is_alternative_branch, [Hyp], assumptions),
	 once((current_assumption(Culprit, Level),
           assertz(assumption_culprit(Culprit, Level, Nr))
           ;
           true))
	 )),
    fail.

% done
find_alternative_branches(_Cin).







/*
 *  if no assumptions have been made, but an assumption now fails
 *  use dummy level and dummy culprit
 *
 */

current_assumption(0, 0).


% %	Select Assumption, optionally filters out conditional derivatives
%
%
select_assumption(As, NAs, A, true):-
	flag(assume_conditional_derivatives, Flag, Flag),
        algorithm_assumption_flag(Flag, true, assume_conditional_derivatives),
	!,
	common_select(As, A, NAs),
	!.

select_assumption(As, NAs, assumption(AssumptionNumber, Remained, Hyp, Assumptions), true):-
	etrace(class_no_conditional_derivatives, [], assumptions),
	common_select(As,
		      assumption(AssumptionNumber, Remained, Hyp, Assumptions),
		      NAs),
	no_conditional_derivatives(assumption(AssumptionNumber, Remained, Hyp, Assumptions)),
	!.

% no assumptions without conditional derivatives...
select_assumption(As, As, _, false).


no_conditional_derivatives(assumption(_AssumptionNumber, _Remained, Hyp, Assumptions)):-
	has_derivative_assumptions(Assumptions, Dass, true),
	etrace(class_no_conditional_derivatives_MF_reject, [Dass, Hyp], assumptions),
	!,
	fail.

%no d assumptions -> continue
no_conditional_derivatives(_).



% done, return false
has_derivative_assumptions([], [], false).

%d_assumption
has_derivative_assumptions([X|T], [X|DT], true):-
	d_inequality_type(X),
	!,
	has_derivative_assumptions(T, DT, _).

% no d_assumption
has_derivative_assumptions([_|T], DT, DaFound):-
	has_derivative_assumptions(T, DT, DaFound).


% ---------------------------------------------------------------------
% class_hypothesis:
% 1. no assumptions: add hypothesis
% 2. givens contradictory: stop
% 3. assumptions necessary: put on assumption list
% ---------------------------------------------------------------------

% no assumptions for a set of system structures (parents & a hypothesis)
% find children for hypothesis and add these as hypotheses

class_hypothesis(assumption(_, _, Hyp, []), Cin, Cout):-
        % no assumptions
    etrace(class_accept_mf, Hyp, [specification, assumptions]),
    !,
    cio_ss_oe_os_hy_nhy_as_re(Cin, Cresult, SS, OSE, OSS, Rhyp, NHyp, As,
            Rejected),
    % get children
    findall(system_structures(Child, Isa, Conditions, Givens),
     (system_structures(Child, Isa, Conditions, Givens),
      isa_list(Isa, IsaList),
      member(Hyp, IsaList),
      system_structure_conditions(Conditions, SSConditions),
      % its system structure conditions
      sub_set(SSConditions, OSS),
      system_element_conditions(Conditions, SEConditions),
      % its system element conditions backtrackable
      super_class_sub_set(SEConditions, OSE),
      % select se conditions (including attributes), backtrackable
      is_new_system_structure(Child, SS, OSS, Rhyp, As, Rejected),
      etrace(class_hyp_child, [Child, Hyp], specification)
    ),
     Children),
    append(Rhyp, Children, NHyp),
    !,
    class(Cresult, Cout).

% givens for parents or hypothesis were contradictory NB FL UNUSED NOW
% this indicates an erroneous model: return empty cio structure
% which causes 'depth' to fail

class_hypothesis(assumption(_, _, Hyp, false), _, Cout):-
    etrace(class_reject_mf_conseq, Hyp, specification),
    !,
    cio_empty(Cout).

%  put an assumption structure on the assumption list

class_hypothesis(assumption(Nr, Remained, Hyp, Assumptions), Cin, Cout):-
    !,
    etrace(class_mf_needs_ass_step_two, [Hyp], assumptions),% FL or assumptions?
    cio_as_nas(Cin, Cnew, As, NAs),
    append(As, [assumption(Nr, Remained, Hyp, Assumptions)], NAs),
    % it must be last (if previous assumable ss are contradictory
    % with a current assumption, we should not think this new
    % assumption is the culprit)
    class(Cnew, Cout).

% ---------------------------------------------------------------------
% assumptions still necessary? or contradictory with other assumption?
% ---------------------------------------------------------------------

%  assumption now derivable
%  add structures:  if that fails return empty cio (third clause)
%  class_hypothesis finds children

class_not_still_assumable(assumption(Nr, Remained, Hyp, Assumptions),
                Still, Cnew, Cout) :-
    Still == [],
    % assumption is a consequence of another assumption
    % thus prevent it from being made separately
    retractall(assumption_on_level(_, Nr)),
    etrace(class_re_check_ass_der, [Hyp, Assumptions], assumptions),
    add_structures(Remained, Cnew, Cnew2),
    cio_sf_nsf(Cnew2, Cnew3, SF, NSF),
    append(SF, Remained, NSF),
    !,
    class_hypothesis(assumption(Nr, [], Hyp, []), Cnew3, Cout).

% assumptions now contradictory
% mark current assumption as culprit

% changed asserta into assertz, 15-11-89, MR
class_not_still_assumable(assumption(AssumptionNumber, _, Hyp, Assumptions),
                 Still, Cnew, Cout) :-
    var(Still), % check failed,  now contradiction !
    once((current_assumption(Culprit, Level),
           assertz(assumption_culprit(Culprit, Level, AssumptionNumber))
           ;
           true)),
    !,
    etrace(class_re_check_ass_inc, [Hyp, Assumptions], assumptions),
    class(Cnew, Cout).

% couldn't add structures for now derivable assumption

class_not_still_assumable(assumption(_, _, Hyp, _), _, _, Cout):-
    etrace(class_reject_mf_conseq, Hyp, specification),
    cio_empty(Cout), !.


% New: FL 12-2-07: fix for bug [43]
% check ss (model fragment) isa conditions, unify ss's name variables with those in parents name, backtrackable
check_ss_isa(isa(IsaList), SS, OSS):-
	append(SS, OSS, AllSS),
	isa_subset(IsaList, AllSS).


isa_subset([], _) :- !.
isa_subset([E|R], Set) :-
	%gp3 0.1: we added Garp3 names (static, process, agent, with process moving to front)
	%to head of the list of possible topnodes
	%but we left the old names as well for legacy etc
	member(E, [static,process,agent,qualitative_state, description_view, composition_view,
        decomposition_view, view, state]),
	isa_subset(R, Set).
isa_subset([E|R], Set) :-
	member(E, Set),
	isa_subset(R, Set).

% EndNew: FL 12-2-07: fix for bug [43]


% -----------------------------------------------------------------------------
% FL May 2004: NEW in GARP 2.0
%
% generate_all_values_assumption(+MFsIn, -MFsOut)
%
% if assumption of class generate_all_values is present,
% and ONE parameter is in the conditions
% generate a MF for every possible value of the parameter,
% the class assumption mechanism will be used to generate all
% branches (with all values for the parameters)
% These virtual model fragments are reproduced on every state specification
% a bit of a hack, but should work fine
% nb. ModelFragment = system structure
% -----------------------------------------------------------------------------

generate_all_values_assumption([], []).

% assumption present: generate specific MF's
generate_all_values_assumption([MF|T], MFsOut):-
    system_structure_slots(MF, N, I, C, G),
    all_values_assumption_present(C, ParInfo, PossibleV),
    !,
    generate_specific_mfs(PossibleV, ParInfo, N/I/C/G, Specific, _Names),
    generate_all_values_assumption(T, MFsT),
    append(Specific, MFsT, MFsOut).

% assumption not present: keep MF
generate_all_values_assumption([MF|T], [MF|MFsT]):-
     generate_all_values_assumption(T, MFsT).


% check if assumption asking all values is present in conditions:
% return info for relevant parameter and possible values
% FL: for now only generate_all_values, not exogenous yet
% FL: should parameter be in conditions or in givens??? Conditions (assumption mechanism works that way)
all_values_assumption_present(Conditions, Par, List):-
    memberchk(system_elements(SE), Conditions),
    member(instance(_Assumption, Class), SE),
    isa_instance(Class, generate_all_values),
    !,
    memberchk(parameters([P]), Conditions), %only one should be present!
    P =.. [_Gen, _Ent, Par, _Type, Qspace],
    quantity_space(Qspace, _, List).


% generate_specific_mfs(+Values, +ParameterInfo, +MfInfo, -SpecificMF's)
% for every value create a new MF
% N/I/C/G = Name/Isa/Conditions/Givens
generate_specific_mfs([], _, _, [], []).

% value is zero, one or minusOne
generate_specific_mfs([point(Constant)|T], Parameter, N/I/C/G, [NewMF|MFsTail], [NewN|NamesTail]):-
    memberchk(Constant, [zero, one, minusone]),
    !,
    % make a newmodelfragment name
    N =.. [Name|NameT],
    atom_concat(Name, '_', X),
    atom_concat(X, Constant, NewName),
    NewN =.. [NewName|NameT],
    % add valuestatement to conditions
    insert_value_statement(value(Parameter, _, Constant, _), C, NewC),
    % make new MF, next
    system_structure_slots(NewMF, NewN, I, NewC, G),
    generate_specific_mfs(T, Parameter, N/I/C/G, MFsTail, NamesTail).

% value is a point
generate_specific_mfs([point(Point)|T], Parameter, N/I/C/G, [NewMF|MFsTail], [NewN|NamesTail]):-
    !,
    % make a newmodelfragment name
    Point =.. [P, Parameter], % note parameter is being unified with point argument
    N =.. [Name|NameT],
    atom_concat(Name, '_', X),
    atom_concat(X, P, NewName),
    NewN =.. [NewName|NameT],
    % add valuestatement to conditions
    insert_value_statement(value(Parameter, _, Point, _), C, NewC),
    % make new MF, next
    system_structure_slots(NewMF, NewN, I, NewC, G),
    generate_specific_mfs(T, Parameter, N/I/C/G, MFsTail, NamesTail).

% value is an interval
generate_specific_mfs([Interval|T], Parameter, N/I/C/G, [NewMF|MFsTail], [NewN|NamesTail]):-
    !,
    % make a newmodelfragment name
    N =.. [Name|NameT],
    atom_concat(Name, '_', X),
    atom_concat(X, Interval, NewName),
    NewN =.. [NewName|NameT],
    % add valuestatement to conditions
    insert_value_statement(value(Parameter, _, Interval, _), C, NewC),
    % make new MF, next
    system_structure_slots(NewMF, NewN, I, NewC, G),
    generate_specific_mfs(T, Parameter, N/I/C/G, MFsTail, NamesTail).


% insert a value statement into conditions:
% possible: valuestatement present for par: unify
insert_value_statement(value(Par, _, Value, _), C, NewC):-
    memberchk(par_values(Values), C),
    memberchk(value(Par, _, _, _), Values),
    !, % valuestatement for par is present
    memberchk(value(par, _, Value, _), Values),
    C = NewC.

% insert valuestatement
insert_value_statement(Value, C, NewC):-
    select(par_values(Values), C, Rest),
    select(system_elements(SE), Rest, NR),
    select(parameters(P), NR, Tail),
    %append in correct order
    append([system_elements(SE), parameters(P), par_values([Value|Values])], Tail, NewC).


%------- end new garp 2.0 generate_all_values, FL may 2004 ---------------


% ---------------------------------------------------------------------
% test if a system structure with name Name is not already known
% i.e. on agenda of ss, already inspected, on hypotheses list
% on assumptions list or rejected
% ---------------------------------------------------------------------

is_new_system_structure(Name, SSAgenda, SSDone, Hypotheses, As, Rejected):-
    (member(Name, SSAgenda);
      member(Name, SSDone);
      member(Name, Rejected);
      member(Hypothese, Hypotheses),
      system_structure_slots(Hypothese, Name, _, _, _);
      member(assumption(_, Structures, _, _), As),
      member(A, Structures),
      system_structure_slots(A, Name, _, _, _) ),
    !,
    fail.

is_new_system_structure(_, _, _, _, _, _).

% ---------------------------------------------------------------------
% collect all parents for a system structure
% ---------------------------------------------------------------------

% no more

get_parents([], _, _, [], TH, TH).

% not a structure

get_parents([H|T], KnownSS, Rejected, R, TH, NTH):-
	%gp3 0.1: we added Garp3 names (static, process, agent, with process moving to front)
	%to head of the list of possible topnodes
	%but we left the old names as well for legacy etc
    member(H, [static,process,agent,qualitative_state, description_view, composition_view,
        decomposition_view, view, state]), !,
    etrace(class_isa_ok, [H], specification),
    get_parents(T, KnownSS, Rejected, R, TH, NTH).

% take parent off hypotheses list

get_parents([H|T], KnownSS, Rejected, [HStruct|R], OtherHypotheses,
            NRestOtherHypotheses):-
    common_select(OtherHypotheses, HStruct, RestOtherHypotheses),
    system_structure_slots(HStruct, H, Isa, _, _),
    !,
    get_parents(Isa, KnownSS, Rejected, Parents1, RestOtherHypotheses,
            NRestOtherHypotheses1),
    etrace(class_isa_uncertain, [H], specification),
    get_parents(T, KnownSS, Rejected, Parents2, NRestOtherHypotheses1,
             NRestOtherHypotheses),
    append(Parents1, Parents2, R).

% parent is already there

get_parents([H|T], KnownSS, Rejected, R, TH, NTH):-
    member(H, KnownSS), !,
    etrace(class_isa_ok_parent, [H], specification),
    get_parents(T, KnownSS, Rejected, R, TH, NTH).

% parent is known to be not applicable

get_parents([H|_], _, Rejected, _, _, _):-
    member(H, Rejected),
    !,
    etrace(class_isa_not_ok_parent, [H], specification),
    fail.

% search parent

get_parents([H|T], KnownSS, Rejected,
        [system_structures(H, Isa, Conditions, Results)|R], TH, NTH):-
    system_structures(H, Isa, Conditions, Results),
    isa_list(Isa, IsaList),
    system_structure_conditions(Conditions, SC),
    system_element_conditions(Conditions, SEC),
    % if a parent has ss or se conditions, it must be already there!
    % otherwise, don't put this child on rejected list
    (SC == [], SEC == [] ->
        true
        ;
        flag(parent_not_yet_there, _, yes),
        fail
    ),
    etrace(class_isa_uncertain_parent_found, [H], specification),
    get_parents(IsaList, KnownSS, Rejected, Parents, TH, NTH1),
    get_parents(T, KnownSS, Rejected, RP, NTH1, NTH),
    append(Parents, RP, R).

% ---------------------------------------------------------------------
% add a list of structures
% all conditions are valid or assumed
% ---------------------------------------------------------------------

add_structures([], Cin, Cin).

% add system structure name too

add_structures([H|T], Cin, Cout):-
    system_structure_slots(H, Name, _, _, Givens),
    add_givens([system_structures([Name]) | Givens], Cin, Cnew),
    add_structures(T, Cnew, Cout).

% ---------------------------------------------------------------------
% add system structure givens
% ---------------------------------------------------------------------

add_givens([], Cin, Cin).

add_givens([parameters(PList)|T], Cin, Cout):-
    (
      PList \== []
    ->
      etrace(class_add_quantities, _, specification)
    ;
      true
    ),
    add_parameter_givens(PList, Cin, Cnew),
    add_givens(T, Cnew, Cout).

add_givens([par_values(VList)|T], Cin, Cout):-
    (
      VList \== []
    ->
      etrace(class_add_values, _, specification)
    ;
      true
    ),
    add_par_value_givens(VList, Cin, Cnew),
    add_givens(T, Cnew, Cout).

add_givens([par_relations(RList)|T], Cin, Cout):-
    (
      RList \== []
    ->
      etrace(class_add_relations, _, add_relation)
    ;
      true
    ),
    add_par_relation_givens(RList, Cin, Cnew),
    add_givens(T, Cnew, Cout).

add_givens([system_elements(EList)|T], Cin, Cout):-
    cio_se_nse_oe_noe(Cin, Cnew, SE, NSE, OE, NOE),
    split_instance_attribute(EList, Instances, Attributes),
    (
      Instances \== []
    ->
      etrace(class_add_entities, Instances, specification)
    ;
      true
    ),
    (
      Attributes \== []
    ->
      etrace(class_add_conf, Attributes, specification)
    ;
      true
    ),
    append(SE, Instances, NSE),
    append(OE, Attributes, NOE),
    add_givens(T, Cnew, Cout).

add_givens([system_structures(SList)|T], Cin, Cout):-
    cio_ss_nss(Cin, Cnew, SS, NSS),
    (
      SList \== [],
      @tracer->>trace_on(specification)
    ->
      etrace(class_add_modelfragments, _, specification),
      forall(member(MF, SList), etrace(class_add_mf, MF, specification))
    ;
      true
    ),
    append(SS, SList, NSS),
    add_givens(T, Cnew, Cout).

add_parameter_givens([], Cout, Cout).

% parameter is on our list and
% qspace exists: we've been here before for this state

add_parameter_givens([H|T], Cin, Cout):-
    parameter(H, NH, Qty, Ent, Instance, _, Space),
    cio_p(Cin, P),
    member(NH, P),
    qspace(Instance, NH, _, _),
    !,
    etrace(class_add_qty_known, [Qty, Ent, Space, Instance], specification),
    add_parameter_givens(T, Cin, Cout).

% parameter is not on our list and qspace exists:
% a parameter from a previous state
% we must add qspace specific relations again
% and a value structure

add_parameter_givens([H|T], Cin, Cout):-
    parameter(H, NH, Genetic, SystemElements, Instance, _Type, Space),
    nonvar(Instance),
    qspace(Instance, NH, SpaceList, _),
    get_quantity_space(Genetic, Instance, Space, _SpaceList2, Relations1),
    % FL new mar 07: under switch control equalities to points on equal qtyspaces are added
    cio_p(Cin, P),
    equal_quantity_spaces(Instance, Space, SpaceList, P, Relations2),
    append(Relations1, Relations2, Relations),
    % FL endnew mar 07
     !,
    etrace(class_add_qty, [Genetic, SystemElements, Space, Instance], specification),
    (
      Relations \== []
    ->
      etrace(class_add_implied_rel, _, add_relation),
      % some qs have relations attached
      add_par_relation_givens(Relations, Cin, Cnew1),
      etrace(implied_nl, add_relation)
    ;
      Cin = Cnew1
    ),
    value(HV, Instance, unk/*_Qval*/, _Val, _Der),
    cio_p_np_v_nv(Cnew1, Cnew2, P, [NH|P], V, [HV|V]),
    !,
    add_parameter_givens(T, Cnew2, Cout).

% a new parameter
% a value structure must be made

add_parameter_givens([H|T], Cin, Cout):-
    % new parameter: find space, add value structure
    parameter(H, NH, Genetic, SystemElements, Instance, _Type, Space),
    var(Instance),
    % Instance must be unbound!
    gensym(Genetic, Instance),
    get_quantity_space(Genetic, Instance, Space, SpaceList, Relations1),
    assertz(qspace(Instance, NH, SpaceList, fail)),
    % FL new mar 07: under switch control equalities to points on equal qtyspaces are added
    cio_p(Cin, P),
    equal_quantity_spaces(Instance, Space, SpaceList, P, Relations2),
    append(Relations1, Relations2, Relations),
    % FL endnew mar 07
    !,
    etrace(class_add_qty, [Genetic, SystemElements, Space, Instance], specification),
    (
      Relations \== []
    ->
      etrace(class_add_implied_rel, _, add_relation),
      add_par_relation_givens(Relations, Cin, Cnew1),
      % some qs have relations attached
      etrace(implied_nl, add_relation)
    ;
      Cin = Cnew1
    ),
    value(HV, Instance, unk/*_Qval*/, _Val, _Der),
    cio_p_np_v_nv(Cnew1, Cnew2, P, [NH|P], V, [HV|V]),
    !,
    add_parameter_givens(T, Cnew2, Cout).

add_parameter_givens([H|_], _, _):-
    etrace(class_add_qty_error, [H], general), fail.

add_par_value_givens([], Cout, Cout).

add_par_value_givens([Value| T], Cin, Cout):-
    value(Value, Instance, _, Interval, Derivative),
    % if not existing value, add (occurs only when coming from previous state)
    !,
    qspace(Instance, _, Space, _),
    interval_relations(value(Instance), Interval, Space, VRelations),
    interval_relations(derivative(Instance), Derivative, derivative, DRelations),
    !,
        % get interval relations before this call because
        % we don't want to add same relations again
    append(VRelations, DRelations, Relations),
    (
      var(Derivative)
    ->
      etrace(class_add_val, [Instance, Interval], specification)
    ;
      etrace(class_add_val_der, [Instance, Interval, Derivative], specification)
    ),
    etrace(class_add_descibing_rel, _, add_relation),
    add_par_relation_givens(Relations, Cin, Cout1),
    etrace(implied_nl, add_relation),
    add_par_value_givens(T, Cout1, Cout).

% TODO: add par value for not continuous parameters

add_par_relation_givens([], Cout, Cout).

add_par_relation_givens([Rel|T], Cin, Cout):-
    influence_proportional_type(Rel),    % add influence/proportional relation
    !,
    etrace(class_add_rel, [Rel], add_relation),
    cio_ip_nip(Cin, Cnew, IP, [Rel|IP]),
    add_par_relation_givens(T, Cnew, Cout).

add_par_relation_givens([Rel|T], Cin, Cout):-
    inequality_type(Rel),
    !,
    etrace(class_add_rel, [Rel], add_relation),
    intern_representation(Rel, Intern, Cin, Cnew1),
    (
      (
        is_multiplication_division(Rel)
      ->
        do_multiplication_division(Rel, Intern, Cnew1, Cnew2) % also adds relations the way derivable_assumable_relation/3 does
      ;
        derivable_assumable_relation(Intern, Cnew1, Cnew2)
      )
    ->
      add_par_relation_givens(T, Cnew2, Cout)
    ;
      etrace(class_add_rel_fail, [Rel], add_relation),
      !,
      fail
    ).


add_par_relation_givens([Rel|T], Cin, Cout):-
    correspondence_type(Rel),
    etrace(class_add_cor, [Rel], add_relation),
    (
      add_correspondence(Rel, Cin, Cnew)
    ->
      !,
      add_par_relation_givens(T, Cnew, Cout)
    ;
      etrace(class_add_cor_fail, [Rel], add_relation),
      !,
      fail
    ).


% ---------------------------------------------------------------------
% add correspondence relation (Patch specific for GARP.1.1 -- 04.03.90 -- BB
% ---------------------------------------------------------------------

%    Value correspondence:
%    Parameter P1 has value V1 if Parameter P2 has value V2

add_correspondence(v_correspondence(P1, V1, P2, V2), Cin, Cout):-
    qspace(P1, _, S1, _),
    qspace(P2, _, S2, _),
    interval_relations(value(P1), V1, S1, R1),
    interval_relations(value(P2), V2, S2, R2),
    intern_representations(R1, I1, Cin, Cnew1),
    intern_representations(R2, I2, Cnew1, Cnew2),
    Rel = correspondence(I1, I2),
    %determine_correspondence_bitvectors(Rel, CondBV, ConseqBV),
    cio_c_nc(Cnew2, Cnew3, Corr, [Rel|Corr]),
    !,
    inspect_a_correspondence(Rel, Cnew3, Cout).

% Directed value correspondence
% Parameter P1 has value V1 if Parameter P2 has value V2

add_correspondence(dir_v_correspondence(P1, V1, P2, V2), Cin, Cout):-
    qspace(P1, _, S1, _),
    qspace(P2, _, S2, _),
    interval_relations(value(P1), V1, S1, R1),
    interval_relations(value(P2), V2, S2, R2),
    intern_representations(R1, I1, Cin, Cnew1),
    intern_representations(R2, I2, Cnew1, Cnew2),
    Rel = if_correspondence(I2, I1),
    %determine_correspondence_bitvectors(Rel, CondBV, ConseqBV),
    cio_c_nc(Cnew2, Cnew3, Corr, [Rel|Corr]),
    !,
    inspect_a_correspondence(Rel, Cnew3, Cout).

% Quantity correspondence
% P1 Relation Point iff P2 relation Point

add_correspondence(q_correspondence(P1, P2), Cin, Cout):-
    cio_c_nc_d(Cin, Cnew, Corr, NCorr, _Derivable),
    qspace(P1, _, S1, _),
    qspace(P2, _, S2, _),
    get_correspondence_relations(S1, S2, S1, S2, P1, P2, L, Cnew, Cnew2),
    %add_bitvectored_correspondences(L, NL, Corr, NCorr),
    append(L, Corr, NCorr),
    !,
    inspect_some_correspondences(L, Cnew2, Cout).
    %inspect_correspondence(Derivable, Cnew2, Cout, true).

% Directed Quantity correspondence
% P1 Relation Point if P2 relation Point

add_correspondence(dir_q_correspondence(P1, P2), Cin, Cout):-
    cio_c_nc_d(Cin, Cnew, Corr, NCorr, _Derivable),
    qspace(P1, _, S1, _),
    qspace(P2, _, S2, _),
    get_dir_correspondence_relations(S1, S2, S1, S2, P1, P2, L, Cnew, Cnew2),
    %add_bitvectored_correspondences(L, NL, Corr, NCorr),
    append(L, Corr, NCorr),
    !,
    inspect_some_correspondences(L, Cnew2, Cout).
    %inspect_correspondence(Derivable, Cnew2, Cout, true).

% The directed general implication relation

add_correspondence(if(R1, R2), Cin, Cout):-
    intern_representations(R1, IR1, Cin, Cnew1),
    intern_representations(R2, IR2, Cnew1, Cnew2),
    Rel = if_correspondence(IR1, IR2),
    %determine_correspondence_bitvectors(Rel, CondBV, ConseqBV),
    cio_c_nc(Cnew2, Cnew3, Corr, [Rel|Corr]),
    !,
    inspect_a_correspondence(Rel, Cnew3, Cout).

% The undirected general implication relation

add_correspondence(iff(R1, R2), Cin, Cout):-
    intern_representations(R1, IR1, Cin, Cnew1),
    intern_representations(R2, IR2, Cnew1, Cnew2),
    Rel = correspondence(IR1, IR2),
    %determine_correspondence_bitvectors(Rel, CondBV, ConseqBV),
    cio_c_nc(Cnew2, Cnew3, Corr, [Rel|Corr]),
    !,
    inspect_a_correspondence(Rel, Cnew3, Cout).

/*********FL 3-3-04, New correspondence primitives added:
- dv_correspondence(_, _, _, _)
- dq_correspondence(_, _)
- dir_dv_correspondence(_, _, _, _)
- dir_dq_correspondence(_, _)
- full_correspondence(_, _)
- dir_full_correspondence(_, _)
- mirror_q_correspondence(_, _)
- mirror_dq_correspondence(_, _)
- dir_mirror_q_correspondence(_, _)
- dir_mirror_dq_correspondence(_, _)

new predicates:
- get_derivative_correspondence_relations(P1, P2, List, Cin, Cout)
- get_dir_derivative_correspondence_relations(P1, P2, List, Cin, Cout)
- add_mirror_pair_correspondence(P1, P2, Pairs, Cin, Cout) (recursive list operation on Pairs)
- get_mirrored_derivative_correspondence_relations(P1, P2, L, Cnew, Cnew2)
- mirrorable_qspaces(P1, P2, [Point/Mirror|restlist])
*/


%    derivativeValue correspondence:
%    derivative of Parameter P1 has value dV1 if derivative Parameter P2 has value dV2

add_correspondence(dv_correspondence(P1, DV1, P2, DV2), Cin, Cout):-
    interval_relations(derivative(P1), DV1, derivative, R1),
    interval_relations(derivative(P2), DV2, derivative, R2),
    intern_representations(R1, I1, Cin, Cnew1),
    intern_representations(R2, I2, Cnew1, Cnew2),
    Rel = correspondence(I1, I2),
    %determine_correspondence_bitvectors(Rel, CondBV, ConseqBV),
    cio_c_nc(Cnew2, Cnew3, Corr, [Rel|Corr]),
    !,
    inspect_a_correspondence(Rel, Cnew3, Cout).

% Directed derivativeValue correspondence
% derivative of Parameter P1 has value dV1 if derivative of Parameter P2 has value dV2

add_correspondence(dir_dv_correspondence(P1, DV1, P2, DV2), Cin, Cout):-
    interval_relations(derivative(P1), DV1, derivative, R1),
    interval_relations(derivative(P2), DV2, derivative, R2),
    intern_representations(R1, I1, Cin, Cnew1),
    intern_representations(R2, I2, Cnew1, Cnew2),
    Rel = if_correspondence(I2, I1),
    %determine_correspondence_bitvectors(Rel, CondBV, ConseqBV),
    cio_c_nc(Cnew2, Cnew3, Corr, [Rel|Corr]),
    !,
    inspect_a_correspondence(Rel, Cnew3, Cout).

% derivativeQuantity correspondence
% derivativeP1 Relation Point iff derivativeP2 relation Point

add_correspondence(dq_correspondence(P1, P2), Cin, Cout):-
    cio_c_nc_d(Cin, Cnew, Corr, NCorr, _Derivable),
    get_derivative_correspondence_relations(P1, P2, L, Cnew, Cnew2),
    %add_bitvectored_correspondences(L, NL, Corr, NCorr),
    append(L, Corr, NCorr),
    !,
    inspect_some_correspondences(L, Cnew2, Cout).
    %inspect_correspondence(Derivable, Cnew2, Cout, true).

% Directed derivativeQuantity correspondence
% derivativeP1 Relation Point if derivativeP2 relation Point

add_correspondence(dir_dq_correspondence(P1, P2), Cin, Cout):-
    cio_c_nc_d(Cin, Cnew, Corr, NCorr, _Derivable),
    get_dir_derivative_correspondence_relations( P1, P2, L, Cnew, Cnew2),
    %add_bitvectored_correspondences(L, NL, Corr, NCorr),
    append(L, Corr, NCorr),
    !,
    inspect_some_correspondences(L, Cnew2, Cout).
    %inspect_correspondence(Derivable, Cnew2, Cout, true).


% full correspondence: quantity & derivative correspondence:
% P1 has value & derivative of P2 and vice versa

add_correspondence(full_correspondence(P1, P2), Cin, Cout):-
    cio_c_nc_d(Cin, Cnew1, Corr, NCorr, _Derivable),
    qspace(P1, _, S1, _),
    qspace(P2, _, S2, _),
    get_correspondence_relations(S1, S2, S1, S2, P1, P2, LV, Cnew1, Cnew2),
    get_derivative_correspondence_relations(P1, P2, LD, Cnew2, Cnew3),
    append(LV, LD, L),
    %add_bitvectored_correspondences(L, NL, Corr, NCorr),
    append(L, Corr, NCorr),
    !,
    inspect_some_correspondences(L, Cnew3, Cout).
    %inspect_correspondence(Derivable, Cnew3, Cout, true).

% directed full correspondence: quantity & derivative correspondence:
% P1 has value & derivative of if P2 has value & derivative

add_correspondence(dir_full_correspondence(P1, P2), Cin, Cout):-
    cio_c_nc_d(Cin, Cnew1, Corr, NCorr, _Derivable),
    qspace(P1, _, S1, _),
    qspace(P2, _, S2, _),
    get_dir_correspondence_relations(S1, S2, S1, S2, P1, P2, LV, Cnew1, Cnew2),
    get_dir_derivative_correspondence_relations(P1, P2, LD, Cnew2, Cnew3),
    append(LV, LD, L),
    %add_bitvectored_correspondences(L, NL, Corr, NCorr),
    append(L, Corr, NCorr),
    !,
    inspect_some_correspondences(L, Cnew3, Cout).
    %inspect_correspondence(Derivable, Cnew3, Cout, true).

% mirrored correspondence:
% P1 has upper value if P2 has lower value, etc
% symmetrical compatible quantityspace needed (identical interval/point structure)

add_correspondence(mirror_q_correspondence(P1, P2), Cin, Cout):-
    mirrorable_qspaces(P1, P2, Pairs),
    add_mirror_pair_correspondence(P1, P2, Pairs, Cin, Cout).

% derivative mirrored correspondence:
% derivativeP1  = plus when derivativeP2 = min etc.

add_correspondence(mirror_dq_correspondence(P1, P2), Cin, Cout):-
    cio_c_nc_d(Cin, Cnew, Corr, NCorr, _Derivable),
    get_mirrored_derivative_correspondence_relations(P1, P2, L, Cnew, Cnew2),
    %add_bitvectored_correspondences(L, NL, Corr, NCorr),
    append(L, Corr, NCorr),
    !,
    inspect_some_correspondences(L, Cnew2, Cout).
    %inspect_correspondence(Derivable, Cnew2, Cout, true).

% directed mirrored correspondence:
% P1 has upper value if P2 has lower value, etc
% symmetrical compatible quantityspace needed (identical interval/point structure)

add_correspondence(dir_mirror_q_correspondence(P1, P2), Cin, Cout):-
    mirrorable_qspaces(P1, P2, Pairs),
    add_dir_mirror_pair_correspondence(P1, P2, Pairs, Cin, Cout).

% directed derivative mirrored correspondence:
% derivativeP1  = plus when derivativeP2 = min etc.

add_correspondence(dir_mirror_dq_correspondence(P1, P2), Cin, Cout):-
    cio_c_nc_d(Cin, Cnew, Corr, NCorr, _Derivable),
    get_dir_mirrored_derivative_correspondence_relations(P1, P2, L, Cnew, Cnew2),
    %add_bitvectored_correspondences(L, NL, Corr, NCorr),
    append(L, Corr, NCorr),
    !,
    inspect_some_correspondences(L, Cnew2, Cout).
    %inspect_correspondence(Derivable, Cnew2, Cout, true).

% error / unkown etc.:

add_correspondence(C, _, _):-
    etrace(class_add_cor_error, [C], general), fail.


/*
% determine_correspondence_bitvectors(Rel, CondBV, ConseqBV)
%
determine_correspondence_bitvectors(correspondence(List1, List2), BV, BV):-
	determine_pointers_list(List1, _, BV1),
	determine_pointers_list(List2, _, BV2),
	map_union(BV1, BV2, BV).

determine_correspondence_bitvectors(if_correspondence(List1, List2), ConditBV, ConseqBV):-
	determine_pointers_list(List1, _, ConditBV),
	determine_pointers_list(List2, _, ConseqBV).


determine_correspondence_bitvectors_list([], []).

determine_correspondence_bitvectors_list([C|T], [C/IfBV/ThenBV|NT]):-
	determine_correspondence_bitvectors(C, IfBV, ThenBV),
	determine_correspondence_bitvectors_list(T, NT).


% add_bitvectored_correspondences(L, Corr, NCorr)
add_bitvectored_correspondences(List, CorrNew, CorrIn, CorrOut):-
	determine_correspondence_bitvectors_list(List, CorrNew),
	append(CorrNew, CorrIn, Corr),
	sort(Corr, CorrOut).
*/

get_correspondence_relations([], [], _, _, _, _, [], Cio, Cio).

% corresponding points: iff P1 rel H1 then P2 rel H2

get_correspondence_relations([point(H1)|T1], [point(H2)|T2], S1, S2,
			     P1, P2, [
				      correspondence([relation(Pi1, >, Hi1)], [relation(Pi2, >, Hi2)]),
				      correspondence([relation(Pi1, >=, Hi1)], [relation(Pi2, >=, Hi2)]),
				      correspondence([relation(CPi1, =, CHi1)], [relation(CPi2, =, CHi2)]),
				      correspondence([relation(Hi1, >, Pi1)], [relation(Hi2, >, Pi2)]),
				      correspondence([relation(Hi1, >=, Pi1)], [relation(Hi2, >=, Pi2)])
				     |CT],
			     Cin, Cout):-
    % get intern representation, greater is a dummy relation
    intern_representations([greater(P1, H1), greater(P2, H2)],
    [ relation(Pi1, _, Hi1), relation(Pi2, _, Hi2)], Cin, Cnew1),
    canonical_form(Pi1, Hi1, =, CPi1, CHi1),
    canonical_form(Pi2, Hi2, =, CPi2, CHi2),
    get_correspondence_relations(T1, T2, S1, S2, P1, P2, CT, Cnew1, Cout).

% skip values
get_correspondence_relations([V1|T1], [V2|T2], S1, S2, P1, P2,
        CT, Cin, Cout):-
    V1 \= point(_),
    V2 \= point(_),
    get_correspondence_relations(T1, T2, S1, S2, P1, P2, CT, Cin, Cout).


get_dir_correspondence_relations([], [], _, _, _, _, [], Cio, Cio).

% corresponding points:  P1 rel H1 if P2 rel H2
get_dir_correspondence_relations([point(H1)|T1], [point(H2)|T2], S1, S2,
				 P1, P2, [
					  if_correspondence([relation(Pi2, >, Hi2)], [relation(Pi1, >, Hi1)]),
					  if_correspondence([relation(Pi2, >=, Hi2)], [relation(Pi1, >=, Hi1)]),
					  if_correspondence([relation(CPi2, =, CHi2)], [relation(CPi1, =, CHi1)]),
					  if_correspondence([relation(Hi2, >, Pi2)], [relation(Hi1, >, Pi1)]),
					  if_correspondence([relation(Hi2, >=, Pi2)], [relation(Hi1, >=, Pi1)])
					 |CT],
				 Cin, Cout):-
    % get intern representation, greater is a dummy relation
    intern_representations([greater(P1, H1), greater(P2, H2)],
    [ relation(Pi1, _, Hi1), relation(Pi2, _, Hi2)], Cin, Cnew1),
    canonical_form(Pi1, Hi1, =, CPi1, CHi1),
    canonical_form(Pi2, Hi2, =, CPi2, CHi2),
    get_dir_correspondence_relations(T1, T2, S1, S2, P1, P2, CT, Cnew1, Cout).

% skip values
get_dir_correspondence_relations([V1|T1], [V2|T2], S1, S2, P1, P2,
        CT, Cin, Cout):-
    V1 \= point(_),
    V2 \= point(_),
    get_dir_correspondence_relations(T1, T2, S1, S2, P1, P2, CT, Cin, Cout).


% new FL 3-3-04: get_derivative_correspondence_relations( P1, P2, List, Cin, Cout)
% for derivative correspondences
% derivatives always have the mzp quantity space so relationset is always equal

get_derivative_correspondence_relations( P1, P2, [
						  correspondence([relation(DPi1, >, Zero)], [relation(DPi2, >, Zero)]),
						  correspondence([relation(DPi1, >=, Zero)], [relation(DPi2, >=, Zero)]),
						  correspondence([relation(Zero, =, DPi1)], [relation(Zero, =, DPi2)]),
						  correspondence([relation(Zero, >, DPi1)], [relation(Zero, >, DPi2)]),
						  correspondence([relation(Zero, >=, DPi1)], [relation(Zero, >=, DPi2)])],
					 Cin, Cout):-
    % get intern representation, greater is a dummy relation
    intern_representations([d_greater(P1, zero), d_greater(P2, zero)],
    [ relation(DPi1, _, Zero), relation(DPi2, _, Zero)], Cin, Cout).


% same for directed derivative correspondences

get_dir_derivative_correspondence_relations( P1, P2, [
						      if_correspondence([relation(DPi2, >, Zero)], [relation(DPi1, >, Zero)]),
						      if_correspondence([relation(DPi2, >=, Zero)], [relation(DPi1, >=, Zero)]),
						      if_correspondence([relation(Zero, =, DPi2)], [relation(Zero, =, DPi1)]),
						      if_correspondence([relation(Zero, >, DPi2)], [relation(Zero, >, DPi1)]),
						      if_correspondence([relation(Zero, >=, DPi2)], [relation(Zero, >=, DPi1)])],
					     Cin, Cout):-
    % get intern representation, greater is a dummy relation
    intern_representations([d_greater(P1, zero), d_greater(P2, zero)],
    [ relation(DPi1, _, Zero), relation(DPi2, _, Zero)], Cin, Cout).

% same for mirrored derivative correspondences

get_mirrored_derivative_correspondence_relations( P1, P2, [
    correspondence([relation(DPi1, >, Zero)], [relation(Zero, >, DPi2)]),
    correspondence([relation(DPi1, >=, Zero)], [relation(Zero, >=, DPi2)]),
    correspondence([relation(Zero, =, DPi1)], [relation(Zero, =, DPi2)]),
    correspondence([relation(Zero, >, DPi1)], [relation(DPi2, >, Zero)]),
    correspondence([relation(Zero, >=, DPi1)], [relation(DPi2, >=, Zero)])], Cin, Cout):-
    % get intern representation, greater is a dummy relation
    intern_representations([d_greater(P1, zero), d_greater(P2, zero)],
    [ relation(DPi1, _, Zero), relation(DPi2, _, Zero)], Cin, Cout).


% same for directed mirrored derivative correspondences

get_dir_mirrored_derivative_correspondence_relations( P1, P2, [
    if_correspondence([relation(DPi2, >, Zero)], [relation(Zero, >, DPi1)]),
    if_correspondence([relation(DPi2, >=, Zero)], [relation(Zero, >=, DPi1)]),
    if_correspondence([relation(Zero, =, DPi2)], [relation(Zero, =, DPi1)]),
    if_correspondence([relation(Zero, >, DPi2)], [relation(DPi1, >, Zero)]),
    if_correspondence([relation(Zero, >=, DPi2)], [relation(DPi1, >=, Zero)])], Cin, Cout):-
    % get intern representation, greater is a dummy relation
    intern_representations([d_greater(P2, zero), d_greater(P1, zero)],
    [ relation(DPi2, _, Zero), relation(DPi1, _, Zero)], Cin, Cout).


% FL new 3-3-04: add_mirror_pair_correspondence(+P1, +P2, +Pairs, +Cin, -Cout)

add_mirror_pair_correspondence(_, _, [], Cio, Cio).

add_mirror_pair_correspondence(P1, P2, [Point/Mirror|T], Cin, Cout):-
    qspace(P1, _, S1, _),
    qspace(P2, _, S2, _),
    interval_relations(value(P1), Point, S1, R1),
    interval_relations(value(P2), Mirror, S2, R2),
    intern_representations(R1, I1, Cin, Cnew1),
    intern_representations(R2, I2, Cnew1, Cnew2),
    Rel = correspondence(I1, I2),
    %determine_correspondence_bitvectors(Rel, CondBV, ConseqBV),
    cio_c_nc(Cnew2, Cnew3, Corr, [Rel|Corr]),
%    cio_c_nc_d(Cnew2, Cnew3, Corr, [correspondence(I1, I2)|Corr], _),
    !,
    inspect_a_correspondence(Rel, Cnew3, Cnew4),
    add_mirror_pair_correspondence(P1, P2, T, Cnew4, Cout).


% FL new 3-3-04: add_dir_mirror_pair_correspondence(+P1, +P2, +Pairs, +Cin, -Cout)

add_dir_mirror_pair_correspondence(_, _, [], Cio, Cio).

add_dir_mirror_pair_correspondence(P1, P2, [Point/Mirror|T], Cin, Cout):-
    qspace(P1, _, S1, _),
    qspace(P2, _, S2, _),
    interval_relations(value(P1), Point, S1, R1),
    interval_relations(value(P2), Mirror, S2, R2),
    intern_representations(R1, I1, Cin, Cnew1),
    intern_representations(R2, I2, Cnew1, Cnew2),
    Rel = if_correspondence(I2, I1),
    %determine_correspondence_bitvectors(Rel, CondBV, ConseqBV),
    cio_c_nc(Cnew2, Cnew3, Corr, [Rel|Corr]),
    %cio_c_nc_d(Cnew2, Cnew3, Corr, [if_correspondence(I2, I1)|Corr], _),
    !,
    inspect_a_correspondence(Rel, Cnew3, Cnew4),
    add_dir_mirror_pair_correspondence(P1, P2, T, Cnew4, Cout).


% FL 3-3-04 new:
% mirrorable_qspaces(+P1, +P2, -Pairs)
% Pairs is a list of mirrored interval or point  pairs:
% e.g.: 2x mzp qspace would render: [plus/min, zero/zero, min/plus]
% no point/interval mapping is allowed. e.g. 2x zp qspace renders: [plus/zero, zero/plus]

mirrorable_qspaces(P1, P2, Pairs):-
    qspace(P1, _, S1, _),
    qspace(P2, _, S2, _),
    %equal length
    length(S1, L),
    length(S2, L),
    %first item S1, is same type as last item S2
    add_interval_structure(S1, NS1),
    add_interval_structure(S2, NS2),
    reverse(NS2, RNS2),
    NS1 = [I1|_],
    RNS2 = [I2|_],
    I1 =.. [Type|_],
    I2 =.. [Type|_],
    !, % Valid qspaces
    add_mirror_pair(NS1, RNS2, Pairs).

mirrorable_qspaces(P1, P2, _):-
    qspace(P1, _, S1, _),
    qspace(P2, _, S2, _),
    etrace(class_inv_corr_error, [S1, S2], general),
    fail.

% add_mirror_pair(+StructuredList1, +ReversedStructuredList2, -ValuePairsList)
add_mirror_pair([], [], []).

%match 2 points
add_mirror_pair([point(H1)|T1], [point(H2)|T2], [H1/H2|PTail]):-
    !,
    add_mirror_pair(T1, T2, PTail).

%match 2 intervals
add_mirror_pair([interval(H1)|T1], [interval(H2)|T2], [H1/H2|PTail]):-
    add_mirror_pair(T1, T2, PTail).


% FL: NEW Correspondence types ENDS HERE


% ---------------------------------------------------------------------
% check a system structure's conditions and givens
% fail if conditions are contradictory
% succeed with Assumptions bound to false if givens are contradictory
% otherwise return Assumptions necessary
% ---------------------------------------------------------------------

check_conditions_givens([], Cio, Cio, []):- !.

check_conditions_givens([Struc|RStruc], Cin, Cout, Assumptions):-
    system_structure_slots(Struc, Name, _, Conditions, Givens),
    etrace(class_conditions_check, _, specification),
    check_conditions(Conditions, Cin, Cnew1, Assumptions1),
    %Now conditions are ok
    (
      Assumptions1 = []
    ->
      etrace(class_conditions_ok, [Name], specification)
    ;
      etrace(class_mf_needs_ass_step_one, [Assumptions1], assumptions)
    ),
    cio_ss_nss(Cnew1, Cnew1a, SS, NSS),
    append(SS, [Name], NSS),
    !, %New cut FL 1-3-07 This stops backtracking to traceclause below if givens or parent fails.
    etrace(class_consequenses_check, _, specification),
    (add_givens(Givens, Cnew1a, Cnew2) ->
	%Now consequenses ok
        etrace(class_consequenses_ok, [Name], specification),
        check_conditions_givens(RStruc, Cnew2, Cout, Assumptions2),
        (Assumptions2 \= false/Assumptions2A ->
        smerge(Assumptions1, Assumptions2, Assumptions)
        ;
        smerge(Assumptions1, Assumptions2A, Assumptions3), Assumptions = false/Assumptions3
        )
      ;
      Assumptions = false/Assumptions1, Cout = Cnew1a
    ),
    !.

%FL 1-3-07 New trace extra clause for failing structure because of conditions (parents / givens, does succe
check_conditions_givens([Struc|_], _, _, _):-
	@tracer->>trace_on(specification),
	system_structure_slots(Struc, Name, _, _, _),
	etrace(class_conditions_not_ok, [Name], specification),
	!,
	fail.


% ---------------------------------------------------------------------
% check system structure conditions
% ---------------------------------------------------------------------

check_conditions([parameters(PList)|TConditions], Cin, Cout, Assumptions):-
    !,
    cio_p(Cin, P),
    check_parameter_conditions(PList, P),   % simple match
    check_conditions(TConditions, Cin, Cout, Assumptions).

check_conditions([par_values(VList)|TConditions], Cin, Cout, Assumptions):-
    !,
    check_par_value_conditions(VList, Cin, Cnew, AsumpV),
    check_conditions(TConditions, Cnew, Cout, AsumpT),
    append(AsumpV, AsumpT, Assumptions).

check_conditions([par_relations(RList)|TConditions], Cin, Cout, Assumptions):-
    !,
    check_par_relation_conditions(RList, Cin, Cnew, AsumpR),
    check_conditions(TConditions, Cnew, Cout, AsumpT),
    append(AsumpR, AsumpT, Assumptions).

check_conditions([_|T], Cin, Cout, As):- check_conditions(T, Cin, Cout, As).
    % system elements / structures

check_conditions([], C, C, []).

% check_parameters ... simple match

check_parameter_conditions([], _).
check_parameter_conditions([H|T], P):-
    memberchk(H, P),
    % parameters must be sufficiently initialised (no backtracking)
    !,
    check_parameter_conditions(T, P).

check_parameter_conditions([H|T], P):-
    parameter(H, NH, _, _, _, _, _),
    memberchk(NH, P),
    % parameters must be sufficiently intitialised (no backtracking)
    check_parameter_conditions(T, P).

%FL 1-3-07 New for tracing:
check_parameter_conditions([H|_], _):-
    etrace(class_check_cond_par_not_found, [H], specification),
    !,
    fail.

check_par_value_conditions([], C, C, []).

%FL 1-3-07 New tracerversion (old below)
check_par_value_conditions([Value|R], Cin, Cout, Assumptions):-
    value(Value, Instance, _, Interval, Derivative),
    qspace(Instance, _, Space, _),
    !,
    interval_relations(value(Instance), Interval, Space, VRelations),
    interval_relations(derivative(Instance),
            Derivative, derivative, DRelations),
    !,
    append(VRelations, DRelations, Relations),
    (
      check_par_relation_conditions(Relations, Cin, Cout1, As1)
    ->
      check_par_value_conditions(R, Cout1, Cout, As2),
      append(As1, As2, Assumptions)
    ;
      %tracer: Value is	not ok
      etrace(class_val_not_ok, [Instance, Interval], specification),
       !, fail
    ).

/*FL 1-3-07 old below before tracerversion
check_par_value_conditions([Value|R], Cin, Cout, Assumptions):-
    value(Value, Instance, _, Interval, Derivative),
    qspace(Instance, _, Space, _),
    !,
    interval_relations(value(Instance), Interval, Space, VRelations),
    interval_relations(derivative(Instance),
            Derivative, derivative, DRelations),
    !,
    append(VRelations, DRelations, Relations),
    check_par_relation_conditions(Relations, Cin, Cout1, As1),
    check_par_value_conditions(R, Cout1, Cout, As2),
    append(As1, As2, Assumptions).
*/

% TODO quantitative, 'discrete'


check_par_relation_conditions([], Cin, Cin, []).

check_par_relation_conditions([Rel|T], Cin, Cout, Assumptions):-
    inequality_type(Rel),
    etrace(class_check_rel, [Rel], add_relation),
    intern_representation(Rel, Intern, Cin, Cnew1),
    derivable_assumable_relation_assump(Intern, Cnew1, Cnew2, Assump),
    !,
    check_par_relation_conditions(T, Cnew2, Cout, RAssumptions),
    if_assumption_add(Assump, Rel, RAssumptions, Assumptions).

%FL 1-3-07 New: tracer version
check_par_relation_conditions([Rel|_], _, _, _):-
      %tracer: relation is not ok
      etrace(class_invalid_rel, [Rel], add_relation),
      !, fail.


if_assumption_add([], _, RAssumptions, RAssumptions):- !.
if_assumption_add(_, Rel, RAssumptions, [Rel|RAssumptions]):- !.

% influences, proportional relations are never derivable (or assumable)

% do not append if relation is derivable

derivable_assumable_relation(Intern, Cin, Cout):-
    append_relation(Intern, Cin, Cout, _, true).

% idem but return necessary assumption

derivable_assumable_relation_assump(Intern, Cin, Cout, Assump):-
    append_relation(Intern, Cin, Cout, Assump, true).

% ---------------------------------------------------------------------
% map interval to relations
% ---------------------------------------------------------------------

% not instantiated

interval_relations(_, Interval, _, []):-
    var(Interval),
    !.

% special cases for derivative

interval_relations(derivative(Q), zero, derivative, [d_equal(Q, zero)]):- !.
interval_relations(derivative(Q), min, derivative, [d_smaller(Q, zero)]):- !.
interval_relations(derivative(Q), plus, derivative, [d_greater(Q, zero)]):- !.

% interval is a point

interval_relations(value(Q), Point, Space, [equal(Q, Point)]):-
    member(point(Point), Space),          % not between points
    !.

% interval between/less than/greater than point(s)

interval_relations(value(Q), Interval, Space, [greater(Q, SmallerPoint),
            smaller(Q, GreaterPoint)]):-
    part_list([point(SmallerPoint), Interval, point(GreaterPoint)], Space),
    % between points
    !.

interval_relations(value(Q), Interval, Space, [greater(Q, SmallerPoint)]):-
    part_list([point(SmallerPoint), Interval], Space),
    % greater than point
    !.

interval_relations(value(Q), Interval, Space, [smaller(Q, GreaterPoint)]):-
    part_list([Interval, point(GreaterPoint)], Space),
    % smaller than point
    !.

% quantity space consists of a single interval
interval_relations(value(_), Interval, [Interval], []):-
    Interval \= point(_),
    !.

% no relations for value
interval_relations(Q, I, S, _):-
    etrace(class_add_val_error, [Q, I, S], general),
    fail.

% ---------------------------------------------------------------------
% split a list of system elements into instances and other relations
% (we only use instances when searching for applicable system structures)
% ---------------------------------------------------------------------

split_instance_attribute([], [], []).

split_instance_attribute([instance(I, G)|Tail], [instance(I, G)|RI], RA) :-
    create_instance(instance(I, G)),
    split_instance_attribute(Tail, RI, RA),
    !.

split_instance_attribute([Attribute|Tail], RI, [Attribute|RA]) :-
    split_instance_attribute(Tail, RI, RA),
    !.

% create new instance

create_instance(instance(I, G)):- var(I), nonvar(G), !, gensym(G, I).
create_instance(instance(_, _)).

% ---------------------------------------------------------------------
% clean up
% ---------------------------------------------------------------------

clean_up_depth :-
    flag(q_cnt, _, 0),
    retractall(assumption_culprit(_, _, _)),
    retractall(current_assumption(_, _)),
    asserta(current_assumption(0, 0)).

% ---------------------------------------------------------------------
% map inequality relations to interval values (if possible)
% ---------------------------------------------------------------------

get_derivable_values(Cin, Cout):-
    cio_p_v_q_d(Cin, Parameters, Values, _, Derivable),
    get_derivable_values(Values, Parameters, Cin, Cout, Derivable),
    !.

get_derivable_values([], _, Cio, Cio, _).

get_derivable_values([  value(Instance, _, Interval, Derivative) | Tail ],
            Parameters, Cin, Cout, Derivable) :-
    qspace(Instance, _, Space, _),
    get_derivable_value(value(Instance), Interval, Space, Cin, Cnew1,
            Derivable),
    get_derivable_value(derivative(Instance), Derivative, derivative,
            Cnew1, Cnew2, Derivable),
    get_derivable_values(Tail, Parameters, Cnew2, Cout, Derivable).

get_derivable_value(Which, Value, Space, Cin, Cout, Derivable):-
    var(Value),
    once((  relations_interval(Which, Value, Space, Relations),
        are_derivable(Relations, Cin, Cout, Derivable)
        )),
    !.

% if a derivative is still not known: set it to zero
% in rare cases this may be misleading, e.g. if
%   derivative(a) > derivative(b)
% and both are unknown. See also resolve influence/proportional

/* problem: can't do it if get_derivable_values is called just before
   searching for existing states. Seems to be superfluous anyway
*/

% get_derivable_value(derivative(_), zero, _, Cio, Cio, _):- !.

get_derivable_value(_, _, _, Cio, Cio, _).


% special cases for derivative

relations_interval(derivative(Q), zero, derivative, [d_equal(Q, zero)]).
relations_interval(derivative(Q), min, derivative, [d_smaller(Q, zero)]).
relations_interval(derivative(Q), plus, derivative, [d_greater(Q, zero)]).

% interval is a point

relations_interval(value(Q), Point, Space, [equal(Q, Point)]):-
    member(point(Point), Space).

% interval between/less than/greater than point(s)

relations_interval(value(Q), Interval, Space, [greater(Q, SmallerPoint),
                smaller(Q, GreaterPoint)]):-
    three_in_list([point(SmallerPoint), Interval, point(GreaterPoint)],
                Space).
    % between points

relations_interval(value(Q), Interval, Space, [greater(Q, SmallerPoint)]):-
    two_last_in_list([point(SmallerPoint), Interval], Space).


relations_interval(value(Q), Interval, [Interval, point(GreaterPoint)|_],
            [smaller(Q, GreaterPoint)]).

relations_interval(value(_), Interval, [Interval], []):-
    Interval \= point(_).


% some special purpose listoperations

three_in_list([A, B, C], [A, B, C|_]).
three_in_list(L, [_|T]):- three_in_list(L, T).

two_last_in_list([A, B], [A, B]).
two_last_in_list(L, [_|T]):- two_last_in_list(L, T).

% ---------------------------------------------------------------------
% inequality relations (normal representation) are derivable ?
% ---------------------------------------------------------------------

are_derivable([], Cio, Cio, _).
are_derivable([H|T], Cin, Cout, Derivable):-
    intern_representation(H, Intern, Cin, Cnew),
    is_derivable(Intern, Derivable),
    are_derivable(T, Cnew, Cout, Derivable).

% ---------------------------------------------------------------------
% when all system structures are found, collect from those structures
% the parameters, values, relations and system elements.
% ---------------------------------------------------------------------

collect_smd_elements_from_system_structures([], [], [], [], []).
collect_smd_elements_from_system_structures([H|T], SE, P, V, R):-
    collect_smd_elements_from_system_structures(T, OSE, OP, OV, OR),
    system_structure_slots(H, _, _, Conditions, Givens),
    append(Conditions, Givens, All),
    collect_smd_elements(All, OSE, OP, OV, OR, SE, P, V, R),
    !.


collect_smd_elements([], SE, P, V, R, SE, P, V, R).

collect_smd_elements([system_elements(FSE)|T], SE, P, V, R, NSE, NP, NV, NR):-
    smerge(FSE, SE, TSE),
    collect_smd_elements(T, TSE, P, V, R, NSE, NP, NV, NR).

collect_smd_elements([parameters(FP)|T], SE, P, V, R, NSE, NP, NV, NR):-
    smerge(FP, P, TP),
    collect_smd_elements(T, SE, TP, V, R, NSE, NP, NV, NR).

collect_smd_elements([par_values(FV)|T], SE, P, V, R, NSE, NP, NV, NR):-
    smerge(FV, V, TV),
    collect_smd_elements(T, SE, P, TV, R, NSE, NP, NV, NR).

collect_smd_elements([par_relations(FR)|T], SE, P, V, R, NSE, NP, NV, NR):-
    smerge(FR, R, TR),
    collect_smd_elements(T, SE, P, V, TR, NSE, NP, NV, NR).

collect_smd_elements([_|T], SE, P, V, R, NSE, NP, NV, NR):-
    collect_smd_elements(T, SE, P, V, R, NSE, NP, NV, NR).

collect_corresponding_values([], _, []).
collect_corresponding_values([P|PT], Vs, [value(Instance, Q, N, D)|VT]):-
    parameter(P, _Match, _Generic, _SystemE, Instance, _Type, _Space),
    memberchk(value(Instance, Q, N, D), Vs),
    collect_corresponding_values(PT, Vs, VT).
