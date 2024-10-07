%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  file: engine_content_api.pl
%  author: Floris Linnebank
%  date: Oktober 2010
%  purpose: top level interface simulation content in the engine
%
%  copyright (c) UvA 2010
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%	epsilon_state_type(+StateNr, -EpsilonType)
%
%	returns the epsilon type of a state, depending on transitions
%	being present or possible or not and their type:
%	(assumed)(non-)immediate.
%
%	Types:
%	- timePointState:
%	      immediate terminations present
%
%	- timeAssumedPointState:
%	      only assumed immediate terminations present
%
%	- timeIntervalState
%	      no immediate terminations present
%	      (assumed immediate is not enough to insure zero time
%	      length! (ok?))
%
%       - timeIntervalEndState:
%             terminated or ordered or closed and no terminations
%	      (NB: this is not completely failsafe! ordering could
%	      remove terminations making this a fake endstate...)
%	      %Todo: record original terminations upon termination, and use these
%
%	- timePointDeadEndState:
%	      immediate terminations present
%	      + closed and no successors
%
%	- timeAssumedPointDeadEndState:
%	      only assumed immediate terminations present
%	      + closed and no successors
%
%	- timeIntervalDeadEndState
%	      non-immediate terminations present
%	      + closed and no successors
%
%	- timeUnknownStateEpsilonOff:
%	      epsilon is off
%
%	- timeUnknownStateSimulationNotComplete:
%	      transitions not yet determined for state:
%	      interpreted state
%
%
%	simulation must be present
%
epsilon_state_type(StateNr, EpsilonType):-
	\+ var(StateNr),
	state_status(StateNr, Status),
	state_to(StateNr, Tolist), %todo: record and use original To's from termination
	!, %valid statenr
	findall(Causes, member(	to(
					   cause(Causes),
					   conditions(_),
					   results(_),
					   to_state(_StateList),
					   _),
					Tolist),
		CausesLists),
	flatten(CausesLists, AllCauses),
	findall(StateList, member(	to(
					   cause(_Causes),
					   conditions(_),
					   results(_),
					   to_state(StateList),
					   _),
					Tolist),
		StateLists),
	flatten(StateLists, AllSuccessors),
	epsilon_type_of_state(Status, AllCauses, AllSuccessors, EpsilonType).

epsilon_state_type(StateNr, _EpsilonType):-
	writef('\n***Warning: invalid state number in epsilontype request: %w ***\n', [StateNr]),
	fail.

/*
Status:
	interpreted
	terminated
	ordered
	closed

Causes, see:
        small_epsilon_set
	assumed_small_epsilon_set
	assumed_large_epsilon_set
	large_epsilon_set
*/

% epsilon ordering is off!
epsilon_type_of_state(_, _, _, timeUnknownStateEpsilonOff):-
	flag(epsilon_ordering, F, F),
	F = fail,
	!.

% transitions not present so info not sufficient to determine type:
epsilon_type_of_state(interpreted, _AllCauses, _AllSuccessors, timeUnknownStateSimulationNotComplete):-
	!.

%transitions not present, but generated: endstate
epsilon_type_of_state(terminated, [], [], timeIntervalEndState):-
	!.

% transitions present and sufficient to determine type:
% no transitions present
epsilon_type_of_state(terminated, AllCauses, _AllSuccessors, EpsilonType):-
	!,
	small_epsilon_set(Small),
	assumed_small_epsilon_set(AssumedSmall),
	(
	  member(C, AllCauses),
	  memberchk(C, Small),
	  !
	->
	  EpsilonType = timePointState
	;
	  (
	    forall(member(C, AllCauses),
		   memberchk(C, AssumedSmall)),
	    !
	  ->
	    EpsilonType = timeAssumedPointState
	  ;
	    EpsilonType = timeIntervalState
	  )
	),
	!.

%transitions not present, but generated: endstate
epsilon_type_of_state(ordered, [], [], timeIntervalEndState):-
	!.

% transitions present, sufficient to determine type:
epsilon_type_of_state(ordered, AllCauses, _AllSuccessors, EpsilonType):-
	!,
	small_epsilon_set(Small),
	assumed_small_epsilon_set(AssumedSmall),
	(
	  member(C, AllCauses),
	  memberchk(C, Small),
	  !
	->
	  EpsilonType = timePointState
	;
	  (
	    forall(member(C, AllCauses),
		   memberchk(C, AssumedSmall)),
	    !
	  ->
	    EpsilonType = timeAssumedPointState
	  ;
	    EpsilonType = timeIntervalState
	  )
	),
	!.


%transitions not present, but generated: endstate
epsilon_type_of_state(closed, [], [], timeIntervalEndState):-
	!.

% transitions present, sufficient to determine type:
epsilon_type_of_state(closed, AllCauses, AllSuccessors, EpsilonType):-
	!,
	small_epsilon_set(Small),
	assumed_small_epsilon_set(AssumedSmall),
	(
	  AllSuccessors = []
	->
	  (
	    member(C, AllCauses),
	    memberchk(C, Small),
	    !
	  ->
	    EpsilonType = timePointDeadEndState
	  ;
	    (
	      forall(member(C, AllCauses),
		     memberchk(C, AssumedSmall)),
	      !
	    ->
	      EpsilonType = timeAssumedPointDeadEndState
	    ;
	      EpsilonType = timeIntervalDeadEndState
	    )
	  )
	;
	  (
	    member(C, AllCauses),
	    memberchk(C, Small),
	    !
	  ->
	    EpsilonType = timePointState
	  ;
	    (
	      forall(member(C, AllCauses),
		     memberchk(C, AssumedSmall)),
	      !
	    ->
	      EpsilonType = timeAssumedPointState
	    ;
	      EpsilonType = timeIntervalState
	    )
	  )
	),
	!.


epsilon_type_of_state(_Status, _Tolist, _AllSuccessors, _EpsilonType):-
	writef('\n***Warning: problem in determining epsilon type of state ***\n'),
	fail.

%%	get_quantity_instances_for_state_entity(+StateNR, +Entity, -QuantityList)
%
%	returns the set of quantity instances associated with an
%	entity instance in a particular state
%
%	simulation must be present
%
get_quantity_instances_for_state_entity(State, Entity, QuantityList):-
	state(State, SMD),
	SMD =.. [smd, _IS, _SE, parameters(PL), _PV, _PR, _SS, _SCE],
	findall(Quantity, (member(P, PL),
			   P =.. [_Gen, Entity, Quantity, _, _QS]
			   ), QuantityList).


%%	get_all_simulation_state_numbers(-StateNumberList)
%
%	returns set of all state numbers in simulation
%
%	simulation must be present
%
get_all_simulation_state_numbers(List):-
	findall(Nr, state_values(Nr, _), List).
