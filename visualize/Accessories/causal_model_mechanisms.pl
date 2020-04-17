% causal_model_mechanisms.pl 
%
% This file contains mechanisms to deal with the details 
% of causal models, i.e., whether the different causal 
% relations actually have an effect or not.
%
% Written by Anders Bouwer, 8 Jan 2002
%
% Last updated 30 June 2003


% effect_status(N, CausalParRel, Direction, Status)
%
% CausalParRel = causal_relation(X, Rel, Y)
% this predicate finds out whether the CausalParRel 
% in state N actually has an effect or not (Status), 
% and in which direction the affected parameter would 
% change if this was the only effect.
%

% X has no effect on Y because X is zero or unknown (in the case of 
% X influencing Y), or X is steady or dX is unknown (in the case of 
% a proportional relationship).
%
% this clause is put first, because it takes the least amount 
% of time to check
%
effect_status(N, causal_relation(X, Rel, Y), none, none):-
	show_causal_model(N, Rel, X, Y, second),
        expected_effect(N, causal_relation(X, Rel, Y), none). 

% X has an effect on Y (as expected)
%
effect_status(N, causal_relation(X, Rel, Y), Direction, effect):-
	show_causal_model(N, Rel, X, Y, second),
        expected_effect(N, causal_relation(X, Rel, Y), Direction),
        Direction \== none, 
        actual_effect(N, Y, Direction).

% X has a submissive effect on Y (Y changes in the other direction)
%
effect_status(N, causal_relation(X, Rel, Y), Direction, submissive):-
	show_causal_model(N, Rel, X, Y, second),
        expected_effect(N, causal_relation(X, Rel, Y), Direction), 
        actual_effect(N, Y, Direction2), 
        reverse_direction(Direction, Direction2).

% X has an effect on Y but this is counterbalanced 
% and consequently Y does not change at all
%
effect_status(N, causal_relation(X, Rel, Y), Direction, balanced):-
	show_causal_model(N, Rel, X, Y, second),
        expected_effect(N, causal_relation(X, Rel, Y), Direction), 
        Direction \== none, 
        actual_effect(N, Y, none). 



% correspondence relationships 
% N.B. Arguments of correspondences are switched 
% with respect to the GARP representation!
%
% effect status of correspondence relationships

% A correspondence relation (of any kind) is active when 
% it can be used in a calculation
%
% This version uses the same visualization format as 
% gen_causal_rel etc.
%
corr_effect_status(N, corr_relation(N, VFrom, VRel, VTo, Dir), active):-
    index_pred_state(N, par_relations, CorrRel), 
    (
     % q_corr
     CorrRel = q_correspondence(Q1, Q2),
     contains_corr_calculation(N, corr_calculation, 
		   corr_calc(N, Q2, V2, Rel, Q1, V1)),
     visualize(Rel, Q1, Q2, VRel, VFrom, VTo, Dir)
     ;
     % dir_q_corr
     CorrRel = dir_q_correspondence(Q1, Q2),
     contains_corr_calculation(N, corr_calculation, 
		   corr_calc(N, Q2, V2, Rel, Q1, V1)),
     visualize(Rel, Q1, Q2, VRel, VFrom, VTo, Dir)
     ;
     % v_corr
     CorrRel = v_correspondence(Q1, V1, Q2, V2),
     contains_corr_calculation(N, corr_calculation, 
		   corr_calc(N, Q2, V2, Rel, Q1, V1)),
     % strip off quantity name, if necessary
     strip_atomize(V1, VA1), 
     strip_atomize(V2, VA2),      
     From =.. [VA1, Q1],
     To =.. [VA2, Q2],
     visualize(Rel, From, To, VRel, VFrom, VTo, Dir)
     ;
     % dir_v_corr
     CorrRel = dir_v_correspondence(Q1, V1, Q2, V2),
     contains_corr_calculation(N, corr_calculation, 
		   corr_calc(N, Q2, V2, Rel, Q1, V1)),
     % strip off quantity name, if necessary
     strip_atomize(V1, VA1), 
     strip_atomize(V2, VA2),      
     From =.. [VA1, Q1],
     To =.. [VA2, Q2],
     visualize(Rel, From, To, VRel, VFrom, VTo, Dir)
    ).



% A correspondence relation (of any kind) is inactive when 
% it is not active
%
corr_effect_status(N, corr_relation(N, VFrom, VRel, VTo, Dir), inactive):-
    index_pred_state(N, par_relations, CorrRel), 
    (
     % q_corr
     CorrRel = q_correspondence(Q1, Q2),
     not(contains_corr_calculation(N, corr_calculation, 
		   corr_calc(N, Q2, V2, Rel, Q1, V1))),
     visualize(q_correspondence, Q1, Q2, VRel, VFrom, VTo, Dir)
     ;
     % dir_q_corr
     CorrRel = dir_q_correspondence(Q1, Q2),
     not(contains_corr_calculation(N, corr_calculation, 
		   corr_calc(N, Q2, V2, Rel, Q1, V1))),
     visualize(dir_q_correspondence, Q1, Q2, VRel, VFrom, VTo, Dir)
     ;
     % v_corr
     CorrRel = v_correspondence(Q1, V1, Q2, V2),
     not(contains_corr_calculation(N, corr_calculation, 
		   corr_calc(N, Q2, V2, Rel, Q1, V1))),
     % strip off quantity name, if necessary
     strip_atomize(V1, VA1), 
     strip_atomize(V2, VA2),      
     From =.. [VA1, Q1],
     To =.. [VA2, Q2],
     visualize(v_correspondence, From, To, VRel, VFrom, VTo, Dir)
     ;
     % dir_v_corr
     CorrRel = dir_v_correspondence(Q1, V1, Q2, V2),
     not(contains_corr_calculation(N, corr_calculation, 
		   corr_calc(N, Q2, V2, Rel, Q1, V1))),
     % strip off quantity name, if necessary
     strip_atomize(V1, VA1), 
     strip_atomize(V2, VA2),      
     From =.. [VA1, Q1],
     To =.. [VA2, Q2],
     visualize(dir_v_correspondence, From, To, VRel, VFrom, VTo, Dir)
    ).




% A correspondence relation (of any kind) is active when 
% it can be used in a calculation
%
% This version has separate parameters for the values
%
corr_effect_status(N, corr_relation(N, Q1, V1, Rel, Q2, V2), active):-
    contains_corr_calculation(N, corr_calculation, 
	   corr_calc(N, Q1, V1, Rel, Q2, V2)).



% A correspondence relation (of any kind) is inactive when 
% it is not active
%
corr_effect_status(N, corr_relation(N, Q1, V1, Rel, Q2, V2), inactive):-
    index_pred_state(N, par_relations, CorrRel), 
    (
     CorrRel = q_correspondence(Q2, Q1),
     Rel = q_correspondence
     ;
     CorrRel = q_correspondence(Q1, Q2),
     Rel = q_corr_rev
     ;
     CorrRel = dir_q_correspondence(Q2, Q1),
     Rel = dir_q_correspondence
     ;
     CorrRel = v_correspondence(Q2, V2, Q1, V1),
     Rel = v_correspondence
     ;
     CorrRel = v_correspondence(Q1, V1, Q2, V2),
     Rel = v_corr_rev
     ;
     CorrRel = dir_v_correspondence(Q2, V2, Q1, V1),
     Rel = dir_v_correspondence
    ),
    not(contains_corr_calculation(N, corr_calculation, 
	   corr_calc(N, Q1, V1, Rel, Q2, V2))).




reverse_direction(pos, neg).
reverse_direction(neg, pos).


% Note that the different clauses of this predicate 
% partly give the same result, so duplicates may occur!
%
positive_value(N, X):-
        index_pred_state(N, par_relations, greater(X, zero)),!.

% if the above par_relation is not present, we have to check 
% it by investigating the value in relation to its quantity space
%
positive_value(N, X):-
       index_pred_state(N, par_values, value(X, _, Value, _Der)),!,
       nonvar(Value),
       engine:qspace(X, _Predicate, QSpaceList, _Fail), 
       qspace_member(Value, QSpaceList, Index),
       qspace_member(zero, QSpaceList, ZeroIndex), 
       Index > ZeroIndex,!.

% if the value is plus, we can also say it's positive, 
% also if zero is not included in the qspace
%
positive_value(N, X):-
       index_pred_state(N, par_values, value(X, _, QV, _Der)),!,
       QV == plus.


% Note that the different clauses of this predicate 
% partly give the same result, so duplicates may occur!
%
negative_value(N, X):-
        index_pred_state(N, par_relations, smaller(X, zero)),!.

% if the above par_relation is not present, we have to check 
% it by investigating the value in relation to its quantity space
%
negative_value(N, X):-
       index_pred_state(N, par_values, value(X, _, Value, _Der)),!,
       nonvar(Value),
       engine:qspace(X, _Predicate, QSpaceList, _Fail), 
       qspace_member(Value, QSpaceList, Index),
       qspace_member(zero, QSpaceList, ZeroIndex), 
       Index < ZeroIndex,!.


% This succeeds when X is unknown in state N
%
unknown_value(N, X):- 
       index_pred_state(N, par_values, value(X, _, Value, _Der)),!,
       var(Value).

        
% This succeeds when the derivative of X is unknown in state N
%
unknown_derivative(N, X):- 
       index_pred_state(N, par_values, value(X, _, _Value, Der)),!,
       var(Der).




increasing(N, X):-
        index_pred_state(N, par_values, value(X, _, _Value, Der)),!,
        Der == plus
        ;
        index_pred_state(N, par_relations, d_greater(X, zero)).


decreasing(N, X):-
        index_pred_state(N, par_values, value(X, _, _Value, Der)),!,
        Der == min
        ; % or
        index_pred_state(N, par_relations, d_smaller(X, zero)).

zero_value(N, X):-
        index_pred_state(N, par_values, value(X, _, Val, _Der)),!,
        Val == zero
        ; % or
        index_pred_state(N, par_relations, equal(X, zero)).

steady(N, X):-
        index_pred_state(N, par_values, value(X, _, _, Der)),!,
        Der == zero
        ; % or
        index_pred_state(N, par_relations, d_equal(X, zero)).


% Value or derivative of X is expected to have a 
% positive effect on Y (causing Y to increase)
%
expected_effect(N, causal_relation(X, 'I+', _Y), pos):-
        positive_value(N, X),!.
expected_effect(N, causal_relation(X, 'I-', _Y), pos):-
        negative_value(N, X),!.
expected_effect(N, causal_relation(X, 'P+', _Y), pos):-
        increasing(N, X),!.
expected_effect(N, causal_relation(X, 'P-', _Y), pos):-
        decreasing(N, X),!.

% Value or derivative of X is expected to have a 
% negative effect on Y (causing Y to decrease)
%
expected_effect(N, causal_relation(X, 'I+', _Y), neg):-
        negative_value(N, X),!.
expected_effect(N, causal_relation(X, 'I-', _Y), neg):-
        positive_value(N, X),!.
expected_effect(N, causal_relation(X, 'P+', _Y), neg):-
        decreasing(N, X),!.
expected_effect(N, causal_relation(X, 'P-', _Y), neg):-
        increasing(N, X),!.

% Value or derivative of X is expected to have  
% no effect on Y
%
expected_effect(N, causal_relation(X, 'I+', _Y), none):-
        zero_value(N, X),!.
expected_effect(N, causal_relation(X, 'I-', _Y), none):-
        zero_value(N, X),!.
expected_effect(N, causal_relation(X, 'P+', _Y), none):-
        steady(N, X),!.
expected_effect(N, causal_relation(X, 'P-', _Y), none):-
        steady(N, X),!.

% Value or derivative of X is not expected to have  
% an effect on Y, because this value or derivative 
% is unknown. Strictly speaking, this is a slightly 
% different case than 'expected to have no effect', 
% but GARP seems to treat them the same way, i.e., 
% no effect is processed. Therefore, the same status
% is given to these dependencies.  
%
expected_effect(N, causal_relation(X, 'I+', _Y), none):-
        unknown_value(N, X),!.
expected_effect(N, causal_relation(X, 'I-', _Y), none):-
        unknown_value(N, X),!.
expected_effect(N, causal_relation(X, 'P+', _Y), none):-
        unknown_derivative(N, X),!.
expected_effect(N, causal_relation(X, 'P-', _Y), none):-
        unknown_derivative(N, X),!.



% rest case - make sure no backtracking can occur, 
% because this clause will always fire
%
% expected_effect(_N, CausalParRel, unknown):-
%       CausalParRel = causal_relation(_X, _Rel, _Y).



% actual_effect(N, Y, Effect) 
%
% This predicate finds out whether Y changes or not 
% (Effect = none) in state N, and if yes, in which direction 
% (Effect = pos or neg).
%
actual_effect(N, Y, pos):-
        increasing(N, Y),!.

actual_effect(N, Y, neg):-
        decreasing(N, Y),!.

actual_effect(N, Y, none):-
        steady(N, Y),!.

% rest case - make sure no backtracking can occur, 
% because this clause will always fire
%
% actual_effect(_N, _Y, unknown).



% causal_chain(StateNr, A, B, Chain, Length)
%
causal_chain(N, A, B, Chain, L):-
        path(A, B, causal_graph(N), Chain, L).




