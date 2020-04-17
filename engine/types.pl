/*  File:    types
    Purpose: relation types
             semantics of influences and proportional relations
    Author:  Martin Reinders & Bert Bredeweg & Floris Linnebank
    Date:    August 1989
    Part-of:  GARP (version 2.0)
    Modified: 30 July 2004

    Copyright (c) 2004, University of Amsterdam. All rights reserved.

*/

inequality_type(greater(_, _)).
inequality_type(greater_or_equal(_, _)).
inequality_type(equal(_, _)).
inequality_type(smaller_or_equal(_, _)).
inequality_type(smaller(_, _)).
inequality_type(d_greater(_, _)).
inequality_type(d_greater_or_equal(_, _)).
inequality_type(d_equal(_, _)).
inequality_type(d_smaller_or_equal(_, _)).
inequality_type(d_smaller(_, _)).

influence_proportional_type(inf_pos_by(_, _)).
influence_proportional_type(inf_neg_by(_, _)).
influence_proportional_type(prop_pos(_, _)).
influence_proportional_type(prop_neg(_, _)).
influence_proportional_type(prop_mult(_, _)).
influence_proportional_type(prop_diw(_, _)).

/* GARP 1.X correspondence types:
correspondence_type(v_correspondence(_, _, _, _)).
correspondence_type(q_correspondence(_, _)).
correspondence_type(dir_v_correspondence(_, _, _, _)).
correspondence_type(dir_q_correspondence(_, _)).
correspondence_type(if(_, _)).
correspondence_type(iff(_, _)).

*/

/* GARP 2.0 correspondence types:
Values:
-   Value correspondence
-   Directed value correspondence
-   Quantity correspondence
-   Directed quantity correspondence
-   Mirrored quantity correspondence
-   Directed mirrored quantity correspondence

Derivatives:
-   Derivative correspondence
-   Directed derivative correspondence
-   Derivative-quantity correspondence
-   Directed derivative-quantity correspondence
-   Mirrored derivative-quantity correspondence
-   Directed mirrored derivative-quantity correspondence

Values and Derivatives:
-   Full correspondence
-   Directed full correspondence

v = value
dv = derivative,
q = whole quantity space
dq = whole derivative quantity space
full = derivatives & values,
mirror = opposite value correspondence: plus/min, zero/zero etc.
*/

%Values
correspondence_type(v_correspondence(_, _, _, _)).
correspondence_type(dir_v_correspondence(_, _, _, _)).
correspondence_type(q_correspondence(_, _)).
correspondence_type(dir_q_correspondence(_, _)).
correspondence_type(mirror_q_correspondence(_, _)).
correspondence_type(dir_mirror_q_correspondence(_, _)).
%Derivatives
correspondence_type(dv_correspondence(_, _, _, _)).
correspondence_type(dir_dv_correspondence(_, _, _, _)).
correspondence_type(dq_correspondence(_, _)).
correspondence_type(dir_dq_correspondence(_, _)).
correspondence_type(mirror_dq_correspondence(_, _)).
correspondence_type(dir_mirror_dq_correspondence(_, _)).
%Both
correspondence_type(full_correspondence(_, _)).
correspondence_type(dir_full_correspondence(_, _)).

%Implications
correspondence_type(if(_, _)).
correspondence_type(iff(_, _)).


%FL July 2004 select inequalities on quantities
pure_inequality_type(greater(_, _)).
pure_inequality_type(greater_or_equal(_, _)).
pure_inequality_type(equal(_, _)).
pure_inequality_type(smaller_or_equal(_, _)).
pure_inequality_type(smaller(_, _)).

%FL July 2004 select inequalities on derivatives
d_inequality_type(d_greater(_, _)).
d_inequality_type(d_greater_or_equal(_, _)).
d_inequality_type(d_equal(_, _)).
d_inequality_type(d_smaller_or_equal(_, _)).
d_inequality_type(d_smaller(_, _)).


%FL July 2004 select inequalities on quantities
% from a set of relations:
pure_inequality_relations([], []).

pure_inequality_relations([H|T], [H|NT]):-
    pure_inequality_type(H),
    !,
    pure_inequality_relations(T, NT).

pure_inequality_relations([_|T], NT):-
    pure_inequality_relations(T, NT).


%FL July 2004 select inequalities on derivatives
% from a set of relations:
d_inequality_relations([], []).

d_inequality_relations([H|T], [H|NT]):-
    d_inequality_type(H),
    !,
    d_inequality_relations(T, NT).

d_inequality_relations([_|T], NT):-
    d_inequality_relations(T, NT).


% map influences / proportional relations to an adder term
% arg1: relation(Par1, Par2)
% arg2: derivative(Par1) or value(Par1) influenced by
% pos/neg sign of derivative(Par2) or value(Par2)


% derivative positively influenced by value

adder_relation(inf_pos_by(Derivative, Quantity),
    derivative(Derivative), pos(value(Quantity))).

% derivative negatively influenced by value

adder_relation(inf_neg_by(Derivative, Quantity),
    derivative(Derivative), neg(value(Quantity))).

% derivative positively proportional to derivative

adder_relation(prop_pos(Derivative, Derivative2),
    derivative(Derivative), pos(derivative(Derivative2))).

% derivative negatively proportional to derivative

adder_relation(prop_neg(Derivative, Derivative2),
    derivative(Derivative), neg(derivative(Derivative2))).

% derivative positively or negatively proportional to derivative
% according to state of affairs

adder_relation(prop_mult(Derivative, Derivative2),
    derivative(Derivative), pos_neg_mult(derivative(Derivative2))).

%todo: fix this
adder_relation(prop_diw(Derivative, Derivative2),
    derivative(Derivative), pos_neg_diw(derivative(Derivative2))).

% prop_bidirectional????

adder_relation(rev_inf_pos_by(Quantity, Derivative),
    value(Quantity), pos(derivative(Derivative))).
adder_relation(rev_inf_neg_by(Quantity, Derivative),
    value(Quantity), neg(derivative(Derivative))).

adder_relation(derivative_pos(Derivative, Quantity),
    derivative(Derivative), pos(value(Quantity))).
adder_relation(derivative_pos(Derivative, Quantity),
    value(Quantity), pos(derivative(Derivative))).

adder_relation(derivative_neg(Derivative, Quantity),
    derivative(Derivative), neg(value(Quantity))).
adder_relation(derivative_neg(Derivative, Quantity),
    value(Quantity), neg(derivative(Derivative))).
/*
adder_relation(directed_equal_pos(Quantity, Quantity2),
    quantity(Quantity), pos(value(Quantity))).
adder_relation(directed_equal_neg(Quantity, Quantity2),
    quantity(Quantity), neg(value(Quantity))).
*/

% opposite directed_equal_???? bidirect????

