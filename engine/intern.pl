/*  File:    intern
    Purpose: Intern representation (for inequality reasoning)
    Author:  Martin Reinders & Bert Bredeweg & Floris Linnebank
    Date:    August 1989
    Part-of:  GARP (version 2.0)
    Modified: 30 July 2004

    Copyright (c) 2004, University of Amsterdam. All rights reserved.

*/

/*  intern representation (for inequality reasoning)

    Each quantity has a unique integer attached to it,
    This number is used to turn on a corresponding bit in a bitmap.

    zero has no number attached to it, thus a 'sum' consisting of zero only
    maps to the empy bitmap.

    Intern representation maps a relation with positive and negative terms
    to a structure:

       relation(MapLeft, Relation, MapRight)

    where MapLeft & MapRight correspond to sums of positive quantities.

    e.g. greater(min(a, b), zero) maps to
    relation(MapA, > MapB) where the corresponding bit for a is set
    in MapA and for b in MapB

    intern_representation may return only >, >= or = relations !

    a problem occurs if a sum can not be represented with a set, i.e.
    if one or more quantities occur more than once. This is likely to
    occur, because when some quantity I is found to be equal to J, its
    number is replaced with J. If this occurs, a new quantity with the
    same identifier is produced that replaces one of the double quantities.

    FL may 07 the empty list will not be used to represent zero anymore. only the zero pointer: 0

    FL nov 07: inequality reasoning redo:
    - canonical form also for = relations
    - division of base and derivable in relevant contexts

*/


%%	intern_quantity_representation(+valder(Name), -Pointer, +Cin, -Cout)
%
%	convert: value(Quantity) or derivative(Quantity),
%	to an internal pointer and add it to the mathematical model if
%	necessary.
%
%	Function not used yet, dev purposes for now... FL sept 2010
%
intern_quantity_representation(Quantity, Internal, Cin, Cout):-
	cio_q_nq(Cin, Cout, Q, NQ),
	(Quantity = value(_) ; Quantity = derivative(_)),
	!,
	i_expression(Quantity, Internal, Q, NQ).

% zero, don't add
i_expression(value(zero), 0, Q, Q):-
    !.

% already present, return pointer
i_expression(Quantity, I, Q, Q):-
    memberchk(Quantity/I, Q),
    !.

% quantity is new in mathmatical model
i_expression(Quantity, I, Q, [Quantity/I|Q]):-
    flag(q_cnt, P, 0),      % increase q_cnt
    I is P + 1,
    flag(q_cnt, _, I),
    !.





intern_representations([], [], Cio, Cio).
intern_representations([H|T], [NH|NT], Cin, Cout):-
    intern_representation(H, NH, Cin, Cnew),
    intern_representations(T, NT, Cnew, Cout).


intern_representation(Relation, relation(Left, Rel, Right), Cin, Cout):-
    cio_q_nq(Cin, Cnew, Q, NQ),
    add_zero_index(Q, Q1),
    i_r(Relation, relation(LeftList, Rel, RightList), Q1, NQ),
    double_quantities(LeftList, NLeftList, [], Cnew, Cnew1),
    double_quantities(RightList, NRightList, [], Cnew1, Cout),
    empty_to_zero(NLeftList, NLeftList1),
    empty_to_zero(NRightList,NRightList1),
    remove_excess_zeros(NLeftList1, NLeftList2),
    remove_excess_zeros(NRightList1,NRightList2),
    list_map(NLeftList2, Left1),
    list_map(NRightList2, Right1),
    canonical_form(Left1, Right1, Rel, Left, Right). % canonical form for equalities...


canonical_form(X, X, =, X, X):-
	!.
canonical_form(Lin, Rin, =, Lout, Rout):-
	!, % is an equality...
	sort([Lin, Rin], [Lout, Rout]).    % canonical form for equalities...

canonical_form(L, R, _, L, R).

apply_cannonical_form([], []).
apply_cannonical_form([relation(L, Rel, R)|T], [relation(NL, Rel, NR)|NT]):-
	canonical_form(L, R, Rel, NL, NR),
	apply_cannonical_form(T, NT).


canonical_form(relation(L, Rel, R), relation(NL, Rel, NR)):-
	canonical_form(L, R, Rel, NL, NR).



% equal quantities: sort will make a set (single item list) so special
% case needed
canonical_multiplication(X*X, X*X).

% different quantitis. put in standard order
canonical_multiplication(X*Y, First*Second):-
	sort([X, Y], [First, Second]),
	!.


% zero pointers in an addition can be removed
% single item: keep (even if 0)
remove_excess_zeros([X], [X]):-
	!.
% list; remove zero if present
remove_excess_zeros(ListIn, ListOut):-
	delete(ListIn, 0, ListOut),
	!.

% FL may 07 the empty list will not be used to represent zero anymore. only the zero pointer: 0
empty_to_zero([], [0]):-!.
empty_to_zero(List, List).

% put zero mapping if not there already
add_zero_index(Q, Q):-
	memberchk(zero/0, Q),
	!.
add_zero_index(Q, [zero/0|Q]).



% check for double quantities, i.e. a + a = x
% it is not possible then to transform the sum to a bitmap
% the solution (a bit of a hack) is to replace one of the double
% quantities with a new quantity & introduce an equality relation
% for those quantities.

% patch 1 29-12-2003 by FL: instead of the new quantity/number combination,
% the existing one was appended... This makes it impossible to identify
% the double quantity bit. Fixed that

% patch 2 januari 2004 by FL: the equality relation between the double
% quantity bits proved insufficient to solve a simple addition like x = y + y.
% (Constraints in the solve algorithm do not allow a relation to be used twice
% for deductions.) This is solved by adding all known relations in wich the
% original quantity is an element, with the double quantity substituted for the
% original.

% patch 3 FL July 2004: First check if an existing double quantity mapping can be
% used... this saves a lot of relations.

/*** Old double_quantities clause before patch 1 & 2 ***

double_quantities([], [], Cio, Cio).

double_quantities([H|T], [I|NT], Cin, Cout):-
    memberchk(H, T),            % H in T
    !,
    flag(q_cnt, P, 0),      % increase q_cnt
    I is P + 1,             % a new quantity
    flag(q_cnt, _, I),
    cio_q_nq(Cin, Cnew, Q, NQ),
    memberchk(V/H, Q),
    append(Q, [V/H], NQ),       % append quantity (same identity!)
    list_map([H], F),
    list_map([I], S),
    append_relation(relation(F, =, S), Cnew, Cnew2, _, false),
    double_quantities(T, NT, Cnew2, Cout).

*** july 2004 new below ****/

% some double quantities can be fixed using an existing double quantity mapping
% added third argument to keep track of used substitutions.
double_quantities([], [], _, Cio, Cio).

%patch3 July 2004: First check if an existing double quantity mapping can be used...
double_quantities([H|T], [I|NT], Used, Cin, Cout):-
    memberchk(H, T),            /* H in T */
    \+ memberchk(H, Used),
    cio_q(Cin, Q),
    available_dq_mapping(H, Q, Used, I),
    !,
    double_quantities(T, NT, [I|Used], Cin, Cout).

double_quantities([H|T], [I|NT], Used, Cin, Cout):-
    memberchk(H, T),            /* H in T */
    !,
    flag(q_cnt, P, 0),      /* increase q_cnt */
    I is P + 1,             /* a new quantity */
    flag(q_cnt, _, I),
    cio_q_nq(Cin, Cnew, Q, NQ),
    % patch idea 3: FL may 07, memberchk leaves old: V/H in place resulting in two mappings for V!
    % select(V/H, Q, Q1),
    % FL aug 07 but that is more correct than having a pointer in other relations with the old mapping that is not existing anymore...
    memberchk(V/H, Q),
    append(Q, [V/I], NQ),       /* append quantity (same identity!) */
    % Patch 1: used to append [V/H] which is just the old one!
    list_map([H], F),
    list_map([I], S),
    % Patch 2: also add all shared relations besides f = s equality.
    cio_d(Cin, Derivable),
    findall(relation(S, Rel, X), member(relation(F, Rel, X), Derivable), Relations1),
    findall(relation(X, Rel, S), member(relation(X, Rel, F), Derivable), Relations2),
    append(Relations1, Relations2, Relations),
    append_relation_all([relation(F, =, S)|Relations], Cnew, Cnew2, _, false),
    double_quantities(T, NT, Used, Cnew2, Cout).

double_quantities([H|T], [H|NT], Used, Cin, Cout):-
    double_quantities(T, NT, Used, Cin, Cout).

% find an equivalent pointer for I which is not in Used
available_dq_mapping(I, Q, Used, J):-
    findall(X/I, member(X/I, Q), Xs),
    subtract(Q, Xs, RestQ),
    !,
    member(X/I, Xs),
    member(X/J, RestQ),
    \+memberchk(J, Used),
    I \= J,
    !.


i_r(greater(Left, Right), relation(NLeft, >, NRight), Q, NQ):-
    i_normal_expression(Left, LPos, LNeg, Q, Q1),
    /* parse expressions */
    i_normal_expression(Right, RPos, RNeg, Q1, NQ),
    append(LPos, RNeg, NLeft),
    /* positive terms in left & negative terms in right make leftmap */
    append(RPos, LNeg, NRight), !.      /* etc */

/* note order of Right and Left */

i_r(smaller(Right, Left), relation(NLeft, >, NRight), Q, NQ):-
    i_normal_expression(Left, LPos, LNeg, Q, Q1),
    i_normal_expression(Right, RPos, RNeg, Q1, NQ),
    append(LPos, RNeg, NLeft),
    append(RPos, LNeg, NRight), !.

i_r(greater_or_equal(Left, Right), relation(NLeft, >=, NRight), Q, NQ):-
    i_normal_expression(Left, LPos, LNeg, Q, Q1),
    i_normal_expression(Right, RPos, RNeg, Q1, NQ),
    append(LPos, RNeg, NLeft),
    append(RPos, LNeg, NRight), !.

i_r(smaller_or_equal(Right, Left), relation(NLeft, >=, NRight), Q, NQ):-
    i_normal_expression(Left, LPos, LNeg, Q, Q1),
    i_normal_expression(Right, RPos, RNeg, Q1, NQ),
    append(LPos, RNeg, NLeft),
    append(RPos, LNeg, NRight), !.

i_r(equal(Left, Right), relation(NLeft, =, NRight), Q, NQ):-
    i_normal_expression(Left, LPos, LNeg, Q, Q1),
    i_normal_expression(Right, RPos, RNeg, Q1, NQ),
    append(LPos, RNeg, NLeft),
    append(RPos, LNeg, NRight), !.

/* derivative relations */

i_r(d_greater(Left, Right), relation(NLeft, >, NRight), Q, NQ):-
    i_derivative_expression(Left, LPos, LNeg, Q, Q1),
    i_derivative_expression(Right, RPos, RNeg, Q1, NQ),
    append(LPos, RNeg, NLeft),
    append(RPos, LNeg, NRight), !.

i_r(d_smaller(Right, Left), relation(NLeft, >, NRight), Q, NQ):-
    i_derivative_expression(Left, LPos, LNeg, Q, Q1),
    i_derivative_expression(Right, RPos, RNeg, Q1, NQ),
    append(LPos, RNeg, NLeft),
    append(RPos, LNeg, NRight), !.

i_r(d_greater_or_equal(Left, Right), relation(NLeft, >=, NRight), Q, NQ):-
    i_derivative_expression(Left, LPos, LNeg, Q, Q1),
    i_derivative_expression(Right, RPos, RNeg, Q1, NQ),
    append(LPos, RNeg, NLeft),
    append(RPos, LNeg, NRight), !.

i_r(d_smaller_or_equal(Right, Left), relation(NLeft, >=, NRight), Q, NQ):-
    i_derivative_expression(Left, LPos, LNeg, Q, Q1),
    i_derivative_expression(Right, RPos, RNeg, Q1, NQ),
    append(LPos, RNeg, NLeft),
    append(RPos, LNeg, NRight), !.

i_r(d_equal(Left, Right), relation(NLeft, =, NRight), Q, NQ):-
    i_derivative_expression(Left, LPos, LNeg, Q, Q1),
    i_derivative_expression(Right, RPos, RNeg, Q1, NQ),
    append(LPos, RNeg, NLeft),
    append(RPos, LNeg, NRight), !.


/* second derivative relations */

i_r(dd_greater(Left, Right), relation(NLeft, >, NRight), Q, NQ):-
    di_derivative_expression(Left, LPos, LNeg, Q, Q1),
    di_derivative_expression(Right, RPos, RNeg, Q1, NQ),
    append(LPos, RNeg, NLeft),
    append(RPos, LNeg, NRight), !.

i_r(dd_smaller(Right, Left), relation(NLeft, >, NRight), Q, NQ):-
    di_derivative_expression(Left, LPos, LNeg, Q, Q1),
    di_derivative_expression(Right, RPos, RNeg, Q1, NQ),
    append(LPos, RNeg, NLeft),
    append(RPos, LNeg, NRight), !.

i_r(dd_greater_or_equal(Left, Right), relation(NLeft, >=, NRight), Q, NQ):-
    di_derivative_expression(Left, LPos, LNeg, Q, Q1),
    di_derivative_expression(Right, RPos, RNeg, Q1, NQ),
    append(LPos, RNeg, NLeft),
    append(RPos, LNeg, NRight), !.

i_r(dd_smaller_or_equal(Right, Left), relation(NLeft, >=, NRight), Q, NQ):-
    di_derivative_expression(Left, LPos, LNeg, Q, Q1),
    di_derivative_expression(Right, RPos, RNeg, Q1, NQ),
    append(LPos, RNeg, NLeft),
    append(RPos, LNeg, NRight), !.

i_r(dd_equal(Left, Right), relation(NLeft, =, NRight), Q, NQ):-
    di_derivative_expression(Left, LPos, LNeg, Q, Q1),
    di_derivative_expression(Right, RPos, RNeg, Q1, NQ),
    append(LPos, RNeg, NLeft),
    append(RPos, LNeg, NRight), !.

% end second order derivatives



% third derivative relations

i_r(ddd_equal(Left, Right), relation(NLeft, =, NRight), Q, NQ):-
    ddi_derivative_expression(Left, LPos, LNeg, Q, Q1),
    ddi_derivative_expression(Right, RPos, RNeg, Q1, NQ),
    append(LPos, RNeg, NLeft),
    append(RPos, LNeg, NRight), !.

% end third derivative relations








%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% EXPRESSIONS:
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

i_normal_expression(plus(A, B), Pos, Neg, Q, NQ):-
    i_normal_expression(A, APos, ANeg, Q, Q1),
    i_normal_expression(B, BPos, BNeg, Q1, NQ),
    append(APos, BPos, Pos),
    append(ANeg, BNeg, Neg).

i_normal_expression(min(A, B), Pos, Neg, Q, NQ):-
    i_normal_expression(A, APos, ANeg, Q, Q1),
    i_normal_expression(B, BPos, BNeg, Q1, NQ),
    append(APos, BNeg, Pos),
    append(ANeg, BPos, Neg).


%  Work in Progress i_normal_expression(multiply(A, B) ....
%
%  mult & div
%
%  for now, assume that multiplicated elements are only simple
%  quantities, known in the system. not negated or anything or additions
%  etc.
%
i_normal_expression(mult(A, B), Pos, Neg, Q, NQ):-
	    memberchk(value(A)/_, Q),
	    memberchk(value(B)/_, Q),
	    !, % quantities are ok, now add the multiplication itself as an abstract quantity
	    canonical_multiplication(A*B, Canonical),
	    i_normal_expression(Canonical, Pos, Neg, Q, NQ).

i_normal_expression(diw(A, B), Pos, Neg, Q, NQ):-
	    memberchk(value(A)/_, Q),
	    memberchk(value(B)/_, Q),
	    !, % quantities are ok, now add the division itself as an abstract quantity
	    i_normal_expression(A/B, Pos, Neg, Q, NQ).




/* map zero to the empty map */

i_normal_expression(zero, [], [], Q, Q):-
    !.

/* set bit fo a quantity */

i_normal_expression(A, [I], [], Q, Q):-
    memberchk(value(A)/I, Q),
    !.

i_normal_expression(A, [I], [], Q, [value(A)/I|Q]):-
    flag(q_cnt, P, 0),      /* increase q_cnt */
    I is P + 1,
    flag(q_cnt, _, I),
    !.

i_derivative_expression(plus(A, B), Pos, Neg, Q, NQ):-
    i_derivative_expression(A, APos, ANeg, Q, Q1),
    i_derivative_expression(B, BPos, BNeg, Q1, NQ),
    append(APos, BPos, Pos),
    append(ANeg, BNeg, Neg).

i_derivative_expression(min(A, B), Pos, Neg, Q, NQ):-
    i_derivative_expression(A, APos, ANeg, Q, Q1),
    i_derivative_expression(B, BPos, BNeg, Q1, NQ),
    append(APos, BNeg, Pos),
    append(ANeg, BPos, Neg).

%TODO i_derivative_expression(multiply(A, B) ....

i_derivative_expression(zero, [], [], Q, Q):-
    !.

i_derivative_expression(A, [I], [], Q, Q):-
    memberchk(derivative(A)/I, Q),
    !.

i_derivative_expression(A, [I], [], Q, [derivative(A)/I|Q]  ):-
    flag(q_cnt, P, 0),      % increase q_cnt
    I is P + 1,
    flag(q_cnt, _, I),
    !.


% Second order derivatives
di_derivative_expression(plus(A, B), Pos, Neg, Q, NQ):-
    di_derivative_expression(A, APos, ANeg, Q, Q1),
    di_derivative_expression(B, BPos, BNeg, Q1, NQ),
    append(APos, BPos, Pos),
    append(ANeg, BNeg, Neg).

di_derivative_expression(min(A, B), Pos, Neg, Q, NQ):-
    di_derivative_expression(A, APos, ANeg, Q, Q1),
    di_derivative_expression(B, BPos, BNeg, Q1, NQ),
    append(APos, BNeg, Pos),
    append(ANeg, BPos, Neg).

%TODO di_derivative_expression(multiply(A, B) ....

di_derivative_expression(zero, [], [], Q, Q):-
    !.

di_derivative_expression(A, [I], [], Q, Q):-
    memberchk(second_derivative(A)/I, Q),
    !.

di_derivative_expression(A, [I], [], Q, [second_derivative(A)/I|Q]  ):-
    flag(q_cnt, P, 0),      % increase q_cnt
    I is P + 1,
    flag(q_cnt, _, I),
    !.

% Third order derivatives
ddi_derivative_expression(plus(A, B), Pos, Neg, Q, NQ):-
    ddi_derivative_expression(A, APos, ANeg, Q, Q1),
    ddi_derivative_expression(B, BPos, BNeg, Q1, NQ),
    append(APos, BPos, Pos),
    append(ANeg, BNeg, Neg).

ddi_derivative_expression(min(A, B), Pos, Neg, Q, NQ):-
    ddi_derivative_expression(A, APos, ANeg, Q, Q1),
    ddi_derivative_expression(B, BPos, BNeg, Q1, NQ),
    append(APos, BNeg, Pos),
    append(ANeg, BPos, Neg).

%TODO ddi_derivative_expression(multiply(A, B) ....

ddi_derivative_expression(zero, [], [], Q, Q):-
    !.

ddi_derivative_expression(A, [I], [], Q, Q):-
    memberchk(third_derivative(A)/I, Q),
    !.

ddi_derivative_expression(A, [I], [], Q, [third_derivative(A)/I|Q]  ):-
    flag(q_cnt, P, 0),      % increase q_cnt
    I is P + 1,
    flag(q_cnt, _, I),
    !.
