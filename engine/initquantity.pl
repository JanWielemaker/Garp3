/*  File:    initquantity
    Purpose: Declaration and initialisation of quantity spaces
    Author:  Martin Reinders & Bert Bredeweg & Floris Linnebank
    Date:    August 1989
    Part-of:  GARP (version 2.0)
    Modified: 17 August 2004

    Copyright (c) 2004, University of Amsterdam. All rights reserved.

    Change 28 April 2003, AB: argument order in last predicate 

    Change May 2004, FL: 2 versions of quantity spaces. With constrained derivative
    and without constrained derivative. 
    Changed last/2 into common_last/2 which has the same semantics to implement 
    compatibility with older prolog. It should be defined in load.pl.

Normal:
common_last(A, B):-
    Last(A, B).
Old:
common_last(A, B):-
    Last(B, A).

*/

/*
    a quantity space is declared as:

    quantity_space(Type, Parameter, Interval/PointList).

    where Type is something like amount, mz (min & zero), etc;

    Parameter is not initialised, but matches with a parameter instance
    and may match with points in the interval/pointlist which are
    specific for that parameter (see example below)

    Interval List is a list of contiguous intervals/pointintervals, where
    intervals have a symbolic name (that may be a structure), and
    point intervals are represented as a structure point(symbol).

    Intervals must be open, in other words: exactly one point occurs
    between two intervals.

    If two parameters have the same type of quantity space, but points
    on that quantity space are not realy equal, such points must have
    different symbols. This is accomplished by giving the symbol an
    argument that matches with the Parameter argument, e.g.:

    quantity_space(my_type, Parameter, 
        [ point(low(Parameter)), some_interval, 
            point(high(Parameter)) ]).

    Notice that the interval 'some_interval' will have different
    bounderies for different parameters. As inequality reasoning
    will only use these bounderies to determine what interval the
    parameter is in, the interval name does not have to be
    different for both parameters.

    In order to reason about inequalities with respect to the
    quantity space, some inequality relations are inferred about
    the order of points, e.g.

    low(Parameter) < high(Parameter).

    When a quantity space has a point at some end (in contrast with
    an interval that is assumed to extend to +/- infinite) the
    corresponding parameter may not go beyond that point. That means
    the relations
        low(Parameter) =< Parameter /\
        Parameter =< high(Parameter)
    must be inferred. In addition, when a parameter is at a low or
    high point at the end of the quantity space, it is impossible
    that it goes lower or higher, thus the implication
    
        equal(Parameter, point(lowest)) ->
            d_greater_or_equal(Parameter, zero)
    
    must be true.

    The purpose of this module is to generate these relations and
    add them to the quantity space as a fourth argument. Whenever
    a new parameter is found, the corresponding relations are
    added. Because we loose that information between states, it
    should be done for every state.

*/


/* FL april 2004:
* In some models a quantity should be allowed to keep going up or down in the
* highest or lowest point of the q-space respectively
* Algorithm option switches ( flags ) are used in point_bounderies
* both for constraining the derivative when zero is an extreme point
* or if some other extreme point is present
*/

initialise_quantity_spaces:-
    once(retractall(quantity_space(_, _, _, _))), 
    quantity_space(Type, Parameter, List), 
    point_order(List, Order),
    % get switch settings 
    flag(free_maxmin_derivative, MMFlag, MMFlag),
    algorithm_assumption_flag(MMFlag, _, free_maxmin_derivative),
    flag(free_zero_derivative, ZFlag, ZFlag),
    algorithm_assumption_flag(ZFlag, _, free_zero_derivative),
    % get quantity space derivative constraints (boundaries)
    derivative_boundaries(ZFlag, MMFlag, List, Parameter, DerivativeBounds),
    % get quantity space value constraints (boundaries)
    % currently, not under flag control. both constrained
    value_boundaries(fail, fail, List, Parameter, ValueBounds),
    flag(equal_intervals, EIFlag, EIFlag),
    equal_distance_qspace(EIFlag, List, Parameter, EqualDistanceCalculus),
    append(ValueBounds, DerivativeBounds, Boundaries),
    append(Order, Boundaries, OB),
    append(OB, EqualDistanceCalculus, All),
    % quantity space complete 
    assert(quantity_space(Type, Parameter, List, All)), 
    fail.

initialise_quantity_spaces.

point_order([ point(Below) | Tail ], [ smaller(Below, Above) | T ]):-
    memberchk(point(Above), Tail),  %only the first
                        % rest is transitive
    !, 
    point_order(Tail, T).

% skip interval or last/only point

point_order([ _ | Tail ], Order):-
    point_order(Tail, Order).

point_order([], []).


% derivative_boundaries/5
% Get upper lower bound for derivative
% First argument is an indicator if the derivative 
% must be constrained or not in zero as an extreme value,
% Second argument indicates if derivative should be constrained 
% in other extreme values. 
% NB 
% true = free = unconstrained
% fail = not free = constrained

% all possible constraints: low point, also check for possible high
derivative_boundaries(fail, fail, [point(First)|RestSpace], Parameter, 
		      [if([equal(Parameter, First)],[d_greater_or_equal(Parameter, zero)])|
		      Relations]):-
    !,
    derivative_boundaries(fail, fail, RestSpace, Parameter, Relations).

% all possible constraints: high point, if low present: already found.
derivative_boundaries(fail, fail, Space, Parameter, 
		 [if([equal(Parameter, Last)],[d_smaller_or_equal(Parameter, zero)])]):-
    common_last(Space, point(Last)), % argument order changed, 28/04/2003, AB 
    !.

% only zero constraints: low point, also check for possible high
derivative_boundaries(fail, true, [point(zero)|RestSpace], Parameter, 
		      [if([equal(Parameter, zero)],[d_greater_or_equal(Parameter, zero)])|
		      Relations]):-
    !,
    derivative_boundaries(fail, true, RestSpace, Parameter, Relations).

% only zero constraints: high point, if low present: already found.
derivative_boundaries(fail, true, Space, Parameter, 
		 [if([equal(Parameter, zero)],[d_smaller_or_equal(Parameter, zero)])]):-
    common_last(Space, point(zero)), % argument order changed, 28/04/2003, AB 
    !.

% only other constraints: low point, also check for possible high
derivative_boundaries(true, fail, [point(First)|RestSpace], Parameter, 
		      [if([equal(Parameter, First)],[d_greater_or_equal(Parameter, zero)])|
		      Relations]):-
    First \= zero, 
    !,
    derivative_boundaries(true, fail, RestSpace, Parameter, Relations).

% only other constraints: high point, if low present: already found.
derivative_boundaries(true, fail, Space, Parameter, 
		 [if([equal(Parameter, Last)],[d_smaller_or_equal(Parameter, zero)])]):-
    common_last(Space, point(Last)), % argument order changed, 28/04/2003, AB 
    Last \= zero, 
    !.

% all other cases or interval at extreme.
derivative_boundaries(_, _, _, _, []).


% value_boundaries/5
% Get upper lower bound for quantity
% First argument is an indicator if the value 
% must be constrained or not in zero as an extreme value,
% Second argument indicates if value should be constrained 
% in other extreme values. 
% NB 
% true = free = unconstrained
% fail = not free = constrained

% all possible constraints: low point, also check for possible high
value_boundaries(fail, fail, [point(First)|RestSpace], Parameter, 
		      [greater_or_equal(Parameter, First)|
		      Relations]):-
    !,
    value_boundaries(fail, fail, RestSpace, Parameter, Relations).

% all possible constraints: high point, if low present: already found.
value_boundaries(fail, fail, Space, Parameter, 
		 [smaller_or_equal(Parameter, Last)]):-
    common_last(Space, point(Last)), % argument order changed, 28/04/2003, AB 
    !.

% only zero constraints: low point, also check for possible high
value_boundaries(fail, true, [point(zero)|RestSpace], Parameter, 
		      [greater_or_equal(Parameter, zero)|
		      Relations]):-
    !,
    value_boundaries(fail, true, RestSpace, Parameter, Relations).

% only zero constraints: high point, if low present: already found.
value_boundaries(fail, true, Space, Parameter, 
		 [smaller_or_equal(Parameter, zero)]):-
    common_last(Space, point(zero)), % argument order changed, 28/04/2003, AB 
    !.

% only other constraints: low point, also check for possible high
value_boundaries(true, fail, [point(First)|RestSpace], Parameter, 
		      [greater_or_equal(Parameter, First)|
		      Relations]):-
    First \= zero, 
    !,
    derivative_boundaries(true, fail, RestSpace, Parameter, Relations).

% only other constraints: high point, if low present: already found.
value_boundaries(true, fail, Space, Parameter, 
		 [smaller_or_equal(Parameter, Last)]):-
    common_last(Space, point(Last)), % argument order changed, 28/04/2003, AB 
    Last \= zero, 
    !.

% all other cases or interval at extreme.
value_boundaries(_, _, _, _, []).


% get a space for a parameter

% space unbound, try genetic parameter name
get_quantity_space(G, I, G, L, R):-
    quantity_space(G, I, L, R), 
    !.

% space unbound, try default
get_quantity_space(_G, I, mzp, L, R):-
    quantity_space(mzp, I, L, R), 
    !.

% space bound
get_quantity_space(_G, I, S, L, R):-
    quantity_space(S, I, L, R), 
    !.

% not found
get_quantity_space(G, I, S, _, _):-
   etrace(initquantity_error, [G, I, S], general).



% New FL mar 07
% if switch is on, add equality between points on equal quantityspaces
% 
% switch is off, return empty set of relations
% 
% NB FL may 07: switch implementation postponed to be done coordinated with 
% more switch changes. Permanently turned off now.
equal_quantity_spaces(_Instance, _Space, _SpaceList, _P, []):-
	flag(equal_qspace_points, Flag, Flag),
	algorithm_assumption_flag(Flag, fail, equal_qspace_points),
	!.

% switch is on: get equalities for each other Q in P with equal Space
% for each point.
equal_quantity_spaces(Instance, Space, SpaceList, P, Relations):-
	flag(equal_qspace_points, Flag, Flag),
	algorithm_assumption_flag(Flag, true, equal_qspace_points),
	equal_qspaces(Instance, Space, SpaceList, P, Relations).

%done
equal_qspaces(_Instance, _Space, _SpaceList, [], []).

%same qspace: Space
equal_qspaces(Instance, Space, SpaceList, [P|T], Relations):-
	parameter(P, _NH, _Genetic, _SystemElements, OtherInstance, _Type, Space), 
	!, %existing parameter with same Space. qspace should exist with equivalent Spacelist
	qspace(OtherInstance, _, OtherSpaceList, _),
	equal_qspaces_rels(Instance, OtherInstance, SpaceList, OtherSpaceList, Rels),
	equal_qspaces(Instance, Space, SpaceList, T, TRels), 
	append(Rels, TRels, Relations).

%other qspace, next
equal_qspaces(Instance, Space, SpaceList, [_P|T], TRels):-
	equal_qspaces(Instance, Space, SpaceList, T, TRels).

% done
equal_qspaces_rels(_Instance, _OtherInstance, [], [], []).
	
% point zero
equal_qspaces_rels(I, OI, [point(zero)|SpaceList], [point(zero)|OtherSpaceList], Rels):-
	!,
	equal_qspaces_rels(I, OI, SpaceList, OtherSpaceList, Rels).

% point
equal_qspaces_rels(I, OI, [point(PI)|SpaceList], [point(POI)|OtherSpaceList], [equal(PI, POI)|Rels]):-
	% unneccessary checks:
	PI =.. [P, I],
	POI =..[P, OI],
	!,
	equal_qspaces_rels(I, OI, SpaceList, OtherSpaceList, Rels).

% interval
equal_qspaces_rels(I, OI, [Int|SpaceList], [Int|OtherSpaceList], Rels):-
	!,
	equal_qspaces_rels(I, OI, SpaceList, OtherSpaceList, Rels).



%%%%%%%%%%%%%%%%	equal_distance_qspace		   %%%%%%%%%%%%%%%
%%%%%%
%%	
%
%	
% This option generates a set of calculus relations that ensure
% that all intervals in	the qspace are treated as having the same size
% 
% 
equal_distance_qspace(0, _List, _Parameter, []):-!.
equal_distance_qspace(fail, _List, _Parameter, []):-!.
equal_distance_qspace(_, List, Parameter, EqualDistanceCalculus):-
	indexed_point_list(List, IndexList),
	e_d_calculus(IndexList, IndexList, EqDC),
	unify_eq_pars(EqDC, Parameter, EqualDistanceCalculus),
	!.

e_d_calculus([], _, []).
e_d_calculus([Point/Index|T], All, Constraints):-
	findall(C, e_d_equality(Point, Index, All, C),  PC),
	e_d_calculus(T, All, TC), 
	append(PC, TC, Constraints).


e_d_equality(Point, Index, All, equal(Point, plus(X, Y))):-
	     member(X/XI, All), 
	     \+ zero == X,
	     member(Y/YI, All), 
	     \+ zero == Y,
	     Index is XI + YI.

e_d_equality(Point, Index, All, equal(Point, min(X, Y))):-
	     member(X/XI, All), 
	     % \+ zero == X, % zero - something is valid: a sign change
	     member(Y/YI, All),
	     \+ X == Y, % something - something is zero: redundant
	     \+ zero == Y,
	     Index is XI - YI.


%findall introduces new variables: unify with Parameter variable
unify_eq_pars([], _, []). 
unify_eq_pars([equal(Point, plus(X, Y))|T], Parameter, [equal(Point, plus(X, Y))|NT]):-
	!,
	unify_eq_par1(Point, Parameter),
	unify_eq_par1(X, Parameter),
	unify_eq_par1(Y, Parameter),
	unify_eq_pars(T, Parameter, NT).

unify_eq_pars([equal(Point, min(X, Y))|T], Parameter, [equal(Point, min(X, Y))|NT]):-
	!,
	unify_eq_par1(Point, Parameter),
	unify_eq_par1(X, Parameter),
	unify_eq_par1(Y, Parameter),
	unify_eq_pars(T, Parameter, NT).
    
% zero needs no unification
unify_eq_par1(zero, _Parameter):-
	!.
unify_eq_par1(Point, Parameter):-
	Point =.. [_, Parameter], 
	!. 

% Construct a op qspace points (discard intervals)
% index with distance from zero
% 
% -2, -1, 0 , 1, 2, 3, etc.
indexed_point_list(List, Indexed):-
	memberchk(point(zero), List),
	!,
	nth0(X, List, point(zero)), 
	NX is X // 2,
	NNX is 0 - NX,
	index_point_lists(List, NNX, Indexed).

indexed_point_list(List, []):-
	\+memberchk(point(zero), List).
% no point zero... still this qspace may be related to zero
% through other inequalities, therefore we cannot predict which
% additions and subtractions to generate. 

	%index_point_lists(List, 0, _Unknown).


% NB index counts upwards from low to high
index_point_lists([], _, []). 

% point 
index_point_lists([point(X)|List], Index, [X/Index|IList]):-
	!,
	NewIndex is Index + 1,
	index_point_lists(List, NewIndex, IList). 

% interval >> skip
index_point_lists([_|List], Index, IList):-
	!,
	index_point_lists(List, Index, IList). 



