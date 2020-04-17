/*  File:    initquantity
    Purpose: Declaration and initialisation of quantity spaces
    Author:  Martin Reinders & Bert Bredeweg & Floris Linnebank
    Date:    August 1989
    Part-of:  GARP (version 2.0)
    Modified: 30 July 2004

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


/* old version: Fl april 2004 New Below
initialise_quantity_spaces:-
	once(retractall(quantity_space(_, _, _, _))), 
	quantity_space(Type, Parameter, List), 
	point_order(List, Order), 
	point_bounderies(List, Parameter, Bounderies), 
	append(Order, Bounderies, OB), 
	assert(quantity_space(Type, Parameter, List, OB)), 
	fail.

initialise_quantity_spaces.

* New april 2004 FL:
* In some models a quantity should be allowed to keep going up or down in the
* highest or lowest point of the q-space respectively
* An algorithm assumption flag is used in point_bounderies
*/

initialise_quantity_spaces:-
	once(retractall(quantity_space(_, _, _, _))), 
	quantity_space(Type, Parameter, List), 
	point_order(List, Order), 
    flag(free_maxmin_derivative, Flag, Flag),
    algorithm_assumption_flag(Flag, _),
	point_bounderies(Flag, List, Parameter, Bounderies), 
	append(Order, Bounderies, OB), 
	assert(quantity_space(Type, Parameter, List, OB)), 
	fail.

initialise_quantity_spaces.

point_order([ point(Below) | Tail ], [ smaller(Below, Above) | T ]):-
	memberchk(point(Above), Tail), 	%only the first
						% rest is transitive
	!, 
	point_order(Tail, T).

% skip interval or last/only point

point_order([ _ | Tail ], Order):-
	point_order(Tail, Order).

point_order([], []).

/* old version FL april 2004 see above & new below
point_bounderies(Space, Parameter, [ smaller_or_equal(First, Parameter), 
					smaller_or_equal(Parameter, Last), 
					if([equal(Parameter, First)], 
					  [d_greater_or_equal(Parameter, zero)]), 
					if([equal(Parameter, Last)], 
					  [d_smaller_or_equal(Parameter, zero)])
				    ]):-
	first(point(First), Space), 
	common_last(Space, point(Last)), % argument order changed, 28/04/2003, AB 
	% last(point(Last), Space), 
	!.

point_bounderies(Space, Parameter, [ smaller_or_equal(First, Parameter), 
					if([equal(Parameter, First)], 
					  [d_greater_or_equal(Parameter, zero)])
				    ]):-
	first(point(First), Space), 
	!.

point_bounderies(Space, Parameter, [ smaller_or_equal(Parameter, Last), 
					if([equal(Parameter, Last)], 
					  [d_smaller_or_equal(Parameter, zero)])
				    ]):-
	common_last(Space, point(Last)), % argument order changed, 28/04/2003, AB 
	% last(point(Last), Space), 
	!.

point_bounderies(_, _, []).

* New Version Below: First argument is an indicator if the derivative 
* must be constrained or not */

point_bounderies(fail, Space, Parameter, [ smaller_or_equal(First, Parameter), 
					smaller_or_equal(Parameter, Last), 
					if([equal(Parameter, First)], 
					  [d_greater_or_equal(Parameter, zero)]), 
					if([equal(Parameter, Last)], 
					  [d_smaller_or_equal(Parameter, zero)])
				    ]):-
    first(point(First), Space), 
	common_last(Space, point(Last)), % argument order changed, 28/04/2003, AB 
	% last(point(Last), Space), 
	!.

point_bounderies(fail, Space, Parameter, [ smaller_or_equal(First, Parameter), 
					if([equal(Parameter, First)], 
					  [d_greater_or_equal(Parameter, zero)])
				    ]):-
	first(point(First), Space), 
	!.

point_bounderies(fail, Space, Parameter, [ smaller_or_equal(Parameter, Last), 
					if([equal(Parameter, Last)], 
					  [d_smaller_or_equal(Parameter, zero)])
				    ]):-
	common_last(Space, point(Last)), % argument order changed, 28/04/2003, AB 
	% last(point(Last), Space), 
	!.
	
point_bounderies(true, Space, Parameter, [ smaller_or_equal(First, Parameter), 
					smaller_or_equal(Parameter, Last) ]):-
    first(point(First), Space), 
	common_last(Space, point(Last)), % argument order changed, 28/04/2003, AB 
	% last(point(Last), Space), 
	!.

point_bounderies(true, Space, Parameter, [ smaller_or_equal(First, Parameter) ]):-
	first(point(First), Space), 
	!.

point_bounderies(true, Space, Parameter, [ smaller_or_equal(Parameter, Last) ]):-
	common_last(Space, point(Last)), % argument order changed, 28/04/2003, AB 
	% last(point(Last), Space), 
	!.

point_bounderies(_, _, _, []).


/* FL april 2004 End New */


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
	writef(
'*** Error: no quantity space specified for parameter %w instance %w space %w\n', 
		[G, I, S]).

