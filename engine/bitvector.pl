:- module(bitvector,
	  [ list_map/2,			% +List, -Map
	    map_list/2,			% +Map, -List
	    map_union/3,		% +Map1, +Map2, -Union
	    map_union_unique/3,		% +Map1, +Map2, -Union
	    map_intersection/3,		% +Map1, +Map2, -Intersection
	    map_without_intersection/4,	% +Map1, +Map2, -Map3, -Map4
	    map_count/2,		% +Map, -Count
	    map_replace/4,		% +Clear, +Set, +Map, -Map
	    map_difference/3,		% +Map, +Delete, -Map
	    window_size/2		% -Rows, -Columns
	  ]).

%	list_map(+List, -Map)
%	
%	Convert a List of integers into a bit-vector.

list_map(List, bitvector(Map)) :-
	list_map2(List, Map).

list_map2([], 0).
list_map2([H|T], V) :-
        list_map2(T, N),
        V is N \/ (1<<H).

%	map_list(+Map, -List)
%	
%	Convert a bit-vector into a sorted list of integers.


map_list(bitvector(Map), List) :-
	map_list2(Map, List).

map_list2(0, List) :- !,
	List = [].
map_list2(I, [H|T]) :-
        H is lsb(I),
        I2 is I /\ \(1<<H),
        map_list2(I2, T).

%	map_union(+Map1, +Map2, -Union)
%	
%	Compute the set-union of two maps

map_union(bitvector(A), bitvector(B), bitvector(C)) :-
	C is A \/ B.

%	map_union_unique(+Map1, +Map2, -Union)
%	
%	like union, but succeed only if intersection is empty

map_union_unique(bitvector(A), bitvector(B), bitvector(C)) :-
	A /\ B =:= 0,			% empty intersection
	C is A \/ B.

%	map_intersection(+Map1, +Map2, -Intersection)

map_intersection(bitvector(A), bitvector(B), bitvector(C)) :-
	C is A /\ B.
	
%	map_without_intersection(+Map1, +Map2, -Map3, -Map4)
%	
%	Remove intersection of both maps from both maps; fail if no
%	intersection

map_without_intersection(bitvector(A), bitvector(B),
			 bitvector(C), bitvector(D)) :-
	Intersection is A /\ B,
	Intersection \== 0, % FL nov 07 used to be =\= but arithmetic is not necessary here (and slower!)
	C is A xor Intersection,
	D is B xor Intersection.

%	map_count(+Map, -Count)
%	
%	Count size of set

map_count(bitvector(A), Count) :-
	Count is popcount(A).

%	map_replace(+Clear, +Set, +Map1, -Map)
%	
%	Replace Clear by Set in Map1

map_replace(Clear, Set, bitvector(A), bitvector(B)) :-
	0 =\= A /\ (1<<Clear),		% fail if not set
	B is (A /\ \(1<<Clear)) \/ (1<<Set).


%	map_difference(+Map, +Delete, -Diff)

map_difference(bitvector(A), bitvector(B), bitvector(C)) :-
	C is A xor B.


%	window_size(-Rows, -Cols)
%	
%	

window_size(Rows, Cols) :-
	tty_size(Rows, Cols).


		 /*******************************
		 *		DEBUG		*
		 *******************************/

:- multifile
	user:portray/1.

user:portray(bitvector(Map)) :-
	integer(Map),
	map_list2(Map, List),
	format('bitvector(~p)', [List]).
