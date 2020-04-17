/*
File: multiplication.pl
Purpose: QR multiplication for Garp / DynaLearn
Author: Floris Linnebank

copyright University of Amsterdam 2011

*/


%%	 Main multiplication loop:
%
%	NB mult(X, Y)
%	and diw(N, D)
%       (not div(_, _), because this is a prolog operator!!!)
%
%	FIRST STEP: a multiplication A*B rel C comes in, to be added to
%	the mathematical model (is checked before with
%	is_multiplication/1)
%
%	1: determine the multiplied quantities and qspaces
%
%	2: find all relevant points:
%	   Set 1 = A & LM’s qspace A
%	   Set 2 = B & LM’s qspace B
%
%	3: Get all relations between points for each set (also pointer
%	equalities?)
%
%	4: For each set of relations and each set of points determine if
%	this is new information. (Compared to the information used to
%	calculate this multiplication context before. First time: all info new)
%	Use only the new point / relation info
%
%	5: For each set:
%	     For each relation in set:
%	       For every point in the other set
%	         - Determine sign of point
%	         - Apply axioms on relation and point
%	         mult:
%	           X rel Y   &    Z  ==>   X*Z  rel  Y*Z  (sign Z is +)
%
%	           X rel Y   &    Z  ==>   Y*Z  rel  X*Z  (sign Z is -)
%
%	           X rel 1   &    Z  ==>   X*Z  rel  Z	  (sign Z is +)
%
%	           X rel 1   &    Z  ==>   Z    rel  X*Z  (sign Z is -)
%
%   DISALLOWED     X rel -1  &    Z  ==>   X*Z  rel  -Z   (sign Z is +)
%	                                   &  -Z + Z = 0
%
%   DISALLOWED     X rel -1  &    Z  ==>   -Z   rel  X*Z  (sign Z is -)
%	                                   &  -Z + Z = 0
%
%		   X rel Y    &   Z = 0      ==>   X*Z = 0  &  Y*Z = 0
%
%	         - Because X*Y is represented as a simple bitvector it
%	           must be explicitly equal to Y*X. Dont Add these
%	           equalities but put X*Y in canonical form (standard order)
%		 - For each element in result: determine if absorbing
%		   element, and simplify result w.r.t absorbing
%		   elements.
%		 - For each element in result: determine
%		   relation to 1 or -1,
%		     - simplify accordingly
%		     - optionally: derive extra relations: for every
%		       element in set: if X rel 1, then X * Y rel Y.
%
%	6: Add all results and the original multiplication to
%	mathematical model
%
%	7: Save multiplication context. Do not recalculate same
%	relations / points. Only new info on the multiplication should
%	be calculated
%
%
%	SECOND STEP: After each addition to the mathematical model
%	(add_relation_core) new info on the multiplication context could
%	have become available. So on the end of that procedure each
%	stored multiplication context is checked to see if new info is
%	known, in that case the multiplication is calculated for this
%	info. (This is the reason for the check/add argument in
%	do_multiplication_check_add/7)
%
% NB: division diw(A, B) has other axioma's:
%%		 diw:
%	           X rel Y   &    Z  ==>   X/Z  rel  Y/Z  (sign Z is +)
%	           X rel Y   &    Z  ==>   Y/Z  rel  X/Z  (sign Z is -)
%	          see in predicates.
%
%  No division by zero allowed. Even 0/0 form.
%
%
%



% a multiplication comes in (added to the model)
do_multiplication_division(Relation, Intern, Cin, Cout):-
	  do_multiplication_division_check_add(Relation, Intern, Cin, Cout, [], [], add, _).


% a multiplication is added or checked
do_multiplication_division_check_add(Relation, Intern, Cin, Cout, AddersIn, AddersOut, _Checkadd, mult):-
	%1
	parse_multiplication_division(Relation, A, B, _Z, _Rel, mult), % it is a multiplication

	%2
	find_quantity_and_landmarks(A, QuantityA, LandMarksA),
	find_quantity_and_landmarks(B, QuantityB, LandMarksB),

	canonical_multiplication(QuantityA*QuantityB, QuantityX*QuantityY), %standardize which is which, saves doing this work in all subclauses
	(
	  QuantityA == QuantityX
	->
	  LandMarksA = LandMarksX,
	  LandMarksB = LandMarksY
	;
	  LandMarksA = LandMarksY,
	  LandMarksB = LandMarksX
	),

	%3
	SetX = [QuantityX|LandMarksX],
	SetY = [QuantityY|LandMarksY],
	get_known_relations_and_signs_for_set(SetX, Cin, RelationsX, SetInfoX, AbsElsX),
	get_known_relations_and_signs_for_set(SetY, Cin, RelationsY, SetInfoY, AbsElsY),

	%4: checking of new relations to multiply.
	unknown_multiplication_relations(QuantityX, QuantityY, RelationsX, SetInfoY, RelationsXNew, SetInfoYNew, Cin), %returns only new relevant relations/points
	unknown_multiplication_relations(QuantityY, QuantityX, RelationsY, SetInfoX, RelationsYNew, SetInfoXNew, Cin), %returns only new relevant relations/points

	%5:
	multiply_set(RelationsXNew, SetInfoYNew, AbsElsY, [], ResultsX),
	multiply_set(RelationsYNew, SetInfoXNew, AbsElsX, [], ResultsY),

	% check if anything new happened...
	(
	  ResultsX = [],
	  ResultsY = []
	->
	  fail %no new calculations: exit to next clause...
	;
	  true
	),

	%6
	append(ResultsX, ResultsY, Results1),
%	list_to_set(Results1, Results2),
%	sort(Results2, Results3), % saves 20% time!
	!,
	etrace(class_check_rel, [Relation], inequality),
	filter_tautologies(Results1, Results2),
	extern_representations(Results2, ResultsExtern),
	intern_representations(ResultsExtern, ResultsIntern, Cin, Cnew1),
	sort(ResultsIntern, ResultsIntern1), % saves 50% time!
	list_to_set(ResultsIntern1, ResultsIntern2),
	append_relation_all([Intern|ResultsIntern2], Cnew1, Cnew2, _AppendNotDerivable, true, AddersIn, AddersOut),

	%6
	mark_known_multiplication(Relation, QuantityX, QuantityY, RelationsX, RelationsY, SetInfoX, SetInfoY, Cnew2, Cout).


% a division is added or checked
do_multiplication_division_check_add(Relation, Intern, Cin, Cout, AddersIn, AddersOut, _Checkadd, div):-
	%1
	parse_multiplication_division(Relation, N, Div, _Z, _Rel, div), % it is a division

	%2
	find_quantity_and_landmarks(N, QuantityN, LandMarksN),
	find_quantity_and_landmarks(Div, QuantityD, LandMarksD),

	% no canonisation: in division numerator and denomenator are fixed.


	%3
	SetN = [QuantityN|LandMarksN],
	SetD = [QuantityD|LandMarksD],
	get_known_relations_and_signs_for_set(SetN, Cin, RelationsN, SetInfoN, AbsElsN),
	get_known_relations_and_signs_for_set(SetD, Cin, RelationsD, SetInfoD, AbsElsD),

	%4: checking of new relations to multiply.
	unknown_division_relations(QuantityN/QuantityD, QuantityN, QuantityD, RelationsN, SetInfoD, RelationsNNew, SetInfoDNew, Cin), %returns only new relevant relations/points
	unknown_division_relations(QuantityN/QuantityD, QuantityD, QuantityN, RelationsD, SetInfoN, RelationsDNew, SetInfoNNew, Cin), %returns only new relevant relations/points

	cio_d(Cin, Derivable),
	cio_q(Cin, QList),
	add_signs_to_relations(RelationsNNew, AbsElsN, SetInfoN, Derivable, QList, SignedRelationsN),
	add_signs_to_relations(RelationsDNew, AbsElsD, SetInfoD, Derivable, QList, SignedRelationsD),
	%5:
	divide_set(SignedRelationsN, SetInfoDNew, AbsElsD, numerator, [], ResultsN),
	divide_set(SignedRelationsD, SetInfoNNew, AbsElsN, divisor, [], ResultsD),

	% check if anything new happened...
	(
	  ResultsN = [],
	  ResultsD = []
	->
	  fail %no new calculations: exit to next clause...
	;
	  true
	),
	!,
	etrace(class_check_rel, [Relation], inequality),
	%test for division by zero
	\+ division_by_zero(N, Div, SetInfoN, SetInfoD, Relation),

	%6
	append(ResultsN, ResultsD, Results1),
	filter_tautologies(Results1, Results2),
	extern_representations(Results2, ResultsExtern),
	intern_representations(ResultsExtern, ResultsIntern1, Cin, Cnew1),
	list_to_set(ResultsIntern1, ResultsIntern2),
	sort(ResultsIntern2, ResultsIntern3), % saves 50% time!
	append_relation_all([Intern|ResultsIntern3], Cnew1, Cnew2, _AppendNotDerivable, true, AddersIn, AddersOut),

	%6
	mark_known_division(Relation, QuantityN, QuantityD, RelationsN, RelationsD, SetInfoN, SetInfoD, Cnew2, Cout).

% multiplication already fully done, just add multiplication
%
% e.g. a*b = c is known, now high(a)*high(b) = high(c) is added
%
do_multiplication_division_check_add(_Relation, Intern, Cin, Cout, AddersIn, AddersOut, add, _):-
	append_relation(Intern, Cin, Cout, _, true, AddersIn, AddersOut).

% multiplication already done (there was no new info), check succesful
do_multiplication_division_check_add(_Relation, _Intern, Cio, Cio, Adders, Adders, check, _).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%% Subprocedures for main multiplication pred %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%	find_quantity_and_landmarks(+Point, -Quantity, -Landmarks)
%
%	- Point is a quantity or landmark
%	- Quantity is this quantity or the quantity associated with the
%	landmark.
%	- Landmarks are all landmarks on the qspace of Quantity
%

% Point = Quantity
find_quantity_and_landmarks(Point, Point, LandMarks):-
	qspace(Point, _, SpaceList, _),
	!,
	get_points_from_spaceList(SpaceList, LandMarks).

% Point = Landmark
find_quantity_and_landmarks(Point, Quantity, LandMarks):-
	qspace(Quantity, _, SpaceList, _), %maybe backtracking needed...
	get_points_from_spaceList(SpaceList, LandMarks),
	memberchk(Point, LandMarks),
	!.

% done
get_points_from_spaceList([], []).

% landmark
get_points_from_spaceList([point(Point)|Tail], [Point|NewTail]):-
	!,
	get_points_from_spaceList(Tail, NewTail).

% interval
get_points_from_spaceList([_|Tail], NewTail):-
	!,
	get_points_from_spaceList(Tail, NewTail).


division_by_zero(_N, Div, _SetInfoN, SetInfoD, Relation):-
	memberchk(pointInfo(Div, _, zero, _, _), SetInfoD),
	etrace(division_by_zero, [Relation, Div], general),
	!.

%%	unknown_multiplication_relations/7
%
%	2 options:
%	- mult never done: use all point/rel info
%	- mult done: use new info (new may be empty)
%
%	NB
%	- new pointinfo is combined with ALL rels
%	- new relinfo is combined with ALL pointinfo
%
%	multiplication/6 construct used for saving multiplication
%	context info that was already calculated in cio:
%
%	multiplication(
%		       1:QuantLeft*QuantRight,
%		         ID, in canonical form
%		       2:Relation,
%		         the original relation,
%		         needed for recalculations with new info
%		       3:RelationsLeft,
%		         left relations already multiplied
%		       4:RelationsRight,
%		         right  relations already multiplied
%		       5:SetInfoLeft,
%		         left pointinfo already multiplied
%		       6:SetInfoRight
%		         right pointinfo already multiplied
%		       )
%

unknown_multiplication_relations(QuantityA, QuantityB, RelationsA, SetInfoB, RelationsA, SetInfoB, Cin):-
	canonical_multiplication(QuantityA*QuantityB, QuantityX*QuantityY),
	cio_m(Cin, Multiplications),
	% if membercheck fails, multiplication never done: use all
	\+ memberchk(multiplication(QuantityX*QuantityY,
				    _Relation,
				    _OldRelationsLeft,
				    _OldRelationsRight,
				    _OldSetInfoLeft,
				    _OldSetInfoRight),
		     Multiplications),
	!.	%memberchk failed: all new relations and points

unknown_multiplication_relations(QuantityA, QuantityB, RelationsA, SetInfoB, RelationsOut, SetInfoOut, Cin):-
	canonical_multiplication(QuantityA*QuantityB, QuantityX*QuantityY),
	cio_m(Cin, Multiplications),
	% if membercheck fails, multiplication never done
	memberchk(multiplication(QuantityX*QuantityY,
				 _Relation,
				 OldRelationsLeft,
				 OldRelationsRight,
				 OldSetInfoLeft,
				 OldSetInfoRight),
		  Multiplications),

	% if membercheck succeeds, test if relations are same as used before

	% first standardize:
	list_to_set(RelationsA, RelationsA1),
	list_to_set(SetInfoB, SetInfoB1),
	sort(RelationsA1, RelationsA2),
	sort(SetInfoB1, SetInfoB2),

	%then the actual tests (based on which is left / right):
	(
	  QuantityA = QuantityX % A = left
	->
	  subtract( RelationsA2, OldRelationsLeft,RelationsXNew),
	  subtract(SetInfoB2, OldSetInfoRight, SetInfoYNew)

	;
	  % A = right
	  subtract(RelationsA2, OldRelationsRight, RelationsXNew),
	  subtract(SetInfoB2, OldSetInfoLeft, SetInfoYNew)
	),
	!,
	(
	  RelationsXNew = []
	->
	  (
	    SetInfoYNew = []
	  ->
	    %nothing new: use empty
	    RelationsOut = [],
	    SetInfoOut = []
	  ;
	    %new pointinfo: use new points with all relations
	    RelationsOut = RelationsA,
	    SetInfoOut = SetInfoYNew
	  )
	;
	  %new relationinfo: use new relations with all points
	  RelationsOut = RelationsXNew,
	  SetInfoOut = SetInfoB
	).


%% same for division
%

unknown_division_relations(QuantityN/QuantityD, _QuantityA, _QuantityB, RelationsA, SetInfoB, RelationsA, SetInfoB, Cin):-
	cio_m(Cin, MultiplicationsDivisions),
	% if membercheck fails, multiplication never done: use all
	\+ memberchk(division(QuantityN/QuantityD,
				    _Relation,
				    _OldRelationsLeft,
				    _OldRelationsRight,
				    _OldSetInfoLeft,
				    _OldSetInfoRight),
		     MultiplicationsDivisions),
	!.	%memberchk failed: all new relations and points

unknown_division_relations(QuantityN/QuantityD, QuantityA, QuantityB, RelationsA, SetInfoB, RelationsOut, SetInfoOut, Cin):-
	cio_m(Cin, MultiplicationsDivisions),
	% if membercheck fails, multiplication never done
	memberchk(division(QuantityN/QuantityD,
				 _Relation,
				 OldRelationsN,
				 OldRelationsD,
				 OldSetInfoN,
				 OldSetInfoD),
		  MultiplicationsDivisions),

	% if membercheck succeeds, test if relations are same as used before

	% first standardize:
	list_to_set(RelationsA, RelationsA1),
	list_to_set(SetInfoB, SetInfoB1),
	sort(RelationsA1, RelationsA2),
	sort(SetInfoB1, SetInfoB2),

	%then the actual tests (based on which is left / right):
	(
	  QuantityA = QuantityN, % A = Numerator / Top
	  QuantityB = QuantityD % B = Divisor
	->
	  subtract( RelationsA2, OldRelationsN,RelationsXNew),
	  subtract(SetInfoB2, OldSetInfoD, SetInfoYNew)

	;
	  % vice versa
	  subtract(RelationsA2, OldRelationsD, RelationsXNew),
	  subtract(SetInfoB2, OldSetInfoN, SetInfoYNew)
	),
	!,
	(
	  RelationsXNew = []
	->
	  (
	    SetInfoYNew = []
	  ->
	    %nothing new: use empty
	    RelationsOut = [],
	    SetInfoOut = []
	  ;
	    %new pointinfo: use new points with all relations
	    RelationsOut = RelationsA,
	    SetInfoOut = SetInfoYNew
	  )
	;
	  %new relationinfo: use new relations with all points
	  RelationsOut = RelationsXNew,
	  SetInfoOut = SetInfoB
	).



% store multiplication in cio to not calculate this multiplication
% context again... (until new facts known)
%
% for explanation on multiplication construct, see
% unknown_multiplication_relations/7 above
%
mark_known_multiplication(Relation, QuantityX, QuantityY, RelationsXin, RelationsYin, SetInfoXin, SetInfoYin, Cin, Cout):-
	cio_m_nm(Cin, Cout, Multiplications, NewMultiplications),
	list_to_set(RelationsXin, RelationsXin1),
	list_to_set(RelationsYin, RelationsYin1),
	list_to_set(SetInfoXin, SetInfoXin1),
	list_to_set(SetInfoYin, SetInfoYin1),
	sort(RelationsXin1, RelationsX),
	sort(RelationsYin1, RelationsY),
	sort(SetInfoXin1, SetInfoXin2),
	sort(SetInfoYin1, SetInfoYin2),
	strip_pointers_from_setinfo(SetInfoXin2, SetInfoX), % pointers may change
	strip_pointers_from_setinfo(SetInfoYin2, SetInfoY),
	canonical_multiplication(QuantityX*QuantityY, QuantityLeft*QuantityRight),
	(
	  QuantityLeft == QuantityX
	->
	  RelationsLeft = RelationsX,
	  SetInfoLeft = SetInfoX,
	  RelationsRight = RelationsY,
	  SetInfoRight = SetInfoY
	;
	  RelationsLeft = RelationsY,
	  SetInfoLeft = SetInfoY,
	  RelationsRight = RelationsX,
	  SetInfoRight = SetInfoX
	),
	(
	  select(multiplication(QuantityLeft*QuantityRight, FirstRelation, _, _, _, _), Multiplications, Rest)
	->
	  NewMultiplications = [multiplication(QuantityLeft*QuantityRight, FirstRelation, RelationsLeft, RelationsRight, SetInfoLeft, SetInfoRight)|Rest]
	;
	  NewMultiplications = [multiplication(QuantityLeft*QuantityRight, Relation, RelationsLeft, RelationsRight, SetInfoLeft, SetInfoRight)|Multiplications]
	).

strip_pointers_from_setinfo([], []).
strip_pointers_from_setinfo([pointInfo(Q, _BV, RZ, AE, RO)|T], [pointInfo(Q, _, RZ, AE, RO)|NT]):-
	strip_pointers_from_setinfo(T, NT).


% store division in cio to not calculate this division
% context again... (until new facts known)
%
% for explanation on division construct, see
% unknown_multiplication_relations/7 above
%
mark_known_division(Relation, QuantityN, QuantityD, RelationsNin, RelationsDin, SetInfoNin, SetInfoDin, Cin, Cout):-
	cio_m_nm(Cin, Cout, Multiplications, NewMultiplications),
	list_to_set(RelationsNin, RelationsNin1),
	list_to_set(RelationsDin, RelationsDin1),
	list_to_set(SetInfoNin, SetInfoNin1),
	list_to_set(SetInfoDin, SetInfoDin1),
	sort(RelationsNin1, RelationsN),
	sort(RelationsDin1, RelationsD),
	sort(SetInfoNin1, SetInfoNin2),
	sort(SetInfoDin1, SetInfoDin2),
	strip_pointers_from_setinfo(SetInfoNin2, SetInfoN), % pointers may change
	strip_pointers_from_setinfo(SetInfoDin2, SetInfoD),
	(
	  select(division(QuantityN/QuantityD, FirstRelation, _, _, _, _), Multiplications, Rest)
	->
	  NewMultiplications = [division(QuantityN/QuantityD, FirstRelation, RelationsN, RelationsD, SetInfoN, SetInfoD)|Rest]
	;
	  NewMultiplications = [division(QuantityN/QuantityD, Relation, RelationsN, RelationsD, SetInfoN, SetInfoD)|Multiplications]
	).


%%	get_known_relations_and_signs_for_set(+PointList, +Cio, -Relations, -Signs)
%
%	construct list of pointInfo/5 constructs
%
%	pointInfo(Point, Bitvector, Sign, AbsorbingElement, RelationToOne)
%
get_known_relations_and_signs_for_set(Points, Cin, Relations, SetInfo, AbsorbingElements):-
	cio_q(Cin, QuantityList),
	cio_d(Cin, Derivable),
	% get bitvectors for points
	points_to_bitvectors(Points, QuantityList, SetInfo1),

	% add sign info
	get_signs_set(SetInfo1, Derivable, SetInfo2),

	% add absorbing element info
	get_absorbing_elements_set(SetInfo2, SetInfo, AbsorbingElements),

	% get all known relations to one for the whole set
	( %if there is a pointer for one...
	  memberchk(value(one)/I1, QuantityList),
	  list_map([I1], BV1)
	->  % ...then get all relevant relations in both directions from derivable
	  findall(relation(PointX, Rel, one), (member(relation(X, Rel, BV1), Derivable),
					       memberchk(pointInfo(PointX, X, _, _, _), SetInfo)),
		  Relations1a),
	  findall(relation(one, Rel, PointY), (member(relation(BV1, Rel, Y), Derivable),
					       memberchk(pointInfo(PointY, Y, _, _, _), SetInfo)),
		  Relations1b),
	  append(Relations1a, Relations1b, Relations1c),
	  % and from pointer equalities in quantity list
	  findall(relation(PointX, =, one), (member(value(PointX)/I1, QuantityList),
					     PointX \= one,
					     memberchk(pointInfo(PointX, _, _, _, _), SetInfo)),
		  Relations1d),
	  append(Relations1c, Relations1d, Relations1)

	;
	  Relations1 = []
	),

	% get all known relations for the whole set
	findall(relation(PointX, Rel, PointY), (member(relation(X, Rel, Y), Derivable),
				      memberchk(pointInfo(PointX, X, _, _, _), SetInfo),
				      memberchk(pointInfo(PointY, Y, _, _, _), SetInfo)),
		Relations2),
	%get all known pointer equality relations for the whole set
	findall(relation(PointX, =, PointY), (select(pointInfo(PointX, BV, _, _, _), SetInfo, Rest),
					      member(pointInfo(PointY, BV, _, _, _), Rest),
					      PointX \= PointY),
		Relations3),

	append(Relations1, Relations2, Relations12),
	append(Relations12, Relations3, Relations4),
	apply_cannonical_form(Relations4, Relations5),
	list_to_set(Relations5, Relations).



% list of points to pointInfo structs with bitvectors
points_to_bitvectors([], _, []).

points_to_bitvectors([Point|T], QuantityList, [pointInfo(Point, BitVector, _, _, notInUse)|NT]):-
	memberchk(value(Point)/I, QuantityList),
	list_map([I], BitVector),
	!,
	points_to_bitvectors(T, QuantityList, NT).

points_to_bitvectors([zero|T], QuantityList, [pointInfo(zero, BitVector, _, _, notInUse)|NT]):-
	memberchk(zero/I, QuantityList),
	list_map([I], BitVector),
	points_to_bitvectors(T, QuantityList, NT).



% add sign to pointinfo
get_signs_set([], _, []).

get_signs_set([pointInfo(P, BV, _, AE, RO)|T], Derivable, [pointInfo(P, BV, Sign, AE, RO)|NT]):-
	get_quantity_sign(BV, Derivable, Sign),
	!,
	get_signs_set(T, Derivable, NT).

% no sign known yet...
get_signs_set([pointInfo(P, BV, _, AE, RO)|T], Derivable, [pointInfo(P, BV, unknown, AE, RO)|NT]):-
	get_signs_set(T, Derivable, NT).



% add absorbing element to pointinfo
get_absorbing_elements_set([], [], []).

% for now: no other absorbing elements then zero
get_absorbing_elements_set([pointInfo(P, BV, S, _, RO)|T], [pointInfo(P, BV, S, AbsEl, RO)|NT], AEList):-
	(
	  P = zero
	->
	  AbsEl = true,
	  AEList = [P|NAE]
	;
	  AbsEl = false,
	  AEList = NAE
	),
	get_absorbing_elements_set(T, NT, NAE).

% for division also the signs of the elements in the relations are
% relevant...
% includes absorbing elements... NO: these are inactive, since they are
% not in interface
%
% and includes pointers. These are better for judging equality.
%
add_signs_to_relations([], _, _SetInfoNNew, _, _, []).

add_signs_to_relations([relation(A, Rel, B)|T], AbsEls, SetInfo, Derivable, QList, [relation(A, BvA, Rel, B, BvB)/SignA/SignB|NT]):-
	relation_element_sign(A, AbsEls, SetInfo, Derivable, QList, SignA, BvA),
	relation_element_sign(B, AbsEls, SetInfo, Derivable, QList, SignB, BvB),
	add_signs_to_relations(T, AbsEls, SetInfo, Derivable, QList, NT).


%relation_element_sign(Point, AbsEls, _SetInfo, _, _, zero):-
%	memberchk(Point, AbsEls),
%	!.

relation_element_sign(Point, _AbsEls, SetInfo, _, _, Sign, BitVector):-
	memberchk(pointInfo(Point, BitVector, Sign, _, _), SetInfo),
	!.

relation_element_sign(zero, _AbsEls, _SetInfo, Derivable, QuantityList, Sign, BitVector):-
	memberchk(zero/I, QuantityList),
	list_map([I], BitVector),
	get_quantity_sign(BitVector, Derivable, Sign),
	!.

relation_element_sign(Point, _AbsEls, _SetInfo, Derivable, QuantityList, Sign, BitVector):-
	memberchk(value(Point)/I, QuantityList),
	list_map([I], BitVector),
	get_quantity_sign(BitVector, Derivable, Sign),
	!.


%%	multiply_set(+RelationsList, +PointInfoList, +AbsorbingElements, +ResultRelationsAccumulator,-ResultRelationsOut)
%
%	The main multiplication loop once all info is found
%	(see step 5 above)
%

multiply_set([], _PointInfo, _, ResultsIn, ResultsOut):-
	list_to_set(ResultsIn, ResultsOut). % is this necessary?

%	     For each relation in set...
multiply_set([Relation|T], PointInfo, AbsEls, ResultsIn, ResultsOut):-
	multiply_relation(PointInfo, Relation, AbsEls, [], RelationResults),
	append(RelationResults, ResultsIn, Results),
	multiply_set(T, PointInfo, AbsEls, Results, ResultsOut).


%	       For every point in the other set...
multiply_relation([], _Relation, _, Results, Results):-!.

multiply_relation([pointInfo(Point, _, Sign, _, _)|T], Relation, AbsEls, ResultsIn, ResultsOut):-
	multiply(Relation, Point, Sign, ResultRelations),
	!,
	simplify_constant_one_rels(ResultRelations, Simplified1), % not necessary anymore, new axioms do this...
	simplify_absorbing_element_rels(Simplified1, AbsEls, Simplified2),
	append(Simplified2, ResultsIn, Results),
	multiply_relation(T, Relation, AbsEls, Results, ResultsOut).

%% multiply with point with unknown sign is not possible untill sign is
% known
multiply_relation([pointInfo(_Point, _, unknown, _, _)|T], Relation, AbsEls, ResultsIn, ResultsOut):-
	multiply_relation(T, Relation, AbsEls, ResultsIn, ResultsOut).





% The actual multiply rules / axioms
% #1	           X rel Y   &    Z  ==>   X*Z  rel  Y*Z  (sign Z is +)
%
% #2	           X rel Y   &    Z  ==>   Y*Z  rel  X*Z  (sign Z is -)
%
% #3	           X rel 1   &    Z  ==>   X*Z  rel  Z	  (sign Z is +)
%
% #4	           X rel 1   &    Z  ==>   Z    rel  X*Z  (sign Z is -)
%
% #5	    (      X rel -1  &    Z  ==>   X*Z + Z rel 0  (sign Z is +))
%
% #6	    (      X rel -1  &    Z  ==>   0 rel X*Z + Z  (sign Z is -))
%
% #7		   X rel Y    &   Z = 0      ==>   X*Z = 0  &  Y*Z = 0
%

% #3
multiply(relation(A, Rel, one), Q, pos, [relation(A*Q, Rel, Q)]).
% #3
multiply(relation(one, Rel, A), Q, pos, [relation(Q, Rel, A*Q)]).
% #4
multiply(relation(A, Rel, one), Q, neg, [relation(Q, Rel, A*Q)]).
% #4
multiply(relation(one, Rel, A), Q, neg, [relation(A*Q, Rel, Q)]).
% #5
 %multiply(relation(A, Rel, minusone), Q, pos, [relation(plus(A*Q, Q), Rel, zero)]). Ook deze vormen zijn computationeel nog veel te duur.
% #5
 %multiply(relation(minusone, Rel, A), Q, pos, [relation(zero, Rel, plus(A*Q, Q))]).
% #6
 %multiply(relation(minusone, Rel, A), Q, neg, [relation(plus(A*Q, Q), Rel, zero)]).
% #6
 %multiply(relation(A, Rel, minusone), Q, neg, [relation(zero, Rel, plus(A*Q, Q))]).
% #7
multiply(relation(A, _Rel, B), Q, zero, [relation(A*Q, =, zero), relation(B*Q, =, zero)]).
% #1 and #2 at bottom because they are generic. 3-6 should match first.
% #1
multiply(relation(A, Rel, B), Q, pos, [relation(A*Q, Rel, B*Q)]).
% #2
multiply(relation(A, Rel, B), Q, neg, [relation(B*Q, Rel, A*Q)]).

/* COMPUTATIONEEL TE DUUR!
% The actual multiply rules / axioms
% #1	           X rel Y   &    Z  ==>   X*Z  rel  Y*Z  (sign Z is +)
%
% #2	           X rel Y   &    Z  ==>   Y*Z  rel  X*Z  (sign Z is -)
%
% #3	           X rel 1   &    Z  ==>   X*Z  rel  Z	  (sign Z is +)
%
% #4	           X rel 1   &    Z  ==>   Z    rel  X*Z  (sign Z is -)
%
% #5	           X rel -1  &    Z  ==>   X*Z  rel  -Z   (sign Z is +)
%	                                   &  -Z + Z = 0
%
% #6	           X rel -1  &    Z  ==>   -Z   rel  X*Z  (sign Z is -)
%	                                   &  -Z + Z = 0
%
% #7		   X rel Y    &   Z = 0      ==>   X*Z = 0  &  Y*Z = 0
%

% #3
multiply(relation(A, Rel, one), Q, pos, [relation(A*Q, Rel, Q)]).
% #3
multiply(relation(one, Rel, A), Q, pos, [relation(Q, Rel, A*Q)]).
% #4
multiply(relation(A, Rel, one), Q, neg, [relation(Q, Rel, A*Q)]).
% #4
multiply(relation(one, Rel, A), Q, neg, [relation(A*Q, Rel, Q)]).
% #5
multiply(relation(A, Rel, minusone), Q, pos, [relation(A*Q, Rel, MinQ), relation(plus(MinQ, Q), =, zero)]):-
	%add internal representation of -Q
	Q =.. [H|T],
	(
	  atom_concat(minus, X, H)
	->  % minus minus => equals out to nothing
	  X = NH
	;
	  atomic_list_concat([minus, H], NH)
	),
	MinQ =.. [NH|T].
% #5
multiply(relation(minusone, Rel, A), Q, pos, [relation(MinQ, Rel, A*Q), relation(plus(MinQ, Q), =, zero)]):-
	%add internal representation of -Q
	Q =.. [H|T],
	(
	  atom_concat(minus, X, H)
	->  % minus minus => equals out to nothing
	  X = NH
	;
	  atomic_list_concat([minus, H], NH)
	),
	MinQ =.. [NH|T].
% #6
multiply(relation(A, Rel, minusone), Q, neg, [relation(MinQ, Rel, A*Q), relation(plus(MinQ, Q), =, zero)]):-
	%add internal representation of -Q
	Q =.. [H|T],
	(
	  atom_concat(minus, X, H)
	->  % minus minus => equals out to nothing
	  X = NH
	;
	  atomic_list_concat([minus, H], NH)
	),
	MinQ =.. [NH|T].
% #6
multiply(relation(minusone, Rel, A), Q, neg, [relation(A*Q, Rel, MinQ), relation(plus(MinQ, Q), =, zero)]):-
	%add internal representation of -Q
	Q =.. [H|T],
	(
	  atom_concat(minus, X, H)
	->  % minus minus => equals out to nothing
	  X = NH
	;
	  atomic_list_concat([minus, H], NH)
	),
	MinQ =.. [NH|T].
% #7
multiply(relation(A, _Rel, B), Q, zero, [relation(A*Q, =, zero), relation(B*Q, =, zero)]).
% #1 and #2 at bottom because they are generic. 3-6 should match first.
% #1
multiply(relation(A, Rel, B), Q, pos, [relation(A*Q, Rel, B*Q)]).
% #2
multiply(relation(A, Rel, B), Q, neg, [relation(B*Q, Rel, A*Q)]).
*/




simplify_constant_one_rels([], []).
simplify_constant_one_rels([Rel|T], [NewRel|NT]):-
	simplify_constant_one(Rel, NewRel),
	simplify_constant_one_rels(T, NT).


%%	simplify_constant_one
%
%	multiplications with the constant 1
%
%	e.g. Q * 1 => Q
%
simplify_constant_one(relation(L, Rel, R), relation(NewL, Rel, NewR)):-
	simplify_constant_one_part(L, NewL),
	simplify_constant_one_part(R, NewR).


simplify_constant_one_part(X*one, X):-
	!.

simplify_constant_one_part(one*Y, Y):-
	!.

%no 1's...
simplify_constant_one_part(Rel, Rel).




simplify_absorbing_element_rels([], _AbsEls, []).
simplify_absorbing_element_rels([Rel|T], AbsEls, [NewRel|NT]):-
	simplify_absorbing_element(Rel, AbsEls, NewRel),
	simplify_absorbing_element_rels(T, AbsEls, NT).

%%	simplify_absorbing_element
%
%	multiplications with an absorbing element are equal to the
%	absorbing element
%
%	e.g. 5 * 0 => 0
%
%	also puts in canonical form if no simplification occurs
%
simplify_absorbing_element(relation(L, Rel, R), AbsEls, relation(NewL, Rel, NewR)):-
	simplify_absorbing_element_part(L, AbsEls, NewL),
	simplify_absorbing_element_part(R, AbsEls, NewR).


simplify_absorbing_element_part(X*_Y, AbsEls, X):-
	memberchk(X, AbsEls),
	!.

simplify_absorbing_element_part(_X*Y, AbsEls, Y):-
	memberchk(Y, AbsEls),
	!.

% no simplification but still a mult
simplify_absorbing_element_part(X*Y, _AbsEls, Canonical):-
	!,
	canonical_multiplication(X*Y, Canonical).

% already simplified (constant one probably)
simplify_absorbing_element_part(X, _AbsEls, X).

% Because X*Y is represented as a simple bitvector it must be explicitly
% equal to Y*X. To circumvent, put X*Y in canonical form (standard
% order)



% filter out obvious things like zero=zero.
% also checks for contradictions.
%
filter_tautologies([], []).

filter_tautologies([relation(X, =, X)|T], NT):-
	!,
	filter_tautologies(T, NT).

filter_tautologies([relation(X, Rel, X)|_T], _NT):-
	memberchk(Rel, [<, >]),
	write('\n*** Warning: multiplication found contradiction ***\n'),
	!,
	fail.
%	filter_tautologies(T, NT).

filter_tautologies([relation(X, Rel, Y)|T], [relation(X, Rel, Y)|NT]):-
	!,
	filter_tautologies(T, NT).



%%	divide_set(+RelationsList, +PointInfoList,
%	+AbsorbingElements,
%	+ResultRelationsAccumulator,-ResultRelationsOut)
%
%	The main division loop once all info is found
%	(see step 5 above)
%

divide_set([], _PointInfo, _, _, ResultsIn, ResultsOut):-
	list_to_set(ResultsIn, ResultsOut). % is this necessary?

%	     For each relation in set...
divide_set([Relation|T], PointInfo, AbsEls, NDFlag, ResultsIn, ResultsOut):-
	divide_relation(PointInfo, Relation, AbsEls, NDFlag, [], RelationResults),
	append(RelationResults, ResultsIn, Results),
	divide_set(T, PointInfo, AbsEls, NDFlag, Results, ResultsOut).


%	       For every point in the other set...
divide_relation([], _Relation, _, _, Results, Results):-!.

divide_relation([pointInfo(Point, BitVector, Sign, _, _)|T], Relation/SignA/SignB, AbsEls, NDFlag, ResultsIn, ResultsOut):-
	divide(Relation, Point, BitVector, SignA, SignB, Sign, NDFlag, ResultRelations),
	!,
	append(ResultRelations, ResultsIn, Results),
	divide_relation(T, Relation/SignA/SignB, AbsEls, NDFlag, Results, ResultsOut).


%% multiply with point with unknown sign is not possible untill sign is
% known
divide_relation([pointInfo(_Point, _, unknown, _, _)|T], Relation, AbsEls, NDFlag, ResultsIn, ResultsOut):-
	divide_relation(T, Relation, AbsEls, NDFlag, ResultsIn, ResultsOut).






%%	divide(+relation(A, Rel, B), +Q, +SignA, +SignB, +SignQ, +Flag, -ResultRels)
%
%	The actual division rules / axioms
%
%
% #Start (with unknown signs: no conclusions)
divide(relation(_A, _BvA, _Rel, _B, _BvB), _Q, _BvQ, unknown, _, _, _, []):-!.
divide(relation(_A, _BvA, _Rel, _B, _BvB), _Q, _BvQ, _, unknown, _, _, []):-!.
divide(relation(_A, _BvA, _Rel, _B, _BvB), _Q, _BvQ, _, _, unknown, _, []):-!.

% Relations about the numerator / top part of the quotient: A/Q and B/Q
% Only the sign of the divisor (Q) is relevant.
%
%#0a: division by zero: no result,
% (In future: unless l'hopital: N also zero.)
% prohibition of div by zero only of the main relation.
%
divide(relation(_A, _BvA, _Rel, _B, _BvB), _Q, _BvQ, _SignA, _SignB, zero, numerator, []):-!.
/*%as of now unclear what to do with 0/0, just return [] results.
	!,
	(
	  SignA = zero % 0/0
	->
	  ResultA = [relation(A/zero, =, ?)]
	;
	  ResultA = []
	),
	(
	  fail, %as of now unclear what to do with 0/0
	  SignB = zero % 0/0
	->
	  ResultB = [relation(B/zero, =, ?)]
	;
	  ResultB = []
	),
	append(ResultA, ResultB, Results).
*/

% #0b: division by one
divide(relation(A, _BvA, Rel, B, _BvB), one, _BvQ, _, _, _, numerator, [relation(A/one, Rel, B/one),
						      relation(A/one, =, A),
						      relation(B/one, =, B)]):-!.
% #1a B = Q (given bitvector equal)
divide(relation(A, _BvA, Rel, B, Bv), Q, Bv, _, _, pos, numerator, [relation(A/Q, Rel, one), relation(B/Q, =, one)]):-!.
% #1b A = Q (given bitvector equal)
divide(relation(A, Bv, Rel, B, _BvB), Q, Bv, _, _, pos, numerator, [relation(one, Rel, B/Q), relation(A/Q, =, one)]):-!.
% #2a B = Q (given bitvector equal)
divide(relation(A, _BvA, Rel, B, Bv), Q, Bv, _, _, neg, numerator, [relation(one, Rel, A/Q), relation(B/Q, =, one)]):-!.
% #2b A = Q (given bitvector equal)
divide(relation(A, Bv, Rel, B, _BvB), Q, Bv, _, _, neg, numerator, [relation(B/Q, Rel, one), relation(A/Q, =, one)]):-!.
% #3
divide(relation(A, _BvA, Rel, B, _BvB), Q, _BvQ, _, _, pos, numerator, [relation(A/Q, Rel, B/Q)]):-!.
% #4
divide(relation(A, _BvA, Rel, B, _BvB), Q, _BvQ, _, _, neg, numerator, [relation(B/Q, Rel, A/Q)]):-!.
%
%
% Relations about the divisor / bottom part of the quotient: Q/A and Q/B
% The sign of A and B and Q is relevant
%
% Q = pos: reverse Rel when signA = signB
%
% Q = neg: reverse Rel when signA =/= signB
%


% #0a
divide(relation(zero, _BvA, _Rel, zero, _BvB), _Q, _BvQ, zero, zero, _, divisor, []):-!.
% #0b
divide(relation(_A, _BvA, _Rel, zero, _BvB), _Q, _BvQ, _, zero, _, divisor, []):-!.
% #0c
divide(relation(zero, _BvA, _Rel, _B, _BvB), _Q, _BvQ, zero, _, _, divisor, []):-!.

% #3b
% #1a1 B = Q (given bitvector equal)
divide(relation(A, _BvA, Rel, B, Bv), Q, Bv, pos, pos, pos, divisor, [relation(one, Rel, Q/A), relation(Q/B, =, one)]):-!.
% #1a2 A = Q (given bitvector equal)
divide(relation(A, Bv, Rel, B, _BvB), Q, Bv, pos, pos, pos, divisor, [relation(Q/B, Rel, one), relation(Q/A, =, one)]):-!.
% #1b1 B = Q (given bitvector equal)
divide(relation(A, _BvA, Rel, B, Bv), Q, Bv, neg, pos, pos, divisor, [relation(Q/A, Rel, one), relation(Q/B, =, one)]):-!.
% #1b2 A = Q (given bitvector equal)
divide(relation(A, Bv, Rel, B, _BvB), Q, Bv, pos, neg, pos, divisor, [relation(one, Rel, Q/B), relation(Q/A, =, one)]):-!.

% #2a1 B = Q (given bitvector equal)
divide(relation(A, _BvA, Rel, B, Bv), Q, Bv, neg, neg, neg, divisor, [relation(Q/A, Rel, one), relation(Q/B, =, one)]):-!.
% #2a2 A = Q (given bitvector equal)
divide(relation(A, Bv, Rel, B, _BvB), Q, Bv, neg, neg, neg, divisor, [relation(one, Rel, Q/B), relation(Q/A, =, one)]):-!.
% #2b1 B = Q (given bitvector equal)
divide(relation(A, _BvA, Rel, B, Bv), Q, Bv, pos, neg, neg, divisor, [relation(one, Rel, Q/A), relation(Q/B, =, one)]):-!.
% #2b2 A = Q (given bitvector equal)
divide(relation(A, Bv, Rel, B, _BvB), Q, Bv, neg, pos, neg, divisor, [relation(Q/B, Rel, one), relation(Q/A, =, one)]):-!.

% #3a
divide(relation(A, _BvA, Rel, B, _BvB), Q, _BvQ, Sign, Sign, pos, divisor, [relation(Q/B, Rel, Q/A)]):-!.
% #3b
divide(relation(A, _BvA, Rel, B, _BvB), Q, _BvQ, _, _, pos, divisor, [relation(Q/A, Rel, Q/B)]):-!.

% #4a
divide(relation(A, _BvA, Rel, B, _BvB), Q, _BvQ, Sign, Sign, neg, divisor, [relation(Q/A, Rel, Q/B)]):-!.
% #4b
divide(relation(A, _BvA, Rel, B, _BvB), Q, _BvQ, _, _, neg, divisor, [relation(Q/B, Rel, Q/A)]):-!.

% #5
divide(relation(A, _BvA, _Rel, B, _BvB), Q, _BvQ, _, _, zero, divisor, [relation(Q/A, =, Q/B), relation(Q/A, =, zero), relation(Q/B, =, zero)]):-!.


% FL april 2012: added rule #3 for deriving zero equalities.
% #3
% divide(relation(A, _Rel, B), Q, zero, [relation(A/Q, =, zero), relation(B/Q, =, zero)]).
%
% division by zero not allowed off course.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% multiplication and division parsing %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%	is_multiplication(+Relation)
%
%	Tests if relation in normal notation is multiplication
%	NB a double multiplication is not yet allowed
%
is_multiplication_division(Relation):-
	parse_multiplication_division(Relation, _X, _Y, _Z, _Rel, _).


%%	parse_multiplication(+Relation, -X, -Y, -Z, -Rel)
%
%	succeeds with X and Y the multiplied parts,
%	Z the related part and Rel the type of relation
%
parse_multiplication_division(greater(L, R), X, Y, Z, >, MD):-
	has_mult_div_part(L, R, X, Y, Z, MD).

parse_multiplication_division(greater_or_equal(L, R), X, Y, Z, >=, MD):-
	has_mult_div_part(L, R, X, Y, Z, MD).

parse_multiplication_division(equal(L, R), X, Y, Z, =, MD):-
	has_mult_div_part(L, R, X, Y, Z, MD).

parse_multiplication_division(smaller_or_equal(L, R), X, Y, Z, =<, MD):-
	has_mult_div_part(L, R, X, Y, Z, MD).

parse_multiplication_division(smaller(L, R), X, Y, Z, <, MD):-
	has_mult_div_part(L, R, X, Y, Z, MD).


has_mult_div_part(L, R, X, Y, R, mult):-
	multiplication_part(L, X, Y),
	\+ multiplication_part(R, _, _),
	\+ division_part(R, _, _),
	!.

has_mult_div_part(L, R, X, Y, L, mult):-
	multiplication_part(R, X, Y),
	\+ multiplication_part(L, _, _),
	\+ division_part(L, _, _),
	!.

has_mult_div_part(L, R, T, N, R, div):-
	division_part(L, T, N),
	\+ multiplication_part(R, _, _),
	\+ division_part(R, _, _),
	!.

has_mult_div_part(L, R, T, N, L, div):-
	division_part(R, T, N),
	\+ multiplication_part(L, _, _),
	\+ division_part(L, _, _),
	!.

multiplication_part(mult(X, Y), X, Y).

division_part(diw(Enum, Denom), Enum, Denom).




%%	extern_representation(+MultResultRelation, -ExternRelation)
%
%	Multiplication generates new relations in intermediate form:
%
%	   relation(L, Rel, R) (NB no bitmaps as internal form)
%
%	these need to be put in external form:
%
%	   greater(zero, multiply(lowest(Q1),highest(Q2)))
%
%	Procedure has the assumption that multiplicated elements are
%	themselves atomic and not nested structures or multiplications
%	etc etc.
%
%
extern_representation(relation(L, Rel, R), Extern):-
	translate_rel_element(Rel, Term),
	translate_element(L, NewL),
	translate_element(R, NewR),
	Extern =.. [Term, NewL, NewR].



translate_rel_element(>, greater):-!.
translate_rel_element(>=, greater_or_equal):-!.
translate_rel_element(=, equal):-!.
translate_rel_element(<, smaller):-!.
translate_rel_element(=<, smaller_or_equal):-!.

translate_element(A*B, mult(A, B)):-!.
translate_element(Q, Q):-!.



extern_representations([], []).
extern_representations([Relation|T], [Extern|NT]):-
	extern_representation(Relation, Extern),
	extern_representations(T, NT).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%% Subprocedures for multiplication checking %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%	inspect_multiplications/5 (And divisions)
%
%	redo calculation if new relevant relations become known
%	Called at end of solve procedure
%
%	But only executed if it is not an appendrelationsall call this
%	is because append all is used by mult, causes unnecessary
%	rechecking of mult context during addition of its results
%
inspect_multiplications(true, Cin, Cout, DoAS, AddersIn, AddersOut):-
	!,
	cio_m(Cin, Multiplications),
	inspect_multiplication(Multiplications, Cin, Cout, DoAS, AddersIn, AddersOut).

inspect_multiplications(_, Cio, Cio, _DoAS, Adders, Adders).


% no more multiplications / divisions to inspect, done
inspect_multiplication([], Cio, Cio, _DoAS, Adders, Adders):-!.

% take multiplication context and check it
% (Same procedure as adding, automatically checks for new info in landmarks or relations.)
inspect_multiplication([multiplication(_QuantityLeftStarQuantityRight,
				       Relation,
				       _RelationsLeft,
				       _RelationsRight,
				       _SetInfoLeft,
				       _SetInfoRight)|Tail],
			Cin, Cout, DoAS, AddersIn, AddersOut):-
	intern_representation(Relation, Intern, Cin, Cnew1),
	do_multiplication_division_check_add(Relation, Intern, Cnew1, Cnew2, AddersIn, Adders, check, mult),
	inspect_multiplication(Tail, Cnew2, Cout, DoAS, Adders, AddersOut).

% take division context and check it
% (Same procedure as adding, automatically checks for new info in landmarks or relations.)
inspect_multiplication([division(_QuantityNSlashQuantityD,
				       Relation,
				       _RelationsLeft,
				       _RelationsRight,
				       _SetInfoLeft,
				       _SetInfoRight)|Tail],
			Cin, Cout, DoAS, AddersIn, AddersOut):-
	intern_representation(Relation, Intern, Cin, Cnew1),
	do_multiplication_division_check_add(Relation, Intern, Cnew1, Cnew2, AddersIn, Adders, check, div),
	inspect_multiplication(Tail, Cnew2, Cout, DoAS, Adders, AddersOut).



/* not needed, but code works fine!
get_all_pointer_equalities(Cin, Equalities):-
	cio_q(Cin, Quantities),
	findall(equal(A, B),
		(
		select(X/I, Quantities, Rest),
		member(Y/I, Rest),
		sort([X, Y], [A, B])
		),
		Equalities1),
	list_to_set(Equalities1, Equalities).
*/



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Influence Resolution: P* P/ stuff %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



contains_p_star_p_slash(AddList):-
	memberchk(pos_neg_mult(_), AddList),
	!.

contains_p_star_p_slash(AddList):-
	memberchk(pos_neg_diw(_), AddList),
	!.


% NB addlist and traddlist should be in same order...
% code depends on it (like in all of influence resolution code)
%
translate_p_star_p_slash(AddListIn, TrAddListIn, Qin, Derivable, TrQ, AddListOut, TrAddListOut):-
	sort_p_types(AddListIn, Pstar, Pslash, Pother),
	sort_p_types(TrAddListIn, TrPstar, TrPslash, TrPother),
	warning_for_bad_pattern(TrPstar, TrPslash, TrQ), % warn if pattern is bad
	get_addlist_val_der_signs(Pstar, TrPstar, Qin, Derivable, PstarSigns),
	get_addlist_val_der_signs(Pslash, TrPslash, Qin, Derivable, PslashSigns),
	!, % all needed signs are known. Do translation of P* and P/
	translate_p_star(Pstar, TrPstar,
			 PstarSigns, PslashSigns,
			 PstarNew, TrPstarNew),
	translate_p_slash(Pslash, TrPslash,
			  PstarSigns, PslashSigns,
			  PslashNew, TrPslashNew),
	append(PstarNew, PslashNew, Pnew),
	append(Pnew, Pother, AddListOut),
	append(TrPstarNew, TrPslashNew, TrPnew),
	append(TrPnew, TrPother, TrAddListOut).



sort_p_types([], [], [], []).

sort_p_types([pos_neg_mult(X)|T], [pos_neg_mult(X)|Pstar], Pslash, Pother):-
	!,
	sort_p_types(T, Pstar, Pslash, Pother).

sort_p_types([pos_neg_diw(X)|T], Pstar, [pos_neg_diw(X)|Pslash], Pother):-
	!,
	sort_p_types(T, Pstar, Pslash, Pother).

sort_p_types([H|T], Pstar, Pslash, [H|Pother]):-
	!,
	sort_p_types(T, Pstar, Pslash, Pother).




get_addlist_val_der_signs([], [], _Qin, _Derivable, []).


get_addlist_val_der_signs([pos_neg_mult(BitVectDerQ)|Pstar],
			  [pos_neg_mult(derivative(Q))|TrPstar],
			  Qin, Derivable,
			  [Q/SignVal/SignDer|PstarSigns]):-
	list_map([0], BitVectZero), % NB for different Absorbing elements a different zero is needed!
	intern_quantity(value(Q), BitVectValQ, Qin, _),
        get_quantity_sign_core(BitVectValQ, BitVectZero, Derivable, SignVal),
	get_quantity_sign_core(BitVectDerQ, BitVectZero, Derivable, SignDer),
	get_addlist_val_der_signs(Pstar, TrPstar, Qin, Derivable, PstarSigns).

get_addlist_val_der_signs([pos_neg_diw(BitVectDerQ)|Pslash],
			  [pos_neg_diw(derivative(Q))|TrPslash],
			  Qin, Derivable,
			  [Q/SignVal/SignDer|PslashSigns]):-
	list_map([0], BitVectZero), % NB for different Absorbing elements a different zero is needed!
	intern_quantity(value(Q), BitVectValQ, Qin, _),
        get_quantity_sign_core(BitVectValQ, BitVectZero, Derivable, SignVal),
	get_quantity_sign_core(BitVectDerQ, BitVectZero, Derivable, SignDer),
	get_addlist_val_der_signs(Pslash, TrPslash, Qin, Derivable, PslashSigns).


% two P*'s: good
warning_for_bad_pattern([_, _], [], _):-!.

% P* and P/: good
warning_for_bad_pattern([_], [_], _):-!.

% else: bad
warning_for_bad_pattern(TrPstar, TrPslash, TrQ):-
	etrace(solve_ir_translate_p_star_bad_pattern, [TrPstar, TrPslash, TrQ], resolve).




%%	translate_p_star
%
%	For element A of multiplication : X * Expression = Z
%
translate_p_star([], [], _PstarSigns, _PslashSigns, [], []).


translate_p_star([pos_neg_mult(BitVectDerQA)|PstarTail], [pos_neg_mult(derivative(QA))|TrPstarTail],
		 PstarSigns, PslashSigns,
		 PstarNew, TrPstarNew):-
	select(QA/SignValA/SignDerA, PstarSigns, Rest),
	append(PslashSigns, Rest, ExpressionSigns),
	get_expression_signs(ExpressionSigns, SignValE, SignDerE),
	translate_p_star_core(SignValA, SignValE, SignDerA, SignDerE,
			      BitVectDerQA, QA,
			      NewP, NewTrP),
	translate_p_star(PstarTail, TrPstarTail, PstarSigns, PslashSigns, PstarNewTail, TrPstarNewTail),
	append(NewP,PstarNewTail, PstarNew),
	append(NewTrP, TrPstarNewTail, TrPstarNew).


%%	translate_p_slash
%
%	For element A of multiplication : 1/X * Expression = Z
%	(a division factor!)
%
translate_p_slash([], [], _PstarSigns, _PslashSigns, [], []).


translate_p_slash([pos_neg_diw(BitVectDerQA)|PstarTail], [pos_neg_diw(derivative(QA))|TrPstarTail],
		 PstarSigns, PslashSigns,
		 PstarNew, TrPstarNew):-
	select(QA/SignValA/SignDerA, PslashSigns, Rest),
	append(PstarSigns, Rest, ExpressionSigns),
	get_expression_signs(ExpressionSigns, SignValE, SignDerE),
	translate_p_slash_core(SignValA, SignValE, SignDerA, SignDerE,
			      BitVectDerQA, QA,
			      NewP, NewTrP),
	translate_p_slash(PstarTail, TrPstarTail, PstarSigns, PslashSigns, PstarNewTail, TrPstarNewTail),
	append(NewP,PstarNewTail, PstarNew),
	append(NewTrP, TrPstarNewTail, TrPstarNew).





get_expression_signs(ExpressionSigns, SignValE, SignDerE):-
	v_signs(ExpressionSigns, VSigns),
	mult_signslist(VSigns, pos, SignValE),
	zero_v_d_signs(ExpressionSigns, ZVDSigns), % if values are zero, then derivatives start to matter...
	mult_signslist(ZVDSigns, pos, SignDerE).



v_signs([], []).

v_signs([_/S/_|T], [S|ST]):-
	v_signs(T, ST).



zero_v_d_signs([], []).

zero_v_d_signs([_/zero/S|T], [S|ST]):-
	!,
	zero_v_d_signs(T, ST).

%not zero value, skip.
zero_v_d_signs([_|T], ST):-
	!,
	zero_v_d_signs(T, ST).



mult_signslist([], Sign, Sign).

mult_signslist([pos|T], pos, Sign):-
	mult_signslist(T, pos, Sign).

mult_signslist([pos|T], neg, Sign):-
	mult_signslist(T, neg, Sign).

mult_signslist([pos|T], zero, Sign):-
	mult_signslist(T, zero, Sign).

mult_signslist([neg|T], neg, Sign):-
	mult_signslist(T, neg, Sign).

mult_signslist([neg|T], pos, Sign):-
	mult_signslist(T, neg, Sign).

mult_signslist([neg|T], zero, Sign):-
	mult_signslist(T, zero, Sign).

mult_signslist([zero|T], neg, Sign):-
	mult_signslist(T, zero, Sign).

mult_signslist([zero|T], pos, Sign):-
	mult_signslist(T, zero, Sign).

mult_signslist([zero|T], zero, Sign):-
	mult_signslist(T, zero, Sign).



%%	translate_p_star_core(SignValA, SignValB, SignDerA,
%	SignDerB, BitVectDerQA, QA, AddList,
%	TrAddList)
%
%	determine correct interpretation of proportionality of A
%	For element A of multiplication : X * Expression = Z
%
%	Table:
%
%	val A  val E  der A der E* : Proportionalities
%
%

%	___________________________________________________
%	A: +
%	       E: +                : P+
%	       E: -                : P-
%	       E: 0		   : none
%	A: -
%	       E: +                : PA
%	       E: -                : PA
%	       E: 0		   : none
%	A: 0
%	       E: +                : PA+
%	       E: -                : PA-
%	       E: 0
%	              dA: +
%	                     dE: + : PA+
%			     dE: - : PA-
%	                     dE: 0 : none
%	              dA: -
%	                     dE: + : PA+
%	                     dE: - : PA-
%	                     dE: 0 : none
%	              dA: 0
%	                     dE: + : PA+
%	                     dE: - : PA-
%	                     dE: 0 : PA+ (result will be zero derivative
%

%	A: +
translate_p_star_core(pos, pos, _, _,
		      BitVectDerQA, QA,
		      [pos(BitVectDerQA)],
		      [pos(derivative(QA))]):-!.
translate_p_star_core(pos, neg, _, _,
		      BitVectDerQA, QA,
		      [neg(BitVectDerQA)],
		      [neg(derivative(QA))]):-!.
translate_p_star_core(pos, zero, _, _,
		      _BitVectDerQA, _QA,
		      [],
		      []):-!.
%	A: -
translate_p_star_core(neg, pos, _, _,
		      BitVectDerQA, QA,
		      [pos(BitVectDerQA)],
		      [pos(derivative(QA))]):-!.
translate_p_star_core(neg, neg, _, _,
		      BitVectDerQA, QA,
		      [neg(BitVectDerQA)],
		      [neg(derivative(QA))]):-!.
translate_p_star_core(neg, zero, _, _,
		      _BitVectDerQA, _QA,
		      [],
		      []):-!.
%	A: 0
translate_p_star_core(zero, pos, _, _,
		      BitVectDerQA, QA,
		      [pos(BitVectDerQA)],
		      [pos(derivative(QA))]):-!.
translate_p_star_core(zero, neg, _, _,
		      BitVectDerQA, QA,
		      [neg(BitVectDerQA)],
		      [neg(derivative(QA))]):-!.
%	A: 0, B: 0
%	dA: +
translate_p_star_core(zero, zero, pos, pos,
		      BitVectDerQA, QA,
		      [pos(BitVectDerQA)],
		      [pos(derivative(QA))]):-!.
translate_p_star_core(zero, zero, pos, neg,
		      BitVectDerQA, QA,
		      [neg(BitVectDerQA)],
		      [neg(derivative(QA))]):-!.
translate_p_star_core(zero, zero, pos, zero,
		      _BitVectDerQA, _QA,
		      [],
		      []):-!.
%	dA: -
translate_p_star_core(zero, zero, neg, pos,
		      BitVectDerQA, QA,
		      [pos(BitVectDerQA)],
		      [pos(derivative(QA))]):-!.
translate_p_star_core(zero, zero, neg, neg,
		      BitVectDerQA, QA,
		      [neg(BitVectDerQA)],
		      [neg(derivative(QA))]):-!.
translate_p_star_core(zero, zero, neg, zero,
		      _BitVectDerQA, _QA,
		      [],
		      []):-!.
%	dA: 0
translate_p_star_core(zero, zero, zero, pos,
		      BitVectDerQA, QA,
		      [pos(BitVectDerQA)],
		      [pos(derivative(QA))]):-!.
translate_p_star_core(zero, zero, zero, neg,
		      BitVectDerQA, QA,
		      [neg(BitVectDerQA)],
		      [neg(derivative(QA))]):-!.
%	dA: 0, dB: 0
translate_p_star_core(zero, zero, zero, zero,
		      BitVectDerQA, QA,
		      [pos(BitVectDerQA)],
		      [pos(derivative(QA))]):-!.




%%	translate_p_slash_core(SignValD, SignValN, SignDerD,
%	SignDerN, BitVectDerQD, BitVectDerQN, QD, QN, AddList,
%	TrAddList)
%
%	determine correct interpretation of proportionality of D
%	the divisor factor:  D =    1/D * Expression = Z
%
%	Table:
%
%	val D, val E, : Proportionality D
%	___________________________________________________
%	D: +
%	       E: +   : P-
%	       E: -   : P+
%	       E: 0   : none
%	D: -
%	       E: +   : P-
%	       E: -   : P+
%	       E: 0   : none
%	D: 0
%	       undefined! : illegal state.
%


%	D: +
translate_p_slash_core(pos, pos, _, _,
		      BitVectDerQD, QD,
		      [neg(BitVectDerQD)],
		      [neg(derivative(QD))]):-!.
translate_p_slash_core(pos, neg, _, _,
		      BitVectDerQD, QD,
		      [pos(BitVectDerQD)],
		      [pos(derivative(QD))]):-!.
translate_p_slash_core(pos, zero, _, _,
		      _BitVectDerQD, _QD,
		      [],
		      []):-!.
%	D: -
translate_p_slash_core(neg, pos, _, _,
		      BitVectDerQD, QD,
		      [neg(BitVectDerQD)],
		      [neg(derivative(QD))]):-!.
translate_p_slash_core(neg, neg, _, _,
		      BitVectDerQD, QD,
		      [pos(BitVectDerQD)],
		      [pos(derivative(QD))]):-!.
translate_p_slash_core(neg, zero, _, _,
		      _BitVectDerQD, _QD,
		      [],
		      []):-!.













%%	%%%%%%%%%%%%%%%%%%%%%%% OLD CODE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




/*
% At this moment we only allow binary multiplications: A * B = C
% and therefore a P* addlist should have exactly two pos_neg_mult items
all_p_star([pos_neg_mult(_), pos_neg_mult(_)]).


% At this moment we only allow binary multiplications: A * B = C
% and therefore a P* addlist should have exactly two pos_neg_mult items
all_p_star_p_slash([pos_neg_mult(_), pos_neg_diw(_)]):-!.

all_p_star_p_slash([pos_neg_diw(_), pos_neg_mult(_)]):-!.



%%	translate_p_star(+AddList, +TrList, +Derivable, -NewAddList, -NewTrList
%
%	Qin = quantitylist from cio
%
%
translate_p_star([pos_neg_mult(BitVectDerQA), pos_neg_mult(BitVectDerQB)],
		 [pos_neg_mult(derivative(QA)), pos_neg_mult(derivative(QB))],
		 Qin, Derivable, AddList, TrAddList):-
	list_map([0], BitVectZero), % NB for different Absorbing elements a different zero is needed!
	intern_quantity(value(QA), BitVectValQA, Qin, Qnew),
	intern_quantity(value(QB), BitVectValQB, Qnew, Qnew),
        get_quantity_sign_core(BitVectValQA,  BitVectZero, Derivable, SignValA),
	get_quantity_sign_core(BitVectValQB,  BitVectZero, Derivable, SignValB),
	get_quantity_sign_core(BitVectDerQA,  BitVectZero, Derivable, SignDerA),
	get_quantity_sign_core(BitVectDerQB,  BitVectZero, Derivable, SignDerB),
        translate_p_star_core(SignValA, SignValB, SignDerA, SignDerB,
				 BitVectDerQA, BitVectDerQB, QA, QB,
				 AddList, TrAddList).





%%	translate_p_star_core(SignValA, SignValB, SignDerA,
%	SignDerB, BitVectDerQA, BitVectDerQB, QA, QB, AddList,
%	TrAddList)
%
%	determine correct interpretation of proportionalities.
%
%	Table:
%
%	val A, val B, der A, der B: Proportionalities
%	___________________________________________________
%	A: +
%	       B: +                : PA+ & PB+
%	       B: -                : PA- & PB+
%	       B: 0		   : PB+
%	A: -
%	       B: +                : PA+ & PB-
%	       B: -                : PA- & PB-
%	       B: 0		   : PB-
%	A: 0
%	       B: +                : PA+
%	       B: -                : PA-
%	       B: 0
%	              dA: +
%	                     dB: + : PA+ & PB+
%			     dB: - : PA- & PB+
%	                     dB: 0 : PB+
%	              dA: -
%	                     dB: + : PA+ & PB-
%	                     dB: - : PA- & PB-
%	                     dB: 0 : PB-
%	              dA: 0
%	                     dB: + : PA+
%	                     dB: - : PA-
%	                     dB: 0 :
%



%	A: +
translate_p_star_core(pos, pos, _, _,
		      BitVectDerQA, BitVectDerQB, QA, QB,
		      [pos(BitVectDerQA), pos(BitVectDerQB)],
		      [pos(derivative(QA)), pos(derivative(QB))]):-!.
translate_p_star_core(pos, neg, _, _,
		      BitVectDerQA, BitVectDerQB, QA, QB,
		      [neg(BitVectDerQA), pos(BitVectDerQB)],
		      [neg(derivative(QA)), pos(derivative(QB))]):-!.
translate_p_star_core(pos, zero, _, _,
		      _BitVectDerQA, BitVectDerQB, _QA, QB,
		      [pos(BitVectDerQB)],
		      [pos(derivative(QB))]):-!.
%	A: -
translate_p_star_core(neg, pos, _, _,
		      BitVectDerQA, BitVectDerQB, QA, QB,
		      [pos(BitVectDerQA), neg(BitVectDerQB)],
		      [pos(derivative(QA)), neg(derivative(QB))]):-!.
translate_p_star_core(neg, neg, _, _,
		      BitVectDerQA, BitVectDerQB, QA, QB,
		      [neg(BitVectDerQA), neg(BitVectDerQB)],
		      [neg(derivative(QA)), neg(derivative(QB))]):-!.
translate_p_star_core(neg, zero, _, _,
		      _BitVectDerQA, BitVectDerQB, _QA, QB,
		      [neg(BitVectDerQB)],
		      [neg(derivative(QB))]):-!.
%	A: 0
translate_p_star_core(zero, pos, _, _,
		      BitVectDerQA, _BitVectDerQB, QA, _QB,
		      [pos(BitVectDerQA)],
		      [pos(derivative(QA))]):-!.
translate_p_star_core(zero, neg, _, _,
		      BitVectDerQA, _BitVectDerQB, QA, _QB,
		      [neg(BitVectDerQA)],
		      [neg(derivative(QA))]):-!.
%	A: 0, B: 0
%	dA: +
translate_p_star_core(zero, zero, pos, pos,
		      BitVectDerQA, BitVectDerQB, QA, QB,
		      [pos(BitVectDerQA), pos(BitVectDerQB)],
		      [pos(derivative(QA)), pos(derivative(QB))]):-!.
translate_p_star_core(zero, zero, pos, neg,
		      BitVectDerQA, BitVectDerQB, QA, QB,
		      [neg(BitVectDerQA), pos(BitVectDerQB)],
		      [neg(derivative(QA)), pos(derivative(QB))]):-!.
translate_p_star_core(zero, zero, pos, zero,
		      _BitVectDerQA, BitVectDerQB, _QA, QB,
		      [pos(BitVectDerQB)],
		      [pos(derivative(QB))]):-!.
%	dA: -
translate_p_star_core(zero, zero, neg, pos,
		      BitVectDerQA, BitVectDerQB, QA, QB,
		      [pos(BitVectDerQA), neg(BitVectDerQB)],
		      [pos(derivative(QA)), neg(derivative(QB))]):-!.
translate_p_star_core(zero, zero, neg, neg,
		      BitVectDerQA, BitVectDerQB, QA, QB,
		      [neg(BitVectDerQA), neg(BitVectDerQB)],
		      [neg(derivative(QA)), neg(derivative(QB))]):-!.
translate_p_star_core(zero, zero, neg, zero,
		      _BitVectDerQA, BitVectDerQB, _QA, QB,
		      [neg(BitVectDerQB)],
		      [neg(derivative(QB))]):-!.
%	dA: 0
translate_p_star_core(zero, zero, zero, pos,
		      BitVectDerQA, _BitVectDerQB, QA, _QB,
		      [pos(BitVectDerQA)],
		      [pos(derivative(QA))]):-!.
translate_p_star_core(zero, zero, zero, neg,
		      BitVectDerQA, _BitVectDerQB, QA, _QB,
		      [neg(BitVectDerQA)],
		      [neg(derivative(QA))]):-!.
%	dA: 0, dB: 0
translate_p_star_core(zero, zero, zero, zero,
		      BitVectDerQA, BitVectDerQB, QA, QB,
		      [pos(BitVectDerQA), pos(BitVectDerQB)],
		      [pos(derivative(QA)), pos(derivative(QB))]):-!.




%%	translate_p_star(+AddList, +TrList, +Derivable, -NewAddList, -NewTrList
%
%	Qin = quantitylist from cio
%
%  D = Divisor
%  N = Numerator
%
%  N/D
%
translate_p_star_p_slash([pos_neg_diw(BitVectDerD), pos_neg_mult(BitVectDerN)],
		 [pos_neg_diw(derivative(D)), pos_neg_mult(derivative(N))],
		 Qin, Derivable, AddList, TrAddList):-
	list_map([0], BitVectZero), % NB for different Absorbing elements a different zero is needed!
	intern_quantity(value(D), BitVectValD, Qin, Qnew),
	intern_quantity(value(N), BitVectValN, Qnew, Qnew),
        get_quantity_sign_core(BitVectValD,  BitVectZero, Derivable, SignValD),
	get_quantity_sign_core(BitVectValN,  BitVectZero, Derivable, SignValN),
	get_quantity_sign_core(BitVectDerD,  BitVectZero, Derivable, SignDerD),
	get_quantity_sign_core(BitVectDerN,  BitVectZero, Derivable, SignDerN),
        translate_p_star_p_slash_core(SignValD, SignValN, SignDerD, SignDerN,
				 BitVectDerD, BitVectDerN, D, N,
				 AddList, TrAddList).

%%	translate_p_star_p_slash_core(SignValD, SignValN, SignDerD,
%	SignDerN, BitVectDerQD, BitVectDerQN, QD, QN, AddList,
%	TrAddList)
%
%	determine correct interpretation of proportionalities.
%
%	Table:
%
%	val D, val N, : Proportionalities
%	___________________________________________________
%	D: +
%	       N: +   : PD- & PN+
%	       N: -   : PD+ & PN+
%	       N: 0   : PN+
%	D: -
%	       N: +   : PD- & PN-
%	       N: -   : PD+ & PN-
%	       N: 0   : PN-
%	D: 0
%	       undefined! : illegal state.
%


%	D: +
translate_p_star_p_slash_core(pos, pos, _, _,
		      BitVectDerQD, BitVectDerQN, QD, QN,
		      [neg(BitVectDerQD), pos(BitVectDerQN)],
		      [neg(derivative(QD)), pos(derivative(QN))]):-!.
translate_p_star_p_slash_core(pos, neg, _, _,
		      BitVectDerQD, BitVectDerQN, QD, QN,
		      [pos(BitVectDerQD), pos(BitVectDerQN)],
		      [pos(derivative(QD)), pos(derivative(QN))]):-!.
translate_p_star_p_slash_core(pos, zero, _, _,
		      _BitVectDerQD, BitVectDerQN, _QD, QN,
		      [pos(BitVectDerQN)],
		      [pos(derivative(QN))]):-!.
%	D: -
translate_p_star_p_slash_core(neg, pos, _, _,
		      BitVectDerQD, BitVectDerQN, QD, QN,
		      [neg(BitVectDerQD), neg(BitVectDerQN)],
		      [neg(derivative(QD)), neg(derivative(QN))]):-!.
translate_p_star_p_slash_core(neg, neg, _, _,
		      BitVectDerQD, BitVectDerQN, QD, QN,
		      [pos(BitVectDerQD), neg(BitVectDerQN)],
		      [pos(derivative(QD)), neg(derivative(QN))]):-!.
translate_p_star_p_slash_core(neg, zero, _, _,
		      _BitVectDerQD, BitVectDerQN, _QD, QN,
		      [neg(BitVectDerQN)],
		      [neg(derivative(QN))]):-!.

*/
