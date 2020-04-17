/*  File:    influence_resolution.pl
    Purpose: resolve effects of I's and P's
    Author:  Martin Reinders & Bert Bredeweg & Floris Linnebank
    Date:    August 1989
    Part-of:  GARP & DynaLearn
    Modified: September 2010

    formerly part of: solve.pl

    Copyright (c) 2010, University of Amsterdam. All rights reserved.

*/




%%%%%%%%%%%%%%%%%%%%%%%%% resolve influences and proportional relations %%%%%%%%%%%%%%%%%%%%
%
%
%
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

resolve_influence_proportional(Cin, Cout, OldSod, Constraints, Old_Values, HigherOrderDerivatives, AmbiguousDerivatives):-
	cio_ip_nip(Cin, Cnew1, IP, []),      % collect i/p relations
	etrace(solve_ir_search, _, resolve),
%	ip_to_causal(IP, Causal, Cin, C1), C1 =_, Causal = _,
	findall(adder(Quantity, AddList),   % find all effects on each quantity
		 adder(Quantity, _, AddList, [], IP, []),
		 Adders1),
	% put adders in causal order:
	sort_adders(Adders1, Adders),
	adder_strings(Adders, Adderstrings), % calculates full causal paths
	!,
	% resolve these effects per quantity
	% backtracking allowed for ambiguous influences/prop. rel.
	% FL mar 08: return list of ambiguos derivatives for termination possibility
	resolve_adders(Adders, Adderstrings, Cnew1, Cnew2, OldSod, AmbiguousDerivatives),
	%resolve_adders(Adders, Adderstrings, Cnew, Cout, OldSod, AmbiguousDerivatives),
	% FL mar 07: NEW: determine 2nd order derivative
	% FL jan 2012: NEW: use 3rd order info from previous state (OldSod variable) and derivative info from precursor to previous
	% state (OV variable) to implement Post-filter for 3rd order derivative functionality
	% Returns a list of preds:
	% [hod_info(Q, Derivative, SecondOrderDerivative, ThirdOrderDerivative, SecondOrderInfluences, ThirdOrderInfluences, FullCausalPath), ..., ...]
	% NB (hod = higher order derivative)
	% SecondOrderDerivative can be [pos, neg, zero, pos_zero, neg_zero, unknown]
	% April 08: Constraints: assumed terminating ambiguous derivatives should not be found to be moving in the opposite direction.
	resolve_2ndorder_adders(Adders, Adderstrings, Cnew2, Cnew3, SecondOrderDerivatives, Constraints),
	%resolve_2ndorder_adders(Adders, Adderstrings, Cout, CioTemp1, SecondOrderDerivatives, Constraints),
	% FL Jan 2012: NEW: determine 3rd order derivative
	resolve_3rdorder_adders(Adders, Adderstrings, Cnew3, Cout, SecondOrderDerivatives, HigherOrderDerivatives, Old_Values).
	%resolve_3rdorder_adders(Adders, Adderstrings, CioTemp1, CioTemp2, SecondOrderDerivatives, HigherOrderDerivatives, Old_Values),
	%_= [CioTemp2].

% NB the influence resolution procedure currently has a double
% administration of quantities in normal and bitvector form
% this makes for quite some coding overhead, the administration needs to
% be passed on to the solve procedure to keep the (changing) bitvector
% pointers correct. This was done in the past to gain speed, but
% currently I doubt this is worthwile wrt to the coding overhead in
% future additions / updates etc. So future coders might consider using
% only normal form adder administration and then translating on the fly
% a little more often. FL april 2012

% adder/6:
% find all influences and proportional relation on a quantity

% no more inf/prop for a certain quantity, return Addlist
adder(Quantity, Quantity, AddListOut, AddListIn, [], _):-
    nonvar(Quantity),
    list_to_set(AddListIn, AddListOut). %FL: filter double adders (bad model)

% backtrack: select next quantity and find inf/prop in restlist
adder(Quantity, _, AddList, _, [], RestIP) :-
	RestIP \== [],
	adder(Quantity, _, AddList, [], RestIP, []).

% if called for the first time Quantity is uninstantiated
% and will be matched with the quantity in prop/inf Head
% otherwise Head must be a next prop/inf on Quantity.
adder(ResultQuantity, Quantity, AddList, Previous, [Head | Tail ], RestIP) :-
	adder_relation(Head, Quantity, First),
	!,
	adder(ResultQuantity, Quantity, AddList, [First|Previous], Tail, RestIP).

% Head is not an inf/prop on Quantity, put it on RestList
adder(ResultQuantity, Quantity, AddList, Previous, [Head | Tail ], RestIP) :-
	adder(ResultQuantity, Quantity, AddList, Previous, Tail, [Head|RestIP]).


%%	%%%
% FL new july 2007,
%
% Add adder order heuristic: try to resolve influences first,
% for better tracing.
% further ordering algorithm is a bit naive:
% insert after adders that influence it.
% progress indicator and several iterations are needed.
% now hacked by doing it three times.

sort_adders(AddersIn, AddersOut):-
	split_adders_inf_prop(AddersIn, Inf, PropIn),
	insertion_sort_adders(PropIn, [], Prop1),
	insertion_sort_adders(Prop1, [], Prop2), % A hack, do it three times, fixes most mess ups
	insertion_sort_adders(Prop2, [], Prop),
	append(Inf, Prop, AddersOut). % influences in front...

split_adders_inf_prop([], [], []).
split_adders_inf_prop([H|T], [H|IT], PT):-
	inf_adder(H),
	!,
	split_adders_inf_prop(T, IT, PT).
split_adders_inf_prop([H|T], IT, [H|PT]):-
	split_adders_inf_prop(T, IT, PT).

%heuristic: assumes if first adder is influence then so is the rest, ignore rest
inf_adder(adder(_, [First|_])):-
	First =.. [_, value(_)].


% insertion_sort_adders(In, Sofar, Out, NotInserted, Progress).

insertion_sort_adders([], Out, Out).

insertion_sort_adders([adder(X, AddList)|T], SoFar, AddersOut):-
	insert_adder(AddList, X, AddList, SoFar, NewSoFar),
	insertion_sort_adders(T, NewSoFar, AddersOut).

%checked all addItems, put in front of rest,
insert_adder([], X, AddList, SoFarBack, [adder(X, AddList)|SoFarBack]):-!.

% check addItem, insert in part of the list after the influencing adder
insert_adder([AddItem|AddListTail], X, AddList, SoFar, NewSoFar):-
	AddItem =.. [_, Qty],
	memberchk(adder(Qty, _), SoFar), % influence Qty is influenced itself.
	!,
	split_adders_sofar(Qty, SoFar, SoFarFront, SoFarBack),
	insert_adder(AddListTail, X, AddList, SoFarBack, NewSoFarBack),
	append(SoFarFront, NewSoFarBack, NewSoFar).

% check addItem, no influencing adder, ignore item
insert_adder([_|AddListTail], X, AddList, SoFar, NewSoFar):-
	insert_adder(AddListTail, X, AddList, SoFar, NewSoFar).

% split_adders_sofar(Qty, Input, InputFront, InputBack)
% find the adder in SoFar that influences Qty,
% return Front and Back of List,
% Front includes the adder that influences Qty
% fails if no adder influences Qty, but with a memberchk in front of the call
% that should never happen.

% found, done
split_adders_sofar(Qty, [adder(Qty, List)|Tail], [adder(Qty, List)], Tail):- !.

split_adders_sofar(Qty, [adder(QtyB, ListB)|Tail], [adder(QtyB, ListB)|Front], Back):-
	split_adders_sofar(Qty, Tail, Front, Back).



%%	adder_strings(+Adders, -Adderstrings)
%
%	For each adder the complete tree of predecessor influences is calculated
%	This information is used in the 2nd order derivative procedure
%	(If the causal path to a quantity changes certain constraints are not applied)
%
adder_strings(Adders, Adderstrings):-
	adder_trees(Adders, Adders, [], AdderstringsR),
	reverse(AdderstringsR, Adderstrings).


%%	adder_trees(+Adders, +Adders, +TreesIn, -TreesOut)
%
%	Take one adder at a time
%	Use Adders as resource
%	Use TreesIn (start with []) as resource and accumulator
%
%	does a backward search through the causal graph
%
adder_trees([], _Adders, Trees, Trees).
adder_trees([adder(Q, List)|T], Adders, TreesIn, TreesOut):-
	list2addertree(List, [Q], TreesIn, Adders, Tree),
	adder_trees(T, Adders, [adder(Q, Tree)|TreesIn], TreesOut).


% done
list2addertree([], _, _, _, []).

% Q is member of path: loop
list2addertree([H|T], PathSoFar, Trees, Adders, [NH|NT]):-
	H =.. [PosNeg, Q],
	memberchk(Q, PathSoFar),
	!,
	NH =.. [PosNeg, loop(Q)],
	list2addertree(T, PathSoFar, Trees, Adders, NT).

%Tree of Q is already known
list2addertree([H|T], PathSoFar, Trees, Adders, [NH|NT]):-
	H =.. [PosNeg, Q],
	memberchk(adder(Q, List), Trees),
	!,
	NH =.. [PosNeg, adder(Q, List)],
	list2addertree(T, PathSoFar, Trees, Adders, NT).

%influenced Q
list2addertree([H|T], PathSoFar, Trees, Adders, [NH|NT]):-
	H =.. [PosNeg, Q],
	memberchk(adder(Q, List), Adders),
	!,
	list2addertree(List, [Q|PathSoFar], Trees, Adders, TreeList),
	NH =.. [PosNeg, adder(Q, TreeList)],
	list2addertree(T, PathSoFar, Trees, Adders, NT).

%Q is uninfluenced (leaf in tree)
list2addertree([H|T], PathSoFar, Trees, Adders, [NH|NT]):-
	H =.. [PosNeg, Q],
	\+ memberchk(adder(Q, _), Adders),
	!,
	NH =.. [PosNeg, leaf(Q)],
	list2addertree(T, PathSoFar, Trees, Adders, NT).




% resolve_adders(+Adders, +Cin, -Cout)
% - change Adders from extern to intern representation
% - apply closed world on all quantities (under switch control)
% this means setting derivatives of unaffected quantities to zero.
% - resolve the resulting adders within Cio context.

resolve_adders(Adders, Adderstrings, Cin, Cout, Sod, AmbiguousDerivatives):-
	cio_q_nq(Cin, Cnew1, Q, NQ),
	adders_quantities(Adders, IntAdders1, Q, NQ), % convert adderlist to intern representation
	!,
	flag(cw_assumption, Flag, Flag),
	(
	    algorithm_assumption_flag(Flag, fail, cw_assumption)
	->
	    Cnew2 = Cnew1
	;
        % set unknown, uninfluenced, influencing quantities to zero
	    etrace(solve_cw_start, _, resolve),
	    apply_closed_world_to_adders(NQ, IntAdders1, Cnew1, Cnew2, [], _) % FL SHOULDNT ADDERS BE PASSED ON FOR POINTER UPDATES? No pointers remain the same, analyse simple is off in CW application.
	),
	test_2nd_order_continuity_constraints(Sod, Adders, Adderstrings, IntAdders1, IntAdders, Cnew2, Cnew3),
	flag(no_causal_loop_handling, F, F),
	(
	  F = 0 % now default on...
	->
	  detect_causal_loops(Adders, CausalLoops),
	  Loops = CausalLoops
	;
	  Loops = []
	),
	flag(comparative_analysis, CA, CA),
	(
	  CA = true
	->
	  detect_comparative_analysis_candidates(Adders, CA_Quantity_Pairs, Cnew3),
	  CAPairs = CA_Quantity_Pairs
	;
	  CAPairs = []
	),
	resolve_adders_2(IntAdders, Adders, Cnew3, Cout, true, AmbiguousDerivatives, Loops, CAPairs, Sod). % FL may 2004: pass on normal adderlist for use in tracer
    % fails if contradiction






%%	detect_causal_loops
%
%	For all Influenced nodes, check if they influence themselves
%	If so return Path and external influences
%	loop(Path, ExternalInfluences,
%	     ExternalInfluencedNodesWithInfluences)
%
%	For every loop an instance of loop is generated for every node
%	in it.
%
%	loops without external influences are not interesting, they are
%	handled by the current algorithm correctly: if assigned a value
%	they succeed, if the aggregate sign is pos, and get to a
%	contradiction if it is min.
%

detect_causal_loops(Adders, CausalLoops):-
	influenced_nodes(Adders, Influenced),
	!,
	findall(Loop, (member(Node, Influenced), causal_loop(Node, Adders, Loop)), CausalLoops1),
	!,
	remove_subloops(CausalLoops1, CausalLoops1, CausalLoops2), % having no subloops gives some strange results for loops with bottlenecks...
	!,
	filter_external_influences(CausalLoops2, CausalLoops3),
	remove_non_externalloops(CausalLoops3, CausalLoops4),
	compute_loop_types(CausalLoops4, CausalLoops),
%	remove_duplicate_loops(CausalLoops4, CausalLoops),% but keeping them is tricky reasoning (their external nodes are unknown, but part of the superloop...
        % solution could be to see if multiple influences are all in loop, in that case, no real external items in loop: just propagate the known value.
	!.



%%	influenced_nodes(+Adders, -Influenced)
%
%	Get all nodes that have an adder.
%
influenced_nodes([], []).

influenced_nodes([adder(Target, _AddList)|AddersT],[Target|AddersNewT]):-
	influenced_nodes(AddersT, AddersNewT).



% call with findall...
% causal_loop(Node, Adders, loop(Node, Type, Path, ExternalInfluences, EINodes)):-
% nb looptype not used anymore...
causal_loop(Node, Adders, loop(Path, ExternalInfluences, EINodes, AdderPath)):-
	forward_causal_loop_search(Node, Node, Adders, [], Path1, [], ExternalInfluences1, [], EINodes, [], AdderPath1),
	AdderPath1 = [LastAdder|_],
	reverse([Node|Path1], Path),
	reverse(AdderPath1, AdderPath2),
	AdderPath = [LastAdder|AdderPath2],
	list_to_set(ExternalInfluences1, ExternalInfluences).


% step
forward_causal_loop_search(StartNode, CurrentNode, Adders, PathIn, PathOut, ExtIn, ExtOut, EINin, EINout, APIn, APOut):-
	member(adder(NextNode, AddList), Adders),
	select(PosNegNode, AddList, Rest),
	PosNegNode =.. [_PosNeg, CurrentNode],
	%\+ memberchk(NextNode, [CurrentNode|PathIn]), %Not a subloop in the path
	%(not  a good constraint, we also want to find big loops trough bottle neck quantities (a -> b, b -> c, c -> b, b -> a)
	%better constraint: this step is not already made before in the path:
	\+ append(_, [NextNode, CurrentNode|_], [CurrentNode|PathIn]),
	\+ NextNode = StartNode, % not at start (proper loop)
	% found next step
	remove_posneg(Rest, Rest1),
	append(Rest1, ExtIn, Ext),
	(Rest = [] -> EINin = EIN ; append([ext(NextNode, Rest)], EINin, EIN)),
	forward_causal_loop_search(StartNode, NextNode, Adders, [CurrentNode|PathIn], PathOut, Ext, ExtOut, EIN, EINout, [adder(NextNode, AddList)|APIn], APOut).


% final step: loop
forward_causal_loop_search(StartNode, CurrentNode, Adders, PathIn, [CurrentNode|PathIn], ExtIn, ExtOut, EINin, EINout, APIn, APOut):-
	member(adder(NextNode, AddList), Adders),
	select(PosNegNode, AddList, Rest),
	PosNegNode =.. [_PosNeg, CurrentNode],
	NextNode = StartNode,
	!, % found final step: proper loop
	remove_posneg(Rest, Rest1),
	append(Rest1, ExtIn, ExtOut),
	(Rest = [] -> EINin = EINout ; append([ext(NextNode, Rest)], EINin, EINout)),
	APOut = [adder(NextNode, AddList)|APIn].


remove_posneg([], []).
remove_posneg([pos(X)|T], [X|NT]):-
	!,
	remove_posneg(T, NT).
remove_posneg([neg(X)|T], [X|NT]):-
	!,
	remove_posneg(T, NT).
remove_posneg([pos_neg_mult(X)|T], [X|NT]):-
	!,
	remove_posneg(T, NT).
remove_posneg([pos_neg_diw(X)|T], [X|NT]):-
	!,
	remove_posneg(T, NT).



%%	filter_external_influences_from_multiplelooploops
%
%	Take out external influences that are in fact part of the loop
%	because the loop has bottleneck quantities.
%
filter_external_influences([], []).

%filter_external_influences([loop(Node, Type, Path, Ext, ExtAdd)|T], [loop(Node, Type, Path, NExt, NExtAdd)|NT]):-
filter_external_influences([loop(Path, Ext, ExtAdd, AddPath)|T], [loop(Path, NExt, NExtAdd, AddPath)|NT]):-
	remove_fake_extAdd(ExtAdd, Path, NExtAdd1),
	list_to_set(NExtAdd1, NExtAdd),
	remove_fake_ext(Ext, Path, NExt),
	filter_external_influences(T, NT).


remove_fake_ext([], _Path, []).

% Node is not external
remove_fake_ext([Node|T], Path, NT):-
	memberchk(Node, Path),
	!,
	remove_fake_ext(T, Path, NT).

remove_fake_ext([Node|T], Path, [Node|NT]):-
	remove_fake_ext(T, Path, NT).


remove_fake_extAdd([], _Path, []).

remove_fake_extAdd([ext(Node, AddList)|T], Path, New):-
	remove_fake_extAddList(AddList, Path, NewAddList),
	(
	  NewAddList = []
	->
	  % Node is not externally influenced at all, all influences are part of the path
	  New = NT
	; % Node is really externally influenced
	  New = [ext(Node, NewAddList)|NT]
	),
	remove_fake_extAdd(T, Path, NT).


remove_fake_extAddList([], _Path, []).

% Node is not external
remove_fake_extAddList([H|T], Path, NT):-
	H =.. [_, Node],
	memberchk(Node, Path),
	!,
	remove_fake_extAddList(T, Path, NT).

remove_fake_extAddList([Node|T], Path, [Node|NT]):-
	remove_fake_extAddList(T, Path, NT).




%%	remove_non_externalloops(+CausalLoopsIn, -CausalLoops)
%
%	remove all loops that have no external influences
%
remove_non_externalloops([], []).

% loop is has empty externals lists, remove
remove_non_externalloops([loop(_Path, [], [], _AddPath)|T], NT):-
	!,
	remove_non_externalloops(T, NT).

% else: keep loop
remove_non_externalloops([Loop|T], [Loop|NT]):-
	remove_non_externalloops(T, NT).





%%	remove_subloops(+CausalLoopsIn, +CausalLoopsIn, -CausalLoops)
%
%	remove all loops that are part of a bigger loop...
%
remove_subloops([], _, []).

% loop is subloop, remove
remove_subloops([Loop|T], AllLoops, NT):-
	Loop =	loop([_|Path], _, _, _), %take off start node (is repeated as endnode)
	member(SuperLoop, AllLoops),
	Loop \= SuperLoop, % not compare loop with itself
	SuperLoop = loop([SH|ST], _, _, _),
	append([SH|ST], ST, SuperPath), % create double version of superpath that can catch
	append(_Front, Back, SuperPath),
	append(Path, _, Back), % path fits in SuperloopPath
	!,
	select(Loop, AllLoops, Rest), % two identical loops at different startpoints should not delete each other
	remove_subloops(T, Rest, NT).

% else: keep loop
remove_subloops([Loop|T], AllLoops, [Loop|NT]):-
	remove_subloops(T, AllLoops, NT).

%%	compute_loop_types
%
%	determine if loop has only one external entry point (and an
%	aggregate sign) or multiple (true complex loop)
%

% done
compute_loop_types([], []).

% loop has multiple external entries: complex:
% for now: remove: we don't calculate these loops: requires looking ahead
% compute_loop_types([loop(Path, Ext, [One, Two|Rest], AddPath)|T], [loop(Path, Ext, [One, Two|Rest], AddPath, complex)|NT]):-
compute_loop_types([loop(_Path, _Ext, [_One, _Two|_Rest], _AddPath)|T], NT):-
	!,
	compute_loop_types(T, NT).

% loop has single external entries point: compute aggregate sign
compute_loop_types([loop([_|Path], Ext, [ext(One, List)], [_|AddPath])|T], [loop(NewPath, Ext, [ext(One, List)], NewAddPath, Type)|NT]):-
	!,
	set_loop_startpoint(One, Path, AddPath, NewPath, NewAddPath),
	NewAddPath = [adder(Start, _ExternalStartList)|SimpleRest],
	loop_type_adder(SimpleRest, Start, pos, Type),
	compute_loop_types(T, NT).


% at startpoint, append first at back to make complete loop with return
% to start...
%
set_loop_startpoint(One, [One|Path], [adder(One, List)|AddPath], NewPath, NewAddPath):-
	!,
	append([One|Path], [One], NewPath),
	append([adder(One, List)|AddPath], [adder(One, List)], NewAddPath).


set_loop_startpoint(One, [First|Path], [adder(First, List)|AddPath], NewPath, NewAddPath):-
	First \= One,
	!,
	append(Path, [First], NextPath),
	append(AddPath, [adder(First, List)], NextAddPath),
	set_loop_startpoint(One, NextPath, NextAddPath, NewPath, NewAddPath).


loop_type_adder([], _, Type, Type).

loop_type_adder([adder(NextNode, List)|T], StartNode, TypeIn, TypeOut):-
	add_loop_type(TypeIn, StartNode, List, Type),
	loop_type_adder(T, NextNode, Type, TypeOut).

add_loop_type(TypeIn, PreviousNode, List, TypeOut):-
	member(AddItem, List),
	AddItem =.. [PosNeg, PreviousNode], % select the node we are propagating from,
	                                    % there can be multiple because of bottlenecks in the loop,
					    % but at this point all influences are internal to the loop.
	add_loop_type(TypeIn, PosNeg, TypeOut),
	!.

add_loop_type(pos, pos, pos).
add_loop_type(pos, neg, neg).
add_loop_type(neg, pos, neg).
add_loop_type(neg, neg, pos).
add_loop_type(pos, pos_neg_mult, pos):-writeln('*** WARNING P* not supported in causal loop handling ***').
add_loop_type(neg, pos_neg_mult, neg):-writeln('*** WARNING P* not supported in causal loop handling ***').
add_loop_type(pos, pos_neg_diw, pos):-writeln('*** WARNING P/ not supported in causal loop handling ***').
add_loop_type(neg, pos_neg_diw, neg):-writeln('*** WARNING P/ not supported in causal loop handling ***').



%%	remove_duplicate_loops(+CausalLoopsIn, -CausalLoops)
%
%	remove all loops that are duplicates of another loop...
%
remove_duplicate_loops([], []).

% loop is duplicate loop, remove
remove_duplicate_loops([Loop|T], NT):-
	Loop =	loop([_|Path], _, _), %take off start node (is repeated as endnode)
	length(Path, N),
	member(DupliLoop, T),
	DupliLoop = loop([SH|ST], _, _),
	length(ST, N),
	append([SH|ST], ST, DupliPath), % create double version of duplipath that can catch the path
	append(_Front, Back, DupliPath),
	append(Path, _, Back), % path fits in DupliPath
	!,
	remove_duplicate_loops(T, NT).

% else: keep loop
remove_duplicate_loops([Loop|T], [Loop|NT]):-
	remove_duplicate_loops(T, NT).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	COMPARATIVE ANALYSIS: Detection of candidates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


detect_comparative_analysis_candidates(AddersIn, CA_Quantity_Pairs, Cio):-
	cio_p(Cio, Parameters),
	cio_oe(Cio, Instances),
	deconstruct_parameters(Parameters, PList),
	deconstruct_adders(AddersIn, Adders1),
	flag(comparative_analysis_on_proportionalities, CaFlagP, CaFlagP),
	flag(comparative_analysis_equal_target_quantity_type, CaFlagTQ, CaFlagTQ),
	flag(comparative_analysis_equal_source_quantity_type, CaFlagSQ, CaFlagSQ),
	flag(comparative_analysis_similar_target_entity_type, CaFlagTE, CaFlagTE),
	flag(comparative_analysis_similar_source_entity_type, CaFlagSE, CaFlagSE),
	flag(comparative_analysis_equal_causal_dependency_sign, CaFlagCS, CaFlagCS),
	filter_pstar_pslash_adders(Adders1, Adders2, CaFlagP),
	detect_CA(Adders2, PList, Instances, [], CA_Quantity_Pairs,
		  CaFlagP, CaFlagTQ, CaFlagSQ, CaFlagTE, CaFlagSE, CaFlagCS).

deconstruct_parameters([], []).
deconstruct_parameters([Par|T], [parameter(List)|NT]):-
	Par =.. List,
	deconstruct_parameters(T, NT).

deconstruct_adders([], []).
deconstruct_adders([adder(Target, SourceList)|T], [adder(ValDer, Q, CausalList, IP, SourceList)|NT]):-
	Target =.. [ValDer, Q],
	deconstruct_addlist(SourceList, CausalList),
	i_or_p_causal_set(CausalList, influence, IP),
	deconstruct_adders(T, NT).

deconstruct_addlist([], []).
deconstruct_addlist([Adder|Tail], [causal(Sign, InfProp, Quantity)|NewTail]):-
	Adder =.. [Sign, Rest],
	Rest =.. [ValDer, Quantity],
	(
	  ValDer = value
	->
	  InfProp = influence
	;
	  InfProp = proportionality
	),
	deconstruct_addlist(Tail, NewTail).



i_or_p_causal_set([], IP, IP).

i_or_p_causal_set([causal(_Sign, InfProp, _Quantity)|T], IPIn, IPOut):-
	i_or_p_add(IPIn, InfProp, NewIP),
	i_or_p_causal_set(T, NewIP, IPOut).

%%	filter_pstar_pslash_adders
%
%	no handling of P* or P/ influenced quantities yet.
%	so these are filtered out if P relations CA is on.
%

% if Flag is fail no filtering (messages would be redundant)
filter_pstar_pslash_adders(Adders, Adders, fail):-!.

filter_pstar_pslash_adders([], [], true).

% P* or P/ in causelist: remove
filter_pstar_pslash_adders([adder(_ValDer, Q, CausalList, _Type, _AddList)|T], NT, true):-
	(
	  memberchk(causal(pos_neg_mult, _, CQ), CausalList)
	;
	  memberchk(causal(pos_neg_diw, _, CQ), CausalList)
	),
	!,
	etrace(solve_ca_filter, [Q, CQ], resolve),
	filter_pstar_pslash_adders(T, NT, true).

% other types of causal input (P+ and P-): keep
filter_pstar_pslash_adders([H|T], [H|NT], true):-
	filter_pstar_pslash_adders(T, NT, true).



% set only has influences...
i_or_p_add(influence, influence, influence):-!.

% ...or set has at least a proportionality
i_or_p_add(_, _, proportionality).


%done
detect_CA([], _, _, CA_Quantity_Pairs, CA_Quantity_Pairs, _, _, _, _, _, _).


% This clause skips all proportionality influenced quantities if CaFlagP = fail
detect_CA([adder(_, _, _, IP, _)|Tail], Parameters, Instances, PairsIn, PairsOut,
	  fail, CaFlagTQ, CaFlagSQ, CaFlagTE, CaFlagSE, CaFlagCS
	 ):-
	IP \= influence,
	!,
	detect_CA(Tail, Parameters, Instances, PairsIn, PairsOut,
	  fail, CaFlagTQ, CaFlagSQ, CaFlagTE, CaFlagSE, CaFlagCS).


% get all CA pairs for current adderquantity...
% This clause is for free quantity types: CaFlagTQ = fail
detect_CA([adder(_ValDer, Q1, Causal1, _, SourceList1)|Tail], Parameters, Instances, PairsIn, PairsOut,
	  CaFlagP, fail, CaFlagSQ, CaFlagTE, CaFlagSE, CaFlagCS
	 ):-
	!,
	% IP identifier of type of causal relations must be influences if CA on props is not allowed.
	% in the other case IP identifier must be same type.
	(
	  CaFlagP = fail
	->
	  NeededIP = influence
	;
	  NeededIP = _
	),
	findall(ca_pair(Q1, unresolved, SourceList1, Q2, unresolved, SourceList2), % return original addlist form
		( % Q2 is:
		  member(adder(_, Q2, Causal2, NeededIP, SourceList2), Tail), % an influenced quantity
                                                                               % with needed type of influences
		  comparative_analysis_target_pattern(CaFlagTE, Q1, Q2, Parameters, Instances),
		  comparative_analysis_source_pattern(CaFlagSQ, CaFlagSE, CaFlagCS, Causal1, Causal2, Parameters, Instances)
		),
		Pairs),
	append(Pairs, PairsIn, NewPairs),
	detect_CA(Tail, Parameters, Instances, NewPairs, PairsOut,
		  CaFlagP, fail, CaFlagSQ, CaFlagTE, CaFlagSE, CaFlagCS
		 ).



% get all CA pairs for current adderquantity...
% with more CA constraints, NoStrictCaFlag must be fail.
% This clause is for restricted quantity types: CaFlagTQ = true
detect_CA([adder(_ValDer, Q1, Causal1, _IP, SourceList1)|Tail], Parameters, Instances, PairsIn, PairsOut,
	  CaFlagP, true, CaFlagSQ, CaFlagTE, CaFlagSE, CaFlagCS
	 ):-
	!,
	% IP identifier of type of causal relations must be influences if CA on props is not allowed.
	% in the other case IP identifier must be same type.
	(
	  CaFlagP = fail
	->
	  NeededIP = influence
	;
	  NeededIP = _
	),
	memberchk(parameter([QuantityType, _EntityInstance1, Q1, _, _QspaceType1]), Parameters),
	findall(ca_pair(Q1, unresolved, SourceList1, Q2, unresolved, SourceList2),
		( % Q2 is:
		  member(parameter([QuantityType, _EntityInstance2, Q2, _, _QspaceType2]), Parameters), %same quantitytype
		  memberchk(adder(_, Q2, Causal2, NeededIP, SourceList2), Tail), % an influenced quantity
										  % with needed type of influences
		  comparative_analysis_target_pattern(CaFlagTE, Q1, Q2, Parameters, Instances), % with correct patterns
		  comparative_analysis_source_pattern(CaFlagSQ, CaFlagSE, CaFlagCS, Causal1, Causal2, Parameters, Instances)
		),
		Pairs),
	append(Pairs, PairsIn, NewPairs),
	detect_CA(Tail, Parameters, Instances, NewPairs, PairsOut,
		  CaFlagP, true, CaFlagSQ, CaFlagTE, CaFlagSE, CaFlagCS
		 ).


%%	comparative_analysis_target_pattern(Flag, Q1, Q2, Parameters, Instances)
%
%	do check on similar entity types.
%	can be higher in hierarchy, but not topnodes.
%
comparative_analysis_target_pattern(true, Q1, Q2, Parameters, Instances):-
	!,
	memberchk(parameter([_QuantityType1, EntityInstance1, Q1, _, _QspaceType1]), Parameters),
	memberchk(parameter([_QuantityType2, EntityInstance2, Q2, _, _QspaceType2]), Parameters),
	memberchk(instance(EntityInstance1, Entity1), Instances),
	memberchk(instance(EntityInstance2, Entity2), Instances),
	isa_similar_entity_type(Entity1, Entity2),
	!.

% no check
comparative_analysis_target_pattern(fail, _Q1, _Q2, _Parameters, _Instances).



% same entity, but both topnode entity: fail
isa_similar_entity_type(entity, entity):- !, fail.

% same entity, but both topnode entity: fail
isa_similar_entity_type(nil, nil):- !, fail.

% same entity, but both topnode entity: fail
isa_similar_entity_type(entity, nil):- !, fail.

% same entity, but both topnode entity: fail
isa_similar_entity_type(nil, entity):- !, fail.

% same entity: succeed
isa_similar_entity_type(Entity, Entity):- !.

% not same entity: step up Entity1...
isa_similar_entity_type(Entity1, Entity2):-
	isa(Entity1, Entity1Super),
	isa_similar_entity_type(Entity1Super, Entity2).

% not same entity: or step up Entity2...
isa_similar_entity_type(Entity1, Entity2):-
	isa(Entity2, Entity2Super),
	isa_similar_entity_type(Entity1, Entity2Super).




%%	comparative_analysis_source_pattern(CaFlagSQ, CaFlagSE, CaFlagCS, Causal1, Causal2, Parameters, _Instances)
%	Test if a pair fits  further constraints for CA.
%	Flexible given flags.
%

% do check on equal quantity types of influencing quantities:
% must be match for each
comparative_analysis_source_pattern(true, fail, CaFlagCS, Causal1, Causal2, Parameters, _Instances):-
	match_quantity_types(Causal1, Causal2, Parameters, CaFlagCS).


% do check on equal entity types of influencing quantities:
% must be match for each
comparative_analysis_source_pattern(fail, true, CaFlagCS, Causal1, Causal2, Parameters, Instances):-
	match_entity_types(Causal1, Causal2, Parameters, Instances, CaFlagCS).


% do check on equal entity types of influencing quantities:
% must be match for each
comparative_analysis_source_pattern(true, true, CaFlagCS, Causal1, Causal2, Parameters, Instances):-
	match_quantity_and_entity_types(Causal1, Causal2, Parameters, Instances, CaFlagCS).


% no quantity or entity type check, only sign check
comparative_analysis_source_pattern(fail, fail, true, Causal1, Causal2, _Parameters, _Instances):-
	match_signs(Causal1, Causal2).

% no quantity or entity type or sign check, just check IP type
comparative_analysis_source_pattern(fail, fail, fail, Causal1, Causal2, _Parameters, _Instances):-
	match_relation_types(Causal1, Causal2).






% remaining non-matches are ok...
% There must have been matches for the rest
match_quantity_types(_, [], _, _):-!.

% idem
match_quantity_types([], _, _, _):-!.

match_quantity_types([causal(Sign1, IP, Q1)|T], Causal2, Parameters, CaFlagCS):-
	memberchk(parameter([QuantityType, _EntityInstance1, Q1, _, _QspaceType1]), Parameters), %get quantitytype1
	select(causal(Sign2, IP, Q2), Causal2, Rest2),
	(
	  CaFlagCS = true
	->
	  Sign1 = Sign2
	;
	  true
	),
	memberchk(parameter([QuantityType, _EntityInstance2, Q2, _, _QspaceType2]), Parameters), %get quantitytype2
	!,% found a match, try to match the rest
	match_quantity_types(T, Rest2, Parameters, CaFlagCS).

% a non match... add as a dummy to the tail.
% If only this quantity has non matches, it's ok, but if the other
% quantity has them as wel the predicate fails.
% dummycheck to prevent loops
match_quantity_types([X|T], Causal2, Parameters, CaFlagCS):-
	X \= dummy,
	append(T, [dummy], New),
	match_quantity_types(New, Causal2, Parameters, CaFlagCS).



% remaining non-matches are ok...
% There must have been matches for the rest
match_entity_types(_, [], _, _, _):-!.
%idem
match_entity_types([], _, _, _, _):-!.


match_entity_types([causal(Sign1, IP, Q1)|T], Causal2, Parameters, Instances, CaFlagCS):-
	memberchk(parameter([_QuantityType1, EntityInstance1, Q1, _, _QspaceType1]), Parameters), %get entityinstance1
	memberchk(instance(EntityInstance1, Entity1), Instances),
	select(causal(Sign2, IP, Q2), Causal2, Rest2),
	(
	  CaFlagCS = true
	->
	  Sign1 = Sign2
	;
	  true
	),
	memberchk(parameter([_QuantityType2, EntityInstance2, Q2, _, _QspaceType2]), Parameters), %get entityinstance2
	memberchk(instance(EntityInstance2, Entity2), Instances),
	isa_similar_entity_type(Entity1, Entity2),
	!,% found a match, try to match the rest
	match_entity_types(T, Rest2, Parameters, Instances, CaFlagCS).

% a non match... add as a dummy to the tail.
% If only this quantity has non matches, it's ok, but if the other
% quantity has them as wel the predicate fails.
% dummycheck to prevent loops
match_entity_types([X|T], Causal2, Parameters, Instances, CaFlagCS):-
	X \= dummy,
	append(T, [dummy], New),
	match_entity_types(New, Causal2, Parameters, Instances, CaFlagCS).


% remaining non-matches are ok...
% There must have been matches for the rest
match_quantity_and_entity_types(_, [], _, _, _):-!.
%idem
match_quantity_and_entity_types([], _, _, _, _):-!.


match_quantity_and_entity_types([causal(Sign1, IP, Q1)|T], Causal2, Parameters, Instances, CaFlagCS):-
	memberchk(parameter([QuantityType, EntityInstance1, Q1, _, _QspaceType1]), Parameters), %get entityinstance1
	memberchk(instance(EntityInstance1, Entity1), Instances),
	select(causal(Sign2, IP, Q2), Causal2, Rest2),
	(
	  CaFlagCS = true
	->
	  Sign1 = Sign2
	;
	  true
	),
	memberchk(parameter([QuantityType, EntityInstance2, Q2, _, _QspaceType2]), Parameters), %get entityinstance2
	memberchk(instance(EntityInstance2, Entity2), Instances),
	isa_similar_entity_type(Entity1, Entity2),
	!,% found a match, try to match the rest
	match_quantity_and_entity_types(T, Rest2, Parameters, Instances, CaFlagCS).

% a non match... add as a dummy to the tail.
% If only this quantity has non matches, it's ok, but if the other
% quantity has them as wel the predicate fails.
% dummycheck to prevent loops
match_quantity_and_entity_types([X|T], Causal2, Parameters, Instances, CaFlagCS):-
	X \= dummy,
	append(T, [dummy], New),
	match_quantity_and_entity_types(New, Causal2, Parameters, Instances, CaFlagCS).




% remaining non-matches are ok...
% There must have been matches for the rest
match_signs(_, []):-!.

% idem
match_signs([], _):-!.

% for pairs: signs should be equal
% (as well as type of influence as always)
match_signs([causal(Sign, IP, _Q1)|T], Causal2):-
	select(causal(Sign, IP, _Q2), Causal2, Rest2),
	!,% found a match, try to match the rest
	match_signs(T, Rest2).

% a non match... add as a dummy to the tail.
% If only this quantity has non matches, it's ok, but if the other
% quantity has them as wel the predicate fails.
% dummycheck to prevent loops
match_signs([X|T], Causal2):-
	X \= dummy,
	append(T, [dummy], New),
	match_signs(New, Causal2).



% remaining non-matches are ok...
% There must have been matches for the rest
match_relation_types(_, []):-!.

% idem
match_relation_types([], _):-!.

% for pairs: type of influence should be equal
match_relation_types([causal(_, IP, _Q1)|T], Causal2):-
	select(causal(_, IP, _Q2), Causal2, Rest2),
	!,% found a match, try to match the rest
	match_relation_types(T, Rest2).

% a non match... add as a dummy to the tail.
% If only this quantity has non matches, it's ok, but if the other
% quantity has them as wel the predicate fails.
% dummycheck to prevent loops
match_relation_types([X|T], Causal2):-
	X \= dummy,
	append(T, [dummy], New),
	match_relation_types(New, Causal2).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%	COMPARATIVE ANALYSIS: Resolution / outcome computation
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% Update the ca_pair structure with signs of resolved derivatives.
%
% NB signs can also be lists of things...  eg. [neg, zero, pos]
%
comparative_analysis_update_candidates(derivative(Q), Sign, CAPairsIn, CAPairsOut):-
	ca_update_candidates(CAPairsIn, Q, Sign, CAPairsOut).

ca_update_candidates([], _, _, []).

% found relevant pair: first Q match
ca_update_candidates([ca_pair(Q, unresolved, AddList1, Q2, Sign2, AddList2)|Tail],
		     Q, Sign,
		     [ca_pair(Q, Sign, AddList1, Q2, Sign2, AddList2)|NewTail]):-
	!,
	ca_update_candidates(Tail, Q, Sign, NewTail).

% found relevant pair: second Q match
ca_update_candidates([ca_pair(Q1, Sign1, AddList1, Q, unresolved, AddList2)|Tail],
		     Q, Sign,
		     [ca_pair(Q1, Sign1, AddList1, Q, Sign, AddList2)|NewTail]):-
	!,
	ca_update_candidates(Tail, Q, Sign, NewTail).

% irrelevant pair: next
ca_update_candidates([Pair|Tail], Q, Sign, [Pair|NewTail]):-
	!,
	ca_update_candidates(Tail, Q, Sign, NewTail).


% check if signs are unequal because in that case CA is unnecessary:
% inequality is trivially derivable.

% quick fail for equal signs, added just for speed.
unequal_ca_signs(Sign, Sign):-
	!,
	fail.

% both lists: eg. [neg, zero]
unequal_ca_signs(Sign1, Sign2):-
	is_list(Sign1),
	is_list(Sign2),
	!,
	intersection(Sign1, Sign2, []). %NB this should always fail: lists should always overlap.

% one list: eg. [neg, zero] and one definite sign
unequal_ca_signs(Sign1, Sign2):-
	is_list(Sign1),
	!,
	\+ memberchk(Sign2, Sign1).

% one list: eg. [neg, zero] and one definite sign
unequal_ca_signs(Sign1, Sign2):-
	is_list(Sign2),
	!,
	\+ memberchk(Sign1, Sign2).

% two definite signs
unequal_ca_signs(Sign1, Sign2):-
	Sign1 \= Sign2. % should always succeed otherwise top clause would have matched.




% make_balance(+Quantity, +AddList, +Cin, -Cout, -Balance, -BalanceValue_intern)
% make balance expression in intern representation:
% [minL, posR, BV] = [posL, minR]

make_ca_balance(Q1, Q2, AddList1, AddList2, Cin, Cout, Relation, BV):-
    % create a balance value quantity:
    make_ca_balance_quantity(Q1, Q2, Cin, Cnew, I, BV),
    cio_q(Cin, Quantities),
    % get pointer lists for the positive and negative influences/props.
    add_to_ca_balance(AddList1, Quantities, [I], LeftList1, [], RightList1),
    add_to_ca_balance(AddList2, Quantities, RightList1, RightList, LeftList1, LeftList), % Addlist for Q2 gets added to opposite sides
    % d_q see intern.pl
	double_quantities(LeftList, NLeftList, [], Cnew, Cnew1),
	double_quantities(RightList, NRightList, [], Cnew1, Cout),
    % create bitmaps for both sides of the balance.
	list_map(NLeftList, Left),
	list_map(NRightList, Right),
	zero_pointer(relation(Left, =, Right), Relation1),
	canonical_form(Relation1, Relation).


% make_abstract_balance_quantity(+Quantity, +Cin, -Cout, -I, -BV),
% adds a balancequantity in the temporary Cio for influence resolution

make_ca_balance_quantity(Q1, Q2, Cin, Cout, I, BV):-
    % construct a pointer for the quantiy:
    flag(q_cnt, P, P),
    I is P + 1,
    flag(q_cnt, _, I),
    cio_q_nq(Cin, Cout, Q, NQ),
    list_map([I], BV),
    %map_list(Quantity, [X]),
    % construct a name for the quantity:
    %memberchk(Name/X, Q),
    %Name =.. [_, N],
    %atom_concat(d_, N, QName),
    atom_concat(c_a_balance_d_, Q1, Front),
    atom_concat(Front, '_d_', Middle),
    atom_concat(Middle, Q2, BalanceName),
    % update the quantity list:
    NQ = [BalanceName/I|Q].


% add_to_balance2(+Addlist, -Leftlist,  -Rightlist)

add_to_ca_balance([], _, Left, Left, Right, Right).

add_to_ca_balance([pos(Q)|Tail], Quantities, LeftIn, LeftOut, RightIn, RightOut):-
    memberchk(Q/I, Quantities),
    %map_list(Q, [I]),
    add_to_ca_balance(Tail, Quantities, LeftIn, LeftOut, [I|RightIn], RightOut).

add_to_ca_balance([neg(Q)|Tail], Quantities, LeftIn, LeftOut, RightIn, RightOut):-
    memberchk(Q/I, Quantities),
    %map_list(Q, [I]),
    add_to_ca_balance(Tail, Quantities, [I|LeftIn], LeftOut, RightIn, RightOut).


% get_ca_balance_result(+Quantity, +Relations, -Sign):-
% Like get quantity sign, but always succeeds:
% returns a relation between Q1 and Q2

get_ca_balance_result(Quantity, Derivable, Rel):-
	list_map([0], M),
	member(relation(Quantity, Rel, M), Derivable),
	!.

get_ca_balance_result(Quantity, Derivable, Inverse):-
	list_map([0], M),
	member(relation(M, Rel, Quantity), Derivable),
	inverse(Rel, Inverse),
	!.


% add_ca_result
add_ca_result(Q1, Q2, Relation, Cin, Cout, [UnresolvedIn, AddersIn], [UnresolvedOut, AddersOut]):-
	cio_q_nq(Cin, Cnew, Q, NQ),
	intern_quantity(Q1, Q1Intern, Q, NQ1),
	intern_quantity(Q2, Q2Intern, NQ1, NQ),
	right_order(relation(Q1Intern, Relation, Q2Intern), Relation1),
	canonical_form(Relation1, ResultRelation),
	append_split(UnresolvedIn, AddersIn, I, OtherAddersIn), % makes one list, for easy passing on with append relatoin. pointers can be updated, but list stays intact.
	try_append(append_relation(ResultRelation, Cnew, Cout, Added, true, OtherAddersIn, OtherAddersOut)),
	split_append(UnresolvedOut, AddersOut, I, OtherAddersOut), % splits the list again at the same point.
	Added = _,
	!.






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% applY_closEd_worlD_to_addeRs(+Quantities, +Adders, +Cin, -Cout)
% december-2003 by FL:
% set every value or derivative to zero if unknown, uninfluenced
% and influencing something:
% NB: In this version it is done for all. But correspondences and complex
% equations may cause quantities to be known later on, causing contradiction.
% Therefore, this feature can be switched off (see algorithm option switches),

apply_closed_world_to_adders([], _, Cio, Cio, HOD, HOD).

% zero pointer can be skipped.. (already zero of course)
apply_closed_world_to_adders([_Value/0|QTail], Adders, Cin, Cout, HOD, HOD):-
	!,
	apply_closed_world_to_adders(QTail, Adders, Cin, Cout, HOD, HOD).

apply_closed_world_to_adders([Value/DQ|QTail], Adders, Cin, Cout, HODin, HODout):-
	list_map([DQ], DM),
	\+ memberchk(adder(DM, _), Adders),	    % if not influenced itself
	member(adder(_, L), Adders),		        % and it influences something
	(	memberchk(pos(DM), L);
		memberchk(neg(DM), L);                   % only pos neg, no up and down yet
	        memberchk(pos_neg_mult(DM), L);
		memberchk(pos_neg_diw(DM), L)
	),
	cio_d(Cin, Derivable),
	\+ get_quantity_sign(DM, Derivable, _),		% and it's sign is not known
	!,
	list_map([0], ZeroMap),				            % then it can be assumed zero
	% Analyse simple & zero equality should be false not to
	% create invalid pointers in Qlist or Adders
	etrace(solve_cw_zero, [Value], resolve),
	canonical_form(relation(DM, =, ZeroMap), Rel),
	append_relation(Rel, Cin, Cnew, _, false),
	% this may fail if the model supplies information in contradiction with the assumption
	!,
	%New FL mar 2012 record HOD assumptions
	(
	  Value = second_derivative(Q)
	->  % a 2nd order assumption: record in hod_info
	  (
	    select(hod_info(Q, FOD, zero, TOD, X, Y, Z), HODin, Rest)
	  ->
	    HOD = [hod_info(Q, FOD, zero, TOD, X, Y, Z)|Rest]
	  ;
	    HOD = [hod_info(Q, unknown, zero, unknown, [], nil, [])|HODin]
	  )
	;

	  (
	    Value = second_derivative(Q)
	  ->  % a 3rd order assumption: record in hod_info
	    (
	      select(hod_info(Q, FOD, SOD, zero, X, Y, Z), HODin, Rest)
	    ->
	      HOD = [hod_info(Q, FOD, SOD, zero, X, Y, Z)|Rest]
	    ;
	      HOD = [hod_info(Q, unknown, unknown, zero, [], nil, [])|HODin]
	    )
	  ; % any lower order assumption...
	    HODin = HOD
	  )
	),
	apply_closed_world_to_adders(QTail, Adders, Cnew, Cout, HOD, HODout).

% quantity not zero-assumable: ignore it

apply_closed_world_to_adders([_|QTail], Adders, Cin, Cout, HODin, HODout):-
	apply_closed_world_to_adders(QTail, Adders, Cin, Cout, HODin, HODout).



% switch is off
test_2nd_order_continuity_constraints(_Sod, _Adders, _Adderstrings, IntAdders, IntAdders, Cnew, Cnew):-
	flag(second_order_continuity, F, F),
	algorithm_assumption_flag(F, fail, second_order_continuity),
	!.

% switch is on
test_2nd_order_continuity_constraints(Sod, Adders, Adderstrings, IntAddersIn, IntAddersOut, Cin, Cout):-
	flag(second_order_continuity, F, F),
	algorithm_assumption_flag(F, true, second_order_continuity),
	%etrace(solve_sod_continuity_start, [], resolve),
	test_2nd_order_continuity_constraints2(Sod, Adders, Adderstrings, IntAddersIn, IntAddersOut, Cin, Cout),
	%etrace(solve_sod_continuity_end, [], resolve),
	!.


% done / no 2nd order derivatives known
test_2nd_order_continuity_constraints2([], _Adders, _Adderstrings, IntAdders, IntAdders, Cnew, Cnew):-
	!.

% 2nd order derivative with unchanged causal model for Qty.
test_2nd_order_continuity_constraints2([hod_info(Par, Der, SODer, TODer, OldList, _TODList, OldPath)|Tail], Adders, Adderstrings, IntAddersIn, IntAddersOut, Cin, Cout):-
	OldList = [_,_|_], % only multiple influenced should be checked...
	SODer \= unknown, %this would not impose constraints.
	memberchk(adder(derivative(Par), NewList), Adders),
	memberchk(adder(derivative(Par), NewPath), Adderstrings),
	translate_addlist_to_2ndorder(NewList, NList1), %translate to make same form as oldlist
	sort(NList1, NList),
	sort(OldList, OList),
	NList = OList,
	NewPath = OldPath, %this could be the only check, the single list of local influences is old code...
	!, % causal model unchanged for this qty. add constraint
	do_2ndorder_continuity(Par, Der, SODer, TODer, Constraint),
	(
	  Constraint \= nil
	->
	  intern_representation(Constraint, Relation, Cin, Cnew),
	  try_append(append_relation(Relation, Cnew, Cnew1, _Added, true, IntAddersIn, IntAdders))
	;
	  Cin = Cnew1,
	  IntAdders = IntAddersIn
	),
	test_2nd_order_continuity_constraints2(Tail, Adders, Adderstrings, IntAdders, IntAddersOut, Cnew1, Cout).

% no compatible adder for this 2nd order derivative
% (causal model changed)
test_2nd_order_continuity_constraints2([_|Tail], Adders, Adderstrings, IntAddersIn, IntAddersOut, Cin, Cout):-
	test_2nd_order_continuity_constraints2(Tail, Adders, Adderstrings, IntAddersIn, IntAddersOut, Cin, Cout).



% no progress, discard adders and derive unknown for these quantities.
%
% NEW FL sept 2010: assume ambiguity in complex causal loops
resolve_adders_2(_, _, Cio, Cio, false, [], _, _, _).


% first time, or progress
resolve_adders_2(Adders, TrAdders, Cin, Cout, true, AmbiguousDerivatives, Loops, CAPairs, Sod):-
	resolve(Adders, TrAdders, Cin, Cnew, [], Unresolved, [], UnTrAd, Progress, AmbDer1, Loops, CAPairs, Sod),
	resolve_adders_2(Unresolved, UnTrAd, Cnew, Cout, Progress, AmbDer2, Loops, CAPairs, Sod),
	append(AmbDer1, AmbDer2, AmbiguousDerivatives).



% FL May 2004: added adders in normal representation for tracer use.
%
% FL July 2004: If semi-ambiguous result is obtained:
% (>=) then only two ambiguous alternatives are generated.


% resolve(+Adders, +TracerAdders, +Cin, -Cout, +[],
%         -Unresolved, +[], -UnresolvedTracerAdders, -Progress)
% for each adder:
% - if not all values of terms are known (incomplete):
%   put adder on Unresolved list,
%   progress for that addder = false,
%   but true in general if other adders produce progress
% - elseif single adder: add result, progress = true
% - else construct balance expression and evaluate:
% - if ambiguous (not enough information):
%   add different values for Q upon backtracking, progress = true
% - if positive, zero, or negative: add this result, progress = true


% Comparative Analysis clause:
% This just checks the Comparative Analysis pairs to see if one has
% gotten both quantities resolved. In this case CA is done.
% NB this also matches the empty list on the adders, but it will only
% match as long as there are unresolved CA pairs and this list can
% ends as each iteration through the clause removes a pair from
% the list. (no looping)
% This clause should be placed before the stopclause to ensure CA is
% done!
%
resolve(Adders, TrAdders, Cin, Cout, UnresolvedIn, UnresolvedOut, UTracer, UTracerOut,
	Progress, AmbDer, Loops, CAPairsIn, Sod):-
    select(ca_pair(Q1, Sign1, AddList1, Q2, Sign2, AddList2), CAPairsIn, NewCAPairs),
    Sign1 \= unresolved,
    Sign2 \= unresolved,
    !, % found CA pair ready for calculation
    etrace(solve_ca_set, [Q1, Q2], resolve),
    (
      unequal_ca_signs(Sign1, Sign2)
    ->
      % no reason for CA, continue as we were...
      etrace(solve_ca_unequal, [], resolve),
      Cin = Cnew,
      UnresolvedIn = NewUnresolved,
      Adders = NewAdders
    ;
      % do CA:
      etrace(solve_ca_bal, [AddList1, AddList2, Pos, Neg], resolve),
      make_ca_balance(Q1, Q2, AddList1, AddList2, Cin, Ctest1, Balance, BV),
      try_balance(append_relation(Balance, Ctest1, Ctest2, _, false)),
        % Analyse simple & zero equality should be false not to create an invalid Q pointers to testcio indexes
      % get value of balancequantity
      cio_d(Ctest2, TestDerivable),
      !,
      (
        %test CA quantity sign (also >= and =<)
        get_ca_balance_result(BV, TestDerivable, Relation) %because no analyse zero & simple equality, BV is still a valid pointer
      ->
        % valid CA result, add this
        etrace(solve_ca_bal_res, [Q1, Q2, Relation, Pos, Neg], resolve),
        add_ca_result(derivative(Q1), derivative(Q2), Relation, Cin, Cnew, [UnresolvedIn, Adders], [NewUnresolved, NewAdders])
      ;
        % no outcome for CA, continue as we were...
        etrace(solve_ca_bal_no_res, [Q1, Q2, Pos, Neg], resolve),
        Cin = Cnew,
        UnresolvedIn = NewUnresolved,
        Adders = NewAdders
      ),
      etrace(solve_ca_done, [Q1, Q2], resolve)
    ),
    !,
    resolve(NewAdders, TrAdders, Cnew, Cout, NewUnresolved, UnresolvedOut,
            UTracer, UTracerOut, Progress, AmbDer, Loops, NewCAPairs, Sod).



% all done, return results:

resolve([], [], Cin, Cin, Unresolved, Unresolved, UTracer, UTracer, false, [], _Loops, _CAPairs, _Sod).

% Multiplication and Division clause
%
% if all P* and P/ have known value and derivative signs they are
% translated to P+ or P- according to their effect in this state
%
resolve([adder(Q, AddList)|Tail], [adder(TrQ, TrList)|TrTail], Cin, Cout,
        Unresolved, UnresolvedOut, UTracer, UTracerOut, Progress, AmbDer, Loops, CAPairs, Sod):-
	contains_p_star_p_slash(AddList),
	cio_q_d(Cin, QList, Derivable),
	translate_p_star_p_slash(AddList, TrList, QList, Derivable, TrQ, NewAddList, NewTrList),
	!, % translation succesful: all P* component values/derivatives are sufficiently known
	etrace(solve_ir_translate_p_star, [TrQ, TrList], resolve),
	% note this may fail
	resolve([adder(Q, NewAddList)|Tail], [adder(TrQ, NewTrList)|TrTail], Cin, Cout,
        Unresolved, UnresolvedOut, UTracer, UTracerOut, Progress, AmbDer, Loops, CAPairs, Sod).

/*
% Multiplication clause:
% Succeeds if Adder consists of only 2 P* pos_neg_mult items ...
% ...and values / derivative are suffictiently known to translate these
% to normal P's.
% Do this, then call resolve (again)
%
resolve([adder(Q, AddList)|Tail], [adder(TrQ, TrList)|TrTail], Cin, Cout,
        Unresolved, UnresolvedOut, UTracer, UTracerOut, Progress, AmbDer, Loops, CAPairs, Sod):-
	all_p_star(AddList),
	cio_q_d(Cin, QList, Derivable),
	translate_p_star(AddList, TrList, QList, Derivable, NewAddList, NewTrList),
	!, % translation succesful: all P* component values/derivatives are sufficiently known
	etrace(solve_ir_translate_p_star, [TrQ, TrList], resolve),
	% note this may fail
	resolve([adder(Q, NewAddList)|Tail], [adder(TrQ, NewTrList)|TrTail], Cin, Cout,
        Unresolved, UnresolvedOut, UTracer, UTracerOut, Progress, AmbDer, Loops, CAPairs, Sod).

% Division clause:
% Succeeds if Adder consists of only 1 P* pos_neg_mult item
% and 1 P/ pos_neg_diw item...
% ...and values / derivative are suffictiently known to translate
% these to normal P's. Do this, then call resolve
%
resolve([adder(Q, AddList)|Tail], [adder(TrQ, TrList)|TrTail], Cin, Cout,
        Unresolved, UnresolvedOut, UTracer, UTracerOut, Progress, AmbDer, Loops, CAPairs, Sod):-
	all_p_star_p_slash(AddList),
	cio_q_d(Cin, QList, Derivable),
	sort(AddList, AddList1), % puts the P/ first
	sort(TrList, TrList1),
	translate_p_star_p_slash(AddList1, TrList1, QList, Derivable, NewAddList, NewTrList),
	!, % translation succesful: all P* component values/derivatives are sufficiently known
	etrace(solve_ir_translate_p_star_p_slash, [TrQ, TrList], resolve),
	% note this may fail
	resolve([adder(Q, NewAddList)|Tail], [adder(TrQ, NewTrList)|TrTail], Cin, Cout,
        Unresolved, UnresolvedOut, UTracer, UTracerOut, Progress, AmbDer, Loops, CAPairs, Sod).
*/



% adder incomplete -> put on Unresolved list

resolve([adder(Q, AddList)|Tail], [adder(TrQ, TrList)|TrTail], Cin, Cout,
        Unresolved, UnresolvedOut, UTracer, UTracerOut, Progress, AmbDer, Loops, CAPairs, Sod):-
    cio_q_d(Cin, QList, Derivable),
    % check quantities in Addlist for known values,
    % transform pos, neg to up, down & noforce,
    % return incomplete or complete to indicate if all values are known
    complete_adder(AddList, Derivable, NewAddList, Incomplete, TrList, TracerAddList),
    Incomplete = incomplete,
    % also check if solvable known loop causes incompleteness! (FL sept 2010)
    \+ loop_completes_addlist(AddList, Q, QList, Derivable, Loops, _Type, _Loop), % = causal loop predicate
    \+ at_bottleneck_in_loop(AddList, TrList, Q, QList, Derivable, Loops, _, _),
    !, %is it ok to cut here? because below it says: note this may fail... choicepoint seems unnecessary... Fl Feb 07 tempflo
    etrace(solve_ir_set, [TrQ, TrList, QList, Derivable], resolve),
    etrace(solve_ir_unres, [TracerAddList], resolve),
    % note this may fail
    resolve(Tail, TrTail, Cin , Cout, [adder(Q, NewAddList)|Unresolved],
	        UnresolvedOut, [adder(TrQ, TrList)|UTracer], UTracerOut, Progress, AmbDer, CAPairs, Loops, Sod).


% adder complete, single influence -> evaluate & add results
% NB cannot be completed by solvable loop, these are always complex and
% solved at an external entry point.

resolve([adder(Q, [Influence])|Tail], [adder(TrQ, [TrInf])|TrTail], Cin, Cout,
        UnresolvedIn, UnresolvedOut, UTracer, UTracerOut, Progress, AmbDer, Loops, CAPairsIn, Sod) :-
    cio_q_d(Cin, QList, Derivable),
    get_sign(Influence, Derivable, Sign),
    !,
    \+postfilter_tod(TrQ, Sign, Sod),
    etrace(solve_ir_set, [TrQ, [TrInf], QList, Derivable], resolve),
    etrace(solve_ir_single_res, [TrInf, Sign], resolve),
    etrace(solve_ir_res, [TrQ, Sign], resolve),
    add_resolution_result(Q, TrQ, Sign, Cin, Cnew, Progress, TailProgress,
	                  [UnresolvedIn, Tail], [Unresolved1, NewTail]), % ANA
    comparative_analysis_update_candidates(TrQ, Sign, CAPairsIn, CAPairsNew),
    !,
    % note this may fail
    resolve(NewTail, TrTail, Cnew, Cout, Unresolved1, UnresolvedOut,
            UTracer, UTracerOut, TailProgress, AmbDer, Loops, CAPairsNew, Sod).


% adder complete -> first try sign addition,
% if that fails: next clause will construct balance

resolve([adder(Q, AddList)|Tail], [adder(TrQ, TrList)|TrTail], Cin, Cout,
        UnresolvedIn, UnresolvedOut, UTracer, UTracerOut, Progress, AmbDer, Loops, CAPairsIn, Sod) :-
    cio_q_d(Cin, QList, Derivable),
    try_sign_addition(AddList, Derivable, zero, Sign, TrList, TrEffects),% NB fails if ambiguous
    !,
    \+postfilter_tod(TrQ, Sign, Sod),
    etrace(solve_ir_set, [TrQ, TrList, QList, Derivable], resolve),
    etrace(solve_ir_add, [TrEffects], resolve),
    etrace(solve_ir_res, [TrQ, Sign], resolve),
    add_resolution_result(Q, TrQ,Sign, Cin, Cnew, Progress, TailProgress,
	                       [UnresolvedIn, Tail],
	                       [NewUnresolved, NewTail]),
    comparative_analysis_update_candidates(TrQ, Sign, CAPairsIn, CAPairsNew),
    !,
    resolve(NewTail, TrTail, Cnew , Cout, NewUnresolved, UnresolvedOut,
	        UTracer, UTracerOut, TailProgress, AmbDer, Loops, CAPairsNew, Sod).

% = causal loop predicate
% adder complete -> try if loop completes the adder,
% if so, then assume ambiguity, but recheck the node later...
resolve([adder(Q, AddList)|Tail], [adder(TrQ, TrList)|TrTail], Cin, Cout,
        UnresolvedIn, UnresolvedOut, UTracer, UTracerOut, Progress, AmbDer, Loops, CAPairsIn, Sod) :-
    cio_q_d(Cin, QList, Derivable),
    loop_completes_addlist(AddList, Q, QList, Derivable, Loops, Type, Loop),
    !,
    (
      Type = pos, % simple loop with positive feedback: try sign addition of external influences
		  % if this fails, assume ambiguity and propagate
      subtract_loop_add_item(TrList, AddList, Loop, TrQ, QList, NewTrList, NewAddList),
      try_sign_addition(NewAddList, Derivable, zero, Sign, NewTrList, TrEffects)% NB fails if ambiguous
    ->
      etrace(solve_ir_set, [TrQ, TrList, QList, Derivable], resolve),
      etrace(solve_ir_add, [TrEffects], resolve),
      etrace(solve_ir_res, [TrQ, Sign], resolve),
      true
    ;
      Sign = [neg, zero, pos],
      %assume ambiguity, but put on unresolved list...
      etrace(solve_ir_set, [TrQ, TrList, QList, Derivable], resolve),
      etrace(solve_ir_loop, [TrQ, Sign], resolve)
    ),
    !, % no backtracking on this part
    add_resolution_result(Q, TrQ, Sign, Cin, Cnew, Progress, TailProgress,
			  [[adder(Q, AddList)|UnresolvedIn], Tail],
			  [NewUnresolved, NewTail]),
    AmbDer = [TrQ|TailAmbDer],
    %add_resolution_result(Q, TrQ,Sign, Cin, Cnew, Progress, TailProgress,
%	                       [UnresolvedIn, Tail],
%	                       [NewUnresolved, NewTail]),
    comparative_analysis_update_candidates(TrQ, Sign, CAPairsIn, CAPairsNew),
    resolve(NewTail, TrTail, Cnew , Cout, NewUnresolved, UnresolvedOut,
	        [adder(TrQ, TrList)|UTracer], UTracerOut, TailProgress, TailAmbDer, Loops, CAPairsNew, Sod).

% = causal loop predicate
% adder complete -> try if node is bottleneck in adder,
% if so, then propagate value of known addernode.
resolve([adder(Q, AddList)|Tail], [adder(TrQ, TrList)|TrTail], Cin, Cout,
        UnresolvedIn, UnresolvedOut, UTracer, UTracerOut, Progress, AmbDer, Loops, CAPairsIn, Sod) :-
    cio_q_d(Cin, QList, Derivable),
    at_bottleneck_in_loop(AddList, TrList, Q, QList, Derivable, Loops, KnownAddList, KTrList),
    try_sign_addition(KnownAddList, Derivable, zero, Sign, KTrList, TrEffects),% NB fails if ambiguous
    !,
    %assume ambiguity, but put on unresolved list...
    etrace(solve_ir_set, [TrQ, TrList, QList, Derivable], resolve),
    etrace(solve_ir_loop_bottleneck, [TrEffects], resolve),
    etrace(solve_ir_res, [TrQ, Sign], resolve),
    !, % no backtracking on this part
    add_resolution_result(Q, TrQ, Sign, Cin, Cnew, Progress, TailProgress,
			  [[adder(Q, AddList)|UnresolvedIn], Tail],
			  [NewUnresolved, NewTail]),
    ( Sign = [_|_] -> AmbDer = [TrQ|TailAmbDer] ; AmbDer = TailAmbDer),
    %add_resolution_result(Q, TrQ,Sign, Cin, Cnew, Progress, TailProgress,
%	                       [UnresolvedIn, Tail],
%	                       [NewUnresolved, NewTail]),
    comparative_analysis_update_candidates(TrQ, Sign, CAPairsIn, CAPairsNew),
    resolve(NewTail, TrTail, Cnew , Cout, NewUnresolved, UnresolvedOut,
	        [adder(TrQ, TrList)|UTracer], UTracerOut, TailProgress, TailAmbDer, Loops, CAPairsNew, Sod).


% adder complete -> construct balance, evaluate & add results

resolve([adder(Q, AddList)|Tail], [adder(TrQ, TrList)|TrTail], Cin, Cout,
        UnresolvedIn, UnresolvedOut, UTracer, UTracerOut, Progress, AmbDer, Loops, CAPairsIn, Sod) :-
    cio_q_d(Cin, QList, Derivable),
    % check quantities in Addlist for known values, transform pos, neg to up, down & noforce
    complete_adder(AddList, Derivable, CompleteAddList, Incomplete, _, _),
    Incomplete = complete,
    etrace(solve_ir_set, [TrQ, TrList, QList, Derivable], resolve),
    etrace(solve_ir_bal, [TrList, Pos, Neg], resolve),
    % make_balance [BalanceValue, min1, min2, ...] = [pos1, pos2, ...] (intern representation)
    make_balance(TrQ, CompleteAddList, Cin, Ctest1, Balance, BV),
    % add and evaluate balance relation in testcio (balance not added to relations)
    % try_balance gives feedback if append_relation fails
    try_balance(append_relation(Balance, Ctest1, Ctest2, _, false)), % Analyse simple & zero equality should be false    not to create an invalid Q pointers to testcio indexes
    % get value of balancequantity
    cio_d(Ctest2, Derivable2),
    %test quantity sign (also >= and =<)
    get_balance_result(BV, Derivable2, Sign),%because no analyse zero & simple equality, BV is still a valid pointer
    !, %no backtracking on this part

    (
    memberchk(Sign, [zero, pos, neg]) % definitive result
    ->
        (
	\+postfilter_tod(TrQ, Sign, Sod),
	etrace(solve_ir_bal_res, [TrQ, Pos, Neg, Sign], resolve),
        add_resolution_result(Q, TrQ,Sign, Cin, Cnew, Progress, TailProgress,
	                       [UnresolvedIn, Tail],
	                       [NewUnresolved, NewTail]),
	AmbDer = TailAmbDer,
	!)
    ;   % else: ambiguous result, Sign is a list of possibilities in this case
	(
	postfilter_tod_ambiguous(TrQ, Sign, FilteredSign, Sod),
	etrace(solve_ir_bal_unres, [TrQ, Pos, Neg, FilteredSign], resolve),
        %add_resolution_result(Q, ambiguous, Cin, Cnew, Progress, TailProgress,
	add_resolution_result(Q, TrQ, FilteredSign, Cin, Cnew, Progress, TailProgress,
	                       [UnresolvedIn, Tail],
	                       [NewUnresolved, NewTail]),
	AmbDer = [TrQ|TailAmbDer]
	)
    ),
    comparative_analysis_update_candidates(TrQ, Sign, CAPairsIn, CAPairsNew),
   % note this may fail
    resolve(NewTail, TrTail, Cnew , Cout, NewUnresolved, UnresolvedOut,
	        UTracer, UTracerOut, TailProgress, TailAmbDer, Loops, CAPairsNew, Sod).



% postfilter_tod/3
% postfilter of third order derivative constraints.
% see Kuipers p.243, fig. 10.3
% (New Jan 2012 FL)
% NB succeeds if state (in fact a transition to this state) should be
% filtered out.


% no sod known from previous state:
% postfilter irrelevant
postfilter_tod(_, _, []):-
	!,
	fail.


% Par moving up,
% previous state: steady der, steady sod and neg tod,
% precursor: moving down (but we don't check this, is irrelevant..)
%
%  -  ==> 0,0,- ==> +
postfilter_tod(derivative(Par), pos, Sod):-
	member(hod_info(Par, zero, zero, neg, _, _, _), Sod),
	etrace(hod_postfilter, [Par, zero, zero, neg, pos], resolve),
	!.

% Par moving down,
% previous state: steady der, steady sod and pos tod,
% precursor: moving up (but we don't check this, is irrelevant..)
%
%  +  ==> 0,0,+ ==> -
postfilter_tod(derivative(Par), neg, Sod):-
	member(hod_info(Par, zero, zero, pos, _, _, _), Sod),
	etrace(hod_postfilter, [Par, zero, zero, pos, neg], resolve),
	!.



% postfilter_tod_ambiguous/4
% postfilter of third order derivative constraints.
% see Kuipers p.243, fig. 10.3
% (New Jan 2012 FL)
% always succeeds and filters out any illegal signs from the
% list of ambiguous signs.

% no sod known from previous state:
% postfilter irrelevant:
postfilter_tod_ambiguous(_, Sign, Sign, []):-
	!.

% active filter:
postfilter_tod_ambiguous(derivative(Par), SignIn, SignOut, Sod):-
	member(hod_info(Par, zero, zero, Sign, _, _, _), Sod),
	opposite_sign(Sign, Opp), %fails on zero on purpose! (not a postfilter case)
	!,
	etrace(hod_postfilter, [Par, zero, zero, Sign, Opp], resolve),
	delete(Opp, SignIn, SignOut).

% in any other case: return all
postfilter_tod_ambiguous(_, Sign, Sign, _).

opposite_sign(pos, neg).
opposite_sign(neg, pos).


% Balance should always be appendible, because BalanceValue is a newly generated quantity
try_balance(X):-
    call(X),!.
try_balance(_):-
    etrace(solve_append_bal_error, _, general),
    fail.

% appending a result is not always possible, sometimes a derivative is already
% known through another information source (defined in model, inequality etc.)
% In this case the tracer shows that resolution result was not added.
try_append(X):-
    call(X),
    !.

try_append(X):-
    etrace(solve_add_ir_res_fail, X, resolve),
    fail.

% add_resolution_result(+Q, +TrQ, +Sign, +Cin, -Cout, -Progress, +TailProgress,
%	                  +[UnresolvedIn, AdderTailIn], -[UnresolvedOut, AdderTailOut])
% Q is the quantity in question (intern). TrQ is its normal name, for tracing
% Sign is a single value, or a list of values (ambiguity)
% unresolved and adderlist are passed on to update their pointers,
% because analyse zero / simple equality can change these

% valid new result: let progress be true if not already derivable,

add_resolution_result(Q, _TrQ, Sign, Cin, Cout, Progress, TailProgress,
	                  [UnresolvedIn, AdderTailIn], [UnresolvedOut, AdderTailOut]):- % ANA
    memberchk(Sign, [zero, pos, neg]),
	zero_relation_sign(Rel, Sign),
	list_map([0], Empty),
	right_order(relation(Q, Rel, Empty), Relation1),
	canonical_form(Relation1, Relation),
    append_split(UnresolvedIn, AdderTailIn, I, OtherAddersIn), % makes one list, for easy passing on with append relatoin. pointers can be updated, but list stays intact.
    try_append(append_relation(Relation, Cin, Cout, Added, true, OtherAddersIn, OtherAddersOut)),
    split_append(UnresolvedOut, AdderTailOut, I, OtherAddersOut), % splits the list again at the same point.
	(Added = [],
	  Progress=TailProgress;
	  Progress=true),
	!.

% ambiguous influence on Q: return all possibilities (upon backtracking)

add_resolution_result(Q, TrQ, Sign, Cin, Cout, Progress, TailProgress,
	                  [UnresolvedIn, AdderTailIn], [UnresolvedOut, AdderTailOut]):- % ANA
	member(S, Sign),
	nth1(N, Sign, S),
	etrace(pathstart, resolve),
	etrace(solve_ir_amb_nth, [TrQ, N, S], resolve),
	zero_relation_sign(Rel, S),
	list_map([0], Empty),
	right_order(relation(Q, Rel, Empty), Relation1),
	canonical_form(Relation1, Relation),
	once((
        append_split(UnresolvedIn, AdderTailIn, I, OtherAddersIn),
        try_append(append_relation(Relation, Cin, Cout, Added, true, OtherAddersIn, OtherAddersOut)),
        split_append(UnresolvedOut, AdderTailOut, I, OtherAddersOut),
	    (Added = [],
	    Progress=TailProgress;
	    Progress=true)
        )).

% place a relation in the right order: internaly only =, > and >= are used (not < and =<)
right_order(relation(L, <, R), relation(R, >, L)) :- !.
right_order(relation(L, =<, R), relation(R, >=, L)) :- !.
right_order(R, R):- !.


% append_split(+L1, +L2, -I, -All)
% append to lists, return the index where the (virtual) separation is

append_split(L1, L2, I, All):-
    length(L1, I),
    append( L1, L2, All).


% split_append(-L1, -L2, +I, +All) % ANA
% divide a list into two lists splitting at index I

split_append([], All, 0, All):-!.

split_append([H|L1], L2, I, [H|T]):-
    J is I - 1,
    split_append(L1, L2, J, T).


% FL Feb 2007 moved write_addone clauses to interface_garp3.pl


% complete_adder: takes an addList and evaluates to zero
% if sign is known then change notation to up/down/noforce
% otherwise return incomplete and semi transformed list.
% unknown quantities are passed on for tracer

complete_adder([], _, [], complete, [], []):-!. %not yet incomplete -> complete

complete_adder([], _, [], incomplete, [], []):-!. %already incomplete

complete_adder([H|T], Derivable, [Result | NewAddList], Incomplete, [_|TT], TraceList):-
    get_balance_sign(H, Derivable, Result),  %returns: up(Q), down(Q), noforce(Q) or fails
    !,
    complete_adder(T, Derivable, NewAddList, Incomplete, TT, TraceList).

% could not get quantity sign -> set incomplete indicator & continue with rest of list.
complete_adder([H|T], Derivable, [H | NewAddList], incomplete, [HT|TT], [HT|TraceTail]):-
    !,
    complete_adder(T, Derivable, NewAddList, incomplete, TT, TraceTail).





%%	loop_completes_addlist(+AddList, +QList, +Derivable, +Loops)
%
%	Succeeds if every item is known or influencing the quantity
%	as part of a fully known loop
%

% no loops in the model, immediate fail.
loop_completes_addlist(_, _, _, _, [], _, _):-
	!,
	fail.

% quantity already has a value, no completion with loop relevant,
% at this point in fact, if a loop node has a value it indicates the
% loop is already handled and should be checked once its assumed effects
% have propagated around the loop.
%
loop_completes_addlist(_, BVNode, QList, Derivable, _, _, _):-
	map_list(BVNode, [IN]),
	memberchk(Quantity/IN, QList),
	sign_known(Quantity, QList, Derivable),
	!,
	fail.

% do the real check
% - first find the appropriate loop
% - then check if all external nodes are known, if so, the node is
% computable via the loop
% (this should include all nodes in the addlist of)
% - and we are at an external entry point (otherwise it could be a
% bottleneck point
%
loop_completes_addlist(_AddList, Q, QList, Derivable, Loops, Type, loop(Path, Ext, Ein, AddPath, Type)):-
	member(loop(Path, Ext, Ein, AddPath, Type), Loops),
	%memberchk(Type, [pos, neg]), % At this point we only deal with non complex loops!
	member(Node, Path),
	memberchk(Node/IN, QList),
	list_map([IN], BVNode),
	BVNode = Q, % Find a loop that contains Q
	member(ext(Node, _), Ein),
	!, % at this point only one loop possible per quantity (otherwise a superloop would cover both)
	known_loop(loop(Path, Ext, Ein, AddPath, Type), QList, Derivable).



%%	at_bottleneck_in_loop(+AddList, +QList, +Derivable, +Loops)
%
%	Succeeds if every item in addlist is part of loop,
%	no external influences on this node of the loop.
%

% no loops in the model, immediate fail.
at_bottleneck_in_loop(_, _, _, _, _, [], _, _):-
	!,
	fail.

% quantity already has a value, no completion with loop relevant,
% at this point in fact, if a loop node has a value it indicates the
% loop is already handled and should be checked once its assumed effects
% have propagated around the loop.
%
at_bottleneck_in_loop(_, _, BVNode, QList, Derivable, _, _, _):-
	map_list(BVNode, [IN]),
	memberchk(Quantity/IN, QList),
	sign_known(Quantity, QList, Derivable),
	!,
	fail.

% do the real check
at_bottleneck_in_loop(AddList, TrList, Q, QList, Derivable, Loops, NewAddList, NTrList):-
	map_list(Q, [IQ]),
	memberchk(QName/IQ, QList),
	findall(loop(Path, External, ExtInf, _, _),
		(
		  member(loop(Path, External, ExtInf, _, _), Loops),
		  %memberchk(Node, Path), % quick checks to see if both nodes are in loop
		  memberchk(QName, Path),
		  %step_in_path(Path, [Node, QName]), % Node influences QName in loop
		  \+memberchk(ext(QName, _), ExtInf) %we are NOT at an external entry point of the loop
		),
		NodeLoops),
	NodeLoops \= [],
	forall(member(Loop, NodeLoops), known_loop(Loop, QList, Derivable)),
	at_bottleneck_in_loop_core(AddList, TrList, QName, QList, Derivable, NodeLoops, NewAddList, NTrList),
	AddList \= NewAddList,
	NewAddList \= [].



% done, every item is known or part of a fully known loop
at_bottleneck_in_loop_core([], [], _, _, _, _, [], []).

% node is known
at_bottleneck_in_loop_core([PosNegNode|AddList], [H|T], Q, QList, Derivable, Loops, [PosNegNode|NewAddList], [H|NT]):-
	PosNegNode =.. [_, Node],
	get_quantity_sign(Node, Derivable, _),
	!,
	at_bottleneck_in_loop_core(AddList, T, Q, QList, Derivable, Loops, NewAddList, NT).

% Addernode influences Quantity as part of a known loop
at_bottleneck_in_loop_core([PosNegNode|AddList], [_H|T], QName, QList, Derivable, Loops, NewAddList, NT):-
	PosNegNode =.. [_, BVNode],
	map_list(BVNode, [IN]),
	memberchk(Node/IN, QList),
	forall(member(loop(Path, _External, _ExtInf, _, _), Loops),
		(
		  memberchk(Node, Path), % quick check to see if nodes is in loop
		  step_in_path(Path, [Node, QName]) % Node influences QName in loop
		)
	      ),
	at_bottleneck_in_loop_core(AddList, T, QName, QList, Derivable, Loops, NewAddList, NT).



%%	step_in_path(Path, [A, B])
%
%	checks if two nodes are sucessive in a list
%

% done
step_in_path([A, B|_], [A, B]):-
	!.

% next...
step_in_path([_|T], [A, B]):-
	step_in_path(T, [A, B]).



% loop: known if all external factors are known
known_loop(loop(_Path, External, _ExtInf, _AdderPath, _Type), QList, Derivable):-
	forall(member(Element, External), sign_known(Element, QList, Derivable)).




sign_known(Quantity, QList, Derivable):-
	memberchk(Quantity/I, QList),
	list_map([I], BV),
	get_quantity_sign(BV, Derivable, _Sign).

subtract_loop_add_item(TrList, AddList, loop(Path, _Ext, _Ein, _AddPath, _Type), TrQ, QList, NewTrList, NewAddList):-
	findall(Node, (member(Node1, Path), %for all nodes in path
		       step_in_path(Path, [Node1, TrQ]), %check if they influence current node
		       member(PosNeg, [pos, neg]), Node =.. [PosNeg, Node1]), %construct pos and neg adder of this node
		LoopAdders1),
	list_to_set(LoopAdders1, LoopAdders),
	subtract(TrList, LoopAdders, NewTrList), % subtract these influences within the loop from the addlist
	findall(BV, (member(LA1, LoopAdders), LA1 =.. [_, LA],
		     memberchk(LA/I, QList), list_map([I], BV1),
		     member(PosNeg, [pos, neg]), BV =.. [PosNeg, BV1]),
		BVLoopAdders),
	subtract(AddList, BVLoopAdders, NewAddList). % do the same for the bitvector addlist.



% FL june 07: before constructing balance, first try sign addition...
% this can save some valuable inequality reasoning time
%
try_sign_addition(List, Derivable, SignIn, SignOut, TrList, EfList):-
	remove_weaker2(Derivable, Derivable, StrongDerivable), %since we draw conclusions on the first relation we find...
	!,
	try_sign_addition2(List, StrongDerivable, SignIn, SignOut, TrList, EfList).

% returns Sign and annotated tracerAddlist: Add/Sign/Effect
try_sign_addition2([], _Derivable, Sign, Sign, [],  []).

try_sign_addition2([H|T], Derivable, SignIn, SignOut, [TrH|TrT], [TrQ/Sign/Effect|EffectsT]):-
	get_add_sign(H, Derivable, Sign, Effect), % nb fails if unknown sign
	!,
	add_signs(SignIn, Effect, NewSign), % nb fails if result is unknown / ambiguous
	!,
	TrH =.. [_,TrQ],
        try_sign_addition2(T, Derivable, NewSign, SignOut, TrT, EffectsT).


% no effect, continue
get_add_sign(noforce(_Quantity), _, zero, zero):- !.

% get interpreted sign
get_add_sign(Add, Relations, Sign, Result):-
    Add =.. [Dir, Quantity],
    list_map([0], Z),
    derive_quantity_sign(Quantity, Relations, Z, Sign),
    !,
    interpret_sign(Sign, Dir, Result),
    !.

% nb differs from get_quantity_sign
% because ambiguous signs are also accepted
% NB no canonical form needed here (FL nov 07)
derive_quantity_sign(Z, _Relations, Z, zero):-!.

derive_quantity_sign(Quantity, Relations, Z, Sign):-
	member(relation(Quantity, Relation, Z), Relations),
	zero_sign(Relation, Sign).

derive_quantity_sign(Quantity, Relations, Z, Sign):-
	member(relation(Z, Relation, Quantity), Relations),
	inverse(Relation, Inverse),
	zero_sign(Inverse, Sign).

%relations to signs
zero_sign(>, pos).
zero_sign(>=, pos_zero).
zero_sign(=, zero).
zero_sign(<, neg).
zero_sign(=<, neg_zero).

% interpret_sign(+signIn, +Direction, -signout).
interpret_sign(zero, _, zero).
interpret_sign(pos, up, pos).
interpret_sign(neg, up, neg).
interpret_sign(pos, pos, pos).
interpret_sign(neg, pos, neg).
interpret_sign(pos, down, neg).
interpret_sign(neg, down, pos).
interpret_sign(pos, neg, neg).
interpret_sign(neg, neg, pos).

% add_signs(+Sign1, +Sign2, NewSign)
% zero has no effect: keep sign
add_signs(Sign, zero, Sign).
add_signs(zero, Sign, Sign).
% equal signs: keep sign
add_signs(Sign, Sign, Sign).
% strong and weak: keep strong
add_signs(pos_zero, pos, pos).
add_signs(neg_zero, neg, neg).
% all others: ambiguous >>> fail!


% make_balance(+TracerQ, +AddList, +Cin, -Cout, -Balance, -BalanceValue_intern)
% make balance expression in intern
% representation: [min, min, BV] = [pos, pos]

make_balance(TrQ, AddList, Cin, Cout, Relation, BV):-
    % create a balance value quantity:
    make_abstract_balance_quantity(TrQ, Cin, Cnew, I, BV),
    % get pointer lists for the positive and negative influences/props.
    add_to_balance(AddList, LeftList, RightList),
	append([I], LeftList, ILeftList),
    % d_q see intern.pl
	double_quantities(ILeftList, NLeftList, [], Cnew, Cnew1),
	double_quantities(RightList, NRightList, [], Cnew1, Cout),
    % create bitmaps for both sides of the balance.
	list_map(NLeftList, Left),
	list_map(NRightList, Right),
	zero_pointer(relation(Left, =, Right), Relation1),
	canonical_form(Relation1, Relation).


% make_abstract_balance_quantity(+Quantity, +Cin, -Cout, -I, -BV),
% adds a balancequantity in the temporary Cio for influence resolution

make_abstract_balance_quantity(TrQ, Cin, Cout, I, BV):-
    % construct a pointer for the quantiy:
    flag(q_cnt, P, P),
    I is P + 1,
    flag(q_cnt, _, I),
    cio_q_nq(Cin, Cout, Q, NQ),
    list_map([I], BV),
    % construct a name for the quantity:
    TrQ =.. [_, QName],
    atom_concat(i_r_balance_, QName, BalanceName),
    % update the quantity list:
    NQ = [BalanceName/I|Q].


% add_to_balance2(+Addlist, -Leftlist,  -Rightlist)

add_to_balance([], [], []).

add_to_balance([up(Q)|Tail], Left, [I|Right]):-
    map_list(Q, [I]),
    add_to_balance(Tail, Left, Right).

add_to_balance([down(Q)|Tail], [I|Left], Right):-
    map_list(Q, [I]),
    add_to_balance(Tail, Left, Right).

add_to_balance([noforce(_Q)|Tail], Left, Right):-
    add_to_balance(Tail, Left, Right).



% get_balance_sign(+Influence, +Derivable, -NewInfluence)
% returns: noforce if influencing quantity is zero, otherwise, leaves influence direction intact,
% but changes from pos/neg to up/down to indicate, that influence has a known relation to zero.

get_balance_sign(up(Quantity), _, up(Quantity)):- !.

get_balance_sign(down(Quantity), _, down(Quantity)):- !.

get_balance_sign(noforce(Quantity), _, noforce(Quantity)):- !.

get_balance_sign(pos(Quantity), Relations, Result):-
    get_quantity_sign(Quantity, Relations, Sign),
    !,
    (Sign = zero ->
    Result = noforce(Quantity);
    Result = up(Quantity)
    ).

get_balance_sign(neg(Quantity), Relations, Result):-
    get_quantity_sign(Quantity, Relations, Sign),
    !,
    (Sign = zero ->
    Result = noforce(Quantity);
    Result = down(Quantity)
    ).

get_sign(pos(Quantity), Relations, Sign):-
	get_quantity_sign(Quantity, Relations, Sign).

get_sign(neg(Quantity), Relations, Sign):-
	get_quantity_sign(Quantity, Relations, QSign),
	inverse_sign(QSign, Sign).

inverse_sign(neg, pos).
inverse_sign(pos, neg).
inverse_sign(zero, zero).
inverse_sign(neg_zero, pos_zero). %signs for second order derivatives
inverse_sign(pos_zero, neg_zero). %signs for second order derivatives

get_quantity_sign(Quantity, Relations, Sign):-
	list_map([0], Zero), % FL [] is old, now bitvector(1) [0] indicates zero
	get_quantity_sign_core(Quantity, Zero, Relations, Sign).

% FL may 07: with zero pointer 0, there is never a x = 0 relation:
% so a special clause for the zero pointer is needed.
get_quantity_sign_core(Quantity, Quantity, _Relations, zero):-!.

get_quantity_sign_core(Quantity, M, Relations, Sign):-
	member(relation(Quantity, Relation, M), Relations),
	zero_relation_sign(Relation, Sign),
	!.

get_quantity_sign_core(Quantity, M, Relations, Sign):-
	member(relation(M, Relation, Quantity), Relations),
	inverse(Relation, Inverse),
	zero_relation_sign(Inverse, Sign),
	!.

% zero_relation_sign(R, Sign):
% if a parameter has relation R to 0, return Sign

zero_relation_sign(>, pos).
% zero_relation_sign(>=, pos_zero).
zero_relation_sign(=, zero).
zero_relation_sign(<, neg).
% zero_relation_sign(=<, neg_zero).

% get_balance_result(+Quantity, +Relations, -Sign):-
% New for garp 2.0
% Like get quantity sign, but always succeeds:
% returns a sign, e.g. pos, or a list of possibilities in case of ambiguity
% e.g. [pos,zero] or [pos, zero, neg]

get_balance_result(Quantity, Relations, Sign):-
	list_map([0], M),
	member(relation(Quantity, Relation, M), Relations),
	zero_relation_signs(Relation, Sign),
	!.

get_balance_result(Quantity, Relations, Sign):-
	list_map([0], M),
	member(relation(M, Relation, Quantity), Relations),
	inverse(Relation, Inverse),
	zero_relation_signs(Inverse, Sign),
	!.

get_balance_result(_, _, [neg, zero, pos]).


zero_relation_signs(>, pos).
zero_relation_signs(>=, [zero, pos]).
zero_relation_signs(=, zero).
zero_relation_signs(<, neg).
zero_relation_signs(=<, [neg, zero]).


% replace quantities with their intern representation in a list of adders

adders_quantities([], [], Qio, Qio).

adders_quantities([ adder(Q, L) | T ], [ adder(IQ, IL) | NT ], Qin, Qout):-
	intern_quantities([ pos(Q) | L ], [ pos(IQ) | IL ], Qin, Qnew),
	adders_quantities(T, NT, Qnew, Qout).

intern_quantities([], [], Qio, Qio).

intern_quantities([Adder|T], [InternAdder|NT], Qin, Qout):-
	Adder =.. [AddType, Quantity],
	intern_quantity(Quantity, InternQuantity, Qin, Qnew),
	InternAdder =.. [AddType, InternQuantity],
	intern_quantities(T, NT, Qnew, Qout).

intern_quantity(Quantity, Intern, QinOut, QinOut):-
	memberchk(Quantity/Item, QinOut), !,
	list_map([Item], Intern).

intern_quantity(Quantity, Intern, Qin, [Quantity/I|Qin]):-
	flag(q_cnt, P, 0),		% increase q_cnt
	I is P + 1,
	flag(q_cnt, _, I),
	list_map([I], Intern).


/* FL april 2012 Old version split pos, neg and pos_neg_mult. seems unnecessary...
intern_quantities([pos(Quantity)|T], [pos(Intern)|NT], Qin, Qout):-
	memberchk(Quantity/Item, Qin), !,
	list_map([Item], Intern),
	intern_quantities(T, NT, Qin, Qout).

intern_quantities([pos(Quantity)|T], [pos(Intern)|NT], Qin, Qout):-
	flag(q_cnt, P, 0),
	I is P + 1,
	flag(q_cnt, _, I),
	list_map([I], Intern),
	intern_quantities(T, NT, [Quantity/I|Qin], Qout),
	!.
intern_quantities([neg(Quantity)|T], [neg(Intern)|NT], Qin, Qout):-
	member(Quantity/Item, Qin), !,
	list_map([Item], Intern),
	intern_quantities(T, NT, Qin, Qout).

intern_quantities([neg(Quantity)|T], [neg(Intern)|NT], Qin, Qout):-
	flag(q_cnt, P, 0),
	I is P + 1,
	flag(q_cnt, _, I),
	list_map([I], Intern),
	intern_quantities(T, NT, [Quantity/I|Qin], Qout),
	!.

% Special clauses for multiplication Proportionality P*: pos_neg_mult
%
% should we add second and third order derivative functionality here?
% yesss, but better in generic version as above... (fl april 2012)
%
intern_quantities([pos_neg_mult(Quantity)|T], [pos_neg_mult(DIntern)|NT], Qin, Qout):-
 %intern_quantities([pos_neg_mult(derivative(Quantity))|T], [pos_neg_mult(DIntern)|NT], Qin, Qout):-
	Quantity = derivative(X), X = _,
	%V = value(Quantity),
	intern_quantity_mult(Quantity, Qin, DIntern, Qnew1),
	%intern_quantity_mult(V, Qnew1, VIntern, Qnew2),
	intern_quantities(T, NT, Qnew1, Qout).

intern_quantity_mult(Quantity, Qin, Intern, Qin):-
	memberchk(Quantity/Item, Qin), !,
	list_map([Item], Intern).

intern_quantity_mult(Quantity, Qin, Intern, [Quantity/I|Qin]):-
	flag(q_cnt, P, 0),		/* increase q_cnt */
	I is P + 1,
	flag(q_cnt, _, I),
	list_map([I], Intern),
	!.
*/


%	--------------------------------------------------  %%
%
%	Second Order Derivative Influence Resolution
%
%	--------------------------------------------------  %%


% resolve_adders(+Adders, +Cin, -Cout)
% - change Adders from extern to intern representation
% - apply closed world on all quantities (under switch control)
% this means setting derivatives of unaffected quantities to zero.
% - resolve the resulting adders within Cio context.

resolve_2ndorder_adders(_Adders, _AdderStrings, Cin, Cin, [], _):-
	flag(second_order_derivatives, F, F),
	algorithm_assumption_flag(F, fail, second_order_derivatives),
	!.


resolve_2ndorder_adders(Adders, AdderStrings1, Cin, Cout, HODout, Constraints):-
	flag(second_order_derivatives, F, F),
	algorithm_assumption_flag(F, true, second_order_derivatives),
	etrace(solve_resolve_2ndorder_start, _, resolve),
	translate_adders_to_2ndorder(Adders, Adders2Tracer1),
	filter_proportionality_propagation(Adders2Tracer1, AdderStrings1, Adders2Tracer, AdderStrings), %FL: NEW okt-2011
	cio_q_nq(Cin, Cnew1, Q, NQ),
	adders_quantities(Adders2Tracer, Adders2Intern, Q, NQ), % convert adderlist to intern representation
	%Cin = Cout,
	!,
	flag(cw_assumption_second_order, Flag, Flag),
	HODin = [],
	(
	    algorithm_assumption_flag(Flag, fail, cw_assumption_second_order)
	->
	    Cnew2 = Cnew1,
	    HODin = HOD
	;
        % set unknown, uninfluenced, influencing quantities to zero
	    etrace(solve_cw_second_order_start, _, resolve),
	    apply_closed_world_to_adders(NQ, Adders2Intern, Cnew1, Cnew2, HODin, HOD)
	),
	resolve_2ndorder_adders(Adders2Intern, Adders2Tracer, AdderStrings, Cnew2, Cout, true, HOD, HODout, Constraints),
	etrace(solve_resolve_2ndorder_done, _, resolve),
	!.



% no progress, discard adders
% BEFORE and derive unknown for these quantities.
% but no added info in there.
resolve_2ndorder_adders(_UnknownAdders, _UnknownTrAdders, _AdderStrings, Cio, Cio, false, Results, Results, _).
	% derive_unknown_2ndorder(UnknownAdders, UnknownTrAdders, ResultsIn, ResultsOut).


% first time, or progress
resolve_2ndorder_adders(Adders, TrAdders, AddStrings, Cin, Cout, true, ResultsIn, ResultsOut, Constraints):-
	resolve_2ndorder(Adders, TrAdders, AddStrings, Cin, Cnew, [], Unresolved, [], UnTrAd,
			 [], UnAddStr, Progress, ResultsIn, Results1, Constraints),
	resolve_2ndorder_adders(Unresolved, UnTrAd, UnAddStr, Cnew, Cout, Progress, Results1, ResultsOut, Constraints).




translate_adders_to_2ndorder([], []).

translate_adders_to_2ndorder([adder(derivative(Q), AddList)|Tail],
			     [adder(second_derivative(Q), AddList2)|TracerTail]):-
	translate_addlist_to_2ndorder(AddList, AddList2),!,
	translate_adders_to_2ndorder(Tail, TracerTail).

translate_addlist_to_2ndorder([], []).
translate_addlist_to_2ndorder([pos(derivative(Q))|Tail], [pos(second_derivative(Q))|NT]):-
	translate_addlist_to_2ndorder(Tail, NT).
translate_addlist_to_2ndorder([neg(derivative(Q))|Tail], [neg(second_derivative(Q))|NT]):-
	translate_addlist_to_2ndorder(Tail, NT).
translate_addlist_to_2ndorder([pos(value(Q))|Tail], [pos(derivative(Q))|NT]):-
	translate_addlist_to_2ndorder(Tail, NT).
translate_addlist_to_2ndorder([neg(value(Q))|Tail], [neg(derivative(Q))|NT]):-
	translate_addlist_to_2ndorder(Tail, NT).

%new P* relation
translate_addlist_to_2ndorder([pos_neg_mult(derivative(Q))|Tail], [pos_neg_mult(second_derivative(Q))|NT]):-
	translate_addlist_to_2ndorder(Tail, NT).

%new P/ relation
translate_addlist_to_2ndorder([pos_neg_diw(derivative(Q))|Tail], [pos_neg_diw(second_derivative(Q))|NT]):-
	translate_addlist_to_2ndorder(Tail, NT).



% if switch for making the Sign-Equality Assumption is not on (Kuipers
% book p255.) then we do not want to propagate second (or higher in
% general) order derivatives over P-relations.
%
filter_proportionality_propagation(Adders, AdderStrings, Adders, AdderStrings):-
	flag(second_order_proportionality_propagation, F, F),
	algorithm_assumption_flag(F, true, second_order_proportionality_propagation),
	etrace(second_order_proportionality_propagation, _, resolve),
	!.


filter_proportionality_propagation(AddersIn, AdderStringsIn, AddersOut, AdderStringsOut):-
	flag(second_order_proportionality_propagation, F, F),
	algorithm_assumption_flag(F, fail, second_order_proportionality_propagation),
	etrace(no_second_order_proportionality_propagation, _, resolve),
	filter_prop_prop(AddersIn, AdderStringsIn, AddersOut, AdderStringsOut).


filter_prop_prop([], [], [], []).

% adder has all 'derivative' elements in addlist -> this indicates
% I-relations so we keep it. (nb at second order level, so
% second_derivative elements indicate P relations)
% The list of Adderstrings has corresponding elements in the same places
% of the list, so we process this list
%
filter_prop_prop([adder(X, AddList)|ATail], [AdderString|ASTail], [adder(X, AddList)|ATailNew], [AdderString|ASTailNew]):-
	all_derivative_items(AddList),
	!,
	filter_prop_prop(ATail, ASTail, ATailNew, ASTailNew).

% other adders: remove
filter_prop_prop([_|ATail], [_|ASTail], ATailNew, ASTailNew):-
	filter_prop_prop(ATail, ASTail, ATailNew, ASTailNew).


all_derivative_items([]).
all_derivative_items([H|T]):-
	H =.. [_PosNeg, derivative(_Q)],
	all_derivative_items(T).


% resolve_2ndorder(+Adders, +TracerAdders, +Cin, -Cout, +[],
%         -Unresolved, +[], -UnresolvedTracerAdders, -Progress)
% Very much like resolve/9, see comments there...
% but on ambiguous result: discard, continue with next.

% all done, return results:
resolve_2ndorder([], [], [], Cin, Cin, Unresolved, Unresolved, UTracer, UTracer, UStrings, UStrings, false, Results, Results, _).


% derivative unknown -> put on Unresolved list
%(FL: in this case determining SOD can cause trouble.
% Continutity for unknown derivative is hard to calculate...
% This may cause other SOD's also not to be computed, but the model is
% incomplete in this case anyway.)
resolve_2ndorder([adder(Q, AddList)|Tail], [adder(TrQ, TrList)|TrTail], [adder(StrQ, TrString)|StrTail], Cin, Cout,
        Unresolved, UnresolvedOut, UTracer, UTracerOut, UStr, UStrOut, Progress, ResultsIn, ResultsOut, Constraints) :-
    cio_q_d(Cin, QList, Derivable),
    % unknown derivative:
    \+ get_derivative_sign_sod(TrQ, QList, Derivable, _),
    !,
    etrace(solve_2ndorder_ir_no_der, [TrQ, TrList, QList, Derivable], resolve),
    resolve_2ndorder(Tail, TrTail, StrTail, Cin , Cout, [adder(Q, AddList)|Unresolved], UnresolvedOut, [adder(TrQ, TrList)|UTracer],
		     UTracerOut, [adder(StrQ, TrString)|UStr], UStrOut, Progress, ResultsIn, ResultsOut, Constraints).


% adder incomplete -> put on Unresolved list

resolve_2ndorder([adder(Q, AddList)|Tail], [adder(TrQ, TrList)|TrTail], [adder(StrQ, TrString)|StrTail], Cin, Cout,
        Unresolved, UnresolvedOut, UTracer, UTracerOut, UStr, UStrOut, Progress, ResultsIn, ResultsOut, Constraints) :-
    cio_q_d(Cin, QList, Derivable),

    % check quantities in Addlist for known values,
    % transform pos, neg to up, down & noforce,
    % return incomplete or complete to indicate if all values are known
    complete_2ndorder_adder(AddList, Derivable, NewAddList, Incomplete, TrList, TracerAddList),
    Incomplete = incomplete,
    !, %is it ok to cut here? because below it says: note this may fail... choicepoint seems unnecessary... Fl Feb 07 tempflo
    etrace(solve_2ndorder_ir_set, [TrQ, TrList, QList, Derivable], resolve),
    etrace(solve_2ndorder_ir_unres, [TracerAddList], resolve),
    % note this may fail
    resolve_2ndorder(Tail, TrTail, StrTail, Cin , Cout, [adder(Q, NewAddList)|Unresolved], UnresolvedOut, [adder(TrQ, TrList)|UTracer],
		     UTracerOut, [adder(StrQ, TrString)|UStr], UStrOut, Progress, ResultsIn, ResultsOut, Constraints).

% adder complete, single influence -> evaluate & add results

resolve_2ndorder([adder(Q, [Influence])|Tail], [adder(TrQ, [TrInf])|TrTail], [adder(_StrQ, TrString)|StrTail], Cin, Cout,
        UnresolvedIn, UnresolvedOut, UTracer, UTracerOut, UStrIn, UStrOut, Progress, ResultsIn, ResultsOut, Constraints) :-
    cio_q_d(Cin, QList, Derivable),
    get_2ndorder_sign(Influence, Derivable, Sign),
    !,
    etrace(solve_2ndorder_ir_set, [TrQ, [TrInf], QList, Derivable], resolve),
    etrace(solve_2ndorder_ir_single_res, [TrInf, Sign], resolve),
    etrace(solve_2ndorder_ir_res, [TrQ, Sign], resolve),
    add_2ndorder_resolution_result(Q, TrQ, Sign, Cin, Cnew, Progress, TailProgress,
	                  [UnresolvedIn, Tail], [Unresolved1, NewTail]), % ANA
    test_assumed_sod_constraints(Constraints, TrQ, Sign),
    !,
    % April 08: for tracing/visualize also single influence sod's are recorded
    % get the derivative sign for our SOD List
    get_derivative_sign_sod(TrQ, QList, Derivable, Der),
    TrQ = second_derivative(X),
    resolve_2ndorder(NewTail, TrTail, StrTail, Cnew , Cout, Unresolved1, UnresolvedOut,
            UTracer, UTracerOut, UStrIn, UStrOut, TailProgress, [hod_info(X, Der, Sign, unknown, [TrInf], nil, TrString)|ResultsIn], ResultsOut, Constraints).



% adder complete -> first try sign addition. if that does not work:
% next clause: construct balance, evaluate & add results
%
% hod_info(Par, Der, SOD, TOD, TrListSOD, TrListTOD, TrString)

resolve_2ndorder([adder(Q, AddList)|Tail], [adder(TrQ, TrList)|TrTail], [adder(_StrQ, TrString)|StrTail], Cin, Cout,
        UnresolvedIn, UnresolvedOut, UTracer, UTracerOut, UStrIn, UStrOut, Progress, ResultsIn, ResultsOut, Constraints) :-
    cio_q_d(Cin, QList, Derivable),
    try_sign_addition(AddList, Derivable, zero, Sign, TrList, TrEffects),% NB fails if ambiguous
    !,
    etrace(solve_2ndorder_ir_set, [TrQ, TrList, QList, Derivable], resolve),
    etrace(solve_2ndorder_ir_add, [TrEffects], resolve),
    etrace(solve_2ndorder_ir_res, [TrQ, Sign], resolve),
    TrQ = second_derivative(X),
    add_2ndorder_resolution_result(Q, TrQ, Sign, Cin, Cnew, Progress, TailProgress,
	                       [UnresolvedIn, Tail],
	                       [NewUnresolved, NewTail]),
    test_assumed_sod_constraints(Constraints, TrQ, Sign),
    !,
    % get the derivative sign for our SOD List
    get_derivative_sign_sod(TrQ, QList, Derivable, Der),
     % note this may fail
    resolve_2ndorder(NewTail, TrTail, StrTail, Cnew , Cout, NewUnresolved, UnresolvedOut,
	        UTracer, UTracerOut, UStrIn, UStrOut, TailProgress, [hod_info(X, Der, Sign, unknown, TrList, nil, TrString)|ResultsIn], ResultsOut, Constraints).


% adder complete -> construct balance, evaluate & add results

resolve_2ndorder([adder(Q, AddList)|Tail], [adder(TrQ, TrList)|TrTail], [adder(_StrQ, TrString)|StrTail], Cin, Cout,
        UnresolvedIn, UnresolvedOut, UTracer, UTracerOut, UStrIn, UStrOut, Progress, ResultsIn, ResultsOut, Constraints) :-
    cio_q_d(Cin, QList, Derivable),
    % check quantities in Addlist for known values, transform pos, neg to up, down & noforce
    complete_2ndorder_adder(AddList, Derivable, CompleteAddList, Incomplete, _, _),
    Incomplete = complete,
    etrace(solve_2ndorder_ir_set, [TrQ, TrList, QList, Derivable], resolve),
    etrace(solve_2ndorder_ir_bal, [TrList, Pos, Neg], resolve),
    % make_balance [BalanceValue, min1, min2, ...] = [pos1, pos2, ...] (intern representation)
    make_balance(TrQ, CompleteAddList, Cin, Ctest1, Balance, BV),
    % add and evaluate balance relation in testcio (balance not added to relations)
    % try_balance gives feedback if append_relation fails
    try_balance(append_relation(Balance, Ctest1, Ctest2, _, false)), % Analyse simple & zero equality should be false    not to create an invalid Q pointers to testcio indexes
    % get value of balancequantity
    cio_d(Ctest2, Derivable2),
    %test quantity sign (also >= and =<)
    get_balance_result(BV, Derivable2, Sign),%because no analyse zero & simple equality, BV is still a valid pointer
    !, %no backtracking on this part
    TrQ = second_derivative(X),
    (
    memberchk(Sign, [zero, pos, neg]) % definitive result
    ->
        (
	etrace(solve_2ndorder_ir_bal_res, [TrQ, Pos, Neg, Sign], resolve),
        add_2ndorder_resolution_result(Q, TrQ, Sign, Cin, Cnew, Progress, TailProgress,
	                       [UnresolvedIn, Tail],
	                       [NewUnresolved, NewTail]),
	test_assumed_sod_constraints(Constraints, TrQ, Sign),
	Result = Sign,
	!)
    ;   % else: ambiguous result, Sign is a list of possibilities in this case
        % discard any results, continue.
	(
	  %flag(assume_steady_derivatives, Flag, Flag), %NB FL may 07 hardcoded off now, switch needed
	  %Flag = fail,
	  (
	    %algorithm_assumption_flag(Flag, fail, assume_steady_derivatives)
	    true
	  ->
	    ambiguous_sign(Sign, Result),
	    etrace(solve_2ndorder_ir_bal_res, [TrQ, Pos, Neg, Result], resolve),
	    add_2ndorder_resolution_result(Q, TrQ, Result, Cin, Cnew, Progress, TailProgress,
					   [UnresolvedIn, Tail],
					   [NewUnresolved, NewTail])
	   %test_assumed_sod_constraints(Constraints, TrQ, Sign)% not needed here, ambiguous is always compatible with assumed termination...
	  ;
	    % in case agressive CW assumption is on, assume a zero effect in case of ambiguity
	    Result = zero,
	    etrace(solve_2ndorder_ir_bal_res_assume_zero, [TrQ, Pos, Neg, Result], resolve),
	    add_2ndorder_resolution_result(Q, TrQ, Result, Cin, Cnew, Progress, TailProgress,
					   [UnresolvedIn, Tail],
					   [NewUnresolved, NewTail])
	  )

	)
    ),
    get_derivative_sign_sod(TrQ, QList, Derivable, Der), %should always be known, because normal IR was already done on this par
    % note this may fail
    resolve_2ndorder(NewTail, TrTail, StrTail, Cnew , Cout, NewUnresolved, UnresolvedOut, UTracer, UTracerOut,
		     UStrIn, UStrOut, TailProgress, [hod_info(X, Der, Result, unknown, TrList, nil, TrString)|ResultsIn], ResultsOut, Constraints).


% get derivative for in the sod-info (second order derivative)
get_derivative_sign_sod(second_derivative(Qty), QList, Derivable, Der):-
    memberchk(derivative(Qty)/I, QList),
    list_map([I], DerQty),
    get_quantity_sign(DerQty, Derivable, Der),
    !. %should always be known, because normal IR was already done on this par


% On an assumed transition the 2nd order derivative cannot indicate
% the reverse transition, in this case the assumption was wrong and
% the transition should fail
test_assumed_sod_constraints(Constraints, second_derivative(Q), Sign):-
	member(Sign/Q, Constraints),
	TrQ = second_derivative(Q),
	etrace(solve_2ndorder_wrong_assumed_derivative_termination, [TrQ, Sign], [general, resolve]),
	!,
	fail.

test_assumed_sod_constraints(_Constraints, _TrQ, _Sign).



% complete_adder: takes an addList and evaluates to zero
% if sign is known then change notation to up/down/noforce
% otherwise return incomplete and semi transformed list.
% unknown quantities are passed on for tracer

complete_2ndorder_adder([], _, [], complete, [], []):-!. %not yet incomplete -> complete

complete_2ndorder_adder([], _, [], incomplete, [], []):-!. %already incomplete

complete_2ndorder_adder([H|T], Derivable, [Result | NewAddList], Incomplete, [_|TT], TraceList):-
    get_2ndorder_balance_sign(H, Derivable, Result),  %returns: up(Q), down(Q), noforce(Q) or fails
    !,
    complete_2ndorder_adder(T, Derivable, NewAddList, Incomplete, TT, TraceList).

% could not get quantity sign -> set incomplete indicator & continue with rest of list.
complete_2ndorder_adder([H|T], Derivable, [H | NewAddList], incomplete, [HT|TT], [HT|TraceTail]):-
    !,
    complete_2ndorder_adder(T, Derivable, NewAddList, incomplete, TT, TraceTail).

get_2ndorder_balance_sign(up(Quantity), _, up(Quantity)):- !.

get_2ndorder_balance_sign(down(Quantity), _, down(Quantity)):- !.

get_2ndorder_balance_sign(noforce(Quantity), _, noforce(Quantity)):- !.

get_2ndorder_balance_sign(pos(Quantity), Relations, Result):-
    get_2ndorder_quantity_sign(Quantity, Relations, Sign),
    !,
    (Sign = zero ->
    Result = noforce(Quantity);
    Result = up(Quantity)
    ).

get_2ndorder_balance_sign(neg(Quantity), Relations, Result):-
    get_2ndorder_quantity_sign(Quantity, Relations, Sign),
    !,
    (Sign = zero ->
    Result = noforce(Quantity);
    Result = down(Quantity)
    ).

get_2ndorder_quantity_sign(Quantity, _Relations, zero):-
	list_map([0], Quantity),
	!.

get_2ndorder_quantity_sign(Quantity, Relations, Sign):-
	list_map([0], M),
	member(relation(Quantity, Relation, M), Relations),
	zero_relation_sign_2ndorder(Relation, Sign),
	!.

get_2ndorder_quantity_sign(Quantity, Relations, Sign):-
	list_map([0], M),
	member(relation(M, Relation, Quantity), Relations),
	inverse(Relation, Inverse),
	zero_relation_sign_2ndorder(Inverse, Sign),
	!.

zero_relation_sign_2ndorder(>, pos).
zero_relation_sign_2ndorder(>=, pos_zero).
zero_relation_sign_2ndorder(=, zero).
zero_relation_sign_2ndorder(<, neg).
zero_relation_sign_2ndorder(=<, neg_zero).
zero_relation_sign_2ndorder(=?=, unknown).

ambiguous_sign([neg, zero, pos], unknown).
ambiguous_sign([zero, pos], pos_zero).
ambiguous_sign([neg, zero], neg_zero).

get_2ndorder_sign(pos(Quantity), Relations, Sign):-
	get_2ndorder_quantity_sign(Quantity, Relations, Sign).

get_2ndorder_sign(neg(Quantity), Relations, Sign):-
	get_2ndorder_quantity_sign(Quantity, Relations, QSign),
	inverse_2ndorder_sign(QSign, Sign).

inverse_2ndorder_sign(neg, pos).
inverse_2ndorder_sign(pos, neg).
inverse_2ndorder_sign(zero, zero).
inverse_2ndorder_sign(pos_zero, neg_zero).
inverse_2ndorder_sign(neg_zero, pos_zero).

add_2ndorder_resolution_result(_Q, _TrQ, unknown, Cio, Cio, Progress, Progress,
	                        [Unresolved, Tail],
	                        [Unresolved, Tail]):- !.


add_2ndorder_resolution_result(Q, _TrQ, pos_zero, Cin, Cout, Progress, TailProgress,
	                        [UnresolvedIn, Tail],
	                        [NewUnresolved, NewTail]):-
	!,
	list_map([0], Empty),
	right_order(relation(Q, >=, Empty), Relation),
	append_split(UnresolvedIn, Tail, I, OtherAddersIn),
	% makes one list, for easy passing on with append relatoin. pointers can be updated, but list stays intact.
	try_append(append_relation(Relation, Cin, Cout, Added, true, OtherAddersIn, OtherAddersOut)),
	split_append(NewUnresolved, NewTail, I, OtherAddersOut), % splits the list again at the same point.
	(Added = [],
	  Progress=TailProgress;
	  Progress=true),
	!.

add_2ndorder_resolution_result(Q, _TrQ, neg_zero, Cin, Cout, Progress, TailProgress,
	                        [UnresolvedIn, Tail],
	                        [NewUnresolved, NewTail]):-
	!,
	list_map([0], Empty),
	right_order(relation(Q, =<, Empty), Relation),
	append_split(UnresolvedIn, Tail, I, OtherAddersIn),
	% makes one list, for easy passing on with append relatoin. pointers can be updated, but list stays intact.
	try_append(append_relation(Relation, Cin, Cout, Added, true, OtherAddersIn, OtherAddersOut)),
	split_append(NewUnresolved, NewTail, I, OtherAddersOut), % splits the list again at the same point.
	(Added = [],
	  Progress=TailProgress;
	  Progress=true),
	!.

add_2ndorder_resolution_result(Q, _TrQ, pos, Cin, Cout, Progress, TailProgress,
	                        [UnresolvedIn, Tail],
	                        [NewUnresolved, NewTail]):-
	!,
	list_map([0], Empty),
	right_order(relation(Q, >, Empty), Relation),
	append_split(UnresolvedIn, Tail, I, OtherAddersIn),
	% makes one list, for easy passing on with append relatoin. pointers can be updated, but list stays intact.
	try_append(append_relation(Relation, Cin, Cout, Added, true, OtherAddersIn, OtherAddersOut)),
	split_append(NewUnresolved, NewTail, I, OtherAddersOut), % splits the list again at the same point.
	(Added = [],
	  Progress=TailProgress;
	  Progress=true),
	!.

add_2ndorder_resolution_result(Q, _TrQ, neg, Cin, Cout, Progress, TailProgress,
	                        [UnresolvedIn, Tail],
	                        [NewUnresolved, NewTail]):-
	!,
	list_map([0], Empty),
	right_order(relation(Q, <, Empty), Relation),
	append_split(UnresolvedIn, Tail, I, OtherAddersIn),
	% makes one list, for easy passing on with append relatoin. pointers can be updated, but list stays intact.
	try_append(append_relation(Relation, Cin, Cout, Added, true, OtherAddersIn, OtherAddersOut)),
	split_append(NewUnresolved, NewTail, I, OtherAddersOut), % splits the list again at the same point.
	(Added = [],
	  Progress=TailProgress;
	  Progress=true),
	!.

add_2ndorder_resolution_result(Q, _TrQ, zero, Cin, Cout, Progress, TailProgress,
	                        [UnresolvedIn, Tail],
	                        [NewUnresolved, NewTail]):-
	!,
	list_map([0], Empty),
	right_order(relation(Q, =, Empty), Relation),
	append_split(UnresolvedIn, Tail, I, OtherAddersIn),
	% makes one list, for easy passing on with append relatoin. pointers can be updated, but list stays intact.
	try_append(append_relation(Relation, Cin, Cout, Added, true, OtherAddersIn, OtherAddersOut)),
	split_append(NewUnresolved, NewTail, I, OtherAddersOut), % splits the list again at the same point.
	(Added = [],
	  Progress=TailProgress;
	  Progress=true),
	!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%	Third Order Derivative Influence Resolution
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


resolve_3rdorder_adders(_Adders, _AdderStrings, Cin, Cin, HodInfo, HodInfo, _):-
	flag(third_order_derivatives, F, F),
	algorithm_assumption_flag(F, fail, third_order_derivatives),
	!.


resolve_3rdorder_adders(Adders, AdderStrings1, Cin, Cout, HodIn, HodOut, OldValues):-
	flag(third_order_derivatives, F, F),
	algorithm_assumption_flag(F, true, third_order_derivatives),
	etrace(solve_resolve_3rdorder_start, _, resolve),
	translate_adders_to_3rdorder(Adders, Adders2Tracer1),
	filter_proportionality_propagation3rd(Adders2Tracer1, AdderStrings1, Adders2Tracer, AdderStrings), %FL: NEW okt-2011
	cio_q_nq(Cin, Cnew1, Q, NQ),
	adders_quantities(Adders2Tracer, Adders2Intern, Q, NQ), % convert adderlist to intern representation
	!,
	flag(cw_assumption_third_order, Flag, Flag),
	(
	    algorithm_assumption_flag(Flag, fail, cw_assumption_third_order)
	->
	    Cnew2 = Cnew1
	;
        % set unknown, uninfluenced, influencing quantities to zero
	    etrace(solve_cw_third_order_start, _, resolve),
	    apply_closed_world_to_adders(NQ, Adders2Intern, Cnew1, Cnew2, HodIn, Hod)
	),
	resolve_3rdorder_adders(Adders2Intern, Adders2Tracer, AdderStrings, Cnew2, Cout, true, Hod, HodOut, OldValues),
	etrace(solve_resolve_3rdorder_done, _, resolve),
	!.



% no progress, discard adders
% BEFORE and derive unknown for these quantities.
% but no added info in there.
resolve_3rdorder_adders(_UnknownAdders, _UnknownTrAdders, _AdderStrings, Cio, Cio, false, Results, Results, _).
	% derive_unknown_3rdorder(UnknownAdders, UnknownTrAdders, ResultsIn, ResultsOut).


% first time, or progress
resolve_3rdorder_adders(Adders, TrAdders, AddStrings, Cin, Cout, true, ResultsIn, ResultsOut, OldValues):-
	resolve_3rdorder(Adders, TrAdders, AddStrings, Cin, Cnew, [], Unresolved, [], UnTrAd,
			 [], UnAddStr, Progress, ResultsIn, Results1, OldValues),
	resolve_3rdorder_adders(Unresolved, UnTrAd, UnAddStr, Cnew, Cout, Progress, Results1, ResultsOut, OldValues).




translate_adders_to_3rdorder([], []).

translate_adders_to_3rdorder([adder(derivative(Q), AddList)|Tail],
			     [adder(third_derivative(Q), AddList2)|TracerTail]):-
	translate_addlist_to_3rdorder(AddList, AddList2),!,
	translate_adders_to_3rdorder(Tail, TracerTail).

translate_addlist_to_3rdorder([], []).
translate_addlist_to_3rdorder([pos(derivative(Q))|Tail], [pos(third_derivative(Q))|NT]):-
	translate_addlist_to_3rdorder(Tail, NT).
translate_addlist_to_3rdorder([neg(derivative(Q))|Tail], [neg(third_derivative(Q))|NT]):-
	translate_addlist_to_3rdorder(Tail, NT).
translate_addlist_to_3rdorder([pos(value(Q))|Tail], [pos(second_derivative(Q))|NT]):-
	translate_addlist_to_3rdorder(Tail, NT).
translate_addlist_to_3rdorder([neg(value(Q))|Tail], [neg(second_derivative(Q))|NT]):-
	translate_addlist_to_3rdorder(Tail, NT).

%new P* relation
translate_addlist_to_3rdorder([pos_neg_mult(derivative(Q))|Tail], [pos_neg_mult(third_derivative(Q))|NT]):-
	translate_addlist_to_3rdorder(Tail, NT).

%new P/ relation
translate_addlist_to_3rdorder([pos_neg_diw(derivative(Q))|Tail], [pos_neg_diw(third_derivative(Q))|NT]):-
	translate_addlist_to_3rdorder(Tail, NT).



% if switch for making the Sign-Equality Assumption is not on (Kuipers
% book p255.) then we do not want to propagate third (or higher in
% general) order derivatives over P-relations.
%
filter_proportionality_propagation3rd(Adders, AdderStrings, Adders, AdderStrings):-
	flag(third_order_proportionality_propagation, F, F),
	algorithm_assumption_flag(F, true, third_order_proportionality_propagation),
	etrace(third_order_proportionality_propagation, _, resolve),
	!.


filter_proportionality_propagation3rd(AddersIn, AdderStringsIn, AddersOut, AdderStringsOut):-
	flag(third_order_proportionality_propagation, F, F),
	algorithm_assumption_flag(F, fail, third_order_proportionality_propagation),
	etrace(no_third_order_proportionality_propagation, _, resolve),
	filter_prop_prop3rd(AddersIn, AdderStringsIn, AddersOut, AdderStringsOut).


filter_prop_prop3rd([], [], [], []).

% adder has all 'second_derivative' elements in addlist -> this
% indicates I-relations at this 3rd order level, so we keep it. (nb at
% third order level, third_derivative elements indicate P relations)
% The list of Adderstrings has corresponding elements in the same places
% of the list, so we process this list
%
filter_prop_prop3rd([adder(X, AddList)|ATail], [AdderString|ASTail], [adder(X, AddList)|ATailNew], [AdderString|ASTailNew]):-
	all_second_derivative_items(AddList),
	!,
	filter_prop_prop3rd(ATail, ASTail, ATailNew, ASTailNew).

% other adders: remove
filter_prop_prop3rd([_|ATail], [_|ASTail], ATailNew, ASTailNew):-
	filter_prop_prop3rd(ATail, ASTail, ATailNew, ASTailNew).


all_second_derivative_items([]).
all_second_derivative_items([H|T]):-
	H =.. [_PosNeg, second_derivative(_Q)],
	all_second_derivative_items(T).


% resolve_3rdorder(+Adders, +TracerAdders, +Cin, -Cout, +[],
%         -Unresolved, +[], -UnresolvedTracerAdders, -Progress)
% Very much like resolve/9, see comments there...
% but on ambiguous result: discard, continue with next.

% all done, return results:
resolve_3rdorder([], [], [], Cin, Cin, Unresolved, Unresolved, UTracer, UTracer, UStrings, UStrings, false, Results, Results, _).


% derivative unknown -> put on Unresolved list
%(FL: in this case determining TOD can cause trouble.
% Continutity for unknown derivative is hard to calculate...
% This may cause other SOD's also not to be computed, but the model is
% incomplete in this case anyway.)
resolve_3rdorder([adder(Q, AddList)|Tail], [adder(TrQ, TrList)|TrTail], [adder(StrQ, TrString)|StrTail], Cin, Cout,
        Unresolved, UnresolvedOut, UTracer, UTracerOut, UStr, UStrOut, Progress, ResultsIn, ResultsOut, OldValues) :-
    cio_q_d(Cin, QList, Derivable),
    % unknown derivative:
    \+ get_derivative_sign_tod(TrQ, QList, Derivable, _),
    !,
    etrace(solve_3rdorder_ir_no_der, [TrQ, TrList, QList, Derivable], resolve),
    resolve_3rdorder(Tail, TrTail, StrTail, Cin , Cout, [adder(Q, AddList)|Unresolved], UnresolvedOut, [adder(TrQ, TrList)|UTracer],
		     UTracerOut, [adder(StrQ, TrString)|UStr], UStrOut, Progress, ResultsIn, ResultsOut, OldValues).


% adder incomplete -> put on Unresolved list

resolve_3rdorder([adder(Q, AddList)|Tail], [adder(TrQ, TrList)|TrTail], [adder(StrQ, TrString)|StrTail], Cin, Cout,
        Unresolved, UnresolvedOut, UTracer, UTracerOut, UStr, UStrOut, Progress, ResultsIn, ResultsOut, OldValues) :-
    cio_q_d(Cin, QList, Derivable),

    % check quantities in Addlist for known values,
    % transform pos, neg to up, down & noforce,
    % return incomplete or complete to indicate if all values are known
    complete_3rdorder_adder(AddList, Derivable, NewAddList, Incomplete, TrList, TracerAddList),
    Incomplete = incomplete,
    !, %is it ok to cut here? because below it says: note this may fail... choicepoint seems unnecessary... Fl Feb 07 tempflo
    etrace(solve_3rdorder_ir_set, [TrQ, TrList, QList, Derivable], resolve),
    etrace(solve_3rdorder_ir_unres, [TracerAddList], resolve),
    % note this may fail
    resolve_3rdorder(Tail, TrTail, StrTail, Cin , Cout, [adder(Q, NewAddList)|Unresolved], UnresolvedOut, [adder(TrQ, TrList)|UTracer],
		     UTracerOut, [adder(StrQ, TrString)|UStr], UStrOut, Progress, ResultsIn, ResultsOut, OldValues).

% adder complete, single influence -> evaluate & add results

resolve_3rdorder([adder(Q, [Influence])|Tail], [adder(TrQ, [TrInf])|TrTail], [adder(_StrQ, TrString)|StrTail], Cin, Cout,
        UnresolvedIn, UnresolvedOut, UTracer, UTracerOut, UStrIn, UStrOut, Progress, ResultsIn, ResultsOut, OldValues) :-
    cio_q_d(Cin, QList, Derivable),
    get_3rdorder_sign(Influence, Derivable, TODSign),
    !,
    etrace(solve_3rdorder_ir_set, [TrQ, [TrInf], QList, Derivable], resolve),
    etrace(solve_3rdorder_ir_single_res, [TrInf, TODSign], resolve),
    etrace(solve_ir_res, [TrQ, TODSign], resolve),
    add_3rdorder_resolution_result(Q, TrQ, TODSign, Cin, Cnew, Progress, TailProgress,
	                  [UnresolvedIn, Tail], [Unresolved1, NewTail]), % ANA
    !,
    % April 08: for tracing/visualize also single influence sod's are recorded
    % get the derivative sign for our SOD List
    % get_derivative_sign_tod(TrQ, QList, Derivable, Der),
    TrQ = third_derivative(X),
    update_hod_list_and_prefilter(ResultsIn, X, nil, TODSign, [TrInf], TrString, OldValues, ResultsNew),
    resolve_3rdorder(NewTail, TrTail, StrTail, Cnew , Cout, Unresolved1, UnresolvedOut,
            UTracer, UTracerOut, UStrIn, UStrOut, TailProgress, ResultsNew, ResultsOut, OldValues).



% adder complete -> first try sign addition. if that does not work:
% next clause: construct balance, evaluate & add results

resolve_3rdorder([adder(Q, AddList)|Tail], [adder(TrQ, TrList)|TrTail], [adder(_StrQ, TrString)|StrTail], Cin, Cout,
        UnresolvedIn, UnresolvedOut, UTracer, UTracerOut, UStrIn, UStrOut, Progress, ResultsIn, ResultsOut, OldValues) :-
    cio_q_d(Cin, QList, Derivable),
    try_sign_addition(AddList, Derivable, zero, TODSign, TrList, TrEffects),% NB fails if ambiguous
    !,
    etrace(solve_3rdorder_ir_set, [TrQ, TrList, QList, Derivable], resolve),
    etrace(solve_ir_add, [TrEffects], resolve),
    etrace(solve_ir_res, [TrQ, TODSign], resolve),
    TrQ = third_derivative(X),
    add_3rdorder_resolution_result(Q, TrQ, TODSign, Cin, Cnew, Progress, TailProgress,
	                       [UnresolvedIn, Tail],
	                       [NewUnresolved, NewTail]),
    !,
    % get the derivative sign for our SOD List
    get_derivative_sign_tod(TrQ, QList, Derivable, Der),
    update_hod_list_and_prefilter(ResultsIn, X, Der, TODSign, TrList, TrString, OldValues, ResultsNew),
     % note this may fail
    resolve_3rdorder(NewTail, TrTail, StrTail, Cnew , Cout, NewUnresolved, UnresolvedOut,
	        UTracer, UTracerOut, UStrIn, UStrOut, TailProgress, ResultsNew, ResultsOut, OldValues).


% adder complete -> construct balance, evaluate & add results

resolve_3rdorder([adder(Q, AddList)|Tail], [adder(TrQ, TrList)|TrTail], [adder(_StrQ, TrString)|StrTail], Cin, Cout,
        UnresolvedIn, UnresolvedOut, UTracer, UTracerOut, UStrIn, UStrOut, Progress, ResultsIn, ResultsOut, OldValues) :-
    cio_q_d(Cin, QList, Derivable),
    % check quantities in Addlist for known values, transform pos, neg to up, down & noforce
    complete_3rdorder_adder(AddList, Derivable, CompleteAddList, Incomplete, _, _),
    Incomplete = complete,
    etrace(solve_3rdorder_ir_set, [TrQ, TrList, QList, Derivable], resolve),
    etrace(solve_ir_bal, [TrList, Pos, Neg], resolve),
    % make_balance [BalanceValue, min1, min2, ...] = [pos1, pos2, ...] (intern representation)
    make_balance(TrQ, CompleteAddList, Cin, Ctest1, Balance, BV),
    % add and evaluate balance relation in testcio (balance not added to relations)
    % try_balance gives feedback if append_relation fails
    try_balance(append_relation(Balance, Ctest1, Ctest2, _, false)), % Analyse simple & zero equality should be false    not to create an invalid Q pointers to testcio indexes
    % get value of balancequantity
    cio_d(Ctest2, Derivable2),
    %test quantity sign (also >= and =<)
    get_balance_result(BV, Derivable2, Sign),%because no analyse zero & simple equality, BV is still a valid pointer
    !, %no backtracking on this part
    TrQ = third_derivative(X),
    (
    memberchk(Sign, [zero, pos, neg]) % definitive result
    ->
        (
	etrace(solve_2ndorder_ir_bal_res, [TrQ, Pos, Neg, Sign], resolve), %same as 3rd
        add_3rdorder_resolution_result(Q, TrQ, Sign, Cin, Cnew, Progress, TailProgress,
	                       [UnresolvedIn, Tail],
	                       [NewUnresolved, NewTail]),
	Result = Sign,
	!)
    ;   % else: ambiguous result, Sign is a list of possibilities in this case
        % discard any results, continue.
	(
	  %flag(assume_steady_derivatives, Flag, Flag), %NB FL may 07 hardcoded off now, switch needed
	  %Flag = fail,
	  (
	    %algorithm_assumption_flag(Flag, fail, assume_steady_derivatives)
	    true
	  ->
	    ambiguous_sign(Sign, Result),
	    etrace(solve_2ndorder_ir_bal_res, [TrQ, Pos, Neg, Result], resolve),
	    add_3rdorder_resolution_result(Q, TrQ, Result, Cin, Cnew, Progress, TailProgress,
					   [UnresolvedIn, Tail],
					   [NewUnresolved, NewTail])
	   %test_assumed_tod_constraints(Constraints, TrQ, Sign)% not needed here, ambiguous is always compatible with assumed termination...
	  ;
	    % in case agressive CW assumption is on, assume a zero effect in case of ambiguity
	    Result = zero,
	    etrace(solve_2ndorder_ir_bal_res_assume_zero, [TrQ, Pos, Neg, Result], resolve),
	    add_3rdorder_resolution_result(Q, TrQ, Result, Cin, Cnew, Progress, TailProgress,
					   [UnresolvedIn, Tail],
					   [NewUnresolved, NewTail])
	  )

	)
    ),
    get_derivative_sign_tod(TrQ, QList, Derivable, Der), %should always be known, because normal IR was already done on this par
    update_hod_list_and_prefilter(ResultsIn, X, Der, Result, TrList, TrString, OldValues, ResultsNew),
    % note this may fail
    resolve_3rdorder(NewTail, TrTail, StrTail, Cnew , Cout, NewUnresolved, UnresolvedOut, UTracer, UTracerOut,
		     UStrIn, UStrOut, TailProgress, ResultsNew, ResultsOut, OldValues).


% get derivative for in the tod-info (third order derivative)
get_derivative_sign_tod(third_derivative(Qty), QList, Derivable, Der):-
    memberchk(derivative(Qty)/I, QList),
    list_map([I], DerQty),
    get_quantity_sign(DerQty, Derivable, Der),
    !. %should always be known, because normal IR was already done on this par


% complete_adder: takes an addList and evaluates to zero
% if sign is known then change notation to up/down/noforce
% otherwise return incomplete and semi transformed list.
% unknown quantities are passed on for tracer

complete_3rdorder_adder([], _, [], complete, [], []):-!. %not yet incomplete -> complete

complete_3rdorder_adder([], _, [], incomplete, [], []):-!. %already incomplete

complete_3rdorder_adder([H|T], Derivable, [Result | NewAddList], Incomplete, [_|TT], TraceList):-
    get_3rdorder_balance_sign(H, Derivable, Result),  %returns: up(Q), down(Q), noforce(Q) or fails
    !,
    complete_3rdorder_adder(T, Derivable, NewAddList, Incomplete, TT, TraceList).

% could not get quantity sign -> set incomplete indicator & continue with rest of list.
complete_3rdorder_adder([H|T], Derivable, [H | NewAddList], incomplete, [HT|TT], [HT|TraceTail]):-
    !,
    complete_3rdorder_adder(T, Derivable, NewAddList, incomplete, TT, TraceTail).

get_3rdorder_balance_sign(up(Quantity), _, up(Quantity)):- !.

get_3rdorder_balance_sign(down(Quantity), _, down(Quantity)):- !.

get_3rdorder_balance_sign(noforce(Quantity), _, noforce(Quantity)):- !.

get_3rdorder_balance_sign(pos(Quantity), Relations, Result):-
    get_3rdorder_quantity_sign(Quantity, Relations, Sign),
    !,
    (Sign = zero ->
    Result = noforce(Quantity);
    Result = up(Quantity)
    ).

get_3rdorder_balance_sign(neg(Quantity), Relations, Result):-
    get_3rdorder_quantity_sign(Quantity, Relations, Sign),
    !,
    (Sign = zero ->
    Result = noforce(Quantity);
    Result = down(Quantity)
    ).

get_3rdorder_quantity_sign(Quantity, _Relations, zero):-
	list_map([0], Quantity),
	!.

get_3rdorder_quantity_sign(Quantity, Relations, Sign):-
	list_map([0], M),
	member(relation(Quantity, Relation, M), Relations),
	zero_relation_sign_3rdorder(Relation, Sign),
	!.

get_3rdorder_quantity_sign(Quantity, Relations, Sign):-
	list_map([0], M),
	member(relation(M, Relation, Quantity), Relations),
	inverse(Relation, Inverse),
	zero_relation_sign_3rdorder(Inverse, Sign),
	!.

zero_relation_sign_3rdorder(>, pos).
zero_relation_sign_3rdorder(>=, pos_zero).
zero_relation_sign_3rdorder(=, zero).
zero_relation_sign_3rdorder(<, neg).
zero_relation_sign_3rdorder(=<, neg_zero).
zero_relation_sign_3rdorder(=?=, unknown).

get_3rdorder_sign(pos(Quantity), Relations, Sign):-
	get_3rdorder_quantity_sign(Quantity, Relations, Sign).

get_3rdorder_sign(neg(Quantity), Relations, Sign):-
	get_3rdorder_quantity_sign(Quantity, Relations, QSign),
	inverse_3rdorder_sign(QSign, Sign).

inverse_3rdorder_sign(neg, pos).
inverse_3rdorder_sign(pos, neg).
inverse_3rdorder_sign(zero, zero).
inverse_3rdorder_sign(pos_zero, neg_zero).
inverse_3rdorder_sign(neg_zero, pos_zero).



add_3rdorder_resolution_result(_Q, _TrQ, unknown, Cio, Cio, Progress, Progress,
	                        [Unresolved, Tail],
	                        [Unresolved, Tail]):- !.


add_3rdorder_resolution_result(Q, _TrQ, pos_zero, Cin, Cout, Progress, TailProgress,
	                        [UnresolvedIn, Tail],
	                        [NewUnresolved, NewTail]):-
	!,
	list_map([0], Empty),
	right_order(relation(Q, >=, Empty), Relation),
	append_split(UnresolvedIn, Tail, I, OtherAddersIn),
	% makes one list, for easy passing on with append relatoin. pointers can be updated, but list stays intact.
	try_append(append_relation(Relation, Cin, Cout, Added, true, OtherAddersIn, OtherAddersOut)),
	split_append(NewUnresolved, NewTail, I, OtherAddersOut), % splits the list again at the same point.
	(Added = [],
	  Progress=TailProgress;
	  Progress=true),
	!.

add_3rdorder_resolution_result(Q, _TrQ, neg_zero, Cin, Cout, Progress, TailProgress,
	                        [UnresolvedIn, Tail],
	                        [NewUnresolved, NewTail]):-
	!,
	list_map([0], Empty),
	right_order(relation(Q, =<, Empty), Relation),
	append_split(UnresolvedIn, Tail, I, OtherAddersIn),
	% makes one list, for easy passing on with append relatoin. pointers can be updated, but list stays intact.
	try_append(append_relation(Relation, Cin, Cout, Added, true, OtherAddersIn, OtherAddersOut)),
	split_append(NewUnresolved, NewTail, I, OtherAddersOut), % splits the list again at the same point.
	(Added = [],
	  Progress=TailProgress;
	  Progress=true),
	!.

add_3rdorder_resolution_result(Q, _TrQ, pos, Cin, Cout, Progress, TailProgress,
	                        [UnresolvedIn, Tail],
	                        [NewUnresolved, NewTail]):-
	!,
	list_map([0], Empty),
	right_order(relation(Q, >, Empty), Relation),
	append_split(UnresolvedIn, Tail, I, OtherAddersIn),
	% makes one list, for easy passing on with append relatoin. pointers can be updated, but list stays intact.
	try_append(append_relation(Relation, Cin, Cout, Added, true, OtherAddersIn, OtherAddersOut)),
	split_append(NewUnresolved, NewTail, I, OtherAddersOut), % splits the list again at the same point.
	(Added = [],
	  Progress=TailProgress;
	  Progress=true),
	!.

add_3rdorder_resolution_result(Q, _TrQ, neg, Cin, Cout, Progress, TailProgress,
	                        [UnresolvedIn, Tail],
	                        [NewUnresolved, NewTail]):-
	!,
	list_map([0], Empty),
	right_order(relation(Q, <, Empty), Relation),
	append_split(UnresolvedIn, Tail, I, OtherAddersIn),
	% makes one list, for easy passing on with append relatoin. pointers can be updated, but list stays intact.
	try_append(append_relation(Relation, Cin, Cout, Added, true, OtherAddersIn, OtherAddersOut)),
	split_append(NewUnresolved, NewTail, I, OtherAddersOut), % splits the list again at the same point.
	(Added = [],
	  Progress=TailProgress;
	  Progress=true),
	!.

add_3rdorder_resolution_result(Q, _TrQ, zero, Cin, Cout, Progress, TailProgress,
	                        [UnresolvedIn, Tail],
	                        [NewUnresolved, NewTail]):-
	!,
	list_map([0], Empty),
	right_order(relation(Q, =, Empty), Relation),
	append_split(UnresolvedIn, Tail, I, OtherAddersIn),
	% makes one list, for easy passing on with append relatoin. pointers can be updated, but list stays intact.
	try_append(append_relation(Relation, Cin, Cout, Added, true, OtherAddersIn, OtherAddersOut)),
	split_append(NewUnresolved, NewTail, I, OtherAddersOut), % splits the list again at the same point.
	(Added = [],
	  Progress=TailProgress;
	  Progress=true),
	!.



% hod_info found, TODinfo added, no prefilter match
update_hod_list_and_prefilter(ResultsIn, X, _, TODX, TrList, Path, OldValues, [hod_info(X, DerX, SODX, TODX, SODInfluences, TrList, Path)|Rest]):-
	select(hod_info(X, DerX, SODX, _, SODInfluences, _, Path), ResultsIn, Rest),
	!,
	\+prefilter_tod(X, DerX, SODX, TODX, OldValues).
	%NB  no clause for prefilter match, this case just filters by failing

% hod_info not found, newly generated, TODinfo added, no prefilter match
% NB should this case occur?? Lets keep it for failsafe
% sodX unknown here!
update_hod_list_and_prefilter(ResultsIn, X, DerX, TODX, TrList, TrString, OldValues, [hod_info(X, DerX, SODX, TODX, nil, TrList, TrString)|ResultsIn]):-
	SODX = unknown,
	\+prefilter_tod(X, DerX, SODX, TODX, OldValues),
	!,
	etrace(no_second_order_derivative_for_calculated_third_derivative, [], general). %generates engine trace warning



/* prefilter always active if third derivatives are calculated (what's the point of calculating if not for filtering?)
prefilter_tod(_, _, _, _, _):-
	flag(prefilter_tod_active, _, true), %set to off/on
	flag(prefilter_tod_active, F, F),
	F = fail,
	!,
        fail.
*/

% see Kuipers p.243, fig 10.3
% SOD and TOD are not variables at this point right?-)
prefilter_tod(Par, zero, zero, pos, ValuesPreviousState):-
	memberchk(value(Par, _, _Val, min), ValuesPreviousState),
	!.

prefilter_tod(Par, zero, zero, neg, ValuesPreviousState):-
	memberchk(value(Par, _, _Val, plus), ValuesPreviousState),
	!.













