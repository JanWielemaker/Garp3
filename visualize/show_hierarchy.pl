/*****************************************************************************************************
Garp3 0.1 Based on main source of visigarp (main.pl). This file is split in parts and edited.
All visiualize source based on visigarp 2.05, designed and implemented by Anders Bouwer 18/08/1999
Original copyright: (C) 2004 University of Amsterdam. All rights reserved.

Now:
PART OF Garp3. SEE COPYRIGHT NOTICE.
*******************************************************************************************************/

%loaded into module (namespace) visualize


% display_hierarchy(P, Type)
% Show a window with the hierarchy described by the relation(s) 
% associated with Type

display_hierarchy(P, entity_isa) :-
	graph_edge(entity_isa, _A, _B, Rel),!,
	% nil is the root of the entity isa hierarchy
	display_hierarchy(P, entity_isa, nil, Rel).

display_hierarchy(P, mf_isa) :-
	%gp3 0.1: allthough this is (now) only used for
	%legacy viewing, we keep up the possibility to view
	%non-legay
	%so we switch the topnodes depending on de modelState   
	
	graph_edge(mf_isa, _A, _B, Rel),!,
	% this graph has multiple roots
	if
		get(@model, modelState, legacy) % JL 
	then
		Top = [	description_view, 
				composition_view, 
				decomposition_view,
				process,
				qualitative_state ]
	else
		Top = [static,process,agent], %non-legacy
	forall(member(Root, Top), 
		display_hierarchy(P, mf_isa, Root, Rel)).

display_hierarchy(P, mf_apply) :-
	graph_edge(mf_apply, _A, _B, Rel),!,
	% this graph has multiple roots
	% find roots:
	findall(Root, 
		(graph_node(mf_apply, Root),
		  % no system structure mentions it in conditions:
		  \+ (	engine:system_structures(_, _, conditions(Cond1), _), %gp3: added engine context
			member(system_structures(S1), Cond1), 
		  	memberchk(Root, S1))
		), 
		Roots), 
	sort(Roots, SortedRoots), 
	forall(member(RootName, SortedRoots), 
		display_hierarchy(P, mf_apply, RootName, Rel)).


display_hierarchy(P, Type, Root, Rel):-
	atomize(Root, RootStr), 
	display_hierarchy_designname(Type,RootStr,DesignName), %gp3
	new(T, tree(new(Node, node(text(DesignName))))),
	send(T, direction, list),
	send(T, level_gap, 40),
	send(T, link_gap, 2), 	   % doesn't show for nodes without images
	send(T, neighbour_gap, 1), % default is 0
	
	% get bounding box area of all graphicals in Picture P
	get(P, bounding_box, area(_X, Y, _W, H)),
	get(P, frame, F),

	text_margin(_LeftMargin, _TopMargin), 
	Y1 is Y + H, % + TopMargin, 

	send(P, display, T, point(0, Y1)), 
	send(T, root_handler, 
        	click_gesture(left, '', double, 
		    and(
			message(@prolog, select_hierarchy_node, F, Type,  
				@event?receiver?node?image?string)
		    )
		)
	),

	send(T, leaf_handler, 
        	click_gesture(left, '', double, 
		    and(
			message(@prolog, select_hierarchy_node, F, Type,  
				@event?receiver?node?image?string)
		    )
		)
	),
	send(T, collapsed_handler, 
        	click_gesture(left, '', double, 
		    and(
			% expand node - or only when Ctrl-selected?
			message(@event?receiver?node, collapsed, @off),
			message(@prolog, select_hierarchy_node, F, Type,  
				@event?receiver?node?image?string)
		    )
		)
	),
	send(T, node_handler, 
        	click_gesture(left, '', double, 
		    and(
			message(@event?receiver?node, collapsed, @off),
			message(@prolog, select_hierarchy_node, F, Type,  
				@event?receiver?node?image?string)
		    )
		)
	),
	expand_hierarchy(Type, Root, Rel, Node).
	


expand_hierarchy(Type, Root, Rel, Node) :-
	forall(graph_edge(Type, Sub, Root, Rel), 
	 (
	 	atomize(Sub, SubStr), 
	    display_hierarchy_designname(Type,SubStr,DesignName), %gp3   
		send(Node, son, new(Son, node(text(DesignName)))),
	        expand_hierarchy(Type, Sub, Rel, Son)
		)
	).

display_hierarchy_designname(entity_isa,InternalName,DesignName):-
%gp3 helper, get the designname for entities
	DesignName = @app<<-designName(en,InternalName).
%
display_hierarchy_designname(MFType,InternalName,DesignName):-
%gp3 helper, get the designname for mfs
	( MFType = mf_isa ; MFType = mf_apply ),
	DesignName = @app<<-designName(mf,InternalName).
%
display_hierarchy_designname(_Type,InternalName,InternalName). %fall back to internal name

% select_hierarchy_node(F, Type, Str)
%
% determines what happens when a node in one of the hierarchies 
% is selected. 
%
% for the entity_isa hierarchy, nothing happens
%
select_hierarchy_node(_F, entity_isa, _Str).

% for the mf_isa hierarchy, show contents of the 
% selected model fragment
%
select_hierarchy_node(F, mf_isa, Str):-
	% this is not a very nice solution, because 
	% actually no state nr should be required!
	show_model_fragment_details(F, text, Str, Str, -1). %gp added 2nd Str argument (label, todo!)

% for the mf_apply hierarchy, idem as mf_isa
%
select_hierarchy_node(F, mf_apply, Str):-
	select_hierarchy_node(F, mf_isa, Str).

