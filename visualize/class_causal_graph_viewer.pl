/*****************************************************************************************************
Garp3 0.1 Based on main source of visigarp (main.pl). This file is split in parts and edited.
All visiualize source based on visigarp 2.05, designed and implemented by Anders Bouwer 18/08/1999
Original copyright: (C) 2004 University of Amsterdam. All rights reserved.
Last updated: 08/05/2006, AB
Now:
PART OF Garp3. SEE COPYRIGHT NOTICE.
*******************************************************************************************************/

%loaded into module (namespace) visualize
  
/* 
 * causal_graph_viewer 
 * 
 * opens the causal_graph_viewer application. With this application 
 * you can view and consult prolog files, and generate and 
 * display a graph based on links specified in these files. 
 */

causal_graph_viewer :-
	new(GV, causal_graph_viewer(parameter_relations)),
	send(GV, open).


:- pce_begin_class(causal_graph_viewer, frame).

variable(state_nr, int:=0, both, "The state nr. associated with this frame").

% variable(type, name:=unknown, both, "The type of node: state, entity, 
%						attribute or quantity").

variable(inactive_displayed, bool:=(@on), both, "Whether inactive dependencies are displayed").
variable(submissive_displayed, bool:=(@on), both, "Whether submissive dependencies are displayed").
variable(balanced_displayed, bool:=(@on), both, "Whether balanced dependencies are displayed").
variable(inactive_colour, colour:=colour(black), both, "Colour of inactive dependencies").
variable(submissive_colour, colour:=colour(black), both, "Colour of submissive dependencies").
variable(balanced_colour, colour:=colour(black), both, "Colour of balanced dependencies").


initialise(GV, ViewType:name, HelpID: [name], ShowLeftDialog: show_left_dialog = [bool]) :->
	"Create causal_graph-viewer"::
	%gp3 0.3.13: added HelpId and assistance frame above
	%also creates the left_dialog used in dependency view. 
	%(when show_left_dialog is @on). 
	%this because we have to do all the placement right the first time, so we keep it here in 1 call
	%show_relations.pl --> create_relations_toggle_menu still has the code to fill the left_dialog, 
	%but the dialog itself is placed here

	send(GV, send_super, initialise, ''),

	GV->>icon(@simulate_icon),
	send(GV, append, new(P, picture(size := size(600,500)))),

	%%gp3 0.3.13: make sure stretching and shrinking is as explicit as possible
	%this also means we wait with placement until this is writen out
	P->>hor_stretch(100),
	P->>ver_stretch(100),
	P->>hor_shrink(100),
	P->>ver_shrink(100),
	%%
	
	send(P, selection_feedback, @nil),
	% send(P, selection_feedback, colour(red)),
        % for some reason, the initialisation in the variable definition 
        % does not work. This may be a bug in XPCE 6.2.2 (SWI-Prolog 5.2.2).
        % Therefore, the colours are specified here again:
	send(GV, inactive_colour, colour(black)), % grey
	send(GV, submissive_colour, colour(black)), % blue
	send(GV, balanced_colour, colour(black)), % orange

	
	D *= dialog,
	send(D, name, bottom_dialog),
	
	%%gp3 0.3.13: make sure stretching and shrinking is as explicit as possible
	D->>hor_stretch(100),
	D->>hor_shrink(100),
	D->>ver_stretch(0),
	D->>ver_shrink(0),
	
	D->>below(P),
	fill_dialog1(GV, D, ViewType),
	
	%gp3 0.3.13 left dialog:
	if
		ShowLeftDialog == @on
	then
	(
		new(Left, dialog),
		send(Left, name, left_dialog),
		Left->>pen(0),
		Left->>border(size(2,2)),
		Left->>gap(size(0,0)),
		Left->>hor_stretch(0),
		Left->>hor_shrink(0),
		Left->>ver_stretch(100),
		Left->>ver_shrink(100),
		send(Left, left, P)
	),
	
	%gp3 0.3.13: add assistance stuff
	LeftOfAssistance *= dialog,
	LeftOfAssistance->>pen(0),
	LeftOfAssistance->>gap(size(0,0)),
	LeftOfAssistance->>border(size(0,0)), %does nothing, just stretching
	LeftOfAssistance->>ver_stretch(0),
	LeftOfAssistance->>hor_stretch(100),
	LeftOfAssistance->>ver_shrink(0),
	LeftOfAssistance->>hor_shrink(100),	
	LeftOfAssistance->>height(1), %need some height to overrule default
	
	Assistance *= dialog,
	Assistance->>name(assistance),
	Assistance->>pen(0),
	Assistance->>gap(size(0,0)),
	Assistance->>border(size(0,0)),
	Assistance->>ver_stretch(0),
	Assistance->>hor_stretch(0),
	Assistance->>ver_shrink(0),
	Assistance->>hor_shrink(0),
	Help *= helpButton(help,HelpID),
	Assistance->>display(Help),
	Assistance->>right(LeftOfAssistance),
	Assistance->>above(P),

	
	%gp3 0.3.13: make sure all tiles fit neatly
	P?tile?root->>border(0).


fill_dialog1(Frame, D, parameter_relations) :-
	%gp3 0.2: changed buttons to imgButtons, and moved reporter to the end of the
	%clause, so that it will be under the buttons instead of above them
	send(D, append, new(imgButton(layout_entities, message(Frame, layout), tt:='Change layout entities' ))),
	send(D, append, new(imgButton(layout_quantities, message(Frame, layout_par), tt:='Change layout quantities'))),
	send(D, append, new(imgButton(zoom_in, message(@prolog, scale_picture, Frame, 2), tt:= 'Zoom in'))),
	send(D, append, new(imgButton(zoom_out, message(@prolog, scale_picture, Frame, 0.5), tt:= 'Zoom out'))),
	send(D, append, new(imgButton(postscript, message(Frame, postscript), tt:= 'Save Diagram to EPS file'))),
	send(D, append, new(imgButton(close, message(Frame, destroy), tt:= 'Close this window'))),
	send(D, append, label(reporter)),
	send(D, layout).


fill_dialog1(Frame, D, entities_and_attributes) :-
	%gp3 0.2: changed buttons to imgButtons, and moved reporter to the end of the
	%clause, so that it will be under the buttons instead of above them

	send(D, append, new(imgButton(layout_entities, message(Frame, layout), tt:='Change layout entities' ))),
	send(D, append, new(imgButton(zoom_in, message(@prolog, scale_picture, Frame, 2), tt:= 'Zoom in'))),
	send(D, append, new(imgButton(zoom_out, message(@prolog, scale_picture, Frame, 0.5), tt:= 'Zoom out'))),
	send(D, append, new(imgButton(postscript, message(Frame, postscript), tt:= 'Save Diagram to EPS file'))),
	send(D, append, new(imgButton(close, message(Frame, destroy), tt:= 'Close this window'))),
	send(D, append, label(reporter)),
	send(D, layout). %gp3 0.3.13 changed this from layout_dialog



clear(F) :->
	"Clear the diagram"::
	get(F, member, picture, P),
	send(P, clear).

layout(F) :->
	"Run graph layout"::
	get(F, member, picture, P),    
	(   
	    get(P, graphicals, Grs)
	    %get(P?graphicals, head, Head)
	->  
	    % layout parameters, with default values: 
	    % spring strength (2) 
	    % natural spring length (30) 	   
	    % outward force between unconnected nodes (15)
	    % how much the network is changed every time (15)
	    % number of iterations (100)
		
	    % send(Head, layout, 2, 15, 7, 10, 20),
	    % send(Head, layout, 5, 15, 5, 1, 200),

	    send(F, reset_moved_tags, Grs), 

	    send(Grs, for_some, message(@arg1, layout, 20, 40, 5, 20, 20)),
	    send(Grs, for_some, message(@arg1, layout, 15, 30, 5, 15, 20)),
	    send(Grs, for_some, message(@arg1, layout, 10, 25, 2, 2, 200)),
	    % send(Grs, for_all, message(@arg1, layout, 10, 30, 5, 10, 100)),
	    % send(Grs, for_all, message(@arg1, layout, 10, 30, 2, 2, 200)),
	    get(P, bounding_box, area(X, Y, _W, _H)),
	    text_margin(XMargin, YMargin),
    	    send(P, scroll_to, point(X-XMargin, Y-YMargin))
	;   send(F, report, error, 'No graph to layout!') % no graph to layout!
	).


% reset_moved_tags(F, Grs)
% 
% resets the tag_has_been_moved variable of all my_tagged_connections 
% (within Grs) between quantities in different entities
%
reset_moved_tags(_F, Grs):->    
       get(Grs, find_all, 
	    and(message(@arg1, instance_of, my_tagged_connection), 
	       @arg1?type == 'par_relation', 
	       @arg1?tag_has_been_moved == @on 
 	    ), ConnectionsChain), 
       send(ConnectionsChain, for_all, 
	    message(@arg1, tag_has_been_moved, @off)
       ).

layout_par(F) :->
	"Run graph layout"::
	get(F, member, picture, P),
        get(F, state_nr, N), 
	(   get(P, graphicals, Grs)
	->  
	    send(Grs, for_some, 
		and(
			message(@arg1, has_get_method, graphicals), 
			message(@arg1, instance_of, graph_node), 
			message(@arg1?graphicals, for_some, 
				and(
				  message(@arg1, has_get_method, graphicals), 
				  message(@arg1, has_get_method, type), 
		    		  @arg1?type == 'quantity',
				  message(@arg1, instance_of, graph_node), 
				  message(@arg1, reset_tag_has_been_moved),
				  message(@arg1, layout, 5, 10, 1, 5, 100),
				  message(@arg1, layout, 3, 15, 3, 3, 200)
				)
			)
		)
	    ),

            % make sure that the subfigures are positioned ok
            send(Grs, for_some, 
                and(message(@arg1, instance_of, graph_node),
		    message(@arg1, has_get_method, type), 
		    @arg1?type == 'entity',
                    % make sure that the subfigures are positioned ok
                    message(@prolog, place_subfigures, @arg1, @default, entity, N)
                )
            ),

	    get(P, bounding_box, area(X, Y, _W, _H)),
	    text_margin(XMargin, YMargin),
    	    send(P, scroll_to, point(X-XMargin, Y-YMargin))
	;   
    	    % !! no graph to layout!
	    send(F, report, error, 'No graph to layout!')  
	).


postscript(F) :->
	"Create PostScript in file"::
	%gp3 0.3.12 made some changes (finder, if )
	if
		get(@garp3_finder, file, F, 'Save Diagram to EPS file', @off, '.eps', FileName)
	then
	(
		get(F, member, picture, Pict),
		new(File, file(FileName)),
		send(File, open, write),
		send(File, append, Pict?postscript),
		send(File, close),
		send(File, done),
		send(F, report, status, 'Saved PostScript in %s', FileName)
	).



% visualization of parameters within entities 
gen_entity_causal_model(F) :->
	"Create graph from causal model relations"::

	get(F, state_nr, N),

	show_system_elements(F, N),
 
	% display parameters within entities
	show_parameters(F, N,causal), %gp3: added 3rd argument

	% display values within parameter quantity spaces
	show_hide(F, values, @on),

	% display derivatives next to parameter quantity spaces
	gen_derivatives(F, N),

	send(F, connect_entities),

	%gp3 1.4 added RelationTerm
	(   Term = visualize:show_binary_model(N, Rel, From, To, Dir, RelationTerm)
	->  
	    forall(user:Term,
		  (
		   atomize(From, FromAtom), 
		   atomize(To, ToAtom), 

		   (catch(
		     gen_dependency_or_inverse(F, N, Rel, FromAtom, ToAtom, Dir, RelationTerm),
			_, fail)
		   ->
			true 
		   ;
%%%% begin changes  - AB 25/09/2005
		       writef('The dependency %d %d %d was not drawn, probably because the items involved are currently not active or visible.\n',[FromAtom, Rel, ToAtom])
%%%% end of changes - AB 25/09/2005
		   )
		  )),
	    send(F, layout)
	;   send(F, report, error,
		 'Error in causal model generator')
	),
	show_hide(F, derivatives, @off),
	show_hide(F, values, @off),
        show_hide(F, assumptions, @off),
	show_hide(F, e_without_q, @off).
 

gen_inverse_math_rel(F, N, Rel, FromAtom, ToAtom, Dir, RelationTerm):-
		   inverse_math_relation(Rel, InvRel), 
		   (catch(
		     gen_dependency_in_ent(F, N, InvRel, ToAtom, FromAtom, Dir, RelationTerm),
			_, fail)
		   ;                       
                        fail
		   ).


% visualization of parameters within entities, especially for input model
gen_entity_causal_model_input(F) :->
	"Create graph from causal model relations"::
	N is 0,
	show_system_elements(F, N),
 
	% display parameters within entities
	show_parameters(F, N, causal), %gp3 added 3rd argument

%	% display values within parameter quantity spaces
%	show_hide(F, values, @on),

%	% display derivatives next to parameter quantity spaces
%	gen_derivatives(F, N),

	(   Term = visualize:show_binary_model(N, Rel, From, To, Dir, RelationTerm)
	->  
	    forall(user:Term,
		  (
		   atomize(From, FromAtom), 
		   atomize(To, ToAtom), 
		   (catch(
		     gen_dependency_or_inverse(F, N, Rel, FromAtom, ToAtom, Dir, RelationTerm),
			_, fail)
		   ->
			true 
		   ;
%%%% begin changes  - AB 25/09/2005
		        writef('The dependency %d %d %d was not drawn, probably because the items involved are currently not active or visible.\n',[FromAtom, Rel, ToAtom])
%%%% end of changes - AB 25/09/2005
		   )
		  )),
	    send(F, layout)
	;   send(F, report, error,
		 'Error in causal model generator')
	),
	show_hide(F, derivatives, @off),
	show_hide(F, values, @off).




% visualization of parameters within entities 
%
% this clause is especially for the mf graphical details view
% because there, the assumptions and entities without quantities
% need to be shown
%
gen_entity_causal_model_for_mf(F) :->
	"Create graph from causal model relations"::

	get(F, state_nr, N),

	show_system_elements(F, N), %gp3: changed this to nice names
 
	% display parameters within entities
	show_parameters(F, N,mf), %gp3 added 3rd argument

	% display values within parameter quantity spaces
	show_hide(F, values, @on),

	% display derivatives next to parameter quantity spaces
	gen_derivatives(F, N),

	send(F, connect_entities),

	(   Term = visualize:show_binary_model(N, Rel, From, To, Dir, RelationTerm)
	->  
	    forall(user:Term,
		  (
		   atomize(From, FromAtom), 
		   atomize(To, ToAtom), 
		   (catch(
		     gen_dependency_or_inverse(F, N, Rel, FromAtom, ToAtom, Dir, RelationTerm),
			_, fail)
		   ->
			true 
		   ;

%%%% begin changes  - AB 25/09/2005
		        writef('The dependency %d %d %d was not drawn, probably because the items involved are currently not active or visible.\n',[FromAtom, Rel, ToAtom])
%%%% end of changes - AB 25/09/2005
		   )
		  )),
	    send(F, layout)
	;   send(F, report, error,
		 'Error in causal model generator')
	),
	show_hide(F, derivatives, @off),
	show_hide(F, values, @off).

 
% display all parameters in state N
% positioned within their appropriate entities
%
show_parameters(F, N, Type):-
	%gp3, we changed this to show the design names
	%except when dependencies_internal_quantity_naming = on and Type = causal
	%we added Type, which is causal (dependencies) or mf
	%gp3 0.2 added tooltips for parameters, gp3 1.4 changed this
	
	index_state(N, parameters, ParList), 
	%we need the inputsystem again
	IS = @app<<-currentScenario,
	get(@model, modelState, MS), % JL 
	forall(member(Par, ParList), 
		(
		engine:parameter(Par, _, Pred, Entity, ParName, _, QS), %gp3: added Pred and QS
		%gp3: find the definition
		if
		(
			MS = legacy
			;
			(
				Type = causal,
				@on = @app<<-setting(dependencies_internal_quantity_naming)
			)
		)
		then
		(
			QuantityName = ParName,
			QD = @nil
		)
		else
		(
			if
				QD = @app<<-findExportedObject(qd,Pred)
			then
				QuantityName = QD<<-name
			else
				QuantityName = Pred
		),
		%gp3 0.2: we need the design object to find the designType
		if
			InsObject = @app<<-findExportedObject(in,Entity,IS)
		then
		(
			InsName = InsObject<<-name,
			DesignType = InsObject?definition<<-class_name
		)
		else
		(
			InsName = Entity,
			DesignType = entity
		),
		
		atomize(Entity, EntityStr), %internal name
		atomize(ParName, ParNameStr), %internal name

		% get or create corresponding entity node
		%gp3 node note: this is not very efficient, because above, we collect all
		%this information while the node might allready exist
		%so we could change this to finding the node first and creating it only
		%when that fails. (under the future research section)
		%gp3 added label and designtype
		
		get(F, node, EntityStr, entity, N, InsName, DesignType, EntityNode),	

		% get or create new node
		(   get(EntityNode, member, ParNameStr, ParNode)
		->  
		    true
		;   
		    get(F, node, ParNameStr, quantity, N, QuantityName, ParNode),
		    position_within(ParNode, EntityNode),
		    %add a tooltip for comments (use the one without 'no comments' automatically added
				relevant_comments(Par,N,'', Tooltip),
				
				%gp3 1.4: because of readability etc, we also add any comments  for QS
				if
				(
					QSDEF = @app<<-findExportedObject(qs,QS),
					QSRemarks = QSDEF<<-relevantComments,
					(\+ 0 = QSRemarks<<-size)
				)
				then
				(
					Tooltip->>ensure_nl(?(QSRemarks,split_lines,120))
				),
		
				%still empty?
				if 
					0 = Tooltip<<-size
				then
					Tooltip->>append('(no comments)'),
				/*
				%saved on request:
		    %add a tooltip to show the entity name
		    Tooltip *= string('Quantity %s\nInstance: %s',
		    	QuantityName,InsName),
		    if
		    (
		    	QD \== @nil,
		    	(\+ 0 = QD?remarks<<-size)
		    )
		    then
		    	Tooltip->>ensure_nl(QD?remarks),
		    */
		    ParNode->>tooltip(Tooltip,model)
		)
		)
	).




% gen_dependency_or_inverse(F, N, VRel, A, B, Dir, RelationTerm)
% gp3 1.4 added the RelationTerm argument (original engine term)
gen_dependency_or_inverse(F, N, VRel, A, B, Dir, RelationTerm):-
	gen_dependency_in_ent(F, N, VRel, A, B, Dir, RelationTerm).


% if the above does not succeed, it may be a 
% math dependency which can be reversed
%
gen_dependency_or_inverse(F, N, VRel, A, B, Dir, RelationTerm):-
        debug(sim_bug, 'try if it is a math dependency which can be inversed: ~w ~w ~w - ~w\n',[A, VRel, B, RelationTerm]), 
        gen_inverse_math_rel(F, N, VRel, A, B, Dir, RelationTerm).

% if the above does not succeed, it may be a 
% general math dependency, which can not be attached to any one quantity
%
gen_dependency_or_inverse(F, N, VRel, A, B, Dir, RelationTerm):-
	% draw labeled connection link
        gen_general_constraint_dependency(F, N, VRel, A, B, Dir, RelationTerm).



% If A and B are not related to specific quantities, then list them 
% textually within a dummy entity called 'general constraints'.
%
gen_general_constraint_dependency(F, N, VRel, A, B, Dir, RelationTerm):-
	gen_math_rel_formulas(F, N, VRel, A, B, Dir, RelationTerm).


% display causal relation between two parameters, which have 
% to be positioned within their appropriate entities
%
gen_dependency_in_ent(F, N, Rel, A, B, Dir, RelationTerm):-
%	term_to_atom(From, A), 

	% find corresponding entities
	find_quantity_entity(N, A, _QPred, _Type, _QSpace, AEntity),
	find_quantity_entity(N, B, _QPred2, _Type2, _QSpace2, BEntity),
 
	atomize(AEntity, AEntityStr), 
	atomize(BEntity, BEntityStr), 

	% get or create corresponding entity nodes
	%gp3 this should allways be "get", otherwise we need to get all the needed
	%design info again. This should allways allready be there.
	get(F, node, AEntityStr, entity, N, NAE),	
	get(F, node, BEntityStr, entity, N, NBE),

	% get or create new nodes	
	(   get(NAE, member, A, NA)
	->  
	    true
	;   
	    get(F, node, A, quantity, N, NA),
	    position_within(NA, NAE) 
	),

	(   get(NBE, member, B, NB)
	->  
	    true
	;   
	    get(F, node, B, quantity, N, NB),
	    position_within(NB, NBE) 
	),

	% draw labeled connection link
	get(string('%s%s%s', A, Rel, B), value, STR),
	% does it exist already?
	(   catch(get(F, my_tc, STR, _TC), _, fail)
	->  
	    true
	;
	    send(F, display_labeled_arc2, STR, NA, NB, Rel, N, Dir, RelationTerm),
	    % connect the entities too, to enforce better layout
	    get(string('%s_extra', STR), value, STR2),
	    new(_TC2, my_tagged_connection(STR2, NAE, NBE, @graph_link2, layout, 'X', none))
	).



% If A and B are not related to specific entities, then just draw 
% the parameters and the relation in between 
%
gen_dependency_in_ent(F, N, VRel, A, B, Dir, RelationTerm):-
	get(string('%s%s%s', A, VRel, B), value, _STR),
        % No entities 
	% somehow, the relation name was already visualized!
	math_relation(Rel, VRel), 
	term_to_atom(AT, A), 
	term_to_atom(BT, B), 
	gen_math_rel(F, N, Rel, AT, BT, Dir, RelationTerm).





% highlight causal relation between two parameters,  
% positioned within their appropriate entities, in Colour
%
%gp3 0.2 We added an argument which - when ground - gives the elementState of
%a dependency_icon node in the tagged connection (c,g or i for imported = 'do not show the state')
%when not given (not ground), the elementstate is not changed.
%this is a bit of a dirty fix, but we do not want to break existing code
%(just yet). And its a better fix than letting class dependency_icon react 
%to ->colour by resetting the image...

colour_dependency_in_ent(F, N, Rel, A, B, Colour, ElementState):-
	% find corresponding entities

	find_quantity_entity(N, A, _QPred, _Type, _QSpace, AEntity),
	find_quantity_entity(N, B, _QPred2, _Type2, _QSpace2, BEntity),

	atomize(AEntity, AEntityStr),	
	atomize(BEntity, BEntityStr),	
 
	% get corresponding entity nodes
	get(F, node, AEntityStr, entity, N, NAE),	
	get(F, node, BEntityStr, entity, N, NBE),

	% get quantity nodes	
   	get(NAE, member, A, _NA),
	get(NBE, member, B, _NB),

	% get labeled connection link
	get(string('%s%s%s', A, Rel, B), value, STR),
	get(F, my_tc, STR, TC), 
	send(TC, colour, Colour),
	%gp3: do we have an element state?
	if
		ground(ElementState)
	then
		TC->>setElementState(ElementState).




% highlight relation between two parameter values,  
% positioned within their appropriate entities, in Colour
% case of formula: A0 Rel B0, where both A0 and B0 are 
% structured values, like in:
% equal(normal(amount_of_fat1), normal(amount_of_fat2))
%
%gp3 0.2 We added an argument which - when ground - gives the elementState of
%a dependency_icon node in the tagged connection (c,g or i for imported = 'do not show the state')
colour_math_rel(F, _N, VRel, Ain, Bin, Colour, ElementState):-
	% check whether A0 and B0 occur in some quantity space of 
	% parameters A and B
	atomize(Ain, A0), 
	atomize(Bin, B0), 
	term_to_atom(A1, A0), 
	term_to_atom(B1, B0), 
	A1 =.. [A, ParA], 
	B1 =.. [B, ParB], 


	engine:qspace(ParA, _PredicateA, QSpaceListA, _FailA), 
	(	
		qspace_member(A1, QSpaceListA, _IndexA)
	;
		qspace_member(A, QSpaceListA, _IndexA2)
	),

	engine:qspace(ParB, _PredicateB, QSpaceListB, _FailB), 
	(	
		qspace_member(B1, QSpaceListB, _IndexB)
	;
		qspace_member(B, QSpaceListB, _IndexB2)
	),


	% construct string ACodeStr for the new information 'A0 VRel B0'
	construct_code_str(A0, B0, VRel, ACodeStr), 

	% draw arc for the relation
	% draw labeled connection link
	% does it exist already?
	(   catch(get(F, my_tc, ACodeStr, TC), _, fail)
	->  
	(
	    send(TC, colour, Colour),
	    %gp3: do we have an element state?
		if
			ground(ElementState)
		then
			TC->>setElementState(ElementState)
	)
	;
		true % gp3 0.4.4 was some write_ln calls
	).



% highlight (v-)correspondence relation between two parameter values,  
% positioned within their appropriate entities, in Colour
%

colour_math_rel(F, _N, VRel, Ain, Bin, Colour,ElementState):-
	% check whether A0 and B0 occur in some quantity space of 
	% parameters A and B
        % this is hopelessly complex, but it works...
	atomize(Ain, Aatom), 
	atomize(Bin, Batom), 
	term_to_atom(Aterm, Aatom), 
	term_to_atom(Bterm, Batom), 
	Aterm =.. [ParA, ValA], 
	Bterm =.. [ParB, ValB], 

        strip_atomize(ValA, A0),
        strip_atomize(ValB, B0),

        ARevTerm =.. [A0, ParA],
        BRevTerm =.. [B0, ParB],

	engine:qspace(ParA, _PredicateA, QSpaceListA, _FailA), 
	(	
		qspace_member(ValA, QSpaceListA, _IndexA)
	;
		qspace_member(A0, QSpaceListA, _IndexA2)
	),

	engine:qspace(ParB, _PredicateB, QSpaceListB, _FailB), 
	(	
		qspace_member(ValB, QSpaceListB, _IndexB)
	;
		qspace_member(B0, QSpaceListB, _IndexB2)
	),

%	math_relation(Rel, VRel), 


	% construct string ACodeStr for the new information 'A0 VRel B0'
	construct_code_str(ARevTerm, BRevTerm, VRel, ACodeStr), 

	
	% draw arc for the relation
	% draw labeled connection link
	% does it exist already?
	(   catch(get(F, my_tc, ACodeStr, TC), _, fail)
	->  
		(
		    send(TC, colour, Colour),
			if
				ground(ElementState)
			then
				TC->>setElementState(ElementState)
		)

	;
		true %gp3 0.4.4 used to be some write_ln calls
	).




% reset_colours(F, Colour):-	
%
% resets the colours of all Graphicals in P in F to Colour. 
% Not just the top-level graphicals, but also graphicals 
% inside other figures
%
reset_colours(F, Colour):-	
	get(F, member, picture, P), 
	% reset colours of all graphicals
	% Jan Wielemakers tips and tricks: repeat by failure [hmm, Bratko basics I would say...]
        \+ get(P, find, @default,
        	and(	
			message(@arg1, colour, Colour),
			% turn off selection_feedback
			message(@arg1, has_send_method, selection_style),
			message(@arg1, selection_style, none),
			new(or)
		), _ColourGr
	),
	% reset fill_patterns of filled graphicals
        \+ get(P, find, @default,
        	and(
			message(@arg1, has_get_method, fill_pattern),
			@arg1?fill_pattern \== @nil, 
			@arg1?fill_pattern \== colour(white), 
			message(@arg1, fill_pattern, colour(Colour)),
			new(or)
		), _FilledGr
	).
	


% make sure node Node is positioned within ParentNode
%
position_within(Node, ParentNode):-
	get(ParentNode, height, H), 
	text_margin(_LeftMargin, TopMargin), 	
	send(ParentNode, display, Node, point(25, H+0.2*TopMargin)).



:- pce_global(@graph_link2, make_graph_link2).

make_graph_link2(L) :-
        new(L, link(link, link, line(0,0,0,0,second))).


:- pce_global(@graph_link3, make_graph_link3).

make_graph_link3(L) :-
        new(L, link(link, link, line(0,0,0,0,second))),
	% adapt black arrow to white diamond shape
	send(L, second_arrow, @uml_many_arrow).

:- pce_global(@graph_link4, make_graph_link4).

make_graph_link4(L) :-
        new(L, link(out, in, line(0,0,0,0,second))).

display_arc(F, From:name, To:name) :->
	"Display arc From -> To"::
        input_to_int1(From, FromInt),

	% get(F, node, From, state, FromInt, SMFromNode), 
	% get(SMFromNode, member, From, FromNode), 
	get(F, node, From, state, FromInt, FromNode), 

	% get(F, node, To, state, To, SMToNode), 
	% get(SMToNode, member, To, ToNode), 
	get(F, node, To, state, To, ToNode), 

	
	new(C, my_connection(FromNode, ToNode, @graph_link2)),
	transition_name1(From, To, Name),
	send(C, name, Name).


% necessary to transform the state name 'input' to an integer,
% at least, a string containing an integer.
input_to_int1('input', 0):-!.

input_to_int1(X,X):-!.


%colour(white).
%colour(black).

display_labeled_arc(F, From:name, To:name, Rel:name, _N:int, _Dir:name) :->
	"Display arc From -> To"::
	get(string('%s%s%s', From, Rel, To), value, STR),

	(   catch(get(F, my_tc, STR, _TC), _, fail)
	->  
	    true 
	;
	    fail
	).
   

display_labeled_arc(F, From:name, To:name, Rel:name, N:int, Dir:name) :->
	"Display arc From -> To"::
	get(string('%s%s%s', From, Rel, To), value, STR),
    	get(F, node, From, quantity, N, NF),
	get(F, node, To, quantity, N, TF),	  

	new(TC, my_tagged_connection(STR, NF, TF, @graph_link4, par_relation, Rel, Dir)),
	% send(TC, type, par_relation), 
        new(Icon, dependency_icon(TC, Rel)), 	 
        % handy for expert users to edit dependency view
	% make_icon_editable(TC, Icon),
	send(TC, tag, Icon).     

make_icon_editable(TC, Icon):-
        new(PopupMenu, popup(popupmenu)), 
        send_list(PopupMenu, append,
		[	
			menu_item('black', 
				  message(TC, colour, black)),
                        menu_item('blue', 
				  message(TC, colour, blue)),
			menu_item('red',
				  message(TC, colour, red)), 
			menu_item('orange',
				  message(TC, colour, orange)), 
			menu_item('grey',
				  message(TC, colour, grey)), 
			menu_item('white',
				  message(TC, colour, white)),
			menu_item(destroy,
				  message(TC, destroy))
	 	]),
        send(Icon, popup, PopupMenu).

% hide_dependency_or_inverse(F, From, To, VRel, _N) 
%
% destroys the visualization of a dependency
%
hide_dependency_or_inverse(F, From, To, VRel, N) :-
	 hide_dependency(F, From, To, VRel, N).



% if the above does not succeed, try to reverse the dependency first
hide_dependency_or_inverse(F, From, To, VRel, N) :-	 
         % if To is not zero, then try the reverse dependency
         To \= zero,
	 inverse_math_relation(VRel, InvRel), 
         % writef('The reverse dependency, %d %d %d will be tried now\n', [To, InvRel, From]),
	 hide_dependency(F, To, From, InvRel, N).
	 
% Rest case - succeed if the above goes wrong
% 
hide_dependency_or_inverse(_F, _From, _To, _VRel, _N) :-
        % writef('The dependency %d %d %d could not be hidden or was already invisible\n', [From, VRel, To]).
        true.


% Currently, this predicate just destroys the link
%
hide_dependency(F, From, To, VRel, _N) :-
	% this is not a very nice fix to ensure that both 
	% terms like a(b) and atom strings like 'a(b)' 
	% get converted to the original terms  
	atomize(From, FromAtom), 
	atomize(To, ToAtom), 
	term_to_atom(FromTerm, FromAtom), 
	term_to_atom(ToTerm, ToAtom), 
 	% construct string ACodeStr 
	construct_code_str(FromTerm, ToTerm, VRel, ACodeStr), 
	% get labeled connection link
	get(F, my_tc, ACodeStr, TC), 

	send(TC, destroy).


% For the simple ACodeStr case
%
hide_dependency(F, From, To, VRel, N) :-
	atomize(From, FromAtom), 
	find_par_node(F, N, FromAtom, NF), 

	get(NF, member, par_relations, ParRelGroupNode),

	% this is not a very nice fix to ensure that both 
	% terms like a(b) and atom strings like 'a(b)' 
	% get converted to the original terms  
	atomize(To, ToAtom), 
	term_to_atom(FromTerm, FromAtom), 
	term_to_atom(ToTerm, ToAtom), 
	construct_code_str(FromTerm, ToTerm, VRel, ACodeStr), 

	get(ParRelGroupNode, member, ACodeStr, ParRelNode),
	send(ParRelNode, destroy).


% Special case for general constraints
%
hide_dependency(F, From, To, VRel, N) :-
	% check if From and To are formulas

	% this is not a very nice fix to ensure that both 
	% terms like a(b) and atom strings like 'a(b)' 
	% get converted to the original terms  
	atomize(From, FromAtom), 
	atomize(To, ToAtom), 
	term_to_atom(A, FromAtom),
	term_to_atom(B, ToAtom),

        (
          (
           % both A and B are formulas
           % Example: (growth1 + growth2) = (inflow1 - outflow1)
           % or: equal(plus(growth1, growth2), min(inflow1, outflow1)), 
           formula(A), 
	   formula(B)
          )
        ;  % OR
          (
           % A is a specific value and B is a formula
           % Example: max(size1) =< max(size2) + max(size3)
           is_value(A), 
	   formula(B)
          )
        ;  % OR
          (
           % A is a formula and B is a specific value
           % Example: max(size2) + max(size3) >= max(size1)
	   formula(A),
           is_value(B) 
          )
        ;  % OR
          (
           % A is a formula and B is zero
           % Example: size2 - size3 = zero
           formula(A), 
	   B = zero
          )
        ;  % OR
          (
           % A is zero and B is a formula
           % Example: zero = size2 - size3
	   A = zero,
           formula(B) 
          )
        ),
	
	% formula(A), 
	% formula(B), 

	get(string('general constraints'), value, EntityStr),	
	construct_math_str(N, A, A0, EntityStr), 
	construct_math_str(N, B, B0, EntityStr),
        % swritef(MathStr, '%w %w %w', [A0, VRel, B0]),
        % writef('MathStr: %w\n', [MathStr]),

        % get node for constraints 
	find_par_node(F, N, 'constraints', NF), 
	% get the par_relations group node within node NF
	get(NF, member, par_relations, ParRelGroupNode),

	construct_code_str(A0, B0, VRel, ACodeStr), 
	% construct_code_str(A0, B0, VRel, ACodeStr), !,
        % writef('ACodeStr: %d \n', [ACodeStr]),
	
	% if a node for this relation exists, destroy it
	( get(ParRelGroupNode, member, ACodeStr, ParRelNode)
	->
	        send(ParRelNode, destroy)
	).




% find_par_node finds the node named in the string FromAtom
% in frame F
%
find_par_node(F, _N, FromAtom, NF):-
	term_to_atom(FromTerm, FromAtom), 
	atom(FromTerm),
	% get(F, node, FromAtom, quantity, N, NF).
	get(F, my_node, FromAtom, NF). % this does not create a node when it is not there

% in case FromAtom is something like 'normal(flow_area1)',
% this returns the node named flow_area1.
%
find_par_node(F, N, FromAtom, NF):-
	term_to_atom(FromTerm, FromAtom), 
	FromTerm =.. [_Value, Parameter],
	get(F, node, Parameter, quantity, N, NF).





% construct_code_str(A, B, Rel, STR)
%
% just concats A, Rel, B to form string STR
%
construct_code_str(A, B, Rel, STR):-
	atomize(A, FromAtom), 
	atomize(B, ToAtom), 
	get(string('%s%s%s', FromAtom, Rel, ToAtom), value, STR).


% construct_math_str(N, _A0, B0, Rel1, AMathStr)
%
% case of complex formula: A0 Rel B0, where B0 is a formula itself, 
% like in equal(growth1, min(inflow1, outflow1)), 
%
%gp3 0.4.11: added state number, needed to find quantity design name
construct_math_str(N, A0, B0, Rel1, AMathStr):-
%	term_to_atom(From, A0), 
%	term_to_atom(To_Structure, B0), 
	B0 =.. [Rel2, B1, C0],

	find_quantity_entity(N, A0, _Pred,_,_, Entity), 
	construct_math_str(N, C0, C, Entity), 
	construct_math_str(N, B1, B, Entity), 

	math_relation(Rel1, VRel1), 
	math_relation(Rel2, VRel2), 

	atomize(B, Batom), 
	atomize(C, Catom), 

	% construct string AMathStr for the new information 'VRel1 B VRel2 C'
	get(string('%s %s %s %s', VRel1, Batom, VRel2, Catom), value, AMathStr).


% case of simple relation involving a derivative and a value: 
% e.g., amount1 d_equal zero 
%
construct_math_str(_N, _A0, B0, Rel1, AMathStr):-
	atom(B0),  % does this need to be an atom?! that is assumed here
	math_relation(Rel1, VRel1), 
	% construct string AMathStr for the new information 'VRel1 B0'
	get(string('%s %s', VRel1, B0), value, AMathStr).






% construct_math_str(N, C0, C, E)
%
% creates substring for nested mathematical relations
%gp3 0.4.11: added state number, needed to find quantity design name
% 
% the atom case
%gp3 0.4.11 We split up the atom case in two cases: one where the legacy name is wanted
%and one where the design name is wanted
	       
	       
construct_math_str(N, C0, C0Str, CurrentEntity):-
	%gp3 0.4.11: added for design name version: name must be an atom, we are not in legacy mode, and we do not want internal naming
	atom(C0),
	(\+ get(@model, modelState, legacy) ), % JL

	@off = @app<<-setting(dependencies_internal_quantity_naming),
	%no cut here: when something fails here, we fall through to the next (legacy) one
	find_quantity_entity(N,C0,Pred,_,_, Entity), %this is why we needed N
	if
		QD = @app<<-findExportedObject(qd,Pred)
	then
		QDName = QD<<-name
	else
		QDName = C0,

	%gp3 1.0.0 By special request of BB we do NOT show the entity name, just the design quantity name
	% Suggestion: only add entity names for entities other than the current one
	% C0Str = string('%s',QDName)<<-value. %make sure prolog understands...

	IS = @app<<-currentScenario,
	if
		Ins = @app<<-findExportedObject(in,Entity,IS)
	then
		InsName = Ins<<-name
	else
		InsName = Entity,

        if Entity = CurrentEntity
        then 
	(
	       C0Str = string('%s',QDName)<<-value
	)
	else 
	(
	       C0Str = string('%s(%s)', QDName, InsName)<<-value
	       % C0Str = string('%s:%s)', InsName,QDName)<<-value
	). 

%
construct_math_str(_N, C0, C0, _E):-
	%the legacy version. Just keep the atom
	atom(C0),!.
%
% the recursive case, involving another mathematical relation
construct_math_str(N, C0, C, E):-
	C0 =.. [Rel, A0, B0], 
	math_relation(Rel, VRel), 
	construct_math_str(N, A0, A, E),
	construct_math_str(N, B0, B, E),
	% construct string C: '(A VRel B)'
	get(string('(%s %s %s)', A, VRel, B), value, C).


% the value(parameter) case (and other unforeseen cases?)
construct_math_str(N, C0, C0Str, _E):-
	C0 =.. [V, Q], 

	(\+ get(@model, modelState, legacy) ), % JL

	@off = @app<<-setting(dependencies_internal_quantity_naming),
	%no cut here: when something fails here, we fall through to the next case
	find_quantity_entity(N, Q, Pred,_,_, Entity), %this is why we needed N
	if
		QD = @app<<-findExportedObject(qd,Pred)
	then
		QDName = QD<<-name
	else
		QDName = Q,

	% Here we show the quantity and entity name

	IS = @app<<-currentScenario,
	if
		Ins = @app<<-findExportedObject(in,Entity,IS)
	then
		InsName = Ins<<-name
	else
		InsName = Entity,

	C0Str = string('%s(%s(%s))', V,QDName,InsName)<<-value. %make sure prolog understands...
	% C0Str = string('%s(%s: %s)', V,InsName, QDName)<<-value. %make sure prolog understands...



% other unforeseen cases?
construct_math_str(_N,C0, C, _E):-
	atomize(C0, C).



display_labeled_arc2(_F, STR:name, FromNode:node, ToNode:node, Rel:name, N:int, Dir:name, RelationTerm: prolog) :->
	"Display arc From -> To"::
	%gp3 1.4 added the RelationTerm (original engine data)
	
	new(TC, my_tagged_connection(STR, FromNode, ToNode, @graph_link4, par_relation, Rel, Dir)),
  new(Icon, dependency_icon(TC, Rel)), 	 

	%%TOOLTIP (gp3 1.4)
	%%get all the comments somehow connected to the RelationTerm
	relevant_comments(RelationTerm,N,Comments),
	Icon->>tooltip(Comments,model),
	
	% make_icon_editable(TC, Icon),
	send(TC, tag, Icon),
        % move if necessary
        send(TC, points).


display_attr_relation(F, From:name, To:name, Rel:name, N:int, Label: name, Tooltip: char_array) :->
	"Display arc From -> To"::
	%gp3 note: this is what we in designtime call "configurations": nominal relations between instances
	%Rel is the internal name of the relation (design: configuration definition)
	%argument Label added by gp3 to set the designtime name without
	%changing any legacy code about finding the object by its name
	%gp3 0.2 added tooltip
	%From and To are the instances
	get(F, node, From, entity, N, NF),
	get(F, node, To, entity, N, TF),
	
	create_attr_link_string(From, Rel, To, STR), 

	%gp3: my_tagged_connection allready has a Label argument, that helps
	new(TC, my_tagged_connection(STR, NF, TF, @graph_link3, attribute, Label, second)),
    
        new(Icon, device),
        send(Icon, display, new(T, text(Label))),
        Icon->>tooltip(Tooltip,model),
        send(T, background, white),

        % handy for expert users to edit dependency view
	% make_icon_editable(TC, Icon),
	send(TC, tag, Icon).     

display_other_attr_relation(F, From:name, To:name, Rel:name, N:int, Tooltip: char_array) :->
	"Display attribute relation within From entity node"::
	%gp3 note: this is what we in designtime call "attributes": local nominal values within an instance
	%Rel is the name of the attribute (design: attribute definition), To is its value
	
	%gp3 1.4 added Tooltip
	
	get(F, node, From, entity, N, NF),
	text_margin(_LeftMargin, TopMargin),
	% create string 'Rel: To' 
	create_attr_node_string(From, Rel, To, A1), 

	% get the attributes group node within entity node NF
	get(NF, member, attributes, AttrGroupNode),

	( get(AttrGroupNode, member, A1, AttrNode) 
	->
	        true
	; 
		% attribute did not exist yet, so draw it
		% create new AttrNode
		new(AttrNode, graph_node(A1, attribute, N)), 
		AttrNode->>tooltip(Tooltip,model),
		% display new AttrNode in AttrGroupNode subfigure
		get(AttrGroupNode, height, H), 
		send(AttrGroupNode, display, AttrNode, point(0, H+0.1*TopMargin))

		% create invisible line 
	).



connect_entities(F) :->
	"Connect all entities in F for layout purposes"::
	
	% a better version could be to connect only those entities 
	% which have no connections yet, plus one with a low (the 
	% lowest?) nr. of connections
	get(F, member, picture, P), 
	get(P, graphicals, GrsChain),
 	% first, find all graphicals of the right type 
	graphical_type(entities, _ArgName, GrType),
	get(GrsChain, find_all, 
		message(@arg1, instance_of, GrType), Grs),
 	% then check whether they are really entities	
	get(Grs, find_all, 
		(@arg1?type == 'entity'), Entities),

 	% then check whether they have connections already
	get(Entities, find_all, 
		    message(@arg1, connected), ConnectedEntities),

	send(Entities, subtract, ConnectedEntities), 
	% Entities are now the UnconnectedEntities

	% connect the unconnected entities in line
	connect_graphicals(Entities),


	% connect one of the previously unconnected ones with 
	% one of the connected ones, if both are present
	% otherwise, do nothing
	( get(Entities, head, Unconnected)
	 ->
	  ( get(ConnectedEntities, head, Connected)
	    -> 
	      new(TwoEntities, chain(Connected, Unconnected)),
	      connect_graphicals(TwoEntities)
	    ;   
	      true
	  )
	 ;
	  true
	).
 

% connect_graphicals(GrsChain)
% 
% connects a chain of Graphicals in line by invisible connection 
% for improved layout
%
connect_graphicals(GrsChain):-
	chain_list(GrsChain, GrsList),         
	forall(	nth1(Index, GrsList, GrTo), 
		((Index == 1)
		 -> 
		    % except for the first item, do nothing
		    true
		 ;
    	    	 % for the other items i = 2 t/m N
    	    	 % connect item i (>1) of GrsList to item i-1
		 Prev is Index - 1, 
		 nth1(Prev, GrsList, GrFrom), 
		  
		 % add a invisible connection to make sure that layout 
		 % mechanism doesn't drive the entities too far from each other
		 new(_C, my_tagged_connection('layout', 
                        GrFrom, GrTo, @graph_link2, layout, 'X', none))
		)
	).

node(F, Name:name, Type:name, StateNr:int, Label: [name], DesignType: [name], Node:graph_node) :<-
	"Get (create) node with specified name"::
	%gp3: optional argument Label which is the name to show (possibly the design-time name)
	%while Name is allways the internal name (also the default)
	%This way, we mess around the least
	%also changed this in the initialize code for graph_node
	%gp3 0.2: added DesignType to send to initialise of new graph_node (see there)
	
	get(F, member, picture, Picture),
	(   get(F, my_node, Name, Node)
	->  true % node existed already
	;   
	    get(Picture, visible, area(X, Y, W, H)),
	    MX is X + W,
	    MY is Y + H,
	    random(X, MX, NX),
	    random(Y, MY, NY),
	    send(Picture, display, 
                     new(Node, graph_node(Name, Type, StateNr,Label, DesignType)), point(NX, NY))
	    %send(Node, name, Name) %gp3 not needed
	).


% get node with name Name from picture in frame F
%
my_node(F, Name:name, Node:graph_node) :<-
	"Get node with specified name"::
	get(F, member, picture, Picture),
	get(Picture, member, Name, Node).

% get node with name Name from picture in frame F
% if it's within a subfigure, the previous clause won't find it
% therefore this clause looks within the subfigures
%
my_node(F, Name:name, Node:graph_node) :<-
	"Get node with specified name"::
	get(F, member, picture, Picture),
	get(Picture, graphicals, GrsChain),
	% not all graphicals are figures
	get(GrsChain, find_all, message(@arg1, instance_of, figure), FigChain),
	chain_list(FigChain, FigList), 
	check_tc(FigList, Name, Node).



% get tagged connection with name Name from picture in frame F
%
my_tc(F, Name:name, TC:my_tagged_connection) :<-
	"Get tagged_connection with specified name"::
	get(F, member, picture, Picture),
	get(Picture, member, Name, TC).


% get tagged connection with name Name from picture in frame F
% if it's within a subfigure, the previous clause won't find it
% therefore this clause looks within the subfigures
%
my_tc(F, Name:name, TC:my_tagged_connection) :<-
	"Get tagged_connection with specified name"::
	get(F, member, picture, Picture),
	get(Picture, graphicals, GrsChain),
	% not all graphicals are figures
	get(GrsChain, find_all, message(@arg1, instance_of, figure), FigChain),
	chain_list(FigChain, FigList), 
	check_tc(FigList, Name, TC).


% TC, with name Name, is a member of the first figure 
% in the list: found!
%
check_tc([Fig|_FigList], Name, TC):-
	get(Fig, member, Name, TC).


% TC is not a member of the first figure; search on
%
check_tc([_Fig|FigList], Name, TC):-
	check_tc(FigList, Name, TC).


:- pce_end_class.
