/*****************************************************************************************************
Garp3 0.1 Based on main source of visigarp (main.pl). This file is split in parts and edited.
All visualize source based on visigarp 2.05, designed and implemented by Anders Bouwer 18/08/1999
Original copyright: (C) 2004 University of Amsterdam. All rights reserved.

Now:
PART OF Garp3. SEE COPYRIGHT NOTICE.

%all code legacy (visigarp) unless mentioned
*******************************************************************************************************/

%loaded into module (namespace) visualize
%class graph_node + subclasses

:- pce_begin_class(graph_node(name, name, int), figure).

variable(state_nr, int:=0, both, "The state nr. associated with this node").
variable(type, name:=unknown, both, "The type of node: state, entity, 
						attribute or quantity").

variable(selection_nr, int:=0, both, "The selection nr. associated with this node").

variable(t_status, name:=unknown, both, "The termination status (for termination nodes and states only").

variable(designType, name:=unknow, both). %gp3 0.2 Added to initialise. In certain cases the designType can be different from type. When type = entity, designtype can be entity, agent, assumption. Normally, initialise sets designtype to type

% top 
handle(w/2, 0, link, link).

% bottom
handle(w/2, h, link, link).

% left
handle(0, h/2, link, link).

% right
handle(w, h/2, link, link).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% In and Out handles
%
% This is done so that connections favour going from the middle 
% straight down or to the right, while the reverse connections 
% generally connect to points just besides the middle
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%
%
% Out handles
%
% top 
handle(w/2, 0, out, link).

% bottom
handle(w/2, h, out, link).

% left
handle(0, h/2, out, link).

% right
handle(w, h/2, out, link).



% In handles

% top 
handle(w/2, 0, in, link).

% bottom
handle(w/2, h, in, link).

% left
handle(0, h/2, in, link).

% right
handle(w, h/2, in, link).

initialise(Node, Name:name, Type:name, StateNr:int,
		Label: label = [name], DesignType: designType = [name]) :->
	"Create from name"::
	%gp3 0.1: added Label, which defaults to Name
	%gp3 0.2: added designType (see variable above) which defaults to Type
	default(Label,Name,RealLabel),
	default(DesignType, Type, RealDesignType),
	Node->>designType(RealDesignType),
	send(Node, send_super, initialise),
    send(Node, type, Type),
	send(Node, display, new(B, box(100,20)), point(0, 0)), %gp3 note: this box is hidden for entity. When this code is rewritten, please use a different subclass for each ?type 
	send(Node, border, 0),
 	set_pen(Node, Type), 
        shape_box(B, Type),
	create_subfigures(Node, Name, Type, StateNr), 
	send(Node, display, new(T, text(RealLabel))),
	T->>name(text), %gp3 added this, always use explicit naming
	position_text(T, B, Type), 
	set_colour(T, B, Type), 
    send(Node, state_nr, StateNr),
	send(Node, send_super, name, Name).


%%%% begin changes  - AB 25/09/2005
% place_subfigures(Node, _Name, entity, N)
%
place_subfigures(Node, _Name, entity, _N):-
        get(Node, type, Type), 
        Type == entity, 
        get(Node, text_member, TitleText), 
        get(Node, member, attributes, AttrGroupNode),  
        get(TitleText, displayed, OnOrOff_TT),  
        get(AttrGroupNode, displayed, OnOrOff_Attr),  
        get(Node, box_member, Box),  
        get(Box, clone, Box2), 
        send(Box, hide), 

        get(TitleText, absolute_position, point(X,Y)), 
        get(TitleText, position, point(TRX,TRY)), 
        % get(AttrGroupNode, absolute_position, point(X2,Y2)), 

        % temporarily, take the title and attributes from 
        % the node and place them somewhere on the picture
        % get(Node?frame, picture_member, P), 
        % this makes sure that they are not destroyed when erased
        get(TitleText, clone, TitleText2), 
        get(AttrGroupNode, clone, AttrGroupNode2), 

        % hide the text 'attributes' of AttrGroupNode2
        send(AttrGroupNode2?text_member, displayed, @off),

        send(Node, erase, TitleText),
        send(Node, erase, AttrGroupNode),
        send(Node, erase, Box),
        send(Node, compute),
        % now, the graphical Node is reduced to the bounding box
        % of its quantity subgraphicals
        % reset the origin to the new left-top corner
        % send(Node, flush), 
	get(Node, bounding_box, area(BX, BY, _BW, _BH)),
        % and redisplay again the title and attributes
	text_margin(LeftMargin, TopMargin),

        % take care of height of attribute_group node if necessary
        (OnOrOff_Attr == @on
        ->
	    get(AttrGroupNode2, height, H)
	 ;
	    H is 0
	),
	send(Node, display, TitleText2, 
               point(TRX+BX-X-LeftMargin, TRY+BY-Y-H-TopMargin)),

        % hide if necessary 
        send(TitleText2, displayed, OnOrOff_TT), 

	send(Node, display, AttrGroupNode2, 
               point(TRX+BX-X, TRY+BY-Y-H+TopMargin)), 
        % hide if necessary 
        send(AttrGroupNode2, displayed, OnOrOff_Attr), 

        % this box has no use, so should disappear from the code at some point
	send(Node, display, Box2, 
               point(TRX+BX-X, TRY+BY-Y-H+TopMargin)), 
        send(Box2, hide), 

        send(Node, flush),
        send(Node, compute).
%%%% end of changes - AB 25/09/2005




% create_subfigures(Node, Name, Type, N).
%SIHW: WARNING ALSO USED BY STATE_NODE! [those 2 should get a common superclass]
%probably does not do anything for state_node, because depending on type
%
create_subfigures(Node, _Name, entity, N):-
	text_margin(LeftMargin, TopMargin),
	new(AttrGroupNode, graph_node(attributes, attribute_group, N)), 
	send(Node, display, AttrGroupNode, point(LeftMargin, 2*TopMargin)).

create_subfigures(Node, Name, quantity, N):-
	text_margin(LeftMargin, _TopMargin),
	new(QuantitySpaceNode, graph_node(quantity_space, par_relation_group, N)),
	new(ParRelGroupNode, graph_node(par_relations, par_relation_group, N)),
	new(DerGroupNode, graph_node(derivative, derivative_group, N)),
	fill_qspace_node(QuantitySpaceNode, Name, N),
	send(Node, display, ParRelGroupNode, point(0, Node?height)),
	send(Node, display, QuantitySpaceNode, point(2*LeftMargin, Node?height)), %gp3 changed this from 1.5 to 2 
	send(Node, display, DerGroupNode, QuantitySpaceNode?position).


create_subfigures(_Node, _Name, _Type, _N).




fill_qspace_node(QSpaceNode, QuantityName, N):-
	% get the quantity space 
	engine:qspace(QuantityName, Predicate, QSpaceList, _Fail), %gp3, we need the predicate
	%gp3: get the qspace design object
	engine:parameter(Predicate,_,_,_,_,_,QSpaceName),
	(
		QSDef = @app<<-findExportedObject(qs,QSpaceName)
	;
		QSDef = @nil
	),
	reverse(QSpaceList, RevQSpaceList),
	%gp3: we changed this loop, we do not use qspace_member any more
	%because we have to have the type of the value as well
	
	% for every value in qspace, add a node to QSpaceNode
	forall(member(QSValue, RevQSpaceList), 
		(
			(
				QSValue = point(Val), Type = point
			;
				Val = QSValue, Type = interval
			),
				
			strip_atomize(Val, ShortVal),
			
			new(ValNode, qsValue_node(ShortVal,N,?(@app,designName,qsv,ShortVal,QSDef),Type)), %gp3: changed to new class, also show design name
			send(QSpaceNode, display, ValNode, 
						point(0, QSpaceNode?height))
		)
	).

% for general constraint 'dummy' quantities, make an exception.
%
fill_qspace_node(_QSpaceNode, 'constraints', _N).

% for nodes of type entity, set the pen to 1 to draw the bounding box
set_pen(Node, entity):-
	%gp3: Node allready knows its type, so why send it along
	%anyhow: we added check voor ?designType variable (see initialise)
	
	
	text_margin(LeftMargin, _TopMargin),
	send(Node, border, LeftMargin), 
	if
		assumption = Node<<-designType
	then
		Node->>texture(dotted),
	if
		agent = Node<<-designType
	then
		Node->>texture(dashdotted),
	send(Node, pen, 1),!.


% for nodes of type quantity, set the pen to 1 to draw the bounding box
set_pen(Node, quantity):-
	send(Node, border, 2), 
	send(Node, radius, 10), 
	send(Node, pen, 1),!.

set_pen(Node, derivative):-
	send(Node, pen, 0).

set_pen(Node, derivative_group):-
	send(Node, pen, 0).


% for nodes of other types, set the pen to 0 (invisible)
set_pen(Node, _Type):-
	send(Node, pen, 0).

% for nodes of type attribute, make box invisible with black text
set_colour(_Text, Box, attribute):-
	send(Box, pen, 0).

% for nodes of type attribute_group, make box invisible 
set_colour(_Text, Box, attribute_group):-
	send(Box, pen, 0).


set_colour(_Text, _Box, _Type).

%gp3: show_hide_name is not used: gone


% for nodes of type entity, place the text in the top left corner with margins 
position_text(Text, _Box, entity):-
	% the entity figure border already takes care of the margin
%	text_margin(LeftMargin, TopMargin),
%	send(Text, position, point(LeftMargin, TopMargin)). 
	send(Text, position, point(0,0)). 
%	send(Node?text_member, position, point(LeftMargin, TopMargin)). 


% for nodes of type quantity, use the appropriate margins
position_text(Text, _Box, quantity):-
	text_margin_quantity(LeftMargin, TopMargin),
	send(Text, position, point(LeftMargin, TopMargin)).

	
% for nodes of type attribute_group, use the appropriate margins
% hide the text 'attributes'
position_text(Text, _Box, attribute_group):-
	text_margin(LeftMargin, TopMargin),
	send(Text, position, point(LeftMargin, TopMargin)), 
	send(Text, displayed, @off).


% for nodes of type attribute, use no margins and no centering
position_text(Text, _Box, attribute):-
	send(Text, position, point(0, 0)). 


	
% for nodes of type par_relation_group, use only left margin
% hide the text 'par_relations'
position_text(Text, _Box, par_relation_group):-
	text_margin(LeftMargin, _TopMargin),
	send(Text, position, point(LeftMargin, 0)), 
	send(Text, displayed, @off).

	
% for nodes of type derivative_group, use only left margin
% hide the text 
position_text(Text, _Box, derivative_group):-
	text_margin(LeftMargin, _TopMargin),
	send(Text, position, point(LeftMargin, 0)), 
	send(Text, displayed, @off).


% for nodes of type par_relation, use the appropriate margins
position_text(Text, _Box, par_relation):-
	text_margin(LeftMargin, _TopMargin),
	send(Text, position, point(LeftMargin, 0)).

% for nodes of type derivative, use no margins
position_text(Text, _Box, derivative):-
	send(Text, position, point(0, 0)).


%gp3: for nodes of type value this is no longer used
%just succeed. Value nodes are now a different class, see below
position_text(_Text, _Box, value).



% for nodes of type graph_point, center the text in the middle of the node
% and decrease the font size
position_text(T, B, graph_point):-
	send(T, font, font(helvetica, roman, 10)),
	send(T, center, B?center).

% for all other types of node, center the text in the middle of the node
position_text(T, B, _Type):-
	send(T, center, B?center).

expose(Node) :->
	send(Node, send_super, expose), 
	send(Node?box_member, expose), 
	send(Node?text_member, expose).


selected(Node, S:bool) :->
	send_super(Node, selected, S),
	get(Node, type, Type), 
	Type == state,
	(   S == @on
	->  
	    % option 1: fill with red when selected 
	    % get(Node?box_member, fill_pattern, C),
	    % send(Node?box_member, fill_pattern, colour(red)),
	    %
	    % option 2: add a red border when selected 
	    send(Node?box_member, pen, 2),
	    send(Node?box_member, colour, red)
	;   
	    % option 1: 
	    % get(Node?box_member, colour, C),
	    % send(Node?box_member, fill_pattern, C)
	    %
    	    % option 2:
	    % reset border again when deselected 
	    % zero seems to cause an error:
	    %% send(Node?box_member, pen, 0)
	    send(Node?box_member, pen, 1),
	    get(Node?box_member, fill_pattern, C),
	    send(Node?box_member, colour, C)
	).


selected(Node, S:bool) :->
	get(Node, type, Type), 
	Type == termination,
	send_super(Node, selected, S),
	(   S == @on
	->  
	    send(Node, colour, red)
	;   
	    send(Node, colour, black)
	).



selected(Node, S:bool) :->
	send_super(Node, selected, S).




colour(Node, C: colour) :->
	% states should be coloured differently: 
	% fill with colour, but text remains white
	get(Node, type, Type), 
	Type == state,
	send(Node, send_super, colour, C), 
	send(Node?box_member, colour, C), 
	send(Node?box_member, fill_pattern, C). 

colour(Node, C: colour) :->
	send(Node, send_super, colour, C), 
	send(Node?box_member, colour, C), 
	send(Node?text_member, colour, C).

show_hide_attributes(Node, OnOrOff) :->
%%%% begin changes  - AB 25/09/2005
	get(Node, type, Type), 
        get(Node, state_nr, N), 
	Type == 'entity',
	get(Node, member, attributes, AttrGroupNode), 
	send(AttrGroupNode, displayed, OnOrOff),
        % place subfigures of entity
        place_subfigures(Node, _Name, entity, N).
%%%% end of changes - AB 25/09/2005


colour_attribute(Node, Attr, AttrArg, Colour) :->
	get(Node, member, attributes, AttrGroupNode), 
	create_attr_node_string(Node, Attr, AttrArg, Str),
	get(AttrGroupNode, member, Str, AttrNode), 
	send(AttrNode, colour, Colour).
 

create_attr_link_string(Node, Attr, AttrArg, Str):-
	get(string('%s%s%s', Node, Attr, AttrArg), value, Str).


create_attr_node_string(_Node, Attr, AttrArg, Str):-
	get(string('%s: %s', Attr, AttrArg), value, Str).



shape_box(B, bigbox):- 
        send(B, width, 200), 
        send(B, height, 50), 
        get(B, height, H),
        R is H // 2, 
        send(B, radius, R).

shape_box(B, quantity):- 
	% hide the box 
	% the figure will draw its bounding box
	send(B, pen, 0).


shape_box(_B, state).


shape_box(B, entity):- 
	% hide the box 
	% the figure will draw its bounding box
	send(B, pen, 0), 
%	send(B, send_super, pen, 1),
        send(B, width, 50), 
        send(B, height, 20).

shape_box(B, attribute):- 
        send(B, width, 100), 
        send(B, height, 20).


shape_box(B, attribute_group):- 
	% hide the box
        send(B, width, 0), 
        send(B, height, 0).


shape_box(B, par_relation):- 
	% hide the box
	send(B, pen, 0),
        send(B, width, 0), 
        send(B, height, 12).


shape_box(B, derivative):- 
	% hide the box
	send(B, pen, 1),
        send(B, width, 0), 
        send(B, height, 12).


shape_box(B, value):- 
	% hide the box
	send(B, pen, 0),
        send(B, width, 0), 
        send(B, height, 12).


shape_box(B, par_relation_group):- 
        % this caused the little black dots in generated eps-file, AB - 18/07/2006
        send(B, destroy).



shape_box(B, derivative_group):- 
        % this caused the little black dots in generated eps-file, AB - 18/07/2006
        send(B, destroy).


shape_box(B, graph_point):- 
	send(B, fill_pattern, colour(white)), 
        % send(B, width, 17), 
        % send(B, height, 17),
        send(B, width, 13), 
        send(B, height, 13),
	send(B, radius, B?height).

    

shape_box(B, _DefaultType):-
        send(B, width, 80), 
        send(B, height, 80).


label(Node, Lbl:name) :->
	"Change label of a node"::
	%gp3 0.1: this was called name, now we call it label, because thats what is does
	get(Node, member, text, Text),
	get(Node, type, Type),
	send(Text, string, Lbl),
	% resize box and position text again
	get(Node, member, box, Box), 
	text_margin(LeftMargin, _TopMargin), 
	send(Box, width, Text?width+2*LeftMargin),
	position_text(Text, Box, Type). 

                   
testname(Gr, Name:name) :->
        send(Gr, name(Name)).


% resize entity nodes
%
resize(Node, XFactor, YFactor, Origin) :->
	get(Node, type, Type), 
	Type == entity,
        send(Node, send_super, resize, XFactor, YFactor, Origin).


% resize state nodes
%
resize(Node, XFactor, YFactor, Origin) :->
	get(Node, type, Type), 
	Type == state,
        send(Node, send_super, resize, XFactor, YFactor, Origin).



% don't resize quantity nodes, only move them
%                   
resize(Node, XFactor, YFactor, Origin:point) :->
% resize(Node, XFactor, YFactor, point(X0,Y0)) :->
	get(Node, type, Type), 
	Type \== entity,
	% Type == quantity,
        get(Node, position, point(X,Y)),
        % get(Node, name, Name),
        get(Origin, x, X0), 
        get(Origin, y, Y0), 
        DiffX is X - X0,
        DiffY is Y - Y0,
        NewX is X0 + XFactor * DiffX,
        NewY is Y0 + YFactor * DiffY,
        send(Node, position, point(NewX,NewY)).


% don't resize all other types of nodes
%
resize(Node, _XFactor, _YFactor, _Origin) :->
	get(Node, type, Type), 
	Type \== quantity,
        true.


reset_selection(P):-
	% reset selection at the picture top-level
	send(P, selection, @nil), 
	% reset selection for every graphical (which may be nested)
	\+ get(P, find, @default, 
		and(
			@arg1?selected == @on, 
			message(@arg1, selected, @off),
			new(or)
		), _SelectedGr).



% reset_tag_has_been_moved(FromNode):->
% 
% reset the tag_has_been_moved variable to @off for all my_tagged_connections
% which go out from the quantity node FromNode
% 
reset_tag_has_been_moved(FromNode):->
	    % reset tag_has_been_moved for my_tagged_connection where necessary
	    (catch(get(FromNode, connections, AllConnections), _, fail)
	     ->
               get(AllConnections, find_all, 
		   and(message(@arg1, instance_of, my_tagged_connection), 
		       @arg1?type == 'par_relation', 
		       @arg1?tag_has_been_moved == @on 
		   ), ConnectionsChain), 
	       send(ConnectionsChain, for_all, 
		    message(@arg1, tag_has_been_moved, @off)
	       )
             ;
               % do nothing
	       true
	     ).

click_graphical(Gr) :-
	get(Gr, type, Type), 
	Type == 'quantity',
        get(Gr, name, Name), 
        get(Gr, state_nr, N), 
	show_quantity_details(Name, N).



click_graphical(Gr) :-
	get(Gr, type, Type), 
	Type == 'state',
        get(Gr, state_nr, N), 
	show_quantity_values(Gr?frame,N).


click_graphical(Gr) :-
	%gp3 0.2 Do nothing for entity (we have tooltips)
	%but succeed, to make sure no side effects (like text selection) occur
	get(Gr, type, Type), 
	Type == 'entity'.

click_graphical(Gr) :-
	get(Gr, type, Type), 
	Type == 'termination',
        get(Gr, name, Name), 
	show_transition(Name).


click_graphical2(Gr) :-
        % Select quantity
	get(Gr, type, Type), 
	Type == 'quantity',
	% do nothing
	true.




click_graphical2(Gr) :-
        % Select state and change state nr.
	get(Gr, type, Type), 
	Type == 'state',
        get(Gr, state_nr, N), 
	get(Gr, frame, F), 
%        send(F, change_state_nr, N),
	get(F, textdialog_member, D), 
	term_to_atom([N], Str),
	send(D?selected_states_member, selection, Str),
	Sel = F<<-selectMode, %gp3

	% if Sel == 'path', find and select path
	(Sel == 'path'
	-> 
		select_path(F)
	; 
		select_states_transitions(F, [N]),
		select_states_terminations(F, [N])
	),
	update_selection_nr(Gr).

click_graphical2(Gr) :-
        % Select entity 
	get(Gr, type, Type), 
        get(Gr, state_nr, N), 
	Type == 'entity',
        place_subfigures(Gr, _Name, entity, N),
        send(Gr, selected, @on),
	% do nothing.
	true.


click_graphical2(Gr) :-
        % Select termination 
	get(Gr, type, Type), 
	Type == 'termination',
	get(Gr, state_nr, N), 
	get(Gr, frame, F), 
	get(F, textdialog_member, D), 
	term_to_atom([N], Str),
	send(D?selected_states_member, selection, Str).


% click_graphical3(Gr) 
%
% change the colour of graphical elements, or destroy them.
% this functionality is useful when you have to draw many pictures
% and need to edit the figure slightly 
% However, this feature is by default turned off

click_graphical3(Gr) :-
	% get(Gr, type, Type), 
	% Type \== 'state',
        new(PopupMenu, popup(popupmenu)), 
        send_list(PopupMenu, append,
		[	
			menu_item('black', 
				  message(Gr, colour, black)),
                        menu_item('blue', 
				  message(Gr, colour, blue)),
			menu_item('red',
				  message(Gr, colour, red)), 
			menu_item('orange',
				  message(Gr, colour, orange)), 
			menu_item('grey',
				  message(Gr, colour, grey)), 
			menu_item('white',
				  message(Gr, colour, white)),
			menu_item(destroy,
				  message(Gr, destroy))
	 	]),
        send(Gr, popup, PopupMenu).

shift_click_graphical(Gr) :-
	get(Gr, type, Type), 
	Type == 'state',
	update_selection_nr(Gr),
	get(Gr, frame, F),
        send(F, update_selected_states, Gr),
        Sel = F<<-selectMode, %gp3
	% if Sel == 'path', find and select path
	(Sel == 'path', !, 
		select_path(F)
	; 
		send(F, select_states)
	).

event(Node, Ev:event) :->
       get(Ev, name, EvName), 
       debug(simulate(recogniser), 'event ~w ~w for graph_node ~w', [Ev, EvName, Node]), 
	(   send(Node, send_super, event, Ev)
	;   send(@graph_node_recogniser, event, Ev)
	).



:- pce_end_class.
% Class par_rel_node 
%
% subclass of graph_node with an extra variable which is the text
% visible on the screen (which can be different from the name)
%

:- pce_begin_class(par_rel_node(name, name, int, name, name), graph_node).


variable(associated_value, name:=unknown, both, "The name of the associated value").

%variable(visual_label, name:=unknown, both, "The visual label - different from the node name").


initialise(Node, Name:name, Type:name, StateNr:int, Value:name, Label:name) :->
	"Create from name"::
	send(Node, send_super, initialise, Name, Type, StateNr),
	get(Node, member, text, Text),
	send(Text, string, Label),
        send(Node, associated_value, Value).

:- pce_end_class.



% Class derivative_node 
%
% subclass of graph_node with an extra variable which is the 
% name for the symbol to be used
%

:- pce_begin_class(derivative_node(name, name, int, name, name), graph_node).


variable(associated_value, name:=unknown, both, "The name of the associated value").

%variable(visual_label, name:=unknown, both, "The visual label - different from the node name").


initialise(Node, Name:name, Type:name, StateNr:int, Value:name, Label:name) :->
	"Create from name"::
	send(Node, send_super, initialise, Name, Type, StateNr),
	get(Node, member, text, Text),
	send(Text, string, Label),
        send(Node, associated_value, Value).

:- pce_end_class.

:- pce_begin_class(qsValue_node, graph_node).
%gp3 0.2: because we need to change the visualisation of this type a bit
%we create a subclass now. Better than going on patching stuff

variable(valueType, {point,interval}, both).

initialise(Node, Name:name, StateNr:int,
		Label: label = name, ValueType: valueType = {point,interval}) :->
		
	%initialise it
	Node->+initialise(Name,value,StateNr,Label), %type argument still needed
	%now, we have to create a bitmap and place the text
	%position text is no longer used (it just succeeeds, see above).
	
	%there is allready a lot displayed at this stage: the text, a not visible box
	%but we reorder and display a bitmap
	
	Node->>valueType(ValueType),
	get_image(elements,ValueType,Image),
	Bitmap *= psBitmap(Image), %gp3 0.3.13: changed to psBitmap
	Bitmap->>name(bitmap),
	Bitmap->>psdef(ValueType),
	Node->>display(Bitmap, point(0,0)),
	
	%reorder:
	Text = Node<<-text_member, 
	BH = Bitmap<<-height,
	TH = Text<<-height,
	
	Text->>set(x := Bitmap?right_side + 1),
	if
		BH > TH
	then
		Text->>set(y:=Bitmap?center_y - (TH / 2))
	else
	(
		Text->>set(y:=0),
		Bitmap->>set(y:=Text?center_y - (BH / 2))
	).
	
%%

colour(Node, C: colour):->
	%gp3 0.2 We overrule colour to make sure all elements are coloured
	
	Node->+colour(C),
	%for the type bitmap, we only have black (default), blue, red and light_grey
	%this definitely is a dirty fix, but we have to use it because
	%of the non-oo way the rest of the code is written in
	
	CN = C<<-name,
	
	%get the bitmap colour:
	(
		CN = red, BC = '_red'
	;
		CN = light_grey, BC = '_grey'
	;
		CN = blue, BC = '_blue'
	;
		BC = ''
	),
	Img *= string('%s%s',Node?valueType,BC), %point_red etc
	get_image(elements,Img,Image),
	Node?bitmap_member->>image(Image),
	%gp3 0.3.13: change ps def too
	Node?bitmap_member->>psdef(Img).
%%
:-pce_end_class.
