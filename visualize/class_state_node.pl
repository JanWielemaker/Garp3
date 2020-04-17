/*****************************************************************************************************
Garp3 0.1 Based on main source of visigarp (main.pl). This file is split in parts and edited.
All visiualize source based on visigarp 2.05, designed and implemented by Anders Bouwer 18/08/1999
Original copyright: (C) 2004 University of Amsterdam. All rights reserved.

Now:
PART OF Garp3. SEE COPYRIGHT NOTICE.
*******************************************************************************************************/

%loaded into module (namespace) visualize
% Class state_node
%
%
%
%
%Garp3 changed this: we now use an image instead of a circle
%and we added a subclass state_termination_node
:- pce_begin_class(state_node(name, name, int), figure).


variable(state_nr, int:=0, both, "The state nr. associated with this node").
% the type variable may not be necessary anymore!

%gp3: this is either state or termination

variable(type, name:=unknown, both).
variable(selection_nr, int:=0, both, "The selection nr. associated with this node").
variable(t_status, name:=unknown, both, "The status of the state: unknown, unprocessed, terminated, ordered, closed").

% for every handle point, there are two handles: 
% one for incoming links, and one for outgoing.
%
% if the bottom one is deleted for every point (except maybe for 
% the top and bottom handles), the handles are consistent with 
% an upward (actually, rightward!) graph

% top 
handle(w/2, 0, in, link).
handle(w/2, 0, out, link).

% bottom
handle(w/2, h, out, link).
handle(w/2, h, in, link).

% left
handle(0, h/2, out).
handle(0, h/2, in).

% right
handle(w, h/2, in).
handle(w, h/2, out).

% extra handles at every quarter

% left top quarter
handle(0.1464*w, 0.1464*h, out).
handle(0.1464*w, 0.1464*h, in).

% left bottom quarter
handle(0.1464*w, 0.8536*h, out).
handle(0.1464*w, 0.8536*h, in).

% right top quarter 
handle(0.8536*w, 0.1464*h, in).
handle(0.8536*w, 0.1464*h, out).

% right bottom quarter 
handle(0.8536*w, 0.8536*h, in).
handle(0.8536*w, 0.8536*h, out).


initialise(Node, Name:name, Type:name, StateNr:int) :->
	"Create from name"::
	send(Node, send_super, initialise),
        send(Node, type, Type),
	%state_node_size(Diameter),
	%gp3: we create an image instead of a circle
	
	Bmp *= psBitmap(Node?stateImage), %gp3: new call to get the right one,
										%0.3: using new class psBitmap
	Bmp->>name(bmp),
	Bmp->>psdef(Node?stateName), %setting the postscript definition file for this bitmap, used for exporting postscript (see psBitmap)
	Node->>display(Bmp),

	%send(Node, display, new(C, circle(Diameter)), point(0, 0)),
	%gp3: I think this might be old code from when this was also entity node... But not sure. Maybe this can/should go?
	send(Node, border, 0),
 	send(Node, pen, 0), %gp3: from set_pen
	create_subfigures(Node, Name, Type, StateNr), 
	send(Node, display, new(T, text(Name))),
	send(T, colour, colour(white)),  %gp3 copied from set_colour(state), set_colour is no longer with us, for state and (named_)termination
	send(T, font, bold),
    send(Node, state_nr, StateNr),
	send(Node, send_super, name, Name),
	position_text(T, Bmp, Type),
	Node->>updateDisplay.
%%

%%
stateImage(Node,Img: image):<-
	%gp3. New general call to get the right image for the state
	
	Name = Node<<-stateName,
	get_image(simulate,Name,Img).
%%

%%
stateName(Node, Name: name):<-
	%gp3 0.3: get the name for the state image and psdef (same name)
	
	if
		@on = Node<<-selected
	then
		Sel = '_selected'
	else
		Sel = '',
		
	%state 0 (input is special)
	if
		0 = Node<<-state_nr
	then
		Name *= string('inputstate%s',Sel)
	else
	(
		S = Node<<-t_status,
		%specials: unknown and interpreted
		if
			member(S,[unknown,interpreted])
		then
			Status = unprocessed
		else
			Status = S,
		Name *= string('state_%s%s',Status,Sel)
	).
%%

%%
updateDisplay(Node):->
	%gp3: replaces ->colour, update the displayed icon
	%gp3 0.3: also resets the bitmaps psdef
	
	Node?bmp_member->>image(Node?stateImage),
	Node?bmp_member->>psdef(Node?stateName),
	%input state is special
	if
		0 = Node<<-state_nr
	then
		Node?text_member->>displayed(@off).
%%
	
% network normally returns a chain with all connected graphicals
% this version only returns the graphicals directly connected
%
network(Node, Chain:chain) :<-
	new(Result, chain),
	send(Result, append, Node),
	% Jan Wielemakers tips and tricks: repeat by failure
	%gp3: this is probably only used on the main windows, so we have to 
	%get the client now, instead of the picture_member
	%get(Node?frame, picture_member, P), 
	P = Node?frame<<-client,
        \+ get(P, find, @default,
        	and(
                 	message(@arg1, instance_of, connection), 
			@arg1?from == Node,
			@arg1?to?type == termination,
			not(message(Result, member, @arg1?to)),
                 	message(Result, append, @arg1?to),
			new(or)
		), _ConnectedGr
	      ),
	Chain = Result.

%gp3: show_hide_name is not used: gone

% for all other types of node, center the text in the middle of the node
position_text(T, C, _Type):-
	send(T, center, C?center).

expose(Node) :->
	send(Node, send_super, expose), 
	send(Node?box_member, expose), 
	send(Node?text_member, expose).

selected(Node, S:bool) :->
	%gp3 rewrite. We use different images, so redraw
	
	send_super(Node, selected, S),
	Node->>updateDisplay.

name(Node, Name:name) :->
	"Change name of a node"::
	%gp3: made some changes: we can no longer change size of the icon
	%because its an image now
	get(Node, member, text, Text),
	get(Node, type, Type),
	send(Text, string, Name),
	Bmp = Node<<-bmp_member,
	position_text(Text, Bmp, Type). 

                   


click_graphical(Gr) :-
	get(Gr, type, Type), 
	Type == 'state',
        get(Gr, state_nr, N), 
	show_quantity_values(Gr?frame,N).

click_graphical1(Gr) :-
	get(Gr, type, Type), 
	Type == 'state',
        get(Gr, state_nr, N), 
	get(Gr, frame, F), 
	send(F, gen_model_fragments, N).



click_graphical2(Gr) :-
        % Select state and change state nr.
	get(Gr, type, Type), 
	Type == 'state',
        get(Gr, state_nr, N), 
	get(Gr, frame, F), 
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



shift_click_graphical(Gr) :-
	get(Gr, type, Type), 
	Type == 'state',
	update_selection_nr(Gr),
	get(Gr, frame, F),
        send(F, update_selected_states, Gr),
	Sel = F<<-selectMode,
	% if Sel == 'path', find and select path
	(Sel == 'path', !, 
		select_path(F)
	; 
		% if Sel \== 'path'
		get(F, textdialog_member, D2), 
		get(D2?selected_states_member, selection, StateListStr), 
		str_to_statelist(StateListStr, StateList), 
		select_states_transitions(F, StateList),
		select_states_terminations(F, StateList)
	).


event(Node, Ev:event) :->
	(   send(Node, send_super, event, Ev)
	;   send(@graph_node_recogniser, event, Ev)
	).


:- pce_end_class.

%garp3: new class state_termination_node

:- pce_begin_class(state_termination_node(name, name, int), state_node).

%we do some stuff different. And hopefully in the future
%all those prolog clauses using type are gone, and everything is
%a method...

%%
initialise(Node, Name:name, StateNr:int) :->
	%gp3: our type is allways termination
	%my guess is that the type of our parent is allways
	%state, but visigarp code still keeps the door open for different
	%stuff?
	
	Node->+initialise(Name,termination,StateNr),
	Node?text_member->>displayed(@off),
	Node->>tooltip(string('Termination %s', Name),model).
%%

%%
stateName(Node,Name: name):<-
	%gp3. New general call to get the right name for the state
	%image and psdef
	%termination nodes only have one image
	
	if
		@on = Node<<-selected
	then
		Name = 'termination_selected'
	else
		Name = 'termination'.
%%


:- pce_end_class.