/*****************************************************************************************************
Garp3 0.1 Based on main source of visigarp (main.pl). This file is split in parts and edited.
All visiualize source based on visigarp 2.05, designed and implemented by Anders Bouwer 18/08/1999
Original copyright: (C) 2004 University of Amsterdam. All rights reserved.

Now:
PART OF Garp3. SEE COPYRIGHT NOTICE.
*******************************************************************************************************/

%loaded into module (namespace) visualize
% Class state_master_node
%
% this type of node comprises one core state_node plus its 
% attached termination nodes. Other than that, it does nothing.
%

:- pce_begin_class(state_master_node(name, name, int), figure).

variable(state_nr, int:=0, both, "The state nr. associated with this node").

initialise(Node, Name:name, Type:name, StateNr:int) :->
	"Create from name"::
	send(Node, send_super, initialise),
	send(Node, border, 0),
	create_state_subfigures(Node, Name, Type, StateNr), 
        send(Node, state_nr, StateNr),
	send(Node, send_super, name, Name),
	send(Node, pen, 0),!.

create_state_subfigures(Node, Name, Type, StateNr):-
	new(StateNode, state_node(Name, Type, StateNr)), 
	send(Node, display, StateNode, point(0,0)).
	


:- pce_global(@state_master_node_recogniser, make_state_master_node_recogniser).

make_state_master_node_recogniser(R) :-
       % make state node movable 
       debug(simulate(recogniser), 'make state master node recogniser...', []),
       new(R, handler_group(
                             % make node movable
                             new(G, move_gesture(left))
			    )
       ),

       debug(simulate(recogniser), 'limit state master node recogniser...', []),
       send(G, send_method,
			send_method(verify, vector(event),
				and(
				 % in the case of table view, this fails, and the recogniser will fail
				 message(?(@app, setting, simulation_view_type), equal, 'graph')
				)
			)
       ).			



event(Node, Ev:event) :->
       get(Ev, name, EvName), 
       get(@app, setting, simulation_view_type, ViewType), 
       debug(simulate(recogniser), 'Event in ~w view: ~w', [ViewType, EvName]),

       % ignore move event in table view is achieved by checking in the recogniser for the move_gesture
       % if (ViewType == 'table', EvName == ms_left_drag) % I thought this could work, but I didn't manage, AB, May 07
       (
            % Check if the event can be recognised by the recogniser above
	    send(@state_master_node_recogniser, event, Ev),
            debug(simulate(recogniser), 'event ~w recognized by state master node. Now also send to super...', [EvName]), 
	    send(Node, send_super, event, Ev) 
	;
            debug(simulate(recogniser), 'event ~w not recognised by state master node. Only send event to super', [EvName]),
	    send(Node, send_super, event, Ev)
       ).


:- pce_end_class.
