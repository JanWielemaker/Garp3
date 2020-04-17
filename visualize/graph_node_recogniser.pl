/*****************************************************************************************************
Garp3 0.1 Based on main source of visigarp (main.pl). This file is split in parts and edited.
All visiualize source based on visigarp 2.05/2.06, designed and implemented by Anders Bouwer 18/08/1999
Original copyright: (C) 2004 University of Amsterdam. All rights reserved.

Now:
PART OF Garp3. SEE COPYRIGHT NOTICE.
*******************************************************************************************************/

/*
gp0.1 sihw 20050506
@graph_node_recogniser global was twice defined in original main.pl. We define it once, here, and use
only the first definition (the one that was still in visigarp 2.06)
*/

%loaded into module (namespace) visualize

:- pce_global(@graph_node_recogniser, make_graph_node_recogniser).


make_graph_node_recogniser(R) :-
	new(R, handler_group(
                             % double left click to investigate  
                             new(click_gesture(left, '', double, 
	                     	message(@prolog, click_graphical, @receiver))),
                             % shift click left button to toggle selection
                             new(click_gesture(left, 's', single,
				and(
					message(@event?receiver, toggle_selected),
					message(@prolog, shift_click_graphical, @receiver)
				))),

			     % make node resizable
			     % this doesn't work yet! why not?
			     % new(resize_gesture(left)),

                             % single left click to select
                             new(click_gesture(left, '', single, 
	                      and(
	            %gp3: make sure this works for old style visigarp windows
	            %and garp3 framedWindow objects
 				message(@prolog, reset_selection, 
 					when(->>(@event?receiver?frame,instance_of,framedWindow),
 						@event?receiver?frame?client,
				 		@event?receiver?frame?picture_member)),
				message(@event?receiver?device,
						selection, @event?receiver), 
				message(@prolog, click_graphical2, @receiver)))),


                             % click right button 
                             % hidden functionality for expert users. 
                             % new(click_gesture(right, '', single, 
	                     %	message(@prolog, click_graphical3, @receiver))),

                             % make node movable, except for graph_points
                             new(G, move_gesture(left))
                            )
           ),

	% make certain types of node immovable
	% when the resize gesture(left) is turned on, it's still 
	% possible to move them, by dragging their corners!
	send(G, send_method,
			send_method(verify, vector(event),
				and(@arg1?receiver?type \== 'graph_point',
				 @arg1?receiver?type \== 'value',
				 @arg1?receiver?type \== 'state',
				 % @arg1?receiver?type \== 'state_master',
				 % @arg1?receiver?type \== 'state_master_node',
				 @arg1?receiver?type \== 'attribute',
				 @arg1?receiver?type \== 'par_relation',
				 @arg1?receiver?type \== 'attribute_group',
				 @arg1?receiver?type \== 'derivative',
				 @arg1?receiver?type \== 'derivative_group',
				 @arg1?receiver?type \== 'par_relation_group'
				 % to do: prevent movable states in table view 
% 				 @app<<-setting(simulation_view_type),
				)
			)
	).			


