/*****************************************************************************************************
Garp3 0.1 Based on main source of visigarp (main.pl). This file is split in parts and edited.
All visiualize source based on visigarp 2.05, designed and implemented by Anders Bouwer 18/08/1999
Original copyright: (C) 2004 University of Amsterdam. All rights reserved.

Now:
PART OF Garp3. SEE COPYRIGHT NOTICE.
*******************************************************************************************************/

%loaded into module (namespace) visualize

:- pce_begin_class(my_connection(node, node, link, handle, handle), connection).

initialise(MyC, FromNode:node, ToNode:node, LName:link) :->
	"Create from name"::
	send(MyC, send_super, initialise, FromNode, ToNode, LName).


selected(MyC, S:bool) :->
	send_super(MyC, selected, S),
	(   S == @on
	->  
	    send(MyC, colour, red)
	;   
	    send(MyC, colour, black)
	).


:- pce_end_class.

%
% non_resizable_device was created to ensure that the zoom-function
% does not scale certain devices.
%
:- pce_begin_class(non_resizable_device, device).

initialise(NRD) :->
	"Create from name"::
	send(NRD, send_super, initialise).


% don't resize quantity nodes, only move them
%                   
resize(_NRD, _XFactor, _YFactor, _Origin) :->
        true.

:- pce_end_class.


% Class movable_tagged_connection
% 
% Exactly the same as pce_tagged_connection, except for points method.
% redefine points method to avoid recentering tag after it has been moved
%

:- pce_begin_class(movable_tagged_connection, connection,
                   "Connection with centered tag").

variable(tag,   graphical*,     get, "Associated tag").
variable(tag_has_been_moved, bool:=(@off), both, "Whether the tag has been moved by the user or not").


tag(C, Tag:graphical*) :->
        "Associate (new) tag with the connection"::
        get(C, tag, Old),
        (   Old == Tag
        ->  true
        ;   (   Old \== @nil
            ->  send(Old, free)
            ;   true
            ),
            send(C, slot, tag, Tag),
            update_tag(C, _All)
        ).


unlink(C) :->
        "Destroy tags"::
        send(C, send_super, unlink),
        get(C, tag, Tag),
        (   Tag \== @nil
        ->  free(Tag)
        ;   true
        ).

device(C, Dev:device*) :->
        "Update the tag"::
        send(C, send_super, device, Dev),
        update_tag(C, device).

displayed(C, Val:bool) :->
        "Update the tag"::
        send(C, send_super, displayed, Val),
        update_tag(C, displayed).

points(C, X1:[int], Y1:[int], X2:[int], Y2:[int]) :->
        "Update the tag"::
        send(C, send_super, points, X1, Y1, X2, Y2),
        % redraw tag only if the tag has not been moved
	get(C, tag_has_been_moved, Bool), 
        (
	    Bool == @off
            -> 
	       update_tag(C, center)
	    ;                  
               % just put the tag on top
               get(C, tag, Tag), 
	       send(Tag, expose),
	       send(Tag, redraw),
               send(Tag, hide_connections)               
        ).


tag_attribute(center).
tag_attribute(device).
tag_attribute(displayed).

update_tag(C, _) :-
        get(C, tag, @nil), !.
update_tag(C, What) :-
        get(C, tag, Tag),
        forall(tag_attribute(What), send(Tag, What, C?What)).

:- pce_end_class.




% Class my_move_gesture
%
% specialization of move_gesture
%
:- pce_begin_class(my_move_gesture(button_name, modifier), move_gesture).

initialise(MG, Button, Modifier) :->
	"Create from name"::
	send(MG, send_super, initialise, Button, Modifier).



terminate(MG, Event) :->
	send(MG, send_super, terminate, Event),
        get(Event, receiver, Tag),
        get(Tag, connection, TC), 
        get(TC, from, FromNode), 
        get(TC, to, ToNode), 
        
	% do these connections exist already?
	(   catch(get(Tag, connections, _TagConnections), _, fail)
	->  
            % hide Connections behind Tag
            send(Tag, hide_connections)
	;
%%%% changed to deal with bidirectional arrows too

         % TC is hidden and replaced by two separate parts: TC1 and TC2
         % TC1 gets a first arrow, if the original TC had a first arrow. 
         % TC2 gets a second arrow, if the original TC had a second arrow.

         % line/arrow for first part
         new(TC1, connection(FromNode, Tag, @graph_link)),
         % show only first arrow
         send(TC1, arrows, first), 
         
         % line/arrow for second part
         new(TC2, connection(Tag, ToNode, @graph_link)),

         % copy arrow style for first part from original connection
         get(TC, first_arrow, Arrow1), 
         send(TC1, first_arrow, Arrow1), 

         % copy arrow style for second part from original connection
         get(TC, second_arrow, Arrow2), 
         send(TC2, second_arrow, Arrow2), 

%%%% begin changes  - AB 25/09/2005
         % copy colour from original connection
         get(TC, colour, Colour), 
         send(TC1, colour, Colour), 
         send(TC2, colour, Colour), 
%%%% end of changes - AB 25/09/2005

         send(TC1, displayed, @on), 
         send(TC2, displayed, @on),

         send(TC1, hide, Tag), 
         send(TC2, hide, Tag)

	),        

        % hide the original connection
        send(TC, hide_line), 

        % register that the tag has been moved
        send(TC, tag_has_been_moved, @on),
        
        % put the tag on top
	send(Tag, put_on_top).

:- pce_end_class.






:- pce_begin_class(my_frame, frame).

initialise(F) :->
	"Create a frame with postscript method"::
	send(F, send_super, initialise, '').


postscript(F) :->
	"Create PostScript in file"::
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

:- pce_end_class.
