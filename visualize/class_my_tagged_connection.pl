/*****************************************************************************************************
Garp3 0.1 Based on main source of visigarp (main.pl). This file is split in parts and edited.
All visiualize source based on visigarp 2.05, designed and implemented by Anders Bouwer 18/08/1999
Original copyright: (C) 2004 University of Amsterdam. All rights reserved.
Last updated: 08/05/2006, AB
Now:
PART OF Garp3. SEE COPYRIGHT NOTICE.
*******************************************************************************************************/

%loaded into module (namespace) visualize
:- pce_begin_class(my_tagged_connection(name, node, node, link, name, name, name), 
							movable_tagged_connection).

variable(type, name:=unknown, both, "The type of connection: par_relation, or attribute").

variable(t_status, name:=unknown, both, "The termination status: terminated, ordered, or closed").

variable(arrow_direction, name:=second, both, "The arrow direction(s): none, first, or second").


initialise(MyTC, Name:name, FromNode:node, ToNode:node, LName:link, Type:name, Label:name, Dir:name) :->
	"Create from name"::
	send(MyTC, send_super, initialise, FromNode, ToNode, LName),
        send(MyTC, type, Type),
        send(MyTC, t_status, Label), 
        	%gp3 note: I really havent a clue why this would be set to Label (which originally is just the relation name)
	( Type == 'layout'
	->
		send(MyTC, displayed, @off),
		send(MyTC, pen, 0)
	;
		true
	),
	send(MyTC, send_super, arrows, Dir),
	send(MyTC, send_super, name, Name).



selected(MyTC, S:bool) :->
        get(MyTC, type, T), 
        % this is only necessary for tagged connections of type termination 
        T == 'termination', 
	send_super(MyTC, selected, S),
	(   S == @on
	->  
	    send(MyTC, colour, red)
	;   
	    send(MyTC, colour, black)
	).



colour(MyTC, C: colour) :->
	send(MyTC, send_super, colour, C), 
	send(MyTC, expose),
	(   catch(get(MyTC, tag, Tag), _, fail)
	->  % if MyTC has a Tag, then colour it
	    (	catch(get(Tag, graphicals, Grs), _, fail)
		-> 	% if Tag has graphicals, then colour them too
			send(Grs, for_all, message(@arg1, colour, C)),
			send(Tag, expose)
		;
		   	% if Tag has no graphicals, then nothing
			true
	    )
	;   % if MyTC has no tag, then nothing
	    true
	).





%%	
setElementState(MyTC, State: name):->
	%gp3 0.2 See colour_dependency_in_ent etc (highlight, show_model_fragment_details)
	%when called, we try and set the elementState for the icon
	%which means c=condition, g=given, i=imported (no condition or given here)
	%this is added to dependency_icon class, it gives the icon a different bitmap
	
	if
	(
		catch(Tag = MyTC<<-tag,_,fail),
		Tag->>instance_of(dependency_icon)
	)
	then
		Tag->>elementState(State).
%%
		
colour_line(MyTC, C: colour) :->
        % Only colour the connection, not the tag
	send(MyTC, send_super, colour, C),
	send(MyTC, hide).
        
hide_line(MyTC) :->
	% send(MyTC, displayed, @off),
	% send(MyTC, send_super, arrows, none),
	send(MyTC, arrows, none),
	send(MyTC, pen, 0),   
	send(MyTC, hide).

points(TC, X1:[int], Y1:[int], X2:[int], Y2:[int]) :->
        get(TC, type, Type), 
        Type \== 'par_relation',
        send(TC, send_super, points, X1, Y1, X2, Y2).
        


points(TC, X1:[int], Y1:[int], X2:[int], Y2:[int]) :->
        get(TC, type, Type), 
        Type == 'par_relation',
        send(TC, send_super, points, X1, Y1, X2, Y2),
        get(TC, from, FromNode), 
        get(TC, to, ToNode), 
        (catch(get(FromNode, connections, ToNode, AllConnections), _, fail)
        ->
             get(AllConnections, find_all, 
                and(@arg1?type == 'par_relation', 
                    @arg1 \== TC
                   ), ConnectionsChain), 
	     chain_list(ConnectionsChain, ConnectionsList)
        ;
             ConnectionsList = []
        ),        
        forall(member(TC2, ConnectionsList), 
               (   
                   % if TC2 has a tag, and both tags 
                   % if they are exactly at the same position
                   get(TC?tag?position, x, XX1), 
                   get(TC2?tag?position, x, XX2), 
                   get(TC?tag?position, y, YY1), 
                   get(TC2?tag?position, y, YY2), 
                   % ((XX1 == XX2, YY1 == YY2)
                   Xdiff is abs(XX1 - XX2),
                   Ydiff is abs(YY1 - YY2),
                   ((Xdiff < 3, Ydiff < 3)
                   ->
                        % The tags of TC and TC2 largely overlap
                        send(TC, move_tag, TC2),

                        % send connection to background
                        send(TC, hide), 
                        send(TC, hide, TC?tag), 
                        send(TC, hide, TC2?tag), 
                        % put tag of TC2 on top
                        % send(TC2?tag, expose),
                        % send the tag to just behind tag of TC2
                        send(TC?tag, hide, TC2?tag)
                   ;
                        % put tags on top
                        send(TC2, hide, TC2?tag),
                        send(TC, hide, TC?tag),
                        send(TC2?tag, put_on_top),
                        send(TC?tag, put_on_top)
                   )
               )
        ).

       
 
% move_tag(TC, TC2) moves the tag of TC along the vector of TC
%
move_tag(TC, TC2) :->
        get(TC, tag, Tag), 
        get(TC2, tag, _Tag2), 
        get(TC, start_x, XX1), 
        get(TC, start_y, YY1), 
        get(TC, end_x, XX2), 
        get(TC, end_y, YY2), 
        Xdiff is (XX1 - XX2),
        Ydiff is (YY1 - YY2),
        Zsq is (Xdiff*Xdiff)+(Ydiff*Ydiff),
        Z is sqrt( abs(Zsq) ),
	calculate_division(Ydiff, Z, Sin), 
	calculate_division(Xdiff, Z, Cos), 
        % calculate necessary translation TX,TY
        get(Tag, width, Diameter), 
        TX is (Cos*Diameter),
        TY is (Sin*Diameter),
        send(Tag, position, point(Tag?x+TX, Tag?y+TY)).

% don't resize tagged connections, only move them
%                   
resize(_MyTC, _XFactor, _YFactor, _Origin:point) :->
	% don't do anything
        % get(MyTC, name, Name),
        true.


:- pce_end_class.
