/*****************************************************************************************************
Garp3 0.1 Based on main source of visigarp (main.pl). This file is split in parts and edited.
All visiualize source based on visigarp 2.05, designed and implemented by Anders Bouwer 18/08/1999
Original copyright: (C) 2004 University of Amsterdam. All rights reserved.

Now:
PART OF Garp3. SEE COPYRIGHT NOTICE.
*******************************************************************************************************/

%loaded into module (namespace) visualize
  
%resize_picture(GW):-  %this call is killed by gp3 0.3.13 (this line is here for ABs sake). See comments in class_visigarp
	

% scale_graphical(Gr, F)
%
% scales a graphical, including its subgraphicals, 
% to a factor F.
% 0.5 means scale down to half its size
% 1 means keep as it is
% 2 means double its size
%
scale_graphical(Gr, F):-
%        send(Gr, resize, F, F).
	% send(Gr, has_get_method, type), 
	% get(Gr, type, Type), 
        % Type == 'entity',
        send(Gr, resize, F, F, point(0,0)).

% automatic_zoom(F)
%
% zooms picture automatically as much as necessary to 
% fit everything into the window
%
automatic_zoom(GW):-
	%gp3 0.2: keep this general, so added possibility of client member
  
	if
		GW->>instance_of(framedWindow)
	then
		P = GW<<-client
	else
		P = GW<<-member(picture),
		
	text_margin(LeftMargin, TopMargin), 
	get(P, bounding_box, area(_X, _Y, W, H)),
        % get(P, graphicals, AllGrsChain),
        get(P, visible, area(_VX, _VY, VW, VH)), 
        (catch((W < VW, H < VH), _, fail)
        ->  
          % everything fits
          % all that's necessary now is normalise 
          % (adjusting scrollbars to fit)
          normalise_margin(P, LeftMargin, TopMargin)
          % send(P, normalise, AllGrsChain)
        ;
          % not everything fits on the picture
          
	  calculate_zoom_factor(VW, W, XFactor), 
          calculate_zoom_factor(VH, H, YFactor), 
	  min(XFactor, YFactor, ZoomFactor), 
	  scale_picture(GW, ZoomFactor),
          normalise_margin(P, LeftMargin, TopMargin)
        ). 


automatic_zoom_sg(GW):-
	%gp3 0.2: keep this general, so added possibility of client member
  
	if
		GW->>instance_of(framedWindow)
	then
		P = GW<<-client
	else
		P = GW<<-member(picture),
	text_margin(LeftMargin, TopMargin), 
	get(P, bounding_box, area(_X, _Y, W, H)),
        % get(P, graphicals, AllGrsChain),
        get(P, visible, area(_VX, _VY, VW, VH)), 
        (catch((W < VW, H < VH), _, fail)
        ->  
          % everything fits
          % all that's necessary now is normalise 
          % (adjusting scrollbars to fit)
          normalise_margin(P, LeftMargin, TopMargin)
          % send(P, normalise, AllGrsChain)
        ;
          % not everything fits on the picture
          
	  calculate_zoom_factor_sg(VW, W, XFactor), 
          calculate_zoom_factor_sg(VH, H, YFactor), 
	  min(XFactor, YFactor, ZoomFactor), 
	  scale_picture_sg(GW, ZoomFactor),
          normalise_margin(P, LeftMargin, TopMargin)
        ). 



% calculate_zoom_factor(X1, X2, ZoomFactor), 
%
% calculates the necessary zoomfactor to fit 
% something of dimension X1 to X2 
%
calculate_zoom_factor(X1, X2, ZoomFactor):-
	   text_margin(LeftMargin, _), 
           calculate_division((X1+LeftMargin), X2, ZoomFactor).


% calculate_zoom_factor_sg(X1, X2, ZoomFactor), 
%
% calculates the necessary zoomfactor to fit 
% something of dimension X1 to X2 
%
%calculate_zoom_factor_sg(X1, X2, ZoomFactor):-
calculate_zoom_factor_sg(_X1, _X2, 0.1).
%	   text_margin(LeftMargin, _),            
%           LargerMargin is 5*LeftMargin, 
%           ZoomFactor is ((X1+LargerMargin)/X2).



% scale_picture(GW, ScaleFactor)
%
% zoom in or zoom out
%
scale_picture(GW, ScaleFactor):- 
	% Jan Wielemaker's tips and tricks: repeat by failure
	%gp3 0.2: keep this general, so added possibility of client member
  
	if
		GW->>instance_of(framedWindow)
	then
		P = GW<<-client
	else
		P = GW<<-member(picture),

        get(GW, state_nr, N), 
        \+ get(P, find, @default,
        	and(
                 	message(@arg1, instance_of, text),
			new(or)
		), _Gr
	      ),
        \+ get(P, find, @default,
        	and(
                 	message(@arg1, instance_of, graph_node),
			message(@arg1, has_get_method, type), 
		    	@arg1?type == 'entity',
                 	message(@prolog, scale_graphical, @arg1, ScaleFactor),
                        % make sure that the subfigures are positioned ok
                        message(@prolog, place_subfigures, @arg1, @default, entity, N),

			new(or)
		), _Gr2
              ),
              text_margin(LeftMargin, TopMargin), 
              normalise_selection(P, LeftMargin, TopMargin). 



% scale_picture_sg(GW, ScaleFactor)
%
% zoom in or zoom out of state transition graph
%
scale_picture_sg(GW, ScaleFactor):- 
	% Jan Wielemaker's tips and tricks: repeat by failure
	%gp3 0.2: keep this general, so added possibility of client member
  
	if
		GW->>instance_of(framedWindow)
	then
		P = GW<<-client
	else
		P = GW<<-member(picture),
        \+ get(P, find, @default,
        	and(
                 	message(@arg1, instance_of, text),
			new(or)
		), _Gr
	      ),
        \+ get(P, find, @default,
        	and(
                 	message(@arg1, instance_of, graph_node),
			message(@arg1, has_get_method, type), 
		    	@arg1?type == 'state',
                 	message(@prolog, scale_graphical, @arg1, ScaleFactor),

			new(or)
		), _Gr2
              ),
              text_margin(LeftMargin, TopMargin), 
              normalise_selection(P, LeftMargin, TopMargin). 







% get_colour_graphicals(P, Colour, Grs):-	
%
% returns all Graphicals of Colour in P. Not just the top-level 
% graphicals, but also graphicals inside other figures
%
get_colour_graphicals(P, Colour, Grs):-	
	new(Result, chain),
	% Jan Wielemaker's tips and tricks: repeat by failure
        \+ get(P, find, @default,
        	and(@arg1?colour?name == Colour,
                 	message(Result, append, @arg1),
			new(or)
		), _ColourGr
	      ),
	Grs = Result.
	


% scroll picture to ensure visibility of coloured items
%
scroll_picture(GW):- 
	%gp3 0.2: keep this general, so added possibility of client member
  
	if
		GW->>instance_of(framedWindow)
	then
		P = GW<<-client
	else
		P = GW<<-member(picture),
	get_colour_graphicals(P, red, RedGrsChain), 
	get_colour_graphicals(P, blue, BlueGrsChain), 
	send(RedGrsChain, merge, BlueGrsChain), % was reversed before v.2.02
	% normalise tries to move all elements of chain in view,
	% but if this is impossible, at least the top-left corner 
	% the area covered by these elements
	send(P, normalise, RedGrsChain). % was Blue before v.2.02

