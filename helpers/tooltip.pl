/*
Part of GARP 3, see copyright notice
Based on the pce library help_message, which was publiced under the GNU GPL 2+
so this license *might* also apply to this code.

Changed: 
- naming so we dont interfere with the original library code
- minor lay-out changes (color)
- the way the tooltip gets its text:
	visual->tooltip can be called with argument:
	- char_array: the static text to be displayed
	- @default: method visual<-tooltipContent will be called for the visual, when it exists and succeeds, the result will be displayed
	- @nil: do not show anything


%gp3 1.4: you can assign a category to any registered tooltip (default: general). 
%and use @tooltip_window->>category_state(bool) to (de)activate tooltips in this category

Original notice:
    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(tooltip, []).

:- pce_global(@tooltip_window, new(tooltip_window)).

active(Bool):-
	%gp3 0.3: helper to tell the tooltip window to be active or not
	@tooltip_window->>active(Bool).
%%
	
:- pce_begin_class(tooltip_window, dialog,
		   "Window to display <-tooltip").

class_variable(background, colour, light_yellow, "Ballon background").

variable(handler,	handler,	get, "Handler for intercept").
variable(message,	'[string]*',	get, "Currently displayed message").

variable(active, bool := @on, get). %gp3 0.3: when @off, never shown. See tooltip:active(..) above.
variable(categories,hash_table,get). %gp3 1.4: all categories and their activation state (see category_state)

initialise(W) :->
	send(W, slot, handler,
	     handler(mouse, message(W, try_hide, @event))),
	send_super(W, initialise),
	get(W, frame, Frame),
	send(Frame, kind, popup),
	send(Frame, sensitive, @off),
	send(Frame, border, 0),
	send(Frame?tile, border, 0),
	send(W, gap, size(1, 1)),
	if
		'windows' = @pce<<-window_system
	then
		send(W, pen, 1)
	else
		W->>pen(0),
	send(W, append, new(L, label(feedback, '', font(helvetica,roman,10)))),
	send(L, length, 0),
	W->>slot(categories,new(hash_table)),
	send(Frame, create).

owner(W, Owner:[any]*) :->
	"Maintain hyperlink to the owner"::
	(   Owner == @nil
	->  send(W, delete_hypers, owner)
	;   Owner == @default
	->  true			% no change
	;   new(_, tooltip_hyper(Owner, W, help_baloon, owner))
	).
owner(W, Owner:any) :<-
	get(W, hypered, owner, Owner).
		

try_hide(W, Ev:event) :->
	get(W, owner, Owner),
	(   send(Ev, inside, Owner),
	    (   send(Ev, is_a, loc_move)
	    ;	send(Ev, is_a, loc_still)
	    )
	->  %send(@pce, format, '%O: Move/still event\n', Owner),
	    get(W, message, OldMsg),
	    (   get(Owner, tooltip, Ev, Msg)
	    ->	%send(@pce, format, '%O: yielding %s\n', Owner, Msg),
	        (   OldMsg \== @nil,
		    send(Msg, equal, OldMsg)
		->  (   send(Ev, is_a, loc_still)
		    ->	send(W, adjust_position, Ev)
		    ;	true
		    )
		;   send(W, feedback, Msg, Ev, @default, @nil)
		)
	    ;	(   get(W, message, @nil)
		->  true
		;   send(W, feedback, @nil, Ev, @default, @nil)
		)
	    )
	;   send(W, owner, @nil),
	    send(W, hide),
	    fail			% normal event-processing
	).


hide(W) :->
	"Remove from the display"::
	send(W, show, @off),
	get(W, handler, H),
	send(W?display?inspect_handlers, delete, H).

	
active(W, A: bool):->
	%gp3 0.3 Set active state. When set to @off, we hide the window if needed
	
	W->>slot(active,A),
	if
		A == @off
	then
		ignore(W->>hide).
%%


%%gp3 1.4: you can assign a category to a registered tooltip, and you can (de)activate categories:

category_state(W,Category: name, State: bool):->
	%change state for category, if it exists, otherwise fails
	
	Categories = W<<-categories,
	Current = Categories<<-member(Category), %fails if none
	if
		Current \== State
	then
	(
		Categories->>append(Category,State),
		if
			State == @off
		then
			ignore(W->>hide) %to be safe
	).
%
category_state(W,Category: name*, State: bool*):<-
	%return the state for the category, or @nil if not found
	
	unless
		State = W?categories<<-member(Category)
	do
		State = @nil.
%
category_add(W, Category: name):->
	%add a new or existing category
	
	unless
		W?categories<<-member(Category)
	do
		W?categories->>append(Category,@on).
%%
	
feedback(W, S:[string]*, Ev:event, For:[any]*, Category:[name]*) :->
	"Display window holding string and grab pointer"::
	%gp3: if the message that was set is @nil nothing is shown (was allready this way)
	%if its @default, we will get the message using For?tooltipContent
	%this can also be @nil (or tooltipContent is not a get message)
	%then no tooltip
	%
	%gp3 0.3: when active is @off we do not show
	%gp3 1.4: when active is @off for the given category, we do not show
	
	default(Category,@nil,DCategory), %@nil is not in categories: we will show. Used by try_hide above
	
	unless
	(
		@off = W<<-active
	;
		@off = W<<-category_state(DCategory)
	)	
	do
	(
		send(W, owner, For),
		%gp3:
		if
			S == @default
		then
			catch(Msg = For<<-tooltipContent,_, Msg = @nil)
		else
			Msg = S,
			
		send(W, slot, message, Msg),
		(   Msg == @nil
		->  send(W, show, @off)
		;   get(W, member, feedback, L),
		    send(L, selection, Msg),
		    send(W, layout),
		    send(W?frame, fit),
		    send(W, adjust_position, Ev),
		    send(W?display, inspect_handler, W?handler)
		)
	).


adjust_position(W, Ev:event) :->
	"Fix the position of the feedback window"::
	get(Ev, position, W?display, P),
	get(P, plus, point(5,20), point(FX, FY)),
	send(W?frame, set, FX, FY),
	send(W?frame, expose).

:- pce_end_class.

:- pce_extend_class(visual).

tooltip(Gr, Msg: [string]*, Category: [name]) :->
	"Associate a tooltip"::
	%gp3: @default means: ask when needed using owner?tooltipContent (see tooltip_window->feedback)
	%gp3 1.4: Category is the category of the tooltip, as used by @tooltip_window->category_state
	%default: general
	
	default(Category,'general',DCategory),
	@tooltip_window->>category_add(DCategory),

	if
		Msg == @nil
	then
	(
		ignore(Gr->>delete_attribute(tooltipmsg)),
		ignore(Gr->>delete_attribute(tooltipcat))
	)
	else
	(
		Gr->>attribute(tooltipmsg,Msg),
		Gr->>attribute(tooltipcat,DCategory)
	).
	

tooltip(V, _Ev:[event], Msg:[string]) :<-
	get(V, attribute, tooltipmsg, Msg).

tooltip_category(V, _Ev:[event], Cat:name) :<- %gp3 1.4
	get(V, attribute, tooltipcat, Cat).
:- pce_end_class.


:- pce_extend_class(graphical).

%gp3 1.4: added Category stuff
show_tooltip(Gr, Ev:event) :->
	find_tooltip(Gr,Ev, Owner, Msg, Category),
	send(@tooltip_window, feedback, Msg, Ev, Owner, Category).


find_tooltip(Gr, Ev, Gr, Msg, Category) :-
	get(Gr, tooltip, Ev, Msg),
	get(Gr, tooltip_category, Ev, Category), !.
	
find_tooltip(Gr, Ev, Owner, Msg,Category) :-
	get(Gr, contained_in, Container),
	find_tooltip(Container, Ev, Owner, Msg, Category).

:- pce_end_class.


:- pce_extend_class(menu).

%gp3 1.4: added Category stuff
tooltip(Gr, Ev:[event], Msg:string) :<-
	"Fetch associated help message"::
	(   get(Gr, item_from_event, Ev, Item),
	    get(Item, tooltip, Msg)
	->  true
	;   get(Gr, get_super, tooltip, Ev, Msg)
	).

tooltip_category(Gr, Ev:[event], Category:name) :<-
	"Fetch associated help category"::
	(   get(Gr, item_from_event, Ev, Item),
	    get(Item, tooltip_category, Category)
	->  true
	;   get(Gr, get_super, tooltip_category, Ev, Category)
	).
	
:- pce_end_class.

%gp3 0.2 does the same trick with list_browser
%but because of a problem we hide when loc_move is the event
:- pce_extend_class(list_browser).

tooltip(Gr, Ev:[event], Msg:string) :<-
	"Fetch associated help message"::
	loc_still = Ev<<-id,
	(   get(Gr, dict_item, Ev, Item),
	    get(Item, tooltip, Msg)
	->  true
	;   get(Gr, get_super, tooltip, Ev, Msg)
	).

%gp3 1.4: category stuff
tooltip_category(Gr, Ev:[event], Category:name) :<-
	"Fetch associated help category"::
	loc_still = Ev<<-id,
	(   get(Gr, dict_item, Ev, Item),
	    get(Item, tooltip_category, Category)
	->  true
	;   get(Gr, get_super, tooltip_category, Ev, Category)
	).

:- pce_end_class.


:- pce_begin_class(tooltip_hyper, hyper,
		   "Hyper between help-balloon and owner").

unlink_from(H) :->
	"->hide the <-to part"::
	get(H, to, Part),
	(   object(Part)
	->  send(Part, hide)
	;   free(Part)
	),
	free(H).

:- pce_end_class.

		 /*******************************
		 *	     REGISTER		*
		 *******************************/

register_tooltip_window :-
	send(@display, inspect_handler,
	     handler(loc_still,
		     message(@receiver, show_tooltip, @event))).
		     
:- initialization
   register_tooltip_window.
