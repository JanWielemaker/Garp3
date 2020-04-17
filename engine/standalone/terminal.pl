/*  File:    terminal
    Purpose: How to underline/bold/reverse etc.
    Author:  Martin Reinders & Bert Bredeweg
    Date:    August 1989
    Part-of: GARP (version 1.0)

    Copyright (c) 1989, University of Amsterdam. All rights reserved.

*/

set_termcaps:-
	getenv('PLLD', true), !.
set_termcaps:-
	(get_capabilities;true), 
	set_screen_width, 
	set_selected, 
	set_header, 
	set_item, 
	!.

% only for direct output to terminal !

% selected items
% if halfbright: selected is normal, not selected is halfbright
% otherwise if bold: selected is bold, not selected is normal
% otherwise if standout: selected is standout, not selected is normal
% otherwise if reverse video: selected is reverse, not selected is normal

set_selected:-
	termcap(mh, HalfOn), 		% halfbright exists
	termcap(me, HalfOff), 
	asserta(select_on(''/''/HalfOn/HalfOff/0)), 
	!.

set_selected:-
	termcap(md, BoldOn), 		% bold exists
	termcap(me, BoldOff), 	% attributes off
	asserta(select_on(BoldOn/BoldOff/''/''/0)), 
	!.

set_selected:-
	termcap(so, StandOut), 	% standout exists
	termcap(se, StandOutEnd), 	% standout end
	asserta(select_on(StandOut/StandOutEnd/''/''/0)), 
	!.

set_selected:-
	termcap(mr, CapOn), 		% reverse on
	termcap(me, CapOff), 		% reverse off
	asserta(select_on(CapOn/CapOff/''/''/0)), 
	!.

set_selected:-
	asserta(select_on('* '/''/'  '/''/2)), 
	!.

% use underline for header

set_header:-
	termcap(us, CapOn), 
	termcap(ue, CapOff), 
	asserta(header_on(CapOn/CapOff)), 
	!.

% otherwise use reverse

set_header:-
	termcap(mr, CapOn), 
	termcap(me, CapOff), 
	asserta(header_on(CapOn/CapOff)), 
	!.

% otherwise use standout

set_header:-
	termcap(so, CapOn), 
	termcap(se, CapOff), 
	asserta(header_on(CapOn/CapOff)), 
	!.

% otherwise use bold

set_header:-
	termcap(md, CapOn), 
	termcap(me, CapOff), 
	asserta(header_on(CapOn/CapOff)), 
	!.

% otherwise nothing
set_header:-
	asserta(header_on(''/'')), 
	!.
/* as bold is same as standout and reverse on sun, don't use it
% use bold for item
set_item:-
	termcap(md, CapOn), 
	termcap(me, CapOff), 
	asserta(item_on(CapOn/CapOff)), 
	!.
*/

% otherwise nothing
set_item:-
	asserta(item_on(''/'')), 
	!.

set_screen_width:-
	window_size(_, C), 
	CC is C - 2, 
	tty_fold(_, CC), 
	!.

set_screen_width:-
	termcap(co, Num), 
	NNum is Num - 2, 
	NNum > 40, 
	tty_fold(_, NNum), 
	!.

set_screen_width:-
	tty_fold(_, 78).

%cls :- !.
cls :-
	termcap(cl, CS), !,
	write(CS).
cls :-
	window_size(R, _),
	forall(between(1,R,_), nl).

% get terminal capabilities (if present) for
% cl: clear screen/home cursor
% co: columns (width)
% md: bold on
% mh: halfbright on
% mr: reverse on
% me: bold/halfbright/reverse off
% so: standout on
% se: standout off
% us: underline on
% ue: underline off

capability(cl,	string).
capability(co,	number).
capability(md,	string).
capability(mh,	string).
capability(mr,	string).
capability(me,	string).
capability(so,	string).
capability(se,	string).
capability(us,	string).
capability(ue,	string).

get_capabilities :-
	feature(arch, 'i386-win32'), !,
	atom_chars(A, [27, 0'[, 0'2, 0'J]),
	assert(termcap(cl, A)).
get_capabilities :-
	capability(Name, Type),
	tty_get_capability(Name, Type, Value),
	assert(termcap(Name, Value)),
	fail.
get_capabilities.
			  
