/*  $Id$

    Part of ModelDraw
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1997 University of Amsterdam. All rights reserved.
*/

:- module(model_arrows, []).
:- use_module(library(pce)).

		 /*******************************
		 *	    DATA ARROW		*
		 *******************************/

:- pce_global(@triangle_arrow, make_triangle_arrow).

make_triangle_arrow(A) :-
	new(A, arrow(16, 14)),
	send(A, style, closed),
	send(A, fill_pattern, @white_image),
	send(A, pen, 1).

:- pce_global(@open_arrow, make_open_arrow).

make_open_arrow(A) :-
	new(A, arrow(16, 14)),
	send(A, style, open),
	send(A, fill_pattern, @nil),
	send(A, pen, 1).


		 /*******************************
		 *     CARDINATITY `ARROW'	*
		 *******************************/

:- pce_global(@uml_many_arrow,     new(uml_cardinality_arrow(many))).
:- pce_global(@uml_optional_arrow, new(uml_cardinality_arrow(optional))).

:- pce_begin_class(uml_cardinality_arrow, circle).

variable(cardinality,	{many,optional}, get, "Registered cardinality").

initialise(CA, Type:{many,optional}) :->
%	send(CA, send_super, initialise, 11),
	send(CA, send_super, initialise, 7), % AB, 1/9/2000
	send(CA, slot, cardinality, Type),
	(   Type == many
	->  Fill = @black_image
	;   Fill = @white_image
	),
	send(CA, fill_pattern, Fill).

points(CA, TX:int, TY:int, RX:int, RY:int) :->
	"Emulate `arrows->points'"::
	get(CA, radius, R),
	Dist is sqrt( abs((RX-TX)**2 + (RY-TY)**2) ),
	(   Dist =:= 0
	->  send(CA, center, point(TX, TY))
	;   M is R/Dist,
	    CX is integer(TX + M*(RX-TX)),
	    CY is integer(TY + M*(RY-TY)),
	    send(CA, center, point(CX, CY))
	).

:- pce_end_class.


		 /*******************************
		 *	 AGGREGATE `ARROW'	*
		 *******************************/

:- pce_global(@open_diamond_arrow, new(diamond_arrow)).
:- pce_global(@closed_diamond_arrow,
	      make_closed_diamond_arrow).

make_closed_diamond_arrow(A) :-
	new(A, diamond_arrow),
	send(A, fill_pattern, @black_image).

:- pce_begin_class(diamond_arrow, path,
		   "Diamand-like arrow indicating aggregation").

variable(length,	int := 24,	get, "Default length").

initialise(A, Len:'[0..]') :->
	send(A, send_super, initialise),
	send(A, closed, @on),
	send(A, fill_pattern, @white_image),
	(   Len \== @default
	->  send(A, slot, length, Len)
	;   true
	).

points(A, TX:int, TY:int, RX:int, RY:int) :->
	get(A, length, L),
	Dist is sqrt( abs((RX-TX)**2 + (RY-TY)**2) ),
	(   Dist =:= 0
	->  send(A, clear)
	;   M is L/Dist,
	    BX is integer(TX + M*(RX-TX)),
	    BY is integer(TY + M*(RY-TY)),
	    CX is integer((TX+BX)/2),
	    CY is integer((TY+BY)/2),
	    P1X is CX + M*(RY-TY)/4,
	    P1Y is CY - M*(RX-TX)/4,
	    P2X is CX - M*(RY-TY)/4,
	    P2Y is CY + M*(RX-TX)/4,
	    send(A, clear),
	    send(A, append, point(TX, TY)),
	    send(A, append, point(P1X, P1Y)),
	    send(A, append, point(BX, BY)),
	    send(A, append, point(P2X, P2Y))
	).

:- pce_end_class.


:- pce_global(@half_arrow, new(half_arrow)).

:- pce_begin_class(half_arrow, line,
		   "On-sided arrow").

variable(length,	int := 16,	get, "Default length").

initialise(A, Len:'[0..]') :->
	send(A, send_super, initialise),
	(   Len \== @default
	->  send(A, slot, length, Len)
	;   true
	).

points(A, TX:int, TY:int, RX:int, RY:int) :->
	get(A, slot, length, L),
	Dist is sqrt( abs( (RX-TX)**2 + (RY-TY)**2) ),
	(   Dist =:= 0
	->  send(A, points, TX, TY, TX, TY)
	;   M is L/Dist,
	    BX is integer(TX + M*(RX-TX)),
	    BY is integer(TY + M*(RY-TY)),
	    P1X is BX - M*(RY-TY)/2,
	    P1Y is BY + M*(RX-TX)/2,
	    send(A, send_super, points, P1X, P1Y, TX, TY)
	).

:- pce_end_class.





