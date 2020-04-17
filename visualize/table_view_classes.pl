

% class definition for table_transition(ToNr, X1, Y1, X2, Y2, ArrowLength)

:- pce_begin_class(table_transition(int, int, int, int, int, int), line).

variable(type, name:=table_transition, both, "The type of transition").
variable(name, name:=unknown, both, "The name of the transition").
variable(toStateNr, int, both, "The state nr of the transition destination").


initialise(L, Name: name, ToNr: int, X1: int, Y1: int, X2: int, Y2: int, ArrowLength: int):->
	 send(L, send_super, initialise, X1, Y1, X2, Y2), 
         % new(L, line(0, 0, XPos2-XPos1, 0)), 
         send(L, pen, 1),
         send(L, arrows, second),
	 send(L?second_arrow, length, ArrowLength),
	 send(L?second_arrow, wing, 9), % default = 7 
         send(L, colour, colour(black)),
         send(L, toStateNr, ToNr),
         send(L, name, Name).


selected(L, S:bool) :->
	get(L, type, Type), 
	Type == table_transition,
	send_super(L, selected, S),
	(   S == @on
	->  
	    send(L, colour, red)
	;   
	    send(L, colour, black)
	).

:- pce_end_class.


% class definition for table_termination_text

:- pce_begin_class(table_termination_text(name), text).

variable(type, name:=table_termination, both, "The type of text").
variable(name, name:=unknown, both, "The name of the transition").


initialise(T, Name: name):->
	 send(T, send_super, initialise, Name, center), 
         send(T, colour, colour(black)),
         send(T, name, Name),
         send(T, font, font(helvetica, roman, 11)).


selected(T, S:bool) :->
	get(T, type, Type), 
	Type == table_termination,
	send_super(T, selected, S),
	(   S == @on
	->  
	    send(T, colour, red)
	;   
	    send(T, colour, black)
	).


status_colour(T, Status: name) :->
         % colour T according to termination status
         % In the old days, before VisiGarp, each termination could have a different 
         % status, but now, all terminations of a state have the same status, i.e., the state_status
 	 %status is unprocessed (yellow), terminated (orange), ordered (brown), closed (black)	

	 if
		Status = unprocessed
	 then
                send(T, background, colour(yellow)), 
	 if
		Status = terminated
	 then
                send(T, background, colour(orange)), 
	 if
		Status = ordered
	 then
                % send(T, background, colour(brown)),          
                send(T, background, colour(chocolate3)).
         


:- pce_end_class.

