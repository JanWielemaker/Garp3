/*
Definitie subclass van text_item die op een toets en andere
events kan reageren

Part of Garp3, see copyright notice
Most code old dutch homer code

2000-2005 Jelmer Jellema, Spin in het Web, spininhetweb.nl

*/

:-pce_begin_class(
		  eventTextItem,
		  text_item,
		  "a text_item with more events to watch for"
		 ).

variable(onKey,'code*',get,"Message to run when a key is pressed").
variable(afterKey, 'code*', get, "Message to run after a key is handled").
variable(onFocus,'code*',both,"Message to run when the field gets focus"). %gp3 0.3.14
variable(onLostFocus,'code*',both,"Message to run when the field looses focus"). %gp3 0.3.14

%%
initialise(E,
	   Name : name = [name],
	   D : default = [any|function],
	   M : apply_message = '[code]*'
	  ):->

	E->+initialise(Name,D,M).
%%

%%
onKey(E,Code : code*):->
	"Set the message to run when a key is pressed" ::
	%@arg1 is het text item, @arg2 is het event
	%als de handler faalt wordt het event niet doorgegeven naar de superclass

	E->>slot(onKey,Code).
%%

%%
afterKey(E,Code : code*):->
	"Set the message to run after a key is handled" ::
	%@arg1 is het text_item

	E->>slot(afterKey,Code).
%%

%%
event(E,Event : event):->
		@on = E<<-active, %gp3 0.3.11 otherwise, never mind
	(   checkOnKeyEvent(E,Event), % or checkOtherEvent..
	    E->+event(Event)
	->  %en nu alle after events
	    ignore(checkAfterKeyEvent(E,Event))
	;   true
	).

%%
%%die checkOn..events moeten slagen als het event niet voor hun bestemd is, en alleen falen
%%wanneer de on.. call faalt

checkOnKeyEvent(E,Event):-
	(  
		 \+ Event->>is_a(keyboard)
	;
	     @nil = E<<-onKey
	),!. %in beide gevallen klaar
%
checkOnKeyEvent(E,Event):-
	E?onKey->>forward(E,Event).
%%

%%
%%checkAfter calls slagen altijd (worden met een ignore aangeroepen)

checkAfterKeyEvent(E,Event):-
	Event->>is_a(keyboard),
	\+ @nil = E<<-afterKey,
	E?afterKey->>forward(E).
%%

%%
activate(E, B:bool):->
	%gp3 0.3.14: call onFocus or onLostFocus
	if
	(
		B == @on,
		\+ @nil = E<<-onFocus
	)
	then
		ignore(E?onFocus->>forward(E)),
	if
	(
		B == @off,
		\+ @nil = E<<-onLostFocus
	)
	then
		ignore(E?onLostFocus->>forward(E)),
	E->+activate(B).
:-pce_end_class.
		  

