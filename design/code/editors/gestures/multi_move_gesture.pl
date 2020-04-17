/*
Definitie multi_move gesture die is gedefinieerd bij graphicals.
LET OP: De gesture moet wel bij elke graphical worden gedefinieerd, anders werkt
ie ook buiten de graphicals.
Kan goed gebruikt worden voor verplaatsen van hele selecties.

Callbacks:
moveableObjects: functie die een lijst met graphicals moet teruggeven die gemoved kunnen worden
hierdoor kan rekening gehouden worden met selectie maar ook met spatials
dragMessage: call voor draggen, meegestuurd wordt de change (@arg1) en de opgegeven lijst te verplaatsten graphicals (@arg2)
gp3 1.0: endMessage called after dragging, with the list of moving graphicals as @arg1
*/

:-pce_begin_class(multi_move_gesture,
		  gesture,
		  "Implementation of multi-object move gesture"
		 ).

variable(getMoveFunction,function,both,"Function retrieving a chain of objects to move").
variable(dragMessage,message,both,"Called when dragging").
variable(endMessage,message*,both,"Called when dragging ended"). %gp3 1.0
variable(movingObjects,chain*,both,"The objects to move").

variable(position,point,both,"Previous cursor position").

initialise(MMG,
	   F : getMoveFunction = function,
	   D: dragMessage = message,
	   B : button = [button_name],
	   M : modifier = [modifier],
	   E : endMessage = [message]	%gp3 1.0
	  ):->
	MMG->+initialise(B,M),
	MMG->>getMoveFunction(F), 
	MMG->>dragMessage(D),
	default(E,@nil,EM),
	MMG->>endMessage(EM). %gp3 1.0, see terminate
%%

%%
initiate(MMG,
	 Event: event
	):->
	%aangeroepen bij het begin van de gesture (na verify)
	%we slaan de objecten op en de huidige positie

	NewPos = Event<<-position(Event?receiver?device),
	MMG->>position(NewPos),
	MMG->>movingObjects(MMG?getMoveFunction).
%%

%%
drag(MMG,
     Event: event
    ):->
	%drag event: We kijken in hoeverre de andere graphicals moeten meebewegen
	%maar daarbij houden we ook rekening met dat ze al meebewegen...
	%gaat uit van visualElement
	
	CurrentPosition = Event<<-position(Event?receiver?device),
	Change = CurrentPosition<<-minus(MMG?position),
	MO = MMG<<-movingObjects,
	MMG?dragMessage->>forward(Change,MO),
	MMG->>position(CurrentPosition),
	Change->>done.
%%

%%
terminate(MMG,
	  _Event: event
	 ):->
	%terminate: button up 
	%gp3 1.0: we call endMessage before freeing up the movingObject

	MO = MMG<<-movingObjects,
	EM = MMG<<-endMessage,
	unless
		EM = @nil
	do
		EM->>forward(MO),
	MMG->>movingObjects(@nil).
%%


:-pce_end_class.
