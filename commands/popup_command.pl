/*
Definitie commandhandler popupCommand

Vervangt: popup (als context menu).
Werkt vaak met een niet runbaar command (gemaakt met runnable :=@off) om gebruik
te maken van de update mogelijkheden via het command. Het kan wel met een runbaar
command: Wanneer bij menu-item geen eigen message heeft, wordt de message van de
popup gebruikt. Hiertoe dus een popupCommand voor de popup zelf, en een 
normaal menu_item voor het item (check enzo werkt dan ook niet bij dat item).

Voor popup in een menu_bar kan menuPopupCommand worden gebruikt, die kan zijn label zetten
in de idle time. Dat is een subclass van popupCommand.

Zie ook de handler contextPopup: een handler die de hele context popup bedoening regelt,
deze heeft een popupCommand als member.
*/

:-pce_begin_class(popupCommand(name,commandname),
		  popup,
		  "context-popup that uses a command object for messages").

variable(commandname,name,get).

%%
initialise(PC,
	   C: commandname = name,
	   N: name = [name]) :->
	"Create the popupCommand" ::
	send(PC,slot, commandname, C),
	send(PC,send_super,initialise,
	     name := N,
	     message := message(@receiver,executeCommand)),
	send(PC,update_message,message(@receiver,executeUpdateMethod)).
%%

%%
executeCommand(PC) :->
	"Execute the associated command" ::

	getCommand(@event?receiver?frame,PC?commandname,Command),
	%toch checken of het mag
	(   Command->>canRun(PC)
	->  Command->>execute(PC?selected_item) %context is het item, niet de popup
	;   true %altijd slagen
	).
%%

%%
executeUpdateMethod(PC) :->
	"Execute the general lay-out method via the associated command" ::
	getCommand(@event?receiver?frame,PC?commandname,Command),
	send(Command,runUpdateMethod,'FillPopup',PC).
%%
:-pce_end_class.

