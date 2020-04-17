/*
Definitie commandhandler menu_command

Vervangt: menu_item. Zie documentatie "Update en infotypes"
*/

:-pce_begin_class(menuCommand(value,commandname,label,end_group,accelerator),
		  menu_item,
		  "menu_item that uses a command object for messages").

variable(commandname,name,get).
variable(simple_label, name*,both). %label zonder keystring

initialise(MC,
	   V: value = any,
	   C: commandname = name,
	   L: label = [name],
	   Eg: end_group = [bool],
	   A: accelerator = [name]) :->
	"Create the menuCommand" ::
	send(MC,slot,commandname,C),
	send(MC,send_super,initialise,
	     value := V,
	     message := message(@receiver?selected_item,executeCommand),
	     end_group := Eg,
	     condition := message(@receiver,executeCondition),
	     accelerator := A
	    ),
	default(L,@nil,DL),
	MC->>label(L),
	MC->>simple_label(DL). %label wordt opnieuw gezet in executeCondition


%%
label(MC, L: [name]):->
	%overwrite, want we gaan opnieuw de accelerators zetten

	MC->+label(L),
	Menu = MC<<-menu,
	if
		Menu \== @nil
	then
		MC?menu->>assign_accelerators.
%%	

executeCommand(MC) :->
	"Find the command for the current frame and execute it" ::
	%altijd checken of het wel echt mag (ook vanwege sneltoetsen)

	pl_findFrame(@event?receiver,Frame),
	getCommand(Frame,MC?commandname,Command),
	(   Command->>canRun(MC)
	->  Command->>execute(MC)
	;   true %wel slagen anders wil het item de volgende keer niet meer
	).


executeCondition(MC) :->
	"Run the update / info procedures and check the condition in the current frame" ::
        pl_findFrame(@event?receiver,Frame),
	getCommand(Frame,MC?commandname,Command),
	Command->>canRun(MC), %mogen we wel?
	get(Command,getInfo,'Label',MC, MC?simple_label, Label), %get new label
	MC->>simple_label(Label),
	KS = Command<<-keystring,
	LabelString *= string('%s %s', Label, KS),
	
	MC->>label(LabelString).


pl_findFrame(Context,Frame):-
	%prolog helper voor het vinden van het frame
	%het frame vinden we bij het menu, maar dit item kan in een
	%submenu zitten, dan moeten we verder naar voren
	%input is de oorspronkelijke receiver van het event
	%(dit kan een graphicl ofzo zijn, of een menu_bar of een popup)
	
	%het idee is dat een popup een menu_item ofzo als context
	%kan hebben. In dat geval kan dat menu_item weer in een
	%ander menu zitten
	%3 clauses: context is een menu_item (we gaan door met het menu)
	%of context is een menu (we zoeken de context)
	%of context is iets anders/iets hierboven faalt->pak het frame

	Context->>instance_of(menu_item),
	NewContext = Context<<-menu,!,
	pl_findFrame(NewContext,Frame).
%
pl_findFrame(Context,Frame):-
	Context->>instance_of(popup),
	NewContext = Context<<-context,!,
	pl_findFrame(NewContext, Frame).
%
pl_findFrame(Context,Frame):-
	%blijkbaar is de vorige clause gefaald, dus vragen we het frame
	%van de huidige
	
	Frame = Context<<-frame.


:-pce_end_class.
