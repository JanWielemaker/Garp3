/*
Definitie commandhandler contextPopup

Vervangt: handler_group voor een context menu.
Deze commandhandler is erg gerelateert aan de commandhandler popupCommand: het is een 
popupCommand die is ingepakt in een handler_group. Hierbij worden de volgende recognisers
geïnstalleerd:
-op de rechterknop een popup_gesture voor het popupCommand.
-op de linkerknop dubbelklik een click_gesture voor het default item uit het popupCommand.

De handler kan op twee manieren gemaakt worden:
1) Met een bestaand popupCommand object. Dit is dan meteen de popup en het command van die popup
   wordt overgenomen bij de contextPopup. Let op: het default item moet een menuCommand zijn, geen menu_item
2) Met een command naam. Er wordt dan een nieuw popupCommand gemaakt met die command naam. Deze
   wordt intern bijgehouden en vernietigd bij de unlink. Deze moet dan dus dynamisch gevuld
   worden (waarbij het default item weer een menuCommand moet zijn).

Ook kan er een selectioncall worden meegestuurd, die zorgt dat het item waar op geklikt wordt
inderdaad geselecteerd wordt. Hierbij is @arg1?receiver de receiving graphical. 
Wanneer die call faalt wordt de boel geannuleerd.

Het interne popupCommand stuurt zelf command-calls, maar deze contextPopup stuurt er ook eentje: 
voor het zetten van het default item (de value die verwijst naar een menuCommand).
Voor deze geldt hetzelfde met betrekking tot de runnable flag van het command als voor 
popupMenu
*/

:-pce_begin_class(contextPopup(commandname),
		  handler_group,
		  "Context-menu handler that uses a command object for messages").

variable(commandname,name,get).

variable(popup,popupCommand,get).

variable(internalPopup,bool,get).

%%
initialise(CP,
	   Arg: initialise_with = 'popupCommand | name',
	   S : selection_call = [code*]
	  ):->
	"Create the contextPopup" ::
	%We delegeren het initialiseren van de popup
	
	CP->>init_popup(Arg), %zet de interne slots goed, en hookt in de update-method
	default(S,@nil,SC),
	%nu de handlers maken
	Popup *= popup_gesture(CP?popup,right), %het popup-menu
	Popup->>condition(SC), %bij de conditie de selectie regelen
	DefaultClick *= click_gesture(left,'',double,
				      ->>(CP,defaultClick)), %hmm geen @arg1 ofzo mogelijk?
	DefaultClick->>condition(SC),
        CP->+initialise(Popup,DefaultClick).
%%

%%
unlink(CP):->
	"Destroy the internal popupCommand" ::

	(   @on = CP<<-internalPopup
	->  CP?popup->>destroy
	;   true
	),
	CP->+unlink.
%%

%%
init_popup(CP,
	   Arg: 'popupCommand | name'
	  ):->
	"Internal helper of initialise"::
	%twee clauses, de ene is voor een meegestuurde popupcommand, de andere
	%voor een meegestuurde naam

	Arg->>instance_of(popupCommand),
	CP->>slot(popup,Arg),
	CP->>slot(internalPopup,@off),
	CP->>slot(commandname,Arg?commandname),
	CP->>hookPopup. %zorg dat wij in de update-message komen
%
init_popup(CP,
	   Arg
	  ):->
	%versie voor meegestuurde name

	Arg->>instance_of(name),
	CP->>slot(commandname,Arg),
	CP->>slot(internalPopup,@on),
	CP->>slot(popup,new(popupCommand(Arg))),
	CP->>hookPopup.
%%

%%
hookPopup(CP):->
	"Internal helper of init_popup" ::
	%hook de popup update-message, zodat het default_item kunnen aangeven

	PrevMessage = CP?popup<<-update_message,
	CP?popup->>update_message(and(PrevMessage,
				      ->>(CP,showDefaultMessage))).
%%

%%
defaultClick(CP):->
	"Execute the default popup-item after double-left click" ::
	%Verschillende clauses

	%1: geen default item
	@nil = CP<<-defaultValue.
%
defaultClick(CP):->
	%2: de naam bestaat en het commando runt
	
	Default = CP<<-defaultValue,
	%we moeten de popup opnieuw vullen
	CP?popup->>executeUpdateMethod, %hopenlijk goed met @arg enzo?
	Item = CP?popup<<-member(Default),
	Item->>instance_of(menuCommand),
	%het moet een menuCommand zijn, dus kunnen we de message runnen
	Item->>executeCommand.
%
defaultClick(_CP):->
	%3: catch-all

	true.
%%

%%
showDefaultMessage(CP):->
	"Called before opening the popup: make sure the default item is bold" ::

	Default = CP<<-defaultValue,
	NormalFont = CP?popup<<-value_font,
	BoldFont *= font(NormalFont?family,
			 bold,
			 NormalFont?points), %hoe doe ik dit?
	CP?popup?members->>for_all(if(->>(@arg1?value,equal,Default),
				      ->>(@arg1,font,BoldFont),
				      ->>(@arg1,font,@default))).
%%

%%
defaultValue(CP,DefaultValue: 'any*'):<-
	"Ask the frame the current default item of the popup" ::
	%werkt alleen in een event natuurlijk

	getCommand(@event?receiver?frame,
			    CP?commandname,
			    Command),
	DefaultValue = Command<<-getInfo('DefaultItem',
					 CP?popup).
%%
:-pce_end_class.

