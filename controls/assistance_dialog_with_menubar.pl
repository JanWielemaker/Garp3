
:-pce_begin_class(assistanceDialogWithMenuBar,
		  assistanceDialog
		 ).

variable(menubar, dialogMenubar, get , "The menu_bar of this assistance dialog").
variable(commands,hash_table,get).
variable(notImplementedCommand,notImplementedCommand,get).
%variable(garpModel, garpModel, both, 'The associated garpModel this editor edits.').


initialise(D, Label: name, HelpId: [name]*, BorderBackgroundColour: bgColour = [colour]):->	
	D->+initialise(Label, HelpId, BorderBackgroundColour),

	D->>application(@app),
	D->>kind(toplevel),
	D->>icon(@build_icon),

        % the code below was inspired by framedWindow, which is used by the graphical sketch editors
	%MENUBAR
	MB *= dialogMenubar(D),
	D->>slot(menubar,MB),		 
	%gp3 0.3.13: reset the root tile border, which will be used by all tiles
	D?tile->>border(0),       
	%het speciale command dat niet geimplementeerde
	%commando's afhandelt
	NIC *= notImplementedCommand(D, 'NotImplemented'),
	D->>slot(notImplementedCommand,NIC),
	%commands slot
	D->>slot(commands,new(hash_table)),
        send(D, display, MB, point(0, 3)),
	D->>updateSpacers,

	/* Multiple models */ % JL
	new(Garp3EditorFrame, garp3EditorFrame(Label)),
	get(@model, '_value', Model),
	send(Garp3EditorFrame, associateModel, Model),
	send(Garp3EditorFrame, append, D).
	
%%
command(FW,
	Name : name = name, 
	Run : runnable = [bool],
	Key: key = [name],
	Keystring: keystring = [name],
	Otherkeys: otherkeys = [chain] %otherkeys this command reacts to
	) :->
	"Create a new command for this frame" ::
	%Argument Run vertelt ons of het commando wel mag runnen. Als dit @off is
	%zal wel update en check gedaan worden, maar niet execute. Default is @on

	C *= command(FW,Name,Run,Key, Keystring, Otherkeys),
	%dit stoppen we in de commands chain
	FW?commands->>append(Name,C). 
%%

%%	
command(FW,
	Name: name,
	Command: command) :<-
	"Return an existing command with the given name" ::

	Command = FW?commands<<-member(Name).
%%

%%
onNotImplemented(EE
		 ) :->
	"Default handler for not implemented commands"::

	EE->>bell,
	EE->>report(warning, 'Warning: Command %s is not implemented in this window',
		    @command?name).
%%


%
key(FW, Key: name):->
	"Handel toetsaanslagen af (doorgestuurd door dialogMenubar)" ::
	%gp3 0.2 Now commands can react to more than 1 key
	%so this is a bit rewritten
	%vind een command die deze key wil hebben

	if
	(
		Command = FW?commands<<-find_value(->>(@arg2,wantsKey,Key)),
		Command->>canRun(FW)
	)
	then
		Command->>execute(FW).
%%


%%
event(FW, Event: event) :->
	"Handle events, disable sub-window resize"::
	%als het event optreedt bij een sub-tile, negeren we hem

	\+ FW?tile<<-sub_tile_to_resize(Event?position),
	FW->+event(Event).
	% FW?frame->event(Event).
%%


:-pce_end_class.


% dialogMenubar class 
%
% inpired by framedMenubar class
%
:-pce_begin_class(
	dialogMenubar,
	menu_bar,
	"Menu bar for (assistance) dialog"
	).

variable(dialog, assistanceDialog, get , "The dialog this bar belongs to").

	
initialise(DMB,D: assistanceDialog):->
	DMB->+initialise,
	DMB->>slot(dialog, D).
	

key(DMB, Key: name):->
	unless
		DMB->+key(Key)
	do
		DMB?dialog->>key(Key).

% commands need a reference to frame (although this may be a dialog too)
frame(DMB, D):<-
        get(DMB, dialog, D).

	
:-pce_end_class.
