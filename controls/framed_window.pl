/*
Definitie framedWindow class: Standaard window met een canvas. Speciaal ingericht
voor menubalk en voor werken met commandobject.
Tevens definitie van:
	 framedCanvas: een subclass van picture, voor de canvas (voor de toekomst)
	 framedMenubar: een subclass van menubar die de hele menubar heeft. Dit is een patch om de sneltoetsen die niet door de menubar worden opgepikt terug te sturen naar het framedWindow (die ze routeert naar zijn commands)
	 

Part of Garp3, see copyright notice
Most code old dutch homer code

2000-2005 Jelmer Jellema, Spin in het Web, spininhetweb.nl
*/
:-pce_begin_class(
		  framedWindow(label,application),
		  frame,
		  "A frame with a menu-bar, client window and status-bar"
		 ).

variable(menubar, framedMenubar, get , "The menu_bar of this frame").
variable(buttonBar, commandButtonHolder*,get, "The buttonbar of this frame"). %gp 3 0.2
variable(notImplementedCommand,notImplementedCommand,get).

variable(commands,hash_table,get).
variable(statusTextTimer,timer,both). %gp3 0.3: timer to show a status text for just a while

variable(garpModel, garpModel, both, 'The garpModel associated to the editor.').

%%
initialise(
	   FW, 
	   Label:label = [name],
	   Status:statustext = [name],
	   BBPos: buttonbar = '[{horizontal,vertical,vertical_with_menu_above_canvas,none}]',
	   ExtraWndBelow: extraWindowBelow = [window], %an extra window to show below the canvas
	   HelpId: helpId = [name]
	   ) :->
	"Initialise the framedWindow using the given label" ::
	%gp3: buttonbar pos: 
		% vertical_with_menu_above_canvas means, the buttonbar is vertical, but the menubar starts above the canvas
	FW->+initialise(label :=Label, application := @app),
	

	%gp3 0.3.13: we make sure all tiles have 0 border, so we have to set border sizes explicitly in the content
	%objects
	%normaly pce gives tiles a border space of 3, there is no way to set a different border horizontally than verticaly
	

	
	%CLIENT
  	Client *= framedCanvas,
  	FW->>append(Client),
	Client->>name(client), %naam voor terugvinden
	Client->>selection_feedback(invert), %default
	  Client->>ver_stretch(100),
  	Client->>hor_stretch(100),
  	Client->>ver_shrink(100),
  	Client->>hor_shrink(100),  	
	%do we have extra info to put below the client
	unless
		ExtraWndBelow = @default
	do
	(
	 	ExtraWndBelow->>ver_stretch(0),
  		ExtraWndBelow->>hor_stretch(100),
  		ExtraWndBelow->>ver_shrink(0),
  		ExtraWndBelow->>hor_shrink(100), 			
		ExtraWndBelow->>below(Client)
	),

		
	%MENUBAR
	Menuholder *= dialog,
	Menuholder->>name(menuholder),
	Menuholder->>pen(0),
	Menuholder->>gap(size(0,0)),
	Menuholder->>border(size(3,3)), %gp3 0.3.13 we use dialog border instead of tile border
	Menuholder->>ver_stretch(0),
	Menuholder->>hor_stretch(100),
	Menuholder->>ver_shrink(0),
	Menuholder->>hor_shrink(100),
	MB *= framedMenubar(FW),
	Menuholder->>display(MB),
	%en deze informatie bewaren we
	%FW->>keyboard_focus(MB),
	FW->>slot(menubar,MB),	
	 
	%ASSISTANCE
	Assistance *= dialog,
	Assistance->>name(assistance),
	Assistance->>pen(0),
	Assistance->>gap(size(0,0)),
	Assistance->>border(size(0,0)),
	Assistance->>ver_stretch(0),
	Assistance->>hor_stretch(0),
	Assistance->>ver_shrink(0),
	Assistance->>hor_shrink(0),
	Assistance->>right(Menuholder), %even before displaying, to get the tiling right
	
	%create helpButton
	Help *= helpButton(help,HelpId),
	Assistance->>display(Help),
	
	/*
	%code disabled, gives problems on Y-axis!
	%to the right of the assistance, we would like some space for alignment with scroll bar
	SB = Client<<-vertical_scrollbar,
	unless
		SB == @nil
	do
	(
		ScrollbarSpacer *= graphical(0,0,SB?width,1),
		Assistance->>append(ScrollbarSpacer,right)
	),
	*/
	
	%BUTTONBAR
	default(BBPos,none, RBBPos),
	if
		RBBPos = none
	then
		FW->>slot(buttonBar,@nil)
	else
	(
		BB *= commandButtonHolder,
		BB->>border(size(3,0)), %gp3 0.3.13 we use dialog border instead of tile border
		FW->>slot(buttonBar,BB),
		%register ourselves, so we get onIdle calls
		@idleDispatcher->>registerClient(FW)
	),
	
	%STATUSBAR
	Statusholder *= dialog,
	Statusholder->>name(statusholder), %see <-statusbar
	Statusholder->>ver_stretch(0),
	Statusholder->>hor_stretch(100),
	Statusholder->>ver_shrink(0),
	Statusholder->>hor_shrink(100),
	Statusholder->>pen(0),
	Statusholder->>display(new(label(reporter,selection:=Status))),
	Statusholder->>gap(size(0,0)),
	Statusholder->>border(size(3,3)), %gp3 0.3.13 we use dialog border instead of tile border

	
	%timer for statusText:
	FW->>statusTextTimer(new(timer(10,->>(FW,onStatusTextTimer)))),
	%placement
	
	if
		( RBBPos = vertical; RBBPos = vertical_with_menu_above_canvas)
	then
	(
		BB->>ver_stretch(100),
		BB->>ver_shrink(100),
		BB->>hor_stretch(0),
		BB->>hor_shrink(0),
		if
			RBBPos = vertical
		then
		(
			BB->>left(Client),
			Menuholder->>above(BB)
		)
		else
		(
			Menuholder->>above(Client),
			%we need space
			SpacerHolder *= dialog,
			SpacerHolder->>name(spacer),
			SpacerHolder->>pen(0),
			SpacerHolder->>gap(size(0,0)),
			Menuholder->>layout,
			Spacer *= graphical(0,0,1,1), %this is a dummysize, see open
			SpacerHolder->>append(Spacer),
			SpacerHolder->>above(BB),
			BB->>left(Client)
		),
		Statusholder->>below(BB)
	),
	if
		( RBBPos = horizontal)
	then
	(
		BB->>ver_stretch(0),
		BB->>ver_shrink(0),
		BB->>hor_stretch(100),
		BB->>hor_shrink(100),
		BB->>above(Client),
		Menuholder->>above(BB),
		Statusholder->>below(Client)
	),
	if
		RBBPos = none
	then
	(	
		Menuholder->>above(Client),
		Statusholder->>below(Client)
	),

	%gp3 0.3.13: reset the root tile border, which will be used by all tiles
	FW?tile->>border(0),
	
	%het speciale command dat niet geimplementeerde
	%commando's afhandelt
	NIC *= notImplementedCommand(FW, 'NotImplemented'),
	FW->>slot(notImplementedCommand,NIC),

	%commands slot
	FW->>slot(commands,new(hash_table)),

	%%FRAME SETTINGS
	FW->>confirm_done(@off).
%%
	
associateModel(FW, GarpModel) :->
    send(FW, slot, garpModel, GarpModel).
	
%%
open(FW, Pos: [point], Grab: [bool], Norm: [bool] ):->
	%gp3 0.2 Make sure the spacer gets correct height
	FW->+open(Pos,Grab,Norm),
	if
		Sp = FW<<-member(spacer)
	then
		Sp->>height(FW?menuholder_member?height).
%%

%%
destroy(FW):->
	@idleDispatcher->>unregisterClient(FW), %if we were registered, we are not anymore
	FW->+destroy.
%%

%%
onIdle(FW):->
	%gp3 0.3: called by @idleDispatcher when we are the exposed framedWindow
	%we tell our buttonbar to updateStatus
	
	BB = FW<<-buttonBar,
	unless
		BB = @nil
	do
		BB->>updateStatus.
%%

%%
client(FW, 
       Client:window
       ) :<-
	"Return the client window" ::
	
	Client = FW<<-member(client).
%%

%%
statusbar(FW, Statusbar: dialog):<-
	%gp3 0.2 Return the statusbar
	
	Statusbar = FW<<-member(statusholder).
%%

%%
statusText(FW, Text: char_array):->
	%gp3 0.3: set the given text for a view seconds, then clear
	%we use our statusTextTimer
	
	FW?statusbar?reporter_member->>selection(Text),
	FW?statusTextTimer->>start(once). %restart
%%

%%
onStatusTextTimer(FW):->
	%gp3 0.3
	%statusTextTimer was set using statusText, so now we have to clear
	FW?statusbar?reporter_member->>selection('').
%%
	
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
	EE->>report(warning, 'Warning: Command %s is not implemented',
		    @command?name).
%%

%%

key(FW, Key: name):->
	"Handel toetsaanslagen af (doorgestuurd door framedMenubar)" ::
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
%%


%%<-prompter(label,prompt,default, [callbackmsg],[position]).
%Stelt gebruiker korte vraag. Returnt het antwoord. Faalt bij cancel.
%Als er een callback wordt meegestuurd wordt deze bij ok aangeroepen
%met het antwoord in @arg1 en de prompter in @arg2. 
%Als de callback slaagt wordt het antwoord
%teruggegeven, anders blijft de dialoog open.

prompter(FW,
	 Label: label = name,
	 Prompt: prompt = char_array,
	 Default: default = char_array,
	 Message: checkmessage = [code],
	 Position: position = [point],
	 Result: char_array
	) :<-
	"Ask for an answer and return it. Fail when canceled" ::

	Dlg *= dialog(Label),
	Dlg->>application(@app),
	Dlg->>transient_for(FW),
	Dlg->>modal(transient),
	Dlg->>append(new(TI,text_item(Prompt,Default))),
	Dlg->>append(button(ok,->>(FW,
				   prompter_checkOk,
				   Dlg,
				   TI,
				   Message))),
	Dlg->>append(button(cancel,
			    ->>(Dlg,return,@nil))),
	Dlg->>default_button(ok),
	default(Position,FW?area?center,Pos),
	Result = Dlg<<-confirm_centered(Pos),
	Dlg->>destroy,!,
	Result \== @nil.
%
prompter_checkOk(_FW,
		 Prompter: dialog,
		 T: text_item,
		 Message: checkmessage = [code]
		):->
	"Internal helper for ->prompter"::

	%eerst checken of er wel een selectie is
	\+ 0 = T?selection<<-size,
	Result = T<<-selection,
	(   Message == @default
	;   Message->>forward(Result,Prompter)
	),!,
	Prompter->>return(Result).
%%

:-pce_end_class.

:-pce_begin_class(
		  framedCanvas,
		  picture,
		  "Canvas for framedWindow"
		 ).

variable(onScroll,message*,both). %gp3 1.0: callback when scrolled. @arg1 is horizontal or vertical
		 
%%	
scroll_horizontal(FC, D, U, A, F):->
	%overwrite: zorg dat er niet gescrolled wordt als de bar al is weggehaald
	%bugfix voor de xwindows versie
	
	if
		(
			SC = FC<<-horizontal_scrollbar,
			SC \== @nil,
			@on = SC<<-displayed
		)
	then
	(
		FC->+scroll_horizontal(D,U,A,F),
		%gp3 1.0: send callback
		unless 
			@nil = FC<<-onScroll
		do
			FC?onScroll->>forward(horizontal)
	).
%%

%%	
scroll_vertical(FC, D, U, A, F):->
	%overwrite: zorg dat er niet gescrolled wordt als de bar al is weggehaald
	%bugfix voor de xwindows versie
	
	if
	(
		SC = FC<<-vertical_scrollbar,
		SC \== @nil,
		@on = SC<<-displayed
	)
	then
	(
		FC->+scroll_vertical(D,U,A,F),
		%gp3 1.0: send callback
		unless 
			@nil = FC<<-onScroll
		do
			FC?onScroll->>forward(vertical)
	).
%%

:-pce_end_class.

:-pce_begin_class(
	framedMenubar,
	menu_bar,
	"Menu bar for framedWindow"
	).

	variable(frame, framedWindow, get , "The frame this bar belongs to").
	
initialise(FMB,Frame: framedWindow):->
	FMB->+initialise,
	FMB->>slot(frame,Frame).
	


key(FMB, Key: name):->
	unless
		FMB->+key(Key)
	do
		FMB?frame->>key(Key).
	
:-pce_end_class.




:-pce_begin_class(idleDispatcher,object).
%gp3 0.3
%helper class that runs a timer for all framedWindows, to get their buttonbars updated
%the windows register themselves (when needed) with @idleDispatcher
%to make sure load does not go up too much, the dispatcher send idle calls
%to the foreground client (if registered) and only one not-foreground client at the time
%the timer is very fast, so updates will go on and on, but only in idle time
%(single-thread design)

variable(idleTimer,timer,both). %the timer
variable(clients,chain,both). %holding the client
%%
initialise(ID):->
	ID->>clients(new(chain)),
	ID->>idleTimer(new(timer(0.05,->>(ID,onTimer)))),
	ID?idleTimer->>start(once).
%%

%%
registerClient(ID, Client: framedWindow):->
	%register the given framedWindow as client of ID. It will receiver onTimer calls

	ID?clients->>add(Client). %add instead of append: only once
%%
	
%%
unregisterClient(ID, Client: framedWindow):->
	%remove the client as registered for this dispatcher
	ID?clients->>delete_all(Client).
%%

%%
onTimer(ID):->
	%let all clients do their work
	
	
	if
	(
		catch(ActiveWindow = @display_manager<<-window_of_last_event,_,fail),
		catch(ActiveFrame = ActiveWindow<<-frame,_,fail),
		catch(Active = ID?clients<<-find(@arg1 == ActiveFrame),_,fail)
	)
	then
	(
		%this is the active one
		catch(ID->>sendOnIdle(Active),_,true)
	)
	else
		Active = @nil,
	
	if
		Current = ID?clients<<-delete_head
	then
	(
		unless
			Current = Active %al gedaan
		do
		(
			ID->>sendOnIdle(Current)
		),
		ID?clients->>append(Current) %to the back
	),
	
	ID?idleTimer->>start(once). %restart
%%

%%
sendOnIdle(_ID, Current: framedWindow):->
	%implement the on idle call
	S = Current<<-status,
	if
		(S = window ; S = open ; S = full_screen)
	then
		catch(ignore(Current->>onIdle),_,true).
%%

:-pce_end_class.

:-pce_global(@idleDispatcher,new(idleDispatcher)).
