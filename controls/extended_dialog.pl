/*
extendedDialog / assistanceDialog class
part of GARP3

Make your dialog a subclass of this one, and you get some handy extras

%resize message:
%instead of the normal resize message, you can use onResize, which works better
%(because it waits a bit and you get the sizechange).
%either overwrite onResize or use onResizeMessage(@arg1) where @arg1 is the sizechange

%assistanceDialog is an extendedDialog subclass, that has support for assistance bar at the top (help button right now)

2006 Jelmer Jellema - Spin in het Web


*/

:-pce_begin_class(extendedDialog,
		  dialog
		 ).


variable(resizeTimer,timer,both). %gp3 0.2
variable(lastSize,size*, both).
variable(minimalSize,size*, both). %minimal size
variable(onResizeMessage, code*, both). %use this if you do not have your own class, otherwise overwrite onResize. Message gets sizechange as as size in @arg1

variable(commands,hash_table:=new(hash_table),get). % JL

%variable(garpModel, garpModel, both, 'Model associated to the dialog'). % JL Garp3 1.4.1

%%

%%
initialise(D, Label: name):->
	
	D->+initialise(Label),
	D->>resize_message(->>(@receiver,preResize)), %gp3 0.2: allways implement resize event.
	D->>resizeTimer(new(timer(0.01,->>(D,doResize)))),
	D->>keyboard_focus(@nil),
	D->>sensitive(@on). % JL
%%

%%
preResize(D):->
	%gp3 0.2
	%set a timer to create an onResize call
	
	D?resizeTimer->>stop,
	D?resizeTimer->>start(once).
%%

%%
doResize(D):->
	%gp3 0.2
	%calculate changes and call overridable event call
	
	DSize *= size(0,0),
	NewSize = D?area<<-size,
	NewWidth = NewSize<<-width,
	NewHeight = NewSize<<-height,
	
	%below minimal?
	unless
		(@nil = D<<-minimalSize)
	do
	(
		MinWidth = D?minimalSize<<-width,
		MinHeight = D?minimalSize<<-height,
		SetSize *= size, SetSize->>copy(NewSize),
		
		if
			NewWidth < MinWidth
		then
			SetSize->>width(MinWidth),
		if
			NewHeight < MinHeight
		then
			SetSize->>height(MinHeight),
		%problems?
		unless
			SetSize->>equal(NewSize)
		do
		(
			SizeChanged = true,
			D->>size(SetSize) %and we do not set anything here, so the differences will be ok
		)
		else
		(
			SizeChanged = false
		)
	)
	else
		SizeChanged = false,
			
	%continue?
	if
		SizeChanged == false %meaning: we rechanged the size
	then
	(
		unless
			@nil = D<<-lastSize
		do
		(
			DSize->>width(NewSize?width - D?lastSize?width),
			DSize->>height(NewSize?height - D?lastSize?height)
		),
		D->>lastSize(NewSize),
		
		D->>callOnResize(DSize),
		D->>redraw %needed because pce will keep some traces of the elements at old places
	).
%%

%%
callOnResize(D,Difference: size):->
	%gp3 0.3.13
	%helper to call onResize. Subclass (assistanceDialog) can overwrite this one and be sure
	%it is called, while other subclasses can override onResize without calling baseclass version
	%and having to deal with deciding when and how
	
	%base class just calls:
	ignore(D->>onResize(Difference)).
%%

%%
onResize(D, Difference: size):->
	%gp3 0.2
	%called after resize and after timer fired
	%overwrite this one in your own subclass, (no need to call super)
	%or, if you do not have your own subclass, set onResizeMessage,
	%we will call that one here
	
	unless
		@nil = D<<-onResizeMessage
	do
		D?onResizeMessage->>forward(Difference).
%%


%%
gapX(D, G: int):<-
	G = D?gap<<-width.
%%

%%
gapY(D,G: int):<-
	G = D?gap<<-height.
%%


%%
destroy(D):->
	%gp3 0.2
	%make sure the timer is stopped

	D?resizeTimer->>stop,
	D->+destroy.
%%

/* %JL Added to allow for key combination presses */
%%
command(D, %JL
	Name : name = name, 
	Run : runnable = [bool],
	Key: key = [name],
	Keystring: keystring = [name],
	Otherkeys: otherkeys = [chain] %otherkeys this command reacts to
	) :->
	"Create a new command for this frame" ::
	%Argument Run vertelt ons of het commando wel mag runnen. Als dit @off is
	%zal wel update en check gedaan worden, maar niet execute. Default is @on

	C *= command(D,Name,Run,Key, Keystring, Otherkeys),
	%dit stoppen we in de commands chain
	D?commands->>append(Name,C). 
%%

%%	
command(D, % JL
	Name: name,
	Command: command) :<-
	"Return an existing command with the given name" ::

	Command = D?commands<<-member(Name).
%%

%%
onNotImplemented(D) :-> % JL
	"Default handler for not implemented commands"::

	D->>bell,
	D->>report(warning, 'Warning: Command %s is not implemented',
		    @command?name).
%%

%%
key(D, Key: name):-> % JL
	"Handel toetsaanslagen af (doorgestuurd door framedMenubar)" ::
	%gp3 0.2 Now commands can react to more than 1 key
	%so this is a bit rewritten
	%vind een command die deze key wil hebben

	(
	    Command = D?commands<<-find_value(->>(@arg2,wantsKey,Key)),
	    Command->>canRun(D) ->
	    Command->>execute(D)
	;
	    true
	).
%%

%%
post_event(D, Event) :-> % JL
    %get(Event, id, EventID),
    get(Event, key, KeyName),
    %format('The key event is: ~w\n', [KeyName]),
    send(D, key, KeyName),
    send_super(D, post_event, Event).


%%some VERY general changeApplied calls
%%only called when the dialog is registered thru dialog->>application(@app)

changeApplied_setCurrentLanguage(D,
	_CR: changeRequestor):->
	
	%we close
	D->>destroy.
%%

:-pce_end_class.

:-pce_begin_class(assistanceDialog,
		  extendedDialog
		 ).

variable(garpModel, garpModel, both, 'Model associated to the dialog'). % JL Garp3 1.4.1

%gp3 0.3.13: we draw our own box instead of the default border. Because helpbutton can be shown above it
% When helpId is given, we will show the helpbutton
%
%you should not display any content in initialise, use displayContent for this (overwrite it). It is called by this class at the right moment. When you have to draw in initialise or something like that (or no subclass), call updateSpacers after drawing, and use extendedDialog?topY to get the spot to start drawing on the Y-axis.
%Backgroundcolours within the border should be set in initialise

	initialise(D, Label: name, HelpId: [name]*, BorderBackgroundColour: bgColour = [colour]):->
	
	D->+initialise(Label),
	%default colours etc
	%we draw our own border, and possibly a helpbutton
	
	D->>pen(0),
	D->>border(size(0,0)), %needed because we draw the border ourself: we add some space
	
	GY = D?gap<<-height,
	GX = D?gap<<-width,
	
	if
		(HelpId == @nil ; HelpId == @default)
	then
		BorderTop = 1
	else
	(
		Help *= helpButton(extendedDialog_HelpButton,HelpId),
		D->>display(Help,point(0,0)),
		BorderTop = Help<<-bottom_side
	),
	BorderRect *= box(D?width - 2,D?height - BorderTop - 1), %space above is calculated using BorderTop
	BorderRect->>name(extendedDialog_BorderRect),
	unless
		BorderBackgroundColour == @default
	do
		BorderRect->>fill_pattern(BorderBackgroundColour),
	BorderRect->>colour(black),
	BorderRect->>pen(1),
	D->>display(BorderRect,point(1,BorderTop)),
	
	%gp3 0.3.13 display the content (classes should overrule this and do all their display here)
	TopY is BorderTop + GY,
	D->>displayContent(TopY),
	%afterward, add spacers to the right and bottom side
	Right *= graphical(0,0,GX,1),
	Right->>name(extendedDialogRightSpacer),
	Bottom *= graphical(0,0,1,GY),
	Bottom->>name(extendedDialogBottomSpacer),
	D->>display(Right),
	D->>display(Bottom),
	D->>updateSpacers.
%%

%%
associateModel(G3EF, GarpModel) :->
    send(G3EF, slot, garpModel, GarpModel).
%%

%%
displayContent(_D,_StartY: int):->
	%gp3 0.3.13. subclasses should overrule this one and draw their content overthere
	%StartY is the place on the Y-axis where the first element could be placed (below assistance stuff)
	
	true.
%%

%%
updateSpacers(D):->
	%gp3 0.3.13 Make sure the spacers are on the right spot. Called from layout and doResize
	
	MaxX *= number(0),
	MaxY *= number(0),
	Right = D<<-member(extendedDialogRightSpacer),
	Bottom = D<<-member(extendedDialogBottomSpacer),
	(
		Help = D<<-member(extendedDialog_HelpButton)
	;
		Help = @nil
	),
	Box = D<<-member(extendedDialog_BorderRect),
	
	D?members->>for_all(if(
		and(@arg1 \== Right, @arg1 \== Bottom, @arg1 \== Help, @arg1 \== Box, @arg1?displayed == @on),
		and(
			->>(MaxX,maximum,@arg1?right_side),
			->>(MaxY,maximum,@arg1?bottom_side)
		))),
	Right->>set(x:=MaxX),
	Bottom->>set(y:= MaxY).
%%

%%
fit(D):->
	%overwrite. Make sure the window fits the contents except for the borderbox + help, which will be fitted
	%after a resize...
	
	D->>updateSpacers, %reupdate the spacers
	D?extendedDialog_BorderRect_member->>size(size(1,1)), %small,
	D?extendedDialog_HelpButton_member->>set(x:= 0),
	D->+fit. %will resize the dialog and the borderrect in resize
%%
topY(D, Y: int):<-
	%get first Y to draw something (depends on gap + display of help)
	%use this one when displayContent is not used.
	if
		Help = D<<-member(extendedDialog_HelpButton)
	then
		StartY = Help<<-bottom_side
	else
		StartY = 0,
	
	GY = D?gap<<-height,
	Y is StartY + GY.
%%

%%
leftTop(D,LeftTop: point):<-
	%a bit like topY but without gaps etc
	%just displays the topleft point inside the box. Handy for buttonbars etc.
	
	BorderRect = D<<-member(extendedDialog_BorderRect),
	LeftTop *= point(BorderRect?left_side,BorderRect?top_side).
%%
	
%%
helpId(D,HelpId: name):->
	%*change* the helpid
	%fails if help was not present when created, so when needed create the dialog with a fake help id
	
	D?extendedDialog_HelpButton_member->>helpId(HelpId).
%%

%%
callOnResize(D,Difference: size):->
	%overwrite of superclass: we have to resize some stuf ourselves and cannot depend
	%on subclasses implementing onResize or not
	
	%set our border + help
	BorderRect = D<<-member(extendedDialog_BorderRect),
	if
		Help = D<<-member(extendedDialog_HelpButton)
	then
	(
		Help->>set(x:= D?width - 1 - Help?width),
		BorderRect->>size(size(D?width - 2, D?height - Help?bottom_side - 1))
	)
	else
		BorderRect->>size(size(D?width - 2, D?height - 2)),
	D->+callOnResize(Difference),
	D->>updateSpacers.
%%

:-pce_end_class.
