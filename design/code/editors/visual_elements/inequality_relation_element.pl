/*
Definitie inequalityRelationElement class:
figuurtje om een niet lokale inequality mee af te beelden

Part of Garp3, see copyright notice
Most code old dutch homer code

2000-2005 Jelmer Jellema, Spin in het Web, spininhetweb.nl
*/


:-pce_begin_class(inequalityRelationElement,
		  visualRelationElement,
		  "Display of non-local inequality"
		 ).

initialise(IE,
	   Fragment : modelFragment,
	   Inequality: inequality,
		Device: device,
		Arg1: [visualElement],
		Arg2: [visualElement],
		FragmentState: '[{normal,imported,parent}]',
		Route: [chain] %zie visualElement
	  ):->
	%maken van de display
	%gp3 0.2 changed this code
	
	IE->+initialise(Fragment,Inequality,Arg1,Arg2,FragmentState,Route),
	Bitmap *= psBitmap(IE?stateImage),
	Bitmap->>psdef(IE?imageName),
	Bitmap->>name(bitmap),
	IE->>display(Bitmap), %onze bitmap
	CommentText *= text('*'),
	CommentText->>font(normal),
	CommentText->>colour(gray35),
	CommentText->>name(comment),
	IE->>display(CommentText,point(0,0)), %wordt allemaal nog goed gezet in updateDisplay
	IE->>initHandles(Bitmap, isCircle := @on),
	IE->>updateDisplay,
	IE->>doDisplay(Device), %relatie doet het zonder punt
	%gp3 0.2: this element has a tooltip with dynamic text
	IE->>tooltip(@default,model).
%%

%%

%%
basename(IE,N:name):<-
	%gp3 0.2
	%depends on the type and swapped state state
	%also special icons for derivative version
	
	FE = IE<<-fragmentElement,
	
	if
		FE->>isDerivative
	then
		Der = 'd'
	else
		Der = '',
		
	if
		IE->>argumentsSwapped
	then
		Type = FE<<-swappedType
	else
		Type = FE<<-type,
	
	Name *= string('ineq_%s%s',Der, Type),
	N = Name<<-value.
%%

%%
updateDisplay(IE):->
	%gp3 0.2 rewrite: just change the image

	IE?bitmap_member->>image(IE?stateImage),
	IE?bitmap_member->>psdef(IE?imageName),
	if
		0 = IE?fragmentElement?remarks<<-size
	then
		IE?comment_member->>displayed(@off)
	else
	(
		IE->>display(IE?comment_member,point(IE?bitmap_member?right_side + 2,
				IE?bitmap_member?top_side))
	),
	IE->+updateDisplay. %doet de relaties (MOET!)
%%

%%
onElementRelationChange(IE):->
	"Spatial relation between the related elements is changed" ::
	%dus we moeten een andere bitmap afbeelden, en de relaties opnieuw doen
	IE->>updateDisplay.
%%

%%
connectFirstArgument(IE,
	Arg: graphical
	):->
	%overrule. Wanneer de argumenten visueel geswapped zijn
	%moeten we de link ook andersom tekenen

	%als het gewoon de linker naar de relatie is, dan is er geen pijl
	%als het de relatie naar de rechter blijkt te zijn, dan is er wel een pijl

	if
		IE->>argumentsSwapped
	then 
		IE->>connectRightArg(Arg)
	else
		IE->>connectLeftArg(Arg).
%%
%%
connectSecondArgument(IE,
	Arg: graphical
	):->
	%overrule. Wanneer de argumenten visueel geswapped zijn
	%moeten we de link ook andersom tekenen

	if
		IE->>argumentsSwapped
	then
		IE->>connectLeftArg(Arg)
	else
		IE->>connectRightArg(Arg).	
%%

%%
connectLeftArg(IE,Arg: graphical):->
	%helpertje

	Link *= link(link,link,line(0,0,0,0,none)),
	Link->>colour(IE?stateColour),
	Arg->>connect(IE,Link).
%%

%%
connectRightArg(IE,Arg: graphical):->
	%helpertje

	Link *= link(link,link,line(0,0,0,0,second)),
	Link->>colour(IE?stateColour),
	IE->>connect(Arg,Link).
%%	

%%
tooltipContent(VE,S: string):<-
	%gp3 1.4 rewrite of the tooltip
	
	El = VE<<-fragmentElement,
	S = VE<<-checkComments(El?relevantComments).
%%
:-pce_end_class.

