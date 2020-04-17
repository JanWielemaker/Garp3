/*
Definitie identityRelationElement class:
figuurtje om een identity mee af te beelden
*/

:-pce_begin_class(identityRelationElement,
		  visualRelationElement,
		  "Display of identityRelation"
		 ).

%%
initialise(CE,
	   Fragment : modelFragment,
	   Identity: identityRelation,
		Device: device,
		Arg1: [visualElement],
		Arg2: [visualElement],
		FragmentState: '[{normal,imported,parent}]',
		Route: [chain] %zie visualElement
	  ):->
	%maken van de display

	CE->+initialise(Fragment,Identity,Arg1,Arg2,FragmentState,Route),

	%geen image, het enige dat we hebben is een lijntje met daaronder een =
	Line *= line(0,0,0,0),
	Line->>name(line),

	NameText *= text('=='),
	NameText->>name(nameText),
	NameText->>font(bold),
		
	CE->>display(Line),
	CE->>display(NameText,point(0,Line?bottom_side)),
	CE->>initHandles,
	CE->>updateDisplay,
	CE->>doDisplay(Device), %relatie doet het zonder punt
	%gp3 0.2: this element has a tooltip with dynamic text
	CE->>tooltip(@default,model).
%%

%%
updateDisplay(CE):->
	"Update the display information" ::

	if
		0 = CE?fragmentElement?remarks<<-size
	then
		Name = '=='
	else
		Name = '==*',
	CE?nameText_member->>string(Name),
	CE?line_member->>left_side(CE?nameText_member?left_side),
	CE?line_member->>right_side(CE?nameText_member?right_side),
	CE?line_member->>colour(CE?stateColour),
	CE?line_member->>pen(2),
	CE->+updateDisplay. %doet de relaties (MOET!)
%%

%%
connectFirstArgument(VE,Arg : graphical):->
	%overrule

	Link *= link(link,link,line(0,0,0,0,none)),
	Link->>colour(VE?stateColour),
	Link?line->>pen(2),	
	Arg->>connect(VE,Link).
%%

%%
connectSecondArgument(VE, Arg : graphical):->
	%overrule

	Link *= link(link,link,line(0,0,0,0,none)),
	Link?line->>pen(2),	
	Link->>colour(VE?stateColour),
	VE->>connect(Arg,Link).
%%

stateColour(VE, C:colour):<-
	%overwrite voor normaal
	if
		normal = VE<<-fragmentState
	then
		C = colour(yellow)
	else
		C = VE+<-stateColour.

%%
tooltipContent(VE,S: string):<-
	%gp3 1.4 rewrite of the tooltip
	
	El = VE<<-fragmentElement,
	S = VE<<-checkComments(El?relevantComments).
%%
:-pce_end_class.

