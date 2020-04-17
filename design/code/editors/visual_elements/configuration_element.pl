/*
Definitie configurationElement class:
figuurtje om een configuration mee af te beelden
*/

:-pce_begin_class(configurationElement,
		  visualRelationElement,
		  "Display of configuration"
		 ).

%%
initialise(CE,
	   Fragment : modelFragment,
	   Config: configuration,
		Device: device,
		Arg1: [visualElement],
		Arg2: [visualElement],
		FragmentState: '[{normal,imported,parent}]',
		Route: [chain] %zie visualElement
	  ):->
	%maken van de display

	CE->+initialise(Fragment,Config,Arg1,Arg2,FragmentState,Route),

	%geen image, het enige dat we hebben is een lijntje met daaronder de naam
	Line *= line(0,0,0,0),
	Line->>name(line),

	NameText *= text(''),
	NameText->>name(nameText),
	NameText->>font(italic),
		
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
		Name = CE?fragmentElement<<-name
	else
		Name = CE?fragmentElement?name<<-append('*'),
	CE?nameText_member->>string(Name),
	CE?line_member->>left_side(CE?nameText_member?left_side),
	CE?line_member->>right_side(CE?nameText_member?right_side),
	CE?line_member->>colour(CE?stateColour),
	CE->+updateDisplay. %doet de relaties (MOET!)
%%

%%
tooltipContent(VE,S: string):<-
	%gp3 1.4 rewrite of the tooltip
	
	El = VE<<-fragmentElement,
	S = VE<<-checkComments(El?definition?relevantComments,El?relevantComments).
%%
:-pce_end_class.

