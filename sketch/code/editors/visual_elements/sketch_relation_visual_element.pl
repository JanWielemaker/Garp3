/*
Definitie sketchRelationVisualElement class:
figuurtje om een relation mee af te beelden
*/

:-pce_begin_class(sketchRelationVisualElement,
		  sketchGenericRelationVisualElement,
		  "Display of relation"
		 ).

%%
initialise(CE,
	   Sketch : sketch,
	   Rel: sketchRelationElement,
		Device: device,
		Arg1: [sketchVisualElement],
		Arg2: [sketchVisualElement],
		SketchState: '[{normal,imported,parent}]',
		Route: [chain] %zie sketchVisualElement
	  ):->
	%maken van de display

	CE->+initialise(Sketch,Rel,Arg1,Arg2,SketchState,Route),

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
	CE->>tooltip(@default).
%%

%%
updateDisplay(CE):->
	"Update the display information" ::
	if
		0 = CE?sketchElement?remarks<<-size
	then
		Name = CE?sketchElement<<-name
	else
		Name = CE?sketchElement?name<<-append('*'),
	CE?nameText_member->>string(Name),
	CE?line_member->>left_side(CE?nameText_member?left_side),
	CE?line_member->>right_side(CE?nameText_member?right_side),
	CE?line_member->>colour(CE?stateColour),
	CE->+updateDisplay. %doet de relaties (MOET!)
%%

%%
tooltipContent(AE,S: string):<-
	%gp3 0.2 define the tooltip content
	
	El = AE<<-sketchElement,
	S *= string('%s (Relation)', El?name),
	unless
		0 = El?remarks<<-size
	do
		S->>ensure_nl(El?remarks).
%%

:-pce_end_class.

