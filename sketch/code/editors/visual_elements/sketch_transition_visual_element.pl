/*
Definitie sketchTransitionVisualElement class:
figuurtje om een sketchTransition mee af te beelden
Anders Bouwer, April 2006
Based on old dutch homer code
2000-2005 Jelmer Jellema, Spin in het Web, spininhetweb.nl
*/

:-pce_begin_class(sketchTransitionVisualElement,
		  sketchDirectRelationVisualElement,
		  "Display of sketchTransitionElement"
		 ).

% based on correspondence_element

%%
initialise(SKDVE,
	   Sk : sketch,
	   SDE: sketchTransitionElement,
		Device: device,
		Arg1: [sketchVisualElement],
		Arg2: [sketchVisualElement],
		FragmentState: '[{normal,imported,parent}]',
		Route: [chain] %zie sketchVisualElement
	  ):->

	SKDVE->+initialise(Sk,SDE,Arg1,Arg2,FragmentState,Route),
	%geen image, het enige dat we hebben is een pijltje (uit de parent class) en daaronder de naam
        % The connection arrow can be selected itself, so no text needed
        swritef(NameStr, 't',[]), 
        % get(SKDVE, displaySketchTransitionName, NameStr), 
	NameText *= text(NameStr),
	NameText->>name(nameText),
	NameText->>font(italic),       	
	SKDVE->>initHandles(SKDVE?area),
	SKDVE->>updateDisplay,
	SKDVE->>doDisplay(Device), %relatie doet het zonder punt
	% SKDVE->>display(NameText), % no matter where - the position is set by the spatial constraints
	% new(_SpatialX, constraint(SKDVE?connection,NameText,identity(center_x))), 
	% new(_SpatialY, constraint(SKDVE?connection,NameText,identity(center_y))), 
	SKDVE->>checkDisplayed, %moeten we wel zichtbaar zijn?
	%gp3 0.2: this element has a tooltip with dynamic text
	SKDVE->>tooltip(@default).
%%



%%
basename(_SKDVE,N:name):<-
	%gp3 0.2
	%depends on the type
	
	Name *= string('sketch_transition'),
	N = Name<<-value.
%%


%
displaySketchTransitionName(VE, S: string):<-
        get(VE?arg1?sketchElement, name, Gr1Name), 
        get(VE?arg2?sketchElement, name, Gr2Name), 
        swritef(S, '%d->%d',[Gr1Name, Gr2Name]).
%%


%
updateDisplay(VE):->
	"Update the display information" ::
        get(VE, connection, C), 
	% Update the connections to the related items - the arguments may be switched
        get(VE, firstArgument, Gr1), 
        get(VE, secondArgument, Gr2), 
        send(C, relate, Gr1, Gr2).
%%



%
tooltipContent(VE,S: string):<-
	%gp3 0.2 define the tooltip content	
        get(VE, displaySketchTransitionName, Str), 
	S *= string('sketch_transition %s', Str).
%%


:-pce_end_class.

