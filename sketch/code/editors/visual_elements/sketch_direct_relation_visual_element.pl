/*
Definitie sketchDirectRelationVisualElement class:

abstracte parent van classes voor grafische weergave van directe relaties in
sketches, zonder icon in het midden.
AB, april 2006

Subclasses kunnen onElementRelationChange implementeren, deze wordt aangeroepen wanneer de twee 
%gerelateerde elementen swappen (links<->rechts).
%Op deze manier kunnen de relaties anders worden weergegeven: updateRelationLinks kan worden aangeroepen,
%die via connectFirstArgument en connectSecondArgument de argumenten connect.

Die twee connectFirstArgument / connectSecondArgument kunnen dus overruled worden. Hierbij moet
wel de kleur van de link goed gezet worden.

updateDisplay MOET updateRelationLinks aanroepen (ivm de kleur) of send_super gebruiken.
worden geïmplementeerd.
 
Zie visualElement

Part of Garp3, see copyright notice
Most code old dutch homer code

2000-2005 Jelmer Jellema, Spin in het Web, spininhetweb.nl
*/

:-pce_begin_class(sketchDirectRelationVisualElement,
		  sketchVisualElement,
		  "Abstract parent of visual sketch relation elements"
		 ).

%%
initialise(VE,
	   Sketch : sketch,
	   SketchElement: sketchGenericRelationElement,
		Arg1: [sketchVisualElement],
		Arg2: [sketchVisualElement],
	   SketchState: '[{normal,imported,parent}]',
		Route: [chain] %zie sketchVisualElement
	  ):->
	
	if
		(Arg1 \== @default)
	then (
		VE->>hyper(Arg1,arg1,relation),
		Arg1->>checkImportant
		),
	if
		(Arg2 \== @default)
	then (
		VE->>hyper(Arg2,arg2,relation),
		Arg2->>checkImportant
		),
	VE->+initialise(Sketch,SketchElement,@nil, SketchState,Route).
%%

%%
destroy(VE):->
	%ook eventjes de argumenten op de hoogte stellen, doen we via centrale argumenten methode
	% VE->>setArguments, % let's forget about this for the moment, AB june 2006
        % Sometimes, the connection is already gone - not sure if that is ok? AB, sept 2006     
	if 
           get(VE, connection, C)
        then 
           send(C, destroy),
	VE->+destroy.
%%

%%
imageName(VE,N: name):<-
	%gp3 0.2 get the image name, overridden from sketchVisualElement
	%called from <-stateImage
	%difference: no displayChar here
	%and stateChar for 'p' and 'i' is allways 'i'
	%basename is set in the subclasses
	
	SC = VE<<-stateChar,
	if
		SC = p
	then
		StateChar = i
	else
		StateChar = SC,
	Name *= string('%s',VE?basename),
	% Name *= string('%s_%s',VE?basename,StateChar),
	N = Name<<-value.
%%

%%
setArguments(VE,
	Arg1: [sketchVisualElement],
	Arg2: [sketchVisualElement]):->
	"Set new arguments for the relation" ::

	%maar we melden de elementen wel dat we weggaan
	if
		OldArg1 = VE<<-firstArgument
	then (
		VE->>delete_hypers(arg1),
		OldArg1->>checkImportant,
		OldArg1->>checkHiddenRelations
		),

	if
		OldArg2 = VE<<-secondArgument
	then (
		VE->>delete_hypers(arg2),
		OldArg2->>checkImportant,
		OldArg2->>checkHiddenRelations
		),

	if
		(Arg1 \== @default)
	then (
		VE->>hyper(Arg1,arg1,relation),
		Arg1->>checkImportant,
		Arg1->>checkHiddenRelations
		),
	if
		(Arg2 \== @default)
	then (
		VE->>hyper(Arg2,arg2,relation),
		Arg2->>checkImportant,
		Arg2->>checkHiddenRelations
		),

        % this line seems to fail for some reason, AB june 2006
	VE->>updateDisplay, %gaat via updateDisplay ivm swappen enzo
	VE->>checkDisplayed.
%%
	
%%
doDisplay(VE,
	Device : device = [device]
       ):->
        % this type of relation does not have a movable element, 
        % and is just visualized as a straight connection
        send(VE, updateDisplay),
	VE->+doDisplay(point(0, 0), Device),
        get(VE, connection, _C), 
	VE->>checkDisplayed. %moeten we wel zichtbaar zijn?
%%


%%
updateDisplay(VE):->
	%doet alleen de relaties, voor aanroep door subclass implementatie
	VE->>updateRelationLinks.
%%

%%
stateColour(VE, C:colour):<-
	%still needed for the links
	FragState = VE<<-sketchState,
	if
		FragState == normal
	then
	(
	    	C *= colour(gray70)
	    
	)
	else
			C *= colour(gray70). %all other states
%%


%%
checkDragMove(VE,
	Change: point,
	Moved: chain):->
	
	%check of de relatie moet moven: zitten we zelf niet, en onze argumenten wel in 
	%Moved?
	
	\+ Moved->>member(VE),
	Moved->>member(VE?firstArgument),
	Moved->>member(VE?secondArgument),
	%dus als hier gekomen: toevoegen aan moved, en verplaatsen
	Moved->>append(VE),
	VE->>relative_move(Change).
%%


%%
relatedGeometryChange(VE,
					  _E: graphical
					  ):->
	"React on ->geometry of related element" ::
	%we gaan kijken wat we met de position enzo moeten doen
	%niet overrulen!

	VE->>updatePosition.
%%


%%
geometry(VE,
	 X : x = [int], Y : y = [int],
	 W : width = [int], H : height = [int]):->
	%ook in dit geval kijken of we de positie moeten updaten
    %niet overrulen!

	@off = VE<<-attribute(inUpdatePosition),!,
	VE->+geometry(X,Y,W,H),
	VE->>checkBetweenElements, %zijn we nog tussen de gerelateerde elementen?
	VE->>updatePosition. %en eens kijken hoe het er mee staat
%
geometry(VE,
	X, Y, W, H
	):->
	VE->+geometry(X,Y,W,H).
%%

%%
updatePosition(VE):->
	"Make sure the relation stays between the to elements" ::
	%niet overrulen!

	@off = VE<<-attribute(inUpdatePosition),!, %recursiestop
	VE->>attribute(inUpdatePosition,@on),
	(   @on = VE<<-attribute(betweenElements) %stonden we er tussen in
	->  checkArgumentLayout(VE) %staan we er nog wel tussen in
	;   true
	),
	(   LastLeft = VE<<-attribute(savedLeftArgument)
	;   LastLeft = @nil
	),
	(   CurrentLeft = VE<<-leftArgument
	;   CurrentLeft = @nil
	),
	(   LastLeft \== CurrentLeft %een andere
	->  VE->>onElementRelationChange
	;   true
	),
	%als we zopas vonden dat we tussen de elementen zaten, dan vinden we dat nu nog
	(   @on = VE<<-attribute(betweenElements)
	;   VE->>checkBetweenElements
	),
	%en we moeten opslaan wie er nu links is
	VE->>attribute(savedLeftArgument,CurrentLeft),
	VE->>attribute(inUpdatePosition,@off).

%
updatePosition(_VE):->
	%blijkbaar al mee bezig
	true.
%%


%%
updateRelationLinks(VE):->
	"Update the connections to the related items" ::
        get(VE, firstArgument, Gr1), 
        get(VE, secondArgument, Gr2),          
        get(VE, connection, C),     
        get(Gr1?sketchElement, name, Gr1Name), 
        get(Gr2?sketchElement, name, Gr2Name), 
        swritef(Str, 'sketch_transition%sto%s',[Gr1Name, Gr2Name]), 
        send(C, colour, VE?stateColour), 
	send(C, name, Str).
%%

connection(VE, C):<-    
        get(VE, hypered, connection, C).

% if the previous clause fails, create a new one
connection(VE, C):<-    
        get(VE, firstArgument, Gr1), 
        get(VE, secondArgument, Gr2), 
        get(VE, sketchElement, SE), 
	new(C, my_sketch_connection(Gr1, Gr2, SE, @graph_link2)),
	VE->>hyper(C, connection, connection).


%%
firstArgument(VE,
	Arg : graphical):<-
	"Return the graphical displaying the first relation argument" ::

	Arg = VE<<-hypered(arg1).
%%
	
%%
secondArgument(VE,
	Arg : graphical):<-
	"Return the graphical displaying the second relation argument" ::

	Arg = VE<<-hypered(arg2).
%%
	
%%
leftArgument(VE,
	     Left : graphical
	    ):<-
	"Returns the most left of the two arguments" ::
	%zodat we het beter kunnen visualiseren

	Arg1 = VE<<-firstArgument,
	Arg2 = VE<<-secondArgument,
	X1 = Arg1<<-left_side,
	X2 = Arg2<<-left_side,
	(   X1 < X2
	->  Left = Arg1
	;   Left = Arg2
	).
%%

%%
rightArgument(VE,
	     Right : graphical
	    ):<-
	"Returns the most right of the two arguments" ::
	%zodat we het beter kunnen visualiseren

	%doen we via left trouwens

	Arg1 = VE<<-firstArgument,
	(  Arg1 = VE<<-leftArgument
	-> Right = VE<<-secondArgument
	;  Right = Arg1
	).
%%

%%
canHide(_VE):->
	%relaties kunnen altijd verborgen worden
	true.
%%

%%
checkDisplayed(VE):->
	%hide wanneer één vd twee argumenten hidden is of wanneer onze hidden flag staat

	if (
		@on = VE?firstArgument<<-displayed,
		@on = VE?secondArgument<<-displayed,
		@off = VE<<-hidden
		)
	then 
		Displayed = @on
	else
		Displayed = @off,

	if
		(\+ Displayed = VE<<-displayed)
	then (
		VE->>displayed(Displayed),
		ignore(VE?firstArgument->>checkHiddenRelations),
		ignore(VE?secondArgument->>checkHiddenRelations)
		).
%%

%%
ensureVisible(VE):->
	%zorgt dat we zichtbaar zijn, dat betekent bij een relatie dat beide argumenten zichtbaar moeten zijn
	%en onze hidden flag uit moet staan

	if
		@off = VE<<-displayed
	then (
		VE->>setHidden(@off),
		VE?firstArgument->>ensureVisible,
		VE?secondArgument->>ensureVisible
		).
%%
		
:-pce_end_class.
