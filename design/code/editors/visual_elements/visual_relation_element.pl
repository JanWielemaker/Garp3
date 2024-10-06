/*
Definitie visualRelationElement class:
abstracte paranet van classes voor grafische weergave van relaties in fragmenten. Deze zorgt ervoor dat het relatie element zelf (dus een 
graphical ergens tussen 2 connections in) altijd tussen de twee 
gerelateerde graphicals in blijft, tenzij expliciet er uit gesleept.

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

:-pce_begin_class(visualRelationElement,
		  visualElement,
		  "Abstract parent of visual model fragment relation elements"
		 ).
%%
initialise(VE,
	   Fragment : modelFragment,
	   FragmentElement: garpRelation,
		Arg1: [visualElement],
		Arg2: [visualElement],
	   FragmentState: '[{normal,imported,parent}]',
		Route: [chain] %zie visualElement
	  ):->
	
	VE->>attribute(betweenElements,@off), %voor bepalen positie
	VE->>attribute(inUpdatePosition,@off), %voor opvangen recursie bij bepalen positie
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
	VE->+initialise(Fragment,FragmentElement,@nil,FragmentState,Route).
%%

%%
destroy(VE):->
	%ook eventjes de argumenten op de hoogte stellen, doen we via centrale argumenten methode
	%gp3 0.4.7: we delete arguments and ask them (if they are still there) to update themselves
	%but we do not update ourselves
	VE->>setArguments(updateDisplay := @off),
	VE->+destroy.
%%

%%
%%
imageName(VE,N: name):<-
	%gp3 0.2 get the image name, overridden from visualElement
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
	Name *= string('%s_%s',VE?basename,StateChar),
	N = Name<<-value.
%%

%%
initHandles(VE, Member: [graphical], IsCircle: isCircle = [bool]):->
%gp3 0.2: init the handles. The Member graphical is the symbol
	%for BW-compatibility reasons, there is some code for when this is not
	%given -> we need a member called 'line' and do some different stuff
	% that one can go if all subclasses are rewritten for graphcal symbols

	if
		Member = @default
	then
	(
		%LEGACY
		VE->>slot(handles,@nil),
		VE->>handle(handle(VE?line_member?left_side,VE?line_member?y)),
		VE->>handle(handle(VE?line_member?right_side - 1,VE?line_member?y))
	)
	else
	(
		%current code
		%all
		VE->+initHandles(Member?area, isCircle := IsCircle)
	).
%%

%%
setArguments(VE,
	Arg1: [visualElement],
	Arg2: [visualElement],
	UpdateDisplay : updateDisplay = [bool]):->
	"Set new arguments for the relation" ::

	%gp3 0.4.7: when updateDisplay is given and @off, we do not update our display
	%(used in destroy to make sure we will not refer to our fragmentElement)
	%normally, we do update
	
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
		
	unless
		UpdateDisplay = @off
	do
	(
		VE->>updateDisplay, %gaat via updateDisplay ivm swappen enzo
		VE->>checkDisplayed
	).
%%
	
%%

%%
doDisplay(VE,
	Device : device = [device]
       ):->
	%we gaan displayen, als punt nemen we precies tussen de twee argumenten als we die
	%hebben, of anders ergens in de visual area
	%door de override van placementPointSearchMove hieronder wordt dat redelijk opgevangen

	VisArea = Device<<-visible,
	if
		Arg1 = VE<<-firstArgument
	then
		Arg1Point = Arg1<<-absolute_position
	else
		Arg1Point *= point(VisArea?left_side,VisArea?top_side),

	if
		Arg2 = VE<<-secondArgument
	then
		Arg2Point = Arg2<<-absolute_position
	else
		Arg2Point *= point(VisArea?right_side,VisArea?bottom_side),

	InBetween = Arg1Point<<-mid(Arg2Point),
        get(InBetween, x, X), 
        get(InBetween, y, Y), 
        debug(sketch(layout), 'InBetween: (~w, ~w)\n',[X, Y]),

	VE->+doDisplay(InBetween,Device),
	VE->>checkDisplayed. %moeten we wel zichtbaar zijn?
%%

%%
placementPointSearchMove(_VE,
	Move: point):<-
	%override om te zorgen dat de relatie een beetje naar rechts onder uitkomt als er anders
	%geen plaats is
	Move *= point(3,2).
%%

%%
updateDisplay(VE):->
	%doet alleen de relaties, voor aanroep door subclass implementatie

	VE->>updateRelationLinks.
%%

%%
saveLayoutInfo(VE):->
	%gp3 1.0: overwrite of visualElement saveLayoutInfo: relations save their position relative to
	%mid(arg1,arg2), to make it indepenend of the order of arguments

	if
		Arg1 = VE<<-firstArgument
	then
		Arg1Point = Arg1<<-absolute_position
	else
		Arg1Point *= point(VE?device?visible?left_side,VE?device?visible?top_side),

	if
		Arg2 = VE<<-secondArgument
	then
		Arg2Point = Arg2<<-absolute_position
	else
		Arg2Point *= point(VE?device?visible?right_side,VE?device?visible?bottom_side),	
		
	
	VE?fragment->>layOutInfo(VE?fragmentElement,
			    relPosition,
			    ?(VE?absolute_position,minus,?(Arg1Point,mid,Arg2Point)),
				VE?route),
	VE?fragment->>layOutInfo(VE?fragmentElement,
				hidden,
				VE?hidden,
				VE?route).
%
savedPosition(VE,
	      Position: point
	     ):<-
	%gp3 1.0: overwrite of visualElement
	%gp3 1.0: could be relative to mid(arg1,arg2) (new style) or just absolute (old style)
	
	%we expect an Arg1 and Arg2, otherwise we just fail
	
	Position = ?(VE?firstArgument?absolute_position,mid,VE?secondArgument?absolute_position)<<-plus(?(VE?fragment,layOutInfo,VE?fragmentElement, relPosition,VE?route)).
%%

%%
stateColour(VE, C:colour):<-
	%still needed for the links
	FragState = VE<<-fragmentState,
	if
		FragState == normal
	then
	(
		if
			VE->>isCondition
		then
			C *= colour(red)
	    else
	    	C *= colour(blue)
	    
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

	%gp3 1.0: we make sure this call fails when there is no movement
	%so we can loop in viewEditor until there is no change (see viewEditor dragMultiMove)
	\+ Moved->>member(VE),
	Moved->>member(VE?firstArgument),
	Moved->>member(VE?secondArgument),
	%dus als hier gekomen: toevoegen aan moved, en verplaatsen
	Moved->>append(VE),
	ignore(VE->>relative_move(Change)).
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
	"Make sure the relation stays between the two elements" ::
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
onElementRelationChange(_VE):->
	"Spatial relation between the related elements is changed" ::
	%overrulebaar, default: niets 

	true.
%%

%%
checkBetweenElements(VE):->
	Left = VE<<-leftArgument,
	Right = VE<<-rightArgument,
	LL = Left<<-right_side,
	L = VE<<-left_side,
	RR = Right<<-left_side,
	R = VE<<-right_side,
	L >= LL,
	R =< RR,!,
	VE->>attribute(betweenElements,@on).
%
checkBetweenElements(VE):->
	VE->>attribute(betweenElements,@off).
%%

%%
checkArgumentLayout(VE):-
	Left = VE<<-leftArgument,
	XL = Left<<-right_side,
	X = VE<<-left_side,
	X < XL,
	VE->>x(XL + 20). %verplaatsen
%

checkArgumentLayout(VE):-
	%misschien rechts van de rechter?
	Right = VE<<-rightArgument,
	XR = Right<<-left_side,
	X = VE<<-right_side,
	X > XR,
	VE->>x(XR-20).
%
checkArgumentLayout(_).
%%

%%
updateRelationLinks(VE):->
	"Update the connections to the related items" ::
	%dit niet overrulen

	VE->>disconnect, %alles weg
	(  F = VE<<-firstArgument
	-> VE->>connectFirstArgument(F)
    ;  true
	),
	(  S = VE<<-secondArgument
	-> VE->>connectSecondArgument(S)
	;  true
	).
%%

%%
connectFirstArgument(VE,Arg : graphical):->
	"Connect the first argument to the relation object" ::
	%bedoeld om te overrulen als nodig (want geen pijl)

	Link *= link(link,link,line(0,0,0,0,none)),
	Link->>colour(VE?stateColour),
	Arg->>connect(VE,Link).
%%

%%
connectSecondArgument(VE, Arg : graphical):->
	"Connect the second argument to the relation object" ::
	%ook weer overrule-baar

	Link *= link(link,link,line(0,0,0,0,second)),
	Link->>colour(VE?stateColour),
	VE->>connect(Arg,Link).
%%

%%
argumentsSwapped(VE):->
	"Succeeds if the 1st arguments is displayed right from the second" ::

	First = VE<<-firstArgument,
	First = VE<<-rightArgument. %thats it
%%

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
