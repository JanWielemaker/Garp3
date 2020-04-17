/*
Definitie visualElement class:
abstracte parent class van de grafische weergave van fragment elementen

Part of Garp3 See copyright notice
2000 - 2005, Jelmer Jellema - Spin in het Web - spininhetweb.nl

Most code is legacy homer code, so dutch comments
*/

:-pce_begin_class(visualElement,
		  figure,
		  "Abstract parent of visual model fragment elements"
		 ).

variable(fragmentState,{normal, %dus een conditie of een consequentie
			imported, %geïmporteerd uit een ander fragment
			parent, %geïmporteerd uit een parent fragment
			fixed %een vast element, dat altijd aanwezig is
		       },get,"State of the element in the fragment it is displayed in"
	).

/*
gp3 0.2: we no longer use colour coding, we now use individual images
for all different combinations of states.
Image is <basename>_<state>_<displaystate>.
	Basename is found through ?basename
	state is: c,g,i,p,f for condition, given, imported, parent, fixed.
	Displaystate is a,r,i: all subs visible, relevant subs hidden, only irrelevant subs hidden
In this abstract class, we define a ?basename for missing code. We also define the code to get the right image or - when not found - display a missing-image image
*/

variable(isImportant,bool,both). %@on als het element zelf important is (zoals de laaste keer bepaald door checkImportant)
							%alleen van belang voor de vraag of de super ingelicht moet worden in checkImportant

variable(hiddenSubs,'{none,unimportant,important}',both). %geeft status met subs aan
variable(importantSub,bool,both). %@on als we een sub hebben die we als important kennen

variable(hidden,bool,both). %@on als we verborgen zijn door de gebruiker
variable(hiddenRelation,bool,both). %@on als relaties verborgen zijn

variable(route,chain,both). 
/*Dit is een chain van importedFragment objecten die vanaf het bewerkte fragment de route
  naar dit element weergeven. Dit is niet hetzelfde als route verwijzingen in fragmentElement
  objecten, maar wanneer een fragmentElement gaat verwijzen naar het element dat door dit
  object wordt afgebeeld, dan moet deze route worden opgeslagen. Zie viewEditor->draw_fragment
*/							

%%
initialise(VE,
	   Fragment : modelFragment, %het model fragment waar dit voor getekend wordt
	   FragmentElement : any, %het fragment-element waaraan dit visuele ding gebonden is
	   SuperElement : visualElement*, %gp 3 1.0: we need a structure for this
	   FS : fragmentState = '[{normal,imported,parent,fixed}]', %default = normal
	   Route: [chain]
	  ):->
	
	VE->+initialise,
	default(FS,normal,RFS),
	VE->>slot(fragmentState,RFS),
	VE->>name(VE?class_name), %voor zoeken in de editor code
	VE->>hyper(FragmentElement,fragmentElement),
	VE->>hyper(Fragment, fragment),
	if
		Route == @default
	then
		VE->>route(new(chain))
	else
		VE->>route(Route?copy),
	%gp3 1.0: we register the super right away, just to make sure it's available (to savedPosition etc)
	unless
		SuperElement == @nil
	do
		VE->>hyper(SuperElement,super,sub),
	VE->>slot(hidden,VE?savedHidden),
	VE->>hiddenSubs(none), %uitgangspunt
	VE->>importantSub(@off), %uitgangspunt
	if
		VE->>important
	then
		VE->>isImportant(@on)
	else
		VE->>isImportant(@off),
	if
		VE?calc_and_relations->>find(@arg1?displayed == @off)
	then
		VE->>hiddenRelation(@on)
	else
		VE->>hiddenRelation(@off).

%%

destroy(VE):->
	%meldt onze super dat we weg zijn na onze destroy
	if
		Sup = VE<<-super
	then
		Super = Sup
	else
		Super = @nil,

	%als er een relation marker is moet ie weg
	if
		RM = VE<<-hypered(relationMarker)
	then
		RM->>destroy,

	VE->+destroy,
	if
		Super \== @nil
	then
		ignore(Super->>checkImportant). %moet uitzoeken wat er nog onder hangt
	
%%

%%
basename(_VE, N: name):<-
	%gp3 0.2 semi-abstract call, should be overwriten
	%gives the basename of the image for the element
	
	N = missing.
%%

%%
stateImage(VE, I: image):<-
	%gp3 0.2 Get the right image for the element, given state etc
	
	Name = VE<<-imageName,
	unless
		get_image(elements,Name,I)
	do
	(
		get_image(elements,missing,I)
	).
%%

%%
imageName(VE,N: name):<-
	%gp3 0.2 get the image name, overridable, overridden in visualRelationElement
	
	Name *= string('%s_%s_%s',VE?basename,VE?stateChar,VE?displayChar),
	N = Name<<-value.
%%

%%
stateChar(VE, C:name):<-
	%gp3 0.2 Get the char indicating the state in the image name
	FragState = VE<<-fragmentState,
	(   FragState == normal
	->  (   VE->>isCondition %te schrijven bij de subclasses
	    ->  C = c
	    ;   C = g
	    )
	;   FragState == fixed
	->  C = f 
	;   FragState == imported
	->  C = i
	;   FragState == parent
	->  C = p
	).
	

%%definitie van kleuren, voor als er een kleur nodig is
stateColour(VE, C:colour):<-
	%old homer code, should be replaced by using image
	%(but some elements might still need it)
	%redefined in visualRelationElement
	"Return the color for drawing the state"::
	FragState = VE<<-fragmentState,
	(   FragState == normal
	->  (   VE->>isCondition %te schrijven bij de subclasses
	    ->  Name = red
	    ;   Name = blue
	    )
	;   FragState == fixed
	->  Name = gray35 
	;   FragState == imported
	->  Name = black
	;   FragState == parent
	->  Name = forestgreen
	),
	new(C, colour(Name)).
%%


displayChar(VE, C: name):<-
	%gp3 0.2 Get the char indicating the display state in the image name
	if
		none = VE<<-hiddenSubs
	then
		C = a
	else (
		if
		(
			important = VE<<-hiddenSubs
		)	
		then
			C = r
		else
			C = i
		).	
	
%%
%gp3 0.2: changed some defaults: all are @on
initHandles(VE,
	    A: handleArea = area,
	    TopLeft : topLeft = [bool], %on
	    TopCenter: topCenter = [bool], %@on
	    TopRight: topRight = [bool], %@on
	    CenterLeft: centerLeft = [bool], %@on
	    CenterRight: centerRight = [bool], %@on
	    BottomLeft: bottomLeft = [bool], %@on
	    BottomCenter: bottomCenter = [bool], %@on
	    BottomRight: bottomRight = [bool], %on
	    IsCircle: isCircle = [bool] %@on
	   ):->
	"Initialise the handles for this element" ::

	%de area is bijvoorbeeld de bitmap area
	%gp3:
	%changed calculation to allways create the handles relative to the receivers area (which might not have (0,0) as topleft)
	%functions no longer allowed
	
	%gp3: when isCircle = @on, the corners are not in the corner
	%but touch the circle (2pi stuff etc)
	
	%oude handles halen we weg
	VE->>slot(handles,@nil),


	VA = VE<<-area,
	Left *= number(A?left_side - VA?left_side),
	Center = A<<-center,
	CX *= number(Center?x - VA?left_side),
	Right *= number(A?right_side - VA?left_side),
	Top *= number(A?top_side - VA?top_side),
	CY *= number(Center?y - VA?top_side),
	Bottom *= number(A?bottom_side - VA?top_side),
	
	%no difference with centerpoints
	default(TopCenter,@on,TC),
	pl_handle(TC,VE, CX,Top),
	default(CenterLeft,@on,CL),
	pl_handle(CL,VE,Left,CY),
	default(CenterRight,@on,CR),
	pl_handle(CR,VE,Right,CY),
	default(BottomCenter,@on,BC),
	pl_handle(BC,VE,CX,Bottom),
	
	default(TopLeft,@on,TL),
	default(TopRight,@on,TR),
	default(BottomLeft,@on,BL),
	default(BottomRight,@on,BR),

	if
		(IsCircle = @default ; IsCircle = @off)
	then
	(	
		pl_handle(TL,VE,Left,Top),
		pl_handle(TR,VE,Right,Top),
		pl_handle(BL, VE,Left,Bottom),
		pl_handle(BR,VE,Right,Bottom)
	)
	else
	(
		CircleLeft *= number(A?left_side + 0.1464 * A?width - VA?left_side),	
		CircleRight *= number(A?right_side - 0.1464 * A?width - VA?left_side),
		CircleTop *= number(A?top_side + 0.1464 * A?height - VA?top_side),
		CircleBottom *= number(A?bottom_side - 0.1464 * A?height - VA?top_side),
		pl_handle(TL,VE,CircleLeft,CircleTop),
		pl_handle(TR,VE,CircleRight,CircleTop),
		pl_handle(BL, VE,CircleLeft,CircleBottom),
		pl_handle(BR,VE,CircleRight,CircleBottom)
	).
%
pl_handle(@on,VE,X,Y):-
	VE->>handle(handle(X,Y)).
pl_handle(@off,_VE,_X,_Y).
%%

%%
fragment(VE,
	 F : modelFragment
	):<-
	"Return the associated modelFragment" ::

	F = VE<<-hypered(fragment).
%%

%%
fragmentElement(VE,
		FE : any
	       ):<-
	"Return the associated fragment element" ::

	FE = VE<<-hypered(fragmentElement).
%%

%%
sameFragmentElement(VE,
	Other: visualElement):->
	%slaagt wanneer beide visuele elementen naar hetzelfde
	%fragment element (inclusief route) wijzen
	%dus bijvoorbeeld een quantityElement en een qsValueElement
	%bij precies diezelfde quantity
	
	FE = VE<<-fragmentElement,
	FE = Other<<-fragmentElement,
	VE?route->>equal(Other?route).
	%%
	
%%
state(VE,
      S : {condition, consequence, imported, parent, fixed}
     ):<-
	"Get the state of the element" ::

	normal = VE<<-fragmentState,!,
	(   VE->>isCondition %hier een default, eventueel te overschrijven bij de subclasses
	->  S = condition
	;   S = consequence
	).
%
state(VE,S):<-
	S = VE<<-fragmentState.
%%


%%
doDisplay(VE,
	PlacementArg: placementarg = any,
	Device : device = framedCanvas,
	Strategy: strategy = [{spot,visibleSub}],
	Arg2: arg2 = [any]
       ):->
	"Display for the first time this element and internal subs" ::

	/*
	gp3 0.4.11: some changes. The first argument can be anything (not just a Point). It depends on the implementation of
	placementPoint, and the uses strategy.
	Stategy is new. Default is 'spot', which finds a an empty spot (starting at the point that is send as PlacementArg)
	but different strategies are possible. Strategy is an argument in placementPoint. The needed information is found in PlacementArg (for legacy purposes, first argument) and in Arg2 (new, more information send to PlacementPoint).
	
	There are now 3 ways of influencing the way an object is placed:
	- By implementing (overruling) placementPointSearchMove. This is still called when strategy is 'spot' (or @default). It defines the direction of the search for an empty spot.
	- By calling doDisplay with strategy set to one of the strategies implemented here (see below). Currently these are:
		- 'spot' search an empty spot starting at point PlacementArg. When PlacementArg is @nil or not a point, a default spot will be used.
		- 'visibleSub'. Place the element below a given visualElement (PlacementArg), according to a strategy that tries
			to place it close to the super. It tries to find an 'empty' spot, but empty is defined here as 'not another sub of my super'. So it might overlap other, less related, elements.
	- By entirely overruling placementPoint in a subclass. This method should return a point.
	
	Remember that placementPoint is only called when there is no saved location for the element.
	
	*/
        debug(sketch(layout), 'doDisplay design visual_element... \n',[]),
        % trace,

	default(Strategy,spot,Strat),
	unless
		RealPoint = VE<<-savedPosition %allready derefenced
	do
	    (
		%not saved
		RealPoint = VE<<-placementPoint(Strat,PlacementArg,Arg2,Device)
	    ),
	Device->>display(VE,RealPoint).
%%

%%
placementPoint(VE,
		   Strat,
	       Arg: any,
	       _Arg2: any,
	       On: framedCanvas,
	       P : point
	      ):<-

	
	%gp3 0.4.11: 'spot' and 'defaultSpot' strategy for finding a spot for an element
	%this default implementation still looks from some point (now in Arg) for an empty spot
	%makkelijk is dat niet en daarom zoeken we alleen maar één richting
	%(welke wordt bepaald door placementPointSearchMove, zodat het te overriden is)

	%STRATEGY SPOT:
	Strat = 'spot', 

	%so in this call, arg must be a point, or we will set it to something default
	if
		Arg->>instance_of(point)
	then
		Point = Arg
	else
	(
		VisArea = On<<-visible,
		Point *= point(VisArea?left_side + VisArea?width / 5,
							VisArea?top_side + VisArea?height / 10)
	),
	Area *= area(Point?x,Point?y,
		     VE?width, VE?height),
	Area->>normalise,
	Graphicals = On<<-graphicals,
	Move = VE<<-placementPointSearchMove,
	pl_trySpotPlacement(VE,Graphicals,Area, Move ,P),
	Move->>done,
	Area->>done.
%
placementPointSearchMove(_VE,
	Move: point):<-
	"Overridable helper for placementpoint: specify search direction" ::

	Move *= point(15,0). %standaard zoeken we dus naar rechts
%
pl_trySpotPlacement(VE,Graphicals,Area, Move, P):-
	%vindt een overlappende graphical (niet met device<<-inside, want
	%dan moet de graphical er helemaal in zitten)
	
	Graphicals->>find(and(not(@arg1 == VE),
			      ->>(@arg1?area,
				overlap,
				  Area))),!,
	%hier is er dus 1 gevonden, We schuiven de area dus naar rechts
	%dit gaat per x pixels en niet per areas
	%omdat de area van bijvoorbeeld een connectie
	%nogal groot is
	Area->>relative_move(Move),
	pl_trySpotPlacement(VE,Graphicals,Area,Move,P). % en door
%
pl_trySpotPlacement(_VE,_Graphicals,Area,_Move, P):-
	%hier gekomen hebben we een mooi gebied gevonden
	P = Area<<-position.
%%

%%
placementPoint(VE,
		   Strat,
	       Arg: any,
	       _Arg2: any,
	       On: framedCanvas,
	       P : point
	      ):<-

	
	/*gp3 0.4.11: 'visibleSub' strategy for finding a spot for an element
	Arg is a visualElement. We try to place the new element below the argument. We try for an empty spot
	but prefer visible area above empty, so we might end up overlapping something
	we use another algoritm: one 'row' below the instance this one belong to, we search the most righthandside subelement
		and try to place right of that one
		of this is offscreen, we try the next row etc. When we found no place, we just put it below the element
		We do not take the qs and derivative into account, these are always places below the quantity element
		Rows are defined as follows: first row is the first Y-coordinate that has a sub
		Rowheight is some spacing constant
		next row is below the row (all subs that overlap the given row)
	*/

	%STRATEGY SPOT:
	Strat = 'visibleSub',
	%so in this call, arg must be a visualElement:
	Arg->>instance_of(visualElement),
	Spacing = 20, %constant for spacing horizontally/vertically
	BeginCol *= number(Arg?left_side),
	Subs = Arg<<-subs,

	if
		Subs->>empty
	then
		P *= point(BeginCol,Arg?bottom_side + Spacing)
	else
	(
		Subs->>unique,
		MaxCol *= number(On?visible?right_side - VE?width),
		MaxRow *= number(On?visible?bottom_side - Spacing),
		SubAreas *= chain,
		FirstRow *= number(Subs?head?top_side), %will be changed in the loop below
		Subs->>for_all(if(
				@arg1 \== VE,
			and(
			->>(SubAreas,append,@arg1?fullArea),
			->>(FirstRow,minimum,@arg1?top_side) 
			))),
		%to make stuff less complicated, FirstRow must be below the super element
		if
			FirstRow->>smaller(Arg?bottom_side + Spacing)
		then
			FirstRow->>value(Arg?bottom_side + Spacing),
		InterestingArea *= area(BeginCol,FirstRow, @pce?max_integer, VE?height), % a row
		unless
			pl_findVisibleSubPlacement(InterestingArea,BeginCol,Spacing,MaxRow,MaxCol,SubAreas,P)
		do
			P *= point(BeginCol,Arg?bottom_side + Spacing) %default, probably overlapping another one
	).
%
pl_findVisibleSubPlacement(InterestingArea,BeginCol,Spacing, MaxRow,MaxCol,SubAreas,P):-
	InterestingArea?bottom_side->>smaller(MaxRow), %otherwise we are going to fail
	MostRight *= number(BeginCol - Spacing), %we will add the spacing again later on
	
	NextRow *= number(InterestingArea?bottom_side),
	
	SubAreas->>for_all(
		if(
			->>(@arg1,overlap,InterestingArea),
			and(
				->>(MostRight,maximum,@arg1?right_side),
				->>(NextRow,maximum,@arg1?top_side) %next row
			)
		)
	),
	if
		MaxCol->>larger(MostRight + Spacing)
	then
		P *= point(MostRight + Spacing, InterestingArea?top_side)
	else
	(
		InterestingArea->>set(y:=NextRow + Spacing),
		pl_findVisibleSubPlacement(InterestingArea,BeginCol,Spacing,MaxRow,MaxCol,SubAreas,P)
	).
%%

%%
updateDisplay(_VE):->
	"Abstract method, does nothing" ::

	true.
%%

%%positie opslaan en andere layout info inlezen gebeurt bij de elementen, hier dus algemene
%%calls

saveLayoutInfo(VE):->
	"Saves current position and other layout info in the definition object" ::
	%hier de algemene, kan overschreven worden door subclasses
	%wordt voor elk element aangeroepen
	%moet gewoon slagen of falen wanneer niet gebruikt voor een bepaald element

	/*
	gp3 1.0 new way of saving position, not compatible with old one, so we use new names
	layoutinfo position means: 'relative to origin'
	layoutinfo relPosition means: 'relative to super or arguments'
	
	savedposition will first try subposition, next position. Will save subposition only
	
	Position of top elements is allways relative to origin or something like that
	Position of subelements is relative to position of super
	Position of relational elements is relative to mid(arg1,arg2) (because that does not depend on which one we call arg1 and arg2)
			--> see overrule in visual_relation_element
	Some elements are - ofcourse - fixed
	*/

	if
		Super = VE<<-super
	then
	(
		%relative to super
		Pos = VE?absolute_position<<-minus(Super?absolute_position)
	)
	else
	(
		Dev = VE<<-device,
		if
			Dev = @nil
		then
			Org *= point(0,0) %last resort
		else
			Org = Dev?bounding_box?position, %this means we have no spacing
		Pos = VE?absolute_position<<-minus(Org)

	),
	VE?fragment->>layOutInfo(VE?fragmentElement,
			    relPosition,
			    Pos,
				VE?route),
	VE?fragment->>layOutInfo(VE?fragmentElement,
				hidden,
				VE?hidden,
				VE?route).
%
savedPosition(VE,
	      Position: point
	     ):<-
	"Returns the saved position from the definition object or fails" ::
	%kan ook weer overschreven worden door subclasses

	%gp3 1.0: could be relative to the super or absolute (because of old version)
	
	if
		Super = VE<<-super
	then
	(
		%relative position also searched in imported fragments
		Position = Super?absolute_position<<-plus(?(VE?fragment,layOutInfo,VE?fragmentElement,relPosition,VE?route)),!
                ,
                get(Position, x, X), 
                get(Position, y, Y), 
                debug(sketch(layout), 'Position derived from super: (~w, ~w)\n',[X, Y])
		;
		Position = VE?fragment<<-layOutInfo(VE?fragmentElement,	position,VE?route,@on) %old style, do not inherit
                , 
                get(Position, x, X), 
                get(Position, y, Y), 
                debug(sketch(layout), 'Position old style 1: (~w, ~w)\n',[X, Y])
	)
	else
	(
		%no super, do never inherit
		Position = VE?fragment<<-layOutInfo(VE?fragmentElement,	relPosition,VE?route,@on),!
                ,
                get(Position, x, X), 
                get(Position, y, Y), 
                debug(sketch(layout), 'Position relative Pos (no super): (~w, ~w)\n',[X, Y])
		;
		Position = VE?fragment<<-layOutInfo(VE?fragmentElement,	position,VE?route,@on)
                ,
                get(Position, x, X), 
                get(Position, y, Y), 
                debug(sketch(layout), 'Position old style 2: (~w, ~w)\n',[X, Y])
	    ).
%%

%
savedHidden(VE,
	Hidden: bool):<-
	%geeft de opgeslagen hidden state terug, als die gevonden wordt

	Hidden = VE?fragment<<-layOutInfoDefault(VE?fragmentElement,
					hidden,@off,VE?route).

%%


%%
subAdded(VE,
       Sub: sub = visualElement,
		M : moveable = [bool] %default @on
      ):->

	%gp3: used to be called registerSub, but sub-super registration is now done in initialise
	%and the code here is more about checking the consequences of having a sub
	%so we renamed it
	
	%Voor interne objecten maar ook aangeroepen door viewEditor

	default(M,@on,Mov),
	if
		Mov == @on
	then
		VE->>registerMoveable(Sub),
	Sub->>checkDisplayed(VE?displayed),
	VE->>checkImportant. %verander de zaak nu?
%%


%%
registerMoveable(VE,
	Mov: visualElement):->
	%registreer een element als samenbewegend met dit element, maar niet andersom
	%zie moveableSubs

	if
		(\+ VE<<-find_hyper(moveable,@arg3 == Mov))
	then
		VE->>hyper(Mov,moveable,moveable_by_parent).
%%

%%
connectSub(VE,Sub: sub = visualElement,
			M : moveable = [bool] %default @on
		):->
	"Connect the element as subelement, sends subAdded as well" ::
	
	%gp3 1.0 because the sub-super hyper is now added before this one is called
	%we can no longer check for existance of this hyper: it should allways be there
	%so we use connected instead
	if
		(\+ VE->>connected(Sub)) %cannot check for a certain link before we do not reuse?
	then
	(
		Link *= link(link,link,line(0,0,0,0)),
		Link->>colour(gray70),
		VE->>connect(Sub,Link),
		VE->>subAdded(Sub,M)
	).
%%

%%
subs(VE,
     Subs : chain
    ):<-
	"Return a new chain of all subelements" ::

	Subs = VE<<-all_named_hypered(sub).
%%

%
moveableSubs(VE,Subs : chain
	    ):<-
	"return a chain of subs that should be automatically moved when element moves" ::
	%maar niet andersom (daarom geen spatial). Dus als je een instance movet, dan moet de param mee
	%maar met dqs en qs elementen ligt dat anders
	%te registreren met connectSub en registerSub
	
	Subs = VE<<-all_named_hypered(moveable).
%%

%%
fullArea(VE, Area: area):<-
	%gp3 0.4.11. Return the area for this element and all its subs (recursively)
	
	Area = VE?area<<-normalised, %normalised also makes sure a *copy* is returned instead of the real area object
	VE?subs->>for_all(->>(Area,union,@arg1?fullArea)).
%%

%%

%%
isMoveTogetherSub(VE,
	Other: visualElement):->
	"Succeeds when argument will move when receiver moves (moveable sub or related sub)" ::
	%gebruikt door vieweditor, dus moveable sub of moveTogetherSub

	Test *= or(@arg3 == Other,
					->>(@arg3,isMoveTogetherSub,Other)),

	%en zoeken maar
	(
		VE<<-hypered(moveable,Test)
	;	VE<<-hypered(moveTogetherSub,Test)
	).
%%

%%
super(VE,
      Super : visualElement
     ):<-
	"Return the super element of this element" ::

	Super = VE<<-hypered(super).
%%

%%
constrainSub(VE, SubElement: subelement = visualElement,
		Constraint : constraint):<-
	"Make spatial relation keeping receiver and subelement on same relative place and register" ::
	%dus de spatial wordt gemaakt en we maken een hyper voor isMoveTogetherSub hierboven

	%een subelement kan maar één super hebben
	SubElement->>delete_hypers(moveTogetherSuper),

	constrainDistance(VE,SubElement,Constraint), %prolog helpers: de echte constraint
	VE->>hyper(SubElement,moveTogetherSub,moveTogetherSuper).
%%

%%
visualRelations(VE,
	VR: chain):<-

	VR = VE<<-all_named_hypered(relation).
%%

%%
calculi(VE,
	C: chain):<-

	C = VE<<-all_named_hypered(calculus).
%%

%%
calc_and_relations(VE,
	C: chain):<-
	%combinatie van bovenstaande elementen

	C = VE?visualRelations<<-merge(VE?calculi).
%%



%%
dragMove(VE, Change: point, Moved: chain, DragMoveableSubs: bool):->
	%doe de move, en zorg dat de subs ook gaan
	%geregeld door de viewEditor en de multi_move_gesture
	
	%gp3 0.3 added DragMoveableSubs argument: when @off, we only move
	%object and moveTogethersubs, not moveablesubs
	%used to drag with ctrl key
	VE->>relative_move(Change),
	Moved->>append(VE),
	if
		DragMoveableSubs = @on
	then
		VE?moveableSubs->>for_all(
			->>(@arg1,dragMove,Change,Moved,@on)),	%door met subs
			
	%en al onze moveTogetherSubs zijn ook gaan bewegen natuurlijk
	%dus dat moet ook opgeslagen worden
	?(VE,all_named_hypered,moveTogetherSub)->>for_all(
		->>(@arg1,dragMoveTogether,Change,Moved,DragMoveableSubs)),
	ignore(VE->>send_hyper(relation,relatedGeometryChange,VE)).	%eventuele relaties updaten
%%

%%
dragMoveTogether(VE, Change: point, Moved: chain, DragMoveableSubs: bool):->
	%aangeroepen door een super-element waarmee we automatisch meebewegen
	%wanneer die dragMove heeft gedaan
	%wij slaan van onszelf op dat we al bewogen zijn
	%en checken onze subs

	Moved->>append(VE),
	if
		DragMoveableSubs = @on
	then
		VE?moveableSubs->>for_all(
			->>(@arg1,dragMove,Change,Moved,@on)),
	?(VE,all_named_hypered,moveTogetherSub)->>for_all(
		->>(@arg1,dragMoveTogether,Change,Moved,DragMoveableSubs)),
	ignore(VE->>send_hyper(relation,relatedGeometryChange,VE)). %en de relaties

%%	

%%
isCondition(VE):->
	"Succeeds if the fragment element is a condition" ::

	%aangeroepen door visualElement<<-state en <<-stateColour
	%indien nodig overrulen!

	VE?fragmentElement->>isCondition.
%%

%%
canHide(VE):->
	%slaagt wanneer een element verborgen mag worden
	%voor override dus
	%normaal: als het een super heeft

	VE<<-super.
%%

%%
collapse(VE ):->
	%collapse het element: alle subs worden hidden
	VE?subs->>for_all(->>(@arg1,setHidden,@on)).
%%

%%
subHidden(VE):->
	%slaagt wanneer er een sub hidden is

	\+ none = VE<<-hiddenSubs.
%%

%%
subCanHide(VE):->
	%slaagt wanneer er een sub zichtbaar is die verborgen kan worden
	VE?subs->>find(and(
				@arg1?hidden == @off,
				->>(@arg1,canHide))).
%%

%%
visibleRelation(VE):->
	%slaagt wanneer er een relatie aan dit element vastzit die zichtbaar is

	VE?calc_and_relations->>find(@arg1?displayed == @on).
%%

%%
invisibleRelation(VE):->
	%slaagt wanneer er een relatie aan dit element vastzit die niet zichtbaar is

	@on = VE<<-hiddenRelation.
%%

%%
expand(VE):->
	%expand het element: alle subs worden zichtbaar
	VE?subs->>for_all(->>(@arg1,setHidden,@off)).
%%

%%
setHidden(VE,Hidden: bool):->
	%zet de hidden flag en werk de boel door
	if (
		VE->>canHide,
		\+ Hidden = VE<<-hidden
		)
	then (
		VE->>hidden(Hidden),
		VE->>checkDisplayed,
		ignore(VE?super->>checkImportant),
		ignore(VE->>saveLayoutInfo) %gp3 0.3: always save layoutinfo, may fail
		).
%%

%%
checkDisplayed(VE,SuperDisplayed: [bool]):->
	%kijk of we verborgen moet worden of juist niet
	%we krijgen mee of de super verborgen is. Als dat default is moeten we het zelf uitzoeken	
	%algemene regel: als onze super niet displayed  is of onze hidden flag staat aan dan mogen we niet afgebeeld worden

	if
		SuperDisplayed = @off
	then
		Displayed = @off
	else (
		if 
			@on = VE<<-hidden
		then
			Displayed = @off
		else (
			if 
				Sup = VE<<-super%kan falen
			then
				Displayed = Sup<<-displayed
			else
				Displayed = @on
			)
		),
	
	VE->>displayed(Displayed),
	
	VE?calc_and_relations->>for_all(->>(@arg1,checkDisplayed)),
	VE?subs->>for_all(->>(@arg1,checkDisplayed, Displayed)),
	VE->>updateRelationMarker.
%%

%%
checkImportant(VE):->
	/*
	Kijk of door een wijziging van/in subs de hiddenSub en importantSub variabelen moet worden aangepast.
	Wanneer dit zo is moet eventueel de achtergrondkleur van dit element worden bijgewerkt
	en de supers op de hoogte gesteld.
	gp3 0.2: we need to update the display when this has changed
	*/

	%importantSub gaat over alle subs
	if
		VE?subs->>find(->>(@arg1,important))
	then
		VE->>importantSub(@on)
	else
		VE->>importantSub(@off),

	%hiddenSubs alleen over de verborgen
	AllHiddenSubs = VE?subs<<-find_all(@arg1?hidden == @on),
	if
		0 = AllHiddenSubs<<-size
	then
		NewHS = none
	else (
		if
			@off = VE<<-importantSub
		then
			NewHS = unimportant
		else (
			if
				AllHiddenSubs->>find(->>(@arg1,important))
			then
				NewHS = important
			else	
				NewHS = unimportant
			)
		),

	AllHiddenSubs->>done,

	%ok, is dat anders?
	if
		(\+ NewHS = VE<<-hiddenSubs)
	then (
		VE->>hiddenSubs(NewHS),
		VE->>updateDisplay
		),

	%en moeten we het doorgeven aan onze super?
	if
		VE->>important
	then
		NewIMP = @on
	else
		NewIMP = @off,
	if
		(\+ NewIMP = VE<<-isImportant)
	then (
		%veranderd
		VE->>isImportant(NewIMP),
		ignore(VE?super->>checkImportant)
		).
%%

%%
important(VE):->
	"Succeeds if this element is important" ::
	%slaagt dus wanneer dit element "normal" is of een important relatie heeft
	%of de importantSub flag aan staat (dus een sub als important bestempeld)

	@on = VE<<-importantSub,!.
%
important(VE):->
	normal = VE<<-fragmentState,!.
%
important(VE):->
	VE?calc_and_relations->>find(->>(@arg1,important)).
%%

%%
recursiveCheckHidden(VE):->
	%gebruikt door viewEditor ExpandAll command om te checken of er iets is ingeklapt bij dit element
	%slaagt wanneer het element verborgen is of een subelement verborgen is
	
	@on = VE<<-hidden,!.
%
recursiveCheckHidden(VE):->
	@on = VE<<-hiddenRelation,!.
%
recursiveCheckHidden(VE):->
	VE?subs->>find(->>(@arg1,recursiveCheckHidden)).
%%

%%
expandAll(VE):->
	%expand dit element en alle subelementen + de relaties
	VE->>expand,
	VE->>expandRelations,
	VE?subs->>for_all(->>(@arg1,expandAll)).
%%	

%%
checkHiddenRelations(VE):->
	%teken de hidden relations marker wanneer er hidden relations zijn
	if
		VE?calc_and_relations->>find(@arg1?displayed == @off)
	then
		NewHiddenRel = @on
	else
		NewHiddenRel = @off,

	if
		(\+ NewHiddenRel = VE<<-hiddenRelation)
	then (
		%de situatie is gewijzigd
		VE->>hiddenRelation(NewHiddenRel),
		VE->>updateRelationMarker
		).
%%

%%
updateRelationMarker(VE):->
	%display of hide de relation marker bij dit element
	%1: de marker bestaat reeds

	RM = VE<<-hypered(relationMarker),!,
	if
		@on = VE<<-displayed
	then
		RM->>displayed(VE?hiddenRelation)
	else
		RM->>displayed(@off).
%
updateRelationMarker(VE):->
	%2) de marker bestaat nog niet en wordt dus eerst gemaakt
	%gp3 0.2: we use a bitmap instead of a text
	
	get_image(elements,hrm,I),
	RM *= psBitmap(I),
	RM->>psdef(hrm),
	VE->>hyper(RM,relationMarker),
	%placement: not in the graphical, but constrained to the right
	VE?device->>display(RM,point(VE?right_side + 2,VE?top_side - 2)),
	constrainDistance(VE,RM,_), %prolog helpers
	%hij is er nu wel, dus nogmaals
	VE->>updateRelationMarker.
%%

%%
collapseRelations(VE):->
	%collapse alle relaties

	VE?calc_and_relations->>for_all(->>(@arg1,setHidden,@on)).
%%

%%
expandRelations(VE):->
	%expand alle relaties, dat sturen we door naar onze hidden relaties

	VE?calc_and_relations->>for_all(->>(@arg1,ensureVisible)).
%%

%%
ensureVisible(VE):->
	%zorgt dat we zichtbaar zijn, dat betekent bij een gewoon element dat de super zichtbaar moet zijn
	%en wij niet hidden

	if
		@off = VE<<-displayed
	then (
		VE->>setHidden(@off),
		ignore(VE?super->>ensureVisible)
		).
%%

%%
showRelevant(VE):->
	%zorgt dat de relevante subs zichtbaar zijn en de rest niet

	%eerst alles hiden en dan de relevante dingen zichtbaar maken
	VE?subs->>for_all(	if(not(->>(@arg1,important)),
					->>(@arg1,setHidden,@on))),
	VE?subs->>for_all(if(->>(@arg1,important),
					->>(@arg1,ensureVisible))),
	VE?subs->>for_all(->>(@arg1,showRelevant)),
	%VE?calc_and_relations->>for_all(->>(@arg1,ensureVisible)), %gp3 removed this line because it would show irrelevant relations in imported fragments + their arguments
	VE?calc_and_relations->>for_all(->>(@arg1,showRelevant)).
%%

%%
%gp3 1.4 relevantcomments helper
checkComments(_VE,Comments: 'char_array ...',Comment: string):<-
	%helper: add any non empty comment argument to the Comment string
	%if this results in an empty string, add '(no comments)'
	
	S *= string,
	Comments->>for_all(
		->>(S,ensure_nl,?(@arg1,split_lines,120))),
	if
		0 = S<<-size
	then
		Comment *= string('(no comments)')
	else
		Comment = S<<-strip.
%%

	

:-pce_end_class.
