/*
Definitie derivativeElement class en dqsValueElement hulpklasse
figuurtje om een afgeleide bij een quantity mee af te beelden

Part of Garp3 See copyright notice
2000 - 2005, Jelmer Jellema - Spin in het Web - spininhetweb.nl

Most code is legacy homer code, so dutch comments
*/

:-pce_begin_class(derivativeElement,
		  visualElement,
		  "Display of derivative"
		 ).

initialise(DE,
	   Fragment: modelFragment,
	   Quantity: garpQuantity,
		Editor: framedWindow, %editor nodig ivm registerElement
		QE: quantityElement, %gp3 0.4.11: needed for visibleSub placement, 1.0 also for general use
		Route: [chain] %zie visualElement
	):->

	DE->+initialise(Fragment,Quantity,QE,fixed,Route),

	get_image(elements,derivative,Image),
	Bitmap *= psBitmap(Image),
	Bitmap->>name(bitmap),
	Bitmap->>psdef(derivative),
	Box *= box(0,0),
	Box->>name(box),
	Box->>pen(0), %gp3 0.2
	Area = Bitmap?area<<-clone,
	Area->>increase(2),
	Box->>area(Area),

	DE->>display(Box),
	DE->>display(Bitmap),
	DE->>initHandles(Box?area, centerRight:= @on),
	
	DE->>doDisplay(QE, Editor?client, strategy := visibleSub), %gp3 0.4.11 changed strategy

	%en we maken de subobjecten aan
	DQS = @model?dqs<<-values,
	Up *= dqsValueElement(Fragment,
			      Quantity,
			      DE,
				  ?(DQS,nth1,1),
			      plus,
					Route),
	Up->>doDisplay(DE,Editor?client, strategy := visibleSub), %gp3 0.4.11 changed strategy
	Editor->>registerElement(Up),
	DE->>connectSub(Up),
	DE->>hyper(Up,up),
	%gp3 0.2: this element has a tooltip with static text
	%Up->>tooltip('Increase'),
	
	Zero *= dqsValueElement(Fragment,
				Quantity,
				DE,
				?(DQS,nth1,2),
				zero,Route),
	Editor?client->>display(Zero,
					point(Up?left_side,Up?bottom_side)), %geen doDisplay
	Editor->>registerElement(Zero),
	DE->>subAdded(Zero,@off),
	DE->>hyper(Zero,zero),
	Up<<-constrainSub(Zero),%visualElement
					%Zero wordt een "sub" van Up in de hierarchie van constraint objecten
					%zie isMoveTogetherSub bij visualElement en viewEditor->getMultiMove
	%gp3 0.2: this element has a tooltip with static text
	%Zero->>tooltip('Stable'),
	
	Down *= dqsValueElement(Fragment,
				Quantity,
				DE,
				?(DQS,nth1,3),
				min,Route),
	Editor?client->>display(Down,
					point(Zero?left_side,Zero?bottom_side)),
	Editor->>registerElement(Down),
	DE->>subAdded(Down,@off),
	DE->>hyper(Down,down),
	Zero<<-constrainSub(Down),
	%gp3 0.2: this element has a tooltip with static text
	%Down->>tooltip('Decrease'),
	
	DE->>updateDisplay.
	%gp3 0.2: this element has a tooltip with static text
	%DE->>tooltip('Derivative').
%%

%%
updateDisplay(DE):->
	"Update the display information" ::

	Box = DE<<-member(box),
	%gp3 0.2 we set the backcolour here, no more general calls
	%we and qs_element are the only ones that use it
	(
		none = DE<<-hiddenSubs, CL = white
	;
		important = DE<<-hiddenSubs, CL = yellow
	;
		CL = grey95
	),
	Box->>fill_pattern(colour(CL)),

	MaxWidth *= number(0),
	DE?qsSubs->>for_all(->>(MaxWidth,
			      maximum,
			      @arg1?width)),
	DE?qsSubs->>for_all(->>(@arg1,
			      updateDisplay,
			      MaxWidth)).
%%

%%overschrijving van saveLayoutInfo en saved... vanwege
%%het feit dat we bij de Quantity horen (net als QuantityElement)
saveLayoutInfo(DE):->

	%gp3 1.0 here too we save the position relative to the super element
	DE?fragment->>layOutInfo(DE?fragmentElement,
			    derivativeElementRelPosition,
			     ?(DE?absolute_position,minus,DE?super?absolute_position),DE?route),
	DE?fragment->>layOutInfo(DE?fragmentElement,
			    derivativeElementHidden,
			    DE?hidden,DE?route).
%
savedPosition(DE,
	      Position: point
	     ):<-
	"Returns the saved position from the definition object or fails" ::

	%gp3 1.0: could be relative to the super or absolute (relative to origin or something like that) (because of old version)
	
	Position = DE?super?absolute_position<<-plus(?(DE?fragment,layOutInfo,DE?fragmentElement,
					derivativeElementRelPosition,DE?route)),!
	;
	Position = DE?fragment<<-layOutInfo(DE?fragmentElement,
					derivativeElementPosition,DE?route,@on).
%
savedHidden(DE,
	C : bool):<-
	%is er nog niet voor deze

	C = DE?fragment<<-layOutInfoDefault(DE?fragmentElement,
					derivativeElementHidden,@off,DE?route).
%%

%%
qsSubs(DE,
	Subs:chain):<-
	"Return chain with all subs that are of type dqsValueElement (not valueMarker)" ::

	Subs = DE?subs<<-find_all(@arg1?name == dqsValueElement).
%%

%%
findQSSubValue(DE,
	V : valueReference,
	Sub : dqsValueElement):<-
	"Return qdsValueElement with the same refered value as given" ::

	Sub = DE?qsSubs<<-find(->>(V,sameValue,@arg1?valueRef)).
%%


:-pce_end_class.



:-pce_begin_class(dqsValueElement,
		  visualElement,
		  "Display of quantitySpace value for derivative"
		 ).

%dqsValueElements doen weinig, ze worden weggegooid
%wanneer de waarde veranderd. Ze zijn alleen apart omdat de editor
%relevante objecten van class visualElement verwacht

variable(value, name, get).

variable(valueRef,valueReference,get).
%%
initialise(DQVE,
	   Fragment : modelFragment,
	   Quantity: garpQuantity,
	   DE: derivativeElement,	%gp3 1.0: the super element
	   ValueRef : valueReference,
	   ValueText : name,
		Route: [chain] %zie visualElement
	  ):->

	DQVE->+initialise(Fragment,Quantity,DE,fixed,Route),
	DQVE->>slot(value,ValueText),
	DQVE->>slot(valueRef, ValueRef),

	ImageName *= name(dqs_),
	FullName = ImageName<<-append(ValueText),
	get_image(elements,FullName,Image), %dqs_plus, dqs_zero, dqs_min
	Box *= box(0,0), %komt goed in updateDisplay
	Box->>name(box),
	Box->>pen(0), %gp3 0.2
	Bitmap *= psBitmap(Image),
	Bitmap->>psdef(FullName),
	Bitmap->>name(bitmap),

	DQVE->>display(Box),
	DQVE->>display(Bitmap),
	Area = Bitmap?area<<-clone,
	Area->>increase(1),
	Box->>area(Area),
	DQVE->>initHandles(Box?area,
			   topLeft := @off,
			   topCenter := when(ValueText == plus,@on,@off), %only for plus
			   topRight := @off,
			   bottomLeft := @off,
			   bottomCenter := @off,
			   bottomRight := @off). 
%%

%%
updateDisplay(DQVE,
	      Width : [int]):->
	"Update the display information" ::
	%wordt aangeroepen door het derivativeElement
	%we moeten net zo breed worden als de meegegeven breedte
	%gp3: if not given: do not change

	Box = DQVE<<-member(box),
	%Bitmap = DQVE<<-member(bitmap),
	unless
		Width = @default
	do
		Box->>set(width:=Width). %berekende grootte
%%

saveLayoutInfo(DQVE):-> %alleen voor plus
	plus = DQVE<<-value,
	DQVE?fragment->>layOutInfo(DQVE?fragmentElement,
				   dplusRelPosition,
				    ?(DQVE?absolute_position,minus,DQVE?super?absolute_position),DQVE?route), %gp3 1.0: save relative position
	DQVE?fragment->>layOutInfo(DQVE?fragmentElement,
				  dplusHidden,
				  DQVE?hidden,DQVE?route).
%%

%%en savedPosition ook alleen voor de plus
savedPosition(DQVE,
	      Position: point):<-

	%gp3 1.0: could be relative to the super or absolute (relative to origin or something like that) (because of old version)

	plus = DQVE<<-value,
	(
		Position = DQVE?super?absolute_position<<-plus(?(DQVE?fragment,layOutInfo,DQVE?fragmentElement,
					dplusRelPosition,DQVE?route)),!
	;
		Position = DQVE?fragment<<-layOutInfo(DQVE?fragmentElement,
					dplusPosition,DQVE?route,@on)
	).
%
savedHidden(DQVE,
	C : bool):<-
	%lezen gewoon die van de plus in
	C = DQVE?fragment<<-layOutInfoDefault(DQVE?fragmentElement,
					dplusHidden,@off,DQVE?route).
%%

%%
setHidden(DQVE,Hidden: bool):->
	%overrule van visualElement: alle broertjes worden ook verborgen
	%of te wel: we sturen het ook naar alle subs van de super
	%(geeft wat recursie maar dat komt goed doordat we de hidden flag tijdig zetten)

	if (
		%DQVE->>canHide, %eigenlijk niet nodig hier
		\+ Hidden = DQVE<<-hidden
		)
	then (
		DQVE->>hidden(Hidden), %beschermt tegen loop
		DQVE?super?subs->>for_all(->>(@arg1,setHidden,Hidden)),
		DQVE->>checkDisplayed,
		ignore(DQVE?super->>checkImportant)
		).
%%

:-pce_end_class.

