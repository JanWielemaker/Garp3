/*
Definitie qsElement class en hulpclass qsValueElement
figuurtje om een qs bij een quantity mee af te beelden

Part of Garp3 See copyright notice
2000 - 2005, Jelmer Jellema - Spin in het Web - spininhetweb.nl

Most code is legacy homer code, so dutch comments

GP3 changed a lot: we eliminated the use of visible boxes for each value
so no need to coordinate the width. The box will be tight now for the
qs name, and no boxes at all for the values
*/

:-pce_begin_class(qsElement,
		  visualElement,
		  "Display of quantitySpace"
		 ).

variable(editor,framedWindow,both). %de editor is nodig ivm registerElement

initialise(QSE,
	   Fragment: modelFragment,
	   Quantity: garpQuantity,
		Editor: framedWindow, %editor nodig ivm registerElement
		QE: quantityElement, %gp3 0.4.11: needed for visibleSub placement, 1.0 standard argument
		Route: [chain] %zie visualElement
		):->
	
	QSE->+initialise(Fragment,Quantity,QE,fixed,Route),

	QSE->>editor(Editor),

	NameText *= text(''),
	NameText->>name(nameText),
	NameText->>font(small),

	Box *= box(0,0),
	Box->>name(box), %alleen om de naam
	Box->>pen(0), %gp3 0.2
	QSE->>display(Box),
	QSE->>display(NameText),
	
	%gp3 we do our own handles, because we want them to be dynamic
	QSE->>handle(handle(0,0)),
	QSE->>handle(handle(w,0)),
	QSE->>handle(handle(w/2,0)),
	QSE->>handle(handle(0,h/2)),
	QSE->>handle(handle(w,h/2)),	
	
	%gp3 0.4.11: we can now use doDisplay with 'visibleSub' strategy
	QSE->>doDisplay(QE, Editor?client, strategy := visibleSub),
	QSE->>updateDisplay,
	
	%gp3 1.4: qs has dynamic tooltip
	QSE->>tooltip(@default,model).
%%

%%
tooltipContent(VE,S: string):<-
	%gp3 1.4 rewrite of the tooltip
	
	QS = VE?fragmentElement<<-quantitySpace,
	S = VE<<-checkComments(QS?relevantComments).
%%

%%
updateDisplay(QE):->
	"Update the display information" ::

	Quantity = QE<<-fragmentElement,
	Box = QE<<-member(box),
	NameText = QE<<-member(nameText),
	QS = Quantity<<-quantitySpace,
	
	NameText->>string(QS?name),
	NameText->>colour(QE?stateColour),
	Box->>area(NameText?area), %gp3 exactly the same area now, just for bg colour
	
	%gp3 0.2 we set the backcolour here, no more general calls
	%we and derivative are the only ones that use it
	(
		none = QE<<-hiddenSubs, CL = white
	;
		important = QE<<-hiddenSubs, CL = yellow
	;
		CL = grey95
	),
	Box->>fill_pattern(colour(CL)),
	Serial *= number(1),

	%gp3 we update the values, but do not do anything with there widths
	
	QE->>updateValueElements(QS?values,
				 QE,
				 Serial).

%%


%%
updateValueElements(QE,
		    Values:chain,
		    Below:graphical,
		    Serial:number	%gp3 0.2 removed one argument
		   ):->

	%interne recursieve bouwer van de sub elementen
	%we pakken steeds de eerste van de values chain af
	%daarvoor voegen we een element toe en gaan dan
	%door voor het volgende element
	%gp3: no longer any messing with the width

	%clause 1, we zijn klaar: als er nog subs zijn met een hogere serial
	%dan moeten die weg
	(   Values->>empty
	->  QE?qsSubs->>for_all(if(->>(Serial,
				       less_equal,
				       @arg1?serial),
				   ->>(@arg1, destroy)))
	;   Element = QE<<-findQSSubSerial(Serial)
	->  ValueRef = Values<<-delete_head,
	    Element->>updateDisplay(ValueRef),
	    Serial->>plus(1),
	    QE->>updateValueElements(Values,
				     Element,
				     Serial)
	;   %het element is er nog niet
	    ValueRef = Values<<-delete_head,
	    VE *= qsValueElement(QE?fragment,
				 QE?fragmentElement,
				 QE,
				 ValueRef,
				 Serial,
				 QE?editor?client,
				 point(Below?left_side,Below?bottom_side),
				 QE?route),
	    QE->>subAdded(VE,@off), %geen connection
	    QE?editor->>registerElement(VE), %directe callback naar de editor
	    Below<<-constrainSub(VE), %VE wordt een "sub" van below in de hierarchie van constraint objecten
							%zie isMoveTogetherSub bij visualElement en viewEditor->getMultiMove

	%en klaar maken voor de volgende
	    Serial->>plus(1),
	    QE->>updateValueElements(Values,
				     VE,
				     Serial)
	).
%%

%%overschrijving van saveLayoutInfo en saved... vanwege
%%het feit dat we bij de quantity horen (net als quantityElement)

saveLayoutInfo(QE):->

%We gebruiken position (geeft de positie van het (0,0) punt in de graphical terug)
%en niet absolute_position omdat al voor het plaatsen de figure naar linksboven is
%uitgebreid en de qs anders langzaam naar boven zou kruipen.
	QE?fragment->>layOutInfo(QE?fragmentElement,
			    qsElementRelPosition,
			     ?(QE?position,minus,QE?super?absolute_position),QE?route), %en de subs slaan we over
	QE?fragment->>layOutInfo(QE?fragmentElement,
				qsElementHidden,
				QE?hidden,QE?route).
%
savedPosition(QE,
	      Position: point
	     ):<-
	"Returns the saved position from the definition object or fails" ::
	%gp3 1.0: could be relative to the super or absolute (because of old version)
	
	Position = QE?super?absolute_position<<-plus(?(QE?fragment,layOutInfo,QE?fragmentElement,
					qsElementRelPosition,QE?route)),!
	;
	Position = QE?fragment<<-layOutInfo(QE?fragmentElement,
					qsElementPosition,QE?route,@on).
%
savedHidden(QE,
		C : bool):<-

	C = QE?fragment<<-layOutInfoDefault(QE?fragmentElement,
					qsElementHidden,@off,QE?route).


%%

%%
qsSubs(QE,
	Subs:chain):<-
	"Return chain with all subs that are of type qsValueElement (not valueMarker)" ::

	Subs = QE?subs<<-find_all(@arg1?name == qsValueElement).
%%

%%
findQSSubSerial(QE,
	Serial: int,
	Sub: qsValueElement):<-
	"Return qsValueElement with the given serial" ::

	Sub = QE?qsSubs<<-find(@arg1?serial == Serial).
%%

%%
findQSSubValue(QE,
	V : valueReference,
	Sub : qsValueElement):<-
	"Return qsValueElement with the same refered value as given" ::

	Sub = QE?qsSubs<<-find(->>(V,sameValue,@arg1?valueRef)).
%%


:-pce_end_class.


:-pce_begin_class(qsValueElement,
		  visualElement,
		  "Display of quantitySpace value"
		 ).

%qsValueElements doen weinig

variable(serial,int,get). %bovenste waarde is 1
variable(valueRef,valueReference,get).

%%
initialise(QVE,
	   Fragment : modelFragment,
	   Quantity: garpQuantity,
	   QE: qsElement, %gp3 1.0: standard argument
	   Value : valueReference,
	   Serial : int,
		Device : device,
		DisplayPoint: point,
		Route: [chain] %zie viualElement
	  ):->

	QVE->+initialise(Fragment,Quantity,QE,fixed,Route),
	QVE->>slot(serial,Serial),
	NameText *= text,
	NameText->>name(nameText),
	Box *= box(0,0),
	Box->>pen(0), %gp3 0.2
	Box->>name(box),
	Bitmap *= psBitmap,
	Bitmap->>name(bitmap),
	QVE->>display(Box),
	QVE->>display(Bitmap),
	QVE->>display(NameText),
	%gp3 we do our own handles, because we want them to be dynamic
	QVE->>handle(handle(0,h/2)),
	QVE->>handle(handle(w,h/2)),	

	Device->>display(QVE,DisplayPoint), %afbeelden
	QVE->>updateDisplay(Value).
	%gp3 1.4 removed trivial tooltip
%%

%%
updateDisplay(QVE,
	      Val : [valueReference]
	     ):->

	%gp3: if Val not given, we do not change it
	%(needed for general updateDisplay call)
	%we make the whole graphical a bit higher
	unless
		Val = @default
	do
		QVE->>slot(valueRef,Val),
	
	Value = QVE<<-valueRef,
	NameText = QVE<<-member(nameText),
	ValueText = Value<<-valueName,
	
	NameText->>string(ValueText),
	NameText->>font(small),
	NameText->>colour(QVE?stateColour),

	if
		interval = Value<<-type
	then
		ImageName = interval
	else
		ImageName = point,
	get_image(elements,ImageName,Image),
	
	Bitmap = QVE<<-member(bitmap),
	Bitmap->>image(Image),
	Bitmap->>psdef(ImageName),
	
	BH = Bitmap<<-height,
	TH = NameText<<-height,
	
	if
		BH > TH
	then
	(
		Bitmap->>set(y:=0),
		NameText->>set(x := Bitmap?right_side + 1, 
				y:=Bitmap?center_y - (TH / 2))
	)
	else
	(
		NameText->>set(x := Bitmap?right_side + 1, y := 0),
		Bitmap->>set(y:=NameText?center_y - (BH / 2))
	),
	Area = NameText?area<<-clone,
	Area->>union(Bitmap?area),
	Area->>height(Area?height),
	QVE?box_member->>area(Area).
%%

%%
doDisplay(_QVE,
	_Point : default_point = point,
	_Device : device = [device]
       ):->
	"Not to be used with this element" ::

	fail.
%%

%%saveLayout doet alleen de hidden status
saveLayoutInfo(QVE):->
	QVE?fragment->>layOutInfo(QVE?fragmentElement,
				qsValueElementHidden,
				QVE?hidden,QVE?route).
%%

%%en savedPosition faalt
savedPosition(_QE,
	      _Position: point):<-
	fail.
%
%savedHidden
savedHidden(QVE,
	H : bool):<-

	H = QVE?fragment<<-layOutInfoDefault(QVE?fragmentElement,
					qsValueElementHidden,@off,QVE?route).
%%

%%
setHidden(VE,Hidden: bool):->
	%overrule van visualElement: alle broertjes worden ook verborgen
	%of te wel: we sturen het ook naar alle subs van de super
	%(geeft wat recursie maar dat komt goed doordat we de hidden flag tijdig zetten)

	if (
		%VE->>canHide, %eigenlijk niet nodig hier
		\+ Hidden = VE<<-hidden
		)
	then (
		VE->>hidden(Hidden), %beschermt tegen loop
		VE?super?subs->>for_all(->>(@arg1,setHidden,Hidden)),
		VE->>checkDisplayed,
		ignore(VE?super->>checkImportant)
		).
%%

:-pce_end_class.
