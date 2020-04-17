/*
Definitie speciale implementatie van een list_browser voor het
previewen van een quantity space.
Kan ook als een editor gebruikt worden: je kan gegevens toevoegen
en wijzigen enzo, en dan opvragen hoe het er nu voor staat.
(geen normatieve controle natuurlijk).


*/

:-pce_begin_class(
		  qsPreviewBrowser,
		  extendedListBrowser,
		  "special extended list browser for previewing garp quantity spaces"
		 ).

%%
initialise(QPB,
	Width : width = [int],
	Height : height = [int]
	):->
	QPB->+initialise(width := Width,
						height := Height),
	QPB->>label('Preview:'),
	QPB->>show_label(@on),
	%en een paar styles met images
	get_image(elements,interval,Interval), %gp3 changed symbol
	get_image(elements,point,Point), %gp3 changed symbol
	QPB->>style(interval,
		       style(icon:=Interval,
			     font := normal)),
	QPB->>style(one,
		       style(icon:=Point,
			     font := italic)),
	QPB->>style(zero,
		       style(icon:=Point,
			     font := italic)),
	QPB->>style(minusOne,
		       style(icon:=Point,
			     font := italic)),
	QPB->>style(point,
		       style(icon:=Point,
			     font := normal)).
%%

%%
preview(QPB,
	QS: quantitySpace
	):->
	"Preview the given quantity space" ::

	QPB->>setValues(QS?values). %kopie->nieuwe valueReference objecten met zelfde ID
%%

setValues(QPB,
	Values: chain
	):->
	%beeldt de opgegeven values af. Wordt ook in het lijstobject opgeslagen
	%dus <<-values geeft kopieen van deze nieuwe objecten terug

	QPB->>clear,
	ValueLength = Values<<-size,
	pl_QSPreview(QPB,
			 Values,
			 1,
			 ValueLength).
%
pl_QSPreview(_Preview,_Values,I,N):-
	I > N,!. %klaar
pl_QSPreview(Preview,Values,I,N):-
	Value = Values<<-nth1(I),
	get(Value, valueName, Name),
	(
	  Name = 'Zero'
	->
	  Style = zero
	;
	  (
	    Name = 'One'
	  ->
	    Style = one
	  ;
	    (
	      Name = 'MinusOne'
	    ->
	      Style = minusOne
	    ;
	      (
	        Style = Value<<-type
	      )
	    )
	  )
	),
	Preview->>append(dict_item(Value?valueName, object:=Value, style := Style)),
	NewI is I + 1,
	pl_QSPreview(Preview,Values,NewI,N).
%%

%%
value(QPB,
	V : name):<-
	"Return currently selected value name" ::

	Item = QPB<<-selection,
	\+ Item == @nil, %iets geselecteerd
	V = Item<<-key.
%%

%%
zeroSelected(QPB):->
	"Succeeds if the zero value is selected" ::

	Item = QPB<<-selection,
	\+ Item == @nil, %iets geselecteerd
	zero = Item<<-style.
%%

%%
oneSelected(QPB):->
	"Succeeds if the one value is selected" ::

	Item = QPB<<-selection,
	\+ Item == @nil, %iets geselecteerd
	one = Item<<-style.
%%

%%
minusOneSelected(QPB):->
	"Succeeds if the minusOne value is selected" ::

	Item = QPB<<-selection,
	\+ Item == @nil, %iets geselecteerd
	minusOne = Item<<-style.
%%



%%
selectedType(QPB,
	Type: {point,interval}):<-
	"Returns type of selection (no special for zero)" ::

	Type = QPB<<-itemType(QPB?selection).
%%

%%
changeCurrent(QPB,
	Value : name):->
	"Explicitly set current value" ::
	%value = Zero wordt bij point herkend als zero point


	Item = QPB<<-selection,
	\+ Item == @nil,

	RealValue = Value<<-makeGarp,
	CurrentStyle = Item<<-style,

	(
	  memberchk(CurrentStyle, [zero, one, minusOne,	point])
	->
	  (
	    RealValue->>equal('Zero')
	  ->
	    Item->>style(zero)
	  ;
	    (
	      RealValue->>equal('One')
	    ->
	      Item->>style(one)
	    ;
	      (
	        RealValue->>equal('Minusone')
	      ->
	        Item->>style(minusOne)
	      ;
	        (
		  memberchk(CurrentStyle, [zero, one, minusOne])
		->
	          % blijkbaar op punt: naar custom gegaan vanaf constante
		  Item->>style(point)	%anders wat het was
		;
	          true
		)
	      )
	    )
	  )
	;
	  true
	),

	Item->>key(RealValue),
	Item?object->>valueName(RealValue).
%%

%%
firstValue(QPB,
	Value: name,
	Type: {point,interval},
	Item : dict_item):<-
	"Set the given value as only value and return the item" ::

	QPB->>clear,
	RealValue = Value<<-makeGarp,
	VR *= valueReference(RealValue,Type),
	Item *= dict_item(RealValue,object:=VR, style:=Type),
	QPB->>append(Item).
%%

%%
addHigh(QPB,
	Value: name,
	Item : dict_item):<-
	"Add a high value and return the added item" ::

	if
		point = QPB<<-firstValueType
	then
		NewType = interval
	else
		NewType = point,

	RealValue = Value<<-makeGarp,
	VR *= valueReference(RealValue,NewType),

	Item *= dict_item(RealValue,object:=VR,style:=NewType),
	QPB->>insert_after(Item,@nil).
%%

%%
addLow(QPB,
	Value: name,
	Item : dict_item):<-
	"Add a low value and return the added item" ::

	if
		point = QPB<<-lastValueType
	then
		NewType = interval
	else
		NewType = point,

	RealValue = Value<<-makeGarp,
	VR *= valueReference(RealValue,NewType),
	Item *= dict_item(RealValue,object:=VR, style:=NewType),
	QPB->>append(Item).
%%

%%
removeHigh(QPB):->
	"Remove the highest value" ::

	QPB->>delete(QPB?members?head).
%%

%%
removeLow(QPB):->
	"Remove the lowest value" ::

	QPB->>delete(QPB?members?tail).
%%

%%
currentValues(QPB,
	Values: chain):<-
	"Return new chain with copies of value references" ::


	Values = QPB?members<<-map(@arg1?object?copy).
%%

%%
firstValueType(QPB,
	Type : {point,interval}):<-
	"Return the type of the first (highest) element" ::

	Type = QPB<<-itemType(QPB?members?head).
%%

%%
lastValueType(QPB,
	Type : {point,interval}):<-
	"Return the type of the last element" ::

	Type = QPB<<-itemType(QPB?members?tail).
%%

%%
itemType(_QPB,
	Item : dict_item*,
	Type : {point,interval}):<-
	"Return the type of the given element (no special voor zero)" ::

	\+ Item == @nil, %Standaard afvangen
	Type = Item?object<<-type. %zit tegenwoordig in valueReference!
%%

%%
splitInterval(QPB,
	Item : dict_item,
	LowName : name,
	PointName: name,
	HighName: name):->
	"Split the given interval item in interval-point-interval. Fail if not an interval value" ::
	%hierbij gaan referenties eraan: het worden nieuwe waarden

	QPB->>member(Item), %moet bestaan
	interval = QPB<<-itemType(Item),

	%ok, nieuwe items maken
	LowItem *= dict_item(LowName?makeGarp,
				object:=valueReference(LowName?makeGarp,interval),
				style:=interval),
	HighItem *= dict_item(HighName?makeGarp,
				object:=valueReference(HighName?makeGarp,interval),
				style:=interval),

	%het item zelf wordt een punt, tenzij de nieuwe naam 'Zero' is
	if
		PointName->>equal('Zero')
	then
		Item->>style(zero)
	else
		Item->>style(point),
	Item->>key(PointName?makeGarp),
	Item->>object(valueReference(PointName?makeGarp,point)),

	%nu moeten de nieuwe items nog toegevoegd worden
	QPB->>insert_after(LowItem,Item), %sortering is van hoog naar laag
	QPB->>insert_before(HighItem,Item). %extendedListBrowser methode
%%

%%
combineIntervals(QPB,
	Point : dict_item,
	NewName : name):->
	"Combine the given point item with it's high and low neighbours to a new interval. Fail if not possible" ::
	%de point interval wordt een nieuwe valueReference, want het type wijzigt

	QPB->>member(Point),
	point = QPB<<-itemType(Point), %wat tests vooraf

	LowItem = QPB<<-itemAfter(Point),
	HighItem = QPB<<-itemBefore(Point),

	QPB->>delete(LowItem),
	QPB->>delete(HighItem),
	Point->>style(interval),
	Point->>key(NewName?makeGarp),
	Point->>object(valueReference(NewName?makeGarp,interval)).
%%

:-pce_end_class.


