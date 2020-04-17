/*
Definitie speciale implementatie van een list_browser voor sketchQuantities
Kan ook als een editor gebruikt worden: je kan gegevens toevoegen
en wijzigen enzo, en dan opvragen hoe het er nu voor staat.
(geen normatieve controle natuurlijk).

*/

:-pce_begin_class(
		  sketchQuantityBrowser,
		  extendedListBrowser,
		  "special extended list browser for sketchQuantities"
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
	QPB->>style(zero,
		       style(icon:=Point,
			     font := italic)),
	QPB->>style(point,
		       style(icon:=Point,
			     font := normal)).
%%

%%
preview(QPB,
	SketchQuantity: sketchQuantity
	):->
	"Preview the given sketchQuantity" ::

	QPB->>setValues(SketchQuantity?values). %kopie->nieuwe valueReference objecten met zelfde ID
%%

setValues(QPB,
	Values: chain
	):->
	%beeldt de opgegeven values af. Wordt ook in het lijstobject opgeslagen
	%dus <<-values geeft kopieen van deze nieuwe objecten terug
	
	QPB->>clear,
	ValueLength = Values<<-size,
	pl_SketchQuantity(QPB,
			 Values,
			 1,
			 ValueLength).
%
pl_SketchQuantity(_Preview,_Values,I,N):-
	I > N,!. %klaar
pl_SketchQuantity(Preview,Values,I,N):-
	Value = Values<<-nth1(I),

	if
		'Zero' = Value<<-valueName
	then
		Style = zero
	else
		Style = Value<<-type,
	Preview->>append(dict_item(Value?valueName,object:=Value,
				   style := Style)),
	NewI is I + 1,
	pl_SketchQuantity(Preview,Values,NewI,N).
%%
	
%%
value(QPB,
	V : sketchQuantityElement):<-
%	V : name):<-
	"Return currently selected value name" ::

	Item = QPB<<-selection,
	\+ Item == @nil, %iets geselecteerd
	V = Item<<-key.
%%


%%
changeCurrentValue(QPB,
	Value : name):->
	"Explicitly set current value" ::
	%value = Zero wordt bij point herkend als zero point

	Item = QPB<<-selection,
	\+ Item == @nil,

	RealValue = Value<<-makeGarp,
	CurrentStyle = Item<<-style,

	if
		(RealValue->>equal('Zero'),
		 (CurrentStyle = point ; CurrentStyle = zero))
	then
		Item->>style(zero)
	else (
		if
			CurrentStyle = zero
		then
			Item->>style(point)	%anders wat het was
		),

	Item->>key(RealValue),
	Item?object->>valueName(RealValue).
%%


%%
changeCurrentQuantity(QPB,
	Quantity : name):->
	"Explicitly set current quantity" ::

	Item = QPB<<-selection,
	\+ Item == @nil,

	QuantityName = Quantity<<-makeGarp,

        Item->>style(point),
	Item->>key(QuantityName),
	Item?object->>Quantity.
%%


%%
currentValues(QPB,
	Values: chain):<-
	"Return new chain with copies of value references" ::


	Values = QPB?members<<-map(@arg1?object?copy).
%%

	
:-pce_end_class.
		  

