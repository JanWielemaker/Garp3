/*
Definitie select_gesture klasse: Een subclass van gesture voor het
selecteren van graphicals met een selectierechthoek.
Op basis van xpce-draw draw_warp_select_gesture (Jan Wielemaker)

De gesture stuurt select/deselect callbacks. Wanneer de select callback voor een bepaalde graphical faalt
zal er geen deselect voor gestuurd worden. Hierdoor kan de gesture ook gebruikt worden voor uitbreiden van 
een bestaande selectie (select callback laten falen op een graphical die al geselecteerd is).
*/

:-pce_begin_class(select_gesture,
		  gesture,
		  "Implementation of a selection rectangle"
		 ).

variable(selectionBox, box, get).

variable(relevantArea,area*,get, "Area where there is something to select").

variable(selectCallback,code,get,"callback message for select").
%@arg1 = de graphical

variable(deselectCallback,code,get,"callback message for deselect (@arg1 = graphical)").
%@arg1 = de graphical

variable(subDevice,'device*',get,"Subdevice holding graphicals of interest").
%bijvoorbeeld een tree. Probleem is dat dit liever code was, maar dat wil
%nog niet werken

variable(currentSelection,chain,both,"Internal variable holding the graphicals currently in the selectionbox").
%hieraan zien we welke nieuw zijn

%Je kan er 1 maken met shift en 1 zonder, en bij die zonder in de condition
%de selectie weghalen en bij die met niet. Shift is dan selectie uitbreiden.

initialise(SG,
	   B : button = [button_name],
	   M : modifier = [modifier],
	   SelectMessage: selectMessage = code,
	   DeselectMessage: deselectMessage = code,
	   Sub: subDevice = '[device*]'
	  ):->
	SG->+initialise(B,M),
	Box *= box(0,0),
	Box->>texture(dotted),
	Box->>fill_pattern(@nil),
	SG->>slot(selectionBox,Box),
	SG->>slot(selectCallback,SelectMessage),
	SG->>slot(deselectCallback,DeselectMessage),
	default(Sub,@nil,SD),
	SG->>slot(subDevice,SD),
	SG->>slot(currentSelection,new(chain)).
%%

%%
initiate(SG,
	 Event: event
	):->
	%aangeroepen bij het begin van de gesture (na verify)
	%we moeten eerst de relevante area vinden

	Canvas = Event<<-receiver,
	RelevantArea *= area(0,0,0,0),
	Canvas?graphicals->>for_all(->>(RelevantArea,
					union,
					@arg1?area)),
	SG->>slot(relevantArea,RelevantArea),
	SG?selectionBox->>set(@default,@default,0,0), %lege rechthoek
	Canvas->>display(SG?selectionBox,Event?position),
	SG?currentSelection->>clear.
%%

%%
drag(SG,
     Event: event
    ):->
	%drag event: we moeten de selectiebox resizen
	%dit gebeurt alleen wanneer het nieuwe punt
	%zichtbaar is in het window (<-area) of in de
	%in initiate berekende relevantArea valt.

	Position = Event<<-position,
	Canvas = Event<<-receiver,
	CanvasArea = Canvas<<-visible,
	RelevantArea = SG?relevantArea<<-union(Canvas?visible),
	Box = SG?selectionBox,
	(   RelevantArea->>in(Position)
	->  (  Box->>corner(Position),
	       Event?receiver->>normalise(area(Position?x,
					Position?y))
	    )
	;   ( %we zoeken dus de positie die nog wel mag
	      fix_position(Position,RelevantArea,NewPosition),
	      Box->>corner(NewPosition),
	      Canvas->>normalise(area(NewPosition?x,
				      NewPosition?y)),
	      NewPosition->>done
	    )
	),
	CanvasArea->>done,
	RelevantArea->>done,

	%nu nog uitzoeken welke objecten ge(de-)selecteerd zijn
	%we doen dit niet met device<-inside, want dan moet de graphical
	%volledig in de area zijn. We vinden 1 pixel genoeg
	%moet dit op een subdevice?
	(   @nil = SG<<-subDevice
	->  SD = Canvas
	;   SD = SG<<-subDevice
	),

	NewSelection *= chain,
	SD?graphicals->>for_all(if(and(not(@arg1 == Box),
				       ->>(@arg1?area,
					 overlap,
					   Box?area
					  )),
				   ->>(NewSelection,
				       append,
				       @arg1
				      ) ) ),
	OldSelection = SG?currentSelection<<-copy,
	StillSelected = OldSelection<<-intersection(NewSelection),
	OldSelection->>subtract(StillSelected), %bevat gedeselecteerde
	NewSelection->>subtract(StillSelected), %bevat geselecteerde
	%weghalen gedeselecteerde uit de currentselection
	SG?currentSelection->>subtract(OldSelection),

	Deselect = SG<<-deselectCallback,
	Select = SG<<-selectCallback,
	OldSelection->>for_some(Deselect),
	NewSelection->>for_all(if(Select,
				  ->>(SG?currentSelection,
				      append,
				      @arg1))),

	OldSelection->>done,
	NewSelection->>done,
	StillSelected->>done.
%
fix_position(Pos,Area,NewPos):-
	PX = Pos<<-x,
	PY = Pos<<-y,
	AX1 = Area<<-left_side,
	AX2 = Area<<-right_side,
	AY1 = Area<<-top_side,
	AY2 = Area<<-bottom_side,
	fix(PX,AX1,AX2,NewX),
	fix(PY,AY1,AY2,NewY),
	NewPos *= point(NewX,NewY).
%
fix(C,C1,_C2,NewC):-
	C < C1,!,
	NewC = C1.

fix(C,_C1,C2,NewC):-
	C > C2,!,
	NewC is C2 - 1.

fix(C,_,_,C).
%%

%%
terminate(SG,
	  _Event: event
	 ):->
	%terminate: button up (in het window)

	SG?selectionBox->>device(@nil). %weg met de rechthoek
%%

:-pce_end_class.
