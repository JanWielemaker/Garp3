/*
definitie quantitySpaceEditor klasse.
Simpele editor voor het bewerken van quantity spaces

Initialise kan aangeroepen met een bestaande qs voor het bewerken ervan.
Wanneer initialise wordt gebruikt zonder argumenten wordt in new mode geopend.

based on homer code, so most comments in dutch. gp3 code only where mentioned
2000-2005 Jelmer Jellema, Spin in het Web, spininhetweb.nl

%gp3 1.4: this class has NO explicit support for translatable values. It works with them, but it does not make any assumption about values
%being translatable or not. This the changerequestor does.

*/

default_quantitySpaceName('New quantity space').

:-pce_begin_class(quantitySpaceEditor,
		  assistanceDialog,
		  "quantity space editor"
		 ).

variable(quantitySpace,quantitySpace*,get,
		"the edited quantity space"). %@nil betekent: nieuwe
%%
initialise(D, OpenedBy:[object]):->
	"Initialise the editor" ::
	%gp3 0.1: added openedBy, will be a hyper to the given object, so others can ask voor ?openedBy

	D->+initialise('Quantity spaces - Build','Build_QSpaces'),
	D->>icon(@build_icon),
	D->>application(@app),
	D->>kind(toplevel),

	%gp3: saved openedBy in a hyper
	default(OpenedBy,@nil,Opener),
	D->>hyper(Opener,openedBy),

	%lay-out:
	%we doen alles met de hand op coordinaten, anders
	%wordt de lay-out niet ok...

	%voor de plaatsing gebruiken we de standard "gap"
	GapX = D?gap<<-width,
	GapY = D?gap<<-height,
	MaxX *= number(0),

	%de onderdelen

	QSList *= extendedListBrowser(width := 30),
	QSList->>name(qsList),
	QSList->>label('Quantity spaces:'),
	QSList->>show_label(@on),
	QSList->>select_message(->>(D,
				       onQSSelection,
						@arg1?object)),
	QSList->>style(normaal,style(colour:=black)),
	QSList->>style(readonly,style(colour:=grey50)),

	D->>display(QSList,point(GapX,D?topY)), %geen align

	%display vd buttons is lastig: ten eerste moet de bovenste precies bij de bovenkant vd lijst
	%maar het moet ook mooi passen dus eventueel moet de lijst hoger
	%daarom maken we eerst alle knoppen voordat we ze plaatsen

	AddQS *= imgButton(addQS, img:=add, tt:='Add quantity space'),
	CopyQS *= imgButton(copyQS, img:=copy, tt:='Copy selected quantity space'),
	RemoveQS *= imgButton(removeQS, img:=remove, tt:='Delete selected quantity space'),

	%ok: aangezien alles gerelateerd is aan de bovenkant en de onderkant van de lijst
	%moeten we even kijken of dat wel gaat passen

	ListBottom *= number(QSList?bottom_side),
	ListBottom->>maximum(QSList?top_side + QSList?image?top_side +
					 AddQS?height +
					 CopyQS?height +
				     RemoveQS?height +
					 2 * GapY), %ruimte ingenomen door label vd lijst + buttons

	QSList->>bottom_side(ListBottom),

	%weergave Add button precies bij de bovenkant vd lijst:
	%QSList?image is het echte lijst window (text_image)
	%die de list_browser als device gebruikt.
	%Dus geeft QSList?image?top_side de afstand van bovenin
	%de list_browser tot de bovenkant vh window (dus de hoogte
	%van het label). Dit opgeteld bij de bovenkant van de
	%list_browser geeft dus de y-coordinaat in termen van de
	%dialoog.

	D->>display(AddQS,
		point(QSList?right_side + GapX,
			QSList?top_side+ QSList?image?top_side)), %hebben we nu list_top voor

	MaxX->>maximum(AddQS?right_side),


	%deze moet dus halverwege komen..
	D->>display(CopyQS,point(AddQS?left_side,
				AddQS?top_side + ((QSList?bottom_side - AddQS?top_side) / 2 )
					- (CopyQS?height / 2))),

	MaxX->>maximum(CopyQS?right_side),


	D->>display(RemoveQS,point(AddQS?left_side,
		QSList?bottom_side - RemoveQS?height)), %precies onder

	MaxX->>maximum(RemoveQS?right_side),

	%een lijntje
	Line *= line(GapX,QSList?bottom_side + GapY,0,QSList?bottom_side + GapY ),
	Line->>name(line1),
	Line->>pen(2),
	D->>display(Line), %rechterkant wordt hieronder wel gezet, als MaxX bekend is

	Name *= eventTextItem(qsName),
	Name->>label('Name:'),
	Name->>length(QSList?width),
	D->>display(Name, point(GapX,
						Line?bottom_side + GapY)),
	MaxX->>maximum(Name?right_side),

	QSShow *= qsPreviewBrowser(width := 30, height:=10),
	QSShow->>name(qsShow),
	QSShow->>label('Definition:'),
	QSShow->>select_message(->>(D,
							onValueSelect)),
	D->>display(QSShow,point(GapX,
							Name?bottom_side + GapY)),

	AddHigh *= imgButton(addHigh, tt:='Add value high'),
	D->>display(AddHigh, point(QSShow?right_side + GapX,
				QSShow?top_side + QSShow?image?top_side)),
				%zie hierboven bij AddQS

	%hoever naar rechts komt deze?
	HighX *= number(AddHigh?right_side),

	SplitInterval *= imgButton(splitInterval, tt:='Split interval'),
	D->>display(SplitInterval,point(AddHigh?left_side,
								AddHigh?bottom_side + GapY)),
	HighX->>maximum(SplitInterval?right_side), %misschien verder naar rechts?

	AddLow *= imgButton(addLow, tt:='Add value low'),
	D->>display(AddLow,point(AddHigh?left_side,
							SplitInterval?bottom_side + GapY)),

	HighX->>maximum(AddLow?right_side),
						%misschien verder naar rechts?

	RemoveHigh *= imgButton(removeHigh, tt:='Delete value high'),
	D->>display(RemoveHigh, point(HighX + GapX,
								AddHigh?top_side)),
	%dit kan de meeste rechtse zijn..
	MaxX->>maximum(RemoveHigh?right_side),
	CombineIntervals *= imgButton(combineIntervals, tt:='Combine intervals'),
	D->>display(CombineIntervals,point(RemoveHigh?left_side,
						SplitInterval?top_side)),
	MaxX->>maximum(CombineIntervals?right_side),

	RemoveLow *= imgButton(removeLow, tt:='Delete value low'),
	D->>display(RemoveLow, point(RemoveHigh?left_side,
							AddLow?top_side)),
	MaxX->>maximum(RemoveLow?right_side),

	QSShow->>bottom_side(RemoveLow?bottom_side),

	%gp3 0.2 moved value name to below list

	ValueType *= menu(valueMenu,marked, %FLNEW
					->>(D,onValueMenu)),
	send(ValueType, gap, size(0,5)),
	ValueType->>label('Value:'),
	ValueType->>layout(vertical), %FLNEW: added constants one and minus one for multiplication and division
	send(ValueType, append, menu_item(value:=custom, label:='')),
	send(ValueType, append, menu_item(value:=one, label:=' 1')),
	send(ValueType, append, menu_item(value:=zero, label:=' 0')),
	%send(ValueType, append, menu_item(value:=minusOne, label:='-1')),

	ValueName *= eventTextItem(valueName),
	ValueName->>show_label(@off),
	ValueName->>length(10), %will be recalculated
	ValueName->>afterKey(->>(D,
							onValueName)),

	D->>display(ValueType,point(GapX, QSShow?bottom_side + GapY)),
	D->>display(ValueName, point(GapX + 22, QSShow?bottom_side + GapY + 17)),

	Remarks *= editor(height := 5, width := 40),
	Remarks->>name(remarks),
	Remarks->>label('Remarks:'),
	Remarks->>show_label(@on),
	Remarks->>fill_mode(@on),
	Remarks->>font(Name?font),
	D->>display(Remarks,point(GapX,
							ValueType?bottom_side + GapY)),
	MaxX->>maximum(Remarks?right_side),
	%en nu weer een lijntje

	Line2 *= line(GapX,Remarks?bottom_side + GapY,0,Remarks?bottom_side + GapY ),
	Line2->>name(line2),
	Line2->>pen(2),
	D->>display(Line2), %rechterkant wordt hieronder wel gezet, als MaxX bekend is

	Save *= imgButton(save, tt:='Save changes'),

	D->>display(Save,point(GapX,
							Line2?bottom_side + GapY)),

	Cancel *= imgButton(cancel, img:= undo, tt:='Undo changes'),
	%deze komt rechts van save
	D->>display(Cancel,point(Save?right_side + GapX,
				Save?top_side)),

	%sluiten moet rechts komen, dus dat moet ook nog uitgezocht
	Close *= imgButton(close,->>(D,onDestroy), tt:='Close this editor'),
	MaxX->>maximum(Cancel?right_side + GapX + Close?width),
	%oftewel: eventueel wordt de dlg nog breder om deze button te laten passen
	%hoe dan ook helemaal rechts

	D->>display(Close,point(
		MaxX - Close?width,
		Save?top_side)),

	%Het een en ander moet nog mooi naar rechts
	%gp3 0.2: we do a lot more about placement here, because that makes resize easier
	AddQS->>set(x:= MaxX - AddQS?width),
	CopyQS->>set(x:= MaxX - CopyQS?width),
	RemoveQS->>set(x:= MaxX - RemoveQS?width),
	QSList->>right_side(AddQS?left_side - GapX),

	Line->>end_x(MaxX),
	Name->>right_side(MaxX),

	ValueName->>right_side(QSShow?right_side),
	Remarks->>right_side(Name?right_side),

	Line2->>end_x(MaxX),

	D->>updateSpacers, %gp3 0.3.13 needed when assistanceDialog is used, but displayContent is not used to fill the dialog
	D->>assign_accelerators, %nodig voor de accels als je niet append gebruikt

	%we roepen onDestroy aan om te checken of het wel mag
	%dit gaat via done_message vh frame, die roept gewoonlijk frame->>wm_delete aan
	D->>done_message(->>(D,onDestroy)),
	D->>confirm_done(@off), %niet vragen
				%minimal size:
	D->>minimalSize(size(MaxX,Close?bottom_side)), %abs min

	send(D, init_commands),

	/* Multiple models */
	get(@model, getModelNameForEditor, 'Quantity space definitions editor - Build', ModelNameForEditor),
	new(Garp3EditorFrame, garp3EditorFrame(ModelNameForEditor)),
	get(@model, '_value', Model),
	send(Garp3EditorFrame, associateModel, Model),
	send(Garp3EditorFrame, append, D).

%%

%%
init_commands(D) :->
	"Initialise command objects" ::

	send(D, command, 'QuantitySpaceDefCopy',  key := '\\ec', keystring := '[Alt + C]'),
	send(D, command, 'QuantitySpaceDefCopy',  key := '\\C-c', keystring := '[CTRL + C]'),
	send(D, command, 'QuantitySpaceDefPaste', key := '\\ev', keystring := '[Alt + V]'),
	send(D, command, 'QuantitySpaceDefPaste', key := '\\C-v', keystring := '[CTRL + V]').
%%

%%
onResize(D, Difference: size):->
	%gp3 0.2:

	D?qsList_member->>pixelWidth(D?qsList_member?pixelWidth + Difference?width),
	D?qsList_member->>pixelHeight(D?qsList_member?pixelHeight + Difference?height / 2),
	D?addQS_member->>set(x:= D?addQS_member?left_side + Difference?width),
	D?copyQS_member->>set(x:= D?addQS_member?left_side, y:= D?copyQS_member?top_side + Difference?height / 4),
	D?removeQS_member->>set(x:= D?addQS_member?left_side, y:= D?removeQS_member?top_side + Difference?height / 2),

	D?line1_member->>right_side(D?line1_member?right_side + Difference?width),
	D?line1_member->>set(y:= D?line1_member?top_side + Difference?height / 2),

	D?qsName_member->>right_side(D?qsName_member?right_side + Difference?width),
	D?qsName_member->>set(y:= D?qsName_member?top_side + Difference?height / 2),

	D?qsShow_member->>set(y:= D?qsShow_member?top_side + Difference?height / 2),
	D?qsShow_member->>pixelHeight(D?qsShow_member?pixelHeight + Difference?height / 2),
	D?qsShow_member->>right_side(D?qsShow?right_side + Difference?width),

	%also split the remaining height difference between the gaps between the buttons
	D?addHigh_member->>set(x:= D?addHigh_member?left_side + Difference?width, y:= D?qsShow_member?list_top),
	D?removeHigh_member->>set(x:=D?removeHigh_member?left_side + Difference?width, y:= D?qsShow_member?list_top),
	D?splitInterval_member->>set(x:=D?splitInterval_member?left_side + Difference?width, y:= D?qsShow_member?list_top + (D?qsShow_member?bottom_side - D?qsShow_member?list_top) / 2 - (D?splitInterval_member?height / 2)),
	D?combineIntervals_member->>set(x:=D?combineIntervals_member?left_side + Difference?width, y:= D?splitInterval_member?top_side),
	D?addLow_member->>set(x:=D?addLow_member?left_side + Difference?width, y:= D?qsShow_member?bottom_side - D?addLow_member?height),
	D?removeLow_member->>set(x:= D?removeLow_member?left_side + Difference?width, y:= D?addLow_member?top_side),
	D?valueMenu_member->>set(y:= D?valueMenu_member?top_side + Difference?height),
	D?valueName_member->>set(y:= D?valueName_member?top_side + Difference?height),
	D?valueName_member->>right_side(D?qsShow_member?right_side),

	D?remarks_member->>right_side(D?remarks_member?right_side + Difference?width),
	D?remarks_member->>set(y:=D?remarks_member?top_side + Difference?height),

	D?line2_member->>right_side(D?line1_member?right_side),
	D?line2_member->>set(y:= D?line2_member?top_side + Difference?height),

	D?save_member->>set(y:=D?save_member?top_side + Difference?height),
	D?cancel_member->>set(y:=D?save_member?top_side),
	D?close_member->>set(x:= D?close_member?left_side + Difference?width, y:=D?save_member?top_side).
%%

%%
onDestroy(D):->
	%check of destroy wel mag
	%zie initialise waarbij dit als done_message wordt gezet
	D->>checkSaveSelection,!, %ok er mag doorgegaan worden
	D->>wm_delete. %default done_message vh frame
%%

%%
openedBy(D, OpenedBy: object*):<-
	%gp3, give the object that opened this window or @nil
	OpenedBy = D<<-hypered(openedBy)
	;
	OpenedBy = @nil.
%%

%%
label(D,L: name):->
	%zorg ervoor dat het framelabel wordt gezet, niet het window_decorator label
	D?frame->>label(L).
%
label(D, L: name):<-
	L = D?frame<<-label.
%%

%%
edit(D,
	QS : [quantitySpace]
	):->
	"Open the editor for editing the given (or the first) quantity space" ::

	D->>fillQSList,
	pl_setQSSelection(QS,D,D?qsList),
	D->>open.
%
pl_setQSSelection(@default,D,List):-
	%alleen bij het openen: zet de zaken even goed.
	0 =List<<-length,!,
	List->>selection(@nil), %toevoegen
	D->>slot(quantitySpace,@nil),
	D->>fillCurrentQSData.
%
pl_setQSSelection(@default,D, List):-!,
	Q = List?members<<-head,
	List->>selection(Q),
	D->>slot(quantitySpace,Q?object),
	D->>fillCurrentQSData.
%
pl_setQSSelection(QS,D,List):-!,
	Item = List<<-member(QS),
	List->>selection(Item),
	D->>slot(quantitySpace,QS),
	D->>fillCurrentQSData.
%%

%%
fillQSList(D):->
	"Fill the list with quantity spaces" ::
	D?qsList->>clear,
	@model?sortedQuantitySpaces->>for_all(->>(D,fillQSList_helper,@arg1)).
%
fillQSList_helper(D,Def):->
	%voeg deze toe aan onze deflist
	if
		0 = Def?remarks<<-size
	then
		L *= string('%s',Def?name)
	else
		L *= string('%s*',Def?name),
	DI *= dict_item(Def,L,Def,when(Def == @model?mzpqs,readonly,normaal)),
	D?qsList->>append(DI).
%%


%%
onQSSelection(D,
	QS : quantitySpace*):->
	"Callback when quantity space selected" ::
	/*
	Het probleem is altijd dat hier nu al de nieuwe staat geselecteerd terwijl de mapping
	in het dataobject (<-quantitySpace) nog op de oude staat ivm opslaan..
	*/
	%clause 1: op dezelfde regel geklikt

	QS = D<<-quantitySpace,!.
%
onQSSelection(D,
	_QS):->
	%clause 2: we mogen niet door ivm de vorige selectie

	\+ D->>checkSaveSelection,!, %faalt dus terug op oude selectie

	if
		@nil = D<<-quantitySpace %we waren in new modus
	then
		D?qsList->>selection(@nil)
	else
		D?qsList->>selection(D?quantitySpace).
%
onQSSelection(D, QS):->
	%clause 3: we mogen door met de nieuwe selectie dus initialiseren we de data
	D->>slot(quantitySpace,QS),
	D->>fillCurrentQSData.
%%

%%
addQS(D):->
	"Add button pressed.." ::

	D->>checkSaveSelection,
	D->>slot(quantitySpace,@nil), %betekent: nieuwe
	D?qsList->>selection(@nil),
	D->>fillCurrentQSData,
	D?qsName->>activate(@on),
	D->>caret(D?qsName).

%%

onQuantitySpaceDefCopy(D) :->
    send(@copyBuffer, copyQuantitySpaceDef, D?quantitySpace).

onQuantitySpaceDefPaste(D) :->
    send(@copyBuffer, pasteQuantitySpaceDef, D).

%%
copyQS(D):->
	"Copy button pressed.." ::
	%lijkt op addQS: we maken gewoon een nieuwe met dezelfde values als de geselecteerde
	%maar wijzigen de naam en zet naam/type-kopien van de values (andere id) in de qsShow

	D->>checkSaveSelection,
	CurrentQS = D<<-quantitySpace,
	CurrentQS \== @nil,
	D->>fillCurrentQSData, %vult dus voor de huidige selectie (en zet save uit)
	NewName *= string('%s (copy)',CurrentQS?name),
	D?qsName->>selection(NewName),
	D->>slot(quantitySpace,@nil), %maar de bewerkte is een nieuwe
	MyValues = CurrentQS?values<<-map(@arg1?copyContent),
	%gp3 1.4: make sure the values are translatable. Needed when copying the default (untranslatable) mzp qs
	MyValues->>for_all(->>(@arg1,setTranslatable,when(->>(@arg1?valueName,equal,'Zero'),@off,@on))),

	D?qsShow->>setValues(MyValues),
	D?qsList->>selection(@nil),
	D?removeQS->>active(@off),
	D?copyQS->>active(@off),
	D?save_member->>active(@on).
%%

%%
removeQS(D):->
	"Remove button pressed.." ::

	%CR checkt de gevolgen wel en waarschuwt eventueel

	if
		not(@nil = D<<-quantitySpace)
	then
		@model->>changeRequest(deleteQuantitySpace,
			@model,
			D,
			D?quantitySpace).
%%

%%
fillCurrentQSData(D):->
	"Fill the dialog with the saved data for the current qs" ::
	%clause 1: geen qs geselecteerd: een nieuwe dus

	@nil = D<<-quantitySpace,!,
	%de remove en copy knop gaat dan uit, save mag aan
	D?removeQS->>active(@off),
	D?copyQS->>active(@off),
	D?save_member->>active(@on),
	default_quantitySpaceName(Name), %zie bovenin deze file
	D?qsName->>selection(Name),
	D?remarks->>contents(new(string)),
	D?qsShow->>clear,
	D?qsShow->>selection(@nil),
	D->>fillCurrentValueData.
%
fillCurrentQSData(D):->
	%clause 2: wel een qs geselecteerd

	QS = D<<-quantitySpace,
	%is het de vaste DQS?
	if
		QS = @model<<-mzpqs
	then
	(
		D?removeQS->>active(@off),
		D?save_member->>active(@off)
	)
	else
	(
		D?removeQS->>active(@on),
		D?save_member->>active(@on)
	),

	D?copyQS->>active(@on),
	QSShow = D<<-qsShow,
	QS = D<<-quantitySpace,
	D?qsName->>selection(QS?name),
	D?remarks->>contents(QS?remarks),
	QSShow->>preview(QS),
	QSShow->>selection(QSShow?members?head),
	D->>fillCurrentValueData.
%%

%%
fillCurrentValueData(D):->
	"Set information for currently selected value" ::

	%vullen van data en zetten van de controls
	%hangt af van selectie en van of er uberhaupt values zijn..

	QSShow = D<<-qsShow,

	(
	  0 = QSShow?members<<-size
	->
	  (
		%geen values: blijkbaar nog geen qs
		%dus gebruiken we wat knoppen voor toevoegen van punt en interval
		D?addHigh->>setImage(addPoint),
		D?addHigh->>tooltipMsg('Add point value'),
		D?addLow->>setImage(addInterval),
		D?addLow->>tooltipMsg('Add interval value'),
		D?combineIntervals->>active(@off),
		D?removeHigh->>active(@off),
		D?removeLow->>active(@off),
		D?valueMenu->>active(@off),
	        send(D, setValueMenuSelection, custom),
		D?valueName->>active(@off),
		D?valueName->>selection('')
	  )
	;
	  (
		D?addHigh->>setImage(addHigh),
		D?addHigh->>tooltipMsg('Add value high'),
		D?addLow->>setImage(addLow),
		D?addLow->>tooltipMsg('Add value low'),
		D?removeHigh->>active(@on),
		D?removeLow->>active(@on),

		D?valueMenu->>active(@on),  % FL: Constant one implementation: menu now always active, separate options can be inactive...
		send(D?valueMenu, active_item, custom, @on), % gets turned off in specific cases. turn on in all others...

		%er moet er 1 geselecteerd zijn
		(
		  @nil = QSShow<<-selection
		->
		  QSShow->>selection(QSShow?members?head)
		;
		  true
		),


		%split interval: mag aan wanneer er een interval is geselecteerd:
		(
		  interval = QSShow<<-selectedType
		->
		  D?splitInterval->>active(@on)
		;
		  D?splitInterval->>active(@off)
		),

		%combine interval: mag aan wanneer er een punt is geselecteerd en er
		%nog een waarde boven en onder zitten
		%NB deze setting kan overschreven worden door code hier beneden!
		(
		  (
			point = QSShow<<-selectedType,
			QSShow<<-nextItem, %extendedListBrowser faalt als het er niet is
			QSShow<<-prevItem %extendedListBrowser faalt als het er niet is
		   )
		->
		   D?combineIntervals->>active(@on)
		;
		  D?combineIntervals->>active(@off)
		),
		get(QSShow, currentValues, ValuesChain),
		get(QSShow, value, CurrentPoint),
		% Specifieke constraints ivm met correct Qspaces met de constanten 1, 0, -1
		% voor de delete en combine interval knoppen:
		send(D, setConstantConstraints, ValuesChain, CurrentPoint),


		%values menu setting of controls(previously zero menu)
		(
		  send(QSShow, oneSelected)
		->
		  %Do one value
		  send(D, setValueMenuSelection, one),
		  D?valueName->>selection(''),
		  D?valueName->>active(@off)
		;
		  (
		    send(QSShow, zeroSelected)
		  ->
		    %Do zero value
		    send(D, setValueMenuSelection, zero),
		    D?valueName->>selection(''),
		    D?valueName->>active(@off)
		  ;
		    (
		      fail %send(QSShow, minusOneSelected) % minusone removed
		    ->
		      %Do minusOne value
		      send(D, setValueMenuSelection, minusOne),
		      D?valueName->>selection(''),
		      D?valueName->>active(@off)
		    ;
		      %Do Custom value
		      send(D, setValueMenuSelection, custom),
		      D?valueName->>active(@on),
		      D?valueName->>selection(QSShow?value)
		    )
		  )
		),
		% valuesmenu is vrij of niet:
		(
		  interval = QSShow<<-selectedType
		->
		  send(D, setValueMenuConstantsActive, @off)
		;
		  send(D, setValueMenuConstantsActive, @on),
		  % bepaal of er nog specifieke constraints zijn:
		  send(D, setValueMenuPointConstraints, ValuesChain, CurrentPoint)
		)
	  )
	).
%%

%%
onValueSelect(D):->
	"Value selected" ::

	D->>fillCurrentValueData,
	D->>selectValueName.
%%

%%
onValueMenu(D):->
	"Valuemenu selected" ::
	%we gaan er vanuit dat de activate state enzo goed staat.
	(
	  @on = D?valueMenu<<-selected(zero)
	->
	  %DOZERO
	  D?valueName->>active(@off),
	  D?valueName->>keyboard_focus(@off), %gp3 0.3
	  D?qsShow->>changeCurrent('Zero')
	;
	  (
	    @on = D?valueMenu<<-selected(one)
	  ->
	    %DOONE
	    D?valueName->>active(@off),
	    D?valueName->>keyboard_focus(@off), %gp3 0.3
	    D?qsShow->>changeCurrent('One')
	  ;
	    (
	      fail %@on = D?valueMenu<<-selected(minusOne) % minusone removed
	    ->
	      %DOMINUSONE
	      D?valueName->>active(@off),
	      D?valueName->>keyboard_focus(@off), %gp3 0.3
	      D?qsShow->>changeCurrent('MinusOne')
	    ;
	      (
	        @on = D?valueMenu<<-selected(custom)
	      ->
	        %DOCUSTOM
		D?valueName->>active(@on),
		D?valueName->>keyboard_focus(@on),
		D?qsShow->>changeCurrent(D?valueName?selection),
		D->>selectValueName
	      ;
	        true % this should never happen!-)
	      )
	    )
	  )
	).
%%

%%
onValueName(D):->
	"Value name changed" ::
	%geven we direct door aan de qs preview, uitzondering:
	%als de value name nu "zero" is en het is een point dan geven
	%we door dat het een zero is

	D?qsShow->>changeCurrent(D?valueName?selection).
%%

%%
addHigh(D):->
	"Add high button clicked" ::
	%als er geen elementen zijn is dit "Add point"..
	%dit sturen we gewoon door naar de qs preview

	if
		D?qsShow?members->>empty
	then
		NewItem = D?qsShow<<-firstValue('Point',point)
	else
		NewItem = D?qsShow<<-addHigh('Highest'),
	D?qsShow->>selection(NewItem),
	D->>fillCurrentValueData,
	D->>selectValueName.
%%

%%
addLow(D):->
	"Add low button clicked" ::
	%als er geen elementen zijn is dit "Add interval" ..
	%dit gaat naar de qs preview


	if
		D?qsShow?members->>empty
	then %toevoegen van een interval
		NewItem = D?qsShow<<-firstValue('Interval',interval)
	else %toevoegen van een low value
		NewItem = D?qsShow<<-addLow('Lowest'),
	D?qsShow->>selection(NewItem),
	D->>fillCurrentValueData,
	D->>selectValueName.
%%

%%
splitInterval(D):->
	"Split interval button clicked" ::
	%dus deze interval moet gesplit worden in interval-punt-interval

	Value = D?qsShow<<-value,
	Item = D?qsShow<<-selection,
	Low *= string('Below %s',Value),
	High *= string('Above %s',Value),
	D?qsShow->>splitInterval(Item,Low,Value,High),
	D->>fillCurrentValueData.
%%

%%
combineIntervals(D):->
	"Combine intervals button clicked" ::
	%dus deze interval-punt-interval moet samengevoegd worden in één interval

	Value = D?qsShow<<-value,
	Low = D?qsShow?nextItem<<-key,
	High = D?qsShow?prevItem<<-key,
	NewName *= string('%s-%s-%s',Low,Value,High),
	D?qsShow->>combineIntervals(D?qsShow?selection,NewName),
	D->>fillCurrentValueData.
%%

%%
removeHigh(D):->
	"Remove high button clicked" ::

	D?qsShow->>removeHigh,
	%als mogelijk een selectie
	ignore(D?qsShow->>selection(D?qsShow?members?head)), %hoogste als ie er is
	D->>fillCurrentValueData.
%%

%%
removeLow(D):->
	"Remove low button clicked" ::

	D?qsShow->>removeLow,
	%selectie?
	ignore(D?qsShow->>selection(D?qsShow?members?tail)),
	D->>fillCurrentValueData.
%%

%%%%%%%%%%%%%SAVE EN CANCEL E.D%%%%%%%%%%%%%%%%%%%%%%
%%
checkSaveSelection(D):->
	"Check changed state of current selection and ask for saving, fail if change canceled or save failed" ::
	D->>notChanged,!. %niets aan de hand
%
checkSaveSelection(D):->
	%ok, er is iets gewijzigd, dus moet er misschien opgeslagen worden

	Dlg *= dialog('Confirm changes'),
	Dlg->>application(@app),
	Dlg->>append(text('You made changes to this quantity space. Do you want to save them?')),
	SC *= imgButton(save_changes,
			    ->>(Dlg,return,save), img:=save, tt:='Save changes to model'),
	Dlg->>append(SC),
	CC *= imgButton(cancel_changes,->>(Dlg,return,cancel), img:=undo, tt:='Cancel changes'),
	Dlg->>append(CC),
	EC *= imgButton(edit_changes,->>(Dlg,return,edit),tt:='Edit changes'),
	Dlg->>append(EC),
	Dlg->>transient_for(D),
	Dlg->>modal(transient),
	Dlg->>kind(toplevel), %wel een titelbalk
	Answer = Dlg<<-confirm_centered(D?frame?area?center),
	Dlg->>destroy,
	if
		Answer = save
	then
		D->>saveQS
	else
		Answer = cancel. %falen bij edit
%%

%%
changed(D):->
	"Succeeeds when the qs is changed by the user" ::

	%dit draaien we even om
	\+ D->>notChanged.
%%

%%
notChanged(D):->
	%slaag als er niets is gewijzigd
	%1: voor een nieuwe definitie, alles moet nog gelijk zijn aan de defaults
	@nil = D<<-quantitySpace,!,
	default_quantitySpaceName(Name), %zie bovenin deze file
	D?qsName?selection->>equal(Name),
	0 = D?remarks?contents<<-size,
	D?qsShow?currentValues->>empty.
%
notChanged(D):->
	%2: de fixed dqs, die is nooit gewijzigd, ookal heeft de user lopen rommelen

	DQS = D<<-quantitySpace,
	DQS = @model<<-mzpqs,!.
%
notChanged(D):->
	%3: Een al eerder opgeslagen definitie
	%we beschouwen het als gewijzigd als het sowieso anders, is, geen gedoe met makeGarp op dit nivo

	\+ @nil = D<<-quantitySpace,
	D?qsName?selection->>equal(D?quantitySpace?name),
	D?remarks?contents->>equal(D?quantitySpace?remarks),

	%de lijst met values moet precies hetzelfde zijn
	%ook op object nivo dus (id + name + type)
	NewValues = D?qsShow<<-currentValues,
	OldValues = D?quantitySpace<<-values,

	S = NewValues<<-size,
	S = OldValues<<-size,
	OldValues->>for_all_pairs(NewValues, %object_extensions
									->>(@arg1,equal,@arg2)).
							 %equal: zelfde inhoud + zelfde id (maar mag verschillende objecten)
%%

%%
cancel(D):->
	"Cancel button" ::
	%wanneer we in new modus waren (quantitySpace = @nil), dan gaan we daar uit door
	%de onderste quantity te selecteren...

	List = D<<-qsList,
	if @nil = D<<-quantitySpace
	then	(
				%als er geen entries in de lijst zijn, blijft het gewoon @nil
				%als er wel entries zijn, dan selecteren we de laatste

				if ( \+ List?members->>empty )
				then	(
							Item = List?members<<-tail,
							D->>slot(quantitySpace,Item?object),
							List->>selection(Item)
						)
			),
	%in alle gevallen opnieuw data inlezen
	D->>fillCurrentQSData.
%%

%%
save(D):->
	"Save button" ::

	D->>saveQS,
	D->>fillCurrentQSData. %opnieuw inlezen
%%

%%
saveQS(D):->
	"Save the changes that were made" ::

	D->>notChanged,!. %niets te doen
%
saveQS(D):->
	%kee, nu moeten we een CR bouwen
	%hangt af of we in nieuw modus zijn of niet

	if
		@nil = D<<-quantitySpace
	then
		@model->>changeRequest(addQuantitySpace,
				@model,
				D,
				D?qsName?selection,
				D?qsShow?currentValues,
				D?remarks?contents)
	else
		@model->>changeRequest(changeQuantitySpace,
				D?quantitySpace,
				D,
				D?qsName?selection,
				D?qsShow?currentValues,
				D?remarks?contents).
%%


%%%%%%%wat handige verwijzingen naar members%%%%%%%%%%%%%%
%%
qsList(D,
			L : list_browser
			):<-
	"Mapping on qs list member" ::

	L = D<<-member(qsList).
%%

%%
copyQS(D,
	B : button):<-
	"Mapping on copyQS button member" ::

	B = D<<-member(copyQS).
%%

%%
removeQS(D,
	B : button):<-
	"Mapping on removeQS button member" ::

	B = D<<-member(removeQS).
%%

%%
qsName(D,
		N : text_item
		):<-
	"Mapping on qs name item" ::
	N = D<<-member(qsName).
%%

%%
qsShow(D,
	Q : qsPreviewBrowser
	):<-
	"Mapping on qsshow list member" ::

	Q = D<<-member(qsShow).
%%

%%
remarks(D,
	R : editor
	):<-
	"Mapping on remarks editor member" ::

	R = D<<-member(remarks).
%%

%%
addHigh(D,
	B : button):<-
	"Mappping on add High button" ::

	B = D<<-member(addHigh).
%%

%%
removeHigh(D,
	B : button):<-
	"Mapping on remove high button" ::

	B = D<<-member(removeHigh).
%%

%%
addLow(D,
	B : button):<-
	"Mapping on add low button" ::

	B = D<<-member(addLow).
%%

%%
removeLow(D,
	B : button):<-
	"Mapping on remove low button" ::

	B = D<<-member(removeLow).
%%

%%
splitInterval(D,
	B: button):<-
	"Mapping on split interval button" ::

	B = D<<-member(splitInterval).
%%

%%
combineIntervals(D,
	B : button):<-
	"Mapping on combine intervals button" ::

	B = D<<-member(combineIntervals).
%%

%%
valueMenu(D,
	M : menu):<-
	"Mapping on zero menu" ::

	M = D<<-member(valueMenu).
%%

%%
valueName(D,
	T : text_item):<-
	"Mapping on value name item" ::

	T = D<<-member(valueName).
%%

%%%%%%%%%%Helpers%%%%%%%%%%%%%%%%
%%
selectValueName(D):->
	"Make value name entry selected" ::

	D?valueName->>activate(@on),
	D->>caret(D?valueName).
%%

%%%%%%%%%%Change requestors%%%%%%%%%%%%%%%
%%
changeApplied_addQuantitySpace(D,
	CR:changeRequestor):->
	/*Er is een quantityspace bijgekomen. Dit kan best in deze editor zijn gebeurd. In dat geval
		moet de nieuwe quantity space worden geselecteerd (tenzij we opslaan doordat er ineens een
		andere is geselecteerd). Wanneer het in een andere is gebeurd dan
		hoeven we alleen de lijst bij te werken.
	Net als bij changeQuantitySpace kan  het opslaan gebeuren doordat de boel in deze editor wordt
	opgeslagen nadat een andere quantity space is geselecteerd: het gaat bij het herselecteren dan om
	de oude selectie en niet om de opgeslagen D?quantitySpace
	*/
	List = D<<-qsList,
	OldSelection = List<<-selection,
	if
	    OldSelection = @nil
	then
	    OldQS = @nil
	else
	    OldQS = OldSelection<<-object,

	D->>fillQSList,
	if
	(
	    CR->>checkEditor(D),
	    OldSelection = @nil %en we zijn nog braaf in new modus
	)
	    then
	(
	    List->>selection(CR?result),
	    D->>slot(quantitySpace,CR?result), %de nieuwe selectie
	    D->>fillCurrentQSData
	)
	else
	(
	    %we moeten dus de oude selectie herstellen
	    if
		(OldQS \== @nil)
	    then
		List->>selection(OldQS)
	).
%%

%%
changeApplied_changeQuantitySpace(D,
		_CR:changeRequestor):->
	%we weten zeker dat de geselecteerde QS nog steeds bestaat, dus daar hoeven we niet op te checken
	%het enige dat we doen is de lijst hervullen en de oude selectie herstellen
	%wanneer de thans geselecteerde QS dezelfde is als de interne "huidige" QS, en deze is gewijzigd
	%dan lezen we de gegevens opnieuw in.
	%Dit gebeurt dus niet op het nivo van de <-editor van de CR, maar op de test: is degene die is afgebeeld gewijzigd
	%(bij selecteren van een andere QS wordt op deze manier hier geen data opnieuw weergegeven, omdat de opgeslagen
	% QS wel de huidige <-quantitySpace is, maar niet de geselecteerde in de lijst)

	@nil = D?qsList<<-selection,!,
	D->>fillQSList.
%
changeApplied_changeQuantitySpace(D,CR):->
	%CLAUSE 2: er is wel een selectie, dus die is er nog na de change
	SelectedQS = D?qsList?selection<<-object,
	D->>fillQSList,
	D?qsList->>selection(SelectedQS), %dit kan gewoon ipv helemaal find etc

	%ok, als SelectedQS gelijk is aan het object van de CR én aan de interne <-quantitySpace
	%dan moeten we opnieuw inlezen
	if
	    (	CR->>checkObject(SelectedQS),
		SelectedQS = D<<-quantitySpace
	    )
	then
	    (D->>fillCurrentQSData).
%%

%%
changeApplied_deleteQuantitySpace(D,
	CR : changeRequestor
	):->
	/*
	Het kan dus zijn dat de geselecteerde QS weg is. In dat geval kiezen we een andere
	*/

	List = D<<-qsList,
	DeletedQS = CR<<-argument(1),
	if
	    (DeletedQS = D<<-quantitySpace)
	then (
	    %we moeten een nieuwe selectie kiezen en de boel opnieuw inlezen
	    (
		NewQS = List?nextItem<<-object
	    ;	NewQS = List?prevItem<<-object
	    ;	NewQS = @nil
	    ),

	    List->>selection(NewQS),
	    D->>slot(quantitySpace,NewQS),
	    D->>fillCurrentQSData
	    ),
	List->>delete(DeletedQS).
%%


%% Set the value menu selection.
% used internally when changing selected value in QS.
setValueMenuSelection(D, Select):->
	memberchk(Select, [custom, one, zero]), % , minusOne
	send(D?valueMenu, clear_selection),
	send(D?valueMenu, selected, Select, @on).
%%


%%
setValueMenuConstantsActive(D, Activity):->
	send(D?valueMenu, active_item, one, Activity),
	send(D?valueMenu, active_item, zero, Activity).
	%send(D?valueMenu, active_item, minusOne, Activity).
%%



%%
%% setValueMenuPointConstraints/3
% Function is called when on a point.
% This ensures only logical quantity spaces can be constructed by
% greying out constant buttons as they are illogical:
%
%    |
%    1
%    |
%    0     ?    Currentvalue
%    |
%   -1
%    |
%
% Set of points in quantityspace above currentvalue:
% - contains -1: -1, 0 and 1 cannot be used
% - contains 0 (not -1): 0 and 1 cannot be used
% - contains 1 (not 0 and -1): 1 cannot be used and -1 also not!
%
% Set of points in quantityspace below currentvalue:
% - contains 1: 1, 0 and -1 cannot be used
% - contains 0 (not 1): 0 and -1 cannot be used
% - contains -1 (not 0 and 1): -1 cannot be used and 1 also not!
%
% At zero and 1 above or -1 above: custom cannot be used (cannot delete
% zero if 1 and/or -1 in use)
%
% Set of points doesn't contain 0: no 1 and -1
% (no construction of qs with constants but without zero)
%
% Also set delete buttons to inactive when these can produce an invalid
% qspace:
%   Qspace = [First, ..., Last]
%   First = 0 ==> cannot delete if 1 is part of rest
%   Last = 0 ==> cannot delete if -1 is part of rest
%
setValueMenuPointConstraints(D, ValuesChain, CurrentPoint):->
	chain_list(ValuesChain, ValuesList),
	findall(Value, (member(Item, ValuesList), get(Item, valueName, Value)), Qspace),
	append(Above, [CurrentPoint|Below], Qspace),
	% first process set above current value
	(
	  memberchk('Minusone', Above)
	->
	  send(D?valueMenu, active_item, one, @off),
	  send(D?valueMenu, active_item, zero, @off)
	  %send(D?valueMenu, active_item, minusOne, @off)
	;
	  (
	    memberchk('Zero', Above)
	  ->
	    send(D?valueMenu, active_item, one, @off),
	    send(D?valueMenu, active_item, zero, @off)
	  ;
	    (
	      memberchk('One', Above)
	    ->
	      send(D?valueMenu, active_item, one, @off)
	      %send(D?valueMenu, active_item, minusOne, @off)
	    ;
	      true % no constraints in this case...
	    )
	  )
	),
	(
	  memberchk('One', Below)
	->
	  send(D?valueMenu, active_item, one, @off),
	  send(D?valueMenu, active_item, zero, @off)
	  %send(D?valueMenu, active_item, minusOne, @off)
	;
	  (
	    memberchk('Zero', Below)
	  ->
	    %send(D?valueMenu, active_item, minusOne, @off),
	    send(D?valueMenu, active_item, zero, @off)
	  ;
	    (
	      memberchk('Minusone', Below)
	    ->
	      send(D?valueMenu, active_item, one, @off)
	      %send(D?valueMenu, active_item, minusOne, @off)
	    ;
	      true % no constraints in this case...
	    )
	  )
	),
        %if on zero and one above or minusOne below, then zero cannot be removed
	(
	  CurrentPoint = 'Zero',
	  (
	    memberchk('One', Above)
	  ;
	    memberchk('Minusone', Below)
	  )
	->
	  send(D?valueMenu, active_item, custom, @off)
        ;
	  true
	),
	%if set above and below doesn't contain zero, then no 1 and -1
	(
	  \+ memberchk('Zero', Above),
	  \+ memberchk('Zero', Below)
	->
	  send(D?valueMenu, active_item, one, @off)
	  %send(D?valueMenu, active_item, minusOne, @off)
        ;
	  true
	).
%%

%% setConstantConstraints/3
% This ensures only logical quantity spaces can be constructed by
% greying out delete and combine interval buttons as they are
% illogical:
%
%    |
%    1
%    |
%    0     ?    Currentvalue
%    |
%   -1
%    |
%
% At zero and 1 above or -1 above: custom cannot be used (cannot delete
% zero if 1 and/or -1 in use)
%
% Set of points doesn't contain 0: no 1 and -1
% (no construction of qs with constants but without zero)
%
% Also set delete buttons to inactive when these can produce an invalid
% qspace:
%   Qspace = [First, ..., Last]
%   First = 0 ==> cannot delete if 1 is part of rest
%   Last = 0 ==> cannot delete if -1 is part of rest
%
setConstantConstraints(D, ValuesChain, CurrentPoint):->
	chain_list(ValuesChain, ValuesList),
	findall(Value, (member(Item, ValuesList), get(Item, valueName, Value)), Qspace),
	append(Above, [CurrentPoint|Below], Qspace),

	%if on zero and one above or minusOne below, then zero cannot be removed
	(
	  CurrentPoint = 'Zero',
	  (
	    memberchk('One', Above)
	  ;
	    memberchk('Minusone', Below)
	  )
	->
	  D?combineIntervals->>active(@off)
        ;
	  true
	),
	%if First item of Qspace is zero and set below contains -1, then no delete of this top zero value
	(
	  Qspace = ['Zero'|RestBelow],
	  memberchk('Minusone', RestBelow)
	->
	  D?removeHigh->>active(@off)
	;
	  true
	),
	%if Last item of Qspace is zero and set before contains -1, then no delete of this top zero value
	(
	  append(RestAbove, ['Zero'], Qspace),
	  memberchk('One', RestAbove)
	->
	  D?removeLow->>active(@off)
	;
	  true
	).
%%
:-pce_end_class.
