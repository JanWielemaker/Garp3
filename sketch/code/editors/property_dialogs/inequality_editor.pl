/*
definitie sketchInequalityEditor klasse.
Simpele editor voor het bewerken van sketchInequalities

Initialise kan aangeroepen met een bestaande sketchIneq voor het bewerken ervan.
Wanneer initialise wordt gebruikt zonder argumenten wordt in new mode geopend.

based on homer code, so most comments in dutch. gp3 code only where mentioned
2000-2005 Jelmer Jellema, Spin in het Web, spininhetweb.nl
*/

default_sketchInequalityName('New sketchInequality').

:-pce_begin_class(sketchInequalityEditor,
		  assistanceDialog,
		  "sketchInequality editor"
		 ).

variable(sketchInequality,sketchInequality*,get, 
		"the edited sketchInequality"). %@nil betekent: nieuwe
%%
initialise(D, OpenedBy:[object]):->
	"Initialise the editor" ::
	%gp3 0.1: added openedBy, will be a hyper to the given object, so others can ask voor ?openedBy
	
	D->+initialise('sketchInequalities - Build','Build_SketchIneqs'),
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

	SketchIneqList *= extendedListBrowser(width := 30),
	SketchIneqList->>name(sketchIneqList),
	SketchIneqList->>label('sketchInequalities:'),
	SketchIneqList->>show_label(@on),
	SketchIneqList->>select_message(->>(D,
				       onSketchIneqSelection,
						@arg1?object)),
	SketchIneqList->>style(normaal,style(colour:=black)), 
	SketchIneqList->>style(readonly,style(colour:=grey50)), 

	D->>display(SketchIneqList,point(GapX,D?topY)), %geen align

	%display vd buttons is lastig: ten eerste moet de bovenste precies bij de bovenkant vd lijst
	%maar het moet ook mooi passen dus eventueel moet de lijst hoger
	%daarom maken we eerst alle knoppen voordat we ze plaatsen

	AddSketchIneq *= imgButton(addSketchIneq, img:=add, tt:='Add sketchInequality'),
	CopySketchIneq *= imgButton(copySketchIneq, img:=copy, tt:='Copy selected sketchInequality'),
	RemoveSketchIneq *= imgButton(removeSketchIneq, img:=remove, tt:='Delete selected sketchInequality'),

	%ok: aangezien alles gerelateerd is aan de bovenkant en de onderkant van de lijst
	%moeten we even kijken of dat wel gaat passen

	ListBottom *= number(SketchIneqList?bottom_side),
	ListBottom->>maximum(SketchIneqList?top_side + SketchIneqList?image?top_side + 
					 AddSketchIneq?height + 
					 CopySketchIneq?height + 
				     RemoveSketchIneq?height + 
					 2 * GapY), %ruimte ingenomen door label vd lijst + buttons

	SketchIneqList->>bottom_side(ListBottom),

	%weergave Add button precies bij de bovenkant vd lijst:
	%SketchIneqList?image is het echte lijst window (text_image)
	%die de list_browser als device gebruikt.
	%Dus geeft SketchIneqList?image?top_side de afstand van bovenin
	%de list_browser tot de bovenkant vh window (dus de hoogte
	%van het label). Dit opgeteld bij de bovenkant van de
	%list_browser geeft dus de y-coordinaat in termen van de
	%dialoog.

	D->>display(AddSketchIneq,
		point(SketchIneqList?right_side + GapX,
			SketchIneqList?top_side+ SketchIneqList?image?top_side)), %hebben we nu list_top voor

	MaxX->>maximum(AddSketchIneq?right_side),


	%deze moet dus halverwege komen..
	D->>display(CopySketchIneq,point(AddSketchIneq?left_side,
				AddSketchIneq?top_side + ((SketchIneqList?bottom_side - AddSketchIneq?top_side) / 2 )
					- (CopySketchIneq?height / 2))),

	MaxX->>maximum(CopySketchIneq?right_side),

	
	D->>display(RemoveSketchIneq,point(AddSketchIneq?left_side,
		SketchIneqList?bottom_side - RemoveSketchIneq?height)), %precies onder

	MaxX->>maximum(RemoveSketchIneq?right_side),

	%een lijntje
	Line *= line(GapX,SketchIneqList?bottom_side + GapY,0,SketchIneqList?bottom_side + GapY ),
	Line->>name(line1),
	Line->>pen(2), 
	D->>display(Line), %rechterkant wordt hieronder wel gezet, als MaxX bekend is

	Name *= eventTextItem(sketchIneqName), 
	Name->>label('Name:'),
	Name->>length(SketchIneqList?width),
	D->>display(Name, point(GapX,
						Line?bottom_side + GapY)),
	MaxX->>maximum(Name?right_side),

	SketchIneqShow *= sketchIneqPreviewBrowser(width := 30, height:=10), 
	SketchIneqShow->>name(qsShow),
	SketchIneqShow->>label('Definition:'),
	SketchIneqShow->>select_message(->>(D,
							onValueSelect)),
	D->>display(SketchIneqShow,point(GapX,
							Name?bottom_side + GapY)),

	AddHigh *= imgButton(addHigh, tt:='Add value high'),
	D->>display(AddHigh, point(SketchIneqShow?right_side + GapX,
				SketchIneqShow?top_side + SketchIneqShow?image?top_side)),
				%zie hierboven bij AddSketchIneq
	
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
	
	SketchIneqShow->>bottom_side(RemoveLow?bottom_side),

	%gp3 0.2 moved value name to below list

	ValueType *= menu(zeroMenu,toggle,
					->>(D,onZero)),
	ValueType->>label('Value:'),
	ValueType->>layout(horizontal),
	ValueType->>append(zero),

	ValueName *= eventTextItem(valueName),
	ValueName->>show_label(@off), 
	ValueName->>length(10), %will be recalculated
	ValueName->>afterKey(->>(D,
							onValueName)),
	
	D->>display(ValueType,point(GapX, SketchIneqShow?bottom_side + GapY)),
	D->>display(ValueName, point(ValueType?right_side + GapX, SketchIneqShow?bottom_side + GapY)),

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
	AddSketchIneq->>set(x:= MaxX - AddSketchIneq?width),
	CopySketchIneq->>set(x:= MaxX - CopySketchIneq?width),
	RemoveSketchIneq->>set(x:= MaxX - RemoveSketchIneq?width),
	SketchIneqList->>right_side(AddSketchIneq?left_side - GapX),
	
	Line->>end_x(MaxX),
	Name->>right_side(MaxX),
	
	ValueName->>right_side(SketchIneqShow?right_side),
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
	% Multiple model support
	get(@model, getModelNameForEditor, 'Sketch Inequalities - Sketch', ModelNameForEditor),
	new(Garp3EditorFrame, garp3EditorFrame(ModelNameForEditor)),
	get(@model, '_value', Model),
	send(Garp3EditorFrame, associateModel, Model),
	send(Garp3EditorFrame, append, D),
	send(Garp3EditorFrame, transient_for, F).

	
%%

%%
onResize(D, Difference: size):->
	%gp3 0.2: 
	
	D?sketchIneqList_member->>pixelWidth(D?sketchIneqList_member?pixelWidth + Difference?width),
	D?sketchIneqList_member->>pixelHeight(D?sketchIneqList_member?pixelHeight + Difference?height / 2),
	D?addSketchIneq_member->>set(x:= D?addSketchIneq_member?left_side + Difference?width),
	D?copySketchIneq_member->>set(x:= D?addSketchIneq_member?left_side, y:= D?copySketchIneq_member?top_side + Difference?height / 4),
	D?removeSketchIneq_member->>set(x:= D?addSketchIneq_member?left_side, y:= D?removeSketchIneq_member?top_side + Difference?height / 2),
	
	D?line1_member->>right_side(D?line1_member?right_side + Difference?width),
	D?line1_member->>set(y:= D?line1_member?top_side + Difference?height / 2),
	
	D?sketchIneqName_member->>right_side(D?sketchIneqName_member?right_side + Difference?width),
	D?sketchIneqName_member->>set(y:= D?sketchIneqName_member?top_side + Difference?height / 2),
	
	D?sketchIneqShow_member->>set(y:= D?sketchIneqShow_member?top_side + Difference?height / 2),
	D?sketchIneqShow_member->>pixelHeight(D?sketchIneqShow_member?pixelHeight + Difference?height / 2),
	D?sketchIneqShow_member->>right_side(D?sketchIneqShow?right_side + Difference?width),
	
	%also split the remaining height difference between the gaps between the buttons
	D?addHigh_member->>set(x:= D?addHigh_member?left_side + Difference?width, y:= D?sketchIneqShow_member?list_top),
	D?removeHigh_member->>set(x:=D?removeHigh_member?left_side + Difference?width, y:= D?sketchIneqShow_member?list_top),
	D?splitInterval_member->>set(x:=D?splitInterval_member?left_side + Difference?width, y:= D?sketchIneqShow_member?list_top + (D?sketchIneqShow_member?bottom_side - D?sketchIneqShow_member?list_top) / 2 - (D?splitInterval_member?height / 2)),
	D?combineIntervals_member->>set(x:=D?combineIntervals_member?left_side + Difference?width, y:= D?splitInterval_member?top_side),
	D?addLow_member->>set(x:=D?addLow_member?left_side + Difference?width, y:= D?sketchIneqShow_member?bottom_side - D?addLow_member?height),
	D?removeLow_member->>set(x:= D?removeLow_member?left_side + Difference?width, y:= D?addLow_member?top_side),
	D?zeroMenu_member->>set(y:= D?zeroMenu_member?top_side + Difference?height),
	D?valueName_member->>set(y:= D?valueName_member?top_side + Difference?height),
	D?valueName_member->>right_side(D?sketchIneqShow_member?right_side),
	
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
	SketchIneq : [sketchInequality]
	):->
	"Open the editor for editing the given (or the first) sketchInequality" ::

	D->>fillSketchIneqList,
	pl_setSketchIneqSelection(SketchIneq,D,D?sketchIneqList),
	D->>open.
%
pl_setSketchIneqSelection(@default,D,List):-
	%alleen bij het openen: zet de zaken even goed.
	0 =List<<-length,!,
	List->>selection(@nil), %toevoegen
	D->>slot(sketchInequality,@nil),
	D->>fillCurrentSketchIneqData.
%
pl_setSketchIneqSelection(@default,D, List):-!,
	Q = List?members<<-head,
	List->>selection(Q),
	D->>slot(sketchInequality,Q?object),
	D->>fillCurrentSketchIneqData.
%
pl_setSketchIneqSelection(SketchIneq,D,List):-!,
	Item = List<<-member(SketchIneq),
	List->>selection(Item),
	D->>slot(sketchInequality,SketchIneq),
	D->>fillCurrentSketchIneqData.
%%

%%
fillSketchIneqList(D):->
	"Fill the list with sketchInequalities" ::

	D?sketchIneqList->>clear,
	@model?sortedQuantitySpaces->>for_all(->>(D,fillSketchIneqList_helper,@arg1)).
%
fillSketchIneqList_helper(D,Def):->
	%voeg deze toe aan onze deflist
	if
		0 = Def?remarks<<-size
	then
%		L *= string('%s',Def?name) 
		L *= string('%s',Def?value) 
	else
%		L *= string('%s*',Def?name),
		L *= string('%s*',Def?value),
	DI *= dict_item(Def,L,Def,when(Def == @model?mzpqs,readonly,normaal)),
	D?sketchIneqList->>append(DI).
%%


%%
onSketchIneqSelection(D,
	SketchIneq : sketchInequality*):->
	"Callback when sketchInequality selected" ::
	/*
	Het probleem is altijd dat hier nu al de nieuwe staat geselecteerd terwijl de mapping
	in het dataobject (<-sketchInequality) nog op de oude staat ivm opslaan..
	*/
	%clause 1: op dezelfde regel geklikt

	SketchIneq = D<<-sketchInequality,!.
%
onSketchIneqSelection(D,
	_SketchIneq):->
	%clause 2: we mogen niet door ivm de vorige selectie

	\+ D->>checkSaveSelection,!, %faalt dus terug op oude selectie

	if 
		@nil = D<<-sketchInequality %we waren in new modus
	then
		D?sketchIneqList->>selection(@nil)
	else
		D?sketchIneqList->>selection(D?sketchInequality).
%
onSketchIneqSelection(D, SketchIneq):->
	%clause 3: we mogen door met de nieuwe selectie dus initialiseren we de data
	D->>slot(sketchInequality,SketchIneq),
	D->>fillCurrentSketchIneqData.
%%

%%
addSketchIneq(D):->
	"Add button pressed.." ::

	D->>checkSaveSelection,
	D->>slot(sketchInequality,@nil), %betekent: nieuwe
	D?sketchIneqList->>selection(@nil),
	D->>fillCurrentSketchIneqData,
	D?sketchIneqName->>activate(@on),
	D->>caret(D?sketchIneqName).

%%

%%
copySketchIneq(D):->
	"Copy button pressed.." ::
	%lijkt op addSketchIneq: we maken gewoon een nieuwe met dezelfde values als de geselecteerde
	%maar wijzigen de naam en zet naam/type-kopien van de values (andere id) in de sketchIneqShow
	
	D->>checkSaveSelection,
	CurrentSketchIneq = D<<-sketchInequality,
	CurrentSketchIneq \== @nil,
	D->>fillCurrentSketchIneqData, %vult dus voor de huidige selectie (en zet save uit)
	NewName *= string('%s (copy)',CurrentSketchIneq?name),
	D?sketchIneqName->>selection(NewName),
	D->>slot(sketchInequality,@nil), %maar de bewerkte is een nieuwe
	MyValues = CurrentSketchIneq?values<<-map(@arg1?copyContent),
	D?sketchIneqShow->>setValues(MyValues),
	D?sketchIneqList->>selection(@nil),
	D?removeSketchIneq->>active(@off),
	D?copySketchIneq->>active(@off),
	D?save_member->>active(@on).
%%
	
%%
removeSketchIneq(D):->
	"Remove button pressed.." ::

	%CR checkt de gevolgen wel en waarschuwt eventueel

	if
		not(@nil = D<<-sketchInequality)
	then
		@model->>changeRequest(deleteQuantitySpace,
			@model,
			D,
			D?sketchInequality).
%%

%%
fillCurrentSketchIneqData(D):->
	"Fill the dialog with the saved data for the current sketchIneq" ::
	%clause 1: geen sketchIneq geselecteerd: een nieuwe dus
	
	@nil = D<<-sketchInequality,!,
	%de remove en copy knop gaat dan uit, save mag aan
	D?removeSketchIneq->>active(@off),
	D?copySketchIneq->>active(@off),
	D?save_member->>active(@on),
	default_sketchInequalityName(Name), %zie bovenin deze file
	D?sketchIneqName->>selection(Name),
	D?remarks->>contents(new(string)),
	D?sketchIneqShow->>clear,
	D?sketchIneqShow->>selection(@nil),
	D->>fillCurrentValueData.
%
fillCurrentSketchIneqData(D):->
	%clause 2: wel een sketchIneq geselecteerd
	
	SketchIneq = D<<-sketchInequality,
	%is het de vaste DSketchIneq?
	if
		SketchIneq = @model<<-mzpqs
	then
	(
		D?removeSketchIneq->>active(@off),
		D?save_member->>active(@off)
	)
	else
	(
		D?removeSketchIneq->>active(@on),
		D?save_member->>active(@on)
	),

	D?copySketchIneq->>active(@on),
	SketchIneqShow = D<<-sketchIneqShow,
	SketchIneq = D<<-sketchInequality,
	D?sketchIneqName->>selection(SketchIneq?name),
	D?remarks->>contents(SketchIneq?remarks),
	SketchIneqShow->>preview(SketchIneq),
	SketchIneqShow->>selection(SketchIneqShow?members?head),
	D->>fillCurrentValueData.
%%

%%
fillCurrentValueData(D):->
	"Set information for currently selected value" ::

	%vullen van data en zetten van de controls
	%hangt af van selectie en van of er uberhaupt values zijn..

	SketchIneqShow = D<<-sketchIneqShow,

	if
		0 = SketchIneqShow?members<<-size
	then (
		%geen values: blijkbaar nog geen sketchIneq
		%dus gebruiken we wat knoppen voor toevoegen van punt en interval
		D?addHigh->>setImage(addPoint),
		D?addHigh->>tooltipMsg('Add point value'),
		D?addLow->>setImage(addInterval),
		D?addLow->>tooltipMsg('Add interval value'),
		D?combineIntervals->>active(@off),
		D?removeHigh->>active(@off),
		D?removeLow->>active(@off),
		D?zeroMenu->>active(@off),
		D?zeroMenu->>selected(zero,@off),
		D?valueName->>active(@off),
		D?valueName->>selection('')
		)
	else
		(
		D?addHigh->>setImage(addHigh),
		D?addHigh->>tooltipMsg('Add value high'),
		D?addLow->>setImage(addLow),
		D?addLow->>tooltipMsg('Add value low'),
		D?removeHigh->>active(@on),
		D?removeLow->>active(@on),

		%er moet er 1 geselecteerd zijn
		if
			@nil = SketchIneqShow<<-selection
		then (
			SketchIneqShow->>selection(SketchIneqShow?members?head)
			),

		%zero menu
		if 
			SketchIneqShow->>zeroSelected
		then (
			D?zeroMenu->>active(@on),
			D?zeroMenu->>selected(zero,@on),
			D?valueName->>selection(''),
			D?valueName->>active(@off)
			)
		else (
			D?zeroMenu->>selected(zero,@off),
			D?valueName->>active(@on),
			D?valueName->>selection(SketchIneqShow?value),

			if
				interval = SketchIneqShow<<-selectedType
			then
				D?zeroMenu->>active(@off)
			else
				D?zeroMenu->>active(@on)
			)
		),
		%split interval: mag aan wanneer er een interval is geselecteerd:
		if
			interval = SketchIneqShow<<-selectedType
		then
			D?splitInterval->>active(@on)
		else 
			D?splitInterval->>active(@off),
	
		%combine interval: mag aan wanneer er een punt is geselecteerd en er
		%nog een waarde boven en onder zitten
		if	(
			point = SketchIneqShow<<-selectedType,
			SketchIneqShow<<-nextItem, %extendedListBrowser faalt als het er niet is
			SketchIneqShow<<-prevItem %extendedListBrowser faalt als het er niet is
			)
		then
			D?combineIntervals->>active(@on)
		else
			D?combineIntervals->>active(@off).
%%

%%
onValueSelect(D):->
	"Value selected" ::

	D->>fillCurrentValueData,
	D->>selectValueName.
%%

	
%%
onValueName(D):->
	"Value name changed" ::
	%geven we direct door aan de sketchIneq preview, uitzondering:
	%als de value name nu "zero" is en het is een point dan geven
	%we door dat het een zero is

	D?sketchIneqShow->>changeCurrent(D?valueName?selection).
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
	Dlg->>append(text('You made changes to this sketchInequality. Do you want to save them?')),
	SC *= imgButton(save_changes,
			    ->>(Dlg,return,save), img:=save, tt:='Save changes to model'),
	Dlg->>append(SC),
	CC *= imgButton(cancel_changes,->>(Dlg,return,cancel), img:=undo, tt:='Cancel changes'),
	Dlg->>append(CC),
	EC *= imgButton(edit_changes,->>(Dlg,return,edit), img:=sketch_edit_changes, tt:='Edit changes'),
	Dlg->>append(EC),
	Dlg->>transient_for(D),
	Dlg->>modal(transient),
	Dlg->>kind(toplevel), %wel een titelbalk
	Answer = Dlg<<-confirm_centered(D?frame?area?center),
	Dlg->>destroy,
	if
		Answer = save
	then
		D->>saveSketchIneq
	else 
		Answer = cancel. %falen bij edit
%%

%%
changed(D):->
	"Succeeeds when the sketchIneq is changed by the user" ::

	%dit draaien we even om
	\+ D->>notChanged.
%%

%%
notChanged(D):->
	%slaag als er niets is gewijzigd
	%1: voor een nieuwe definitie, alles moet nog gelijk zijn aan de defaults
	@nil = D<<-sketchInequality,!,
	default_sketchInequalityName(Name), %zie bovenin deze file
	D?sketchIneqName?selection->>equal(Name),
	0 = D?remarks?contents<<-size,
	D?sketchIneqShow?currentValues->>empty.	
%
notChanged(D):->
	%2: de fixed dsketchIneq, die is nooit gewijzigd, ookal heeft de user lopen rommelen
	
	DSketchIneq = D<<-sketchInequality,
	DSketchIneq = @model<<-mzpqs,!.
%
notChanged(D):->
	%3: Een al eerder opgeslagen definitie
	%we beschouwen het als gewijzigd als het sowieso anders, is, geen gedoe met makeGarp op dit nivo

	\+ @nil = D<<-sketchInequality,
	D?sketchIneqName?selection->>equal(D?sketchInequality?name),
	D?remarks?contents->>equal(D?sketchInequality?remarks),

	%de lijst met values moet precies hetzelfde zijn
	%ook op object nivo dus (id + name + type)
	NewValues = D?sketchIneqShow<<-currentValues,
	OldValues = D?sketchInequality<<-values,

	S = NewValues<<-size,
	S = OldValues<<-size,
	OldValues->>for_all_pairs(NewValues, %object_extensions
									->>(@arg1,equal,@arg2)).
							 %equal: zelfde inhoud + zelfde id (maar mag verschillende objecten)
%%

%%
cancel(D):->
	"Cancel button" ::
	%wanneer we in new modus waren (sketchInequality = @nil), dan gaan we daar uit door
	%de onderste quantity te selecteren...

	List = D<<-sketchIneqList,
	if @nil = D<<-sketchInequality 
	then	(
				%als er geen entries in de lijst zijn, blijft het gewoon @nil
				%als er wel entries zijn, dan selecteren we de laatste

				if ( \+ List?members->>empty )
				then	(
							Item = List?members<<-tail,
							D->>slot(sketchInequality,Item?object),
							List->>selection(Item)
						)
			),
	%in alle gevallen opnieuw data inlezen
	D->>fillCurrentSketchIneqData.
%%

%%
save(D):->
	"Save button" :: 

	D->>saveSketchIneq,
	D->>fillCurrentSketchIneqData. %opnieuw inlezen
%%

%%
saveSketchIneq(D):->
	"Save the changes that were made" ::

	D->>notChanged,!. %niets te doen
%
saveSketchIneq(D):->
	%kee, nu moeten we een CR bouwen
	%hangt af of we in nieuw modus zijn of niet

	if 
		@nil = D<<-sketchInequality 
	then 
		@model->>changeRequest(addQuantitySpace,
				@model,
				D,
				D?sketchIneqName?selection,
				D?sketchIneqShow?currentValues,
				D?remarks?contents)
	else
		@model->>changeRequest(changeQuantitySpace,
				D?sketchInequality,
				D,
				D?sketchIneqName?selection,
				D?sketchIneqShow?currentValues,
				D?remarks?contents).
%%	


%%%%%%%wat handige verwijzingen naar members%%%%%%%%%%%%%%
%%
sketchIneqList(D,
			L : list_browser
			):<-
	"Mapping on sketchIneq list member" ::

	L = D<<-member(sketchIneqList).
%%

%%
copySketchIneq(D,
	B : button):<-
	"Mapping on copySketchIneq button member" ::

	B = D<<-member(copySketchIneq).
%%

%%
removeSketchIneq(D,
	B : button):<-
	"Mapping on removeSketchIneq button member" ::

	B = D<<-member(removeSketchIneq).
%%

%%
sketchIneqName(D,
		N : text_item
		):<-
	"Mapping on sketchIneq name item" ::
	N = D<<-member(sketchIneqName).
%%

%%
sketchIneqShow(D,
	Q : sketchIneqPreviewBrowser
	):<-
	"Mapping on sketchIneqshow list member" ::

	Q = D<<-member(sketchIneqShow).
%%

%%
remarks(D,
	R : editor
	):<-
	"Mapping on remarks editor member" ::

	R = D<<-member(remarks).
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
%%		

:-pce_end_class.
