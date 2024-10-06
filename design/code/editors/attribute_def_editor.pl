/*
definitie attributeDefEditor klasse.
Editor voor het bewerken van garpAttributeDef

edit kan aangeroepen met een bestaande attributedef voor het bewerken ervan.
Wanneer edit wordt gebruikt zonder argumenten wordt in new mode geopend.

based on homer code, so most comments in dutch. gp3 code only where mentioned
2000-2005 Jelmer Jellema, Spin in het Web, spininhetweb.nl
*/

default_attributeDefName('New attribute definition').

:-pce_begin_class(attributeDefEditor,
		  assistanceDialog,
		  "Attribute definition editor"
		 ).

variable(attributeDef,garpAttributeDefinition*,get, 
		"the edited attributeDefinition"). %@nil betekent: nieuwe

%%
initialise(D, OpenedBy: [object]):->
	"Initialise the editor" ::
	%gp3 0.1: added openedBy, will be a hyper to the given object, so others can ask voor ?openedBy
	
	%gp3: saved openedBy in a hyper
	D->+initialise('Attribute definitions - Build','Build_AttrDefinitions'), %gp3 0.3.13 added helpid. This one will call displayContent
	default(OpenedBy,@nil,Opener),
	D->>hyper(Opener,openedBy),
	D->>application(@app),
	D->>kind(toplevel),
	D->>icon(@build_icon),

	send(D, init_commands), % JL 

	/* Multiple models */ % JL
	get(@model, getModelNameForEditor, 'Attribute definitions editor - Build', ModelNameForEditor),
	new(Garp3EditorFrame, garp3EditorFrame(ModelNameForEditor)),
	get(@model, '_value', Model),
	send(Garp3EditorFrame, associateModel, Model),
	send(Garp3EditorFrame, append, D).
%%

%%
init_commands(D) :->
    	"Initialise command objects" ::

	send(D, command, 'AttributeDefCopy',  key := '\\ec', keystring := '[Alt + C]'),
	send(D, command, 'AttributeDefCopy',  key := '\\C-c', keystring := '[CTRL + C]'),
	send(D, command, 'AttributeDefPaste', key := '\\ev', keystring := '[Alt + V]'),
	send(D, command, 'AttributeDefPaste', key := '\\C-v', keystring := '[CTRL + V]').
%%

%%
displayContent(D, TopY: int):->

	%gp3 0.3.13 Split off from initialise, called by assistanceDialog->initialise, display all stuff
	%TopY tells us where to start on the Y-axis
		
	%de onderdelen
	%De plaatsing doen we zelf op coordinaten, we gebruiken wel de standaard "gap"
	GapX = D?gap<<-width,
	GapY = D?gap<<-height,
	MaxX *= number(0), %houden we de maximale X coordinaat in bij zodat we rechts kunnen uitlijnen

	
	DefList *= extendedListBrowser(width := 30, height := 10), 
	DefList->>name(defList),
	DefList->>label('Attribute definitions:'),
	DefList->>show_label(@on),
	DefList->>select_message(->>(D,
				       onDefSelection,
						@arg1?object)),

	D->>display(DefList,point(GapX,TopY)), 

	Add *= imgButton(add, tt:= 'Add attribute definition'),
	Copy *= imgButton(copy, tt:='Copy selected attribute definition'),
	Remove *= imgButton(remove, tt:='Delete selected attribute definition'),

	%buttons komen rechts van de lijst, en New helemaal boven, Remove onder
	%dus de lijst moet ook hoog genoeg zijn en bovendien moet New niet bij de bovenkant
	%van de lijst maar bij de bovenkant van het window in de lijst (anders bij label)

	ListBottom *= number(DefList?bottom_side),
	ListBottom->>maximum(DefList?top_side + DefList?image?top_side + %=bovenkant vd lijst
					Add?height + Copy?height + Remove?height +
					2 * GapY), %hieruit volgt de onderkant van de lijst (minimaal ruimte van label + buttons)
	DefList->>bottom_side(ListBottom),

	%Add komt dus niet op de top_side van DefList, maar van het lijstgedeelte
	%DefList?image?top_side is de bovenkant van het lijstgedeelte tov de hele lijst
	D->>display(Add,point(DefList?right_side + GapX,
						DefList?top_side + DefList?image?top_side)),
	MaxX->>maximum(Add?right_side),
	
	%copy komt halverwege
	D->>display(Copy,point(Add?left_side,
				Add?top_side + ((ListBottom - Add?top_side) / 2) -
					(Copy?height / 2))), %uitleg niet nodig, reken maar na
	MaxX->>maximum(Copy?right_side),

	%en Remove komt keurig onder
	D->>display(Remove,point(Add?left_side,
			ListBottom - Remove?height)),
	MaxX->>maximum(Remove?right_side),
	
	%Een lijntje
	Line1 *= line(GapX,ListBottom + GapY,0,ListBottom + GapY),
	Line1->>name(line1),
	%deze lijn wordt later goed naar rechts uitgelijnd
	Line1->>pen(2),
	D->>display(Line1),

	Name *= eventTextItem(name), %kan net zo goed text_item zijn
	Name->>label('Name:'),
	Name->>length(DefList?width),
	D->>display(Name,point(GapX,Line1?bottom_side + GapY)),
	MaxX->>maximum(Name?right_side),

	%lijst met waarden. Item: we kijken alleen naar key (de naam: label wordt gelijk gehouden aan key)
	ValueList *= extendedListBrowser(width := 30, height := 10),
	ValueList->>name(valueList),
	ValueList->>label('Possible values:'),
	ValueList->>show_label(@on),
	ValueList->>select_message(->>(D,
				     onValueSelection)),

	D->>display(ValueList,point(GapX,Name?bottom_side + GapY)),

	ValueAdd *= imgButton(valueAdd, img := add, tt:='Add attribute value'),
	
	D->>display(ValueAdd,point(ValueList?right_side + GapX,
							ValueList?top_side + ValueList?image?top_side)),
	MaxX->>maximum(ValueAdd?right_side),

	ValueUp *= imgButton(valueUp, img:=up, tt:='Move value up'),
	D->>display(ValueUp,point(ValueAdd?left_side,
					ValueAdd?bottom_side + GapY)),

	ValueDown *= imgButton(valueDown, img:=down, tt:='Move value down'),
	D->>display(ValueDown,point(ValueUp?right_side + GapX,
							ValueUp?top_side)),

	MaxX->>maximum(ValueDown?right_side),

	ValueRemove *= imgButton(valueRemove, img:=remove, tt:='Delete selected attribute value'),
	D->>display(ValueRemove,point(ValueAdd?left_side,
								ValueDown?bottom_side + GapY)),

	MaxX->>maximum(ValueRemove?right_side),

	%gp3: moved valueedit to below the value list because of new lay-out
	%ook hier moeten we weer uitkijken dat het wel past
	ValueList->>bottom_side(ValueRemove?bottom_side),
	
	ValueEdit *= eventTextItem(valueEdit),
	ValueEdit->>show_label(@off), 
	ValueEdit->>length(30),
	ValueEdit->>afterKey(->>(D,
							onValueEdit)),

	D->>display(ValueEdit,point(GapX,
					ValueList?bottom_side + GapY)),

	Remarks *= editor(height := 5, width := 40),
	Remarks->>name(remarks),
	Remarks->>label('Remarks:'),
	Remarks->>show_label(@on),
	Remarks->>fill_mode(@on),
	Remarks->>font(Name?font),

	D->>display(Remarks,point(GapX,
							ValueEdit?bottom_side + GapY)),
	MaxX->>maximum(Remarks?right_side),

	%en nu weer een lijntje

	Line2 *= line(GapX,Remarks?bottom_side + GapY,0,Remarks?bottom_side + GapY ),
	Line2->>name(line2),
	Line2->>pen(2), 
	D->>display(Line2), %rechterkant wordt hieronder wel gezet, als MaxX bekend is

	Save *= imgButton(save, tt:='Save changes to model'),

	D->>display(Save,point(GapX,
							Line2?bottom_side + GapY)),

	Cancel *= imgButton(cancel, img:=undo, tt:='Undo changes'),
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

	%Nog meer van het een en ander moet nog mooi naar rechts
	%gp3 0.2: we do a lot more about placement here, because that makes resize easier
	
	
	Add->>set(x:= MaxX - Add?width),
	Copy->>set(x:= MaxX - Copy?width),
	Remove->>set(x:= MaxX - Remove?width),
	DefList->>right_side(Add?left_side - GapX),
	Name->>right_side(MaxX),
	
	ValueDown->>set(x:=MaxX - ValueDown?width),
	ValueUp->>set(x:=ValueDown?left_side - GapX - ValueUp?width),
	ValueAdd->>set(x:=ValueUp?left_side),
	ValueRemove->>set(x:=ValueUp?left_side),
	ValueList->>right_side(ValueUp?left_side - GapX),
	ValueEdit->>right_side(ValueList?right_side),
	
	Line1->>end_x(MaxX),
	Line2->>end_x(MaxX),
	Remarks->>right_side(MaxX),

	
	D->>assign_accelerators, %nodig voor de accels als je niet append gebruikt	

	%we roepen onDestroy aan om te checken of het wel mag
	%dit gaat via done_message vh frame, die roept gewoonlijk frame->>wm_delete aan
	D->>done_message(->>(D,onDestroy)),
	D->>confirm_done(@off), %niet vragen
	
	
	%minimal size:
	D->>minimalSize(size(Close?right_side,Close?bottom_side)). %abs min
%%

%%
onResize(D, Difference: size):->
	%gp3 0.2: 
	
	%vertical resize shared between definitions and values
	
	D?defList_member->>pixelWidth(D?defList_member?pixelWidth + Difference?width),
	D?defList_member->>pixelHeight(D?defList_member?pixelHeight + Difference?height / 2),
	D?add_member->>set(x:= D?add_member?left_side + Difference?width),
	D?copy_member->>set(x:= D?add_member?left_side, y:= D?copy_member?top_side + Difference?height / 4),
	D?remove_member->>set(x:= D?add_member?left_side, y:= D?remove_member?top_side + Difference?height / 2),
	
	D?line1_member->>right_side(D?line1_member?right_side + Difference?width),
	D?line1_member->>set(y:= D?line1_member?top_side + Difference?height / 2),
	
	D?name_member->>right_side(D?name_member?right_side + Difference?width),
	D?name_member->>set(y:= D?name_member?top_side + Difference?height / 2),
	
	D?valueList_member->>pixelWidth(D?valueList_member?pixelWidth + Difference?width),
	D?valueList_member->>pixelHeight(D?valueList_member?pixelHeight + Difference?height / 2),
	D?valueList_member->>set(y:= D?valueList_member?top_side + Difference?height / 2),
	D?valueAdd_member->>set(x:= D?valueAdd_member?left_side + Difference?width, y:= D?valueAdd_member?top_side + Difference?height / 2),
	D?valueUp_member->>set(x:= D?valueAdd_member?left_side, y:= D?valueUp_member?top_side + Difference?height / 2),
	D?valueDown_member->>set(x:= D?valueDown_member?left_side + Difference?width, y:= D?valueUp_member?top_side),	
	D?valueRemove_member->>set(x:= D?valueAdd_member?left_side, y:= D?valueRemove_member?top_side + Difference?height /2),
	D?valueEdit_member->>set(y:= D?valueEdit_member?top_side + Difference?height),
	D?valueEdit_member->>right_side(D?valueList_member?right_side),
	
	D?remarks_member->>right_side(D?name_member?right_side),
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
	A : attributeDef = [garpAttributeDefinition]
	):->
	"Open the editor for editing the given (or the first) attribute definition" ::

	D->>fillDefList,
	
	if
		0 = D?defList_member<<-length
	then 
		D->>slot(attributeDef,@nil)
	else (
		if
			A == @default
		then (
			I = D?defList_member?members<<-head,
			D?defList_member->>selection(I),
			D->>slot(attributeDef,I?object)
			)
		else (
			D?defList_member->>selection(A),
			D->>slot(attributeDef,A)
			)
		),
	D->>fillCurrentDefData,
	D->>open.
%%

%%
fillDefList(D):->
	"Fill the list with attribute definitions" ::

	D?defList_member->>clear,
	@model?sortedAttributeDefinitions->>for_all(->>(D,fillDefList_helper,@arg1)).
%
fillDefList_helper(D,Def):->
	%voeg deze toe aan onze deflist
	if
		0 = Def?remarks<<-size
	then
		L *= string('%s',Def?name)
	else
		L *= string('%s*',Def?name),
	DI *= dict_item(Def,L,Def),
	D?defList_member->>append(DI).
%%

%%
onDefSelection(D,
	A : garpAttributeDefinition*):->
	"Callback when attribute definition selected" ::
	/*
	Het probleem is altijd dat hier nu al de nieuwe staat geselecteerd terwijl de mapping
	in het dataobject (<-quantitySpace) nog op de oude staat ivm opslaan...
	*/
	%clause 1: op dezelfde regel geklikt

	A = D<<-attributeDef,!.
%
onDefSelection(D,
	_A):->
	%clause 2: we mogen niet door ivm de vorige selectie

	\+ D->>checkSaveSelection,!, %faalt dus terug op oude selectie

	if 
		@nil = D<<-attributeDef %we waren in new modus
	then
		D?defList_member->>selection(@nil)
	else
		D?defList_member->>selection(D?attributeDef).
%
onDefSelection(D,A):->
	%clause 3: we mogen door met de nieuwe selectie dus initialiseren we de data
	D->>slot(attributeDef,A),
	D->>fillCurrentDefData.
%%

%%
add(D):->
	"Add button pressed..." ::

	D->>checkSaveSelection,
	D->>slot(attributeDef,@nil), %betekent: nieuwe
	D?defList_member->>selection(@nil),
	D->>fillCurrentDefData,
	D?name_member->>activate(@on),
	D->>caret(D?name_member).
%%

%%
copy(D):->
	"Copy button pressed..." ::
	
	%lijkt op add: we maken gewoon een nieuwe met dezelfde values als de geselecteerde
	D->>checkSaveSelection,
	CurrentDef = D<<-attributeDef,
	CurrentDef \== @nil,
	D->>fillCurrentDefData, %vult dus voor de huidige selectie
	NewName *= string('%s (copy)',CurrentDef?name),
	D?name_member->>selection(NewName),
	D->>slot(attributeDef,@nil), %maar de bewerkte is een nieuwe
	D?defList_member->>selection(@nil),
	D?remove_member->>active(@off),
	D?copy_member->>active(@off), 
	send(D, fillDefList).
%%

%%
remove(D):->
	"Remove button pressed..." ::

	%CR checkt de gevolgen wel en waarschuwt eventueel

	if
		not(@nil = D<<-attributeDef)
	then
		@model->>changeRequest(deleteAttributeDef,
			@model,
			D,
			D?attributeDef).
%%

%%
fillCurrentDefData(D):->
	"fill the editor with saved data for current attribute definition selection" ::
	%clause 1: niets geselecteerd dus in nieuw modus
	
	@nil = D<<-attributeDef,!,
	D?remove_member->>active(@off), %er valt niets te verwijderen
	D?copy_member->>active(@off), %er valt niets te kopieren
	default_attributeDefName(Name), %zie bovenin deze file
	D?name_member->>selection(Name),
	D?remarks_member->>contents(new(string)),
	%en de values enzo zijn dus leeg
	D?valueList_member->>clear,
	D->>fillCurrentValueData.
%
fillCurrentDefData(D):->
	%clause 2: wel een def geselecteerd

	A = D<<-attributeDef,
	D?remove_member->>active(@on),
	D?copy_member->>active(@on),
	D?name_member->>default(A?name),
	D?remarks_member->>contents(A?remarks),
	%en er zijn dus values
	D?valueList_member->>clear,
	CreateItem *= create(dict_item, @arg1?valueName,object := @arg1), %label gelijk aan key, geen object
	A?values->>for_all(->>(D?valueList_member,append,CreateItem)),
	D?valueList_member->>selection(D?valueList_member?members?head),
	D->>fillCurrentValueData.
%%

%%
fillCurrentValueData(D):->
	"Set information for currently selected value" ::

	%vullen van data en zetten van de controls
	%hangt af van selectie en van of er uberhaupt values zijn...

	if
		D?valueList_member?members->>empty
	then (
		%geen values
		D?valueUp_member->>active(@off),
		D?valueDown_member->>active(@off),
		D?valueRemove_member->>active(@off),
		D?valueEdit_member->>active(@off),
		D?valueEdit_member->>selection('')
		)
	else
		(

		%er moet er 1 geselecteerd zijn
		if
			@nil = D?valueList_member<<-selection
		then (
			D?valueList_member->>selection(D?valueList_member?members?head)
			),

		S = D?valueList_member<<-selection,

		if
			S = D?valueList_member?members<<-head
		then
			D?valueUp_member->>active(@off)
		else
			D?valueUp_member->>active(@on),

		if
			S = D?valueList_member?members<<-tail
		then
			D?valueDown_member->>active(@off)
		else
			D?valueDown_member->>active(@on),

		D?valueRemove_member->>active(@on),
		D?valueEdit_member->>active(@on),
		D?valueEdit_member->>selection(D?valueList_member?selection?label)
		).
%%

%%
onValueSelection(D):->
	"Value selected" ::

	D->>fillCurrentValueData,
	D->>selectValueEdit.
%%

%%
onValueEdit(D):->
	"Value name changed" ::

	%er is een selectie, dat is zeker. We veranderen het item
	I = D?valueList_member<<-selection,
	I \== @nil, %dat moet wel
	S = D?valueEdit_member?selection<<-makeGarp,
	I->>key(S),
	I?object->>valueName(S).
%%

%%
valueAdd(D):->
	"value add button clicked" ::
	%value komt onder de huidige selectie of onderaan als er geen selectie is

	VR *= valueReference('New value',state),
	Item *= dict_item(VR?valueName,object := VR),
	S = D?valueList_member<<-selection,
	
	D?valueList_member->>insert_after(Item,S),
	D?valueList_member->>selection(Item),
	D->>fillCurrentValueData,
	D->>selectValueEdit.
%%

%%
valueUp(D):->
	"Value up button clicked" ::
	%Als het goed is is dit dus niet de bovenste

	Item = D?valueList_member<<-selection,
	BeforeItem = D?valueList_member<<-prevItem,
	D?valueList_member->>insert_before(Item,BeforeItem),
	D?valueList_member->>selection(Item),
	D->>fillCurrentValueData.
%%

%%
valueDown(D):->
	"Value down button clicked"::
	%dus dit is niet de onderste

	Item = D?valueList_member<<-selection,
	NextItem = D?valueList_member<<-nextItem,
	D?valueList_member->>insert_after(Item,NextItem),
	D?valueList_member->>selection(Item),
	D->>fillCurrentValueData.
%%

%%
valueRemove(D):->
	"Value remove button clicked" ::
	%er is dus een selectie

	%wat wordt de nieuwe selectie?
	(
		NewSelection = D?valueList_member<<-nextItem
	;	NewSelection = D?valueList_member<<-prevItem
	;	NewSelection = @nil
	),
	D?valueList_member->>delete(D?valueList_member?selection),
	D?valueList_member->>selection(NewSelection),
	D->>fillCurrentValueData.
%%

%%
onAttributeDefCopy(D) :->
    send(@copyBuffer, copyAttributeDef, D?attributeDef).
%%

%%
onAttributeDefPaste(D) :->
    send(@copyBuffer, pasteAttributeDef, D).
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
	Dlg->>append(text('You made changes to this attribute definition. Do you want to save them?')),
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
		D->>saveDef
	else 
		Answer = cancel. %falen bij edit
%%


%%
notChanged(D):->
	%true als er niets gewijzigd is

	%1: Versie voor nieuwe: naam moet standaard zijn, geen remarks en geen values
	@nil = D<<-attributeDef,!,
	default_attributeDefName(DN),
	D?name_member?selection->>equal(DN),
	0 = D?remarks_member?contents<<-size,
	D?valueList_member?members->>empty.
	
notChanged(D):->
	%2: versie voor een bestaand

	\+ @nil = D<<-attributeDef,
	D?name_member?selection->>equal(D?attributeDef?name),
	D?remarks_member?contents->>equal(D?attributeDef?remarks),

	%de lijst met values moet precies hetzelfde zijn
	%ook op object nivo dus (id + name + type)
	NewValues = D?valueList_member?members<<-map(@arg1?object?copy), %de valueReference objecten
	OldValues = D?attributeDef<<-values,
	S = NewValues<<-size,
	S = OldValues<<-size,
	D?attributeDef?values->>for_all_pairs(NewValues,->>(@arg1,equal,@arg2)).
%%

%%
cancel(D):->
	"Cancel button" ::
	%wanneer we in new modus waren (attributeDef = @nil), dan gaan we daar uit door
	%de onderste def te selecteren...

	if 
		@nil = D<<-attributeDef 
	then	(
		%als er geen entries in de lijst zijn, blijft het gewoon @nil
		%als er wel entries zijn, dan selecteren we de laatste
		if 
			( \+ D?defList_member?members->>empty )
		then	(
			Item = D?defList_member?members<<-tail,
			D->>slot(attributeDef,Item?object),
			D?defList_member->>selection(Item)
			)
		),
	%in alle gevallen opnieuw data inlezen
	D->>fillCurrentDefData.
%%

%%
save(D):->
	"Save button" :: 

	D->>saveDef,
	D->>fillCurrentDefData. %opnieuw inlezen
%%

%%
saveDef(D):->
	"Save the changes that were made" ::

	D->>notChanged,!. %niets te doen
%
saveDef(D):->
	%kee, nu moeten we een CR bouwen
	%hangt af of we in nieuw modus zijn of niet

	Values=D?valueList_member?members<<-map(@arg1?object),
	if
		@nil = D<<-attributeDef
	then
		@model->>changeRequest(addAttributeDef,
				@model,
				D,
				D?name_member?selection,
				Values,
				D?remarks_member?contents)
	else
		@model->>changeRequest(changeAttributeDef,
				D?attributeDef,
				D,
				D?name_member?selection,
				Values,
				D?remarks_member?contents).
%%	


%%%%%%%%%%Helpers%%%%%%%%%%%%%%%%
%%
selectValueEdit(D):->
	"Make value edit entry selected" ::

	D?valueEdit_member->>activate(@on),
	D->>caret(D?valueEdit_member),
	D?valueEdit_member->>select_all.
%%

%%%%%%%%%CHANGES%%%%%%%%%%%%%%%%
%%
changeApplied_addAttributeDef(D,
	CR:changeRequestor):->
	%de nieuwe attribdef wordt toegevoegd aan de lijst met defs
	%als dat in deze editor is gebeurd zonder dat we naar een andere zijn gesprongen, dan 
	%selecteren we hem
	/*
	Net als bij changeAttributeDef kan  het opslaan gebeuren doordat de boel in deze editor wordt 
	opgeslagen nadat een andere def is geselecteerd: het gaat bij het herselecteren dan om
	de oude selectie en niet om de opgeslagen D?attributeDef
	*/
	send(CR?garpModel, equal, D?frame?garpModel) ->

	List = D<<-defList_member,
	OldSelection = List<<-selection,
	if
	    OldSelection = @nil
	then
	    OldDef = @nil
	else
	    OldDef = OldSelection<<-object,

	D->>fillDefList,
	if	
	(
	    CR->>checkEditor(D), 
	    OldSelection = @nil %en we zijn nog braaf in new modus
	)
	then
	(
	    List->>selection(CR?result),
	    D->>slot(attributeDef,CR?result), %de nieuwe selectie
	    D->>fillCurrentDefData
	)
	else
	(
	    %we moeten dus de oude selectie herstellen
	    if 
		(OldDef \== @nil)
	    then
		List->>selection(OldDef)
	).
%%

%%
changeApplied_changeAttributeDef(D,
		_CR:changeRequestor):->
	%we weten zeker dat de geselecteerde def nog steeds bestaat, dus daar hoeven we niet op te checken
	%het enige dat we doen is de lijst hervullen en de oude selectie herstellen
	%wanneer de thans geselecteerde def dezelfde is als de interne "huidige" def, en deze is gewijzigd
	%dan lezen we de gegevens opnieuw in.
	%Dit gebeurt dus niet op het nivo van de <-editor van de CR, maar op de test: is degene die is afgebeeld gewijzigd
	%(bij selecteren van een andere def wordt op deze manier hier geen data opnieuw weergegeven, omdat de opgeslagen
	% def wel de huidige <-attributeDef is, maar niet de geselecteerde in de lijst)
	
	%CLAUSE 1: er is geen selectie
	@nil = D?defList_member<<-selection,!,
	D->>fillDefList.
%
changeApplied_changeAttributeDef(D,CR):->
	send(CR?garpModel, equal, D?frame?garpModel) ->

	%CLAUSE 2: er is wel een selectie, dus die is er nog na de change
	Selected= D?defList_member?selection<<-object,
	D->>fillDefList,
	D?defList_member->>selection(Selected), 

	%ok, als Selected gelijk is aan het object van de CR én aan de interne <-attributeDef
	%dan moeten we opnieuw inlezen

	if
	(
	    CR->>checkObject(Selected),
	    Selected = D<<-attributeDef
	)
	then
	    (D->>fillCurrentDefData).
%%

%%
changeApplied_deleteAttributeDef(D,
	CR : changeRequestor
	):->
	/*
	Het kan dus zijn dat de geselecteerde AD weg is. In dat geval kiezen we een andere 
	*/
	List = D<<-defList_member,
	Deleted = CR<<-argument(1),
	if 
	    (Deleted = D<<-attributeDef)
	then (
	    (
		New = List?nextItem<<-object
	    ;	New = List?prevItem<<-object
	    ;	New = @nil
	    ),

	    List->>selection(New),
	    D->>slot(attributeDef,New),
	    D->>fillCurrentDefData
	    ),
	List->>delete(Deleted).
%%

		
:-pce_end_class.
