/*
definitie quantityDefEditor klasse.
Simpele editor voor het bewerken van quantity definities

edit kan aangeroepen met een bestaande quantitydef voor het bewerken ervan.
Wanneer edit wordt gebruikt zonder argumenten wordt in new mode geopend.

based on homer code, so most comments in dutch. gp3 code only where mentioned
2000-2005 Jelmer Jellema, Spin in het Web, spininhetweb.nl
*/

default_quantityDefName('New quantity definition').

:-pce_begin_class(quantityDefEditor,
		  assistanceDialog,
		  "quantity definition editor"
		 ).

variable(quantityDef,garpQuantityDefinition*,get, 
		"the edited quantityDefinition"). %@nil betekent: nieuwe
%%
initialise(D, OpenedBy: [object]):->
	"Initialise the editor" ::
	%gp3 0.1: added openedBy, will be a hyper to the given object, so others can ask voor ?openedBy

	D->+initialise('Quantity definitions - Build','Build_QuantityDefinitions'),
	D->>icon(@build_icon),
	D->>application(@app),
	D->>kind(toplevel),

	%gp3: saved openedBy in a hyper
	default(OpenedBy,@nil,Opener),
	D->>hyper(Opener,openedBy),
	
	%de onderdelen
	%De plaatsing doen we zelf op coordinaten, we gebruiken wel de standaard "gap"
	GapX = D?gap<<-width,
	GapY = D?gap<<-height,
	MaxX *= number(0), %houden we de maximale X coordinaat in bij zodat we rechts kunnen uitlijnen

	QList *= extendedListBrowser(width := 20, height := 8), 
	QList->>name(quantities),
	QList->>label('Quantity definitions:'),
	QList->>show_label(@on),
	QList->>select_message(->>(D,
				       onQuantitySelection,
						@arg1?object)),

	D->>display(QList,point(GapX,D?topY)),

	Add *= imgButton(addQuantity, img:= add, tt:='Add quantity definition'),
	Copy *= imgButton(copyQuantity, img:=copy, tt:= 'Copy selected quantity definition'),
	Remove *= imgButton(removeQuantity, img:=remove, tt:='Delete selected quantity definition'),

	%buttons komen rechts van de lijst, en New helemaal boven, Remove onder
	%dus de lijst moet ook hoog genoeg zijn en bovendien moet New niet bij de bovenkant
	%van de lijst maar bij de bovenkant van het window in de lijst (anders bij label)

	ListBottom *= number(QList?bottom_side),
	ListBottom->>maximum(QList?top_side + QList?image?top_side + %=bovenkant vd lijst
					Add?height + Copy?height + Remove?height +
					2 * GapY), %hieruit volgt de onderkant van de lijst (minimaal ruimte van label + buttons)
	QList->>bottom_side(ListBottom),

	%Add komt dus niet op de top_side van QList, maar van het lijstgedeelte
	%QList?image?top_side is de bovenkant van het lijstgedeelte tov de hele lijst
	D->>display(Add,point(QList?right_side + GapX,
						QList?top_side + QList?image?top_side)),
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

	
	Name *= eventTextItem(quantityName), %kan net zo goed text_item zijn
	Name->>label('Name:'),
	Name->>length(QList?width),
	D->>display(Name,point(GapX,Line1?bottom_side + GapY)),
	
	MaxX->>maximum(Name?right_side),

	QSList *= extendedListBrowser(width := 30, height:=4),
	QSList->>name(allowedQSList),
	QSList->>label('Allowed quantity spaces:'),
	QSList->>show_label(@on),
	QSList->>select_message(->>(D,
				     onAllowedQSSelection,@arg1?object)),

	D->>display(QSList,point(GapX,Name?bottom_side + GapY)),

	QSAddRemove *= imgButton(qsAddRemove,
						->>(D,onQSAddRemove), img:= remove, tt:= 'Delete selected quantity space'), %gp3: this will be changed when needed

	D->>display(QSAddRemove,point(GapX, %real placement below
		QSList?bottom_side + GapY)),
		
	QSEdit *= imgButton(qsEdit, img:=qs_edit,tt:='Open quantity space definitions editor'),

	D->>display(QSEdit,point(GapX,QSList?bottom_side + GapY)), %real placement below

	AllQSList *= extendedListBrowser(width := 30,height:=5),
	AllQSList->>name(allQSList),
	AllQSList->>label('All quantity spaces:'),
	AllQSList->>show_label(@on),
	AllQSList->>select_message(->>(D,
					onOtherQSSelection,@arg1?object)),

	D->>display(AllQSList,point(GapX,QSAddRemove?bottom_side + GapY)),

	QSShow *= qsPreviewBrowser(width := 18), %controls
	QSShow->>name(qsshow),
	QSShow->>changeable(@off), %gp3 0.1 no selections
	
	D->>display(QSShow,point(QSList?right_side + GapX,
							QSList?top_side)),
	QSShow->>bottom_side(AllQSList?bottom_side),
	MaxX->>maximum(QSShow?right_side),


	Remarks *= editor(height := 4, width := 30),
	Remarks->>name(remarks),
	Remarks->>label('Remarks:'),
	Remarks->>show_label(@on),
	Remarks->>fill_mode(@on),
	Remarks->>font(Name?font),

	D->>display(Remarks,point(GapX,
							AllQSList?bottom_side + GapY)),
	MaxX->>maximum(Remarks?right_side),
	%en nu weer een lijntje

	Line2 *= line(GapX,Remarks?bottom_side + GapY,0,Remarks?bottom_side + GapY ),
	Line2->>name(line2),
	Line2->>pen(2), 
	D->>display(Line2), %rechterkant wordt hieronder wel gezet, als MaxX bekend is

	Save *= imgButton(save, tt:='Save changes'),

	D->>display(Save,point(GapX,
							Line2?bottom_side + GapY)),

	Cancel *= imgButton(cancel, img:=undo, tt:='Undo changes'),
	%deze komt rechts van save
	D->>display(Cancel,point(Save?right_side + GapX,
				Save?top_side)),

	%sluiten moet rechts komen, dus dat moet ook nog uitgezocht
	Close *= imgButton(close,->>(D,onDestroy),tt:='Close this editor'),
	
	MaxX->>maximum(Cancel?right_side + GapX + Close?width),
	%oftewel: eventueel wordt de dlg nog breder om deze button te laten passen
	%hoe dan ook helemaal rechts

	D->>display(Close,point(
		MaxX - Close?width,
		Save?top_side)),

	%Het een en ander moet nog mooi naar rechts
	%gp3 0.2: we do a lot more about placement here, because that makes resize easier
	Add->>set(x:= MaxX - Add?width),
	Copy->>set(x:= MaxX - Copy?width),
	Remove->>set(x:= MaxX - Remove?width),
	QList->>right_side(Add?left_side - GapX),
	
	Line1->>end_x(MaxX),
	Name->>right_side(MaxX),
	QSShow->>set(x:= MaxX - QSShow?pixelWidth),
	QSList->>right_side(QSShow?left_side - GapX),
	QSAddRemove->>set(x:= GapX + ((QSList?right_side - QSList?left_side) / 2) - 
								(QSAddRemove?width / 2)),
	QSEdit->>set(x:= QSList?right_side - QSEdit?width),
	AllQSList->>right_side(QSList?right_side),
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

	send(D, init_commands), % JL

	/* Multiple models */
	get(@model, getModelNameForEditor, 'Quantity definitions editor - Build', ModelNameForEditor),
	new(Garp3EditorFrame, garp3EditorFrame(ModelNameForEditor)),
	get(@model, '_value', Model),
	send(Garp3EditorFrame, associateModel, Model),
	send(Garp3EditorFrame, append, D).
%%

%%
init_commands(D) :->
	"Initialise command objects" ::

	send(D, command, 'QuantityDefCopy',  key := '\\ec', keystring := '[Alt + C]'),
	send(D, command, 'QuantityDefCopy',  key := '\\C-c', keystring := '[CTRL + C]'),
	send(D, command, 'QuantityDefPaste', key := '\\ev', keystring := '[Alt + V]'),
	send(D, command, 'QuantityDefPaste', key := '\\C-v', keystring := '[CTRL + V]').
%%	

%%
onResize(D, Difference: size):->
	%gp3 0.2: 
	
	D?quantities_member->>pixelWidth(D?quantities_member?pixelWidth + Difference?width),
	D?quantities_member->>pixelHeight(D?quantities_member?pixelHeight + Difference?height / 2),
	D?addQuantity_member->>set(x:= D?addQuantity_member?left_side + Difference?width),
	D?copyQuantity_member->>set(x:= D?addQuantity_member?left_side, y:= D?copyQuantity_member?top_side + Difference?height / 4),
	D?removeQuantity_member->>set(x:= D?addQuantity_member?left_side, y:= D?removeQuantity_member?top_side + Difference?height / 2),
	
	D?line1_member->>right_side(D?line1_member?right_side + Difference?width),
	D?line1_member->>set(y:= D?line1_member?top_side + Difference?height / 2),
	
	D?quantityName_member->>right_side(D?quantityName_member?right_side + Difference?width),
	D?quantityName_member->>set(y:= D?quantityName_member?top_side + Difference?height / 2),
		
	D?allowedQSList_member->>right_side(D?allowedQSList_member?right_side + Difference?width),
	D?allowedQSList_member->>set(y:= D?allowedQSList_member?top_side + Difference?height / 2),

	D?qsAddRemove_member->>set(x:= D?qsAddRemove_member?left_side + Difference?width / 2,
							y:= D?qsAddRemove_member?top_side + Difference?height / 2),

	
	D?qsEdit_member->>set(x:= D?qsEdit_member?left_side + Difference?width, y:= D?qsAddRemove_member?top_side),
	
	D?allQSList_member->>pixelHeight(D?allQSList_member?pixelHeight + Difference?height / 2),
	D?allQSList_member->>right_side(D?allQSList_member?right_side + Difference?width),
	D?allQSList_member->>set(y:= D?allQSList_member?top_side + Difference?height / 2),

	D?qsshow_member->>set(x:= D?qsshow_member?left_side + Difference?width,
					y:= D?qsshow_member?top_side + Difference?height / 2),
	D?qsshow_member->>bottom_side(D?allQSList_member?bottom_side),

	
	D?remarks_member->>right_side(D?quantityName_member?right_side),
	D?remarks_member->>set(y:=D?remarks_member?top_side + Difference?height),

	D?line2_member->>right_side(D?line1_member?right_side),
	D?line2_member->>set(y:= D?line2_member?top_side + Difference?height),
	
	D?save_member->>set(y:=D?save_member?top_side + Difference?height),
	D?cancel_member->>set(y:=D?save_member?top_side),
	D?close_member->>set(x:= D?close_member?left_side + Difference?width, y:=D?save_member?top_side).
%%
%%%
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
	Q : quantityDef = [garpQuantityDefinition]
	):->
	"Open the editor for editing the given (or the first) quantity definition" ::

	D->>fillQuantityList,
	pl_setQuantitySelection(Q,D,D?quantityList),
	D->>open.
%
pl_setQuantitySelection(@default,D,List):-
	%alleen bij het openen: zet de zaken even goed.
	0 =List<<-length,!,
	List->>selection(@nil), %toevoegen
	D->>slot(quantityDef,@nil),
	D->>fillCurrentQuantityData.
%
pl_setQuantitySelection(@default,D, List):-!,
	Q = List?members<<-head,
	List->>selection(Q),
	D->>slot(quantityDef,Q?object),
	D->>fillCurrentQuantityData.
%
pl_setQuantitySelection(Quantity,D,List):-!,
	Item = List?members<<-find( @arg1?object == Quantity ),
	List->>selection(Item),
	D->>slot(quantityDef,Quantity),
	D->>fillCurrentQuantityData,
	List->>normalise(Item). %gp3 0.1 added to make sure def is visible
%%

%%
fillQuantityList(D):->
	"Fill the list with quantity definitions" ::

	D?quantityList->>clear,
	@model?sortedQuantityDefinitions->>for_all(->>(D,fillQuantityList_helper,@arg1)).
%
fillQuantityList_helper(D,Def):->
	%voeg deze toe aan onze deflist
	if
		0 = Def?remarks<<-size
	then
		L *= string('%s',Def?name)
	else
		L *= string('%s*',Def?name),
	DI *= dict_item(Def,L,Def),
	D?quantityList->>append(DI).
%%
	
%%
cancel(D):->
	"Cancel button" ::
	%wanneer we in new modus waren (quantityDef = @nil), dan gaan we daar uit door
	%de onderste quantity te selecteren...

	List = D<<-quantityList,
	if @nil = D<<-quantityDef 
	then	(
				%als er geen entries in de lijst zijn, blijft het gewoon @nil
				%als er wel entries zijn, dan selecteren we de laatste

				if ( \+ List?members->>empty )
				then	(
							Item = List?members<<-tail,
							D->>slot(quantityDef,Item?object),
							List->>selection(Item)
						)
			),
	%in alle gevallen opnieuw data inlezen
	D->>fillCurrentQuantityData.
%%

%%
save(D):->
	"Save button" :: 

	D->>saveDefinition,
	D->>fillCurrentQuantityData. %opnieuw inlezen
%%

%%
onQuantitySelection(D,
	Quantity : garpQuantityDefinition*):->
	"Callback when quantity definition selected" ::
	/*
	Het probleem is altijd dat hier nu al de nieuwe staat geselecteerd terwijl de mapping
	in het dataobject (<-quantityDef) nog op de oude staat ivm opslaan...
	*/
	%clause 1: op dezelfde regel geklikt

	Quantity = D<<-quantityDef,!.
%
onQuantitySelection(D,
	_Quantity):->
	%clause 2: we mogen niet door ivm de vorige selectie

	\+ D->>checkSaveSelection,!, %faalt dus terug op oude selectie

	if 
		@nil = D<<-quantityDef %we waren in new modus
	then
		Item = @nil
	else
		Item = D?quantityList?members<<-find( @arg1?object == D?quantityDef ),

	D?quantityList->>selection(Item).
%
onQuantitySelection(D, Quantity):->
	%clause 3: we mogen door met de nieuwe selectie dus initialiseren we de data
	D->>slot(quantityDef,Quantity),
	D->>fillCurrentQuantityData.
%%

%%
fillCurrentQuantityData(D):->
	"Fill the dialog with the saved data for the current definition" ::
	%clause 1: geen quantitydef geselecteerd: een nieuwe dus
	
	@nil = D<<-quantityDef,!,
	D?removeQuantity->>active(@off), %er valt niets te verwijderen
	D?copyQuantity->>active(@off), %er valt niets te kopieren
	default_quantityDefName(Name), %zie bovenin deze file
	D?quantityName->>selection(Name),
	D?remarks->>contents(new(string)),
	D->>fillQSLists.
%
fillCurrentQuantityData(D):->
	%clause 2: wel een quantitydef geselecteerd

	Q = D<<-quantityDef,
	D?removeQuantity->>active(@on),
	D?copyQuantity->>active(@on),
	D?quantityName->>default(Q?name),
	D?remarks->>contents(Q?remarks),
	D->>fillQSLists.
%%

%%
fillQSLists(D):->
	"Fill the dialog with the saved quantity space data" ::
	%we lopen alle qs-en af en zetten ze in de lijst met alle qs-en, en evt in de allowed lijst
	%Als er een quantitydef is geselecteerd worden de daarbij toegestane QS-en
	%afgebeeld.
	%Wanneer er geen qd wordt bewerkt( add-mode), worden alle qs-en in de allQSList afgebeeld
	Q = D<<-quantityDef,
	Allowed = D<<-allowedQSList,
	All = D<<-allQSList,

	Allowed->>clear,
	All->>clear,
	D?qsShow->>clear,

	CreateItem *= create(dict_item,
							@arg1,
							@arg1?name,
							@arg1),
	%het hangt er vanaf of we een quantity definition bewerken of een nieuwe maken:
	if (Q == @nil)
	then
		%nieuwe
		Msg *= ->>(All,append,CreateItem)
	else
		Msg *= 	and(
					if(
						->>(Q,quantitySpaceAllowed,@arg1),
						->>(Allowed, append, CreateItem)
					),
					->>(All,append,CreateItem)
		  		),
	
	%en alle qs-en aflopen
	@model?sortedQuantitySpaces->>for_all(Msg),
	Msg->>done,
	CreateItem->>done,
	D->>checkQsAddRemove.
%%

%%
onAllowedQSSelection(D,
	QS: quantitySpace
	):->
	"Callback when an allowed qs is selected" ::

	D?allQSList->>selection(@nil),
	%gp3: change the image etc for qsAddRemove
	D?qsAddRemove->>setImage(remove),
	D?qsAddRemove->>tooltip('Delete selected quantity space'),
	D?qsShow->>preview(QS),
	D->>checkQsAddRemove.
%%

%%
onOtherQSSelection(D,
	QS: quantitySpace
	):->
	"Callback when an other qs is selected" ::

	D?allowedQSList->>selection(@nil),
	%gp3: change the image etc for qsAddRemove
	D?qsAddRemove->>setImage(up),
	D?qsAddRemove->>tooltip('Add selected quantity space'),
	D?qsShow->>preview(QS),
	D->>checkQsAddRemove.
%%

%%
checkQsAddRemove(D):->
	%kijk of de button aan mag
	if
	(
		\+ @nil = D?allowedQSList<<-selection
	)
	then
		D?qsAddRemove->>active(@on)
	else
	(
		if
		(
			S = D?allQSList<<-selection,
			S \== @nil,
			\+ D?allowedQSList?members->>find(@arg1?key == S?key)
		)
		then
			D?qsAddRemove->>active(@on)
		else
			D?qsAddRemove->>active(@off)
	).
%%
	
%%
onQSAddRemove(D):->
	"Callback when clicking on the Add/Remove QS button" ::

	%1 vd twee lijsten heeft een selectie, of geen van beiden...
	%clause 1: een qs weg
	I = D?allowedQSList<<-selection, %extendedListBrowser slaagt met @nil als geen selectie
	I \== @nil, !, %faalt als geen selectie
	AllSelection = D?allQSList?members<<-find(@arg1?key == I?key),
	D?allowedQSList->>delete(I),
	D?allQSList->>selection(AllSelection),
	D->>onOtherQSSelection(AllSelection?object).
%
onQSAddRemove(D):->
	%clause 2: een qs erbij

	I = D?allQSList<<-selection,
	I \== @nil, !, %faalt als geen selectie
	D?allowedQSList->>append(dict_item(I?key,I?label,I?object)),
	D?allowedQSList->>sort(@on,@on),
	NewSelection = D?allowedQSList?members<<-find(@arg1?key == I?key),
	D?allowedQSList->>selection(NewSelection),
	D->>onAllowedQSSelection(NewSelection?object),
	D?allQSList->>selection(@nil).
%
onQSAddRemove(D):->
	%clause 3: geen qs geselecteerd

	D->>msgBox('Please select a quantity space to add or remove.',alarm). %gp3 0.3: no label needed anymore
%%
 
%%
changed(D):->
	"Succeeeds when the definition is changed by the user" ::

	%dit draaien we even om
	\+ D->>notChanged.
%%

%%
notChanged(D):->
	%slaag als er niets is gewijzigd
	%1: voor een nieuwe definitie, alles moet nog gelijk zijn aan de defaults
	@nil = D<<-quantityDef,!,
	default_quantityDefName(Name), %zie bovenin deze file
	D?quantityName?selection->>equal(Name),
	0 = D?remarks?contents<<-size,
	D?currentAllowedQS->>empty.
%
notChanged(D):->
	%2: Een al eerder opgeslagen definitie
	%we beschouwen het als gewijzigd als het sowieso anders, is, geen gedoe met makeGarp op dit nivo

	\+ @nil = D<<-quantityDef,
	D?quantityName?selection->>equal(D?quantityDef?name),
	D?remarks?contents->>equal(D?quantityDef?remarks),
	%dezelfde qs-en in de lijst met allowedQs..
	
	CurrentAllowedQS = D<<-currentAllowedQS,
	CurrentAllowedQS->>for_all(->>(D?quantityDef,
								   quantitySpaceAllowed,
								   @arg1)),
	D?quantityDef?allowedQuantitySpaces->>for_all(->>(CurrentAllowedQS,
													  member,@arg1)).
%%

%%
checkSaveSelection(D):->
	"Check changed state of current selection and ask for saving, fail if change canceled" ::
	D->>notChanged,!. %niets aan de hand
%
checkSaveSelection(D):->
	%ok, er is iets gewijzigd, dus moet er misschien opgeslagen worden

	Dlg *= dialog('Confirm changes'),
	Dlg->>application(@app),
	Dlg->>append(text('You made changes to the definition of this quantity. Do you want to save them?')),
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
	pl_processSaveAnswer(D,Answer).
%
pl_processSaveAnswer(D,save):-
	%user wil opslaan: slaag als dat lukt
	D->>saveDefinition.
%
pl_processSaveAnswer(_D,cancel).
	%user wil doorgaan zonder wijzigingen op te slaan: gewoon slagen dus
	%in het geval van edit (user wil niet doorgaan) gewoon falen
%%
	
%%
saveDefinition(D):->
	"Save the changes that were made" ::

	D->>notChanged,!. %niets te doen
%
saveDefinition(D):->
	%kee, nu moeten we een CR bouwen
	%hangt af of we in nieuw modus zijn of niet

	if 
		@nil = D<<-quantityDef 
	then 
		@model->>changeRequest(addQuantityDef,
				@model,
				D,
				D?quantityName?selection,
				D?currentAllowedQS,
				D?remarks?contents)
	else
		@model->>changeRequest(changeQuantityDef,
				D?quantityDef,
				D,
				D?quantityName?selection,
				D?currentAllowedQS,
				D?remarks?contents).
%%	
	
%%
removeQuantity(D):->
	"Remove button pressed..." ::
	%dus een change requestor erachter aan (geen are u sure->hoeft niet aangezien het niet mag als er quantities zijn?)
	
	if 
		not(@nil = D<<-quantityDef)
	then
		@model->>changeRequest(deleteQuantityDef,
			@model,
			D,
			D?quantityDef).
%%

%%
addQuantity(D):->
	"Add button pressed..." ::

	D->>checkSaveSelection,
	D->>slot(quantityDef,@nil), %betekent: nieuwe
	D?quantityList->>selection(@nil),
	D->>fillCurrentQuantityData,
	D?quantityName->>activate(@on),
	D->>caret(D?quantityName).

%%

%%
copyQuantity(D):->
	"Copy button pressed..." ::
	%lijkt op addQuantity: we maken gewoon een nieuwe met dezelfde gegevens als de geselecteerde
	D->>checkSaveSelection,
	CurrentQ = D<<-quantityDef,
	CurrentQ \== @nil,
	D->>fillCurrentQuantityData, %vult dus voor de huidige selectie
	NewName *= string('%s (copy)',CurrentQ?name),
	D?quantityName->>selection(NewName),
	D->>slot(quantityDef,@nil), %maar de bewerkte is een nieuwe
	D?quantityList->>selection(@nil),
	D?removeQuantity->>active(@off),
	D?copyQuantity->>active(@off).
%%

%%
qsEdit(D):->
	"Edit QS button pressed..." ::

	%welke QS?
	Item = D?allowedQSList<<-selection,
	if
		Item = @nil
	then
		RItem = D?allQSList<<-selection
	else
		RItem = Item,

    	get(@model, all_hypers, HyperChain),
	chain_list(HyperChain, HyperList),
	(
	    member(Hyper, HyperList),
	    get(Hyper, to, QuantitySpaceDefEditor),
	    get(QuantitySpaceDefEditor, class_name, 'quantitySpaceEditor') ->
	    send(QuantitySpaceDefEditor, expose)
	;
	    new(QuantitySpaceDefEditor, quantitySpaceEditor),
	    new(_PartOfHyper, partof_hyper(@model, QuantitySpaceDefEditor)),

	    (
		RItem = @nil,
		send(QuantitySpaceDefEditor, edit)
	    ;
		send(QuantitySpaceDefEditor, edit, RItem?object)
	    )
	).
%%

%%
onQuantityDefCopy(D) :->
    send(@copyBuffer, copyQuantityDef, D?quantityDef).

onQuantityDefPaste(D) :->
    send(@copyBuffer, pasteQuantityDef, D).
%%


%%%%%%%wat handige verwijzingen naar members%%%%%%%%%%%%%%
%%
quantityList(D,
			L : list_browser
			):<-
	"Mapping on quantity list member" ::

	L = D<<-member(quantities).
%%

%%
quantityName(D,
		N : text_item
		):<-
	"Mapping on quantitydef name item" ::
	N = D<<-member(quantityName).
%%

%%
copyQuantity(D,
	B : button
	):<-
	"Mapping on copy button" ::
	B = D<<-member(copyQuantity).
%%

%%
removeQuantity(D,
	B : button
	):<-
	"Mapping on remove button" ::
	B = D<<-member(removeQuantity).
%%

%%
qsShow(D,
	Q : qsPreviewBrowser
	):<-
	"Mapping on qsshow list member" ::

	Q = D<<-member(qsshow).
%%

%%
remarks(D,
	R : editor
	):<-
	"Mapping on remarks editor member" ::

	R = D<<-member(remarks).
%%

%%
allowedQSList(D,
	L : list_browser
	):<-
	"Mapping on allowed qs list member" ::

	L = D<<-member(allowedQSList).
%%

%%
allQSList(D,
	L : list_browser
	):<-
	"Mapping on all qs list member" ::

	L = D<<-member(allQSList).
%%

%%
qsAddRemove(D,
	QAR: button
	):<-
	"Mapping on add/remove qs button member" ::

	QAR = D<<-member(qsAddRemove).
%%

%%
currentAllowedQS(D,
	C : chain
	):<-
	"Return the currently selected allowed quantity spaces" :: 

	C *= chain,
	D?allowedQSList?members->>for_all(->>(C,append,@arg1?object)).
%%

%%%%%%%%%%Change requestors%%%%%%%%%%%%%%%
%%
changeApplied_changeQuantityDef(D,
		_CR:changeRequestor):->
	%we weten zeker dat de geselecteerde QD nog steeds bestaat, dus daar hoeven we niet op te checken
	%het enige dat we doen is de lijst hervullen en de oude selectie herstellen
	%wanneer de thans geselecteerde QD dezelfde is als de interne "huidige" QD, en deze is gewijzigd
	%dan lezen we de gegevens opnieuw in.
	%Dit gebeurt dus niet op het nivo van de <-editor van de CR, maar op de test: is degene die is afgebeeld gewijzigd
	%(bij selecteren van een andere QD wordt op deze manier hier geen data opnieuw weergegeven, omdat de opgeslagen
	% QD wel de huidige <-quantityDef is, maar niet de geselecteerde in de lijst)

	%CLAUSE 1: er is geen selectie
	List = D<<-quantityList,
	@nil = List<<-selection,!,
	D->>fillQuantityList.
%
changeApplied_changeQuantityDef(D,CR):->
	%CLAUSE 2: er is wel een selectie, dus die is er nog na de change
	List = D<<-quantityList,
	SelectedQD = List?selection<<-object,
	D->>fillQuantityList,
	NewSelection = List?members<<-find(@arg1?object == SelectedQD),
	List->>selection(NewSelection),
	%ok, als SelectedQD gelijk is aan het object van de CR én aan de interne <-quantityDef
	%dan moeten we opnieuw inlezen

	(	
	    CR->>checkObject(SelectedQD),
	    SelectedQD = D<<-quantityDef ->
	    (D->>fillCurrentQuantityData)
	;
	    true
	).
%%

%%
changeApplied_deleteQuantityDef(D,
	CR : changeRequestor
	):->
	/*
	Het kan dus zijn dat de geselecteerde QD weg is. In dat geval kiezen we een andere 
	*/
	List = D<<-quantityList,
	DeletedQD = CR<<-argument(1),
	(
	    (DeletedQD = D<<-quantityDef) ->
	    (
		( 
		    NewQD = List?nextItem<<-object ;	
		    NewQD = List?prevItem<<-object ;
		    NewQD = @nil
		),
		List->>selection(NewQD),
		D->>slot(quantityDef,NewQD),
		D->>fillCurrentQuantityData
	    )
	;
	    true
	),
	List->>delete(DeletedQD).
%%			
	
%%
changeApplied_addQuantityDef(D,
	CR:changeRequestor):->
	/*Er is een quantitydef bijgekomen. Dit kan best in deze editor zijn gebeurd. In dat geval
		moet de nieuwe quantitydef worden geselecteerd (tenzij we opslaan doordat er ineens een
		andere is geselecteerd). Wanneer het in een andere is gebeurd dan
		hoeven we alleen de lijst bij te werken.
	Net als bij changeQuantityDef kan  het opslaan gebeuren doordat de boel in deze editor wordt 
	opgeslagen nadat een andere quantitydef is geselecteerd: het gaat bij het herselecteren dan om
	de oude selectie en niet om de opgeslagen D?quantityDef 
	*/
	List = D<<-quantityList,
	OldSelection = List<<-selection,
	(
	    OldSelection = @nil ->
	    OldQD = @nil
	;
	    OldQD = OldSelection<<-object
	),

	D->>fillQuantityList,
			
	(
	    CR->>checkEditor(D), 
	    OldSelection = @nil -> %en we zijn nog braaf in new modus
	    Item = List?members<<-find(@arg1?object == CR?result),
	    List->>selection(Item),
	    D->>slot(quantityDef,Item?object), %de nieuwe selectie
	    D->>fillCurrentQuantityData
	;
	    %we moeten dus de oude selectie herstellen
	    (
		OldSelection = @nil ->
		Item = @nil
	    ;
		Item = List?members<<-find(@arg1?object == OldQD)
	    ),
	    List->>selection(Item)
	).
%%

%%
changeApplied_addQuantitySpace(D, % JL Prevent crash because Editor == @nil
	CR:changeRequestor):->
	/*een quantity space is nieuw, die gaan we dus toevoegen aan de lijst met alle QS-en*/
	CurSel = D?allQSList<<-selection,
	D?allQSList->>append(dict_item(CR?result,CR?result?name,CR?result)),
	D?allQSList->>sort(@on,@on),

	%gp3: if we opened the editor, we want to select the new qs
	(
	    D = CR?editor<<-openedBy ->
	    D?allQSList->>selection(CR?result),
	    D->>onOtherQSSelection(CR?result)
	;
	    D?allQSList->>selection(CurSel)
	).
%%

%%
changeApplied_changeQuantitySpace(D,
	CR : changeRequestor):->
	/*Een quantity space is gewijzigd. Dit betekent dat de lijsten met QS-en moeten worden
		geupdate, en dat eventueel de preview moet gewijzigd worden*/

	%eerst maar eens de huidige selectie opslaan
	CurAllowedSel = D?allowedQSList<<-selection,

	(
	    CurAllowedSel = @nil ->
	    CurSel = D?allQSList<<-selection,
	    CurSelList = D<<-allQSList
	;
	    CurSel = CurAllowedSel,
	    CurSelList = D<<-allowedQSList
	),

	%het item bijwerken in de lijsten
	(
	    AllowedItem =  D?allowedQSList<<-member(CR?object) ->
	    AllowedItem->>label(CR?object?name)
	;
	    true
	),
		
	AllItem = D?allQSList<<-member(CR?object),
	AllItem->>label(CR?object?name),
	
	%als geselecteerd ook de preview bijwerken
	(
	    CurSel \== @nil ->
	    D?qsShow->>preview(CurSel?object)
	;
	    true
	),
	%sowieso even overnieuw

	%en de lijsten opnieuw sorteren en het geselecteerde item weer selecteren
	D?allowedQSList->>sort(@on,@on),
	D?allQSList->>sort(@on,@on),
	CurSelList->>selection(CurSel).
%%

%%
changeApplied_deleteQuantitySpace(D,
	CR : changeRequestor):->

	%verwijder de quantity space uit de lijsten. Als de quantity space is geselecteerd
	%dan is dat nu niet meer het geval...
	ignore(D?allowedQSList->>delete(CR?arg1)),
	D?allQSList->>delete(CR?arg1),
	
	%selectie checken
	(
	    \+ @nil = D?allowedQSList<<-selection ->
	    D?qsShow->>preview(D?allowedQSList?selection?object)
	;
	    (
	        \+ @nil = D?allQSList<<-selection ->
	        D?qsShow->>preview(D?allQSList?selection?object)
	    ;
	        D?qsShow->>clear
	    )
	),
	D->>checkQsAddRemove.
%%

:-pce_end_class.
