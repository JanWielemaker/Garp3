/*
definitie configurationDefEditor klasse.
Editor voor het bewerken van configurationDefinition

edit kan aangeroepen met een bestaande definition voor het bewerken ervan.
Wanneer edit wordt gebruikt zonder argumenten wordt in new mode geopend.

based on homer code, so most comments in dutch. gp3 code only where mentioned
2000-2005 Jelmer Jellema, Spin in het Web, spininhetweb.nl
*/

default_defName('New configuration definition').

:-pce_begin_class(configurationDefEditor,
		  assistanceDialog,
		  "Configuration definition editor"
		 ).

variable(def,configurationDefinition*,get, 
		"the edited configurationDefinition"). %@nil betekent: nieuwe
%%
initialise(D, OpenedBy: [object]):->
	"Initialise the editor" ::
	%gekopieerd van attributeDefEditor
	%gp3 0.1: added openedBy, will be a hyper to the given object, so others can ask voor ?openedBy

	D->+initialise('Configuration definitions - Build','Build_ConfigDefinitions'),
	D->>application(@app),
	D->>kind(toplevel),
	D->>icon(@build_icon),

	%gp3: saved openedBy in a hyper
	default(OpenedBy,@nil,Opener),
	D->>hyper(Opener,openedBy),
	
	%de onderdelen
	%De plaatsing doen we zelf op coordinaten, we gebruiken wel de standaard "gap"
	GapX = D?gap<<-width,
	GapY = D?gap<<-height,
	MaxX *= number(0), %houden we de maximale X coordinaat in bij zodat we rechts kunnen uitlijnen

	DefList *= extendedListBrowser(width := 40, height := 10), 
	DefList->>name(defList),
	DefList->>label('Configuration definitions:'),
	DefList->>show_label(@on),
	DefList->>select_message(->>(D,
				       onDefSelection,
						@arg1?object)),

	D->>display(DefList,point(GapX,D?topY)), %topY because of help button

	Add *= imgButton(add, tt:='Add configuration definition'),

	Remove *= imgButton(remove, tt:='Delete selected configuration definition'),

	%buttons komen rechts van de lijst, en New helemaal boven, Remove onder
	%dus de lijst moet ook hoog genoeg zijn en bovendien moet New niet bij de bovenkant
	%van de lijst maar bij de bovenkant van het window in de lijst (anders bij label)

	ListBottom *= number(DefList?bottom_side),
	ListBottom->>maximum(DefList?top_side + DefList?image?top_side + %=bovenkant vd lijst
					Add?height + Remove?height +
					GapY), %hieruit volgt de onderkant van de lijst (minimaal ruimte van label + buttons)
	DefList->>bottom_side(ListBottom),

	%Add komt dus niet op de top_side van DefList, maar van het lijstgedeelte
	%DefList?image?top_side is de bovenkant van het lijstgedeelte tov de hele lijst
	D->>display(Add,point(DefList?right_side + GapX,
						DefList?top_side + DefList?image?top_side)),
	MaxX->>maximum(Add?right_side),
	
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


	Remarks *= editor(height := 4, width := 60),
	Remarks->>name(remarks),
	Remarks->>label('Remarks:'),
	Remarks->>show_label(@on),
	Remarks->>fill_mode(@on),
	Remarks->>font(Name?font),

	D->>display(Remarks,point(GapX,
							Name?bottom_side + GapY)),
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
	Close *= imgButton(close,->>(D,onDestroy),tt:='Close this editor'), 
	
	MaxX->>maximum(Cancel?right_side + GapX + Close?width),

	%oftewel: eventueel wordt de dlg nog breder om deze button te laten passen
	%hoe dan ook helemaal rechts

	D->>display(Close,point(
		MaxX - Close?width,
		Save?top_side)),

	%Nog meer van het een en ander moet nog mooi naar rechts
	Add->>set(x:= MaxX - Add?width),
	Remove->>set(x:= MaxX - Remove?width),
	DefList->>right_side(Add?left_side - GapX),
	Line1->>end_x(MaxX),
	Name->>right_side(MaxX),
	Remarks->>right_side(MaxX),
	Line2->>end_x(MaxX),
	D->>assign_accelerators, %nodig voor de accels als je niet append gebruikt	

	%we roepen onDestroy aan om te checken of het wel mag
	%dit gaat via done_message vh frame, die roept gewoonlijk frame->>wm_delete aan
	D->>done_message(->>(D,onDestroy)),
	D->>confirm_done(@off), %niet vragen
	D->>updateSpacers, %gp3 0.3.13 needed when assistanceDialog is used, but displayContent is not used to fill the dialog
		%minimal size:
	D->>minimalSize(size(MaxX,Close?bottom_side)), %abs min

	/* Multiple models */
	get(@model, getModelNameForEditor, 'Configuration definitions editor - Build', ModelNameForEditor),
	new(Garp3EditorFrame, garp3EditorFrame(ModelNameForEditor)),
	get(@model, '_value', Model),
	send(Garp3EditorFrame, associateModel, Model),
	send(Garp3EditorFrame, append, D),

	send(D, init_commands).
%%

%%
init_commands(D) :->
	"Initialise command objects" ::

	send(D, command, 'ConfigurationCopy',  key := '\\ec', keystring := '[Alt + C]'),
	send(D, command, 'ConfigurationCopy',  key := '\\C-c', keystring := '[CTRL + C]'),
	send(D, command, 'ConfigurationPaste', key := '\\ev', keystring := '[Alt + V]'),
	send(D, command, 'ConfigurationPaste', key := '\\C-v', keystring := '[CTRL + V]').
%%	

%%
onResize(D, Difference: size):->
	%gp3 0.2: 
	
	D?defList_member->>pixelWidth(D?defList_member?pixelWidth + Difference?width),
	D?defList_member->>pixelHeight(D?defList_member?pixelHeight + Difference?height),
	D?add_member->>set(x:= D?add_member?left_side + Difference?width),
	D?remove_member->>set(x:= D?add_member?left_side, y:= D?remove_member?top_side + Difference?height),
	
	D?line1_member->>right_side(D?line1_member?right_side + Difference?width),
	D?line1_member->>set(y:= D?line1_member?top_side + Difference?height),
	
	D?name_member->>right_side(D?name_member?right_side + Difference?width),
	D?name_member->>set(y:= D?name_member?top_side + Difference?height),
	
	D?remarks_member->>right_side(D?name_member?right_side),
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
	Def :  [configurationDefinition]
	):->
	"Open the editor for editing the given (or the first) config definition" ::

	D->>fillDefList,

	if
		0 = D?defList_member<<-length
	then 
		D->>slot(def,@nil)
	else (
		if
			Def == @default
		then (
			I = D?defList_member?members<<-head,
			D?defList_member->>selection(I),
			D->>slot(def,I?object)
			)
		else (
			D?defList_member->>selection(Def),
			D->>slot(def,Def)
			)
		),
	D->>fillCurrentDefData,
	D->>open.
%%

%%
fillDefList(D):->
	"Fill the list with configuration definitions" ::

	D?defList_member->>clear,
	@model?sortedConfigurationDefinitions->>for_all(->>(D,fillDefList_helper,@arg1)).
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
	Def : configurationDefinition*):->
	"Callback when definition selected" ::
	/*
	Het probleem is altijd dat hier nu al de nieuwe staat geselecteerd terwijl de mapping
	in het dataobject nog op de oude staat ivm opslaan...
	*/
	%clause 1: op dezelfde regel geklikt

	Def = D<<-def,!.
%
onDefSelection(D,
	_Def):->
	%clause 2: we mogen niet door ivm de vorige selectie

	\+ D->>checkSaveSelection,!, %faalt dus terug op oude selectie

	if 
		@nil = D<<-def %we waren in new modus
	then
		D?defList_member->>selection(@nil)
	else
		D?defList_member->>selection(D?def).
%
onDefSelection(D,Def):->
	%clause 3: we mogen door met de nieuwe selectie dus initialiseren we de data
	D->>slot(def,Def),
	D->>fillCurrentDefData.
%%

%%
add(D):->
	"Add button pressed..." ::

	D->>checkSaveSelection,
	D->>slot(def,@nil), %betekent: nieuwe
	D?defList_member->>selection(@nil),
	D->>fillCurrentDefData,
	D?name_member->>activate(@on),
	D->>caret(D?name_member).
%%

%%
remove(D):->
	"Remove button pressed..." ::

	%CR checkt de gevolgen wel en waarschuwt eventueel

	if
		not(@nil = D<<-def)
	then
		@model->>changeRequest(deleteConfigurationDef,
			@model,
			D,
			D?def).
%%

%%
fillCurrentDefData(D):->
	"fill the editor with saved data for current configuration definition selection" ::
	%clause 1: niets geselecteerd dus in nieuw modus
	
	@nil = D<<-def,!,
	D?remove_member->>active(@off), %er valt niets te verwijderen
	default_defName(Name), %zie bovenin deze file
	D?name_member->>selection(Name),
	D?remarks_member->>contents(new(string)).
%
fillCurrentDefData(D):->
	%clause 2: wel een def geselecteerd

	Def = D<<-def,
	D?remove_member->>active(@on),
	D?name_member->>default(Def?name),
	D?remarks_member->>contents(Def?remarks).
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
	Dlg->>append(text('You made changes to this configuration definition. Do you want to save them?')),
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
	%slaag als de definitie gewijzigd is
	%1: versie voor een nieuwe definitie: alles moet er nog precies
	%zo bijstaan als ingevuld door fillCurrentDefData
	
	@nil = D<<-def,!,
	default_defName(Name), %zie bovenin deze file
	D?name_member?selection->>equal(Name),
	0 = D?remarks_member?contents<<-size.
	
notChanged(D):->
	%2: Versie voor een bestaande definitie
	%we beschouwen het als gewijzigd als het sowieso anders, is, geen gedoe met makeGarp op dit nivo

	\+ @nil = D<<-def,
	D?name_member?selection->>equal(D?def?name),
	D?remarks_member?contents->>equal(D?def?remarks).
%%

%%
cancel(D):->
	"Cancel button" ::
	%wanneer we in new modus waren (def = @nil), dan gaan we daar uit door
	%de onderste def te selecteren...

	if 
		@nil = D<<-def 
	then	(
		%als er geen entries in de lijst zijn, blijft het gewoon @nil
		%als er wel entries zijn, dan selecteren we de laatste
		if 
			( \+ D?defList_member?members->>empty )
		then	(
			Item = D?defList_member?members<<-tail,
			D->>slot(def,Item?object),
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

	if
		@nil = D<<-def
	then
		@model->>changeRequest(addConfigurationDef,
				@model,
				D,
				D?name_member?selection,
				D?remarks_member?contents)
	else
		@model->>changeRequest(changeConfigurationDef,
				D?def,
				D,
				D?name_member?selection,
				D?remarks_member?contents).
%%

%%%%%%%% Copy Paste%%%%%%%%%%%%%%%
onConfigurationCopy(D) :->
    send(@copyBuffer, copyConfigurationDef, D?def).
onConfigurationPaste(D) :->
    send(@copyBuffer, pasteConfigurationDef, D).

%%


%%%%%%%%%%Helpers%%%%%%%%%%%%%%%%
%%%%%%%%%CHANGES%%%%%%%%%%%%%%%%
%%
changeApplied_addConfigurationDef(D,
	CR:changeRequestor):->
	%de nieuwe configuratiedef wordt toegevoegd aan de lijst met defs
	%als dat in deze editor is gebeurd zonder dat we naar een andere zijn gesprongen, dan 
	%selecteren we hem
	/*
	Net als bij changeConfigurationDef kan  het opslaan gebeuren doordat de boel 
	in deze editor wordt 
	opgeslagen nadat een andere def is geselecteerd: het gaat bij het herselecteren dan om
	de oude selectie en niet om de opgeslagen D?def
	*/
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
	    D->>slot(def,CR?result), %de nieuwe selectie
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
changeApplied_changeConfigurationDef(D,
		_CR:changeRequestor):->
	%we weten zeker dat de geselecteerde def nog steeds bestaat, dus daar hoeven we niet op te checken
	%het enige dat we doen is de lijst hervullen en de oude selectie herstellen
	%wanneer de thans geselecteerde def dezelfde is als de interne "huidige" def, en deze is gewijzigd
	%dan lezen we de gegevens opnieuw in.
	%Dit gebeurt dus niet op het nivo van de <-editor van de CR, maar op de test: is degene die is afgebeeld gewijzigd
	%(bij selecteren van een andere def wordt op deze manier hier geen data opnieuw weergegeven, omdat de opgeslagen
	% def wel de huidige <-def is, maar niet de geselecteerde in de lijst)
	
	%CLAUSE 1: er is geen selectie
	@nil = D?defList_member<<-selection,!,
	D->>fillDefList.
%
changeApplied_changeConfigurationDef(D,CR):->
	%CLAUSE 2: er is wel een selectie, dus die is er nog na de change
	
	Selected= D?defList_member?selection<<-object,
	D->>fillDefList,
	D?defList_member->>selection(Selected), 

	%ok, als Selected gelijk is aan het object van de CR én aan de interne <-def
	%dan moeten we opnieuw inlezen

	if
	    (	CR->>checkObject(Selected),
		Selected = D<<-def
	    )
	then
	    (D->>fillCurrentDefData).
%%

%%
changeApplied_deleteConfigurationDef(D,
	CR : changeRequestor
	):->
	/*
	Het kan dus zijn dat de geselecteerde def weg is. In dat geval kiezen we een andere 
	*/
	    
	List = D<<-defList_member,
	Deleted = CR<<-argument(1),
	if 
	    (Deleted = D<<-def)
	then (
	    (
		New = List?nextItem<<-object
	    ;	New = List?prevItem<<-object
	    ;	New = @nil
	    ),

	    List->>selection(New),
	    D->>slot(def,New),
	    D->>fillCurrentDefData
	),
	List->>delete(Deleted).
%%

:-pce_end_class.
