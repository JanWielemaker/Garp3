/*
definitie importedFragmentPropsDlg klasse.
De standaard eigenschappen dialoog van applies_to modelfragmenten.
*/

:-pce_begin_class(importedFragmentPropsDlg,
		  propertyDialog,
		  "Standard conditional model fragment properties dialog"
		 ).

variable(readOnly,bool,both,"Saved read-only state").
%%
initialise(D, F: frame):->
	"Initialise the properties dialog" ::
	%gp3 changed buttons to imgButtons
	
	D->+initialise('Conditional fragment properties - Build',F, later),

	D->>readOnly(@off), %standaard
	
	%De plaatsing doen we zelf op coordinaten, we gebruiken wel de standaard "gap"
	GapX = D?gap<<-width,
	GapY = D?gap<<-height,

	DefList *= extendedListBrowser(height:=15, width := 60), 
	DefList->>name(mfList),
	DefList->>label('Model fragments'),
	DefList->>show_label(@on),
	DefList->>select_message(->>(D,
				       onMFSelection)),

	D->>display(DefList,point(GapX,D?topY)),

	%rechts onder komt een edit knop
	Edit *= imgButton(edit, img:=edit_modelfragment, tt:='Open model fragment editor'),
	D->>display(Edit,point(DefList?right_side + GapX,
						DefList?bottom_side - Edit?height)),

	Remarks *= editor(height := 5, width := 60),
	Remarks->>name(remarks),
	Remarks->>label('Remarks:'),
	Remarks->>show_label(@on),
	Remarks->>fill_mode(@on),
	Remarks->>font(DefList?font),
	D->>display(Remarks,point(GapX,DefList?bottom_side + GapY)), 
	
	Ok *= imgButton(ok, img:=save, tt:='Apply changes'),
	D->>display(Ok,point(GapX,Remarks?bottom_side + GapY)),
	Ok->>default_button(@on),
	
	Cancel *= imgButton(cancel, img:=undo, tt:= 'Cancel changes'),
	D->>display(Cancel,point(Edit?right_side - Cancel?width, Ok?top_side)),
	D->>updateSpacers, %gp3 0.3.13
	D->>assign_accelerators,
		%minimal size:
	D->>minimalSize(size(Cancel?right_side,Cancel?bottom_side)), %abs min

	% Multiple model support
	get(@model, getModelNameForEditor, 'Conditional fragment properties - Build', ModelNameForEditor),
	new(Garp3EditorFrame, garp3EditorFrame(ModelNameForEditor)),
	get(@model, '_value', Model),
	send(Garp3EditorFrame, associateModel, Model),
	send(Garp3EditorFrame, append, D),
	send(Garp3EditorFrame, transient_for, F).

%%

%%
onResize(D, Difference: size):->
	%gp3 0.2: 
	
	DefList = D<<-mfList_member,
	DefList->>pixelWidth(DefList?pixelWidth + Difference?width),
	DefList->>pixelHeight(DefList?pixelHeight + Difference?height),
	%so the rest moves
	D?edit_member->>set(x:= D?edit_member?left_side + Difference?width,
							y:= D?edit_member?top_side + Difference?height),

	D?remarks_member->>right_side(DefList?right_side),
	D?remarks_member->>set(y:=D?remarks_member?top_side + Difference?height),
	D?ok_member->>set(y:=D?ok_member?top_side + Difference?height),
	D?cancel_member->>move(point(DefList?right_side - D?cancel_member?width, D?ok_member?top_side)).
%%

%%
newObject(D):->
	"Open the dialog for a new conditional fragment" ::

	get(@model, getModelNameForEditor, 'Add a new conditional model fragment - Build', ModelNameForEditor),
	D->>label(ModelNameForEditor),
	D->>helpId(string('Build_%s_AddNewConditionalMF',D?containerType)), %gp3 0.3.13
	D->>fillMFList,
	D->>onMFSelection,
	D->>openDialog.
%%

%%
editObject(D,
	IF: importedFragment,
    ReadOnly : bool	
	):->
	"Open dialog for existing imported fragment" ::

	%bij een bestaand fragment mag je het MF niet wijzigen
	%dat gaan we dus aanpassen
	
	D->>element(IF),
	get(@model, getModelNameForEditor, string('Edit conditional modelfragment%s - Build',when(ReadOnly == @on,' [Read Only]',''))?value, ModelNameForEditor),
	D->>label(ModelNameForEditor),	
	D->>helpId(string('Build_%s_EditConditionalMF',D?containerType)), %gp3 0.3.13
	D->>fillMFList,
	D?mfList_member->>selection(IF?referencedFragment),
	D?mfList_member->>changeable(@off), %niet meer wijzigen (ook niet bij niet read only)
	D?remarks_member->>contents(IF?remarks),
	D->>readOnly(ReadOnly),
	D->>onMFSelection,
	D->>openDialog.
%%

%%
fillMFList(D):->
	"Vul de lijst met modelfragmenten" ::

	List = D<<-member(mfList),
	if
		(\+ @nil = List<<-selection)
	then
		OudeSelectie = List?selection<<-object
	else
		OudeSelectie = @nil,
		
	List->>clear,

	%ok, de basis mf's worden eerst genoemd en hun kinderen komen daar onder
	chain(
		@model?topStaticFragment,
		@model?topProcessFragment,
		@model?topAgentFragment)->>for_all(->>(D,
			fillMFListRecursive,
			List,
			@arg1,
			0)),

	%selectie herstellen
	if
		Item = List?members<<-find( @arg1?object == OudeSelectie)
	then
		List->>selection(Item),
		
	List->>done.
%
fillMFListRecursive(D,
	List: list_browser,
	MF: modelFragment,
	Tab: int):->
	%recursief hulpje

	Name *= string,
	Name->>insert_character('-',0,Tab),
	Name->>append(MF?name),
	List->>append(dict_item(MF,Name,MF)),

	NewTab is Tab + 1,
	Children = MF<<-children,
	Children->>sort(?(@arg1?name,compare,@arg2?name)),
	
	Children->>for_all(->>(D,fillMFListRecursive,List,@arg1,NewTab)),
	Children->>done,
	Name->>done.
%%

%%
onMFSelection(D):->

	%we moeten even kijken wat zoal geactiveerd mag zijn
	%ok mag niet geactiveerd worden wanneer het een kind van het huidige fragment
	%betreft, of het geselecteerde fragment het huidige fragment zelf
	%ook weer inmporteert (wordt ook nog afgevangen door de CR)
	%wij vangen hier alleen de children af, da's alles
	%de CR doet al het overige werk (+ dit opnieuw)
	
	%is er iets geselecteerd

	CurrentFragment = D<<-modelFragment,
	MF = D<<-selected,	%kan @nil zijn
	
	if
	(
			MF == @nil
		;
			CurrentFragment->>checkAllDown(@arg1 == MF, @on, @on)
				%deze ook, alleen onze children (recursief)
		;
			MF?parents->>empty	%topfragment mag ook niet
	)
	then
		D?ok_member->>active(@off)
	else
		D?ok_member->>active(D?readOnly?negate),

	%en edit mag alleen als het geen topfragment is
	%en natuurlijk niet ons fragment
	if
	(
		MF \== @nil,
		MF \== CurrentFragment
	)
	then
		D?edit_member->>active(when(->>(MF?parents,empty),@off,@on))
	else
		D?edit_member->>active(@off).
%%

%%
edit(D):->
	%we willen het huidig geselecteerde MF editen
	
	MF = D<<-selected,
	MF \== @nil,
	\+ MF = D<<-modelFragment,
	\+ MF?parents->>empty, %geen topnode

	send(@app, openViewEditor, MF). 
%%

%%
saveNewElement(D):->
	%nieuwe maken
	@model->>changeRequest(newConditionalFragment,
		D?modelFragment,
		D?editor,
		D?selected,
		D?remarks_member?contents).
%%

%%
saveChangedElement(D):->
	%bestaande wijzigen, nooit de MF, alleen het commentaar
	@model->>changeRequest(changeConditionalFragment,
		D?modelFragment,
		D?editor,
		D?element,
		D?remarks_member?contents).
%%

%%
notChanged(D):->
	%definitieobject hetzelfde?
	CurrentMF = D<<-selected,
	CurrentMF = D?element<<-referencedFragment,

	%remarks hetzelfde?
	D?remarks_member?contents->>equal(D?element?remarks).
%%

%%DATA MAPPINGS%%
%%
selected(D,
	MF: modelFragment):<-
	%geef @nil als niets geselecteerd


	DI = D?mfList_member<<-selection,
	if
		DI == @nil
	then
		MF = @nil
	else
		MF = DI<<-object.
%%
%%%%%%%%%change requestors%%%%%%%%%%%%
%%om te zorgen dat er niet iets wordt geselecteerd dat er niet meer is
%%etc...

changeApplied_newMF(D,
	_CR: changeRequestor):->

	%alleen de lijst met alle fragmenten kan wijzigen
	D->>fillMFList.
%%

changeApplied_changeMF(D,
	_CR: changeRequestor):->
	
	%we sluiten
	D->>cancel.

%%

changeApplied_deleteMF(D,
	_CR: changeRequestor):->
	
	D->>cancel. %sluiten
%%

changeApplied_changeConditionalFragment(D,
	CR: changeRequestor):->

		\+ CR->>checkEditor(D?editor), %niet als deze editor (anders dubbel sluiten)
		CR->>checkArgument(1,D?element),
		D->>return.
%%

%%
changeApplied_deleteConditionalFragment(D,
	CR: changeRequestor):->
	
		CR->>checkArgument(1,D?element),
		D->>return.
%%

:-pce_end_class.
