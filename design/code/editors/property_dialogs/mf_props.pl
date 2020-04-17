/*
definitie mfPropsDlg klasse.
De standaard eigenschappen dialoog van Model Fragments.
Aangeroepen vanuit structureeditor en mf-editor (voor het bewerkte of een geimporteerd fragment)
*/

:-pce_begin_class(mfPropsDlg,
		  propertyDialog,
		  "Standard model fragments properties dialog"
		 ).

%%
initialise(D, F: frame):->
	"Initialise the properties dialog" ::
	%gp3 0.2 changed buttons to imgButtons
	
	D->+initialise('Object properties - Build',F,later), 

	GapX = D?gap<<-width,
	GapY = D?gap<<-height,

	%de onderdelen
	FragmentList *= extendedListBrowser(width := 60,  
				  height := 8),
	FragmentList->>name(fragments),
	FragmentList->>label('All Model Fragments:'),
	FragmentList->>show_label(@on),
	FragmentList->>select_message(->>(D,
				     onSelectFragment)),
 	FragmentList->>style(onbruikbaar,style(colour:='#757575')), %niet te kiezen grijs
	FragmentList->>style(ok,style(colour:='#cc3366')), %ok mf, paarsig	
	D->>display(FragmentList,point(GapX,D?topY)),

	ParentAddRemove *= imgButton(parentAddRemove, img:= down, tt:= 'Add the selected model fragment to the list of parents'),

	D->>display(ParentAddRemove,point(GapX + ((FragmentList?right_side - FragmentList?left_side) / 2) - 
		(ParentAddRemove?width / 2),
		FragmentList?bottom_side + GapY)),
	
	ParentList *= extendedListBrowser(width := 60,
					height := 6),
	ParentList->>name(parents),
	ParentList->>label('Parents:'),
	ParentList->>show_label(@on),
	ParentList->>select_message(->>(D,
				     onSelectParent)),
	D->>display(ParentList, point(GapX,
		ParentAddRemove?bottom_side + GapY)),
		
	Name *= eventTextItem(name), 
	Name->>label('Name:'),
	D->>display(Name,point(GapX,ParentList?bottom_side + GapY)),
	Name->>right_side(ParentList?right_side),
	Name->>afterKey(->>(D,checkOk)), %anders gaat ie weer aan als er een naam staat (pce)
	Name->>keyboard_focus(@on),
	
	Active *= menu(activeMenu,toggle),
	Active->>show_label(@off),
	Active->>layout(horizontal),
	Active->>append(active),

	D->>display(Active,point(GapX,Name?bottom_side + GapY)),
	
	Remarks *= editor(width := 60, height := 5),
	Remarks->>name(remarks),
	Remarks->>label('Remarks:'),
	Remarks->>show_label(@on),
	Remarks->>fill_mode(@on),
	Remarks->>font(Name?font),
	D->>display(Remarks,point(GapX,Active?bottom_side + GapY)),
	
	StructureMessage *= label(structureMessage, ''),
	D->>display(StructureMessage,point(GapX,Remarks?bottom_side + GapY)),
	
	Ok *= imgButton(ok, img:=save, tt:='Apply changes'),
	D->>display(Ok,point(GapX,StructureMessage?bottom_side + GapY)),
	Ok->>default_button(@on),
	%Ok->>keyboard_focus(@on),

	Cancel *= imgButton(cancel, img:=undo, tt:= 'Cancel changes'),
	D->>display(Cancel,point(Remarks?right_side - Cancel?width,
		Ok?top_side)),
	D->>updateSpacers, %gp3 0.3.13
	D->>assign_accelerators,
		%minimal size:
	D->>minimalSize(size(Cancel?right_side,Cancel?bottom_side)), %abs min

	% Multiple model support
	get(@model, getModelNameForEditor, 'Object properties - Build', ModelNameForEditor),
	new(Garp3EditorFrame, garp3EditorFrame(ModelNameForEditor)),
	get(@model, '_value', Model),
	send(Garp3EditorFrame, associateModel, Model),
	send(Garp3EditorFrame, append, D),
	send(Garp3EditorFrame, transient_for, F).

%%

%%
onResize(D, Difference: size):->
	%gp3 0.2: 
	
	FragmentList = D<<-fragments_member,
	FragmentList->>pixelWidth(FragmentList?pixelWidth + Difference?width),
	FragmentList->>pixelHeight(FragmentList?pixelHeight + Difference?height),
	
	D?parentAddRemove_member->>set(x:= D?parentAddRemove_member?left_side + Difference?width / 2,
									y:= D?parentAddRemove_member?top_side + Difference?height),
	
	D?parents_member->>pixelWidth(D?parents_member?pixelWidth + Difference?width),
	D?parents_member->>set(y:= D?parents_member?top_side + Difference?height),
	
	D?name_member->>right_side(FragmentList?right_side),
	D?name_member->>set(y:= D?name_member?top_side + Difference?height),
	
	D?activeMenu_member->>set(y:= D?activeMenu_member?top_side + Difference?height),
	
	D?remarks_member->>right_side(FragmentList?right_side),
	D?remarks_member->>set(y:=D?remarks_member?top_side + Difference?height),
	D?structureMessage_member->>set(y:=D?structureMessage_member?top_side + Difference?height),
	D?ok_member->>set(y:=D?ok_member?top_side + Difference?height),
	D?cancel_member->>move(point(FragmentList?right_side - D?cancel_member?width, D?ok_member?top_side)).
%%

%%
newObject(D,
	Parent: modelFragment %de geselecteerde parent
	 ):->
	"Open the dialog for a new modelFragment" ::

	D->>element(@nil), %betekent nieuw
	get(@model, getModelNameForEditor, 'Add a new Model Fragment - Build', ModelNameForEditor),
	D->>label(ModelNameForEditor),
	D->>helpId('Build_AddNewModelFragment'), %gp3 0.3.13
	D->>addParent(Parent), %refillt ook de lijsten
	D?name_member->>selection('New model fragment'),
	D?activeMenu_member->>selected(active,@on),
	
	D->>checkOk, %zou goed moeten zijn
	D->>openDialog.
%%

%%
editObject(D,
	Object: modelFragment %het bijbehorende object	
	 ):->
	"Open dialog for existing modelFragment" ::

	D->>element(Object),
	get(@model, getModelNameForEditor, 'Model Fragment Properties - Build', ModelNameForEditor),
	D->>label(ModelNameForEditor),
	D->>helpId('Build_ModelFragmentProperties'), %gp3 0.3.13
	D->>fillParentList,	%moet altijd voor fillFragmentList, vanwege uitsluitingen
	D->>fillFragmentList,
	%data er in zetten
	D?name_member->>selection(Object?name),
	D?activeMenu_member->>selected(active,Object?active),
	D?remarks_member->>contents(Object?remarks),
	D->>checkOk, %zou goed moeten zijn
	D->>checkStructureMessage, %incomplete?
	D->>openDialog.
%%

%%
fillFragmentList(D):->
	List = D<<-member(fragments),
	List->>clear,

	%ok, de basis mf's worden eerst genoemd en hun kinderen komen daar onder
	TopStatic = @model<<-hypered(topStaticFragment),
	TopProcess = @model<<-hypered(topProcessFragment),
	TopAgentF = @model<<-hypered(topAgentFragment),

	chain(TopStatic,TopProcess,TopAgentF)->>for_all(->>(D,
											fillMFListRecursive,
											List,
											@arg1,
											0)),
	D->>checkParentSelection. %bijwerken van knoppen
%
fillMFListRecursive(D,
	List: list_browser,
	MF: modelFragment,
	Tab: int):->
	%recursief hulpje

	Name *= string,
	Name->>insert_character(' ',0,Tab),
	PS = MF?parents<<-size,
	if
		PS > 1
	then
		Name->>append('+')
	else
		Name->>append(' '),
	Name->>append(MF?name),
	
	DI *= dict_item(new(secondaryID),Name,MF), %unieke key ivm Multiple Inheritance
	%de MI is beperkt, zie datamodel
	%hier checken we dat met een aparte call
	if
		fail %(\+ ok = @model<<-norm_FragmentInheritance(MF,D?currentParents,D?element))
	then
		DI->>style(onbruikbaar)
	else
		DI->>style(ok),
		
	List->>append(DI),

	NewTab is Tab + 1,
	Children = MF<<-children,
	Children->>sort(?(@arg1?name,compare,@arg2?name)),
	Children->>for_all(->>(D,fillMFListRecursive,List,@arg1,NewTab)),
	Children->>done,
	Name->>done.
%%


%%
fillParentList(D):->
	%vul de parentlist
	%dit is dus nu alleen nog de lijst met parents volgens de selectie
	"(re)fill the parentlist" ::

	List = D<<-parents_member,
	List->>clear,
	E = D<<-element,
	if
		E \== @nil
	then
	(
		Parents = E<<-parents,
		Parents->>sort(?(@arg1?name,compare,@arg2?name)),
		Parents->>for_all(
			->>(List,append,create(dict_item,@arg1,@arg1?name,@arg1))),
		Parents->>done
	),
	%en voortaan moet de lijst het zelf maar bijhouden
	List->>sort_by(@default),
	D->>checkParentSelection.
%%	

%%
checkStructureMessage(D):->
	%De structuremessage wordt gezet: is het mf compleet
	%(wordt niet bijgewerkt bij wijzigingen, dit is tenslotte een dialoog)
	
	if
	(
		\+ @nil = D<<-element,
		D?element->>isIncomplete
	)
	then
		D?structureMessage_member->>selection('Warning: This modelfragment has an incomplete structure')
	else
		D?structureMessage_member->>selection('').
%%
	
%%
onSelectFragment(D):->
	%de bovenste lijst geselecteerd
	%deselecteer de onderste dus
	
	D?parents_member->>selection(@nil),
	D->>checkParentSelection.
%%

%%
onSelectParent(D):->
	%de onderste lijst geselecteerd
	%deselecteer de bovenste dus
	
	D?fragments_member->>selection(@nil),
	D->>checkParentSelection.
%%

%%
parentAddRemove(D):->
	%button ingedrukt, op op een lijst gedubbelklikt, wordt het add of remove?
	P = D<<-selectedFragment, %add
	D->>addParent(P).
%
parentAddRemove(D):->
	P = D<<-selectedParent, %remove
	%pak even de volgende als dat kan, anders de vorige
	(
		D?parents_member->>selection(D?parents_member?nextItem)
	;
		ignore(D?parents_member->>selection(D?parents_member?prevItem))
	),
	D->>removeParent(P).
%%

%%
addParent(D, Parent: modelFragment):->
	%voeg deze parent toe aan de lijst met parents
	
	D?parents_member->>insert(new(dict_item(Parent,Parent?name,Parent))),
	D->>fillFragmentList,
	D->>checkOk.
%%

%%
removeParent(D, Parent: modelFragment):->
	%verwijder deze uit de lijst met parents

	D?parents_member->>delete(Parent), %in parentlist is de key gewoon de MF
	D->>fillFragmentList, %consequenties voor mogelijke toevoegingen
	D->>checkOk.
%%

%%
checkParentSelection(D):->
	%%check of de add/remove knop aan moet, en welke kant op
	%er kan maar 1 van de 2 lijsten een selectie hebben
	%(zie onSelect...)
	
	%1: de bovenste geselecteerd
	MF = D<<-selectedFragment,!,
	%gp3: change the image
	D?parentAddRemove_member->>setImage(down),
	D?parentAddRemove_member->>tooltip('Add the selected model fragment to the list of parents'),
	D?parents_member->>selection(@nil), %voor de zekerheid
	if
		(\+ ok = @model<<-norm_FragmentInheritance(
			MF,D?currentParents,D?element))
	then
		D?parentAddRemove_member->>active(@off)
	else
		D?parentAddRemove_member->>active(@on).
%
checkParentSelection(D):->
	%2: de onderste geselecteerd, bovenste gegarandeerd niet (1e clause faalde)
	D<<-selectedParent,!,
	%gp3: change the image
	D?parentAddRemove_member->>setImage(remove),
	D?parentAddRemove_member->>tooltip('Delete the selected model fragment from the list of parents'),
	D?parentAddRemove_member->>active(@on).
%
checkParentSelection(D):->
	%3: niets geselecteerd
	D?parentAddRemove_member->>active(@off). %gp3: do not change the image
%%

%%
checkOk(D):->
	%kijk of de ok button aan mag, ff afsnijden
	if
	(
		D?parents_member?members->>empty
		;
		0 = D?name_member?selection<<-size
	)
	then
		D?ok_member->>active(@off)
	else
		D?ok_member->>active(@on). 
%%

%%
selectedFragment(D,
	MF: modelFragment):<-
	%faalt als niet geselecteerd

	DI = D?fragments_member<<-selection,
	DI \== @nil,
	MF = DI<<-object.
%%

%%
selectedParent(D,
	MF: modelFragment):<-
	%faalt als niet geselecteerd

	DI = D?parents_member<<-selection,
	DI \== @nil,
	MF = DI<<-object.
%%

%%
saveNewElement(D):->
    @model->>changeRequest(newMF,
	@model,
	D?editor,
	D?name_member?selection,
	D?remarks_member?contents,
	D?currentParents,
	?(D?activeMenu_member,selected,active)
    ).
    % Give the model fragment the default editSize/origin
    %get(@model?modelFragments, find, @arg1?name == D?name_member?selection, ModelFragment),
    %get(@model?modelFragments, head, ModelFragment),
    %new(Position, point(@display?width *0.15, @display?height * 0.15)),
    %send(ModelFragment, layOutInfo(ModelFragment,displayOrigin, Position)),
    %new(DefSize, size(550,480)),
    %send(ModelFragment, layOutInfo(ModelFragment,editSize, DefSize)).
    
%%

%%
saveChangedElement(D):->
	%bestaande wijzigen
	@model->>changeRequest(changeMF,
		D?element,
		D?editor,
		D?name_member?selection,
		D?remarks_member?contents,
		D?currentParents,
		?(D?activeMenu_member,selected,active)
		).
%%

%%
notChanged(D):->

	D?name_member?selection->>equal(D?element?name),
	Active = D?element<<-active, Active = D?activeMenu_member<<-selected(active),
	D?remarks_member?contents->>equal(D?element?remarks),
	D?currentParents->>equal(D?element?parents).
%%

%%%%%%%%%mapping op selectie

%%
currentParents(D,C: chain):<-
	%geef lijst met de parents in de parentlijst
	C = D?parents_member<<-memberObjects.
%%

%%%%%%%%%change requestors%%%%%%%%%%%%
%%om te zorgen dat er niet iets wordt geselecteerd dat er niet meer is
%%etc., regelen we simpel dat bij belangrijkere wijzigingen deze dialoog sluit
%%

changeApplied_newMF(D,
	_CR: changeRequestor):->

	%hij is nieuw, dus de parents blijven sowieso hetzelfde
	%alleen de lijst met alle fragmenten kan wijzigen
	D->>fillFragmentList.
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
:-pce_end_class.
