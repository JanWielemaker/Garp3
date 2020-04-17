/*
definitie inputSystemPropsDlg klasse.
De standaard eigenschappen dialoog van  InputSystems
Aangeroepen vanuit inputSystemList en view-editor
*/

:-pce_begin_class(inputSystemPropsDlg,
		  propertyDialog,
		  "Standard input system properties dialog"
		 ).

%variable(inputSystemList, inputSystemList, both, 'The input system list instead of the frame').

%%
initialise(D, F: 'frame|dialog'):->
	"Initialise the properties dialog" ::
	D->+initialise('Object properties - Build', F, later),

	GapX = D?gap<<-width,
	GapY = D?gap<<-height,
		
	Name *= eventTextItem(name), 
	Name->>width(60),
	Name->>label('Name:'),
	D->>display(Name,point(GapX,D?topY)),

	Name->>afterKey(->>(D,checkOk)), %anders gaat ie weer aan als er een naam staat (pce)
	Name->>keyboard_focus(@on),
	
	Remarks *= editor(width := 60, height := 5),
	Remarks->>name(remarks),
	Remarks->>label('Remarks:'),
	Remarks->>show_label(@on),
	Remarks->>fill_mode(@on),
	Remarks->>font(Name?font),
	D->>display(Remarks,point(GapX,Name?bottom_side + GapY)),
	Remarks->>right_side(Name?right_side),
	
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
	
	D?name_member->>right_side(D?name_member?right_side + Difference?width),
	D?remarks_member->>right_side(D?name_member?right_side),
	D?remarks_member->>bottom_side(D?remarks_member?bottom_side + Difference?height),
	%D?structureMessage_member->>right_side(D?name_member?right_side),
	D?structureMessage_member->>set(y:=D?structureMessage_member?top_side + Difference?height),
	D?ok_member->>set(y:=D?ok_member?top_side + Difference?height),
	D?cancel_member->>move(point(D?remarks_member?right_side - D?cancel_member?width, D?ok_member?top_side)).
%%

%%
newObject(D):->
	"Open the dialog for a new inputSystem" ::

	D->>element(@nil), %betekent nieuw
	get(@model, getModelNameForEditor, 'Add a new scenario - Build', ModelNameForEditor),
	D->>label(ModelNameForEditor),
	D->>helpId('Build_AddNewScenario'), %gp3 0.3.13
	D?name_member->>selection('New scenario'),
	D->>checkOk, %zou goed moeten zijn
	D->>openDialog.
%%

%%
editObject(D,
	Object: inputSystem %het bijbehorende object	
	 ):->
	"Open dialog for existing modelFragment" ::

	D->>element(Object),
	get(@model, getModelNameForEditor, 'Scenario Properties - Build', ModelNameForEditor),
	D->>label(ModelNameForEditor),
	D->>helpId('Build_ScenarioProperties'), %gp3 0.3.13
	%data er in zetten
	D?name_member->>selection(Object?name),
	D?remarks_member->>contents(Object?remarks),
	D->>checkOk, %zou goed moeten zijn
	D->>checkStructureMessage, %incomplete?
	D->>openDialog.
%%

%%
checkStructureMessage(D):->
	%De structuremessage wordt gezet: is het is compleet
	%(wordt niet bijgewerkt bij wijzigingen, dit is tenslotte een dialoog)
	
	if
	(
		\+ @nil = D<<-element,
		D?element->>isIncomplete
	)
	then
		D?structureMessage_member->>selection('Warning: This scenario has an incomplete structure')
	else
		D?structureMessage_member->>selection('').
%%
	
%%
checkOk(D):->
	%kijk of de ok button aan mag, ff afsnijden
	if
	(
		0 = D?name_member?selection<<-size
	)
	then
		D?ok_member->>active(@off)
	else
		D?ok_member->>active(@on). 
%%

%%
saveNewElement(D):->
    @model->>changeRequest(newInputSystem,
	@model,
	D?editor,
	D?name_member?selection,
	D?remarks_member?contents
	),
    % Give the scenario the default editSize/origin
    %get(@model?modelFragments, find, @arg1?name == D?name_member?selection, Scenario),
    get(@model?modelFragments, head, Scenario),
    new(Position, point(@display?width *0.15, @display?height * 0.15)),
    send(Scenario, layOutInfo(Scenario,displayOrigin, Position)),
    new(DefSize, size(550,480)),
    send(Scenario, layOutInfo(Scenario, editSize, DefSize)).
	
%%

%%
saveChangedElement(D):->
	%bestaande wijzigen
	@model->>changeRequest(changeInputSystem,
		D?element,
		D?editor, 
		D?name_member?selection,
		D?remarks_member?contents).
%%

%%
notChanged(D):->

	D?name_member?selection->>equal(D?element?name),
	D?remarks_member?contents->>equal(D?element?remarks).
%%
%%%%%%%%%change requestors%%%%%%%%%%%%
%%om te zorgen dat er niet iets wordt geselecteerd dat er niet meer is
%%etc., regelen we simpel dat bij belangrijkere wijzigingen deze dialoog sluit
%%

changeApplied_changeInputSystem(D,
	_CR: changeRequestor):->
	
	%we sluiten
	D->>cancel.

%%

changeApplied_deleteInputSystem(D,
	_CR: changeRequestor):->
	
	D->>cancel. %sluiten

%%
:-pce_end_class.
