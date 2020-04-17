/*
definitie instancePropsDlg klasse.
De standaard eigenschappen dialoog van instances.
*/

:-pce_begin_class(instancePropsDlg,
		  propertyDialog,
		  "Standard instance properties dialog"
		 ).

variable(startType,{entity,agent},both).
variable(prevName,name,both). %voor de standaardnaam
%%
initialise(D, F: frame):->
	"Initialise the properties dialog" ::

	D->+initialise('Instance properties - Build', F, later),
	D->>prevName(''),
	
	GapX = D<<-gapX,
	GapY = D<<-gapY,

	%de onderdelen
	State *= menu(state,marked), 
	State->>append(new(Cond,menu_item(condition))),
	Cond->>colour(red),
	State->>append(new(Cons,menu_item(consequence))),
	Cons->>colour(blue),
	D->>display(State, point(GapX,D?topY)),
	
	Entities *= extendedListBrowser(width := 60,
					height := 6),
	Entities->>name(entities),
	Entities->>label('Entities:'), %kan nog wijzigen
	Entities->>show_label(@on),
	Entities->>select_message(->>(D,
				      onEntitySelection)),

	D->>display(Entities, point(GapX,State?bottom_side + GapY)),
	
	Name *= eventTextItem(name), 
	Name->>label('Name:'),
	D->>display(Name,point(GapX,Entities?bottom_side + GapY)),
	Name->>right_side(Entities?right_side),
	Name->>keyboard_focus(@on),
	
	Remarks *= editor(height := 5, width := 60),
	Remarks->>name(remarks),
	Remarks->>label('Remarks:'),
	Remarks->>show_label(@on),
	Remarks->>fill_mode(@on),
	Remarks->>font(Name?font),
	D->>display(Remarks,point(GapX,Name?bottom_side + GapY)),
	
	Ok *= imgButton(ok, img:=save, tt:='Apply changes'),
	D->>display(Ok,point(GapX,Remarks?bottom_side + GapY)),
	Ok->>default_button(@on),
	%Ok->>keyboard_focus(@on),

	%gp3: added definitions
	Defs *= imgButton(editDef, img:=edit_entities), %we are going to put that right later on
	D->>display(Defs, point(GapX + (Remarks?right_side - GapX - Defs?width) / 2, Ok?top_side)),
	
	Cancel *= imgButton(cancel, img:=undo, tt:= 'Cancel changes'),
	D->>display(Cancel,point(Remarks?right_side - Cancel?width,
		Ok?top_side)),
	D->>updateSpacers, %gp3 0.3.13
	D->>assign_accelerators,
		%minimal size:
	D->>minimalSize(size(Cancel?right_side,Cancel?bottom_side)), %abs min

	% Multiple model support
	get(@model, getModelNameForEditor, 'Instance properties - Build', ModelNameForEditor),
	new(Garp3EditorFrame, garp3EditorFrame(ModelNameForEditor)),
	get(@model, '_value', Model),
	send(Garp3EditorFrame, associateModel, Model),
	send(Garp3EditorFrame, append, D),
	send(Garp3EditorFrame, transient_for, F).
%%

%%
onResize(D, Difference: size):->
	%gp3 0.2: 
	
	Entities = D<<-entities_member,
	Entities->>pixelWidth(Entities?pixelWidth + Difference?width),
	%Entities is the one that gets higher
	Entities->>pixelHeight(Entities?pixelHeight + Difference?height),
	%so the rest moves
	D?name_member->>right_side(Entities?right_side),
	D?name_member->>set(y:=D?name_member?top_side + Difference?height),
	D?remarks_member->>right_side(Entities?right_side),
	D?remarks_member->>set(y:=D?remarks_member?top_side + Difference?height),
	D?ok_member->>set(y:=D?ok_member?top_side + Difference?height),
	D?editDef_member->>set(x:= D?editDef_member?left_side + Difference?width / 2, y:= D?ok_member?top_side),
	D?cancel_member->>move(point(Entities?right_side - D?cancel_member?width, D?ok_member?top_side)).
%%

%%
newObject(D,
	  StartType : {entity,agent},
	  StartState: {condition,consequence},
	CanSwitchState: [bool] %default @on

	 ):->
	"Open the dialog for a new instance" ::

	D->>startType(StartType),	
	D->>element(@nil),
	
	%gp3 0.2: set the right defbutton
	if
		StartType = entity
	then
	(
		get(@model, getModelNameForEditor, 'Add a new entity - Build', ModelNameForEditor),
		D->>label(ModelNameForEditor),
		D->>helpId(string('Build_%s_AddNewEntity',D?containerType)), %gp3 0.3.13
		D?editDef_member->>setImage(edit_entities),
		D?editDef_member->>tooltip('Open entity definitions editor')
	),
	if
		StartType = agent
	then
	(
		get(@model, getModelNameForEditor, 'Add a new agent - Build', ModelNameForEditor),
		D->>label(ModelNameForEditor),
		D->>helpId(string('Build_%s_AddNewAgent',D?containerType)), %gp3 0.3.13
		D?editDef_member->>setImage(edit_agents),
		D?editDef_member->>tooltip('Open agent definitions editor')
	),

       
	D?state_member->>selection(StartState),
	default(CanSwitchState,@on,CanSwitch),
	D?state_member->>active(CanSwitch),	

	D->>fillEntityList, %kan nu best
	D->>openDialog.
%%
	
	  
%%
editObject(D,
		Instance : garpInstance,
		CanSwitchState: bool,
	    ReadOnly : bool
	  ):->
	"Open the dialog for editing an existing instance"::

	D->>element(Instance),
	if
		Instance?entity->>instance_of(entity)
	then
	(
		D->>startType(entity),
		get(@model, getModelNameForEditor, string('Entity Instance properties%s - Build',when(ReadOnly == @on,' [Read Only]',''))?value, ModelNameForEditor),
		D->>label(ModelNameForEditor),
		D->>helpId(string('Build_%s_EntityInstanceProperties',D?containerType)), %gp3 0.3.13
		D?editDef_member->>setImage(edit_entities),
		D?editDef_member->>tooltip('Open entity definitions editor')
	)
	else
	(
		D->>startType(agent),
		get(@model, getModelNameForEditor, string('Agent Instance properties%s - Build',when(ReadOnly == @on,' [Read Only]',''))?value, ModelNameForEditor),
		D->>label(ModelNameForEditor),
		D->>helpId(string('Build_%s_AgentInstanceProperties',D?containerType)), %gp3 0.3.13
		D?editDef_member->>setImage(edit_agents),
		D?editDef_member->>tooltip('Open agent definitions editor')
	),
	
	if 
		Instance->>isCondition
	then
		D?state_member->>selection(condition)
	else
		D?state_member->>selection(consequence),
	D?state_member->>active(when(ReadOnly == @on,@off,CanSwitchState)),
	
	D->>fillEntityList,
	Item = D?entities_member?members<<-find(@arg1?object == Instance?entity),
	D?entities_member->>selection(Item),
	
	D?name_member->>selection(Instance?name),
	D?name_member->>editable(ReadOnly?negate),
	
	D?remarks_member->>contents(Instance?remarks),

	D?entities_member->>changeable(ReadOnly?negate),
	D?ok_member->>active(ReadOnly?negate),

	D->>openDialog.	
%%


%%
onEntitySelection(D):->
	%check of we nog met defaultnamen werken
	if
	(
		D?prevName->>equal(D?instanceName)
	;
		0 = D?instanceName<<-size
	)
	then
	(
		D->>prevName(D?entity?name),
		D?name_member->>selection(D?entity?name)
	).
%%

%%
editDef(D):->
	%gp3 0.2: open the definition
	E = D<<-entity,
	if
		E = @nil
	then
		EN = @default
	else
		EN = E,
	if
		entity = D<<-startType
	then
		@app->>openEntities(E)
	else
		@app->>openAgents(E).
%%
%%
notChanged(D):->
	%bij bewerkt element iets gewijzigd?
	
	D?instanceName->>equal(D?element?name),
	D?entity->>equal(D?element?entity),
	D?remarks->>equal(D?element?remarks),
	%conditiestatus hetzelfde?
	if
		D?element->>isCondition
	then
		condition = D<<-state
	else
		consequence = D<<-state.
%%

%%
saveNewElement(D):->
	@model->>changeRequest(newFInstance,
		D?modelFragment,
		D?editor,
		D?entity,
		D?state,
		D?instanceName,
		D?remarks).
%%

%%
saveChangedElement(D):->
	@model->>changeRequest(changeFInstance,
		D?modelFragment,
		D?editor,
		D?element,
		D?entity,
		D?state,
		D?instanceName,
		D?remarks).
%%

%%
state(D,
      S : {condition,consequence}
     ):<-
	"Return the selected state" ::

	State = D<<-member(state),
	S = State<<-selection.
%%

%%
entity(D,
       E : 'abstractEntity*'
      ):<-
	"Return the currently selected entity or @nil" ::

	Entities = D<<-member(entities),
	S = Entities<<-selection,
	if
		S = @nil
	then
		E = @nil
	else
		E = S<<-object.
%%

%%
instanceName(D,
	     N : name
	    ):<-
	"Return the given name" ::

	Name = D<<-member(name),
	N = Name<<-selection.
%%

%%
remarks(D,
	R : char_array
       ):<-
	"Return the given remarks" ::

	Remarks = D<<-member(remarks),
	R = Remarks<<-contents.
%%


	
%%
fillEntityList(D):->
	"Internal helper: fill the picklist" ::

	%we bewaren de selectie, als mogelijk

	List = D<<-member(entities),
	if
		(\+ @nil = List<<-selection)
	then
		OudeSelectie = List?selection<<-object
	else
			OudeSelectie = @nil,
		
	List->>clear,
	if
		entity = D<<-startType
	then
		( Top = @model<<-hypered(topEntity),
	      List->>label('Entities:')
	    )
	else
		( Top = @model<<-hypered(topAgent),
	      List->>label('Agents:')
	    ),
	
	%en vullen via helper
	D->>recursiveFillEntityList(Top,List,0,0),

	%selectie herstellen
	if
		Item = List?members<<-find( @arg1?object == OudeSelectie)
	then
		List->>selection(Item)
	else
	(
		List->>selection(List?members?head),
		D->>onEntitySelection
	).
%
recursiveFillEntityList(D,
			AE : abstractEntity,
			List : list_browser,
			Level : int,
			Key : number
		       ):->
	"Internal helper for fillEntityList" ::
	%we voegen de opgegeven abstr. entity toe aan de lijst
	%en gaan door voor de kinderen

	S *= string,
	S->>insert_character('-',0,Level), %geeft level aan
	S->>append(AE?name),

	%de naam van de entity is niet uniek omdat hij door multiple
	%inheritance vaker in de lijst kan komen
	%we sturen daarom een uniek nummer, waarvoor we de pce klasse
	%number gebruiken en niet een prolog getal, zodat we geen last
	%hebben van backtracken over het nummer als we uit de recursie komen

	List->>append(dict_item(Key,S,AE)),
	%recursie:
	Children = AE<<-children,
	Children->>sort(?(@arg1?name,compare,@arg2?name)),
	Children->>for_all(and(->>(Key,plus,1),
				  ->>(D, recursiveFillEntityList,
				    @arg1,
				      List,
				      Level + 1,
				      Key))).
%%

%%%%%%%%%%%%CHANGE REQUESTORS%%%%%%%%%%%%%%%%%

%%Bij sommige change requestors moeten we de entityList opnieuw doen

changeApplied_newHObjectChild(D,
			      _CR : changeRequestor
			     ):->
	D->>fillEntityList.

changeApplied_changeHObject(D,
				_CR : changeRequestor
			       ):->
	D->>fillEntityList. %ongeacht het type maar

changeApplied_deleteHObjectTree(D,
				CR : changeRequestor
			       ):->
	%we kunnen niet gewoon hervullen, want de entities zijn er waarschijnlijk nog
	%dus gooien we ze weg
	
	List = D<<-member(entities),
	List?members->>for_all(if(
		or(
			(@arg1?object == CR?object),
			->>(@arg1?object, isUp, CR?object)
		),
		->>(List,delete,@arg1)
		)).	
%%

%%Bij wijzigingen van onze instantie sluiten we
changeApplied_changeFInstance(D,
	CR: changeRequestor):->

		\+ CR->>checkEditor(D?editor), %niet als deze editor (anders dubbel sluiten)
		CR->>checkArgument(1,D?element),
		D->>return.
%%

%%
changeApplied_deleteFInstance(D,
	CR: changeRequestor):->
	
		CR->>checkArgument(1,D?element),
		D->>return.
%%

:-pce_end_class.
