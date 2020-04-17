/*
definitie hObjectPropsDlg klasse.
De standaard eigenschappen dialoog van Hierarchical Objects.

Part of GARP3
Old homer code, so a lot of comments in dutch

Note when this one is changed anyway: propertyDialog is not a very good choice for a parent class, because
that one assumes this is for the properties of an MF element.

*/

:-pce_begin_class(hObjectPropsDlg,
		  propertyDialog,
		  "Standard hierarchical object properties dialog"
		 ).

%%
initialise(D, F: frame):->
	"Initialise the properties dialog" ::

	D->+initialise('Object properties - Build',F, later), %moet later gp3 0.3.13: we do give a helpid, but change it later on
	GapX = D?gap<<-width,
	GapY = D?gap<<-height,
	
	%de onderdelen
	ParentList *= extendedListBrowser(width := 40,  
				  height := 10),
	ParentList->>name(parents),
	ParentList->>label('Parent:'),
	ParentList->>show_label(@on),
	D->>display(ParentList,point(GapX,D?topY)),

	Name *= eventTextItem(name), 
	Name->>label('Name:'),
	D->>display(Name,point(GapX,ParentList?bottom_side + GapY)),
	Name->>right_side(ParentList?right_side),
	Name->>keyboard_focus(@on),
	
	Remarks *= editor(width := 40, height := 5),
	Remarks->>name(remarks),
	Remarks->>label('Remarks:'),
	Remarks->>show_label(@on),
	Remarks->>fill_mode(@on),
	Remarks->>font(Name?font),
	D->>display(Remarks,point(GapX,Name?bottom_side + GapY)),

	Ok *= imgButton(ok, img:=save, tt:='Apply changes'),
	D->>display(Ok,point(GapX,Remarks?bottom_side + GapY)),
	Ok->>default_button(@on),

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
	
	Parent = D<<-parents_member,
	Parent->>pixelWidth(Parent?pixelWidth + Difference?width),
	%Parent is the one that gets higher
	Parent->>pixelHeight(Parent?pixelHeight + Difference?height),
	%so the rest moves
	D?name_member->>right_side(Parent?right_side),
	D?name_member->>set(y:=D?name_member?top_side + Difference?height),
	D?remarks_member->>right_side(Parent?right_side),
	D?remarks_member->>set(y:=D?remarks_member?top_side + Difference?height),
	D?ok_member->>set(y:=D?ok_member?top_side + Difference?height),
	D?cancel_member->>set(x:=Parent?right_side - D?cancel_member?width, y:=D?ok_member?top_side).
%%

%%
newObject(D,
	Parent: hierarchicalObject %de geselecteerde parent
	 ):->
	"Open the dialog for a new hierarchicalobject" ::

	D->>element(@nil), %betekent nieuw
	if
		Parent->>instance_of(entity)
	then
	(
		Label = 'Add a new entity - Build',
		Help = 'Build_EntityHierarchy_AddNewEntity'
	),
	if
		Parent->>instance_of(agent)
	then
	(
		Label = 'Add a new agent - Build',
		Help = 'Build_AgentHierarchy_AddNewAgent'
	),
	if
		Parent->>instance_of(assumption)
	then
	(
		Label = 'Add a new assumption - Build',
		Help = 'Build_AssumptionHierarchy_AddNewAssumption'
	),
	get(@model, getModelNameForEditor, Label, Label2),
	D->>label(Label2),
	D->>helpId(Help), %gp3 0.3.13
	D->>fillParentList(Parent?class_name),
	%en bepaal de selectie
	D?parents_member->>selection(Parent),	
	D->>openDialog.
%%

%%
editObject(D,
	Object: hierarchicalObject %het bijbehorende object	
	 ):->
	"Open dialog for existing hierarchicalObject" ::

	%gp3 0.13: This can also be the top entity
	%then we do not want any parent selection
	
	D->>element(Object),
	if
		Object->>instance_of(entity)
	then
	(
		Label = 'Entity Properties - Build',
		Help = 'Build_EntityHierarchy_EntityProperties'
	),
	if
		Object->>instance_of(agent)
	then
	(
		Label = 'Agent properties - Build',
		Help = 'Build_AgentHierarchy_AgentProperties'
	),
	if
		Object->>instance_of(assumption)
	then
	(
		Label = 'Assumption properties - Build',
		Help = 'Build_AssumptionHierarchy_AssumptionProperties'
	),
	get(@model, getModelNameForEditor, Label, Label2),
	D->>label(Label2),
	D->>helpId(Help), %gp3 0.3.13

	unless
		Object = @model<<-topEntity %gp3 only if it has a parent we set the parent list
	do
	(
		D->>fillParentList(Object?class_name),
		%data er in zetten
		Item = D?parents_member?members<<-find(@arg1?object == Object?parent),
		D?parents_member->>selection(Item)
	),
	D?name_member->>selection(Object?name),
	D?remarks_member->>contents(Object?remarks),
	D->>openDialog.
%%

%%
fillParentList(D, Type: {entity,agent,assumption}):->
	"(re)fill the parentlist" ::

	List = D<<-parents_member,
	List->>clear,
	if
		Type == entity
	then
		Top = @model<<-hypered(topEntity),
	if
		Type == agent
	then
		Top = @model<<-hypered(topAgent),
	if
		Type == assumption
	then
		Top = @model<<-hypered(topAssumption),
	%voor snelheid eventjes van te voren het object opzoeken
	O = D<<-element,
	D->>recursiveFillParentList(Top,List,O,0).
%%	

%%
recursiveFillParentList(_D,
			HO : hierarchicalObject,
			_List : extendedListBrowser,
			Object: hierarchicalObject,
			_Level : int):->
	"Internal helper for fillParentList" ::
	%en dit is de stopclause: we hoeven niet verder den boom in als we bij het object zijn (want hij kan geen kind van zichzelf of zijn kinderen worden)
	
	HO == Object,!.
%		
recursiveFillParentList(D,
			HO,List,Object,Level
		       ):->

	%we voegen de opgegeven hierarchicalObject toe aan de lijst
	%en gaan door voor de kinderen

	S *= string,
	S->>insert_character('-',0,Level), %geeft level aan
	S->>append(HO?name),

	%we hebben (itt versies voor multiple inheritance) geen key nodig
	%die een entry in de lijst qua weergave uniek maakt: elke naam en elk
	%object komt maar één keer voor

	List->>append(dict_item(HO,S,HO)),
	%recursie:
	Children = HO<<-children,
	Children->>sort(?(@arg1?name,compare,@arg2?name)),
	Children->>for_all(->>(D, recursiveFillParentList,
				    @arg1,
				      List,
					  Object,
				      Level + 1)).
%%

%%
saveNewElement(D):->
	%nieuwe maken
	Parent = D<<-selectedParent,
	@nil \== Parent, %voor de zekerheid, als het goed is doet de list dat ok
	@model->>changeRequest(newHObjectChild,
		Parent,
		D?editor,
		D?name_member?selection,
		D?remarks_member?contents).
%%

%%
saveChangedElement(D):->
	%bestaande wijzigen
	%gp3: topEntity can be edited but has no parent
	if
		D?element = @model<<-topEntity
	then
		Parent = @nil
	else
		Parent = D<<-selectedParent,
	@model->>changeRequest(changeHObject,
		D?element,
		D?editor,
		D?name_member?selection,
		Parent,
		D?remarks_member?contents).
%%

%%
notChanged(D):->

	%zelfde parent?
	%gp3: topEntity can be edited but has no parent
	unless
		D?element = @model<<-topEntity
	do
	(
		P = D<<-selectedParent,
		P = D?element<<-parent
	),
	D?name_member?selection->>equal(D?element?name),
	D?remarks_member?contents->>equal(D?element?remarks).
%%

%%%%%%%%%mapping op selectie
%%
selectedParent(D,
	HO: hierarchicalObject*
	):<-

	if 
		@nil = D?parents_member<<-selection
	then
		HO = @nil
	else
		HO = D?parents_member?selection<<-object.
%%

%

	
%%%%%%%%%change requestors%%%%%%%%%%%%
%%om te zorgen dat er niet iets wordt geselecteerd dat er niet meer is
%%etc...

:-pce_end_class.
