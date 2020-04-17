/*
definitie attributePropsDlg klasse.
De standaard eigenschappen dialoog van attributes.
*/

:-pce_begin_class(attributePropsDlg,
		  propertyDialog,
		  "Standard attribute properties dialog"
		 ).

variable(instance,garpInstance,get,"The associated instance").
variable(lastValue,name*,both,"Last selected value"). %handig voor terugvinden
variable(instanceRoute, chain, both, "Route to the instance").

%%
initialise(D,F: frame):->
	"Initialise the properties dialog" ::
	%gp3 changed the buttons to imgButtons
	
	D->+initialise('Attribute properties - Build', F, later),
	
	%de onderdelen: lay out weer helemaal uitgeschreven
	GapX = D<<-gapX,
	GapY = D<<-gapY,
	MaxX *= number(0), %houden we de maximale X coordinaat in bij zodat we rechts kunnen uitlijnen


	State *= menu(state,marked),
	State->>append(new(Cond,menu_item(condition))),
	Cond->>colour(red),
	State->>append(new(Cons,menu_item(consequence))),
	Cons->>colour(blue),

	D->>display(State,point(GapX,D?topY)),

	MaxX->>maximum(State?right_side),

	DefList *= extendedListBrowser(width := 40),
	DefList->>name(defList),
	DefList->>label('Attribute definition:'),
	DefList->>select_message(->>(D,onDefSelection)),
	
	D->>display(DefList,point(GapX,State?bottom_side + GapY)),
	MaxX->>maximum(DefList?right_side),

	EditDef *= imgButton(editDef, img:=edit_attribute, tt:='Open attribute definitions editor'),
	D->>display(EditDef,point(DefList?right_side + GapX,
					DefList?bottom_side - EditDef?height)), %mooi onder aan de lijst
	MaxX->>maximum(EditDef?right_side),

	ValueList *= extendedListBrowser(width := 40),
	ValueList->>name(valueList),
	ValueList->>label('Value:'),
	ValueList->>select_message(->>(D,lastValue,@arg1?key)),

	D->>display(ValueList,point(GapX,
							DefList?bottom_side + GapY)),
	MaxX->>maximum(ValueList?right_side),


	%remarks komen zo breed als kan, maar dat wordt pas gezet wanneer we MaxX weten
	
	Remarks *= editor(height := 5, width := 40),
	Remarks->>name(remarks),
	Remarks->>label('Remarks:'),
	Remarks->>show_label(@on),
	Remarks->>fill_mode(@on),
	Remarks->>font(DefList?font),
	D->>display(Remarks,point(GapX,ValueList?bottom_side + GapY)), 
	MaxX->>maximum(Remarks?right_side),
	
	InstanceName *= label(instanceName, 'Instance:'),
	D->>display(InstanceName,point(GapX,Remarks?bottom_side + GapY)),

	Ok *= imgButton(ok, img:=save, tt:='Apply changes'),
	D->>display(Ok,point(GapX,InstanceName?bottom_side + GapY)),
	Ok->>default_button(@on),
	
	%MaxX is bekend nu
	ValueList->>right_side(MaxX),
	Remarks->>right_side(MaxX),
	Cancel *= imgButton(cancel, img:=undo, tt:= 'Cancel changes'),
	D->>display(Cancel,point(MaxX - Cancel?width,Ok?top_side)),
	D->>updateSpacers, %gp3 0.3.13
	D->>assign_accelerators,
	%minimal size:
	D->>minimalSize(size(Cancel?right_side,Cancel?bottom_side)), %abs min

	% Multiple model support
	get(@model, getModelNameForEditor, 'Attribute properties - Build', ModelNameForEditor),
	new(Garp3EditorFrame, garp3EditorFrame(ModelNameForEditor)),
	get(@model, '_value', Model),
	send(Garp3EditorFrame, associateModel, Model),
	send(Garp3EditorFrame, append, D),
	send(Garp3EditorFrame, transient_for, F).
%%

%%
onResize(D, Difference: size):->
	%gp3 0.2: 
	
	Definitions = D<<-defList_member,
	Definitions->>pixelWidth(Definitions?pixelWidth + Difference?width),
	%Definitions is the one that gets higher
	Definitions->>pixelHeight(Definitions?pixelHeight + Difference?height),
	%so the rest moves
	D?editDef_member->>set(x:= D?editDef_member?left_side + Difference?width,
							y:= D?editDef_member?top_side + Difference?height),
	D?valueList_member->>right_side(D?valueList_member?right_side + Difference?width),
	D?valueList_member->>set(y:=D?valueList_member?top_side + Difference?height),
	
	D?remarks_member->>right_side(D?valueList_member?right_side),
	D?remarks_member->>set(y:=D?remarks_member?top_side + Difference?height),
	D?instanceName_member->>right_side(D?valueList_member?right_side),
	D?instanceName_member->>set(y:=D?instanceName_member?top_side + Difference?height),
	D?ok_member->>set(y:=D?ok_member?top_side + Difference?height),
	D?cancel_member->>move(point(D?valueList_member?right_side - D?cancel_member?width, D?ok_member?top_side)).
%%

%%
newObject(D,
	Instance: garpInstance, %de bijbehorende instance
	InstanceRoute: chain, %route naar de bijbehorende instance
	  StartState: {condition,consequence}, %de state waarin de nieuwe gemaakt moet worden...
	CanSwitchState: [bool] %default @on
	 ):->
	"Open the dialog for a new attribute" ::

	%Als er geen attributeDefinitions zijn, openen we toch, want vanaf hier kan je naar de def-editor

	D->>slot(instance,Instance),
	D->>instanceRoute(InstanceRoute),

	D->>element(@nil),
	get(@model, getModelNameForEditor, 'Add a new attribute - Build', ModelNameForEditor),
	D->>label(ModelNameForEditor),
	D->>helpId(string('Build_%s_AddNewAttribute',D?containerType)),
	%zetten van de waarden
	if
		InstanceRoute->>empty
	then
		D?instanceName_member->>format('Instance: %s', Instance?name)
	else
		D?instanceName_member->>format('Instance: %s (imported)', Instance?name),

	D?state_member->>selection(StartState),
	default(CanSwitchState,@on,CanSwitch),
	D?state_member->>active(CanSwitch),

	D->>fillDefList,

	D->>openDialog.
%%

%%
editObject(D,
	A : garpAttribute,
	CanSwitchState: bool,
	ReadOnly : bool
	 ):->
	"Open dialog for existing attribute" ::

	D->>slot(instance,A?garpInstance),
	D->>instanceRoute(A?instanceRoute),

	D->>element(A), 

	%zetten van de waarden
	if
		A?instanceRoute->>empty
	then
		D?instanceName_member->>format('Instance: %s', A?garpInstance?name)
	else
		D?instanceName_member->>format('Instance: %s (imported)', A?garpInstance?name),

	if 
		A->>isCondition
	then
		D?state_member->>selection(condition)
	else
		D?state_member->>selection(consequence),

	D?state_member->>active(when(ReadOnly == @on,@off,CanSwitchState)),

	D->>fillDefList,

	D?defList_member->>selection(A?definition),
	D->>fillValueList,
	D?valueList_member->>selection(A?valueReference?valueName),
	D->>lastValue(A?valueReference?valueName),
	D?remarks_member->>contents(A?remarks),

	get(@model, getModelNameForEditor, string('Attribute properties%s - Build',when(ReadOnly == @on,' [Read Only]',''))?value, ModelNameForEditor),
	D->>label(ModelNameForEditor),
	D->>helpId(string('Build_%s_AttributeProperties',D?containerType)),
	D?ok_member->>active(ReadOnly?negate),
	D?defList_member->>changeable(ReadOnly?negate),
	D?valueList_member->>changeable(ReadOnly?negate),
	
	D->>openDialog.
%%

%%
fillDefList(D):->
	"(re)fill the attribute definitions list" ::

	List = D<<-defList_member,
	List->>clear,
	Defs = @model<<-sortedAttributeDefinitions,
	CreateItem *= create(dict_item,
						@arg1,
						@arg1?name,
						@arg1),
	Defs->>for_all(->>(List,
			    append,
				CreateItem)),
	CreateItem->>done,
	Defs->>done.
	
%%	

%%
onDefSelection(D):->
	"Attribute def selected" ::

	D->>fillValueList.
%%

%%
fillValueList(D):->
	"(Re)fill the attribute definitions values" ::

	%daarbij proberen we de huidige value vast te houden

	List = D<<-valueList_member,
	List->>clear,

	CurrentDefItem = D?defList_member<<-selection,

	if
		CurrentDefItem \== @nil
	then 
		CurrentDefItem?object?values->>for_all(->>(List,append,
						create(dict_item,@arg1?valueName,object := @arg1?copy))),

	%terugvinden vd selectie
	if
		List->>member(D?lastValue) 
	then
		List->>selection(D?lastValue)
	else
	(
		%gp3: make sure to get the first one
		unless
			List?members->>empty
		do
		(
			First = List?members<<-head,
			List->>selection(First), %first one
			D->>lastValue(First?key)
		)
	).
%%

%%
editDef(D):->
	"Edit definitions pressed" ::

	%kijk of we een selectie hebben

	CurrentDefItem = D?defList_member<<-selection,
	(
	    CurrentDefItem = @nil,
	    send(@app, openAttributeDefinitions) 
	;
	    send(@app, openAttributeDefinitions, CurrentDefItem?object)
	).

	%Editor *= attributeDefEditor(D), %gp3: send this dialog as openedBy
	%if
	%	CurrentDefItem = @nil
	%then
	%	Editor->>edit
	%else
	%	Editor->>edit(CurrentDefItem?object).
%%

%%
saveNewElement(D):->
	%nieuw element opslaan
	@model->>changeRequest(newAttribute,
		D?modelFragment,
		D?editor,
		D?instance,
		D?instanceRoute,
		D?state_member?selection,
		D?selectedAttributeDef,
		D?selectedValue,
		D?remarks_member?contents).
%%

%%
saveChangedElement(D):->
	%bestaand element opslaan
	@model->>changeRequest(changeAttribute,
		D?modelFragment,
		D?editor,
		D?element,
		D?state_member?selection,
		D?selectedAttributeDef,
		D?selectedValue,
		D?remarks_member?contents).
%%

%%
notChanged(D):->
	"Succeeds when the attribute is not changed by the user" ::
	%alleen voor bestaande attributen
	%we beschouwen het als gewijzigd als het sowieso anders, is, geen gedoe met makeGarp op dit nivo

	%definitieobject hetzelfde?
	D?selectedAttributeDef->>equal(D?element?definition),
	%value dezelfde?
	D?selectedValue->>equal(D?element?valueReference),

	%conditiestatus hetzelfde?
	if
		D?element->>isCondition
	then
		condition = D?state_member<<-selection
	else
		consequence = D?state_member<<-selection,

	%remarks hetzelfde?
	D?remarks_member?contents->>equal(D?element?remarks).
%%

%%%%%%%%%%%HELPERS%%%%%%%%%%%%%%%%%%%%%
%%
selectedAttributeDef(D,
	Def : garpAttributeDefinition*):<-
	"Return selected attribute definition or @nil" ::

	CurItem = D?defList_member<<-selection,
	if
		CurItem = @nil
	then
		Def = @nil
	else
		Def = CurItem<<-object.
%%

%%
selectedValue(D, V : valueReference *):<-
	"Return selected attribute value or @nil" ::

	CurItem = D?valueList_member<<-selection,
	if
		CurItem = @nil
	then
		V = @nil
	else
		V = CurItem<<-object.
%%

%%%%%%%%%change requestors%%%%%%%%%%%%

%%addAttributeDef
changeApplied_addAttributeDef(D,
	CR : changeRequestor):->
	%we moeten dus de lijst met defs uitbreiden 

	Def = D<<-selectedAttributeDef,
	D->>fillDefList,
	%gp3: if we opened the editor, we want to select the new definition
	if
		D = CR?editor<<-openedBy
	then
	(
		D?defList_member->>selection(CR?result),
		D->>fillValueList
	)
	else
		D?defList_member->>selection(Def).
%%

%%
changeApplied_changeAttributeDef(D,
	_CR : changeRequestor):->
	
	%Bijwerken en de selectie bewaren etc

	Def = D<<-selectedAttributeDef,
	D->>fillDefList,
	D?defList_member->>selection(Def),
	%en de waarden opnieuw vullen, doet een mooie poging om de value weer te pakken
	%(op naam nivo)
	D->>fillValueList,
	%als er nu niets geselecteerd is en we hebben een bestaande attribute, dan
	%moeten we maar eventjes opnieuw de echte value selecteren
	if (
		@nil = D<<-selectedValue,
		A = D<<-element,
		A \== @nil
		)
	then (
		D?valueList_member->>selection(A?valueReference?valueName),
		D->>lastValue(A?valueReference?valueName)
		).
		
%%

%%
changeApplied_deleteAttributeDef(D,
	_CR : changeRequestor):->	
	%bijwerken van de lijst met attribuut definities

	%we proberen weer dezelfde selectie aan te houden..
	Def = D<<-selectedAttributeDef,
	D->>fillDefList,
	
	if
		D?defList_member->>member(Def)
	then
		D?defList_member->>selection(Def),

	D->>fillValueList.	
%%

%%Bij wijzigingen van ons object sluiten we
changeApplied_changeAttribute(D,
	CR: changeRequestor):->

		\+ CR->>checkEditor(D?editor), %niet als deze editor (anders dubbel sluiten)
		CR->>checkArgument(1,D?element),
		D->>return.
%%

%%
changeApplied_deleteAttribute(D,
	CR: changeRequestor):->
	
		CR->>checkArgument(1,D?element),
		D->>return.
%%

:-pce_end_class.
