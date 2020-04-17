/*
definitie configurationPropsDlg klasse.
De standaard eigenschappen dialoog van configuraties.
*/

:-pce_begin_class(configurationPropsDlg,
		  propertyDialog,
		  "Standard configuration properties dialog"
		 ).

variable(arg1,garpInstance,both,"The associated first argument").
variable(arg1Route, chain, both, "Route to the first instance").
variable(arg2,garpInstance,both,"The associated second argument").
variable(arg2Route, chain, both, "Route to the second instance").
variable(readOnly,bool,both).

%%
initialise(D,F: frame):->
	"Initialise the properties dialog" ::
	%gp3 0.2 changed buttons to imgButtons
	
	D->+initialise('Configuration properties - Build', F,later),
	D->>readOnly(@off),
	
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

	Arg1 *= extendedListBrowser(width:=25, height:=3),
	Arg1->>name(arg1),
	Arg1->>show_label(@off),
	Arg1->>pen(0),
	Arg1?image->>slot(background,D?background),
	D->>display(Arg1,point(GapX, State?bottom_side + GapY)),

	Switch *= imgButton(switch, tt:='Switch arguments'),
	D->>display(Switch,point(Arg1?right_side + GapX,
							Arg1?bottom_side - Switch?height)),

	Arg2 *= extendedListBrowser(width:=25, height:=3),
	Arg2->>name(arg2),
	Arg2->>show_label(@off),
	Arg2->>pen(0),
	Arg2?image->>slot(background,D?background),
	D->>display(Arg2,point(Switch?right_side + GapX,
							Arg1?top_side)),

	MaxX->>maximum(Arg2?right_side),

	DefList *= extendedListBrowser(width := 40),
	DefList->>name(defList),
	DefList->>label('Configuration definition:'),
	
	D->>display(DefList,point(GapX,Switch?bottom_side + GapY)),
	MaxX->>maximum(DefList?right_side),

	EditDef *= imgButton(editDef, img:= edit_configuration, tt:='Open configuration definitions editor'),
	D->>display(EditDef,point(DefList?right_side + GapX,
					DefList?bottom_side - EditDef?height)), %mooi onder aan de lijst
	MaxX->>maximum(EditDef?right_side),

	%remarks komen zo breed als kan, maar dat wordt pas gezet wanneer we MaxX weten
	
	Remarks *= editor(height := 5, width := 40),
	Remarks->>name(remarks),
	Remarks->>label('Remarks:'),
	Remarks->>show_label(@on),
	Remarks->>fill_mode(@on),
	Remarks->>font(DefList?font),
	D->>display(Remarks,point(GapX,EditDef?bottom_side + GapY)), 
	MaxX->>maximum(Remarks?right_side),
	
	Ok *= imgButton(ok, img:=save, tt:='Apply changes'),
	D->>display(Ok,point(GapX,Remarks?bottom_side + GapY)),
	Ok->>default_button(@on),
	
	%MaxX is bekend nu
	Cancel *= imgButton(cancel, img:=undo, tt:= 'Cancel changes'),
	D->>display(Cancel,point(MaxX - Cancel?width,Ok?top_side)),
	D->>updateSpacers, %gp3 0.3.13
	D->>assign_accelerators,
	%minimal size:
	D->>minimalSize(size(MaxX,Cancel?bottom_side)), %abs min

	% Multiple model support
	get(@model, getModelNameForEditor, 'Configuration properties - Build', ModelNameForEditor),
	new(Garp3EditorFrame, garp3EditorFrame(ModelNameForEditor)),
	get(@model, '_value', Model),
	send(Garp3EditorFrame, associateModel, Model),
	send(Garp3EditorFrame, append, D),
	send(Garp3EditorFrame, transient_for, F).

%%

%%
newObject(D,
	  Arg1: garpInstance, %Instance argument 1
		Arg1Route: chain,
	  Arg2: garpInstance, %Instance argument 2
		Arg2Route: chain,
	  StartState: {condition,consequence}, %de state waarin de nieuwe gemaakt moet worden...
	CanSwitchState: [bool] %default @on
	 ):->
	"Open the dialog for a new configuration" ::

	%Als er geen configurationDefinitions zijn, openen we toch, want vanaf hier kan je naar de def-editor
	D->>arg1(Arg1),
	D->>arg1Route(Arg1Route),
	D->>arg2(Arg2),
	D->>arg2Route(Arg2Route),

	D->>element(@nil),
	get(@model, getModelNameForEditor, 'Add a new configuration - Build', ModelNameForEditor),
	D->>label(ModelNameForEditor),
	D->>helpId(string('Build_%s_AddNewConfiguration',D?containerType)), %gp3 0.3.13

	%zetten van de waarden
	D->>displayArguments,

	D?state_member->>selection(StartState),
	default(CanSwitchState,@on,CanSwitch),
	D?state_member->>active(CanSwitch),

	D->>fillDefList,
	D->>openDialog.
%%

%%
editObject(D,
	C : configuration,
	CanSwitchState: bool,
	ReadOnly : bool	 
	 ):->
	"Open dialog for existing configuration" ::

	D->>arg1(C?argument1),
	D->>arg1Route(C?argument1Route),
	D->>arg2(C?argument2),
	D->>arg2Route(C?argument2Route),
	D->>element(C), 

	D->>readOnly(ReadOnly),
	
	%zetten van de waarden
	D->>displayArguments,

	if 
		C->>isCondition
	then
		D?state_member->>selection(condition)
	else
		D?state_member->>selection(consequence),

	D?state_member->>active(when(ReadOnly == @on,@off,CanSwitchState)),

	D->>fillDefList,

	D?defList_member->>selection(C?definition),
	D?remarks_member->>contents(C?remarks),

	get(@model, getModelNameForEditor, string('Configuration properties%s - Build',when(ReadOnly == @on,' [Read Only]',''))?value, ModelNameForEditor),
	D->>label(ModelNameForEditor),
	D->>helpId(string('Build_%s_ConfigurationProperties',D?containerType)), %gp3 0.3.13
	D?ok_member->>active(ReadOnly?negate),
	D?switch_member->>active(ReadOnly?negate),
	D?defList_member->>changeable(ReadOnly?negate),
	D->>openDialog.
%%

%%
displayArguments(D):->
	%beeldt de argumenten op een standaard manier af

	D?arg1_member->>clear,
	if
		D?arg1Route->>empty
	then
		S1 = 'Instance: %s'
	else
		S1 = 'Instance: %s (imported)',

	D?arg1_member->>append(
			dict_item(?(string(S1,D?arg1?name),split_lines,D?arg1_member?width))),
	D?arg2_member->>clear,
	if
		D?arg2Route->>empty
	then
		S2 = 'Instance: %s'
	else
		S2 = 'Instance: %s (imported)',

	D?arg2_member->>append(
			dict_item(?(string(S2,D?arg2?name),split_lines,D?arg2_member?width))).

%%
fillDefList(D):->
	"(re)fill the configuration definitions list" ::

	List = D<<-defList_member,
	List->>clear,
	Defs = @model<<-sortedConfigurationDefinitions,
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
onResize(D, Difference: size):->
	%gp3 0.2: 
	
	Arg1 = D<<-arg1_member,
	Arg2 = D<<-arg2_member,
	
	Arg1->>pixelWidth(Arg1?pixelWidth + Difference?width / 2),
	Arg2->>pixelWidth(Arg2?pixelWidth + Difference?width / 2),	
	Arg2->>set(x:= Arg2?left_side + Difference?width / 2),
	
	D?switch_member->>set(x:= D?switch_member?left_side + Difference?width / 2),
	
	D?editDef_member->>set(x:= Arg2?right_side - D?editDef_member?width),
	D?defList_member->>right_side(D?editDef_member?left_side - D?gapX),
	D?defList_member->>pixelHeight(D?defList_member?pixelHeight + Difference?height),
	D?editDef_member->>set(y:= D?defList_member?bottom_side - D?editDef_member?height),
	
	
	D?remarks_member->>right_side(Arg2?right_side),
	D?remarks_member->>set(y:= D?remarks_member?top_side + Difference?height),
	D?ok_member->>set(y:=D?ok_member?top_side + Difference?height),
	D?cancel_member->>move(point(Arg2?right_side - D?cancel_member?width, D?ok_member?top_side)).
%%

%%
editDef(D):->
	"Edit definitions pressed" ::

	%kijk of we een selectie hebben

	CurrentDefItem = D?defList_member<<-selection,

	%Editor *= configurationDefEditor(D), %gp3: add the openedBy..
	(
	    CurrentDefItem = @nil,
	    send(@app, openConfigurationDefinitions)
	;
	    send(@app, openConfigurationDefinitions, CurrentDefItem?object)
	).
%%

%%
switch(D):->
	"Switch arguments pressed" ::

	A1 = D<<-arg1,
	A1R = D?arg1Route<<-copy,
	D->>arg1(D?arg2),
	D->>arg1Route(D?arg2Route?copy),
	D->>arg2(A1),
	D->>arg2Route(A1R),
	D->>displayArguments.
%%

%%
saveNewElement(D):->
		%nieuwe maken
	@model->>changeRequest(newConfiguration,
		D?modelFragment,
		D?editor,
		D?state_member?selection,
		D?selectedDef,
		D?arg1,D?arg1Route,D?arg2,D?arg2Route,
		D?remarks_member?contents).
%%

%%
saveChangedElement(D):->
	%bestaande opslaan
	@model->>changeRequest(changeConfiguration,
		D?modelFragment,
		D?editor,
		D?element,
		D?state_member?selection,
		D?selectedDef,
		D?arg1,D?arg1Route,D?arg2,D?arg2Route, %we moeten het meesturen, want
											%het kan gewisseld zijn
		D?remarks_member?contents).
%%

%%
notChanged(D):->
	"Succeeds when the configuration is not changed by the user" ::
	%we beschouwen het als gewijzigd als het sowieso anders, is, geen gedoe met makeGarp op dit nivo

	%definitieobject hetzelfde?
	D?selectedDef->>equal(D?element?definition),
	%argumenten hetzelfde (aangezien we alleen kunnen switchen hoeven we alleen arg1 te testen)
	D?arg1->>equal(D?element?argument1),
	%gp3 0.2: also have to check route
	D?arg1Route->>equal(D?element?argument1Route),

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
selectedDef(D,
	Def : configurationDefinition*):<-
	"Return selected configuration definition or @nil" ::

	CurItem = D?defList_member<<-selection,
	if
		CurItem = @nil
	then
		Def = @nil
	else
		Def = CurItem<<-object.
%%


%%%%%%%%%change requestors%%%%%%%%%%%%
%%
changeApplied_addConfigurationDef(D,
	CR : changeRequestor):->
	%hervullen vd lijst en de goede weer selecteren

	Def = D<<-selectedDef,
	D->>fillDefList,
	%gp3: if we opened the editor, we want to select the new definition
	if
		D = CR?editor<<-openedBy
	then
		D?defList_member->>selection(CR?result)
	else
		D?defList_member->>selection(Def).
%%

%%
changeApplied_changeConfigurationDef(D,
	_CR : changeRequestor):->
	
	%Bijwerken en de selectie bewaren etc

	Def = D<<-selectedDef,
	D->>fillDefList,
	D?defList_member->>selection(Def).
%%

%%
changeApplied_deleteConfigurationDef(D,
	_CR : changeRequestor):->	
	%bijwerken van de lijst met definities

	%we proberen weer dezelfde selectie aan te houden..
	Def = D<<-selectedDef,
	D->>fillDefList,
	
	if
		D?defList_member->>member(Def)
	then
		D?defList_member->>selection(Def).
%%

%%
changeApplied_changeConfiguration(D,
	CR: changeRequestor):->

	%dicht als wij dezelfde editen
	\+ CR->>checkEditor(D?editor), %niet als deze editor (anders dubbel sluiten)
	CR->>checkArgument(1,D?element),
	D->>return.
%%

%%
changeApplied_deleteConfiguration(D,
	CR: changeRequestor):->

	%dicht als wij dezelfde editen
	CR->>checkArgument(1,D?element),
	D->>return.
%%
:-pce_end_class.
