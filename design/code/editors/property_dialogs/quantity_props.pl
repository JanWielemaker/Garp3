/*
definitie quantityPropsDlg klasse.
De standaard eigenschappen dialoog van quantities.
*/

:-pce_begin_class(quantityPropsDlg,
		  propertyDialog,
		  "Standard quantity properties dialog"
		 ).

variable(lastQS, quantitySpace*, both, "The last selected qs").
variable(instance,garpInstance,get,"The associated instance").
variable(instanceRoute, chain, both, "Route to the instance").
%%
initialise(D, F: frame):->
	"Initialise the properties dialog" ::
	%gp3 0.2 changed buttons to imgButtons
	%gp3 0.4.9 made the dialog less high by lowering the heights of the lists
	%(see also onResize)
	%gp3 0.4.11 made the dialog higher again, and introduced a local variable others can use to tweak this
	

	D->+initialise('Quantity properties - Build',F, later),
	if
		D?modelFragment->>isInputSystem
	then
		LISTHEIGHT = 6
	else
		LISTHEIGHT = 9,
		
	%de onderdelen
	GapX = D<<-gapX,
	GapY = D<<-gapY,
	MaxX *= number(0),

	State *= menu(state,marked),
	State->>append(new(Cond,menu_item(condition))),
	Cond->>colour(red),
	State->>append(new(Cons,menu_item(consequence))),
	Cons->>colour(blue),

	D->>display(State,point(GapX,D?topY)),

	QuantityDefList *= extendedListBrowser(width := 25,  
				  height := LISTHEIGHT),
	QuantityDefList->>name(quantityDefs),
	QuantityDefList->>label('Quantity definition:'),
	QuantityDefList->>show_label(@on),
	QuantityDefList->>select_message(->>(D,
				       onQuantityDefSelection)),

	D->>display(QuantityDefList,point(GapX,State?bottom_side + GapY)),

	EditDef *= imgButton(editDef, img:=edit_quantity, tt:='Open quantity definitions editor'),
	D->>display(EditDef, point(GapX, QuantityDefList?bottom_side + GapY)),
	
	QSList *= extendedListBrowser(width := 25,
			       height := LISTHEIGHT),
	QSList->>name(qslist),
	QSList->>label('Quantity space:'),
	QSList->>show_label(@on),
	QSList->>select_message(->>(D,
				     onQSSelection,@arg1?object)),
	D->>display(QSList, point(GapX, EditDef?bottom_side + GapY)),

	QSShow *= qsPreviewBrowser(width := 15, height := LISTHEIGHT), %controls
	QSShow->>name(qsshow),
	QSShow->>changeable(@off), %gp3 0.1
	QSShow->>label('Preview:'),
	QSShow->>show_label(@on),
	D->>display(QSShow, point(QSList?right_side 
	+ GapX, QSList?top_side)),
	
	MaxX->>maximum(QSShow?right_side),
	
	Remarks *= editor(width := 20, %will be corrected
			  height := 4),
	Remarks->>name(remarks),
	Remarks->>label('Remarks:'),
	Remarks->>show_label(@on),
	Remarks->>fill_mode(@on),
	Remarks->>font(QuantityDefList?font),
	D->>display(Remarks, point(GapX, QSShow?bottom_side + GapY)),
	MaxX->>maximum(Remarks?right_side),
	
	InstanceName *= label(instanceName, 'Instance:'),
	D->>display(InstanceName, point(GapX, Remarks?bottom_side + GapY)),
	if
		D?modelFragment->>isInputSystem
	then
	(
		%gp3 0.3.16: quantity behaviours
		BehaviourLabel *= label(behaviourLabel,'Advanced quantity behaviour:',italic),
		D->>display(BehaviourLabel,point(GapX,InstanceName?bottom_side + GapY)),
		new(Behaviours, menu(behaviours, toggle)),
		Behaviours->>show_label(@off),	
		Behaviours->>layout(vertical),
		Behaviours->>append(menu_item(generate_all_values, label:= 'Generate all values')),
		Behaviours->>append(menu_item(constant, label:= 'Constant')),
		D->>display(Behaviours,point(GapX,BehaviourLabel?bottom_side + GapY)),
	
		%gp3 0.4.6: extra menu for exogenous behaviour (used to be in behaviours menu)
		new(ExBehaviours, menu(exbehaviours, cycle)),
		ExBehaviours->>label('Exogenous:'),
		ExBehaviours->>label_font('normal'),
		ExBehaviours->>show_label(@on),	
		ExBehaviours->>append(menu_item(exogenous_none, label:= 'None')),
		ExBehaviours->>append(menu_item(exogenous_decreasing, label:= 'Decrease')),
		ExBehaviours->>append(menu_item(exogenous_steady, label:= 'Steady')),
		ExBehaviours->>append(menu_item(exogenous_increasing, label:= 'Increase')),
		ExBehaviours->>append(menu_item(exogenous_sinus, label:= 'Sinusoidal')),
		ExBehaviours->>append(menu_item(exogenous_pos_parabola, label:= 'Parabola (Positive)')), % new FL june 07
		ExBehaviours->>append(menu_item(exogenous_neg_parabola, label:= 'Parabola (Negative)')), % new FL june 07
		ExBehaviours->>append(menu_item(exogenous_free, label:= 'Random')),
		D->>display(ExBehaviours,point(Behaviours?right_side + GapX, Behaviours?top_side)),
		
		MaxX->>maximum(ExBehaviours?right_side),
		ButtonY = Behaviours?bottom_side + GapY
	)
	else
		ButtonY =  InstanceName?bottom_side + GapY,
	
	Ok *= imgButton(ok, img:=save, tt:='Apply changes'),
	D->>display(Ok,point(GapX,ButtonY)),
	Ok->>default_button(@on),
	%Ok->>keyboard_focus(@on),

	Cancel *= imgButton(cancel, img:=undo, tt:= 'Cancel changes'),
	D->>display(Cancel,point(MaxX - Cancel?width,ButtonY)),
	D->>updateSpacers, %gp3 0.3.13
	QuantityDefList->>right_side(MaxX),
	QSShow->>set(x:=MaxX - QSShow?pixelWidth),
	QSList->>right_side(QSShow?left_side - GapX),
	D->>assign_accelerators,
	%minimal size:
	D->>minimalSize(size(Cancel?right_side,Cancel?bottom_side)), %abs min

	% Multiple model support
	get(@model, getModelNameForEditor, 'Quantity properties - Build', ModelNameForEditor),
	new(Garp3EditorFrame, garp3EditorFrame(ModelNameForEditor)),
	get(@model, '_value', Model),
	send(Garp3EditorFrame, associateModel, Model),
	send(Garp3EditorFrame, append, D),
	send(Garp3EditorFrame, transient_for, F).
%%

%%
onResize(D, Difference: size):->
	%gp3 0.2: 
	%gp3 0.4.9: height resize is shared between the defs and the qs:
	%we calculate that in order to avoid rounding differences
	
	FirstHeight *= number(Difference?height / 2),
	RestHeight *= number(Difference?height - FirstHeight),
	
	Definitions = D<<-quantityDefs_member,
	Definitions->>pixelWidth(Definitions?pixelWidth + Difference?width),
	%Definitions is the one that gets higher
	Definitions->>pixelHeight(Definitions?pixelHeight + FirstHeight),
	
	D?editDef_member->>set(y:= D?editDef_member?top_side + FirstHeight),
	D?qslist_member->>pixelWidth(D?qslist_member?pixelWidth + Difference?width),
	D?qslist_member->>set(y:=D?qslist_member?top_side + FirstHeight),
	
	D?qsshow_member->>set(
		x:=D?qsshow_member?left_side + Difference?width,
		y:=D?qsshow_member?top_side + FirstHeight),

	%rest of height change goes here
	D?qslist_member->>pixelHeight(D?qslist_member?pixelHeight + RestHeight),		
	D?qsshow_member->>pixelHeight(D?qsshow_member?pixelHeight + RestHeight),	
	D?remarks_member->>right_side(Definitions?right_side),
	D?remarks_member->>set(y:=D?remarks_member?top_side + Difference?height),
	D?instanceName_member->>right_side(Definitions?right_side),
	D?instanceName_member->>set(y:=D?instanceName_member?top_side + Difference?height),
	if
		D<<-member(behaviours)
	then
	(
		%gp3 0.3.16: we have the behaviourprops
		D?behaviourLabel_member->>set(y:=D?behaviourLabel_member?top_side + Difference?height),
		D?behaviours_member->>set(y:=D?behaviours_member?top_side + Difference?height),
		%gp3 0.4.6: So we also have the exbehaviours
		D?exbehaviours_member->>set(y:=D?exbehaviours_member?top_side + Difference?height)
	),
	D?ok_member->>set(y:=D?ok_member?top_side + Difference?height),
	D?cancel_member->>move(point(Definitions?right_side - D?cancel_member?width, D?ok_member?top_side)).
%%

%%
newObject(D,
	Instance: garpInstance, %de bijbehorende instance
	InstanceRoute: chain, %route naar de bijbehorende instance
	  StartState: {condition,consequence}, %de state waarin de nieuwe gemaakt moet worden...
	CanSwitchState: bool
	 ):->
	"Open the dialog for a new quantity" ::

	%Als er geen quantityDefinitions zijn, openen we toch, want vanaf hier kan je naar de def-editor

	D->>slot(instance,Instance),
	D->>instanceRoute(InstanceRoute),
	D->>element(@nil), %betekent nieuw
	get(@model, getModelNameForEditor, 'Add a new quantity - Build', ModelNameForEditor),
	D->>label(ModelNameForEditor),
	D->>helpId(string('Build_%s_AddNewQuantity',D?containerType)),
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

	D->>fillQuantityDefList,
	D->>openDialog.
%%

%%
editObject(D,
	Q : garpQuantity,
	CanSwitchState: bool,
	ReadOnly : bool	 ):->
	"Open dialog for existing quantity" ::

	D->>slot(instance,Q?garpInstance),
	D->>instanceRoute(Q?instanceRoute),

	D->>element(Q),

	%zetten van de waarden
	if
		Q?instanceRoute->>empty
	then
		D?instanceName_member->>format('Instance: %s', Q?garpInstance?name)
	else
		D?instanceName_member->>format('Instance: %s (imported)', Q?garpInstance?name),

	if 
		Q->>isCondition
	then
		D?state_member->>selection(condition)
	else
		D?state_member->>selection(consequence),

	D?state_member->>active(when(ReadOnly == @on,@off,CanSwitchState)),

	D->>fillQuantityDefList,

	%data er in zetten
	Item = D?quantityDefs_member?members<<-find(@arg1?object == Q?definition),
	D?quantityDefs_member->>selection(Item),
	D->>refillQSList,
	QSItem = D?qslist_member?members<<-find(@arg1?object == Q?quantitySpace),
	D?qslist_member->>selection(QSItem),
	D->>onQSSelection(Q?quantitySpace),
	D?remarks_member->>contents(Q?remarks),
	
	%gp3 0.3.16: loop over all set quantityAssumptions and set them, but only if 
	%we show the menu (there should not be any quantity assumptions set when we do not show it, because
	%it is currently only relevant in scenarios)
	%0.4.6: we have to separate the exogenous behaviours from the other ones, different menu's
	
	if
		Behaviours = D<<-member(behaviours) %which also implies we have an exbehaviours member
	then
	(
		ExBehaviours = D<<-member(exbehaviours),
		%we put ExBehaviours to none until proven otherwise
		ExBehaviours->>selection(exogenous_none),
		Q?quantityAssumptions->>for_all(
			if(
				->>(@arg1,prefix,exogenous),       %gp3 0.4.6
				->>(ExBehaviours,selection,@arg1), %single selection
				->>(Behaviours, selected,@arg1,@on)
			)
		),
		Behaviours->>active(ReadOnly?negate),
		ExBehaviours->>active(ReadOnly?negate)
	),

	get(@model, getModelNameForEditor, string('Quantity properties%s - Build',when(ReadOnly == @on,' [Read Only]',''))?value, ModelNameForEditor),
	D->>label(ModelNameForEditor),
	D->>helpId(string('Build_%s_QuantityProperties',D?containerType)),
	
	D?quantityDefs_member->>changeable(ReadOnly?negate),
	D?qslist_member->>changeable(ReadOnly?negate),
	D?ok_member->>active(ReadOnly?negate),
	
	D->>openDialog.
%%

%%
fillQuantityDefList(D):->
	"(re)fill the quantity definitions list" ::

	List = D<<-quantityDefs_member,
	List->>clear,
	Quantities = @model<<-sortedQuantityDefinitions,
	CreateItem *= create(dict_item,
						@arg1,
						@arg1?name,
						@arg1),
	Quantities->>for_all(->>(List,
			    append,
				CreateItem)),
	CreateItem->>done,
	Quantities->>done.
%%	

%%
onQuantityDefSelection(D):->
	"Quantity def selected" ::
	
	D->>refillQSList. %houdt rekening met thans geselecteerde qs, als mogelijk
%%

%%
refillQSList(D):->
	"Refill list of allowed quantity spaces" ::
	%waarbij we proberen de laatst geselecteerde weer te selecteren

	List = D<<-qslist_member,
	%kunnen we hervullen
	CurrentQD = D<<-selectedQuantityDef,

	List->>clear,
	
	if
		CurrentQD \== @nil
	then 
		CurrentQD?allowedQuantitySpaces->>for_all(
				->>(List,append,
						create(
							dict_item,
							@arg1,
							@arg1?name,
							@arg1))),
	%terugvinden vd selectie
	if
		List->>member(D?lastQS) 
	then
	(
		List->>selection(D?lastQS),
		D?qsshow_member->>preview(D?lastQS)
	)
	else
	(
		%gp3: make sure to get the first one
		unless
			List?members->>empty
		do
		(
			First = List?members<<-head,
			List->>selection(First), %first one
			D->>lastQS(First?key),
			D?qsshow_member->>preview(First?key)
		)
		else
			D?qsshow_member->>clear
	).
%%

%%
onQSSelection(D,
	QS: quantitySpace):->
	"QS Selected: update preview" ::

	%en sla op wat de laatst geselecteerde QS was
	D->>lastQS(QS),
	D?qsshow_member->>preview(QS).
%%

%%
editDef(D):->
	"The edit definitions button is pressed..." ::
	%dus openen we de quantityDefEditor

	QD = D<<-selectedQuantityDef,
	( 
	    QD == @nil,
	    send(@app, openQuantityDefinitions)
	;
	    send(@app, openQuantityDefinitions, QD)
	).
%%

%%
saveNewElement(D):->
	%nieuwe maken
	
	%gp3 0.3: create a new quantityAssumptions chain
	QuantityAssumptions *= chain, %we allways reset them to zero if behaviours are NOT shown (i.e in a modelfragment)
	if
		Behaviours = D<<-member(behaviours)
	then
	(
		Behaviours?members->>for_all(
			if(@arg1?selected == @on, ->>(QuantityAssumptions,append,@arg1?value))
		),
		%gp3 0.4.6: same for ExBehaviours, allways there when behaviours is there
		%single selection:
		unless
			exogenous_none = D?exbehaviours_member?selection<<-value
		do
			QuantityAssumptions->>append(D?exbehaviours_member?selection?value)
	),
	
	@model->>changeRequest(newQuantity,
		D?modelFragment,
		D?editor,
		D?instance,
		D?instanceRoute,
		D?state_member?selection,
		D?selectedQuantityDef,
		D?selectedQuantitySpace,
		D?remarks_member?contents,
		QuantityAssumptions).
%%

%%
saveChangedElement(D):->
	%bestaande wijzigen
	%gp3 0.3: create a new quantityAssumptions chain
	QuantityAssumptions *= chain, %we allways reset them to zero if behaviours are NOT shown (i.e in a modelfragment)
				%no problem there, because a quantity(-instance) can never be in a scenario and in a modelfragment at the same time
	if
		Behaviours = D<<-member(behaviours)
	then
	(
		Behaviours?members->>for_all(
			if(@arg1?selected == @on, ->>(QuantityAssumptions,append,@arg1?value))
		),
		%gp3 0.4.6: same for ExBehaviours, allways there when behaviours is there
		%single selection:
		unless
			exogenous_none = D?exbehaviours_member?selection<<-value
		do
			QuantityAssumptions->>append(D?exbehaviours_member?selection?value)
	),

	@model->>changeRequest(changeQuantity,
		D?modelFragment,
		D?editor,
		D?element,
		D?state_member?selection,
		D?selectedQuantityDef,
		D?selectedQuantitySpace,
		D?remarks_member?contents,
		QuantityAssumptions).
%%

%%
notChanged(D):->
	%definitieobject hetzelfde?
	CurrentQD = D<<-selectedQuantityDef,
	CurrentQD = D?element<<-definition,

	%QS dezelfde?
	CurrentQS = D<<-selectedQuantitySpace,
	CurrentQS = D?element<<-quantitySpace,

	%conditiestatus hetzelfde?
	if
		D?element->>isCondition
	then
		condition = D?state_member<<-selection
	else
		consequence = D?state_member<<-selection,

	%remarks hetzelfde?
	D?remarks_member?contents->>equal(D?element?remarks),
	
	%gp3 0.3.16: behaviours the same?
	%if behaviours not shown, they dont count (and we reset them to empty when saved)
	
	if
		Behaviours = D<<-member(behaviours)
	then
	(
		OldAssumptions = D?element<<-quantityAssumptions,
		Behaviours?members->>for_all(
			if(@arg1?selected == @on,
				->>(OldAssumptions,member,@arg1?value),
				not(->>(OldAssumptions,member,@arg1?value))
			)
		),
		%gp3 0.4.16: same for exbehaviours
		D?exbehaviours_member?members->>for_all(
			if(
				@arg1?value \== exogenous_none,
				if(
					@arg1?selected == @on,
					->>(OldAssumptions,member,@arg1?value),
					not(->>(OldAssumptions,member,@arg1?value))
				)
			)
		)
	).
%%

%%%%%%%%%mapping op selectie
%%
selectedQuantityDef(D,
	QD: garpQuantityDefinition*
	):<-

	if 
		@nil = D?quantityDefs_member<<-selection
	then
		QD = @nil
	else
		QD = D?quantityDefs_member?selection<<-object.
%%

%%
selectedQuantitySpace(D,
	QS:quantitySpace*
	):<-

	if
		@nil = D?qslist_member<<-selection
	then
		QS = @nil
	else
		QS = D?qslist_member?selection<<-object.
%%

%%%%%%%%%change requestors%%%%%%%%%%%%
%%om te zorgen dat er niet iets wordt geselecteerd dat er niet meer is
%%etc...

%%addQuantityDef
changeApplied_addQuantityDef(D,
	CR : changeRequestor):->
	%we moeten dus de lijst met quantitydefs uitbreiden zonder aan een selectie te komen

	QD = D<<-selectedQuantityDef, %even bewaren
	D->>fillQuantityDefList,

		%gp3: if we opened the editor, we want to select the new definition
	if
		D = CR?editor<<-openedBy
	then
	(
		D?quantityDefs_member->>selection(CR?result),
		D->>refillQSList
	)
	else
		D?quantityDefs_member->>selection(QD).

%%

%%
changeApplied_changeQuantityDef(D,
	_CR : changeRequestor):->
	
	%We moeten de lijst met quantitydefs opnieuw tonen en de selectie doen en opnieuw de daarbij getoonde QS laten zien
	%als dit allemaal nog kan..

	QD = D<<-selectedQuantityDef,

	D->>fillQuantityDefList,

	if 
		QD \== @nil
	then (
		Item = D?quantityDefs_member?members<<-find(@arg1?object == QD), %altijd
		D?quantityDefs_member->>selection(Item),
		D->>refillQSList
		).
%%

%%
changeApplied_deleteQuantityDef(D,
	_CR : changeRequestor):->	
	%bijwerken van de lijst met quantity definities
	
	QD = D<<-selectedQuantityDef,

	D->>fillQuantityDefList,
	List = D<<-quantityDefs_member,

	if 
		QD \== @nil
	then (
		if
			Item = List?members<<-find(@arg1?object == QD) %kan weg zijn
		then
			List->>selection(Item)
		else
			List->>selection(@nil),

		D->>refillQSList
		)
	else
		List->>selection(@nil).
%%

%%
changeApplied_changeQuantity(D,
	CR : changeRequestor):->
	%als wij dezelfde quantity bewerken en we zijn niet de ->editor
	%dan smern we hem...

	\+ CR->>checkEditor(D?editor),
	CR->>checkArgument(1,D?element),
	%nou wegwezen dus maar
	D->>return.
%%

%%
changeApplied_deleteQuantity(D,
	CR: changeRequestor):->
	%idem voor delete, maar editor maakt dan niet uit

	CR->>checkArgument(1,D?element),
	D->>return.
%%

%%
changeApplied_changeQuantitySpace(D,
	_CR : changeRequestor):->
	%Het kan zijn dat dit één van de mogelijke quantity spaces bij de definitie is
	%dus we moeten de lijst opnieuw vullen, en de preview ook opnieuw doen
	%we checken niet, we doen gewoon

	QS = D<<-selectedQuantitySpace,
	D->>refillQSList,
	
	if
		not( QS = @nil )
	then (
		D?qslist_member->>selection(QS), %werkt ook op key
		D?qsshow_member->>preview(QS)
		).
%%

:-pce_end_class.
