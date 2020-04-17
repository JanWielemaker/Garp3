/*
definitie assumptionPropsDlg klasse.
De standaard eigenschappen dialoog van assumptions.

GARP3
This is homer code with garp3 changes (where gp3 mentioned)
*/

:-pce_begin_class(assumptionPropsDlg,
		  propertyDialog,
		  "Standard assumption properties dialog"
		 ).
		 
variable(instance,garpInstance*,both). %gp3: assumption may be connected to an instance
variable(instanceRoute, chain*, both). %gp3, even in a route
		 %%
initialise(D, F: frame):->
	"Initialise the properties dialog" ::
	%gp3 changed buttons to imgButtons

	D->+initialise('Assumption properties - Build', F,later),
	
	GapX = D<<-gapX,
	GapY = D<<-gapY,

	Assumptions *= extendedListBrowser(width := 40,
					height := 6),
	Assumptions->>name(assumptions),
	Assumptions->>label('Assumptions:'),
	Assumptions->>show_label(@on),
	D->>display(Assumptions, point(GapX,D?topY)),

	
	Remarks *= editor(height := 5, width := 40),
	Remarks->>name(remarks),
	Remarks->>label('Remarks:'),
	Remarks->>show_label(@on),
	Remarks->>fill_mode(@on),
	Remarks->>font(Assumptions?font),
	D->>display(Remarks,point(GapX,Assumptions?bottom_side + GapY)),
	
	InstanceName *= label(instanceName, 'Instance:'), %gp3
	D->>display(InstanceName, point(GapX, Remarks?bottom_side + GapY)),
	
	Ok *= imgButton(ok, img:=save, tt:= 'Apply changes'),
	D->>display(Ok,point(GapX,InstanceName?bottom_side + GapY)),
	Ok->>default_button(@on),

	%gp3: added definitions
	Defs *= imgButton(edit_assumptions, tt:= 'Open assumption definitions editor'),
	D->>display(Defs, point(GapX + (Remarks?right_side - GapX - Defs?width) / 2, Ok?top_side)),
	
	Cancel *= imgButton(cancel, img:=undo, tt:= 'Cancel changes'),
	D->>display(Cancel,point(Remarks?right_side - Cancel?width,
		Ok?top_side)),
	D->>assign_accelerators,
	D->>fillAssumptionList,
	D->>assign_accelerators,
	D->>updateSpacers, %gp3 0.3.13
	%minimal size:
	D->>minimalSize(size(Cancel?right_side,Cancel?bottom_side)), %abs min
	
	% Multiple model support
	get(@model, getModelNameForEditor, 'Assumption properties - Build', ModelNameForEditor),
	new(Garp3EditorFrame, garp3EditorFrame(ModelNameForEditor)),
	get(@model, '_value', Model),
	send(Garp3EditorFrame, associateModel, Model),
	send(Garp3EditorFrame, append, D),
	send(Garp3EditorFrame, transient_for, F).

	
%%
newObject(D,
	Instance: [garpInstance], %gp3: optional the instance this assumption belongs to
	InstanceRoute: [chain] %gp3, then the route also needed
	):->
	"Open the dialog for a new assumption instance" ::

	get(@model, getModelNameForEditor, 'Add a new assumption - Build', ModelNameForEditor),
	D->>label(ModelNameForEditor),
	D->>helpId(string('Build_%s_AddNewAssumption',D?containerType)),
	D->>element(@nil),
	%gp3: set the instance information
	default(Instance,@nil,I),
	D->>instance(I),
	if
		I == @nil
	then
	(
		D?instanceName_member->>selection(''),
		D->>instanceRoute(@nil)
	)
	else
	(
		default(InstanceRoute,new(chain),IR),
		D->>instanceRoute(IR),
		if
			IR->>empty
		then
			D?instanceName_member->>format('Instance: %s', I?name)
		else
			D?instanceName_member->>format('Instance: %s (imported)', I?name)
	),

	D->>openDialog.
%%
	
	  
%%
editObject(D,
		Assumption : assumptionInstance,
	    ReadOnly : bool
	  ):->
	"Open the dialog for editing an existing assumption instance"::

	%gp3 0.1: changed because the assumption can now be connected (as a subelement) to an instance
	
	D->>element(Assumption),
	get(@model, getModelNameForEditor, string('Assumption properties%s - Build',when(ReadOnly == @on,' [Read Only]',''))?value, ModelNameForEditor),
	D->>label(ModelNameForEditor),
	D->>helpId(string('Build_%s_AssumptionInstanceProperties',D?containerType)),
	D->>instance(Assumption?garpInstance),
	if
		@nil = D<<-instance
	then
	(
		D?instanceName_member->>selection(''),
		D->>instanceRoute(@nil)
	)
	else
	(
		D->>instanceRoute(Assumption?instanceRoute),
		if
			D?instanceRoute->>empty
		then
			D?instanceName_member->>format('Instance: %s', D?instance?name)
		else
			D?instanceName_member->>format('Instance: %s (imported)', D?instance?name)
	),

	Item = D?assumptions_member?members<<-find(@arg1?object == Assumption?assumption),
	D?assumptions_member->>selection(Item),
	D?assumptions_member->>changeable(ReadOnly?negate),
	
	D?remarks_member->>contents(Assumption?remarks),

	D?ok_member->>active(ReadOnly?negate),

	D->>openDialog.	
%%

%%
onResize(D, Difference: size):->
	%gp3 0.2: 
	
	Assumptions = D<<-assumptions_member,
	Assumptions->>pixelWidth(Assumptions?pixelWidth + Difference?width),
	%Assumptions is the one that gets higher
	Assumptions->>pixelHeight(Assumptions?pixelHeight + Difference?height),
	%so the rest moves
	D?remarks_member->>right_side(Assumptions?right_side),
	D?remarks_member->>set(y:=D?remarks_member?top_side + Difference?height),
	D?instanceName_member->>right_side(Assumptions?right_side),
	D?instanceName_member->>set(y:=D?instanceName_member?top_side + Difference?height),
	D?ok_member->>set(y:=D?ok_member?top_side + Difference?height),
	D?edit_assumptions_member->>set(x:= D?edit_assumptions_member?left_side + Difference?width / 2, y:= D?ok_member?top_side),
	D?cancel_member->>move(point(Assumptions?right_side - D?cancel_member?width, D?ok_member?top_side)).
%%

%%
edit_assumptions(D):->
	%gp3 0.2: Open the assumption editor
	
	A = D<<-assumption,
	if
		A = @nil
	then
		AS = @default
	else
		AS = A,
	@app->>openAssumptions(A).
%%

%%
notChanged(D):->
	%bij bewerkt element iets gewijzigd?
	
	%gp3: instance cannot change
	D?assumption->>equal(D?element?assumption),
	D?remarks->>equal(D?element?remarks).
%%

%%
saveNewElement(D):->
	%gp3 0.1 added 2 arguments at the end for instance and instanceroute
	@model->>changeRequest(newAssumptionInstance,
		D?modelFragment,
		D?editor,
		D?assumption,
		D?remarks,
		D?instance,
		D?instanceRoute).
%%

%%
saveChangedElement(D):->
	%gp3: no changes needed here
	@model->>changeRequest(changeAssumptionInstance,
		D?modelFragment,
		D?editor,
		D?element,
		D?assumption,
		D?remarks).
%%

%%
assumption(D,
       A : 'assumption*'
      ):<-
	"Return the currently selected entity or @nil" ::

	S = D?assumptions_member<<-selection,
	if
		S = @nil
	then
		A = @nil
	else
		A = S<<-object.
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
fillAssumptionList(D):->
	"Internal helper: fill the picklist" ::
	%gp3 0.2: top assumption is now also a valid pick
	%(meaning: 'there should be an assumption defined')
	
	List = D<<-member(assumptions),
	if
		(\+ @nil = List<<-selection)
	then
		OudeSelectie = List?selection<<-object
	else
			OudeSelectie = @nil,

	List->>clear,
	Top = @model<<-hypered(topAssumption),
	/*
	gp3 0.2: top itself is topnode so we use the same
	code as instance_props (no explicit unique key for the list
	needed: no multiple inheritance)
	*/
	D->>recursiveFillAssumptionList(Top,List,0,0),
	
	
	%selectie herstellen
	if
		Item = List?members<<-find( @arg1?object == OudeSelectie)
	then
		List->>selection(Item)
	else
	(
		if
			FirstSelection = List?members<<-head
		then
			List->>selection(FirstSelection)
	).
%
recursiveFillAssumptionList(D,
			A: assumption,
			List : list_browser,
			Level : int,
			Key : number
		       ):->
	"Internal helper for fillAssumptionList" ::
	%we voegen de opgegeven assumption toe aan de lijst
	%en gaat door voor de kinderen

	S *= string,
	S->>insert_character('-',0,Level), %geeft level aan
	S->>append(A?name),

	%de naam van de assumption is niet uniek omdat hij door multiple
	%inheritance vaker in de lijst kan komen
	%we sturen daarom een uniek nummer, waarvoor we de pce klasse
	%number gebruiken en niet een prolog getal, zodat we geen last
	%hebben van backtracken over het nummer als we uit de recursie komen

	List->>append(dict_item(Key,S,A)),
	%recursie:
	Children = A<<-children,
	Children->>sort(?(@arg1?name,compare,@arg2?name)),
	Children->>for_all(and(->>(Key,plus,1),
				  ->>(D, recursiveFillAssumptionList,
				    @arg1,
				      List,
				      Level + 1,
				      Key))).
%%

%%%%%%%%%%%%CHANGE REQUESTORS%%%%%%%%%%%%%%%%%
%%Bij sommige change requestors moeten we de assumptionlist opnieuw doen

changeApplied_newHObjectChild(D,
			      _CR : changeRequestor
			     ):->
	D->>fillAssumptionList.

changeApplied_changeHObject(D,
				_CR : changeRequestor
			       ):->
	D->>fillAssumptionList. %ongeacht het type maar
	
changeApplied_deleteHObjectTree(D,
				CR : changeRequestor
			       ):->
	%we kunnen niet gewoon hervullen, want de entities zijn er waarschijnlijk nog
	%dus gooien we ze weg
	
	List = D<<-member(assumptions),
	List?members->>for_all(if(
		or(
			(@arg1?object == CR?object),
			->>(@arg1?object, isUp, CR?object)
		),
		->>(List,delete,@arg1)
		)).	
%%

%%Bij wijzigingen van onze instantie sluiten we
changeApplied_changeAssumptionInstance(D,
	CR: changeRequestor):->

		\+ CR->>checkEditor(D?editor), %niet als deze editor (anders dubbel sluiten)
		CR->>checkArgument(1,D?element),
		D->>return.
%%

%%
changeApplied_deleteAssumptionInstance(D,
	CR: changeRequestor):->
	
		CR->>checkArgument(1,D?element),
		D->>return.
%%

:-pce_end_class.
