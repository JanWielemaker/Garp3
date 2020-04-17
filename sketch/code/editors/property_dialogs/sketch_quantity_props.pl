/*
definitie sketchQuantityPropsDlg klasse.
De standaard eigenschappen dialoog van sketchQuantities.
*/

:-pce_begin_class(sketchQuantityPropsDlg,
		  propertyDialog,
		  "Standard sketchQuantity properties dialog"
		 ).

variable(startType,{sketchQuantity},both).
variable(prevName,name,both). %voor de standaardnaam
%%
initialise(D, F: frame):->
	"Initialise the properties dialog" ::

	D->+initialise('Sketch Quantity properties - Sketch', F, fake),
	D->>prevName(''),
	
	GapX = D<<-gapX,
	GapY = D<<-gapY,

	%de onderdelen
	
	Name *= eventTextItem(name), 
	Name->>label('Name:'),
	D->>display(Name,point(GapX,D?topY)),

	Name->>keyboard_focus(@on),
	
	Remarks *= editor(height := 5, width := 60),
	Remarks->>name(remarks),
	Remarks->>label('Remarks:'),
	Remarks->>show_label(@on),
	Remarks->>fill_mode(@on),
	Remarks->>font(Name?font),
	D->>display(Remarks,point(GapX,Name?bottom_side + GapY)),
	Name->>right_side(Remarks?right_side),
	
	Ok *= imgButton(ok, img:=sketch_save, tt:='Apply changes'),
	D->>display(Ok,point(GapX,Remarks?bottom_side + GapY)),
	Ok->>default_button(@on),
	%Ok->>keyboard_focus(@on),

	Cancel *= imgButton(cancel, img:=sketch_undo, tt:= 'Cancel changes'),
	D->>display(Cancel,point(Remarks?right_side - Cancel?width,
		Ok?top_side)),
	D->>updateSpacers, %gp3 0.3.13
	D->>assign_accelerators,
		%minimal size:
	D->>minimalSize(size(Cancel?right_side,Cancel?bottom_side)), %abs min

	% Multiple model support
	get(@model, getModelNameForEditor, 'Sketch Quantity properties - Sketch', ModelNameForEditor),
	new(Garp3EditorFrame, garp3EditorFrame(ModelNameForEditor)),
	get(@model, '_value', Model),
	send(Garp3EditorFrame, associateModel, Model),
	send(Garp3EditorFrame, append, D),
	send(Garp3EditorFrame, transient_for, F).
%%


%%
sketch(D,SK: sketch):<-
	%geef de bewerkte sketch terug, dit is altijd de sketch van de editor

	SK=D?editor<<-sketch.
%%



%%
onResize(D, Difference: size):->
	Remarks = D<<-remarks_member,
	% Remarks is the one that gets higher and wider
	Remarks->>right_side(D?right_side-20),
	D?name_member->>right_side(Remarks?right_side),
	D?ok_member->>set(y:=D?ok_member?top_side + Difference?height),
	D?cancel_member->>move(point(Remarks?right_side - D?cancel_member?width, D?ok_member?top_side)),
        Remarks->>bottom_side(D?ok_member?top_side - 20).
        % the trick with difference does not work because the Remarks editor 
        % has a different coordinate system
%%


%%
newObject(D,
	  StartType : {sketchQuantity} % this variable could be removed. AB, feb 2006
	 ):->
	"Open the dialog for a new sketchQuantity" ::

	D->>startType(StartType),	
	D->>element(@nil),
	
	%gp3 0.2: set the right defbutton
	if
		StartType = sketchQuantity
	then
	(
		get(@model, getModelNameForEditor, 'Add a new sketch Quantity - Sketch', ModelNameForEditor),
		D->>label(ModelNameForEditor),
		D->>helpId('Build_MF_AddNewSketchQuantity') %gp3 0.3.13
	),
	D->>openDialog.
%%
	
	  
%%
editObject(D,
 	    SketchQuantity : sketchQuantityElement,
	    ReadOnly : bool
	  ):->
	"Open the dialog for editing an existing sketchQuantityElement"::

	D->>element(SketchQuantity),

	D->>startType(sketchQuantity),
	get(@model, getModelNameForEditor, string('Sketch Quantity properties%s - Sketch',when(ReadOnly == @on,' [Read Only]',''))?value, ModelNameForEditor),
	D->>label(ModelNameForEditor),
	D->>helpId('Build_MF_SketchQuantityProperties'), %gp3 0.3.13
	
	D?name_member->>selection(SketchQuantity?name),
	D?name_member->>editable(ReadOnly?negate),
	
	D?remarks_member->>contents(SketchQuantity?remarks),
	D?ok_member->>active(ReadOnly?negate),

	D->>openDialog.
%%

%%
onSketchQuantitySelection(D):->
	%check of we nog met defaultnamen werken
	if
	(
		D?prevName->>equal(D?sketchQuantityElementName)
	;
		0 = D?sketchQuantityElementName<<-size
	)
	then
	(
		% D->>prevName(D?sketchQuantityElement?name),
		% D?name_member->>selection(D?sketchQuantityElement?name)
		D->>prevName(D?entity?name),
		D?name_member->>selection(D?entity?name)
	).
%%


%%
editDef(D):->
	%gp3 0.2: open the definition
	E = D<<-sketchQuantity,
	if
		E = @nil
	then
		EN = @default
	else
		EN = E,
	if
		sketchQuantity = D<<-startType
	then
		@app->>openSketchQuantities(E)
	else
		@app->>openAgents(E).
%%
%%
notChanged(D):->
	%bij bewerkt element iets gewijzigd?
	
	D?sketchQuantityName->>equal(D?element?name),
	% D?sketchQuantity->>equal(D?element?sketchQuantityElement),
	D?entity->>equal(D?element?entity),
	D?remarks->>equal(D?element?remarks).
%%

%%
saveNewElement(D):->
	@model->>changeRequest(newSketchQuantity,
		D?sketch,
		D?editor,
		% D?sketchQuantityElement,
		D?entity,
		D?sketchQuantityName,
		D?remarks).
%%

%%
saveChangedElement(D):->
	@model->>changeRequest(changeSketchQuantity,
		D?sketch,
		D?editor,
		D?element,
		% D?sketchQuantity,
		D?entity,
		D?sketchQuantityName,
		D?remarks).
%%


%%
% this whole variable could be removed, AB, feb 2006
state(_D,
      S : {condition,consequence}
     ):<-
	"Return the selected state" ::

	S = consequence.
%%

%%
entity(_D,
       E : 'abstractEntity*'
      ):<-
	"Return top level sketchQuantityElement = sketchQuantityElement" ::
	E = @model<<-hypered(topSketchQuantity).

%%

%%
sketchQuantity(D,
       E : 'abstractEntity*'
      ):<-
	"Return the currently selected sketchQuantity or @nil" ::

	SketchQuantities = D<<-member(sketchQuantities),
	S = SketchQuantities<<-selection,
	if
		S = @nil
	then
		E = @nil
	else
		E = S<<-object.
%%

%%
sketchQuantityName(D,
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
fillSketchQuantityList(D):->
	"Internal helper: fill the picklist" ::

	%we bewaren de selectie, als mogelijk

	List = D<<-member(sketchQuantities),
	if
		(\+ @nil = List<<-selection)
	then
		OudeSelectie = List?selection<<-object
	else
			OudeSelectie = @nil,
		
	List->>clear,
	Top = @model<<-hypered(topSketchQuantity), 
	List->>label('SketchQuantities:'),
	
	%en vullen via helper
	D->>recursiveFillSketchQuantityList(Top,List,0,0),

	%selectie herstellen
	if
		Item = List?members<<-find( @arg1?object == OudeSelectie)
	then
		List->>selection(Item)
	else
	(
		List->>selection(List?members?head),
		D->>onSketchQuantitySelection
	).
%
recursiveFillSketchQuantityList(D,
			AE : abstractEntity,
			List : list_browser,
			Level : int,
			Key : number
		       ):->
	"Internal helper for fillSketchQuantityList" ::
	%we voegen de opgegeven abstr. entity toe aan de lijst
	%en gaan door voor de kinderen

	S *= string,
	S->>insert_character('-',0,Level), %geeft level aan
	S->>append(AE?name),

	%de naam van de sketchQuantity is niet uniek omdat hij door multiple
	%inheritance vaker in de lijst kan komen
	%we sturen daarom een uniek nummer, waarvoor we de pce klasse
	%number gebruiken en niet een prolog getal, zodat we geen last
	%hebben van backtracken over het nummer als we uit de recursie komen

	List->>append(dict_item(Key,S,AE)),
	%recursie:
	Children = AE<<-children,
	Children->>sort(?(@arg1?name,compare,@arg2?name)),
	Children->>for_all(and(->>(Key,plus,1),
				  ->>(D, recursiveFillSketchQuantityList,
				    @arg1,
				      List,
				      Level + 1,
				      Key))).
%%


%%%%%%%%%%%%CHANGE REQUESTORS%%%%%%%%%%%%%%%%%

%%Bij sommige change requestors moeten we de sketchQuantityElementList opnieuw doen

changeApplied_newHObjectChild(D,
			      _CR : changeRequestor
			     ):->
	D->>fillSketchQuantityList.

changeApplied_changeHObject(D,
				_CR : changeRequestor
			       ):->
	D->>fillSketchQuantityList. %ongeacht het type maar

changeApplied_deleteHObjectTree(D,
				CR : changeRequestor
			       ):->
	%we kunnen niet gewoon hervullen, want de sketchQuantities zijn er waarschijnlijk nog
	%dus gooien we ze weg
	
	List = D<<-member(sketchQuantities),
	List?members->>for_all(if(
		or(
			(@arg1?object == CR?object),
			->>(@arg1?object, isUp, CR?object)
		),
		->>(List,delete,@arg1)
		)).	
%%

%%Bij wijzigingen van onze instantie sluiten we
changeApplied_changeSketchQuantity(D,
	CR: changeRequestor):->

		\+ CR->>checkEditor(D?editor), %niet als deze editor (anders dubbel sluiten)
		CR->>checkArgument(1,D?element),
		D->>return.
%%

%%
changeApplied_deleteSketchQuantity(D,
	CR: changeRequestor):->
	
		CR->>checkArgument(1,D?element),
		D->>return.
%%

:-pce_end_class.
