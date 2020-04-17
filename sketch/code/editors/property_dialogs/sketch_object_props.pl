/*
definitie sketchObjectPropsDlg klasse.
De standaard eigenschappen dialoog van sketchObjects.
Note: internally, they are called sketchObject, 
but on screen, these are called Sketch Concept
*/

:-pce_begin_class(sketchObjectPropsDlg,
		  propertyDialog,
		  "Standard sketchObject properties dialog"
		 ).

variable(type,{undefined,entity,agent,assumption},both).
variable(prevName,name,both). %voor de standaardnaam
%%
initialise(D, F: frame):->
	"Initialise the properties dialog" ::

	D->+initialise('Sketch Concept properties - Sketch', F, fake),
	D->>prevName(''),
	
	GapX = D<<-gapX,
	GapY = D<<-gapY,

	%de onderdelen
	
	Name *= eventTextItem(name), 
	Name->>label('Name:'),
	D->>display(Name,point(GapX,D?topY)),

	Name->>keyboard_focus(@on),


	Type *= menu(type,marked),
	Type->>label('Type'),
	% Type->>show_label(@off),
	Type->>layout(vertical),
	Type->>append(menu_item(entity, label:= 'Entity')),
	Type->>append(menu_item(agent, label:= 'Agent')),
	Type->>append(menu_item(assumption, label:= 'Assumption')),
	Type->>append(menu_item(undefined, label:= 'Undefined')),
        Type->>message(->>(D, onTypeChange)), 

	D->>display(Type,point(GapX,Name?bottom_side + GapY)),

	% MiddenBreedte *= number(Type?width),
	% D->>display(Type,point(Arg1?right_side + GapX +
	% 							((MiddenBreedte / 2) -
	%							(Type?width / 2)), Arg1?top_side)),
	
	Remarks *= editor(height := 5, width := 60),
	Remarks->>name(remarks),
	Remarks->>label('Remarks:'),
	Remarks->>show_label(@on),
	Remarks->>fill_mode(@on),
	Remarks->>font(Name?font),
	% D->>display(Remarks,point(GapX,Name?bottom_side + GapY)),
	D->>display(Remarks,point(GapX,Type?bottom_side + GapY)),
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
	get(@model, getModelNameForEditor, 'Sketch Concept properties - Sketch', ModelNameForEditor),
	new(Garp3EditorFrame, garp3EditorFrame(ModelNameForEditor)),
	get(@model, '_value', Model),
	send(Garp3EditorFrame, associateModel, Model),
	send(Garp3EditorFrame, append, D),
	send(Garp3EditorFrame, transient_for, F).
%%


onTypeChange(D):->
        get(D?type_member, selection, Type), 
        D->>slot(type, Type).


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
newObject(D, Type:{undefined,entity,agent,assumption}):->
	"Open the dialog for a new sketchObject" ::

	D->>type(Type),
        D?type_member->>selection(Type), 

	D->>element(@nil),
	get(@model, getModelNameForEditor, 'Add a new Sketch Concept - Sketch', ModelNameForEditor),
	D->>label(ModelNameForEditor),
	D->>helpId('Build_MF_AddNewSketchObject'), %gp3 0.3.13
	D->>openDialog.
%%
	
	  
%%
editObject(D,
	    SketchObject : sketchObjectElement,   % AB, why is this a sketchObjectElement, while newObject creates a sketchObject?
	    ReadOnly : bool
	  ):->
	"Open the dialog for editing an existing sketchObject"::

	D->>element(SketchObject),        
	get(SketchObject, type, Type), 
	D->>type(Type),
        D?type_member->>selection(Type), 
   
	get(@model, getModelNameForEditor, string('Sketch Concept properties%s - Sketch',when(ReadOnly == @on,' [Read Only]',''))?value, ModelNameForEditor),
	D->>label(ModelNameForEditor),
	D->>helpId('Build_MF_SketchObjectProperties'), %gp3 0.3.13
	
	D?name_member->>selection(SketchObject?name),
	D?name_member->>editable(ReadOnly?negate),
	
	D?remarks_member->>contents(SketchObject?remarks),
	D?ok_member->>active(ReadOnly?negate),

	D->>openDialog.	
%%


%%
onSketchObjectSelection(D):->
	%check of we nog met defaultnamen werken
	if
	(
		D?prevName->>equal(D?sketchObjectName)
	;
		0 = D?sketchObjectName<<-size
	)
	then
	(
		% D->>prevName(D?sketchObject?name),
		% D?name_member->>selection(D?sketchObject?name)
		D->>prevName(D?entity?name),
		D?type_member->>selection(D?entity?type),
		D?name_member->>selection(D?entity?name)
	).
%%

%%
notChanged(D):->
	%bij bewerkt element iets gewijzigd?
	
	D?sketchObjectName->>equal(D?element?name),
	D?type->>equal(D?element?type),
	% D?sketchObject->>equal(D?element?sketchObject),
	D?entity->>equal(D?element?entity),
	D?remarks->>equal(D?element?remarks).
%%

%%
saveNewElement(D):->
	@model->>changeRequest(newFSketchObject,
		D?sketch,
		D?editor,
		% D?sketchObject,
		D?entity,
		D?sketchObjectName,
		D?type,
		D?remarks).
%%

%%
saveChangedElement(D):->
	@model->>changeRequest(changeFSketchObject,
		D?sketch,
		D?editor,
		D?element,
		% D?sketchObject,
		D?entity,
		D?sketchObjectName,
		D?type,
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
	"Return top level sketchObject = sketchObject" ::
	E = @model<<-hypered(topSketchObject).

%%

%%
sketchObject(D,
       E : 'abstractEntity*'
      ):<-
	"Return the currently selected sketchObject or @nil" ::

	SketchObjects = D<<-member(sketchObjects),
	S = SketchObjects<<-selection,
	if
		S = @nil
	then
		E = @nil
	else
		E = S<<-object.
%%

%%
sketchObjectName(D,
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
fillSketchObjectList(D):->
	"Internal helper: fill the picklist" ::

	%we bewaren de selectie, als mogelijk

	List = D<<-member(sketchObjects),
	if
		(\+ @nil = List<<-selection)
	then
		OudeSelectie = List?selection<<-object
	else
			OudeSelectie = @nil,
		
	List->>clear,
	Top = @model<<-hypered(topSketchObject), 
	List->>label('Sketch Objects:'),
	
	%en vullen via helper
	D->>recursiveFillSketchObjectList(Top,List,0,0),

	%selectie herstellen
	if
		Item = List?members<<-find( @arg1?object == OudeSelectie)
	then
		List->>selection(Item)
	else
	(
		List->>selection(List?members?head),
		D->>onSketchObjectSelection
	).
%
recursiveFillSketchObjectList(D,
			AE : abstractEntity,
			List : list_browser,
			Level : int,
			Key : number
		       ):->
	"Internal helper for fillSketchObjectList" ::
	%we voegen de opgegeven abstr. entity toe aan de lijst
	%en gaan door voor de kinderen

	S *= string,
	S->>insert_character('-',0,Level), %geeft level aan
	S->>append(AE?name),

	%de naam van de sketchObject is niet uniek omdat hij door multiple
	%inheritance vaker in de lijst kan komen
	%we sturen daarom een uniek nummer, waarvoor we de pce klasse
	%number gebruiken en niet een prolog getal, zodat we geen last
	%hebben van backtracken over het nummer als we uit de recursie komen

	List->>append(dict_item(Key,S,AE)),
	%recursie:
	Children = AE<<-children,
	Children->>sort(?(@arg1?name,compare,@arg2?name)),
	Children->>for_all(and(->>(Key,plus,1),
				  ->>(D, recursiveFillSketchObjectList,
				    @arg1,
				      List,
				      Level + 1,
				      Key))).
%%


%%%%%%%%%%%%CHANGE REQUESTORS%%%%%%%%%%%%%%%%%

%%Bij sommige change requestors moeten we de sketchObjectList opnieuw doen

changeApplied_newHObjectChild(D,
			      _CR : changeRequestor
			     ):->
	D->>fillSketchObjectList.

changeApplied_changeHObject(D,
				_CR : changeRequestor
			       ):->
	D->>fillSketchObjectList. %ongeacht het type maar

changeApplied_deleteHObjectTree(D,
				CR : changeRequestor
			       ):->
	%we kunnen niet gewoon hervullen, want de sketchObjects zijn er waarschijnlijk nog
	%dus gooien we ze weg
	
	List = D<<-member(sketchObjects),
	List?members->>for_all(if(
		or(
			(@arg1?object == CR?object),
			->>(@arg1?object, isUp, CR?object)
		),
		->>(List,delete,@arg1)
		)).	
%%

%%Bij wijzigingen van onze instantie sluiten we
changeApplied_changeFSketchObject(D,
	CR: changeRequestor):->

		\+ CR->>checkEditor(D?editor), %niet als deze editor (anders dubbel sluiten)
		CR->>checkArgument(1,D?element),
		D->>return.
%%

%%
changeApplied_deleteFSketchObject(D,
	CR: changeRequestor):->
	
		CR->>checkArgument(1,D?element),
		D->>return.
%%

:-pce_end_class.
