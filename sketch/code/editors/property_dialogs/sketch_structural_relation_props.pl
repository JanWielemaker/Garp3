/*
definitie sketchStructuralRelationPropsDlg klasse.
De standaard eigenschappen dialoog van SketchStructuralRelations.

Editor voor het toevoegen van SketchStructuralRelations
Based on configurationDefEditor and configurationPropsDlg
*/


default_defNameSketchStructuralRelation('New Sketch Structural Relation definition').


:-pce_begin_class(sketchStructuralRelationPropsDlg,
		  propertyDialog,
		  "Standard sketchStructuralRelation properties dialog"
		 ).

variable(arg1,sketchObjectElement,both,"The associated first argument").
variable(arg1Route, chain, both, "Route to the first concept").
variable(arg2,sketchObjectElement,both,"The associated second argument").
variable(arg2Route, chain, both, "Route to the second concept").
variable(readOnly,bool,both).
variable(def,sketchStructuralRelationDefinition*,get, 
		"the edited sketchStructuralRelationDefinition"). %@nil betekent: nieuwe

%%
initialise(D,F: frame):->
	"Initialise the properties dialog" ::
	%gp3 0.2 changed buttons to imgButtons
	
	D->+initialise('Sketch Structural Relation properties - Sketch', F,later),
	D->>readOnly(@off),
	
	%de onderdelen: lay out weer helemaal uitgeschreven
	GapX = D<<-gapX,
	GapY = D<<-gapY,
	MaxX *= number(0), %houden we de maximale X coordinaat in bij zodat we rechts kunnen uitlijnen

	MaxX->>maximum(D?right_side), 
	% MaxX->>maximum(State?right_side),

	Arg1 *= extendedListBrowser(width:=25, height:=3),
	Arg1->>name(arg1),
	Arg1->>show_label(@off),
	Arg1->>pen(0),
	Arg1?image->>slot(background,D?background),
	D->>display(Arg1,point(GapX, D?topY)),
	% D->>display(Arg1,point(GapX, point(State?bottom_side + GapY)),

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
	DefList *= extendedListBrowser(width := 20), 
	DefList->>name(defList),
	DefList->>label('Structural relation definitions:'),
	DefList->>select_message(->>(D,
				       onDefSelection,
						@arg1?object)),	

	D->>display(DefList,point(GapX,Switch?bottom_side + GapY)),

	ListBottom *= number(DefList?bottom_side),

	Add *= imgButton(add, tt:='Add sketch structural relation definition'),

	Remove *= imgButton(remove, tt:='Delete selected sketch structural relation definition'),

	%buttons komen rechts van de lijst, en New helemaal boven, Remove onder
	%dus de lijst moet ook hoog genoeg zijn en bovendien moet New niet bij de bovenkant
	%van de lijst maar bij de bovenkant van het window in de lijst (anders bij label)

	ListBottom->>maximum(DefList?top_side + DefList?image?top_side + %=bovenkant vd lijst
					Add?height + Remove?height +
					GapY), %hieruit volgt de onderkant van de lijst (minimaal ruimte van label + buttons)
	DefList->>bottom_side(ListBottom),

	%Add komt dus niet op de top_side van DefList, maar van het lijstgedeelte
	%DefList?image?top_side is de bovenkant van het lijstgedeelte tov de hele lijst
	D->>display(Add,point(DefList?right_side + GapX,
						DefList?top_side + DefList?image?top_side)),
	MaxX->>maximum(Add?right_side),
	
	%en Remove komt keurig onder
	D->>display(Remove,point(Add?left_side,
			ListBottom - Remove?height)),
	MaxX->>maximum(Remove?right_side),	

	%Nog meer van het een en ander moet nog mooi naar rechts
	Add->>set(x:= MaxX - Add?width),
	Remove->>set(x:= MaxX - Remove?width),

	%Een lijntje
	Line1 *= line(GapX,ListBottom + GapY,0,ListBottom + GapY),
	Line1->>name(line1),
	%deze lijn wordt later goed naar rechts uitgelijnd
	Line1->>pen(2),
	Line1->>end_x(MaxX),
	D->>display(Line1),

	% Name 
		      
	Name *= eventTextItem(name), %kan net zo goed text_item zijn
	Name->>label('Name:'),
	% Name->>length(DefList?width),
	% Name->>length(DefList?width - Add?width - GapX),
	D->>display(Name,point(GapX,Line1?bottom_side + GapY)),

	MaxX->>maximum(Name?right_side),

	%remarks komen zo breed als kan, maar dat wordt pas gezet wanneer we MaxX weten
	
	Remarks *= editor(height := 5, width := 60),
	Remarks->>name(remarks),
	Remarks->>label('Remarks:'),
	Remarks->>show_label(@on),
	Remarks->>fill_mode(@on),
	Remarks->>font(DefList?font),
	% D->>display(Remarks,point(GapX,EditDef?bottom_side + GapY)), 
	D->>display(Remarks,point(GapX,Name?bottom_side + GapY)), 
	MaxX->>maximum(Remarks?right_side),


	% Behave as if Add button pressed in ConfigPropsDlg..." ::
	% D->>checkSaveSelection,
	D->>slot(def,@nil), %betekent: nieuwe
	D?defList_member->>selection(@nil),
	D->>fillCurrentDefData,
	D?name_member->>activate(@on),
	D->>caret(D?name_member),
	
	Ok *= imgButton(ok, img:=sketch_save, tt:='Apply changes'),
	D->>display(Ok,point(GapX,Remarks?bottom_side + GapY)),
	Ok->>default_button(@on),
	
	%MaxX is bekend nu
	Cancel *= imgButton(cancel, img:=sketch_undo, tt:= 'Cancel changes'),
	D->>display(Cancel,point(MaxX - Cancel?width,Ok?top_side)),
	DefList->>right_side(Add?left_side - GapX), 
	D->>updateSpacers, %gp3 0.3.13
	D->>assign_accelerators,
	%minimal size:
	D->>minimalSize(size(MaxX,Cancel?bottom_side)),  %abs min

	% Multiple model support
	get(@model, getModelNameForEditor, 'Sketch Structural Relation properties - Sketch', ModelNameForEditor),
	new(Garp3EditorFrame, garp3EditorFrame(ModelNameForEditor)),
	get(@model, '_value', Model),
	send(Garp3EditorFrame, associateModel, Model),
	send(Garp3EditorFrame, append, D),
	send(Garp3EditorFrame, transient_for, F).

%%

/* old version - AB, nov 2006

	D->>readOnly(@off),
	
	%de onderdelen: lay out weer helemaal uitgeschreven
	GapX = D<<-gapX,
	GapY = D<<-gapY,
	MaxX *= number(0), %houden we de maximale X coordinaat in bij zodat we rechts kunnen uitlijnen


	MaxX->>maximum(D?right_side), % is this right? AB, feb 2006
	% MaxX->>maximum(State?right_side),

	Arg1 *= extendedListBrowser(width:=25, height:=3),
	Arg1->>name(arg1),
	Arg1->>show_label(@off),
	Arg1->>pen(0),
	Arg1?image->>slot(background,D?background),
	D->>display(Arg1,point(GapX, D?topY)),
	% D->>display(Arg1,point(GapX, point(State?bottom_side + GapY)),

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
		      
	DefList *= extendedListBrowser(width := 20),
	DefList->>name(defList),
	DefList->>label('Sketch Structural Relation definitions:'),
	DefList->>select_message(->>(D,
				       onDefSelection,
						@arg1?object)),	

	D->>display(DefList,point(GapX,Switch?bottom_side + GapY)),
	MaxX->>maximum(DefList?right_side),

	% Name 
		      
	ListBottom *= number(DefList?bottom_side),
	ListBottom->>maximum(DefList?top_side + DefList?image?top_side + %=bovenkant vd lijst
					% EditDef?height +
					GapY), %hieruit volgt de onderkant van de lijst (minimaal ruimte van label + buttons)
	DefList->>bottom_side(ListBottom),

	%Een lijntje
	Line1 *= line(GapX,ListBottom + GapY,0,ListBottom + GapY),
	Line1->>name(line1),
	%deze lijn wordt later goed naar rechts uitgelijnd
	Line1->>pen(2),
	Line1->>end_x(MaxX),
	D->>display(Line1),


	Name *= eventTextItem(name), %kan net zo goed text_item zijn
	Name->>label('Name:'),
	Name->>length(DefList?width),
	D->>display(Name,point(GapX,Line1?bottom_side + GapY)),

	MaxX->>maximum(Name?right_side),

	% End from sketchStructuralRelation_def_editor.pl

	%remarks komen zo breed als kan, maar dat wordt pas gezet wanneer we MaxX weten
	
	Remarks *= editor(height := 5, width := 40),
	Remarks->>name(remarks),
	Remarks->>label('Remarks:'),
	Remarks->>show_label(@on),
	Remarks->>fill_mode(@on),
	Remarks->>font(DefList?font),
	% D->>display(Remarks,point(GapX,EditDef?bottom_side + GapY)), 
	D->>display(Remarks,point(GapX,Name?bottom_side + GapY)), 
	MaxX->>maximum(Remarks?right_side),

	% Behave as if Add button pressed in ConfigPropsDlg..." ::
	% D->>checkSaveSelection,
	D->>slot(def,@nil), %betekent: nieuwe
	D?defList_member->>selection(@nil),
	D->>fillCurrentDefData,
	D?name_member->>activate(@on),
	D->>caret(D?name_member),
	
	Ok *= imgButton(ok, img:=sketch_save, tt:='Apply changes'),
	D->>display(Ok,point(GapX,Remarks?bottom_side + GapY)),
	Ok->>default_button(@on),
	
	%MaxX is bekend nu
	Cancel *= imgButton(cancel, img:=sketch_undo, tt:= 'Cancel changes'),
	D->>display(Cancel,point(MaxX - Cancel?width,Ok?top_side)),
	D->>updateSpacers, %gp3 0.3.13
	D->>assign_accelerators,
	%minimal size:
	D->>minimalSize(size(MaxX,Cancel?bottom_side)). %abs min
%%
*/




%%
sketch(D,SK: sketch):<-
	%geef de bewerkte sketch terug, dit is altijd de sketch van de editor

	SK=D?editor<<-sketch.
%%



%%
fillCurrentDefData(D):->
	"fill the editor with saved data for current sketchStructuralRelation definition selection" ::
	%clause 1: niets geselecteerd dus in nieuw modus
	
	@nil = D<<-def,!,
	default_defNameSketchStructuralRelation(Name), %zie bovenin deze file
	D?name_member->>selection(Name),
	D?remarks_member->>contents(new(string)).
%
fillCurrentDefData(D):->
	%clause 2: wel een def geselecteerd

	Def = D<<-def,
	D?name_member->>default(Def?name),
	D?remarks_member->>contents(Def?remarks).


%%
add(D):->
	"Add button pressed..." ::

	D->>checkSaveSelection,
	D->>slot(def,@nil), %betekent: nieuwe
	D?defList_member->>selection(@nil),
	D->>fillCurrentDefData,
	D?name_member->>activate(@on),
	D->>caret(D?name_member).
%%

%%
remove(D):->
	"Remove button pressed..." ::

	%CR checkt de gevolgen wel en waarschuwt eventueel
	if
		not(@nil = D<<-def)
	then
		@model->>changeRequest(deleteSketchStructuralRelationDef, 
			@model, 
			D,
			D?def).
%%



%%
newObject(D,
	  Arg1: sketchObjectElement, %Concept argument 1
		Arg1Route: chain,
	  Arg2: sketchObjectElement, %Concept argument 2
		Arg2Route: chain
	 ):->
	"Open the dialog for a new sketchStructuralRelation" ::

	%Als er geen sketchStructuralRelationDefinitions zijn, openen we toch, want vanaf hier kan je naar de def-editor
	D->>arg1(Arg1),
	D->>arg1Route(Arg1Route),
	D->>arg2(Arg2),
	D->>arg2Route(Arg2Route),

	D->>element(@nil),
	get(@model, getModelNameForEditor, 'Add a new Sketch Structural Relation - Sketch', ModelNameForEditor),
	D->>label(ModelNameForEditor),
	D->>helpId('Sketch_AddNewSketchStructuralRelation'), %gp3 0.3.13

	%zetten van de waarden
	D->>displayArguments,

	D->>fillDefList,
	D->>openDialog.
%%

%%
editObject(D,
	C : sketchStructuralRelationElement,
	ReadOnly : bool	 
	 ):->
	"Open dialog for existing sketchStructuralRelation" ::
	D->>arg1(C?argument1),
	D->>arg1Route(C?argument1Route),
	D->>arg2(C?argument2),
	D->>arg2Route(C?argument2Route),
	D->>element(C), 

	D->>readOnly(ReadOnly),
	
	%zetten van de waarden
	D->>displayArguments,

	D->>fillDefList,

	D?defList_member->>selection(C?definition),
	D?remarks_member->>contents(C?remarks),

	D->>slot(def,C?definition),
	D->>fillCurrentDefData,

	get(@model, getModelNameForEditor, string('Sketch Structural Relation properties%s - Sketch',when(ReadOnly == @on,' [Read Only]',''))?value, ModelNameForEditor),
	D->>label(ModelNameForEditor),
	D->>helpId('Sketch_StructuralRelationProperties'), %gp3 0.3.13
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
		S1 = 'Concept: %s'
	else
		S1 = 'Concept: %s (imported)',

	D?arg1_member->>append(
			dict_item(?(string(S1,D?arg1?name),split_lines,D?arg1_member?width))),
	D?arg2_member->>clear,
	if
		D?arg2Route->>empty
	then
		S2 = 'Concept: %s'
	else
		S2 = 'Concept: %s (imported)',

	D?arg2_member->>append(
			dict_item(?(string(S2,D?arg2?name),split_lines,D?arg2_member?width))).



%
onDefSelection(D,
	_Def):->
	%clause 1: we mogen niet door ivm de vorige selectie
	\+ D->>checkSaveSelection,!, %faalt dus terug op oude selectie
	if 
		@nil = D<<-def %we waren in new modus
	then
		D?defList_member->>selection(@nil)
	else
		D?defList_member->>selection(D?def).
%
onDefSelection(D,Def):->
	%clause 2: we mogen door met de nieuwe selectie dus initialiseren we de data
        % selected other Def
	D->>slot(def,Def),
	D->>fillCurrentDefData.
%%


%%
fillDefList(D):->
	"(re)fill the sketchStructuralRelation definitions list" ::

	List = D<<-defList_member,
	List->>clear,
	% Defs = @model<<-sortedSketchStructuralRelationDefinitions,
	Defs = D?sketch<<-sortedSketchStructuralRelationDefinitions,
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

	GapX = D<<-gapX,
	Arg1 = D<<-arg1_member,
	Arg2 = D<<-arg2_member,
	
	Arg1->>pixelWidth(Arg1?pixelWidth + Difference?width / 2),
	Arg2->>pixelWidth(Arg2?pixelWidth + Difference?width / 2),	
	Arg2->>set(x:= Arg2?left_side + Difference?width / 2),
	
	D?switch_member->>set(x:= D?switch_member?left_side + Difference?width / 2),
	
	D?line1_member->>right_side(D?line1_member?right_side + Difference?width),
	D?line1_member->>set(y:= D?line1_member?top_side + Difference?height),
	
	% D?name_member->>right_side(D?name_member?right_side + Difference?width),	
	D?name_member->>right_side(Arg2?right_side),
	D?name_member->>set(y:= D?name_member?top_side + Difference?height),

	D?add_member->>set(x:= D?add_member?left_side + Difference?width),
	D?remove_member->>set(x:= D?add_member?left_side, y:= D?remove_member?top_side + Difference?height),

	D?defList_member->>right_side(D?add_member?left_side - GapX), 
	D?defList_member->>pixelHeight(D?defList_member?pixelHeight + Difference?height),

	% Set the right end point of the line again
	MaxX *= number(0), %houden we de maximale X coordinaat in bij zodat we rechts kunnen uitlijnen
	MaxX->>maximum(Arg2?right_side),
	% MaxX->>maximum(D?name_member?right_side),
	D?line1_member->>end_x(MaxX),
		
	D?remarks_member->>right_side(Arg2?right_side),
	D?remarks_member->>set(y:= D?remarks_member?top_side + Difference?height),
	D?ok_member->>set(y:=D?ok_member?top_side + Difference?height),
	D?cancel_member->>move(point(Arg2?right_side - D?cancel_member?width, D?ok_member?top_side)).
%%


%
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
        % if no definition has been selected, or entered, fail.
        @nil = D<<-selectedDef,
        send(D, notChangedSketchStructuralRelationDef), !, fail. 


%%
saveNewElement(D):->
        % first, create and save a sketchStructuralRelation definition
        D->>saveDef,
        D->>fillCurrentDefData, %opnieuw inlezen
	% then, make a new sketchStructuralRelation
	@model->>changeRequest(newSketchStructuralRelation,
		D?sketch,
		D?editor,
		% D?state_member?selection,
		D?selectedDef,
		D?arg1,D?arg1Route,D?arg2,D?arg2Route,
		D?remarks_member?contents).
%%

%%
saveChangedElement(D):->
        % first, create and save a sketchStructuralRelation definition, if necessary
        D->>saveDef,
        D->>fillCurrentDefData, %opnieuw inlezen
	%bestaande opslaan
	@model->>changeRequest(changeSketchStructuralRelation,
		D?sketch,
		D?editor,
		D?element, % old SketchStructuralRelation?
		% D?state_member?selection,
		% D?element?definition,  % new SketchStructuralRelation definition? OR
		D?selectedDef, % new SketchStructuralRelation?
		% D?def, % new SketchStructuralRelation?
		D?arg1,D?arg1Route,D?arg2,D?arg2Route, %we moeten het meesturen, want
			               		       %het kan gewisseld zijn
		D?remarks_member?contents).
%%


%%
notChangedSketchStructuralRelationDef(D):->
	%slaag als de definitie niet gewijzigd is
	%1: versie voor een nieuwe definitie: alles moet er nog precies
	%zo bijstaan als ingevuld door fillCurrentDefData	
	@nil = D<<-def,!,
	default_defNameSketchStructuralRelation(Name), %zie bovenin deze file
	D?name_member?selection->>equal(Name),
	0 = D?remarks_member?contents<<-size.

	
notChangedSketchStructuralRelationDef(D):->
	%2: Versie voor een bestaande definitie
	%we beschouwen het als gewijzigd als het sowieso anders, is, geen gedoe met makeGarp op dit nivo
	\+ @nil = D<<-def,
        % get(D?def, name, DefName), 
        % get(D?element, name, ElName), 
	D?name_member?selection->>equal(D?def?name),
	D?remarks_member?contents->>equal(D?def?remarks).
%%


%%
notChangedSketchStructuralRelation(D):->
	"Succeeds when the sketchStructuralRelation is not changed by the user" ::
	%we beschouwen het als gewijzigd als het sowieso anders, is, geen gedoe met makeGarp op dit nivo

	%definitieobject hetzelfde?
	D?element \= @nil, % AB, feb 2006
        get(D, element, E),        
        % get(D, def, Def), 
        E \== @nil, 
        % get(E, name, EName), 
        % get(Def, name, DefName), 

	D?name_member?selection->>equal(D?def?name),
	D?selectedDef->>equal(D?element?definition),
	%argumenten hetzelfde (aangezien we alleen kunnen switchen hoeven we alleen arg1 te testen)
	D?arg1->>equal(D?element?argument1),
	%gp3 0.2: also have to check route
	D?arg1Route->>equal(D?element?argument1Route),

	%remarks hetzelfde?
	D?remarks_member?contents->>equal(D?element?remarks).
%%


%%
notChanged(D):->
	"Succeeds when the sketchStructuralRelation is not changed by the user" ::

        % When the SketchStructuralRelationDef, and the SketchStructuralRelation itself have not been changed
	D->>notChangedSketchStructuralRelationDef, 
	D->>notChangedSketchStructuralRelation.




%%%%%%%%%%%%%SAVE EN CANCEL E.D%%%%%%%%%%%%%%%%%%%%%%
%%
checkSaveSelection(D):->
	"Check changed state of current selection and ask for saving, fail if change canceled or save failed" ::
	D->>notChangedSketchStructuralRelationDef,!. %niets aan de hand
%
checkSaveSelection(D):->
	%ok, er is iets gewijzigd, dus moet er misschien opgeslagen worden

	Dlg *= dialog('Confirm changes'),
	Dlg->>application(@app),
	Dlg->>append(text('You made changes to this sketchStructuralRelation definition. Do you want to save them?')),
	SC *= imgButton(save_changes,
			    ->>(Dlg,return,save), img:=sketch_save, tt:='Save changes to model'),
	Dlg->>append(SC),
	CC *= imgButton(cancel_changes,->>(Dlg,return,cancel), img:=sketch_undo, tt:='Cancel changes'),
	Dlg->>append(CC),
	EC *= imgButton(edit_changes,->>(Dlg,return,edit), img:=sketch_edit_changes, tt:='Edit changes'),
	Dlg->>append(EC),
	Dlg->>transient_for(D),
	Dlg->>modal(transient),
	Dlg->>kind(toplevel), %wel een titelbalk
	Answer = Dlg<<-confirm_centered(D?frame?area?center),
	Dlg->>destroy,
	if
		Answer = save
	then
		D->>saveDef
	else 
		Answer = cancel. %falen bij edit
%%



%%
save(D):->
	"Save button" :: 

	D->>saveDef,
	D->>fillCurrentDefData. %opnieuw inlezen
%%

%%
saveDef(D):->
	"Save the changes that were made" ::
	D->>notChangedSketchStructuralRelationDef,!. %niets te doen
%
saveDef(D):->
	%kee, nu moeten we een CR bouwen
	%hangt af of we in nieuw modus zijn of niet
	if
		@nil = D<<-def
	then
                @model->>changeRequest(addSketchStructuralRelationDef,
				@model,
				D,
				D?name_member?selection,
				D?remarks_member?contents)
	else
		@model->>changeRequest(changeSketchStructuralRelationDef,
				D?def,
				D,
				D?name_member?selection,
				D?remarks_member?contents).

%%	


%%%%%%%%%%%HELPERS%%%%%%%%%%%%%%%%%%%%%
%%
selectedDef(D,
	Def : sketchStructuralRelationDefinition*):<-
	"Return selected sketchStructuralRelation definition or @nil" ::

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

 
%
changeApplied_addSketchStructuralRelationDef(D,
	CR : changeRequestor):->
	%hervullen vd lijst en de goede weer selecteren
	% Def = D<<-selectedDef,
	% Def = D<<-def,
	D->>fillDefList,

	% select the new definition
        D?defList_member->>selection(CR?result),
	D->>slot(def,CR?result). % set def slot. AB, march 2006
        % D?defList_member->>selection(Def).
%%


%
changeApplied_changeSketchStructuralRelationDef(D,
	_CR : changeRequestor):->
	
	%Bijwerken en de selectie bewaren etc
	Def = D<<-selectedDef,
	D->>fillDefList,

	% select the right definition
        D?defList_member->>selection(Def),
	D->>slot(def,Def). 
%%


%%
changeApplied_deleteSketchStructuralRelationDef(D,
	CR : changeRequestor
	):->

	(
            % if editors belong to the same model - this does not work, AB nov 2006
	    % send(@app, editorBelongsToSameModel, CR?editor, D)
            % hence this condition (may be too simple!):
            % if editors are the same 
            (CR?editor->>equal(D)) ->
	    /*
	    Het kan dus zijn dat de geselecteerde def weg is. In dat geval kiezen we een andere 
	    */
	    
	    List = D<<-defList_member,
	    Deleted = CR<<-argument(1),
	    if 
		(Deleted = D<<-def)
	    then (
		(
			New = List?nextItem<<-object
		;	New = List?prevItem<<-object
		;	New = @nil
		),

		List->>selection(New),
		D->>slot(def,New),
		D->>fillCurrentDefData
		),
	    List->>delete(Deleted)
	;
	    true
	).
%%


%%
changeApplied_changeSketchStructuralRelation(D,
	CR: changeRequestor):->

        % There is no longer another editor - Ab, feb 2006
	% %dicht als wij dezelfde editen
	% \+ CR->>checkEditor(D?editor), %niet als deze editor (anders dubbel sluiten)
	CR->>checkArgument(1,D?element),
	D->>return.
%%

%%
changeApplied_deleteSketchStructuralRelation(D,
	CR: changeRequestor):->

	%dicht als wij dezelfde editen
	CR->>checkArgument(1,D?element),
	D->>return.
%%


%
:-pce_end_class.
