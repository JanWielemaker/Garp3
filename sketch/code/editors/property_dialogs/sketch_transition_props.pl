/*
Definition of class sketchTransitionPropsDlg.
Based on defition of class qRelPropsDlg
*/

:-pce_begin_class(sketchTransitionPropsDlg,
		  propertyDialog
		 ).

variable(arg1,sketchStateElement,both,"The associated first argument").
variable(arg1Route, chain, both, "Route to the first argument").
variable(arg2,sketchStateElement,both,"The associated second argument").
variable(arg2Route, chain, both, "Route to the second argument").

variable(switched,bool,both).



%%
initialise(D, F: frame):->
	"Initialise the properties dialog" ::
	%gp3 0.2 changed buttons to imgButtons
	%gp3 0.3.13 added the helpid fake, that must be set in editObject and newObject
	
	D->+initialise('Transition properties - Sketch', F,fake),
	D->>switched(@off),

	%de onderdelen: lay out weer helemaal uitgeschreven
	GapX = D<<-gapX,
	GapY = D<<-gapY,
	MaxX *= number(0), %houden we de maximale X coordinaat in bij zodat we rechts kunnen uitlijnen

	Arg1 *= extendedListBrowser(width:=30, height:=1), % height was 4
        % make ListBrowser inactive, i.e., don't respond to user selection
        Arg1->>select_message(message(Arg1, selection, @nil)),
	Arg1->>name(arg1),
	Arg1->>show_label(@off),
	Arg1->>pen(0),
	Arg1?image->>slot(background,D?background),
	D->>display(Arg1,point(GapX, D?topY)),

	% switch knop komt in de "midden kolom"

	Switch *= imgButton(switch, tt:='Switch arguments'),

	MiddenBreedte *= number(Switch?width),
	MiddenBreedte->>maximum(Switch?width),

	D->>display(Switch,point(Arg1?right_side + GapX + 
								((MiddenBreedte / 2) -
								(Switch?width / 2)), Arg1?bottom_side + GapY / 2)),

	Arg2 *= extendedListBrowser(width:=30, height:=1),
        % make ListBrowser inactive, i.e., don't respond to user selection
        Arg2->>select_message(message(Arg2, selection, @nil)),
	Arg2->>name(arg2),
	Arg2->>show_label(@off),
	Arg2->>pen(0),
	Arg2?image->>slot(background,D?background),
	D->>display(Arg2,point(Arg1?right_side + MiddenBreedte + 2 * GapX,Arg1?top_side)),

	MaxX->>maximum(Arg2?right_side),

	%hiervoor nemen we de maximale hoogte
	ArgHoogte *= number(Arg1?bottom_side),
	ArgHoogte->>maximum(Switch?bottom_side),
	Arg1->>bottom_side(ArgHoogte),
	Arg2->>bottom_side(ArgHoogte),

		
	Ok *= imgButton(ok, img:=sketch_save, tt:='Apply changes'),
	D->>display(Ok,point(GapX,Switch?bottom_side + GapY)),
	Ok->>default_button(@on),
	Ok->>keyboard_focus(@on),
	
	Cancel *= imgButton(cancel, img:=sketch_undo, tt:= 'Cancel changes'),
	D->>display(Cancel,point(MaxX - Cancel?width,Ok?top_side)),
	D->>updateSpacers, %gp3 0.3.13
	D->>assign_accelerators,
	%minimal size:
	D->>minimalSize(size(MaxX,Cancel?bottom_side)), %abs min
	
	% Multiple model support
	get(@model, getModelNameForEditor, 'Transition properties - Sketch', ModelNameForEditor),
	new(Garp3EditorFrame, garp3EditorFrame(ModelNameForEditor)),
	get(@model, '_value', Model),
	send(Garp3EditorFrame, associateModel, Model),
	send(Garp3EditorFrame, append, D),
	send(Garp3EditorFrame, transient_for, F).

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
	
	% D?remarks_member->>right_side(Arg2?right_side),
	% D?remarks_member->>bottom_side(D?remarks_member?bottom_side + Difference?height),
	D?ok_member->>set(y:=D?ok_member?top_side + Difference?height),
	D?cancel_member->>move(point(Arg2?right_side - D?cancel_member?width, D?ok_member?top_side)).
%%



%%
sketch(D,SK: sketch):<-
	%return the edited sketch; this is always the sketch of the editor
	SK=D?editor<<-sketch.
%%



%%
newObject(D,
	Arg1: sketchStateElement,
	Arg1Route: chain,
	Arg2: sketchStateElement,
	Arg2Route: chain
	 ):->
	%openen voor nieuwe sketch Transition
	%zet geen label (moet subclass doen)

	%open dialoog voor nieuwe sketch transition
	get(@model, getModelNameForEditor, 'Add a new transition - Sketch', ModelNameForEditor),
	D->>label(ModelNameForEditor),
	D->>helpId('Sketch_TransitionProps'), %gp3 0.3.13

	D->>setArguments(Arg1,Arg1Route,Arg2,Arg2Route),

	D->>element(@nil),
	D->>openDialog.



%%

%%
editObject(D,
	QR : sketchTransitionElement,
	ReadOnly: bool
	 ):->
	%openen voor bestaande state relation
	%type check moet gebeuren bij subclass

	get(@model, getModelNameForEditor, 'Transition properties - Sketch', ModelNameForEditor),
	D->>label(ModelNameForEditor),
	D->>helpId('Sketch_TransitionProps'), %gp3 0.3.13

	D->>setArguments(QR?argument1, QR?argument1Route,QR?argument2, QR?argument2Route),
	D->>element(QR), 
	% D?remarks_member->>contents(QR?remarks),
	
	if
		ReadOnly = @on
	then
		D->>label(string('%s [Read Only]',D?label)),
	
	D?ok_member->>active(ReadOnly?negate),
	D->>openDialog.


%%

%%
setArguments(D,
	Arg1: sketchStateElement,
	Arg1Route: chain,
	Arg2: sketchStateElement,
	Arg2Route: chain
	):->
	%we zetten de argumenten 
	
	D->>arg1(Arg1),
	D->>arg1Route(Arg1Route),
	D->>arg2(Arg2),
	D->>arg2Route(Arg2Route),

	D->>setArgumentsDisplay.

%%

%%
setArgumentsDisplay(D):->
	"Display the descriptions of the 2 arguments" ::
	List1 = D<<-member(arg1),
	List1->>clear,
	if
		D?arg1Route->>empty
	then
		A1I = ''
	else
		A1I = ' (imported)',
	List1->>append(dict_item(string('SketchState: %s', D?arg1?name))),	

	List2 = D<<-member(arg2),
	List2->>clear,
	if
		D?arg2Route->>empty
	then
		A2I = ''
	else
		A2I = ' (imported)',
	List2->>append(dict_item(string('SketchState: %s',D?arg2?name))).
%%	

%%
switch(D):->
	"Switch arguments pressed" ::
	
	A1 = D<<-arg1,
	A1R = D<<-arg1Route,
	A1R->>lock_object(@on),
	D->>arg1(D?arg2),
	D->>arg1Route(D?arg2Route),
	D->>arg2(A1),
	D->>arg2Route(A1R),
	D->>setArgumentsDisplay,
	A1R->>lock_object(@on),
	D->>switched(D?switched?negate).
%%

%%
saveNewElement(D):->
	%nieuwe maken
	@model->>changeRequest(newSketchTransition,
		D?sketch,
		D?editor,
		D?arg1,
		D?arg1Route,
		D?arg2,
		D?arg2Route,
		'').
		% D?remarks_member?contents).
%%

%%
saveChangedElement(D):->
	%bestaande opslaan
	@model->>changeRequest(changeSketchTransition,
		D?sketch,
		D?editor,
		D?element,
		D?arg1,
		D?arg1Route,
		D?arg2,
		D?arg2Route,
		'').
		% D?remarks_member?contents).
%%


%%
notChanged(D):->
	%we kunnen de args alleen switchen, dus checken we of ze niet geswitched zijn
	@off = D<<-switched.
	%verder kunnen alleen de remarks wijzigen
	% D?remarks_member?contents->>equal(D?element?remarks).
%%

%%%%%%%%%%%%%%%%%%%%%%CHANGE%%%%%%%%%%%%%%%
%%changeSketchTransition: als niet door ons maar wel onze relatie, dan stoppen we er mee
changeApplied_changeSketchTransition(D,
	CR: changeRequestor):->
	\+ CR->>checkEditor(D?editor),
	CR->>checkArgument(1,D?element),
	D->>return.
%%

%%deleteSketchTransition: precies hetzelfde als change (maar niet letten op editor)
changeApplied_deleteSketchTransition(D,
	CR: changeRequestor):->
	CR->>checkArgument(1,D?element),
	D->>return.
%%
:-pce_end_class.
