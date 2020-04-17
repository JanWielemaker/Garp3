/*
definitie qRelPropsDlg klasse.
Abstracte parent van de dialogen voor proportionalities (proportionalityPropsDlg)
en influences influencePropsDlg
*/

:-pce_begin_class(qRelPropsDlg,
		  propertyDialog
		 ).

variable(arg1,garpQuantity,both,"The associated first argument").
variable(arg1Route, chain, both, "Route to the first argument").
variable(arg2,garpQuantity,both,"The associated second argument").
variable(arg2Route, chain, both, "Route to the second argument").

variable(switched,bool,both).

%%
initialise(D, F: frame):->
	"Initialise the properties dialog" ::
	%gp3 0.2 changed buttons to imgButtons
	%gp3 0.3.13 added the helpid fake, that must be set in editObject and newObject

	D->+initialise('Quantity relation properties - Build', F,fake),
	D->>switched(@off),

	%de onderdelen: lay out weer helemaal uitgeschreven
	GapX = D<<-gapX,
	GapY = D<<-gapY,
	MaxX *= number(0), %houden we de maximale X coordinaat in bij zodat we rechts kunnen uitlijnen

	Arg1 *= extendedListBrowser(width:=30, height:=4),
	Arg1->>name(arg1),
	Arg1->>show_label(@off),
	Arg1->>pen(0),
	Arg1?image->>slot(background,D?background),
	D->>display(Arg1,point(GapX, D?topY)),

	%Sign menu en switch knop komen in de "midden kolom"
	%hun gezamelijke breedte doet er dus toe

	Switch *= imgButton(switch, tt:='Switch arguments'),

	PlusImageName = D?quantityRelationType<<-append('_plus'),
	MinImageName = D?quantityRelationType<<-append('_min'),
	get_image(actions,PlusImageName,Plus),
	get_image(actions,MinImageName,Min),


	Sign *= menu(sign,marked),
	Sign->>show_label(@off),
	Sign->>layout(vertical),
	Sign->>append(menu_item(plus, label:= Plus)),
	Sign->>append(menu_item(min, label:= Min)),

	% NEW FL jan 2012: P* and P/ to implement prop_mult (and prop_diw) relations for multiplication and division
	% the prop diw should not be used yes,
	% to do: comment out the prop div
	% (we use the P* for all mult and div P-relations)
	get(D, quantityRelationType, RelType),
	(
	  RelType = prop
	->  % add prop_mult (and prop_diw)
	  atomic_concat(RelType, '_mult', MultImageName), %alternative: _mult_cross (not P* but Px image)
	  atomic_concat(RelType, '_diw', DivImageName), %todo: commentout
	  get_image(actions,MultImageName, Mult),
	  get_image(actions,DivImageName, Div), %todo: commentout
	  Sign->>append(menu_item(mult, label:= Mult)),
	  Sign->>append(menu_item(diw, label:= Div)) %todo: commentout
	;
	  true
	),
	% ENDNEW FL jan 2012


	MiddenBreedte *= number(Sign?width),
	MiddenBreedte->>maximum(Switch?width),

	D->>display(Sign,point(Arg1?right_side + GapX +
								((MiddenBreedte / 2) -
								(Sign?width / 2)), Arg1?top_side)),
	D->>display(Switch,point(Arg1?right_side + GapX +
								((MiddenBreedte / 2) -
								(Switch?width / 2)), Sign?bottom_side + GapY / 2)),

	Arg2 *= extendedListBrowser(width:=30, height:=4),
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

	%remarks komen zo breed als kan

	Remarks *= editor(height := 5, width := 60),
	Remarks->>name(remarks),
	Remarks->>label('Remarks:'),
	Remarks->>show_label(@on),
	Remarks->>fill_mode(@on),
	Remarks->>font(Arg1?font),
	D->>display(Remarks,point(GapX,ArgHoogte + GapY)),
	MaxX->>maximum(Remarks?right_side),

	Ok *= imgButton(ok, img:=save, tt:='Apply changes'),
	D->>display(Ok,point(GapX,Remarks?bottom_side + GapY)),
	Ok->>default_button(@on),
	%Ok->>keyboard_focus(@on),

	Cancel *= imgButton(cancel, img:=undo, tt:= 'Cancel changes'),
	D->>display(Cancel,point(MaxX - Cancel?width,Ok?top_side)),
	D->>updateSpacers, %gp3 0.3.13
	D->>assign_accelerators,
	%minimal size:
	D->>minimalSize(size(MaxX,Cancel?bottom_side)), %abs min

	% Multiple model support
	get(@model, getModelNameForEditor, 'Quantity relation properties - Build', ModelNameForEditor),
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
	D?sign_member->>set(x:= D?sign_member?left_side + Difference?width / 2),

	D?remarks_member->>right_side(Arg2?right_side),
	D?remarks_member->>bottom_side(D?remarks_member?bottom_side + Difference?height),
	D?ok_member->>set(y:=D?ok_member?top_side + Difference?height),
	D?cancel_member->>move(point(Arg2?right_side - D?cancel_member?width, D?ok_member?top_side)).
%%

%%
newObject(D,
	Arg1: garpQuantity,
	Arg1Route: chain,
	Arg2: garpQuantity,
	Arg2Route: chain
	 ):->
	%openen voor nieuwe quantity relatie
	%zet geen label (moet subclass doen)

	D->>setArguments(Arg1,Arg1Route,Arg2,Arg2Route,plus),

	D->>element(@nil),
	D->>openDialog.
%%

%%
editObject(D,
	QR : garpQuantityRelation,
	ReadOnly: bool
	 ):->
	%openen voor bestaande quantity relation
	%type check moet gebeuren bij subclass

	D->>setArguments(QR?argument1, QR?argument1Route,QR?argument2, QR?argument2Route, QR?sign),
	D->>element(QR),
	D?remarks_member->>contents(QR?remarks),

	if
		ReadOnly = @on
	then
	(
	    get(@model, getModelNameForEditor, string('%s [Read Only]',D?label)?value, ModelNameForEditor),
	    D->>label(ModelNameForEditor)
	),

	D?ok_member->>active(ReadOnly?negate),
	D->>openDialog.
%%

%%
setArguments(D,
	Arg1: garpQuantity,
	Arg1Route: chain,
	Arg2: garpQuantity,
	Arg2Route: chain,
	Sign: {plus,min,mult,diw}
	):->
	%we zetten de argumenten en het sign

	D->>arg1(Arg1),
	D->>arg1Route(Arg1Route),
	D->>arg2(Arg2),
	D->>arg2Route(Arg2Route),
	D?sign_member->>selection(Sign),

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
	List1->>append(dict_item(string('Instance: %s%s',D?arg1?garpInstance?name,A1I))),
	List1->>append(dict_item(string('Quantity: %s',D?arg1?name))),

	List2 = D<<-member(arg2),
	List2->>clear,
	if
		D?arg2Route->>empty
	then
		A2I = ''
	else
		A2I = ' (imported)',
	List2->>append(dict_item(string('Instance: %s%s',D?arg2?garpInstance?name,A2I))),
	List2->>append(dict_item(string('Quantity: %s',D?arg2?name))).
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
	@model->>changeRequest(newQuantityRelation,
		D?modelFragment,
		D?editor,
		D?quantityRelationType, %ingevuld door subklassen
		D?arg1,
		D?arg1Route,
		D?arg2,
		D?arg2Route,
		D?sign_member?selection,
		D?remarks_member?contents).
%%

%%
saveChangedElement(D):->
	%bestaande opslaan
	@model->>changeRequest(changeQuantityRelation,
		D?modelFragment,
		D?editor,
		D?element,
		D?element?type, %ongewijzigd
		D?arg1,
		D?arg1Route,
		D?arg2,
		D?arg2Route,
		D?sign_member?selection,
		D?remarks_member?contents).
%%

%%
quantityRelationType(_D,
	_N: name):<-
	%geef het type terug voor nieuwe relaties die met deze dialoog worden gemaakt
	%moet door subklassen overschreven worden

	fail.
%%

%%
notChanged(D):->
	%we kunnen de args alleen switchen, dus checken we of ze niet geswitched zijn
	@off = D<<-switched,
	%verder kunnen alleen het sign en de remarks wijzigen
	PrevSign = D?element<<-sign,
	PrevSign = D?sign_member<<-selection,
	D?remarks_member?contents->>equal(D?element?remarks).
%%

%%%%%%%%%%%%%%%%%%%%%%CHANGE%%%%%%%%%%%%%%%
%%changeQuantityRelation: als niet door ons maar wel onze relatie, dan stoppen we er mee
changeApplied_changeQuantityRelation(D,
	CR: changeRequestor):->

	\+ CR->>checkEditor(D?editor),
	CR->>checkArgument(1,D?element),
	D->>return.
%%

%%deleteQuantityRelation: precies hetzelfde als change (maar niet letten op editor)
changeApplied_deleteQuantityRelation(D,
	CR: changeRequestor):->

	CR->>checkArgument(1,D?element),
	D->>return.
%%
:-pce_end_class.
