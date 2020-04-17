/*
definitie calculusPropsDlg klasse.
De standaard eigenschappen dialoog van calculi.
*/

:-pce_begin_class(calculusPropsDlg,
		  propertyDialog,
		  "Standard calculus properties dialog"
		 ).

variable(type,{quantity,derivative},both,"Type of the calculus").
variable(arg1,'garpQuantity|calculus',both,"The associated first argument").
variable(arg1Route, chain, both, "Route to the first argument").
variable(arg1QSPoint,valueReference*,both,"QS Point refered to when arg 1 is quanityQSPoint").
variable(arg2,'garpQuantity|calculus',both,"The associated second argument").
variable(arg2Route, chain, both, "Route to the second argument").
variable(arg2QSPoint,valueReference*,both,"QS Point refered to when arg 2 is quanityQSPoint").

variable(switched,bool,both).

%%
initialise(D, F: frame):->
	"Initialise the properties dialog" ::
	%gp3 changed buttons to imgButtons

	D->+initialise('Calculus properties - Build', F,later),
	D->>switched(@off),

	%de onderdelen: lay out weer helemaal uitgeschreven
	GapX = D?gapX,
	GapY = D?gapY,
	MaxX *= number(0), %houden we de maximale X coordinaat in bij zodat we rechts kunnen uitlijnen

	Arg1 *= extendedListBrowser(width:=25, height:=5),
	Arg1->>name(arg1),
	Arg1->>show_label(@off),
	Arg1->>pen(0),
	Arg1?image->>slot(background,D?background),
	D->>display(Arg1,point(GapX, D?topY)),

	%Sign menu en switch knop komen in zeg maar een midden kolom
	%hun gezamelijke breedte doet er dus toe

	Sign *= menu(sign,marked,->>(D,onSign)),
	Sign->>show_label(@off),
	Sign->>layout(vertical),
	get_image(elements,math_plus_i_a,Plus),
	get_image(elements,math_min_i_a,Min),
	get_image(elements,math_mult_i_a,Mult), %FL new jan 2012
	get_image(elements,math_diw_i_a,Div), %FL new jan 2012
	Sign->>append(menu_item(plus, label:= Plus)),
	Sign->>append(menu_item(min, label:= Min)),
	Sign->>append(menu_item(mult, label:= Mult)), %FL new jan 2012
	Sign->>append(menu_item(diw, label:= Div)), %FL new jan 2012

	Switch *= imgButton(switch, tt:='Switch arguments'),

	MiddenBreedte *= number(Sign?width),
	MiddenBreedte->>maximum(Switch?width),

	D->>display(Sign,point(Arg1?right_side + GapX +
								((MiddenBreedte / 2) -
								(Sign?width / 2)), Arg1?top_side)),
	D->>display(Switch,point(Arg1?right_side + GapX +
								((MiddenBreedte / 2) -
								(Switch?width / 2)), Sign?bottom_side + GapY / 2)),

	Arg2 *= extendedListBrowser(width:=25, height:=5),
	Arg2->>name(arg2),
	Arg2->>show_label(@off),
	Arg2->>pen(0),
	Arg2?image->>slot(background,D?background),
	D->>display(Arg2,point(Arg1?right_side + MiddenBreedte + 2 * GapX,Arg1?top_side)),

	MaxX->>maximum(Arg2?right_side),

	%we gaan het nog even mooi vertikaal uitlijnen
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
	Remarks->>right_side(MaxX),

	Ok *= imgButton(ok, img:=save, tt:='Apply changes'),
	D->>display(Ok,point(GapX,Remarks?bottom_side + GapY)),
	Ok->>default_button(@on),

	Cancel *= imgButton(cancel, img:=undo, tt:= 'Cancel changes'),
	D->>display(Cancel,point(MaxX - Cancel?width,Ok?top_side)),
	D->>updateSpacers, %gp3 0.3.13
	D->>assign_accelerators,
	%minimal size:
	D->>minimalSize(size(MaxX,Cancel?bottom_side)), %abs min

	% Multiple model support
	get(@model, getModelNameForEditor, 'Calculus properties - Build', ModelNameForEditor),
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

	D?sign_member->>set(x:= D?sign_member?left_side + Difference?width / 2),
	D?switch_member->>set(x:= D?switch_member?left_side + Difference?width / 2),

	D?remarks_member->>right_side(Arg2?right_side),
	D?remarks_member->>bottom_side(D?remarks_member?bottom_side + Difference?height),
	D?ok_member->>set(y:=D?ok_member?top_side + Difference?height),
	D?cancel_member->>move(point(Arg2?right_side - D?cancel_member?width, D?ok_member?top_side)).
%%

%%
newObject(D,
	Type: {quantity, derivative},
	Sign: {plus,min},
	Arg1: 'garpQuantity|calculus',
	Arg1Route: chain,
	Arg1QSPoint:valueReference*,
	Arg2: 'garpQuantity|calculus',
	Arg2Route: chain,
	Arg2QSPoint:valueReference*
	 ):->
	"Open the dialog for a new calculus" ::

	D->>setArguments(Type,Sign,Arg1,Arg1Route,Arg1QSPoint,Arg2,Arg2Route,Arg2QSPoint),

	D->>element(@nil),
	get(@model, getModelNameForEditor, 'Add a new calculus - Build', ModelNameForEditor),
	D->>label(ModelNameForEditor),
	D->>helpId(string('Build_%s_AddNewCalculation',D?containerType)), %gp3 0.3.13

	% Remove the multiplication and division buttons
	% if either argument is a calculus
	send(D, checkMultDivAllowed, Arg1, Arg2),
	%send(D, multiplicationRestriction), % FL: I think greying out is more in line with garpstyle then removing buttons

	D->>openDialog.
%%

/**
 * Checks whether either argument is a calculus and prevents
 * multiplication and division as options.
 */
multiplicationRestriction(This) :->
    % Multiplication/Division restriction:
    % You cannot change the sign to multiplication or division
    % when one of the arguments is a calculus object
    (
	(
	    get(This?arg1, class_name, calculus)
	;
	    get(This?arg2, class_name, calculus)
	) ->
	get(This, sign_member, SignMenu),
	get(SignMenu, member, mult, MultMenuItem),
	get(SignMenu, member, diw,  DiwMenuItem),
	send(SignMenu, delete, MultMenuItem),
	send(SignMenu, delete, DiwMenuItem)
    ;
	true
    ).


%%
 /**
 * Checks whether either argument, or the outcome! is a calculus and prevents
 * multiplication and division as options.
 */

checkMultDivAllowed(D,
		    Left: 'garpQuantity|calculus',
		    Right: 'garpQuantity|calculus'
		   ):->	"Set mult and diw to active or inactive depending on whether nesting of calculus is attempted" ::
	(
	  (
	    send(Left, instance_of, calculus)
	  ;
	    send(Right, instance_of, calculus)
	  ;
	    send(D, nested_calculus_modelwide)
	  )
	->
	  send(D, disallowMultDiv)
	;
	  send(D, allowMultDiv)
	).
%%

/*
nested_calculus(D):->
	get(D, element, CurrentCalculus),
	get(D, modelFragment, MF),
	get(MF, findElements, calculus, Chain),
	chain_list(Chain, ElementList),
	member(Element, ElementList),
	(
	  get(Element, argument1, CurrentCalculus)
	;
	  get(Element, argument2, CurrentCalculus)
	),
	!.
*/

nested_calculus_modelwide(D):->
	get(D, element, CurrentCalculus),
	get(@model, modelFragmentsOnly, MFChain),
	chain_list(MFChain, MFList),
	member(MF, MFList),
	get(MF, findElements, calculus, EChain),
	chain_list(EChain, ElementList),
	member(Element, ElementList),
	(
	  get(Element, argument1, CurrentCalculus)
	;
	  get(Element, argument2, CurrentCalculus)
	),
	!.




allowMultDiv(D):->
	get(D, graphicals, G),
	get(G, find, @arg1?name == sign, SignMenu),
	get(SignMenu, members, MenuItems),
	get(MenuItems, find, @arg1?value == mult, Mult),
	get(MenuItems, find, @arg1?value == diw, Div),
	send(Mult, active, @on),
	send(Div, active, @on).


disallowMultDiv(D):->
	get(D, graphicals, G),
	get(G, find, @arg1?name == sign, SignMenu),
	get(SignMenu, members, MenuItems),
	get(MenuItems, find, @arg1?value == mult, Mult),
	get(MenuItems, find, @arg1?value == diw, Div),
	send(Mult, active, @off),
	send(Div, active, @off).

%%
editObject(D,
	C : calculus,
	ReadOnly: bool
	 ):->
	"Open dialog for existing calculus" ::

	D->>setArguments(C?type,C?sign,
				C?argument1, C?argument1Route, C?argument1QSPoint,
				C?argument2, C?argument2Route, C?argument2QSPoint),
	D->>element(C),
	D?remarks_member->>contents(C?remarks),

	get(@model, getModelNameForEditor, string('Calculus properties%s - Build',when(ReadOnly == @on,' [Read Only]',''))?value, ModelNameForEditor),
	D->>label(ModelNameForEditor),
	D->>helpId(string('Build_%s_CalculationProperties',D?containerType)), %gp3 0.3.13
	D?ok_member->>active(ReadOnly?negate),
	D?switch_member->>active(ReadOnly?negate),
	D?sign_member->>active(ReadOnly?negate),

	% Remove the multiplication and division buttons
	% if either argument is a calculus
	send(D, checkMultDivAllowed, C?argument1, C?argument2),
	%send(D, multiplicationRestriction), % FL: I think greying out is more in line with garpstyle then removing buttons

	D->>openDialog.
%%

%%
setArguments(D,
	Type: {quantity, derivative},
	Sign: {plus, min, mult, diw}, % mult & diw new FL jan 2012
	Arg1: 'garpQuantity|calculus',
	Arg1Route: chain,
	Arg1QSPoint:valueReference*,
	Arg2: 'garpQuantity|calculus',
	Arg2Route: chain,
	Arg2QSPoint:valueReference*
	):->
	%we zetten de argumenten
	%ook wordt sign gezet

	D->>arg1(Arg1),
	D->>arg1Route(Arg1Route),
	D->>arg1QSPoint(Arg1QSPoint),
	D->>arg2(Arg2),
	D->>arg2Route(Arg2Route),
	D->>arg2QSPoint(Arg2QSPoint),

	D->>type(Type),
	D?sign_member->>selection(Sign),

	%er mag alleen geswitched worden als de calculus een min is
	(
		Sign = plus ; Sign = mult
	->
		D?switch_member->>active(@off)
	;
		D?switch_member->>active(@on)
	),
	send(D,setArgumentsDisplay).

%%

%%
setArgumentsDisplay(D):->
	"Display the descriptions of the 2 arguments" ::

	Type = D<<-type,
	A1 = D<<-arg1,
	if
		D?arg1Route->>empty
	then
		Arg1Imported = ''
	else
		Arg1Imported = ' (imported)',
	A1P = D<<-arg1QSPoint,
	A2 = D<<-arg2,
	if
		D?arg2Route->>empty
	then
		Arg2Imported = ''
	else
		Arg2Imported = ' (imported)',
	A2P = D<<-arg2QSPoint,
	D?arg1_member->>clear,
	D?arg2_member->>clear,
	pl_setArgumentsDisplay_Calculus(D?arg1_member,'',Type,A1,Arg1Imported,A1P),
	pl_setArgumentsDisplay_Calculus(D?arg2_member,'',Type,A2,Arg2Imported,A2P).

%
pl_setArgumentsDisplay_Calculus(List,Prepend,_,Arg,ArgImport,_):-
	Arg->>instance_of(calculus),!,
	List->>append(dict_item(string('%sCALCULUS%s:',Prepend,ArgImport))),
	NewPrepend *= string('%s  ',Prepend),
	A1 = Arg<<-argument1,
	if
		Arg?argument1Route->>empty
	then
		A1I = ''
	else
		A1I = ' (imported)',
	A1P = Arg<<-argument1QSPoint,
	A2 = Arg<<-argument2,
	if
		Arg?argument2Route->>empty
	then
		A2I = ''
	else
		A2I = ' (imported)',
	A2P = Arg<<-argument2QSPoint,
	CType = Arg<<-type,
	pl_setArgumentsDisplay_Calculus(List,NewPrepend,CType,A1,A1I,A1P),
	List->>append(dict_item(string('%s%s',Prepend,Arg?sign?upcase))),
	pl_setArgumentsDisplay_Calculus(List,NewPrepend,CType,A2,A2I,A2P).

pl_setArgumentsDisplay_Calculus(List,Prepend,derivative,Arg,ArgImported,_Point):-!,
	List->>append(dict_item(string('%sInstance: %s%s',Prepend,Arg?garpInstance?name,ArgImported))),
	List->>append(dict_item(string('%sQuantity: %s',Prepend, Arg?name))),
	List->>append(dict_item(string('%sDERIVATIVE',Prepend))).
%
pl_setArgumentsDisplay_Calculus(List,Prepend,quantity,Arg,ArgImported,@nil):-!,
	List->>append(dict_item(string('%sInstance: %s%s',Prepend,Arg?garpInstance?name,ArgImported))),
	List->>append(dict_item(string('%sQuantity: %s', Prepend,Arg?name))),
	List->>append(dict_item(string('%sQUANTITY',Prepend))).
%
pl_setArgumentsDisplay_Calculus(List,Prepend,quantity,Arg,ArgImported,Point):-
	Value = Point<<-valueName,
	List->>append(dict_item(string('%sInstance: %s%s',Prepend,Arg?garpInstance?name,ArgImported))),
	List->>append(dict_item(string('%sQuantity: %s', Prepend,Arg?name))),
	List->>append(dict_item(string('%sQS VALUE: %s',Prepend,Value))).
%%

%%
onSign(D):->
	%sign is gewijzigd, check switch knop
	(
		(
		 get(D?sign_member, selection, min)
		;
		 get(D?sign_member, selection, diw)
		)
	->
		send(D?switch_member, active, @on)
	;
		send(D?switch_member, active, @off)
	).
%%

%%
switch(D):->
	"Switch arguments pressed" ::

	A1 = D<<-arg1,
	A1R = D<<-arg1Route,
	A1P = D<<-arg1QSPoint,
	A1R->>lock_object(@on),
	A1P->>lock_object(@on),
	D->>arg1(D?arg2),
	D->>arg1Route(D?arg2Route),
	D->>arg1QSPoint(D?arg2QSPoint),
	D->>arg2(A1),
	D->>arg2Route(A1R),
	D->>arg2QSPoint(A1P),
	D->>setArgumentsDisplay,
	A1P->>lock_object(@off),
	A1R->>lock_object(@off),
	if
		@on = D<<-switched
	then
		D->>switched(@off)
	else
		D->>switched(@on).
%%

%%
saveNewElement(D):->
	%nieuwe maken
	@model->>changeRequest(newCalculus,
	D?modelFragment,
	D?editor,
	D?sign_member?selection,
	D?type,
	D?arg1,D?arg1Route,D?arg1QSPoint,
	D?arg2,D?arg2Route,D?arg2QSPoint,
	D?remarks_member?contents).
%%

%%
saveChangedElement(D):->
	@model->>changeRequest(changeCalculus,
	D?modelFragment,
	D?editor,
	D?element,
	D?sign_member?selection,
	D?type,
	D?arg1,D?arg1Route,D?arg1QSPoint,
	D?arg2,D?arg2Route,D?arg2QSPoint,
	D?remarks_member?contents).
%%

%%
notChanged(D):->

	%we kunnen de args alleen switchen, dus checken we of ze niet geswitched zijn
	@off = D<<-switched,
	%verder kan het type niet wijzigen. Wel het sign en de remarks
	D?sign_member?selection->>equal(D?element?sign),
	%remarks hetzelfde?
	D?remarks_member?contents->>equal(D?element?remarks).
%%

%%%%%%%%%%%%%%%%%%%%%%CHANGE%%%%%%%%%%%%%%%
%%changeCalculus: als niet door ons maar wel onze calculus, dan stoppen we er mee
changeApplied_changeCalculus(D,
	CR: changeRequestor):->

	\+ CR->>checkEditor(D?editor),
	CR->>checkArgument(1,D?element),
	D->>return.
%%

%%deleteCalculus: precies hetzelfde als changeCalculus (behalve dat de editor nooit uitmaakt)
changeApplied_deleteCalculus(D,
	CR: changeRequestor):->

	CR->>checkArgument(1,D?element),
	D->>return.
%%
:-pce_end_class.
