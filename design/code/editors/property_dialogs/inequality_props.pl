/*
definitie inequalityPropsDlg klasse.
De standaard eigenschappen dialoog van inequalities.
*/

:-pce_begin_class(inequalityPropsDlg,
		  propertyDialog,
		  "Standard inequality properties dialog"
		 ).

variable(arg1,'garpQuantity|calculus',both,"The associated first argument").
variable(arg1Route, chain, both, "Route to the first argument").
variable(arg1Type,{currentValue,currentDerivative,quantityQSPoint,derivativeZero, calculus},both,
							"Type of first argument").
variable(arg1QSPoint,valueReference*,both,"QS Point refered to when arg 1 is quanityQSPoint").
variable(arg2,'garpQuantity|calculus',both,"The associated second argument").
variable(arg2Route, chain, both, "Route to the second argument").
variable(arg2Type,{currentValue,currentDerivative,quantityQSPoint,derivativeZero, calculus},
						both,"Type of second argument").
variable(arg2QSPoint,valueReference*,both,"QS Point refered to when arg 2 is quanityQSPoint").

variable(switched,bool,both).

%%
initialise(D, F: frame):->
	"Initialise the properties dialog" ::
	%gp3 changed buttons to imgButtons
	
	D->+initialise('Inequality properties - Build', F,later),
	D->>switched(@off),

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

	Arg1 *= extendedListBrowser(width:=25, height:=4),
	Arg1->>name(arg1),
	Arg1->>show_label(@off),
	Arg1->>pen(0),
	Arg1?image->>slot(background,D?background),
	D->>display(Arg1,point(GapX,State?bottom_side + GapY)),

	%Type menu en switch knop komen in zeg maar een midden kolom
	%hun gezamelijke breedte doet er dus toe

	Type *= menu(type,marked),
	Type->>show_label(@off),
	Type->>layout(vertical),
	get_image(actions,ineq_g,IG),
	Type->>append(menu_item(g,@nil,IG)),
	get_image(actions,ineq_geq,IGE),
	Type->>append(menu_item(geq,@nil,IGE)),
	get_image(actions,ineq_eq,IE),
	Type->>append(menu_item(eq,@nil,IE)),
	get_image(actions,ineq_leq,ILE),
	Type->>append(menu_item(leq,@nil,ILE)),
	get_image(actions,ineq_l,IL),
	Type->>append(menu_item(l,@nil,IL)),

	Switch *= imgButton(switch, tt:='Switch arguments'),

	MiddenBreedte *= number(Type?width),
	MiddenBreedte->>maximum(Switch?width),

	D->>display(Type,point(Arg1?right_side + GapX +
								((MiddenBreedte / 2) -
								(Type?width / 2)), Arg1?top_side)),
	D->>display(Switch,point(Arg1?right_side + GapX + 
								((MiddenBreedte / 2) -
								(Switch?width / 2)), Type?bottom_side + GapY / 2)),

	Arg2 *= extendedListBrowser(width:=25, height:=4),
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
	get(@model, getModelNameForEditor, 'Inequality properties - Build', ModelNameForEditor),
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
	D?type_member->>set(x:= D?type_member?left_side + Difference?width / 2),
	
	D?remarks_member->>right_side(Arg2?right_side),
	D?remarks_member->>bottom_side(D?remarks_member?bottom_side + Difference?height),
	D?ok_member->>set(y:=D?ok_member?top_side + Difference?height),
	D?cancel_member->>move(point(Arg2?right_side - D?cancel_member?width, D?ok_member?top_side)).
%%

%%
newObject(D,
	Arg1: 'garpQuantity|calculus',
	Arg1Route: chain,
	Arg1Type: {currentValue,currentDerivative,quantityQSPoint,derivativeZero, calculus}, 
	Arg1QSPoint:valueReference*,
	Arg2: 'garpQuantity|calculus',
	Arg2Route: chain,
	Arg2Type: {currentValue,currentDerivative,quantityQSPoint,derivativeZero, calculus}, 
	Arg2QSPoint:valueReference*,
	  StartState: {condition,consequence}, %de state waarin de nieuwe gemaakt moet worden...
	CanSwitchState: [bool]):->
	"Open the dialog for a new inequality" ::


	D->>setArguments(Arg1,Arg1Route,Arg1Type,Arg1QSPoint,Arg2,Arg2Route,Arg2Type,Arg2QSPoint,eq),

	D->>element(@nil),
	get(@model, getModelNameForEditor, 'Add a new inequality - Build', ModelNameForEditor),
	D->>label(ModelNameForEditor),
	D->>helpId(string('Build_%s_AddNewInequality',D?containerType)),

	D?state_member->>selection(StartState),
	default(CanSwitchState,@on,CanSwitch),
	D?state_member->>active(CanSwitch),

	D->>openDialog.
%%

%%
editObject(D,
	I : inequality,
	CanSwitchState: bool,
	ReadOnly : bool	 ):->
	"Open dialog for existing inequality" ::

	D->>setArguments(I?argument1, I?argument1Route, I?argument1Type, I?argument1QSPoint,
					 I?argument2, I?argument2Route, I?argument2Type, I?argument2QSPoint,
						I?type),
	D->>element(I), 

	if 
		I->>isCondition
	then
		D?state_member->>selection(condition)
	else
		D?state_member->>selection(consequence),

	D?state_member->>active(when(ReadOnly == @on,@off,CanSwitchState)),

	D?remarks_member->>contents(I?remarks),
	get(@model, getModelNameForEditor, string('Inequality properties%s - Build',when(ReadOnly == @on,' [Read Only]',''))?value, ModelNameForEditor),
	D->>label(ModelNameForEditor),
	D->>helpId(string('Build_%s_InequalityProperties',D?containerType)),
	D?ok_member->>active(ReadOnly?negate),

	D->>openDialog.
%%

%%
setArguments(D,
	Arg1: 'garpQuantity|calculus',
	Arg1Route: chain,
	Arg1Type: {currentValue,currentDerivative,quantityQSPoint,derivativeZero, calculus}, 
	Arg1QSPoint:valueReference*,
	Arg2: 'garpQuantity|calculus',
	Arg2Route: chain,
	Arg2Type: {currentValue,currentDerivative,quantityQSPoint,derivativeZero, calculus}, 
	Arg2QSPoint:valueReference*,
	Type : name
	):->
	%we zetten de argumenten, maar switchen in sommige gevallen daarbij
	%ook wordt type gezet
	%(zonder switched aan te zetten trouwens)
	
	if (
		(Arg1Type == quantityQSPoint; Arg1Type == derivativeZero),
		Arg2Type \== quantityQSPoint, Arg2Type \== derivativeZero
		)
	then ( %switchen!
		D->>arg1(Arg2),
		D->>arg1Route(Arg2Route),
		D->>arg1Type(Arg2Type),
		D->>arg1QSPoint(Arg2QSPoint),
		D->>arg2(Arg1),
		D->>arg2Route(Arg1Route),
		D->>arg2Type(Arg1Type),
		D->>arg2QSPoint(Arg1QSPoint),
		pl_swapType_IPD(Type,RealType)
		)
	else ( %niet switchen
		D->>arg1(Arg1),
		D->>arg1Route(Arg1Route),
		D->>arg1Type(Arg1Type),
		D->>arg1QSPoint(Arg1QSPoint),
		D->>arg2(Arg2),
		D->>arg2Route(Arg2Route),
		D->>arg2Type(Arg2Type),
		D->>arg2QSPoint(Arg2QSPoint),
		RealType = Type
		),

	D?type_member->>selection(RealType), %eventueel geswapt

	%De gebruiker mag niet switchen als de een een afgeleide of quantity is en de ander
	%een value
	if (
		( currentValue = D<<-arg1Type ; currentDerivative = D<<-arg1Type),
		( quantityQSPoint = D<<-arg2Type ; derivativeZero = D<<-arg2Type )
		)
	then
		D?switch_member->>active(@off)
	else
		D?switch_member->>active(@on),

	D->>setArgumentsDisplay.
%
pl_swapType_IPD(l,g).
pl_swapType_IPD(leq,geq).
pl_swapType_IPD(geq,leq).
pl_swapType_IPD(g,l).
pl_swapType_IPD(eq,eq).
%%

%%
setArgumentsDisplay(D):->
	"Display the descriptions of the 2 arguments" ::

	A1 = D<<-arg1,
	if
		D?arg1Route->>empty
	then
		Arg1Imported = ''
	else
		Arg1Imported = ' (imported)',
	A1T = D<<-arg1Type,
	A1P = D<<-arg1QSPoint,
	A2 = D<<-arg2,
	if
		D?arg2Route->>empty
	then
		Arg2Imported = ''
	else
		Arg2Imported = ' (imported)',
	A2T = D<<-arg2Type,
	A2P = D<<-arg2QSPoint,
	D?arg1_member->>clear,
	D?arg2_member->>clear,
	pl_setArgumentsDisplay_INP(D?arg1_member,'',A1,Arg1Imported,A1T,A1P),
	pl_setArgumentsDisplay_INP(D?arg2_member,'',A2,Arg2Imported,A2T,A2P).

%
pl_setArgumentsDisplay_INP(List,Prepend,Arg,ArgImported,currentValue,_Point):-
	%als de quantity geïmporteerd is, is de instantie dat ook, dus daar zetten we het bij
	List->>append(dict_item(string('%sInstance: %s%s',Prepend,Arg?garpInstance?name,ArgImported))),
	List->>append(dict_item(string('%sQuantity: %s',Prepend,Arg?name))),
	List->>append(dict_item(string('%sVALUE',Prepend))).
%
pl_setArgumentsDisplay_INP(List,Prepend,Arg,ArgImported,currentDerivative,_Point):-
	List->>append(dict_item(string('%sInstance: %s%s',Prepend,Arg?garpInstance?name,ArgImported))),
	List->>append(dict_item(string('%sQuantity: %s', Prepend,Arg?name))),
	List->>append(dict_item(string('%sDERIVATIVE',Prepend))).
%
pl_setArgumentsDisplay_INP(List,Prepend,Arg,ArgImported,quantityQSPoint,Point):-
	if
		Point = @nil
	then
		Value = '(unknown)'
	else
		Value = Point<<-valueName,
	List->>append(dict_item(string('%sInstance: %s%s',Prepend,Arg?garpInstance?name,ArgImported))),
	List->>append(dict_item(string('%sQuantity: %s',Prepend, Arg?name))),
	List->>append(dict_item(string('%sQS VALUE: %s',Prepend,Value))).
%
pl_setArgumentsDisplay_INP(List,Prepend,_Arg,_ArgImported, derivativeZero,_Point):-
	List->>append(dict_item(string('%sZERO',Prepend))).
%
pl_setArgumentsDisplay_INP(List,Prepend,Arg,ArgImported,calculus,_):-
	List->>append(dict_item(string('%sCALCULUS%s:',Prepend,ArgImported))),
	NewPrepend *= string('%s  ',Prepend),
	A1 = Arg<<-argument1,
	if
		Arg?argument1Route->>empty
	then
		A1Imp = ''
	else
		A1Imp = ' (imported)',
	A1T = A1<<-class_name,
	A1P = Arg<<-argument1QSPoint,
	A2 = Arg<<-argument2,
	if
		Arg?argument2Route->>empty
	then
		A2Imp = ''
	else
		A2Imp = ' (imported)',
	A2T = A2<<-class_name,
	A2P = Arg<<-argument2QSPoint,
	CType = Arg<<-type,
	pl_setArgumentsDisplayCalc(List,NewPrepend,CType,A1,A1Imp,A1T,A1P),
	List->>append(dict_item(string('%s%s',Prepend,Arg?sign?upcase))),
	pl_setArgumentsDisplayCalc(List,NewPrepend,CType,A2,A2Imp,A2T,A2P).
%
pl_setArgumentsDisplayCalc(List,Prepend,quantity,Arg,ArgImported, garpQuantity,@nil):-!,
	pl_setArgumentsDisplay_INP(List,Prepend,Arg,ArgImported, currentValue,_).
%
pl_setArgumentsDisplayCalc(List,Prepend,quantity,Arg,ArgImported,garpQuantity,Point):-
	pl_setArgumentsDisplay_INP(List,Prepend,Arg,ArgImported,quantityQSPoint,Point).
%
pl_setArgumentsDisplayCalc(List,Prepend,derivative,Arg,ArgImported,garpQuantity,@nil):-
	pl_setArgumentsDisplay_INP(List,Prepend,Arg,ArgImported,currentDerivative,_).
%
pl_setArgumentsDisplayCalc(List,Prepend,_,Arg,ArgImported,calculus,@nil):-
	pl_setArgumentsDisplay_INP(List,Prepend,Arg,ArgImported,calculus,_).
%%	

%%
switch(D):->
	"Switch arguments pressed" ::

	A1 = D<<-arg1,
	A1R = D<<-arg1Route,
	A1T = D<<-arg1Type,
	A1P = D<<-arg1QSPoint,
	A1R->>lock_object(@on),
	A1P->>lock_object(@on),
	D->>arg1(D?arg2),
	D->>arg1Route(D?arg2Route),
	D->>arg1Type(D?arg2Type),
	D->>arg1QSPoint(D?arg2QSPoint),
	D->>arg2(A1),
	D->>arg2Route(A1R),
	D->>arg2Type(A1T),
	D->>arg2QSPoint(A1P),
	D->>setArgumentsDisplay,
	A1R->>lock_object(@off),
	A1P->>lock_object(@off),
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
	@model->>changeRequest(newInequality,
		D?modelFragment,
		D?editor,
		D?state_member?selection,
		D?type_member?selection,
		D?arg1,D?arg1Route,D?arg1Type,D?arg1QSPoint,
		D?arg2,D?arg2Route,D?arg2Type,D?arg2QSPoint,
		D?remarks_member?contents).
%%

%%
saveChangedElement(D):->
	%bestaande opslaan
	@model->>changeRequest(changeInequality,
		D?modelFragment,
		D?editor,
		D?element,
		D?state_member?selection,
		D?type_member?selection,
		D?arg1,D?arg1Route,D?arg1Type,D?arg1QSPoint,
		D?arg2,D?arg2Route,D?arg2Type,D?arg2QSPoint,
		D?remarks_member?contents).
%%

%%
notChanged(D):->
	%we kunnen de args alleen switchen, dus checken we of ze niet geswitched zijn
	@off = D<<-switched,
	%conditiestatus hetzelfde?
	if
		D?element->>isCondition
	then
		condition = D?state_member<<-selection
	else
		consequence = D?state_member<<-selection,
	%type hetzelfde
	D?type_member?selection->>equal(D?element?type),
	%remarks hetzelfde?
	D?remarks_member?contents->>equal(D?element?remarks).
%%

%%
%%
changeApplied_changeInequality(D,
	CR: changeRequestor):->

	%dicht als wij dezelfde editen
	\+ CR->>checkEditor(D?editor), %niet als deze editor (anders dubbel sluiten)
	CR->>checkArgument(1,D?element),
	D->>return.
%%

%%
changeApplied_deleteInequality(D,
	CR: changeRequestor):->

	%dicht als wij dezelfde editen
	CR->>checkArgument(1,D?element),
	D->>return.
%%

:-pce_end_class.
