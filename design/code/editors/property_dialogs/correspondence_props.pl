/*
definitie correspondencePropsDlg klasse.
De standaard eigenschappen dialoog van correspondences.
*/

:-pce_begin_class(correspondencePropsDlg,
		  propertyDialog,
		  "Standard correspondence properties dialog"
		 ).

variable(derivative,bool,both,"remembers wether the correspondence is between quantity(values) or derivative(values)").
variable(full,bool,both). %gp3 0.3: remember wether the corr is full or not.
variable(arg1,garpQuantity,both,"The associated first argument").
variable(arg1Route, chain, both, "Route to the first argument").
variable(arg1Value,valueReference*,both,"QS Point refered to when editing value correspondence").
variable(arg2,garpQuantity,both,"The associated second argument").
variable(arg2Route, chain, both, "Route to the second argument").
variable(arg2Value,valueReference*,both,"QS Point refered to when editing value correspondence").

variable(switched,bool,both).
variable(readOnly,bool,both).

%%
initialise(D, F: frame):->
	"Initialise the properties dialog" ::
	
	D->+initialise('Correspondence properties - Build', F, later),
	D->>switched(@off),
	D->>readOnly(@off),
	
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

	%Directed menu en switch knop komen in de "midden kolom"
	%hun gezamelijke breedte doet er dus toe

	Switch *= imgButton(switch, tt:='Switch arguments'),
	
	DirectedMenu *= menu(directedMenu,toggle,
			->>(Switch,active,?(@receiver,selected,directed))),
	DirectedMenu->>show_label(@off),
	DirectedMenu->>layout(vertical),
	get_image(actions,directed_label,Directed),
	DirectedMenu->>append(new(DI,menu_item(directed, label:=Directed))),
	DI->>tooltip('Select directed correspondence'),
	
	MirrorMenu *= menu(mirrorMenu,toggle), %2.1
	MirrorMenu->>show_label(@off),
	MirrorMenu->>layout(vertical),
	get_image(actions,inverse_label,Inverse),
	MirrorMenu->>append(new(MI,menu_item(mirror, label:=Inverse))),
	MI->>tooltip('Select inverse correspondence'),
	
	%2.1: mooi uitlijnen verschillende checkboxen
	CheckboxBreedte *= number(DirectedMenu?width),
	CheckboxBreedte->>maximum(MirrorMenu?width),
	
	MiddenBreedte *= number(CheckboxBreedte),
	MiddenBreedte->>maximum(Switch?width),
	
	D->>display(DirectedMenu,point(Arg1?right_side + GapX +
								((MiddenBreedte / 2) -
								(CheckboxBreedte / 2)), Arg1?top_side)),
								
	D->>display(MirrorMenu,point(Arg1?right_side + GapX + 	%2.1
								((MiddenBreedte / 2) -
								(CheckboxBreedte / 2)), DirectedMenu?bottom_side + GapY / 2)),
								
	D->>display(Switch,point(Arg1?right_side + GapX + 
								((MiddenBreedte / 2) -
								(Switch?width / 2)), MirrorMenu?bottom_side + GapY / 2)),

	Arg2 *= extendedListBrowser(width:=30, height:=4),
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
	get(@model, getModelNameForEditor, 'Correspondence properties - Build', ModelNameForEditor),
	new(Garp3EditorFrame, garp3EditorFrame(ModelNameForEditor)),
	get(@model, '_value', Model),
	send(Garp3EditorFrame, associateModel, Model),
	send(Garp3EditorFrame, append, D),
	send(Garp3EditorFrame, transient_for, F).

%%

%%
newObject(D,
	Derivative: bool,
	Full: bool, %gp3 0.3
	Arg1: garpQuantity,
	Arg1Route: chain,
	Arg1Value:valueReference*,
	Arg2: garpQuantity,
	Arg2Route: chain,
	Arg2Value:valueReference*
	 ):->
	%openen voor nieuwe correspondentie

	D->>setArguments(Derivative,Full, Arg1,Arg1Route,Arg1Value,Arg2,Arg2Route,Arg2Value,@off,@off),

	D->>element(@nil),
	get(@model, getModelNameForEditor, 'Add a new correspondence - Build', ModelNameForEditor),
	D->>label(ModelNameForEditor),
	D->>helpId(string('Build_%s_AddNewCorrespondence',D?containerType)), %gp3 0.3.13
	D->>openDialog.
%%

%%
editObject(D,
	C : correspondence,
	ReadOnly: bool
	 ):->
	%openen voor bestande correspondence

	D->>readOnly(ReadOnly),
	D->>setArguments(C?derivative, C?full,	C?argument1, C?argument1Route,C?argument1Value,
				C?argument2, C?argument2Route,C?argument2Value,
				C?directed,C?mirror),
	D->>element(C), 
	D?remarks_member->>contents(C?remarks),

	get(@model, getModelNameForEditor, string('Build - Correspondence properties%s',when(ReadOnly == @on,' [Read Only]',''))?value, ModelNameForEditor),
	D->>label(ModelNameForEditor),
	D->>helpId(string('Build_%s_CorrespondenceProperties',D?containerType)), %gp3 0.3.13
	if
		ReadOnly = @on
	then
	(
		D?directedMenu_member->>active(@off),
		D?mirrorMenu_member->>active(@off),
		D?ok_member->>active(@off)
	),

	D->>openDialog.
%%

%%
setArguments(D,
	Derivative: bool,
	Full: bool,
	Arg1: garpQuantity,
	Arg1Route: chain,
	Arg1Value:valueReference*,
	Arg2: garpQuantity,
	Arg2Route: chain,
	Arg2Value:valueReference*,
	Directed: bool,
	Mirror: bool
	):->
	%we zetten de argumenten en directed
	%gp3 0.3 Also check whether inverse or normal correspondence is possible
	%of just one of them is possible: set the value and make the menu read only
	%Added full possibility. This disables the inverse right away
	
	D->>derivative(Derivative),
	D->>full(Full),
	D->>arg1(Arg1),
	D->>arg1Route(Arg1Route),
	D->>arg1Value(Arg1Value),
	D->>arg2(Arg2),
	D->>arg2Route(Arg2Route),
	D->>arg2Value(Arg2Value),
	
	D?directedMenu_member->>selected(directed,Directed),
	D?switch_member->>active(when(D?readOnly == @on,@off,Directed)), %alleen switchen bij directed correspondence
	
	
	%make sure the mirrorMenu is set to the only value possible when there is
	%only one value possible
	
	MM = D<<-member(mirrorMenu),
	if
		Full = @on
	then
		MM->>active(@off)
	else
	(
		if
			Arg1Value \== @nil
		then
			MM->>active(@off) %no inverse corr for value-corr
		else
		(
			if
				Derivative == @on
			then
			(
				NotInversed = yes,
				Inversed = yes
			)
			else
			(
				if
					Arg1->>checkQSCorrCompatibility(Arg2,@off)
				then
					NotInversed = yes
				else
					NotInversed = no,
				if
					Arg1->>checkQSCorrCompatibility(Arg2,@on)
				then
					Inversed = yes
				else
					Inversed = no
			),
			if 
				(NotInversed = no ; Inversed = no)
			then
			(
				MM->>active(@off),
				if
					NotInversed = yes
				then
					MM->>selected(mirror,@off)
				else
					MM->>selected(mirror,@on)
			)
			else
			(
				%everything ok
				MM->>selected(mirror,Mirror),
				MM->>active(@on)
			)
		)
	),
	D->>setArgumentsDisplay.

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
	A1V = D<<-arg1Value,
	A2 = D<<-arg2,
	if
		D?arg2Route->>empty
	then
		Arg2Imported = ''
	else
		Arg2Imported = ' (imported)',
	A2V = D<<-arg2Value,
	D?arg1_member->>clear,
	D?arg2_member->>clear,
	Derivative = D<<-derivative,
	Full = D<<-full,
	pl_setArgumentsDisplay_Corr(Full,Derivative,D?arg1_member,A1,Arg1Imported,A1V),
	pl_setArgumentsDisplay_Corr(Full,Derivative,D?arg2_member,A2,Arg2Imported,A2V).

%
pl_setArgumentsDisplay_Corr(@on,_,List,Arg,ArgI,_):-	%garp3 0.3: full correspondence
	List->>append(dict_item(string('Instance: %s%s',Arg?garpInstance?name,ArgI))),
	List->>append(dict_item(string('Quantity: %s',Arg?name))),
	List->>append(dict_item('FULL CORRESPONDENCE')).
%
pl_setArgumentsDisplay_Corr(@off,@off,List,Arg,ArgI,Value):- %2.1: @off: no derivative
	List->>append(dict_item(string('Instance: %s%s',Arg?garpInstance?name,ArgI))),
	List->>append(dict_item(string('Quantity: %s',Arg?name))),
	if
		Value == @nil
	then (
		List->>append(dict_item('QUANTITY SPACE')),
		List->>append(dict_item(string('  %s',Arg?quantitySpace?name)))
		)
	else (
		List->>append(dict_item('VALUE')),
		List->>append(dict_item(string('  %s', Value?valueName)))
		).
%
pl_setArgumentsDisplay_Corr(@off,@on, List,Arg,ArgI,Value):- %2.1: @on, derivative
	List->>append(dict_item(string('Instance: %s%s',Arg?garpInstance?name,ArgI))),
	List->>append(dict_item(string('Quantity: %s',Arg?name))),
	if
		Value == @nil
	then (
		List->>append(dict_item('DERIVATIVE')),
		List->>append(dict_item(string('  %s',Arg?quantitySpace?name)))
		)
	else (
		List->>append(dict_item('DERIVATIVE VALUE')),
		List->>append(dict_item(string('  %s', Value?valueName)))
		).
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
	D?directedMenu_member->>set(x:= D?directedMenu_member?left_side + Difference?width / 2),
	D?mirrorMenu_member->>set(x:= D?mirrorMenu_member?left_side + Difference?width / 2),
		
	D?remarks_member->>right_side(Arg2?right_side),
	D?remarks_member->>bottom_side(D?remarks_member?bottom_side + Difference?height),
	D?ok_member->>set(y:=D?ok_member?top_side + Difference?height),
	D?cancel_member->>move(point(Arg2?right_side - D?cancel_member?width, D?ok_member?top_side)).
%%

%%
switch(D):->
	"Switch arguments pressed" ::

	A1 = D<<-arg1,
	A1R = D<<-arg1Route,
	A1V = D<<-arg1Value,
	A1R->>lock_object(@on),
	A1V->>lock_object(@on),
	D->>arg1(D?arg2),
	D->>arg1Route(D?arg2Route),
	D->>arg1Value(D?arg2Value),
	D->>arg2(A1),
	D->>arg2Route(A1R),
	D->>arg2Value(A1V),
	D->>setArgumentsDisplay,
	A1R->>lock_object(@on),
	A1V->>lock_object(@off),
	D->>switched(D?switched?negate).
%%

%%
saveNewElement(D):->
	%nieuwe maken
	@model->>changeRequest(newCorrespondence,
		D?modelFragment,
		D?editor,
		?(D?directedMenu_member,selected,directed),
		D?derivative, %2.1: derivative?
		?(D?mirrorMenu_member,selected,mirror), %2.1
		D?full, %gp3 0.3
		D?arg1,D?arg1Route,D?arg1Value,
		D?arg2,D?arg2Route,D?arg2Value,
		D?remarks_member?contents).
%%

%%
saveChangedElement(D):->
	%bestaande opslaan
	@model->>changeRequest(changeCorrespondence,
		D?modelFragment,
		D?editor,
		D?element,
		?(D?directedMenu_member,selected,directed),
		D?derivative, %2.1
		?(D?mirrorMenu_member,selected,mirror), %2.1
		D?full, %gp3 0.3
		D?arg1,D?arg1Route,D?arg1Value,
		D?arg2,D?arg2Route,D?arg2Value,
		D?remarks_member?contents).
%%

%%
notChanged(D):->
	%we kunnen de args alleen switchen, dus checken we of ze niet geswitched zijn
	@off = D<<-switched,
	%verder kunnen alleen de directed status, mirror status en de remarks wijzigen
	PrevDirected = D?element<<-directed,
	PrevDirected = D?directedMenu_member<<-selected(directed),
	PrevMirror = D?element<<-mirror,
	PrevMirror = D?mirrorMenu_member<<-selected(mirror),
	D?remarks_member?contents->>equal(D?element?remarks).
%%

%%%%%%%%%%%%%%%%%%%%%%CHANGE%%%%%%%%%%%%%%%
%%changeCorrespondence: als niet door ons maar wel onze correspondence, dan stoppen we er mee
changeApplied_changeCorrespondence(D,
	CR: changeRequestor):->

	\+ CR->>checkEditor(D?editor),
	CR->>checkArgument(1,D?element),
	D->>return.
%%

%%deleteCorrespondence: precies hetzelfde als change (alleen editor maakt niet uit)
changeApplied_deleteCorrespondence(D,
	CR: changeRequestor):->

	CR->>checkArgument(1,D?element),
	D->>return.
%%
:-pce_end_class.
