/*
definitie identityPropsDlg klasse.
De standaard eigenschappen dialoog van identities.
Is alleen een commentaardialoogje, alleen voor editObject
*/

:-pce_begin_class(identityPropsDlg,
		  propertyDialog,
		  "Standard identity relation properties dialog"
		 ).

%%
initialise(D,F: frame):->
	"Initialise the properties dialog" ::
	
	D->+initialise('Identity properties - Build', F, fake),
	%D->>helpid(string('Build_%s_IdentityProperties',D?containerType)),
	%de onderdelen: lay out weer helemaal uitgeschreven
	GapX = D<<-gapX,
	GapY = D<<-gapY,

	Remarks *= editor(height := 5, width := 60),
	Remarks->>name(remarks),
	Remarks->>label('Remarks:'),
	Remarks->>show_label(@on),
	Remarks->>fill_mode(@on),
	%gp3 1.4. problem, no name, so no default textedit font. Silly solution, but this works
	Remarks->>font(new(text_item(dummy))?font),
	D->>display(Remarks,point(GapX,D?topY)), 
	
	Ok *= imgButton(ok, img:=save, tt:='Apply changes'),
	D->>display(Ok,point(GapX,Remarks?bottom_side + GapY)),
	Ok->>default_button(@on),
	
	Cancel *= imgButton(cancel, img:=undo, tt:= 'Cancel changes'),
	D->>display(Cancel,point(Remarks?right_side - Cancel?width,Ok?top_side)),
	D->>updateSpacers, %gp3 0.3.13
	D->>assign_accelerators,
		%minimal size:
	D->>minimalSize(size(Cancel?right_side,Cancel?bottom_side)), %abs min

	% Multiple model support
	get(@model, getModelNameForEditor, 'Identity properties - Build', ModelNameForEditor),
	new(Garp3EditorFrame, garp3EditorFrame(ModelNameForEditor)),
	get(@model, '_value', Model),
	send(Garp3EditorFrame, associateModel, Model),
	send(Garp3EditorFrame, append, D),
	send(Garp3EditorFrame, transient_for, F).

%%

%%
onResize(D, Difference: size):->
	%gp3 0.2: 
	
	D?remarks_member->>right_side(D?remarks_member?right_side + Difference?width),
	D?remarks_member->>bottom_side(D?remarks_member?bottom_side + Difference?height),
	D?ok_member->>set(y:=D?ok_member?top_side + Difference?height),
	D?cancel_member->>move(point(D?remarks_member?right_side - D?cancel_member?width, D?ok_member?top_side)).
%%

%%
editObject(D,
	I : identityRelation,
	ReadOnly : bool	 
	 ):->
	"Open dialog for existing identity" ::

	D->>element(I),
	D?remarks_member->>contents(I?remarks),

	get(@model, getModelNameForEditor, string('Identity properties%s - Build',when(ReadOnly == @on,' [Read Only]','')), ModelNameForEditor),
	D->>label(ModelNameForEditor),
	D?ok_member->>active(ReadOnly?negate),

	D->>openDialog.
%%

%%
saveChangedElement(D):->
	%bestaande opslaan
	@model->>changeRequest(changeIdentity,
		D?modelFragment,
		D?editor,
		D?element,
		D?remarks_member?contents).
%%

%%
notChanged(D):->
	"Succeeds when the configuration is not changed by the user" ::

	%remarks hetzelfde?
	D?remarks_member?contents->>equal(D?element?remarks).
%%

%%%%%%%%%change requestors%%%%%%%%%%%%
%%

changeApplied_changeIdentity(D,
	CR: changeRequestor):->

	%dicht als wij dezelfde editen
	\+ CR->>checkEditor(D?editor), %niet als deze editor (anders dubbel sluiten)
	CR->>checkArgument(1,D?element),
	D->>return.
%%

%%
changeApplied_deleteIdentity(D,
	CR: changeRequestor):->

	%dicht als wij dezelfde editen
	CR->>checkArgument(1,D?element),
	D->>return.
%%
:-pce_end_class.
