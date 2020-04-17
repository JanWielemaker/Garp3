/*
Garp3 0.2
The helper dialog for copying modelfragments or scenarios

PART OF GARP3 SEE COPYRIGHT NOTICE
	
*/

:-pce_begin_class(mfCopyHelperDlg,
		  assistanceDialog
		 ).
variable(hasChildren,bool,both).
variable(multipleInheritance, bool, both).

%%
initialise(D, Parent: ['dialog'|'frame'], Fragment: modelFragment):->

	D->+initialise(string('Copy %s', Fragment?displayTypeName),'Build_CopyMF'),
	D->>icon(@build_icon),

	D->>application(@app),
	D->>hyper(Parent,parent),
	D->>hyper(Fragment,fragment),
	D->>transient_for(Parent),
	D->>modal(transient),

	%we do the placement ourselves
	GapX = D?gap<<-width,
	GapY = D?gap<<-height,
	MaxX *= number(0), %for right justification

	Text *= label(lbl,string('How to process children:')),
	D->>display(Text,point(GapX,D?topY)),
	MaxX->>maximum(Text?right_side),
	
	Action *= menu(action,marked),
	Action->>show_label(@off),
	Action->>layout(vertical),
	Action->>append(menu_item(notree, label:= 'Do not copy any child objects')),
	if
		0 = Fragment?children<<-size
	then
	(
		D->>hasChildren(@off), %we will never show anyway, but just in case
		Action->>selection(notree)
	)
	else
	(
		D->>hasChildren(@on),
		%there are children, is there also multiple inheritance?
		if
			Fragment->>checkAllDown(@arg1?parents?size > 1,@off,@on) %not us, only parent-child tree
		then
		(
			%multiple inheritance
			D->>multipleInheritance(@on),
			Action->>append(menu_item(singletree, label:= 'Copy the tree of children that fully belong to the subtree, do not copy children with more than one parent.')),
			Action->>append(menu_item(multitree, label:= 'Copy the whole tree of children, also the ones with more than one parent.'))
		)
		else
		(
			%single inheritance
			D->>multipleInheritance(@off),
			Action->>append(menu_item(singletree, label:= 'Copy the whole tree of children.'))
		),
		Action->>selection(singletree) %this is allways the default
	),
	
	D->>display(Action,point(GapX, Text?bottom_side + GapY)),
	MaxX->>maximum(Action?right_side),
	

	Ok *= imgButton(ok,img:=save,tt:='Start the copy action'),
	D->>display(Ok,point(GapX,Action?bottom_side + GapY)),
	Ok->>default_button(@on),
	
	Cancel *= imgButton(cancel, img:=undo, tt:='Cancel the copy action'),
	D->>display(Cancel,point(MaxX - Cancel?width,Ok?top_side)),
	D->>assign_accelerators,
	D->>updateSpacers. %gp3 0.3.13 needed when assistanceDialog is used, but displayContent is not used to fill the dialog
%%

%%
doCopy(D):->
	%see if we have to show this dialog or can just go to the copystuff
	if
		@on = D<<-hasChildren
	then
	(
		%show
		D<<-confirm_centered(D?parent?area?center),
		D->>destroy
	)
	else
	(
		%do not show, just do it
		D->>startCopyCR
	).

%%
parent(D, P: frame):<- 
	P = D<<-hypered(parent).
%%

%%
fragment(D, F: modelFragment):<- 
	F = D<<-hypered(fragment).
%%

%%
cancel(D):->
	D->>return.
%%

%%
ok(D):->
	%create the CR
	D->>startCopyCR,
	D->>return.
%%

%%
startCopyCR(D):->
	%see what we have to do, and do it
	Action = D?action_member?selection<<-value,
	pl_startCopyCR_getvalue(Action, CopySingle, CopyMulti),
	get(D, parent, InputSystemList), % Resolves bug 35 (dialog converted to frame)
	@model->>changeRequest(copyMF,D?fragment,InputSystemList,CopySingle,CopyMulti).
%
pl_startCopyCR_getvalue(notree,@off,@off).
pl_startCopyCR_getvalue(singletree,@on,@off).
pl_startCopyCR_getvalue(multitree,@on,@on).

:-pce_end_class.
