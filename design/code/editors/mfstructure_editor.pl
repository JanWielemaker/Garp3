/*
mfStructureEditor class
part of GARP3

gp3 0.3
Replaces the old structureEditor: shows model fragments and their relations and
gives oportunity to edit their properties and contents.

Helper class mfNode also included: this is the graphical element displayed

Helper class conditionalConnection: a connection that displays a small 'c' for condition

Helper class: mfViewList: this is the list of saved views with possibility to create a new one etc

2005 Jelmer Jellema, Spin in het Web, spininhetweb.nl
*/

:- use_module(library(pce_tagged_connection)). %needed for conditional connections

:-pce_begin_class(
		  mfStructureEditor,
		  framedWindow
		 ).

variable(currentView,name := 'Default view',get). %name of the currently displayed view. The name 'Default view' is handled specially.

variable(parentLink,link,get). %link used for parent->child
variable(parentLinksShown,bool := @on, get).

variable(conditionalLink,link,get). %link user for conditional_in
variable(conditionalLinksShown, bool := @off, get).

variable(moveSelect,gesture,get,"The saved click=select / move gesture").
variable(moveSelectSingle,gesture,get,"The saved click=select / move-single gesture"). %gp3 0.3
variable(toggleSelect,click_gesture,get,"The saved shift-click = toggle select gesture").
variable(toggleSelect_alternative, click_gesture, get, "Alternative for shift_click: middle_click").
variable(contextMenu, contextPopup,get,"The context menu").
variable(mustRedrawView, bool := @off, get). %@on when we need to update the display in mustRedrawView
%%
initialise(E
	   ) :->
	%create the editor

	get(@model, getModelNameForEditor, 'Model fragment definitions editor - Build', ModelNameForEditor),

	E->+ initialise(label:=ModelNameForEditor,
			 buttonbar := vertical, %gp3 added buttonbar
			 helpId := 'Build_ModelFragments'
		),
	E->>icon(@build_icon),
	
	%special entries in mfViews (key=@nil and key='Default view') are set in @model pl_versionPatch(M,4).
	
	%do we have saved view?
	if
		CurrentView = ?(@model?mfViews,member,@nil)<<-member(currentView)
	then
		E->>currentView(CurrentView)
	else
		E->>currentView('Default view'), %default view for starters, saves too
		
	E->>slot(parentLink,link(parentHandle,childHandle,line(0,0,0,0,first))),
	E?parentLink->>colour(black), 

	E->>slot(conditionalLink,link(containerHandle,conditionHandle,line(0,0,0,0,first))),
	E?conditionalLink->>colour(gray70),
	
	E->>init_commands,
	E->>init_menu,
	E->>init_buttonbar,
	E->>init_handlers,
	
	E->>open, %it helps to have the window open before drawing connections etc
	
	E->>redrawView(@on), %view changed because nothing visible yet
	E->>saveViewNow, %make sure default view exists
	
	MaxSize *= size(@display?width * 0.80,
		       @display?height * 0.80),
	DefSize *= size(800,600),
	%gp3 0.4.7: try and use a saved size and position
	unless
		SavedSize = ?(@model?mfViews,member,@nil)<<-member(frameSize)
	do
		SavedSize = DefSize,
	%bepaal de grootte: er kan opgeslagen zijn dat het window kleiner moet
	%groter mag echter niet

	Width *= number(MaxSize?width),
	Width->>minimum(SavedSize?width),
	Height *= number(MaxSize?height),
	Height->>minimum(SavedSize?height),
	E->>size(size(Width,Height)),
	
	if
		Position = ?(@model?mfViews,member,@nil)<<-member(framePosition)
	then
		E->>position(Position),

	get(@model, '_value', GarpModel),
	send(E, associateModel, GarpModel).

%%

%%
destroy(E):->
	%free all gestures etc
	E->>saveViewNow, %needed to make sure all changes are saved
	
	%gp3 0.4.7: save position and size of the window
	%these two values are copied from viewEditor.
	?(@model?mfViews,member,@nil)->>append(framePosition,E?position),
	?(@model?mfViews,member,@nil)->>append(frameSize,E?size), 

	%gp3 0.4.7: we no longer explicitly free the gestures etc. Pce should take care of this
	%and when we destroy a gesture that has not yet returned (open dialog etc)
	%pce crashes
	
	/*
	E?moveSelect->>free,
	E?moveSelectSingle->>free,
	E?toggleSelect->>free,
	E?toggleSelect_alternative->>free,
	E?contextMenu->>free, 
	*/
	E->+destroy.
%%

%%
init_commands(E):->
	%all commands
	E->>command('SaveModel', key := '\\C-s', keystring := '[Ctrl + S]'),
	E->>command('AddChild',key := '\\C-a', keystring := '[Ctrl + A]'),
	E->>command('Properties', key := '\\C-j', keystring := '[Ctrl + ENTER]'),
	E->>command('ToggleActive', key := 'a', keystring := '[A]'), %gp3 0.3.11
	E->>command('Edit',key := 'RET', keystring := '[ENTER]'),
	E->>command('Copy'),
	E->>command('ModelFragmentCopy',  key := '\\C-c', keystring := '[Ctrl + C]'), % JL
	E->>command('ModelFragmentPaste', key := '\\C-v', keystring := '[Ctrl + V]'), % JL
	E->>command('Delete',key := 'DEL', keystring := '[DEL]', otherkeys := chain(backspace)),
	E->>command('ViewDefault',key := 'd', keystring := '[D]'),
	E->>command('ViewSave',key := 's', keystring := '[S]'),
	E->>command('ViewOpen',key := 'o', keystring := '[O]'),
	E->>command('ViewToggleParent',key := 'p', keystring := '[P]'),
	E->>command('ViewToggleConditional',key := 'c', keystring := '[C]'),
	E->>command('ViewTranslations',key := '\\C-t', keystring := '[Ctrl + T]'), %%gp3 1.4.0
	E->>command('ToggleTooltips'), %gp3 1.4.0
	E->>command('SaveEPS'), %gp3 0.3
	E->>command('ContextPopup',runnable := @off). %to fill the contextmenu
%%

%%
init_menu(E):->
	%fill the menubar
	send_list(E?menubar, append,
		  [
		   new(File,popup('File')),
		   new(Edit,popup('Edit')),
		   new(View,popup('View'))
		  ]),
	send_list(File, append,
	    [
   		   menuCommand(properties,'Properties', label := 'Properties...', end_group := @on),
   		   menuCommand(eps,'SaveEPS', label := 'Save Diagram to EPS file'), %gp3 0.3
   		   menuCommand(saveModel,'SaveModel','Save model to disk')
	    ]),

	send_list(Edit,append,
		  [
		   menuCommand(add_child,'AddChild',label := 'Add child...'),
   		   menuCommand(edit,'Edit', label := 'Edit...'),
		   menuCommand(delete,'Delete', label := 'Delete', end_group := @on),
		   menuCommand(copy,'Copy',label := 'Clone...'),
		   menuCommand(copy, 'ModelFragmentCopy', label := 'Copy ...'), %JL
		   menuCommand(paste, 'ModelFragmentPaste', label := 'Paste ...', end_group := @on), %JL
   		   menuCommand(toggleActive, 'ToggleActive', label := 'Make active')
   		   ]),

	send_list(View,append,
		  [
		   menuCommand(default,'ViewDefault', label := 'Default view'),
		   menuCommand(save,'ViewSave', label := 'Save this view...'),
		   menuCommand(open, 'ViewOpen', label := 'Open another view...',end_group := @on),
		   menuCommand(toggle_parent, 'ViewToggleParent', label := 'Show parent-child relations'),
		    menuCommand(toggle_conditional, 'ViewToggleConditional', label := 'Show conditional relations',end_group := @on),
		    menuCommand(translations, 'ViewTranslations', label := 'Translations...'),
		    menuCommand(tooltips,'ToggleTooltips') %gp3 1.4.0 (dynamic label)
		   ]).
%%

%%
init_buttonbar(E):->
	
	B = E<<-buttonBar,
	B<<-add(add_child,'AddChild',buttonbars,element_new,element_new_inactive, 'Add model fragment'),
	B<<-add(edit,'Edit',buttonbars,edit_mf,edit_mf_inactive, 'Edit selected model fragment',below,add_child),	
	B<<-add(properties,'Properties',buttonbars,element_properties,element_properties_inactive, 'Show model fragment properties',below,edit),
	B<<-add(copy,'Copy',buttonbars,copy_mf,copy_mf_inactive, 'Clone selected model fragment',below,properties),
	B<<-add(delete,'Delete',buttonbars,element_delete,element_delete_inactive,'Delete selected model fragment',below,copy),
	B<<-add(toggleActive,'ToggleActive',buttonbars,toggle_active_mf,toggle_active_mf_inactive, E?toggleActiveTooltip,below,delete, toggle := @on),
	B->>append(graphical(0,0,0,8),below), %small gap
	
	B<<-add(default,'ViewDefault',buttonbars,layout_horizontal,layout_horizontal_inactive, 'Show default view'),
	B<<-add(save,'ViewSave',buttonbars,save_currentview,@nil, 'Save current view',below,default),
	B<<-add(open,'ViewOpen',buttonbars,open_anotherview,@nil, 'Open another view',below,save),
	B<<-add(toggleParent,'ViewToggleParent',buttonbars,toggle_parent_mf_relations,@nil,'Show parent-child relations',below,open, toggle := @on),
	B?toggleParent_member->>value(E?parentLinksShown),
	B<<-add(toggleConditional,'ViewToggleConditional',buttonbars,toggle_conditional_mf_relations,@nil,'Show conditional relations',below,toggleParent, toggle := @on),
	B?toggleConditional_member->>value(E?conditionalLinksShown).
%%

%%
parentLinksShown(E, Value: bool):->
	%set parentLinksShown variable and update button
	
	E?buttonBar?toggleParent_member->>value(Value),
	E->>slot(parentLinksShown,Value).
%%

%%
conditionalLinksShown(E, Value: bool):->
	%set conditionalLinksShown variable and update button
	
	E?buttonBar?toggleConditional_member->>value(Value),
	E->>slot(conditionalLinksShown,Value).
%%

%%
currentView(E, ViewName: name):->
	%set currentView and save it too
	
	E->>slot(currentView,ViewName),
	?(@model?mfViews,member,@nil)->>append(currentView,ViewName). %always exists: created in our initialise
%%

%%%%%%%%%%% ACTIONS
%copied from otherparts, so dutch comments
%%
%% NB: in default view you cannot move any element, you will have to save the view first

init_handlers(E
	      ):->
	%copied frm viewEditor (and edited), hence the dutch comments
	%de handlers worden opgeslagen voor hergebruik
	%(niet als pce_global, maar gewoon als member)

	%wat functies
	Element *= @arg1?receiver,
	Frame *= Element?frame,
	
	%een move_gesture die meteen ook selecteert
	%Move *= move_gesture(left),
	Move *= multi_move_gesture(E?getMultiMove,
		->>(E,dragMultiMove,@arg1,@arg2,@on),left), %multi_move_gesture.pl
	Move->>condition(->>(Frame,
			     action_singleSelection,
			     Element)),
	E->>slot(moveSelect,Move),

	%gp3: the same but with control key only moves an object, not its
	%moveablesubs
	MoveSingle *= multi_move_gesture(E?getMultiMove,
		->>(E,dragMultiMove,@arg1,@arg2,@off),left,c), %multi_move_gesture.pl
	MoveSingle->>condition(->>(Frame,
			     action_singleSelection,
			     Element)),
	E->>slot(moveSelectSingle,MoveSingle),

	%en de toggle versie voor select
	E->>slot(toggleSelect,click_gesture(left,
					      's',
					      single,
					      ->>(Frame,
						action_toggleSelect,
						  Element))),
	E->>slot(toggleSelect_alternative,click_gesture(middle,'',
							single,
							->>(Frame,action_toggleSelect,Element))),
	%context menu
	E->>slot(contextMenu, contextPopup('ContextPopup',
					     ->>(Frame,
					       action_singleSelection,
						 Element))),
	%selectie-rectangle heeft een beetje andere vars
	Select *= ->>(@arg1?frame,
		      action_multiselection_select,
		      @arg1),
	Deselect *= ->>(@arg1?frame,
			action_multiselection_deselect,
			@arg1),
	%de normale zonder shift
	SGNormaal *= select_gesture(left,
				    '',
				    Select,
				    Deselect),
	%de conditie zorgt voor het weghalen vd oude selectie
	SGNormaal->>condition(->>(Frame,
				  action_removeSelection)),
	E?client->>recogniser(SGNormaal),
	%de versie met shift heeft geen conditie, zodat
	%oude selectie blijft bestaan
	SGAdd *= select_gesture(left,
				's',
				Select,
				Deselect),
	E?client->>recogniser(SGAdd).
%%

%%
getMultiMove(E,
	M : chain):<-

	%get all graphicals that need to move: the selection and all 
	%children / applies-to stuff

	S = E?client<<-selection, 
	M = S<<-copy.
%%

%%
dragMultiMove(E,Change: point, Dragged: chain, DragMoveableSubs: bool):->
	%callback van multi_move_gesture bij een drag
	%in Dragged zit al de lijst met te verplaatsten graphicals
	%(volgens getMultiMove van hierboven)
	
	%gp3 0.3 added DragMoveableSubs argument
	
	%in Default view you cannot move at all:
	if
		'Default view' = E<<-currentView
	then
		E->>statusText('You cannot move elements while in default view. Save the view first.')
	else
	(
		Moved *= chain,
		Dragged->>for_all(->>(@arg1,dragMove,Change,Moved,DragMoveableSubs))
	).
%%
			
%%
action_removeSelection(E):->
	"Implement remove selection action"::

	E?client->>selection(@nil).
%%

%%
action_singleSelection(E, G: graphical):->
	"Implement single selection action"::
	%selecteren van deze graphical, maar niet als dat al zo is 
	%dan blijft dus de multiple-selection intakt

	\+ @on = G<<-selected
	-> E?client->>selection(G)
	;  true.
%%

%%
action_toggleSelect(_E, G: graphical):->
	"Implement toggle select action" ::

	G->>selected(G?selected?negate).
%%


%%
action_multiselection_select(_VE,E: graphical):->
	"Implement select callback for select_gesture" ::
	%het element wordt geselecteerd. Maar als ie dat al is dan laten we de call
	%falen: de gesture zal dan niet de deselect callback gebruiken voor dit element
	%zodat het ding geselecteerd blijft (voor de shift variant)

	\+ @on = E<<-selected,
	E->>instance_of(mfNode),
	E->>selected(@on).
%%

%%
action_multiselection_deselect(_VE, E: graphical):->
	"Implement deselect callback for select_gesture" ::

	E->>selected(@off).
%%

%%%%%% END ACTIONS


%%%%%%%%%%%%% COMMAND HANDLERS %%%%%%%%%%%%%
updateFillPopupContextPopup(_E,Popup) :->
	%fill the context menu
	
	send(Popup,clear),
	send_list(Popup,append,
		  [
		   menuCommand(properties,'Properties', label := 'Properties...'),
		   menuCommand(toggleActive,'ToggleActive', label := 'Make active'),
		   gap,
   		   menuCommand(edit,'Edit', label := 'Edit...'),
   		   menuCommand(add_child,'AddChild',label := 'Add child...'),
		   menuCommand(copy,'Copy',label := 'Clone...'),

   		   menuCommand(delete,'Delete', label := 'Delete')
   		   ]).
%
infoDefaultItemContextPopup(_E,
			     DefaultItem: 'any*'):<-

	DefaultItem = edit. %allways
%%

%%properties
checkProperties(E):->
	%1 item, not top element
	
	El = E<<-singleSelection,
	\+ El?fragment?parents->>empty.
%
onProperties(E):->
	%open the propsdlg for editing the selection
	new(mfPropsDlg(E))->>editObject(E?selectedMF).
%%

%%add child
checkAddChild(E):->
	%single selection
	E<<-singleSelection.
%
onAddChild(E):->
	%open the propsdlg for adding a child
	new(mfPropsDlg(E))->>newObject(E?selectedMF).
%%

%%edit: edit the selection
checkEdit(E):->
	%1 item, not top element
	
	El = E<<-singleSelection,
	\+ El?fragment?parents->>empty.
%
onEdit(E):->
	%open the view editor if non-existant
    	MForIS = E<<-selectedMF,

	%open an existing or new viewEditor
	get(@model, all_hypers, HyperChain),
	chain_list(HyperChain, HyperList),
	(
	    member(Hyper, HyperList),
	    get(Hyper, to, ScenarioEditor),
	    get(ScenarioEditor, class_name, 'viewEditor'),
	    get(ScenarioEditor, fragment, MForIS) ->
	    send(ScenarioEditor, expose)
	;
	    new(ScenarioEditor, viewEditor(MForIS)),
	    new(_PartOfHyper, partof_hyper(@model, ScenarioEditor)),
	    send(ScenarioEditor, expose)
	).
%%

%%toggleActive
checkToggleActive(E):->
	%1 item, not top element
	El = E<<-singleSelection,
	\+ El?fragment?parents->>empty.
%
infoLabelToggleActive(E, Label: name):<-
	%which label to show
	El = E<<-singleSelection,
	if
		@on = El?fragment<<-active
	then
		Label = 'Make inactive'
	else
		Label = 'Make active'.
%
toggleActiveTooltip(E, Tooltip: name):<-
	%which tooltip to show (for button)
	%called as function from button, so not implemented as command infotype
	if
		@on = E?singleSelection?fragment<<-active
	then
		Tooltip = 'The selected model fragment is active. Click to make it inactive.'
	else
		Tooltip = 'The selected model fragment is inactive. Click to make it active.'.
%
infoValueToggleActive(E, Value: bool):<-
	%return the value this button should have right now
	
	Value = E?singleSelection?fragment?active<<-negate. %gp3 0.4.7 made 'active' = @off, 'inactive' = @on, by special request of BB
%
onToggleActive(E):->
	%do not get the new value from the buttonbar, because this
	%command can also be executed by a menu
	%just negate it
	
	Fragment = E?singleSelection<<-fragment,
	
	%just use CR for changing MF
	@model->>changeRequest(changeMF,
		Fragment,
		E,
		Fragment?name,
		Fragment?remarks,
		Fragment?parents,
		Fragment?active?negate
		).
%%


%%

%%saveModel
%%
checkSaveModel(_E):->
	% Save even if the model has not changed 
	true.
	%@on = @model<<-changed.
%
onSaveModel(_E):->
	%gp3 just tell the app we want to save
	@app->>save.
%%

%%saveEPS

onSaveEPS(E):->
	%gp3 0.3: save the graph to file
	%copied code from class_visigarp
	
	if
		get(@garp3_finder, file, E, 'Save Diagram to EPS file',@off, '.eps', FileName)
	then
	(
		Pict = E<<-client, %gp3
		new(File, file(FileName)),
		send(File, open, write),
		send(File, append, Pict?postscript),
		send(File, close),
		send(File, done),
		send(E, statusText, string('Saved PostScript in %s', FileName)) %gp3 0.3 changed this from report
	).
%%	

%%copy: copy the MF and possibly its children, through a helper
checkCopy(E):->
	%1 item, not top element
	
	El = E<<-singleSelection,
	\+ El?fragment?parents->>empty.
%
onCopy(E):->
	CHD *= mfCopyHelperDlg(E,E?selectedMF),
	ignore(CHD->>doCopy),
	object(CHD),
    CHD->>destroy.
%%

%%
onModelFragmentCopy(E) :->
    get(E, selectedMF, SelectedMF),
    chain_list(ModelFragments, [SelectedMF]),
    send(@copyBuffer, copyModelFragmentDef, ModelFragments).

onModelFragmentPaste(E) :->
    send(@copyBuffer, pasteModelFragmentDef, E).

%%delete: delete the MF and all of its children
checkDelete(E):->
	%1 item, not top element
	
	El = E<<-singleSelection,
	\+ El?fragment?parents->>empty.
%
onDelete(E):->
	@model->>changeRequest(deleteMF,E?selectedMF,E).
%%

%%viewDefault: goto the default view
checkViewDefault(E):->
	%when current is default, we cannot go to default
	\+ 'Default view' = E<<-currentView.
%
onViewDefault(E):->
	E->>saveViewNow, %needed to make sure all changes are saved
	E->>currentView('Default view'),
	E->>redrawView(@on). %view changed
%%

%%
onViewOpen(E):->
	L *= mfViewList(E,load),
	if
	(
		View = L<<-confirm_centered
	)
	then
	(
		L->>destroy,
		E->>saveViewNow, %needed to make sure all changes are saved to old view
		E->>currentView(View),
		E->>redrawView(@on) %view changed
	).
%%

%%
onViewSave(E):->
	L *= mfViewList(E,save),
	if
	(
		View = L<<-confirm_centered
	)
	then
	(
		L->>destroy,
		E->>currentView(View),
		E->>setLabel,
		E->>saveViewNow, %after change: old version not saved
		@app->>setModelChanged
	).
%%

%%ViewToggleParent: show or hide the parent link, allways possible
infoLabelViewToggleParent(E, Label: name):<-
	%which label to show
	
	if
		@on = E<<-parentLinksShown
	then
		Label = 'Hide parent-child relations'	
	else
		Label = 'Show parent-child relations'.
%
onViewToggleParent(E):->
	%show or hide the relations
	
	E->>parentLinksShown(E?parentLinksShown?negate),
	E->>showHideParentLinks.
%%

%%
%ViewToggleConditional: show or hide the applies-to links, allways possible
infoLabelViewToggleConditional(E, Label: name):<-
	if
		@on = E<<-conditionalLinksShown
	then
		Label = 'Hide conditional relations'	
	else
		Label = 'Show conditional relations'.
%
onViewToggleConditional(E):->

	E->>conditionalLinksShown(E?conditionalLinksShown?negate),
	E->>showHideConditionalLinks.
%%

%%translation: gp3 1.4.0

onViewTranslations(_E):->
	@app->>openLanguageEditor.
%%

%%toggle tooltips
%gp3: this just switches the 'model' category in tooltip window on or of (global setting)
infoLabelToggleTooltips(_E,
	Label: name):<-
	
	if
		@on = @tooltip_window<<-category_state(model)
	then
		Label = 'Hide model ingredient tooltips'
	else
		Label = 'Show model ingredient tooltips'.
%
onToggleTooltips(_E):->
	@tooltip_window->>category_state(model,?(@tooltip_window,category_state,model)?negate).
%%

%%%%%%%%%%%%%%% END COMMAND HANDLERS
%%

redrawView(E,ViewChanged: [bool]):->
	%(re-)draw the current view
	
	%like viewEditor,
	%we keep a list of (re)drawn elements and delete all elements that
	%are not redrawn afterwards
	
	%normaly, we save the view before redrawing, this way we allways have
	%the right positions etc in the @model?mfViews data
	%but when ViewChanged is given and @on, we do not do this
	%because we redraw a different view than the one visible now

	
	unless
		ViewChanged = @on
	do
		E->>saveViewNow,
	
	Drawn *= chain,
	if
		'Default view' = E<<-currentView
	then
	(
		E->>redrawDefaultView(Drawn)
	)
	else
	(
		E->>redrawCustomView(Drawn)
	),
	E->>setLabel,
	E?elements->>for_all(
			if(
				not(->>(Drawn,member,@arg1)),
				and(
					->>(@arg1,destroy)
				)
			)),		
	%load some general information
	ViewData = @model?mfViews<<-member(E?currentView),	
	ignore(E->>parentLinksShown(?(ViewData,member,parentLinksShown))),
	ignore(E->>conditionalLinksShown(?(ViewData,member,conditionalLinksShown))),
	
	%connections, we do them afterwards, when all elements are there (visible or not)
	%showHideLinks only draws them when needed
	E->>showHideParentLinks,
	E->>showHideConditionalLinks.
%%

%%
redrawDefaultView(E,Drawn: chain):->
	%draw the default hierarchical view

	CurrentY *= number(2),
	E->>redrawDefaultViewRecursive(@model?topStaticFragment,5,CurrentY,Drawn),
	CurrentY->>plus(10), %more space
	E->>redrawDefaultViewRecursive(@model?topProcessFragment,5,CurrentY,Drawn),
	CurrentY->>plus(10), %more space
	E->>redrawDefaultViewRecursive(@model?topAgentFragment,5,CurrentY,Drawn).
%
redrawDefaultViewRecursive(E,
	MF: modelFragment, X: number, Y: number,
	Drawn: chain):->
	%position a modelFragment and its children, setting the Y value to where the
	%next subtree could start
	
	%when there is already an element for this fragment, we reuse it
	%2 possibilities: the element is already placed, but is
	% also child of the current ParentNode. When this is the case, the element
	% is already in Drawn.
	% Otherwise: the element is not in Drawn, we have to move it to the correct
	% place and remove all old links
	
	
	unless
		G = E<<-findElement(MF)
	do
	(
		%non existant, create it now
		G = E<<-createElement(MF),
		E?client->>display(G) %just put it somewhere, will move later
		%we do not put it in Drawn yet, so code below will see this as a not yet reused element
	),
	%now the element exists. Is this already reused? We do not have to do anything
	unless
		Drawn->>member(G)
	do
	(
		%not (re)-used yet, go on
		Drawn->>append(G), %now this element is current again
		G->>updateDisplay,
		
		%placement and children:
		TopY = Y<<-value, %currentvalue
		NewX *= number(X),
		NewX->>plus(G?width + 15), %some space
		%do we have children? If so, they decide the Y placement for the next one
		Children = MF<<-sortedChildren,
		if
			Children->>empty
		then
		(
			Y->>plus(G?height) %place for next item on same level
		)
		else
		(
			Spacer *= var(value:=0), %space only between children
			Children->>for_all(
				and(
					->>(Y,plus,Spacer),
					->>(E,redrawDefaultViewRecursive,@arg1,NewX,Y,Drawn), %moving Y all the time
					assign(Spacer,5,global)
					)
				)
		),
			E?client->>display(G,point(X, TopY + ((Y - TopY  - G?height) / 2)))
	).
%%

%%
redrawCustomView(E, Drawn: chain):->
	%draw a saved view
	%just get all information needed from the view data
	
	ViewData = @model?mfViews<<-member(E?currentView),
	%just like in default view, we draw topnodes and then recursively their
	%children, only placement etc is different
	
	E->>redrawCustomViewRecursive(@model?topStaticFragment,@nil,ViewData,Drawn),
	E->>redrawCustomViewRecursive(@model?topProcessFragment,@nil,ViewData,Drawn),
	E->>redrawCustomViewRecursive(@model?topAgentFragment,@nil,ViewData,Drawn).
%
redrawCustomViewRecursive(E,
	MF: modelFragment, ParentNode: mfNode*, ViewData: hash_table,
	Drawn: chain):->
	
	unless
		G = E<<-findElement(MF)
	do
	(
		%does not exist yet, create it now
		G = E<<-createElement(MF),
		E?client->>display(G) %will move later
		%we do not put it in drawn yet, so code below will see this as a not yet reused element
	),
	unless
		Drawn->>member(G)
	do
	(
		%not done yet, place the element
		Drawn->>append(G),
		G->>updateDisplay,
		
		%placement
		if
		(
			MFData = ViewData<<-member(MF)
		)
		then
		(
			( SavedPosition = MFData<<-member(position) ; SavedPosition = @default)
		)
		else
		(
			SavedPosition = @default
		),
		
		if
			SavedPosition = @default %no information saved
		then
		(
			%where to place this one?
			if
				ParentNode = @nil
			then
				NewPosition *= point(5,5)
			else
				NewPosition = ParentNode?position<<-plus(point(ParentNode?width + 15,0))
		)
		else
		(
			NewPosition = SavedPosition
		),
		E?client->>display(G,NewPosition),
		%children?
		MF?sortedChildren->>for_all(
			->>(E,redrawCustomViewRecursive,@arg1,G,ViewData,Drawn)
			)
	).
%%


%%
mustRedrawView(E):->
	%make sure that the view is redrawn on the next changeTreeApplied event
	E->>slot(mustRedrawView,@on).
%%

%%
setLabel(E):->
	%set the view name in the label
	get(@model, getModelNameForEditor, 'Model fragment definitions editor', ModelNameForEditorLabel),
	E->>label(string('%s - %s - Build', ModelNameForEditorLabel, E?currentView)).
%%

%%
findElement(E, MF: modelFragment, Element: mfNode):<-
	%find an existing element for the fragment
	%fail if not existing
	Element = E?elements<<-find(
			@arg1?fragment == MF
		).
%%

%%
elements(E, Elements: chain):<-
	%return new chain with all mfNode elements
	
	Elements = E?client?graphicals<<-find_all(
		->>(@arg1,instance_of,mfNode)).
%%
	
%%
createElement(E,MF: modelFragment, Element: mfNode):<-
	%create a new element for the model fragment, not checking existing
	
	Element *= mfNode(MF),
	%set recognisers
	Element->>recogniser(E?contextMenu), 
	Element->>recogniser(E?moveSelect),
	Element->>recogniser(E?moveSelectSingle), %gp3 0.3
	Element->>recogniser(E?toggleSelect),
	Element->>recogniser(E?toggleSelect_alternative).
%%

%%
singleSelection(E, Element: mfNode):<-
	%returns the one and only selected element, fails if selection is more than one
	Sel = E?client<<-selection,
	1 = Sel<<-size,
	Element = Sel<<-head,
	Sel->>done.
%%

%%
selectedMF(E, MF: modelFragment):<-
	%returns the fragment of the one and only selected element, fails otherwise
	
	MF = E?singleSelection<<-fragment.
%%


%%
saveViewNow(E):->
	%save all information for the current view
	%todo: save name of current view for reopening
	%todo: find all data that needs to be saved
	%
	%We do NOT set the model to changed... (we will do so when a new view is created)

	/*
	Structure of mfViews:
	Head: viewname ['Default view' for default view, @nil for standard information]
	Tail: View data hash_table:
		Head: Modelfragment or a name for data about the whole view
		Tail: Value. For modelfragments: a hash_table with data:
			Head: name ('position')	
			Tail: value (point)
	In default view information will be saved, but positional information will not be used.
	*/
	
	NewData *= hash_table,
	%standard stuff
	NewData->>append(parentLinksShown,E?parentLinksShown),
	NewData->>append(conditionalLinksShown, E?conditionalLinksShown),
	%%
	E?elements->>for_all(
		and(
			->>(NewData,append,@arg1?fragment,@arg1?elementViewData)
		)
	),
	@model?mfViews->>append(E?currentView,NewData).
%%

%%
showHideParentLinks(E):->
	%show or hide the parent connections
	%hiding = removing them, because displayed does not work very well

	%we always remove existing connections, because they might be wrong
	%after a change (this is also called from redraw code)
	
	Link = E<<-parentLink,
	E?elements->>for_all(->>(@arg1,disconnect, link := Link)),
	
	if
		@on = E<<-parentLinksShown
	then
		E?elements->>for_all(
			and(
				assign(new(Node,var),@arg1),
				->>(Node?fragment?parents,for_all,
					->>(quote_function(create(connection,
						?(E,findElement,@arg1),Node,Link)),
						hide) %hide below elements
					)
				)
			).
%%

%%
showHideConditionalLinks(E):->
	%show or hide all conditional connections
	%hiding = removing them, because displayed does not work very well

	%we always remove existing connections, because they might be wrong
	%after a change (this is also called from redraw code)
	
	Link = E<<-conditionalLink,
	E?elements->>for_all(->>(@arg1,disconnect, link := Link)),
	if
		@on = E<<-conditionalLinksShown
	then
		E?elements->>for_all(
			and(
				assign(new(Node,var),@arg1),
				assign(new(Importing,var),Node?fragment?conditionalIn),
				->>(Importing,unique),
				->>(Importing,for_all,
					quote_function(create(conditionalConnection, %does the 'hide' itself
						?(E,findElement,@arg1),Node,Link))
					)
				)
			).
%%

/******************** MULTIPLE MODELS ***********************/ %JL
/* Make the right model active */
input_focus(T, Boolean) :->
	send(@app, selectCorrectTab, T), % T is the window object (of the editor)
	send_super(T, input_focus, Boolean).
%%


%%%%%%%%%%%%% CHANGE REQUESTORS %%%%%%%%%%%%%%%%%%%%
%%
%%logic copied from old Homer structureEditor (now gone)

changeApplied_newMF(E,
	_CR: changeRequestor):->
	E->>mustRedrawView.
%%

changeApplied_changeMF(E,
	_CR: changeRequestor):->
	E->>mustRedrawView.
%%
changeApplied_deleteMF(E,
	CR: changeRequestor):->

	%we delete the element first, but afterwards
	%also full redraw, because of placement in default view
	(
	    G = E<<-findElement(CR?object) ->
	    G->>destroy
	;
	    true
	),
	E->>mustRedrawView.
%%

changeApplied_newConditionalFragment(E, _CR: changeRequestor):->
	E->>mustRedrawView.
%%

%%
changeApplied_changeConditionalFragment(E, _CR: changeRequestor):->
	E->>mustRedrawView.
%%

%%
changeApplied_deleteConditionalFragment(E, _CR: changeRequestor):->
	E->>mustRedrawView.
%%

%%
changeApplied_newFragmentRefiner(E, _CR: changeRequestor):->
	E->>mustRedrawView.
%%

%%
changeApplied_deleteFragmentRefiner(E, _CR: changeRequestor):->
	E->>mustRedrawView.
%%

%%
changeApplied_copyMF(E,
	_CR:changeRequestor):->	
	%just redraw, no selection change
	E->>mustRedrawView.
%%

changeApplied(E,
	_CR: changeRequestor):->
	%this one we use to update information about structure rule
	%just redraw:
	E?elements->>for_all(
		    ->>(@arg1,updateDisplay)).

%%

%%
changeTreeApplied(E):->
	%when other changeApplied events told us to redraw, we do it now
	if
		@on = E<<-mustRedrawView
	then
	(
		E->>redrawView(@on), %no view change: the view can be saved again % JL IT should be redrawn! not @off!! 
			% JJ: the @on or @off is not to decide whether this should redraw but whether it shoud redraw after a view-change, 
			% please try to understand code before changing it. ;-O
			%so the @on above should probably be @off anyway..., but who am i to change it back
		E->>slot(mustRedrawView,@off)
	).
%%

:- pce_end_class.

:- pce_begin_class(mfNode,
	figure).

%the graphical element displayed

handle(0,h/2,childHandle,@default).
handle(w,h/2,parentHandle,@default).
handle(w,h/2,childHandle,@default).
handle(0,h/2,parentHandle,@default).

handle(w/2, 0, containerHandle, @default).
handle(w/2 + 5, 0, conditionHandle, @default).
handle(w/2 - 5, 0, conditionHandle, @default).
handle(w/2, h, containerHandle, @default).
handle(w/2 + 5, h, conditionHandle, @default).
handle(w/2 - 5, h, conditionHandle, @default).

initialise(MFN, MF: modelFragment) :->
	%init this element

	MFN->+initialise,
	MFN->>hyper(MF,fragment,mfNode),
	%gp3 1.4: if MF is a top model fragment, do not show any tooltip
	%(this editor does not show inputSystems, so no check for that)
	unless 
		MF?parents->>empty
	do
		MFN->>tooltip(@default,model), %dynamic
	Bitmap *= psBitmap(MFN?currentImage), %gp3 0.3.11 changed to psBitmap
	Bitmap->>name(bitmap),
	Bitmap->>psdef(MFN?currentImageName),
	MFN->>display(Bitmap),
	NameText *= text(''),
	NameText->>name(nameText),
	NameText->>font(italic),
	MFN->>display(NameText,
			point(0, %will be done in updateDisplay
			      Bitmap?bottom_side)),
	MFN->>updateDisplay.
%%

%%
updateDisplay(MFN):->
	%update the current display
	%currently, we change the color of the text only
	MF = MFN<<-fragment,
	Text *= string('%s',MF?name),

	unless
		0 = MF?remarks<<-size
	do
		Text->>append(' *'),
	MFN?nameText_member->>string(Text),
	MFN?nameText_member->>colour(MFN?statusColour),
	MFN?bitmap_member->>image(MFN?currentImage),
	MFN?bitmap_member->>psdef(MFN?currentImageName),
	MFN?nameText_member->>center_x(MFN?bitmap_member?center_x).
	
%%

%%
currentImageName(MFN, N: name):<-
	%get the name for the right image and psdef
	
	%gp3 0.3.12: there are now state-dependend pictures
	
	MF = MFN<<-fragment,
	N *= string('mf'),
	if
		MF->>isIncomplete 
	then
		N->>append('_incomplete'),
		
	if
		@off = MF<<-active
	then
		N->>append('_inactive')
	else %maybe inactive because of dependency?
	(
		unless
			MF->>activated
		do
			N->>append('_depinactive')
	).
%%

%%
currentImage(MFN, I: image):<-
	%get the right image.
	%For now, this is allways the same one
	
	Name = MFN<<-currentImageName,
	get_image(mfstructure,Name,I).
%%

%%
statusColour(MFN, C: colour):<-
	%return the text-colour
	
	%1: no parents = topnode
	MF = MFN<<-fragment,
	if
		MF?parents->>empty
	then
		%C = colour<<-convert('#757575') %cannot use colour('#6323...'). 
		C *= colour(black) %gp3 0.3.14 also black
	else
		%gp3 0.3.12: just black
		C = colour<<-convert('black').
%%	

%%
fragment(MFN, MF: modelFragment):<-
	%return the fragment
	MF = MFN<<-hypered(fragment).
%%

%%
%%
tooltipContent(MFN,S: string):<-
	%gp3 1.4 rewrite of the tooltip
	
	MF = MFN<<-fragment,
	C = MF<<-relevantComments,
	if
		0 = C<<-size
	then
		S = string('(no comments)')
	else
	(
		S = C<<-strip
	).
%%

%%
dragMove(MFN, Change: point, Moved: chain, _DragMoveableSubs: bool):->
	%copied from visualElement
	%we have to move and maybe we have to move related stuff to
	
	%for now: just move
	MFN->>relative_move(Change),
	Moved->>append(MFN).
%%


%%
elementViewData(MFN, NodeData: hash_table):<-
	%return a chain containing all data for this node
	%to be saved in view data
	
	NodeData *= hash_table, %element data
	%all relevant information:
	NodeData->>append(position,MFN?absolute_position). %absolute_position works better than position, because of possible negative values in the x of the name member (after centering).
	%thats all for now
%%


:- pce_end_class.

:-pce_begin_class(conditionalConnection,tagged_connection
		 ).

	%a connection with a small 'c' for condition

initialise(CC, From: graphical, To: graphical, Link: [link]):->
	%we do not need the other arguments
	
	%te doen: circle erachter, geen pen, even groot als max hoogte/breedte en centreren
	CC->+initialise(From,To,Link),
	Icon *= figure,
	Text *= text('C',center,bold),
	%Text->>colour(CC?link?colour), %same colour as link
	Text->>colour(red),
	Diameter *= number(Text?width),
	Diameter->>maximum(Text?height),
	Circle *= circle(Diameter),
	Circle->>pen(0),
	White *= colour(white),
	Circle->>fill_pattern(White),
	Icon->>display(Circle),
	Icon->>display(Text),
	Circle->>center(Text?center),
	CC->>tag(Icon),
	%we first hide the Icon, then the connection
	%this way the connection + icon is below elements, but the icon is above connection
	Icon->>hide,
	CC->>hide.
				 
:-pce_end_class.

:-pce_begin_class(mfViewList,assistanceDialog %gp3 0.3.5 changed this to assistance: with helpid
		 ).
%list used for showing all views, for saving or loading
%code mostly stolen from savedSimulationList

variable(type,{load,save},both).

%%
initialise(D, Editor: mfStructureEditor, Type: {load,save}):->
	D->+initialise('Model fragment views',later),
	D->>icon(@build_icon),

	D->>type(Type),
	if
		Type = load
	then
	(
		D->>label('Open a model fragment view'),
		D->>helpId('Build_MF_OpenAnotherView')
	)
	else
	(
		D->>label('Save a model fragment view in the model'),
		D->>helpId('Build_MF_SaveCurrentView')
	),
	D->>application(@app),
	D->>transient_for(Editor), %also used to get back to the editor
	D->>modal(transient),
	%D->>kind(toplevel),

	GapX = D?gap<<-width,
	GapY = D?gap<<-height,

	DefList *= extendedListBrowser(height := 15, width := 30), 
	DefList->>name(list),
	DefList->>label('Saved views:'),
	DefList->>show_label(@on),
	DefList->>select_message(->>(D,onSelection)),
	if
		Type = load
	then
		DefList->>open_message(->>(D,openView))
	else
		DefList->>open_message(->>(D,saveView,DefList?selection?key)),
		
	D->>display(DefList,point(GapX,D?topY)),
	D->>fillList, %fill now, do not bother later
	
	ButtonHeight *= number(0),
	
	if
		Type= load
	then
	(
		Open *= imgButton(openView, img:= edit_changes, tt:= 'Open selected model fragment view'),
		Open->>active(@off),
		ButtonHeight->>plus(Open?height)
	),
	
	Remove *= imgButton(remove, img:= remove, tt:= 'Delete selected model fragment view'),
	Remove->>active(@off),
	ButtonHeight->>plus(Remove?height),
	
	Cancel *= imgButton(cancel, img:=undo, tt:= 'Cancel'),
	ButtonHeight->>plus(Cancel?height),
	
	ListBottom *= number(DefList?bottom_side),
	ListBottom->>maximum(DefList?list_top + 
					ButtonHeight +
					2 * GapY), 
	DefList->>bottom_side(ListBottom),

	if
		Type = load
	then
		D->>display(Open,point(DefList?right_side + GapX,
						DefList?list_top)),
	
	%we have remove halfway (even if Open is not shown)
	D->>display(Remove,point(DefList?right_side + GapX,
					DefList?list_top + ((ListBottom - DefList?list_top) / 2) -
							(Remove?height / 2))),

	%and Cancel below
	D->>display(Cancel,point(DefList?right_side + GapX,
			ListBottom - Cancel?height)),

	%Save the view?
	if
		Type = save
	then
	(
		Msg *= label(msg,'Select a model fragment view to overwrite or type a new name.\nClicking save will save the view in the model.\nYou will have to save the model as well.',
			font(helvetica,oblique,10)),
		D->>display(Msg,point(GapX,ListBottom + GapY)),
		Name *= eventTextItem(name), 
		Name->>label('Name:'),
		Name->>afterKey(->>(D,checkSave)), %check if we can enable the save button
		D->>display(Name,point(GapX,Msg?bottom_side + GapY)),
		Name->>keyboard_focus(@on),
		Name->>right_side(DefList?right_side),
		Save *= imgButton(saveView, img:= save, tt:='Save model fragment view in model'),
		Save->>active(@off),
		D->>display(Save,point(Name?right_side + GapX,Name?top_side)),
		Below = Save?bottom_side
	)
	else
	(
		Below = Cancel?bottom_side,
		DefList->>keyboard_focus(@on),
		DefList->>selection('Default view') %gp3 0.3.11 by request of BB
	),
		
	D->>assign_accelerators,	
	D->>confirm_done(@off),
	D->>updateSpacers, %gp3 0.3.13 needed when assistanceDialog is used, but displayContent is not used to fill the dialog
	D->>minimalSize(size(Cancel?right_side,Below)).
%%

%%
onResize(D, Difference: size):->
	%gp3 0.2: 
	
	D?list_member->>right_side(D?list_member?right_side + Difference?width),
	D?list_member->>bottom_side(D?list_member?bottom_side + Difference?height),
	if
		OpenView = D<<-member(openView)
	then
		OpenView->>set(x:= OpenView?left_side + Difference?width),
	D?remove_member->>set(x:= D?remove_member?left_side + Difference?width,
							y:= D?remove_member?top_side + Difference?height / 2),
	D?cancel_member->>set(x:= D?cancel_member?left_side + Difference?width,
							y:= D?cancel_member?top_side + Difference?height),
	if
		Msg = D<<-member(msg)
	then
		Msg->>set(y:= Msg?top_side + Difference?height),
	if
		Name = D<<-member(name)
	then
	(
		Name->>set(y:= Name?top_side + Difference?height),
		Name->>right_side(Name?right_side + Difference?width)
	),
	
	if
		Save = D<<-member(saveView)
	then
		Save->>set(x:= Save?left_side + Difference?width,
					y:= Save?top_side + Difference?height).

%%
label(D,L: name):->
	%make sure the frame label is set, not the decorator label
	D?frame->>label(L).
%
label(D, L: name):<-
	L = D?frame<<-label.
%%

%%
fillList(D):->
	%fill the list of view names
	%Default view is added just like other views

	List = D<<-list_member,
	List->>clear,
	List->>sort_by(@nil), %cancel sorting for now
	@model?mfViews->>for_all(
		if(@arg1 \== @nil,
			->>(List,append,create(dict_item,@arg1,object:= @arg1)))),
	List->>sort_by. %restore sorting
%%

%%
onSelection(D):->
	%something selected in the list
	
	Selection = D?list_member<<-selection, %dict_item
	
	if
		load = D<<-type
	then
		D?openView_member->>active(@on),
	
	if
		'Default view' = Selection<<-object %default view
	then
		D?remove_member->>active(@off)
	else
		D?remove_member->>active(@on),
	if
		save = D<<-type
	then
	(
		D?name_member->>selection(Selection?key),
		D->>checkSave
	).
%%

%%
openView(D):->
	%just return the selection, we assume there is one
	View = D?list_member?selection<<-object,
	D->>return(View).
%%

%%
remove(D):->
	%remove the selection from the model, return @nil
	S = D?list_member?selection<<-key,
	if
		@pce->>confirm('This will permanently remove the saved view \'%s\' from the model',
			S)
	then
	(
		%we do this ourselves
		@model?mfViews->>delete(S),
		@app->>setModelChanged, %no conceptual change
		D->>fillList,
		D?openView_member->>active(@off),
		D?remove_member->>active(@off)
	).
%%


%%
saveView(D,SelectedName: [name]):->
	TypedName = D?name_member<<-selection,
	default(SelectedName, TypedName, Name),
	\+ char_array(Name)->>equal('Default View',@on), %just fail if default view
	if
	(
		@model?mfViews<<-member(Name)
	)
	then
		@pce->>confirm('This will overwrite the already existing saved view \'%s\'.',Name), 
			%fails if not confirmed
	D->>return(Name).
%%

%%
cancel(D):->
	D->>destroy. %makes D<<-confirm fail
%%

%%
checkSave(D):->
	%check whether we should enable the save button
	if
	(
		0 = D?name_member?selection<<-size
	;
		char_array(D?name_member?selection)->>equal('Default View',@on)
	)
	then
		D?saveView_member->>active(@off)
	else
		D?saveView_member->>active(@on).
%%
:-pce_end_class.
