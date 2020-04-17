/*
Definitie entityEditor class
Editor voor entities, agents en assumptions
Bevat ook de class voor de figuren (entityNode), als simpele helperclass

Part of Garp3, see copyright notice
Most code old dutch homer code

2000-2005 Jelmer Jellema, Spin in het Web, spininhetweb.nl
*/

:-pce_begin_class(
		  entityEditor,
		  framedWindow,
		  "Implements the entity editor main frame"
		 ).

variable(mappings,hash_table,get, "Mappings from entities to nodes").

variable(top,hierarchicalObject,get,"The top object").

variable(type,{entity,agent,assumption},get,"The edited type").

variable(tree, tree, get, "the displayed tree").

variable(contextMenu, contextPopup,get,"The context menu").
%%
initialise(EE,
	   Top: top = hierarchicalObject,
	   Select: select = [hierarchicalObject] %gp3: node to select
	   ) :->
	"Create an entity editor and open it" ::

	EE->>slot(type,Top?class_name),
	if
		entity = EE<<-type
	then
	(
		Label = 'Entity hierarchy editor - Build',
		Status = 'Edit the entity hierarchy',
		HelpId = 'Build_EntityHierarchy'
	),
	if
		agent = EE<<-type
	then
	(
		Label = 'Agent hierarchy editor - Build',
		Status = 'Edit the agent hierarchy',
		HelpId = 'Build_AgentHierarchy'
	),
	if
		assumption = EE<<-type
	then
	(
		Label = 'Assumption hierarchy editor - Build',
		Status = 'Edit the assumption hierarchy',
		HelpId = 'Build_AssumptionHierarchy'
	),

	get(@model, getModelNameForEditor, Label, ModelNameForEditorLabel),

	EE ->+ initialise(label:=ModelNameForEditorLabel,
			 statustext:= Status, buttonbar := vertical, %gp3 added buttonbar
			 helpId:= HelpId
		),
	EE->>icon(@build_icon),
	%%%%%%%%%%%%overige slots%%%%%%%%%%%%%%%%%%%
	EE->>slot(top,Top),
	EE->>slot(type,Top?class_name),
	EE->>slot(mappings,new(hash_table)),
	Tree *= tree,
	EE->>slot(tree,Tree),
	%%%%%%%%%%%verdere initialisatie%%%%%%
	
	EE->>init_commands,
	EE->>init_menu,
	EE->>init_buttonbar,
	EE->>init_tree,
	EE->>init_handlers,	%zet ook het popupmenu, dat dynamisch gevuld wordt
	EE?client->>display(Tree),
	EE->>open,
	EE->>size(size(600,300)),
	%gp3: optionally select a node
	unless
		Select = @default
	do
	(
		Node = EE<<-nodeForEntity(Select),
		Node->>selected(@on),
		EE?client->>normalise(Node)
	),

	get(@model, '_value', GarpModel),
	send(EE, associateModel, GarpModel).
%%

%%
destroy(EE):->
	"Destroy the editor" ::

	%dit is nodig om de popups in gestures te destroyen
	EE?contextMenu->>free, 
	EE->+destroy.
%%

%%
init_menu(EE
	  ) :->
	"Initialise menu-structures" ::
	%zie ook hieronder bij initCommands
	
	send_list(EE?menubar, append, %zie framedWindow
		  [
		   new(File,popup('File')),
		   new(Edit,popup('Edit')),
		   new(View,popup('View'))
		  ]),
	
	send_list(File,append,
		[
   		   menuCommand(eps,'SaveEPS', label := 'Save Diagram to EPS file') %gp3 0.3
		]),
	send_list(Edit,append,
		  [
		   menuCommand(add_child,'NodeAddChild',label := 'Add child...'),
		   menuCommand(delete,'SelectionDelete', label := 'Delete', end_group := @on),
   		   menuCommand(properties,'NodeProperties', label := 'Properties...', end_group := @on),
		   menuCommand(copy, 'EntityCopy', label := 'Copy ...'), %JL
		   menuCommand(paste, 'EntityPaste', label := 'Paste ...') %JL
   		  ] ),

	send_list(View,append,
		  [
		   menuCommand(horizontal,'ViewHorizontal', label := 'Horizontal'),
		   menuCommand(vertical,'ViewVertical', label := 'Vertical'),
		   menuCommand(list,'ViewList', end_group := @on, label := 'List'),
		   menuCommand(toggle_expand, 'NodeCollapseExpand',label := 'Collapse'),
		   menuCommand(expand_all, 'ExpandAll', label := 'Expand all', end_group := @on),
		   menuCommand(translations, 'ViewTranslations', label := 'Translations...'),
		   menuCommand(tooltips,'ToggleTooltips') %gp3 1.4.0 (dynamic label)
		   ]).
%%

%%
init_buttonbar(EE):->
	%gp3 0.2: create the elements for the buttonbar
	
	B = EE<<-buttonBar,
	B<<-add(delete,'SelectionDelete',buttonbars,element_delete,element_delete_inactive,'Delete entry from hierarchy'),
	B<<-add(add_child,'NodeAddChild',buttonbars,element_new,element_new_inactive, string('Add %s to hierarchy', EE?type),right,delete),
	B<<-add(properties,'NodeProperties',buttonbars,element_properties,element_properties_inactive, 'Show properties',below,delete),
	B<<-add(horizontal,'ViewHorizontal',buttonbars,layout_horizontal,layout_horizontal_inactive, 'Layout hierarchy: horizontal',right,properties),
	B<<-add(list,'ViewList',buttonbars,layout_list,layout_list_inactive, 'Layout hierarchy: list',below,properties),
	B<<-add(vertical,'ViewVertical',buttonbars,layout_vertical,layout_vertical_inactive, 'Layout hierarchy: vertical',right,list).
%%
	
%%
init_tree(EE
	  ):->
	"Initialise the tree"::
	Tree = EE<<-tree,
	Tree?link->>arrows(first),
	Tree->>auto_layout(@on),
	Tree->>neighbour_gap(5),
	Tree->>level_gap(25),
	EE->>fillTree,
	%hebben we een opgeslagen direction?
	(
		if
		(
			D = EE?tree?root?element<<-attribute(entityEditDirection),
			D \== @nil
		)
		then
			EE->>treeDirection(D)
	).
%%

%%
init_handlers(EE
	      ):->
	"Initialise the event handlers" ::

	%zie beneden bij Acties voor de implementatie van de action_... calls
	Frame *= @arg1?receiver?frame,
	Node *= @arg1?receiver?node,
	%click_gesture voor selectie
	EE?tree->>node_handler(click_gesture(left,
					     '',
					     single,
					     ->>(Frame,
					       action_singleSelection,
						Node))),
	%met shift kan je selectie toggelen
	CG *= click_gesture(left,'s',single,
				->>(Frame,
			      action_toggleSelection,
				Node)),
	EE?tree->>node_handler(CG),

	%het context menu
	Popup *= contextPopup('SelectionMenu',
			      ->>(Frame,
				action_singleSelection,
				  Node)),
	EE?tree->>node_handler(Popup),
	EE->>slot(contextMenu,Popup),

	%de select gesture doet selectie-rectangle en dus ook selectie
	%weghalen bij alleen klikken op het canvas
	%beetje andere vars bij selectie/deselectie
	Select *= ->>(@arg1?frame,
		     action_multiselection_select,
		      @arg1?node),
	Deselect *= ->>(@arg1?frame,
			action_multiselection_deselect,
			@arg1?node),
	SG *= select_gesture(left,
			     '',
			     Select,
			     Deselect,
			     EE?tree), %parent van te selecteren graphicals
	SG->>condition(->>(Frame, %condite: oude selectie weghalen
			  action_removeSelection)),
	EE?client->>recogniser(SG), %selectie-rechthoek normaal

	%de tweede select gesture doet hetzelfde, maar dan met shift:
	%oude selectie niet weghalen
	SG2 *= select_gesture(left,
			      's',
			      Select,
			      Deselect,
			      EE?tree),
	%geen conditie: oude selectie blijft
	EE?client->>recogniser(SG2).
			      
%%
	
%%
fillTree(EE
	 ) :->
	"(re)Fill the tree from the model" ::

	EE?mappings->>clear, %clear oude mappings
	RootNode = EE<<-nodeForEntity(EE?top), %maak root node
	EE?tree->>root(RootNode), %clear oude tree en zet rootnode
	RootNode->>updateDisplay, %teken hem
	RootNode->>checkCollapsed, %en check of hij niet collapsed zou moeten zijn (opgeslagen bij de entity)
	EE?top?children->>for_all(->>(EE,
					    recursive_fillTree,
					    RootNode,
					    @arg1)), %zie daar
	EE->>sortTree(RootNode).
%%

%%
/*Recursive_fillTree methode. Helper voor fillTree: vult de boom recursief met de
  entitystruktuur. We volgen het geheel via de children methode van een entity
  (met de parents komt het dan vanzelf goed). We maken een knoop aan voor een kind
  (als die er nog niet is), voegen die als kind van de parentnode toe, en gaan
  recursief voor de children verder.
  Dit is een pce methode, geen prolog callback.
*/

recursive_fillTree(EE,
		   ParentNode : node,
		   Child : hierarchicalObject
		   ) :->
	"Recursive iterating callback for fillTree" ::

	NewNode = EE<<-nodeForEntity(Child),
	ParentNode->>son(NewNode),
	NewNode->>updateDisplay,
	NewNode->>checkCollapsed,
	Child?children->>for_all(->>(EE,
					   recursive_fillTree,
					   NewNode,
					   @arg1)). %dit dus recursief
%%

%%
sortTree(EE,Parent: node):->
	%sorteer de boom die al bestaat opnieuw

	Sons = Parent<<-sons,
	Sons->>sort(?(@arg1?element?name,compare,@arg2?element?name)),
	Sons->>for_all(and(->>(@arg1,move_after),->>(EE,sortTree,@arg1))).

/*******************ACTIES*****************************/
%%
action_removeSelection(EE
		      ):->
	"Implement remove selection action" ::
	EE?tree->>selection(@nil).
%%

%%
action_singleSelection(EE,
		       Node: node
		       ):->
	"Implement single-selection action" ::
	%maar als de node al geselecteerd is doen we niets

	\+ @on = Node<<-selected
	->  EE?tree->>selection(Node)
	;   true.
%%

%%
action_toggleSelection(_EE,
		       Node: node
		      ):->
	"Implement selection toggle for single node" ::

	Node->>selected(Node?selected?negate).
%%

%%
action_multiselection_select(_EE,
			     Node: node
			    ):->
	"Implement select callback for select_gesture"::
	%de node wordt geselecteerd. We laten de call falen voor al
	%geselecteerde nodes zodat de select_gesture met shift die niet
	%zal deselecteren. (de conditie van de select_gesture zonder shift
	%zorg voor deselectie voordat de gesture begint, dus dat gaat goed)

	\+ @on = Node<<-selected,
	Node->>selected(@on).
%%
	
%%
action_multiselection_deselect(_EE,
			       Node: node
			      ):->
	"Implement deselect callback for select_gesture" ::
	%de graphical wordt gedeselecteerd

	Node->>selected(@off).
%%

/******************COMMANDS**************************/
%%
init_commands(EE
	      ) :->
	"Initialise command objects" ::
	%initialiseer onze command objecten (zie command.pl en framedWindow.pl)

	%1) Simpele commando s die niet gedelegeerd worden
	EE->>command('ViewHorizontal'),
	EE->>command('ViewVertical'),
	EE->>command('ViewList'),
	EE->>command('ViewTranslations',key := '\\C-t', keystring := '[Ctrl + T]'), %%gp3 1.4.0
	EE->>command('ToggleTooltips'), %gp3 1.4.0
	EE->>command('NodeAddChild',key := '\\C-a', keystring := '[Ctrl + A]'),
	EE->>command('NodeCollapseExpand', key := 'SPC', keystring := '[SPACE]'),
	EE->>command('ExpandAll', key := '*', keystring := '[*]'),
	EE->>command('NodeProperties',key := 'RET', keystring := '[ENTER]'),
	EE->>command('SelectionDelete',key := 'DEL', keystring := '[DEL]', otherkeys := chain(backspace)),
	EE->>command('SaveEPS'), %gp3 0.3
	EE->>command('EntityCopy',  key := '\\C-c', keystring := '[Ctrl + C]'), % JL
	EE->>command('EntityPaste', key := '\\C-v', keystring := '[Ctrl + V]'), % JL

	%2 commands voor het afhandelen van de menu-balk, deze runt dus niet
	%maar de update command is handig voor het vullen van het menu enzo
	send(EE,command,'SelectionMenu',runnable := @off).
%%	

%%ViewHorizontal
onViewHorizontal(EE
		 ) :->
	
	EE->>treeDirection(horizontal).
%
checkViewHorizontal(EE
		    ) :->
	get(EE,treeDirection,Direction),
	\+ (Direction == horizontal).
%%

%%ViewVertical
onViewVertical(EE) :->
	send(EE, treeDirection, vertical).
%
checkViewVertical(EE) :->
	get(EE, treeDirection,Direction),
	\+ (Direction == vertical).
%%

%%ViewList
onViewList(EE) :->
	send(EE,treeDirection,list).
%
checkViewList(EE) :->
	get(EE,treeDirection,Direction),
	\+ (Direction == list).
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

%%NodeCollapseExpand
onNodeCollapseExpand(EE) :->
	"Collapse or expand the selected node" ::
	get(EE,selectedNode,Node),
	(   get(Node,collapsed,@on)
	->  send(Node,collapsed,@off)
	;   send(Node,collapsed,@on)
	).
%
checkNodeCollapseExpand(EE) :->
	"Check if the command will be available" ::
	%commando is er als er een node geselecteerd is die children heeft
	get(EE?selectedNode?sons,size,S),
	S > 0.
%
infoLabelNodeCollapseExpand(EE,Label : name) :<-
	"return a label for this command" ::
	get(EE,selectedNode,Node),
	(   
	    get(Node,collapsed,@on)
	->  Label = 'Expand'
	;   Label = 'Collapse'
	).
%%

%%ExpandAll
checkExpandAll(EE) :->
	"Check if this command will be available" ::
	%1. selectie is collapsed
	%2. selectie heeft een collapsed kind
	%3. er is geen selectie en er is een collapsede knoop

	if
		EE?tree?selection->>empty
	then
		Node = EE?tree<<-root
	else
		Node = EE<<-selectedNode,
					
	Node<<-find(@arg1?collapsed == @on). %geldt ook voor de node zelf
%
onExpandAll(EE) :->
	"Expand the selected node and all subnodes" ::
	
	if
		EE?tree?selection->>empty
	then
		Node = EE?tree<<-root
	else
		Node = EE<<-selectedNode,
	Node->>for_all(->>(@arg1,collapsed,@off)).
%%
		 
%%NodeAddChild: Toevoegen van een child-entity bij de geselecteerde entity
%
checkNodeAddChild(EE) :->
	%Er moet een node geselecteerd zijn
	EE->>nodeSelected.

%
onNodeAddChild(EE) :->
	"Add a new object as child of this one" ::

	new(hObjectPropsDlg(EE))->>newObject(EE?selectedNode?element).
%%

%%NodeProperties
checkNodeProperties(EE) :->
	%de node mag niet de root node zijn
	%gp3: The top node can be changed (its name, that is) if its an entity
	get(EE,selectedNode,S),
	if
		S = EE?tree<<-root
	then
		entity = EE<<-type. %so any other topnode will fail

%
onNodeProperties(EE) :->
	"Change the name of the selected entity" ::
	%een prompter voor de naam en zorgen dat er meteen een changerequest
	%wordt gepost in de check callback van de prompter

	new(hObjectPropsDlg(EE))->>editObject(EE?selectedNode?element).
%%

/* JL: EntityCopy */
onEntityCopy(EE) :->
    get(EE, type, EEtype),
    ( EEtype == entity,     Type = 'Entity' ;
      EEtype == agent,      Type = 'Agent' ;
      EEtype == assumption, Type = 'Assumption'),
    get(EE?tree?selection, map, @arg1?node?element, SelectedEntities),
    send(@copyBuffer, entityCopy, SelectedEntities, Type).
%

/* JL: EntityPaste  */
onEntityPaste(EE) :->
    get(EE, type, EEtype),
    ( EEtype == entity,     Type = 'Entity' ;
      EEtype == agent,      Type = 'Agent' ;
      EEtype == assumption, Type = 'Assumption'),
    (
	% Paste underneath a node if possible
	send(@copyBuffer, entityPaste, EE?selectedNode?element, Type, EE)
    ;
	send(@copyBuffer, entityPaste, @nil, Type, EE)
    ).


%%selectionDelete: verwijderen van de selectie
checkSelectionDelete(EE):->
	%er moet een selectie van 1 zijn en dit mag niet de hoofdentity zijn

	S = EE<<-selectedNode,
	\+ S = EE?tree<<-root.
%
onSelectionDelete(EE):->
	@model->>changeRequest(deleteHObjectTree,
		EE?selectedNode?element,
		EE).

onSaveEPS(EE):->
	%gp3 0.3: save the graph to file
	
	if
		get(@garp3_finder, file, EE, 'Save Diagram to EPS file',@off, '.eps', FileName) %added arguments
	then
	(
		Pict = EE<<-client, %gp3
		new(File, file(FileName)),
		send(File, open, write),
		send(File, append, Pict?postscript),
		send(File, close),
		send(File, done),
		send(EE, statusText, string('Saved PostScript in %s', FileName)) %gp3 0.3 changed this from report
	).
	
	
%%selectionMenu: command controleert popups
checkSelectionMenu(EE) :->
	%er moet een selectie zijn
	send(EE,nodeSelected).
%

infoDefaultItemSelectionMenu(_EE,
			     DefaultItem: 'any*'):<-
	%We moeten de value van het default item teruggeven voor de contextPopup

	DefaultItem = properties. %altijd
%
updateFillPopupSelectionMenu(_EE,Popup) :->
	%We vullen de popup met de juiste items
	%In dit geval alleen maar voor de entity
	send(Popup,clear),
	send_list(Popup,append,
		  [
		   menuCommand(properties,
			       'NodeProperties',
			       label := 'Properties...',
				   end_group := @on),
		   menuCommand(toggle_expand,
			       'NodeCollapseExpand',
				   label := 'Collapse'),
		   menuCommand(expand_all,
			       'ExpandAll',
			       label := 'Expand all'),
		   menuCommand(add_child,
			       'NodeAddChild',
			       label := 'Add child...',
				   end_group := @on),
		   menuCommand(delete,
			       'SelectionDelete',
			       label := 'Delete')		
		  ]).	
%%

/*******************CHANGE REQUESTORS*******************/
%%

changeApplied_newHObjectChild(EE,
			      CR:changeRequestor
			     ):->
	%Onze reaktie op de add child change requestor
	%is het iets voor ons (zelfde klasse object als onze root?)
    %gp3 0.1: removed code that selects the new child object [klein1]
	EE?top?class->>equal(CR?result?class),
	ChildNode = EE<<-nodeForEntity(CR?result),
	ParentNode = EE<<-nodeForEntity(CR?object), %die is er al
	ParentNode->>son(ChildNode),
	ChildNode->>updateDisplay,
	EE->>sortTree(ParentNode).
%%

%%
changeApplied_changeHObject(EE,
				CR:changeRequestor
			       ):->
	%onze reaktie bij een verandering van naam/plaats in hierarchie
	EE?top?class->>equal(CR?object?class), %zelfde klasse?
	%naamwijziging is eenvoudig:
	Node = EE<<-nodeForEntity(CR?object),
	Node->>updateDisplay,
	%gp3 0.13: This only if the object has a parent
	if
	    Parent = CR?object<<-parent
	then
	(
	    PNode = EE<<-nodeForEntity(Parent),
	    PNode->>move(Node),
	    EE->>sortTree(PNode),
	    PNode->>collapsed(@off)
	).
%%

%%
changeApplied_deleteHObjectTree(EE,
	CR:changeRequestor):->
	%we moeten de tree verwijderen, dat doen we door te unrelaten
	%en uit de mapping te halen
	%op dit moment zijn alle objecten er nog (autofree door CR).
	EE?top?class->>equal(CR?object?class), %zelfde klasse?
	Node = EE<<-nodeForEntity(CR?object),
	ParentNode = EE<<-nodeForEntity(CR?object?parent),
	ParentNode->>unrelate(Node),
	M = EE<<-mappings,
	CR?object->>for_some_down(->>(M,delete,@arg1)).
%%

%%
changeApplied_setCurrentLanguage(EE, _CR: changeRequestor):->
	%gp3 1.4.0 jj: redraw
	EE?tree->>for_all(->>(@arg1,updateDisplay)).
%%	

	
/******************** MULTIPLE MODELS ***********************/ %JL

/* Make the right model active */
input_focus(T, Boolean) :->
	send(@app, selectCorrectTab, T), % T is the window object (of the editor)
	send_super(T, input_focus, Boolean).

	
/********************HELPERS***********************/
%%
nodeForEntity(EE, Object : hierarchicalObject = hierarchicalObject, Node : entityNode) :<-
	"Find the node for the given entity, or create one if it doesn't exist" ::

	(
	Node = EE?mappings<<-member(Object), ! %de node bestaat
	)
	;
	(   
	%de Node bestaat niet, maak hem dus en voeg hem toe aan de mappings
	%dus eerst het figuur maken
	Node *= entityNode(Object),
	send(EE?mappings,append,Object,Node)
	).
%%

%%
treeDirection(EE, Direction : {horizontal, vertical, list}) :<-
	"Get the direction of the entity tree" ::

	get(EE?tree,direction,Direction).
%%

%%
treeDirection(EE, Direction : {horizontal, vertical, list}) :->
	"Set the direction of the entity tree" ::
	%slaan we op in de entity van de rootnode

	send(EE?tree,direction,Direction),
	EE?tree?root?element->>attribute(entityEditDirection,Direction).
%%	
	
%%
nodeSelected(EE):->
	"Succeed if there is 1 node selected" ::
	1 = EE?tree?selection<<-size.
%%

%%
selectedNode(EE, Node : entityNode):<-
	"Returns the (single) selected node, fails if none" ::
	send(EE,nodeSelected),
	get(EE?tree?selection?head,node,Node).
%%

%%
someSelection(EE):->
	"Succeeds if the is something selected" ::

        S = EE?tree?selection<<-size,
	S > 0.
%%

:-pce_end_class.

:-pce_begin_class(
		  entityNode,
		  node,
		  "Implements the entity node"
		 ).
		 
initialise(EN, HO: hierarchicalObject) :->
	%doet bijna niets! updateDisplay moet aangeroepen worden door caller, na toevoegen aan tree!

	EN->+initialise(new(graphical)),
	%gp3 1.4: dynamic tooltip
	EN->>tooltip(@default,model),
	EN->>hyper(HO,hierarchicalObject,entityNode).
		
	
updateDisplay(EN) :->
	%update de display, ook voor de eerste keer
	%moet aangeroepen worden na toevoegen aan de tree

	S = EN<<-selected,
	EL = EN<<-element,
	(   0 = EL?remarks<<-size
	->  DName = EL<<-name
	;   DName = EL?name<<-append('*')
	),
	T *= text(DName,center,small),
	T->>name(label_text),
	T->>font( italic),
	
	F *= figure,
	I = EN<<-bitmap,
	F->>display(I),
	F->>display(T,point(0,I?height)), %tekst eronder
	_ *= constraint(I,T,identity(center_x)), %tekst altijd gecentreerd
	EN->>image(F),
	EN->>selected(S).
	
	
element(EN, HO: hierarchicalObject) :<-
	%return het bijbehorende hierarchicalObject
	HO = EN<<-hypered(hierarchicalObject).

%vind het juiste plaatje bij dit element, en geeft het ff de goede kleur
bitmap(EN, B: psBitmap):<-
	%gp3 0.3: filename depends on collapsed state
	%gp3 0.3: changed class to psBitmap, so it can write in the eps
	
	C *= string('%s%s',
		EN?element?class_name,
		when(EN?collapsed == @on, '_collapsed','')),
	get_image(entity_edit,C,I), %gp3: named after class, no longer mono
	B *= psBitmap(I,@on),
	B->>psdef(C). %save as definition for ps
		
collapsed(EN, State: bool*):->
	%overwrite: even de display updaten
	
	EN->+collapsed(State),
	EN?element->>attribute(entityEditCollapsed,State),
	EN->>updateDisplay.

checkCollapsed(EN):->
	%update de collapsed als de huidige state anders is dan de opgeslagen state
	
	C = EN?element<<-attribute(entityEditCollapsed),
	\+ C = EN<<-collapsed,
	EN->>collapsed(C).
%
checkCollapsed(_EN):->
	true.
%%

%%
tooltipContent(EN,S: string):<-
	%gp3 1.4 added a dynamic tooltip
	
	EL = EN<<-element,
	C = EL<<-relevantComments,
	if
		0 = C<<-size
	then
		S = string('(no comments)')
	else
	(
		S = C<<-strip
	).
%%

:-pce_end_class.

