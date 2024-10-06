/*
Definitie viewEditor class
Editor voor modelfragments en input systems

Part of Garp3, see copyright notice
Most code old dutch homer code

2000-2005 Jelmer Jellema, Spin in het Web, spininhetweb.nl
*/                                                                                                                                                                                                                            

:-pce_begin_class(
		  viewEditor,
		  framedWindow,
		  "Implements the view editor main frame"
		 ).

variable(fragment,modelFragment,get, "the edited model fragment").

variable(moveSelect,gesture,get,"The saved click=select / move gesture").
variable(moveSelectSingle,gesture,get,"The saved click=select / move-single gesture"). %gp3 0.3

variable(toggleSelect,click_gesture,get,"The saved shift-click = toggle select gesture").

variable(toggleSelect_alternative, click_gesture, get, "Alternative for shift_click: middle_click").

variable(contextMenu, contextPopup,get,"The context menu").

variable(mustUpdateVisualisation, bool, get). %gp3: @on when it is needed to update visualisation in de next changeTreeApplied call (see mustUpdateVisualisation) 
%%
initialise(VE,
	   Fragment: fragment = modelFragment
	   ) :->
	"Create an frament editor and open it" ::

	VE->>slot(fragment,Fragment), %moet eerder ivm label
	VE ->+ initialise(
			  VE?makeWindowLabel
			  ,buttonbar := vertical
			  , helpId := when(->>(Fragment,isInputSystem),'Build_ScenarioEditor','Build_MFEditor')
			),
	VE->>icon(@build_icon),
	
	VE->>init_commands,
	VE->>init_menu,
	VE->>init_buttonbar,
	VE->>init_handlers,
	VE->>update_visualisation(@on), 
	VE->>slot(mustUpdateVisualisation,@off),
	VE?client->>onScroll(->>(VE,saveLayoutInfo)), %gp3 1.0, make sure all relative positions are resaved when origin changes
	VE->>open,

	VE->>position(point(@display?width *0.15,@display?height * 0.15)),
	MaxSize *= size(@display?width * 0.80,
		       @display?height * 0.80),
	DefSize *= size(550,480),
	(
	    /* The model fragment has been opened before */
	    SavedSize = Fragment<<-layOutInfo(Fragment, editSize, new(chain), @off) ->
	    FirstTime = @off
	;
	    /* The model fragment is opened for the first time */
	    SavedSize = Fragment<<-layOutInfoDefault(Fragment,editSize, DefSize) ->
	    FirstTime = @on
	), 
	%bepaal de grootte: er kan opgeslagen zijn dat het window kleiner moet
	%groter mag echter niet

	Width *= number(MaxSize?width),
	Width->>minimum(SavedSize?width),
	Height *= number(MaxSize?height),
	Height->>minimum(SavedSize?height),
	VE->>size(size(Width,Height)),

	/* Make sure the submodel fragments are not shown the first time the model fragment is opened */
	(
	    FirstTime == @on ->
	    % Hide the sub model fragments
	    get(Fragment, parentReferences, PR),
	    send(PR, for_all, 
		if(
		    message(@prolog, isPredefinedModelFragment, @arg1?referencedFragment),
		    message(PR, delete, @arg1),
		    message(@prolog, true)
		)
	    ),
	    (
		/* There is actually an inherited model fragment */
		get(PR, head, PRHead) ->
		get(VE, findElements, PRHead, @default, Elements),
		get(Elements, head, ImportedMFVisual),
		/* Do not show the subfragments of the inherited model fragment */
		send(ImportedMFVisual, subMFsShown, @off),
		send(VE, update_visualisation, @on)
	    ;
		true
	    )
	;
	    FirstTime == @off ->
	    true
	), 
	%we want some space left en top:
	VE?client->>scroll_to(point(-20,-15)),
	
	% Multiple model support
	get(@model, '_value', GarpModel),
	send(VE, associateModel, GarpModel).

%%

%%
destroy(VE):->
	"Destroy the editor" ::

	%we slaan layout-info op bij de elementen
	VE->>saveLayoutInfo,
	VE->+destroy.
%%

%%
saveLayoutInfo(VE):->
	"Save info about the lay-out of elements and editor in model" ::
	
	VE?fragment->>clearLayOutInfo, %gp3 0.3: clear old information
 	
	All = VE<<-all_named_hypered(element),
	All->>for_some(->>(@arg1,
			  saveLayoutInfo)), %can fail
	%en zelf slaan we ook het een en ander op
	%dit doen we gewoon in het fragment zelf, met het fragment zelf als verwijzing
	VE?fragment->>layOutInfo(VE?fragment,
						editSize,
						VE?size).

%%
					     
%%
init_menu(VE
	  ) :->
	"Initialise menu-structures" ::
	%de code is gesplitst voor input systems en "gewone" modelfragmenten
	%zie ook hieronder bij init_commands
	
	%clause 1: input systems
	VE?fragment->>isInputSystem,!,
	
	send_list(VE?menubar, append, %zie framedWindow
		  [
		   new(File,popup('File')),
		   new(Edit,popup('Edit')),
		   new(Elements, popup('Elements')),
		   new(View,popup('View'))
		  ]),
	send_list(File,append,
		[
		    menuCommand(fragmentProperties,
			    'FragmentProperties','Scenario Properties...',
			    end_group := @on),
		    menuCommand(eps,'SaveEPS', label := 'Save Diagram to EPS file'), %gp3 0.3
		    menuCommand(saveModel,'SaveModel','Save model to disk')
		]),

	send_list(Edit,append, 
		  [
		   menuCommand(delete,
			       'Delete', 'Delete'),
		   menuCommand(properties,
			       'Properties','Properties...',end_group := @on)
		  ]),

	Elements->>colour(blue),

	send_list(Elements,append,
		  [
		   menuCommand(entityinstance,
			       'GivenEntityInstance','Entity...'),
		   menuCommand(attribute,
			       'GivenAttribute','Attribute...'),	   
		   menuCommand(configuration,
			       'GivenConfiguration','Configuration...',
			       end_group := @on),
		   menuCommand(quantity,
			       'GivenQuantity','Quantity...'),
		   menuCommand(value,
			       'GivenValue','Value', end_group := @on),
		  new(Plus2,menuCommand(plus,'AddPlus','Plus...')),
		  new(Min2,menuCommand(min,'AddMin','Min...')),
		   menuCommand(inequality,
			       'GivenInequality','Inequality...',
			       end_group := @on),
		   menuCommand(assumption, 'AddAssumption',
			       'Assumption...'),	%hier dus given!!!
		   menuCommand(agentinstance,
			       'GivenAgentInstance','Agent...')			
		  ]
		  ),
	Plus2->>colour(black),
	Min2->>colour(black),

	send_list(View,append,
		[
			menuCommand(collapse,
				'Collapse','Collapse'),
			menuCommand(expand,
				'Expand','Expand'),
			menuCommand(collapseRelations,
				'CollapseRelations',
				'Collapse relations'),
			menuCommand(expandRelations,
				'ExpandRelations',
				'Expand relations',
				end_group := @on),
			menuCommand(showRelevant,
				'ShowRelevant',
				'Show relevant'),
    			menuCommand(fullRedraw,
				'FullRedraw',
				'Full redraw (show all, default placing)'),
			menuCommand(expandAll,
				'ExpandAll',
				'Expand all',
				end_group := @on),
			menuCommand(hide,
				'Hide','Hide', end_group := @on),
			menuCommand(translations, 'ViewTranslations', label := 'Translations...'),%gp3 1.4.0
			menuCommand(tooltips,'ToggleTooltips') %gp3 1.4.0 (dynamic label)
		]).
%
init_menu(VE
	  ) :->

	%clause 2: model fragments
	\+ VE?fragment->>isInputSystem,
	
	send_list(VE?menubar, append, %zie framedWindow
		  [
		   new(File,popup('File')),
		   new(Edit,popup('Edit')),
		   %conditie en consequentie menus zijn gewone popups
		   %wel zitten er command objecten
		   %in. Mooier zou zijn om die popups ok te vullen
		   %maar dat is van later zorg (kan gewoon met
		   %menuPopupCommand)
		   new(Condition, popup('Conditions')),
		   new(Consequence, popup('Consequences')),
		   new(View,popup('View'))
		  ]),
		send_list(File,append,
		    [
		    menuCommand(fragmentProperties, 'FragmentProperties','Model Fragment Properties...',
			    end_group := @on),
		    menuCommand(eps,'SaveEPS', label := 'Save Diagram to EPS file'), %gp3 0.3
		    menuCommand(saveModel,'SaveModel','Save model to disk')
		    ]),
		send_list(Edit,append, 
		  [
		   menuCommand(delete,
			       'Delete', 'Delete'),
		   menuCommand(properties,
			       'Properties','Properties...')
		  ]),

	Condition->>colour(red),
	Consequence->>colour(blue),
	send_list(Condition,append,
		  [
		   menuCommand(entityinstance,
			       'ConditionEntityInstance','Entity...'),
		   menuCommand(attribute,
			       'ConditionAttribute','Attribute...'),
		   menuCommand(configuration,
			       'ConditionConfiguration','Configuration...',
			       end_group := @on),
		   menuCommand(quantity,
			       'ConditionQuantity','Quantity...'),
		   menuCommand(value,
			       'ConditionValue','Value', end_group := @on),
		   new(Plus1,menuCommand(plus,'AddPlus','Plus...')),
		   new(Min1,menuCommand(min,'AddMin', 'Min...')),
		   menuCommand(inequality,
			       'ConditionInequality','Inequality...',
			       end_group := @on),
		   menuCommand(assumption, 'AddAssumption',
			       'Assumption...'),
		   menuCommand(agentinstance,
			       'ConditionAgentInstance','Agent...'),				   
		   menuCommand(fragment,
			       'AddModelFragment','Model fragment...',
			       end_group := @on),
		   menuCommand(identity,'AddIdentity',
			       'Identity...')
		  ]
		 ),
	Plus1->>colour(black),
	Min1->>colour(black),
	send_list(Consequence,append,
		  [
		   menuCommand(entityinstance,
			       'GivenEntityInstance','Entity...'),
		   menuCommand(attribute,
			       'GivenAttribute','Attribute...'),
		   menuCommand(configuration,
			       'GivenConfiguration','Configuration...',
			       end_group := @on),
		   menuCommand(quantity,
			       'GivenQuantity','Quantity...'),
		   menuCommand(value,
			       'GivenValue','Value',end_group := @on),
		  new(Plus2,menuCommand(plus,'AddPlus','Plus...')),
		  new(Min2,menuCommand(min,'AddMin','Min...')),
		   menuCommand(inequality,
			       'GivenInequality','Inequality...',
			       end_group := @on),
		   menuCommand(correspondence,
			       'AddCorrespondence','Correspondence...'),
		   menuCommand(proportionality,
			       'AddProportionality','Proportionality...'),
		   menuCommand(influence,
			       'AddInfluence','Influence...',
			       end_group := @on)
		  ]
		  ),
	Plus2->>colour(black),
	Min2->>colour(black),

	send_list(View,append,
		[
			menuCommand(toggleSubfragments,
				'ToggleSubfragments',
				'Show subfragments',end_group:= @on),
			menuCommand(collapse,
				'Collapse','Collapse'),
			menuCommand(expand,
				'Expand','Expand'),
			menuCommand(collapseRelations,
				'CollapseRelations',
				'Collapse relations'),
			menuCommand(expandRelations,
				'ExpandRelations',
				'Expand relations',
				end_group := @on),
			menuCommand(showRelevant,
				'ShowRelevant',
				'Show relevant'),
			menuCommand(fullRedraw,
				'FullRedraw',
				'Full redraw (show all, default placing)'),
			menuCommand(expandAll,
				'ExpandAll',
				'Expand all',
				end_group := @on),
			menuCommand(hide,
				'Hide','Hide', end_group := @on),
			menuCommand(translations, 'ViewTranslations', label := 'Translations...'),%gp3 1.4.0
			menuCommand(tooltips,'ToggleTooltips') %gp3 1.4.0 (dynamic label)
		]).
%
%%

%%
init_buttonbar(VE):->
	%gp3 0.2: create the elements for the buttonbar
	%version for scenarios
	
	VE?fragment->>isInputSystem,
	B = VE<<-buttonBar,
	B<<-add(cq1,'GivenEntityInstance',buttonbars,cq1,cq1g,'Add entity'),
	B<<-add(cq2,'GivenAttribute',buttonbars,cq2,cq2g,'Add attribute',below,cq1),
	B<<-add(cq3,'GivenConfiguration',buttonbars,cq3,cq3g,'Add configuration',below,cq2),	
	B<<-add(cq4,'GivenQuantity',buttonbars,cq4,cq4g,'Add quantity',below,cq3),
	B<<-add(cq5,'GivenValue',buttonbars,cq5,cq5g,'Add value',below,cq4),	
	B<<-add(cq6,'AddPlus',buttonbars,cq6,cq6g,'Add plus',below,cq5),
	B<<-add(cq7,'AddMin',buttonbars,cq7,cq7g,'Add min',below,cq6),
	B<<-add(cq8,'GivenInequality',buttonbars,cq8,cq8g,'Add inequality',below,cq7),
	B<<-add(cq13,'AddAssumption',buttonbars,cq13,cq13g,'Add assumption',below,cq8),
	B<<-add(cq14,'GivenAgentInstance',buttonbars,cq14,cq14g,'Add agent',below,cq13).
%
init_buttonbar(VE):->
	%gp3 0.2: create the elements for the buttonbar
	%version for model fragments
	
	\+ VE?fragment->>isInputSystem,
	B = VE<<-buttonBar,
	B<<-add(cd1,'ConditionEntityInstance',buttonbars,cd1,cd1g,'Add entity as condition'),
	B<<-add(cq1,'GivenEntityInstance',buttonbars,cq1,cq1g,'Add entity as consequence',right,cd1),
	B<<-add(cd2,'ConditionAttribute',buttonbars,cd2,cd2g,'Add attribute as condition',below,cd1),
	B<<-add(cq2,'GivenAttribute',buttonbars,cq2,cq2g,'Add attribute as consequence',right,cd2),
	B<<-add(cd3,'ConditionConfiguration',buttonbars,cd3,cd3g,'Add configuration as condition',below,cd2),
	B<<-add(cq3,'GivenConfiguration',buttonbars,cq3,cq3g,'Add configuration as consequence',right,cd3),	
	B<<-add(cd4,'ConditionQuantity',buttonbars,cd4,cd4g,'Add quantity as condition',below,cd3),
	B<<-add(cq4,'GivenQuantity',buttonbars,cq4,cq4g,'Add quantity as consequence',right,cd4),
	B<<-add(cd5,'ConditionValue',buttonbars,cd5,cd5g,'Add value as condition',below,cd4),
	B<<-add(cq5,'GivenValue',buttonbars,cq5,cq5g,'Add value as consequence',right,cd5),	
	B<<-add(cd6,'AddPlus',buttonbars,cd6,cd6g,'Add plus',below,cd5),
	B<<-add(cq6,'AddPlus',buttonbars,cq6,cq6g,'Add plus',right,cd6),
	B<<-add(cd7,'AddMin',buttonbars,cd7,cd7g,'Add min',below,cd6),
	B<<-add(cq7,'AddMin',buttonbars,cq7,cq7g,'Add min',right,cd7),
	B<<-add(cd8,'ConditionInequality',buttonbars,cd8,cd8g,'Add inequality as condition',below,cd7),
	B<<-add(cq8,'GivenInequality',buttonbars,cq8,cq8g,'Add inequality as consequence',right,cd8),
	B<<-add(cd9,'AddAssumption',buttonbars,cd9,cd9g,'Add assumption as condition',below,cd8),
	B<<-add(cq9,'AddCorrespondence',buttonbars,cq9,cq9g,'Add correspondence as consequence',right,cd9),
	B<<-add(cd10,'ConditionAgentInstance',buttonbars,cd10,cd10g,'Add agent as condition',below,cd9),
	B<<-add(cq10,'AddProportionality',buttonbars,cq10,cq10g,'Add proportionality as consequence',right,cd10),
	B<<-add(cd11,'AddModelFragment',buttonbars,cd11,cd11g,'Add model fragment as condition',below,cd10),
	B<<-add(cq11,'AddInfluence',buttonbars,cq11,cq11g,'Add influence as consequence',right,cd11),
	B<<-add(cd12,'AddIdentity',buttonbars,cd12,cd12g,'Add identity',below,cd11).
%%

/*******************ACTIES*****************************/
%%
init_handlers(VE
	      ):->
	"Initialise the event handlers" ::
	%zie beneden bij Acties voor de implementatie van de action_... calls
	%de handlers worden opgeslagen voor hergebruik
	%(niet als pce_global, maar gewoon als member)

	%wat functies
	Element *= @arg1?receiver,
	Frame *= Element?frame,
	
	%een move_gesture die meteen ook selecteert
	%Move *= move_gesture(left),
	Move *= multi_move_gesture(VE?getMultiMove,
		->>(VE,dragMultiMove,@arg1,@arg2,@on),left,
		endMessage := ->>(VE,saveLayoutInfo)),  %last arg: gp3 1.0
	Move->>condition(->>(Frame,
			     action_singleSelection,
			     Element)),
	VE->>slot(moveSelect,Move),

	%gp3: the same but with control key only moves an object, not its
	%moveablesubs
	MoveSingle *= multi_move_gesture(VE?getMultiMove,
		->>(VE,dragMultiMove,@arg1,@arg2,@off),left,c,
		endMessage := ->>(VE,saveLayoutInfo)), %last arg: gp3 1.0
	MoveSingle->>condition(->>(Frame,
			     action_singleSelection,
			     Element)),
	VE->>slot(moveSelectSingle,MoveSingle),

	%en de toggle versie voor select
	VE->>slot(toggleSelect,click_gesture(left,
					      's',
					      single,
					      ->>(Frame,
						action_toggleSelect,
						  Element))),
	VE->>slot(toggleSelect_alternative,click_gesture(middle,'',
							single,
							->>(Frame,action_toggleSelect,Element))),
	%context menu
	VE->>slot(contextMenu, contextPopup('ContextPopup',
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
	VE?client->>recogniser(SGNormaal),
	%de versie met shift heeft geen conditie, zodat
	%oude selectie blijft bestaan
	SGAdd *= select_gesture(left,
				's',
				Select,
				Deselect),
	VE?client->>recogniser(SGAdd).
%%

%%
getMultiMove(VE,
	M : chain):<-
	"Callback for multi_move_gesture" ::
	%welke graphicals moeten bij een multi move bewegen
	%dit zijn in principe de graphicals die geselecteerd zijn
	%maar als van een geselecteerde graphical ook een moveablesub is geselecteerd
	%dan kan die moveablesub weg, want die gaat dan wel mee
	%anders dubbel

	S = VE<<-selection, 
	M = S<<-copy,
	Current *= var,
	S->>for_all(if(->>(M,member,@arg1),		%moet er nog zijn
				and(assign(Current,@arg1),
					->>(M,for_all,
							if(and(@arg1 \== Current,
									->>(Current,isMoveTogetherSub,@arg1)),
								->>(M,delete,@arg1)))))).
%%

%%
dragMultiMove(VE,Change: point, Dragged: chain, DragMoveableSubs: bool):->
	%callback van multi_move_gesture bij een drag
	%in Dragged zit al de lijst met te verplaatsten graphicals
	%(volgens getMultiMove van hierboven)
	
	%gp3 0.3 added DragMoveableSubs argument
	
	Moved *= chain,
	Dragged->>for_all(->>(@arg1,dragMove,Change,Moved,DragMoveableSubs)),

	%in Moved zitten nu alle verplaatste objecten
	%eens kijken of er relaties en calculi zijn die maar beter mee kunnen
	
	%gp3 1.0: there may be relations between calculi etc. So we keep doing this until 
	%no new element moves
	RelCalc = VE?elements<<-find_all(or(
		->>(@arg1,instance_of,visualRelationElement),
		->>(@arg1,instance_of,calculusElement))),
		
	Looper *= var(value := @on),
	while(Looper == @on,
		and(
			assign(Looper,@off),
			if(->>(RelCalc,find,->>(@arg1,checkDragMove,Change,Moved)),
				assign(Looper,@on)
			)
		)
	)->>forward.
%%
			
%%
action_removeSelection(VE):->
	"Implement remove selection action"::

	VE?client->>selection(@nil).
%%

%%
action_singleSelection(VE, E: graphical):->
	"Implement single selection action"::
	%selecteren van deze graphical, maar niet als dat al zo is 
	%dan blijft dus de multiple-selection intakt

	\+ @on = E<<-selected
	-> VE?client->>selection(E)
	;  true.
%%

%%
action_toggleSelect(_VE, E: graphical):->
	"Implement toggle select action" ::

	E->>selected(E?selected?negate).
%%


%%
action_multiselection_select(_VE,E: graphical):->
	"Implement select callback for select_gesture" ::
	%het element wordt geselecteerd. Maar als ie dat al is dan laten we de call
	%falen: de gesture zal dan niet de deselect callback gebruiken voor dit element
	%zodat het ding geselecteerd blijft (voor de shift variant)

	\+ @on = E<<-selected,
	E->>instance_of(visualElement), %alleen visual elements
	E->>selected(@on).
%%

%%
action_multiselection_deselect(_VE, E: graphical):->
	"Implement deselect callback for select_gesture" ::

	E->>selected(@off).
%%
/******************COMMANDS**************************/
%%
init_commands(VE
	      ) :->
	"Initialise command objects" ::
	%initialiseer de commando s. Zie framedwindow.pl en command.pl
	%opgesplitst voor inputsystems en gewone modelfragmenten
	%om te zorgen dat toetsen van niet zichtbare commands het ook niet doen
	
	%1: inputsystems
	
	VE?fragment->>isInputSystem,!,
	
	VE->>command('Delete',key := 'DEL', keystring := '[DEL]', otherkeys := chain(backspace)), %verwijder de selectie
	VE->>command('Properties',key := 'RET', keystring := '[ENTER]'),
	VE->>command('FragmentProperties'),
	VE->>command('SaveEPS'), %gp3 0.3
	VE->>command('SaveModel', key := '\\C-s', keystring := '[Ctrl + S]'),
		
	VE->>command('GivenAgentInstance', key := '\\C-a', keystring := '[Ctrl + A]'), 
	VE->>command('GivenEntityInstance',key := '\\C-e', keystring := '[Ctrl + E]'),
	VE->>command('AddAssumption',key := '?', keystring := '[?]'),
	VE->>command('GivenAttribute',key := '.', keystring := '[.]'),
	VE->>command('GivenQuantity',key := '\\C-q', keystring := '[Ctrl + Q]'),
	VE->>command('GivenValue',key := '\\C-v', keystring := '[Ctrl + V]'),
	VE->>command('GivenConfiguration',key := '\\C-c', keystring := '[Ctrl + C]'),
	VE->>command('GivenInequality',key := '\\C-i', keystring := '[Ctrl + I]'),


	VE->>command('AddPlus',key := '+', keystring := '[+]'),
	VE->>command('AddMin',key := '-', keystring := '[-]'),

	VE->>command('Collapse', key := 'SPC', keystring := '[SPACE]'),
	VE->>command('Expand', key := '\\C-@', keystring := '[Ctrl + SPACE]'),
	VE->>command('Hide', key := 'h', keystring := '[H]'),	
	VE->>command('ExpandAll', key := '*', keystring := '[*]'),
	VE->>command('ShowRelevant', key := '\\C-r', keystring := '[Ctrl + R]'),	
	VE->>command('FullRedraw'),	%gp3 1.0
	VE->>command('ViewTranslations',key := '\\C-t', keystring := '[Ctrl + T]'), %%gp3 1.4.0

		send_list(VE,command,
		  [
		   'CollapseRelations',
		   'ExpandRelations',
		   'ToggleTooltips' %gp3 1.4.0
		  ]
		 ),

	%commands voor het afhandelen van de menu-balk, deze runt dus niet
	%maar de update command is handig voor het vullen van het menu enzo

	VE->>command('ContextPopup',runnable := @off).
%
init_commands(VE
	      ) :->
	
	%2: modelfragments
	
	\+ VE?fragment->>isInputSystem,
	
	VE->>command('Delete',key := 'DEL', keystring := '[DEL]', otherkeys := chain(backspace)), %verwijder de selectie
	VE->>command('Properties',key := 'RET', keystring := '[ENTER]'),
	VE->>command('FragmentProperties'),
	VE->>command('SaveEPS'), %gp3 0.3
	VE->>command('SaveModel', key := '\\C-s', keystring := '[Ctrl + S]'),
		
	VE->>command('ConditionEntityInstance',key := 'e', keystring := '[E]'),
	VE->>command('ConditionAgentInstance',key := 'a', keystring := '[A]'),
	VE->>command('GivenEntityInstance',key := '\\C-e', keystring := '[Ctrl + E]'),
	VE->>command('AddAssumption',key := '?', keystring := '[?]'),
	VE->>command('ConditionAttribute',key := ',', keystring := '[,]'),
	VE->>command('GivenAttribute',key := '.', keystring := '[.]'),
	VE->>command('ConditionQuantity',key := 'q', keystring := '[Q]'),
	VE->>command('GivenQuantity',key := '\\C-q', keystring := '[Ctrl + Q]'),
	VE->>command('ConditionValue',key := 'v', keystring := '[V]'),
	VE->>command('GivenValue',key := '\\C-v', keystring := '[Ctrl + V]'),
	VE->>command('ConditionConfiguration',key := 'c', keystring := '[C]'),
	VE->>command('GivenConfiguration',key := '\\C-c', keystring := '[Ctrl + C]'),
	VE->>command('ConditionInequality',key := 'i', keystring := '[I]'),
	VE->>command('GivenInequality',key := '\\C-i', keystring := '[Ctrl + I]'),

	VE->>command('AddPlus',key := '+', keystring := '[+]'),
	VE->>command('AddMin',key := '-', keystring := '[-]'),

	VE->>command('AddCorrespondence',key := '=', keystring := '[=]'),
	VE->>command('AddProportionality',key := '/', keystring := '[/]'),
	VE->>command('AddInfluence',key := '>', keystring := '[>]'),

	VE->>command('AddModelFragment',key := 'm', keystring := '[M]'),
	
	VE->>command('Collapse', key := 'SPC', keystring := '[SPACE]'),
	VE->>command('Expand', key := '\\C-@', keystring := '[Ctrl + SPACE]'),
	VE->>command('Hide', key := 'h', keystring := '[H]'),	
	VE->>command('ExpandAll', key := '*', keystring := '[*]'),
	VE->>command('ShowRelevant', key := '\\C-r', keystring := '[Ctrl + R]'),	
	VE->>command('FullRedraw'),	%gp3 1.0
	VE->>command('ViewTranslations',key := '\\C-t', keystring := '[Ctrl + T]'), %%gp3 1.4.0

		send_list(VE,command,
		  [
		   'AddIdentity',	   
		   'CollapseRelations',
		   'ExpandRelations',
		   'ToggleTooltips', %gp3 1.4.0
		   'ToggleSubfragments', %gp3 0.3: command to show icons for conditionalfragments in imported fragments
		   'Refinement' %gp3 0.3: command to refine an imported mf or remove the refinement
		  ]
		 ),

	%commands voor het afhandelen van de menu-balk, deze runt dus niet
	%maar de update command is handig voor het vullen van het menu enzo

	VE->>command('ContextPopup',runnable := @off).
%%	

/***ALGEMENE COMMANDO'S**********/

%%gerelateerd aan het ContextPopup commando, voor het contextmenu
%geen check
%geen run want runnable := @off (zie boven)

updateFillPopupContextPopup(VE,Popup) :->
%het vullen vh menu, eerst de algemene dingen, dan afhankelijk van het geselecteerde object(??)
	%gp3 0.3 changed this: we added the new toggleSubfragments command
	%but only when the selection is a modelfragment
	
	send(Popup,clear),
	Popup->>append(menuCommand(properties,
			       'Properties','Properties...',end_group := @on)), %allways first
			       
	%gp3: for imported fragments, add some
	if
	(
		importedMFElement = VE?singleSelection<<-name
	)
	then
	(
		Popup->>append(menuCommand(toggleSubfragments,
						'ToggleSubfragments','Show subfragments')),
				%right label is set in info call
		Popup->>append(menuCommand(refinement,
					'Refinement','Refine...',end_group:= @on))
	),
	
	send_list(Popup,append,
		  [
			menuCommand(collapse,
				'Collapse','Collapse'),
			menuCommand(expand,
				'Expand','Expand'),
			menuCommand(collapseRelations,
				'CollapseRelations',
				'Collapse relations'),
			menuCommand(expandRelations,
				'ExpandRelations',
				'Expand relations'),
			menuCommand(showRelevant,
				'ShowRelevant',
				'Show relevant'),
			menuCommand(expandAll,
				'ExpandAll', 'Expand All'),
			menuCommand(hide,
				'Hide','Hide', end_group := @on),
		   menuCommand(delete,
			       'Delete', 'Delete')
		  ]).	

infoDefaultItemContextPopup(_EE,
			     DefaultItem: 'any*'):<-
	%We moeten de value van het default item teruggeven voor de contextPopup

	DefaultItem = properties. %altijd

/***********************************/
%%fragmentProperties

onFragmentProperties(VE):->
	if
		VE?fragment->>isInputSystem
	then
		Dlg *= inputSystemPropsDlg(VE)
	else
		Dlg *= mfPropsDlg(VE),
	Dlg->>editObject(VE?fragment).
%%

%%saveModel
%%
checkSaveModel(_VE):->
	% Save even if the model has not changed!
	true.
	%@on = @model<<-changed.
%
onSaveModel(_VE):->
	%gp3 just tell the app we want to save
	@app->>save.
%%

%%saveEPS

onSaveEPS(E):->
	%gp3 0.3: save the graph to file
	%copied code from class_visigarp
	
	if
		get(@garp3_finder, file, E,  'Save Diagram to EPS file', @off, '.eps', FileName)
	then
	(
		unless
			catch(
			(
				Pict = E<<-client, %gp3
				new(File, file(FileName)),
				send(File, open, write),
				send(File, append, Pict?postscript),
				send(File, close),
				send(File, done),
				send(E, statusText, string('Saved PostScript in %s', FileName)) %gp3 0.3 changed this from report
			)
			,_,fail)
		do
			E->>statusText('Save failed!')
	).
%%

/******************** MULTIPLE MODELS ***********************/ %JL
/* Make the right model active */
input_focus(T, Boolean) :->
	send(@app, selectCorrectTab, T), % T is the window object (of the editor)
	send_super(T, input_focus, Boolean).
%%



/*****LOSSE OBJECTEN ED *********/

%%ConditionEntityInstance
checkConditionEntityInstance(VE):->
	%mag in alle fragmenten, maar niet in inputSystem

	checkEditorState(VE,
			 any,
			 inputSystem,
			 none,
			 any).
%
onConditionEntityInstance(VE):->
	%toevoegen van een entityinstantie als conditie
	%we openen de instance dialoog
	
	if
		VE->>checkGivenEntityInstance
	then
		CanSwitch = @on
	else
		CanSwitch = @off,
	new(instancePropsDlg(VE))->>newObject(entity,condition,CanSwitch).
%%	

%%given entity instance
checkGivenEntityInstance(VE):->
	%mag alleen in proces en agent fragmenten en in inputsystems
	checkEditorState(VE,
			 [processFragment,agentFragment,inputSystem],
			 any,none,any).
%
onGivenEntityInstance(VE):->
	%toevoegen van een entityinstantie als conditie
	%we openen de instance dialoog
	
	if
		VE->>checkConditionEntityInstance
	then
		CanSwitch = @on
	else
		CanSwitch = @off,
	new(instancePropsDlg(VE))->>newObject(entity,consequence,CanSwitch).

%%
%%ConditionAgentInstance
checkConditionAgentInstance(VE):->
	%mag alleen in agent fragmenten

	checkEditorState(VE,
			 agentFragment,
			 any,
			 none,
			 any).
%
onConditionAgentInstance(VE):->
	%toevoegen van een agentinstance als conditie
	%we openen de instance dialoog
	
	new(instancePropsDlg(VE))->>newObject(agent,condition,@off). %nooit given
%%	

%%GivenAgentInstance
checkGivenAgentInstance(VE):->
	%mag alleen in inputsystems

	checkEditorState(VE,
			 inputSystem,
			 any,
			 none,
			 any).
%
onGivenAgentInstance(VE):->
	%toevoegen van een agentinstance als consequencde
	%we openen de instance dialoog
	
	new(instancePropsDlg(VE))->>newObject(agent,consequence,@off).
%%	

%gp3 0.1 addAssumption can now be used either without any selection
%or with a garpInstance selected 

checkAddAssumption(VE):->
	VE?selection->>empty %we decide on the condition/consequence state when applying the CR
	;
	checkEditorState(VE,
			 [staticFragment,processFragment,agentFragment],
			 	any,
		      1,[[instanceElement/[condition,imported,parent]]]) %in mfs only conditional
	;
	checkEditorState(VE,inputSystem,any,1,[[instanceElement/any]]). %in scenarios given
		
%
onAddAssumption(VE):->
	%gp3 0.1 changed, we might need to send the instance to the property dialog
	if
		VE?selection->>empty
	then
		new(assumptionPropsDlg(VE))->>newObject
	else
		new(assumptionPropsDlg(VE))->>newObject(VE?singleSelection?fragmentElement,
												VE?singleSelection?route).
%%	

%%conditionAttribute
checkConditionAttribute(VE):->
	%mag in static, process en agent
	%en de selectie moet een conditionele instantie zijn
	%(of in een conditioneel fragment zitten)

	checkEditorState(VE,			
			 [staticFragment,processFragment,agentFragment],
			 	any,
		      1,[[instanceElement/[condition,imported,parent]]]).
%
onConditionAttribute(VE):->
	new(attributePropsDlg(VE))->>newObject(	VE?singleSelection?fragmentElement,
										VE?singleSelection?route,
										condition,
										@on).
%%

%%givenAttribute
checkGivenAttribute(VE):->
	%mag in alle types en er moet een instance geselecteerd zijn

	checkEditorState(VE,
			 any,any,
			 1,
			 [[instanceElement/any]]).
%
onGivenAttribute(VE):->
	%toevoegen van attribuut. We moeten even uitzoeken of dit ook als conditie zou mogen en daar
	%kunnen we dus gewoon checkConditionAttribute voor gebruiken

	if
		VE->>checkConditionAttribute
	then
		CanSwitch = @on
	else
		CanSwitch = @off,
	new(attributePropsDlg(VE))->>newObject(VE?singleSelection?fragmentElement,
								VE?singleSelection?route,
								consequence,CanSwitch).
								
								
%%conditionQuantity
checkConditionQuantity(VE):->
	%mag in static, process en agent
	%en er moet een conditioneel of geïmporteerde instantie zijn geselecteerd
	checkEditorState(VE,
			 [staticFragment,processFragment,agentFragment],
			 any,
			 1,
			 [[instanceElement/[condition,imported,parent]]]).
%
onConditionQuantity(VE):->
	%toevoegen van een quantity als condition 

	if
		VE->>checkGivenQuantity
	then
		CanSwitch = @on
	else
		CanSwitch = @off,

	new(quantityPropsDlg(VE))->>newObject(VE?singleSelection?fragmentElement,
										VE?singleSelection?route,
										condition,
										CanSwitch).
%%

%%givenQuantity
checkGivenQuantity(VE):->
	%mag in alle types
	%en er moet een instantie zijn geselecteerd

	checkEditorState(VE,
			 any,any,
			 1,
			 [[instanceElement/any]]).

%
onGivenQuantity(VE):->
	%toevoegen van een quantity als consequentie

	if
		VE->>checkConditionQuantity
	then
		CanSwitch = @on
	else
		CanSwitch = @off,

	new(quantityPropsDlg(VE))->>newObject(VE?singleSelection?fragmentElement,
						VE?singleSelection?route,
						consequence,
						CanSwitch).
%%


%%

%%conditionValue
checkConditionValue(VE):->
	/*Dit mag in alle fragmenten, wanneer er een waarde element is geselecteerd die
	  zelf bij een conditionele quantity hoort. 
	  Mag natuurlijk niet in inputsystems
	Bij een geïmporteerde quantity mag er uberhaupt geen waarde zijn: DWZ: we kunnen daar niet
	een geïmporteerde waarde verwijderen, dus mag er alleen een waarde worden toegevoegd wanneer er nog
	niet één is.
	*/

	%dit kan niet handig via checkEditorState, dus apart
	
	%geen inputsystem
	\+ VE?fragment->>isInputSystem,
	
	%we checken of het element een conditie is in dit fragment of geïmporteerd

	%check op conditie of geïmporteerd
	S = VE<<-singleSelection,
	(
		qsValueElement = S<<-name
	;
		dqsValueElement = S<<-name
	),
	Q = S<<-fragmentElement,
	MF = VE<<-fragment,
	
	if
		MF = Q<<-fragment
	then
		%lokaal, het mag als de quantity conditioneel is
		Q->>isCondition
	else	
	(
		%het mag alleen als er nog geen geïmporteerde value is
		if
			qsValueElement = S<<-name
		then
			D = @off
		else
			D = @on,

		%we vinden op de route van de quantity (of van het waardelement) niet
		%een fragment die bij diezelfde quantity al een value van hetzelfde type
		%bevat

		LokaleRoute *= var,		

		\+ MF<<-checkRouteUp(S?route,
			and(
				%sla de route naar de quantity op gezien vanuit het nu geteste mf
				assign(LokaleRoute, @arg2),	
				->>(?(@arg1,findElements,value),
					find,
					and(
						@arg1?derivative == D,
						@arg1?quantity == Q,
						->>(@arg1?quantityRoute,
							equal,
							LokaleRoute)
						)
					)
				)
			)
		).
%
onConditionValue(VE):->
	%meteen een change requestor posten
	%voor het juiste type
	
	S = VE<<-singleSelection,
	if
		qsValueElement = S<<-name
	then
		D = @off
	else
		D = @on,

	@model->>changeRequest(setValue,
				VE?fragment,
				VE,
				S?fragmentElement,
				S?route, %route staat ook in de subelementen
				D,
				condition,
				S?valueRef?copy).
%%

%%
%givenValue:
checkGivenValue(VE):->
	/*
	Dit mag in alle fragmenten wanneer er een waarde element is geselecteerd die bij een conditionele
	of given quantity hoort.
	Bij geïmporteerde mag het weer alleen wanneer er niet al een waarde is

	*/
	%1) Waarde element?
	checkEditorState(VE,any,any,1,[[qsValueElement/any],[dqsValueElement/any]]),

	S = VE<<-singleSelection,
	Q = S<<-fragmentElement,

	if
		qsValueElement = S<<-name
	then
		D = @off
	else
		D = @on,

	%we kunnen meteen gaan zoeken, als we deze maar overslaan is er geen probleem
	LokaleRoute *= var,		
	
	\+ VE?fragment<<-checkRouteUp(S?route,
		and(
			%sla de route naar de quantity op gezien vanuit het nu geteste mf
			assign(LokaleRoute, @arg2),	
			->>(?(@arg1,findElements,value),
				find,
				and(
					@arg1?derivative == D,
					@arg1?quantity == Q,
					->>(@arg1?quantityRoute,
						equal,
						LokaleRoute)
					)
				)
			)
		).
%
onGivenValue(VE):->
	%meteen een change requestor posten
	%voor het juiste type

	S = VE<<-singleSelection,
	if
		qsValueElement = S<<-name
	then
		D = @off
	else
		D = @on,

	@model->>changeRequest(setValue,
				VE?fragment,
				VE,
				S?fragmentElement,
				S?route, %route staat ook in de subelementen
				D,
				consequence,
				S?valueRef?copy).
%%

%%conditionConfiguration
checkConditionConfiguration(VE):->
	%mag in static, process en agent

	%ze zijn of conditie, of geïmporteerd / parent
	checkEditorState(VE,
			[staticFragment,processFragment,agentFragment],
			any,any,
				[[instanceElement,instanceElement]/
						[condition,imported,parent]]).
	%check of er geen identity tussen zit wordt gedaan in de CR

%
onConditionConfiguration(VE):->
	Sel = VE<<-selection,
	First= Sel<<-head,
	Sec = Sel<<-tail,
	new(configurationPropsDlg(VE))->>newObject(First?fragmentElement, %1e
										First?route,	
										Sec?fragmentElement, %2e
										Sec?route,
										condition,
										@on).
%%

%%givenConfiguration
checkGivenConfiguration(VE):->
	%mag in alle typen

	checkEditorState(VE,any,any,any,
			 [[instanceElement,instanceElement]/any]).
%
onGivenConfiguration(VE):->
	%toevoegen van gegeven configuratie, de dlg moet weten of een conditionele ook zou
	%mogen, dus roepen we eventjes checkConditionConfiguration ook aan

	Sel = VE<<-selection,
	First= Sel<<-head,
	Sec = Sel<<-tail,
	new(configurationPropsDlg(VE))->>newObject(First?fragmentElement,
									First?route,
									Sec?fragmentElement,
									Sec?route,
									consequence,
		when(->>(VE,checkConditionConfiguration),
										@on,@off)).
%%

	
%%conditionInequality
checkConditionInequality(VE):->
	%mag in static, process en agent, en er moeten 2 elementen zijn geselecteerd
	%(tenzij het een waarde punt uit een qs is, dan is één element voldoende)
	%waarbij weer van alles moet gelden
	%dit gaat een beetje te ver voor checkEditorState, dus moeten we het opsplitsen. 
	%checken op of die inequality er al is gebeurt in de CR
	%gp3 0.3: no inequalities between two zero-points - done in helper

	\+ VE?fragment->>isInputSystem, %niet in inputsystem
	
	Sel = VE<<-selection,

	%selectie = 1 of 2
	S = Sel<<-size,
	(S = 1; S = 2),

	First = Sel<<-head,
	FirstType = First<<-name,
	(
		First?fragmentElement->>isCondition %is de quantity of calculus
	;
		(
			FirstMF = First?fragmentElement<<-fragment,
			\+ FirstMF = VE<<-fragment %geïmporteerd / parent
		)
	),

	if
		S = 2
	then (
			Sec = Sel<<-tail,
			SecType = Sec<<-name,
			(
				Sec?fragmentElement->>isCondition
			;
				(
					SecMF = Sec?fragmentElement<<-fragment,
					\+ SecMF = VE<<-fragment
				)
			)
		)
	else (
			Sec = none,
			SecType = none
		),

	%en nu moeten de typen nog kloppen, via een helper die ook veel extra info geeft
	%(staat verderop)
	(
		pl_inequalityType(First,FirstType,Sec,SecType,
									_RelArg1,_RelArg1R,_RelArg1T,_RelArg1P,_RelArg2,_RelArg2R,_RelArg2T,_RelArg2P) 
	;
		pl_inequalityType(Sec,SecType,First,FirstType,
									_RelArg2b,_RelArg2Rb,_RelArg2Tb,_RelArg2Pb,_RelArg1b,_RelArg1Rb,_RelArg1Tb,_RelArg1Pb)
	).
%

onConditionInequality(VE):->
	Sel = VE<<-selection,
	%we roepen nu nogmaals pl_inequalityType aan, om de juiste informatie te vinden
	S = Sel<<-size,

	First = Sel<<-head,
	FirstElType = First<<-name,
	if
		S = 2
	then (
		Sec = Sel<<-tail,
		SecElType = Sec<<-name
		)
	else (
		Sec = none,
		SecElType = none
		),

	(
		pl_inequalityType(First,FirstElType,Sec,SecElType,
							Arg1,FirstRoute,FirstType,FirstVal,Arg2,SecRoute,SecType,SecVal)
	;
		pl_inequalityType(Sec,SecElType,First,FirstElType,
							Arg2,SecRoute,SecType,SecVal,Arg1,FirstRoute,FirstType,FirstVal)
	),!,

	new(inequalityPropsDlg(VE))->>newObject(Arg1,
								FirstRoute,
								FirstType,
								FirstVal,
								Arg2,
								SecRoute,
								SecType,
								SecVal,
								condition,
								@on).
%%

%%givenInequality
checkGivenInequality(VE):->
	%net zulke tests als bij conditionInequality
	%mag in alle soorten MF, ook inputSystem
	%er moeten 1 of 2 elementen zijn geselecteerd
	%waarbij weer van alles moet gelden
	%verder weer opgesplitst
	%gp3 0.3: no inequalities between two zero-points - done in helper
	
	Sel = VE<<-selection,
	S = Sel<<-size,
	(S = 1 ; S = 2),
	First = Sel<<-head,
	FirstType = First<<-name,
	if 
		S = 2
	then (
		Sec = Sel<<-tail,
		SecType = Sec<<-name
		)
	else (
		Sec = none,
		SecType = none
		),
	(
		pl_inequalityType(First,FirstType,Sec,SecType,
									_RelArg1,_RelArg1R,_RelArg1T,_RelArg1P,_RelArg2,_RelArg2R,_RelArg2T,_RelArg2P) 
	;
		pl_inequalityType(Sec,SecType,First,FirstType,
									_RelArg2b,_RelArg2Rb,_RelArg2Tb,_RelArg2Pb,_RelArg1b,_RelArg1Rb,_RelArg1Tb,_RelArg1Pb)
	).
%
onGivenInequality(VE):->
	%zelfde als condition, maar we moeten nog even moeilijk doen om te kijken of er
	%binnen de propsdlg ook geswapt mag worden naar condition
	Sel = VE<<-selection,
	S = Sel<<-size,

	First = Sel<<-head,
	FirstElType = First<<-name,
	if
		S = 2
	then (
		Sec = Sel<<-tail,
		SecElType = Sec<<-name
		)
	else (
		Sec = none,
		SecElType = none
		),
	(
		pl_inequalityType(First,FirstElType,Sec,SecElType,
							Arg1,FirstRoute,FirstType,FirstVal,Arg2,SecRoute,SecType,SecVal)
	;
		pl_inequalityType(Sec,SecElType,First,FirstElType,
							Arg2,SecRoute,SecType,SecVal,Arg1,FirstRoute,FirstType,FirstVal)
	),!,

	new(inequalityPropsDlg(VE))->>newObject(Arg1,
								FirstRoute,
								FirstType,
								FirstVal,
								Arg2,
								SecRoute,
								SecType,
								SecVal,
								consequence,
								when(->>(VE,checkConditionInequality),
									@on,@off)).
%
%pl_inequalityType krijgt als eerste 4 argumenten de 2 elementen + hun type die zijn geselecteerd
%binnen. De laatste 8 argumenten geven dan van die elementen de beschrijving zoals gedefinieerd
%bij inequality(argument, argumentroute, type element + qs waarde of @nil)
%faalt als er een combinatie wordt meegegeven waarbij geen inequality mogelijk is
%verder geen check op conditie/consequentie e.d.
%als element + type kan ook none worden meegegeven, dit in het geval dat er maar één element is geselecteerd

pl_inequalityType(El1,quantityElement,El2,quantityElement,
							El1?fragmentElement,El1?route, currentValue,@nil,
							El2?fragmentElement,El2?route, currentValue,@nil).
%
pl_inequalityType(El1,quantityElement,El2,qsValueElement,
							El1?fragmentElement,El1?route,currentValue,@nil,
							El2?fragmentElement,El2?route,quantityQSPoint,El2?valueRef?copy
				):-
	point = El2?valueRef<<-type.
%
pl_inequalityType(El1,qsValueElement,El2,qsValueElement,
					Arg1,El1?route,quantityQSPoint,El1?valueRef?copy,
					Arg2,El2?route,quantityQSPoint,El2?valueRef?copy
			):-

    \+ (El1->>sameFragmentElement(El2)), %niet zelfde fragment + route
	
	point = El1?valueRef<<-type,
	point = El2?valueRef<<-type,
	%gp3 0.3: no inequalities between qs point zero, and another qs point:
	\+ ('Zero' = El1?valueRef<<-name),
	\+ ('Zero' = El2?valueRef<<-name),	
	Arg1 = El1<<-fragmentElement,
	Arg2 = El2<<-fragmentElement.
%
pl_inequalityType(El1,quantityElement,El2,calculusElement,
					El1?fragmentElement,El1?route,currentValue,@nil,
					Arg2,El2?route,calculus,@nil):-

	Arg2 = El2<<-fragmentElement,
	quantity = Arg2<<-type.
%
pl_inequalityType(El1,qsValueElement,El2,calculusElement,
					El1?fragmentElement,El1?route,quantityQSPoint,El1?valueRef?copy,
					Arg2,El2?route,calculus,@nil):-
	
	Arg2 = El2<<-fragmentElement,
	quantity = Arg2<<-type,
	point = El1?valueRef<<-type.
%
pl_inequalityType(El1,calculusElement,El2,calculusElement,
					Arg1,El1?route,calculus,@nil,
					Arg2,El2?route,calculus,@nil):-

	Arg1 = El1<<-fragmentElement,
	Arg2 = El2<<-fragmentElement,
	CalcType = Arg1<<-type,
	CalcType = Arg2<<-type. %zelfde type
%
pl_inequalityType(El1,derivativeElement,El2,derivativeElement,
						El1?fragmentElement, El1?route,currentDerivative,@nil,
						El2?fragmentElement, El2?route,currentDerivative,@nil).
%
pl_inequalityType(El1,derivativeElement,El2,dqsValueElement,
							El1?fragmentElement, El1?route,currentDerivative,@nil,
							El2?fragmentElement, El2?route,derivativeZero,@nil
	):-

	El1->>sameFragmentElement(El2),
	point = El2?valueRef<<-type.
%
pl_inequalityType(El1,derivativeElement,El2,calculusElement,
					El1?fragmentElement, El1?route,currentDerivative,@nil,
					Arg2, El2?route,calculus,@nil):-

	Arg2 = El2<<-fragmentElement,
	derivative = Arg2<<-type.
%
pl_inequalityType(El1,calculusElement,El2,dqsValueElement,
					Arg1, El1?route,calculus,@nil,
					El2?fragmentElement, El2?route,derivativeZero,@nil):-

	Arg1 = El1<<-fragmentElement,
	derivative = Arg1<<-type,
	point = El2?valueRef<<-type.
%
pl_inequalityType(El1,qsValueElement,none,none,
					Arg,El1?route,currentValue,@nil,
					Arg,El1?route,quantityQSPoint,Value):-
	point = El1?valueRef<<-type,
	Arg= El1<<-fragmentElement,
	Value = El1?valueRef<<-copy.
%
pl_inequalityType(El1,dqsValueElement,none,none,
					Arg,El1?route,currentDerivative,@nil,
					Arg,El1?route,derivativeZero,@nil):-

	point = El1?valueRef<<-type,
	Arg = El1<<-fragmentElement.
%%
	

%%calculation (niet speciaal conditie / consequentie)
checkAddPlus(VE):->
	%mag bij alle fragmenten
	%er moeten 2 objecten zijn geselecteerd. Er mogen zoveel calculi als men wil
	%tussen 2 objecten

	VE->>checkAddCalc.
%
onAddPlus(VE):->
	%ook door naar centrale
	VE->>onAddCalc(plus).
%
checkAddMin(VE):->
	%zelfde als plus
	VE->>checkAddCalc.
%
onAddMin(VE):->
	VE->>onAddCalc(min).
%
checkAddCalc(VE):->
	%samenvoegen van addPlus en addMin commando

    % Would be nice if this would have comments
	Sel = VE<<-selection,
	2 = Sel<<-size,
	First = Sel<<-head,
	FirstType = First<<-name,
	Sec = Sel<<-tail,
	SecType = Sec<<-name,
	(
		pl_calculusType(First,FirstType,Sec,SecType, %zie verderop
						_,_,_)
	;
		pl_calculusType(Sec,SecType,First,FirstType,
						_,_,_)
	),

    % Prevent "nesting" of mult and div
    % E.g. A * B + C
    % Neither of the arguments can be a mult/div calculus
    (
	FirstType = calculusElement,
	get(First?fragmentElement,sign,Sign),
        member(Sign, [mult,diw]) ->
        fail
    ;
        SecType = calculusElement,
        get(Sec?fragmentElement,sign,Sign),
        member(Sign, [mult,diw]) ->
        fail
    ;
        true
    ).


%
onAddCalc(VE,Sign: {plus, min}):->
	%open de calc props dlg

	Sel = VE<<-selection,
	First = Sel<<-head,
	FirstType = First<<-name,
	Sec = Sel<<-tail,
	SecType = Sec<<-name,
	(
		pl_calculusType(First,FirstType,Sec,SecType,
				Type, Arg1QSPoint, Arg2QSPoint)
	;
		pl_calculusType(Sec,SecType,First,FirstType,
				Type, Arg2QSPoint, Arg1QSPoint)
	),!,
	new(calculusPropsDlg(VE))->>newObject(Type, Sign,
							 	First?fragmentElement,
								First?route,
								Arg1QSPoint,
								Sec?fragmentElement,
								Sec?route,
								Arg2QSPoint).
%
%pl_calculusType slaagt wanneer de 2 meegestuurde elementen geldige argumenten van een
%calculus zouden zijn. prototype:
%pl_calculusType(+FirstElement, +FirstType, +SecondElement, + SecondType,
%				-CalculusType,-CalcArg1QSPoint,-CalcArg2QSPoint)
%(de args voor de calc zijn altijd element?fragmentElement)

pl_calculusType(_El1,quantityElement,_El2,quantityElement,
			quantity,@nil,@nil).
%
pl_calculusType(_El1,quantityElement,El2,qsValueElement,
			quantity,@nil,El2?valueRef?copy):-
	point = El2?valueRef<<-type.
%
pl_calculusType(El1,qsValueElement,El2,qsValueElement,
			quantity,El1?valueRef?copy,El2?valueRef?copy):-
	point = El1?valueRef<<-type,
	point = El2?valueRef<<-type.	
%
pl_calculusType(_El1,quantityElement,El2,calculusElement,
			quantity,@nil,@nil):-
	quantity = El2?fragmentElement<<-type.
%
pl_calculusType(El1,qsValueElement,El2,calculusElement,
			quantity, El1?valueRef?copy,@nil):-
	point = El1?valueRef<<-type,
	quantity = El2?fragmentElement<<-type.
%
pl_calculusType(_El1,derivativeElement,_El2,derivativeElement,
			derivative,@nil,@nil).
%
pl_calculusType(_El1,derivativeElement,El2, calculusElement,
			derivative,@nil,@nil):-
	derivative = El2?fragmentElement<<-type.
%
pl_calculusType(El1,calculusElement,El2,calculusElement,
			CalcType,@nil,@nil):-
	CalcType = El1?fragmentElement<<-type,
	CalcType = El2?fragmentElement<<-type. %zelfde type
%%


%%addCorrespondence
checkAddCorrespondence(VE):->
%mag ik elk type view behalve inputsystem,
% bij twee quantity spaces of twee waarden uit QS
%2.1: ook bij dqs of dqs-waarden
%verder zijn alle normen afgeschaft, dus doen we het gewoon, ook als
%het nergens op slaat omdat er bijvoorbeeld al een undirected qs-correspondentie
%is
%gp3 0.3: we added full correspondence between to quantities
%but removed the possibility to use it on clients request

%we added a new norm: qs-correspondence and full-correspondence only when there is a compatibility
%(i.e.: a normal or inverse correspondence would make sense)

	checkEditorState(VE,any,inputSystem,any,
			[
				%[quantityElement,quantityElement]/any, %gp3: full correspondence [removed possibility on clients request]
				[qsElement,qsElement]/any,
				[qsValueElement,qsValueElement]/any,
				[derivativeElement,derivativeElement]/any,
				[dqsValueElement,dqsValueElement]/any
			]),

	%niet van dezelfde quantity
	Q1 = VE?selection?head<<-fragmentElement,
	\+ (
		Q1 = VE?selection?tail<<-fragmentElement,
		VE?selection?head?route->>equal(VE?selection?tail?route)
		),
	%gp3: when the selection is qsElement or quantityElement, we have to check the qs-compatibility
	
	if
	(
		qsElement = VE?selection?head<<-name %both are qsElement
	;
		quantityElement = VE?selection?head<<-name
	)
	then
	(
		%the nice thing is that both quantityElement and qsElement have the quantity as their <-fragmentElement..
		Q1->>checkQSCorrCompatibility(VE?selection?tail?fragmentElement,@off)
	;
		Q1->>checkQSCorrCompatibility(VE?selection?tail?fragmentElement,@on) %inverse
	).

%
onAddCorrespondence(VE):->
	%2.1: elementen kunnen ook dqs of derivative zijn
	Sel = VE?selection,
	El1 = Sel<<-head,
	El2 = Sel<<-tail,

	if (
			qsValueElement = El1<<-name ;
			dqsValueElement = El1<<-name
		)
	then (
		Arg1Value = El1?valueRef?copy,
		Arg2Value = El2?valueRef?copy
		)
	else (
		Arg1Value = @nil,
		Arg2Value = @nil
		),

	%derivative?
	if (
			derivativeElement = El1<<-name ;
			dqsValueElement = El1<<-name
		)
	then
		Derivative = @on
	else
		Derivative = @off,
		
	%gp3: full?
	if
		quantityElement = El1<<-name
	then
		Full = @on
	else
		Full = @off,
	new(correspondencePropsDlg(VE))->>newObject(
										Derivative, %2.1, eigenlijk alleen van belang bij hele afgeleide (niet waarde), want bij waarden gaat het door de ref vanzelf goed
										Full, %gp3 0.3 Derivative cannot be @on if Full is @on...
										El1?fragmentElement,
										El1?route,
										Arg1Value,
										El2?fragmentElement,
										El2?route,
										Arg2Value).
%%

%%proportionality
checkAddProportionality(VE):->
	%dit mag in elke type fragment behalve inputsystem
	% zolang er maar 2 quantities zijn geselecteerd
	%zelfs als er al 1 zit (nieuw beleid: zo weinig mogelijk normcheck)

	checkEditorState(VE,any,inputSystem,any,
			 [[quantityElement,quantityElement]/any]).
%
onAddProportionality(VE):->
	%we openen de quantityRelationPropsDlg voor een nieuwe proportionality
	Sel = VE<<-selection,
	El1 = Sel<<-head,
	El2 = Sel<<-tail,
	new(proportionalityPropsDlg(VE))->>newObject(El1?fragmentElement,El1?route,
										El2?fragmentElement,El2?route).
%%

%%influence
checkAddInfluence(VE):->
	%dit mag alleen in proces en agent fragmenten, en er moeten 2 quantities zijn
	%geselecteerd
	%verdere normen afgeschaft

	checkEditorState(VE,[processFragment,agentFragment],any,any,
			[
				[quantityElement,quantityElement]/any
			]).
%
onAddInfluence(VE):->
	El1 = VE?selection?head,
	El2 = VE?selection?tail,
	new(influencePropsDlg(VE))->>newObject(El1?fragmentElement,El1?route,
										El2?fragmentElement,El2?route).
%%

%%Modelfragment
checkAddModelFragment(VE):->
	/*In principe mag een modelfragment in alle modelfragmenten worden ingevoegd.
	(niet in inputSystems)
	  We kijken ook niet naar het type modelfragment: dat mag allemaal, maar wijzig
	  niet het type van het huidige modelfragment.
	  
	  Parents mogen toegevoegd worden. Maar het fragment zelf, children of
	  kleinkinderen niet.
	  Dit wordt echter in de CR geregeld.
	*/
	  
	checkEditorState(VE,any,inputSystem,none,any). %geen selectie
%
onAddModelFragment(VE):->
	new(importedFragmentPropsDlg(VE))->>newObject.
%%

%%addIdentity
checkAddIdentity(VE):->
	%twee instanties waarvan minstens één geïmporteerd, en niet
	%in dezelfde import. Als niet geïmporteerd dan conditie
	%check of er geen structuur tussen zit gebeurt in CR
	%zij zijn ook van hetzelfde type (entity/agent)
	
	%ze zijn of conditie, of geïmporteerd / parent
	checkEditorState(VE,any,inputSystem,any,
				[[instanceElement,instanceElement]/
						[condition,imported,parent]]),
	Sel = VE<<-selection,
	First = Sel<<-head,
	Sec = Sel<<-tail,
	
	First?fragmentElement?entity->>instance_of(Sec?fragmentElement?entity?class),
	
	if
		First?route->>empty
	then
	(
		First->>isCondition,
		FR = @nil
	)
	else
		FR = First?route<<-head,
		
	if
		Sec?route->>empty
	then
	(
		Sec->>isCondition,
		SR = @nil
	)
	else
		SR = Sec?route<<-head,
		
	FR \== SR.
%
onAddIdentity(VE):->
	Sel = VE<<-selection,
	First= Sel<<-head,
	Sec = Sel<<-tail,
	%net als bij value meteen doen, geen gedoe met properties
	@model->>changeRequest(newIdentity,
				VE?fragment,
				VE,
				First?fragmentElement,
				First?route,
				Sec?fragmentElement,
				Sec?route,
				new(string)).
%%
/*******Voor meerdere objecten*************/

%%Delete
checkDelete(VE):->

	checkEditorState(VE,
			 any, %welke fragmenten mogen
			 any, %welke fragmenten mogen niet
			 1, %aantal geselecteerde objecten
			 [
				[instanceElement/[condition,consequence]],
				[assumptionInstanceElement/[condition,consequence]], %condition allleen in mf, cons alleen in IS
				[attributeElement/[condition,consequence]],
				[quantityElement/[condition,consequence]],
				[valueMarker/[condition,consequence]],
				[configurationElement/[condition,consequence]],				
				[inequalityRelationElement/[condition,consequence]],
				[calculusElement/[condition,consequence]],
				[correspondenceElement/consequence],
				[quantityRelationElement/consequence],
				[importedMFElement/condition],
				[identityRelationElement/condition]
			] %combinaties
			).
%
onDelete(VE):->
	%Regel het verwijderen

	E = VE<<-singleSelection,
	Type = E<<-name,
	FE = E<<-fragmentElement,
	pl_onDelete(VE,FE,Type).
%
pl_onDelete(VE,FE,instanceElement):-
	%we sturen de betreffende CR

	@model->>changeRequest(deleteFInstance,
			       VE?fragment,
			       VE,
			       FE).
%
pl_onDelete(VE,FE,assumptionInstanceElement):-
	%we sturen de betreffende CR

	@model->>changeRequest(deleteAssumptionInstance,
			       VE?fragment,
			       VE,
			       FE).
%
pl_onDelete(VE,FE,attributeElement):-
	@model->>changeRequest(deleteAttribute,
			       VE?fragment,
			       VE,
			       FE).
%
pl_onDelete(VE,FE,quantityElement):-
	@model->>changeRequest(deleteQuantity,
			       VE?fragment,
			       VE,
			       FE).
%
pl_onDelete(VE,FE,valueMarker):-
	@model->>changeRequest(deleteValue,
			       VE?fragment,
			       VE,
			       FE).
%
pl_onDelete(VE,FE,configurationElement):-
	@model->>changeRequest(deleteConfiguration,
			       VE?fragment,
			       VE,
			       FE).
%
pl_onDelete(VE,FE,calculusElement):-
	@model->>changeRequest(deleteCalculus,
					VE?fragment,
					VE,
					FE).
%
pl_onDelete(VE,FE,inequalityRelationElement):-
	@model->>changeRequest(deleteInequality,
				VE?fragment,
				VE,
				FE).
%
pl_onDelete(VE,FE,correspondenceElement):-
	@model->>changeRequest(deleteCorrespondence,
					VE?fragment,VE,FE).
%
pl_onDelete(VE,FE,quantityRelationElement):-
	@model->>changeRequest(deleteQuantityRelation,
					VE?fragment,VE,FE).
%
pl_onDelete(VE,FE,importedMFElement):-
	@model->>changeRequest(deleteConditionalFragment,
					VE?fragment,VE,FE).
%
pl_onDelete(VE,FE,identityRelationElement):-
	@model->>changeRequest(deleteIdentity,
					VE?fragment,VE,FE).

%%

%%properties
checkProperties(VE):->
	%mag bij elk niet-fixed object, alleen mag er maar 1 geselecteerd zijn

	checkEditorState(VE,any,any,1,[[any/[condition,consequence,imported,parent]]]),
	\+ valueMarker = VE?singleSelection<<-name.
%
onProperties(VE):->
	%dit hangt natuurlijk erg van het geselecteerde object af
	%we delegeren dit naar een prolog helper, die de rest doet
	%we doen alleen wat voorbereiding

	Element = VE<<-singleSelection,
	if
		normal = Element<<-fragmentState
	then
		ReadOnly = @off
	else
		ReadOnly = @on, %alleen readonly bij geïmporteerde zaken
	Type = Element<<-name,
	FE = Element<<-fragmentElement,
	Fragment = VE<<-fragment,
	pl_onProperties(VE,Type,FE,Fragment,Element,ReadOnly).

%voor elk type een clause...
pl_onProperties(VE,instanceElement,FE,_Fragment,_Element,ReadOnly):-

	%we mogen stateswitchen als het om een entityinstance gaat
	%en zijn type proces of agent is
	if
		(
			FE?entity->>instance_of(entity),
			(
				processFragment = FE?fragment<<-currentType
			;
				agentFragment = FE?fragment<<-currentType
			)
		)
	then
		CanSwitch = @on
	else
		CanSwitch = @off,
	new(instancePropsDlg(VE))->>editObject(FE,
						CanSwitch,
						ReadOnly).
%
pl_onProperties(VE,assumptionInstanceElement,FE,_Fragment,_Element,ReadOnly):-

	new(assumptionPropsDlg(VE))->>editObject(FE,ReadOnly).
%
pl_onProperties(VE,attributeElement,FE,_Fragment,_Element, ReadOnly):-

	%het gaat eerst om de vraag of we mogen switchen van conditie / consequentie
	%dat mag als de Instance conditioneel is tov het attribute (dus
	%zelf conditioneel of in een andere MF dan het attribute

	Instance = FE<<-garpInstance,
	if
	(
		(\+ FE?instanceRoute->>empty)	%geimporteerd tov attribute
	;
		Instance->>isCondition
	)
	then
		CanSwitch = @on
	else
		CanSwitch = @off,

	new(attributePropsDlg(VE))->>editObject(FE,
						CanSwitch,
						ReadOnly).
%
pl_onProperties(VE,quantityElement,FE,_Fragment,_Element, ReadOnly):-
	%het gaat eerst om de vraag of we mogen switchen van conditie / consequentie

	Instance = FE<<-garpInstance,
	if
	(
		(\+ FE?instanceRoute->>empty)	%geimporteerd tov quantity
	;
		Instance->>isCondition
	)
	then
		CanSwitch = @on
	else
		CanSwitch = @off,
	new(quantityPropsDlg(VE))->>editObject(FE,
						CanSwitch, ReadOnly).
%
pl_onProperties(VE,configurationElement,FE,_Fragment,_Element, ReadOnly):-
	%zijn beide elementen als conditie te beschouwen vanuit de configuratie?

	if
	(
		(
			(\+ FE?argument1Route->>empty)
		;
			FE?argument1->>isCondition
		),
		(
			(\+ FE?argument2Route->>empty)
		;
			FE?argument2->>isCondition
		)
	)
	then
		CanSwitch = @on
	else
		CanSwitch = @off,

	new(configurationPropsDlg(VE))->>editObject(FE,
						CanSwitch, ReadOnly).

pl_onProperties(VE,inequalityRelationElement,FE,_Fragment,_Element, ReadOnly):-
	%of we mogen switchen hangt af van de argumenten
	%conditioneel mag alleen wanneer beide argumenten conditioneel zijn


	if (
		FE?argument1->>isCondition,
		FE?argument2->>isCondition
	   )
	then
		CanSwitch = @on
	else
		CanSwitch = @off,
	new(inequalityPropsDlg(VE))->>editObject(FE,CanSwitch, ReadOnly).
%

pl_onProperties(VE,calculusElement,Calculus,_Fragment,_Element,ReadOnly):-
	%geen probleem met switchen ofzo, we kunnen gewoon openen
	new(calculusPropsDlg(VE))->>editObject(Calculus,ReadOnly).

%
pl_onProperties(VE,correspondenceElement,Cor,_Fragment,_Element,ReadOnly):-
	new(correspondencePropsDlg(VE))->>editObject(Cor,ReadOnly).

%
pl_onProperties(VE,quantityRelationElement,QR,_Fragment,_Element,ReadOnly):-
	if
		prop = QR<<-type
	then
		D *= proportionalityPropsDlg(VE)
	else
		D *= influencePropsDlg(VE),

	D->>editObject(QR,ReadOnly).
%
pl_onProperties(VE,importedMFElement,IF,Fragment,_Element,ReadOnly):-
	%gp3 0.3: special case. If the element refers to a refiner in this fragment
	%we open the refinementproperties
	
	if
	(
		IF->>instance_of(fragmentRefiner),
		Fragment = IF<<-fragment
	)
	then
		new(refinerPropsDlg(VE))->>editObject(IF) %not readonly
	else
		new(importedFragmentPropsDlg(VE))->>editObject(IF,ReadOnly).
%
pl_onProperties(VE,identityRelationElement,IF,_Fragment,_Element,ReadOnly):-
	new(identityPropsDlg(VE))->>editObject(IF,ReadOnly).

%%
%Refinement
%The refinement command is only for creating a refinement
%changing it (the comments) or deleting it is done through 

checkRefinement(VE):->
	%rules:
	%must be a conditional one, not a parent
	%not in this fragment
	%not allready refined (seen from our fragment)
	
	IFE = VE<<-singleSelection,
	IF = IFE<<-fragmentElement,
	VEFR = VE<<-fragment,

	FR = IF<<-fragment, %fails if parent representation, thats what we want
	FR \== VEFR, %not in our fragment
	\+ VEFR<<-checkRouteUp(IFE?route,
		?(@arg1,refinementFor,IF,@arg2),@off).

onRefinement(VE):->
	%open the dialog: for new refinement only
	
	IFE = VE<<-singleSelection,
	IF = IFE<<-fragmentElement,
	
	Dlg *= refinerPropsDlg(VE),
	Dlg->>newObject(IF,IFE?route). %add a new one.
%%

%%%%view

%%Collapse
checkCollapse(VE):->
	%Mag bij bepaalde elementen wanneer ze subs hebben die zichtbaar zijn en die gehide kunnen worden
	VE?singleSelection->>subCanHide.
%
onCollapse(VE):->
	VE?singleSelection->>collapse.
%%

%%Expand
checkExpand(VE):->
	%Mag bij elementen wanneer ze subs hebben die niet zichtbaar zijn

	VE?singleSelection->>subHidden.
%
onExpand(VE):->
	VE?singleSelection->>expand.
%%

%%collapse relations
checkCollapseRelations(VE):->
	%element moet relatie hebben die zichtbaar is
	VE?singleSelection->>visibleRelation.
%
onCollapseRelations(VE):->
	
	VE?singleSelection->>collapseRelations.
%%

%%expand relations
checkExpandRelations(VE):->
	%element moet een hidden relatie hebben

	VE?singleSelection->>invisibleRelation.
%
onExpandRelations(VE):->

	VE?singleSelection->>expandRelations.
%%

%%
checkShowRelevant(VE):->
	%show relevant zorgt dat alleen de relevante sub elementen zichtbaar zijn
	%wanneer er niets is geselecteerd geldt dit voor alles
	%anders voor de subs van het geselecteerde element
	%de check doet niet heel veel...

	VE?selection->>empty,!. %ok, we doen het gewoon zometeen
%
checkShowRelevant(VE):->
	Sel = VE<<-singleSelection,
	\+ Sel?subs->>empty. %er zijn subs
%
onShowRelevant(VE):->
	if
		VE?selection->>empty
	then (
		%2 fasen: eerst oninteressante verbergen, dan interessante tonen (voor de zekerheid)
		VE?elements->>for_all(if(not(->>(@arg1,important)),
							->>(@arg1,setHidden,@on))),
		VE?elements->>for_all(if(->>(@arg1,important),
							->>(@arg1,ensureVisible)))
		)
	else
		%uitbesteden
		VE?singleSelection->>showRelevant.	
%%

%%
%%Full Redraw
%%gp3 1.0: Full redraw all element: remove all saved layout info and redraw. All hidden stuff is show, default positions are used
onFullRedraw(VE):->
	Fragment = VE<<-fragment,
	VE->>destroy, %we are going to restart this window
	Fragment->>clearLayOutInfo,
	@app->>openViewEditor(Fragment).

%%

%%translation: gp3 1.4.0

onViewTranslations(_VE):->
	@app->>openLanguageEditor.
%%


%%expandAll
checkExpandAll(VE):->
	%expand all mag als er ergens iets is ingeklapt onder dit ding
	%of als er niets is geselecteerd
	VE?selection->>empty,
	VE?elements->>find(@arg1?hidden == @on).
%
checkExpandAll(VE):->
	Sel = VE<<-singleSelection,
	Sel->>recursiveCheckHidden.
%
infoLabelExpandAll(VE,
	Label: name):<-

	if
		VE?selection->>empty
	then
		Label = 'Expand all'
	else
		Label = 'Expand tree'.
%
onExpandAll(VE):->
	if
		VE?selection->>empty
	then
		VE?elements->>for_all(->>(@arg1,setHidden,@off))
	else	
		VE?singleSelection->>expandAll.
%%
	
	
%%Hide
checkHide(VE):->
	%mag bij elementen die kunnen hiden
	VE?singleSelection->>canHide.
%
onHide(VE):->
	Sel = VE<<-singleSelection,
	Sel->>setHidden(@on),
	if
		Sup = Sel<<-super
	then
		VE?client->>selection(Sup)
	else
		VE?client->>selection(@nil).
%%

%%ToggleSubfragments
%%gp3 0.3
checkToggleSubfragments(VE):->
	%this command is available when an importedfragment is selected
	%that has no super itself and has subfragments
	
	Sel = VE<<-singleSelection,
	importedMFElement = Sel<<-name,
	\+ Sel<<-super,
	Sel?fragmentElement?referencedFragment->>containsConditionalFragments.
%
infoLabelToggleSubfragments(VE,
	Label: name):<-
	
	if
		@on = VE?singleSelection<<-subMFsShown
	then
		Label = 'Hide subfragments'
	else
		Label = 'Show subfragments'.
%
onToggleSubfragments(VE):->
	Sel = VE<<-singleSelection,
	Sel->>subMFsShown(Sel?subMFsShown?negate),
	VE->>update_visualisation. %redraw now
%%

%%toggle tooltips
%gp3: this just switches the 'model' category in tooltip window on or of (global setting)
infoLabelToggleTooltips(_VE,
	Label: name):<-
	
	if
		@on = @tooltip_window<<-category_state(model)
	then
		Label = 'Hide model ingredient tooltips'
	else
		Label = 'Show model ingredient tooltips'.
%
onToggleTooltips(_VE):->
	@tooltip_window->>category_state(model,?(@tooltip_window,category_state,model)?negate).
%%

/*******************CHANGE REQUESTORS****************/
%%
changeApplied_changeMF(VE,
				_CR:changeRequestor):->
	%een MF is gewijzigd (naam, remarks, parents)
	
	%eventueel ons label wijzigen
	VE->>label(VE?makeWindowLabel),
	%en alles bijwerken
	VE->>mustUpdateVisualisation.
%%

%%
changeApplied_changeInputSystem(VE,
				_CR:changeRequestor):->
	%een IS is gewijzigd (naam, remarks)
	
	%eventueel ons label wijzigen
	VE->>label(VE?makeWindowLabel).
%%


%%
changeApplied_deleteMF(VE,
	CR: changeRequestor):->
	%wanneer het onze MF is gaan we sluiten (layOutInfo kan nog opgeslagen worden
	%want het object bestaat nog)
	%anders natuurlijk bijwerken
	if
	    CR->>checkObject(VE?fragment)
	then
	    VE->>destroy
	else
	    VE->>mustUpdateVisualisation.
%%


%%
changeApplied_deleteInputSystem(VE,
	CR: changeRequestor):->
	%wanneer het onze IS is gaan we sluiten (layOutInfo kan nog opgeslagen worden
	%want het object bestaat nog)
	%anders natuurlijk bijwerken
	if
	    CR->>checkObject(VE?fragment)
	then
	    VE->>destroy
	else
	    VE->>mustUpdateVisualisation.
%%

%%
changeApplied_changeHObject(VE,
				_CR:changeRequestor
			       ):->
	%onze reaktie bij naamverandering van een hierarchicalObject
	VE->>mustUpdateVisualisation.

%%

%%
changeApplied_newFInstance(VE,
			    CR:changeRequestor
			   ):->
	%onze reaktie bij een nieuwe instantie: we moeten hem maken
	%en afbeelden
	%gp3 0.3: no selection change even when this editor initiated the change
	VE->>update_visualisation, %need to do it right now
	
	IE = VE<<-findFirstElement(CR?result),
	unless
	    CR->>checkEditor(VE)
	do
	    VE->>checkHideNewElement(IE,CR).
%%

%%
changeApplied_changeFInstance(VE,
			      CR: changeRequestor
			     ):->
	%gp3 0.2: changed to only update the relevant elements
	VE->>updateElement(CR?arg1).
%%

%%
changeApplied_deleteFInstance(VE,
			      _CR:changeRequestor
			     ):->
	VE->>mustUpdateVisualisation. %gp3, changed this to update when all done
%%

%%
changeApplied_newAssumptionInstance(VE,
	CR : changeRequestor
	):->
	%als de assumptie hier komt: afbeelden
	%en als wij de editor waren, dan ook nog effe selecteren
	VE->>update_visualisation,
	AE = VE<<-findFirstElement(CR?result),
	if
	    CR->>checkEditor(VE)
	then
	    VE?client->>normalise(AE)
	else
	    VE->>checkHideNewElement(AE,CR).
%%

%%
changeApplied_changeAssumptionInstance(VE,
			      CR: changeRequestor
			     ):->
	%gp3 0.2: changed to only update the relevant elements
	VE->>updateElement(CR?arg1).
%%

%%
changeApplied_deleteAssumptionInstance(VE,
			      _CR: changeRequestor
			     ):->
	VE->>mustUpdateVisualisation. %gp3, changed this to update when all done
%%

%%
changeApplied_newAttribute(VE,
			   CR: changeRequestor
			  ):->
	VE->>update_visualisation,
	AE = VE<<-findFirstElement(CR?result,attributeElement),
	VE->>checkHideNewElement(AE,CR).
%%

%%
changeApplied_changeAttribute(VE,
			CR: changeRequestor):->
	%gp3 0.2: changed to only update the relevant elements
	VE->>updateElement(CR?arg1).
%%

%%
changeApplied_deleteAttribute(VE,
			_CR: changeRequestor):->
	VE->>mustUpdateVisualisation. %gp3, changed this to update when all done
%%


%%
changeApplied_changeAttributeDef(VE,
	_CR:changeRequestor):->
	VE->>mustUpdateVisualisation.
%%

%%
changeApplied_newQuantity(VE,
			   CR: changeRequestor
			  ):->
	%afbeelden en eventueel selecteren
	%gp3 0.3: no selection change even when this editor initiated the change
	VE->>update_visualisation,
	QE = VE<<-findFirstElement(CR?result,quantityElement),
	unless
	    CR->>checkEditor(VE)
	do
	    VE->>checkHideNewElement(QE,CR).
%%

%%
changeApplied_changeQuantityDef(VE,
	_CR: changeRequestor):->
	VE->>mustUpdateVisualisation.
%%

%%
changeApplied_changeQuantity(VE,
	_CR: changeRequestor):->
	%gp3 0.2: changed to only update the relevant elements
	%VE->>updateElement(CR?arg1),
	VE->>mustUpdateVisualisation. % Updating the element does not work, update everything
%%

%%
changeApplied_deleteQuantity(VE,
	_CR: changeRequestor):->
	VE->>mustUpdateVisualisation. %gp3, changed this to update when all done
%%

%%
changeApplied_setValue(VE,
	CR: changeRequestor):->
	%als we de bijbehorende quantity afbeelden, dan moet de value erbij
	%we could do this smarter I guess
	VE->>update_visualisation, %need to do it now
	V = VE<<-findFirstElement(CR?result,valueMarker),
	VE->>checkHideNewElement(V,CR).
%%

%%
changeApplied_deleteValue(VE,
	_CR : changeRequestor):->
	VE->>mustUpdateVisualisation. %gp3, changed this to update when all done
%%

%%
changeApplied_newConfiguration(VE,
	CR: changeRequestor):->
	VE->>update_visualisation,
	CE = VE<<-findFirstElement(CR?result,configurationElement),
	VE->>checkHideNewElement(CE,CR),
	%gp3: make sure the element is visible
	if
	    CR->>checkEditor(VE)
	then
	    VE?client->>normalise(CE).
%%

%%
changeApplied_changeConfiguration(VE,
	_CR: changeRequestor):->
	VE->>mustUpdateVisualisation.
%%

%%
changeApplied_deleteConfiguration(VE,
	_CR: changeRequestor):->
	VE->>mustUpdateVisualisation.
%%

%%
changeApplied_newInequality(VE,
	CR: changeRequestor):->
	VE->>update_visualisation,
	IE = VE<<-findFirstElement(CR?result,inequalityRelationElement),
	VE->>checkHideNewElement(IE,CR),
	%gp3: make sure the element is visible
	if
	    CR->>checkEditor(VE)
	then
	    VE?client->>normalise(IE).
%%

%%
changeApplied_changeInequality(VE,
	_CR: changeRequestor):->
	VE->>mustUpdateVisualisation. %gp3, changed this to update when all done
%%
	
%%
changeApplied_deleteInequality(VE,
	_CR : changeRequestor):->
	VE->>mustUpdateVisualisation.
%%

%%
changeApplied_newCalculus(VE,
	CR: changeRequestor):->
	VE->>update_visualisation,
	CE = VE<<-findFirstElement(CR?result,calculusElement),
	VE->>checkHideNewElement(CE,CR),
	%gp3: make sure the element is visible
	if
	    CR->>checkEditor(VE)
	then
	    VE?client->>normalise(CE).
%%

%%
changeApplied_changeCalculus(VE,
	_CR: changeRequestor):->
	VE->>mustUpdateVisualisation.
%%

%%
changeApplied_deleteCalculus(VE,
	_CR: changeRequestor):->
	VE->>mustUpdateVisualisation.
%%

%%
changeApplied_newCorrespondence(VE,
	CR: changeRequestor):->
	VE->>update_visualisation, %must do it now
	CE = VE<<-findFirstElement(CR?result,correspondenceElement),
	VE->>checkHideNewElement(CE,CR),
	%gp3: make sure the element is visible
	if
	    CR->>checkEditor(VE)
	then
	    VE?client->>normalise(CE).
%%

%%
changeApplied_changeCorrespondence(VE,
	_CR: changeRequestor):->
	VE->>mustUpdateVisualisation. %gp3, changed this to update when all done
%%

%%
changeApplied_deleteCorrespondence(VE,
	_CR: changeRequestor):->
	VE->>mustUpdateVisualisation.
%%

%%
changeApplied_newQuantityRelation(VE,
	CR: changeRequestor):->
	VE->>update_visualisation,
	QRE = VE<<-findFirstElement(CR?result,quantityRelationElement),
	VE->>checkHideNewElement(QRE,CR),
	%gp3: make sure the element is visible
	if
	    CR->>checkEditor(VE)
	then
	    VE?client->>normalise(QRE).
%%

%%
changeApplied_changeQuantityRelation(VE,
	_CR: changeRequestor):->
	VE->>mustUpdateVisualisation.
%%
	
%%
changeApplied_deleteQuantityRelation(VE,
	_CR: changeRequestor):->
	VE->>mustUpdateVisualisation.
%%

%%
changeApplied_newConditionalFragment(VE,
			    CR:changeRequestor
			   ):->
	%onze reaktie bij een nieuw conditioneel fragment
	VE->>update_visualisation,
	VE->>checkHideNewElement(?(VE,findFirstElement,CR?result),CR).
%%

%%
changeApplied_changeConditionalFragment(VE,
			    CR:changeRequestor
			   ):->
	%gp3 0.2: changed to only update the relevant elements
	VE->>updateElement(CR?arg1).
%%

%%
changeApplied_deleteConditionalFragment(VE,
			_CR: changeRequestor):->
	VE->>mustUpdateVisualisation. %gp3, changed this to update when all done
%%

%%
changeApplied_newFragmentRefiner(VE, _CR: changeRequestor):->
	VE->>mustUpdateVisualisation.
%%

%%
changeApplied_deleteFragmentRefiner(VE,
			_CR: changeRequestor):->
	VE->>mustUpdateVisualisation.
%%

%%
changeApplied_newIdentity(VE,
			    CR:changeRequestor
			   ):->
	%omdat we geen dialoog geven selecteren we hem, zodat makkelijker
	%de dialoog geopend kan worden
	%gp3 0.3: no selection change even when this editor initiated the change
	VE->>update_visualisation,
	IE = VE<<-findFirstElement(CR?result),
	unless
	    CR->>checkEditor(VE)
	do
	    VE->>checkHideNewElement(IE,CR).
%%

%%
changeApplied_changeIdentity(VE,
	_CR: changeRequestor):->
	VE->>mustUpdateVisualisation.
%%

%%
changeApplied_deleteIdentity(VE,
	_CR: changeRequestor):->
	VE->>mustUpdateVisualisation.
%%

%%
changeApplied_changeQuantitySpace(VE,
	_CR: changeRequestor):->
	VE->>mustUpdateVisualisation.
%%

%%
changeApplied_changeConfigurationDef(VE,
	_CR:changeRequestor):->
	VE->>mustUpdateVisualisation.
%%

%%
%language
%%
changeApplied_setCurrentLanguage(VE,
	_CR:changeRequestor):->	
	%just redraw
	VE->>mustUpdateVisualisation.
%%
%%
%%de algemene changeApplied gebruiken we voor het bijwerken van de info
%%over de structuurregel

changeApplied(VE,
	_CR: changeRequestor):->
	VE->>label(VE?makeWindowLabel).
%%
changeTreeApplied(VE):->
	
	%gp3 0.2: instead of using changeApplied, we update the window label only
	%after a whole changeTree is applied. Still not very efficient, but its a fast 
	%call
	%when mustUpdateVisualisation is set (a changeAppled_... call wants to do this)
	%we redraw all contents
	
	VE->>label(VE?makeWindowLabel),
	if
		@on = VE<<-slot(mustUpdateVisualisation)
	then
	(
		VE->>update_visualisation,
		VE->>slot(mustUpdateVisualisation,@off)
	).
%%
/***********DISPLAY NIEUWE OBJECTEN***********************/
%%

mustUpdateVisualisation(VE):->
	%gp3 0.2: set the flag that the next changeTreeApplied call should call update_visualisation
	VE->>slot(mustUpdateVisualisation,@on).
%%

%%
update_visualisation(VE, SkipSaveLayout: [bool]):->
	"Update the current fragment view" ::
	%gp3: this is the old homer update_visualisation code
	%gp3 0.4.6 added a flag SkipSaveLayout: @on means that we do not save the layout first (as in: never drawn before (in this window), or reposition elements). Default: @off
	
	/*
	Algemene herteken code: checkt welke onderdelen bijgetekend moeten worden, en welke onderdelen juist weg moeten (omdat ze niet meer voorkomen). Deze code kan dus na een update aangeroepen worden (niet efficient, wel heel praktisch).
	*/
	%gp3: this is old code, we like to do this much more efficient
	UpdatedElements *= chain, %bevat alle visuele elementen die niet weg moeten, alle overige kunnen na de update weg
	
	%gp3 0.4.6: because of routemapping a lot of elements get recreated in the process of drawing
	%to make sure positions stay the same, we have to save the layout info (but only if we are redrawing)
	
	unless
		SkipSaveLayout == @on
	do
		VE->>saveLayoutInfo,
	
	VE->>draw_fragment(VE?fragment,new(chain), @nil, normal,@off, UpdatedElements),
		%gp3 0.3 added ShowSubMFSymbols argument (default: @off)
	%alle elementen die zich niet in UpdatedElements bevinden kunnen nu weg
	VE?client?graphicals->>for_all(if(
			and(
				->>(@arg1,instance_of,visualElement),
				not(->>(UpdatedElements,member,@arg1))
				),
				->>(@arg1,destroy)
			)).
%%

%%
draw_fragment(VE,
	Fragment: modelFragment,
	Route: chain, %de route waarmee een getekend element terug gevonden kan worden. Bestaat uit importedFragment objecten
					%lege route betekent dat het element zich in het huidige fragment bevindt
	ContainedFragmentElement: 'importedMFElement*', %waaraan dus de instanties zich moeten koppelen
	State: {normal,imported,parent},
	ShowSubMFSymbols: bool,
	UpdatedElements: chain,
	RM: [chain]
	):->
	%helper bij update_visualisation
	%herteken een fragment, dit kan het bewerkte fragment zelf zijn of
	%een fragment dat daarin hoort (parent of imported)
	%checkt bij elk element of het niet al gevisualiseerd is
	%stopt alle nieuwe en relevante visuele elementen in UpdatedElements
	
	%do not use default/3 with new(..), because this will not evaluate directly
	%but each time the Var is used...
	
	if
		RM = @default
	then
		RouteMapping *= chain
	else
		RouteMapping = RM,
	
	%PARENT FRAGMENTEN
	/*
	Parent fragmenten zelf (dwz een icon ervoor) worden alleen afgebeeld voor de directe parents van
	het bewerkte fragment. De parents van parents fragmenten worden niet
	afgebeeld (maar hun elementen natuurlijk wel).
	*/
	
	Fragment?parentReferences->>for_all(
		if(
			not(
				->>(@arg1?referencedFragment?parents,empty) %geen topFragment
			),
			->>(VE,
				displayImportedFragment,
				parent,
				@arg1,
				Route,
				ContainedFragmentElement,
				State,
				ShowSubMFSymbols, %gp3 0.3: added ShowSubMFSymbols argument, see displayImportedFragment
				UpdatedElements,
				RouteMapping))),
	%CONDITIONELE FRAGMENTEN
	%die gaan eigenlijk hetzelfde
	%gp3: no subclasses here, only the importedFragments itself, we do fragmentRefiners differently
	?(Fragment,findElements,importedFragment,@off)->>for_all(
				->>(VE,displayImportedFragment,
					imported,
					@arg1,
					Route,
					ContainedFragmentElement,
					State,
					ShowSubMFSymbols, %gp3 0.3: added ShowSubMFSymbols argument, see displayImportedFragment
					UpdatedElements,
					RouteMapping)),
					

	%INSTANCES
	?(Fragment,findElements,garpInstance)->>for_all(
				->>(UpdatedElements,append,
						?(VE,
					      displayInstance,
					      @arg1,
							Route,
							ContainedFragmentElement,
							State))),
	%ASSUMPTIES
	Assumptions = Fragment<<-findElements(assumptionInstance),
	Assumptions->>for_all(
				->>(UpdatedElements,append,
						?(VE,
							displayAssumptionInstance,
							@arg1,
							Route,
							ContainedFragmentElement,
							State,UpdatedElements,RouteMapping))),
	%CONFIGURATIES
	Configs = Fragment<<-findElements(configuration),
	Configs->>for_all(
				->>(UpdatedElements,append,
						?(VE,
						displayConfiguration,
						@arg1,
						Route,
						ContainedFragmentElement,
						State, UpdatedElements,RouteMapping))),
	%QUANTITIES
	Quantities = Fragment<<-findElements(garpQuantity),
	Quantities->>for_all(
				->>(UpdatedElements,append,
					?(VE,displayQuantity,@arg1,Route,
						State,
						UpdatedElements,RouteMapping))),
						
	%ATTRIBUTES
	Attributes = Fragment<<-findElements(garpAttribute),
	Attributes->>for_all(
		->>(UpdatedElements,append,
					?(VE,displayAttribute,@arg1,Route,
						State,UpdatedElements,RouteMapping))),

	%VALUES
	Values = Fragment<<-findElements(value),
	Values->>for_all(
				->>(UpdatedElements,append,
					?(VE,displayValue,@arg1,Route,
						State,UpdatedElements,RouteMapping))),
						
	%CALCULI
	Calculi = Fragment<<-findElements(calculus),
	Calculi->>for_all(
				->>(UpdatedElements,append,
						?(VE,displayCalculus,@arg1,Route,
						ContainedFragmentElement,
						State,UpdatedElements,RouteMapping))),
						
	%INEQUALITIES
	Inequalities = Fragment<<-findElements(inequality),
	Inequalities->>for_all(
				->>(UpdatedElements,append,
						?(VE,displayInequality,@arg1,Route,
						ContainedFragmentElement,
						State,UpdatedElements,RouteMapping))),
						
	%CORRESPONDENCES
	?(Fragment,findElements,correspondence)->>for_all(
				->>(UpdatedElements,append,
						?(VE,
							displayCorrespondence,
							@arg1,Route,
							ContainedFragmentElement,
							State,UpdatedElements,RouteMapping))),
							
	%QUANTITY RELATIONS
	?(Fragment,findElements,garpQuantityRelation)->>for_all(
					->>(UpdatedElements,append,
						?(VE,
						displayQuantityRelation,
						@arg1, Route,
						ContainedFragmentElement,
						State,UpdatedElements,RouteMapping))),
						
	%IDENTITIES
	%only identityRelations, not fragmentRefinerIdentity objects
	?(Fragment,findElements,identityRelation,@off)->>for_all(
				->>(UpdatedElements,append,
						?(VE,
						displayIdentity,
						@arg1,
						Route,
						ContainedFragmentElement,
						State,UpdatedElements,RouteMapping))).
	
%%

displayImportedFragment(VE,
			Type: '{parent,imported, refiner}', %gp3 0.3 added refiner type
			IF: importedFragment,
			Route: chain, %de route tot nu toe
			ContainedFragmentElement: 'importedMFElement*',
			State: '{normal,parent,imported}',
			ShowSubMFSymbols: bool, %gp3 0.3: added ShowSubMFSymbols argument (never set right now, see draw_fragment)
			UpdatedElements: chain,
			RouteMapping: chain
			):-> 
			
	%Afbeelden van een parent of conditioneel fragment.
	%  Hiervoor tekenen we het fragment symbool, en daaronder alle elementen in de juiste state
	/*
	gp3 0.3 changed the way this works: it is now possible to show submf symbols
	Also changed this to a send method, so we can succeed without any adding of elements.
	
	Added new type 'refiner': this is for drawing fragmentRefiners. We need a lot of reasoning and abracadabra to get refinement drawing working without breaking other code
	
	Other stuff is unchanged, hence the dutch comments
	*/

	/*Als we al onderdeel zijn van een imported/parent fragment dan blijven we in diezelfde state zitten
	  anders gaan we naar een state van het gegeven type. Maar als we nog geen onderdeel van een imported/parent zijn
	  en het gaat om een geimporteerd fragment, dan wordt dat fragment zelf 'normal' en alles eronder 'imported'
	*/
	
	Continue = true, %helps us to be able to stop recursion when needed in a later version
	if
		Continue = true
	then
	(	
			
		if
			State == normal	%toegevoegd aan het huidige fragment
		then
		(
			Type == imported, ElementState =normal, SubState = imported
		;
			Type == parent, ElementState = parent, SubState = parent
		;
			Type == refiner, ElementState = normal, SubState = imported
		)
		else
		(
			ElementState = State,
			SubState = State
		),
		
		
		ContentData = VE<<-mapRefinements(IF,Route,RouteMapping),
		ContentFragment = ContentData<<-first,
		ContentRoute = ContentData<<-second,
		
		NewRoute = ContentRoute<<-copy,
		NewRoute->>append(ContentFragment), %voor de subelementen van dit fragment moet dit fragment erbij
	
		%gp3 0.3: when there is not yet a ContainedFragmentElement, we display the symbol (like homer did)
		%when there is a containedfragmentelement it depends
			
		if
		(
			ContainedFragmentElement == @nil
		;
			
			(
				(Type = imported ; Type = refiner),
				ShowSubMFSymbols = @on
			)
		)
		then 
		(
			%afbeelden van een element voor het fragment
			(
				(
					IMF = VE<<-findElement(ContentFragment,importedMFElement,ContentRoute),!,
					ignore(IMF->>updateDisplay)
				)
			;
				(
					VisArea = VE?client<<-visible,
					IMF *= importedMFElement(VE?fragment,ContentFragment,VE?client, ContainedFragmentElement,
								point(VisArea?left_side + VisArea?width / 5,
									VisArea?top_side + VisArea?height / 10), %defaultplek
									when(ContainedFragmentElement == @nil,
										ElementState,
										imported), ContentRoute),
					VE->>registerElement(IMF)
				)
			),
			
		
			%if this is a subsymbol, it should be connected now
			unless
				ContainedFragmentElement = @nil
			do
				ContainedFragmentElement->>connectSub(IMF),
	 
			%showing of subsymbols is something that can be set to @on or @off in the 
			%top symbols, not in the subsymbols: its either all or nothing.
			if
				@on = IMF<<-subMFsShown
			then
				NewShowSubMFSymbols = @on
			else
				NewShowSubMFSymbols = ShowSubMFSymbols
		)
		else
		(
			IMF = ContainedFragmentElement, %anders deze weer gebruiken
			NewShowSubMFSymbols = ShowSubMFSymbols % allways @off
		),
		
		UpdatedElements->>append(IMF),
		

		VE->>draw_fragment(ContentFragment?referencedFragment,NewRoute,IMF,SubState, NewShowSubMFSymbols, UpdatedElements,RouteMapping)
	).
%

mapRefinements(VE,IF: importedFragment,Route: chain, RouteMapping: chain,
	ContentData: tuple):<-

		
	%map any refinements on the route to the current IF
	%this way finding the if that needs to be used. We add all mapping to
	%routemapping
	
	%during the mapping the route to the mapped fragment can change
	%because fragmentRefiners are kept in the fragment they belong to,
	%and cut off a route (the rest of which is in there ?refinedRoute member)
	%so the result is a tuple(fragment,route).
	
	%there is no need to map the route first, because
	%it will be checked all the way

	%recursive call, we fall back to prolog
	FR = VE<<-fragment,
	
	RouteDone *= chain,
	pl_mapRefinements(FR,FR,IF,Route,RouteDone,RouteMapping,Route,ContentFragment,ContentRoute),
	ContentData *= tuple(ContentFragment,ContentRoute).
%
pl_mapRefinements(FR,_FirstFR, IF, Route, RouteDone, RouteMapping, FullRoute,ContentFragment, ContentRoute):-

	%empty route, which means we have done al up to the fragment containing IF
	%check this level
	
	Route->>empty,
	pl_mapRefinements_checkLevel(FR,IF,Route,RouteDone,RouteMapping,FullRoute,ContentFragment,ContentRoute).
%
pl_mapRefinements(FR,FirstFR, IF, Route,RouteDone,RouteMapping, FullRoute, ContentFragment, ContentRoute):-

	%recurse first
	
	RestRoute = Route<<-copy,
	NextStep = RestRoute<<-delete_head,
	NextFR = NextStep<<-referencedFragment,
	NewRouteDone = RouteDone<<-copy,
	NewRouteDone->>append(NextStep),
	pl_mapRefinements(NextFR,FirstFR,IF,RestRoute,NewRouteDone,RouteMapping,FullRoute,NewIF,NewRoute),
	%restart with this information, if needed, in which case we are done
	unless
		NewIF == IF %no refinement
	do
	(
		%restart, NewRoute is the new route seen from FirstFR, so also Fullroute for mapping
		RestartRouteDone *= chain,
		pl_mapRefinements(FirstFR,FirstFR,NewIF,NewRoute,RestartRouteDone,RouteMapping, NewRoute, ContentFragment,ContentRoute)
	)
	else
		%check refiners at our level
		pl_mapRefinements_checkLevel(FR,IF,Route,RouteDone,RouteMapping,FullRoute, ContentFragment,ContentRoute).
%

pl_mapRefinements_checkLevel(FR,IF,Route,RouteDone, RouteMapping,FullRoute,NewIF,RouteDone):-
	NewIF = FR<<-refinementFor(IF,Route),!,
	%found, we add this mapping
	OldRoute = FullRoute<<-copy,
	OldRoute->>append(IF),
		%newroute is the route to this element + the refiner + refinement route
		NewRoute = RouteDone<<-copy,
		NewRoute->>append(NewIF),
		NewRoute->>merge(NewIF?refinementRoute),
		RouteMapping->>append(tuple(OldRoute,NewRoute)).

pl_mapRefinements_checkLevel(_FR,IF,Route,_RouteDone,_RouteMapping,_FullRoute,IF,Route).
	%no refiners here


%
routeMapping(VE,RouteMap: chain, RoutePart1: chain, RoutePart2: [chain], NewFullRoute: chain):<-
	%gp3 0.3
	%
	%look in the routemapping chain for mappings of old routes to new (refined) routes, and return the newFullRoute part
	%
	
	%for reasoning about refiners, RoutePart1 and RoutePart2 are merged
	%this is just for convenience in calling
	%the returned route is always a full one. This is needed because
	%mapping of routes may change even the first part (when our fragment
	%refines something)
	%
	%we do this recursively to get mapping on mapping when there is refinement on refinement
	
	FullRoute = RoutePart1<<-copy,
	unless
		RoutePart2 = @default
	do
		FullRoute->>merge(RoutePart2),
	
	if
		Found = RouteMap<<-find(->>(FullRoute,is_head,@arg1?first))
	then
	(
		%recursive call.
		%new elementroute is the found route
		%with the non-mapped part of the old route at the back
		NewCheckRoute = Found?second<<-copy,
		RestElementRoute = FullRoute<<-sub(Found?first?size),
		NewCheckRoute->>merge(RestElementRoute),
		NewFullRoute = VE<<-routeMapping(RouteMap,NewCheckRoute) %try again with new route, add extras at the back
	)
	else
		NewFullRoute = FullRoute<<-copy.

%%

%%
displayInstance(VE,
		   I : garpInstance,
			Route: chain,
			IMF: 'importedMFElement*',
		   FragmentState: '[{normal,parent,imported}]',
		   IE: instanceElement
		  ):<-
	"Display this instance" ::

	%we maken het instance element en melden hoe ie afgebeeld moet worden
	%ook sturen we ons fragment mee, zodat eventueel opgeslagen lay-out info
	%opgehaald wordt. Het element wordt dan meteen afgebeeld.
	%wanneer er een importedMFElement is meegestuurd wordt de instance daar een sub van
	
		%sub van geïmporteerd fragment?
		default(IMF,@nil,RC),

	(
		(
			IE = VE<<-findElement(I,instanceElement,Route),
			ignore(IE->>updateDisplay)
		)
	;
		(
			%gp3 0.4.11: we do not send a point for placement, but only the IMF
			%when this is @nil , placement will be according to visibleSub strategy
			%otherwise we use 'spot' with a default startingpoint
			%gp3 1.0: super element now standard argument in visualElement
			IE *= instanceElement(VE?fragment,
						I,
						VE?client,
						RC,
						FragmentState,Route), 
			VE->>registerElement(IE)
		)
	),

	if
		RC \== @nil
	then
		RC->>connectSub(IE).
%%

%%
displayAssumptionInstance(VE,
	AI: assumptionInstance,
	Route: chain,
	IMF: '[importedMFElement*]',
	FragmentState: '[{normal,parent,imported}]',
	UpdatedElements: chain,
	RouteMap: chain,
	AIE: assumptionInstanceElement
	):<-
	"Display this assumption instance" ::
	%gp3: we now have 2 possible situations:
	%1. The AI is free floating in its modelFragment
	%2. It is connected to a garpInstance, in which case we always have to redraw, because the instantelement can change after refinement

	if
		@nil = AI<<-garpInstance
	then
	
	(
		(
			AIE = VE<<-findElement(AI,assumptionInstanceElement,Route),
			ignore(AIE->>updateDisplay)
		)
	;
		(
			AIE *= assumptionInstanceElement(
			VE?fragment,
			AI,
			VE?client,
			IMF,	%gp3 0.4.11 we send the superelement
			FragmentState,Route),
			VE->>registerElement(AIE),
			%if it is in an imported fragment, register it as a sub
			default(IMF,@nil,RC),
			if
				RC \== @nil
			then
				RC->>connectSub(AIE)
		)
	)
	else
	(
		%connected to an instance, redraw
		MRoute = VE<<-routeMapping(RouteMap,Route,AI?instanceRoute), %gives full route

		IE = VE<<-findElement(AI?garpInstance,instanceElement,MRoute,@default,UpdatedElements), %only search new instances. 
		AIE *= assumptionInstanceElement(
			VE?fragment,
			AI,
			VE?client,
			IE,	%gp3 0.4.11 we send the superelement
			FragmentState,Route),
		VE->>registerElement(AIE),
		IE->>connectSub(AIE) %sub for this garpinstance
	).
%%

%%
displayConfiguration(VE,
	C: configuration,
	Route: chain,
	_IMF: '[importedMFElement*]',
	FS: '[{normal,parent,imported}]',
	UpdatedElements: chain,
	RouteMap: chain,
	CE: configurationElement):<-
	"Display this configuration" ::

	%argumenten vinden
	Arg1 = C<<-argument1,
	Arg2 = C<<-argument2,
		
	Arg1R = VE<<-routeMapping(RouteMap,Route,C?argument1Route), %gives full route
	Arg2R = VE<<-routeMapping(RouteMap,Route,C?argument2Route),
	
	if
		Arg1 = @nil
	then
		Arg1El = @default
	else
		Arg1El = VE<<-findElement(Arg1,instanceElement,Arg1R,@default,UpdatedElements),
		
	if
		Arg2 = @nil
	then
		Arg2El = @default
	else
		Arg2El = VE<<-findElement(Arg2,instanceElement,Arg2R,@default,UpdatedElements),
		
	(
		(
			CE = VE<<-findElement(C,configurationElement,Route),
			ignore(CE->>setArguments(Arg1El,Arg2El)) %doet ook updateDisplay
		)
	;
		(

			CE *= configurationElement(VE?fragment,
					C, VE?client, Arg1El,Arg2El,FS,Route), %punt wordt door relatie bepaalt
			VE->>registerElement(CE)
		)
	).

%%


%%
displayQuantity(VE,
	Q: garpQuantity,
	Route: chain,
	FragmentState: '[{normal,parent,imported}]',
	UpdatedElements: chain,
	RouteMap: chain,
	QE: quantityElement):<-
	"Display a quantity element" ::
	%hidden state wordt door het element genomen (opgeslagen info e.d.)
	%UpdatedElements nodig vanwege subelementen qs en dqs..


	InstanceRoute = VE<<-routeMapping(RouteMap,Route,Q?instanceRoute),%gives full route
		
	%gp3 0.3 because of routemapping, we cannot use quick updateDisplay code
	%because the quantity might get connected to another instance element
	%we could check this first, but that is as slow as just redrawing
	

	IE = VE<<-findElement(Q?garpInstance,instanceElement,InstanceRoute,@default,UpdatedElements),
	QE *= quantityElement(	
		VE?fragment,
		Q,
		VE?client,
		IE, 
		FragmentState,Route),
	VE->>registerElement(QE),
	IE->>connectSub(QE),
	%en de qs er aan vast maken (veel interne subelementen)
	%gp3 0.4.11: we do a visubleSub placement now, so this call changed. We send the QE along
	QS *= qsElement(VE?fragment,Q,VE, QE,Route),
	VE->>registerElement(QS),
	QE->>connectSub(QS),

	UpdatedElements->>append(QS),
	UpdatedElements->>merge(QS?qsSubs),
	%gp3 0.4.11: rewrite of dqs placement. Because we now just use the 'visibleSub' strategy
	%there is no need for any calculation
	DQS *= derivativeElement(VE?fragment,Q,VE,QE,Route), %again, we send the QE along
	
	VE->>registerElement(DQS),
	QE->>connectSub(DQS),

	UpdatedElements->>append(DQS),
	UpdatedElements->>merge(DQS?qsSubs).
%%


%%
displayAttribute(VE,
	A: garpAttribute,
	Route: chain,
	FragmentState: '[{normal,parent,imported}]',
	UpdatedElements: chain,
	RouteMap: chain,
	AE: attributeElement):<-
	"Display an attribute element" ::

	%gp3 0.3: remap. Cannot use quick updateDisplay (see displayQuantity)
	
	InstanceRoute = VE<<-routeMapping(RouteMap,Route,A?instanceRoute),%gives full route
	IE = VE<<-findElement(A?garpInstance, instanceElement, InstanceRoute,@default,UpdatedElements),
	AE *= attributeElement(VE?fragment,
			A, VE?client,
			IE, %gp3 0.4.11: we send the IE instead of a point, using the visibleSubs strategy
			FragmentState,Route),
	VE->>registerElement(AE),
	IE->>connectSub(AE).
%%
				
%%
displayValue(VE,
	V: value,
	Route: chain,
	FragmentState: '[{normal,parent,imported}]',
	UpdatedElements: chain,
	RouteMap: chain,
	VM: valueMarker):<-
	"Display this value" ::

	Q = V<<-quantity,
	
	QR = VE<<-routeMapping(RouteMap,Route,V?quantityRoute), %gives full route

	%en zoek het juiste element
	ValEl = VE<<-findValueElement(Q,V?valueReference,QR,@default,UpdatedElements),

	%deze doen we altijd opnieuw, oude wordt verwijderd (doordat er een nieuwe is)
	VM *= valueMarker(VE?fragment,
						V,
						ValEl,
						FragmentState,Route), %doet ook het tekenen
	VE->>registerElement(VM).
%%

%%
displayCalculus(VE,
	C: calculus,
	Route: chain,
	_IMF: '[importedMFElement*]',
	FS: '[{normal,parent,imported}]',
	UpdatedElements: chain,
	RouteMap: chain,
	CE: calculusElement):<-

	%maak een nieuw calculusElement en beeldt hem af
	%eerst moeten we de argumenten vinden

	Type = C<<-type,
	A1 = C<<-argument1,
	A1P = C<<-argument1QSPoint,
	A2 = C<<-argument2,
	A2P = C<<-argument2QSPoint,

	%gp3: map routes. We can use updateDisplay, because relations can change arguments dynamically
	R1 = VE<<-routeMapping(RouteMap,Route,C?argument1Route), %gives full route
	R2 = VE<<-routeMapping(RouteMap,Route,C?argument2Route),
	
	pl_calculusarg(VE,Type,A1,A1P,R1,UpdatedElements,Element1), %zie onder
	pl_calculusarg(VE,Type,A2,A2P,R2,UpdatedElements,Element2),

	(
		(
			CE = VE<<-findElement(C,calculusElement,Route),
			ignore(CE->>setArguments(Element1,Element2)) %werk argumenten bij, doet ook updateDisplay
		)
	;
		(
			CE *= calculusElement(VE?fragment,C, VE?client, Element1, Element2,
				FS,Route), %punt wordt tussen de twee arg2 berekent
			VE->>registerElement(CE)
		)
	).
%
%pl_calculusarg: geeft op basis van type vd calc, klasse van gezocht arg, routes en eventueel waardepunt
%het bijbehorende element terug
pl_calculusarg(VE,_Type,Arg,_Point,FullRoute,Updated,El):-
	Arg->>instance_of(calculus),!,
	%dan altijd een calculus natuurlijk
	El = VE<<-findElement(Arg,calculusElement,FullRoute, @default, Updated).

pl_calculusarg(VE,quantity,Arg,@nil,FullRoute,Updated,El):-!,
	El = VE<<-findElement(Arg,quantityElement,FullRoute,@default, Updated). %want geen calculus
%
pl_calculusarg(VE,quantity,Arg,Point,FullRoute,Updated,El):-
	%dit moet wel een waarde-punt zijn
	El = VE<<-findValueElement(Arg,Point,FullRoute,@default,Updated).
%
pl_calculusarg(VE,derivative,Arg,_Point,FullRoute,Updated,El):-
	%wederom calculus of quantity, maar calculus al hierboven
	El = VE<<-findElement(Arg,derivativeElement,FullRoute,@default, Updated).
%%

%%
displayInequality(VE,
	I: inequality,
	Route: chain,
	_IMF: '[importedMFElement*]',
	FS: '[{normal,parent,imported}]',
	UpdatedElements: chain,
	RouteMap: chain,
	IE: inequalityRelationElement):<-
	%display  inequality

	%argumenten vinden
	%gp3 0.3: map routes
	
	A1 = I<<-argument1,
	T1 = I<<-argument1Type,
	R1 = VE<<-routeMapping(RouteMap,Route,I?argument1Route), %gives full route
	P1 = I<<-argument1QSPoint,
	A2 = I<<-argument2,
	T2 = I<<-argument2Type,
	R2 = VE<<-routeMapping(RouteMap,Route,I?argument2Route),
	P2 = I<<-argument2QSPoint,

	pl_findInequalityArgument(VE,A1,T1,R1,P1,UpdatedElements,El1),
	pl_findInequalityArgument(VE,A2,T2,R2,P2,UpdatedElements,El2),

	(
		(
			IE = VE<<-findElement(I,inequalityRelationElement,Route),
			ignore(IE->>setArguments(El1,El2)) %bijwerken argumenten
		)
	;
		(
			IE *= inequalityRelationElement(VE?fragment,
				I, VE?client, El1,El2,FS,Route), %punt wordt door relatie bepaalt
			VE->>registerElement(IE)
		)
	).
%
pl_findInequalityArgument(_,_,@nil,_,_,_,@default). %is er niet
pl_findInequalityArgument(VE,Calc,calculus,ArgRoute,_,UpdatedElements,El):-
	El = VE<<-findElement(Calc,calculusElement,ArgRoute,@default,UpdatedElements).
pl_findInequalityArgument(VE,Q,currentValue,ArgRoute,_,UpdatedElements,El):-
	El = VE<<-findElement(Q,quantityElement,ArgRoute,@default,UpdatedElements).
pl_findInequalityArgument(VE,Q,currentDerivative,ArgRoute,_,UpdatedElements,El):-
	El = VE<<-findElement(Q,derivativeElement,ArgRoute,@default,UpdatedElements).
pl_findInequalityArgument(VE,Q,derivativeZero,ArgRoute,_,UpdatedElements,El):-
	%die is lastiger
	El = VE<<-findDerivativeValueElement(Q,'Zero',ArgRoute,@default,UpdatedElements).
pl_findInequalityArgument(VE,Q, quantityQSPoint,ArgRoute,V,UpdatedElements,El):-
	El = VE<<-findValueElement(Q,V,ArgRoute,@default, UpdatedElements).
%%

%%
displayCorrespondence(VE,
	C: correspondence,
	Route: chain,
	_IMF: '[importedMFElement*]',
	FS: '[{normal,parent,imported}]',
	UpdatedElements: chain,
	RouteMap: chain,
	CE: correspondenceElement):<-
	%display een lokale correspondence

	%gp3 0.3: routemapping
	R1 = VE<<-routeMapping(RouteMap,Route,C?argument1Route), %gives full route
	R2 = VE<<-routeMapping(RouteMap,Route,C?argument2Route),
	
	%gp3: getting the type could be easier, but anyhow
	
	if
		@nil = C<<-argument1Value
	then 
	( %qs correspondentie. gp3 0.3: or full
		if
			@on = C<<-full
		then
			ElType = quantityElement
		else
		(
			if
				@on = C<<-derivative
			then %afgeleide
				ElType = derivativeElement
			else
				ElType = qsElement
		),
		Arg1 = VE<<-findElement(C?argument1,ElType,R1,@default,UpdatedElements),
		Arg2 = VE<<-findElement(C?argument2,ElType,R2,@default,UpdatedElements)
	)
	else %value correspondence (afgeleiden gaan vanzelf goed, door de valueref)
		(
		Arg1 = VE<<-findValueElement(C?argument1,
								   C?argument1Value,R1,@default,UpdatedElements),
		Arg2 = VE<<-findValueElement(C?argument2,
								   C?argument2Value,R2,@default,UpdatedElements)
		),

	(
		(
			CE = VE<<-findElement(C,correspondenceElement,Route),
			ignore(CE->>setArguments(Arg1,Arg2))	%bijwerken argumenten
		)
	;
		(
			CE *= correspondenceElement(VE?fragment,
							C,
							VE?client,
							Arg1,Arg2,
							FS,Route),
			VE->>registerElement(CE)
		)
	).
%

%%
displayQuantityRelation(VE,
	QR: garpQuantityRelation,
	Route: chain,
	_IMF: '[importedMFElement*]',
	FS: '[{normal,parent,imported}]',
	UpdatedElements: chain,
	RouteMap: chain,
	QRE: quantityRelationElement):<-
	%beeld een lokale quantityRelation af

	%gp3 0.3: map routes
	R1 = VE<<-routeMapping(RouteMap,Route,QR?argument1Route), %gives full route
	R2 = VE<<-routeMapping(RouteMap,Route,QR?argument2Route),
	
	Arg1 = VE<<-findElement(QR?argument1,quantityElement,R1,@default,UpdatedElements),
	Arg2 = VE<<-findElement(QR?argument2,quantityElement,R2,@default,UpdatedElements),

	(
		(
			QRE = VE<<-findElement(QR,quantityRelationElement,Route),
			ignore(QRE->>setArguments(Arg1,Arg2)) %werk argumenten bij, doet ook updateDisplay
		)
	;
		(
			QRE *= quantityRelationElement(VE?fragment,
								QR,
								VE?client,
								Arg1,
								Arg2,
								FS,Route),
			VE->>registerElement(QRE)
		)
	).
%%

%%
displayIdentity(VE,
	I: identityRelation,
	Route: chain,
	_IMF: '[importedMFElement*]',
	FS: '[{normal,parent,imported}]',
	UpdatedElements: chain,
	RouteMap: chain,
	IRE: identityRelationElement):<-
	"Display this identity" ::

	%argumenten vinden
	Arg1 = I<<-argument1,
	Arg2 = I<<-argument2,
	
	%gp3 0.3: map routes
	R1 = VE<<-routeMapping(RouteMap,Route,I?argument1Route), %gives full route
	R2 = VE<<-routeMapping(RouteMap,Route,I?argument2Route),
	
	if
		Arg1 = @nil
	then
		Arg1El = @default
	else
		Arg1El = VE<<-findElement(Arg1,instanceElement,R1,@default,UpdatedElements),
		
	if
		Arg2 = @nil
	then
		Arg2El = @default
	else
		Arg2El = VE<<-findElement(Arg2,instanceElement,R2,@default,UpdatedElements),
		
	(
		(
			IRE = VE<<-findElement(I,identityRelationElement,Route),
			ignore(IRE->>setArguments(Arg1El,Arg2El)) %doet ook updateDisplay
		)
	;
		(

			IRE *= identityRelationElement(VE?fragment,
					I, VE?client, Arg1El,Arg2El,FS,Route), %punt wordt door relatie bepaalt
			VE->>registerElement(IRE)
		)
	).

%%

%%
registerElement(VE,
	     Element: graphical
	    ):->
	"Init a new element for display (add gestures)" ::
	%Gebruikt bij het aanmaken van elementen in deze klasse, maar ook als callback
	%bij subklassen van visualElement als er interne subelementen worden aangemaakt
	%(zoals qsElement en dqsElement doen met qsValueElement en dqsValueElement)

	 Element->>recogniser(VE?contextMenu), %voor de andere muisakties, anders werkt de dubbelklik niet
	 Element->>recogniser(VE?moveSelect),
	 Element->>recogniser(VE?moveSelectSingle), %gp3 0.3
	 Element->>recogniser(VE?toggleSelect),
	 Element->>recogniser(VE?toggleSelect_alternative),
	 VE->>hyper(Element,element). %info dat ie er bij hoort
%%

/******************** UPDATE BY ELEMENT ************/
%gp3 code to try and make updates etc more efficient

%%
updateElement(VE,
	FragmentElement: fragmentElement):->
%gp3 0.2: send all visualElements displaying this element the  updateDisplay message
%does not work very well, especially for relations, so we do not use it often

	?(VE,findElements,FragmentElement)->>for_all(
		if(
			->>(@arg1,instance_of,visualElement),
			->>(@arg1,updateDisplay)
		)
	).
%%

%%
removeElement(VE,
	FragmentElement: fragmentElement):->
%gp3 0.2: delete all visualElements displaying this element the delete message
%no recursive calling over subs, so we need to call for every sub
%problem is that this is even less efficient than a whole update
%so we use mustUpdateVisualisation for subchanges, and do the update
%on changeTreeAppled

	?(VE,findElements,FragmentElement)->>for_all(
		if(
			->>(@arg1,instance_of,visualElement),
			->>(@arg1,destroy))).
%%
		
/********************HELPERS***********************/
%%
findElement(VE,
	    FragmentElement : any,
	    ClassName : name,
		Route: route = [chain],
		ImportedRoute: importedRoute = [chain],
		UpdatedElements: updated = [chain],
	    Element : visualElement):<-
	"Finds the first visual element visualising the fragment element" ::
	%Route is een gedeelte van de route en ImportedRoute een ander gedeelte
	%wanneer een object bijvoorbeeld zelf route [a,b,c] heeft en verwijst naar
	%een object met route [d], dan is die verwijzing [d] gezien vanaf de oorspronkelijke
	%route [a,b,c]. We vinden het tweede object dan dus op route [a,b,c,d]
	%om niet overal merge te hoeven gebruiken doen we dat hier
	%MET ROUTE!
	
	%gp3 0.3: added UpdatedElements argument, when given, these elements
	%are the only ones searches. When not given, all elements are searched
	%also the ones that will be removed after an update.
	
	default(Route,new(chain),DRoute),
	default(ImportedRoute,new(chain),DImportedRoute),
	RRoute = DRoute<<-merge(DImportedRoute),
	default(UpdatedElements,VE?client?graphicals,Elements),
	Element = Elements<<-find(
		and(@arg1?name == ClassName,
			@arg1?fragmentElement == FragmentElement,
			->>(@arg1?route,equal,RRoute))).
%%

%%
findElements(VE,
	FragmentElement: any,
	ClassName: [name],
	Elements: chain):<-
	%geef chain van alle elementen die op de een of andere wijze een bepaald object visualiseren
	%eventueel alleen op classnaam

	All = VE?elements<<-find_all(@arg1?fragmentElement == FragmentElement),
	if
		ClassName == @default
	then
		Elements = All
	else
		Elements = All<<-find_all(@arg1?name == ClassName).
%%

%%
findFirstElement(VE,
	FragmentElement: any,
	ClassName: [name],
	Element: graphical):<-
	%als findElements, maar geeft maar één element terug (eerstgevonden)
	%en alleen visualElement subs
	
	Element = VE?client?graphicals<<-find(and(
					->>(@arg1,instance_of,visualElement),
					@arg1?fragmentElement == FragmentElement,
					or(
						ClassName == @default,
						@arg1?name == ClassName
						))).
%%
					
%%
findValueElement(VE,
	Q: garpQuantity,
	V: valueReference,
	Route: route = [chain],
	ImportedRoute: importedRoute = [chain],
	UpdatedElements: updated = [chain],
	ValEl: visualElement):<-
	"Returns (d)qsValueElement representing the value reference or fails" ::
	%zelfde route informatie als bij findElement

	
	if
		Route == @default
	then
		RRoute *= chain
	else (
		if
			ImportedRoute == @default
		then
			RRoute = Route
		else
			RRoute = Route<<-merge(ImportedRoute)
		),
	
	default(UpdatedElements, VE?elements, Elements),
	ValEl = Elements<<-find(and(
						@arg1?fragmentElement == Q,						
						or(@arg1?name == dqsValueElement, @arg1?name == qsValueElement),
						->>(@arg1?route,equal,RRoute),
						->>(@arg1?valueRef,sameValue,V))).
%%

%%
findDerivativeValueElement(VE,
	Q: garpQuantity,
	Name: {'Plus','Zero','Min'},
	Route: route = [chain],
	ImportedRoute: importedRoute = [chain],
	UpdatedElements: updated = [chain],
	El: visualElement):<-

	"Returns dqsValueElement representing the given d value for the given quantity" ::
	%zelfde routeinfo als bij findElement

	%in een keer opvragen, want anders krijgen we allerlei problemen ivm het feit
	%dat @model?dqs?values een kopie oplevert, dus als je Ref = @model?dqs?values<<-find...
	%doet dan is het resultaat al weer weg voordat je het kan gebruiken, omdat values dan
	%bevrijdt wordt...

	El = VE<<-findValueElement(Q,?(@model?dqs?values,find,@arg1?valueName == Name),Route,ImportedRoute,UpdatedElements).
%%


%%
elements(VE,
	E: chain):<-
	"Return all visualElements" ::

	E = VE<<-all_named_hypered(element).
%%

%%
topElements(VE,
	TE: chain):<-
	"Return all visualElements that have no super" ::

	TE = VE?elements<<-find_all(not(@arg1?super)).
%%


%%
makeWindowLabel(VE,
		Label2 : name
		) :<-
	"Internal: create caption" ::

	if
		VE?fragment->>instance_of(inputSystem)
	then
		Label *= string('%s %s - Scenario Editor - Build',
			VE?fragment?name,
			when(->>(VE?fragment,isIncomplete),
				'[INCOMPLETE!]',
				''))		
	else
		Label *= string('%s (%s) %s - Model Fragment Editor - Build', 
			VE?fragment?name,
			VE?fragment?type,
			when(->>(VE?fragment,isIncomplete),
					'[INCOMPLETE!]',
					'')),
	get(@model, getModelNameForEditor, Label?value, Label2).

%%

%%
selection(VE,
	  Selection : chain):<-
	"Returns the selection in a chain" ::

	Selection = VE?client<<-selection.
%%

%%
singleSelection(VE,
		Element: graphical
	       ):<-
	"Returns the single selection, fails if none or multi" ::

	Sel = VE<<-selection,
	1 = Sel<<-size,
	Element = Sel<<-head,
	Sel->>done.
%%

%%
checkHideNewElement(VE,
	E: visualElement,
	CR: changeRequestor):->

	/*Bepaalt of een nieuw afgebeeld element hidden moet worden. Alleen na changeApplied.
	Er wordt 
van de hidden state van het nieuwe element afgebleven (dus zichtbaar) tenzij:
	De CR?editor niet deze editor is en het element een super heeft die verbergbare subs heeft die allemaal
        verborgen zijn en de super zelf zichtbaar is.
	*/

	if (
		\+ CR->>checkEditor(VE),
		Sup = E<<-super,
		Subs = Sup<<-subs,
		\+ Subs->>empty,
		\+ Subs->>find(and(
						@arg1 \== E,
						->>(@arg1,canHide),
						@arg1?hidden == @off))
		)
	then
		E->>setHidden(@on).
%%
		
%%
/*
checkEditorState: Een algemene call voor het checken op de editor state.
Prototype:
checkEditorState(VE,AllowedFragments,DisallowedFragments,
		      AllowedSelectionTypes, AllowedCombinations)
		      
AllowedFragments: Een lijst met typen fragmenten die mogen, of any (geen check)
                  [processFragment,agentFragment]
DisallowedFragments: Een lijst met typen fragmenten die niet mogen, of any (geen check)
		  [staticFragment]
AllowedSelectionTypes: Een lijst met soorten selectie die mogen (none, single, multi of een aantal)
                                 of any (geen check)
		  [none,multi]
AllowedCombinations: Een lijst met daarin lijsten die aangeven wat mag ([Combination,...]) of any (geen check)
Combination: [{elementtype|any}/{[State,...]|any},....] of [{elementtype|any},...]/{[State,...]|any} 
State: condition|consequence|imported|parent|fixed	

Voor AllowedFragments, DisallowedFragments, AllowedSelectionTypes en de Statelijst in AllowedCombinations
geldt dat er geen lijst hoeft te worden gebruikt als het om maar één element gaat.

Voorbeeld
checkEditorState(VE,[processFragment,agentFragment],any,2,
		 [[quantityElement/condition,quantityElement/any],
		  [calculusElement,calculusElement]/[condition,imported]]).
Deze call slaagt wanner:
1 Het bewerkte modelfragment een processFragment of agentFragment is
2 Er meer 2 elementen geselecteerd zijn
3 Er een conditionele quantity en nog een quantity is geselecteerd OF er twee conditionele of geimporteerde
  calculi zijn geselecteerd

Los te gebruiken zijn ook checkES_Fragments, _DisallowedFragments, _Selection
en _Combinations.
*/

checkEditorState(VE, AllowedFragments,
		 DisallowedFragments,
		 AllowedSelectionTypes,
		 AllowedCombinations):-

	checkES_Fragments(VE,AllowedFragments),
	checkES_DisallowedFragments(VE,DisallowedFragments),
	checkES_Selection(VE,AllowedSelectionTypes),
	checkES_Combinations(VE,AllowedCombinations).
%
checkES_Fragments(_VE,any):-!.

checkES_Fragments(VE,Allowed):-
	C = VE?fragment<<-currentType,
	member(C,Allowed).

checkES_Fragments(VE,Allowed):-
	%geen lijst
	Allowed = VE?fragment<<-currentType.
%
checkES_DisallowedFragments(_VE,any):-!.

checkES_DisallowedFragments(VE,Disallowed):-
	C = VE?fragment<<-currentType,
	\+ member(C,Disallowed), %lijst
	\+ Disallowed = C. %geen lijst
%
checkES_Selection(_VE,any):-!.

checkES_Selection(VE,Selection):-
	N = VE?client?selection<<-size,
	checkES_SelectionNum(Selection,N).
%
checkES_SelectionNum(Selection,0):-
	(   member(none,Selection)
	;   Selection = none
	),!.

checkES_SelectionNum(Selection,1):-
	(   member(single,Selection)
	;   Selection = single
	),!.

checkES_SelectionNum(Selection,N):-
	N > 1,
	(   member(multi,Selection)
	;   Selection = multi
	),!.

checkES_SelectionNum(Selection,N):-
	(   member(N,Selection)
	;   Selection = N
	).
%
checkES_Combinations(_VE,any):-!.

checkES_Combinations(VE,Combinations):-
	SelChain = VE?client<<-selection,
	chain_list(SelChain,Selection),
	checkES_SelCombination(Selection,Combinations).
%
checkES_SelCombination(Selection,[First/GeneralState|_Rest]):-
	checkES_makeSpecificState(First,GeneralState,Specific),
	checkES_checkCombination(Selection,Specific),!.

checkES_SelCombination(Selection,[First|_Rest]):-
	checkES_checkCombination(Selection,First),!.

checkES_SelCombination(Selection,[_First|Rest]):-
	checkES_SelCombination(Selection,Rest).
%
checkES_makeSpecificState([],_GeneralState,[]).

checkES_makeSpecificState([First|Rest],GeneralState,[First/GeneralState|SpecificRest]):-
	checkES_makeSpecificState(Rest,GeneralState,SpecificRest).
%
checkES_checkCombination(Selection,Combination):-
	%deze is lastig, er kunnen meerdere mappings op elk element van de combinatie
	%geprobeerd worden, dus dat moeten we eerst uitwerken
	checkES_makeAllCombinations(Combination,AllCombinations),
	checkES_matchCombinations(Selection,AllCombinations).
%
%bij het maken van de combinaties is de situatie met nog maar 1 element te gaan een speciaal geval
%deze moet namelijk de basic combinaties bouwen
%bij-effect: als state geen lijst is, dan wordt het dat hier

checkES_makeAllCombinations([One/[State]], [[One/State]]):-!.

checkES_makeAllCombinations([One/[State|States]],[[One/State]|Others]):-!,
	checkES_makeAllCombinations([One/States],Others).

checkES_makeAllCombinations([One/State], [[One/State]]):-!. %ook /any

%en de situatie voor de overige elementen:
checkES_makeAllCombinations([Element|Rest],Combinations):-
	checkES_makeAllCombinations(Rest,RestCombinations),
	checkES_makeCombinationElement(Element,RestCombinations,Combinations).
%
checkES_makeCombinationElement(_El/[],_RestCombinations,[]):-!.

checkES_makeCombinationElement(El/[State|States],CurrentCombinations,Combinations):-!,
	checkES_makeCombinationElement(El/States,CurrentCombinations,NewCombinations),
	checkES_appendElementState(El,State,CurrentCombinations,NewCombinations,Combinations).

checkES_makeCombinationElement(El/State,CurrentCombinations,Combinations):- %ook any
	checkES_appendElementState(El,State,CurrentCombinations,[],Combinations). 
%
checkES_appendElementState(_El,_State,[], NewCombinations, NewCombinations).

checkES_appendElementState(El,State,[First|RestCombinations],AddedCombinations,Combinations):-
	checkES_appendElementState(El,State,RestCombinations,[[El/State|First]|AddedCombinations],
				   Combinations).
%
checkES_matchCombinations(Selection,[First|_Rest]):-
	checkES_combinationSelected(Selection,First),!.

checkES_matchCombinations(Selection,[_First|Rest]):-
	checkES_matchCombinations(Selection,Rest).
%
checkES_combinationSelected([],[]):-!.

checkES_combinationSelected(Selection,[Element/State|RestElements]):-!,
	select(SelElement,Selection,RestSelection),
	(   Element == any
	;   Element = SelElement<<-name
	),
	(   State == any
	;   State = SelElement<<-state
	),
	%1 gevonden, door met de rest
	checkES_combinationSelected(RestSelection,RestElements).
%%

:-pce_end_class.
