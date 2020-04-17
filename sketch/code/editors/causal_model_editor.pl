/*
Definition of causal_model_editor class
Editor for causal model 

Part of Garp3, see copyright notice
Code partly inspired by old homer code
2000-2005 Jelmer Jellema, Spin in het Web, spininhetweb.nl
2006 Anders Bouwer 
*/                                                                                                                                                                                                                            

:-pce_begin_class(
		  causal_model_editor,
		  framedWindow,
		  "Implements the causal model editor"
		 ).

variable(sketch,sketch,get, "the edited model sketch").

variable(moveSelect,gesture,get,"The saved click=select / move gesture").
variable(moveSelectSingle,gesture,get,"The saved click=select / move-single gesture"). %gp3 0.3

variable(toggleSelect,click_gesture,get,"The saved shift-click = toggle select gesture").

variable(toggleSelect_alternative, click_gesture, get, "Alternative for shift_click: middle_click").

variable(contextMenu, contextPopup,get,"The context menu").

variable(mustUpdateVisualisation, bool, get). %gp3: @on when it is needed to update visualisation in de next changeTreeApplied call (see mustUpdateVisualisation) 
%%
initialise(VE,
	   Sketch: sketch = sketch
	   ) :->
	"Create an sketch editor and open it" ::
	VE->>slot(sketch,Sketch), %moet eerder ivm label
	VE ->+ initialise(
			  VE?makeWindowLabel
			  ,buttonbar := vertical
			  , helpId := 'CausalModelEditor'
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
	SavedSize = Sketch<<-layOutInfoDefault(Sketch,editSize,
								DefSize),

	%bepaal de grootte: er kan opgeslagen zijn dat het window kleiner moet
	%groter mag echter niet

	Width *= number(MaxSize?width),
	Width->>minimum(SavedSize?width),
	Height *= number(MaxSize?height),
	Height->>minimum(SavedSize?height),
	VE->>size(size(Width,Height)),
	% create some space as margin at the top and left
	% to do: scroll to the origin as the user left it
	VE?client->>scroll_to(point(-20,-20)),

	get(@model, '_value', Model),
	send(VE, associateModel, Model).

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
	
	VE?sketch->>clearLayOutInfo, %gp3 0.3: clear old information
 	
	All = VE<<-all_named_hypered(element),
	All->>for_some(->>(@arg1,
			  saveLayoutInfo)), %can fail
	%en zelf slaan we ook het een en ander op
	%dit doen we gewoon in het sketch zelf, met het sketch zelf als verwijzing
	VE?sketch->>layOutInfo(VE?sketch,
						editSize,
						VE?size).
%%

				     
%
init_menu(VE
	  ) :->

	send_list(VE?menubar, append, %zie framedWindow
		  [
		   new(File,popup('File')),
		   new(Edit,popup('Edit')),
		   %I.p.v. conditie en consequentie menu's maar een: Elements (gewone popup)
		   %wel zitten er command objecten
		   %in. Mooier zou zijn om die popups ok te vullen
		   %maar dat is van later zorg (kan gewoon met
		   %menuPopupCommand)
		   new(Elements, popup('Elements')),
		   new(Import,popup('Import'))
		  ]),
		send_list(File,append,
		    [
		        menuCommand(saveSketch, 'SaveSketch','Save sketch in model'),
		        menuCommand(loadSketch, 'LoadSketch','Open saved sketch'),
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

	Elements->>colour(black),
	send_list(Elements,append,
		  [
		   menuCommand(addQuantity,
			       'AddQuantity','Add Quantity'),
                   % Generic types of dependency - turned off for now
		   % menuCommand(addDependency,
			%       'AddDependency','Add Dependency'),
		   % menuCommand(addPosDependency,
		   %       'AddPosDependency','Add Positive Dependency'),
		   % menuCommand(addNegDependency,
		   %       'AddNegDependency','Add Negative Dependency'),
		   menuCommand(addPosProp,
		                'AddPosProp','Add Positive Proportionality'),
		   menuCommand(addNegProp,
			       'AddNegProp','Add Negative Proportionality'),
		   menuCommand(addPosInfluence,
			       'AddPosInfluence','Add Positive Influence'),
		   menuCommand(addNegInfluence,
			       'AddNegInfluence','Add Negative Influence',
			       end_group := @on)
		  ]
		  ),

	Import->>colour(black),
	send_list(Import,append,
		  [
		   menuCommand(importConcepts,
			       'ImportConcepts','Import concepts from Concept map'), 
		   menuCommand(importQuantitiesFromProcesses,
			       'ImportQuantitiesFromProcesses','Import quantities from Process definitions'),
		   menuCommand(importQuantitiesFromAgents,
			       'ImportQuantitiesFromAgents','Import quantities from Agent and external influence definitions',
		   % menuCommand(importQuantitiesFromScenarios,
		   %	       'ImportQuantitiesFromScenarios','Import quantities from Scenario definitions',
			       end_group := @on)
		  ]
		  ).
%%

%
init_buttonbar(VE):->
	% create the elements for the buttonbar
	% version for causal model editor
	B = VE<<-buttonBar,
        % for causal model editor

        % To Do - buttons should have textual names
        % new(ButtonAddQuantity, button('Quantity', message(VE, addQuantity))), 
        % new(ButtonAddInfluence, button('Influence', message(VE, addInfluence))), 
	% send(B, append, ButtonAddQuantity, below), 
	% send(B, append, ButtonAddInfluence, below),
        % send(B, size, size(105, 200)),

	B<<-add(addQuantity,'AddQuantity',sketch_buttonbars,new_quantity,new_quantity_g,'Add quantity'),
        % more general dependencies are currently not used
 	% B<<-add(addDependency,'AddDependency',sketch_buttonbars,addDependencyTxt,addDependencyTxtg,'Add dependency',below, addQuantity),
	% B<<-add(addPosDependency,'AddPosDependency',sketch_buttonbars,addPosDependencyTxt,addPosDependencyTxtg,'Add positive dependency',below, addQuantity),
	% B<<-add(addNegDependency,'AddNegDependency',sketch_buttonbars,addNegDependencyTxt,addNegDependencyTxtg,'Add negative dependency',below, addPosDependency),

 	B<<-add(addPosProp,'AddPosProp',sketch_buttonbars,new_p_plus,new_p_plus_g,'Add positive proportionality',below, addQuantity),

	B<<-add(addNegProp,'AddNegProp',sketch_buttonbars,new_p_min,new_p_min_g,'Add negative proportionality',below, addPosProp),

	B<<-add(addPosInfluence,'AddPosInfluence',sketch_buttonbars,new_i_plus,new_i_plus_g,'Add positive influence',below, addNegProp),

	B<<-add(addNegInfluence,'AddNegInfluence',sketch_buttonbars,new_i_min,new_i_min_g,'Add negative influence',below, addPosInfluence),
	B<<-add(delete,'Delete',sketch_buttonbars,delete,delete_g,'Delete',below, addNegInfluence).



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
	%in Moved zitten nu alle verplaatste objecten
	%eens kijken of er relaties en calculi zijn die maar beter mee kunnen
	
        % I think this is not relevant for Sketch? - AB, 5 sept 2006
	%gp3 1.0: there may be relations between calculi etc. So we keep doing this until 
	%no new element moves
	RelCalc = VE?elements<<-find_all(
		->>(@arg1,instance_of,sketchGenericRelationVisualElement)),
		
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
	E->>instance_of(sketchVisualElement), %alleen visual elements
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
	
	VE->>command('LoadSketch'), %load sketch
	VE->>command('SaveSketch'), %save sketch
	VE->>command('Delete',key := 'DEL', keystring := '[DEL]', otherkeys := chain(backspace)), %verwijder de selectie
	VE->>command('Properties',key := 'RET', keystring := '[ENTER]'),
	VE->>command('SaveEPS'), %gp3 0.3
	VE->>command('SaveModel', key := '\\C-s', keystring := '[Ctrl + S]'),
	VE->>command('AddQuantity', key := '\\C-n', keystring := '[Ctrl + N]'), 
	% VE->>command('AddInfluence',key := '\\C-r', keystring := '[Ctrl + R]'),

	% VE->>command('AddDependency',key := '\\C-d', keystring := '[Ctrl + D]'),
	% VE->>command('AddPosDependency',key := '\\C-p', keystring := '[Ctrl + P]'),
	% VE->>command('AddNegDependency',key := '\\C-n', keystring := '[Ctrl + N]'),
	VE->>command('AddPosInfluence'),
	VE->>command('AddNegInfluence'),
	VE->>command('AddPosProp'),
	VE->>command('AddNegProp'),
	VE->>command('ImportConcepts',key := '\\C-i', keystring := '[Ctrl + I]'),
	VE->>command('ImportQuantitiesFromProcesses'),
	VE->>command('ImportQuantitiesFromAgents'),
	% VE->>command('ImportQuantitiesFromScenarios'),

	%commands voor het afhandelen van de menu-balk, deze runt dus niet
	%maar de update command is handig voor het vullen van het menu enzo


	VE->>command('ContextPopup',runnable := @off).
%
%%	

/***ALGEMENE COMMANDO'S**********/

%%gerelateerd aan het ContextPopup commando, voor het contextmenu
%geen check
%geen run want runnable := @off (zie boven)

updateFillPopupContextPopup(_VE,Popup) :->
%het vullen vh menu, eerst de algemene dingen, dan afhankelijk van het geselecteerde object(??)	
	
        send(Popup,clear),
	Popup->>append(menuCommand(properties,
			       'Properties','Properties...',end_group := @on)), %always first
			       	
	send_list(Popup,append,
		  [
		   menuCommand(delete,
			       'Delete', 'Delete')
		  ]).	

infoDefaultItemContextPopup(_EE,
			     DefaultItem: 'any*'):<-
	%We moeten de value van het default item teruggeven voor de contextPopup

	DefaultItem = properties. %altijd

/***********************************/
%%saveModel
%%
checkSaveModel(_VE):->
	% Always save the model
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
		Pict = E<<-client, %gp3
		new(File, file(FileName)),
		send(File, open, write),
		send(File, append, Pict?postscript),
		send(File, close),
		send(File, done),
		send(E, statusText, string('Saved PostScript in %s', FileName)) %gp3 0.3 changed this from report
	).
%%




/****** IMPORT Functionality *********/
%

%
onImportConcepts(VE):->
	% Open a dialog with options for ImportConcepts
        send(VE, importQuantities, 'ConceptMap').
%%

%
onImportQuantitiesFromProcesses(VE):->
	% Open a dialog with options for ImportQuantities
        send(VE, importQuantities, 'Process').
%%

%
onImportQuantitiesFromAgents(VE):->
	% Open a dialog with options for ImportQuantities
        send(VE, importQuantities, 'Agent').
%%

% currently not in use
%
onImportQuantitiesFromScenarios(VE):->
	% Open a dialog with options for ImportQuantities
        send(VE, importQuantities, 'Scenario').
%%


%
importQuantities(VE, SourceSketchType) :->
	"Display window for importing concepts / quantities from ConceptMap / Processes/Agents/Scenarios"::
		   
        swritef(HelpID, 'Sketch_ImportQuantitiesFrom%sToCausalModel',[SourceSketchType]), 
	new(Dialog, assistanceDialog('Import',HelpID)),
	send(Dialog, transient_for, VE),
	send(Dialog, border, size(20,20)),

	MaxX *= number(0), %houden we de maximale X coordinaat in bij zodat we rechts kunnen uitlijnen
	% GapX = Dialog<<-gapX,
	GapY = Dialog<<-gapY,
		   
	% create a listbrowser for choosing which quantities should be imported from the Source sketch
		      
	DefList *= extendedListBrowser(width := 55),
	DefList->>name(defList),
	% DefList->>show_label(@off),
	DefList->>multiple_selection(@on),
        if (SourceSketchType = 'ConceptMap')
	then
		   swritef(Label, 'Import the following concepts from the Concept map:',[])
	else
		   swritef(Label, 'Import the following quantities from the %w definitions:',[SourceSketchType]), 
	DefList->>label(Label),
	send(Dialog, append, DefList, below),
	% Dialog->>display(DefList, point(100, 200)),
	% Dialog->>display(DefList, point(10*GapX, Dialog?topY+200)),
	MaxX->>maximum(DefList?right_side),

	% Buttons for Select all and none
	%
        new(AllButton, imgButton(all, 
			message(DefList, selection, DefList?members),
			img:=select_all,
			tt:= 'Select all'
			)
	),
        new(NoneButton, imgButton(none, 
			message(DefList, selection, @nil),
			img := select_none,
			tt := 'Select none'
			)
	),
	send(Dialog, append, new(BTS, dialog_group(buttons, group)), right), 
	send(BTS, gap, size(0, 0)),		
	send(BTS, append, new(T1, text('Select')), below),
	send(T1, font, bold), 
	send(BTS, append, AllButton, below),
	send(BTS, append, NoneButton, below),	
		   		      
	ListBottom *= number(DefList?bottom_side),
	ListBottom->>maximum(DefList?top_side + DefList?image?top_side + %=bovenkant vd lijst
					% EditDef?height +
					GapY), %hieruit volgt de onderkant van de lijst (minimaal ruimte van label + buttons)
	DefList->>bottom_side(ListBottom),
	VE->>fillDefListForSourceSketch(Dialog, SourceSketchType),

	% create a menu for choosing what the concepts should become in the new context

	new(M1, menu('to the following type in the Causal model:', choice)),
	% default message when a menu item is chosen - no real message necessary
	send(M1, message, and(	 
	 			 message(@prolog, true)
	 		  )		
	),
	send(M1, alignment, left), 
	send(M1, columns, 1),
	send(M1, layout, vertical),
	send(M1, gap, size(0,10)), % difference between gap & border?
	send(M1, border, 0),       % difference between gap & border?
	send(M1, value_width, 140), 
	send(M1, format, left), 
	send_list(M1, append,
		% first column
		[ 
		 menu_item('quantity', @default, 'Quantities')
		]),
	% select this one - in fact, there is no other option
	send(M1, selection, quantity),

	send(Dialog, append, M1, below), 

	Ok *= imgButton(ok_import_quantities, ->>(VE, ok_import_quantities, Dialog, SourceSketchType), img:=sketch_save, tt:='Apply'),
	Dialog->>append(Ok,below),
	% Dialog->>display(Ok,point(GapX, (M1?bottom_side + GapY)) ),
	Ok->>default_button(@on),
	%Ok->>keyboard_focus(@on),

	Close *= imgButton(close,->>(Dialog,destroy),tt:='Close this window'), 
	Dialog->>append(Close, right), 
	% Dialog->>display(Close,point((Dialog?right_side - Close?width),Ok?top_side)),
	send(Dialog, open).
%%



%
ok_import_quantities(VE, D, SourceSketchType):->
	"Ok pressed: import as quantities" ::
        % for all selected elements from the Concept map, copy them to quantities in the causal model
        % OR for all selected quantities from the SourceSketch map, copy them to the causal model
	get(D, member, defList, DefList), 
	get(DefList, selection, Sel), 
	send(Sel, for_all, message(@prolog, pl_import_quantity, VE, DefList, @arg1, SourceSketchType)),
	D->>return.
%%

%
pl_import_quantity(VE, List, Item, SourceSketchType):-
	get(Item?name, copy, ItemName), 
	pl_create_unique_quantity_name(VE, ItemName, 1, Name), 
	get(Item?remarks, copy, Remarks), 	  
	get(Name, value, NameV), 
        % or should I use the copyBuffer, or an importBuffer?
        send(VE, importQuantity(Name, Remarks)),
	send(VE, show_import_result(List, Item, NameV, SourceSketchType)).
%%



%
pl_create_unique_quantity_name(VE, Name, _Number, Name):-
	% writef('create a unique name based on %w\n',[Name]), 
	not(pl_existing_quantity_name(VE?sketch, Name)),!.
%
pl_create_unique_quantity_name(VE, Name, Number, NewName):-
	if 
             Number = 1
	then 
	     NewName *= string('%s (imported)', Name)
        else
	     NewName *= string('%s (imported %s)', Name, Number),
	not(pl_existing_quantity_name(VE?sketch, NewName)),!.
%
pl_create_unique_quantity_name(VE, Name, Number, NewName):-
	%copy Number exists, raise the number
	NewNumber is Number + 1,
	pl_create_unique_quantity_name(VE, Name, NewNumber, NewName).
%%



%
pl_existing_quantity_name(SK, NewName):-
        % check for existing sketchQuantities with equal name
	?(SK,findElements,sketchQuantityElement)->>find(->>(@arg1?name,
			    equalExportName,
			    NewName)).
%%


%%
importQuantity(VE, ConceptName, Remarks):->
        % This E should not really be necessary, I think.	     
	E = @model<<-hypered(topSketchQuantity),
	@model->>changeRequest(newSketchQuantity,
		VE?sketch,
		VE,
		% D?sketchQuantityElement,
		E,
		ConceptName,
		Remarks).
%%


%
show_import_result(_VE, List, Item, NewName, SourceSketchType):->
	"Show the result of what has been imported to what"::
	get(List?members, find, ->>(@arg1, equal, Item), El), 

        % string shows the item's name and, between brackets, its origin 
        if (SourceSketchType = 'ConceptMap')
	then
                   % string shows the item's name
	           CreateLabelLeftSide *= create(string, '%s', Item?name)
	else
                   % string shows the item's name and, between brackets, its origin 
	           CreateLabelLeftSide *= create(string, '%s (%s)', Item?name, Item?def?name), 
	get(CreateLabelLeftSide, value, OldName),
 	swritef(ReportTxt, '%w --> %w',[OldName, NewName]),	
        % VE->>msgBox(string(ReportTxt)),
	send(El, label, ReportTxt).
%%


%
fillDefListForSourceSketch(VE, D, SourceSketchType):->
	"(re)fill the definitions list with quantities from SourceSketch" ::

	List = D<<-defList_member,
	List->>clear,
        if (SourceSketchType = 'ConceptMap')
	then
		   Defs = @model<<-sortedSketchConcepts
	else
		   Defs = @model<<-sortedSketchDefinitionQuantities(SourceSketchType),
	Defs->>for_all(->>(VE, createAndAddItem, List, @arg1, SourceSketchType)),
	Defs->>done.
%%	

%
createAndAddItem(_VE, List, Def, SourceSketchType):->
        if (SourceSketchType = 'ConceptMap')
	then
                   % string shows the item's name
	           CreateLabel *= create(string, '%s', Def?name)
	else
                   % string shows the item's name and, between brackets, its origin 
	           CreateLabel *= create(string, '%s (%s)', Def?name, Def?def?name), 
	CreateItem *= create(dict_item,
						Def,
						CreateLabel,
						Def),
	send(List, append, CreateItem), 
	CreateItem->>done.
%%



%%
% menuCommand for loadSketch
checkLoadSketch(_VE):->
	%only possible in non-legacy mode with a model that contains saved sketches
	%\+ legacy = @app<<-modelState,
	\+ get(@model, modelState, legacy),
	\+ @model?savedSketches->>empty.
%
onLoadSketch(VE):->
	%pass thru to app
	@app->>loadSketch(VE, causalModelMF).
%%

% menuCommand for saveSketch
checkSaveSketch(_VE):->
	true.
%
%
onSaveSketch(VE):->
	%pass thru to app
	@app->>saveSketch(VE, causalModelMF).
%%


/*****LOSSE OBJECTEN ED *********/


% AddQuantity % AB, feb 2006
% based on givenQuantity
%
checkAddQuantity(_VE):->
	% always possible
	true.

%
onAddQuantity(VE):->
	%Add a sketchQuantity in the Causal Model Editor
	%Open the sketchQuantity properties dialog
	new(sketchQuantityPropsDlg(VE))->>newObject(sketchQuantity).




% AddDependency % Ab, june 2006
% based on givenConfiguration
checkAddDependency(VE):->
	%possible in all types, when two instanceElements are selected

	checkEditorState_CM(VE,any,any,any,
			 [[sketchQuantityVisualElement,sketchQuantityVisualElement]/any]).




% AddPosDependency % Ab, june 2006
% based on givenConfiguration
checkAddPosDependency(VE):->
	%possible in all types, when two instanceElements are selected

	checkEditorState_CM(VE,any,any,any,
			 [[sketchQuantityVisualElement,sketchQuantityVisualElement]/any]).


% AddNegDependency % Ab, june 2006
% based on givenConfiguration
checkAddNegDependency(VE):->
	%possible in all types, when two instanceElements are selected

	checkEditorState_CM(VE,any,any,any,
			 [[sketchQuantityVisualElement,sketchQuantityVisualElement]/any]).


%
onAddPosDependency(VE):->
	% add PosDependency
	Sel = VE<<-selection,
	First= Sel<<-head,
	Sec = Sel<<-tail,
	new(sketchDependencyPropsDlg(VE))->>newObject(First?sketchElement,
									First?route,
									Sec?sketchElement,
									Sec?route, dep_plus).
%%


%
onAddNegDependency(VE):->
	% add NegDependency
	Sel = VE<<-selection,
	First= Sel<<-head,
	Sec = Sel<<-tail,
	new(sketchDependencyPropsDlg(VE))->>newObject(First?sketchElement,
									First?route,
									Sec?sketchElement,
									Sec?route, dep_min).
%%


% AddPosInfluence % Ab, feb 2006
% based on givenConfiguration
checkAddPosInfluence(VE):->
	%possible in all types, when two instanceElements are selected

	checkEditorState_CM(VE,any,any,any,
			 [[sketchQuantityVisualElement,sketchQuantityVisualElement]/any]).


%
onAddPosInfluence(VE):->
	% add Pos Influence
	Sel = VE<<-selection,
	First= Sel<<-head,
	Sec = Sel<<-tail,
	new(sketchInfDependencyPropsDlg(VE))->>newObject(First?sketchElement,
									First?route,
									Sec?sketchElement,
									Sec?route, inf_plus).
%%


% AddNegInfluence % Ab, feb 2006
% based on givenConfiguration
checkAddNegInfluence(VE):->
	%possible in all types, when two instanceElements are selected

	checkEditorState_CM(VE,any,any,any,
			 [[sketchQuantityVisualElement,sketchQuantityVisualElement]/any]).
%%

%
onAddNegInfluence(VE):->
	% add Neg Influence
	Sel = VE<<-selection,
	First= Sel<<-head,
	Sec = Sel<<-tail,
	new(sketchInfDependencyPropsDlg(VE))->>newObject(First?sketchElement,
									First?route,
									Sec?sketchElement,
									Sec?route, inf_min).
%%

% AddPosProp % Ab, feb 2006
% based on givenConfiguration
checkAddPosProp(VE):->
	%possible in all types, when two instanceElements are selected

	checkEditorState_CM(VE,any,any,any,
			 [[sketchQuantityVisualElement,sketchQuantityVisualElement]/any]).
%%

%
onAddPosProp(VE):->
	% add Pos Proportionality
	Sel = VE<<-selection,
	First= Sel<<-head,
	Sec = Sel<<-tail,
	new(sketchPropDependencyPropsDlg(VE))->>newObject(First?sketchElement,
									First?route,
									Sec?sketchElement,
									Sec?route, prop_plus).
%%

% AddNegProp % Ab, feb 2006
% based on givenConfiguration
checkAddNegProp(VE):->
	%possible in all types, when two instanceElements are selected

	checkEditorState_CM(VE,any,any,any,
			 [[sketchQuantityVisualElement,sketchQuantityVisualElement]/any]).



%
onAddNegProp(VE):->
	% add Neg Proportionality
	Sel = VE<<-selection,
	First= Sel<<-head,
	Sec = Sel<<-tail,
	new(sketchPropDependencyPropsDlg(VE))->>newObject(First?sketchElement,
									First?route,
									Sec?sketchElement,
									Sec?route, prop_min).
%%


%%given entity instance
checkGivenQuantity(VE):->
	%mag alleen in proces en agent sketchen en in inputsystems
	checkEditorState_CM(VE,
			 [processSketch,agentSketch,inputSystem],
			 any,none,any).
%
onGivenQuantity(VE):->
	%toevoegen van een Quantity als consequence
	new(sketchQuantityPropsDlg(VE))->>newObject(sketchQuantity).

%%
								

%%

%%givenInfluence
checkGivenInfluence(VE):->
	%mag in alle typen

	checkEditorState_CM(VE,any,any,any,
			 [[instanceElement,instanceElement]/any]).
%
onGivenInfluence(VE):->
	%toevoegen van gegeven configuratie
	Sel = VE<<-selection,
	First= Sel<<-head,
	Sec = Sel<<-tail,
	new(sketchDependencyPropsDlg(VE))->>newObject(First?sketchElement,
									First?route,
									Sec?sketchElement,
									Sec?route).

%%

	
%%
/*******Voor meerdere objecten*************/

%%Delete
checkDelete(VE):->
	checkEditorState_CM(VE,
			 any, %welke sketchen mogen
			 any, %welke sketchen mogen niet
			 1, %aantal geselecteerde objecten
			 [
				[sketchDependencyVisualElement/[condition,consequence]],
				[sketchQuantityVisualElement/[condition,consequence]]
			] %combinaties
			).
%
onDelete(VE):->
	%Regel het verwijderen

	E = VE<<-singleSelection,
	Type = E<<-name,
	FE = E<<-sketchElement,
	pl_onDelete_CM(VE,FE,Type).
%
pl_onDelete_CM(VE,FE,sketchQuantityVisualElement):-
	%we sturen de betreffende CR
	@model->>changeRequest(deleteSketchQuantity,
			       VE?sketch,
			       VE,
			       FE).
%
pl_onDelete_CM(VE,FE,sketchDependencyVisualElement):-
	@model->>changeRequest(deleteSketchDependency,
			       VE?sketch,
			       VE,
			       FE).
%%

%%properties
checkProperties(VE):->
	%mag bij elk niet-fixed object, alleen mag er maar 1 geselecteerd zijn

	checkEditorState_CM(VE,any,any,1,[[any/[condition,consequence,imported,parent]]]),
	\+ valueMarker = VE?singleSelection<<-name.
%
onProperties(VE):->
	%dit hangt natuurlijk erg van het geselecteerde object af
	%we delegeren dit naar een prolog helper, die de rest doet
	%we doen alleen wat voorbereiding

	Element = VE<<-singleSelection,
	ReadOnly = @off,
	Type = Element<<-name,
	FE = Element<<-sketchElement,
	Sketch = VE<<-sketch,
	pl_onProperties_CM(VE,Type,FE,Sketch,Element,ReadOnly).

%voor elk type een clause...
pl_onProperties_CM(VE,sketchQuantityVisualElement,FE,_Sketch,_Element,ReadOnly):-
	new(sketchQuantityPropsDlg(VE))->>editObject(FE, ReadOnly).


%
pl_onProperties_CM(VE,sketchDependencyVisualElement,FE,_Sketch,_Element, ReadOnly):-
	prop = FE<<-type,!,
	new(sketchPropDependencyPropsDlg(VE))->>editObject(FE, ReadOnly).


%
pl_onProperties_CM(VE,sketchDependencyVisualElement,FE,_Sketch,_Element, ReadOnly):-
	inf = FE<<-type,!,
	new(sketchInfDependencyPropsDlg(VE))->>editObject(FE, ReadOnly).

/*
% this one was meant for undefined dependencies (currently not used)
%
pl_onProperties_CM(VE,sketchDependencyVisualElement,FE,_Sketch,_Element, ReadOnly):-
	new(sketchDependencyPropsDlg(VE))->>editObject(FE, ReadOnly).
*/


/******************* VIEW ***************************/

%%
%%Full Redraw
%%gp3 1.0: Full redraw all element: remove all saved layout info and redraw. All hidden stuff is show, default positions are used
onFullRedraw(VE):->
	Sketch = VE<<-sketch,
	VE->>destroy, %we are going to restart this window
	Sketch->>clearLayOutInfo,
	@app->>openViewEditor(Sketch).
%%




/*******************CHANGE REQUESTORS****************/


%%
changeApplied_newSketchQuantity(VE,
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
changeApplied_newSketchQuantityElement(VE,
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
changeApplied_changeSketchQuantity(VE,
			      CR: changeRequestor
			     ):->
	%gp3 0.2: changed to only update the relevant elements
	VE->>updateElement(CR?arg1).
%%

%%
changeApplied_deleteSketchQuantity(VE,
			      _CR:changeRequestor
			     ):->
	VE->>mustUpdateVisualisation. %gp3, changed this to update when all done
%%




%%
changeApplied_newSketchDependency(VE,
	CR: changeRequestor):->

	VE->>update_visualisation,
	CE = VE<<-findFirstElement(CR?result,sketchDependencyVisualElement),
	VE->>checkHideNewElement(CE,CR),
	%gp3: make sure the element is visible
	if
		CR->>checkEditor(VE)
	then
		VE?client->>normalise(CE).
%%

%%
changeApplied_changeSketchDependency(VE,
	_CR: changeRequestor):->

	VE->>mustUpdateVisualisation.
%%

%%
changeApplied_deleteSketchDependency(VE,
	_CR: changeRequestor):->
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
%changeTreeApplied(VE,
%	_CR: changeRequestor):->
	
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
	"Update the current sketch view" ::
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
	
	VE->>draw_causal_model(VE?sketch,new(chain), @nil, normal, UpdatedElements),
	%alle elementen die zich niet in UpdatedElements bevinden kunnen nu weg
	VE?client?graphicals->>for_all(if(
			and(
				->>(@arg1,instance_of,sketchVisualElement),
				not(->>(UpdatedElements,member,@arg1))
				),
				->>(@arg1,destroy)
			)).						
%%


%%
draw_causal_model(VE,
	Sketch: sketch,
	Route: chain, %de route waarmee een getekend element terug gevonden kan worden. Bestaat uit importedSketch objecten
					%lege route betekent dat het element zich in het huidige sketch bevindt
	ContainedSketchElement: 'importedMFElement*', %waaraan dus de instanties zich moeten koppelen
	State: {normal,imported,parent},
	% ShowSubMFSymbols: bool,
	UpdatedElements: chain,
	RM: [chain]
	):->
	%helper bij update_visualisation
	%herteken een sketch, dit kan het bewerkte sketch zelf zijn of
	%een sketch dat daarin hoort (parent of imported)
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
	
	%SketchQuantities (or if fact, SketchQuantityElements)
%@pce->>write_ln(Sketch?name,': QuantityElements'),
	%get(Sketch,findElements,sketchQuantityElement, Quantities),
        %visualize:write_chain(Quantities),
        %writef('\n',[]),
	% ?(Sketch,findElements,garpQuantity)->>for_all(
	?(Sketch,findElements,sketchQuantityElement)->>for_all(
				->>(UpdatedElements,append,
						?(VE,
					      displayQuantity,
					      @arg1,
							Route,
							ContainedSketchElement,
							State))),						

%@pce->>write_ln(Sketch?name,': Sketch Dependencies'),	
	%SketchDependencies (or if fact, SketchDependencyElements)
	SketchDependencies = Sketch<<-findElements(sketchDependencyElement), 
	SketchDependencies->>for_all(
				->>(UpdatedElements,append,
						?(VE,
						displaySketchDependencyElement,
						@arg1,
						Route,
						ContainedSketchElement,
						State, UpdatedElements,RouteMapping))).	
%%



%
routeMapping(VE,RouteMap: chain, RoutePart1: chain, RoutePart2: [chain], NewFullRoute: chain):<-
	%gp3 0.3
	%
	%look in the routemapping chain for mappings of old routes to new (refined) routes, and return the newFullRoute part
	%
	
	%for reasoning about refiners, RoutePart1 and RoutePart2 are merged
	%this is just for convenience in calling
	%the returned route is always a full one. This is needed because
	%mapping of routes may change even the first part (when our sketch
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
		NewFullRoute = FullRoute<<-copy,
unless
	FullRoute->>equal(NewFullRoute)
do
(
	@pce->>write('Routemapping:\noud='),
	FullRoute->>for_all(->>(@pce,write,' ',@arg1,'=',@arg1?name)),
	@pce->>write('\nnieuw='),
	NewFullRoute->>for_all(->>(@pce,write,' ',@arg1,'=',@arg1?name)),
	@pce->>write_ln
).
%%

%%
displayQuantity(VE,
		   I : sketchQuantityElement,
			Route: chain,
			IMF: 'importedMFElement*',
		   SketchState: '[{normal,parent,imported}]',
		   IE: sketchQuantityVisualElement % or shouldn't this be changed? AB, feb 2006
%		   IE: instanceElement
		  ):<-
	"Display this sketchQuantityElement" ::
	%we maken het instance element en melden hoe ie afgebeeld moet worden
	%ook sturen we ons sketch mee, zodat eventueel opgeslagen lay-out info
	%opgehaald wordt. Het element wordt dan meteen afgebeeld.
	%wanneer er een importedMFElement is meegestuurd wordt de instance daar een sub van

	%sub van geïmporteerd fragment?
	default(IMF,@nil,RC),

	(
		(
			IE = VE<<-findElement(I,sketchQuantityVisualElement,Route), % or should this not be changed? Ab, feb 2006
			% IE = VE<<-findElement(I,instanceElement,Route),
			ignore(IE->>updateDisplay)
		)
	;
		(
			%gp3 0.4.11: we do not send a point for placement, but only the IMF
			%when this is @nil , placement will be according to visibleSub strategy
			%otherwise we use 'spot' with a default startingpoint
			%gp3 1.0: super element now standard argument in visualElement
			IE *= sketchQuantityVisualElement(VE?sketch, % or should this not be changed? Ab, feb 2006
						I,
						VE?client,
						RC,
						SketchState,Route), 
			VE->>registerElement(IE)
		)
	),

	if
		RC \== @nil
	then
		RC->>connectSub(IE).
%%



%%
displaySketchDependencyElement(VE,
	C: sketchDependencyElement,
	Route: chain,
	_IMF: '[importedMFElement*]',          % this is probably irrelevant, AB, march 2006
	FS: '[{normal,parent,imported}]',      % this is probably irrelevant, AB, march 2006
	UpdatedElements: chain,
	RouteMap: chain,
	CE: sketchGenericRelationVisualElement):<-
	"Display this relation" ::

	%gp3 0.3: map routes
	R1 = VE<<-routeMapping(RouteMap,Route,C?argument1Route), %gives full route
	R2 = VE<<-routeMapping(RouteMap,Route,C?argument2Route),
	
	Arg1 = VE<<-findElement(C?argument1,sketchQuantityVisualElement,R1,@default,UpdatedElements),
	Arg2 = VE<<-findElement(C?argument2,sketchQuantityVisualElement,R2,@default,UpdatedElements),

	(
		(
			CE = VE<<-findElement(C,sketchDependencyVisualElement,Route),
			ignore(CE->>setArguments(Arg1,Arg2)) %werk argumenten bij, doet ook updateDisplay
		)
	;
		(
			CE *= sketchDependencyVisualElement(VE?sketch,
								C,
								VE?client,
								Arg1,
								Arg2,
								FS,
                                                                Route),
			VE->>registerElement(CE)
		)
	).
%%



%%
registerElement(VE,
	     Element: graphical
	    ):->
	"Init a new element for display (add gestures)" ::
	%Gebruikt bij het aanmaken van elementen in deze klasse, maar ook als callback
	%bij subklassen van sketchVisualElement als er interne subelementen worden aangemaakt
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
	SketchElement: sketchElement):->
%gp3 0.2: send all sketchVisualElements displaying this element the  updateDisplay message
%does not work very well, especially for relations, so we do not use it often

	?(VE,findElements,SketchElement)->>for_all(
		if(
			->>(@arg1,instance_of,sketchVisualElement),
			->>(@arg1,updateDisplay)
		)
	).
%%

%%
removeElement(VE,
	SketchElement: sketchElement):->
%gp3 0.2: delete all sketchVisualElements displaying this element the delete message
%no recursive calling over subs, so we need to call for every sub
%problem is that this is even less efficient than a whole update
%so we use mustUpdateVisualisation for subchanges, and do the update
%on changeTreeAppled
	?(VE,findElements,SketchElement)->>for_all(
		if(
			->>(@arg1,instance_of,sketchVisualElement),
			->>(@arg1,destroy))).
%%




%%
/******************** MULTIPLE MODELS ***********************/ %JL

/* Make the right model active */
input_focus(T, Boolean) :->
	send(@app, selectCorrectTab, T), % T is the window object (of the editor)
	send_super(T, input_focus, Boolean).

		
/********************HELPERS***********************/
%%
findElement(VE,
	    SketchElement : any,
	    ClassName : name,
		Route: route = [chain],
		ImportedRoute: importedRoute = [chain],
		UpdatedElements: updated = [chain],
	    Element : sketchVisualElement):<-
	"Finds the first visual element visualising the sketch element" ::
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
			@arg1?sketchElement == SketchElement,
			->>(@arg1?route,equal,RRoute))).
%%

%%
findElements(VE,
	SketchElement: any,
	ClassName: [name],
	Elements: chain):<-
	%geef chain van alle elementen die op de een of andere wijze een bepaald object visualiseren
	%eventueel alleen op classnaam

	All = VE?elements<<-find_all(@arg1?sketchElement == SketchElement),
	if
		ClassName == @default
	then
		Elements = All
	else
		Elements = All<<-find_all(@arg1?name == ClassName).
%%

%%
findFirstElement(VE,
	SketchElement: any,
	ClassName: [name],
	Element: graphical):<-
	%als findElements, maar geeft maar één element terug (eerstgevonden)
	%en alleen sketchVisualElement subs
	
	Element = VE?client?graphicals<<-find(and(
					->>(@arg1,instance_of,sketchVisualElement),
					@arg1?sketchElement == SketchElement,
					or(
						ClassName == @default,
						@arg1?name == ClassName
						))).
%%
					


%%
elements(VE,
	E: chain):<-
	"Return all sketchVisualElements" ::

	E = VE<<-all_named_hypered(element).
%%

%%
topElements(VE,
	TE: chain):<-
	"Return all sketchVisualElements that have no super" ::

	TE = VE?elements<<-find_all(not(@arg1?super)).
%%


%%
makeWindowLabel(VE,
		Label2 : name
		) :<-
	"Internal: create caption" ::

        catch(get(VE?sketch, savedSketchName, SavedSketchName), _, fail),
        if @nil = SavedSketchName
        then 
        (
           Label *= string('Causal Model Editor - Sketch')
        )
        else
	(
           get(SavedSketchName, value, Str), 
	   swritef(Label, 'Causal Model Editor - Sketch - %w', [Str])
        ),
	get(@model, getModelNameForEditor, Label?value, Label2).

%%


%
updateWindowLabel(VE):->
	"Internal: update caption" ::

        get(VE, makeWindowLabel, Label), 
        send(VE, label, Label).
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
	E: sketchVisualElement,
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
checkEditorState_CM: Een algemene call voor het checken op de editor state.
Prototype:
checkEditorState_CM(VE,AllowedSketchs,DisallowedSketchs,
		      AllowedSelectionTypes, AllowedCombinations)
		      
AllowedSketchs: Een lijst met typen sketchen die mogen, of any (geen check)
                  [processSketch,agentSketch]
DisallowedSketchs: Een lijst met typen sketchen die niet mogen, of any (geen check)
		  [staticSketch]
AllowedSelectionTypes: Een lijst met soorten selectie die mogen (none, single, multi of een aantal)
                                 of any (geen check)
		  [none,multi]
AllowedCombinations: Een lijst met daarin lijsten die aangeven wat mag ([Combination,...]) of any (geen check)
Combination: [{elementtype|any}/{[State,...]|any},....] of [{elementtype|any},...]/{[State,...]|any} 
State: condition|consequence|imported|parent|fixed	

Voor AllowedSketchs, DisallowedSketchs, AllowedSelectionTypes en de Statelijst in AllowedCombinations
geldt dat er geen lijst hoeft te worden gebruikt als het om maar één element gaat.

Voorbeeld
checkEditorState_CM(VE,[processSketch,agentSketch],any,2,
		 [[quantityElement/condition,quantityElement/any],
		  [calculusElement,calculusElement]/[condition,imported]]).
Deze call slaagt wanner:
1 Het bewerkte sketch een processSketch of agentSketch is
2 Er meer 2 elementen geselecteerd zijn
3 Er een conditionele quantity en nog een quantity is geselecteerd OF er twee conditionele of geimporteerde
  calculi zijn geselecteerd

Los te gebruiken zijn ook checkES_CM_Sketchs, _DisallowedSketchs, _Selection
en _Combinations.
*/


checkEditorState_CM(VE, AllowedSketches,
		 DisallowedSketches,
		 AllowedSelectionTypes,
		 AllowedCombinations):-

	checkES_CM_Sketches(VE,AllowedSketches),
	checkES_CM_DisallowedSketches(VE,DisallowedSketches),
	checkES_CM_Selection(VE,AllowedSelectionTypes),
	checkES_CM_Combinations(VE,AllowedCombinations).
%
checkES_CM_Sketches(_VE,any):-!.

checkES_CM_Sketches(VE,Allowed):-
	C = VE?sketch<<-currentType,
	member(C,Allowed).

checkES_CM_Sketches(VE,Allowed):-
	%geen lijst
	Allowed = VE?sketch<<-currentType.
%
checkES_CM_DisallowedSketches(_VE,any):-!.

checkES_CM_DisallowedSketches(VE,Disallowed):-
	C = VE?sketch<<-currentType,
	\+ member(C,Disallowed), %lijst
	\+ Disallowed = C. %geen lijst
%
checkES_CM_Selection(_VE,any):-!.

checkES_CM_Selection(VE,Selection):-
	N = VE?client?selection<<-size,
	checkES_CM_SelectionNum(Selection,N).
%
checkES_CM_SelectionNum(Selection,0):-
	(   member(none,Selection)
	;   Selection = none
	),!.

checkES_CM_SelectionNum(Selection,1):-
	(   member(single,Selection)
	;   Selection = single
	),!.

checkES_CM_SelectionNum(Selection,N):-
	N > 1,
	(   member(multi,Selection)
	;   Selection = multi
	),!.

checkES_CM_SelectionNum(Selection,N):-
	(   member(N,Selection)
	;   Selection = N
	).
%
checkES_CM_Combinations(_VE,any):-!.

checkES_CM_Combinations(VE,Combinations):-
	SelChain = VE?client<<-selection,
	chain_list(SelChain,Selection),
	checkES_CM_SelCombination(Selection,Combinations).
%
checkES_CM_SelCombination(Selection,[First/GeneralState|_Rest]):-
	checkES_CM_makeSpecificState(First,GeneralState,Specific),
	checkES_CM_checkCombination(Selection,Specific),!.

checkES_CM_SelCombination(Selection,[First|_Rest]):-
	checkES_CM_checkCombination(Selection,First),!.

checkES_CM_SelCombination(Selection,[_First|Rest]):-
	checkES_CM_SelCombination(Selection,Rest).
%
checkES_CM_makeSpecificState([],_GeneralState,[]).

checkES_CM_makeSpecificState([First|Rest],GeneralState,[First/GeneralState|SpecificRest]):-
	checkES_CM_makeSpecificState(Rest,GeneralState,SpecificRest).
%
checkES_CM_checkCombination(Selection,Combination):-
	%deze is lastig, er kunnen meerdere mappings op elk element van de combinatie
	%geprobeerd worden, dus dat moeten we eerst uitwerken
	checkES_CM_makeAllCombinations(Combination,AllCombinations),
	checkES_CM_matchCombinations(Selection,AllCombinations).
%
%bij het maken van de combinaties is de situatie met nog maar 1 element te gaan een speciaal geval
%deze moet namelijk de basic combinaties bouwen
%bij-effect: als state geen lijst is, dan wordt het dat hier

checkES_CM_makeAllCombinations([One/[State]], [[One/State]]):-!.

checkES_CM_makeAllCombinations([One/[State|States]],[[One/State]|Others]):-!,
	checkES_CM_makeAllCombinations([One/States],Others).

checkES_CM_makeAllCombinations([One/State], [[One/State]]):-!. %ook /any

%en de situatie voor de overige elementen:
checkES_CM_makeAllCombinations([Element|Rest],Combinations):-
	checkES_CM_makeAllCombinations(Rest,RestCombinations),
	checkES_CM_makeCombinationElement(Element,RestCombinations,Combinations).
%
checkES_CM_makeCombinationElement(_El/[],_RestCombinations,[]):-!.

checkES_CM_makeCombinationElement(El/[State|States],CurrentCombinations,Combinations):-!,
	checkES_CM_makeCombinationElement(El/States,CurrentCombinations,NewCombinations),
	checkES_CM_appendElementState(El,State,CurrentCombinations,NewCombinations,Combinations).

checkES_CM_makeCombinationElement(El/State,CurrentCombinations,Combinations):- %ook any
	checkES_CM_appendElementState(El,State,CurrentCombinations,[],Combinations). 
%
checkES_CM_appendElementState(_El,_State,[], NewCombinations, NewCombinations).

checkES_CM_appendElementState(El,State,[First|RestCombinations],AddedCombinations,Combinations):-
	checkES_CM_appendElementState(El,State,RestCombinations,[[El/State|First]|AddedCombinations],
				   Combinations).
%
checkES_CM_matchCombinations(Selection,[First|_Rest]):-
	checkES_CM_combinationSelected(Selection,First),!.

checkES_CM_matchCombinations(Selection,[_First|Rest]):-
	checkES_CM_matchCombinations(Selection,Rest).
%
checkES_CM_combinationSelected([],[]):-!.

checkES_CM_combinationSelected(Selection,[Element/State|RestElements]):-!,
	select(SelElement,Selection,RestSelection),
	(   Element == any
	;   Element = SelElement<<-name
	),
	(   State == any
	;   State = SelElement<<-state
	),
	%1 gevonden, door met de rest
	checkES_CM_combinationSelected(RestSelection,RestElements).
%%



:-pce_end_class.



