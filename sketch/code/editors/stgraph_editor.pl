/*
Definition of stgraphEditor class
Editor for state transition graph for specifying expected behaviour 

Part of Garp3, see copyright notice
2006 Anders Bouwer 
*/                                                                                                                                                                                                                            

:-pce_begin_class(
		  stgraphEditor,
		  framedWindow,
		  "Implements the state transition graph editor"
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
	"Create a sketch stgraphEditor and open it" ::
	VE->>slot(sketch,Sketch), %moet eerder ivm label
	VE ->+ initialise(
			  VE?makeWindowLabel
			  ,buttonbar := vertical
			  , helpId := 'StateTransitionGraphEditor'
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
		   new(Elements, popup('Elements')),
		   new(Import,popup('Import'))
		  ]),

		send_list(File, append,
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
		   menuCommand(addState,
			       'AddState','Add State'),
		   menuCommand(copyState,
			       'CopyState','Copy State'),
		   menuCommand(addTransition,
			       'AddTransition','Add Transition',
			       end_group := @on)
		  ]
		  ),

	Import->>colour(black),
	send_list(Import,append,
		  [
		   menuCommand(importQuantities,
			       'ImportQuantities','Import quantities from Causal Model'),
		   menuCommand(importSettings,
			       'ImportSettings','Automatic import settings',
			       end_group := @on)
		  ]
		  ).
%%

%
init_buttonbar(VE):->
	% create the elements for the buttonbar
	% version for stgraph editor
	B = VE<<-buttonBar,

        % To Do - buttons should have textual names
	B<<-add(addState,'AddState',sketch_buttonbars,new_state,new_state_g,'Add state'),
	B<<-add(copyState,'CopyState',sketch_buttonbars,copy_state,copy_state_g,'Copy state', below, addState),
	B<<-add(addTransition,'AddTransition',sketch_buttonbars,new_transition,new_transition_g,'Add transition',below, copyState),
	B<<-add(delete,'Delete',sketch_buttonbars,delete,delete_g,'Delete',below, addTransition).
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
	
        % I think this is not relevant for Sketch? - AB, 5 sept 2006
	%gp3 1.0: there may be relations between calculi etc. So we keep doing this until 
	%no new element moves
	RelCalc = VE?elements<<-find_all(
		->>(@arg1,instance_of,sketchDirectRelationVisualElement)),
		
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
	VE->>command('AddState', key := '\\C-n', keystring := '[Ctrl + N]'), 
	VE->>command('CopyState'), 
	VE->>command('AddTransition',key := '\\C-t', keystring := '[Ctrl + T]'),
	VE->>command('ImportQuantities',key := '\\C-i', keystring := '[Ctrl + I]'),
	VE->>command('ImportSettings'),

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
	%gp3 0.3 changed this: we added the new toggleSubsketchs command
	%but only when the selection is a modelsketch
		
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

/****** IMPORT SETTINGS *********/
%

%
onImportSettings(VE):->
	% Open a dialog with options for ImportSettings
        send(VE, settings).


%
onImportQuantities(VE):->
	% Open a dialog with options for ImportConcepts
        send(VE, importQuantities, causal_model, quantity).
%%


%
importQuantities(VE, SourceSketch, SourceType) :->
	"Display window for importing concepts from Concept map, Causal Model, or Structural model"::

        if  (SourceSketch = causal_model)
        then HelpID = 'Sketch_ImportQuantitiesFromCausalModelToSketchDefinition',
        
	new(Dialog, assistanceDialog('Import', HelpID)),
	send(Dialog, transient_for, VE),
	send(Dialog, border, size(20,20)),

	MaxX *= number(0), %houden we de maximale X coordinaat in bij zodat we rechts kunnen uitlijnen
	% GapX = Dialog<<-gapX,
	GapY = Dialog<<-gapY,
		   
	% create a listbrowser for choosing which concepts should be imported from the Concept map
		      
	DefList *= extendedListBrowser(width := 53),
	DefList->>name(defList),
	% DefList->>show_label(@off),
	DefList->>multiple_selection(@on),

        if (SourceSketch = causal_model)
        then 
        (
             swritef(Label, 'Import the following quantities from the Causal model:',[])
	),
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


        if (SourceSketch = causal_model)
        then 	VE->>fillDefListForType(Dialog, sketchQuantity, SourceType),

	% create a menu for choosing what the concepts/objects should become in the new context
        get(VE, create_import_type_menu, SourceType, M1), 

	send(Dialog, append, M1, below), 
	% Dialog->>display(M1, point(Dialog?gapX, (DefList?bottom_side+Dialog?gapY)) ),       

	Ok *= imgButton(import_ok, ->>(VE, import_ok, Dialog), img:=sketch_save, tt:='Apply'),
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
create_import_type_menu(_VE, SourceType, M1):<-
	new(M1, menu('to the following type in the current Behaviour graph:', choice)),
	% default message when a menu item is chosen
        % This is not necessary, since we don't allow any choice here
	% send(M1, message, and(	 
	% 			 message(VE, set_import_type_for_stgraph, @arg1)
	% 		  )		
	% ),
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

        % the first time, set the selection to the default
	send(M1, selection, SourceType).
        % send(VE, set_import_type_for_stgraph, SourceType).
%%


%
import_type(_VE, Type):<-
        Type = quantity.
%%

%
report_import_error(VE):->
        VE?frame->>report(error, 
	       'Warning: Could not import.').
%%

%
import_ok(VE, D):->
	"Ok pressed: save changes" ::
        % for all selected elements from the Causal model, copy them to the right type

        get(VE, import_type, Type),
        get(D, member, defList, DefList), 
	get(DefList, selection, Sel), 
	send(Sel, for_all, message(@prolog, pl_import_for_stgraph, VE, DefList, @arg1, Type)),
	D->>return.
%%

%
report_import_error(_VE):->
        writef('report_import_error called: This should be specialized in subclasses\n',[]).
%%



%
show_import_result(_VE, List, Def, NewName, Type):->
	"Show feedback about the result of what has been imported to what"::
	get(Def, name, Name), 
	% get(NewNameStr, value, NewName), 
	swritef(ReportTxt, '%w --> %w: %w',[Name, Type, NewName]),	
        % VE->>msgBox(string(ReportTxt)),
	get(List?members, find, ->>(@arg1?name, equal, Name), El), 
	send(El, label, ReportTxt).
%%



%
pl_import_for_stgraph(VE, List, Def, Type):-
        debug(sketch(import), 'Import ~w to type ~w',[Def, Type]),
	get(Def?name, copy, DefName), 
	pl_create_unique_name_STG(VE, DefName, 1, Name), 
	get(Def?remarks, copy, Remarks), 	  
        % or should I use the copyBuffer, or an importBuffer?
	get(Name, value, NameV), 
        send(VE, importQuantity(Name, Remarks)),
	send(VE, show_import_result(List, Def, NameV, Type)).
%%


% create a unique name based on Name 
%
pl_create_unique_name_STG(VE, Name, _Number, Name):-
	not(pl_existing_name_STG(VE, Name)),!.
%
pl_create_unique_name_STG(VE, Name, Number, NewName):-
	if 
             Number = 1
	then 
	     NewName *= string('%s (imported)', Name)
        else
	     NewName *= string('%s (imported %s)', Name, Number),
	not(pl_existing_name_STG(VE, NewName)),!.
%
pl_create_unique_name_STG(VE, Name, Number, NewName):-
	%copy Number exists, raise the number
	NewNumber is Number + 1,
	pl_create_unique_name_STG(VE, Name, NewNumber, NewName).
%%



%
pl_existing_name_STG(VE, Name):-
        % check for existing sketchDefinitionElements of same type with equal name
	get(VE?sketch, sortedSketchQuantities, Elements), 
        get(Elements, find, message(@arg1?name, equal, Name), _ElementWithSameName).
%%
		  

%
importQuantity(VE, QuantityName, Remarks):->
        % It would be nicer if changeRequest system was used to check names etc.

        get(VE, sketch, SK), 
        get(SK, sketchQuantities, SketchQuantities), 
        new(SQ, sketchQuantityElement(QuantityName, Remarks, SK)), 
        send(SketchQuantities, append, SQ),
        % sort
	send(SketchQuantities, sort, ?(@arg1?name, compare, @arg2?name)).
%%


%
fillDefListForType(_VE, D, Type, ObjectType):->
	"(re)fill the definitions list with concepts of the concept map" ::

	List = D<<-defList_member,
	List->>clear,
	if (Type == concept)
        then Defs = @model<<-sortedSketchConcepts,
	if (Type == sketchQuantity)
        then 
        (
          get(@model, hypered, causalModelMF, CM), 
	  Defs = CM<<-sortedSketchQuantities
	),
	if (Type == sketchObject)
        then Defs = @model<<-sortedSketchObjectsTyped(ObjectType),
	CreateItem *= create(dict_item,
						@arg1,
						@arg1?name,
						@arg1),
	Defs->>for_all(->>(List,
			    append,
				CreateItem)),
	CreateItem->>done,
	Defs->>done.
%%	






settings(VE) :->
	"Display window with settings to edit"::
	%gp3: we commented out 3 items + the code to fill/set them
	%and added a new setting (legacy quantity names in dependencies)
	%gp3 0.3.13: changed dialog to assistanceDialog for helpbutton. Rewrote lay out code to fixed layout
	new(Dialog, assistanceDialog('Import settings','Sketch_ImportPreferenceSettings')),
	send(Dialog, transient_for, VE),
	% create a menu for toggling different preference settings

	new(M1, menu('Import settings for Behaviour graph - Sketch:', toggle)),

	% default message when a menu item is chosen
	send(M1, message, and(	 
				 message(@prolog, change_import_settings, 
						VE, @arg1, @arg2)/*,
                                 message(Dialog, destroy)*/ %gp3: dont like destroy after changing one option
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
		 menu_item('import_quantities', @default, 'Import new quantities from Causal Model automatically')
		]),

		%gp3 0.2 made these settings app-settings which are automatically saved
        IQ = @app<<-setting(import_quantities_from_causal_model_to_stgraph), 
	send(M1, selected, 'import_quantities', IQ),

	Dialog->>display(M1,point(Dialog?gapX,Dialog?topY)),
	
	Close *= imgButton(close,->>(Dialog,destroy),tt:='Close this window'), 
	Dialog->>display(Close,point(Dialog?gapX + (M1?right_side - Dialog?gapX - Close?width) / 2,M1?bottom_side + Dialog?gapY)),
	send(Dialog, open).



change_import_settings(_VE, 'import_quantities', OnOrOff):-
        % writef('Setting for importing quantities turned %d.\n',[OnOrOff]),
        @app->>setting(import_quantities_from_causal_model_to_stgraph, OnOrOff).



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
	@app->>loadSketch(VE, stgraphMF).
%%

% menuCommand for saveSketch
checkSaveSketch(_VE):->
	true.
%
%
onSaveSketch(VE):->
	%pass thru to app
	@app->>saveSketch(VE, stgraphMF).
%%


/*****LOSSE OBJECTEN ED *********/


% AddState % AB, feb 2006
% based on givenState
%
checkAddState(_VE):->
	% always possible
	true.

%
onAddState(VE):->
	%Add a sketchState in the State Transition Graph Editor
	%Open the sketchState properties dialog
	new(sketchStatePropsDlg(VE))->>newObject(sketchState).


% CopyState % AB, june 2006
% based on givenState
%
checkCopyState(VE):->
        % possible when one state is selected
	checkEditorState_STG(VE,any,any,any,
			 [[sketchStateVisualElement]/any]).

%
onCopyState(VE):->
	%Copy the selected sketchState in the State Transition Graph Editor
	%Open the sketchState properties dialog with the same data as the original

	Sel = VE<<-singleSelection,
        Readonly = @off,
	new(sketchStatePropsDlg(VE))->>copyObject(Sel?sketchElement, Readonly).
%%



%%properties
checkProperties(VE):->
	%mag bij elk niet-fixed object, alleen mag er maar 1 geselecteerd zijn
	% checkEditorState_STG(VE,any,any,1,[[any/[condition,consequence,imported,parent]]]),
	checkEditorState_STG(VE,any,any,1,[[any/any]]),
	\+ valueMarker = VE?singleSelection<<-name.
%%


%
onProperties(VE):->
        % special case for sketch transition connection 
        % redirect to SketchTransitionVisualElement
	Element = VE<<-singleSelection,
	ReadOnly = @off,
	Element->>instance_of(my_connection),!,
        get(Element, hypered, connection, SketchTransitionVisualElement), 
	Type = SketchTransitionVisualElement<<-name,
	FE = SketchTransitionVisualElement<<-sketchElement,
	Sketch = VE<<-sketch,
	pl_onProperties_STG(VE,Type,FE,Sketch,SketchTransitionVisualElement,ReadOnly).
%%

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
	pl_onProperties_STG(VE,Type,FE,Sketch,Element,ReadOnly).
%%

% 
checkAddTransition(VE):->
	%possible in all types, when two sketchStates are selected

	checkEditorState_STG(VE,any,any,any,
			 [[sketchStateVisualElement,sketchStateVisualElement]/any]).
%%

%
onAddTransition(VE):->
	Sel = VE<<-selection,
	First= Sel<<-head,
	Sec = Sel<<-tail,
	new(sketchTransitionPropsDlg(VE))->>newObject(First?sketchElement,
									First?route,
									Sec?sketchElement,
									Sec?route).
%%

	
%%
/*******Voor meerdere objecten*************/

%%Delete
checkDelete(VE):->
        % writef('checkDelete in stgraph_editor.pl \n', []),  

	checkEditorState_STG(VE,any,any,1,[[any/any]]).


%
onDelete(VE):->
	%Regel het verwijderen
        % special case for sketch transition connection 
	Element = VE<<-singleSelection,
	Element->>instance_of(my_connection),!,
        get(Element, hypered, connection, SketchTransitionVisualElement), 
	Type = SketchTransitionVisualElement<<-name,
	FE = SketchTransitionVisualElement<<-sketchElement,
	pl_onDelete_STG(VE,FE,Type).
%%

%
onDelete(VE):->
	%Regel het verwijderen
	E = VE<<-singleSelection,
	Type = E<<-name,
	FE = E<<-sketchElement,
	pl_onDelete_STG(VE,FE,Type).
%
pl_onDelete_STG(VE,FE,sketchStateVisualElement):-
	%we sturen de betreffende CR
	@model->>changeRequest(deleteSketchState,
			       VE?sketch,
			       VE,
			       FE).
%
pl_onDelete_STG(VE,FE,sketchTransitionVisualElement):-
	@model->>changeRequest(deleteSketchTransition,
			       VE?sketch,
			       VE,
			       FE).
%%



%voor elk type een clause...
pl_onProperties_STG(VE,sketchStateVisualElement,FE,_Sketch,_Element,ReadOnly):-
	new(sketchStatePropsDlg(VE))->>editObject(FE, ReadOnly).


%
pl_onProperties_STG(VE,sketchTransitionVisualElement,FE,_Sketch,_Element, ReadOnly):-
	new(sketchTransitionPropsDlg(VE))->>editObject(FE, ReadOnly).


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
changeApplied_newSketchState(VE,
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
changeApplied_changeSketchState(VE,
			      CR: changeRequestor
			     ):->
	%gp3 0.2: changed to only update the relevant elements
	VE->>updateElement(CR?arg1),
	VE->>update_visualisation.
%%


%%
changeApplied_copySketchState(VE,
			      CR: changeRequestor
			     ):->
	%gp3 0.2: changed to only update the relevant elements
	VE->>updateElement(CR?arg1),
	VE->>update_visualisation.
%%

%%
changeApplied_deleteSketchState(VE,
			      _CR:changeRequestor
			     ):->
	VE->>mustUpdateVisualisation. %gp3, changed this to update when all done
%%


%%
changeApplied_newSketchTransition(VE,
	CR: changeRequestor):->

	VE->>update_visualisation,
	CE = VE<<-findFirstElement(CR?result,sketchTransitionVisualElement),
	VE->>checkHideNewElement(CE,CR),
	%gp3: make sure the element is visible
	if
		CR->>checkEditor(VE)
	then
		VE?client->>normalise(CE).
%%

%%
changeApplied_changeSketchTransition(VE,
	_CR: changeRequestor):->

	VE->>mustUpdateVisualisation.
%%

%%
changeApplied_deleteSketchTransition(VE,
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
	%when mustUpdateVisualisation is set (a changeApplied_... call wants to do this)
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
	
	VE->>draw_stgraph(VE?sketch,new(chain), @nil, normal, UpdatedElements),
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
draw_stgraph(VE,
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

	% get(Sketch,elements,Elements),
        % writef('Elements in STG: %d\n',[Elements]),
        % writef('Updated elements in STG: %d\n',[UpdatedElements]),
	
	%SketchStates (or if fact, SketchStateElements)
%@pce->>write_ln(Sketch?name,': StateElements'),
	% get(Sketch,findElements,sketchStateElement, States),
        % visualize:write_chain(States),
        % writef('\n',[]),
	?(Sketch,findElements,sketchStateElement)->>for_all(
				->>(UpdatedElements,append,
						?(VE,
					      displayState,
					      @arg1,
							Route,
							ContainedSketchElement,
							State))),						

%@pce->>write_ln(Sketch?name,': Sketch Transitions'),	
	% SketchTransitions (or if fact, SketchTransitionElements)
	SketchTransitions = Sketch<<-findElements(sketchTransitionElement),
	SketchTransitions->>for_all(
				->>(UpdatedElements,append,
						?(VE,
						displaySketchTransitionElement,
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
displayState(VE,
		   I : sketchStateElement,
			Route: chain,
			IMF: 'importedMFElement*',
		   SketchState: '[{normal,parent,imported}]',
		   IE: sketchStateVisualElement 
		  ):<-
	"Display this sketchStateElement" ::
	%we maken het instance element en melden hoe ie afgebeeld moet worden
	%ook sturen we ons sketch mee, zodat eventueel opgeslagen lay-out info
	%opgehaald wordt. Het element wordt dan meteen afgebeeld.
	%wanneer er een importedMFElement is meegestuurd wordt de instance daar een sub van

	%sub van geïmporteerde sketch?
	default(IMF,@nil,RC),

	(
		(
			IE = VE<<-findElement(I,sketchStateVisualElement,Route),
			ignore(IE->>updateDisplay)
		)
	;
		(
			%gp3 0.4.11: we do not send a point for placement, but only the IMF
			%when this is @nil , placement will be according to visibleSub strategy
			%otherwise we use 'spot' with a default startingpoint
			%gp3 1.0: super element now standard argument in visualElement
			IE *= sketchStateVisualElement(VE?sketch,
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
displaySketchTransitionElement(VE,
	C: sketchTransitionElement,
	Route: chain,
	_IMF: '[importedMFElement*]',          % this is probably irrelevant, AB, march 2006
	FS: '[{normal,parent,imported}]',      % this is probably irrelevant, AB, march 2006
	UpdatedElements: chain,
	RouteMap: chain,
	CE: sketchDirectRelationVisualElement):<-
	"Display this relation" ::

	%gp3 0.3: map routes
	R1 = VE<<-routeMapping(RouteMap,Route,C?argument1Route), %gives full route
	R2 = VE<<-routeMapping(RouteMap,Route,C?argument2Route),
	
	Arg1 = VE<<-findElement(C?argument1,sketchStateVisualElement,R1,@default,UpdatedElements),
	Arg2 = VE<<-findElement(C?argument2,sketchStateVisualElement,R2,@default,UpdatedElements),

	(
		(
			CE = VE<<-findElement(C,sketchTransitionVisualElement,Route),
			ignore(CE->>setArguments(Arg1,Arg2)) %werk argumenten bij, doet ook updateDisplay
		)
	;
		(
			CE *= sketchTransitionVisualElement(VE?sketch,
								C,
								VE?client,
								Arg1,
								Arg2,
								FS,
                                                                Route),
			VE->>registerElement(CE),
			VE->>registerElement(CE?connection)

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
        % writef('removeElement VE: %d, in stgraph_editor.pl \n',[VE]), 
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
           Label *= string('State Transition Graph Editor - Sketch')
        )
        else
	(
           get(SavedSketchName, value, Str), 
	   swritef(Label, 'State Transition Graph Editor - Sketch - %w', [Str])
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
checkEditorState_STG: Een algemene call voor het checken op de editor state.
Prototype:
checkEditorState_STG(VE,AllowedSketchs,DisallowedSketchs,
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
checkEditorState_STG(VE,[processSketch,agentSketch],any,2,
		 [[quantityElement/condition,quantityElement/any],
		  [calculusElement,calculusElement]/[condition,imported]]).
Deze call slaagt wanner:
1 Het bewerkte sketch een processSketch of agentSketch is
2 Er meer 2 elementen geselecteerd zijn
3 Er een conditionele quantity en nog een quantity is geselecteerd OF er twee conditionele of geimporteerde
  calculi zijn geselecteerd

Los te gebruiken zijn ook checkES_STG_Sketchs, _DisallowedSketchs, _Selection
en _Combinations.
*/


checkEditorState_STG(VE, AllowedSketches,
		 DisallowedSketches,
		 AllowedSelectionTypes,
		 AllowedCombinations):-

	checkES_STG_Sketches(VE,AllowedSketches),
	checkES_STG_DisallowedSketches(VE,DisallowedSketches),
	checkES_STG_Selection(VE,AllowedSelectionTypes),
	checkES_STG_Combinations(VE,AllowedCombinations).
%
checkES_STG_Sketches(_VE,any):-!.

checkES_STG_Sketches(VE,Allowed):-
	C = VE?sketch<<-currentType,
	member(C,Allowed).

checkES_STG_Sketches(VE,Allowed):-
	%geen lijst
	Allowed = VE?sketch<<-currentType.
%
checkES_STG_DisallowedSketches(_VE,any):-!.

checkES_STG_DisallowedSketches(VE,Disallowed):-
	C = VE?sketch<<-currentType,
	\+ member(C,Disallowed), %lijst
	\+ Disallowed = C. %geen lijst
%
checkES_STG_Selection(_VE,any):-!.

checkES_STG_Selection(VE,Selection):-
	N = VE?client?selection<<-size,
	checkES_STG_SelectionNum(Selection,N).
%
checkES_STG_SelectionNum(Selection,0):-
	(   member(none,Selection)
	;   Selection = none
	),!.

checkES_STG_SelectionNum(Selection,1):-
	(   member(single,Selection)
	;   Selection = single
	),!.

checkES_STG_SelectionNum(Selection,N):-
	N > 1,
	(   member(multi,Selection)
	;   Selection = multi
	),!.

checkES_STG_SelectionNum(Selection,N):-
	(   member(N,Selection)
	;   Selection = N
	).
%
checkES_STG_Combinations(_VE,any):-!.

checkES_STG_Combinations(VE,Combinations):-
	SelChain = VE?client<<-selection,
	chain_list(SelChain,Selection),
	checkES_STG_SelCombination(Selection,Combinations).
%
checkES_STG_SelCombination(Selection,[First/GeneralState|_Rest]):-
	checkES_STG_makeSpecificState(First,GeneralState,Specific),
	checkES_STG_checkCombination(Selection,Specific),!.

checkES_STG_SelCombination(Selection,[First|_Rest]):-
	checkES_STG_checkCombination(Selection,First),!.

checkES_STG_SelCombination(Selection,[_First|Rest]):-
	checkES_STG_SelCombination(Selection,Rest).
%
checkES_STG_makeSpecificState([],_GeneralState,[]).

checkES_STG_makeSpecificState([First|Rest],GeneralState,[First/GeneralState|SpecificRest]):-
	checkES_STG_makeSpecificState(Rest,GeneralState,SpecificRest).
%
checkES_STG_checkCombination(Selection,Combination):-
	%deze is lastig, er kunnen meerdere mappings op elk element van de combinatie
	%geprobeerd worden, dus dat moeten we eerst uitwerken
	checkES_STG_makeAllCombinations(Combination,AllCombinations),
	checkES_STG_matchCombinations(Selection,AllCombinations).
%
%bij het maken van de combinaties is de situatie met nog maar 1 element te gaan een speciaal geval
%deze moet namelijk de basic combinaties bouwen
%bij-effect: als state geen lijst is, dan wordt het dat hier

checkES_STG_makeAllCombinations([One/[State]], [[One/State]]):-!.

checkES_STG_makeAllCombinations([One/[State|States]],[[One/State]|Others]):-!,
	checkES_STG_makeAllCombinations([One/States],Others).

checkES_STG_makeAllCombinations([One/State], [[One/State]]):-!. %ook /any

%en de situatie voor de overige elementen:
checkES_STG_makeAllCombinations([Element|Rest],Combinations):-
	checkES_STG_makeAllCombinations(Rest,RestCombinations),
	checkES_STG_makeCombinationElement(Element,RestCombinations,Combinations).
%
checkES_STG_makeCombinationElement(_El/[],_RestCombinations,[]):-!.

checkES_STG_makeCombinationElement(El/[State|States],CurrentCombinations,Combinations):-!,
	checkES_STG_makeCombinationElement(El/States,CurrentCombinations,NewCombinations),
	checkES_STG_appendElementState(El,State,CurrentCombinations,NewCombinations,Combinations).

checkES_STG_makeCombinationElement(El/State,CurrentCombinations,Combinations):- %ook any
	checkES_STG_appendElementState(El,State,CurrentCombinations,[],Combinations). 
%
checkES_STG_appendElementState(_El,_State,[], NewCombinations, NewCombinations).

checkES_STG_appendElementState(El,State,[First|RestCombinations],AddedCombinations,Combinations):-
	checkES_STG_appendElementState(El,State,RestCombinations,[[El/State|First]|AddedCombinations],
				   Combinations).
%
checkES_STG_matchCombinations(Selection,[First|_Rest]):-
	checkES_STG_combinationSelected(Selection,First),!.

checkES_STG_matchCombinations(Selection,[_First|Rest]):-
	checkES_STG_matchCombinations(Selection,Rest).
%
checkES_STG_combinationSelected([],[]):-!.

checkES_STG_combinationSelected(Selection,[Element/State|RestElements]):-!,
	select(SelElement,Selection,RestSelection),
	(   Element == any
	;   Element = SelElement<<-name
	),
	(   State == any
	;   State = SelElement<<-state
	),
	%1 gevonden, door met de rest
	checkES_STG_combinationSelected(RestSelection,RestElements).
%%



:-pce_end_class.

