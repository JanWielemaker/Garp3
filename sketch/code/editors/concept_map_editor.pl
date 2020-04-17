/*
Definition of concept_map_editor class
Editor for concept maps 

Part of Garp3, see copyright notice
Code partly inspired by old homer code
2000-2005 Jelmer Jellema, Spin in het Web, spininhetweb.nl
2006 Anders Bouwer 
*/                                                                                                                                                                                                                            

:-pce_begin_class(
		  concept_map_editor,
		  framedWindow,
		  "Implements the concept map editor"
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
			  , helpId := 'ConceptMapEditor'
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
		   new(Elements, popup('Elements'))
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
		   menuCommand(addConcept,
			       'AddConcept','Add Sketch Concept'),
		   menuCommand(addRelation,
			       'AddRelation','Add Sketch Relation',
			       end_group := @on)
		  ]
		  ).
%
%%

%
init_buttonbar(VE):->
	% create the elements for the buttonbar
	% version for concept map editor
	B = VE<<-buttonBar,
        % for concept map editor

	B<<-add(addConcept,'AddConcept',sketch_buttonbars,new_concept,new_concept_g,'Add Sketch Concept'),
	B<<-add(addRel,'AddRelation',sketch_buttonbars,new_relation,new_relation_g,'Add Sketch Relation',below, addConcept),
	B<<-add(delete,'Delete',sketch_buttonbars,delete,delete_g,'Delete',below, addRel).

	% B<<-add(addConcept,'AddConcept',sketch_buttonbars,addConceptTxt,addConceptTxtg,'Add concept'),
	% B<<-add(addRel,'AddRelation',sketch_buttonbars,addRelTxt,addRelTxtg,'Add relation',below, addConcept).
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
	VE->>command('AddConcept', key := '\\C-n', keystring := '[Ctrl + N]'), 
	VE->>command('AddRelation',key := '\\C-r', keystring := '[Ctrl + R]'),

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
	%gp3 0.3 changed this: we added the new toggleSubsketches command
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
        % Always save the model!
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
	@app->>loadSketch(VE, conceptMapMF).
%%

% menuCommand for saveSketch
checkSaveSketch(_VE):->
	true.
%
%
onSaveSketch(VE):->
	%pass thru to app
	@app->>saveSketch(VE, conceptMapMF).
%%


/*****LOSSE OBJECTEN ED *********/


% AddConcept % AB, feb 2006
% based on givenConcept
%
checkAddConcept(_VE):->
	% always possible
	true.

%
onAddConcept(VE):->
	%Add a concept in the Concept Map Editor
	%Open the concept properties dialog
	new(sketchConceptPropsDlg(VE))->>newObject(concept).


% AddRelation % Ab, feb 2006
% based on givenConfiguration
checkAddRelation(VE):->
	%possible in all types, when two instanceElements are selected

	checkEditorState(VE,any,any,any,
			 [[sketchConceptVisualElement,sketchConceptVisualElement]/any]).% or should this not be changed? Ab, feb 2006
%			 [[instanceElement,instanceElement]/any]).
%
onAddRelation(VE):->
	% add Relation
	Sel = VE<<-selection,
	First= Sel<<-head,
	Sec = Sel<<-tail,
	new(sketchRelationPropsDlg(VE))->>newObject(First?sketchElement,
									First?route,
									Sec?sketchElement,
									Sec?route).

	
%%
/*******Voor meerdere objecten*************/

%%Delete
checkDelete(VE):->
	checkEditorState(VE,
			 any, %welke sketchen mogen
			 any, %welke sketchen mogen niet
			 1, %aantal geselecteerde objecten
			 [
				[sketchConceptVisualElement/any],
				[sketchRelationVisualElement/any]
			]
			).
%%


%
onDelete(VE):->
	%Regel het verwijderen
	E = VE<<-singleSelection,
	Type = E<<-name,
	FE = E<<-sketchElement,
	pl_onDelete(VE,FE,Type).
%
pl_onDelete(VE,FE,sketchConceptVisualElement):-
	%we sturen de betreffende CR
	@model->>changeRequest(deleteFConcept,
			       VE?sketch,
			       VE,
			       FE).
%
pl_onDelete(VE,FE,sketchRelationVisualElement):-       
	@model->>changeRequest(deleteRelation,
			       VE?sketch,
			       VE,
			       FE).
%%

%%properties
checkProperties(VE):->
	%mag bij elk niet-fixed object, alleen mag er maar 1 geselecteerd zijn

	checkEditorState(VE,any,any,1,[[any/any]]),
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
	pl_onProperties(VE,Type,FE,Sketch,Element,ReadOnly).

%voor elk type een clause...
pl_onProperties(VE,sketchConceptVisualElement,FE,_Sketch,_Element,ReadOnly):-
	new(sketchConceptPropsDlg(VE))->>editObject(FE, ReadOnly).


%
pl_onProperties(VE,sketchRelationVisualElement,FE,_Sketch,_Element, ReadOnly):-
	new(sketchRelationPropsDlg(VE))->>editObject(FE, ReadOnly).


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
changeApplied_newFConcept(VE,
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
changeApplied_changeFConcept(VE,
			      CR: changeRequestor
			     ):->
	%gp3 0.2: changed to only update the relevant elements
	VE->>updateElement(CR?arg1).
%%

%%
changeApplied_deleteFConcept(VE,
			      _CR:changeRequestor
			     ):->
	VE->>mustUpdateVisualisation. %gp3, changed this to update when all done
%%


%%
changeApplied_newRelation(VE,
	CR: changeRequestor):->

	VE->>update_visualisation,
	CE = VE<<-findFirstElement(CR?result,sketchRelationVisualElement),
	VE->>checkHideNewElement(CE,CR),
	%gp3: make sure the element is visible
	if
		CR->>checkEditor(VE)
	then
		VE?client->>normalise(CE).
%%

%%
changeApplied_changeRelation(VE,
	_CR: changeRequestor):->

	VE->>update_visualisation.
	% VE->>mustUpdateVisualisation.
%%

%%
changeApplied_deleteRelation(VE,
	_CR: changeRequestor):->

	% VE->>deleteUnusedDefs,
	VE->>update_visualisation.
	% VE->>mustUpdateVisualisation.
%%


/* not used anymore, AB, nov 2006
%
deleteUnusedDefs(VE):->
	get(@model, sketchRelationDefinitions, RelDefs), 
	send(RelDefs, for_some, 
	     message(VE, deleteUnusedDef, @arg1)
	     ).
%%

%
deleteUnusedDef(VE, RelDef):->
	% Delete relation definition RelDef if it is not used anymore

	% if there is another sketch concept relation using the same relation definition
	if (
	    Configs = VE?sketch<<-findElements(sketchRelationElement), % or relation? AB, feb 2006
	    chain_list(Configs, CList), 
	    member(C, CList), 
	    get(C, hypered, definition, RelDef2), 
	    RelDef2->>equal(RelDef)
	    )
        then
	    % no need to do delete
	    true
	else
	   (
	    % delete the RelationDef
	    @model->>changeRequest(deleteRelationDef,
		@model,
		% VE?sketch,
		VE,
 	        RelDef)
	   ).
%%
*/

%%
changeApplied_changeRelationDef(VE,
	_CR:changeRequestor):->

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
% changeTreeApplied(VE,
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
	
	VE->>draw_concept_map(VE?sketch,new(chain), @nil, normal, UpdatedElements),
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
draw_concept_map(VE,
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
	
	%CONCEPTS
%@pce->>write_ln(Sketch?name,': Sketch Concepts'),
	?(Sketch,findElements,sketchConceptElement)->>for_all(
				->>(UpdatedElements,append,
						?(VE,
					      displayConcept,
					      @arg1,
							Route,
							ContainedSketchElement,
							State))),
%%@pce->>write_ln(Sketch?name,': Sketch Relations'),	
	%RELATIONS
	Configs = Sketch<<-findElements(sketchRelationElement), % or relation? AB, feb 2006
	% Configs = Sketch<<-findElements(relation),
	Configs->>for_all(
				->>(UpdatedElements,append,
						?(VE,
						displayRelation,
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
displayConcept(VE,
		   I : sketchConceptElement,
			Route: chain,
			IMF: 'importedMFElement*',
		   SketchState: '[{normal,parent,imported}]',
		   IE: sketchConceptVisualElement % or shouldn't this be changed? AB, feb 2006
%		   IE: instanceElement
		  ):<-
	"Display this concept" ::

	%we maken het instance element en melden hoe ie afgebeeld moet worden
	%ook sturen we ons sketch mee, zodat eventueel opgeslagen lay-out info
	%opgehaald wordt. Het element wordt dan meteen afgebeeld.
	%wanneer er een importedMFElement is meegestuurd wordt de instance daar een sub van
	
	%sub van geïmporteerd fragment?
	default(IMF,@nil,RC),

	(
		(
			IE = VE<<-findElement(I,sketchConceptVisualElement,Route), % or should this not be changed? Ab, feb 2006
			% IE = VE<<-findElement(I,instanceElement,Route),
			ignore(IE->>updateDisplay)
		)
	;
		(
			%gp3 0.4.11: we do not send a point for placement, but only the IMF
			%when this is @nil , placement will be according to visibleSub strategy
			%otherwise we use 'spot' with a default startingpoint
			%gp3 1.0: super element now standard argument in visualElement
			IE *= sketchConceptVisualElement(VE?sketch, % or should this not be changed? Ab, feb 2006
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
displayRelation(VE,
	C: sketchRelationElement,
	Route: chain,
	_IMF: '[importedMFElement*]',
	FS: '[{normal,parent,imported}]',
	UpdatedElements: chain,
	RouteMap: chain,
	CE: sketchRelationVisualElement):<-
	"Display this relation" ::

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
		Arg1El = VE<<-findElement(Arg1,sketchConceptVisualElement,Arg1R,@default,UpdatedElements),
		
	if
		Arg2 = @nil
	then
		Arg2El = @default
	else
		Arg2El = VE<<-findElement(Arg2,sketchConceptVisualElement,Arg2R,@default,UpdatedElements),
		
	(
		(
			CE = VE<<-findElement(C,sketchRelationVisualElement,Route),
			ignore(CE->>setArguments(Arg1El,Arg2El)) %doet ook updateDisplay
		)
	;
		(

			CE *= sketchRelationVisualElement(VE?sketch,
					C, VE?client, Arg1El,Arg2El,FS,Route), %punt wordt door relatie bepaald
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
           Label *= string('Concept Map Editor - Sketch')
        )
        else
	(
           get(SavedSketchName, value, Str), 
	   swritef(Label, 'Concept Map Editor - Sketch - %w', [Str])
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
checkEditorState: Een algemene call voor het checken op de editor state.
Prototype:
checkEditorState(VE,AllowedSketches,DisallowedSketches,
		      AllowedSelectionTypes, AllowedCombinations)
		      
AllowedSketches: Een lijst met typen sketchen die mogen, of any (geen check)
                  [processSketch,agentSketch]
DisallowedSketches: Een lijst met typen sketchen die niet mogen, of any (geen check)
		  [staticSketch]
AllowedSelectionTypes: Een lijst met soorten selectie die mogen (none, single, multi of een aantal)
                                 of any (geen check)
		  [none,multi]
AllowedCombinations: Een lijst met daarin lijsten die aangeven wat mag ([Combination,...]) of any (geen check)
Combination: [{elementtype|any}/{[State,...]|any},....] of [{elementtype|any},...]/{[State,...]|any} 
State: condition|consequence|imported|parent|fixed	

Voor AllowedSketches, DisallowedSketches, AllowedSelectionTypes en de Statelijst in AllowedCombinations
geldt dat er geen lijst hoeft te worden gebruikt als het om maar één element gaat.

Voorbeeld
checkEditorState(VE,[processSketch,agentSketch],any,2,
		 [[quantityElement/condition,quantityElement/any],
		  [calculusElement,calculusElement]/[condition,imported]]).
Deze call slaagt wanner:
1 Het bewerkte sketch een processSketch of agentSketch is
2 Er meer 2 elementen geselecteerd zijn
3 Er een conditionele quantity en nog een quantity is geselecteerd OF er twee conditionele of geimporteerde
  calculi zijn geselecteerd

Los te gebruiken zijn ook checkES_Sketches, _DisallowedSketches, _Selection
en _Combinations.
*/

checkEditorState(VE, AllowedSketches,
		 DisallowedSketches,
		 AllowedSelectionTypes,
		 AllowedCombinations):-

	checkES_Sketches(VE,AllowedSketches),
	checkES_DisallowedSketches(VE,DisallowedSketches),
	checkES_Selection(VE,AllowedSelectionTypes),
	checkES_Combinations(VE,AllowedCombinations).
%
checkES_Sketches(_VE,any):-!.

checkES_Sketches(VE,Allowed):-
	C = VE?sketch<<-currentType,
	member(C,Allowed).

checkES_Sketches(VE,Allowed):-
	%geen lijst
	Allowed = VE?sketch<<-currentType.
%
checkES_DisallowedSketches(_VE,any):-!.

checkES_DisallowedSketches(VE,Disallowed):-
	C = VE?sketch<<-currentType,
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



