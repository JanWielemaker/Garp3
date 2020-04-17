/*
definition of class sketchScenariosDefinitionEditor
Intended to be a superclass for the three Editors for 
creating and editing of sketch processes, external influences, and scenarios

edit kan aangeroepen met een bestaande definition voor het bewerken ervan.
Wanneer edit wordt gebruikt zonder argumenten wordt in new mode geopend.

2006 Anders Bouwer, Universiteit van Amsterdam,
Based on Garp3 Attribute definition editor
Still includes some comments in Dutch
*/



:-pce_begin_class(scenariosEditor,
		  sketchDefinitionEditor,
		  "Scenario editor"
		 ).

variable(def,sketchScenarioDefinition*,get, 
		"the edited sketchScenarioDefinition"). %@nil betekent: nieuwe
%%
initialise(D, OpenedBy: [object]):->
	"Initialise the editor" ::
	%gp3 0.1: added openedBy, will be a hyper to the given object, so others can ask voor ?openedBy  
	get(@model, getModelNameForEditor, 'Scenario definitions - Sketch', ModelNameForEditorLabel),
	D->+initialise(OpenedBy, ModelNameForEditorLabel,'Sketch_ScenarioDefinitions'),
	D?defList_member->>label('Scenario definitions:').
%%

%
init_import_menu(_VE, Import):->
	send_list(Import,append,
		  [
		   menuCommand(importConcepts,
			       'ImportConcepts','Import concepts from Concept Map'),
		   menuCommand(importEntities,
			       'ImportEntities','Import entities from Structural Model'),
		   menuCommand(importAgents,
			       'ImportAgents','Import agents from Structural Model'),
		   menuCommand(importAssumptions,
			       'ImportAssumptions','Import assumptions from Structural Model'),
		   menuCommand(importQuantities,
			       'ImportQuantities','Import quantities from Causal Model',
			       end_group := @on)
		  ]
		  ).
%%


/******************COMMANDS**************************/
%%
init_commands(VE
	      ) :->
	"Initialise command objects" ::
	%initialiseer de commando s. Zie assistance_dialog_with_menubar.pl, framedwindow.pl en command.pl 
	VE->>command('SaveModel', key := '\\C-s', keystring := '[Ctrl + S]'),
	VE->>command('ImportConcepts',key := '\\C-i', keystring := '[Ctrl + I]'),
	VE->>command('ImportEntities'),
	VE->>command('ImportAgents'),
	VE->>command('ImportAssumptions'),
	VE->>command('ImportQuantities'),
	% Old version - more general 
	% VE->>command('ImportObjects',key := '\\C-j', keystring := '[Ctrl + J]'),

	%commands voor het afhandelen van de menu-balk, deze runt dus niet
	%maar de update command is handig voor het vullen van het menu enzo
	VE->>command('ContextPopup',runnable := @off).
%%	


/***********************************/


%
add_Add_button(_D, Add):<-
	Add *= imgButton(add, img:=new_scenario, tt:='Add scenario definition').

add_Remove_button(_D, Remove):<-
	Remove *= imgButton(remove, img:=delete_scenario, tt:='Delete selected scenario definition').
%%

%
add_tabtool(D, TabTool):<-
 	new(TabTool, 
		 tab_stack(
			  new(EntitiesTab, tab('Entities')),
			  new(AgentsTab, tab('Agents (if any)')),
			  new(QuantitiesTab, tab('Quantities')),
			  new(InitialValuesTab, tab('Initial Values')),
			  new(InequalityStatementsTab, tab('(In)equality Statements')),
			  new(AssumptionsTab, tab('Assumptions'))
		 )
	),
 	TabTool->>name(tabtool), 

        % init tabs
        D->>init_table_tab(EntitiesTab, entity), 
        D->>init_table_tab(AgentsTab, agent), 
        D->>init_table_tab(QuantitiesTab, quantity), 
        D->>init_editor_tab(InitialValuesTab, initial_values), 
        D->>init_editor_tab(InequalityStatementsTab, inequality_statements), 
        D->>init_table_tab(AssumptionsTab, assumption).
%%


%
default_sketchDefName(_D, Name: name):<-
         Name = 'New sketch scenario definition'.
%%



%
fillDefList(D):->
	"Fill the list with scenario definitions" ::
	D?defList_member->>clear,
	@model?sortedSketchScenarioDefinitions->>for_all(->>(D,fillDefList_helper,@arg1)).
%%
%
clearDefSheet(D):->
        D->>clear_table_tab(entity), 
        D->>clear_table_tab(agent),
        D->>clear_table_tab(quantity),
        D->>clear_tab(initial_values), 
        D->>clear_tab(inequality_statements), 
        D->>clear_table_tab(assumption).
        % To Do: make code more systematical
%%

%
fillDefSheet(D, _Def):->
        % fill tabs
        D->>fill_table_tab(entity), 
        D->>fill_table_tab(agent),
        D->>fill_table_tab(quantity),
        D->>fill_tab(initial_values), 
        D->>fill_tab(inequality_statements), 
        D->>fill_table_tab(assumption).
        % To Do: make code more systematical
%%    

%
create_data_sheet(D, S:sheet):<-
        get(D, create_def_chain, entity, E1), 
        get(D, create_def_chain, agent, E2), 
        get(D, create_def_chain, quantity, E3), 
        get(D, hypered, initial_valuesTabEditor, E4), 
        get(D, hypered, inequality_statementsTabEditor, E5), 
        get(D, create_def_chain, assumption, E6), 
        new(S, sheet(
		     attribute(entity, E1), 
		     attribute(agent, E2), 
		     attribute(quantity, E3),
		     attribute(initial_values, E4?text_buffer?contents),
		     attribute(inequality_statements, E5?text_buffer?contents),
		     attribute(assumption, E6)
		    )).
%%

/* old version
%
create_data_sheet(D, S:sheet):<-
        get(D, hypered, entitiesTabEditor, E1), 
        get(D, hypered, agentsTabEditor, E2), 
        get(D, hypered, quantitiesTabEditor, E3), 
        get(D, hypered, initial_valuesTabEditor, E4), 
        get(D, hypered, inequality_statementsTabEditor, E5), 
        get(D, hypered, assumptionsTabEditor, E6), 
        new(S, sheet(
		     attribute(entity, E1?text_buffer?contents), 
		     attribute(agent, E2?text_buffer?contents), 
		     attribute(quantity, E3?text_buffer?contents),
		     attribute(initial_values, E4?text_buffer?contents),
		     attribute(inequality_statements, E5?text_buffer?contents),
		     attribute(assumption, E6?text_buffer?contents)
		    )).
%%
*/



% 
notChangedDefSheet(D):->
        forall(member(X, [entity, agent, quantity, initial_values, inequality_statements, assumption]), 
	       D->>notChangedTabContents(X)
	       ). 
%%

       
%%
saveDef(D):->
	"Save the changes that were made" ::

	D->>notChanged,!. %niets te doen
%
saveDef(D):->
	%kee, nu moeten we een CR bouwen
	%hangt af of we in nieuw modus zijn of niet
        
        get(D, create_data_sheet, S), 
	if
		@nil = D<<-def
	then
		@model->>changeRequest(addSketchScenarioDef,
				@model,
				D,
				D?name_member?selection,
				D?remarks_member?contents, 
				S)
	else
		@model->>changeRequest(changeSketchScenarioDef,
				D?def,
				D,
				D?name_member?selection,
				D?remarks_member?contents, 
				S).
%%	

%%%%%%% Import Functionality

%
create_import_type_menu(VE, SourceType, M1):<-
	new(M1, menu('to the following type in the current scenario definition:', choice)),
	% default message when a menu item is chosen
	send(M1, message, and(	 
				 message(VE, set_import_type_for_scenario_definition, @arg1)
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
		 menu_item('entity', @default, 'Entities'),
		 menu_item('agent', @default, 'Agents'), 
		 menu_item('quantity', @default, 'Quantities'),
		 menu_item('assumption', @default, 'Assumptions')
		]),

        % the first time, set the selection to the default
	send(M1, selection, SourceType), 
        send(VE, set_import_type_for_scenario_definition, SourceType).
%%


%
set_import_type_for_scenario_definition(_VE, Type):->
        @app->>setting(import_concepts_from_concept_map_to_scenario_definition, Type).
%%

%
import_type(_VE, Type):<-
        Type = @app<<-setting(import_concepts_from_concept_map_to_scenario_definition).
%%

%
report_import_error(VE):->
        VE?frame->>report(error, 
	       'Warning: Could not import. Select a scenario definition in the scenario definitions editor first').
%%

%%
/******************** MULTIPLE MODELS ***********************/ %JL

/* Make the right model active */
input_focus(T, Boolean) :->
	send(@app, selectCorrectTab, T), % T is the window object (of the editor)
	send_super(T, input_focus, Boolean).


%%%%%%%%%%Helpers%%%%%%%%%%%%%%%%
%%%%%%%%%CHANGES%%%%%%%%%%%%%%%%
%%
changeApplied_addSketchScenarioDef(D,
	CR:changeRequestor):->
	%de nieuwe sketchScenarioDef wordt toegevoegd aan de lijst met defs
	%als dat in deze editor is gebeurd zonder dat we naar een andere zijn gesprongen, dan 
	%selecteren we hem
	/*
	Net als bij changeSketchScenarioDef kan  het opslaan gebeuren doordat de boel 
	in deze editor wordt 
	opgeslagen nadat een andere def is geselecteerd: het gaat bij het herselecteren dan om
	de oude selectie en niet om de opgeslagen D?def
	*/

	List = D<<-defList_member,
	OldSelection = List<<-selection,
	if
		OldSelection = @nil
	then
		OldDef = @nil
	else
		OldDef = OldSelection<<-object,

	D->>fillDefList,
	if	
	(
		CR->>checkEditor(D), 
		OldSelection = @nil %en we zijn nog braaf in new modus
	)
	then
	(
		List->>selection(CR?result),
		D->>slot(def,CR?result), %de nieuwe selectie
	        % save again, because otherwise, def is not known in the data_sheet members
	        % because def is now selected, saveDef will go down the other branch
	        D->>saveDef, 
		D->>fillCurrentDefData
	)
	else
	(
		%we moeten dus de oude selectie herstellen
		if 
			(OldDef \== @nil)
		then
			List->>selection(OldDef)
	).
%%

%%
changeApplied_changeSketchScenarioDef(D,
		_CR:changeRequestor):->
	%we weten zeker dat de geselecteerde def nog steeds bestaat, dus daar hoeven we niet op te checken
	%het enige dat we doen is de lijst hervullen en de oude selectie herstellen
	%wanneer de thans geselecteerde def dezelfde is als de interne "huidige" def, en deze is gewijzigd
	%dan lezen we de gegevens opnieuw in.
	%Dit gebeurt dus niet op het nivo van de <-editor van de CR, maar op de test: is degene die is afgebeeld gewijzigd
	%(bij selecteren van een andere def wordt op deze manier hier geen data opnieuw weergegeven, omdat de opgeslagen
	% def wel de huidige <-def is, maar niet de geselecteerde in de lijst)

	%CLAUSE 1: er is geen selectie
	@nil = D?defList_member<<-selection,!,
	D->>fillDefList.
%
changeApplied_changeSketchScenarioDef(D,CR):->
	%CLAUSE 2: er is wel een selectie, dus die is er nog na de change
	Selected= D?defList_member?selection<<-object,
	D->>fillDefList,
	D?defList_member->>selection(Selected), 

	%ok, als Selected gelijk is aan het object van de CR én aan de interne <-def
	%dan moeten we opnieuw inlezen

	if
		(	CR->>checkObject(Selected),
			Selected = D<<-def
		)
	then
		(D->>fillCurrentDefData).
%%

%%
changeApplied_deleteSketchScenarioDef(D,
	CR : changeRequestor
	):->
	/*
	Het kan dus zijn dat de geselecteerde def weg is. In dat geval kiezen we een andere 
	*/

	List = D<<-defList_member,
	Deleted = CR<<-argument(1),
	if 
		(Deleted = D<<-def)
	then (
		(
			New = List?nextItem<<-object
		;	New = List?prevItem<<-object
		;	New = @nil
		),

		List->>selection(New),
		D->>slot(def,New),
		D->>fillCurrentDefData
		),
	List->>delete(Deleted).
%%



:-pce_end_class.
