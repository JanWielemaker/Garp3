/*
Definition sketchMainTabs class

Sketch Environment consisting of several tab windows
Based on framedWindow and viewEditor class

Part of Garp3, see copyright notice

2006 Anders Bouwer, Universiteit van Amsterdam

*/                                                                                                                                                                                                                            

:-pce_begin_class(
		  sketchMainTabs,
		  framedWindow, 
		  "Implements the sketch main frame"
		 ).


variable(fragment,sketch,get, "the edited sketch").

variable(moveSelect,gesture,get,"The saved click=select / move gesture").
variable(moveSelectSingle,gesture,get,"The saved click=select / move-single gesture"). %gp3 0.3

variable(toggleSelect,click_gesture,get,"The saved shift-click = toggle select gesture").

variable(toggleSelect_alternative, click_gesture, get, "Alternative for shift_click: middle_click").

variable(contextMenu, contextPopup,get,"The context menu").

variable(mustUpdateVisualisation, bool, get). %gp3: @on when it is needed to update visualisation in de next changeTreeApplied call (see mustUpdateVisualisation) 
%%


initialise(VE,
	   Fragment: fragment = sketch
	   ) :->
	"Create the sketchbook and open it" ::
Begin = @pce<<-cpu_time,
	VE->>slot(fragment,Fragment), %moet eerder ivm label
	VE ->+ initialise(
			   windowLabel
			  ,buttonbar := vertical
			  , helpId := 'Sketch'),

	send(VE, label, 'Garp3 Sketch Tool'),
	VE->>icon(@build_icon),	     

        % this is the important one, for the rest, VE should be discarded.
	VE->>init_tabs,

	VE->>position(point(@display?width *0.15,@display?height * 0.15)),
	MaxSize *= size(@display?width * 0.80,
		       @display?height * 0.80),
	DefSize *= size(550,480),
	SavedSize = Fragment<<-layOutInfoDefault(Fragment,editSize,
								DefSize),

	%bepaal de grootte: er kan opgeslagen zijn dat het window kleiner moet
	%groter mag echter niet

	Width *= number(MaxSize?width),
	Width->>minimum(SavedSize?width),
	Height *= number(MaxSize?height),
	Height->>minimum(SavedSize?height),
	VE->>size(size(Width,Height)),

	%DISPLAY POSITION
	if
		Origin = Fragment<<-layOutInfo(Fragment,
									displayOrigin)
	then
		VE?client->>scroll_to(Origin),
Eind = @pce<<-cpu_time,
if
	runMode(debug)
then
	@pce->>write_ln('Cpu-time: ', Eind - Begin),

	get(@model, '_value', Model),
	send(VE, associateModel, Model).
%%



init_tabs(VE):->
        % get(VE, hypered, input_window, D), 
        new(D, dialog('Garp3 Sketchbook')),        
        % writef('VE: %d, D: %d.\n', [VE, D]), 
        % send(VE, append, D), 
 	send(D, append,
		 tab_stack(
			  new(OrientationTab, tab('Orientation and Initial Specification')),
			  new(StructuralModelTab, tab('System Selection and Structural Model')),
			  new(GlobalBehaviourTab, tab('Global Behaviour'))
		 )
	),
        send(VE, init_orientation_buttonbar, OrientationTab), 
        send(VE, init_structural_model_buttonbar, StructuralModelTab), 
        send(VE, init_global_behaviour_buttonbar, GlobalBehaviourTab),            
        % create hyper to Garp3 model
        send(@model, hyper, D, sketchbook),
        send(D, open).


init_orientation_buttonbar(VE, ORTab):->
        new(BB, commandButtonHolder(vertical, size(100,300))), % buttonBar
        send(VE, init_buttonbar_OR, BB), 
        send(ORTab, append, BB),         
        send(VE, init_commands_OR).


init_structural_model_buttonbar(VE, SMTab):->
        new(BB, commandButtonHolder(vertical, size(100,300))), % buttonBar
        send(VE, init_buttonbar_SM, BB), 
        send(SMTab, append, BB),         
        send(VE, init_commands_SM).


init_global_behaviour_buttonbar(VE, GBTab):->
        new(BB, commandButtonHolder(vertical, size(100,300))), % buttonBar
        send(VE, init_buttonbar_GB, BB), 
        send(GBTab, append, BB),         
        send(VE, init_commands_GB).


% for Orientation buttonbar
init_commands_OR(VE) :->
	% VE->>command('Concept Map',key := 'C', keystring := '[C]'), 
	VE->>command('ConceptMap').


% for SM buttonbar
init_commands_SM(VE) :->
	VE->>command('StructuralModel').


% for GB buttonbar
init_commands_GB(VE) :->
	VE->>command('CausalModel').


onConceptMap(_VE):->
        get(@model, hypered, conceptMapMF, MF1),                 
        new(SCM, concept_map_editor(MF1)), 
        send(SCM, open).

onStructuralModel(_VE):->
        get(@model, hypered, structureMF, MF1),                 
        new(SCM, structure_editor(MF1)), 
        send(SCM, open).

onCausalModel(_VE):->
        get(@model, hypered, causalModelMF, MF1),                 
        new(SCM, causal_model_editor(MF1)), 
        send(SCM, open).

onSTGraph(_VE):->
        get(@model, hypered, stgraphMF, MF1),                 
        new(SCM, stgraphEditor(MF1)), 
        send(SCM, open).

onProcesses(_VE):->
        % get(@model, hypered, processesSketch, PSK),                 
        new(SCM, processesEditor), 
        % new(SCM, processesEditor(PSK)), 
        send(SCM, open).

onExternalInfluences(_VE):->
        % get(@model, hypered, externalInfluencesSketch, _EXSK),                 
        new(SCM, agentsEditor), 
        send(SCM, open).

onScenarios(_VE):->
        % get(@model, hypered, scenariosSketch, _SSK),                 
        new(SCM, scenariosEditor), 
        send(SCM, open).



%%
destroy(VE):->
	"Destroy the editor" ::

	%dit is nodig om de popups in gestures te destroyen
	%we bevrijden alle opgeslagen gestures

	VE?moveSelect->>free,
	VE?moveSelectSingle->>free,
	VE?toggleSelect->>free,
	VE?toggleSelect_alternative->>free,
	VE?contextMenu->>free, 

	%we slaan layout-info op bij de elementen
	% VE->>saveLayoutInfo,
	VE->+destroy.
%%

%%
saveLayoutInfo(VE):->
	"Save info about the lay-out of elements and editor in model" ::
	
	%bij destroy
	VE?fragment->>clearLayOutInfo, %gp3 0.3: clear old information
 	All = VE<<-all_named_hypered(element),
	All->>for_some(->>(@arg1,
			  saveLayoutInfo)), %can fail
	%en zelf slaan we ook het een en ander op
	%dit doen we gewoon in het fragment zelf, met het fragment zelf als verwijzing
	VE?fragment->>layOutInfo(VE?fragment,
						displayOrigin,
						?(VE?client?visible?position,plus,point(1,1))), %zitten er 1 naast?
	VE?fragment->>layOutInfo(VE?fragment,
						editSize,
						VE?size).

%%
					     



% for opening all sketch screens
init_buttonbar_OR(VE, B):->
        new(B_ConceptMap, button('ConceptMap', message(VE, onConceptMap))),
	send(B, append, B_ConceptMap, below), 
        send(B, size, size(150, 200)).



% for opening all GB screens
init_buttonbar_GB(VE, B):->
        new(B_Processes, button('Processes', message(VE, onProcesses))), 
        new(B_ExternalInfluences, button('External Influences', message(VE, onExternalInfluences))), 
        new(B_CausalModel, button('Causal Model', message(VE, onCausalModel))), 
        new(B_Scenarios, button('Scenarios', message(VE, onScenarios))), 
        new(B_STGraph, button('Behaviour Graph', message(VE, onSTGraph))), 
	send(B, append, B_Processes, below), 
	send(B, append, B_ExternalInfluences, below), 
	send(B, append, B_CausalModel, below), 
	send(B, append, B_Scenarios, below), 
	send(B, append, B_STGraph, below), 
        % why doesn't this work?
        % send(B_Processes, status, inactive),
        % send(B_ExternalInfluences, status, inactive),
        % send(B_Scenarios, status, inactive).
        send(B, size, size(150, 200)).

        


% for structural model editor
init_buttonbar_SM(VE, B, _P):->
        new(B_StructuralModel, button('StructuralModel', message(VE, onStructuralModel))), 
	send(B, append, B_StructuralModel, below), 
        send(B, size, size(125, 200)).


%%


		

:-pce_end_class.
