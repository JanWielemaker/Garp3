/*
Editors.pl
ensure_loaded en autoload declaraties voor deze directory
*/

/* Sketch files - graphical classes */
:-load_garp(conceptMapEditor,'concept_map_editor.pl'). % AB, feb 2006
% :-load_garp(relationDefEditor,'relation_def_editor.pl'). % AB
% :-load_garp(sketchMain,'sketch_main.pl'). % AB
% :-load_garp(sketchEditor,'sketch_editor.pl'). % AB
:-load_garp(stgraphEditor,'stgraph_editor.pl'). % AB
:-load_garp(causalModelEditor,'causal_model_editor.pl'). % AB
:-load_garp(structureEditor,'structure_editor.pl'). % AB
:-load_garp(sketchMainTabs,'sketch_main_tabs.pl'). % AB
:-load_garp(sketchQuantityBrowser,'sketch_quantity_browser.pl'). % AB, april 2006
:-load_garp(sketchDefinitionEditor,'sketch_definition_editor.pl'). % AB, aug 2006
:-load_garp(processesEditor,'processes_editor.pl'). % AB, june 2006
:-load_garp(scenariosEditor,'scenarios_editor.pl'). % 
:-load_garp(agentsEditor,'agents_editor.pl'). 
:-load_garp(savedSketchList,'saved_sketch_list.pl'). % AB, nov 2006

%gestures 
% not necessary
%:-ensure_loaded('gestures/gestures.pl').

%de subelementen
:-ensure_loaded('visual_elements/visual_elements.pl').

%en de eigenschap dialogen
:-ensure_loaded('property_dialogs/property_dialogs.pl').

%helpers
:-ensure_loaded('helpers/table_with_row_selection.pl'). % AB, 7 nov 2006
