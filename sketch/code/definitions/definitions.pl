/*
definitions.pl
ensure_loaded en autoload declaraties voor deze directory
*/

/*definitie classes*/
% Sketch Definitions
:-load_garp(sketch,'sketch.pl'). % AB, march 2006
:-load_garp(sketchConcept,'sketch_concept.pl'). % AB, feb 2006
:-load_garp(sketchRelationDefinition,'sketch_relation_definition.pl'). % AB, feb 2006

:-load_garp(sketchObject,'sketch_object.pl'). % AB, june 2006
:-load_garp(sketchStructuralRelationDefinition,'sketch_structural_relation_definition.pl'). % AB, june 2006
:-load_garp(sketchDefinition,'sketch_definition.pl'). % AB, aug 2006
:-load_garp(sketchProcessDefinition,'sketch_process_definition.pl'). % AB, aug 2006
:-load_garp(sketchAgentDefinition,'sketch_agent_definition.pl'). % AB, aug 2006
:-load_garp(sketchScenarioDefinition,'sketch_scenario_definition.pl'). % AB, aug 2006

:-load_garp(conceptMap,'concept_map.pl'). % AB, march 2006
:-load_garp(structuralModel,'structural_model.pl'). % AB, june 2006
:-load_garp(causalModel,'causal_model.pl'). % AB, march 2006
:-load_garp(stgraph,'stgraph.pl'). % AB, april 2006
:-load_garp(sketchQuantity,'sketch_quantity.pl'). % AB, march 2006
:-load_garp(sketchValue,'sketch_value.pl'). % AB, may 2006
:-load_garp(sketchState,'sketch_state.pl'). % AB, april 2006
:-load_garp(sketchTransition,'sketch_transition.pl'). % AB, april 2006
%:-load_garp(sketchDependency,'sketch_dependency.pl'). % AB, march 2006
%:-load_garp(sketchDependencyDefinition,'sketch_dependency_definition.pl'). % AB, march 2006
%:-load_garp(dummyModelFragment,'dummy_model_fragment.pl').

