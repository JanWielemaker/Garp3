/*
Elements.pl
ensure_loaded en autoload declaraties voor deze directory
*/

/*element classes for Sketch environment, AB feb 2006  */

:-load_garp(sketchGenericRelationElement,'sketch_generic_relation_element.pl').
:-load_garp(sketchElement,'sketch_element.pl').
:-load_garp(sketchConceptElement,'sketch_concept_element.pl').
:-load_garp(sketchRelationElement,'sketch_relation_element.pl').

:-load_garp(sketchObjectElement,'sketch_object_element.pl').
:-load_garp(sketchStructuralRelationElement,'sketch_structural_relation_element.pl').

:-load_garp(sketchQuantityElement,'sketch_quantity_element.pl').
:-load_garp(sketchValueElement,'sketch_value_element.pl').
:-load_garp(sketchDependencyElement,'sketch_dependency_element.pl').
:-load_garp(sketchInequalityElement,'sketch_inequality_element.pl').
:-load_garp(sketchStateElement,'sketch_state_element.pl').
:-load_garp(sketchTransitionElement,'sketch_transition_element.pl').
:-load_garp(sketchDefinitionElement,'sketch_definition_element.pl'). % AB, nov 2006
