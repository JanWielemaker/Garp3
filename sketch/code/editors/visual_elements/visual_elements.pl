/*
visual_elements.pl
ensure_loaded en autoload declaraties voor deze directory
Dit zijn dus allemaal af te beelden elementjes in de Sketch views
*/


:-load_garp(sketchVisualElement,'sketch_visual_element.pl').
:-load_garp(sketchGenericRelationVisualElement,'sketch_generic_relation_visual_element.pl').
:-load_garp(sketchDirectRelationVisualElement,'sketch_direct_relation_visual_element.pl').

:-load_garp(sketchConceptVisualElement,'sketch_concept_visual_element.pl').
:-load_garp(sketchRelationVisualElement,'sketch_relation_visual_element.pl').

:-load_garp(sketchObjectVisualElement,'sketch_object_visual_element.pl').
:-load_garp(sketchStructuralRelationVisualElement,'sketch_structural_relation_visual_element.pl').
:-load_garp(sketchQuantityVisualElement,'sketch_quantity_visual_element.pl').
:-load_garp(sketchDependencyVisualElement,'sketch_dependency_visual_element.pl').
:-load_garp(sketchStateVisualElement,'sketch_state_visual_element.pl').
:-load_garp(sketchTransitionVisualElement,'sketch_transition_visual_element.pl').
:-load_garp(my_sketch_connection,'helper_classes.pl').

