/*
property_dialogs.pl
ensure_loaded en autoload declaraties voor deze directory
*/

/*de dialoog klassen*/

:-load_garp(sketchConceptPropsDlg,'sketch_concept_props.pl'). % AB, feb 2006
:-load_garp(sketchRelationPropsDlg,'sketch_relation_props.pl'). % AB, feb 2006

:-load_garp(sketchObjectPropsDlg,'sketch_object_props.pl'). % AB, june 2006
:-load_garp(sketchStructuralRelationPropsDlg,'sketch_structural_relation_props.pl'). % AB, june 2006

:-load_garp(sketchQuantityPropsDlg,'sketch_quantity_props.pl'). % AB, march 2006
:-load_garp(sketchDependencyPropsDlg,'sketch_dependency_props.pl'). % AB, march 2006
:-load_garp(sketchPropDependencyPropsDlg,'sketch_prop_dependency_props.pl'). % AB, march 2006
:-load_garp(sketchInfDependencyPropsDlg,'sketch_inf_dependency_props.pl'). % AB, march 2006
:-load_garp(sketchGenericDependencyPropsDlg,'sketch_generic_dependency_props.pl'). % AB, march 2006
:-load_garp(sketchTransitionPropsDlg,'sketch_transition_props.pl'). % AB, april 2006
:-load_garp(sketchStatePropsDlg,'sketch_state_props.pl'). % AB, april 2006

