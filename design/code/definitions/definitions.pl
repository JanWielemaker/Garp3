/*
definitions.pl
ensure_loaded en autoload declaraties voor deze directory
*/

/*definitie classes*/
:-load_garp(garpModel, 'garp_model.pl').
:-load_garp(hierarchicalObject,'hierarchical_object.pl').
:-load_garp(abstractEntity,'abstract_entity.pl').
:-load_garp(entity,'entity.pl').
:-load_garp(agent,'agent.pl').
:-load_garp(assumption,'assumption.pl').
:-load_garp(garpQuantityDefinition,'garp_quantity_definition.pl').
:-load_garp(garpAttributeDefinition,'garp_attribute_definition.pl').
:-load_garp(configurationDefinition,'configuration_definition.pl').
:-load_garp(quantitySpace,'quantity_space.pl').
:-load_garp(modelFragment,'model_fragment.pl').
:-load_garp(inputSystem,'input_system.pl').

