/*
Elements.pl
ensure_loaded en autoload declaraties voor deze directory
*/

/*element classes*/
:-load_garp(fragmentElement,'fragment_element.pl').
:-load_garp(importedFragment,'imported_fragment.pl').
:-load_garp(fragmentRefiner,'fragment_refiner.pl').
:-load_garp(garpInstance,'garp_instance.pl').
:-load_garp(assumptionInstance,'assumption_instance.pl').
:-load_garp(garpQuantity,'garp_quantity.pl').
:-load_garp(garpAttribute,'garp_attribute.pl').
:-load_garp(value,'value.pl').
:-load_garp(garpRelation, 'garp_relation.pl').
:-load_garp(parRelation, 'par_relation.pl').
:-load_garp(configuration, 'configuration.pl').
:-load_garp(inequality,'inequality.pl').
:-load_garp(correspondence,'correspondence.pl').
:-load_garp(garpQuantityRelation,'garp_quantity_relation.pl').
:-load_garp(identityRelation,'identity_relation.pl').
:-load_garp(calculus,'calculus.pl').

:-load_garp(valueReference,'value_reference.pl').