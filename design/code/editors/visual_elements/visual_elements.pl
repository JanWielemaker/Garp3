/*
visual_elements.pl
ensure_loaded en autoload declaraties voor deze directory
Dit zijn dus allemaal af te beelden elementjes in een view
*/

:-load_garp(visualElement,'visual_element.pl').

:-load_garp(instanceElement,'instance_element.pl').
:-load_garp(assumptionInstanceElement,'assumption_instance_element.pl').
:-load_garp(quantityElement,'quantity_element.pl').
:-load_garp(attributeElement,'attribute_element.pl').
:-load_garp(qsElement,'qs_element.pl').
:-load_garp(qsValueElement, 'qs_element.pl'). %helper class
:-load_garp(derivativeElement,'derivative_element.pl').
:-load_garp(dqsValueElement,'derivative_element.pl'). %helper

:-load_garp(staticMarker,'static_marker.pl'). %parentklasse van statische markeringen (valueMarker en 
									%hiddenRelationMarker)
:-load_garp(valueMarker,'value_marker.pl'). %value weergeven bij een quantity

:-load_garp(visualRelationElement,'visual_relation_element.pl').
:-load_garp(configurationElement,'configuration_element.pl').

:-load_garp(inequalityRelationElement,'inequality_relation_element.pl').

:-load_garp(calculusElement,'calculus_element.pl').
:-load_garp(correspondenceElement,'correspondence_element.pl').
:-load_garp(quantityRelationElement,'quantity_relation_element.pl').

:-load_garp(importedMFElement,'imported_mf_element.pl').
:-load_garp(identityRelationElement,'identity_relation_element.pl').
