/*
property_dialogs.pl
ensure_loaded en autoload declaraties voor deze directory
*/

/*de dialoog klassen*/
:-load_garp(propertyDialog,'property_dialog.pl').

:-load_garp(hObjectPropsDlg,'hierobject_props.pl'). %tbv de entityEditor
:-load_garp(mfPropsDlg,'mf_props.pl'). %tbv de structureEditor
:-load_garp(inputSystemPropsDlg,'is_props.pl'). %tbv de inputSystemList

:-load_garp(instancePropsDlg,'instance_props.pl').
:-load_garp(assumptionPropsDlg,'assumption_props.pl').
:-load_garp(quantityPropsDlg,'quantity_props.pl').
:-load_garp(attributePropsDlg,'attribute_props.pl').
:-load_garp(configurationPropsDlg,'configuration_props.pl').
:-load_garp(inequalityPropsDlg,'inequality_props.pl').
:-load_garp(calculusPropsDlg,'calculus_props.pl').
:-load_garp(correspondencePropsDlg,'correspondence_props.pl').

:-load_garp(qRelPropsDlg,'q_rel_props.pl'). %abstracte parent van proportionality en infl
:-load_garp(proportionalityPropsDlg,'proportionality_props.pl').
:-load_garp(influencePropsDlg,'influence_props.pl').
:-load_garp(importedFragmentPropsDlg,'importedfragment_props.pl').
:-load_garp(refinerPropsDlg,'refiner_props.pl').
:-load_garp(identityPropsDlg,'identity_props.pl').
