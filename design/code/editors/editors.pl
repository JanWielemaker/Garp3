/*
Editors.pl
ensure_loaded en autoload declaraties voor deze directory
*/

/*grafische classes*/
:-load_garp(entityEditor,'entity_editor.pl').
:-load_garp(mfStructureEditor,'mfstructure_editor.pl').
:-load_garp(inputSystemList,'input_system_list.pl'). 
:-load_garp(viewEditor,'view_editor.pl').
:-load_garp(quantityDefEditor,'quantity_def_editor.pl').
:-load_garp(quantitySpaceEditor,'quantity_space_editor.pl').
:-load_garp(attributeDefEditor,'attribute_def_editor.pl').
:-load_garp(configurationDefEditor,'configuration_def_editor.pl').
:-load_garp(mfCopyHelperDlg,'mf_copy_helper.pl').
:-load_garp(metadataEditor,'metadata_editor.pl').
:-load_garp(garp3EditorFrame,'garp3EditorFrame.pl').
:-load_garp(languageEditor,'language_editor.pl').


%gestures
:-ensure_loaded('gestures/gestures.pl').

%de subelementen
:-ensure_loaded('visual_elements/visual_elements.pl').

%en de eigenschap dialogen
:-ensure_loaded('property_dialogs/property_dialogs.pl').
