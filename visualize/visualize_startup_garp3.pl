/*
Garp3 0.1
Startup the visualize code

To start the visualize code for testing, use the call 'visigarp.'

*/

:-module(visualize,[]).


:- use_module(library(pce)).
:- use_module(library(debug)).
:- require([ forall/2
	   , free_variables/2
	   , random/3
	   , term_to_atom/2
	   ]).


:- use_module(library(pce_tagged_connection)).

/*dynamic etc for the visialize main source */

:- dynamic
        current_state/1,
        currentX/2,
        currentY/2,
        type_of_file_first_opened/1,
        state_node_size/1,
        settings/2,
        settings/3,
        settings/4,
        rec/1,
        rec/2,
	entity_var_nr/2,
	rec_transition/2,
	rec_real_transition/2,
	preferred_equation_type/2, %from visigarp 2.07, equation history view
        preferred_equation_format/2.


%these predicates are defined for state and graph-like stuff (in class_graph_node and class_state_node):
:- multifile
		set_pen/2, set_colour/3, position_text/3, shape_box/2, click_graphical/1,
			click_graphical2/1, shift_click_graphical/1,settings/3.

%gp0.1 these preds are just like everywhere
:- discontiguous
		settings/2, settings/4.

/*
dynamic etc for accessories(aggregation)
*/

:-
	dynamic temp_event_counter/2.

user:file_search_path(accessories,visualize('Accessories')).
%user:file_search_path(quags,visualize('Quags')).

%load all files (classes are allready consulted, no pce_autoload. This to get them in the visualise module...
%this is a todo...

:- [
	 settings
	,globals
	,class_visigarp
	,table_view_classes
	,helper_classes
	,graph_node_recogniser
	,class_graph_node
	,class_state_master_node
	,class_state_node
	,class_my_tagged_connection
	,dependency_icon
	,question_generation_globals	%quags?
	,show_transitions
	,structure_transform
	,show_quantity
	,show_entity
	,show_value_history
	,show_modelfragment
	,graphics_helpers
	,class_causal_graph_viewer
	,show_relations
	,showhide
	,show_hierarchy
	,arrows
	,buttons
	,tracer
	,quantityvalues_dialog
	,equation_history_view %code added from visigarp 2.07
	,accessories(interface)
	,accessories(graph_routines)
	,accessories(causal_model_mechanisms)
	,accessories(aggregation)
	,find_in_design %gp3 1.4
%	,quags(main_qg)
	].

%TODOs
reload_library:-todo. %see original load.pl
