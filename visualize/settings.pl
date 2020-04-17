/*****************************************************************************************************
Garp3 0.1
All visiualize source based on visigarp 2.05, designed and implemented by Anders Bouwer 18/08/1999
Original copyright: (C) 2004 University of Amsterdam. All rights reserved.

Now:
PART OF Garp3. SEE COPYRIGHT NOTICE.

Code based on the global part of the original main.pl file.
*******************************************************************************************************/

%loaded into module (namespace) visualize

/* global settings 
 *
 */

garp_version(2.0).    % latest version, with changed transition rules
% garp_version(1.72).  % garp 1-7-2

currentX(1,0).  % first state should be positioned at X-coordinate 0
currentY(1,0).  % first state should be positioned at Y-coordinate 0
grid_block_size(30, 20).
%state_size(75,75).
state_size(40,40).
relation_space(25,25).
extra_space(20,20,50,5). % Left, Right, Top, Bottom
text_margin(10,10).
text_margin_quantity(11,4).
graph_margin(15, 5). % 10,7
graph_point_margin(2, 1).
text_margin_spaces(0,3). % spaces on the left and right
entity_var_nr(_Type, 0). % for naming uninstantiated entity instances 
default_tab(10).
arc_label_size_math(15).
arc_label_size_attr(40).
max_window_size(400, 400). % was 500 x 500

%gp3: next 3 moved to @app?setting, so defaults are in main/appsettings.pl
%default_settings(input_state, on).
%default_settings(termination_nodes, off).
%gp3: added this setting:
%default_settings(dependencies_internal_quantity_naming,@off).
%gp3: the next 3 are still set, but not user changeable
default_settings(transitive_input, on).
default_settings(transitive_reduction, on).
default_settings(aggregate_orderings, off).
% The colours of the dependencies with status \== effective
% should be black by default, 
% but adjustable in preferences. To do, AB, 11/7/2005
%gp3 0.2: these settings are no longer used. The elements are allways shown
%settings(inactive_dependencies, @on, black). % was grey, AB 11/7/2005
%settings(submissive_dependencies, @on, black). % was blue, AB 11/7/2005
%settings(balanced_dependencies, @on, black). % was orange, AB 11/7/2005
settings(quantities_with_low_degree, on, black). 
settings(event_types, [structure_events, val_der_events, inequality_events]). 
% settings(generate_questions_after_every_change, false). 
% settings(generate_questions_after_every_change, true). 
settings(qmenu, _M, perspective, _Var).
settings(qmenu, _M, answer_method, _Var).
settings(qmenu, _M, screen_visibility, _Var).
settings(qmenu, _M, behaviour, _Var).
settings(qmenu, _M, subject_concepts, _VarList).
settings(max_question_string_length, 110). % was 100 or 127
settings(question_listbox_height, 13). % For larger screens, 18 or 25
settings(question_quantity_listbox_size, 20, 12). % was 25/35 x 13
settings(events_listbox_size(110, 20)). % was @default x 25
settings(dependencies_bottom_buttons_size, large, 105, 25). % was 115 x 25 
settings(dependencies_bottom_buttons_size, small, 70, 25).  % was 80 x 25
settings(mf_filter_type, quantities).
settings(dependencies_view, on([entities, quantities, dependencies]), 
          off([attributes, derivatives, values, q_spaces, 'All', 'None'])).
          
          
settings(dependencies_view_menu, on([entities, quantities]), 
          off([attributes, derivatives, values, q_spaces])). %gp3: we removed All & None, no longer boolean element. We would like to remove this setting all together, but well.

% settings(language, nederlands).
% settings(language, english).
% settings(language, portugese).
type_of_file_first_opened(none).



assert_system_specific_details:-
      get(@pce, window_system, 'X'), % unix 
      % size of state-nodes 
      retractall(state_node_size(_)),
      asserta(state_node_size(32)),

      % size of labels on dependencies
      retractall(arc_label_size_causal(_)), 
      asserta(arc_label_size_causal(20)).

      %gp3 removed colour_code

assert_system_specific_details:-
      get(@pce, window_system, 'windows'), % pc windows
      % size of state-nodes 
      retractall(state_node_size(_)),
      asserta(state_node_size(34)),

      % size of labels on dependencies
      retractall(arc_label_size_causal(_)), 
      asserta(arc_label_size_causal(24)).

     %gp3 removed colour_code         % black


reset_settings:-
	%gp3 0.11: rewrite to make the code more general
	%rest all settings of which we know a default to this default

	default_settings(Setting,Value),
	retractall(settings(Setting,_)),
	assert(settings(Setting,Value)),
	fail
	;
	true.


:- assert_system_specific_details.
:- reset_settings.
