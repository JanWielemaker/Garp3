/*
appsettings.pl

Default settings file. Used by application when @app<<-setting needs a value
it cannot find in @app<<-settings.
When closing, @app?settings will be written to disk, so changes (using @app->>setting)
will be saved.

18-8-2005 Jelmer Jellema (Spin in het Web)
Part of GARP3
See copyright notice

You might need to restart garp when you change this (though 'make' might do the trick).
*/


appsetting(allwayskeepthisone, please).

appsetting(input_state, @on).
appsetting(termination_nodes, @off).
%gp3: added this setting:
appsetting(dependencies_internal_quantity_naming,@off).

% Sketch environment, AB Sept 2006 - Nov 2006
appsetting(import_quantities_from_causal_model_to_stgraph, @off).
appsetting(import_concepts_from_concept_map_to_structure_model, entity).
appsetting(import_concepts_from_concept_map_to_process_definition, entity).
appsetting(import_concepts_from_concept_map_to_agent_definition, entity).
appsetting(import_concepts_from_concept_map_to_scenario_definition, entity).

% Added for selection of end states in State-transition graph, AB, March 2007
appsetting(select_end_states, @off).
appsetting(select_cyclic_paths, @on).
appsetting(select_full_paths, @on).

% Added for different view types for the simulation: graph or table view
appsetting(simulation_view_type, graph).

% Added for displaying 2nd order derivatives
appsetting(second_order_derivatives, @on). 
