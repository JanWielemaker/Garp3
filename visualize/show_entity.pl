/*****************************************************************************************************
Garp3 0.1 Based on main source of visigarp (main.pl). This file is split in parts and edited.
All visiualize source based on visigarp 2.05, designed and implemented by Anders Bouwer 18/08/1999
Original copyright: (C) 2004 University of Amsterdam. All rights reserved.

Now:
PART OF Garp3. SEE COPYRIGHT NOTICE.
*******************************************************************************************************/

%show e-r structure. Uses causal_graph_viewe class
%(code a bit scattered)

%loaded into module (namespace) visualize
      
show_system_elements(F, N):-

    % Show system elements for state N
	display_entities(F, N),
	% like instance(container1, container)

	display_entity_attributes(F, N), 
	% atttribute relations between entities, like 
	% has_attribute(container1, contains, liquid1)

	display_other_attributes(F, N),
	% like has_attribute(fluid_path1, status, aligned)

	% connect all entities invisibly to ensure that the layout 
	% mechanism keeps them together. This is a bad solution! A 
	% better way would be add a minimum of connections, only to 
	% those entities which are not connected yet.
	%
	send(F, connect_entities),
	send(F, layout),!.



display_entities(F, N):-
	%changed in gp3 0.1
	%if we are not in legacy mode we find all the instances in this state
	%and try to get back to the input system to get their design-time names
	%in legacy mode we use the old code
		
	\+ get(@model, modelState, legacy), !, %otherwise, fall through % JL
	%get the is
	IS = @app<<-currentScenario, %N could be 0, which does not give any SMD, so this is the way to go
								  %fails when only an input state (not possible?)
	(N < 1, 
		reset_entity_var_nrs
	;
		true
	),
	
	forall(visualize:find_instance(N,Instance,Type),
		(
			%gp3 0.3.16: skip all names starting with 'garp_' (internal)
			unless
				Instance->>prefix('garp_')
			do
			(
				%gp3 1.4: new way of creating tooltip: we use relevant_comments for a internal garp term
				%(see find_in_design.pl). find_instance_details remains unchanged: might be in future we use this tooltip feature?? (otherwise, just
				%remove tooltip generation in find_instance_details
				find_instance_details(F,N, IS,Instance,Type,InstanceName, DesignType, _Tooltip),
				relevant_comments(instance(Instance,_),N,Tooltip), %any instance fact using this instance name will do, because of isa tree!
				create_entity_name(F,Instance,Type,InternalName), %also create the internal (visigarp) name
																%to make sure no code breaks
				Node = F<<-node(InternalName,entity,N,InstanceName, DesignType),
				Node->>tooltip(Tooltip,model) %gp3 0.2: add tooltip
			)
		)
	).


display_entities(F,N):-
	%old code, kept for legacy mode
 
	      Term = visualize:find_instance(N, Entity, Type),
	% if displaying for the input model, which may contain 
	% uninstantiated entities, reset the entity variable nrs
	%gp3 0.2 We try to add a tooltip as well
	
	(N < 1, 
		reset_entity_var_nrs
	;
		true
	),
        forall(user:Term, 
		(
			create_entity_name(F, Entity, Type, EntityName),
			get(F, node, EntityName, entity, N, Node),
			Node->>tooltip(string('%s\nInstance of %s', EntityName,Type),model)
		)
	).

find_instance_details(_F,N, IS,InternalInstance,_Type,InstanceName, DesignType, Tooltip):-
	%gp3 0.1
	%see if we can find the right name for this instance from the export
	%gp3 0.2 added designtype and tooltip text to show as well
	
	\+ get(@model, modelState, legacy), %fall through otherwise % JL

	%get the designtime object
	if
		Instance = @app<<-findExportedObject(in,InternalInstance,IS)
	then
	(
		InstanceName = Instance<<-name,
		InstanceRemarks = Instance<<-remarks,
		Def = Instance<<-definition,
		Generated = 0
	)
	else
	(
		%maybe the instance was generated?
		InstanceName = InternalInstance,
		InstanceRemarks *= string,
		find_instance(N, InternalInstance, InstanceType), %get the entityname used by engine
		Def = @app<<-findExportedObject(en,InstanceType), %and get the design type object
		Generated = 1
	),
	if
		Def->>instance_of(assumption)
	then
		Tooltip *= string('Assumption')
	else
		Tooltip *= string('%s\n%s instance of %s', InstanceName,Def?class_name?capitalise,Def?name),
	if
		Generated = 1
	then
		Tooltip->>ensure_nl('(generated)'),
	unless
		0 = InstanceRemarks<<-size
	do
		Tooltip->>ensure_nl(InstanceRemarks),
	%add designtype
	DesignType = Def<<-class_name.
%
find_instance_details(F, _N,_IS,Instance,Type,InstanceName,entity,InstanceName):-
	%not found? fall back to legacy mode
	%and make the designType allways an entity
	create_entity_name(F,Instance,Type,InstanceName).
	

	
% LEGACY:
% create a name for the entity
% default: the entity instance name
%
create_entity_name(_F, Entity, _Type, EntityName):-
	nonvar(Entity),
	atomize(Entity, EntityName).

% in case of uninstantiated variables, type name + a number + ?
% every type of entity has its own number
create_entity_name(_F, Entity, Type, EntityName):-
	var(Entity),
	update_entity_var_nr(Type),
	entity_var_nr(Type, X),
	get(string('%s%s?', Type, X), value, EntityName).


% check the name for the entity - no update of X is done
%
% in case of uninstantiated variables, type name + a number + ?
%
check_entity_name(_F, _Entity, Type, EntityName):-
	% does EntityName contain a '?'?
	sub_string(EntityName, _Start, _L, _RL, '?'), !,
	entity_var_nr(Type, X),
	get(string('%s%s?', Type, X), value, EntityName).

% in case it does not contain a '?', it's ok.
%
check_entity_name(_F, _Entity, _Type, _EntityName).


% Increase entity type var nr to ensure unique entity names
% Don't retract the previous ones, so they can be found too
%
update_entity_var_nr(Type):-
	entity_var_nr(Type, X),
	NewX is X + 1,
	asserta(entity_var_nr(Type, NewX)).

% Reset entity type var nr to 0
%
reset_entity_var_nrs:-
	retractall(entity_var_nr(_T, _X)),
	asserta(entity_var_nr(_Type, 0)).
	

display_entity_attributes(F, N):-
		%gp3 0.1: we added a call to designName to see if we can find the design-time name of the configuration
		%gp3 0.1: special case: when the configurationname is has_assumption
		%and we are not in legacy mode, we rename to "assumption"
		%gp3 0.2: added tooltip
		%gp3 0.3.16: if any one of the two instances is named 'garp_', we skip the relation (when not in legacy)
		%because this means "internal and generated"
		
        Term = visualize:find_entity_attribute(N, EntityA, Rel, EntityB),
        if
        	get(@model, modelState, legacy) % JL
        then
        	NotInLegacy = false
        else
        	NotInLegacy = true,
        forall(user:Term, 
		(
			%skip if not in legacy and one of the entities is internal
			unless
				(NotInLegacy == true, (EntityA->>prefix('garp_') ; EntityB->>prefix('garp_')))
			do
			(
				get(F, node, EntityA, entity, N, _NA), %just checking, apparently
				get(F, node, EntityB, entity, N, _NB),	
				
				%gp3 1.4: we no longer do a lot of stuff about tooltips, but use the general find_in_design solution
				%old tooltip code commented out, so could be restored
				relevant_comments(has_attribute(EntityA,Rel,EntityB),N,Comments),
				if
				(
					Rel = has_assumption,
					NotInLegacy = true
				)
				then
				(
					DName = 'Assumption'
					%Tooltip *= string('Connected assumption')
				)
				else
				(
					DName = @app<<-designName(cd,Rel) %no context needed
					%Tooltip *= string('Configuration')
				),
				/*
				if
					Object = @app<<-findExportedObject(cd,Rel)
				then
				(
					unless
						0 = Object?remarks<<-size
					do
						Tooltip->>ensure_nl(Object?remarks) %we show the definition comments -> it is impossible to find the right comments (input system element? condition or given somewhere, possibly in a lot of places)
				),
				*/
				send(F, display_attr_relation, EntityA,
								EntityB, Rel, N,DName,Comments) %extra gp3 arguments for label and tooltip
			)
		)
	).
	 

display_other_attributes(F, N):-
		%gp3 0.1: these are nominal values of an instance (attribute definitions)
		%so we also will find the name here, and we need the object for the valuename as well (context)
		
		%gp3 0.3.16: currently no issues with internally generated names here, so no checks
		
        Term = visualize:find_other_attribute(N, EntityA, Rel, OtherB),
        forall(user:Term, 
		(
			(
				(
					AD = @app<<-findExportedObject(ad,Rel), %no context needed
					catch(ADName = AD<<-name,_,fail),
					Val = @app<<-designName(adv,OtherB,AD) %valuename in the context of the attribute
				)
			;
				(
					ADName = Rel,
					Val = OtherB
				)
			),
			%gp3 1.4: general tooltip, see find_in_design.pl
			%there was no old tooltip code
			relevant_comments(has_attribute(EntityA,Rel,OtherB),N,Comments),
			send(F, display_other_attr_relation, EntityA,
							Val, ADName, N,Comments)
		)
	).	
	 
