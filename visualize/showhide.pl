/*****************************************************************************************************
Garp3 0.1 Based on main source of visigarp (main.pl). This file is split in parts and edited.
All visiualize source based on visigarp 2.05, designed and implemented by Anders Bouwer 18/08/1999
Original copyright: (C) 2004 University of Amsterdam. All rights reserved.

Now:
PART OF Garp3. SEE COPYRIGHT NOTICE.
*******************************************************************************************************/

%loaded into module (namespace) visualize
show_hide(F, q_spaces, OnOrOff):-
	get(F, member, picture, P), 
	show_hide_qspace(P, OnOrOff, _QSNode), 
	show_hide(F, q_spaces, OnOrOff).

%%%% show/hide quantities too 
% together with quantity spaces, also show/hide par_rel_group nodes 
% and if @on, also show quantity nodes
show_hide(F, q_spaces, OnOrOff):-
	show_hide(F, internal_par_rels, OnOrOff),
        (OnOrOff == @on
        -> 
	   % show quantities too
	   show_hide(F, quantities, OnOrOff)
	 ;
	   true
        ).



show_hide(F, internal_par_rels, OnOrOff):-
	get(F, member, picture, P), 
	show_hide_internal_par_rel(P, OnOrOff, _ParRelGroupNode), !,
	show_hide(F, internal_par_rels, OnOrOff).

% make sure it succeeds
show_hide(_F, internal_par_rels, _OnOrOff).



show_hide(F, values, OnOrOff):-
        (OnOrOff == @on
        -> 
%%%% begin changes  - AB 23/09/2005
	   % show quantities and quantity spaces too
	   show_hide(F, quantities, OnOrOff),
           show_hide(F, q_spaces, OnOrOff)
%%%% end of changes - AB 23/09/2005
	 ;
	   true
        ),
	get(F, state_nr, N),
	findall(QuantityName/QV/Der, 
		(
			find_quantity_value(N, QuantityName, _RealVal, QualVal, Der),
			% strip the parameter name of the valuename, 
			% if applicable
			term_to_atom(QV_Term, QualVal),
			strip_atomize(QV_Term, QV)
		),
		QList), 
	forall(member(QuantityName/QV/Der, QList), 
		show_hide_value(F, OnOrOff, QuantityName, QV, Der)
	).



show_hide(F, derivatives, OnOrOff):-
	get(F, member, picture, P), 
	get(P, find, @default, 
		and(@arg1?name == derivative, 
		    @arg1?displayed \== OnOrOff
		), 
		DerGroupNode),	
	send(DerGroupNode, displayed, OnOrOff),
	show_hide(F, derivatives, OnOrOff).

% make sure it succeeds
% show quantities too, if necessary
show_hide(F, derivatives, OnOrOff):-
        (OnOrOff == @on
        -> 
	   % show quantities too
	   show_hide(F, quantities, OnOrOff)
	 ;
	   true
        ).

show_hide(F, quantities, OnOrOff):-
        get_quantity_nodes(F, QuantityNodes), 
%%%% Begin changes  - AB 20/09/2005 
%%%% replaced two lines of code to deal with case of empty list
%%%%
        forall(member(QN, QuantityNodes), 
	       show_hide1(QN, quantity, OnOrOff)),
%%%%
%%%%	old version:
%%%%
%%%%	send(QuantityNodes, for_all, 
%%%%		message(@prolog, show_hide1, @arg1, quantity, OnOrOff)),
%%%%
%%%% End of changes - AB 20/09/2005 
	(OnOrOff == @off
	->
		% if quantities are turned off,
		% turn off also quantity spaces, values and derivatives
		show_hide(F, q_spaces, OnOrOff),
		show_hide(F, values, OnOrOff),
		show_hide(F, derivatives, OnOrOff)
		% setting of radiobutton is handled by select_extra
	;
		true
	).
			


%%%% begin changes  - AB 25/09/2005
show_hide(F, entities, OnOrOff):-
	get(F, member, picture, P), 
	get(P, graphicals, GrsChain),
 	% first, find all graphicals of the right type 
	graphical_type(entities, _ArgName, GrType),
	get(GrsChain, find_all, 
		message(@arg1, instance_of, GrType), Grs),
 	% then check whether they are really entities	
        % discard assumptions 
        get(F, state_nr, N), 
	get(Grs, find_all, 
		and(@arg1?type == 'entity', 
                    message(@prolog, is_no_assumption, N, @arg1?name)
                ), Entities),
			
	send(Entities, for_all, 
		message(@prolog, show_hide1, @arg1, entity, OnOrOff)),
	(OnOrOff == @off
	->
		% if entities are turned off,
		% turn off attributes as well
		show_hide(F, attributes, OnOrOff)
		% setting of radiobutton is handled by select_extra
	;
		true
	).
%%%% end of changes - AB 25/09/2005


show_hide(F, assumptions, OnOrOff):-
	get(F, member, picture, P), 
        get(F, state_nr, N), 
	get(P, graphicals, GrsChain),
 	% first, find all graphicals of the right type 
	graphical_type(entities, _ArgName, GrType),
	get(GrsChain, find_all, 
		message(@arg1, instance_of, GrType), Grs),
 	% then check whether they are really entities	
	get(Grs, find_all, 
		(@arg1?type == 'entity'), Entities),
        % then check whether they are assumptions


	(catch(OnOrOff == @off, _, fail)
        ->  
	  send(Entities, for_some, 
	    and(message(@prolog, is_assumption, N, @arg1?name), 
                % quick bug-fix for the experiment of nov 2001:
                message(@arg1, destroy)
	       )
	      )
	;
	  send(Entities, for_some, 
	    and(message(@prolog, is_assumption, N, @arg1?name), 
                message(@prolog, show_hide1, @arg1, entity, @on)
	       )
	      )
	).

	show_hide(F, e_without_q, OnOrOff):-
	get(F, member, picture, P), 
        get(F, state_nr, N), 
	get(P, graphicals, GrsChain),
 	% first, find all graphicals of the right type 
	graphical_type(entities, _ArgName, GrType),
	get(GrsChain, find_all, 
		message(@arg1, instance_of, GrType), Grs),
 	% then check whether they are really entities	
	get(Grs, find_all, 
		(@arg1?type == 'entity'), Entities),
        % then check whether they have any subfigures 



	(catch(OnOrOff == @off, _, fail)
        ->  
	  send(Entities, for_some, 
	    and(message(@prolog, without_quantities, N, @arg1)
                % quick bug-fix for the experiment of nov 2001:
                , message(@arg1, destroy)
	       )
	      )
	;
	  send(Entities, for_some, 
	    and(message(@prolog, without_quantities, N, @arg1), 
                message(@prolog, show_hide1, @arg1, entity, @on)
	       )
	      )
	).



show_hide(F, attributes, OnOrOff):-
	% if on, 
	% turn on entities as well
	% also set entities radiobutton to on!
        (OnOrOff == @on
	->
	    show_hide(F, entities, @on)
	;
	    true
	), 

	get(F, member, picture, P), 
	get(P, graphicals, GrsChain),
	% first, find all attributes which are inside entities
	graphical_type(entities, _ArgName, GrType),
	get(GrsChain, find_all, 
		message(@arg1, instance_of, GrType), Grs),

	get(Grs, find_all, 
		(@arg1?type == 'entity'), Entities),

        % check whether they are displayed or not
	get(Entities, find_all, 
		(@arg1?text_member?displayed == @on), VisibleEntities),

	send(VisibleEntities, for_all, 
                message(@arg1, show_hide_attributes, OnOrOff)),

	% second, find all attributes which are immediate members of P,
	% (the connections) 
	get(GrsChain, find_all, 
		and(
		 message(@arg1, instance_of, connection), 
		 message(@arg1, has_get_method, type)
		), Connections),
        % find all connections with nodes at both ends 
        % displayed

	get(Connections, find_all, 
		and(@arg1?type == 'attribute', 
                    @arg1?from?text_member?displayed == @on,
                    @arg1?to?text_member?displayed == @on
                ), ConAttrs),

	send(ConAttrs, for_all, message(@arg1, displayed, OnOrOff)),
	send(ConAttrs, for_all, message(@arg1?tag, displayed, OnOrOff)),
        

	send(ConAttrs, for_all, 
                message(@prolog, show_hide_connection, @arg1, OnOrOff)),
	send(P, redraw),
	% Why doesn't this work? Probably because it's automatically redrawn
	send(ConAttrs, for_all, message(@arg1, displayed, OnOrOff)),
	send(ConAttrs, for_all, message(@arg1?tag, displayed, OnOrOff)),
	send(P, redraw),
        send(P, flush).

%%%END SHOWHIDE



% get_quantity_nodes(F, QuantityNodes)
%
% returns all quantity nodes in the dependencies view of frame F 
% 
get_quantity_nodes(F, QuantityNodes):-
        get(F, state_nr, N), 
	findall(Instance, 
		(	engine:state(N, SMD), 
			engine:smd_slots(SMD, _, _, ParList, _, _, _, _), 
			member(SomePar, ParList), 
			engine:parameter(SomePar, _, _, _, Instance, _, _)
			% engine:parameter_visible_item(SomePar, ParName)
		),
		Parameters), 
        findall(QNode, 		
	  (
	   member(QuantityName, Parameters), 
	   get(F, node, QuantityName, quantity, N, QNode)
	  ), QuantityNodes
        ).




is_no_assumption(N, Name):-
        not(is_assumption(N, Name)).

% is_assumption(N, Name) 
% 
% succeeds if entity Name in state N is an instance  
% of type assumption, or any of its subtypes.
% 
is_assumption(N, Name):-
        % this cut is necessary to prevent Name from 
        % being instantiated as a variable - then it 
        % would satisfy any InstanceType, which is not 
        % the intention 
        find_instance(N, Name, InstanceType),!,
        is_assumption_word(AssumptionInAnyLanguage),
        isa_predecessor(InstanceType, AssumptionInAnyLanguage, _NrLinks).

/*
sihw 20050425. The translator is not used, so is_assumption_word only succeeds with 'assumption'
*/

is_assumption_word(assumption).
/*
% is_assumption_word(AssumptionWord) 
% 
% succeeds for 'assumption', 'aanname', and translations
% in other languages, if present 
% 
is_assumption_word(AssumptionWord):-
        domain_terms( AssumptionInAllLanguages ), 
        member(english(assumption), AssumptionInAllLanguages),
        member(AssumptionTerm, AssumptionInAllLanguages), 
        AssumptionTerm =.. [_Language, AssumptionWord]

*/

% without_quantities(N, EntityNode) 
% 
% succeeds if entity EntityNode in state N has no 
% quantity subfigures
% 
without_quantities(N, EntityNode):-
        % if it's an assumption, do nothing
        get(EntityNode, name, EntityName),         
	is_assumption(N, EntityName), 
        !, fail.

without_quantities(_N, EntityNode):-
        % if it's not an assumption, erase
        % get(EntityNode, name, EntityName),         
	graphical_type(quantities, _ArgName, GrType),
	get(EntityNode, graphicals, GrsChain),
	get(GrsChain, find_all, 
		message(@arg1, instance_of, GrType), Grs),
	(catch(get(Grs, find, 
		(@arg1?type == 'quantity'), _Quantity), _, fail)
        -> 
          !, fail
        ;
          true
        ).



show_hide_connection(Con, OnOrOff):-
        % get(Con, name, Name), 
	(OnOrOff == @on, % !, why did I use a ! here?
		% if on, display attributes in black (why necessarily black?!)
		send(Con, colour, black)
	;
		% otherwise, colour white and hide (= put in background)
		send(Con, hide),
		send(Con, colour, white),
		send(Con?tag, hide),
		send(Con, hide),
		send(Con, displayed, @off)
	).


% show_hide1(Gr, Type, OnOrOff):-
%
%	
%%%% begin changes  - AB 29/09/2005
show_hide1(Gr, entity, OnOrOff):-
        % this doesn't work because it would also erase all quantities
        % send(Gr, displayed, OnOrOff).
	send(Gr?box_member, displayed, OnOrOff),
	send(Gr?text_member, displayed, OnOrOff),

        % turn off its relations and attributes if necessary
        (OnOrOff == @off
        ->
           % turn off its relations
           hide_attr_relation_connections(Gr),
           % turn off its attributes
           send(Gr, show_hide_attributes, @off)           
        ; 
            true
        ), 
	send(@prolog, show_hide_border, Gr, OnOrOff).
%%%% end of changes - AB 29/09/2005

	

show_hide1(Gr, attribute, OnOrOff):-
	send(Gr, displayed, OnOrOff).


show_hide1(Gr, quantity, OnOrOff):-
	send(Gr, displayed, OnOrOff).


%%%% begin of changes - AB 29/09/2005
% hide_attr_relation_connections(Gr)
% 
% turns off the attribute relation connections of graphical Gr
% 
hide_attr_relation_connections(Gr):-
           % if Gr has connections of type attribute, hide them
	   (get(Gr, connections, Conn)
            -> 
	       get(Conn, find_all, 
		  and(
		    message(@arg1, has_get_method, type),
		    @arg1?type == 'attribute'
                  ), ConAttrs),
               send(ConAttrs, for_all, 
                  message(@prolog, show_hide_connection, @arg1, @off)
               )
           ;
               true
           ).
%%%% end of changes - AB 29/09/2005


show_hide_border(Gr, @on):-
   	send(Gr, pen, 1).

show_hide_border(Gr, @off):-
   	send(Gr, pen, 0).



show_hide_value(F, OnOrOff, QuantityName, QualVal, _Der):-
	get(F, state_nr, N),
	get(F, node, QuantityName, quantity, N, QNode),	
	% find quantity space node
	get(QNode, member, quantity_space, QSNode), 
	term_to_atom(QualValTerm, QualVal),
	strip_atomize(QualValTerm, QVal),
	get(QSNode, member, QVal, ValNode),
	% if value is turned on, turn on q-space first
	(OnOrOff == @on, 
		send(QSNode, displayed, @on),
		send(ValNode, colour, red),
		send(ValNode, displayed, OnOrOff)
	;
		send(ValNode, colour, black),
		true
	).


% what to show when the value is unknown, or non-existent?!!
% should the q_space be shown nevertheless?
%
show_hide_value(_F, _OnOrOff, _QuantityName, QualVal, _Der):-
	term_to_atom(QualValTerm, QualVal),
	strip_atomize(QualValTerm, _QVal).



show_hide_qspace(P, OnOrOff, QSpaceNode):-
	get(P, find, @default, 
		and(@arg1?name == quantity_space, 
		    @arg1?displayed \== OnOrOff
		), 
		QSpaceNode),	
	send(QSpaceNode, displayed, OnOrOff).


show_hide_internal_par_rel(P, OnOrOff, ParRelGroupNode):-
	get(P, find, @default, 
		and(@arg1?name == par_relations, 
		    @arg1?displayed \== OnOrOff
		), 
		ParRelGroupNode),	
	send(ParRelGroupNode, displayed, OnOrOff).



	
% right now, only this might work
graphical_type(entities, entity, graph_node).

graphical_type(attributes, attribute, connection).
graphical_type(quantities, quantity, graph_node).

% this doesn't work yet!!
graphical_type(attributes, attribute, graph_node).
graphical_type(attributes, attribute, figure).
graphical_type(values, value, graph_node).
graphical_type(derivatives, derivative, graph_node).
	

