/*****************************************************************************************************
Garp3 0.1 Based on main source of visigarp (main.pl). This file is split in parts and edited.
All visiualize source based on visigarp 2.05, designed and implemented by Anders Bouwer 18/08/1999
Original copyright: (C) 2004 University of Amsterdam. All rights reserved.

Now:
PART OF Garp3. SEE COPYRIGHT NOTICE.
*******************************************************************************************************/

%loaded into module (namespace) visualize

%gp3 0.3.13: we changed the creation of dialogs for showing the data. Now we have a new class, listBrowserDialog, which we use to show the data. This dialog does nothing at all except creating the elements, so filling and handling is still done through the legacy visigarp code below

show_transitions(F) :-
	get_selected_states_or_path(F, InputStateList),
	delete(InputStateList, 0, StateList),
	StateList \== [],!,
	show_transitions(F, StateList).

% If the above fails, because no states were selected, do nothing. 
%
show_transitions(_F).

show_transitions(f, statelist) :-
        debug(simulate(table_view), 'show_transitions for ~w', [statelist]), 
        get(f, textdialog_member, d2),

	sel = f<<-selectmode, %gp3
	% if radio button is set to 'path'
	sel == 'path', 
	get(d2?selected_path_member, selection, str), 

	/* anders: does this look correct? */
	% convert to a valid statelist        
        str_to_statelist(str, statelist), 

	% check whether it is a valid path 
	catch(term_to_atom(statelist, str), _, fail), 
	is_list(statelist),
        
	first(a, statelist), 
        last(statelist, b), 
	(  % is it a path?
           a \== b, 
           path([a], [b], state_graph, statelist, _)
         ; 
           % or is it a cyclic path?
           a == b, 
	   cyclic_path(a, b, state_graph, statelist, _)
        ),
	show_transitions_path(f, str),!.


% radio button was set to 'states', or no path was found
%
show_transitions(F, _StateList) :-
        get(F, textdialog_member, D2),

	get(D2?selected_states_member, selection, Str),

	% convert to a valid StateList        
        str_to_statelist(Str, StateList), 

	StateList \== [],!,
	show_transitions_states(F, Str).

% if none of the above succeeds, do nothing
%
show_transitions(_F, _StateList).

	       
show_transitions_path(F, StateListStr):-
        debug(simulate(table_view), 'show_transitions_path for ~w', [StateListStr]), 
	term_to_atom(StateList, StateListStr), 

		%gp3 0.3.13 changed this: we create the dialog through a class, then get the list into B and use old code
	%to do the rest (except for resizing: we do not resize the list anymore).
	%to do this, we set the legacy messages in the detailsMessage variable of the new dialog
	
	D *= listBrowserDialog(string('Transition history for path: %s - Simulate', StateListStr),
		70,10,@on,'Sim_TransitionHistoryViewForPath'),
	D->>detailsMessage(->>(@prolog,show_transition_details1,@arg1)), %this is the old visigarp call, except for @arg1 used directly
	
	B = D<<-list,
	send(B, tab_stops, vector(60)),
		
	% if StateList is a selected Path
	forall(pair_member(From/To, StateList), 
		show_transition1(B, From, To)
	),

	send(D, open),
	send(D, transient_for, F). %gp3: moved to after open, so wm will not know, but ?transient_for does


show_transitions_states(F, StateListStr):-
	% if StateList is a selection of states
	%why is this a different call with totally different implementation? [gp3: this comment is not ours, it is old]
	
	%gp3 0.3.13 changed this: we create the dialog through a class, then get the list into B and use old code
	%to do the rest (except for resizing: we do not resize the list anymore).
	%to do this, we set the legacy messages in the detailsMessage variable of the new dialog

	term_to_atom(StateList, StateListStr), 

	D *= listBrowserDialog(string('Transitions for selected states: %s - Simulate', StateListStr),
			70,10,@on,'Sim_TransitionHistoryForSelectedStates'),
	D->>detailsMessage(->>(@prolog,show_transition_t_details,@arg1)), %this is the old visigarp call, except for @arg1 used directly
	
	B = D<<-list,
	
	send(B, tab_stops, vector(60)),

	forall(member(FromNr, StateList), 
	    (
		findall(FromNr/ToNr/ToStates/Status, 
			(
			engine:state_to(FromNr, ToList), 
			nth1(ToNr, ToList, to(_, _, _, to_state(ToStates), 
								Status))
			), 
			AllTo), 
		engine:transition_names(AllTo, Transitions), 
		forall(member(T, Transitions),
			show_transition_t(B, T)
		)
	    )
	),

	send(D, open),
	send(D, transient_for, F). %gp3: moved to after open, so wm will not know, but ?transient_for does


show_transition_t(B, TransitionName):-
        %gp3 0.3.13 kept all code about filling the list the way it was, but removed all logic about
        %resizing and setting messages for the list. That is now done in the listBrowserDialog class

	find_transition_data(TransitionName, FromState, InternalCausesList, 
				_ConditionsList, _ResultsList, 
				_ToStateList, _Status), 
	%gp3: rewrite the CausesList to design-time names
	show_transition_rewrite_list(FromState,InternalCausesList,CausesList),
	show_list_new(B, TransitionName, CausesList, @default, _MaxLength).

%


show_transition_t_details(TransitionName):-
	%gp3: this is the code for termination details (state mode, not path mode)
	%this is almost the same as show_transition_details (which is for states)
	
	%gp3 0.3.13 changed this: we create the dialog through a class, then get the list into B and use old code
	%to do the rest (except for resizing: we do not resize the list anymore).
	
	D *= listBrowserDialog(string('Details about transition %s - Simulate', TransitionName),
			70,20, @off, 'Sim_DetailsAboutTransition'),
	B = D<<-list,
	
	send(B, tab_stops, vector(75, 150, 225, 300, 375, 450, 525, 600)),


	find_transition_data(TransitionName, FromState, CausesList, 
				ConditionsList, ResultsList, 
				ToStateList, Status), 	

	%gp3: we need to get a list of the kind "immgrated3 = Grass population: immigrated"
	B->>style(internal,style(colour := grey)),
	%gp3 0.3.13: disabled sizing logic for the code below. List opens with standard size, but dialog can be resized now
	show_transition_details_naminglist(FromState,CausesList,NamingList),
	show_list_new(B, internal_naming,NamingList,internal,_MaxLength0),
	%%
	show_list_new(B, cause, CausesList, @default, _MaxLength1), 
	show_list_new(B, conditions, ConditionsList, @default, _MaxLength2), 
	show_list_new(B, results, ResultsList, @default, _MaxLength3), 
	show_list_new(B, status, [Status], @default, _MaxLength4), 
	show_list_new(B, to_state, ToStateList, @default, _MaxLength5), 
 
   send(D, open),
    %gp3: moved this after open
	(   catch(get(@event?window, frame, Application), _, fail) 
	->  send(D, transient_for, Application) %gp3 moved this after open: wm wont know, but ?transient_for does
	;   true
	).


show_transition1(B, From, To):-
        % Show short description of state transition between From and To
        %gp3 0.3.13 kept all code about filling the list the way it was, but removed all logic about
        %resizing and setting messages for the list. That is now done in the listBrowserDialog class
        
	find_transition_details(From, To,  
              InternalCausesList, _ConditionsList, _ResultsList, _Status), 
	get(string('%s -> %s', From, To), value, String),
	show_transition_rewrite_list(From,InternalCausesList,CausesList), %gp3: create design names
	show_list(B, String, CausesList, _MaxLength).



show_transition(TransitionName):-
	show_transition_t_details(TransitionName).


show_transition(From, To):-
     % Show short description of state transition between From and To
        
       %gp3 0.3.13 changed this: we create the dialog through a class, then get the list into B and use old code
	%to do the rest (except for resizing: we do not resize the list anymore).
	%to do this, we set the legacy messages in the detailsMessage variable of the new dialog

	D *= listBrowserDialog(string('Transition between state %s and %s - Simulate', From, To),
		70, 10, @on, 'Sim_TransitionBetweenTwoStates'),
	D->>detailsMessage(->>(@prolog,show_transition_details,From,To)), %this is the old visigarp call
	B = D<<-list,
	
	send(B, tab_stops, vector(60)),

	find_transition_details(From, To,  
              InternalCausesList, _ConditionsList, _ResultsList, _Status), 

	
	get(string('%s -> %s', From, To), value, String),
	show_transition_rewrite_list(From,InternalCausesList,CausesList), %gp3: create design names
	show_list(B, String, CausesList, _MaxLength), 

	%old code
	%but we made sure the window is opened before setting transient_for
	

	(   catch(get(@event?window, frame, TF), _, fail)
	;   TF = @nil
	),
	send(D, open),
	D->>transient_for(TF).

show_transition_details1(Str):-
	term_to_atom(Term, Str), 
 	Term =.. [_Operator, X, Y],
	show_transition_details(X, Y).


 
show_transition_details(From, To):-
        % Show details about state transition between From and To
        
%gp3 0.3.13 changed this: we create the dialog through a class, then get the list into B and use old code
	%to do the rest (except for resizing: we do not resize the list anymore).
	
	D *= listBrowserDialog(string('Details about transition between state %s and %s - Simulate',From, To),
			70,20, @off, 'Sim_DetailsAboutTransition'),
	B = D<<-list,
	
	send(B, tab_stops, vector(75, 150, 225, 300, 375, 450, 525, 600)),
	
	find_transition_details(From, To,  
              CausesList, ConditionsList, ResultsList, Status), 
	%gp3: we need to get a list of the kind "immgrated3 = Grass population: immigrated"
	B->>style(internal,style(colour := grey)),
	%gp3 0.3.13: disabled sizing logic for the code below. List opens with standard size, but dialog can be resized now

	show_transition_details_naminglist(From,CausesList,NamingList),
	show_list_new(B, internal_naming,NamingList,internal,_MaxLength0),
	%%
	show_list_new(B, cause, CausesList, @default, _MaxLength1), 
	show_list_new(B, conditions, ConditionsList, @default, _MaxLength2), 
	show_list_new(B, results, ResultsList, @default, _MaxLength3), 
	show_list_new(B, status, [Status], @default, _MaxLength4), 

    send(D, open),
	(   catch(get(@event?window, frame, Application), _, fail) 
	->  send(D, transient_for, Application) %gp3 moved this after open: wm wont know, but ?transient_for does
	;   true
	).


        %show_transition_rewrite_list(State,Internal,External)
%gp3 0.1: rewrite the list containing internal descriptions of causes to external descriptions
%so to_interval_above(immigrated3) will become: to_interval_above(Grass population: Immigrated)

show_transition_rewrite_list(_,[],[]).
show_transition_rewrite_list(FromState,[Internal1|InternalRest],[External1|ExternalRest]):-
	Internal1 =.. [Cause|Arguments],
	%we need to get the quantity information for each argument, we get them from the FromState
	Result *= string('%s(',Cause),
	Comma *= string(''),
	forall(
		member(Quantity,Arguments),
		(
			quantityDescription(FromState,Quantity,Description),
			Result->>append(string('%s%s',Comma,Description)),
			Comma->>value(', ')
		)
	),
	Result->>append(')'),
	External1 = Result<<-value, %make it a name again
	show_transition_rewrite_list(FromState,InternalRest,ExternalRest).

%%
%show_transition_details_naming_list(State,CausesList,NamingList)
%gp3 0.1: create a list containing members of the kind 'immigrated3 = Grass Population: immigrated'
%for use in transition details

show_transition_details_naminglist(State,CausesList,NamingList):-
	findall(
		InternalName,
		(
			member(Cause, CausesList),
			Cause =.. [_|Arguments],
			member(InternalName,Arguments)
		),
		AllInternalNames
	),
	list_to_set(AllInternalNames,InternalNames),
	sort(InternalNames, Sorted),
	Namings *= chain,
	forall(
		member(Quantity,Sorted),
		(
			quantityDescription(State,Quantity,Description),
			S *= string('%s = %s',Quantity,Description),
			Namings->>append(S?value)
		)
	),
	chain_list(Namings,NamingList). %well, just make it prolog again, only to make legacy code happy
