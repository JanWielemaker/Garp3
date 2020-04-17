/*****************************************************************************************************
Garp3 0.1
All visiualize source based on visigarp 2.05, designed and implemented by Anders Bouwer 18/08/1999
Original copyright: (C) 2004 University of Amsterdam. All rights reserved.

Now:
PART OF Garp3. SEE COPYRIGHT NOTICE.

Code based on the global part of the original main.pl file.
Code is old visigarp code unless gp3 is mentioned
*******************************************************************************************************/

%loaded into module (namespace) visualize

%TODO: nieuwe call start_visualize, met een inputsystem als (optioneel) argument
%terugvinden interne naam ervan en meteen die openen.
%als niet gegeven: gewoon input system lijst openen net als oude call visigarp.

/*
start_visualize: open the simulator on the current model.
gp3 0.1
start_visualize/1 does this with a scenario (an inputSystem object from the designer) allready chosen.
*/

start_visualize:-
	%is there is only 1 scenario? select it
	%we use the engine calls here, not @model, so it will also use in legacy mode
	
	findall(Name, engine:smd(input_system(Name),_,_,_,_,_), [Scenario]),!, %only one
	start_visualize(Scenario).
%
start_visualize:-
	%otherwise, return a list
	GV *= visigarp,
	GV->>open,
	new(_PartOfHyper, partof_hyper(@model, GV)), % JL
	choose_input_models(GV).
%
start_visualize(Scenario):-
	%open the Scenario, which is the internal engine name
	GV *= visigarp,
	GV->>open,
	new(_PartOfHyper, partof_hyper(@model, GV)), % JL
	load_input_model(GV,Scenario).

/*
start_full: open the simulator on the current model with a given scenario and do a full simulation
see garp3 (application)->>startFullSimulation
gp3 0.1
*/

start_full(Scenario):-
	%scenario again is the internal engine name
	GV *= visigarp,
	GV->>open,
	new(_PartOfHyper, partof_hyper(@model, GV)), % JL
	load_input_model(GV,Scenario),
	GV->>run_simulation.

%%
/*
reopen_visualize: open the simulator on the current engine state and repaint
gp3 0.1
*/
reopen_visualize:-
	GV *= visigarp,
	GV->>open,
	new(_PartOfHyper, partof_hyper(@model, GV)), % JL
	GV->>gen_simulation_view(@on),
	% GV->>layout,
	%gp 0.4.9: get the currently simulated scenario and set the title
	%accordingly
	GV->>setLabel.


%
%quantityDescription(State,QuantityName, Description: name/atom)
%gp3 0.1: Create a designtime description for engine-time QuantityName
%given a current State (and a current Scenario)
%
%given QuantityName immigrated3, Description might be: 'Grass population: Immigrated'

quantityDescription(State, QuantityName, Description):-
			visualize:find_quantity_entity(State,QuantityName,Pred,_,_,Entity),
			if
				QDef = @app<<-findExportedObject(qd,Pred)
			then
				PredName = QDef<<-name
			else
				PredName = Pred,
			%instance name:
			if
				Instance = @app<<-findExportedObject(in,Entity,@app?currentScenario)
			then
				InstanceName = Instance<<-name
			else
				InstanceName = Entity,
			Description = string('%s: %s',InstanceName,PredName)<<-value.

/* 
 * view(Frame, Filename) 
 * 
 * creates a new frame and displays the file named
 * Filename in it
 * gp3: we add @app as the application, to make sure we can control it
 */

view(Frame, Filename):-
	new(FV, frame(Filename, application := @app)),
	FV->>icon(@simulate_icon),
	send(FV, append, new(V, view(Filename))),
	send(V, load, Filename),
	send(new(D, dialog), below, V), 
	send(D, append,
	     button(close, message(FV, destroy))),
        send(FV, open),
        send(FV, transient_for, Frame). % stay on top & destroy with frame


/* 
 * visigarp 
 * 
 * opens the visigarp application. With this application 
 * you can view and consult prolog files, and generate and 
 * display a graph based on links specified in these files. 
 */
visigarp :-
	new(GV, visigarp),
	send(GV, open),
	new(_PartOfHyper, partof_hyper(@model, GV)), % JL
        choose_input_models(GV).


%gp3: choose_inpt_models uses the design-time scenario list in non-legacy mode
choose_input_models(_F):-
	\+ get(@model, modelState, legacy), % JL 
	!,
	@app->>openScenarios(@on). %same as design mode (but in pickmode), will close any opened visualize window when not canceled.
%
choose_input_models(F):-
		%legacy mode - legacy code
		
		%gp3 0.3.13: rewrote this one to fit in assistanceDialog with help button
        % Show input models to choose from for loading
        % deal with current bug of not being able to 
        % switch between using saved states and input models
        %
        %
        
        
        new(D, assistanceDialog('Choose a scenario','Sim_ChooseAScenarioLegacyMode')),
       D->>icon(@simulate_icon),
       GapX = D?gap<<-width,
       GapY = D?gap<<-height,
       
       D->>display(new(B, extendedListBrowser),point(GapX,D?topY)),
	send(B, width, 40),

	% find all input models and append to the listbox

	findall(Name, engine:smd(input_system(Name),_,_,_,_,_), Names),	

	forall(member(Name, Names),
		(
			%gp3 0.1: lets see if we can find the design object for it
		 	if
		 		SC = @app<<-findExportedObject(mf,Name)
		 	then
		 	(
		 		B->>append(new(dict_item(Name,SC?name,SC)))
		 	)
		 	else
		 	(
		 		B->>append(new(dict_item(Name,Name,@nil)))
		 	)
		)
	),

	% determine accurate width of listbox
	find_max_length(Names, MaxLength),
	MaxLengthPlus is MaxLength + 25, % extra margin - better solution?!!
	min(MaxLengthPlus, 70, Width), 
	send(B, width, Width),
 
%	send(B, height, Height), 

	% if possible, select the first element
	(   catch(get(B?members, head, First), _, fail)
	->  send(B, selection, First) 
	;   true
	),
		
	send(B, open_message,
		and(
			message(@prolog, write_ln, @arg1?key),
			message(@prolog, load_input_model, F, @arg1?key), 
			%message(D, destroy)
			message(D,return,@on)
		)
	),			

        send(D, display, new(Open,button(open, 
		and(
			message(@prolog, 
				load_input_model, F, B?selection?key),
			%message(D, destroy)
			message(D,return,@on)
		))),point(GapX,B?bottom_side + GapY) %will center lateron
	),
        send(D, display, new(Cancel,button(cancel, message(D, destroy))), point(Open?left_side,Open?top_side)),
        Open->>set(x:= B?left_side + (B?pixelWidth - (Open?width + GapX + Cancel?width)) / 2 ),
        Cancel->>set(x:= Open?right_side+ GapX),
    D->>updateSpacers, %gp3 0.3.13
        send(D, default_button, open),
	send(D, modal, application),!,
	send(D, transient_for, F),
        get(D, confirm, _Answer),
        D->>destroy.

load_input_model(F, Str):-
%gp3 0.1: changed calls to engine:...
	send(F, clear),
  
	enginectrl:command(new_scenario(input_system(Str))),
        type_of_file_first_opened(FirstFileType), 
        set_file_type(F, FirstFileType, input_model),
        send(F, deselect_all),
    	send(F, reset_position),    
	reset_entity_var_nrs,
        reset_aggregation, 
        reset_settings, 
	send(F, gen_simulation_view, @on),
	% send(F, layout), 
	%gp3 0.1: tell the app we loaded this scenario, so it can be saved as last edited with the model
	@app->>visualizing(Str),
	%and put the label on the window
	F->>setLabel. %gp3 0.4.9 made this an explicit call

 
%%from garp_cleanup.pl:
% reset_aggregation
%
% maybe more should be retracted - check! 
% 
reset_aggregation:-
        retractall(rec_aggr_transition( _, _)),
        retractall(rec(upper_bound_path_length( _, _))),
        retractall(rec_transition(_, _)),
        retractall((rec(aggregation, _))).
        

pair_member(X/Y, [X, Y | _Rest]).

pair_member(X/Y, [_A | Rest]):-
	pair_member(X/Y, Rest).

%%%%%%%%%%%%%%%%
% show_list(LB, Title, List, MaxLength)
%
% appends a titled list of terms to list browser LB
%
% empty list
show_list(LB, Title, [], 0):-
	atomize(Title, TitleStr),
 	get(string('%s: \tnone', TitleStr),
		value, String),
	send(LB, append, String).
%
% title & first element	
show_list(LB, Title, [First|Rest], MaxLength):-
	atomize(Title, TitleStr),
	atomize(First, FirstStr),
 	get(string('%s: \t%s', TitleStr, FirstStr),
		value, String),
	string_length(String, TempMax),
	% append item with Title as key Title, and String as label and object
	send(LB, append, dict_item(Title, String, String)),	
	show_list2(LB, Title, Rest, TempMax, MaxLength).




%
% other elements
%
% no more elements - temporary max length is the real maximum
show_list2(_LB, _Title, [], MaxLength, MaxLength).

show_list2(LB, Title, [First|Rest], TempMax, MaxLength):-
	atomize(Title, TitleStr),
	atomize(First, FirstStr),
	string_length(TitleStr, N), 
	% construct empty string of N spaces
	sformat(EmptyStrCode, '~w~w~w', ['~t~', N, '|']),
	sformat(EmptyStr, EmptyStrCode, []),	
	% put empty string in front of the first string
 	get(string('%s  \t%s', EmptyStr, FirstStr),
		value, String),
	% append item with Title as key Title, and String as label and object
	send(LB, append, dict_item(Title, String, String)),	
%	send(LB, append, String),
%	write(String),
	string_length(String, NewLength),
%	write_ln(NewLength),
	max(TempMax, NewLength, NewTempMax),	
	show_list2(LB, Title, Rest, NewTempMax, MaxLength).



% show_list_new(LB, Title, ComplexStructure, Style, MaxLength)
%
% transforms an unknown, possibly complex structure 
% (which can be a list or a term, or atom) into a 
% list of strings, indented for every term-structure
% and puts them into list browser LB.
% MaxLength is the length of the longest string in LB.
% 
% Items which denote categories like 'system elements', 
% 'parameters', 'par_values', 'par_relations', and 'system structures' 
% are added in plain style; the rest of the items are added with 
% the Style argument.
%
show_list_new(LB, Title, Contents, Style, Max):-	
	ComplexStructure =.. [Title, Contents], 
	sl(ComplexStructure, StrList, 0, Max),
	forall(member(Str, StrList), 
		((
		  % sub_string(Str, _Start, _L, _RL, String)
		  sub_string(Str, _, _, _, 'entities and relations');
		  sub_string(Str, _, _, _, 'quantities');
		  sub_string(Str, _, _, _, 'quantity values');
		  sub_string(Str, _, _, _, 'dependencies');
		  sub_string(Str, _, _, _, 'model fragments')
		 )
		 ->
%			send(LB, append, dict_item(Str))
		        send(LB, append, dict_item(Title, Str, Str))
		;
			send(LB, append, 
%				dict_item(Str, style := Style)
				dict_item(Title, Str, Str, style := Style)
			)
		)
	).

% qspace_member(Value, QSpaceList, Index) succeeds when Value occurs 
% at place Index in QSpaceList, which consists of points and 
% intervals successively 
% 	 
% point value
qspace_member(Value, [point(Value)|_RestQSpaceList], 1).

	
% interval value
qspace_member(Value, [Value|_RestQSpaceList], 1):-
	strip_atomize(Value, Atom), 
	Atom \== point. 

% look for value in the rest of the quantity space
qspace_member(Value, [_X|RestQSpaceList], N1):-
	qspace_member(Value, RestQSpaceList, N),
	N1 is N + 1.

print_selection(SelectionChain):-
	chain_list(SelectionChain, SelectionList), 
	my_write_list(SelectionList).

my_write_list([]).

my_write_list([X|Tail]):-
	write_ln(X),
	my_write_list(Tail). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Utility Routines
% 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 

atomize(X, X):-
	atom(X),!.

atomize(X, Y):-
	term_to_atom(X, Y).


% strip_atomize(X, Y) 
%
% strips all additional elements from X, e.g., 
% strip_atomize(pred(bla(bla1,bla2)), pred).
%
strip_atomize(X, X):-
	atom(X).

strip_atomize(X, Y):-
	X =.. [Y, _Z|_Qualification].




% list_max(IntList, Max)
%
% determines the maximum value of a list of integers
%
list_max(IntList, Max):-
	sort(IntList, Sorted), 
	reverse(Sorted, [Max|_Rest]).



% str_to_statelist(Str, RealStateList):-
%
% transforms string Str to RealStateList, containing all 
% state integers in Str, which should be something like '[1,2,3]'
%
str_to_statelist(StateListStr, RealStateList):-
	% check whether it's a valid StateList
	catch(term_to_atom(StateList, StateListStr), _, fail), 
	is_list(StateList),
	findall(N, 
		(member(N, StateList), 
		% check whether it's a valid State
		% If not, give a warning message?!
		catch(engine:state(N, _SMD), _, fail)
		),
		RealStateList),!.

% backup case in case the above clause fails: return empty list
str_to_statelist(_StateListStr, []).



% transition_name(FromNr, ToStateList, Name)
%
% given a state numbered FromNr, it finds all transitions 
% with their lists of resulting states (can this be empty?!)
%
transition_name(StateFrom, ToStateList, TransitionName):-        
        To = to(_, _, _, to_state(ToStateList), _Status),
	engine:state_to(StateFrom, ToList), 
	nth1(TransitionNr, ToList, To),         
        create_transition_name(StateFrom, TransitionNr, TransitionName).



decompose_transition_name(TransitionName, StateFrom, TransitionNr):-
        garp_version(N),
        N =< 1.72,
        % old version
	name(TransitionName, String), 
	last(String, Last), % order of attributes changed, 28/4/2003
	common_select(String, Last, Rest), 
	name(StateFrom, Rest), 
	TransitionNr is Last - 96.



decompose_transition_name(TransitionName, StateFrom, TransitionNr):-
        garp_version(N),
        N > 1.72,
        % new version
	concat_atom([StateFromStr, TransitionStr], t, TransitionName), 
        atom_number(StateFromStr, StateFrom), 
        atom_number(TransitionStr, TransitionNr).



create_transition_name(StateFrom, TransitionNr, TransitionName):-
        garp_version(N),
        N =< 1.72,
        % old version

	findall(StateFrom/ToNr/ToStates/Status, 
		(
		engine:state_to(StateFrom, ToList), 
		nth1(ToNr, ToList, to(_, _, _, to_state(ToStates), Status))
		), 
		AllTo), 
	engine:transition_names(AllTo, Transitions), 
	!,
	nth1(TransitionNr, Transitions, TransitionName).


create_transition_name(StateFrom, TransitionNr, TransitionName):-
        garp_version(N),
        N > 1.72,
        % new version
	concat_atom([StateFrom, TransitionNr], t, TransitionName).



% transition_name1(FromNr, ToNr, Name)
%
% succeeds for every transition named Name from state FromNr
% to ToNr, e.g., transition_name1(1, 2, '1a2')
%

% special case, for 'transitions' from input to state, 
% e.g., transition_name1(input, 2, 'inputto2')
%
transition_name1(From, To, UniqueTransitionName):-
	% construct string Name by appending 'to%ToNr' to 'input'
        From == input, 
	!, 
        rec_transition(input, ToNr), 
	atom_number(To, ToNr),
	concat_atom([input, ToNr], to, UniqueTransitionName).


transition_name1(From, ToNr, UniqueTransitionName):-
        integer(From),!,
        rec_transition(From, ToNr), 
        transition_name(From, ToStateList, TransitionName),
	% atom_number(To, ToNr),
        member(ToNr, ToStateList), 
        swritef(Str, '%wto%d', [TransitionName, ToNr]), 
        string_to_atom(Str, UniqueTransitionName).



% if From and To are strings, convert them first to numbers
% 
transition_name1(FromStr, ToStr, UniqueTransitionName):-
        nonvar(FromStr), 
        nonvar(ToStr), !,
        atom_number(FromStr, FromNr), 
        atom_number(ToStr, ToNr), 
        rec_transition(FromNr, ToNr), 
        transition_name(FromNr, ToStateList, TransitionName),
        member(ToNr, ToStateList),
        swritef(Str, '%wto%d', [TransitionName, ToNr]),
        string_to_atom(Str, UniqueTransitionName).



% if From is a string, convert it to a number first
% 
transition_name1(FromStr, ToStr, UniqueTransitionName):-
        nonvar(FromStr), 
        var(ToStr), !,
        atom_number(FromStr, FromNr), 
        rec_transition(FromNr, ToNr), 
        %gp3: we change ToNr to ToStr, this line was missing?
        atom_number(ToStr,ToNr),
        
        transition_name(FromNr, ToStateList, TransitionName),
        member(ToNr, ToStateList),
        swritef(Str, '%wto%d', [TransitionName, ToNr]),
        string_to_atom(Str, UniqueTransitionName).
 


% if From is a variable too, find all 
% 
transition_name1(From, ToNr, UniqueTransitionName):-
        var(From), 
        var(ToNr), !,
        rec_transition(From, ToNr), 
        transition_name(From, ToStateList, TransitionName),
        member(ToNr, ToStateList),
        swritef(Str, '%wto%d', [TransitionName, ToNr]),
        string_to_atom(Str, UniqueTransitionName).




write_chain(GrsChain):-
	chain_list(GrsChain, GrsList), 
	write('Chain consists of: '),
	write_ln(GrsList),
	forall(member(Gr, GrsList), 
		( 
		 get(Gr, name, Name),
		 write('Name: '),
		 write_ln(Name)
		)
	).



% not_derivative_rel(Rel),
%
% fails when Rel is a relation for a derivative
% succeeds in all other cases
%
not_derivative_rel(Rel):-
	(
		Rel == 'd_equal';
		Rel == 'd_greater';
		Rel == 'd_greater_or_equal';
		Rel == 'd_smaller';
		Rel == 'd_smaller_or_equal';
		Rel == 'd_less';
		Rel == 'd_less_or_equal'
	),
	!, fail.

% succeed in all other cases
%
not_derivative_rel(_Rel).

	


find_max_length(StrList, MaxLength):-
	findall(Length, 
		(
		 member(Str, StrList),
		 string_length(Str, Length)
		),
		IntList
	),
	list_max(IntList, MaxLength).

% insert_newlines(Str, Max, chartab(T), NewStr)
%
% If Str is longer than Max, insert newline and T spaces		
%
% try to break at a space
insert_newlines(Str, Max, chartab(T), NewStr):-
        string_length(Str, N),
        N > Max, 
        empty_str(T, EmptyStr),      
        cut_at_last_space(Str, Max, _Index, L, R),!,
        insert_newlines(R, Max, chartab(T), NewR),
        swritef(NewStr, '%w\n%w%w', [L, EmptyStr, NewR]).

% if that doesn't succeed, break exactly at Max
insert_newlines(Str, Max, chartab(T), NewStr):-
        string_length(Str, N),
        N > Max, !,
        empty_str(T, EmptyStr),      
	sub_string(Str, 0, Max, RestLength, SubStr), 
        string_concat('\n', EmptyStr, NewLinePlusTab),
        string_concat(SubStr, NewLinePlusTab, NewSubStr),
        sub_string(Str, Max, RestLength, _, RestStr), 
        insert_newlines(RestStr, Max, chartab(T), NewRestStr),
        string_concat(NewSubStr, NewRestStr, NewStr).
        
% N <= Max 
% No line break necessary
insert_newlines(Str, _Max, chartab(_T), Str).
                
      
cut_at_first_space(Str, Index, L, R):-
        string_length(Str, Length), 
        sub_string(Str, 0, Index, _LengthL, L), 
        sub_string(Str, Index, 1, _Length, ' '), 
        Index1 is Index + 1, 
        LengthR is Length - Index1,
        sub_string(Str, Index1, LengthR, 0, R).


cut_at_last_space(Str, Max, Index, L, R):-
        string_length(Str, Length), 
        str_reverse(Str, RevStr), 
        cut_at_first_space(RevStr, RevIndex, RevL, RevR),
        Index is Length - RevIndex, 
        str_reverse(RevR, L), 
        str_reverse(RevL, R),
        Index =< Max.

str_reverse(Str, RevStr):-
        string_to_list(Str, List), 
        reverse(List, RevList), 
        string_to_list(RevStr, RevList).




% add(List, X, NewList)
%
% If X is not already in List, add it, and 
% create NewList
%
add(List, X, List):-
	member(X, List),!.

add(List, X, NewList):-
	conc(List, [X], NewList).


%formerly identically defined in both class_state_node and class_graph_node
update_selection_nr(Gr) :-
	get(Gr, frame, F), 
	% write_ln('update selection nr: '), 
	get(F, selection_nr, SelNr), 
	NewSelNr is SelNr + 1, 
	send(Gr, selection_nr, NewSelNr),
	% write('new selection nr: '), write_ln(NewSelNr), 
	send(F, selection_nr, NewSelNr).


/*
save_in_chain
gp3: chain-based rewrite of legacy my_save_current_state code.
we now write the clauses to the given chain, (which is a variable of @model)
instead of to a file
For the rest, we save exactly the same information as did visigarp
(see application.pl: @app->>saveSimulation)
*/

save_in_chain(C):-
	save_in_chain_call(C,engine:state(_,_)),
	save_in_chain_call(C,engine:state_status(_, _)),
	save_in_chain_call(C,engine:state_from(_, _)),
	save_in_chain_call(C,engine:state_to(_, _)),
	save_in_chain_call(C,engine:qspace(_,_, _, _)),
	save_in_chain_call(C,engine:state_values(_, _)), % FL new june 07 helper pred to speed up state matching
	save_in_chain_call(C,engine:scenario_state(_)),
	%we also save all the gensym flags
	%we put them in a special term so read_from_chain will recognise it
	forall(
		recorded('$gensym',Key),
		(
			flag(Key,Old,Old),
			term_to_atom(saved_state_gensymkey(Key,Old),Atom),
			C->>append(Atom)
		)
	),
	%finally, save state_number
	flag(state_number, StNr, StNr), 
	term_to_atom(saved_state_state_number(StNr),Atom),
	C->>append(Atom).

save_in_chain_call(Chain,Call):-
	%helper, get the atom to save
	forall(
		Call,
		(
			term_to_atom(Call,Atom),
			Chain->>append(Atom)
		)
	).

/*
load_from_chain
gp3: chain_based rewrite of legacy open_simulation and vg_load_current_state (in engine/standalone/interface.pl)
We now read clauses from the given chain (which is saved in @model)
see application.pl: @app->>loadSimulation
This (as allways) only works if the model is not changed too much..
*/

load_from_chain(Saved):-
	%clear all visualize-dedicated data
	%copied literally from load_input_model, so should work
	reset_entity_var_nrs,
	reset_aggregation,
    reset_settings, 
    once((reset_gensym ; true)), %vg_load_current_state
	%get the chain contents to prolog
	Saved->>for_all(->>(@prolog,load_from_chain_atom,@arg1)). %default context = current = visualize
%
load_from_chain_atom(Atom):-
	%Atom is an atom describing one call
	%Atom can be special stuff like saved gensymkey flag (see save_in_chain)
	
	term_to_atom(Term,Atom),
	load_from_chain_fact(Term).
%
load_from_chain_fact(saved_state_gensymkey(Key,Value)):-!,
	%make sure the gensym value is restored (as in vg_load_current_state)
	flag(Key,_,Value),
	gensym:record_gensym(Key, 0).
%
load_from_chain_fact(saved_state_state_number(StNr)):-!,
	flag(state_number, _, StNr).
%
load_from_chain_fact(Term):-
	%else, just assert the knowledge
	assertz(Term).


% calculate_division(X, Z, D):-
% 
% circumvent division by zero
%
calculate_division(X, Z, D):-
	(Z =:= 0
	->
	    D is 0
	;
	    D is X/Z
	).

