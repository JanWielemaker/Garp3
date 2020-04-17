/*  File:    interface
    Purpose: SWI-prolog interface to GARP
    Author:  Martin Reinders & Bert Bredeweg & Floris Linnebank
    Date:    August 1989
    Part-of:  GARP (version 2.0)
    Modified: 12 September 2004

    Needs: file terminal & pl-bit.o (for screen_size)

    Copyright (c) 2004, University of Amsterdam. All rights reserved.

    updated 28 April 2003 by Anders Bouwer
    updated 12 September 2004 by Floris Linnebank
*/


/* ------- print quantity_space for a  parameter for all states ------ */

value_table(Parameter) :-
	findall(Value/Derivative,
		 (      state(_, SMD),
		 	smd_slots(SMD, _, _, _, Values, _, _, _),
		 	(memberchk(value(Parameter, _, Value, Derivative),
		 			Values)
		 		-> true
		 		;  Value = nonexistent
		 	)
		),
		 ValueList),
	qspace(Parameter, Par, Space, _),
	parameter(Par, _, _, SE, Parameter, _, _),
	term_to_atom(SE, SEatom),
	concat_atom([Parameter, '(', SEatom, ')' ], Item),
	write_ln(Item), 	% e.g. temp1(water1)
	value_table(Space, ValueList, Space),
	nl,
	!.

value_table([], _, _).

value_table([point(H)|T], ValueList, Space) :-
	value_table(T, ValueList, Space),
	value_table_point_line(H, ValueList).

value_table([H|T], ValueList, Space) :-
	Space = [H|_], 		% lowest in space
	value_table(T, ValueList, Space),
	value_table_intermediate_interval_line(ValueList),
	value_table_interval_line(H, ValueList),
	value_table_bottom_interval_line(ValueList).

value_table([H|T], ValueList, Space) :-
	last(Space, H),     % argument order changed, 28/04/2003, AB
	% last(H, Space), 		% highest in space
	value_table(T, ValueList, Space),
	value_table_top_interval_line(ValueList),
	value_table_interval_line(H, ValueList),
	value_table_intermediate_interval_line(ValueList).

value_table([H|T], ValueList, Space) :-
	value_table(T, ValueList, Space),
	value_table_intermediate_interval_line(ValueList),
	value_table_interval_line(H, ValueList),
	value_table_intermediate_interval_line(ValueList).

% written all: write name

value_table_point_line(Point, []):- tab(2), write_ln(Point).

% empty space for state that doesn't use parameter

value_table_point_line(Point, [ Value/_ | T ]) :-
	Value == nonexistent,
	tab(5),
	value_table_point_line(Point, T).

% place in space corresponds to value, mark according to derivative

value_table_point_line(Point, [ Value/Derivative | T ]) :-
	Value == Point,
	write('['),
	write_derivative_in_space(Derivative),
	write(']'),
	tab(2),
	value_table_point_line(Point, T).

% place doesn't correspond to value

value_table_point_line(Point, [ _ | T ]) :-
	write('[ ]'),
	tab(2),
	value_table_point_line(Point, T).


value_table_interval_line(Interval, []):- tab(2), write_ln(Interval).

value_table_interval_line(Interval, [ Value/_ | T ]) :-
	Value == nonexistent,
	tab(5),
	value_table_interval_line(Interval, T).

value_table_interval_line(Interval, [ Value/Derivative | T ]) :-
	Value == Interval,
	write('|'),
	write_derivative_in_space(Derivative),
	write('|'),
	tab(2),
	value_table_interval_line(Interval, T).

value_table_interval_line(Interval, [ _ | T ]) :-
	write('| |'),
	tab(2),
	value_table_interval_line(Interval, T).

value_table_top_interval_line([]):- nl.

value_table_top_interval_line([ Value/_ | T ]) :-
	Value == nonexistent,
	tab(5),
	value_table_top_interval_line(T).

value_table_top_interval_line([ _ | T ]) :-
	write('/ \'),
	tab(2),
	value_table_top_interval_line(T).

value_table_bottom_interval_line([]):- nl.

value_table_bottom_interval_line([ Value/_ | T ]) :-
	Value == nonexistent,
	tab(5),
	value_table_bottom_interval_line(T).

value_table_bottom_interval_line([ _ | T ]) :-
	write('\ /'),
	tab(2),
	value_table_bottom_interval_line(T).

value_table_intermediate_interval_line([]):- nl.

value_table_intermediate_interval_line([ Value/_ | T ]) :-
	Value == nonexistent,
	tab(5),
	value_table_intermediate_interval_line(T).

value_table_intermediate_interval_line([ _ | T ]) :-
	write('| |'),
	tab(2),
	value_table_intermediate_interval_line(T).

write_derivative_in_space(Derivative) :-
	var(Derivative),
	write(?).

write_derivative_in_space(zero) :-
	write('0').

write_derivative_in_space(plus) :-
	write('+').

write_derivative_in_space(min) :-
	write('-').


/* --------------------- transition table ---------------------------- */

transition_table(Transitions) :-
	findall(StateNr,
		state(StateNr, _),
		StateNrList),
	findall(FromState/ToNr/ToStates/Status,
		(
		member(FromState, StateNrList), 	%right order
		state_to(FromState, ToList),
		nth1(ToNr, ToList, to(_, _, _, to_state(ToStates), Status))
		),
		AllTo),
	transition_table(AllTo, StateNrList),
	transition_names(AllTo, Transitions),
	!.


% transition_names: New version 18-2-2004 by FL:
% old '1a' style transition notation had only room for 256 transitions,
% new version '1t1, 1t2, 1t3, etc.' notation has no bounds

/*** Old version: ***
transition_names([], []).
transition_names([ FromState/ToNr/_/_ |T1 ], [ H|T2 ]) :-
	ToChar is 96 + ToNr,
	name(ToName, [ToChar]),
	concat(FromState, ToName, H),
	transition_names(T1, T2).
*** New Below: '1t1' notation ***/

transition_names([], []).
transition_names([ FromState/ToNr/_/_ |T1 ], [ Name|T2 ]) :-
    concat_atom([FromState, ToNr], t, Name),
	transition_names(T1, T2).

% end new FL

transition_table([], _).
transition_table([ FromState/ToNr/ToStates/Status | T], StateNrList) :-
	write(' '),
	transition_table_line(StateNrList, FromState, ToNr, ToStates, Status),
        transition_table(T, StateNrList).


% transition_table_line: New version 18-2-2004 by FL:
% old '1a' style transition notation had only room for 256 transitions,
% new version '1t1, 1t2, 1t3, etc.' notation has no bounds

/*** Old version: ***
transition_table_line([], FromState, ToNr, _, Status) :-
	write(FromState),
	C is 96 + ToNr,
	put(C), tab(1),
	write_ln(Status).
*** New Below: '1t1' notation ***/

transition_table_line([], FromState, ToNr, _, Status) :-
	concat_atom([FromState, ToNr], t, Name),
    write(Name),
    tab(1),
	write_ln(Status).

% endnew

% at FromState: transition has status terminated (no ToStates)
transition_table_line([FromState|T], FromState, ToNr, [], terminated) :-
	write('+'),
	tab(4),
	transition_table_line(T, FromState, ToNr, [], terminated).

% at FromState: transition has status ordered (no ToStates)
transition_table_line([FromState|T], FromState, ToNr, [], ordered) :-
	write('='),
	tab(4),
	transition_table_line(T, FromState, ToNr, [], ordered).

% at FromState: transition (also) to state itself
transition_table_line([FromState|T], FromState, ToNr, ToStates, Status) :-
	memberchk(FromState, ToStates),
	write(*),
	(	member(SomeState, ToStates), 	% also transition to after
		memberchk(SomeState, T),
		write('----')
	;
		tab(4)
	),
	transition_table_line(T, FromState, ToNr, ToStates, Status).

% at FromState: no transitions
transition_table_line([FromState|T], FromState, ToNr, [], Status) :-
	write(x),
	tab(4),
	transition_table_line(T, FromState, ToNr, [], Status).

% at FromState: normal transition
transition_table_line([FromState|T], FromState, ToNr, ToStates, Status) :-
	write(o),
	(	member(SomeState, ToStates),
		memberchk(SomeState, T),
		write('----')
	;
		tab(4)
	),
	transition_table_line(T, FromState, ToNr, ToStates, Status).

% transition to this state, before FromState
transition_table_line([CurrentState|T], FromState, ToNr, ToStates, Status) :-
	memberchk(CurrentState, ToStates),
	memberchk(FromState, T),
	write('<----'),
	transition_table_line(T, FromState, ToNr, ToStates, Status).

% transition to this state, after FromState
transition_table_line([CurrentState|T], FromState, ToNr, ToStates, Status) :-
	memberchk(CurrentState, ToStates),
	write(>),
	(	member(SomeState, ToStates),
		memberchk(SomeState, T),
		write('----')
	;
		tab(4)
	),
	transition_table_line(T, FromState, ToNr, ToStates, Status).

% not a transition to this state, but to one before it
transition_table_line([_|T], FromState, ToNr, ToStates, Status) :-
	memberchk(FromState, T), 	% before fromstate
	member(SomeState, ToStates), 	% some tostate
	\+ memberchk(SomeState, T), 	% not in tail
	write('-----'),
	transition_table_line(T, FromState, ToNr, ToStates, Status).

% not a transition to this state, but to one after it
transition_table_line([_|T], FromState, ToNr, ToStates, Status) :-
	\+ memberchk(FromState, T), 	% after fromstate
	member(SomeState, ToStates), 	% some tostate
	memberchk(SomeState, T), 	% after current
	write('-----'),
	transition_table_line(T, FromState, ToNr, ToStates, Status).

% no transitions to this, or before, or after
transition_table_line([_|T], FromState, ToNr, ToStates, Status) :-
	tab(5),
	transition_table_line(T, FromState, ToNr, ToStates, Status).

tables(States, Transitions) :-
	forall(qspace(Instance, _, _, true), value_table(Instance)),
	transition_tables(States, Transitions).

transition_tables(WhichStates, Transitions) :-
	write(' '),
	findall(Which,
		(state(Which, _),
		  write(Which),
		  (immediate_cause(Which) ->
		    write('!')
		    ;
		    write(' ')
		  ),
		  atom_length(Which, L),
		  Tab is 4 - L,
		  (Tab > 0 -> tab(Tab) ; true)
		),
		WhichStates),
	nl,
	transition_table(Transitions),
	nl,
	!.

% New FL July 2004: checks if a state is closed and at
% least one cause in one transition is of small epsilon type
% (should also include use epsilon switch..., 
% if no epsilon notion is used, immediate transitions are not distinguished)

immediate_cause(Which):-
    state_status(Which, closed),
    state_to(Which, [ to(cause(Causes), _, _, _, _) |_]),
    small_epsilon_set(Small), % collect the set of terminations defined small: in transitions.pl 
    member(Cause, Causes), 
    memberchk(Cause, Small),
    !.


/* FL May 2004, immediate_cause/2 was used in transition_tables, 
replaced by code from epsilon ordering procedure, see transitions.pl

% a cause that should result in a single immediate termination
immediate_cause(to_interval_above(_)).
immediate_cause(to_interval_below(_)).
immediate_cause(from_equal_to_greater(_, _)).
*/

/* --------------------------- main menu --------------------------------- */

main_menu :-
	repeat,
	set_screen_width,
	noprotocol,
	np,
	transition_tables(WhichStates, Transitions),
	append(WhichStates, Transitions, InvisibleOptions),
	(flag(protocol_file, 0, 0) ->
	    	ProtOption = p / 'protocol on'
	    	;
	  	flag(protocol_file, PFN, PFN),
		writef('Protocol on, file %w\n', [ PFN ]),
		ProtOption =  np / 'protocol off'
	),
	append(
		[
		as/'show all states',
		s / 'save current system state',
		(l) / 'load system state',
		i / 'garp 1.0 information',
		h / ('help'),
		el / 'edit library system structure',
		ei / 'edit input systems',
		er / 'edit rules',
		es / 'edit semantic network',
		eq / 'edit quantity spaces',
		sl / 'show system structure',
		si / 'show input system',
		hs / 'structures isa hierarchy',
		ha / 'structures applies to hierarchy',
		hi / 'semantic (isa) hierarchy',
		cs / 'recheck system structure library',
		ci / 'recheck input system models',
		new / 'new input system',
		tv / 'toggle show parameter values',
		sv / 'show parameter value table',
		ta / 'terminate all interpreted states',
		oa / 'order all terminated states',
		ca / 'close all ordered terminations',
		all / 'terminate & order & close all',
		table/ 'draw tables to protocol',
		chlib/ 'change library',
		o / 'trace options',
		am / 'assumptions menu',        % added februari 2004 by FL: menu for algorithm option switches
		da / 'set default assumptions', % added may 2004 by FL: load default algorithm option switch settings (including overrides in model)
		ah / 'assumptions help',        % added may 2004 by FL: show algorithm option switches helpfile
		q / 'quit'
		],
		[ProtOption], OptionList),
	small_menu('enter state(s), transition(s) or', [],
		OptionList,
		InvisibleOptions, ChosenList, true),
	findall(StateChosen,
		(member(StateChosen/_, ChosenList),
		  memberchk(StateChosen, WhichStates) ),
		 StatesChosen),
	state_menu(StatesChosen),
	forall(member(Chosen/_, ChosenList),
		(memberchk(Chosen, WhichStates) ;
	  	  memberchk(Chosen, Transitions) -> transition_menu(Chosen) ;
	  	  main_menu_action(Chosen))),
	memberchk(q/_, ChosenList).

main_menu_action(q) :-
	small_menu('quit: are you sure? ', [], [y/quit, n/cancel], [], C/_, false),
	C = y,
	!.

% added februari 2004 by FL: menu for algorithm settings switches
main_menu_action(am):- assumptions_menu.

% added may 2004 by FL: loads default algorithm settings AND overrides associated with the model
main_menu_action(da):-
    nl,
    write('Press y to load default algorithm assumptions, or any other to cancel: '),nl,
    write('> '),
    get0(C),
    (char_code(y, C) -> init_algorithm_assumptions ; true).

% added may 2004 by FL: shows helpfile for algorithm assumptions
main_menu_action(ah):-
	more(garp_help(algorithm_help)).

main_menu_action(o) :-
	trace_option_menu.

main_menu_action(tv):- toggle_visible_parameters('Toggle visible parameters').

main_menu_action(as):- show_all_states_menu.

main_menu_action(s) :-
	write_ln('Save current system state, enter file name (no extension) or c to cancel:'),
	read_filename(FileName),
	save_current_state(FileName).

main_menu_action(l) :-
	write_ln('*** Warning: load system state destroys current state !'),
	write_ln('Load system state, enter file name (no extension) or c to cancel:'),
	read_filename(FileName),
	load_current_state(FileName).

main_menu_action(h) :-
	more(garp_help(main_help)).

main_menu_action(i) :-
	more(garp_help(garp_info)).

main_menu_action(hs) :-
	tell_more,
	show_isa_ss_hierarchy,
	told_more.

main_menu_action(ha) :-
	tell_more,
	show_applies_to_ss_hierarchy,
	told_more.

main_menu_action(hi) :-
	tell_more,
	show_isa_hierarchy,
	told_more.

main_menu_action(sl) :-
	show_library_structure.

main_menu_action(si) :-
	show_input_model.

main_menu_action(cs) :-
	check_library.

main_menu_action(ci) :-
	check_input_system.

main_menu_action(table) :-
	findall(qspace(Instance, Par, Space, Visible),
		retract(qspace(Instance, Par, Space, Visible)),
		QSpaces),
	forall(member(qspace(Instance, Par, Space, _), QSpaces),
		assertz(qspace(Instance, Par, Space, fail))),
	toggle_visible_parameters('Toggle visible parameters for table'),
	tell_more,
	tables(_, _),
	told_more,
	retractall(qspace(_, _, _, _)),
	forall(member(QSpace, QSpaces),
		assertz(QSpace)),
	!.

main_menu_action(sv) :-
	tell_more,
	tables(_, _),
	told_more,
	!.

main_menu_action(table).

% applies only to output directed to the screen

main_menu_action(p) :-
	flag(protocol_file, 0, 0) ->
		repeat,
		gensym(protocol, PFName),
		\+ exists_file(PFName),
		writef('start protocol, file %w\n', [PFName]),
		flag(protocol_file, _, PFName),
		start_protocol
	;
	flag(protocol_file, PFName, PFName) ->
		writef('protocol already on, file %w\n', [PFName]).

main_menu_action(np) :-
	flag(protocol_file, 0, 0) ->
		write_ln('no current protocol file')
	;
	flag(protocol_file, FileName, 0),
	noprotocol,
	writef('protocol stopped, file %w\n', [FileName]).

main_menu_action(el) :-
	edit_warning('library'),
	edit_system_structure.
main_menu_action(ei) :-
	edit_warning('input system'),
	edit_input_system.
main_menu_action(es) :-
	edit_warning('semantic hierarchy'),
	edit(garp_library(isa)).
main_menu_action(er) :-
	edit_warning('rules'),
	edit(garp_library(rules)).
main_menu_action(eq) :-
	edit_warning('quantity spaces'),
	edit(garp_library(quantity_space)),
	(   quantity_space(_, _, _)
	->  initialise_quantity_spaces
	;   true
	).


main_menu_action(ta) :-
	terminate_all.
main_menu_action(oa) :-
	order_all.
main_menu_action(ca) :-
	close_all.
main_menu_action(all) :-
	repeat,
	terminate_all,
	order_all,
	close_all,
	\+ state_status(_, open),
	\+ state_status(_, interpreted),
	\+ state_status(_, terminated),
	\+ state_status(_, ordered).

main_menu_action(new) :-
	(   state(_, _)
	->  write_ln('*** Warning: new input model will erase current system state.')
	;   true
	),
	findall(Name, smd(Name, _, _, _, _, _), Names),
	single_column_menu('Choose input model', Names, [ (c)/'CANCEL' ], [],
		COption/CDescription, false),
	(   COption = c
	->  true
	;   new_input_model(CDescription)
	), !.

main_menu_action(chlib) :-
	ls, nl,
	write_ln('choose library file'),
	read_filename(Lib),
	retractall(current_library(_)),
	asserta(current_library(Lib)),
	read_library.

edit_warning(Which) :-
	(state(_, _) ->
	write_ln('*** Warning: no attempt is made to find consequences of changes'),
	write('made to the '), write_ln(Which), sleep(2);
	true).

start_protocol:-
	flag(protocol_file, P, P),
	P \= 0,
	protocola(P),
	!.

start_protocol.

/* -------------------------- state menu ------------------------------ */

state_menu([]):- !.

state_menu(States) :-
	(States = [State] ->
		concat('state ', State, Header)
		;
		space_items(States, StL),
		append(['states '], StL, HL),
		concat_atom(HL, Header)
	),
	repeat,
	small_menu(Header, [],
		[ 	s/show,
			t/terminate,
			o/order,
			(c)/close,
			u/'undo transitions',
			m/'main_menu' ],
		[],
		ChosenList, true),
	forall(member(Chosen/_, ChosenList),
		state_menu_action(States, Chosen)),
	memberchk(m/_, ChosenList),
	!.

state_menu_action(States, s) :-
	show_state_menu(States).
state_menu_action(States, t) :-
	forall(member(State, States), termination(State)).
state_menu_action(States, o) :-
	forall(member(State, States), precedence(State)).
state_menu_action(States, c) :-
	forall(member(State, States), transition(State)).
state_menu_action(States, u) :-
	forall(member(State, States), undo_transition(State)).
state_menu_action(_, _).

show_state_menu(Names) :-
	findall(fail/Label, show_state_selection(_, Label), InitSelections),
	(Names = [Name ] ->
		concat('select (toggle) items to show for state ', Name, Header)
		;
		space_items(Names, SI),
		append(['select (toggle) items to show for states '], SI, HL),
		concat_atom(HL, Header)
	),
	toggle_menu(
		Header,
		InitSelections,
		m/'cancel (main menu)',
		d/'done (show)',
		Selections),
	% fails if m is selected
	findall(Item, (member(true/Label, Selections),
			 show_state_selection(Item, Label)),
		 Items),
	tell_more,
	forall(member(Name, Names), 	show_state(Name, Items)),
	told_more.

show_state_menu(_).

space_items([], []).
space_items([H|T], [H, ' '|NT]):- space_items(T, NT).

show_all_states_menu:-
	findall(fail/Label, show_state_selection(_, Label), InitSelections),
	toggle_menu(
		'select (toggle) items to show for all states',
		InitSelections,
		(c)/'cancel (main menu)',
		d/'done (show)',
		Selections),
	% fails if c is selected
	findall(Item, (member(true/Label, Selections),
			 show_state_selection(Item, Label)),
		 Items),
	tell_more,
	forall(state(Name, _),
		(show_state(Name, Items), nl)),
	told_more.

show_all_states_menu.

show_state_selection(from, predecessors).
show_state_selection(to, 'terminations / transitions').
show_state_selection(system_elements, 'smd: system elements').
show_state_selection(parameters, 'smd: parameters').
show_state_selection(par_values, 'smd: values').
show_state_selection(par_relations, 'smd: relations').
show_state_selection(system_structure_names, 'smd: system structure names').
show_state_selection(system_structures, 'smd: system structures').

show_state_selection(input_model, 'smd: input model').

/* ------------------------- transition menu ------------------------- */

% transition_menu: New version 18-2-2004 by FL:
% old '1a' style transition notation had only room for 256 transitions,
% new version '1t1, 1t2, 1t3, etc.' notation has no bounds

transition_menu(TransitionName) :-
	/*** Old '1a' notation: ***
	name(TransitionName, String),
	last(String, Last),     % argument order changed, 28/04/2003, AB
	% last(Last, String),
	common_select(String, Last, Rest),
	name(State, Rest),
	Transition is Last - 96,
    *** New Below: '1t1' notation ***/
	concat_atom([StateA, TransitionA], t, TransitionName),
	atom_number(StateA, State),
	atom_number(TransitionA, Transition),
    /*** End New ***/
	repeat,
	state_to(State, ToList),
	nth1(Transition, ToList, To),
	To = to(_, _, _, _, Status),
once((Status == ordered ->
	  small_menu('ordered transition', [],
		[ s/show,
		  (c) / 'close (apply transition)',
		  m / 'main menu' ],
		[], Chosen/_, false),
	  (Chosen == c ->  transition(State, Transition);
	    Chosen == s -> show_transition(State, Transition, To); true)
	  ;
	  Status == closed ->
	  small_menu('closed transition', [],
		[ s/show,
		  u / 'undo transition',
		  m / 'main menu' ],
		[], Chosen/_, false),
	  (Chosen == u -> undo_transition(State, Transition) ;
	    Chosen == s -> show_transition(State, Transition, To); true)
	  ;
	  small_menu('terminated transition', [],
		[ s/show, m / 'main menu' ],
		[], Chosen/_, false),
	  (Chosen == s -> show_transition(State, Transition, To);true)
	)),
        Chosen == m,
	!.


% show_transition: New version 18-2-2004 by FL:
% old '1a' style transition notation had only room for 256 transitions,
% new version '1t1, 1t2, 1t3, etc.' notation has no bounds

/*** Old Version ***
show_transition(State, Nr, To) :-
 	tell_more,
	write('state '),
	write(State),
	write(' transition '''),
	Which is Nr + 96,
	put(Which),
	write_ln(''''),
	show_to_list(0, [To]),
	nl,
	told_more.
*** New Below: '1t1' notation ***/

show_transition(State, Nr, To) :-
 	tell_more,
	write('state '),
	write(State),
	write(' transition '''),
	write(Nr),
	write_ln(''''),
	show_to_list(0, [To]),
	nl,
	told_more.

% end new

/* ------------------------- trace option menu ----------------------- */

trace_option_menu:-
	findall(Selected/Label, trace_select(_ , Selected, Label), InitSelections),
	toggle_menu('select trace options',
		InitSelections,
		(c)/'cancel (main menu)',
		d/'done',
		Selections),
	retractall(trace_select(_, _, _)),
	forall(nth1(N, Selections, Selected/Label),
		assertz(trace_select(N, Selected, Label))).

trace_option_menu.

:- dynamic
	trace_select/3.

trace_select(1, fail, 'Searching for applicable system structures').
trace_select(2, fail, 'Adding/checking relations').
trace_select(3, fail, 'Inequality reasoning').
trace_select(4, fail, 'Resolving influences and proportional relations').
trace_select(5, fail, 'Searching for system structures still applicable after transition').
trace_select(6, fail, 'Searching for terminations').
trace_select(7, fail, 'Ordering of terminations').
trace_select(8, fail, 'Application of transitions').
trace_select(9, fail, 'Show derivable relations').
trace_select(10, fail, 'Assuming system structures').

trace_on(specification):- trace_select(1, true, _).
trace_on(add_relation):- trace_select(2, true, _).
trace_on(inequality):- trace_select(3, true, _).
trace_on(resolve):- trace_select(4, true, _).
trace_on(respecification):- trace_select(5, true, _).
trace_on(termination):- trace_select(6, true, _).
trace_on(ordering):- trace_select(7, true, _).
trace_on(transition):- trace_select(8, true, _).
trace_on(derivable):- trace_select(9, true, _).
trace_on(assumptions):- trace_select(10, true, _).

tracer(Which, Atom, List) :-
	is_list(Which),
	member(Some, Which),
	trace_on(Some),
	(	member(assumptions, Which) ->
			current_assumption(_, Tab),
			tab(Tab);
		true),
	writef(Atom, List),
	nl,
	!.

tracer(Which, Atom, List) :-
	atomic(Which),
	trace_on(Which),
	(	Which == assumptions ->
			current_assumption(_, Tab),
			tab(Tab);
		true),
	writef(Atom, List),
	nl,
	!.

tracer(_, _, _).


/* ----------------------- load / save current state ----------------- */


% save state: write all global information to a file that can be consulted

save_current_state(c).
save_current_state(FileName) :-
	atom(FileName),
	(exists_directory(saved_states) ->
		concat_atom([ 'saved_states/', FileName, '.svst'], File);
		concat(FileName, '.svst', File)
	),
	(exists_file(File) ->
		small_menu('File exists, overwrite? ',
		[], [y/yes, n/no], [], C/_, false),
		C = y
	  ;
	  true
	),
	write('saving current system state to '),
	write_ln(File),
	tell(File),
	repeat,
	(	state(A, B),
		writeq(state(A, B)), write_ln('.'), nl,
		fail
		;
		state_status(A, B),
		writeq(state_status(A, B)), write_ln('.'), nl,
		fail
		;
		state_from(A, B),
		writeq(state_from(A, B)), write_ln('.'), nl,
		fail
		;
		state_to(A, B),
		writeq(state_to(A, B)), write_ln('.'), nl,
		fail
		;
		qspace(Q, P, S, Visible),
		writeq(qspace(Q, P, S, Visible)),
		write_ln('.'), nl,
		fail
		;
		recorded('$gensym', Key),
		flag(Key, Old, Old),
		write('?- '),
		writeq(flag(Key, _, Old)),
		write(', '),
		writeq(gensym:record_gensym(Key, 0)),
		write_ln('.'),
		fail
		;
		true
	),
	flag(state_number, StNr, StNr),
	write('?- '),
	writeq(flag(state_number, _, StNr)),
	write_ln('.'),
	told,
	!.

save_current_state(FileName) :-
	\+ atom(FileName),
	write('Not a legal filename: '),
	write_ln(FileName),
	write_ln('Abandoned save state.'),
	sleep(2).

save_current_state(_) :-
	write_ln('Abandoned save state.'),
	sleep(2).

load_current_state(c):- !.

load_current_state(FileName) :-
	atom(FileName),
	(exists_directory(saved_states) ->
		concat_atom([ 'saved_states/', FileName, '.svst'], File);
		concat(FileName, '.svst', File)
	),
	(exists_file(File) ->
		write('loading current system state from '),
		write_ln(File),
		once((reset_gensym ; true)),
		style_check(-singleton),
		% consult(File),
                % autoload ensures the file is loaded despite warnings
		load_files([File], autoload(true)), % AB, 01/10/2002
		style_check(+singleton)
	;
		write_ln('File does not exist, load state abandoned.'),
		sleep(2)
	),
        % Added BB 27 / 09 / 2002
        % After consult these predicates lose their 'dynamic' status.
        % This must be updated again.
        dynamic(
            ( qspace/4,
              state/2,
              state_status/2,
              state_from/2,
              state_to/2 ) ).

load_current_state(FileName) :-
	\+ atom(FileName),
	write('Not a legal filename: '),
	write_ln(FileName),
	write_ln('Abandoned load state.'),
	sleep(2).






vg_load_current_state(File) :-
        (exists_file(File) ->
		write('loading current system state from '),
		write_ln(File),
		once((reset_gensym ; true)),
		style_check(-singleton),
		consult(File),
		style_check(+singleton)
	;
		write_ln('File does not exist, load state abandoned.'),
		sleep(2)
	),
        dynamic(
		( qspace/4,
		  state/2,
		  state_status/2,
		  state_from/2,
		  state_to/2 )
	).




vg_load_current_state(FileName) :-
	write('Not a legal filename: '),
	write_ln(FileName),
	write_ln('Abandoned load state.'),
	sleep(2).






banner:-
	cls,
	show_file(garp_help(banner)).

/* ------------------------ read the library ---------------------- */

read_library:-
        (  current_prolog_flag( bbwww_garp, true )
        -> xml_comment( 2, ['reading and checking system structure library ..'], garpinfo)
        ;  write_ln('reading and checking system structure library .. ')
        ),
	tell_more,
	retractall(system_structures(_, _, _, _)),
	(   recorded_erase(library_index, _),
	    fail
	;   true
	),
	current_library(Name),
	absolute_file_name(garp_library(Name), [access(read)], Path),
	see(Path),
	repeat,
	read_clause(Clause), 		% check singleton !
	(   Clause == end_of_file
	->  seen
	),
	see(Path),
	repeat,
	read_variables(SystemStructure, Bindings),
	source_location(_, CurrentLine),
	(   SystemStructure == end_of_file
	->  seen
	;   assertz(SystemStructure),
	    do_bindings(Bindings),
	    (	system_structure_slots(SystemStructure, _, _, _, _)
	    ->  recordz(library_index, SystemStructure/CurrentLine)
	    ;   write_ln('*** Warning: not a systemstructure in library (asserted anyway):'),
		write_ln(SystemStructure)
	    ),
	    fail
	),
	check_library,
	told_more, !.

read_input_system:-
        (  current_prolog_flag( bbwww_garp, true )
        -> xml_comment( 2, ['reading and checking input systems ..'], garpinfo)
        ;  write_ln('reading and checking input systems .. ')
        ),
	tell_more,
	retractall(smd(_, _, _, _, _, _)),
	(recorded_erase(input_system_index, _),
	  fail
	  ;
	  true
	),
	absolute_file_name(garp_library(input_system),
			   [ access(read),
			     file_errors(true),
			     extensions([gp, ''])
			   ],
			   Path),
	see(Path),
	repeat,
	read_clause(Clause), 		% check singleton !
	(	Clause == end_of_file ->
	  	seen
	),
	see(Path),
	repeat,
	read_variables(InputSystem, Bindings),
	source_location(_, CurrentLine),
	(	InputSystem == end_of_file ->
	  	seen
	  	;
	  	assertz(InputSystem),
	  	do_bindings(Bindings),
	  	(InputSystem = smd(_, _, _, _, _, _) ->
	  		recordz(input_system_index, InputSystem/CurrentLine)
	  		;
	  		write_ln(
'*** Warning: not an input system model in library (asserted anyway):'),
	  		write_ln(InputSystem)
	  	),
	  	fail
	),
	check_input_system,
	told_more,
	!.

% call list of Variable = Name returned by read_variables

do_bindings([H|T]):- H, do_bindings(T).
do_bindings([]).

edit_system_structure:-
	findall(Name,
		(recorded(library_index, Structure/_),
		 system_structure_slots(Structure, Name, _, _, _)
		),
		Names),
	sort(Names, SortedNames),
	multi_column_menu('edit system structure', SortedNames,
				[m/'main menu'], [], COption/CDesc, false),
	(COption \= m -> edit_system_structure(CDesc) ; true),
	!.

edit_input_system:-
	findall(Name,
		recorded(input_system_index, smd(Name, _, _, _, _, _) /_),
		Names),
	sort(Names, SortedNames),
	multi_column_menu('edit input system model', SortedNames,
				[m/'main menu'], [], COption/CDesc, false),
	(COption \= m -> edit_input_system(CDesc) ; true),
	!.

show_library_structure:-
	findall(Name,
		(recorded(library_index, Structure/_),
		 system_structure_slots(Structure, Name, _, _, _)
		),
		Names),
	sort(Names, SortedNames),
	multi_column_menu('show system structure(s)', SortedNames,
				[m/'main menu'], [], ChosenList, true),
	(common_select(ChosenList, m/_, RChosenList)
	  ;
	  ChosenList = RChosenList),
	tell_more,
	forall(member(_/CDesc, ChosenList),
	 	show_library_structure(CDesc)),
	told_more,
	!.

show_input_model:-
	findall(Name,
		recorded(input_system_index, smd(Name, _, _, _, _, _)/_),
		Names),
	sort(Names, SortedNames),
	multi_column_menu('show input model(s)', SortedNames,
				[m/'main menu'], [], ChosenList, true),
	(common_select(ChosenList, m/_, RChosenList)
	  ;
	  ChosenList = RChosenList),
	tell_more,
	forall(member(_/CDesc, ChosenList),
	 	show_input_model(CDesc)),
	told_more,
	!.

edit_system_structure(Name) :-
	recorded(library_index, Structure/LineS),
	system_structure_slots(Structure, Name, _, _, _),
	current_library(Library),
	(    edit_file(garp_library(Library), LineS),
	     write_ln('Reloading library..'),
	     write_ln('*** Warning: no attempt is made to find consequences of any change'),
	     write_ln('for the current state. When in doubt: choose new input model'),
	     write_ln('from the main menu.'),
	     read_library
	;    write_ln('No change in library.'),
	     true
	), !.

edit_input_system(Name) :-
	recorded(input_system_index, smd(Name, _, _, _, _, _)/Line),
	int_to_atom(Line, LineS),
	(    edit_file(garp_library(input_system), LineS)
	->   write_ln('Reloading input models..'),
	     write_ln('*** Warning: no attempt is made to find consequences of any change'),
	     write_ln('for the current state. When in doubt: choose new input model'),
	     write_ln('from the main menu.'),
	     read_input_system
	;    write_ln('No change in input systems.')
	), !.

edit_file(File, Line) :-
	absolute_file_name(File, Path),
	edit_file_(Path, Line).

edit_file_(File, Line) :-
	user:edit_source(File:Line: - :0), !.
edit_file_(File, Line) :-
	(   getenv('EDITOR', Editor)
	->  true
	;   '$edit':'$default_editor'(Editor)
	),
	'$edit':edit_command(Editor, File, Line, -, Command),
	shell(Command).


/* ----------------------- show structures ------------------------- */

% library struture: system structure with variables bound to names
show_library_structure(Name) :-
	recorded(library_index, Structure/_),
	system_structure_slots(Structure, Name, _, _, _),
	show_system_structure(0, Structure),
	write_ln('.'),
	nl.

show_input_model(Name) :-
	recorded(input_system_index, smd(Name, SE, P, V, R, SS)/_),
	show_smd(0, smd(Name, SE, P, V, R, SS, nil),
		[system_elements, parameters, par_values,
		par_relations, system_structures]),
	write_ln('.'),
	nl.

show_system_structure(Indent, Structure) :-
	system_structure_slots(Structure, Name, Isa, Cond, Giv),
	NI is Indent + 4,
	tab(Indent),
	write('system_structures('),
	write(Name),
	write_ln(', '),
	show_term_arg_list(NI, isa(Isa)),
	write_ln(', '),
	show_term_arg_list(NI, conditions(Cond)),
	write_ln(', '),
	show_term_arg_list(NI, givens(Giv)),
	write(')'),
	!.

show_state(Name, Items) :-
	state(Name, SMD),
	state_status(Name, Status),
	state_to(Name, ToList),
	state_from(Name, From),
	write('state('),
	write(Name),
	write(', '),
	write(Status),
	(memberchk(from, Items) ->
		write_ln(', '),
		tab(4),
		write(From)
		;
		true),
	(memberchk(to, Items) ->
		write_ln(', '),
		tab(4),
		(ToList == [] -> write('[]') ;
			write_ln('['),
			show_to_list(8, ToList),
			write(']')
		)
		;
		true
	),
	(member(Some, Items),
	  memberchk(Some, [ system_elements,
	  			parameters,
	  			par_values,
	  			par_relations,
	  			system_structure_names,
	  			system_structures,
	  			input_model ]) ->
	  	write_ln(', '),
		show_smd(4, SMD, Items)
		;
		true
	),
	write_ln(').'),
	!.

% show a term with single argument which is a list
% show each term of the same kind indented + 4
show_term_arg_list(I, Term) :-
	Term =.. [ Functor, List ],
	is_list(List),
	!,
	tab(I),
	(List == [] ->
	  write(Term)
	  ;
	  write(Functor),
	  write_ln('([ '),
	  NI is I + 4,
	  show_arg_list(NI, List),
	  tab(I),
	  atom_length(Functor, L),
	  tab(L),
	  write('  ])')
	).

% term is a system structure

show_term_arg_list(I, S) :-
	system_structure_slots(S, _, _, _, _),
	!,
	show_system_structure(I, S).

% write term anyway if not a term with a single list argument

show_term_arg_list(I, Term) :-
	tab(I),
	write(Term).

% show elements of list

show_arg_list(_, []).
show_arg_list(Indent, [H|T]) :-
	show_term_arg_list(Indent, H),
	(T \= [] ->
		write_ln(', '),
		show_arg_list(Indent, T)
		;
		nl
	).

% show termination/transition structures

show_to_list(_, []).
show_to_list(Indent, [to(Causes, Conditions, Results, ToStates, Status)|T]) :-
	tab(Indent),
	write_ln('to('),
	NI is (Indent + 4),
	show_term_arg_list(NI, Causes),
	write_ln(', '),
	show_term_arg_list(NI, Conditions),
	write_ln(', '),
	show_term_arg_list(NI, Results),
	write_ln(', '),
	show_term_arg_list(NI, ToStates),
	write_ln(', '),
	tab(NI),
	write(Status),
	write(')'),
	(T \= [] -> write_ln(', '), show_to_list(Indent, T) ; true).

% show system model description

show_smd(Indent, SMD, Items) :-
	smd_slots2(SMD, Name, SE, P, V, R, SS, IS),
	tab(Indent),
	write('smd('),
	write(Name),
	write_ln(', '),
	NI is Indent + 4,
	(memberchk(system_elements, Items) ->
		show_term_arg_list(NI, SE),
		write_ln(', ')
		;
		true
	),
	(memberchk(parameters, Items) ->
		show_term_arg_list(NI, P),
		write_ln(', ')
		;
		true
	),
	(memberchk(par_values, Items) ->
		show_term_arg_list(NI, V),
		write_ln(', ')
		;
		true
	),
	(memberchk(par_relations, Items) ->
		show_term_arg_list(NI, R),
		write_ln(', ')
		;
		true
	),
	(memberchk(system_structure_names, Items),
	  \+ memberchk(system_structures, Items) ->
	  	SS = system_structures(SSL),
	  	findall(SName,
	  		(member(Str, SSL),
	  	  	  system_structure_slots(Str, SName, _, _, _)
	  		), SNames),
	  	show_term_arg_list(NI, system_structures(SNames)),
	  	write_ln(', ')
	  	;
	  memberchk(system_structures, Items) ->
		show_term_arg_list(NI, SS),
		write_ln(', ')
	  	;
	  	true
	),
	(IS = nil -> tab(Indent), write('_)')
		;
		(memberchk(input_model, Items) ->
			show_smd(NI, IS, [system_elements, parameters,
				par_values, par_relations, system_structures]),
			write(')')
			;
			tab(NI),
			write('_)')
		)
	).


/* ---------------- toggle visible parameters menu ------------------- */

toggle_visible_parameters(Header) :-
	findall(qspace(Instance, Par, Space, Visible)/Item,
		(	qspace(Instance, Par, Space, Visible),
			(      (state(_, SMD),
				smd_slots(SMD, _, _, P, _, _, _, _),
				member(SomePar, P),
				parameter(SomePar, _, _, _, Instance, _, _)) ->
				true
				;
				retract(qspace(Instance, Par, Space, Visible)),
				fail
			),
			parameter_visible_item(Par, Item)),
		QSpacesItems),
	findall(Visible/Item,
		member(qspace(_, _, _, Visible)/Item, QSpacesItems),
		InitialSelections),
	toggle_menu(Header,
		InitialSelections,
		(c)/'cancel',
		d/'done',
		Selections),
	% fails if c was selected
	retractall(qspace(_, _, _, _)),
	forall(member(qspace(Instance, Par, Space, _)/Item, QSpacesItems),
		(	memberchk(Visible/Item, Selections),
			assertz(qspace(Instance, Par, Space, Visible))
		)),
	!.

parameter_visible_item(Par, Item) :-
	parameter(Par, _, _, SE, Inst, _, _),
	term_to_atom(SE, SEatom),
	concat_atom([Inst, '(', SEatom, ')' ], Item),
	!.

np :- cls.


/* ---------------  algorithm option switches menu  --------------------- */

% 18-2-2004 FL: algorithm option switches menu
% In the Garp 2.0 engine, some options in the algorithm can be
% switched on and off, flags are used with the values true and fail,
% these values match the generic toggle_menu structure:

% 12-9-2004 FL: All code (including the menu) associated with the algorithm option switches
% has moved to grprefs.pl

/* ----------------------  generic menus -------------------------------- */

% multi column menu of numbered items, followed by some alternative options
% the number of columns is determined by the longest item and
% the width of the screen
% 1+: Header
% 2+: List of descriptions (showed with number, starting at 1)
% 3+: List of Option/Description
% 4+: List of options (no description): not shown, but possible to choose
% 5-  Option/Description chosen. if MultiInput: list
% 6+: MultiInput: true or false

multi_column_menu(Header, List, AlternativeOptions, Invisible, Chosen, MultiInput) :-
	noprotocol,
	number_list(1, List, NumberList), 	% append prefix number/option
	append(NumberList, AlternativeOptions, AllOptions),
	longest_menu_item(0, 0, AllOptions,
		LengthLongestOption, LengthLongestDescription),
	tty_fold(Fold, -1),
	window_size(_, Width),
	NCol1 is Width //
		 (LengthLongestOption + LengthLongestDescription + 2),
	max(NCol1, 1, NumberColumns),
	repeat,
	display_menu_header(Header),
	multi_column_menu_display(NumberColumns, AllOptions,
		LengthLongestOption, LengthLongestDescription),
	tty_fold(_, Fold),
	get_menu_input(AllOptions, Invisible, Chosen, MultiInput),
	Chosen \= redo,
	start_protocol,
	!.

% except for layout the same a multi column

single_column_menu(Header, List, AlternativeOptions, Invisible, Chosen, MultiInput) :-
	noprotocol,
	number_list(1, List, NumberList), 	% append prefix number/option
	append(NumberList, AlternativeOptions, AllOptions),
	longest_menu_item(0, 0, AllOptions,
		LengthLongestOption, _),
	repeat,
	tty_fold(Fold, -1),
	display_menu_header(Header),
	forall(member(Item, AllOptions),
		(display_menu_item(Item, LengthLongestOption, 0), nl)),
	tty_fold(_, Fold),
	get_menu_input(AllOptions, Invisible, Chosen, MultiInput),
	Chosen \= redo,
	start_protocol,
	!.

% as multi column menu, but show only options.
% if user types ? show as multi column menu

small_menu(Header, List, AlternativeOptions, Invisible, RChosen, MultiInput) :-
	noprotocol,
	length(List, Length),
	number_list(1, List, NumberList), 	% append prefix number/option
	append(NumberList, AlternativeOptions, AllOptions),
	append(AllOptions, [ (?) / big_menu ], SAllOptions),
	repeat,
	write(Header),
	write(': '),
	(Length > 0 -> write('1 - '), write(Length), write(', '); true),
	forall(member(Option/_, AlternativeOptions),
		(write(Option), write(', '))),
	write('? '),
	get_menu_input(SAllOptions, Invisible, Chosen, MultiInput),
	Chosen \= redo,
	((Chosen = (?)/big_menu ; Chosen = [(?)/big_menu]) ->
		multi_column_menu(Header, List, AlternativeOptions,
			Invisible, RChosen, MultiInput)
		; Chosen = RChosen),
	start_protocol,
	!.

% toggle a list of items
% 2+ ToggleList: true/item or fail/item: selected or not selected
% 3+ CancelOption: if chosen: fail
% 4+ DoneOption: use selections
% 5+ Return true/item or fail/item selections
% alternative options: all: select all; none: deselect all; rev: reverse all
% the order of input is important: e.g.
%     all 4 <return> will select all and then deselect 4

toggle_menu(Header, ToggleList, CancelOption, DoneOption, NewToggleList) :-
	multi_column_menu(Header, ToggleList,
		[all/'select all', none/'deselect all', rev/'reverse all',
			CancelOption, DoneOption],
		[], Chosen, true),
	!,
	(memberchk(CancelOption, Chosen) ->
	  	fail; true
	),
	(common_select(Chosen, DoneOption, RChosen) ->
	  toggle_list(RChosen, ToggleList, NewToggleList)
	  ;
	  toggle_list(Chosen, ToggleList, TToggleList),
	  toggle_menu(Header, TToggleList, CancelOption, DoneOption,
			NewToggleList)
	),
	!.

toggle_list([], Tio, Tio).

toggle_list([all/_|T], Tin, Tout) :-
  	findall(true/D, member(_/D, Tin), Tnew),
  	!,
  	toggle_list(T, Tnew, Tout).

toggle_list([none/_|T], Tin, Tout) :-
  	findall(fail/D, member(_/D, Tin), Tnew),
  	!,
  	toggle_list(T, Tnew, Tout).

toggle_list([rev/_|T], Tin, Tout) :-
  	findall(Reverse/D, (member(Status/D, Tin),
  			     reverse_status(Status, Reverse)), Tnew),
	!,
  	toggle_list(T, Tnew, Tout).

toggle_list([N/_|T], Tin, Tout) :-
	toggle_item(N, Tin, Tnew),
	!,
	toggle_list(T, Tnew, Tout).

toggle_item(1, [ Status/D | T ], [ Reverse /D | T ]) :-
	reverse_status(Status, Reverse).

toggle_item(N, [ H | T ], [ H | NT ]) :-
	succ(NN, N),
	toggle_item(NN, T, NT).

reverse_status(fail, true).
reverse_status(true, fail).


/* ----------- display header centered and surrounded by hyphens --------- */

display_menu_header(Header) :-
	window_size(_, Width),
	display_length(Header, Length),
	Before is (Width - Length - 2) // 2,
	After is (Width - Length - Before),
	header_on(HowOn/HowOff),
	write(HowOn),
	tab(Before),
	write(Header),
	tab(After),
	write(HowOff),
	nl,
	!.

% find length of longest option and description

longest_menu_item(LLI, LLD, [], LLI, LLD).
longest_menu_item(PIL, PDL, [I/D|T], LLI, LLD) :-
	display_length(I, LI),
	((D = true/DD ; D = fail/DD) ->	% toggle item
		display_length(DD, LD1),
		select_on(_/_/_/_/WidthSelect),
		LD is LD1 + WidthSelect
		;
		display_length(D, LD)
	),
	max(PIL, LI, NewMaxI),
	max(PDL, LD, NewMaxD),
	longest_menu_item(NewMaxI, NewMaxD, T, LLI, LLD).

% length of atom

display_length(Atom, Ln) :-
	atomic(Atom), 	% integers, floats, strings, atoms
	atom_length(Atom, Ln).

% length of term
display_length(Term, Ln) :-
	term_to_atom(Term, A),
	atom_length(A, Ln),
	!.

display_length(Term, Ln) :-
	Term =.. [F|L],
	atom_length(F, LF),
	display_length_args(L, L1),
	length(L1, L2),
	Ln is LF + L1 + (L2 - 1) + 2.

display_length_args([], 0).
display_length_args([H|T], L) :-
	display_length_args(T, L1),
	display_length(H, L2),
	L is L1 + L2.



% display multi column menu
% 1+ number of columns
% 2+ Option/Description list
%   (if Description = true/Desc display '* Desc'
%                      fail/Desc         '  Desc'
% 3+ length of longest option
% 4+ length of longest description

multi_column_menu_display(NC, Options, LOption, LDesc) :-
	number_list(1, Options, NumberedOptions),
	% because memberchk(N/Option, NumberList) is faster than
	% nth1(N, List, Option)
	length(Options, LengthMenu),
	window_size(WinL, _),
	(LengthMenu mod NC > 0 -> Lines is (LengthMenu // NC) + 1
				;  Lines is (LengthMenu // NC)),
	(	between(1, Lines, CurrentLine),
		(CurrentLine is WinL - 2 ->
			write('Hit a key'),
			get_single_char(_),
			put(13),
			write('          '),
			put(13)
			;
			true
		),
		(	between(1, NC, CurrentColumn),
			Item is ((CurrentColumn - 1) * Lines) + CurrentLine,
			memberchk(Item/Option, NumberedOptions),
			% fails if last line doesn't have all columns
			(   CurrentColumn == NC ->
			    display_menu_item(Option, LOption, 0)
			    ;
			    display_menu_item(Option, LOption, LDesc)
			),
			fail
		;
			true
		),
		nl,
		fail
	;
		true
	).

% display toggle item: option toggle description

display_menu_item(Option/(Toggle/Description), LOption, LDesc) :-
	display_length(Option, DLO),
	display_length(Description, DLD1),
	select_on(HowOn/HowOff/HowNotOn/HowNotOff/Width),
	item_on(HowItemOn/HowItemOff),
	DLD is DLD1 + Width,
	write(HowItemOn),
	write(Option),
	write(HowItemOff),
	Tab1 is LOption - DLO + 1,
	(Tab1 > 0 -> tab(Tab1); true),
	(Toggle ->
		write(HowOn),
		write(Description),
		write(HowOff)
		;
		write(HowNotOn),
		write(Description),
		write(HowNotOff)
	),
	Tab2 is LDesc - DLD + 1,
	(Tab2 > 0 -> tab(Tab2); true),
	!.

% display normal item: option description

display_menu_item(Option/Description, LOption, LDesc) :-
	display_length(Option, DLO),
	display_length(Description, DLD),
	item_on(HowItemOn/HowItemOff),
	write(HowItemOn),
	write(Option),
	write(HowItemOff),
	Tab1 is LOption - DLO + 1,
	(Tab1 > 0 -> tab(Tab1); true),
	write(Description),
	Tab2 is LDesc - DLD + 1,
	(Tab2 > 0 -> tab(Tab2); true),
	!.

% prefix list with item number

number_list(_, [], []).
number_list(N, [H|T], [N/H|NT]) :-
	succ(N, NN),
	number_list(NN, T, NT).

/* ---------------------- get input ------------------------ */

/*  at prompt input of a shell command is allways possible,
    preceded by a '!'. After execution 'redo' is returned,
    which tells the menu procedure to redraw and prompt again.

    If MultiInput is false, a single option must be entered, which
    must be in the OptionList (Option/Description), or in the
    Invisible list (no descriptions). If not: write ?, prompt again.
    Return is ChosenOption/ChosenDescription

    If MultiInput is true, more than one options, separated by white
    space can be entered, which must all be in the optionlists.
    Return is a list of ChosenOption/ChosenDescription
*/

% 1+ list of Option/Description
% 2+ list of Options (descriptions not shown)
% 3- Return Option/Description (or list of 4+ MultiInput == true)
% if multi input: prompt  >> else prompt  >
% if shell comand: execute and return redo (redraw menu)

get_menu_input(OptionList, Invisible, Return, MultiInput) :-
	repeat,
	(MultiInput == true -> write(>) ; true),
	write('> '),
	read_ln(OptionString),
	strip_leading_trailing_blanks(OptionString, StrippedString),
	(shell_command(StrippedString) -> Return = redo ;
	  check_menu_input(MultiInput, StrippedString,
	  	OptionList, Invisible, Return) -> true ;
	  write_ln(?), fail),
	!.

% single input: option in option lists
% note that CDesc will remain unbound if option is in Invisible list

check_menu_input(false, InputString, OptionList, Invisible, COption/CDesc) :-
	!,
	name(COption, InputString),
	(memberchk(COption/CDesc, OptionList);
	  memberchk(COption, Invisible)).

% multiple input: all options in option lists

check_menu_input(true, InputString, OptionList, Invisible, ReturnList) :-
	split_at_blanks(InputString, [], ChosenOptionNames),
	!,
	forall(member(Option, ChosenOptionNames),
		(	memberchk(Option/_, OptionList);
	  		memberchk(Option, Invisible))),
	findall(COption/CDesc,
		 (member(COption, ChosenOptionNames),
		      (	memberchk(COption/CDesc, OptionList);
	          	memberchk(COption, Invisible)) ),
	         ReturnList).

% read until return or newline

read_ln(S) :-
	get0(C),
	(C = -1 -> halt ; true),
	((C = 10 ; C = 13) -> S = [] ; S = [C|T], read_ln(T)).

read_filename(FileName) :-
	repeat,
	write('Enter filename (c to cancel) > '),
	read_ln(String),
	(String = [] ->
		write_ln(?),
		fail
		;
		true
	),
	strip_leading_trailing_blanks(String, StrippedString),
	(shell_command(StrippedString) ->
		fail;
		name(FileName, StrippedString)
	),
	!,
	FileName \== (c).

max(A, B, A):- A >= B.
max(A, B, B):- B > A.

% split string into argument strings

% FL januari 2004: transition '1e' is interpreted as 1 EXP 0 wich is  1.0,
% Clauses added to catch '1e' to '99e': Marked EBUG
% FL march 2004: new transition notation '1t1' style does not have this problem.

split_at_blanks([], [], []).

% EBUG
split_at_blanks([], [X, 101], [N]):-
    name(NX, [X]),
    atom_concat(NX, 'e', N).

% EBUG
split_at_blanks([], [X, Y, 101], [N]):-
    name(NXY, [X, Y]),
    atom_concat(NXY, 'e', N).

split_at_blanks([], L, [N]) :-
	name(N, L).

split_at_blanks([H|T], [], NT) :-
	H =< 32,
	split_at_blanks(T, [], NT).

% EBUG
split_at_blanks([H|T], [X, 101], [N|NT]) :-
	H =< 32,
    name(NX, [X]),
    atom_concat(NX, 'e', N),
	split_at_blanks(T, [], NT).

% EBUG
split_at_blanks([H|T], [X, Y, 101], [N|NT]) :-
	H =< 32,
    name(NXY, [X, Y]),
    atom_concat(NXY, 'e', N),
	split_at_blanks(T, [], NT).

split_at_blanks([H|T], L, [N|NT]) :-
	H =< 32,
	name(N, L),
	split_at_blanks(T, [], NT).

split_at_blanks([H|T], P, NT) :-
	append(P, [H], NP),
	split_at_blanks(T, NP, NT).

% strip leading/trailing blanks

strip_leading_trailing_blanks(S, S2) :-
	strip_leading_blanks(S, S1),
	strip_trailing_blanks(S1, S2),
	!.

strip_leading_blanks([B|T], NT) :-
	B < 33,
	strip_leading_blanks(T, NT).
strip_leading_blanks(L, L).

strip_trailing_blanks([], []).
strip_trailing_blanks([H|T], L) :-
	strip_trailing_blanks(T, NT),
	(NT = [], H < 33 -> L = [] ; L = [H|T]).

% if input starts with !: execute shell command (otherwise fail)

shell_command([0'!|CommandString]) :-
	strip_leading_blanks(CommandString, StrippedCommandString),
	name(Command, StrippedCommandString),
	common_shell(Command, Exit),
	(Exit < 0 -> write('shell error code: '), write_ln(Exit); true),
	!.

/* --------------------  show hierarchies -------------------------- */

show_isa_hierarchy:-
	show_hierarchy(0, nil, isa(Child, Parent), Child, Parent, isa).


show_isa_ss_hierarchy:-
	forall(member(Root, [ description_view,
				composition_view,
				decomposition_view,
				process,
				qualitative_state ]),
		show_hierarchy(0, Root,
			(system_structures(Child, isa(Parents), _, _),
		  	memberchk(Parent, Parents) ), Child, Parent,
		  	system_structure)).

show_applies_to_ss_hierarchy:-
	% find roots:
	findall(Root,
		(system_structures(Root, _, _, _),
		  % no system structure mentions it in conditions:
		  \+ (	system_structures(_, _, conditions(Cond1), _),
			member(system_structures(S1), Cond1),
		  	memberchk(Root, S1))
		),
		Roots),
	sort(Roots, SortedRoots),
	forall(member(Name, SortedRoots),
		show_hierarchy(0, Name,
		        % Relation: Parent mentions Child in conditions
			(system_structures(Parent, _, conditions(Cond), _),
			member(system_structures(S), Cond),
		  	member(Child, S) ), Child, Parent, system_structure)).

% show hierarchy for any relation (sorted to the standard order of terms)
% if multiple inheritance occurs, the child will occur below
% every parent

% 1+ Indentlevel
% 2+ Current Parent
% 3+ Relation between Child and Parent
% 4+ Child variable in 3
% 5+ Parent variable in 3
% 6+ Kind of hierarchy (how to write Parent, see print_kind)

show_hierarchy(Tab, Parent, Relation, RelationChild, RelationParent, Kind) :-
	hierarchy_tab(Tab),
	print_kind(Kind/Parent),
	nl,
	succ(Tab, NewTab),
	findall(RelationChild,
		(RelationParent = Parent,  Relation),
		Children),
	sort(Children, SortedChildren),
	forall(member(AChild, SortedChildren),
		show_hierarchy(NewTab, AChild, Relation, RelationChild,
			RelationParent, Kind)),
	!.

hierarchy_tab(0).
hierarchy_tab(N) :-
	succ(NN, N),
	write('|  '),
	hierarchy_tab(NN).


% portray system structure name :

print_kind(system_structure/Name) :-
	(recorded(library_index, Structure/_),  % variables bounded
	  system_structure_slots(Structure, Name, _, _, _),
	  write(Name)
	  ;
	  write(Name)
	),
	!.

print_kind(isa/Name):- write(Name).


/* ----- some simple checks on library and input model  ---------- */

/*
 *  checked are:
 *
 *  are parents and system structure conditions declared ?
 *  are generic concepts of instances declared?
 *  is a parameter declared twice ? (bit silly)
 *  is a parameter declared before its value?
 *  is a parameter's quantity space declared?
 *  are instantiated values part of the quantity space?
 *  are relations/expressions known?
 *  are parameters/constants in expressions known?
 *  (assumed is that a constant must be in a quantity space of a known par.)
 *  do the conditions specify only inequality relations?
 *  syntax specifications of fields, parameters, values, relations
 *
 *  not checked are:
 *  import/export behaviour
 *  system_element relations
 *  and probably a few other things
 */

check_library:-
	forall(recorded(library_index, Structure/_),
		check_system_structure(Structure)).

check_input_system:-
	forall(recorded(input_system_index, SMD/_),
		check_input_system(SMD) ).

check_system_structure(S) :-
	flag(ss_error, _, no),
	system_structure_slots(S, Name, Isa, Conditions, Givens),
	check_ss_isa(Isa),
	append(Conditions, Givens, All),
	check_ss_all(All, []),
	forall(member(par_relations(R), Conditions),
		check_condition_relations(R)),
	(flag(ss_error, yes, yes) ->
		show_library_structure(Name)
		;
		true
	),
	!.

check_input_system(smd(Name, SE, P, V, R, SS)) :-
	flag(ss_error, _, no),
	check_ss_all([SE, P, V, R, SS], []),
	(flag(ss_error, yes, yes) ->
		show_smd(4, smd(Name, SE, P, V, R, SS, nil),
			[system_elements, parameters, par_values,
				par_relations, system_structures])
		;
		true
	),
	!.

% check if isa list exists

check_ss_isa([]).

check_ss_isa([H|T]) :-
	memberchk(H, [view, description_view, composition_view,
		decomposition_view, qualitative_state, process ]),
	!,
	check_ss_isa(T).

check_ss_isa([H|T]) :-
	system_structures(H, _, _, _),
	!,
	check_ss_isa(T).

check_ss_isa([H|T]) :-
	write('*** Warning: unknown parent '),
	write_ln(H),
	flag(ss_error, _, yes),
	check_ss_isa(T).

% check conditions and givens

check_ss_all([], _).

check_ss_all([system_elements(L)|T], P) :-
	check_ss_system_elements(L),
	check_ss_all(T, P).

check_ss_all([parameters(L)|T], P) :-
	check_ss_par(L, Instances),
	(member(Some, Instances),
	  member(Some, P),
	  write('*** Warning: parameter occurs twice: '),
	  write_ln(Some),
	  flag(ss_error, _, yes),
	  fail
	  ;
	  true
	),
	append(P, Instances, NP),
	check_ss_all(T, NP).


check_ss_all([par_values(L)|T], P) :-
	check_ss_par_values(L, P),
	check_ss_all(T, P).

check_ss_all([par_relations(L)|T], P) :-
	check_ss_par_relations(L, P),
	check_ss_all(T, P).

check_ss_all([system_structures(L)|T], P) :-
	check_ss_system_structures(L),
	check_ss_all(T, P).

check_ss_all([H|T], P) :-
	write('*** Error: unknown field '),
	write_ln(H),
	flag(ss_error, _, yes),
	check_ss_all(T, P).

% check system elements: do generic names exist?

check_ss_system_elements([]).

check_ss_system_elements([instance(P, Generic)|T]) :-
	(\+ isa(Generic, _) ->
		write('*** Warning: generic concept not defined in isa hierarchy:'),
		write_ln(instance(P, Generic)),
		flag(ss_error, _, yes)
		;
		true),
	check_ss_system_elements(T).

% not very much to say about other relations

check_ss_system_elements([_|T]) :-
	check_ss_system_elements(T).

% check parameters: legal structure; does quantity space exist

check_ss_par([], []).

check_ss_par([H|T], [I/Space|T2]) :-
	parameter(H, _, Genetic, _, I, _, Space),
	!,
	(get_quantity_space(Genetic, I, Space, _, _) ->
	  true
	  ;
	  write('*** Warning: can not find quantity space for'),
	  write_ln(H),
	  flag(ss_error, _, yes)
	),
	check_ss_par(T, T2).

check_ss_par([H|T], T2) :-
	write('*** Error: not a legal parameter: '),
	write_ln(H),
	flag(ss_error, _, yes),
	check_ss_par(T, T2).

% check values: legal structure ; is parameter declared before ; check values

check_ss_par_values([], _).

check_ss_par_values([ value(I, Q, V, D) | T ], P) :-
	(memberchk(I/S, P) ->
		check_value(V, S),
		check_value(D, mzp)
	 	;
write_ln('*** Error: parameter instance must be declared before value is used:'),
		write_ln(value(I, Q, V, D)),
		flag(ss_error, _, yes)
	),
	check_ss_par_values(T, P).

check_ss_par_values([H|T], P) :-
	write('*** Error: not a legal value: '),
	write_ln(H),
	flag(ss_error, _, yes),
	check_ss_par_values(T, P).

% check value: variable or occurs in quantity space

check_value(Name, _) :-
	var(Name).	% only if don't care was used

check_value(Name, _) :-
	atomic(Name),
	name(Name, [H|_]), 	% a variable name
	(name('_', [H]) ; H >= 65, H =< 90).

check_value(Name, Space) :-
	quantity_space(Space, _, SpaceL, _),
	(member(Name, SpaceL) ; member(point(Name), SpaceL)).

check_value(Name, Space) :-
	write('*** Warning (possible error): specified value '),
	write(Name),
	write('not in quantity space '),
	write_ln(Space),
	flag(ss_error, _, yes).

% check relations

check_ss_par_relations([], _).

% inequality relation: parse it, quantities ok?

check_ss_par_relations([H|T], P) :-
	inequality_type(H),
	flag(q_cnt, _, 0),
	cio_empty(Cin),
	(intern_representation(H, _, Cin, Cout) ->
		cio_q(Cout, Quantities),
		check_quantities(Quantities, P)
		;
		write('*** Error: Unknown expression or relation: '),
		write_ln(H),
		flag(ss_error, _, yes)
	),
	check_ss_par_relations(T, P).


/*** FL 3-3-2004: Old correspondence types checking ***************************
                  before adding new correspondence primitives for garp 2.0

                  NB> check_ss_par_relations continued below! <

% correspondence types

check_ss_par_relations([H|T], P) :-
	(H = q_correspondence(P1, P2) ; H = dir_q_correspondence(P1, P2)),
	(	memberchk(P1/_, P),
		memberchk(P2/_, P)
	;
		write('*** Error: parameters not declared in '),
		write_ln(H),
		flag(ss_error, _, yes)
	),
	check_ss_par_relations(T, P).

check_ss_par_relations([H|T], P) :-
	(	H = v_correspondence(P1, V1, P2, V2)
	 	;
	 	H = dir_v_correspondence(P1, V1, P2, V2)),
	(	memberchk(P1/_, P),
		memberchk(P2/_, P)
	;
		write('*** Error: paramterers not declared in '),
		write_ln(H),
		flag(ss_error, _, yes)
	),
	check_quantities([value(V1)/_, value(V2)/_], P),
	check_ss_par_relations(T, P).

check_ss_par_relations([H|T], P) :-
	(H = if(L1, L2);  H = iff(L1, L2)),
	check_ss_par_relations(L1, P),
	check_ss_par_relations(L2, P),
	check_ss_par_relations(T, P).

*** New correspondence type checking including new primitives garp 2.0 ***/

% FL 3-3-2004: new correspondence primitives:

% d = derivative,
% full = derivatives & values,
% mirror = opposite value correspondence: plus/min, zero/zero min/plus for mzp

% dv_correspondence(_, _, _, _)
% dq_correspondence(_, _)
% dir_dv_correspondence(_, _, _, _)
% dir_dq_correspondence(_, _)
% full_correspondence(_, _)
% dir_full_correspondence(_, _)
% mirror_q_correspondence(_, _)
% dir_mirror_q_correspondence(_, _)
% mirror_dq_correspondence(_, _)
% dir_mirror_dq_correspondence(_, _)


% (possibility of mirroring quantity spaces is now checked upon assertion,
% maybe this needs to be done here. This gives more direct error feedback,
% which is more user friendly)


%correspondence types

check_ss_par_relations([H|T], P) :-
	memberchk(H , [ q_correspondence(P1, P2),
	                dir_q_correspondence(P1, P2),
	                dq_correspondence(P1, P2),
	                dir_dq_correspondence(P1, P2),
	                full_correspondence(P1, P2),
	                dir_full_correspondence(P1, P2),
	                mirror_q_correspondence(P1, P2),
	                dir_mirror_q_correspondence(P1, P2),
	                mirror_dq_correspondence(P1, P2),
	                dir_mirror_dq_correspondence(P1, P2)
	                ]),
	(	memberchk(P1/_, P),
		memberchk(P2/_, P)
	;
		write('*** Error: parameters not declared in '),
		write_ln(H),
		flag(ss_error, _, yes)
	),
	check_ss_par_relations(T, P).

check_ss_par_relations([H|T], P) :-
    memberchk(H, [v_correspondence(P1, V1, P2, V2),
                    dir_v_correspondence(P1, V1, P2, V2),
                    dv_correspondence(P1, V1, P2, V2),
                    dir_dv_correspondence(P1, V1, P2, V2)
                    ]),
	(	memberchk(P1/_, P),
		memberchk(P2/_, P)
	;
		write('*** Error: parameters not declared in '),
		write_ln(H),
		flag(ss_error, _, yes)
	),
	check_quantities([value(V1)/_, value(V2)/_], P),
	check_ss_par_relations(T, P).

% End new correspondence types


check_ss_par_relations([H|T], P) :-
	(H = if(L1, L2);  H = iff(L1, L2)),
	check_ss_par_relations(L1, P),
	check_ss_par_relations(L2, P),
	check_ss_par_relations(T, P).


% influence proportional type

check_ss_par_relations([H|T], P) :-
	influence_proportional_type(H),
	H =.. [ _, P1, P2 ],
	(	memberchk(P1/_, P),
		memberchk(P2/_, P)
	;
		write('*** Error: parameters not declared in '),
		write_ln(H),
		flag(ss_error, _, yes)
	),
	check_ss_par_relations(T, P).

check_ss_par_relations([H|T], P) :-
	write('*** Error: unknown relation: '),
	write_ln(H),
	flag(ss_error, _, yes),
	check_ss_par_relations(T, P).

% system structures: exist?

check_ss_system_structures([]).

check_ss_system_structures([H|T]) :-
	system_structures(H, _, _, _),
	!,
	check_ss_system_structures(T).

check_ss_system_structures([H|T]) :-
	write('*** Warning: unknown system_structure: '),
	write_ln(H),
	flag(ss_error, _, yes),
	check_ss_system_structures(T).

% quantities in a relation: parameter/constant?

check_quantities([], _).
check_quantities([value(H)/_|T], P) :-
	(memberchk(H/_, P)
	;
	  member(_/Space, P),
	  quantity_space(Space, _, SpaceL, _),
	  (memberchk(H, SpaceL) ; memberchk(point(H), SpaceL))
	;
	  write('*** Warning: '),
	  write(H),
  write_ln(' is not declared as a parameter or a constant in a parameter space'),
	  flag(ss_error, _, yes)
	),
	check_quantities(T, P).

check_quantities([derivative(H)/_|T], P) :-
	(memberchk(H/_, P)
	;
	  quantity_space(mzp, _, SpaceL, _),
	  (memberchk(H, SpaceL) ; memberchk(point(H), SpaceL))
	;
	  write('*** Warning: '),
	  write(H),
  write_ln(' is not declared as a parameter or a constant in a parameter space'),
	  flag(ss_error, _, yes)
	),
	check_quantities(T, P).

% relations in conditions must be of inequality type

check_condition_relations([]).

check_condition_relations([H|T]) :-
	inequality_type(H),
	check_condition_relations(T).

check_condition_relations([H|T]) :-
	influence_proportional_type(H),
	write_ln(
'*** Error: relation of influence or proportional type can only appear in givens: '),
	write_ln(H),
	flag(ss_error, _, yes),
	check_condition_relations(T).

check_condition_relations([H|T]) :-
	correspondence_type(H),
	write_ln(
	'*** Error: relation of correspondence type can only appear in givens: '),
	write_ln(H),
	flag(ss_error, _, yes),
	check_condition_relations(T).

% unknown, but that's already signalled
check_condition_relations([_|T]) :-
	check_condition_relations(T).

/* ---------------------------- more ------------------------------- */

/* a direct pipe to more is impossible, because a quit will produce
   a broken pipe error. Therefore: write everything to scratch file.
   A pipe to cat is used, if protocol is active.
   When done: call user's favourite pager.

   Writing must be done between tell_more and told_more calls.
   If the write procedure fails, stop output to scratch file (pager not called)

*/

tell_more :-
	current_prolog_flag(www_garp, true), !,
	open_null_stream(Out),
	(   set_output(Out)
	;   set_output(user_output),
	    close(Out),
	    fail
	).
tell_more:-
  	tell(garp_scratch).
tell_more:-
	told,
	fail.

told_more :-
	current_prolog_flag(www_garp, true), !,
	current_output(Dummy),
	set_output(user_output),
	close(Dummy).
told_more:-
	told,
	empty_file(garp_scratch), !.

told_more:-
	noprotocol,
	flag(protocol_file, F, F),
	(   F == 0
	->  true
	;   append_file_to_file(garp_scratch, F)
	),
	more(garp_scratch).


%	empty_file(+File)
%
%	Succeeds if File does not exist, or holds no characters.

empty_file(File) :-
	(   open(File, read, Fd)
	->  (   get0(Fd, C),
	        C == -1
	    ->	true
	    ;	close(Fd),
		fail
	    )
	;   true
	).

append_file_to_file(In, Out) :-
	open(In, read, InFd),
	open(Out, append, OutFd),
	get0(InFd, C),
	append_stream(C, InFd, OutFd),
	close(OutFd),
	close(InFd).

append_stream(-1, _, _) :- !.
append_stream(C, In, Out) :-
	put(Out, C),
	get0(In, C2),
	append_stream(C2, In, Out).


% find favourite pager and use it

more(File) :-
	absolute_file_name(File, [access(read), file_errors(true)], Path),
	(   setting(use_shell, true)
	->  (   getenv('PAGER', Pager)
	    ->  true
	    ;   Pager = more
	    ),
	    concat_atom([Pager, ' ', Path], Command),
	    common_shell(Command)
	;   simple_more(Path)
	).

% simple pager

simple_more(File) :-
	open(File, read, Fd),
	(   protocolling(File)
	->  noprotocol
	;   true
	),
	my_more(" ", Fd),
	(   nonvar(File)
	->  protocola(File)
	;   true
	),
	close(Fd).

my_more(Cmd, Fd) :-
	more_cmd(Cmd, Fd), !,
	get_single_char(C),
	my_more([C], Fd).
my_more(_, _).

more_cmd("q", _) :- !,
	fail.
more_cmd(_, Fd) :-
	at_end_of_stream(Fd), !,
	put(7).
more_cmd(" ", Fd) :- !,
	window_size(R, _),
	Show is R-1,
	show_lines(Show, Fd).
more_cmd("d", Fd) :- !,
	window_size(R, _),
	Show is max(1, R//2),
	show_lines(Show, Fd).
more_cmd([13], Fd) :- !,
	show_lines(1, Fd).
more_cmd([10], Fd) :- !,
	show_lines(1, Fd).
more_cmd(_, _) :-
	put(7).

show_lines(N, Fd) :-
	clear_more_prompt,
	do_show_lines(N, Fd),
	more_prompt(Fd).

do_show_lines(N, Fd) :-
	N > 0, !,
	get0(Fd, C),
	show_line(C, Fd),
	NN is N - 1,
	do_show_lines(NN, Fd).
do_show_lines(_, _).

show_line(13, Fd) :- !,
	show_line(Fd).
show_line(10, _) :- !,
	nl.
show_line(-1, _) :- !,
	nl.
show_line(C, Fd) :-
	put(C),
	get0(Fd, C2),
	show_line(C2, Fd).

more_prompt(Fd) :-
	at_end_of_stream(Fd), !,
	format('--more (END)--').
more_prompt(_) :-
	format('--more--').

clear_more_prompt :-
	put(13),
	format('              '),
	put(13).

/* ----------------------- show_file -------------------------------- */

show_file(File) :-
	absolute_file_name(File, TheFile),
	open(TheFile, read, In),
	get0(In, C0),
	do_cat(C0, In),
	close(In).

do_cat(-1, _) :- !.
do_cat(C, Fd) :-
	put(C),
	get0(Fd, C2),
	do_cat(C2, Fd).


/* ----------------------- print relations (tracing) ---------------- */


/* FL May 2004: because of simple equality- & zero analysis (see solve.pl)
the pointerlist is not useful for simple translation from intern to extern:
too many quantities share the same pointers.
Below is a new approach which prints the elements of the sum,
with a list of relevant options open for interpretattion by the user.

% previous code:

print_relations(L, Cin) :-
	cio_q(Cin, Q),
	forall(member(X, L), (print_relation(X, Q), nl)).

print_relation(relation(Left, Rel, Right), Q) :-
        print_sum(Left, Q),
        tab(1),
        write(Rel),
        tab(1),
        print_sum(Right, Q).


print_sum(L, Q) :-
	map_list(L, T),
	print_terms(T, Q).

print_terms([], _) :-
	write(zero), !.

print_terms([H], Q) :-
	\+ is_list(H),
	memberchk(W/H, Q), !,
	write(W).

print_terms([H|T], Q) :-
	\+ is_list(H), !,
	print_terms([H], Q),
	write(' + '),
	print_terms(T, Q).

% print_terms(_, _) :-
%	trace.

*/

% New Code, May 2004, do not print terms recursively
% but print the sum of indexes followed by an interpretation list
% eg. 1 + 5 (value(Q1)/1, value.. )etc.

% print relation & subsitution at once
% L is list of intern relations, Cin is current cio structure (see intern.pl & selector.pl)
print_relations(L, Cin) :-
	cio_q(Cin, Q),
	forall(member(X, L), (print_relation(X, Q, Subst), print_substitution_list(Subst), nl)).

% print_relation(+Relation, +Q, -Substitutions)
% print Relation, using Q, return substitutions list for use in solve_trace (see solve.pl)
print_relation(relation(Left, Rel, Right), Q, Subst):-
        print_sum(Left, Q, Subst1),
        tab(1),
        write(Rel),
        tab(1),
        print_sum(Right, Q, Subst2),
        smerge(Subst1, Subst2, Subst).

% print_relation(+Relation, +Q)
% print Relation, using Q, followed by substitutions 
print_relation(relation(Left, Rel, Right), Q):-
        print_sum(Left, Q, Subst1),
        tab(1),
        write(Rel),
        tab(1),
        print_sum(Right, Q, Subst2),nl,
        smerge(Subst1, Subst2, Subst),
        print_substitution_list(Subst).

print_sum(L, Q, S) :-
	map_list(L, T),
	print_terms(T, Q, S).

print_terms([], _, []) :-
	write(zero), !.

print_terms(Sum, Q, Substitutions):-
	is_list(Sum),
	findall(Quantity/Index, (member(Index, Sum), member(Quantity/Index, Q)), Substitutions),
	print_index_sum(Sum).

% write indexes as sum of pointers
print_index_sum([X]):-
    write('X'),
    write(X).

print_index_sum([X, Y|Tail]):-
    write('X'),
    write(X),
    write(' + '),
    print_index_sum([Y|Tail]).

% print the list of substitutions with a space and normal brackets
print_substitution_list([]):-
    !.

print_substitution_list(Interpretation):-
    write('-- substitutions: '),
    print_substitution_list_items(Interpretation),
    write_ln(' . ').


print_substitution_list_items([Q/X]):-
    write(Q),
    write('/X'),
    write(X).

print_substitution_list_items([Q/X, Y|Tail]):-
    write(Q),
    write('/X'),
    write(X),
    write(', '),
    print_substitution_list_items([Y|Tail]).

% END NEW print relations code

% New FL July 2004, garp 2.0
% print_derivable_relations( +Der, +Q )
% to print a batch of relations it is nicer to print all relations then all substitutions

print_derivable_relations( List, Q ):-
    print_derivable(List, Q), nl,
    write('-- substitutions: '), nl,
    sort_substitutions(Q, SQ),
    print_substitutions_set(SQ).

print_derivable(L, Q):-
	forall(member(X, L), (tab(2), print_relation(X, Q, _),nl)).

print_substitutions_set(Q):-
    forall(member(X, Q), (tab(2), print_substitution_list_items([X]),nl)).

sort_substitutions(In, Out):-
    add_key_quantities(In, Keyed),
    keysort(Keyed, Sorted),
    add_key_quantities(Out, Sorted).

add_key_quantities([], []).

add_key_quantities([H/I|T], [I-H/I|NT]):-
    add_key_quantities(T, NT).

% END NEW print derivable relations code


print_equalities(Q) :-
	flag(q_cnt, H, H), 	% highest quantity number
	succ(H, HH),
        print_equalities(1, HH, Q).

print_equalities(N, N, _) :- !.
print_equalities(N, H, Q) :-
        common_select(Q, _/N, NQ),
        memberchk(_/N, NQ), !,
        write('Equal quantities: '),
        forall(member(V/N, Q), (write(V), tab(2))),
        nl,
        succ(N, NN),
        print_equalities(NN, H, Q).

print_equalities(N, H, Q) :-
	succ(N, NN),
	print_equalities(NN, H, Q).

/* ---------------------------------------------------------------------- */
