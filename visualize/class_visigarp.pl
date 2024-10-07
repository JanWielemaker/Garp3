/*****************************************************************************************************
Garp3 0.1 Based on main source of visigarp (main.pl). This file is split in parts and edited.
All visualize source based on visigarp 2.05, designed and implemented by Anders Bouwer 18/08/1999
Original copyright: (C) 2004 University of Amsterdam. All rights reserved.
Last updated: 08/05/2006, AB
Now:
PART OF Garp3. SEE COPYRIGHT NOTICE.
All old code except when gp3 mentioned.
*******************************************************************************************************/

%loaded into module (namespace) visualize

:- pce_begin_class(visigarp, framedWindow). %gp3 changed class

% selection_nr indicates how many graphicals have been
% selected during the lifetime of the frame.
% every time a graphical in the frame is selected,
% this graphicals own selection_nr is set to
% Frame?selection_nr + 1, and Frame?selection_nr is
% increased.
% It is used to sort selected graphicals according to
% the order in which they were selected.
%
variable(selection_nr, int:=0, both, "The selection nr. for this frame").
% variable(simulation_view_type, name:='graph', both, "The type of view used for displaying states and transitions").

initialise(VG):->
	%gp3 0.2 new init code, with framedwindow as superclass
	%gp3: the old buttondialog was D1, so we start with D2 now..
	%we do this before initialise, so we can send it along as extra window

	D2 *= dialog(textdialog),
	send(D2, name, textdialog),
	send(D2, alignment, left),

	send(VG, send_super, initialise, 'Simulate',
		buttonbar := vertical,
		extraWindowBelow := D2, %gp3 0.1 changed label and added buttonbar
		helpId := 'Sim_SimulateMain'),
	VG->>icon(@simulate_icon),
	P = VG<<-client,

	%old code: (and a good idea: see state_node->Selected)
	send(P, selection_feedback, @nil),
	VG->>initCommands, %gp3 added this: these are commands now
	VG->>initButtonbar,
	VG->>fill_dialogs_and_menubar(D2, VG?menubar). %gp3 changed this to a method, D1 fallen out
%%



%%
initCommands(VG):->
	%gp3 0.2: store some commands with this frame, to use in the buttonbar
	%we are slowely pushing this class into the vgarp/homer framework
	%menu is not using the commands just now, but the new buttonbar is
	%its not very difficult to rewrite the menu using menuCommand objects
	%
	%3.4.7: we also added commands for the menuItem loadSimulation

	send_list(VG, command, [
		'LoadSimulation', %load simulation
		'StateMode', %state mode
		'PathMode', %path mode
		'SelectAll',
		'DeselectAll',

		'ERStructure', %open e-r structure
		'QuantityValues', %show quantity values
		'ModelFragments', %show modelfragments
		'Dependencies', %show dependencies

		'TransitionHistory',
		'EquationHistory',
		'ValueHistory',

		'TraceWindow',
		'SimulationPreferences',
		'SelectScenario',
		'FullSimulation',
		'TerminateStates',
		'OrderStates',
		'CloseStates'
		]).
%%

%%
initButtonbar(VG):->
	%gp3 fill the command based buttonbar
	%we use the handy shortcut call add for this one

	B = VG<<-buttonBar,
	B->>append(label(slabel, 'Select:', font:=bold)),
	B?slabel_member->>width(0), %make width ok

	StateMode = B<<-addWithRepeat(stateMode,'StateMode',buttonbars,select1,@nil,'Select individual states',below,slabel, toggle := @on, repeat:= @on),
	StateMode->>value(@on),
	PathMode = B<<-add(pathMode,'PathMode',buttonbars,select2,@nil,'Select a path', right, stateMode, toggle:= @on),
	%these 2 make a group
	StateMode->>group(PathMode),

	B<<-add(selectAll,'SelectAll',buttonbars,select3,@default,'Select all states', below, stateMode),
	B<<-add(deselectAll,'DeselectAll',buttonbars,select4,@default,'Deselect all states', right, selectAll),
	B->>append(graphical(0,0,0,8),next_row), %small gap
	B->>append(label(vlabel, 'View:', font:=bold),next_row),
	B?vlabel_member->>width(0), %make width ok

	B<<-add(erStructure,'ERStructure',buttonbars,view1,@default,'Show entities, configurations and attributes'),
	B<<-add(quantityValues,'QuantityValues',buttonbars,view2,@default,'Show quantity values', right, erStructure),
	B<<-add(modelFragments,'ModelFragments',buttonbars,view3,@default,'List model fragments', below, erStructure),
	B<<-add(dependencies,'Dependencies',buttonbars,view4,@default,'Show dependencies', right, modelFragments),
	B->>append(graphical(0,0,0,8),next_row), %small gap

	B<<-add(transitionHistory,'TransitionHistory',buttonbars,history1,@default,'Transition history'),
	B<<-add(equationHistory,'EquationHistory',buttonbars,history2,@default,'Equation history',right,transitionHistory),
	B<<-add(valueHistory,'ValueHistory',buttonbars,history3,@default,'Value history',right,equationHistory),

	B->>append(graphical(0,0,0,8),next_row), %small gap
	B->>append(label(rlabel, 'Run:', font:=bold),next_row),
	B?rlabel_member->>width(0), %make width ok

	B<<-add(traceWindow,'TraceWindow',buttonbars,sim1,@default,'Open trace window'),
	B<<-add(simulationPreferences,'SimulationPreferences',buttonbars,sim2,@default,'Simulation preferences',right,traceWindow),
	B<<-add(selectScenario,'SelectScenario',buttonbars,sim3,@default,'Select a scenario to simulate',below,simulationPreferences),
	B<<-add(fullSimulation,'FullSimulation',buttonbars,sim4,@default,
		when(@model?modelState == legacy, % JL
			'Full simulation',
			create(string,'Full simulation: %s',@app?currentScenario?name)),
		right,selectScenario),
	B<<-add(terminateStates,'TerminateStates',buttonbars,sim5,@default,'Terminate selected states',below,selectScenario),
	B<<-add(orderStates,'OrderStates',buttonbars,sim6,@default,'Order selected states',right,terminateStates),
	B<<-add(closeStates,'CloseStates',buttonbars,sim7,@default,'Find successors for selected states',right,orderStates).
%%

%%
setLabel(VG):->
	%gp3 0.4.9 Set the label according to the currently loaded scenario
	%which we try to get dynamically, so it also works after loading
	%a saved simulation etc

	(
		(
			IS = @app<<-currentScenario,
			IS \== @nil,
			Name = IS<<-name,!
		)
	;
		(
			engine:state(_,smd(input_system(Name),_,_,_,_,_,_)),!
		)
	;
		(
			Name = '[Unknown]'
		)
	),
	VG->>label(string('%s - Simulate', Name)).

%%
selectMode(VG, Mode: {path,states}):<-
	%gp3 fix for getting the mode
	%old code just checked a button in the old dialog, we made this
	%a bit less interface dependent.
	%change interface: change this call
	%and ->selectMode

	B = VG<<-buttonBar,
	if
		@on = B?stateMode_member<<-value
	then
		Mode = states
	else
		Mode = path.
%
selectMode(VG,Mode: {path,states}):->
	%gp3
	%change the the internal mode as reported by <-selectMode
	%also changes interface buttons, but no side-effects

	if
		Mode = path
	then
		VG?buttonBar?pathMode_member->>value(@on)
	else
		VG?buttonBar?stateMode_member->>value(@on).
%%

%%
fill_dialogs_and_menubar(Frame, D2: dialog, MB: framedMenubar) :->
		%gp3: changed this into a method, and changed MB class.
		%also: de left dialog is a buttonbar now, see initButtonbar
		%so it is no longer here
		%we changed some menu_items to menuCommands to make them sync with
		%the command in the buttonbar

	%
	% Fill Menubar
	%

	send(MB, append, new(File, popup(file))),
	send(MB, append, new(View, popup(view))),
	send(MB, append, new(Display, popup(display))),
	File->>append(menu_item('select a scenario',
				message(@prolog, choose_input_models, Frame))),
	unless
		get(@model, modelState, legacy) % JL
	do
	(
		File->>append(
			menu_item('save simulation in model',
				  message(@app, saveSimulation,Frame))), %save state in @model
		File->>append(menuCommand(loadSimulation,	%0.4.7: changed this to a command
			       'LoadSimulation','Open saved simulation')),
		File->>append(
			menu_item('Simulate all scenarios',
			message(Frame, simulateAll)))

	),
	File->>append(
		menu_item('Save Diagram to EPS file',
			message(Frame, postscript))),

	send_list(View, append,
		[
			menuCommand(erStructure,'ERStructure', 'E-R structure'),
			menuCommand(quantityValues,'QuantityValues', 'Quantity values'),
			menuCommand(modelFragments,'ModelFragments', 'Model fragments'),
			menuCommand(dependencies,'Dependencies','Dependencies', end_group := @on),
			menuCommand(transitionHistory,'TransitionHistory', 'Transition history'),
			menuCommand(equationHistory,'EquationHistory','Equation history'),
			menuCommand(valueHistory,'ValueHistory', 'Value history'),
			gap
		]),
	if
		(\+ get(@model, modelState, legacy) ) % JL
	then
	(
		%gp3 0.1: added line for current scenario
		View->>append(menu_item('current scenario',
							message(Frame, openCurrentScenario)))
	),
	%gp3 0.1: changed this. Now opens the design-time scenario list unless legacy mode.
	View->>append(menu_item('scenarios',
						if(quote_function(@model?modelState) == legacy, % JL  JJ: changed @model?modelState to quote(..)
							->>(Frame,legacyViewInputsystem),
                              ->>(@app,openScenarios)))),

    %gp3 0.1: entity hierarchy is visible only in legacy mode

    if
	%legacy = @app<<-modelState
	get(@model, modelState, legacy)
    then
		View->>append(menu_item('entity isa hierarchy',
                              message(Frame, gen_isa_hierarchy))),

    %transition rules are only visible if there are custom rules
    if
	engine:rule(_,_,_,_)
    then
		View->>append(menu_item('transition rules',
				if(@model?modelState == legacy, % JL
					->>(Frame,legacyViewRules),
					->>(@app, showRules)))), %showRules is todo??

    %gp3: model fragments hierarchies only in legacy mode
    if
	get(@model, modelState, legacy) % JL
    then
	(
		View->>append(menu_item('model fragments isa hierarchy',
	                                  message(Frame, gen_isa_mf_hierarchy))),
		View->>append(menu_item('model fragments applies-to hierarchy',
	                                  message(Frame, gen_apply_mf_hierarchy)))
	),
	send_list(Display, append,
		[	menu_item('layout',
				  message(Frame, layout)),
			menu_item('layout_states',
				  message(Frame, layout_states)),
			menu_item('layout_terminations',
				  message(Frame, layout_terminations)),
			menu_item('settings',
				  message(Frame, settings))
%			menu_item('language',
%				  message(Frame, language_settings))
		]),

	%the D2 text_dialog member is still there

	%get(Frame, member, picture, P), %gp3: framedWindows uses Frame?client instead
	send(D2, left_side, Frame?client?left_side),
        % this does not seem to work
	SelStates *= text_item(selected_states, '[]',
                          message(Frame, select_states)), %gp3: changed receiver to Frame
        D2->>append(SelStates),
        SelStates->>activate(@on),
	SelPath *= text_item(selected_path, '[]',
                          message(@prolog, select_path, Frame)),
        D2->>append(SelPath,right).
%%

/*
%% Buttonbar commands
%% All on... command implementations are gp3
%% But they just refer to the legacy code from the old buttondialog
%% These settings could all just be used in menuCommand objects in the menubar
*/

%%
%0.4.7: menuCommand for loadSimulation
checkLoadSimulation(_VG):->
	%only possible in non-legacy mode with a model that contains saved simulations
	%\+ legacy = @app<<-modelState,
	\+ get(@model, modelState, legacy),
	\+ @model?savedSimulations->>empty.
%
onLoadSimulation(VG):->
	%pass thru to app
	@app->>loadSimulation(VG).
%%
onStateMode(VG):->
        VG->>select_states_or_end_states.
%%

%%
onPathMode(VG):->
	select_path(VG). %legacy prolog call
%%

%%
onSelectAll(VG):->
	VG->>select_all.
%%

%%
onDeselectAll(VG):->
	VG->>deselect_all.
%%

%%
checkERStructure(VG):->
	%gp3
	%there should be a selection
	get_selection_list(VG,[_|_]).
%
onERStructure(VG):->
	VG->>gen_system_elements.
%%

%%
checkQuantityValues(VG):->
	get_selection_list(VG,[_|_]). %any selection will do
%
onQuantityValues(VG):->
	VG->>gen_quantity_values.
%%

%%
checkModelFragments(VG):->
	get_selection_list(VG,[_|_]). %any selection will do
%
onModelFragments(VG):->
	VG->>gen_model_fragments.
%%

%%
checkDependencies(VG):->
	get_selection_list(VG,[_|_]). %any selection will do
%
onDependencies(VG):->
	VG->>gen_causal_model.
%%

%%
checkTransitionHistory(VG):->
	%gp3 there should be a selection, and any part of it should have termination
	get_selection_list(VG,List),
	member(State,List),
	engine:state_status(State,Status),
	member(Status,[terminated, ordered, closed]).
%
onTransitionHistory(VG):->
	show_transitions(VG). %old prolog
%%

%%
checkEquationHistory(VG):->
	%a selection that is not exclusively state 0
	get_selection_list(VG,List),
	member(State,List),
	State \== 0.
%
onEquationHistory(VG):->
	gen_equation_history_menu(VG).
%%

%%
checkValueHistory(VG):->
	%a selection that is not exclusively state 0
	get_selection_list(VG,List),
	member(State,List),
	State \== 0.
%
onValueHistory(VG):->
	gen_history_menu(VG).
%%

%%
onTraceWindow(_VG):->
	%new in gp3 0.3
	@app->>openTracer.

%%


%%
checkSimulationPreferences(_VG):->
	%new in gp3 0.3, only available in non-legacy mode

	\+ get(@model, modelState, legacy). % JL
%

onSimulationPreferences(VG):->
	%new in gp3 0.3
	@app->>runPrefs(VG).
%%


%%
onSelectScenario(VG):->
	choose_input_models(VG).
%%

%%
checkFullSimulation(_VG):->
	%there is a state that is not closed
	engine:state_status(_State,Status),
	Status \== closed.
%
onFullSimulation(VG):->
	VG->>run_simulation.
%%

%%
checkTerminateStates(VG):->
	%the selection should include a state that is not yet terminated
	get_selection_list(VG,List),
	member(State,List),
	engine:state_status(State,Status),
	\+ member(Status,[terminated, ordered, closed]).
%
onTerminateStates(VG):->
	VG->>terminate.
%%

%%
checkOrderStates(VG):->
	%the selection should include a state that is not yet ordered
	get_selection_list(VG,List),
	member(State,List),
	engine:state_status(State,Status),
	\+ member(Status,[ordered, closed]).
%
onOrderStates(VG):->
	VG->>order.
%%

%%
checkCloseStates(VG):->
	%the selection should include a state that is not yet closed
	get_selection_list(VG,List),
	member(State,List),
	\+ engine:state_status(State,closed).
%
onCloseStates(VG):->
	VG->>close.
%%

% run_simulation(F)
%
% runs the complete simulation, if not done already, and
% displays the state-transition graph
%
run_simulation(F) :->
        debug(simulate(table_view), 'run_simulation', []),
	%send(@prolog, main_menu_action, all),
	enginectrl:command(full_simulation),
	send(F, gen_simulation_view, @on),
	% send(F, layout_states),
	% give every state its appropriate colour
	forall(engine:state(N, _),
		catch(F->>updateStateStatus(N, closed), _, fail) %new gp3 call
	),
	% erase terminations from states N with lower status than closed
	% (or succeeded - then no termination nodes will appear)
	forall(lower_status(LowerStatus, closed),
		forall(engine:state(N, _),
			delete_terminations(F, N, LowerStatus)
		)
	),
	send(F, select_states_or_end_states).


openCurrentScenario(_F):->
	%gp3 0.1 Open the currently used scenario in design or succeed silently (legacy etc)

	\+ get(@model, modelState, legacy), % JL
	@app->>openViewEditor(@model?lastEditedIS). %just get it from the design-time model
%
openCurrentScenario(_F):->
	true. %never fail

% This sets the file type after the first is opened:
% input_model or saved_state
%
set_file_type(_F, none, NewType):-
       retractall(type_of_file_first_opened(_)),
       asserta(type_of_file_first_opened(NewType)), !.

% In other cases, do nothing
%
set_file_type(_F, _OldType, _NewType).


% terminate(F)
%
% Find the terminations for each selected state in StateListStr
% Terminate always succeeds, also when no states where terminated
%
terminate(F) :->
	% check whether it's a valid StateList
%	str_to_statelist(StateListStr, StateList),
	get_selected_states_or_path(F, InputStateList),
	delete(InputStateList, 0, StateList),
	StateList \== [],!,
	forall(member(N, StateList),
	   (
		% if StateList is directly given to state_menu_action
		% it will fail when it contains a state
		% for which the action is impossible
		(enginectrl:command(termination([N]))
		 ->
			F->>updateStateStatus(N, terminated) %new gp3 call
		;
			true %gp3 0.4.4: used to be a write_ln call
		)
	   )
	),
	send(F, gen_simulation_view, @off),
        % send(F, layout_states),
	% to do!
	send(F, select_states_or_end_states).




% order(F)
%
% Order the terminations for each selected state in StateListStr
% Order always succeeds, also when no states could be ordered
%
order(F) :->
	% check whether it's a valid StateList
%	str_to_statelist(StateListStr, StateList),
	get_selected_states_or_path(F, InputStateList),
	delete(InputStateList, 0, StateList),
	StateList \== [],!,
	forall(member(N, StateList),
	   (
		% if StateList is directly given to state_menu_action
		% it will fail when it contains a state
		% for which the action is impossible
		% order will first terminate, to allow shortcuts
		(enginectrl:command(order([N]))
		 ->
			F->>updateStateStatus(N, ordered)
		;
			true %gp3 0.4.4: used to be a write_ln call
		)
	   )
	),
	send(F, gen_simulation_view, @off),
        % send(F, layout_states),
	% to do! check whether this still works!
	% erase terminations from states N with lower status than ordered
	forall(lower_status(LowerStatus, ordered),
		forall(member(N, StateList),
			 delete_terminations(F, N, LowerStatus)
		)
	),
	send(F, select_states_or_end_states).




% close(F)
%
% Close the terminations for each selected state in StateList
% by finding successor states for all ordered terminations of each
% of these states.
%
close(F) :->
	% check whether it's a valid StateList
	get_selected_states_or_path(F, InputStateList),
	delete(InputStateList, 0, StateList),
	StateList \== [],!,
	forall(member(N, StateList),
	   (
		% if StateList is directly given to state_menu_action
		% it will fail when it contains a state
		% for which the action is impossible
		% close will first terminate and order to allow shortcuts
		(enginectrl:command(close([N]))
		 ->
			F->>updateStateStatus(N, closed) %new gp3 call
		;
			true %gp3 0.4.4: used to be a write_ln call
		)
	   )
	),
	send(F, gen_simulation_view, @off),
        % send(F, layout_states),
	% to do! check whether this still works!
	% erase terminations from states N with lower status than closed
	forall(lower_status(LowerStatus, closed),
		forall(member(N, StateList),
			 delete_terminations(F, N, LowerStatus)
		)
	),
	send(F, select_states_or_end_states).


select_all(F) :->
	findall(Nr, state_to_be_shown(Nr, state_graph), AllStatesList),

	%gp3 0.2: we get the selectMode through a helper call
	%instead of from a dialog

	Sel = F<<-selectMode,

	get(F, textdialog_member, D2),

	%gp3: set to states mode
	F->>selectMode(states),
	%send(CM, selection, 'states'),
	select_states(F, AllStatesList),
	select_states_transitions(F, AllStatesList),
	select_states_terminations(F, AllStatesList),
        term_to_atom(AllStatesList, ListStr),
        send(D2?selected_states_member, selection, ListStr),

	% if radio button is set to `path', turn that off, and present a message
	( Sel == 'path'
	->
		send(D2?selected_path_member, selection, 'Path selection turned off'),
		% send(F, report, error, 'Path selection turned off.')
		send(F, change_selected_states)
	;
		% radio button was not set to path
                send(D2?selected_states_member, selection, ListStr)
	).



deselect_all(F) :->
	% reset frame selection nr
	send(F, selection_nr, 0),
	%get(F, member, picture, P),
	P = F<<-client, %gp3
	% clear selection
	reset_selection(P),
	% reset things on the screen
	get(F, textdialog_member, D),
        send(D?selected_path_member, selection, ''),
        send(D?selected_states_member, selection, '').

%gp3 0.2
%we still have get_selected_states_or_path to not break old code
%but we split of the logic from the error message
%that is why we have a different call to get the selection (get_selection_list)
%and get_selected_states_or_path still complains when the result is []
%gp3 0.3: no longer using ?selection, but ?displayed_value
%to make sure the ?modified status of the text field remains unchanged

% if radio button is set to `select a path'
get_selection_list(F, ReturnStateList) :-
	Sel = F<<-selectMode, %gp3 new call
        get(F, textdialog_member, D2),
	% if radio button is set to `select path'
	Sel == 'path',!,
        get(D2?selected_states_member, modified, Modified),
        if
        (
          % user is typing in state numbers
          Modified = @on
        )
        then
        (
          % change selection mode to individual states to prevent
          % path search while the user is busy typing state numbers
	  F->>selectMode(states)
        )
        else
        (
	  get(D2?selected_path_member?displayed_value, value, PathStr), %make it into a name too
	  % check whether it's a valid StateList
	  catch(term_to_atom(ReturnStateList, PathStr), _, fail)
        ).

% if radio button is set to `select individual states'
get_selection_list(F, ReturnStateList) :-
        get(F, textdialog_member, D2),
	get(D2?selected_states_member?displayed_value, value, StatesStr),
	% translate into a valid StateList, if possible
        str_to_statelist(StatesStr, ReturnStateList),
        get(D2?selected_states_member, modified, Modified),
        if
        (
          Modified = @on
        )
        then
        (
          % Select states on screen
          % It would be good to also set this text field with the correct result,
          % but this works too soon, and disrupts when you're editing it
          % term_to_atom(ReturnStateList, NewStr),
          % send(D2?selected_states_member, selection, NewStr)
          send(F, select_states)
        ).



%%
%gp3: so if get_selection_list returns an empty list or fails, we report error and return []
% in case the above fails, return empty list
%
get_selected_states_or_path(F,StateList):-
	get_selection_list(F,StateList),
	StateList \== [],
	!.

get_selected_states_or_path(F, []):-
	send(F, report, error, 'No states selected').
%%


select_states(F) :->
        debug(simulate(stgraph), 'select_states', []),
        get(F, textdialog_member, D2),
	get(D2?selected_states_member, selection, Str),

	% convert to a valid StateList
        str_to_statelist(Str, StateList),

	Sel = F<<-selectMode, %gp3
	send(D2?selected_path_member, selection, ''),
	% send(F, change_selected_states),

	% if radio button is set to `select states'
	(Sel == 'states', !,
		select_states(F, StateList),
		select_states_transitions(F, StateList),
		select_states_terminations(F, StateList)
	;
		% Sel == 'path'
		select_path(F)
	).

%
select_states_or_end_states(F) :->
        ES = @app<<-setting(select_end_states),
        ES = @on,!,
	debug(simulate(stgraph), 'select_states_or_end_states: select_end_states', []),
        send(F, select_end_states).

select_states_or_end_states(F) :->
        ES = @app<<-setting(select_end_states),
        ES = @off,!,
        debug(simulate(stgraph), 'select_states_or_end_states: select_states', []),
        send(F, select_states).
%%


%
select_end_states(F) :->
        % select all end states
        Graph = state_graph(original, noinput),
        findall(Z, end_node(Graph, Z), EndNodes),
        select_states(F, EndNodes),

	Sel = F<<-selectMode, %gp3
        get(F, textdialog_member, D2),
	send(D2?selected_path_member, selection, ''),
	send(F, change_selected_states),

	% if radio button is set to `select states'
	(Sel == 'states', !,
		select_states(F, EndNodes),
		select_states_transitions(F, EndNodes),
		select_states_terminations(F, EndNodes)
	;
		% Sel == 'path'
		select_path(F)
	),
        if EndNodes = []
        then
        (
            send(F, report, error,
            'There are no end states in this state-transition graph.')
        ).
%%


%%%% begin changes  - AB 21/09/2005
%%%% added predicate select_state, to make it work (and clearer too)

select_states(F, StateList):-
	% reset frame selection nr
	send(F, selection_nr, 0),
	%get(F, member, picture, P),
	P = F<<-client, %gp3
	% clear selection first
	reset_selection(P),
	forall(member(Nr, StateList),
             select_state(F, Nr)
        ).


% select_state(F, Nr)
%
% input state
select_state(F, 0):-
	get(F, find_node, input, Node),
	% set selection nr of graphical
	update_selection_nr(Node),
	send(Node, selected, @on),!.

select_state(F, Nr):-
    % check if its an existing state nr
    (
	graph_node(state_graph, Nr),
	get(F, node, Nr, state, Nr, Node),
	% set selection nr of graphical
	update_selection_nr(Node),
	send(Node, selected, @on)
     ;
	% invalid state nr
		true %gp3 0.4.4 used to be an write_ln call only
     ).

%%%% end of changes - AB 21/09/2005


%%
updateStateStatus(F, N: int, NewStatus: [name]):->
	%gp3: class method rewrite of colour_state/2 and /3
	%set status of state to NewStatus, if given, unless status
	%is allready 'more'
	%status is unprocessed, terminated, ordered, closed

	N > 0, %no input state
	if
		NewStatus = @default
	then
		engine:state_status(N,Status) %use engine info
	else
		Status = NewStatus, %force status
	Node = F<<-node(N,state,N), %old code
	OldStatus = Node<<-t_status,
	max_status(OldStatus, Status, MaxStatus), %old call
	Node->>t_status(MaxStatus), %might be the same
	Node->>updateDisplay, %gp3 call to update the image
	%update gp3 tooltip:
	Node->>tooltip(string('State %s (%s)',N,MaxStatus),model).
%colour_state used to do something with find_state_status
%not sure why. We do not do that right now

updateStateStatus(_F,N,_NewStatus):->
	N = 0. %do nothing


find_state_status(N, MinStatus):-
	% find all terminations and state transitions from N
	findall(Status,
		(
		engine:state_to(N, ToList),
		nth1(_ToNr, ToList, to(_, _, _, to_state(_ToStates),
							Status))
		),
		StatusList
	),
	min_status_list(StatusList, closed, MinStatus).




change_state_nr(F, N) :->
        "Change state nr on screen"::
        get(F, textdialog_member, D2),
        send(D2?state_member, selection, N).


update_selected_states(F, Gr) :->
        "Change selected states on screen"::
        get(F, textdialog_member, D2),
	get(D2?selected_states_member, selection, Str),

	% convert to a valid StateList
        str_to_statelist(Str, StateList),

	Sel = F<<-selectMode, %gp3
	% if radio button is set to `states'
	(Sel == 'states', !,
		send(F, change_selected_states)
	;
		% if Sel == 'path'
		get(Gr, state_nr, N),
		(
			send(Gr, instance_of, state_node),
			get(Gr, type, Type),
			Type == state,
			not(member(N, StateList))
		->
			% if node was not included yet, add it to statelist
			conc(StateList, [N], NewStateList)
		;
			% if node was on the list, delete it
			delete(StateList, N, NewStateList)
		),

		term_to_atom(NewStateList, NewStr),
		send(D2?selected_states_member, selection, NewStr),
		send(@prolog, select_states_transitions, F, NewStateList),
		send(@prolog, select_states_terminations, F, NewStateList)
	).




change_selected_states(F) :->
        "Change selected states on screen"::
        get(F, textdialog_member, D2),
	%get(F, member, picture, P),
	P = F<<-client, %gp3
	new(StatesChain, chain),
	% Jan Wielemakers tips and tricks: repeat by failure
        \+ get(P, find, @default,
		and(
			message(@arg1, instance_of, state_node),
			@arg1?selected == @on,
			@arg1?type == state,
			message(StatesChain, append, @arg1),
			new(or)
		), _StateNode
	      ),
	chain_list(StatesChain, GrsList),
	% the order in which graphicals were selected
	% is registered by their selection_nr
	findall(SelNr/N/Gr,
		(
			member(Gr, GrsList),
			get(Gr, state_nr, N),
			get(Gr, selection_nr, SelNr)
		),
		SelNrNGrList),
	sort(SelNrNGrList, SortedSelNrNGrList),
	findall(N, member(_SelNr/N/_Gr, SortedSelNrNGrList), NList),
	term_to_atom(NList, NListStr),
        send(D2?selected_states_member, selection, NListStr),
	select_states_transitions(F, NList),
	select_states_terminations(F, NList).



% this may be never used?
%
change_selected_path(F) :->
        "Change selected states on screen"::
        get(F, textdialog_member, D2),
	%get(F, member, picture, P),
	P = F<<-client, %gp3
	get(P, selection, GrsChain),
	get(GrsChain, find_all,
		message(@arg1, instance_of, graph_node), NodesChain),
	get(NodesChain, find_all,
		(@arg1?type == 'state'), StatesChain),
	chain_list(StatesChain, GrsList),

	% the order in which graphicals were selected
	% is registered by their selection_nr
	findall(SelNr/N/Gr,
		(
			member(Gr, GrsList),
			get(Gr, state_nr, N),
			get(Gr, selection_nr, SelNr)
		),
		SelNrNGrList),
	sort(SelNrNGrList, SortedSelNrNGrList),
	findall(N, member(_SelNr/N/_Gr, SortedSelNrNGrList), NList),
	term_to_atom(NList, NListStr),
        send(D2?selected_states_member, selection, NListStr).

clear(F) :->
	"Clear the diagram"::
	%get(F, member, picture, P),
	P = F<<-client, %gp3
	send(P, clear).



reset_position(_F) :->
	"Reset position coordinates"::
	retract(currentX(_NrX, _X)),
	retract(currentY(_NrY, _Y)),
	assert(currentX(1, 0)),
	assert(currentY(1, 0)).



settings(F) :->
	"Display window with settings to edit"::
	%gp3: we commented out 3 items + the code to fill/set them
	%and added a new setting (legacy quantity names in dependencies)
	%gp3 0.3.13: changed dialog to assistanceDialog for helpbutton. Rewrote lay out code to fixed layout
	new(Dialog, assistanceDialog('Preference settings','Sim_Display_PreferenceSettings')),
	send(Dialog, transient_for, F),
	% create a menu for toggling different preference settings
	% so that they are shown or hidden
	%
	new(M1, menu('Visualization preferences:', toggle)),

	send(M1, alignment, left),
	send(M1, columns, 1),
	send(M1, layout, vertical),
	send(M1, gap, size(0,10)), % difference between gap & border?
	send(M1, border, 0),       % difference between gap & border?
	send(M1, value_width, 140),
	send(M1, format, left),

	send_list(M1, append,
		% first column
		[
		 menu_item('input_state', @default, 'Input State'),
		 menu_item('termination_nodes', @default, 'Termination Nodes'),
		 menu_item('dependencies_internal_quantity_naming',@default,'Internal quantity names in dependency view'),
		 menu_item('model_tooltips',@default,'Show model ingredient tooltips'), %gp3 1.4
		 menu_item('second_order_derivatives',@default,'Show higher order derivatives') %FL april 08 %FL new feb 2012: this now has the meaning: higher order derivatives
		 %gp3: these three commented out
		 /*
		 menu_item('transitive_input', @default, 'Transitive Input'),
		 menu_item('transitive_reduction', @default, 'Transitive Reduction'),
		 menu_item('aggregate_orderings', @default, 'Aggregate Orderings')
		 */
		]),

	% default message when a menu item is chosen
	send(M1, message, and(
				 message(@prolog, change_settings,
						F, @arg1, @arg2)/*,
                                 message(Dialog, destroy)*/ %gp3: dont like destroy after changing one option
			  )
	),

	new(M2, menu('Search and selection preferences:', toggle)),

	send(M2, alignment, left),
	send(M2, columns, 1),
	send(M2, layout, vertical),
	send(M2, gap, size(0,10)), % difference between gap & border?
	send(M2, border, 0),       % difference between gap & border?
	send(M2, value_width, 140),
	send(M2, format, left),

	send_list(M2, append,
		% first column
		[
		 menu_item('select_cyclic_paths', @default, 'Search for cyclic path'),
		 menu_item('select_full_paths', @default, 'Search for path from begin to end state or end loop'),
		 menu_item('select_end_states', @default, 'Select end states in individual states mode')
		]),

	% default message when a menu item is chosen
	send(M2, message, and(
				 message(@prolog, change_settings,
						F, @arg1, @arg2)/*,
                                 message(Dialog, destroy)*/ %gp3: dont like destroy after changing one option
			  )
	),

	%gp3 0.2 made these settings app-settings which are automatically saved
        TN = @app<<-setting(termination_nodes),
        IS = @app<<-setting(input_state),
        SES = @app<<-setting(select_end_states),
        SFP = @app<<-setting(select_full_paths),
        SCP = @app<<-setting(select_cyclic_paths),
        IN = @app<<-setting(dependencies_internal_quantity_naming),
	SOD = @app<<-setting(second_order_derivatives),
        %settings(transitive_input, TI),
        %settings(transitive_reduction, TR),
        %settings(aggregate_orderings, AO),
	send(M1, selected, 'termination_nodes', TN),
	send(M1, selected, 'input_state', IS),
	send(M1, selected, 'second_order_derivatives', SOD),
	M1->>selected('dependencies_internal_quantity_naming',IN),
	%gp3 1.4:
	TooltipState = @tooltip_window<<-category_state(model),
	M1->>selected('model_tooltips',TooltipState),
	%send(M1, selected, 'transitive_input', TI),
	%send(M1, selected, 'transitive_reduction', TR),
	%send(M1, selected, 'aggregate_orderings', AO),
	% send(M1, selected, 'old_terminations', @on),
	Dialog->>display(M1,point(Dialog?gapX,Dialog?topY)),
	send(M2, selected, 'select_end_states', SES),
	send(M2, selected, 'select_full_paths', SFP),
	send(M2, selected, 'select_cyclic_paths', SCP),
	Dialog->>display(M2,point(M1?left_side,M1?bottom_side+Dialog?gapY)),

        % M3 for type of view: State-transition graph, Table view 1, or Table view 2
	new(M3, menu('Type of view for states and transitions:', choice)),

	% default message when a menu item is chosen
	send(M3, message, and(
				 message(F, change_simulation_view_setting, @arg1)
			  )
	),
	send(M3, alignment, left),
	send(M3, columns, 1),
	send(M3, layout, vertical),
	send(M3, gap, size(0,10)), % difference between gap & border?
	send(M3, border, 1),       % difference between gap & border?
	send(M3, value_width, 140),
	send(M3, format, left),

	send_list(M3, append,
		% first column
		[
		 menu_item('graph', @default, 'Graph View'),
		 menu_item('table', @default, 'Table View: States x Transitions')
		]),

        ViewType = @app<<-setting(simulation_view_type),
	send(M3, selection, ViewType),
	Dialog->>display(M3,point(M2?left_side,M2?bottom_side+Dialog?gapY)),


	Close *= imgButton(close,->>(Dialog,destroy),tt:='Close this window'), %gp3 0.3.13 changed this without warning to the same button used elsewhere
	Dialog->>display(Close,point(Dialog?gapX + (M2?right_side - Dialog?gapX - Close?width) / 2,M3?bottom_side + Dialog?gapY)),
	send(Dialog, open).


legacyViewInputsystem(F):->
	%gp3 legacy way to view a legacy is file
	%we use the old call, but have to change the path
	%no problem: in legacymode @app?filename is the path to the input systems

	%recheck legacymode, better safe than less safe

	if
		get(@model, modelState, legacy) % JL
	then
	(
		FN = @app<<-filename,
		view(F,FN) %legacy code
	).

legacyViewRules(F):->
	%gp3 legacy way to view a legacy rules file
	%we use the old call, but have to change the path

	%recheck legacymode, better safe than less safe

	if
		get(@model, modelState, legacy) % JL
	then
	(
		FN = string('%s/rules',@app?fileDir)<<-value,
		view(F,FN) %legacy code
	).


change_simulation_view_setting(VG, ViewType):->

        PreviousViewType = @app<<-setting(simulation_view_type),
        % or use an application setting, so that it will be saved for each model?
        @app->>setting(simulation_view_type, ViewType),
	if PreviousViewType \== ViewType
	then
	(
          P = VG<<-client,
          send(P, clear)
	),
	% in any case, generate anew
        send(VG, gen_simulation_view, @on).




gen_simulation_view(VG, Layout: bool):->
        record_transitions(state_graph),
        ViewType = @app<<-setting(simulation_view_type),
        if
          ViewType = (table)
        then
          send(VG, draw_table),
        if
          ViewType = graph
        then
        (
	  send(VG, gen_state_graph),
	  unless Layout == @off
	  do
	  (
	    send(VG, layout_states)
          )
	),
	send(VG, select_states_or_end_states).



draw_table(VG):->
        new(Table, device),
        get(VG, client, P),
        send(P, clear),

        % include input state 0 (scenario) or not, based on setting
        IS = @app<<-setting(input_state),
        if IS == @on
        then
           ISS = input
        else
           ISS = noinput,
        Graph = state_graph(original, ISS),
        findall(S,
               graph_node(Graph, S),
               StateList
        ),
        sort(StateList, SortedStateList),
        length(StateList, NrStates),

        GridColour *= colour(grey92), % = RGB 235,235,235, lighter than light_grey
        GridWidth is 30,
        GridHeight is 14,
        XMargin is 15,
        YTopMargin is 7,
        YMargin is YTopMargin + 20,
        % create table 2
        draw_table_states(Table, SortedStateList, GridWidth, XMargin, YTopMargin),
        draw_table_transitions(Table, SortedStateList, ISS, GridWidth, GridHeight, XMargin, YMargin, GridColour),
        send(VG, draw_table_grid, Table, NrStates, GridWidth, XMargin, YMargin, YTopMargin, GridColour),

        send(P, display, Table),
        send(Table, displayed, @on),
        % scroll to origin of table view, plus Ymargin
	send(P, scroll_to, point(0, 0-YTopMargin)).



draw_table_states(D, SortedStateList, GridWidth, XMargin, YMargin):-
        send(D, clear),
        % maybe the StateList could be a selection, instead of all states...
        forall(nth0(X, SortedStateList, S),
           draw_state(D, S, X, GridWidth, XMargin, YMargin)
        ).


draw_table_transitions(D, SortedStateList, ISS, GridWidth, GridHeight, XMargin, YMargin, GridColour):-
        % get input transitions. If ISS = noinput, InputTransitions = []
        get_input_transitions(ISS, InputTransitions),
	forall( member(ToState, InputTransitions),
		(
			draw_input_transition(D, SortedStateList, ToState, GridWidth, GridHeight, XMargin, YMargin)
		)
	),
        % to do: make sure that input transitions get some room in the grid too.

        if ISS = input
        then
                % add 1 gridheight to the YMargin to make room for the input transitions
                YMargin2 is YMargin + (1 * GridHeight)
        else
                YMargin2 is YMargin,


        get_transitions(SortedTransitions),
	forall( member(Trans, SortedTransitions),
		(
			draw_transition(D, SortedStateList, SortedTransitions, Trans, GridWidth, GridHeight, XMargin, YMargin2, GridColour)
		)
	).


draw_state(D, S, X, GridWidth, XMargin, YMargin):-
      debug(simulate(inputnode), 'draw state ~w', [S]),
      input_to_int(I, S),
      debug(simulate(inputnode), 'state name ~w', [I]),
        new(MasterNode, state_master_node(I, state, S)),
	get(MasterNode, member, S, Node),
        XPos is X * GridWidth + XMargin,
        YPos is 0 + YMargin,
        send(D, display, MasterNode, point(XPos, YPos)),
        if S == 0
        then
        (
	  %gp3 0.2: find the input system name for the node
	  if
	  (
		IS = @app<<-currentScenario, %we make sure we donot fail
		IS \= @nil
	  )
	  then
	  (
		TT *= string('%s',IS?name),
		unless
			0 = IS?remarks<<-size
		do
			TT->>ensure_nl(IS?remarks),
		Node->>tooltip(TT)
	  )

        )
        else
        (
	  engine:state_status(S, Status),
          % maybe this can be done with only one call to F->>updateStateStatus(S) if done in same frame F
	  Node->>t_status(Status), %might be the same
	  Node->>updateDisplay, %gp3 call to update the image
	  %update gp3 tooltip:
	  Node->>tooltip(string('State %s (%s)',S , Status))
        ).


draw_table_grid(_VG, D, NrStates, GridWidth, XMargin, YMargin, YTopMargin, GridColour):->
        W is GridWidth,
        get(D, height, H0),
        H is H0 + YMargin,
        % To do: forall states do

	% small box on the left
	new(B1, box(XMargin, H)),
        send(B1, pen, 0),
        send(B1, fill_pattern, GridColour),
        send(D, display, B1, point(0, 0-YTopMargin)),
        % place box behind the rest of the picture elements
        send(B1, hide, @default),

        forall(between(1, NrStates, N),
         (R is N mod 2,
          if R = 0
          then % N is even
          (
	   new(B, box(W, H)),
           send(B, pen, 0),
           send(B, fill_pattern, GridColour),
           send(D, display, B, point((N-1)*GridWidth+XMargin, 0-YTopMargin)),
           % place box behind the rest of the picture elements
           send(B, hide, @default)
          )
         )
        ),

	% small box on the right
	new(B2, box(XMargin, H)),
        send(B2, pen, 0),
        RT is NrStates mod 2,
        if RT = 0
        then % NrStates is even
           send(B2, fill_pattern, colour(white))
        else
           send(B2, fill_pattern, GridColour),
        send(D, display, B2, point((NrStates)*GridWidth+XMargin, 0-YTopMargin)),
        % place box behind the rest of the picture elements
        send(B2, hide, @default).




draw_input_transition(T, SortedStateList, ToState, GridWidth, GridHeight, XMargin, YMargin):-
        draw_input_transition_row(T, SortedStateList, 0, ToState, GridWidth, GridHeight, XMargin, YMargin).



draw_transition(T, SortedStateList, SortedTransitions, TransitionName, GridWidth, GridHeight, XMargin, YMargin, GridColour):-
	find_transition_data(TransitionName, FromState, CausesList,
				ConditionsList, ResultsList,
				ToStateList, Status),
        decompose_transition_name(TransitionName, FromState, TransitionNr),
	engine:state_to(FromState, ToList),
	nth1(TransitionNr, ToList, To),
        To = to(cause(CausesList),
		conditions(ConditionsList),
		results(ResultsList),
		to_state(ToStateList),
		Status),

        % Which To-state it is within the transition is given by TransitionIndex
        % forall(nth0(TransitionIndex, ToStateList, ToState), % not so important
        if ToStateList = []
        then
            draw_empty_transition_row(T, SortedStateList, TransitionName, TransitionNr, FromState, CausesList, GridWidth, GridHeight, XMargin, YMargin, GridColour)
        else
         (
          forall(member(ToState, ToStateList),
	    draw_transition_row(T, SortedStateList, SortedTransitions, TransitionName, TransitionNr, FromState, ToState, CausesList, ToStateList, GridWidth, GridHeight, XMargin, YMargin, GridColour)
         )
        ).



draw_input_transition_row(T, SortedStateList, A, B, GridWidth, GridHeight, XMargin, YMargin):-
         A == 0,

         % to do: make a whole line
         IndexStateA = 0,
         nth0(IndexStateB, SortedStateList, B),

         % to do: make a whole line
         % begin blob should be located at gridposition 1 x 1

	 % Calculate position for begin point and end point of the transition arrow
         ArrowLength is 8,  % default = 10
         XOffSet is GridWidth/2,
         YOffSet is GridHeight/2,
         XPos1 is XMargin + IndexStateA * GridWidth + XOffSet,
         if IndexStateA < IndexStateB
         then
            XPos2 is XMargin + IndexStateB * GridWidth + XOffSet + ArrowLength/2
         else
            % Arrows directed backwards should be a bit longer (approx the size of arrowhead)
            XPos2 is XMargin + IndexStateB * GridWidth + XOffSet - ArrowLength/2,


         IndexY = 1,  % for now: to do: where to place this exactly?

         YPos is IndexY * GridHeight + YOffSet + YMargin,

	 % Create line of the appropriate length
         new(L, line(0, 0, XPos2-XPos1, 0)),
         send(L, pen, 1),
         send(L, arrows, second),
	 send(L?second_arrow, length, ArrowLength),
	 send(L?second_arrow, wing, 9), % default = 7

	 findall(StartState, start_node(state_graph, StartState), StartStates),
	 term_to_atom(StartStates, StartStatesStr),
	 L->>tooltip(string('Begin states: %s', StartStatesStr)),

         send(T, display, L, point(XPos1, YPos)),
         send(L, colour, colour(dark_grey)).



draw_transition_row(T, SortedStateList, SortedTransitions, TransitionName, TransitionNr, A, B, CausesList, ToStateList, GridWidth, GridHeight, XMargin, YMargin, GridColour):-


	 % make a table cell for the begin blob
         % new(StartPoint, state_termination_node(S, S)),

         swritef(StrA, '%wt%w', [A,TransitionNr]),
         new(T1, table_termination_text(StrA)),

         % to do: make a whole line
         nth0(IndexStateA, SortedStateList, A),
         nth0(IndexStateB, SortedStateList, B),
         % begin blob should be located at IndexStateA x Index (of edges)

         % find out the index for the Y coordinate, depending on which transition it is
         nth1(IndexY, SortedTransitions, TransitionName),!,

	 % Calculate position for begin point and end point of the transition arrow
         ArrowLength is 8,  % default = 10
         XOffSet is GridWidth/2,
         YOffSet is GridHeight/2,
         XPos1 is XMargin + IndexStateA * GridWidth + XOffSet,
         if IndexStateA < IndexStateB
         then
            XPos2 is XMargin + IndexStateB * GridWidth + XOffSet + ArrowLength/2
         else
            % Arrows directed backwards should be a bit longer (approx the size of arrowhead)
            XPos2 is XMargin + IndexStateB * GridWidth + XOffSet - ArrowLength/2,
         YPos is IndexY * GridHeight + YOffSet + YMargin,

	 % Create line with arrow of the appropriate length
	 get(StrA, value, StrA_Val),
	 new(L, table_transition(StrA_Val, B, 0, 0, XPos2-XPos1, 0, ArrowLength)),
         create_transition_recogniser(L, StrA),

         send(T, display, L, point(XPos1, YPos)),

	 create_list_str(CausesList, CausesStr, '  '),
	 if ToStateList = [B] % single to state
         then
	   TooltipTxt *= string('Transition %s from state %s to state %s:\n%s', StrA, A, B, CausesStr)
         else
         (
	   term_to_atom(ToStateList, ToStateListStr),
	   TooltipTxt *= string('Transition %s from %s to states %s:\n%s', StrA, A, ToStateListStr, CausesStr)
         ),
         L->>tooltip(TooltipTxt),

         % Match background colour of text to Grid to hide begin of arrow
         R is (IndexStateA+1) mod 2,
         if R = 0
         then % IndexStateA is even
         (
           send(T1, background, GridColour)
         )
         else
         (
           send(T1, background, colour(white))
         ),
         get(T1, width, TW),
         get(T1, height, TH),
         create_transition_recogniser(T1, StrA),
         T1->>tooltip(TooltipTxt),

         % center text around point(XPos1, YPos)
         send(T, display, T1, point(XPos1-TW/2, YPos-TH/2)).




draw_empty_transition_row(T, SortedStateList, TransitionName, TransitionNr, A, CausesList, GridWidth, GridHeight, XMargin, YMargin, GridColour):-

	 debug(table_view, 'draw_empty_transition_row: ...', []),

	 % make a table cell for the begin blob
        % new(StartPoint, state_termination_node(S, S)),

         swritef(StrA, '%wt%w', [A,TransitionNr]),
         new(T1, table_termination_text(StrA)),

         nth0(IndexStateA, SortedStateList, A),

         get_transitions(SortedTransitions),

         % find out the index for the Y coordinate, depending on which transition it is
         nth1(IndexY, SortedTransitions, TransitionName),!,

	 % Calculate position for begin point of the imaginary transition arrow
         XOffSet is GridWidth/2,
         YOffSet is GridHeight/2,
         XPos1 is IndexStateA * GridWidth + XOffSet + XMargin,
         YPos is IndexY * GridHeight + YOffSet + YMargin,

         % Match background colour of text to Grid to hide begin of arrow
         R is (IndexStateA+1) mod 2,
         if R = 0
         then % IndexStateA is even
         (
           send(T1, background, GridColour)
         )
         else
         (
           send(T1, background, colour(white))
         ),

	 engine:state_status(A, Status),
	 debug(table_view, 'engine:state_status for state A: ~w', [Status]),
	 send(T1, status_colour, Status),

         get(T1, width, TW),
         get(T1, height, TH),
         create_transition_recogniser(T1, StrA),

	 create_list_str(CausesList, CausesStr, '  '),
         TooltipTxt *= string('Termination %s from state %s (%s):\n%s', StrA, A, Status, CausesStr),
         T1->>tooltip(TooltipTxt),

         % center text around point(XPos1, YPos)
         send(T, display, T1, point(XPos1-TW/2, YPos-TH/2)).

%
% create an indented list
%
create_list_str([], '', _Tab).

create_list_str([X], Str, Tab):-
	 term_to_atom(X, Str1),
	 new(Str, string('%s%s', Tab, Str1)).

create_list_str([X|Rest], Str, Tab):-
	 term_to_atom(X, Str1),
	 create_list_str(Rest, RestStr, Tab),
	 new(Str, string('%s%s\n%s', Tab, Str1, RestStr)).


% create_transition_recogniser(T1, StrA):-
%
% make transition/termination active, i.e., show details on double-leftclick
%
create_transition_recogniser(T1, StrA):-
         get(StrA, value, StrA_Val),
         send(T1, recogniser,
                  click_gesture(left, '', double,
	                           message(@prolog,
					show_transition,
						StrA_Val)
		  )
	 ).


get_input_transitions(ISS, AllTo):-
        ISS == input,
	findall(ToState,
		(
		rec_transition(input, ToState)
		),
		AllTo
	).


get_input_transitions(ISS, []):-
        ISS == noinput.


get_transitions(SortedTransitions):-
        TN = @app<<-setting(termination_nodes),
        TN == @off,!,
	findall(FromNr/ToNr/ToStates/Status,
		(
		engine:state_to(FromNr, ToList),
		nth1(ToNr, ToList, to(_, _, _, to_state(ToStates),
							Status)),
                ToStates \== []
		),
		AllTo
	),
	sort(AllTo, AllToSorted),
	engine:transition_names(AllToSorted, SortedTransitions).


get_transitions(SortedTransitions):-
        TN = @app<<-setting(termination_nodes),
        TN == @on,!,
	findall(FromNr/ToNr/ToStates/Status,
		(
		engine:state_to(FromNr, ToList),
		nth1(ToNr, ToList, to(_, _, _, to_state(ToStates),
							Status))
		),
		AllTo
	),
	sort(AllTo, AllToSorted),
	engine:transition_names(AllToSorted, SortedTransitions).




change_settings(F, 'input_state', OnOrOff):-
        input_to_int(input, InputInt),
	(   get(F, node, 'input', state, InputInt, InputNode)
	->
	    send(InputNode, displayed, OnOrOff)
        ;
            % do nothing
            true
        ),
        @app->>setting(input_state,OnOrOff),
        % update simulation view (graph or table)
	send(F, gen_simulation_view, @off).



change_settings(F, 'termination_nodes', OnOrOff):-
	forall(lower_status(LowerStatus, succeeded),
		forall(engine:state(N, _),
			show_hide_terminations(F, N, LowerStatus, OnOrOff)
		)
	),
	@app->>setting(termination_nodes,OnOrOff),
        % update simulation view (graph or table)
	send(F, gen_simulation_view, @off).



change_settings(F, 'select_end_states', OnOrOff):-
	@app->>setting(select_end_states, OnOrOff),
        (OnOrOff == @on
        ->
          % select all end states
          send(F, select_end_states)
        ;
          % deselect all states
	  select_states(F, []),
	  send(F, change_selected_states)
        ).


change_settings(F, 'select_full_paths', OnOrOff):-
	@app->>setting(select_full_paths, OnOrOff),
        send(F, select_states_or_end_states).

change_settings(F, 'select_cyclic_paths', OnOrOff):-
	@app->>setting(select_cyclic_paths, OnOrOff),
        send(F, select_states_or_end_states).




change_settings(F, 'transitive_reduction', OnOrOff):-
        (OnOrOff == @off
        ->
          % give warning and explain
          send(F, report, error,
'Now, VisiGarp will perform transitive reduction on the graph, \n
by eliminating any shortcut transition that contains the same \n
events (of the types selected in the events view) as an \n
alternative, longer path. This may take a while.  \n \n
Note that the reduced digraph does not show all possible paths \n
anymore, although they may still be found by VisiGarp. \n
This functionality is currently in the beta-testing state.')
        ;
          true
        ),
        show_hide_transitive_reduction(F, OnOrOff),
        retractall(settings(transitive_reduction, _)),
        asserta(settings(transitive_reduction, OnOrOff)).



change_settings(F, 'transitive_input', OnOrOff):-
        (OnOrOff == @off
        ->
          % give warning and explain
          send(F, report, error,
'Now, VisiGarp will perform transitive reduction on the \n
input-state transitions, by eliminating any shortcut \n
transition that contains the same transition rules as \n
an alternative, longer path. This may take a while.  \n \n
Note that the reduced digraph does not show all possible paths \n
anymore, although they may still be found by VisiGarp.')
        ;
          true
        ),
        show_hide_transitive_input_transitions(F, OnOrOff),
        retractall(settings(transitive_input, _)),
        asserta(settings(transitive_input, OnOrOff)).






change_settings(F, 'aggregate_orderings', OnOrOff):-
         (OnOrOff == @on
        ->
          % give warning and explain
          send(F, report, error,
'Now, VisiGarp will perform further aggregation on the graph, by \n
adding shortcut transitions for groups of equivalent paths, which \n
contain the same events, of the types selected in the events view. \n
This may take a while.  \n \n
The aggregation functionality is currently in the beta-testing state.')
        ;
          true
        ),
        show_hide_aggregate_orderings(F, OnOrOff),
        retractall(settings(aggregate_orderings, _)),
        asserta(settings(aggregate_orderings, OnOrOff)).

%gp3 1.4: tooltip
change_settings(_F,model_tooltips,OnOrOff):-
	@tooltip_window->>category_state(model,OnOrOff).
%%


%gp3: added a clause for settings without any side effect for a current screen or anyhow
%not very happy about the ingeneral way these settings are handled
%but for now we leave it like that
%so we only assert old-style settings/2 stuff when it is allready there
%otherwise we use @app->>setting
%
%By the way, there seems to be some issues with using off instead of @off etc.

change_settings(_F,Setting,OnorOff):-
	if
		settings(Setting,_)
	then
	(
		retractall(settings(Setting,_)),
		assert(settings(Setting,OnorOff))
	)
	else
		@app->>setting(Setting,OnorOff).


% show or hide quantities with low degree, i.e.,
% with few effective causal relations
%
change_settings(F, 'quantities_with_low_degree', OnOrOff, ThresholdStr):-
	get(F, state_nr, N),
	term_to_atom(Int, ThresholdStr),
	(   catch( integer(Int), _, fail)
	->
	    Threshold is Int
	;
	    Threshold is 0
	),
	forall(graph_node(causal_graph(N), Quantity),
	(
		   in_degree(causal_graph(N), Quantity, InDegree),
		   out_degree(causal_graph(N), Quantity, OutDegree),
		   Degree is InDegree + OutDegree,
		   (   Degree < Threshold,
		       OnOrOff == @off ->
		    get(F, node, Quantity, quantity, N, ParNode),
		    send(ParNode, colour, white),
		    send(ParNode?connections, for_all, message(@arg1, colour, white)),
		    send(ParNode, hide)
		    % send(ParNode, destroy)
		    ;
		    get(F, node, Quantity, quantity, N, ParNode),
		    send(ParNode?connections, for_all, message(@arg1, colour, black)),
		    send(ParNode, colour, black)
		    % send(ParNode, displayed, @on)
		   )
	)),
        retractall(settings(quantities_with_low_degree, _)),
        asserta(settings(quantities_with_low_degree, Threshold)).

settings(_F, _, _OnOrOff). %gp3 0.4.4: used to be a write_ln call only





% layout(F): tries to improve the layout of the state-transition graph
%
layout(F) :->
	"Run graph layout"::
        ViewType = @app<<-setting(simulation_view_type),
	% only relevant when view_type is graph (to do: deactivate layout option in case of table_view)
	if ViewType == graph
        then
        (
	  send(F, layout_states),
	  send(F, layout_terminations)
        ).


% layout_states(F): tries to improve the layout of the state-transition graph
%
layout_states(F) :->
	"Run graph layout"::
        ViewType = @app<<-setting(simulation_view_type),
	% only relevant when view_type is graph (to do: deactivate layout option in case of table_view)
	ViewType == graph,!,
	P = F<<-client, %gp3
%	(   get(P?graphicals, head, Head)
        input_to_int(input, InputInt),
	(   get(F, node, 'input', state, InputInt, Head)
	->
	    % layout parameters, with default values:
	    % spring strength (2)
	    % natural spring length (30)
	    % outward force between unconnected nodes (15)
	    % how much the network is changed every time (15)
	    % number of iterations (100)
	    % get(P, graphicals, GrsChain),
	    % write_chain(GrsChain),
	    % chain_list(GrsChain, List),
	    % length(List, Nr),
            % writef('Nr of graphicals: %d\n', [Nr]),
            % for the experiment! the first scenario has 35 elements,
            % the 2nd many more. nov 2001
            %(	catch((Nr < 40), _, fail)
            %->
%	      send(Head?device, layout, 2, 30, 7, 1, 10), % length was 15
	      send(Head?device, layout, 2, 15, 5, 15, 200), % length was 15
	      send(Head?device, layout, 5, 15, 5, 2, 150), % length was 15
	      send(Head?device, layout, 5, 15, 2, 2, 50), % length was 15
	    %;
            %  true
	    %),

	    % send(Head, layout, 2, 20, 5, 10, 50),
	    % send(Head, layout, 3, 20, 3, 2, 200),
	    get(P, bounding_box, area(X, Y, _W, _H)),
	    text_margin(XMargin, YMargin),
	    send(P, scroll_to, point(X-XMargin, Y-YMargin))
	;   send(F, report, error, 'No graph to layout') % No graph to layout!
	).

layout_states(_F) :->
        ViewType = @app<<-setting(simulation_view_type),
	% when view_type is table, do nothing
	ViewType == table.


% layout_terminations(F): tries to improve the layout of the terminations in
% the state-transition graph. This will probably place them close to their
% originating state, spread approximately evenly around it.
%
layout_terminations(F) :->
	"Run graph layout"::
        ViewType = @app<<-setting(simulation_view_type),
	% only relevant when view_type is graph (to do: deactivate layout option in case of table_view)
	ViewType == graph,!,
	P = F<<-client, %gp3
	% layout parameters, with default values:
	% spring strength (2)
	% natural spring length (30)
	% outward force between unconnected nodes (15)
	% how much the network is changed every time (15)
	% number of iterations (100)

	% for all state nodes, do a layout one level deep
        \+ get(P, find, @default,
        and(
		message(@arg1, instance_of, state_node),
		@arg1?type == state,
			message(@arg1, layout, 2, 30, 10, 10, 20),
			message(@arg1, layout, 5, 5, 5, 1, 200),
		new(or)
	   ), _StateNode
	),

	get(P, bounding_box, area(X, Y, _W, _H)),
	text_margin(XMargin, YMargin),
	send(P, scroll_to, point(X-XMargin, Y-YMargin)).

layout_terminations(_F) :->
        ViewType = @app<<-setting(simulation_view_type),
	% when view_type is table, do nothing
	ViewType == table.



mild_layout(F) :->
	"Run graph layout"::
        ViewType = @app<<-setting(simulation_view_type),
	% only relevant when view_type is graph (to do: deactivate layout option in case of table_view)
	ViewType == graph,!,
	P = F<<-client, %gp3
	(   get(P?graphicals, head, Head)
	->
	    % layout parameters, with default values:
	    % spring strength (2)
	    % natural spring length (30)
	    % outward force between unconnected nodes (15)
	    % how much the network is changed every time (15)
	    % number of iterations (100)
%	    get(Head, network, NetworkChain),
%	    write_chain(NetworkChain),
	    send(Head, layout, 2, 10, 2, 10, 200),
%	    send(Head, layout, 2, 20, 5, 10, 200),
	    get(P, bounding_box, area(X, Y, _W, _H)),
	    text_margin(XMargin, YMargin),
	    send(P, scroll_to, point(X-XMargin, Y-YMargin))
%	;   send(F, report, error, '')
	;   send(F, report, error, 'No graph to layout') % No graph to layout!
	).

mild_layout(_F) :->
        ViewType = @app<<-setting(simulation_view_type),
	% when view_type is table, do nothing
	ViewType == table.


postscript(F) :->
	"Create PostScript in file"::
	if
		get(@garp3_finder, file, F, 'Save Diagram to EPS file', @off, '.eps', FileName)
	then
	(
		%get(F, member, picture, Pict),
		Pict = F<<-client, %gp3
		new(File, file(FileName)),
		send(File, open, write),
		send(File, append, Pict?postscript),
		send(File, close),
		send(File, done),
		send(F, statusText, string('Saved PostScript in %s', FileName)) %gp3 0.3 changed this from report
	).

simulateAll(_F) :->
    @app?mainMenu->>msgBox('This task will simulate all scenarios and save them (saved simulations with the same name as a scenario will be overwritten). This task can take a long time (depending on model complexity). The model is automatically saved after each simulation.', confirm, @on),
    design:getAllScenarios(@model, AllScenarios),
    chain_list(AllScenarios, Scenarios),
    forall(
	member(Scenario, Scenarios),
	(
	    send(@app, startFullSimulation, Scenario),
	    send(@app, saveSimulation2, Scenario?name),
	    send(@app, save)
	)
    ),
    get(@app, visigarp, VisiGarp),
    send(@app, loadSimulation(VisiGarp)).


gen_isa_hierarchy(F) :->
	new(FV, frame('Entity isa hierarchy')),
	FV->>icon(@simulate_icon),
	send(FV, append, new(P, picture)),
	display_hierarchy(P, entity_isa),
	send(new(D, dialog), below, P),
	send(D, append,
	     button(close, message(FV, destroy))),
        send(D, default_button, close),
	send(FV, open),
	send(FV, transient_for, F).


gen_isa_mf_hierarchy(F) :->
	new(FV, frame('Model fragments isa hierarchy')),
	FV->>icon(@simulate_icon),
	send(FV, append, new(P, picture)),
	display_hierarchy(P, mf_isa),
	send(new(D, dialog), below, P),
	send(D, append,
	    button(close, message(FV, destroy))),
        send(D, default_button, close),
	send(FV, open),
	send(FV, transient_for, F).


gen_apply_mf_hierarchy(F) :->
	new(FV, frame('Model fragment applies-to hierarchy')),
	FV->>icon(@simulate_icon),
	send(FV, append, new(P, picture)),
	display_hierarchy(P, mf_apply),
	send(new(D, dialog), below, P),
	send(D, append,
	     button(close, message(FV, destroy))),
        send(D, default_button, close),
	send(FV, open),
        send(FV, transient_for, F).

gen_causal_model(F) :->
	"Create causal graph for every state N in StateList"::
	get_selected_states_or_path(F, StateList),
	reverse(StateList, RevStateList),
	forall(member(N, RevStateList),
		(
	        gen_causal_model(F, N)
		)
	).





% if aggregation has been performed
%
state_to_be_shown(N, state_graph):-
        rec(aggregation, _AggregationOptionsPerformed),!,
        graph_node(state_graph(aggregated, noinput), N).

%
% if no aggregation has been performed, use original graph
%
state_to_be_shown(N, state_graph):-
        not(rec(aggregation, _AggregationOptionsPerformed)),!,
        graph_node(state_graph(original, noinput), N).

%
% for the big state graph, use graph_node(global_view, N)
%
state_to_be_shown(N, bigstate):-
        graph_node(global_view, N).

state_to_be_shown(N, ministate):-
        state_to_be_shown(N, state_graph).


% if aggregation has been performed, use aggregated graph
%
edge_to_be_shown(A, B, Type):-
        rec(aggregation, _AggregationOptionsPerformed),!,
        state_to_be_shown(A, Type),
        state_to_be_shown(B, Type),
        graph_edge(state_graph(aggregated, input), A, B, to).
%
% if no aggregation has been performed, use rec_transition
%
edge_to_be_shown(A, B, Type):-
        not(rec(aggregation, _AggregationOptionsPerformed)),!,
        state_to_be_shown(A, Type),
        state_to_be_shown(B, Type),
        rec_transition(A, B).

% special hack for transitions from input!
%
edge_to_be_shown(0, B, Type):-
        state_to_be_shown(0, Type),
        state_to_be_shown(B, Type),
        rec_transition(input, B).



% if aggregation has been performed, use aggregated state graph
%
path_to_be_shown(N1, N2, state_graph, _P, _L):-
        rec(aggregation, _AggregationOptionsPerformed),
        path(N1, N2, state_graph(aggregated, input), _P2, _L2).

%
% if no aggregation has been performed, use original graph
%
path_to_be_shown(N1, N2, state_graph, _P, _L):-
        not(rec(aggregation, _AggregationOptionsPerformed)),
        path(N1, N2, state_graph(original, input), _P2, _L2).





% for input state, not all options are available
%
gen_causal_model(F, N) :-
	N == 0,!,
	new(F2, causal_graph_viewer(parameter_relations,'Sim_DependenciesInState',@on)), %gp3 0.3.13 added helpid and request to create left_dialog
	send(F2, state_nr, N),
	send(F2, label, string('Dependencies in input state - Simulate')),
	send(F2, open),

	send(F2, transient_for, F),
	%gp3: we make sure the e-r model is drawn BEFORE we create the toggle menu
	%because a lot of stuff gets drawn as a side effect of creating the menu (!!!)
	%so we switched the next 2 calls
	send(F2, gen_entity_causal_model_input),

        % hide entities without quantities
	create_relations_toggle_menu(F2), %gp3 removed N
	show_hide(F2, e_without_q, @off),

	send(F2, layout_par),
	send(F2, layout),
	send(F2, fit), %gp3 0.3.13
	%%gp3 0.3.13: resize picture does not work at all. It aimed to reason about the needed size
	%%of the picture, but it just did not work. So now we use a standard size picture
	%%which has the same effect: automatic_zoom messes the graph up a bit
	%%so we might consider disabling automatic_zoom as well
	%resize_picture(F2),
        automatic_zoom(F2).

gen_causal_model(F, N) :-
	new(F2, causal_graph_viewer(parameter_relations,'Sim_DependenciesInState',@on)),  %gp3 0.3.13 added helpid and request to create left_dialog
	send(F2, state_nr, N),
	send(F2, label, string('Dependencies in state %s - Simulate', N)),
	send(F2, open),
	send(F2, transient_for, F),
	%gp3: we make sure the e-r model is drawn BEFORE we create the toggle menu
	%because a lot of stuff gets drawn as a side effect of creating the menu (!!!)
	%so we switched the next 2 calls
	send(F2, gen_entity_causal_model),
	create_relations_toggle_menu(F2), %gp3 removed N
	show_hide(F2, attributes, @off),
	show_hide(F2, values, @off),
	show_hide(F2, q_spaces, @off),
	show_hide(F2, derivatives, @off),
        % hide entities without quantities
	show_hide(F2, e_without_q, @off),
	send(F2, layout_par),
	send(F2, layout),
	send(F2, fit), %gp3 0.3.13
		%%gp3 0.3.13: resize picture does not work at all. It aimed to reason about the needed size
	%%of the picture, but it just did not work. So now we use a standard size picture
	%%which has the same effect: automatic_zoom messes the graph up a bit
	%%so we might consider disabling automatic_zoom as well
	%resize_picture(F2),
        automatic_zoom(F2).


% Patchy predicate that modifies the graph by showing/hiding
% selected type of relations
%
% gen_causal_model_rel(F2, N:int, Rel, OnOrOff) :->
%	"Modify graph of dependencies"::
%
gen_causal_model_rel(F2, N, Rel, @on):-
	%gp3 1.4 added RelationTerm
	(   Term = visualize:show_binary_model(N, Rel, From, To, Dir, RelationTerm)
	->
	    forall(user:Term,
		  (atomize(From, FromAtom),
		   atomize(To, ToAtom),
		   % write_ln(Term),
		   gen_dependency_or_inverse(F2, N, Rel, FromAtom, ToAtom, Dir, RelationTerm)
		  ))
	;   send(F2, report, error,
		 'Error in causal model relation generator')
	).



gen_causal_model_rel(F2, N, Rel, @off):-
	(   Term = visualize:show_binary_model(N, Rel, From, To, _Dir,_)
	->
%	    send(F2, clear),
	    forall(user:Term,
		(
%		   atomize(From, FromAtom),
%		   atomize(To, ToAtom),
%		   send(F2, colour_labeled_arc, FromAtom, ToAtom, Rel, N, white)
%		   write_ln(string('cm_rel off %s %s %s', From, To, Rel)),
		   hide_dependency_or_inverse(F2, From, To, Rel, N)
		))
	;   send(F2, report, error,
		 'Error in causal model relation generator')
	).


% gen_math_rel(F, N, Rel1, A, B, Dir, RelationTerm)
%
% generates visualization of math dependency A Rel1 B
% (incl. correspondences) in state N
%
% case of derivative correspondences
%
% a link is shown between the derivative symbols, but only if:
% - Q1 and Q2 currently have the relevant derivatives (i.e., corr. is active)
% - the derivative symbols are currently displayed in the figure
%
%gp3 1.4 added the RelationTerm argument: original engine term

gen_math_rel(F, N, Rel1, A, B, Dir, RelationTerm):-
        derivative_correspondence_relation(Rel1, VRel1),!,
%%%% begin changes  - AB, 25/09/2005
        % writef('derivative_correspondence: %d %d %d \n',[A, Rel1, B]),
%%%% end of changes - AB, 25/09/2005
        A =.. [Der1, Q1],
        B =.. [Der2, Q2],
	get(F, node, Q1, quantity, N, QNode1),
	get(F, node, Q2, quantity, N, QNode2),
	% get the derivative group node within the quantity nodes
        % Do the quantities currently have these derivatives,
        % and are the derivative (group) nodes visible?
        % if not, the correspondency can not be shown
        find_quantity_value(N, Q1, _RealVal, _QualVal, CurrentDer1),
	find_quantity_value(N, Q2, _RealVal2, _QualVal2, CurrentDer2),!,
        CurrentDer1 == Der1,
        CurrentDer2 == Der2,
	get(QNode1, member, derivative, DerGroupNode1),
	get(QNode2, member, derivative, DerGroupNode2),

	construct_code_str(A, B, VRel1, ACodeStr),
	send(F, display_labeled_arc2, ACodeStr, DerGroupNode1, DerGroupNode2, VRel1, N, Dir, RelationTerm).


% case of simple formula: A Rel B, where Val is a value
% e.g., greater(dead1, zero), or greater(dead1, normal(dead1))
%
% N.B. relations involving derivatives are not handled by this clause
%
gen_math_rel(F, N, Rel1, A, B, _Dir, RelationTerm):-
	atom(A),
%	atom(B),
	not_derivative_rel(Rel1),

	% check whether B occurs in quantity space of A
	engine:qspace(A, _Predicate, QSpaceList, _Fail),
%        send(@prolog, write_ln, string('test gen_math_rel atom 1QSpace', QSpaceList)),
	qspace_member(B, QSpaceList, _Index),
	strip_atomize(B, BAtom),
	text_margin(_LeftMargin, _TopMargin),
	get(F, node, A, quantity, N, NA),

	math_relation(Rel1, VRel),
	construct_code_str(A, B, VRel, ACodeStr),

	% check whether a node for this relation already existed
	% get the par_relations group node within entity node NA
	get(NA, member, par_relations, ParRelGroupNode),

	get(NA, member, quantity_space, QuantitySpaceNode),

	( get(ParRelGroupNode, member, ACodeStr, ParRelNode)
	->
	        true
	;
		% par rel node did not exist yet, so draw it
		% create new ParRelNode

		% get node for value in qspace
		get(QuantitySpaceNode, member, BAtom, BNode),

		new(ParRelNode, par_rel_node(ACodeStr, par_relation, N, BAtom, VRel)),
		%%TOOLTIP (gp3 1.4)
		%%get all the comments somehow connected to the RelationTerm
		relevant_comments(RelationTerm,N,Comments),
		ParRelNode->>tooltip(Comments,model),
		% display new ParRelNode in ParRelGroupNode subfigure
		send(ParRelGroupNode, display, ParRelNode,
			point(0, BNode?position?y))
	).




% case of complex formula: A0 Rel B0, where A0 is a quantity and B0 is a formula itself,
% like in equal(growth1, min(inflow1, outflow1)),
%
gen_math_rel(F, N, Rel1, A0, B0, _Dir, RelationTerm):-
	is_quantity(A0),
        % send(@prolog, write_ln, string('gen_math_rel complex formula: %s %s %s', A0, Rel1, B0)),
	get(F, node, A0, quantity, N, NA),

	math_relation(Rel1, VRel),
	construct_code_str(A0, B0, VRel, ACodeStr),
        % send(@prolog, write_ln, string('ACodeStr:', ACodeStr)),
	construct_math_str(N,A0, B0, Rel1, AMathStr),  %gp3 0.4.11: added state number, needed to find quantity design name
        % send(@prolog, write_ln, string('AMathStr:', AMathStr)),
	get(NA, member, quantity_space, QuantitySpaceNode),

	% check whether a node for this relation already existed
	% get the par_relations group node within entity node NA
	get(NA, member, par_relations, ParRelGroupNode),
	( get(ParRelGroupNode, member, ACodeStr, ParRelNode)
	->
	        true
	;
		% par rel node did not exist yet, so draw it
		% create new ParRelNode
		new(ParRelNode, par_rel_node(ACodeStr, par_relation, N, unknown, AMathStr)),

		%%TOOLTIP (gp3 1.4)
		%%get all the comments somehow connected to the RelationTerm
		relevant_comments(RelationTerm,N,Comments),
		ParRelNode->>tooltip(Comments,model),

		get(ParRelGroupNode, bottom_side, BS),
	        get(QuantitySpaceNode, height, H2),
		H0 is BS - 24,  % empirically determined...
	        max(H0, H2, MaxH),
		send(ParRelGroupNode, display, ParRelNode, point(0, MaxH))
	).



% case of formula: A0 Rel B0, where both A0 and B0 are
% structured values, like in:
% equal(normal(amount_of_fat1), normal(amount_of_fat2))
%
gen_math_rel(F, N, Rel, A0, B0, Dir, RelationTerm):-
	% check whether A0 and B0 occur in some quantity space of
	% parameters A and B
        % send(@prolog, write_ln, string('gen_math_rel between 2 vals of 2 pars', A0, Rel, B0)),


	A0 =.. [A, ParA],
	B0 =.. [B, ParB],

	engine:qspace(ParA, _PredicateA, QSpaceListA, _FailA),
	(
		qspace_member(A0, QSpaceListA, _IndexA)
	;
		qspace_member(A, QSpaceListA, _IndexA2)
	),

	engine:qspace(ParB, _PredicateB, QSpaceListB, _FailB),
	(
		qspace_member(B0, QSpaceListB, _IndexB)
	;
		qspace_member(B, QSpaceListB, _IndexB2)
	),

        % send(@prolog, write_ln, string('gen_math_rel 2 parameters: %s %s', ParA, ParB)),

	math_relation(Rel, VRel),
        % send(@prolog, write_ln, string('gen_math_rel draw arc for Rel: %s', VRel)),


	get(F, node, ParA, quantity, N, NA),
        % send(@prolog, write_ln, string('gen_math_rel NodeA: %s', NA)),
	get(F, node, ParB, quantity, N, NB),
        % send(@prolog, write_ln, string('gen_math_rel NodeB: %s', NB)),

	% construct string ACodeStr for the new information 'A0 VRel B0'
	construct_code_str(A0, B0, VRel, ACodeStr),
        % send(@prolog, write_ln, string('gen_math_rel. ACodeStr: %s', ACodeStr)),

	get(NA, member, quantity_space, QuantitySpaceNodeA),
	get(NB, member, quantity_space, QuantitySpaceNodeB),
        % send(@prolog, write_ln, string('gen_math_rel. QSNodeA: %s', QuantitySpaceNodeA)),

	% get node for value in qspace
	get(QuantitySpaceNodeA, member, A, ANode),
	get(QuantitySpaceNodeB, member, B, BNode),
        % send(@prolog, write_ln, string('gen_math_rel. ANode: %s', ANode)),

	% draw arc for the relation
	% draw labeled connection link
	% does it exist already?
	(   catch(get(F, my_tc, ACodeStr, _TC), _, fail)
	->
	    % send(@prolog, write_ln, string('link existed already', _TC1))
	    true
	;
	    send(F, display_labeled_arc2, ACodeStr, ANode, BNode, VRel, N, Dir, RelationTerm),
	    % connect the entities too, to enforce better layout
	    find_quantity_entity(N, ParA, _, _, _, EntityA),
	    find_quantity_entity(N, ParB, _, _, _, EntityB),
	    get(F, node, EntityA, entity, N, EANode),
	    get(F, node, EntityB, entity, N, EBNode),
	    get(string('%s_extra', ACodeStr), value, ACodeStr2),
	    new(_TC2, my_tagged_connection(ACodeStr2, EANode, EBNode, @graph_link2, layout, 'X', none))
	).



% case of complex formula: A Rel B, where one or both of A and B are formulas
% and the other is not a quantity
%
% The order of arguments is kept here as specified in the model
%
gen_math_rel_formulas(F, N, VRel, Aatom, Batom, _Dir, RelationTerm):-
	% check if A and B are formulas

	term_to_atom(A, Aatom),
	term_to_atom(B, Batom),

        (
          (
           % both A and B are formulas
           % Example: (growth1 + growth2) = (inflow1 - outflow1)
           % or: equal(plus(growth1, growth2), min(inflow1, outflow1)),
           formula(A),
	   formula(B)
          )
        ;  % OR
          (
           % A is a specific value and B is a formula
           % Example: max(size1) =< max(size2) + max(size3)
           is_value(A),
	   formula(B)
          )
        ;  % OR
          (
           % A is a formula and B is a specific value
           % Example: max(size2) + max(size3) >= max(size1)
	   formula(A),
           is_value(B)
          )
        ;  % OR
          (
           % A is a formula and B is zero
           % Example: size2 - size3 = zero
           formula(A),
	   B = zero
          )
        ;  % OR
          (
           % A is zero and B is a formula
           % Example: zero = size2 - size3
	   A = zero,
           formula(B)
          )
        ),

	% get or create dummy 'entity' node for 'general constraints'
	get(string('general constraints'), value, EntityStr),
	get(F, node, EntityStr, entity, N, NE),
        NE->>tooltip(string('General constraints: placeholder for displaying general constraints'),model),

	construct_math_str(N, A, A0, EntityStr), %gp3 0.4.11: added state number, needed to find quantity design name
	construct_math_str(N, B, B0, EntityStr),!, %gp3 0.4.11: added state number, needed to find quantity design name
        swritef(MathStr, '%w %w %w', [A0, VRel, B0]),

        % dummy quantity node NA
	get(F, node, 'constraints', quantity, N, NA),
        NA->>tooltip(string('Constraints: placeholder for general constraints, i.e., constraints between multiple quantities'),model),

	position_within(NA, NE),

	construct_code_str(A0, B0, VRel, ACodeStr), !,
        % writef('ACodeStr: %d \n', [ACodeStr]),

	get(NA, member, quantity_space, QuantitySpaceNode),

	% check whether a node for this relation already existed
	% get the par_relations group node within entity node NA
	get(NA, member, par_relations, ParRelGroupNode),
	( get(ParRelGroupNode, member, ACodeStr, ParRelNode)
	->
	        true
	;
		% par rel node did not exist yet, so draw it
		% create new ParRelNode
		new(ParRelNode, par_rel_node(ACodeStr, par_relation, N, unknown, MathStr)),
	        % writef('new par rel node: %d \n', [ParRelNode]),
		% display new ParRelNode in ParRelGroupNode subfigure
		get(ParRelGroupNode, bottom_side, BS),
	        get(QuantitySpaceNode, height, H2),
		H0 is BS - 24,  % empirically determined...
	        max(H0, H2, MaxH),
		send(ParRelGroupNode, display, ParRelNode, point(0, MaxH)),
		%%TOOLTIP (gp3 1.4)
		%%get all the comments somehow connected to the RelationTerm
		relevant_comments(RelationTerm,N,Comments),
		ParRelNode->>tooltip(Comments,model),
		% add invisible connections to all entities to improve layout
                Term = visualize:find_instance(N, Entity, Type),
	        forall(user:Term,
		  (
		   %gp3 0.3.16: skip all names starting with 'garp_' (internal)
		   unless
			Entity->>prefix('garp_')
		   do
                   (
		    create_entity_name(F, Entity, Type, EntityName),
		    (get(F, node, EntityName, entity, N, NAE)
		    ->
		       % entity node NAE found
		       get(string('%s_extra', ACodeStr), value, STR2),
		       new(_TC, my_tagged_connection(STR2, NE, NAE, @graph_link2, layout, 'X', none))
		     ;
		       % not sure if this ever happens...
		       % node NAE was not an existing entity node
                       % writef('node NAE: %d was not an existing entity node, so destroy\n',[EntityName]),
		       send(NAE, destroy)
		     )
                    )
		  )
		)
	).


formula(F):-
	F =.. [Rel, _A0, _B0],
	math_relation(Rel, _VRel).

is_value(F):-
        % F is of the form max(size), i.e., value(quantity)
        % note that it can not be an interval value
        F =.. [_V, Q] ,
	% check whether V occurs in quantity space of Q
	engine:qspace(Q, _Predicate, QSpaceList, _Fail),
	qspace_member(F, QSpaceList, _Index).


gen_derivatives(F, N):-
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
		gen_derivative(F, N, QuantityName, QV, Der)
	).



% gen_derivative(F, N, Parameter, Value, Derivative):-
%
% creates a symbol besides the current Value of Parameter,
% if that value is shown - otherwise, just show it somewhere else!
% The symbol shown is as follows:
% Derivative = plus: arrow/triangle pointing upwards
% Derivative = zero: circle
% Derivative = min: arrow/triangle pointing downwards
% Derivative = unknown: ?
%
% N.B. this is only for the value of the derivative, not for
% the par_relations involving derivatives
%
gen_derivative(F, N, Par, Val, Der):-
%	write_ln('gen_derivative:'),
%	write_ln(string('N: %s, Par: %s, Val: %s Der: %s', N, Par, Val, Der)),
	strip_atomize(Val, QVAtom),
	text_margin(_LeftMargin, _TopMargin),
	get(F, node, Par, quantity, N, ParNode),

	% check whether a node for this relation already existed
	% get the par_relations group node within quantity node ParNode
	get(ParNode, member, derivative, DerGroupNode),

	get(ParNode, member, quantity_space, QuantitySpaceNode),

%	visualize(Der, VDer),
	get(string('d=%s', Der), value, DStr),
%        send(@prolog, write_ln, string('derivative group node:', DerGroupNode)),
%        send(@prolog, write_ln, string('qspace node:', QuantitySpaceNode)),
	( get(DerGroupNode, member, DStr, _DerNode)
	->
	        true
%		send(@prolog, write_ln, string('Derivative node already existed:', DerNode))
	;
%	        send(@prolog, write_ln, string('Derivative node did not exist:', DStr)),
		% Derivative node did not exist yet, so draw it
		% create new Derivative Node
%		new(DerNode, derivative_node(DStr, derivative, N, QVAtom, VDer)),
		create_derivative_graphical(Der, DerGr),


		% get node for value in qspace, if it exists
		( get(QuantitySpaceNode, member, QVAtom, QVNode)
		->
			% Value node existed, so draw derivative besides it
			get(QVNode?position, x, QVX),
			get(QVNode?position, y, QVY),
			get(QVNode, width, QVW),
			X is QVX + QVW + 8,
			Y is QVY + 5
		;
			% Value node did not exist, so value may be unknown
			% draw derivative somewhere at right bottom side
			% of quantity space node
			get(QuantitySpaceNode?position, x, QSX),
			get(QuantitySpaceNode?position, y, QSY),
			get(QuantitySpaceNode, width, QSW),
			% get(QuantitySpaceNode, height, QSH),
			X is QSX + QSW + 8,
			Y is QSY
		),
		% display new DerNode in DerGroupNode subfigure
%		send(DerGroupNode, display, DerNode,
%			point(X, Y))
		send(DerGroupNode, display, DerGr,
			point(X, Y))
	).


%gp3 0.3: we changed this to bitmaps

% Derivative == Unknown (variable)?
%
create_derivative_graphical(X, DerGr):-
	var(X),!,
	new(DerGr, text('')),
	% new(DerGr, text('?')),
	% send(DerGr, font, bold),
	send(DerGr, format_center).


% Derivative == plus
%
create_derivative_graphical(plus, DerGr):-
	new(DerGr, line(0, 0, 0, 3, first)),
	send(DerGr?first_arrow, length, 7), % default = 10
	send(DerGr?first_arrow, wing, 9), % default = 7
	send(DerGr?first_arrow, pen, 0). % default = 0?

% Derivative == min
%
create_derivative_graphical(min, DerGr):-
        % it seems that a slightly longer line is better for the position
	new(DerGr, line(0, 0, 0, 7, second)),
	% its strange, but this arrowhead needs to be smaller
	% to look the same size as the upwards arrow!
	% send(DerGr?second_arrow, length, 6), % default = 10
	% send(DerGr?second_arrow, wing, 8), % default = 7
	send(DerGr?second_arrow, length, 7), % default = 10
	send(DerGr?second_arrow, wing, 8), % default = 7
	send(DerGr?second_arrow, pen, 0). % default = 0?


% Derivative == zero
%
create_derivative_graphical(zero, DerGr):-
	new(DerGr, circle(7)),
        send(DerGr, pen, 1),
	send(DerGr, fill_pattern, colour(black)).


% Derivative == unknown
%
create_derivative_graphical(unknown, DerGr):-
	new(DerGr, text('')).

% Derivative == ?
%
create_derivative_graphical('?', DerGr):-
	new(DerGr, text('')).



gen_quantity_values(F) :->
        % Show quantities for every state N in StateList
	get_selected_states_or_path(F, StateList),
	reverse(StateList, RevStateList),
	forall(member(N, RevStateList),
		(
	        show_quantity_values(F, N) %gp3 0.1: added F to make the valuelist transient
		)
	).




% 2nd Derivative == Unknown (variable)?
%
create_2nd_derivative_graphical(X, DerGr):-
	var(X),!,
	new(DerGr, text('')),
	send(DerGr, format_center).


% 2nd_Derivative == pos
create_2nd_derivative_graphical(pos, DerGr):-
	new(DerGr, line(0, 0, 0, 1, first)),
	send(DerGr?first_arrow, length, 4), % default = 10, small 3
	send(DerGr?first_arrow, wing, 8), % default = 7, small 5
	send(DerGr?first_arrow, pen, 0). % default = 0?

% 2nd_Derivative == neg
create_2nd_derivative_graphical(neg, DerGr):-
	!,
	new(DerGr, line(0, 0, 0, 1, second)),
	send(DerGr?second_arrow, length, 4), % default = 10
	send(DerGr?second_arrow, wing, 6), % default = 7
	send(DerGr?second_arrow, pen, 0). % default = 0?

% 2nd_Derivative == zero
create_2nd_derivative_graphical(zero, DerGr):-
	!,
	new(DerGr, circle(4)),
        send(DerGr, pen, 1),
	send(DerGr, fill_pattern, colour(black)).

% 2nd_Derivative == unknown
create_2nd_derivative_graphical(_, DerGr):-
	new(DerGr, text('')),
	send(DerGr, format_center).




% 3rd Derivative == Unknown (variable)?
%
create_3rd_derivative_graphical(X, DerGr):-
	var(X),!,
	new(DerGr, text('')),
	send(DerGr, format_center).


% 3rd_Derivative == pos
create_3rd_derivative_graphical(pos, DerGr):-
	new(DerGr, line(0, 0, 0, 1, first)),
	send(DerGr?first_arrow, length, 4), % default = 10
	send(DerGr?first_arrow, wing, 8), % default = 7
	send(DerGr?first_arrow, pen, 0). % default = 0?

% 3rd_Derivative == neg
create_3rd_derivative_graphical(neg, DerGr):-
	!,
	new(DerGr, line(0, 0, 0, 1, second)),
	send(DerGr?second_arrow, length, 4), % default = 10
	send(DerGr?second_arrow, wing, 6), % default = 7
	send(DerGr?second_arrow, pen, 0). % default = 0?

% 3rd_Derivative == zero
create_3rd_derivative_graphical(zero, DerGr):-
	!,
	new(DerGr, circle(4)),
        send(DerGr, pen, 1),
	send(DerGr, fill_pattern, colour(black)).


/*
% 3rd_Derivative alternative: everybody gets a dot
create_3rd_derivative_graphical(TOD, DerGr):-
	TOD \= unknown,
	TOD \= nil,
	!,
%	new(DerGr, line(0, 0, 2, 0, second)),
%	send(DerGr?second_arrow, length, 0), % default = 10
%	send(DerGr?second_arrow, wing, 0), % default = 7
%	send(DerGr?second_arrow, pen, 2). % default = 0?
	new(DerGr, circle(2)),
        send(DerGr, pen, 1),
	send(DerGr, fill_pattern, colour(black)).
*/

% 3rd_Derivative == unknown
create_3rd_derivative_graphical(_, DerGr):-
	new(DerGr, text('')),
	send(DerGr, format_center).



gen_quantity_values(F, N) :-
        % Show quantities for state N
        show_quantity_values(F,N). %gp3 0.1: added F to make the valuelist transient



gen_model_fragments(F) :->
        % Show list of model fragments for every state N in StateList
%	send(@prolog, write_ln, 'gen model fragments list'),
%	str_to_statelist(StateListStr, StateList),
	get_selected_states_or_path(F, InputStateList),
	delete(InputStateList, 0, StateList),
	StateList \== [],!,
	reverse(StateList, RevStateList),
	forall(member(N, RevStateList),
		(
		gen_model_fragments(F, N)
		)
	).


gen_system_elements(F) :->
        % Show system elements for every state N in StateList
	get_selected_states_or_path(F, StateList),
	reverse(StateList, RevStateList),
        % Show system elements for state N
	forall(member(N, RevStateList),
		(
		gen_system_elements(F, N)
		)
	).


gen_system_elements(F, N) :-
        % Show system elements for state N
	new(F2, causal_graph_viewer(entities_and_attributes,'Sim_EntitiesAndRelationsInState')), %gp3 0.3.13 added helpid
	%gp3 : this code moved from show_system_elements, because it should not run when
	%the system elements are shown for causal graph or mf display etc
	send(F2, state_nr, N),
	send(F2, label, string('Entities and relations in state %s - Simulate', N)),
	send(F2, open),
	%gp3 0.2: setting transient_for before opening lets the wm know this one is transient
	%this makes the frame unresizable and allways higher z-ordered than ?transient_for
	%so we moved this call after open, which makes it a toplevel for the wm
	%but still part of the frames ?transients (used in restore/destroy etc)

	send(F2, transient_for, F),
	%
	show_system_elements(F2, N),
	% get(F2, member, picture, P),
	%%gp3 0.3.13: resize picture does not work at all. It aimed to reason about the needed size
	%%of the picture, but it just did not work. So now we use a standard size picture
	%%which has the same effect: automatic_zoom messes the graph up a bit
	%%so we might consider disabling automatic_zoom as well
	%%resize_picture(F2),
        automatic_zoom(F2).
	% send(P, request_geometry).


gen_state_graph(F) :->
	"Create graph from states and transitions"::
	gen_states(F),
	gen_transitions(F),
        automatic_zoom_sg(F),
        %get(F, picture_member, P),
        P = F<<-client, %gp3
        % get(P, graphicals, Grs),
	text_margin(LeftMargin, TopMargin),
        normalise_margin(P, LeftMargin, TopMargin).
%	write_ln('gen_state graph done').


normalise_margin(P, LeftMargin, TopMargin):-
        get(P, bounding_box, area(X, Y, _W, _H)),
        send(P, scroll_to, point(X-LeftMargin, Y-TopMargin)).



normalise_selection(P, _LeftMargin, _TopMargin):-
        get(P, selection, SelGrs),
	chain_list(SelGrs, GrsList),
        GrsList \== [],!,
        get(SelGrs, head, Head),
        get(Head?area, center, point(XC, YC)),
        get(P, visible, area(_XV,_YV, WV,HV)),
        send(P, scroll_to, point(XC-WV/2, YC-HV/2)).

% if there is no selection
normalise_selection(P, LeftMargin, TopMargin):-
        normalise_margin(P, LeftMargin, TopMargin).



gen_states(F):-
	% include input model as a kind of state
	%gp3 0.2: also add a tooltip for this input system
        input_to_int(input, InputInt),!,
	get(F, node, 'input', state, InputInt, InputNode),
	%gp3 0.2: find the input system name for the node
	if
	(
		IS = @app<<-currentScenario, %we make sure we donot fail
		IS \= @nil
	)
	then
	(
		TT *= string('%s',IS?name),
		unless
			0 = IS?remarks<<-size
		do
			TT->>ensure_nl(IS?remarks),
		InputNode->>tooltip(TT,model)
	),

	OnOrOff = @app<<-setting(input_state), %gp3
        send(InputNode, displayed, OnOrOff),
	forall(	engine:state(N, _SMD),
	    (
		get(F, node, N, state, N, _Node),
		F->>updateStateStatus(N) %new gp3 call
	    )
	).


gen_transitions(F):-
	% include 'semi-transitions' between input and start states
	% write_ln('gen_transitions'),
	forall(rec_transition(input, StartState),
		(
		 % write_ln('gen_transitions 1a'),
		 send(F, display_arc, input, StartState)
		 % layout after every arc - a bit expensive!
		 % send(F, mild_layout)
		)
	),
	% real terminations and state transitions
	findall(FromNr/ToNr/ToStates/Status,
		(
		engine:state_to(FromNr, ToList),
		nth1(ToNr, ToList, to(_, _, _, to_state(ToStates),
							Status))
		),
		AllTo
	),
	engine:transition_names(AllTo, Transitions),
	% actually, the way they are sorted is not very good
	% 11a comes between 1a and 2a, for example!
	% using predsort could lead to improvements
	% and it would be even better to use the ordering
	% following from traversing the network
	sort(Transitions, SortedTransitions),
	forall( member(T, SortedTransitions),
		(
			gen_transition(F, T)
			% write_ln(string('Transition shown', T))
		)
	).



gen_transitions_still(F):-
	% include 'semi-transitions' between input and start states
	% write_ln('gen_transitions'),
	forall(rec_transition(input, StartState),
		(
		 % write_ln('gen_transitions 1a'),
		 send(F, display_arc_still, input, StartState)
		 % layout after every arc - a bit expensive!
		 % send(F, mild_layout)
		)
	),
	% real terminations and state transitions
	findall(FromNr/ToNr/ToStates/Status,
		(
		engine:state_to(FromNr, ToList),
		nth1(ToNr, ToList, to(_, _, _, to_state(ToStates),
							Status))
		),
		AllTo
	),
	engine:transition_names(AllTo, Transitions),
	% actually, the way they are sorted is not very good
	% 11a comes between 1a and 2a, for example!
	% using predsort could lead to improvements
	% and it would be even better to use the ordering
	% following from traversing the network
	sort(Transitions, SortedTransitions),
	forall( member(T, SortedTransitions),
		(
			gen_transition_still(F, T)
			% write_ln(string('Transition shown', T))
		)
	).



gen_transition(F, TransitionName):-
	find_transition_data(TransitionName, FromState, _CausesList,
				_ConditionsList, _ResultsList,
				ToStateList, Status),
	gen_transition(F, FromState, TransitionName, ToStateList, Status).



gen_transition_still(F, TransitionName):-
	find_transition_data(TransitionName, FromState, _CausesList,
				_ConditionsList, _ResultsList,
				ToStateList, Status),
	gen_transition_still(F, FromState, TransitionName, ToStateList, Status).

find_transition_data(TransitionName, FromState, CausesList,
			ConditionsList, ResultsList,
				ToStateList, Status):-
        decompose_transition_name(TransitionName, FromState, TransitionNr),
	engine:state_to(FromState, ToList),
	nth1(TransitionNr, ToList, To),
	To = to(cause(CausesList),
		conditions(ConditionsList),
		results(ResultsList),
		to_state(ToStateList),
		Status).




% Complex version, with termination nodes in between
% only do this for closed terminations without successors
% in other words, with an empty ToStateList
%
gen_transition(F, N, TransitionName, ToStateList, closed):-
	ToStateList == [],
	% write_ln(string('gen_transition: closed', TransitionName)),
	% write_ln('empty ToStateList'),
	% draw connections to all states this transition connects to
	% get(F, node, N, state, N, SMFromNode),
	% get(SMFromNode, member, N, FromNode),
	get(F, node, N, state, N, FromNode),
%	get(FromNode, device, SMFromNode),
	get(F, termination_node, TransitionName, N, TransNode),
%	send(SMFromNode, display, TransNode, point(35,35)),
	send(TransNode, t_status, closed),
	send(F, display_arc2,
		TransitionName, FromNode, TransNode, TransitionName, closed, N, none),
	forall(member(To, ToStateList),
		(
		 % get(F, node, To, state, To, SMToNode),
		 % get(SMToNode, member, To, ToNode),
		 get(F, node, To, state, To, ToNode),
		 send(F, display_arc2,
			TransitionName, TransNode, ToNode, TransitionName, succeeded, N, second),
		 send(TransNode, t_status, succeeded)
%		 send(F, display_arc, N, To),
		 % layout after every arc - a bit expensive!
		 % send(F, mild_layout)
		)
	),!.



% Simple version: direct arrows between from and to state
%
%
gen_transition(F, State, TransitionName, ToStateList, closed):-
	% write_ln(string('gen_transition: closed', TransitionName)),
	% erase temporary transition node, if it exists
	( catch(get(F, termination_node, TransitionName, State,
		TransNode), _, fail)
	->
		% write('TransNode found: '),
		% write_ln(TransNode),
		% write_ln(': TransNode destroyed')
		send(TransNode, destroy)
	;
		% write('No TransNode found'),
		true
	),
	% write_ln(string('gen_transition: closed 2', TransitionName)),
	% draw connections to all states this transition connects to
	forall(member(To, ToStateList),
		(
		 send(F, display_arc, State, To)
		 % layout after every arc - a bit expensive!
		 % send(F, mild_layout)
		)
	),!.
	% write_ln(string('gen_transition: closed ready', TransitionName)).





% if Status == terminated OR ordered, then draw a special kind of
% node and connection
%
gen_transition(F, N, TransitionName, _ToStateList, Status):-
	Status \== 'closed',
	% write_ln(string('gen_transition: ', TransitionName)),
	% write_ln(string('Status: ', Status)),

	% get(F, node, N, state, N, SMFromNode),
	% get(SMFromNode, member, N, FromNode),

	get(F, node, N, state, N, FromNode),
	% write_ln(string('gen_transition FromNode: ', FromNode)),
	get(F, termination_node, TransitionName, N, TransNode),
%	write_ln(string('gen_transition TransNode: ', TransNode)),
%	send(SMFromNode, display, TransNode, point(35,35)),
	send(TransNode, t_status, Status),
%	write_ln(string('TransNode: ', TransNode)),
	send(F, display_arc2,
		TransitionName, FromNode, TransNode, TransitionName, Status, N, none).
%	send(F, display_arc, N, TransitionName),
%	write_ln(string('gen_transition displayed: ', TransitionName)),
	% layout after every arc - a bit expensive!
	% send(F, mild_layout).



% Simple version: direct arrows between from and to state
%
%
gen_transition_still(F, State, TransitionName, ToStateList, closed):-
	% write_ln(string('gen_transition: closed', TransitionName)),
	% erase temporary transition node, if it exists
	( catch(get(F, termination_node, TransitionName, State,
		TransNode), _, fail)
	->
		% write('TransNode found: '),
		% write_ln(TransNode),
		% write_ln(': TransNode destroyed')
		send(TransNode, destroy)
	;
		% write('No TransNode found'),
		true
	),
	% write_ln(string('gen_transition: closed 2', TransitionName)),
	% draw connections to all states this transition connects to
	forall(member(To, ToStateList),
		(
		 send(F, display_arc_still, State, To)
		 % layout after every arc - a bit expensive!
		 % send(F, mild_layout)
		)
	),!.
	% write_ln(string('gen_transition: closed ready', TransitionName)).





% lower_status(Low, High)
%
% succeeds when Low is a lower status than High, in the ordering
% unknown < terminated < ordered < closed < succeeded
%
% the status succeeded is an extra status to distinguish terminations
% which have resulted in a state-transition (succeeded) from
% terminations which didn't (closed). Both are called 'closed' in Garp!
%
lower_status(LowerStatus, Status):-
	lower_status1(LowerStatus, Status).

lower_status(LowerStatus, HighStatus):-
	lower_status1(LowerStatus, Status),
	lower_status(Status, HighStatus).

lower_status1(Unknown, Status):-
%gp3: interpreted and unprocessed are also valid now
%and for visualise purposed the same
	member(Unknown,[unknown,interpreted,unprocessed]),
	nonvar(Status).

lower_status1(terminated, ordered).
lower_status1(ordered, closed).
lower_status1(closed, succeeded).


% max_status(S1, S2, Max)
%
max_status(S1, S2, S2):-
	lower_status(S1, S2),!.

max_status(S1, _S2, S1).



% min_status(S1, S2, Min)
%
min_status(S1, S2, S2):-
	lower_status(S2, S1),!.

min_status(S1, _S2, S1).


% min_status_list(StatusList, TempMin, Min)
%
min_status_list([], TempMin, TempMin).

min_status_list([Status|Rest], TempMin, Min):-
	min_status(Status, TempMin, NewTempMin),
	min_status_list(Rest, NewTempMin, Min).



% show/hide terminations from state N with status == Status
%
% in case of @off, this procedure hides the termination nodes;
% that way the associated connection links are turned off
% automatically too.
%
show_hide_terminations(F, N, Status, OnOrOff):-
	P = F<<-client, %gp3
	%get(F, member, picture, P),
	engine:state(N, _),
	get(P, find, @default,	and(
				 message(@arg1, instance_of, state_node),
%				 message(@arg1, instance_of, connection),
%				 @arg1?from?state_nr == N,
				 message(@arg1, has_get_method, t_status),
				 @arg1?state_nr == N,
				 @arg1?type == termination,
				 @arg1?t_status == Status,
				 @arg1?displayed \== OnOrOff
				),
				Termination),
	% write_ln('Termination found to hide'),
	% get(Termination, name, TStr),
	% write_ln(TStr),
	% write('Status: '),
	% write_ln(Status),
	send(Termination, displayed, OnOrOff),
	( (OnOrOff == @on)
	->
		send(Termination, pen, 0),
		send(Termination, displayed, OnOrOff)
	;
		send(Termination, pen, 0),
		send(Termination, displayed, OnOrOff)
	),
%	write_ln('Termination display status updated'),
%	write_ln(Termination),
	show_hide_terminations(F, N, Status, OnOrOff).




show_hide_terminations(_F, _N, _Status, _OnOrOff).


% delete terminations from state N with status == Status
%
% in contrast to show_hide_terminations, delete is permanent
%
delete_terminations(F, N, Status):-
	P = F<<-client, %gp3
	engine:state(N, _),
	get(P, find, @default,	and(
				 message(@arg1, instance_of, state_node),
				 message(@arg1, has_get_method, t_status),
				 @arg1?state_nr == N,
				 @arg1?type == termination,
				 @arg1?t_status == Status
				),
				Termination),
	% Termination found to delete
	send(Termination, destroy),
	% Repeat
	delete_terminations(F, N, Status).

delete_terminations(_F, _N, _Status).


show_hide_transitive_reduction(F, @on):-
        gen_transitions_still(F),!.


% show/hide transitive edges: terminations for which another
% longer path exists which doesn't include the transition
%
% in case of @off, this procedure hides the transitive transitions;
%
show_hide_transitive_reduction(F, OnOrOff):-
        reduce_digraph(state_graph),
        forall(transitive_edge(state_graph, A, B),
            (
             writef('transitive edge %p -> %p  \n', [A, B]),
             show_hide_transition(F, A, B, OnOrOff)
            )
        ).



show_hide_transitive_input_transitions(F, @on):-
        gen_transitions_still(F),!.


% show/hide transitive edges: terminations for which another
% longer path exists which doesn't include the transition
%
% in case of @off, this procedure hides the transitive input transitions;
%
show_hide_transitive_input_transitions(F, OnOrOff):-
        reduce_digraph_input(state_graph),
        A = input,
        forall(transitive_edge(state_graph, A, B),
            (
             writef('transitive edge %p -> %p  \n', [A, B]),
             show_hide_transition(F, A, B, OnOrOff)
            )
        ).



show_hide_aggregate_orderings(F, @off):-
        % first, hide all transitions which are still visible
        forall(rec_transition(From, To),
	      (
               writef('Delete transition from %d -> %d. \n', [From, To]),
               show_hide_transition(F, From, To, @off)
              )
        ),
        % also, hide all aggregated transitions
        forall(graph_edge(state_graph(aggregated, input), A, B, to),
            (
             writef('Delete aggregated transition from  %p -> %p  \n', [A, B]),
             show_hide_transition(F, A, B, @off)
            )
        ),

        writef('Deleted all transitions, now show them all. \n', []),
	% then, show them all
        gen_transitions_still(F),!,
        writef('gen_transitions_still done. \n', []).


show_hide_aggregate_orderings(F, OnOrOff):-
        % first, hide all transitions
        forall(rec_transition(From, To),
	      (
               % writef('Delete transition from %d -> %d. \n', [From, To]),
               show_hide_transition(F, From, To, @off)
              )
        ),
        % also, hide all aggregated transitions
        forall(graph_edge(state_graph(aggregated, input), A, B, to),
            (
             writef('Delete aggregated transition from  %p -> %p  \n', [A, B]),
             show_hide_transition(F, A, B, @off)
            )
        ),

        writef('Turn aggregated transitions  %p  \n', [OnOrOff]),
        aggregate_digraph(state_graph(aggregated, input)),
        writef('aggregate digraph complete  \n', []), !,
        forall(graph_edge(state_graph(aggregated, input), A, B, to),
            (
             % writef('aggregated graph edge %p -> %p  \n', [A, B]),
             show_hide_transition(F, A, B, OnOrOff)
            )
        ),
        writef('show hide aggregate orderings complete  \n', []).



show_hide_transition(F, From, To, OnOrOff):-
%gp3 JJ: not sure if this is for the main window, hopefully, because thats what we changed it for
	(OnOrOff == @off
        ->
	    %get(F, member, picture, P),
	    P = F<<-client, %gp3
            atomize(From, A),
            atomize(To, B),
	    % there may be more connections present - why?
	    % repeat,
	    get(P, find, @default,
              and(
                 message(@arg1, instance_of, connection),
                 message(@arg1?from, instance_of, state_node),
                 message(@arg1?to, instance_of, state_node),
		 @arg1?from?name == A,
		 @arg1?to?name == B
	         ), Transition),

            % for some reason, the effect of setting displayed to @off
            % is over when the layout is changed!
	    % send(Transition, displayed, OnOrOff).
	    % get(Transition, name, Name),
	    % writef('Transition %d %d to destroy %d -> %d. \n', [Transition, Name, From, To]),
	    send(Transition, destroy)
        ;
	    send(F, display_aggregated_arc_still, From, To)
	    % writef('Transition drawn from %d -> %d. \n', [From, To])
        ).


show_hide_transition(_F, _From, _To, _OnOrOff).
%	writef('Transition  %d -> %d already turned %d? \n',
%                     [From, To, OnOrOff]).


%%%% begin changes  - AB 21/09/2005
select_path(F) :-
	get(F, textdialog_member, D),
	get(D?selected_states_member, selection, Str),
	debug(simulate(stgraph), 'Select path including the following states: ~w\n', [Str]),
	% check whether it's a valid SelectedStateList
	catch(term_to_atom(SelectedStateList, Str), _, fail),
	is_list(SelectedStateList),
	% write('Selected state list:'),
	% write_ln(SelectedStateList),
	debug(simulate(table_view), 'Valid state list ~w. Searching...', [SelectedStateList]),

	% find a path covering the selected states
	% (find_path(SelectedStateList, Path),
        send(D?selected_path_member, selection, 'Searching...'),
        send(D?selected_path_member, flush),
        send(D, synchronise),
	(catch(find_path(SelectedStateList, Path), _, fail),
		% path found
		!,
		select_path(F, Path)
	;
		% no path found
		no_path_found_procedure(F, SelectedStateList)
	).



% no_path_found_procedure(F, SelectedStateList)
%
% When no path was found, give an appropriate error message
% based on the number of selected states

% no state selected
%
no_path_found_procedure(F, []):-
	get(F, textdialog_member, D),
	select_states(F, []),
	send(D?selected_path_member, selection, 'Select some state(s)').
	% send(F, report, error,	'').


% only input state selected
%
no_path_found_procedure(F, [0]):-
	get(F, textdialog_member, D),
	select_states(F, [0]),
	send(D?selected_path_member, selection, 'Select other state(s)').
	% send(F, report, error,	'').



% one state selected
%
% Currently, [N] is already considered a path, so this is currently not active
no_path_found_procedure(F, [N]):-
	% write_ln('No path found'),
	get(F, textdialog_member, D),
	send(D?selected_path_member, selection, 'Shift-select another state'),
	select_states(F, [N]),
	select_states_transitions(F, [N]).
	% send(F, report, error,
	%	'Select and/or deselect (shift-mouseclick) other states.').

% more than one state selected, but no path was found
%
no_path_found_procedure(F, SelectedStateList):-
	SelectedStateList = [_N1, _N2 | _Rest],
	% write_ln('No path found'),
	get(F, textdialog_member, D),
	send(D?selected_path_member, selection, 'No path found'),
	select_states(F, SelectedStateList).
	% send(F, report, error,
	%	'No path was found between the selected states.').


select_path(F, Path) :-
	get(F, textdialog_member, D),
	% reset frame selection nr
	send(F, selection_nr, 0),
	%get(F, member, picture, P),
	P = F<<-client, %gp3
	% clear selection first
	reset_selection(P),
	forall(member(Nr, Path),
             select_state(F, Nr)
        ),
	select_path_transitions(F, Path),
	% update the path on the screen
	term_to_atom(Path, PathStr),
        send(D?selected_path_member, selection, PathStr).
	% send(F, report, error,	'').


%%%% end of changes - AB 21/09/2005


% in case of direct transitions between From and To
%
select_path_transitions(F, Path):-
	% write_ln('select path transitions A'),
	%get(F, member, picture, P),
	P = F<<-client, %gp3
	forall(pair_member(From/To, Path),
		(
			transition_name1(From, To, Name),
			% write('transition name:'),
			% write_ln(Name),
			get(P, find, @default,
			   and(
				@arg1?name == Name,
%				@arg1?type == termination,
				@arg1?displayed == @on,
				@arg1?selected == @off
			      ), Termination),
			% write('transition found:'),
			% write_ln(Termination),
			send(Termination, selected, @on)
		)
	).


% in case of indirect transitions between From and To, via
% transition node
%
select_path_transitions(F, Path):-
	forall(pair_member(From/To, Path),
		select_transition(F, From, To)
	).

select_transition(F, From, To):-
        ViewType = @app<<-setting(simulation_view_type),
	if ViewType == graph
        then
          select_graph_transition(F, From, To)
        else
          select_table_transition(F, From, To).



select_graph_transition(F, From, To):-
	debug(simulate(stgraph), 'select graph transition, from ~w to ~w...', [From, To]),
	transition_name(From, ToStateList, Name),
	member(To, ToStateList),
	%write_ln(Name),

	%get(F, member, picture, P),
	P = F<<-client, %gp3
	% currently, this doesn't necessarily find the right one!
	get(P, find, @default,	and(
				 @arg1?name == Name,
%				 message(@arg1, instance_of, graph_node),
%%				 message(@arg1, instance_of, connection),
%%				 @arg1?from?state_nr == N,
%				 message(@arg1, has_get_method, t_status),
%				 @arg1?state_nr == From,
				 @arg1?type == termination,
				 % @arg1?displayed == @on,
				 @arg1?selected == @off
				),
				Termination),
	get(Termination, name, TerminationName),
	debug(simulate(stgraph), 'Termination found ~w', [TerminationName]),
%	find_transition_data(TerminationName, From, _CausesList,
%			_ConditionsList, _ResultsList,
%				ToStateList, _Status),
%
	send(Termination, selected, @on),
	% select termination node as well as the connecting line and arrow
	select_transition(F, From, To).

% because the names of terminations become unreliable when
% there are > 30, this prevents that the program stops.
%
select_graph_transition(_F, From, To):-
        debug(simulate(stgraph), 'select_transition for transition from ~w to ~w.', [From, To]).


select_table_transition(F, From, To):-
	debug(simulate(stgraph), 'Select table transition, from ~w to ~w...', [From, To]),
	transition_name(From, ToStateList, Name),
	member(To, ToStateList),
	debug(simulate(stgraph), 'Transition name: ~w', [Name]),
	P = F<<-client, %gp3

	% select transition arrow
	get(P, find, @default,	and(
				 @arg1?name == Name,
				 @arg1?type == table_transition,
				 @arg1?toStateNr == To
				),
				Termination),
	debug(simulate(stgraph), 'Table transition found: ~w', [Termination]),
	get(Termination, name, TerminationName),
	debug(simulate(stgraph), 'Termination name ~w', [TerminationName]),
	send(Termination, selected, @on),
	send(Termination, expose),

	% and select termination text
	get(P, find, @default,	and(
				 @arg1?name == Name,
				 @arg1?type == table_termination
				),
				TerminationTxt),
	send(TerminationTxt, selected, @on),
	send(TerminationTxt, expose).



% select all transitions leading to another state from any
% of the states in StateList
%
select_states_transitions(F, StateList):-
	% select state transitions for all states in statelist
	forall(member(From, StateList),
		select_state_transitions(F, From)
	).





% select all termination nodes and connections from any
% of the states in StateList
%
select_states_terminations(F, StateList):-
	% select termination nodes and connections for all
	% states in statelist
	forall(member(From, StateList),
		(
		 select_state_terminations(F, From)
		)
	).


% select_state_terminations(F, N).
%
% select all terminations originating from state N
% if these are visible (termination nodes and connections)
%
% for input state (0), do nothing
%
select_state_terminations(_F, 0):-!.

% for any other state (From)
select_state_terminations(F, From):-
        ViewType = @app<<-setting(simulation_view_type),
	if ViewType == graph
        then
          select_state_graph_terminations(F, From)
        else
          select_state_table_terminations(F, From).


select_state_graph_terminations(F, From):-
	get(F, node, From, state, From, Node),
        get(Node, connections, Connections),
	get(Connections, find_all,
		   and(
			message(@arg1, instance_of, my_tagged_connection),
			 @arg1?displayed == @on,
			 @arg1?from?state_nr == From,
			 @arg1?selected == @off
		), Terminations),
        send(Terminations, for_all,
                   and(
                        % select connection
                        message(@arg1, selected, @on),
                        % select connected termination node
                        message(@arg1?to, selected, @on)
                   )
        ).

select_state_table_terminations(F, From):-
	debug(simulate(stgraph), 'select table terminations from state ~w ...', [From]),
	transition_name(From, _ToStateList, Name),
	debug(simulate(stgraph), 'Termination name: ~w', [Name]),
	P = F<<-client, %gp3
	get(P, find, @default,	and(
				 @arg1?name == Name,
				 @arg1?type == table_termination,
				 @arg1?selected == @off
				),
				Termination),
	debug(simulate(stgraph), 'Table termination text found: ~w', [Termination]),
	get(Termination, name, TerminationName),
	debug(simulate(stgraph), 'Termination name ~w', [TerminationName]),
	send(Termination, selected, @on),
	% continue until all are selected
	select_state_table_terminations(F, From).

select_state_table_terminations(_F, _From).




% select_state_transitions(F, N).
%
% select all transitions from a certain state N directly to another state
%
% for input state (0), do nothing
%
select_state_transitions(_F, 0):-!.
%
% To do. Select the grey arrows from input state (0) to start states.
% These arrows are instance_of connection, not my_connection.
% Therefore, they do not inherit the selection behaviour of becoming red when selected.
% For now, just succeed.
%
% select all transitions from state From
%
select_state_transitions(F, From):-
        ViewType = @app<<-setting(simulation_view_type),
	if ViewType == graph
	then
	    select_state_graph_transitions(F, From)
	else
	    select_state_table_transitions(F, From).


% select all graph transitions from state From
%
select_state_graph_transitions(F, From):-
	get(F, node, From, state, From, Node),
        get(Node, connections, Connections),
	get(Connections, find_all,
		   and(
			message(@arg1, instance_of, my_connection),
			 @arg1?displayed == @on,
			 @arg1?from?state_nr == From,
			 @arg1?selected == @off
		), Transitions),
        send(Transitions, for_all,
                   and(
                        % select connection
                        message(@arg1, selected, @on)
                   )
        ).


% select all table transitions from state From
%
select_state_table_transitions(F, From):-
        ViewType = @app<<-setting(simulation_view_type),
	ViewType == (table),!,
	forall(rec_transition(From, To),
	       select_table_transition(F, From, To)
        ).



% no state selected: fail
find_path([], _):-
        !,
        % write_ln('No state selected'),
	fail.

% input state selected: fail
find_path(L, _):-
        member(0,L),!,
        % write_ln('Input selected for path selection'),
	fail.


% only one state selected, check longer cycle
find_path([A], Path):-
        Graph = state_graph(original, noinput),
        SCP = @app<<-setting(select_cyclic_paths),
        SCP = @on,
        % writef('find_path: cycle from %d to %d\n',[A,A]),
        % cycle_bfs([A], Path),!
	cyclic_path(A, A, Graph, Path, _L),!.



% only one state selected (real start state), return path from A to an end state Z or end with a cycle
find_path([A], Path):-
        SFP = @app<<-setting(select_full_paths),
        SFP = @on,
        Graph = state_graph(original, noinput),
	real_start_node(Graph, A),!,
        % write_ln('find_path: from selected real start state A to end state Z or end loop'),

        findall(Z, end_node(Graph, Z), EndNodes),
        % writef('find_path: end nodes: %w\n',[EndNodes]),
        % my_write_list(EndNodes),

        (
             % find a path to one of the EndNodes
	     path([A], EndNodes, Graph, Path, _L),
             % path should consist of more than one state
             Path \== [A],!
        ;
             % if no path to an EndNode can be found, try to
             % find a path that includes a loop at the end
             % writef('No path to end state found\n',[]),
             % writef('Searching for shortest path to include a loop\n',[]),
             % extend the path to Y, just before the loop
             path([A], [Y], Graph, Path1, _L3),
             last(Path1, Y),
             Y \= A,
             graph_edge(Graph, Y, Z, to),
             % Z was already included in Path1, so this creates a loop
             member(Z, Path1),
             % add Z to create the loop
             conc(Path1, [Z], Path)
         ).


% only one state selected (end state), return path from begin state A to selected end state Z
find_path([Z], Path):-
        Graph = state_graph(original, noinput),
	end_node(Graph, Z),!,
        % write_ln('find_path: begin A to selected end state Z'),
        findall(A, start_node(Graph, A), StartNodes),
	path(StartNodes, [Z], Graph, Path, _L),!.



% only one state selected, and no full path wanted - return path to selected state
find_path([S], Path):-
        Graph = state_graph(original, noinput),
        SFP = @app<<-setting(select_full_paths),
        SFP = @off,
        findall(A, start_node(Graph, A), StartNodes),
        % writef('find_path: from start state to %d\n',[S]),
        single_bfs_path(StartNodes, [S], Path),!.



% only one state selected, return path from begin state A to end state Z via selected state S
% or end in a cycle
find_path([S], Path):-
        SFP = @app<<-setting(select_full_paths),
        SFP = @on,
        % write_ln('find_path: from begin state A via selected state S to end state Z, or end in a cycle'),
        unless
           multi_bfs_full_path([S], Path)
        do
        (
           multi_bfs_path_ending_in_cycle([S], Path)
           % writef('found path through selected states ending in cycle : %w\n',[Path])
        ).



% multiple states selected:
%
% first, try to find a cyclic path covering the selected states
%
find_path(SelectedStatesList, Path):-
        SCP = @app<<-setting(select_cyclic_paths),
        SCP = @on,
        % writef('find cyclic path through selected states: %w\n',[SelectedStatesList]),
        cycle_bfs(SelectedStatesList, Path).


% multiple states selected:
%
% find a path covering the selected states
%
find_path(SelectedStatesList, Path):-
        % check first whether this is a path through these states at all - this will fail sooner
        multi_bfs(SelectedStatesList, TempPath),
        SFP = @app<<-setting(select_full_paths),
        if SFP = @on
        then
        (
           % find a full path (= from begin to end state, or To do: ending with a cycle)
           % covering the selected states
           % writef('find FULL path through selected states: %w\n',[SelectedStatesList]),
           unless
           (
              multi_bfs_full_path(SelectedStatesList, Path)
           )
           do
           (
              % find a path through selected states ending in a cycle
              % writef('or find a path through selected states ending in a cycle: %w\n',[SelectedStatesList]),
              multi_bfs_path_ending_in_cycle(SelectedStatesList, Path),!
              % writef('found path through selected states ending in cycle : %w\n',[Path]),
           )
        )
        else
        (
           % return shortest path covering the selected states
           Path = TempPath
        ).




sublist_or_set(Sub, Path):-
	sublist(Sub, Path),!.

sublist_or_set(Sub, Path):-
	sublist_skip(Sub, Path),!.

sublist_or_set(Sub, Path):-
	subset(Sub, Path).





:- pce_global(@graph_link, make_graph_link).

make_graph_link(L) :-
        new(L, link(in, out, line(0,0,0,0,second))).


% just a line, no arrow
%
:- pce_global(@graph_line, make_graph_line).

make_graph_line(L) :-
        new(L, link(in, out, line(0,0,0,0,none))).

display_arc(F, From:name, To:name) :->
	"Display arc From -> To"::
	% write_ln('display_arc for transition: '),
	% get(F, member, picture, P),
        input_to_int(From, FromInt),
	transition_name1(From, To, Name),
	% write_ln(Name),
%	get(F, node, From, state, FromInt, SMFromNode),
	get(F, node, From, state, FromInt, FromNode),
	get(FromNode, device, SMFromNode),
%	get(SMFromNode, member, From, FromNode),
	from_to_int(FromInt, To, ToInt),
%	get(F, node, To, state, ToInt, SMToNode),
%	get(SMToNode, member, To, ToNode),
	get(F, node, To, state, ToInt, ToNode),
	get(ToNode, device, SMToNode),
	(   catch(get(F, my_c, Name, C), _, fail)
	->
	    % write_ln('link existed already'),
	    true
	;
	    % write_ln('create new link'),
	    % write('Move to-state: '),
	    % write(To), write(' '),
	    % write_ln(ToNode),
	    get(FromNode, absolute_position, point(X,Y)),
	    % get(SMFromNode, position, point(X,Y)),

	    (	catch(get(FromNode, connections, Connections), _, fail)
	    ->
		% write_chain(Connections),
		% get all transitions from FromNode - this determines
		% where on the y-axis ToNode will be displayed
		get(Connections, find_all,
		   and(
			message(@arg1, instance_of, my_connection),
			@arg1?from == FromNode
		   ), Transitions),
		% write_chain(Transitions),
		get(Transitions, size, NrOfTransitions)
	    ;
		NrOfTransitions is 0
	    ),
	    get(FromNode, size, size(Diameter, _Diameter)),
	    % write('Diameter: '), write_ln(Diameter),
	    % write('NrTransitions: '), write_ln(NrOfTransitions),
	    % write('X,Y: '), write(X), write(', '), write_ln(Y),
	    NX is X + (2.5 * Diameter),
	    NY is (NrOfTransitions * 1.5 * Diameter) + Y,
	    % write_ln('new position determined: '),
	    % write('X,Y: '), write(NX), write(', '), write_ln(NY),
	    send(SMToNode, position, point(NX, NY)),
	    % send(Node, center, point(NX, NY))

            % colour grey in case of connection from input state
            (From == 'input'
            ->
               % this is a normal connection, so it doesn't
               % inherit the methods of my_connection
	       % write_ln('create new input link'),
	       new(C, connection(FromNode, ToNode, @graph_link)),
	       send(C, name, Name),
               send(C, colour, colour(grey))
            ;
               % in all other cases
	       % write_ln('create new normal link'),
	       new(C, my_connection(FromNode, ToNode, @graph_link)),
	       send(C, name, Name)
            ),

	    % add an extra invisible connection to enable layout
	    % mechanism to work on the top-level nodes
	    new(C2, connection(SMFromNode, SMToNode, @graph_link)),
	    send(C2, displayed, @off),
	    % send(@pce, write_ln, 'hide link'),
	    send(C, pen, 2),		% default = 1?
	    send(C?second_arrow, length, 10), % default = 10
	    send(C?second_arrow, wing, 10), % default = 7
	    send(C?second_arrow, pen, 0), % default = 0?
            % make arrow active
            send(C, recogniser,
                  click_gesture(left, '', double,
		  %	message(@pce, write_ln, From, To),
			message(@prolog, show_transition, FromNode?state_nr, ToNode?state_nr) %gp3 0.3: make sure we get the int version
		  )
	    )
	).


% this version does not move the nodes involved
%
display_arc_still(F, From:name, To:name) :->
	"Display arc From -> To"::
	% write_ln('display_arc for transition: '),
	% get(F, member, picture, P),
        input_to_int(From, FromInt),
	transition_name1(From, To, Name),
	% write_ln(Name),
%	get(F, node, From, state, FromInt, SMFromNode),
	get(F, node, From, state, FromInt, FromNode),
	get(FromNode, device, SMFromNode),
%	get(SMFromNode, member, From, FromNode),
	from_to_int(FromInt, To, ToInt),
%	get(F, node, To, state, ToInt, SMToNode),
%	get(SMToNode, member, To, ToNode),
	get(F, node, To, state, ToInt, ToNode),
	get(ToNode, device, SMToNode),
	(   catch(get(F, my_c, Name, C), _, fail)
	->
	    % write_ln('link existed already'),
	    true
	;
            % colour grey in case of connection from input state
            (From == 'input'
            ->
               % this is a normal connection, so it doesn't
               % inherit the methods of my_connection
	       new(C, connection(FromNode, ToNode, @graph_link)),
	       send(C, name, Name),
               send(C, colour, colour(grey))
            ;
               % in all other cases
	       % write_ln('create new link'),
	       new(C, my_connection(FromNode, ToNode, @graph_link)),
	       send(C, name, Name)
            ),

	    % add an extra invisible connection to enable layout
	    % mechanism to work on the top-level nodes
	    new(C2, connection(SMFromNode, SMToNode, @graph_link)),
	    send(C2, displayed, @off),
	    % send(@pce, write_ln, 'hide link'),
	    send(C, pen, 2),		% default = 1?
	    send(C?second_arrow, length, 10), % default = 10
	    send(C?second_arrow, wing, 10), % default = 7
	    send(C?second_arrow, pen, 0), % default = 0?
            % make arrow active
            send(C, recogniser,
                  click_gesture(left, '', double,
		  %	message(@pce, write_ln, From, To),
			message(@prolog, show_transition, FromNode?state_nr, ToNode?state_nr) %gp3 0.3 make sure we use the int version, not name version
		  )
	    )
	).



% display_aggregated_arc_still
% does not move the nodes involved
%
display_aggregated_arc_still(F, From:name, To:name) :->
	"Display arc From -> To"::
	writef('display_arc for aggregated transition: %d -> %d \n', [From, To]),
	% get(F, member, picture, P),
        input_to_int(From, FromInt),
	transition_name1(From, To, Name),
	% write_ln(Name),
%	get(F, node, From, state, FromInt, SMFromNode),
	get(F, node, From, state, FromInt, FromNode),
	get(FromNode, device, SMFromNode),
%	get(SMFromNode, member, From, FromNode),
	from_to_int(FromInt, To, ToInt),
%	get(F, node, To, state, ToInt, SMToNode),
%	get(SMToNode, member, To, ToNode),
	get(F, node, To, state, ToInt, ToNode),
	get(ToNode, device, SMToNode),
	% writef('link From: %d, To: %d SM To: %d\n',[FromNode, ToNode, SMToNode]),

	(   catch(get(F, my_c, Name, C), _, fail)
	->
	    % write_ln('link existed already'),
	    % writef('link: %d, Name: %d, From %d -> %d \n',[C, Name, From, To]),
	    true
	;
            % colour grey in case of connection from input state
            (From == 'input'
            ->
               % this is a normal connection, so it doesnt
               % inherit the methods of my_connection
	       new(C, connection(FromNode, ToNode, @graph_link)),
	       % writef('link: %d, Name: %d, From %d -> %d \n',[C, Name, From, To]),
	       send(C, name, Name),
               send(C, colour, colour(grey))
            ;
               % in all other cases
	       % write_ln('create new link'),
	       new(C, my_connection(FromNode, ToNode, @graph_link)),
	       % writef('create link: %d, Name: %d, From %d -> %d \n',[C, Name, From, To]),
	       send(C, name, Name)
            ),

	    % add an extra invisible connection to enable layout
	    % mechanism to work on the top-level nodes
	    new(C2, connection(SMFromNode, SMToNode, @graph_link)),
	    send(C2, displayed, @off),
	    % send(@pce, write_ln, 'hide link'),
	    send(C, pen, 2),		% default = 1?
	    send(C?second_arrow, length, 10), % default = 10
	    send(C?second_arrow, wing, 10), % default = 7
	    send(C?second_arrow, pen, 0), % default = 0?
            % make arrow active
            send(C, recogniser,
                  click_gesture(left, '', double,
			% message(@pce, write_ln, 'make arrow ', [From, To]),
			message(@prolog, show_transition, FromNode?state_nr, ToNode?state_nr) %explicit asking for state_nr, that is an int, not a char_array
		  )
	    )
	).
	% writef('link: %d, Name: %d, From %d -> %d \n',[C, Name, From, To]),
	% writef('display_arc for aggregated transition: %d -> %d done \n', [From, To]).




display_arc2(F, STR:name, FromNode:node, ToNode:node, _Label:name, Status:name, _N:int, Dir:name) :->
	"Display arc From -> To"::
	% write_ln('display_arc2'),
	% write('STR: '),
	% write_ln(STR),
	(   catch(get(F, my_tc, STR, TC), _, fail)
	->
	    % send(@prolog, write_ln, string('link existed already', TC)),
	    send(TC, t_status, Status),
	    true
	;
	    new(TC, my_tagged_connection(STR, FromNode, ToNode,
				@graph_link, termination, Status, Dir)),
	    % send(@prolog, write_ln, string('created new link:', TC)),
	    send(TC, pen, 2),		% default = 1?
	    ( Dir == second
	    ->
		send(TC?second_arrow, length, 10), % default = 10
		send(TC?second_arrow, wing, 10), % default = 7
		send(TC?second_arrow, pen, 0) % default = 0?
	    ;
		true
	    ),
            % make arrow active
            send(TC, recogniser,
                  click_gesture(left, '', double,
	                           % message(@pce, write_ln, STR),
	                           message(@prolog,
					show_transition,
						STR)
		  )
	    )
	).



display_labeled_arc2(_F, STR:name, FromNode:node, ToNode:node, Rel:name, N:int, Dir:name, RelationTerm: prolog) :->
	"Display arc From -> To"::
	%gp3 1.4 added the RelationTerm (original engine data)
	new(TC, my_tagged_connection(STR, FromNode, ToNode, @graph_link, termination, Rel, Dir)),
	send(TC, pen, 2),		% default = 1?
	( Dir == second
	->
		send(TC?second_arrow, length, 10), % default = 10
		send(TC?second_arrow, wing, 10), % default = 7
		send(TC?second_arrow, pen, 0) % default = 0?
	;
		true
	),
        % make arrow active
        send(TC, recogniser,
                  click_gesture(left, '', double,
%	                           message(@pce, write_ln, STR),
	                           message(@prolog,
					show_transition,
						STR)
		  )
	),
        new(Icon, non_resizable_device),
	arc_label_size_causal(Size),
        send(Icon, display, new(C, circle(Size))),
        send(Icon, display, new(T, text(Rel))),

        new(_, constraint(C,T, identity(center_x))),
        new(_, constraint(C,T, identity(center_y))),
        send(T, background, white),
        send(C, colour, black),
%        send(C, fill, white),
        send(C, fill_pattern, colour(white)),
        % make relation name active
        send(Icon, recogniser,
                  click_gesture(left, '', double,
%	                           message(@pce, write_ln, STR),
	                           message(@prolog,
					show_transition,
						STR)
		  )
	),
	send(TC, tag, Icon),
	%%TOOLTIP (gp3 1.4)
write('class_visigarp.pl '), write(3172), write_ln(RelationTerm),
	%%get all the comments somehow connected to the RelationTerm
	relevant_comments(RelationTerm,N,Comments),
	Icon->>tooltip(Comments,model).




% the state_nr associated with the ToNode is the ToStateNr,
% if there is one
%
from_to_int(_FromInt, To, To):-
	integer(To),!.

% if not, the state_nr associated with the ToNode is the FromStateNr.
%
from_to_int(FromInt, _To, FromInt).



% necessary to transform the state name 'input' to an integer,
% at least, a string containing an integer.
input_to_int('input', 0).
input_to_int('input', -1).

input_to_int(X,X):-!.


%colour(white).
%colour(black).




node(F, Name:name, Type:name, StateNr:int, Node:graph_node) :<-
	"Get (find or create) node with specified name"::
	%depending on the type
	%gp3 note: termination nodes are created by termination_node
	%(legacy code), there we use a new class: state_termination_node
	%to try to get rid of type= -like code, and use more oo features

	Picture = F<<-client, %gp3
	% this doesn't find nodes inside nodes

	(
	    % check if node exists already
            get(F, find_node, Name, Node)
	;
	    % if node did not exist yet, create it
	    get(Picture, visible, area(X, Y, W, H)),
	    currentX(NrX, LastX),
	    currentY(NrY, LastY),

            % extra margin
            state_size(SH,SW),
	    MX is X + W - SW,
	    MY is Y + H - SH,

	    determine_position(LastX, LastY, MX, MY, NX, NY),

	    NewNrX is NrX + 1,
            retract(currentX(NrX, LastX)),
            assert(currentX(NewNrX, NX)),

	    NewNrY is NrY + 1,
            retract(currentY(NrY, LastY)),
            assert(currentY(NewNrY, NY)),

	    (Type == state
	    ->
                debug(simulate(inputnode), 'draw graph state Name: ~w, nr: ~w', [Name, StateNr]),

		send(Picture, display,
                     new(MasterNode, state_master_node(Name, Type, StateNr)),
							% point(0, 0)),
							point(NX, NY)),
		% write_ln('state_master_node drawn'),
		get(MasterNode, member, Name, Node)
	    ;
		% node is of another type than state
		send(Picture, display,
                     new(Node, state_node(Name, Type, StateNr)),
						% point(0, 0)),
						point(NX, NY))
		% write_ln('node drawn')
	    )
	).

% this clause just searches for a node, it does not create any
%
find_node(F, Name:name, Node:graph_node) :<-
	"Get (find) node with specified name"::
	%depending on the type
	%gp3 note: termination nodes are created by termination_node
	%(legacy code), there we use a new class: state_termination_node
	%to try to get rid of type= -like code, and use more oo features

	Picture = F<<-client, %gp3
	% this doesn't find nodes inside nodes

	(   get(Picture, find, @default, and(
				 @arg1?name == Name,
				 message(@arg1, instance_of, state_node) %gp3: state_termination_node is also an instance_of state_node
				), Node)
	->
	    debug(simulate(inputnode), 'node existed already', []),
	    true
	;
	    fail
        ).


termination_node(F, Name:name,StateNr:int, Node:graph_node) :<-
	"Get (find or create) termination node with specified name"::
	%gp3: create a new class for state_termination_node (see class_state_node.pl)
	%and removed argument Type (allways: termination...)

	P = F<<-client, %gp3
	% this doesn't find nodes inside nodes
	% (   get(P, member, Name, Node)
	(   get(P, find, @default, and(
				 @arg1?name == Name,
				 message(@arg1, instance_of, state_termination_node) %gp3: only the termination kind
				), Node)
	->
	    true
	;
	    % find originating state-node
	    term_to_atom(StateNr, StateNrStr),
	    get(P, find, @default, (@arg1?name == StateNrStr), StateNode),
	    get(StateNode, connections, Connections),
	    get(Connections, find_all,
		and(
			message(@arg1, instance_of, my_tagged_connection),
			@arg1?type == termination
		), Terminations),
	    get(Terminations, size, NrOfTerminations),
	    get(StateNode, device, StateMasterNode),
	    get(StateNode, size, size(Diameter, _Diameter)),
	    NX is 1.5 * Diameter,
	    NY is (NrOfTerminations + 1) * Diameter/2,
	    %gp3 changed class of node:
	send(StateMasterNode, display,
                    new(Node, state_termination_node(Name, StateNr)),
						point(NX, NY)),
	    send(Node, center, point(NX, NY)),
	    OnOrOff = @app<<-setting(termination_nodes), %gp: saved in app now
            send(Node, displayed, OnOrOff),
	    send(Node, pen, 0)
	).


% my_c(F, Name:name, C:my_connection) :<-
my_c(F, Name:name, C:connection) :<-
	"Get connection with specified name"::
	%get(F, member, picture, P),
	P = F<<-client, %gp3
        get(P, find, @default,
		and(
			message(@arg1, instance_of, connection),
			% message(@arg1, instance_of, my_connection),
			@arg1?name == Name
		), C
	).
        % only checking the name may return a state instead
        % of connection
	% get(Picture, member, Name, C).



my_tc(F, Name:name, TC:my_tagged_connection) :<-
	"Get tagged_connection with specified name"::
	%get(F, member, picture, P),
	P = F<<-client, %gp3
        get(P, find, @default,
		and(
			message(@arg1, instance_of, my_tagged_connection),
			@arg1?name == Name
		), TC
	).



determine_position(X, Y, MX, _MY, NX, NY):-
%	    currentX(Nr, _LastX),
            state_size(_H,W),
	    NX is X + W,
	    NX =< MX,!,
%            send(@prolog, write_ln, string('testing X: for state:', NX, Nr)),
	    NY is Y.


determine_position(X, Y, _MX, MY, NX, NY):-
%	    currentX(Nr, _LastX),
            state_size(H, _W),
	    NY is Y + H,
	    NY =< MY,!,
%            send(@prolog, write_ln, string('testing X: for state:', NX, Nr)),
	    NX is X.



determine_position(X, Y, _MX, _MY, NX, NY):-
%	    currentX(Nr, _LastX),
            state_size(H, W),
	    NX is X + W,
%            send(@prolog, write_ln, string('testing X: for state:', NX, Nr)),
	    NY is Y + H.



:- pce_end_class.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% history of parameter value in path of states
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


par_value_history(Parameter, PathStateList, IndexNValueDerivativeList) :-
	findall(Index/N/Value/Derivative,
		 (      % to be sure that the order
			% in which states are found is correct!
%			my_member(N, PathStateList),
			nth0(Index, PathStateList, N),

			engine:state(N, SMD),

			engine:smd_slots(SMD, _, _, _, Values, _, _, _),
			(memberchk(value(Parameter, _, Value, Derivative),
					Values)
				-> true
				;  Value = nonexistent
			)
		),
		 IndexNValueDerivativeList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
