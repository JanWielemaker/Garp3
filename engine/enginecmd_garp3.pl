/*
enginecmd_garp3.pl
Implements the commands you can send to the engine, using enginectrl:command, as defined in enginectrl_garp3.pl.
This code runs in the engine namespace, giving easy access to all engine internals and dynamic predicates.

Commands are called by the engine mainpump (in enginectrl:) like this: engine:do_cmd(C).

PART OF Garp3. SEE COPYRIGHT NOTICE.

runs in namespace  'engine'
*/



%cleanup all temp garp stuff: empty the model
%based on visialize/garp_clean_up.pl stuff (visigarp/BB/AB/SIHW).
%all dynamic predicates defined by the engine are asserted in the engine namespace, as are predicates loaded
%from legacy model files.
do_cmd(cleanup) :-!,

	@tracer->>trace('Clearing old simulation',general),
	@app->>clearExportedObjects, %make sure the exportedObjects hash is valid (filled in @model->>exportAllInternal)

    % In DL Wouter posted a bug [438] about number increasing over simulations. Which is exactly what you expect without resetting the number generator. [FL]
    %once((reset_gensym ; true)),
    % But this statement was commented out for years: Reset gensym IS evil.
    % It is a global reset and will most probably cause problems in other number generation uses in the prolog instance (e.g. in OWL generation). [FL]
    % Solution: specifically reset the bases that were previously used:
    reset_simulation_gensym, %uses current asserted info to find bases, must be done before before retracting. [FL]


    %%%engine working data & output
    retractall( state(_, _)),
    % Remove states
    retractall( state(_, _)),
    % Remove transitions
    retractall( state_from(_, _)),
    retractall( state_to(_, _)),
    retractall( state_values(_, _)), % new FL june 07
    % Remove state_status information
    retractall(state_status(_, _)),
    % Remove quantity_space/4.
    retractall(quantity_space( _, _, _, _ ) ),
    % Remove qspace/4.
    retractall( qspace( _, _, _, _ ) ),

	%%%modeldata:
	%Remove quantity_space/3.
    retractall( quantity_space( _, _, _) ),
    % Remove static clauses: isa/2.
    retractall(isa(_, _)),
    % Remove static clauses: rule/4 (experts can still assert rules)
    retractall( rule( _, _, _, _ ) ),
    %remove model fragments + sourcefile data
    retractall(system_structures(_, _, _, _)),
    (   recorded_erase(library_index, _),
	    fail
	;   true
	),
	%remove scenarios + sourcefile data
	retractall(smd(_, _, _, _, _, _)),
	(recorded_erase(bound_input_system, _), %gp3 version of input_system_index
	  fail
	  ;
	  true
	),
	retractall(scenario_state(_)), %gp3 1.4 saved scenario SMD
	%algorithm options from a model (is this correct? defaults should also be checked. sihw will do in RUNPREFS subtask)
	retractall(algorithm_option_switch(_, _)).

%load the current designer model (@model)
%implies cleanup, no need to call it
%asks the designer to assert all stuff needed in the engine namespace

do_cmd(loadmodel):-
	object(@model),!,
	do_cmd(cleanup),
	@tracer->>trace(string('Translating model %s (model definition version %s)',@model?name,@model?modelDefinitionVersion),general),
	%we create a stream for the model to export itself to (using regular export calls and an internal flag)
	% JL: Created a workaround as file(@default, text), does not produce a readable temporary file on Windows 7
	tmp_file_stream(text, TmpFile, TmpStream), %Tmp *= file(@default, text),
	absolute_file_name(TmpFile, TmpPath), %Path = Tmp<<-absolute_path,
	close(TmpStream), %Tmp->>close, % File % Temporary files are automatically deleted
	new(PCETmpFile, file(TmpPath, text)),
	send(PCETmpFile, open, write),
	send(@model,export_all_internal,PCETmpFile),
	send(PCETmpFile, close),

	%read thru our own code
	readAndAssert([TmpPath],Errors),
	@pce->>write_ln('Legacy model single-file-exported to:',TmpPath,'(will be remove at_halt)'),
	%errors are reported to console
	forall(member(E, Errors), print_message(error,E)),
	%init all the stuff
	init_algorithm_assumptions, %overruling engine defaults with stuff we exported to the isa part (or file)
	initialise_quantity_spaces, %sihw: these 2 were originally done after isa/qs but before smd/system_structures. But should work this way as well
	@tracer->>trace('Translation done',general).


do_cmd(loadmodel):-!,
	writeln('Warning: could not load designer model. Model not found.').

%%loadLegacy: load a legacy file, implies cleanup
%we do not need all the recorded(...) and bindings stuff the old engine code used

do_cmd(loadLegacy(Directory)):-
	do_cmd(cleanup),
	QS = string('%s/quantity_space',Directory)<<-value,
	Isa = string('%s/isa',Directory)<<-value,
	Lib = string('%s/library',Directory)<<-value,
	Rules = string('%s/rules',Directory)<<-value,
	Sc = string('%s/input_system.gp',Directory)<<-value,
	%any rules?
	if
		file(Rules)->>exists
	then
		AllFiles = [QS,Isa,Lib,Sc,Rules]
	else
		AllFiles = [QS,Isa,Lib,Sc],
	readAndAssert(AllFiles,Errors),
	%errors are reported to console
	forall(member(E, Errors), print_message(error,E)),
	%init all the stuff
	init_algorithm_assumptions,
	initialise_quantity_spaces. %sihw: these 2 were originally done after isa/qs but before smd/system_structures. But should work this way as well

%%
%terminate states
%old state_menu_action(States, t)
%from standalone->interface.pl
do_cmd(termination(States)):-!,
		ignore(forall(member(State, States), termination(State))).

%order states
%old state_menu_action(States, o), but also terminates, so you can order without terminating
%from standalone->interface.pl
do_cmd(order(States)):-!,
	do_cmd(termination(States)),
		ignore(forall(member(State, States), precedence(State))).

%close states
%old state_menu_action(States, c), but also terminates and orders, so you can close without ordering
%from standalone->interface.pl
do_cmd(close(States)):-!,
	do_cmd(order(States)), %also terminates
		ignore(forall(member(State, States), transition(State))).

%full simulation
%from standalone->interface.pl
do_cmd(full_simulation):-!,
	repeat,
	terminate_all,
	order_all,
	close_all,
	\+ state_status(_, open),
	\+ state_status(_, interpreted),
	\+ state_status(_, terminated),
	\+ state_status(_, ordered).


%load a new input model
do_cmd(new_scenario(S)):-!,
	new_input_model(S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%last one: unknown command

do_cmd(M):-
	writef('Warning. Unknown command \'%w\' sent to engine ignored.\n',[M]).


%%%%helpers
%%
readAndAssert(Filenames, Errors):-
	%gp 3
	%code to read files (list of FileNames) and assert its clauses
	%failsave, but saving errors in the errors list

	forall(
		recorded(readAndAssert_error,_,Ref),
		erase(Ref)
		),

	forall(
		member(Filename, Filenames),
		(
			if
				catch(open(Filename,read,Stream,[type(text)]),E,(recordz(readAndAssert_error,E),fail))
			then
			(
				readAndAssertTerms(Stream),
				close(Stream)
			)
		)
	),
	%collect errors
	findall(
		Error,
		(
			recorded(readAndAssert_error,Error,Ref),
			erase(Ref)
		),
		Errors
	).
%
readAndAssertTerms(Stream):-
	catch(
		(
			read_term(Stream,ReadTerm,[variable_names(Bindings)]),
			Term = ReadTerm
		)
		,E,
		(
			recordz(readAndAssert_error,E),
			Term = readAndAssert_error
		)
	),
	unless
		Term == end_of_file
	do
	(
		unless
			Term == readAndAssert_error
		do
		(
			assertz(engine:Term),
			if
				Term = smd(input_system(_),_,_,_,_,_)
			then
			(
				%patch: we need the Bindings, because we want the generated varnames
				do_bindings(Bindings), %make 'Parameter1' = G321 unify
				recordz(bound_input_system,Term) %gp3 version of input_system_index
			)
		),
		readAndAssertTerms(Stream) %I dont like repeat-by-failure, but do continue after readAndAssert_error
	).

%do_bindings: taken from garp interface.pl
do_bindings([H|T]):- H, do_bindings(T).
do_bindings([]).



%%	reset_simulation_gensym/1
%
%	Reset gensym specifically for the bases used in the simulation
%	must be done as first step of cleanup when asserted predicates
%	are still available
%
reset_simulation_gensym:-
	% reset assumptions in Model Fragment adding reasoning:
	reset_gensym(assumption),
	% reset all quantity numbers:
	findall(Q, (qspace(_Instance, Predicate, _, _), Predicate =.. [Q|_]), All),
	list_to_set(All, AllQuantityBases),
	forall(member(Base, AllQuantityBases), reset_gensym(Base)),
	% reset entity instances (seem not to be in use, but gensym in code, reset doesn't hurt anyway:
	findall(Sub, isa(Sub, _Super), AllEntityTypes), %top node not generated, not interesting anyway
	list_to_set(AllEntityTypes, AllEntityBases),
	forall(member(EntityBase, AllEntityBases), reset_gensym(EntityBase)).


