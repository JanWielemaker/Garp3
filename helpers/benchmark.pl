/* Main call to do a benchmark */
simple_benchmark :-
    profile(simple_benchmark2).

full_benchmark :-
    profile(full_benchmark2(@off)).

full_save_simulation :-
    full_benchmark2(@on).


/* Do a simple benchmark of all .hgp files in the benchmark directory */
simple_benchmark2 :-
    hgp_files_in_benchmark_dir(Models),
    forall(
	member(Model, Models),
	(
	    simple_benchmark_model(Model)
	)
    ).

/* Do a full benchmark of all .hgp files in the benchmark directory */
/* +Save specifies whether the simulations should be saved or not */
full_benchmark2(Save) :-
    hgp_files_in_benchmark_dir(Models),
    forall(
	member(Model, Models),
	(
	    full_benchmark_model(Model, Save)
	)
    ).


/*  Benchmark by loading the input .hgp file, simulating the last edited scenario, 
    and closing the model again. */
simple_benchmark_model(Model) :-
    send(@app, loadModelFromHGPFile, Model),
    send(@app, startFullSimulation,@app?model?lastEditedIS),
    send(@app, closeCurrentModel).

full_benchmark_model(Model, Save) :-
    send(@app, loadModelFromHGPFile, Model),
    design:getAllScenarios(@model, AllScenarios),
    chain_list(AllScenarios, Scenarios),
    forall(
	member(Scenario, Scenarios),
	(
	    send(@app, startFullSimulation,Scenario),
	    
	    /* Save the simulation if so specified */
	    (
		Save == @on ->
		send(@app, saveSimulation2, Scenario?name)
	    ;
		true
	    )
	)
    ),
    /* Save the model if so desired */
    (
	Save == @on ->
	send(@app, save) 
    ;
	true
    ),

    send(@app, closeCurrentModel).


/* Create a list of all the hgp files in the benchmark directory */
hgp_files_in_benchmark_dir(HGPFiles) :-
    working_directory(WD, WD),
    atom_concat(WD, 'benchmark/', BenchmarkDir),
    hgp_files_in_dir(BenchmarkDir, HGPFiles).

/* Give all hgp files in a certain directory */
hgp_files_in_dir(Dir, Files) :-
    atom_concat(Dir, '*.hgp', Pattern),
    expand_file_name(Pattern, Files).
