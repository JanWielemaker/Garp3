/*
engine_startup_garp3.pl
Loads all neccessary code, patches and fixes to run the garp engine from Garp3. We stripped a lot of stuff, so you cannot (really) use this file to load a standalone garp engine. See readme standalone

Based on load.pl, part of the Garp 2.01 standalone legacy version. Named authors: Martin Reinders & Bert Bredeweg & Jan Wielemaker & Floris Linnebank

Namespaces:
- all stuff used by the garpengine (including models that are read) is loaded into the engine namespace
- the stuff to control the engine (enginectrl_garp3.pl) is loaded into the enginectrl namespace

PART OF Garp3. SEE COPYRIGHT NOTICE.
*/

%:-module(engine,[isa/2,quantity_space/3,smd/6,system_structures/4]).
:-module(engine,[
		 epsilon_state_type/2,
		 get_all_simulation_quantity_instances/1,
		 get_all_simulation_entity_instances/1,
		 get_all_simulation_entity_and_quantity_instances/2,
		 get_all_simulation_state_numbers/1
		]).

:- dynamic
        isa/2,
        rule/4,
        quantity_space/3,
        quantity_space/4,
	    qspace/4,
	state/2,
	state_status/2,
	state_from/2,
	state_to/2,
	    algorithm_switch/2, % new in GARP 2.0  FL, July 2004 (only one assertion though)
	    assumption_culprit/3,
	current_assumption/2,
	smd/6,
	system_structures/4,
	state_values/2,
	scenario_state/1. %gp3 1.4: see reclass.pl first clause

%gp3 0.4.8 load_bits loads the list_map calls etc
%this used to be a shared library, but is now writen in native prolog
%because in the future these calls might be changed into built-in prolog calls
%we check first

load_bits :-
    current_predicate(list_map/2),!. %system
load_bits :-
	current_predicate(engine:list_map/2),!. %allready loaded in some way
load_bits :-
	[engine(bitvector)].

/*
%old code
%gp3: because load_bits is the only place where the archlib is used
%we removed file_search_path entries, and just figure it out in the code
%this is needed too because it has become a bit dynamic


load_bits :-
    getenv('PLLD', true), !.
load_bits :-
    current_predicate(_, _:list_map(_,_)), !.
load_bits :-
	%gp3 0.3: changed this code to get an explicit path
	%when archlibs/<arg> is not there, we try a shorter (and shorter and shorter) path
	current_prolog_flag(arch,Arch),
	absolute_file_name(engine(archlibs),LibDir),
	get_arch_path(Arch, LibDir,ArchPath),
	writef('Engine running on %w\nUsing library %w/pl-bit\n',[Arch,ArchPath]),
	@app->>engineArchPath(ArchPath), %just saving, to give nice info
	absolute_file_name('pl-bit',[relative_to(ArchPath)],Lib),
    load_foreign_library(Lib, init_bits).
%
get_arch_path(Archname,LibDir,ArchPath):-
	%gp3 0.3 See if there exists an arch lib for the given arch
	Archname \== '',
	absolute_file_name(Archname,[relative_to(LibDir)],ArchPath),
	exists_directory(ArchPath),!. %done
%
get_arch_path(Archname,LibDir,ArchPath):-
	%else: remove the last char and try again
	Archname \== '',
	sub_atom(Archname,0,_,1,NewArchname),
	get_arch_path(NewArchname,LibDir,ArchPath).
*/

:- initialization load_bits.

:- use_module(library(gensym)).

:- [ engine(initquantity)
   , engine(selector)
   , engine(types)
   , engine(intern)
   , engine(pllib_garp3)
   , engine(interface_garp3)
   , engine(class)
   , engine(solve)
   , engine(influence_resolution)
   , engine(multiplication)
   , engine(semantic)
   , engine(reclass)
   , engine(methods)
   , engine(transitions) %nb. new in GARP 2.0
   , engine(alg_assumptions_garp3)
   , engine(enginectrl_garp3)
   , engine(enginecmd_garp3)
   , engine(engine_content_api)
   ].

% maintenance: readable trace
% FL July 2004: with the quitracer in prolog
% the following portray settings are more useful
portray(relation(L, Rel, R)):-
    map_list(L, LL),
    map_list(R, RL),
    write(relation(LL, Rel, RL)).
portray(String) :-
    string(String),
    map_list(String, List),
    format('map(~p)', [List]).
