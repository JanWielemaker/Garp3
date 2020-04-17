/*  File:     Load.pl 
    Purpose:  Loading all GARP related files
    Author:   Martin Reinders & Bert Bredeweg & Jan Wielemaker & Floris Linnebank
    Date:     August 1989
    Part-of:  GARP (version 2.0)
    Modified: 12 September 2004

    Copyright (c) 2004, University of Amsterdam. All rights reserved.

*/


%   Old code, don't use ISO extensions.

:- set_feature(character_escapes, false).

:- dynamic
    file_search_path/2.
:- multifile
    file_search_path/2.

/*
:- prolog_load_context(directory, Dir0),
   file_directory_name(Dir0, Dir),
   writef('\n Garp dir: %d \n', [Dir]),
   asserta(file_search_path(garp, Dir)).
*/

% Garp on UNIX at SWI
% file_search_path(garp,    '/swi/staff/bouwer/Garp1-7-2').

% Garp on drive C on PC 
file_search_path(garp,      'C:/Program Files/Garp').

% Garp on drive C on PC in Brazil
file_search_path(garp,      'C:/Arquivos de Programas/Garp').

/*
file_search_path(garp,      '..').
file_search_path(garp,      Garp) :-
    getenv('GARP', Garp).
file_search_path(garp,      swi(garp)).
file_search_path(garp,      '/usr/local/lib/garp').
*/

file_search_path(garp_base,     garp(program_files)).
file_search_path(garp_library,  '.').
file_search_path(garp_help, garp_base(help)).
file_search_path(foreign,   garp_base(Arch)) :-
    feature(arch, Arch).


:- dynamic
        % isa, rule and quantity_space were 
        % added 3 july 2003 to replace abolish by retractall in language.pl
        isa/2, 
        rule/4,
        quantity_space/3,

    termcap/2, 
    qspace/4, 
    state/2, 
    state_status/2, 
    state_from/2, 
    state_to/2,

    algorithm_switch/2, % new in GARP 2.0  FL, July 2004 (only one assertion though)

    assumption_culprit/3, 
    current_assumption/2,
    current_library/1.
    
current_library(library).

%setting(use_shell, true).
setting(use_shell, false).

%   tty_fold/2 has been deleted from the SWI-Prolog distribution.
%   It used to make SWI-Prolog fold long lines, but this is done
%   by the terminal these days.

tty_fold(_, _).

%FL July 2004: for prolog versions older then 5.20 reverse A and B arguments.
common_last(A, B):-
    last(A, B).

common_select(A, B, C) :-
    select(B, A, C).

common_shell(Command):-
    shell(Command).

common_shell(Command, R):-
    shell(Command, R).

recorded_erase(Key, Structure):-
    recorded(Key, Structure, Reference), 
    erase(Reference).

/*% maintenance: readable trace
portray(cio(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _)):- write('cio(...)'). 
portray(relation(L, Rel, R)):-
    map_list(L, LL), 
    map_list(R, RL), 
    write(relation(LL, Rel, RL)).
%portray(String) :-  
%   string(String),
%   map_list(String, List),
%   format('map(~p)', [List]).
*/
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

% dynamic predicates in garp

load_bits :-
    getenv('PLLD', true), !.
load_bits :-
    current_predicate(_, _:list_map(_,_)), !.
load_bits :-
%   absolute_file_name(foreign('pl-bit.so'), Name),
%   format('Loading pl-bit from ~w~n', [Name]),
    load_foreign_library(foreign('pl-bit'), init_bits).

:- initialization load_bits.


:- use_module(library(gensym)).

:- [ garp_base(initquantity)
   , garp_base(selector)
   , garp_base(types)
   , garp_base(intern)
   , garp_base(pllib)
   , garp_base(terminal)
   , garp_base(interface)
   , garp_base(class)
   , garp_base(solve)
   , garp_base(semantic)
   , garp_base(reclass)
   , garp_base(methods)
   , garp_base(transitions) %nb. new in GARP 2.0
   , garp_base(grprefs)     %nb. new in GARP 2.0
   ].

load_library :-
    (   current_prolog_flag(bbwww_garp, true)
    ->  Options = [silent(true)]
    ;   Options = []
    ),
    load_files([ garp_library(quantity_space), 
             garp_library(isa), 
             garp_library(rules)
           ],
           Options).

% initialise terminal capabilities
:- initialization
   set_termcaps.

% read the library and the input system

save_garp(Name) :-
    qsave_program(Name, [stand_alone=true, goal=go]).

find_directory_from_file_argument :-
    unix(argv(Argv)),
    member(F, Argv),
    prolog_to_os_filename(Pl, F),
    file_name_extension(Base, gp, Pl),
    file_directory_name(Base, Dir), !,
    chdir(Dir).
find_directory_from_file_argument.



go :- 
    find_directory_from_file_argument,
    load_library, 
    init_algorithm_assumptions, %NB new in garp 2.0, must happen before initialise_quantity_spaces
    initialise_quantity_spaces, 
    show_file(garp_help(banner)),
    read_library, 
    read_input_system, 
    sleep(3),
    main_menu.


/* during specification we check if enough global or trail stack is left
   for the next recursive (deterministic) call of 'class'
   if not results are stored in the database and backtracking to the
   second clause of do_action is forced
   These facts specify how much stack a single recursive level of 'class'
   will need (an estimate ofcourse).
*/

% medium
need_global(70000).
need_trail(60000).

