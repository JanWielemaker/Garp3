/*
Garp3 1.2.1 Startup file
Purpose: Try and load all 3 main elements in an orderly fashion.
*/
/* Add this line to boost the memory. On unix-like systems, the file has to
be made executable started with ./startup.pl
#!/usr/bin/pl -G32m -T32m -L16m -si
*/

% JW: the garp sources are ISO-Latin-1 while most modern machines use
% UTF-8 these days.
%:-set_prolog_flag(encoding, iso_latin_1).
% JW: Garp suffers from many _semantic_ singletons.  This warning was
% added long after Garp was written.  If anyone wishes to do development
% on Garp, please delete this declaration and fix the warnings.  In our
% experience the vast majority of warnings are in fact programming
% errors.
:-style_check(-singleton).
% JW: This check triggers one warning that is almost surely an error in
% Garp in engine/transitions.pl:4800
:-style_check(-no_effect).
:-use_module(library(pce)).

%mode is debug or nodebug

%runMode(debug).
runMode(nodebug).

%show console?
setup_console :-
	(   runMode(debug)
	->  true
	;   ignore(send(@pce,show_console,hidden))
	),
	ignore(send(@pce,console_label,'GARP3 DEBUG CONSOLE')).

/* Show the console if Garp3 throws an unhandled exception */

:- multifile
    user:message_hook/3.

message_hook(_Term, Kind, _Lines) :-
		runMode(nodebug), %JJ: make sure we get the errors when developing
    (Kind == warning ; Kind == error),
    catch(win_window_pos([show(true), activate]), _TrowArgument, fail),
    fail.

%set basic searchpaths
set_searchpaths:-
	source_file(set_searchpaths,F),
	file_directory_name(F,P),
	assert(file_search_path(garp3,P)),
	new(@garp3_dir,directory(P)).

:-set_searchpaths.

file_search_path(main,garp3(main)).
file_search_path(helpers,garp3(helpers)).
file_search_path(controls,garp3(controls)).
file_search_path(commands,garp3(commands)).
file_search_path(engine,garp3(engine)).
file_search_path(design,garp3(design)).
file_search_path(visualize,garp3(visualize)).
file_search_path(ontologies,garp3(ontologies)).
file_search_path(sketch,garp3(sketch)). % for Sketch, AB, Aug 2006

%we load all. This is needed because pce-autoload does not honour included modules
%(when we :-consult something from a module and the code has no module of its own, it loads into
%the namespace of its parent. autoload loads into user:).

%first of all the helpers and other main stuff
:-load_files([helpers(helpers)],[silent(true)]).

%make sure the app is there
:-load_files([garp3(application)],[silent(true)]).
:-new(@app,garp3).
:-send(@app,pleaseWait,loading).

%create @old_slots for old model version conversion, see object_extensions
%object->convert_old_slot
:-new(@old_slots,hash_table).

%load controls
:-load_files([controls(controls)],[silent(true)]).
:-load_files([commands(commands)],[silent(true)]).

%start with the engine, should just work
:-load_files([engine(engine_startup_garp3)],[silent(true)]).

%designer
:-load_files([design(design_startup_garp3)],[silent(true)]).

%visualize
:-load_files([visualize(visualize_startup_garp3)],[silent(true)]).

%sketch, AB, Aug 2006
:-load_files([sketch(sketch_startup_garp3)],[silent(true)]).

%main
:-load_files([main(main_startup)],[silent(true)]).

:-send(new(mainMenu),open_centered).

% put window on top
:-send(@app?mainMenu, expose).

%done
:-send(@app,thankYou). %end of pleaseWait
%check version
:-send(@app,checkPLVersion).

:-setup_console.
%%
