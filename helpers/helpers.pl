/*
Garp3 0.1
Moved from Homer 2.1 to garp3 global. Hence old comments in Dutch (as all comments should be)

See copyright notice
Helpers.pl
ensure_loaded en autoload declaraties voor deze directory
*/

%pce library stuff

%gp3 0.3.12: We do not use the build in find_file library for @finder
%but our own. No difference, except confirm_overwrite is @off under unix
%so @garp3_finder is defined here


/*Helpers die altijd geladen moeten zijn*/

:-ensure_loaded('xpce_syntax_modifiers.pl'). % ->>, <<-, *= and friends
:-ensure_loaded('prolog_helpers.pl'). %simpele prolog helpers if then else etc.
:-ensure_loaded('object_extensions.pl').
:-ensure_loaded('unlink_notifier.pl').
:-ensure_loaded('icons.pl').
:-ensure_loaded('export_naming.pl').
:-ensure_loaded('tooltip.pl').
:-ensure_loaded('ps_bitmap.pl').
:-ensure_loaded('garp3_finder.pl').
:-ensure_loaded('warning_window.pl').
:-ensure_loaded(library('hyper')).
:-pce_global(@garp3_finder,new(garp3_finder)).

/* Tabs */
:-ensure_loaded('tabButton.pl').
:-ensure_loaded('managedTabStack.pl').

/* Benchmark */
:-ensure_loaded('benchmark.pl').

/*pce helper classes*/
:-pce_autoload(secondaryID,'secondary_id.pl').
:-pce_autoload(lookupTable,'lookup_table.pl').

