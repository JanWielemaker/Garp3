/*
Garp3 0.1

See copyright notice
Globals, ensure_loaded and autoload declarations for this directory

*/

%load the files

:-pce_autoload(finder, library(find_file)).
:-ensure_loaded('main_menu.pl').
:-ensure_loaded('web.pl').
:-ensure_loaded('saved_simulation_list.pl').
:-ensure_loaded('helper_dialogs.pl').
:-ensure_loaded('runprefs_dialog.pl').
:-ensure_loaded('corruptmodelpatches.pl').
:-ensure_loaded('versionpatch.pl').
:-ensure_loaded('socket_connection.pl').
%:-ensure_loaded('export_dialog.pl'). %will be loaded when needed
%appsettings.pl is loaded from the application(.pl)
