/*
commands.pl
ensure_loaded en autoload declaraties voor deze directory

*/

/*helpers*/
:-ensure_loaded('command_helpers.pl').

/*commands zelf*/
:-ensure_loaded('command.pl').
:-ensure_loaded('not_implemented_command.pl').

/*commandhandlers
Zie ook de documentatie "update en infotypes"
*/

:-ensure_loaded('menu_command.pl').
:-ensure_loaded('popup_command.pl').
:-ensure_loaded('menu_popup_command.pl').
:-ensure_loaded('context_popup.pl').
:-ensure_loaded('button_command.pl'). %gp3 0.2
/*helper classes*/
