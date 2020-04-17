/*
command_helpers.pl
Prolog helpers voor de command handlers
*/

getCommand(Frame,CommandName,Command):-
	%vind het commando-object bij het frame
	%faal met een error op stdout als
	%die er niet is
	Command=Frame<<-command(CommandName),!.
%
getCommand(_Frame,CommandName,_Command):-
	%falen

	@pce->>write_ln('Error: No command object in current frame for ', CommandName),
	!,fail.
%%
