/*
engine_garp3.pl
Garp3 1.0 15-4-2005
Single threaded version of code to control the engine.
There is/was also a multi threaded version, not used any more. So all the control calls are gone,
all we have are the calls to execute commands in the engine.

SENDING COMMANDS
use enginectrl:command to send a command. This command will be run immediately in the same thread as the caller (single thread version). The command must be defined ofcourse, and can be a complex structure (all elements should be bound).

The actual implementation of the accepted commands is in enginecmd_garp3.pl. That file runs in the engine namespace, making it easier to call internal predicates.).


PART OF Garp3. SEE COPYRIGHT NOTICE.

*/

:-module(enginectrl,[]).

%command: send a command to the engine.
%single_thread version just hands the command to the engine, and wait for the execution to finish
%allways succeeds!
%(implementation of the check_and_parse_message main clause from the multithreaded version)

command(M):-
	@app->>pleaseWait(run),
	catch(
		ignore(engine:do_cmd(M)),
		error(F,C),
		print_message(error,error(F,C))
		),
	@app->>thankYou,
	@tracer->>told, %just to make sure all trace calls have been processed.
	@tracer->>finish, % to flush text on tracewindow. 
	% (NEW: FL apr. 07: trace does not flush anymore: too expensive to do it for every line.)
	!.
