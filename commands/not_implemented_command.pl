/*
Definitie notImplementedCommand. Een subclass van command. Elke framedWindow heeft
zo'n notImplementedCommand gedefinieerd. Deze wordt gebruikt wanneer een bepaald command
gerunt moet worden, maar het command niet bij het frame blijkt te zijn geimplementeerd.
Geeft een foutmelding, maar laat de context op de naam van het verwachte command staan.

*/
:-pce_begin_class(notImplementedCommand(frame,name),
		  command,
		  "Internal class voor handling onNotImplemented command").

setEnvironment(_C,_) :->
	%houd de environment wat ie was
	true.

notImplemented(C,_Context) :->
	"The not implemented command is not implemented! Send error" ::
	send(C?frame,report,error,
	     'Warning: Error handling unimplemented command %s',
	     @command?name).

:-pce_end_class.

