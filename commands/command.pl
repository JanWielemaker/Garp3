/*
Definitie command klasse: Gebruikt in framedWindow voor afhandelen standaard
commando routeren. Zie framedWindow en de command handler klassen.
command handlers (zoals menuCommand) zijn op naam nivo gekoppeld aan een command object
bij een frame. Als de opdracht uitgevoerd moet worden wordt het command object bij het
huidige frame opgezocht en krijgt die de opdracht het commando te routeren.
Op deze manier wordt ook allerlei andere informatie gerouteerd, zoals een check of het
commando mag, visuele updates en dergelijke.

Part of Garp3, see copyright notice
Most code old dutch homer code

2000-2005 Jelmer Jellema, Spin in het Web, spininhetweb.nl
*/
:-pce_begin_class(command(frame,name),
		  object,
		  "Support for centralised command handling").

		  
variable(frame,'framedWindow|dialog',get).
variable(name,name,get).	
variable(run,bool,get).
variable(keys,chain,both). %gp3 0.2: more than 1 key per command possible
variable(keystring, name,both).
variable(context,any,both). %gezet door een commandhandler
variable(checkMethod,name,both). %gp3 0.3 quicker than generating all the time
variable(runMethod,name,both). %gp3 0.3 idem

%global var object @command
:-pce_global(@command, new(var('command*', value := @nil))).

initialise(C, Frame : frame = 'framedWindow|dialog', 
	   Name : name = name, 
	   Run : runnable =  [bool],
	   Key: key = [name],
	   Keystring: keystring = [name],
	   OtherKeys: otherkeys = [chain]) :->
	"Initialise the command and set it's name" ::
	send(C,slot,frame,Frame),
	send(C,slot,name,Name),
	%gp3 0.3: save check and run method for quick access
	C->>checkMethod(string('check%s',Name)?value),
	C->>runMethod(string('on%s',Name)?value),
	default(Run,@on,RunFlag),
	send(C,slot,run,RunFlag),
	Keys *= chain, %gp3: now a command can have more than 1 key
	unless
		Key = @default
	do
		Keys->>append(Key),
	unless
		OtherKeys = @default
	do
		Keys->>merge(OtherKeys),
	C->>keys(Keys),
	default(Keystring,'',KS),
	C->>keystring(KS).
%

%%
wantsKey(C, Key: name):->
	%gp3 0.2 succeeds if the given key is defined as one of the keys for the command
	
	C?keys->>member(Key).
%%
	
%execute checkt niet of het wel mag, dat had de commandhandler tijdig moeten doen
execute(C,_: context = [any]) :->
	"Send the command to the frame if this command is set to run" ::
	get(C,run,@off),!. %dit commando runt niet

execute(C,Context) :->
	get(C,frame,Frame),
	%niet eerst checken of het mag, dat hoort door de interface geregeld te zijn
	get(C,runMethod,Method),
	send(Frame,has_send_method, Method),!, %bestaat ie?
	send(C,setEnvironment,Context),
	send(Frame,Method). %en run hem

execute(C,Context) :->
	%blijkbaar niet geimplementeerd. We sturen het naar
	%de notImplemented handler
	send(C,notImplemented,Context).

notImplemented(C,Context : context = [any]) :->
	"Handle unimplemented command" ::
	%wordt overruled door subclass notImplementedCommand
	get(C?frame,notImplementedCommand,NIC),
	send(C,setEnvironment,Context), %wij zetten onze environment
	send(NIC,execute).

canRun(C,_Context : context = [any]):->
	
	Frame = C<<-frame,
	\+ Frame->>has_send_method(C?checkMethod),!,
	%geen check, als er nou ook geen run is, dan is ie niet geimplementeerd
	%tenzij hij niet bedoeld is om te runnen
	(   Frame->>has_send_method(C?runMethod)
	;   @off = C<<-run).

canRun(C,Context):->
	get(C,frame,Frame),
	Method = C<<-checkMethod, %die bestaat, zie bovenstaande clause	
	send(C,setEnvironment,Context),
	send(Frame,Method). %faalt of slaagt

checkCondition(C, Context: context = [any], Result : bool) :<-
	"Check if this command could run. Return @off if not." ::
	send(C,canRun, Context)
	->
	Result = @on
	;
	Result = @off.

%%runUpdateMethod
%%Altijd slagend: Verzoek van een interface element om het frame
%%de mogelijkheid te geven het element
%%te updaten. Argumenten: Type = soort update, gedefineerd door het element
%%                        Context = Context als gedefinieerd door de commandhandler
%%De context wordt ook meegestuurd met de updatemethode
runUpdateMethod(C,Type : type = name, Context : context = [any]) :->
	"Send a typed update message to the frame or object, allways succeeds" ::
	get(C,frame,Frame),
	get(C,updateMethod, Type, Method),
	send(Frame,has_send_method,Method),!, %bestaat ie?
	send(C,setEnvironment,Context),
	(   send(Frame,Method,Context); %context wordt dubbel verstuurd
	    true %slaagt altijd
	).

runUpdateMethod(_, _Type, _Context) :->
	%deze updatemethod is nergens geimplementeerd
	true.
%%
%%getInfo	
/*Kijkt of het frame deze informatie voor het command
verschaft. Zoja: Haal het op. Zonee: Geef de default terug (of @nil als geen
default)
Slaagt altijd: als de information method bij frame of object faalt wordt de
default teruggegeven*/


getInfo(C,Type : type = name, Context : context = [any], 
	Default : defaultresult = [any],
	Result : any) :<-
	"Get a piece of information from the frame" ::
	get(C,frame,Frame),
	get(C,infoMethod,Type,Method),
	send(Frame,has_get_method,Method),!,
	send(C,setEnvironment,Context),
	(   get(Frame,Method,Result);
	    default(Default,@nil,Result)
	).

getInfo(_C,_Type,_Context, Default,Result) :<-
	%deze infomethod is nergens geimplementeerd
	default(Default,@nil,Result).

updateMethod(C,Type: name ,MethodName: name) :<-
	%voor commando Test en type MenuCommand: updateMenuCommandTest
	new(N, name(update)),
	get(N, append,Type,N2),
	get(N2, append, C?name, MethodName).

infoMethod(C, Type: name, MethodName: name) :<-
	%voor commando Test type LabelText: infoLabelTextTest
	new(N, name(info)),
	get(N,append,Type,N2),
	get(N2,append,C?name,MethodName).
	
setEnvironment(C, Context : context = [any]) :->
	"Sets the @command context" ::
	%apart want overruled in class notImplementedCommand (zie onder)
	default(Context,@nil,RealContext),
	send(C,slot,context,RealContext),
	send(@command,assign,C,local).


/*******************************************/

:-pce_end_class.


