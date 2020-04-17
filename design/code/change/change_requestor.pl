/*
changeRequestor class
Implementatie van expliciet wijzigingsmanagement.
Zie ook de helper class changeFeedback

Part of Garp3, see copyright notice
Most code old dutch homer code

2000-2005 Jelmer Jellema, Spin in het Web, spininhetweb.nl

Changes in gp3: After the final changeApplied call, the change requestor that started the request (the root of the request tree) also sends a changeTreeApplied call to all objects, frames and frame-members

Gp3 1.4:  new slot called silent. When set to @on before routing, changeApplied and changeTreeApplied will NOT be called. It is the responsability of the called to make sure this is okay. Sometimes one needs a temporary change (i.e. of currentlanguage) to do something else...

*/


/***************************changeRequestor class**********************************/
:-pce_begin_class(changeRequestor(type,object,editor),
		  object,
		  "Objects for requesting, making and undoing of changes").

variable(type,name,get,"Type of change"). 
%vastgestelde types

variable(object,object,get,"Changed object"). 
%het object dat gewijzigd wordt

variable(editor,any*,get,"Editor where change was made"). 
%handig bij doorvoeren van de change

variable(arguments, chain, get, "Arguments of change request"). 
%volgens protocol bij type/object

variable(subChanges, chain, get, "All subchanges").
%Bij elkaar geveegde lijst van wijzigingen die ook doorgevoerd moeten worden.

variable(feedback, chain, get, "Top feedback nodes").
%Lijst van changeFeedback objecten die gebruikt worden. Deze kunnen weer subfeedback hebben etcetera.

variable(route, chain, get, "Route this requestor has followed with routeRequest").
%Slaan we op want we sturen de subChanges langs dezelfde route

variable(checkMethod,name,get,"Internal name of send method for checking this type of change").

variable(applyMethod,name,get,"Internal name of send method for applying this type of change").

variable(appliedMethod,name,get,"Internal name of send method for telling editors this type of change has been applied").

variable(result,any,both,"Slot than can be filled during applyChange").
%gaat volgens protocol (wat gebeurt er bij welk type)

variable(autoFreeList,chain,get,"List of objects that are going to be destroyed by freeObject").

variable(silent,bool := @off, both, "Do not send applied messages"). %gp3 1.4
variable(nofeedback,bool := @off, both, "Do not show user confirmation or information"). %gp3 1.4
variable(garpModel, garpModel, both, "Model associated to the changeRequest"). % gp3 1.4

%%
initialise(CR,
	   T : type = name,
	   O : object = object,
	   E : editor = ['dialog*|frame*'],  % E: editor = [any*],  % Het moet een frame of een dialog zijn!
	   M : garpModel,
	   A : arguments = any ... %Wordt prolog lijst
	   ) :->
	"Create the change requestor" ::

	CR ->+ initialise,
	CR ->> slot(type,T),
	CR ->> slot(object,O),
	CR ->> slot(garpModel,M),
	default(E,@nil,Editor),
	CR ->> slot(editor,Editor),
	%van de Argumenten (een prologlijst) maken we alsvolgt een chain
	list_chain(A,ArgChain), %prolog_helpers.pl
	CR ->>slot(arguments,ArgChain),
	CR ->> slot(subChanges,new(chain)),
	CR ->> slot(feedback,new(chain)),
	CR ->> slot(route,new(chain)), %wordt gevuld in routeRequest
	CR->>slot(checkMethod,?(name('checkChange_'),append,T)),
	CR->>slot(applyMethod,?(name('applyChange_'),append,T)),
	CR->>slot(appliedMethod,?(name('changeApplied_'),append,T)),
	CR->>slot(autoFreeList,new(chain)).
%%

%%
argument(CR,
	 I : index = int,
	 Arg : any
	 ):<-
	"Return the 1-based argument at the given index. Fail if unavailable." ::

	Arg = CR?arguments <<- nth1(I).
%%

%%
catch_all(CR,Selector: name, Arg: any):<-
	%Alternatief voor argument: CR<<-arg3 wordt hetzelfde als CR<<-argument(3)
	%dus CR?arg5 is makkelijker dan ?(CR,argument,5)

	Arg = CR<<-argument(?(@pce,convert,?(Selector,delete_prefix,arg),int)).
%%

%%
checkType(CR,
	  Type: type = name
	 ):->
	"Succeeds if the type is the same as the given type" ::

	Type->>equal(CR?type).
%%

%%
checkObject(CR,
	    Object: object = object
	   ):->
	"Succeeds if the object is the same as the given object" ::

	Object->>equal(CR?object).
%%

%%
checkEditor(CR,
	    E: editor = any
	   ):->
	"Succeeds if the editor is the same as the given editor" ::

	E = CR<<-editor
	;
	(
		F = E<<-frame,
		F = CR<<-editor
	).
%%

%%
checkArgument(CR,
	      I: index = int,
	      Arg : argument = any
	     ):->
	"Succeeds if the argument at the given index is the same as the given argument"::

	Arg->>equal(?(CR?arguments,nth1,I)).
%%

%%
copyChangeInfo(CR,
	       NewCR : changeRequestor
	       ):<-
	"Return a new change requestor holding a copy of the change information (not the feedback)" ::
	%we kijken dus niet naar de subChanges, feedback en autoFreeList

	NewCR *= changeRequestor(CR?type,
				 CR?object,
				 CR?editor,
				 CR?garpModel),
        CR?arguments ->> for_all(->>(NewCR?arguments,append,@arg1)).
%%

%%
checkImpossibleName(CR,
	I: argument_index =  int,
	_O: object = object
	):->
	"Check if the given argument is a valid name, become impossible if not" ::
	%in dat geval impossible met object als object

	Name = CR<<-argument(I),
	Name = @default,!. %klaar
%
checkImpossibleName(CR,I,O):->
	%deze clause slaagt altijd! (als impossible slaagt)
	Name = CR<<-argument(I),
	if
		(not @model->>validName(Name))
	then
		CR->>impossible('Invalid name given',O).
%%
	
%%
equal(CR, 
      OtherCR : changeRequestor
      ):->
	"Succeeds if the change information (not the feedback) in both requestors is the same" ::
	%we kijken dus niet naar de subChanges, feedback en autoFreeList

	CR?type ->> equal( OtherCR?type ),
	CR?object ->> equal( OtherCR?object ),
	CR?arguments->>equal(OtherCR?arguments).
%%


%%
go(CR,
	     Route : route = 'object|chain' ...
	     ):->
	"Check this change with the given objects and apply it. Fails if not applied" ::
	%we slaan de route op in routeRequest, want subchanges volgen die ook

	%Route is een prologlijst van objecten of chains
	RouteChain *= chain,
	forall(member(ObjOrChain,Route),
		if
			ObjOrChain->>instance_of(chain)
		then
			RouteChain->>union(ObjOrChain)
		else
			RouteChain->>add(ObjOrChain)
		),
	CR->>routeRequest(RouteChain),!,
	CR->>apply.
%%

/*
Routeren van het request, de changerequestor wordt naar de opgegeven
objecten gestuurd. Het object reageert door directe manipulatie van
de requestor via de daartoe beschikbare methoden. 
*/
routeRequest(CR,
	     Route : chain
	     ):->
	"Internal: route the request through the given objects voor inspection" ::
	
	CR ->> slot( route, Route ), %bewaren voor eventuele subchanges
	Route ->> for_some( if(
				and(
					->>(@prolog,call,object,@arg1),
					->>(@arg1,has_send_method,CR?checkMethod)
				),
			       ->>( @arg1, CR?checkMethod, CR ))).
%%

%%
/*
apply van het request, verschillende clauses voor feedback
*/

%
apply(CR
      ):->
	"Internal. Give feedback and apply changes" ::
	%clause 1: De change is impossible

	CR ->> isImpossibleChange,!,
	%gp3 1.4: do not show any feedback when nofeedback = true

	if 
		@off = CR<<-nofeedback	
	then
	(
		%Wat we bij elkaar willen krijgen is een nieuwe feedbackboom
		%met daarin alleen de takken die impossible zijn
		Impossible *= new(chain),
		CR?feedback->>for_some(->>(Impossible,append,
					   @arg1?impossibleTree)),
		CR<<-showOverview('Conflict',
				  'This action is impossible. Please check the following feedback:',
				  Impossible,
				  @off,
				  'Build_Conflict')
	),
	!,fail. %want niet applied
%
apply(CR
     ):->
	%clause 2: De change mag, al dan niet na bevestiging
	%we maken een lijst met alle feedback die moet worden afgebeeld
	%gegeven hun quietWhen lijst

	%gp3: do not give any feedback when nofeedback = true. just confirm
	%nofeedback = @on when internal procedures know what they are doing...
	
	Feedback *= new(chain),
	AllChanges = CR?subChanges<<-copy,
	AllChanges->>append(CR?copyChangeInfo),
	CR?feedback->>for_some(->>(Feedback,append,
				   ?(@arg1,feedbackTree,AllChanges))),
	%die kan leeg zijn, er is dan geen feedback nodig
	(   
		(Feedback->>empty ; @on = CR<<-nofeedback)
	->  R = @on
	;   R = CR<<-showOverview('Please confirm change',
				  @nil,
				  Feedback,
				  @on,
				  'Build_ConfirmChanges')
	),!,
	(   R == @on
	->  CR->>routeApply
	;   fail %niet applied
	).
%%


%Nieuwe versie: eerst apply, dan applied, dan autoFree
routeApply(CR):->
	"Internal: Apply the change." ::

	\+ CR->>isImpossibleChange,
	AllChanges = CR?subChanges<<-copy,
	AllChanges->>prepend(CR), %als eerste
	AllChanges->>for_some(->>(@arg1,sendApply)),
	
	%gp3 1.4: do not send applied when silent = @on
	Silent = CR<<-silent, %save in a var
	unless
		Silent = @on
	do
		AllChanges->>for_all(->>(@arg1,doSendApplied)),
	AllChanges->>for_all(->>(@arg1,doAutoFree)),
	%gp3 0.4.6: we now destroy ourselves and send a global changeTreeApplied call
	%this used to happen as a method before autoFree, but the new way all model state is as it will be after the change
	Route = CR?route<<-copy,
	CR->>free,
	%gp3 1.4: not if silent = @on
	unless
		Silent = @on
	do
		sendTreeApplied(Route). %global prolog

sendApply(CR):->
	"Internal: Send applyMethod to its object" ::

	ApplyMethod = CR<<-applyMethod,
	Object = CR<<-object,
	
	if
		object(Object)
	then
	(
		if
			Object->>has_send_method(ApplyMethod)
		then
			ignore(send(Object,ApplyMethod,CR))
	).
%
doSendApplied(CR):->
	"Internal: activate changeApplied messages"::

	%eerst bij de objecten in de route, daarna bij de editors

	%gp3: we delegate to a more general call, no code changes
	CR->>sendAll(CR?appliedMethod),
	CR->>sendAll(changeApplied). %general one
%%

%%
sendAll(CR, MethodName: name):->
	%gp3: code split from doSendApplied. Send a given method to all objects on the route, and to all app members
	
	CR?route->>for_some( if(
				->>(@arg1,has_send_method,MethodName),
			    ->>( @arg1, MethodName, CR ))),
	%@app?members->>for_all(->>(CR,sendFrame,@arg1,MethodName)).
	send(
	    ?(@app?members, find_all, 
		and(
		    message(@arg1, has_get_method, garpModel),
		    (@arg1?garpModel == CR?garpModel)
		    )
	    ),
	    for_all, 
	    message(CR, sendFrame, @arg1, MethodName)
	).
%

%%
sendFrame(CR,
	    Frame: frame,
	    MethodName: name
	   ):->
	   %gp3 helper for  sendAll: send message to all frame and frame members
	   %used to be sendApplied helper, sending a specific method
	   %gp3 made it more general

	 %old comment:
	 %als het frame het niet accepteert sturen we het naar de members van het frame
	%dit is bijvoorbeeld een property dialoog object
	%wanneer deze het weer zou delegeren naar het frame, dan is dat niet erg,
	%want die heeft het dus niet geimplementeert
	
	if
		object(Frame)
	then
	(
		if
			Frame->>has_send_method(MethodName)
		then
			ignore(send(Frame,MethodName,CR))
		else
			Frame?members->>for_some(if(
				->>(@arg1,has_send_method,MethodName),
				->>(@arg1,MethodName,CR)
				))
	).
%%

doAutoFree(CR):->
	"Internal: Start freeing of autodestroy objects" ::

	AutoFree = CR<<-autoFreeList,
	chain_list(AutoFree,AutoFreeList),
	pl_doAutoFree(CR,AutoFreeList).
%
pl_doAutoFree(CR,[First|Rest]):-
	if
		object(First)
	then
	(
		ignore(First->>free)
	),
	pl_doAutoFree(CR,Rest).

pl_doAutoFree(_,[]).

%%

%%
sendTreeApplied(Route):- %%%PROLOG%%%%
	%gp3 0.4.16: no longer a method because the CR is allready destroyed
	%gp3: send the changeTreeApplied message to all objects and editors
	%this is done only after the root CR of the change tree is done
	%and is destroyed.
	%the CR is not send as an argument, so its members are unavailable (and invalid, as the CR object is destroyed allready)
	
	%this used to be a method called with the root CR as an argument after the apply but before the
	%freeObject mechanism auto-freed the objects
	%now this is changed: all autoFree objects are freed allready, even the CR exists no longer
	
	Route->>for_some( if(
				->>(@arg1,has_send_method,changeTreeApplied),
			    ->>( @arg1, changeTreeApplied ))),
	@app?members->>for_all(and(
		assign(new(Frame,var),@arg1),
		if(
			->>(Frame,has_send_method,changeTreeApplied),
			if(->>(Frame,changeTreeApplied)),
			->>(Frame?members,for_some,
				if(
					->>(@arg1,has_send_method,changeTreeApplied),
					if(->>(@arg1,changeTreeApplied))
				)
			)
		)
	)).
%%

%%
showOverview(CR,
	     Label: name,
	     Prompt: name*, %gp3 0.3.11 changed this to also nil possible
	     Feedback: chain,
	     CanApply: bool,
	     HelpId: name, %gp3 0.3.13 added this
	     Result: bool
	    ):<-
	"Internal: show overview dialog"::
	%toon de dialoog en geef @on terug bij ok
	%en @off bij cancel. CanApply = @off betekent dat de apply button disabeled is
	
	D *= assistanceDialog(Label,HelpId), %gp3 0.3.13 changed this to extended with help
	D->>can_resize(@off),
	D->>application(@app),
	D->>modal(application),
	
	%layout
	GapX = D?gap<<-width,
	GapY = D?gap<<-height,
	
	if
		Prompt = @nil
	then
		ListTop = D?topY
	else
	(	
		LabelText *= label(labeltext, 'Labeltext'),
		LabelText->>selection(Prompt),
		D->>display(LabelText,point(GapX,D?topY)),
		ListTop = LabelText?bottom_side + GapY
	),
	List *=	extendedListBrowser(width:= 80, height:= 10),
	D->>display(List,point(GapX, ListTop)),
	List->>style(impossible,style(colour:='red')), %onmogelijkheden rood
	Feedback->>for_all(->>(@arg1,fillFeedbackList,List)),
	Apply *= imgButton(apply,->>(D,return,@on),img:=save,tt:='Apply changes'),
	Apply->>active(CanApply),
	D->>display(Apply,point(GapX, List?bottom_side + GapY)),

	Cancel *= imgButton(cancel,->>(D,return,@off), img:=undo,tt:='Cancel changes'),
	D->>display(Cancel,point(List?right_side - GapY - Cancel?width, Apply?top_side)),
	D->>updateSpacers, %gp3 0.3.13
	Cancel->>default_button(@on),
	Cancel->>keyboard_focus(@on),
	%en tonen
	@display->>bell,
	D->>assign_accelerators,
	D->>input_focus(@on), %iets nodig om hem de focus te geven
	if
		@nil = CR<<-editor
	then
		Result = D<<-confirm_centered
	else
		Result = D<<-confirm_centered(CR?editor?frame?area?center),
	D->>destroy.
%%

%%
impossible(CR, 
	   Reason : reason = string, 
	   Object : object = object
	   ):->
	"Set this change as being impossible for the given reason" ::
	%Dit slaan we als feedback op als we het nog niet wisten
	
	CR <<- appendFeedback( Object, Reason, @on ).
%%

%%
warning(CR,
	Warning : warning = string,
	Object: object = object
	):->
	"Set a warning to be given to the user before this change is applied" ::
	%als feedback dus: feedback zonder subchange

	CR<<-appendFeedback(Object,Warning,@off).
%%

%%
freeObject(CR,
	O : object):->
	"Destroy object after this change is totally done" ::

	CR?autoFreeList->>append(O).
%%

%%
/*
Toevoegen van een sub-changerequest met eventueel feedback.
Er wordt gekeken of we de requestor al kennen: dan hoeven we niets
te doen. Anders wordt de requestor langs onze <-route gestuurd
en het resultaat verwerkt in de ontvanger (en de requestor afgedaan)

Het object dat de feedback geeft hoeft niet dezelfde te zijn
als het object in NewCR. Een view kan zeggen dat een quantity wijzigt
*/

%
addChangeRequest(CR,
		 NewCR : changeRequest = changeRequestor,
		 Feedback : feedback = string,
		 FO : feedback_object = object,
		 NFW : no_feedback_when = [chain*]
		 ):->
	"Request a change to be made when this change happens" ::
	%clause 1: We kennen de change requestor al. De feedback erbij kan wel
	%anders zijn (maar de subfeedback niet)
	%NFW is de quietWhen chain van het changeFeedback object
	%als NFW @nil is wordt er geen feedback gegeven, tenzij er subfeedback is

	CR?subChanges ->> find( ->>( NewCR,
				     equal,
				     @arg1 )),
	!,
	if
		NFW \== @nil
	then
		CR <<- appendFeedback( FO, Feedback, @off, NFW ).
%
addChangeRequest(CR,NewCR,Feedback,FO,NFW):->
	%clause 2: De change requestor is nieuw. Deze wordt dus eerst gerouteerd
	
	NewCR ->> routeRequest( CR?route ),
	CR ->> appendSubChange( NewCR ), %voegt toe
	NewCR?subChanges ->> for_all(->>(CR,
					 appendSubChange,
					 @arg1)), %boom info verloren
	%Nu gaan we de feedbackboom van die NewCR toevoegen aan onze boom
	%onder de nieuwe feedback (als de feedback nieuw is)
	%Wederom opletten op NFW: als die @nil is wordt er alleen toegevoegd wanneer er
	%sub feedback is

	if
		NFW == @nil
	then (
		if
			(not(NewCR?feedback->>empty))
		then
			QuietWhen = @default %wel een feedback dus
		else
			QuietWhen = @nil %geen feedback
		)
	else
		QuietWhen = NFW,

	if
		QuietWhen \== @nil %iets te doen
	then (
		NewFeedback = CR<<-appendFeedback(FO,Feedback,@off,QuietWhen),
		NewCR?feedback ->> for_all( ->>( NewFeedback,
						 appendSubFeedback,
						 @arg1 ))
		).
%%

%%
/*appendFeedback
Toevoegen van feedback. Als dezelfde feedback er al is wordt de quietWhen lijst
aangevuld (indien nodig).
Resultaat is de nieuwe of matchende feedback
*/

appendFeedback(CR,
	       Object : object,
	       Feedback : name,
	       Impossible : bool,
	       QuietWhen : [chain],
	       NewFeedback : changeFeedback
	      ):<-
	"Internal"::

	(   NewFeedback = CR?feedback <<- find( and( ->>(@arg1?object, equal, Object ),
						     ->>(@arg1?feedback, equal, Feedback ),
						     ->>(@arg1?impossible, equal, Impossible )))
	;   (
	     NewFeedback *= changeFeedback( Object, Feedback, Impossible, QuietWhen ),
	     CR?feedback ->> append(NewFeedback)
	    )
	),
	(   \+ QuietWhen == @default
	->  QuietWhen ->> for_all(->>(NewFeedback,appendQuietWhen,@arg1))
	;   true
	).
%%

%%
appendSubChange(CR,
		NewCR : changeRequestor
	       ):->
	"Internal"::
	CR?subChanges ->> find( ->>( @arg1,
				     equal,
				     NewCR )) %hebben we al
	;
	CR?subChanges ->> append( NewCR?copyChangeInfo ).
%%

%%
isImpossibleChange(CR
		   ):->
	"Succeed if this change is impossible" ::
	%Er is minstens 1 impossible feedback
	
	CR?feedback ->> find( ->>(@arg1,
				     recursiveImpossible)).
%%

:-pce_end_class.

