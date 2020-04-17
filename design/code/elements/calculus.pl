/*
Definitie calculus class

Part of Garp3, see copyright notice
Most code old dutch homer code

2000-2005 Jelmer Jellema, Spin in het Web, spininhetweb.nl
*/

:-pce_begin_class(
		  calculus,
		  fragmentElement,
		  "definition of calculi in model fragments"
		 ).

variable(sign,{min,plus,mult,diw},both,"Sign of the calculus").
variable(type,{quantity,derivative},both,"type of the calculus").
variable(argument1QSPoint, 'valueReference*', both, "QS Point reference for 1st arg").
variable(argument1Route, chain,none). %chain met verwijzingsroute voor 1e argument
							%zie garpRelation voor uitleg
variable(argument2QSPoint, 'valueReference*', both, "QS Pint reference for 2nd arg").
variable(argument2Route, chain,none). %chain met verwijzingsroute voor 2e argument
							%zie garpRelation voor uitleg
%%

%%
initialise(C,MF: modelFragment, Sign: {min,plus,mult,diw}, Type: {quantity, derivative},
			Arg1: 'garpQuantity | calculus', Arg1Route: chain, Arg1QSPoint: 'valueReference*',
			Arg2: 'garpQuantity | calculus', Arg2Route: chain, Arg2QSPoint: 'valueReference*',
			Remarks: string):->

	C->+initialise(MF, Remarks),
	C->>changeArguments(Type, Arg1,Arg1Route, Arg1QSPoint,Arg2,Arg2Route,Arg2QSPoint),
	C->>sign(Sign).
%%

%%
changeArguments(C,
	Type: {quantity, derivative},
	Arg1: 'garpQuantity | calculus', Arg1Route: chain, Arg1QSPoint: 'valueReference*',
	Arg2: 'garpQuantity | calculus', Arg2Route: chain, Arg2QSPoint: 'valueReference*'
	):->

	%Zet deze argumenten als de argumenten van de calculus + het type

	C->>type(Type),

	%eerst de oude maar eens weg
	C->>delete_hypers(argument1),
	C->>hyper(Arg1,argument1,calculus),
	C->>delete_hypers(argument2),
	C->>hyper(Arg2,argument2,calculus),

	C->>argument1Route(Arg1Route),
	C->>argument2Route(Arg2Route),

	if
		Type = derivative
	then (
		C->>argument1QSPoint(@nil),
		C->>argument2QSPoint(@nil)
		)
	else (
		if (
			Arg1->>instance_of(garpQuantity),
			Arg1QSPoint \== @nil
			)
		then
			C->>argument1QSPoint(Arg1QSPoint?copy)
		else
			C->>argument1QSPoint(@nil),

		if (
			Arg2->>instance_of(garpQuantity),
			Arg2QSPoint \== @nil
			)
		then
			C->>argument2QSPoint(Arg2QSPoint?copy)
		else
			C->>argument2QSPoint(@nil)
		).
%%

%%
isArgument(C,
	   Arg: 'garpQuantity|calculus'
          ):->
	%is meegestuurde object een argument?
	Arg = C<<-argument1
	;
	Arg = C<<-argument2.
%%


%%
argument1(C,
	  Arg : 'garpQuantity|calculus*'
	 ):<-
	%eerste argument returnen als die er is, anders @nil

	Arg = C<<-hypered(argument1)
	;
	Arg = @nil.
%%

%%
argument2(C,
	  Arg : 'garpQuantity|calculus*'
	 ):<-
	%2e argument returnen als die er is, anders @nil
	Arg = C<<-hypered(argument2)
	;
	Arg = @nil.
%%

%%
argument1Route(C,
	R : chain):->
	"Set argument1route" ::

	C->>slot(argument1Route,R?copy).
%
argument1Route(C,
	R : chain):<-
	"Get argument1Route" ::

	R = ?(C,slot,argument1Route)<<-copy.
%%

%%
argument2Route(C,
	R : chain):->
	"Set argument2route" ::

	C->>slot(argument2Route,R?copy).
%
argument2Route(C,
	R : chain):<-
	"Get argument2Route" ::

	R = ?(C,slot,argument2Route)<<-copy.
%%

%%
isCondition(C):->
	%slaagt wanneer de calculus kan worden opgevat als een conditioneel element
	%dwz: alle argumenten zijn conditioneel (of hebben een route)

	Arg1 = C<<-argument1,
	Arg2 = C<<-argument2,

	(Arg1 = @nil; Arg1->>isCondition ; (\+ C?argument1Route->>empty)),
	(Arg2 = @nil; Arg2->>isCondition ; (\+ C?argument2Route->>empty)).
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%relevantComments
%gp3 1.4: return a string with the remarks to show for this one
%when no remarks: return an empty string
%overwrite of the general one in fragment_element: shows, - sign because this is a sub element, 'CA' tag and also show comments for calculi in arguments

relevantComments(CA,RC: string):<-
	if
		0 = CA?remarks<<-size
	then
		RC *= string
	else
	(
		FR = CA<<-fragment,
		if
			FR->>isInputSystem
		then
			FT = 'SC'
		else
			FT = 'MF',
		RC *= string('- %s %s CA: %s', FT, FR?name,CA?remarks)
	),
	if
		CA?argument1->>instance_of(calculus)
	then
		RC->>ensure_nl(CA?argument1?relevantComments),

	if
		CA?argument2->>instance_of(calculus)
	then
		RC->>ensure_nl(CA?argument2?relevantComments).

%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
checkDeleteElement(C,
	Element: fragmentElement,
	CR: changeRequestor):->
	%overwrite van fragmentElement: check onze argumenten

	if
		(
			Element = C<<-argument1
		;
			C?argument1Route->>member(Element)
		;
			Element = C<<-argument2
		;
			C?argument2Route->>member(Element)
		)
	then
		CR->>addChangeRequest(changeRequestor(deleteCalculus,
								C?fragment,
								@default,
								CR?garpModel,
								C),
			string('A calculus in %s "%s" will be deleted because one of it\'s arguments is no longer part of the %s', C?fragment?displayTypeName, C?fragment?name, C?fragment?displayTypeName),
			C?fragment).
%%

%%
checkChangedQS(C,
	_CheckArg1: bool,
	_CheckArg2: bool,
	_NewValues: chain,
	_MF: modelFragment,
	_CR: changeRequestor):->

	%check onze argumenten als die gewijzigd kunnen zijn door een qs wijziging
	%(dus qs definitie veranderd, quantity veranderd)

	%1: niets aan de hand als we alleen maar naar de afgeleide van de
	%quantity wijzen

	derivative = C<<-type,!.
%
checkChangedQS(C,
	CheckArg1,
	CheckArg2,
	NewValues,
	_MF, %this is the MF that called us, not the MF that contains the calculus!
	CR):->

	/* Determine the MF that contains the Calculus */
	get(C, fragment, CalculusMF),

	Arg1P = C<<-argument1QSPoint,
	Arg2P = C<<-argument2QSPoint,

	if
	(
		CheckArg1 = @on,
		Arg1P \== @nil
	)
	then
	(
		Result1 = Arg1P<<-checkNewValues(NewValues,@off),
		Arg1Result = Result1<<-first,
		Arg1ResultArg = Result1<<-second
	)
	else
		Arg1Result = same,

	if
	(
		CheckArg2 = @on,
		Arg2P \== @nil
	)
	then
	(
		Result2 = Arg2P<<-checkNewValues(NewValues,@off),
		Arg2Result = Result2<<-first,
		Arg2ResultArg = Result2<<-second
	)
	else
		Arg2Result = same,

	%wat te doen: uitbesteden
	pl_checkChangedQS_Result_calc(C,Arg1Result,Arg1ResultArg,Arg2Result,Arg2ResultArg,
		CalculusMF,CR).
%
pl_checkChangedQS_Result_calc(_,same,_,same,_,_,_):-!. %niets veranderd
%
pl_checkChangedQS_Result_calc(C,Res1,_,Res2,_,MF,CR):-

	(
		(Res1 = delete,VName = C?argument1QSPoint?valueName,QName = C?argument1?name)
	;
		(Res2 = delete,VName = C?argument2QSPoint?valueName,QName = C?argument2?name)
	),!,

	Feedback *= string('A calculus in MF "%s" that used the value "%s" for "%s" will be deleted',
				MF?name,
				VName,
				QName),
	CR->>addChangeRequest(changeRequestor(deleteCalculus,
										MF,
										@default,
										CR?garpModel,
										C),
						Feedback,C).
%
pl_checkChangedQS_Result_calc(C,Arg1Result,Arg1ResultArg,
								Arg2Result,Arg2ResultArg,MF,CR):-
	(
		Arg1Result = changeSameType
	;
		Arg2Result = changeSameType
	),!,
	Feedback *= string('A calculus in MF "%s" will be updated to point to the changed values',
						MF?name),
	%nu nog de juiste informatie vinden
	if (
			Arg1Result = changeSameType
		;	Arg1Result = internal
		)
	then
		NewArg1Point = Arg1ResultArg
	else
		NewArg1Point = C<<-argument1QSPoint,

	if (
			Arg2Result = changeSameType
		;	Arg2Result = internal
		)
	then
		NewArg2Point = Arg2ResultArg
	else
		NewArg2Point = C<<-argument2QSPoint,

	CR->>addChangeRequest(changeRequestor(changeCalculus,
									MF,
									@default,
									CR?garpModel,
									C,
									C?sign,
									C?type,
									C?argument1,C?argument1Route,NewArg1Point,
									C?argument2,C?argument2Route,NewArg2Point,
									C?remarks),
						Feedback,C,@nil). %@nil voor: geen feedback tenzij subfeedback
%
pl_checkChangedQS_Result_calc(C,Arg1Result,Arg1ResultArg,
								Arg2Result,Arg2ResultArg,_MF,CR):-
	if
		Arg1Result = internal
	then
		CR->>addChangeRequest(changeRequestor(internalUpdateValueRef,
						@model,@default, CR?garpModel,
						C?argument1QSPoint,
						Arg1ResultArg),
				'Internal',@nil,@nil),

	if
		Arg2Result = internal
	then
		CR->>addChangeRequest(changeRequestor(internalUpdateValueRef,
						@model,@default, CR?garpModel,
						C?argument2QSPoint,
						Arg2ResultArg),
				'Internal',@nil,@nil).
%

%%
copyToNewElement(FE,Mappings: hash_table, NewMF: modelFragment, New: calculus):<-
	%gp3 0.2 Copy contents and relations (hypers) to a new assumptionInstance
	%see fragmentElement<<-copyToNewElement for details
	%we fail if required connected objects are missing from the Mappings table

	%lets see if the needed other elements are allready copied

	NewArg1Info = NewMF<<-copyMF_mapRelatedElement(FE?argument1,FE?argument1Route,Mappings),
	NewArg2Info = NewMF<<-copyMF_mapRelatedElement(FE?argument2,FE?argument2Route,Mappings),
		%fails when these ones are not available yet, so will try again later

	if
		@nil = FE<<-argument1QSPoint
	then
		NewArg1Point = @nil
	else
		NewArg1Point = FE?argument1QSPoint<<-copy,
	if
		@nil = FE<<-argument2QSPoint
	then
		NewArg2Point = @nil
	else
		NewArg2Point = FE?argument2QSPoint<<-copy,
	%everything checked, we can create the item
	New = NewMF<<-addNewCalculus(FE?sign, FE?type,
				NewArg1Info?first, NewArg1Info?second, NewArg1Point,
				NewArg2Info?first, NewArg2Info?second, NewArg2Point,
				FE?remarks).
%%
:-pce_end_class.

