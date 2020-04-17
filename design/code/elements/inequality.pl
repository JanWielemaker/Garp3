/*
Definitie inequality class
*/

:-pce_begin_class(
		  inequality(argument1,argument1Type,argument2,argument2Type),
		  parRelation,
		  "definition of inequality relation in model fragments"
		 ).

variable(type,{l,leq,eq,geq,g},both,"Type of inequality").
variable(argument1Type,{currentValue,currentDerivative,
			quantityQSPoint,derivativeZero, calculus},both,"Type of first argument").
variable(argument2Type,{currentValue,currentDerivative,
			quantityQSPoint,derivativeZero, calculus},both,"Type of second argument").
variable(argument1QSPoint,'valueReference*',get,"Reference to value in qs of first quantity (when quantityQSPoint)").
variable(argument2QSPoint,'valueReference*',get,"Reference to value in qs of second quantity (when quantityQSPoint)").

%%
initialise(I,
	MF: modelFragment, 
	Type: type = {l,leq,eq,geq,g},
	Arg1 : argument1 = 'garpQuantity | calculus',
	Arg1Route: chain,
	Arg1Type: argument1Type = 
		{currentValue, currentDerivative, quantityQSPoint,derivativeZero,calculus},
	Arg1QSPoint: argument1QSPoint = 'valueReference*',
	Arg2 : argument2 = 'garpQuantity | calculus',
	Arg2Route: chain,
	Arg2Type: argument2Type = 
		{currentValue, currentDerivative, quantityQSPoint,derivativeZero,calculus},
	Arg2QSPoint: argument2QSPoint = 'valueReference*',
	Remarks: string):->

	I->+initialise(MF, Arg1, Arg1Route, Arg2, Arg2Route, Remarks),
	I->>changeArguments(Arg1,Arg1Route, Arg1Type,Arg1QSPoint,Arg2,Arg2Route, 
							Arg2Type,Arg2QSPoint),
	I->>type(Type),
	I->>slot(type,Type).
%%

%%
changeArguments(I,
	Arg1 : argument1 = 'garpQuantity | calculus',
	Arg1Route: chain,
	Arg1Type: argument1Type = 
		{currentValue, currentDerivative, quantityQSPoint,derivativeZero,calculus},
	Arg1QSPoint: argument1QSPoint = 'valueReference*',
	Arg2 : argument2 = 'garpQuantity | calculus',
	Arg2Route: chain,
	Arg2Type: argument2Type = 
		{currentValue, currentDerivative, quantityQSPoint,derivativeZero,calculus},
	Arg2QSPoint: argument2QSPoint = 'valueReference*'
	):->

	%Zet deze argumenten als de argumenten van de ongelijkheid
	%hier
	I->+argument1(Arg1,Arg1Route),
	I->+argument2(Arg2,Arg2Route),

	if
		Arg1->>instance_of(calculus)
	then
		I->>slot(argument1Type,calculus)
	else
		I->>slot(argument1Type,Arg1Type),

	if
		Arg2->>instance_of(calculus)
	then
		I->>slot(argument2Type,calculus)
	else
		I->>slot(argument2Type,Arg2Type),

	if
		quantityQSPoint = I<<-argument1Type
	then
		I->>slot(argument1QSPoint,Arg1QSPoint?copy)
	else
		I->>slot(argument1QSPoint,@nil),

	if
		quantityQSPoint = I<<-argument2Type
	then
		I->>slot(argument2QSPoint,Arg2QSPoint?copy)
	else
		I->>slot(argument2QSPoint,@nil).
%%

%%
isLocal(I):->
	%Slaagt wanneer het een locale inequality betreft

	Q = I<<-argument1,
	Q = I<<-argument2. %per definitie lokaal
%%

%%
isDerivative(I):->
	%gp3 0.2
	%succeeds when this inequality is about derivatives:
	
	A1T = I<<-argument1Type,
	A2T = I<<-argument2Type,
	
	(
		A1T = currentDerivative ; A2T = currentDerivative ;
		A1T = derivativeZero ; A2T = derivativeZero ;
		(
			%or there is a calculus which is derivative type
			%we only need to check arg1, because if that one
			%is not derivative calculus, the other one cannot derivative
			
			A1T = calculus,
			derivative = I?argument1<<-type
		)
	).
%%

%%
swappedType(I,
	Type : '{l,leq,eq,geq,g}'):<-
	%geeft het type geswapt terug

	CurType = I<<-type,
	pl_swapType_INE(CurType,Type).
%
pl_swapType_INE(l,g).
pl_swapType_INE(leq,geq).
pl_swapType_INE(geq,leq).
pl_swapType_INE(g,l).
pl_swapType_INE(eq,eq).
%%

%%
typeName(I,
	Type: name):<-
	%gp3 0.2
	%give a natlang description of the type
	
	CurType = I<<-type,
	pl_typeName(CurType,Type).
%
swappedTypeName(I,
	Type: name):<-
	%gp3 0.2
	%like typeName, but give the name for the swapped type
	
	CurType = I<<-swappedType,
	pl_typeName(CurType,Type).
%
pl_typeName(l,'smaller').
pl_typeName(leq, 'smaller or equal').
pl_typeName(geq, 'greater or equal').
pl_typeName(g, 'greater').
pl_typeName(eq, 'equal').
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%relevantComments
%gp3 1.4: return a string with the remarks to show for this one
%when no remarks: return an empty string
%overwrite of the general one in fragment_element: also show comments for calculi in arguments

relevantComments(IN,RC: string):<-
	if
		0 = IN?remarks<<-size
	then
		RC *= string
	else
	(
		FR = IN<<-fragment,
		if
			FR->>isInputSystem
		then
			FT = 'SC'
		else
			FT = 'MF',
		RC *= string('%s %s: %s', FT, FR?name,IN?remarks)
	),
	if 
		calculus = IN<<-argument1Type
	then
		RC->>ensure_nl(IN?argument1?relevantComments),
		
	if 
		calculus = IN<<-argument2Type
	then
		RC->>ensure_nl(IN?argument2?relevantComments).
	
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
checkDeleteElement(I,
	Element: fragmentElement,
	CR: changeRequestor):->
	%overwrite van fragmentElement: check onze argumenten
	if
		(
			Element = I<<-argument1
		;
			I?argument1Route->>member(Element)
		;
			Element = I<<-argument2
		;
			I?argument2Route->>member(Element)
		)
	then
	(
		CR->>addChangeRequest(changeRequestor(deleteInequality,
								I?fragment,
								@default,
								CR?garpModel,
								I),
			string('An inequality in %s "%s" will be deleted because one of it\'s arguments is no longer part of the %s',I?fragment?displayTypeName, I?fragment?name, I?fragment?displayTypeName),
			I?fragment)
	).
%%

%%
checkChangedQS(I,
	CheckArg1: bool,
	CheckArg2: bool,
	NewValues: chain,
	_MF: modelFragment, %this is the MF that called us, not the MF that contains the inequality!
	CR: changeRequestor):->
	
	%check onze argumenten als die gewijzigd kunnen zijn door een qs wijziging
	%(dus qs definitie veranderd, quantity veranderd)
	
	/* Determine the MF that contains the Inequality */
	get(I, fragment, InequalityMF),
	
	%even uitbesteden
	Arg1P = I<<-argument1QSPoint,
	Arg2P = I<<-argument2QSPoint,

	if
	(
		CheckArg1 = @on,
		quantityQSPoint = I<<-argument1Type	%alleen bij verwijzing naar de QS
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
		quantityQSPoint = I<<-argument2Type
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
	pl_checkChangedQS_Result_inq(I,Arg1Result,Arg1ResultArg,Arg2Result,Arg2ResultArg,
		InequalityMF,CR).
%
pl_checkChangedQS_Result_inq(_,same,_,same,_,_,_):-!. %niets veranderd
%
pl_checkChangedQS_Result_inq(I,Res1,_,Res2,_,MF,CR):-

	(
		(Res1 = delete,VName = I?argument1QSPoint?valueName,QName = I?argument1?name)
	;
		(Res2 = delete,VName = I?argument2QSPoint?valueName,QName = I?argument2?name)
	),!,

	Feedback *= string('An inequality in %s "%s" that used the value "%s" for "%s" will be deleted',
				MF?displayTypeName,
				MF?name,
				VName,
				QName),
	CR->>addChangeRequest(changeRequestor(deleteInequality,
										MF,
										@default,
										CR?garpModel,
										I),
						Feedback,I).
%
pl_checkChangedQS_Result_inq(I,Arg1Result,Arg1ResultArg,
								Arg2Result,Arg2ResultArg,MF,CR):-
	(
		Arg1Result = changeSameType 
	;
		Arg2Result = changeSameType
	),!,
	Feedback *= string('An inequality in MF "%s" will be updated to point to the changed values', MF?name),
	%nu nog de juiste informatie vinden
	if (
			Arg1Result = changeSameType
		;	Arg1Result = internal
		)
	then
		NewArg1Point = Arg1ResultArg
	else
		NewArg1Point = I<<-argument1QSPoint,

	if (
			Arg2Result = changeSameType
		;	Arg2Result = internal
		)
	then
		NewArg2Point = Arg2ResultArg
	else
		NewArg2Point = I<<-argument2QSPoint,

	CR->>addChangeRequest(changeRequestor(changeInequality,
									MF,
									@default,
									CR?garpModel,
									I,
									I?stateName,
									I?type,
									I?argument1,I?argument1Route, I?argument1Type,NewArg1Point,
									I?argument2,I?argument2Route, I?argument2Type,NewArg2Point,
									I?remarks),
						Feedback,I,@nil). %@nil voor: geen feedback tenzij subfeedback%

pl_checkChangedQS_Result_inq(I,Arg1Result,Arg1ResultArg,
								Arg2Result,Arg2ResultArg,_MF,CR):-
	if
		Arg1Result = internal
	then
		CR->>addChangeRequest(changeRequestor(internalUpdateValueRef,
						@model,@default, CR?garpModel,
						I?argument1QSPoint,
						Arg1ResultArg),
				'Internal',@nil,@nil),

	if
		Arg2Result = internal
	then
		CR->>addChangeRequest(changeRequestor(internalUpdateValueRef,
						@model,@default, CR?garpModel,
						I?argument2QSPoint,
						Arg2ResultArg),
				'Internal',@nil,@nil).
%

%%
copyToNewElement(FE,Mappings: hash_table, NewMF: modelFragment, New: inequality):<-
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
		NewArgument1QSPoint = @nil
	else
		NewArgument1QSPoint = FE?argument1QSPoint<<-copy,
	if
		@nil = FE<<-argument2QSPoint
	then
		NewArgument2QSPoint = @nil
	else
		NewArgument2QSPoint = FE?argument2QSPoint<<-copy,
		
	%everything checked, we can create the item
	New = NewMF<<-addNewInequality(FE?stateName, FE?type,
				NewArg1Info?first, NewArg1Info?second, FE?argument1Type, NewArgument1QSPoint,
				NewArg2Info?first, NewArg2Info?second, FE?argument2Type, NewArgument2QSPoint,
				FE?remarks).
%%
:-pce_end_class.
		  
