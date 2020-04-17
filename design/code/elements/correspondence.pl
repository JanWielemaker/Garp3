/*
Definitie correspondence class
2.1: uitbreiding mogelijkheden. Correspondentie kan nu ook tussen afgeleiden, op dezelfde wijzen als bij quantiteiten zelf

*/

:-pce_begin_class(
		  correspondence,
		  parRelation,
		  "definition of correspondence relation in model fragments"
		 ).

variable(argument1Value,'valueReference*',both). %verwijzing naar waarde uit qs van eerste arg als waarde correspondentie
variable(argument2Value,'valueReference*',both). %verwijzing naar waarde uit qs van tweede arg als waarde correspondentie
variable(derivative,'bool',both,"@on if this correspondence is between the quantity spaces or values of derivatives").
variable(directed,bool,both,"@on if this correspondence is directed").
variable(mirror,bool,both,"@on if this correspondence is mirrored").
variable(full, bool, both, "@on if this correspondence is full"). %gp3 0.3 if full, no argumentNValue..., no mirror
%%
initialise(C,
	MF: modelFragment, 
	Directed: bool,
	Derivative: bool,
	Mirror: bool,
	Full: bool, 	
	Arg1 : garpQuantity,
	Arg1Route: chain,
	Arg1Value: 'valueReference*',
	Arg2: garpQuantity,
	Arg2Route: chain,
	Arg2Value: 'valueReference*',
	Remarks: string):->

	%gp3 0.3 added Full. Watch out a lot of these booleans are not compatible with eachother: a full corr cannot be mirrored (just now)
	
	C->+initialise(MF,Arg1, Arg1Route, Arg2, Arg2Route, Remarks),
	C->>changeArguments(Arg1, Arg1Route, Arg1Value,Arg2, Arg2Route, Arg2Value),
	C->>directed(Directed),
	C->>derivative(Derivative), %2.1
	C->>mirror(Mirror), %2.1
	C->>full(Full).
%%

%%
isVCorrespondence(C):->
	%gp3 0.2 Succeeds if this correspondence is a value correspondence
	%this is the case if the argumentNvalue slots are not nil
	
	\+
	(
		@nil = C<<-argument1Value
	;
		@nil = C<<-argument2Value
	).
	
%%
changeArguments(C, Arg1: garpQuantity, Arg1Route: chain, Arg1Value: 'valueReference*',
				Arg2: garpQuantity, Arg2Route: chain, Arg2Value: 'valueReference*'):->
	%zet de argumenten goed, geen controle op geldigheid

	C->>argument1(Arg1,Arg1Route),
	C->>argument2(Arg2,Arg2Route),

	if
		Arg1Value == @nil
	then
		C->>argument1Value(@nil)
	else
		C->>argument1Value(Arg1Value?copy),

	if
		Arg2Value == @nil
	then
		C->>argument2Value(@nil)
	else
		C->>argument2Value(Arg2Value?copy).
%
	
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
		CR->>addChangeRequest(changeRequestor(deleteCorrespondence,
								C?fragment,
								@default,
								CR?garpModel,
								C),
			string('A correspondence in %s "%s" will be deleted because one of it\'s arguments is no longer part of the %s', C?fragment?displayTypeName, C?fragment?name, C?fragment?displayTypeName),
			C?fragment).
%%

%%
checkChangedQS(C,
	CheckArg1: bool,
	CheckArg2: bool,
	NewValues: chain,
	_MF: modelFragment, %this is the MF that called us!!
	CR: changeRequestor):->
	
	%check onze argumenten als die gewijzigd kunnen zijn door een qs wijziging
	%(dus qs definitie veranderd, quantity veranderd)

	%gp3 0.3: we also check wether qs- and full-correspondences still have correct qs-mapping
	%if not: delete.
	%that is what we do in this clause
	%make sure that failure let control flow to next clause, so no nice if-then-else stuff
	
	@off = C<<-derivative,
	@nil = C<<-argument1Value, %not a value correspondence: this is a qs or full correspondence
	
	%the qs might change for our argument 1 and/or argument 2
	if
		CheckArg1 = @on
	then
		Arg1QS = NewValues
	else
		Arg1QS = @default,
		
	if
		CheckArg2 = @on
	then
		Arg2QS = NewValues
	else
		Arg2QS = @default,
	
	\+ C?argument1->>checkQSCorrCompatibility(C?argument2,C?mirror,Arg1QS,Arg2QS),
	%when the above call fails, we have to delete the correspondence
	Feedback *= string('A%s correspondence in MF "%s" will be deleted because the quantity spaces are no longer compatible for use in this kind of correspondence.',
				when(C?full == @on, ' full',
					when(C?mirror == @on,'n inverse','')),
				C?fragment?name),
	CR->>addChangeRequest(changeRequestor(deleteCorrespondence,
										C?fragment,
										@default,
										CR?garpModel,
										C),
						Feedback,C).
%
checkChangedQS(C,
	CheckArg1: bool,
	CheckArg2: bool,
	NewValues: chain,
	_MF: modelFragment,
	CR: changeRequestor):->
	
	%this second clause is the old homer code that checks for existence of referenced values etc
	%2.1: niets te doen als dit over een derivative-correspondence gaat
	@off = C<<-derivative,
		
	%even uitbesteden
	Arg1 = C<<-argument1Value,
	Arg2 = C<<-argument2Value,

	if
	(
		CheckArg1 = @on,
		Arg1 \== @nil
	)
	then
	(
		Result1 = Arg1<<-checkNewValues(NewValues,@on),
		Arg1Result = Result1<<-first,
		Arg1ResultArg = Result1<<-second
	)
	else
		Arg1Result = same,

	if
	(
		CheckArg2 = @on,
		Arg2 \== @nil
	)
	then
	(
		Result2 = Arg2<<-checkNewValues(NewValues,@off),
		Arg2Result = Result2<<-first,
		Arg2ResultArg = Result2<<-second
	)

	else
		Arg2Result = same,
	
	%wat te doen: uitbesteden
	pl_checkChangedQS_Result_cor(C,Arg1Result,Arg1ResultArg,Arg2Result,Arg2ResultArg,
		CR).
%
pl_checkChangedQS_Result_cor(_,same,_,same,_,_):-!. %niets veranderd
%
pl_checkChangedQS_Result_cor(C,Res1,_,Res2,_,CR):-

	(
		(Res1 = delete,VName = C?argument1Value?valueName,QName = C?argument1?name)
	;
		(Res2 = delete,VName = C?argument2Value?valueName,QName = C?argument2?name)
	),!,

	Feedback *= string('A correspondence in MF "%s" that used the value "%s" for "%s" will be deleted',
				C?fragment?name,
				VName,
				QName),
	CR->>addChangeRequest(changeRequestor(deleteCorrespondence,
										C?fragment,
										@default,
										CR?garpModel,
										C),
						Feedback,C).
%
pl_checkChangedQS_Result_cor(C,Arg1Result,Arg1ResultArg,
								Arg2Result,Arg2ResultArg,CR):-

	(Arg1Result = changeOtherType ; Arg2Result = changeOtherType;
	 Arg1Result = changeSameType ; Arg2Result = changeSameType),!,

	if (
		Arg1Result == changeOtherType
		;
		Arg2Result == changeOtherType
	   )
	then
		FeedbackMode = @default %altijd feedback
	else
		FeedbackMode = @nil, %stil tenzij subfeedback
	if
		Arg1Result = same
	then
		NewArg1Val = C<<-argument1Value
	else
		NewArg1Val = Arg1ResultArg, %geldig bij change en bij internal
	if
		Arg2Result = same
	then
		NewArg2Val = C<<-argument2Value
	else
		NewArg2Val = Arg2ResultArg,

	CR->>addChangeRequest(changeRequestor(changeCorrespondence,
									C?fragment,
									@default,
									CR?garpModel,
									C,
									C?directed, C?derivative,C?mirror, C?full,
									C?argument1, C?argument1Route,NewArg1Val,
									C?argument2, C?argument2Route, NewArg2Val,
									C?remarks),
			string('A correspondence in %s "%s" that pointed from %s "%s" to %s "%s" will be updated to point from %s "%s" to %s "%s"',
					C?fragment?displayTypeName,
					C?fragment?name,
					C?argument1Value?type, C?argument1Value?valueName,
					C?argument2Value?type, C?argument2Value?valueName,
					NewArg1Val?type, NewArg1Val?valueName,
					NewArg2Val?type, NewArg2Val?valueName),
			C,FeedbackMode).								
								
	
pl_checkChangedQS_Result_cor(C,Arg1Result,Arg1ResultArg,
								Arg2Result,Arg2ResultArg,CR):-

	if
		Arg1Result = internal
	then
		CR->>addChangeRequest(changeRequestor(internalUpdateValueRef,
						@model,@default, CR?garpModel,
						C?argument1Value,
						Arg1ResultArg),
				'Internal',@nil,@nil),

	if
		Arg2Result = internal
	then
		CR->>addChangeRequest(changeRequestor(internalUpdateValueRef,
						@model,@default, CR?garpModel,
						C?argument2Value,
						Arg2ResultArg),
				'Internal',@nil,@nil).

%%
copyToNewElement(FE,Mappings: hash_table, NewMF: modelFragment, New: correspondence):<-
	%gp3 0.2 Copy contents and relations (hypers) to a new assumptionInstance
	%see fragmentElement<<-copyToNewElement for details
	%we fail if required connected objects are missing from the Mappings table
	
	%lets see if the needed other elements are allready copied
	
	NewArg1Info = NewMF<<-copyMF_mapRelatedElement(FE?argument1,FE?argument1Route,Mappings),
	NewArg2Info = NewMF<<-copyMF_mapRelatedElement(FE?argument2,FE?argument2Route,Mappings),
		%fails when these ones are not available yet, so will try again later
		
	if
		@nil = FE<<-argument1Value
	then
		NewArg1V = @nil
	else
		NewArg1V = FE?argument1Value<<-copy,
	if
		@nil = FE<<-argument2Value
	then
		NewArg2V = @nil
	else
		NewArg2V = FE?argument2Value<<-copy,
		
	%everything checked, we can create the item
	New = NewMF<<-addNewCorrespondence(FE?directed, FE?derivative, FE?mirror, FE?full,
				NewArg1Info?first, NewArg1Info?second, NewArg1V,
				NewArg2Info?first, NewArg2Info?second, NewArg2V,
				FE?remarks).
%%
:-pce_end_class.
		  
