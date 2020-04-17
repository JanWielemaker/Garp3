/*
Definitie garpQuantityRelation class
*/

:-pce_begin_class(
		  garpQuantityRelation,
		  parRelation,
		  "definition of quantity proportionality or influence relation in model fragments"
		 ).

variable(type,{prop,inf},both,"Type of the quantity relation").
variable(sign,{min,plus,mult,diw},both,"Sign of the quantity relation").

%%
initialise(PR, MF: modelFragment, Sign: {min,plus,mult,diw}, Type: {prop,inf},
			Arg1: garpQuantity, Arg1Route: chain,
			Arg2: garpQuantity, Arg2Route: chain,
			Remarks: string) :->

	PR->+initialise(MF, Arg1, Arg1Route, Arg2, Arg2Route, Remarks),
	PR->>sign(Sign),
	PR->>type(Type).
%%

%%
checkDeleteElement(QR,
	Element: fragmentElement,
	CR: changeRequestor):->
	%overwrite van fragmentElement: check onze argumenten
	if
		(
			Element = QR<<-argument1
		;
			QR?argument1Route->>member(Element)
		;
			Element = QR<<-argument2
		;
			QR?argument2Route->>member(Element)
		)
	then
		CR->>addChangeRequest(changeRequestor(deleteQuantityRelation,
								QR?fragment,
								@default,
								CR?garpModel,
								QR),
			string('A %s in %s "%s" will be deleted because one of it\'s arguments is no longer part of the %s',when(QR?type == prop,'proportionality','influence'), QR?fragment?displayTypeName, QR?fragment?name, QR?fragment?displayTypeName),
			QR?fragment).
%%

%%
%%
copyToNewElement(FE,Mappings: hash_table, NewMF: modelFragment, New: garpQuantityRelation):<-
	%gp3 0.2 Copy contents and relations (hypers) to a new assumptionInstance
	%see fragmentElement<<-copyToNewElement for details
	%we fail if required connected objects are missing from the Mappings table

	%lets see if the needed other elements are allready copied

	NewArg1Info = NewMF<<-copyMF_mapRelatedElement(FE?argument1,FE?argument1Route,Mappings),
	NewArg2Info = NewMF<<-copyMF_mapRelatedElement(FE?argument2,FE?argument2Route,Mappings),
		%fails when these ones are not available yet, so will try again later

	%everything checked, we can create the item
	New = NewMF<<-addNewQuantityRelation(FE?type,
				NewArg1Info?first, NewArg1Info?second, NewArg2Info?first, NewArg2Info?second,
				FE?sign,FE?remarks).
%%
:-pce_end_class.

