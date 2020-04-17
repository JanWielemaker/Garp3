/*
Definitie identityRelation class
*/

:-pce_begin_class(
		  identityRelation,
		  garpRelation,
		  "definition of identity relation in model fragments"
		 ).

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
		CR->>addChangeRequest(changeRequestor(deleteIdentity,
								I?fragment,
								@default,
								CR?garpModel,
								I),
			string('An identity in %s "%s" will be deleted because one of it\'s arguments is no longer part of the %s',I?fragment?displayTypeName,I?fragment?name, I?fragment?displayTypeName),
			I?fragment).
%%

checkChangedConfiguration(I,
	Arg1: garpInstance,
	Arg1Route: chain,
	Arg2: garpInstance,
	Arg2Route: chain,
	_OtherMF: modelFragment,
	CR: changeRequestor):->
	
	%check de gevolgen voor deze identity als er een configuratie
	%wordt gewijzigd of bijkomt met de meegestuurde gegevens
	%er moet een rechtlijnige relatie zijn qua MF's en dezelfde argumenten
	%natuurlijk, dan mag het niet
	%rechtlijnig is alleen als beide argumenten ons mf importeren
	%of ons mf beide argumenten via de mf van de nieuwe configuratie importeert (of het om hetzelfde fragment als de onze gaat)
	
	%gp3 0.3.14 General rewrite because of bugs in thinking about routes
	
	%we block when the given configuration is in a more general setting of our arguments (so in parent MF's etc)
	%or in a more specialised setting
	%or in our Mf exacty
	
	if
	(
		I->>isMoreGeneralRelation(Arg1,Arg1Route,Arg2,Arg2Route)
	;
		I->>isSpecializedRelation(Arg1,Arg1Route,Arg2,Arg2Route)
	;
		I->>sameArguments(Arg1,Arg1Route,Arg2,Arg2Route)
	)
	then
		CR->>impossible(
			string('There is already an identity relation between these instances (defined in %s "%s")',
			I?fragment?displayTypeName, I?fragment?name),I?fragment).

%%

%%
copyToNewElement(FE,Mappings: hash_table, NewMF: modelFragment, New: identityRelation):<-
	%gp3 0.2 Copy contents and relations (hypers) to a new assumptionInstance
	%see fragmentElement<<-copyToNewElement for details
	%we fail if required connected objects are missing from the Mappings table
	
	%lets see if the needed other elements are allready copied
	
	NewArg1Info = NewMF<<-copyMF_mapRelatedElement(FE?argument1,FE?argument1Route,Mappings),
	NewArg2Info = NewMF<<-copyMF_mapRelatedElement(FE?argument2,FE?argument2Route,Mappings),
		%fails when these ones are not available yet, so will try again later
		
	%everything checked, we can create the item
	%gp3 0.3 added the identity class element, for subclassing purposes (see fragment_refiner.pl for fragmentRefinerIdentity definition
	New = NewMF<<-addNewIdentity(NewArg1Info?first, NewArg1Info?second, 
				NewArg2Info?first, NewArg2Info?second,
				FE?remarks,FE?class).
%%
:-pce_end_class.
		  
