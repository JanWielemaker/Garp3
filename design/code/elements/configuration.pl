/*
Definitie configuration class
*/

:-pce_begin_class(
		  configuration(definition),
		  garpRelation,
		  "definition of configuration relation in model fragments"
		 ).
%%
initialise(C, MF: modelFragment, 
			D: configurationDefinition,
			Arg1: argument1 = garpInstance,
			Arg1Route: chain,
			Arg2 : argument2 = garpInstance,
			Arg2Route: chain,
			Remarks: string) :->

	C->+initialise(MF,Arg1,Arg1Route,Arg2,Arg2Route,Remarks),
	C->>changeDefinition(D).
%%

%%
changeDefinition(C,D : configurationDefinition):->
	"Change the definition " ::

	C->>delete_hypers(definition), %is er 0 of 1 als het goed is
	C->>hyper(D,definition,configuration).
%%

%%
definition(C, D : configurationDefinition):<-
	"Return the definition this configuration is based on" ::

	%faalt als ie er niet is, dan is er echt iets mis
	D = C<<-hypered(definition).
%%

%%
name(C,
	N : name):<-
	"Return the name according to the definition" ::

	N = C?definition<<-name.
%%

%%
checkDeleteElement(C,
	Element: fragmentElement,
	CR: changeRequestor):->
	%overwrite van fragmentElement: als één van onze argumenten Import in zijn 
	%route heeft, gaan we deze relatie verwijderen via een subchange

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
		CR->>addChangeRequest(changeRequestor(deleteConfiguration,
								C?fragment,
								@default,
								CR?garpModel,
								C),
			string('A configuration "%s" in %s "%s" will be deleted because one of it\'s arguments is no longer part of the %s',C?name, C?fragment?displayTypeName, C?fragment?name, 
				C?fragment?displayTypeName),
			C?fragment).
%%

%%
checkChangedConfiguration(C,
	Other: 'configuration*',
	_Def: configurationDefinition,
	Arg1: garpInstance,
	_Arg1Route: chain,
	Arg2: garpInstance,
	_Arg2Route: chain,
	_OtherMF: modelFragment,
	_CR: changeRequestor):->
	
	%check de gevolgen voor deze configuratie als er een configuratie
	%wordt gewijzigd of bijkomt met de meegestuurde gegevens
	
	%WORDT IVM GEWIJZIGDE NORMEN (ALS HET KAN MAG HET) NIET MEER GEBRUIKT
	%1: we zijn het zelf, of het gaat helemaal niet over dezelfde instances
	%niets aan de hand
	
	(
		C == Other
	;
		(\+ C->>isArgument(Arg1))
	;
		(\+ C->>isArgument(Arg2))
	),!.
%
checkChangedConfiguration(C,
	_Other: 'configuration*',
	Def: configurationDefinition,
	Arg1: garpInstance,
	Arg1Route: chain,
	_Arg2: garpInstance,
	Arg2Route: chain,
	OtherMF: modelFragment,
	CR: changeRequestor):->

	%2: de ontvanger zit in hetzelfde of een algemener fragment
	%als de ontvanger algemener is, zit zijn MF in beide argumentroutes
	%(niet slechts in één, want dan kan het andere op een andere wijze
	%geïmporteerd zijn, bijvoorbeeld via een aparte applies-to)
	
	if
	(
		OtherMF = C<<-fragment
	;
		(
			Arg1Route->>find(@arg1?referencedFragment == C?fragment),
			Arg2Route->>find(@arg1?referencedFragment == C?fragment)
		)
	)
	then
	(
		%als het dezelfde (+ zelfde richting)
		%is is het impossible, anders een warning
		if
		(
			Def = C<<-definition,
			Arg1 = C<<-argument1
		)
		then
			CR->>impossible('The given configuration already exists',C?fragment)
		else
			CR->>warning(
				string('There is already a "%s" configuration between these instances (defined in %s "%s")',
					C?name,C?fragment?displayTypeName, C?fragment?name),
				C?fragment)
	),
	
	%3: de ontvanger zit in een specifieker fragment
	
	if
	(
		C?argument1Route->>find(
			@arg1?referencedFragment == OtherMF), %weer allebei
		C?argument2Route->>find(
			@arg1?referencedFragment == OtherMF) %weer allebei
	)
	then
	(
		%als zelfde, dan verwijderen,
		%anders weer een waarschuwing
		if
		(
			Def = C<<-definition,
			Arg1 = C<<-argument1
		)
		then
			CR->>addChangeRequest(changeRequestor(deleteConfiguration,
								C?fragment,
								@default,
								CR?garpModel,
								C),
				string('The same configuration in the specialised %s "%s" will be deleted',C?fragment?displayTypeName, C?fragment?name),
				C?fragment)
		else
			CR->>warning(
				string('There is already a "%s" configuration between these instances in the specialised %s "%s")',
					C?name,C?fragment?displayTypeName, C?fragment?name),
				C?fragment)
	).
	
%%
checkNewIdentity(C,
	IArg1: garpInstance,
	IArg1Route: chain,
	IArg2: garpInstance,
	IArg2Route: chain,
	_OtherMF: modelFragment,
	CR: changeRequestor):->

	%gp3 0.13: general rewrite because of bugs related to routing
	
	%if the new identity is in a more general mf, we delete the conf
	if 
		C->>isMoreGeneralRelation(IArg1,IArg1Route,IArg2,IArg2Route)
	then
		CR->>addChangeRequest(changeRequestor(deleteConfiguration,
								C?fragment,
								@default,
								CR?garpModel,
								C),
				string('The configuration between the same arguments in the specialised %s "%s" will be deleted',C?fragment?displayTypeName,C?fragment?name),
				C?fragment),
				
	%if the new identity is in a specialized mf (which is blocked by the editor anyway)
	%we block
	if
		C->>isSpecializedRelation(IArg1,IArg1Route,IArg2,IArg2Route)
	then
		CR->>impossible(string('There is allready a configuration between these instances in the %s "%s"',C?fragment?name),C?fragment?displayTypeName, C?fragment),
	%if it is in the same fragment, we block as well
	if
		C->>sameArguments(IArg1,IArg1Route, IArg2, IArg2Route)
	then
		CR->>impossible('There is allready a configuration between these instances.').
%%		

%%

%%
%%
copyToNewElement(FE,Mappings: hash_table, NewMF: modelFragment, New: configuration):<-
	%gp3 0.2 Copy contents and relations (hypers) to a new assumptionInstance
	%see fragmentElement<<-copyToNewElement for details
	%we fail if required connected objects are missing from the Mappings table
	
	%lets see if the needed other elements are allready copied
	
	NewArg1Info = NewMF<<-copyMF_mapRelatedElement(FE?argument1,FE?argument1Route,Mappings),
	NewArg2Info = NewMF<<-copyMF_mapRelatedElement(FE?argument2,FE?argument2Route,Mappings),
		%fails when these ones are not available yet, so will try again later
		
	%everything checked, we can create the item
	New = NewMF<<-addNewConfiguration(FE?stateName, FE?definition,
				NewArg1Info?first, NewArg1Info?second, NewArg2Info?first, NewArg2Info?second,
				FE?remarks).
%%

:-pce_end_class.
		  
