/*
Definitie sketchStructuralRelationElement class
*/

:-pce_begin_class(
		  sketchStructuralRelationElement(definition),
		  sketchGenericRelationElement,
		  "definition of structural relation (between concepts) in model sketches"
		 ).
%%
initialise(C, D: sketchStructuralRelationDefinition,
			Arg1: argument1 = sketchObjectElement,
			Arg1Route: chain,
			Arg2 : argument2 = sketchObjectElement,
			Arg2Route: chain,
			Remarks: string,
	                Sketch: sketch) :->

	C->+initialise(Arg1,Arg1Route,Arg2,Arg2Route,Remarks),
	C->>changeDefinition(D),
        % get(@model, hypered, structureMF, Sketch),   % AB, sept 2006 
        send(C, hyper(Sketch, sketch, sketchStructuralRelationElement)).  % AB, april 2006 

%%

%%
changeDefinition(C,D : sketchStructuralRelationDefinition):->
	"Change the definition " ::

	C->>delete_hypers(definition), %is er 0 of 1 als het goed is
	C->>hyper(D,definition,relation).
%%

%%
definition(C, D : sketchStructuralRelationDefinition):<-
	"Return the definition this relation is based on" ::

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
	Element: sketchElement,
	CR: changeRequestor):->
	%overwrite van sketchElement: als ��n van onze argumenten Import in zijn 
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
		CR->>addChangeRequest(changeRequestor(deleteSketchStructuralRelation,
								C?sketch,
								@default,
								CR?garpModel,
								C),
			string('A sketch structural relation "%s" in this %s will also be deleted because one of its arguments is no longer part of the %s',C?name, C?sketch?displayTypeName, 
				C?sketch?displayTypeName),
			C?sketch).
%%



%%%%%%%% updates relations after another relation with same def has been changed, AB, nov 2006
%
updateStructuralRelationsWithSameDefinition(C,
	Other: sketchStructuralRelationElement,
	Def: sketchStructuralRelationDefinition,
	OtherMF: sketch):->

        % de ontvanger zit in dezelfde sketch, en maakt gebruik van dezelfde def
	if
	(
		Other \= C,
		OtherMF = C<<-sketch, 
	        Def = C<<-definition
	)
	then
	(
	        % Structural Relation needs updating
                send(C, remarks, Def?remarks?copy), 
	        % get associated sketchVisualRelationElement and update
                get(C, hypered, sketchElement, SE), 
	        SE->>updateDisplay
	).



%%
checkChangedRelation(C,
	Other: 'relation*',
	_Def: sketchStructuralRelationDefinition,
	Arg1: sketchObjectElement,
	_Arg1Route: chain,
	Arg2: sketchObjectElement,
	_Arg2Route: chain,
	_OtherMF: sketch,
	_CR: changeRequestor):->
	
	%check de gevolgen voor deze relation als er een relation
	%wordt gewijzigd of bijkomt met de meegestuurde gegevens
	
	%WORDT IVM GEWIJZIGDE NORMEN (ALS HET KAN MAG HET) NIET MEER GEBRUIKT
	%1: we zijn het zelf, of het gaat helemaal niet over dezelfde concepts
	%niets aan de hand
	
	(
		C == Other
	;
		(\+ C->>isArgument(Arg1))
	;
		(\+ C->>isArgument(Arg2))
	),!.
%
checkChangedRelation(C,
	_Other: 'relation*',
	Def: sketchStructuralRelationDefinition,
	Arg1: sketchObjectElement,
	Arg1Route: chain,
	_Arg2: sketchObjectElement,
	Arg2Route: chain,
	OtherMF: sketch,
	CR: changeRequestor):->

	%2: de ontvanger zit in hetzelfde of een algemener sketch
	%als de ontvanger algemener is, zit zijn MF in beide argumentroutes
	%(niet slechts in ��n, want dan kan het andere op een andere wijze
	%ge�mporteerd zijn, bijvoorbeeld via een aparte applies-to)
	
	if
	(
		OtherMF = C<<-sketch
	;
		(
			Arg1Route->>find(@arg1?referencedSketch == C?sketch),
			Arg2Route->>find(@arg1?referencedSketch == C?sketch)
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
			CR->>impossible('The given relation already exists',C?sketch)
		else
			CR->>warning(
				string('There is already a "%s" relation between these concepts (defined in %s "%s")',
					C?name,C?sketch?displayTypeName, C?sketch?name),
				C?sketch)
	),
	
	%3: de ontvanger zit in een specifieker sketch
	
	if
	(
		C?argument1Route->>find(
			@arg1?referencedSketch == OtherMF), %weer allebei
		C?argument2Route->>find(
			@arg1?referencedSketch == OtherMF) %weer allebei
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
			% CR->>addChangeRequest(changeRequestor(deleteRelation,
			CR->>addChangeRequest(changeRequestor(deleteSketchStructuralRelation,
								C?sketch,
								@default,
								CR?garpModel,
								C),
				string('The same relation in the specialised %s "%s" will be deleted',C?sketch?displayTypeName, C?sketch?name),
				C?sketch)
		else
			CR->>warning(
				string('There is already a "%s" relation between these concepts in the specialised %s "%s")',
					C?name,C?sketch?displayTypeName, C?sketch?name),
				C?sketch)
	).
	
%%
checkNewIdentity(C,
	IArg1: sketchObjectElement,
	IArg1Route: chain,
	IArg2: sketchObjectElement,
	IArg2Route: chain,
	_OtherMF: sketch,
	CR: changeRequestor):->

	%gp3 0.13: general rewrite because of bugs related to routing
	
	%if the new identity is in a more general mf, we delete the conf
	if 
		C->>isMoreGeneralRelation(IArg1,IArg1Route,IArg2,IArg2Route)
	then
		CR->>addChangeRequest(changeRequestor(deleteSketchStructuralRelation,
								C?sketch,
								@default,
								CR?garpModel,
								C),
				string('The relation between the same arguments in the specialised %s "%s" will be deleted',C?sketch?displayTypeName,C?sketch?name),
				C?sketch),
				
	%if the new identity is in a specialized mf (which is blocked by the editor anyway)
	%we block
	if
		C->>isSpecializedRelation(IArg1,IArg1Route,IArg2,IArg2Route)
	then
		CR->>impossible(string('There is already a relation between these concepts in the %s "%s"',C?sketch?name),C?sketch?displayTypeName, C?sketch),
	%if it is in the same sketch, we block as well
	if
		C->>sameArguments(IArg1,IArg1Route, IArg2, IArg2Route)
	then
		CR->>impossible('There is already a relation between these concepts.').
%%		

%%

%%
%%
copyToNewElement(FE,Mappings: hash_table, NewSK: sketch, New: sketchStructuralRelationElement):<-
	%gp3 0.2 Copy contents and relations (hypers) to a new assumptionInstance
	%see sketchElement<<-copyToNewElement for details
	%we fail if required connected objects are missing from the Mappings table
	
	%lets see if the needed other elements are already copied
	
	NewArg1Info = NewSK<<-copySK_mapRelatedElement(FE?argument1,FE?argument1Route,Mappings),
	NewArg2Info = NewSK<<-copySK_mapRelatedElement(FE?argument2,FE?argument2Route,Mappings),
		%fails when these ones are not available yet, so will try again later
		
	%everything checked, we can create the item
	New = NewSK<<-addNewSketchStructuralRelation(FE?definition,
				NewArg1Info?first, NewArg1Info?second, NewArg2Info?first, NewArg2Info?second,
				FE?remarks).
%%

:-pce_end_class.
		  
