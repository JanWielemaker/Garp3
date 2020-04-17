/*
Definitie garpAttribute class
*/

:-pce_begin_class(
		  garpAttribute,
		  fragmentElement,
		  "definition of garpAttribute element of model fragments"
		 ).

variable(instanceRoute, chain, none). %route van geïmporteerde fragmenten om vanuit
									%dit attribute bij de bijbehorende garpInstance te komen

variable(valueReference,valueReference,both,"Reference to the attribute value").

%%
initialise(A, MF: modelFragment, D : garpAttributeDefinition, 
		Value: valueReference, Remarks : remarks = string) :->

	A->+initialise(MF, Remarks),
	A->>changeDefinition(D,Value),
	A->>instanceRoute(new(chain)).
%%

%%
changeDefinition(A,D : garpAttributeDefinition,
		Value : valueReference):->
	"Change the definition and value of this attribute" ::

	A->>delete_hypers(definition), %is er 0 of 1 als het goed is
	A->>hyper(D,definition,garpAttribute),
	A->>valueReference(Value?copy).
%%	

%%
definition(A, D : garpAttributeDefinition):<-
	"Return the definition this attribute is based on" ::

	%faalt als ie er niet is, dan is er echt iets mis
	D = A<<-hypered(definition).
%%

%%
garpInstance(A,
	 I : garpInstance
	):<-
	"returns the associated instance" ::

	I = A<<-hypered(garpInstance).
%%

%%
name(A,
	N : name):<-
	"Return the name according to the definition" ::

	N = A?definition<<-name.
%%

%%
instanceRoute(A,
	R: chain):->
	"Set instanceRoute" ::

	A->>slot(instanceRoute,R?copy).
%
instanceRoute(A,
	R: chain):<-
	"Return instanceRoute" ::

	R = ?(A,slot,instanceRoute)<<-copy.
%%

%%
checkDeleteElement(A,
	Element: fragmentElement,	
	CR: changeRequestor):->
	%overwrite van fragmentElement: check onze argumenten
	
	if
	(
		Element = A<<-garpInstance
	;
		A?instanceRoute->>member(Element)
	)
	then
		CR->>addChangeRequest(changeRequestor(deleteAttribute,
								A?fragment,
								@default,
								CR?garpModel,
								A),
			string('The attribute "%s" in %s "%s" will be deleted because it\'s instance "%s" is no longer part of the %s',A?name, A?fragment?displayTypeName,A?fragment?name, A?garpInstance?name,A?fragment?displayTypeName),
			A?fragment).
%%

%%
checkChangedAttribute(A,
	Other: 'garpAttribute*',
	R: chain,
	D: garpAttributeDefinition,
	MF: modelFragment,
	CR: changeRequestor):->
	
	/*
	Check of dit attribuut dezelfde definitie heeft als de meegestuurde definitie
	als dat zo is, dan moeten we als volgt reageren:
	
	1. Als wij lager in de importhierarchie van fragmenten zitten, dan gaan wij weg (subchange)
	2. Als wij hoger in de importhierarchie zitten, dan mag het niet
	3. Als we in hetzelfde modelfragment zitten mag het ook niet
	4. Het kan ook nog dat het attribute bij een broertje van ons wordt gedefinieerd. Dat is geen probleem
	*/
	
	%gp3 0.2: changed this to also look at the import path (route), because we can have
	%several versions of the same instance in an MF (different routes)
	
	if
	(
		Other \== A,	%wijzelf is nooit een probleem
		D = A<<-definition
	)
	then
	(
		%1: we are lower in the hierarchy. So the new route is the tail of ours
	
		if
		(
			\+ R->>equal(A?instanceRoute),
			A?instanceRoute->>is_tail(R)
		)
		then
			CR->>addChangeRequest(changeRequestor(deleteAttribute,
								A?fragment,
								@default,
								CR?garpModel,
								A),
				string('The attribute "%s" for the same instance in the specialised %s "%s" will be deleted',A?name, A?fragment?systemTypeName,A?fragment?name),
				A?fragment),
		
		%2. We are higher in the hierarchy. So our route is the tail of the new one
		if
		(
			\+ R->>equal(A?instanceRoute),
			R->>is_tail(A?instanceRoute)
		)
		then
			CR->>impossible(
				string('This instance already has an attribute based on the given definition in the more general %s "%s"',A?fragment?systemTypeName, A?fragment?name),A?fragment),

		%3. The same MF: same route
		if
			A?instanceRoute->>equal(R)
		then
			CR->>impossible(
				'This instance already has an attribute based on the given definition',MF)
	).			

%%
%%checkChangedValues: check of we een subchange moeten posten vanwege gewijzigde values in de def

checkChangedValues(A,
	Values: chain,
	MF: modelFragment,
	CR: changeRequestor):->
	
	Result = A?valueReference<<-checkNewValues(Values,@off),
	Actie = Result<<-first,
	Arg = Result<<-second,

	if (
		Actie = changeSameType
		)
	then
		CR->>addChangeRequest(changeRequestor(changeAttribute,
								MF,
								@default,
								CR?garpModel,
								A,
								A?stateName,
								A?definition,
								Arg?copy,
								A?remarks
								),
				string('The "%s" attribute for "%s" in %s "%s" will be updated',
					A?name,
					A?garpInstance?name,
					MF?systemTypeName,
					MF?name),
				A,
				@nil), %<<-flag voor "alleen feedback wanneer subfeedback"
	if
		Actie = internal
	then
		CR->>addChangeRequest(changeRequestor(internalUpdateValueRef,
						@model,@default, CR?garpModel,
						A?valueReference,
						Arg),
				'Internal',@nil,@nil),
	if
		Actie = delete
	then
		%dat wordt impossible, we deleten niet
		CR->>impossible(
				string('The attribute "%s" (%s "%s", instance "%s") would have an invalid value ("%s")"',
				A?name,
				MF?systemTypeName,
				MF?name,
				A?garpInstance?name,
				A?valueReference?valueName),
			MF).
%%

%%

%%
copyToNewElement(FE,Mappings: hash_table, NewMF: modelFragment, New: garpAttribute):<-
	%gp3 0.2 Copy contents and relations (hypers) to a new assumptionInstance
	%see fragmentElement<<-copyToNewElement for details
	%we fail if required connected objects are missing from the Mappings table
	
	%lets see if the needed other elements are allready copied
	
	NewRef = NewMF<<-copyMF_mapRelatedElement(FE?garpInstance,FE?instanceRoute,Mappings),
		%fails when this one is not available yet, so will try again later
	%everything checked, we can create the item
	New = NewMF<<-addNewAttribute(NewRef?first,NewRef?second,FE?stateName,FE?definition,
			FE?valueReference?copy,FE?remarks).
%%

:-pce_end_class.
		  
