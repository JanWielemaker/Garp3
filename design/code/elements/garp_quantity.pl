/*
Definitie garpQuantity class
*/

:-pce_begin_class(
		  garpQuantity(name),
		  fragmentElement,
		  "definition of quantity element of model fragments"
		 ).

variable(instanceRoute, chain, none). %route van geïmporteerde fragmenten om vanuit
									%deze quantity bij de bijbehorende garpInstance te komen
variable(quantityAssumptions, chain, both, "List of quantityAssumption keywords like generate_all_values etc"). %gp 0.3.15, modeldefinition 8. %kept this one a free list to make it more flexible
%%
initialise(P, 
	MF: modelFragment, 
	D: garpQuantityDefinition,
	QS : quantitySpace = quantitySpace,
	Remarks: remarks = string) :->

	P->+initialise(MF, Remarks),
	P->>instanceRoute(new(chain)),
	P->>quantityAssumptions(new(chain)), %gp3 0.3.15
	ignore(P->>changeDefinition(D,QS)).
%%

%%
changeDefinition(P,
	D: garpQuantityDefinition,
	QS: quantitySpace):->
	"Make this quantity an instance of the given definition (Internal)" ::
	%faalt wanneer er de meegegeven QS geen bij de definitie toegestane QS is

	%we checken dit eventjes apart, omdat we niet aan de oude definitie willen rommelen en dat dan blijkt dat de nieuwe niet deugt

	D->>quantitySpaceAllowed(QS),
	P->>delete_hypers(definition), %is er 0 of 1 als het goed is
	P->>hyper(D,definition,garpQuantity),
	P->>delete_hypers(quantitySpace),
	P->>hyper(QS,quantitySpace,quantity).
%%
	
%%
definition(P, 
	D : garpQuantityDefinition
	):<-
	"Return the definition this quantity is based on" ::

	%er is echt iets mis als die er niet is
	D = P<<-hypered(definition).
%%
	
%%
garpInstance(P,
	 I : garpInstance
	):<-
	"returns the associated instance" ::

	I = P<<-hypered(garpInstance).
%%

%%
name(P,
	N : name
	):<-
	"Return the name according to the definition" ::

	N = P?definition<<-name.
%%

%%
quantitySpace(P,
	      QS):<-
	"Return the associated quantity space" ::

	QS = P<<-hypered(quantitySpace).
%%

%%
calculi(Q,
	C: chain):<-
	%geef alle calculi waarin de quantity een argument is in een nieuwe chain

	C = Q<<-all_named_hypered(calculus).
%%

%%
values(Q,
	V : chain):<-
	"Return all value elements that are defined with this quantity" ::
	%kunnen er meer zijn want qs + dqs & meerdere fragmenten

	V = Q<<-all_named_hypered(value).
%%

%%
instanceRoute(Q,
	R: chain):->
	"Set instanceRoute" ::

	Q->>slot(instanceRoute,R?copy).
%
instanceRoute(Q,
	R: chain):<-
	"Return instanceRoute" ::

	R = ?(Q,slot,instanceRoute)<<-copy.
%%

%%
checkDeleteElement(Q,
	Element: fragmentElement,	
	CR: changeRequestor):->
	%overwrite van fragmentElement:
	if
	(
		Element = Q<<-garpInstance
	;
		Q?instanceRoute->>member(Element)
	)

	then
		CR->>addChangeRequest(changeRequestor(deleteQuantity,
								Q?fragment,
								@default,
								CR?garpModel,
								Q),
			string('The quantity "%s" in %s "%s" will be deleted because it\'s instance "%s" is no longer part of the %s',Q?name, Q?fragment?displayTypeName, Q?fragment?name, Q?garpInstance?name,Q?fragment?displayTypeName),
			Q?fragment).
%%

%%
checkChangedQuantity(Q,
	Other: 'garpQuantity*',
	R: chain,
	D: garpQuantityDefinition,
	MF: modelFragment,
	CR: changeRequestor):->
	
	/*
	Check of deze quantity dezelfde definitie heeft als de meegestuurde definitie
	als dat zo is, dan moeten we als volgt reageren:
	
	1. Als wij lager in de importhierarchie van fragmenten zitten, dan gaan wij weg (subchange)
	2. Als wij hoger in de importhierarchie zitten, dan mag het niet
	3. Als we in hetzelfde modelfragment zitten mag het ook niet
	4. Het kan ook nog dat de quantity bij een broertje van ons wordt gedefinieerd. Dat is geen probleem
	*/
	
	%gp3: we should not only check whether the MF matches, but also if its the same
	%import path
	%
	%this method is called for all quantities of the instance that gets the new/changed one
	
	if
	(
		Other \== Q,	%wijzelf is nooit een probleem
		D = Q<<-definition
	)
	then
	(
		%1: we are lower in the hierarchy. So the new route is the tail of ours
		if
		(
			\+ R->>equal(Q?instanceRoute),
			Q?instanceRoute->>is_tail(R)
		)
			
		then
			CR->>addChangeRequest(changeRequestor(deleteQuantity,
								Q?fragment,
								@default,
								CR?garpModel,
								Q),
				string('The quantity "%s" for the same instance in the specialised %s "%s" will be deleted',Q?name, Q?fragment?displayTypeName, Q?fragment?name),
				Q?fragment),
		
		%2. We are higher in the hierarchy. So our route is the tail of the new one
		if
		(
			\+ R->>equal(Q?instanceRoute),
			R->>is_tail(Q?instanceRoute)
		)
		then
			CR->>impossible(
				string('This instance already has a quantity based on the given definition in the more general %s "%s"',Q?fragment?displayTypeName, Q?fragment?name),Q?fragment),

		%3. The same MF: same route
		if
			Q?instanceRoute->>equal(R)
		then
			CR->>impossible(
				'This instance already has a quantity based on the given definition',MF)
	).			
%%

%%
copyToNewElement(FE,Mappings: hash_table, NewMF: modelFragment, New: garpQuantity):<-
	%gp3 0.2 Copy contents and relations (hypers) to a new assumptionInstance
	%see fragmentElement<<-copyToNewElement for details
	%we fail if required connected objects are missing from the Mappings table
	
	%lets see if the needed other elements are allready copied
	
	NewRef = NewMF<<-copyMF_mapRelatedElement(FE?garpInstance,FE?instanceRoute,Mappings),
		%fails when this one is not available yet, so will try again later
	%everything checked, we can create the item
	%gp3 0.3.16: added the quantity assumptions
	New = NewMF<<-addNewQuantity(NewRef?first,NewRef?second,FE?stateName,FE?definition,
			FE?quantitySpace,FE?remarks,FE?quantityAssumptions).
%%

%%
checkQSCorrCompatibility(FE, Other: garpQuantity, _CheckInverse: bool, NewQSValues: newQSValues = [chain], NewOtherQSValues: newOtherQSValues = [chain]):->
	%gp3 0.3 Succeeds if the qs of receiver is compatible with the one of Other
	%for a qs/full-correspondence (same order of interval-point), either normal
	%(checkInverse = @off) or when inversed (@on).
	%for the receiver and or other argument new qs-values can be given
	%in that case, we use those instead of the current qs
	%(needed for checking a change-to-come in qs)
	
	%because of possibly new qs values, we cannot use a shortcut when the 2 quantities have the same qs
	
	%gp3 0.3.8: No longer checks for same order of interval-point: just same number
	%of values. So we comment out the rest.
	
	QS1 = FE<<-quantitySpace,
	QS2 = Other<<-quantitySpace,

	if
		NewQSValues = @default
	then
		V1 = QS1<<-values %allready a copy
	else
		V1 = NewQSValues<<-copy,
	if
		NewOtherQSValues = @default
	then
		V2_Current = QS2<<-values
	else
		V2_Current = NewOtherQSValues<<-copy,
		
	S = V1<<-size,
	S = V2_Current<<-size. %else just forget it
	
	%rest is gone in version 0.3.8
%%
				
:-pce_end_class.
		  
