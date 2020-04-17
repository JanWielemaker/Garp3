/*
Definitie value class
part of garp3
Homer code except where gp3 mentioned
*/

:-pce_begin_class(
		  value(valueReference,derivative),
		  fragmentElement,
		  "definition of value element of model fragments"
		 ).

variable(derivative,bool,get,"@on when this is a derivative value").
variable(valueReference,valueReference,get,"Reference to qs value").
variable(quantityRoute, chain, none). %route van geïmporteerde fragmenten om vanuit
									%deze value bij de bijbehorende quantity te komen

%%
initialise(V, MF: modelFragment, Q : garpQuantity, D : derivative = bool, Val : valueReference) :->
	V->+initialise(MF, new(string)),
	send(V,slot,derivative,D),
	send(V,slot,valueReference,Val?copy),
	V->>hyper(Q,quantity,value),
	V->>quantityRoute(new(chain)).
%%

%%
quantity(V,
	Q : garpQuantity):<-
	"Return related quantity" ::

	Q = V<<-hypered(quantity).
%%

%%
quantityRoute(V,
	R : chain):->
	"Set quantityRoute" ::

	V->>slot(quantityRoute,R?copy).
%
quantityRoute(V,
	R : chain):<-
	"Get quantityRoute" ::

	R = ?(V,slot,quantityRoute)<<-copy.
%%

%%
checkDeleteElement(V,
	Element: fragmentElement,	
	CR: changeRequestor):->
	%overwrite van fragmentElement: check onze argumenten
	if
	(
		Element = V<<-quantity
	;
		V?quantityRoute->>member(Element)
	)
	then
		CR->>addChangeRequest(changeRequestor(deleteValue,
								V?fragment,
								@default,
								CR?garpModel,
								V),
			string('Value information for the quantity "%s" in %s "%s" will be deleted because the quantity is no longer part of the %s',V?quantity?name, V?fragment?displayTypeName,V?fragment?name,V?fragment?displayTypeName),
			V?fragment).
%%

%%
checkChangedQS(V,
	 _QN: name,
	_NewValues: chain,
	_CR: changeRequestor):->
	
	%check of we nog kunnen vinden waar we bij horen
	%uitgangspunt: we horen inderdaad bij de opgegeven quantity
	%1: niets aan de hand als we naar de afgeleide wijzen
	@on = V<<-derivative,!.
%
checkChangedQS(V,QN,NewValues,CR):->

	Result = V?valueReference<<-checkNewValues(NewValues,@on),
	Actie = Result<<-first,
	Arg = Result<<-second,
	
	if (
		Actie = changeSameType
	;
		Actie = changeOtherType
		)
	then
		CR->>addChangeRequest(changeRequestor(setValue,
								V?fragment,
								@default,
								CR?garpModel,
								V?quantity,
								V?quantityRoute,
								@off,
								V?stateName,
								Arg?copy
								),
				string('A %s value in %s "%s" will be updated to point to the new "%s" %s',
					QN,
					V?fragment?displayTypeName,
					V?fragment?name,
					Arg?valueName,
					Arg?type),
				V?fragment,
				@nil), %<<-flag voor "alleen feedback wanneer subfeedback"
	if
		Actie = internal
	then
		CR->>addChangeRequest(changeRequestor(internalUpdateValueRef,
						@model,@default, CR?garpModel,
						V?valueReference,
						Arg),
				'Internal',@nil,@nil),
	if
		Actie = delete
	then
		CR->>addChangeRequest(changeRequestor(deleteValue,
										V?fragment,
										@default,
										CR?garpModel,
										V),
			string('A %s value in %s "%s" that pointed to the value "%s" will be deleted',
				QN,
				V?fragment?displayTypeName,
				V?fragment?name,
				V?valueReference?valueName)
				,V?fragment).
%%			

%%
checkSetValue(V, _R: chain, D: bool, _State: '{condition,consequence}',
				_VR: valueReference, _MF: modelFragment,
				_CR: changeRequestor):->
	%er wordt bij onze quantity een nieuwe waarde gezet
	%check of dat wel gaat, of dat wij weg moeten
	%gp3: a lot of bugfixing done in 0.13, issues regarding route-checking

	\+ D = V<<-derivative.	%klaar
%
checkSetValue(V, R: chain, _D: bool, State: '{condition,consequence}',
				VR: valueReference, MF: modelFragment,
				CR: changeRequestor):->

	%als wij hoger in de hiërarchie zitten mag het niet
	%gp3 not only will it be necessary that our fragment is part of the route for the new value
	%our value must be connected to the same routed quantity as the lower
	%so our route must be the top part of the lower route
	
	if
	(
		R->>find(@arg1?referencedFragment == V?fragment), %just to make sure there is a connexion
		R->>is_tail(V?quantityRoute)%QR exists as tail of R
	)
	then
		CR->>impossible(
			string('This quantity already has a value in the more general %s "%s"', V?fragment?displayTypeName,V?fragment?name),V?fragment),

	%als we lager in de hiërarchie zitten verdwijnen wij, al dan niet stilzwijgend
	%gp3 The lower-level value will only disappear if
	%it is connected to the same routed quantity as the higher
	%so not only will the MF of the new value have to be part of the quantityroute of receiver
	%the whole quantityroute of the new value must be part of the quantityroute of the receiver
	
	if
	(
		V?quantityRoute->>find(@arg1?referencedFragment == MF), %we are below
		V?quantityRoute->>is_tail(R)
	)
	then
		CR->>addChangeRequest(changeRequestor(deleteValue,
										V?fragment,
										@default,
										CR?garpModel,
										V),
			string('A %s value in %s "%s" that pointed to the value "%s" will be deleted',
				V?quantity?name,
				V?fragment?displayTypeName,
				V?fragment?name,
				V?valueReference?valueName)
			,V?fragment,
			when(and(
					or(
						->>(V?valueReference,sameValue,VR),
						->>(V?valueReference?valueName,equalExportName,
							VR?valueName)
						),
					V?stateName == 	State
					),
				@nil,@default)
				%(geen feedback als hij dezelfde (waarde of naam) + state had, anders wel)
				),
			
	%als we in hetzelfde fragment zitten, verdwijnen we stilzwijgend
	
	%if gp3: if the receiver is in the same fragment as the given value
	%AND the route to the quantity is the same, we can delete the old value..
	if
	(
		MF = V<<-fragment,
		R->>equal(V?quantityRoute)
	)
	then
		CR->>addChangeRequest(changeRequestor(deleteValue,
										V?fragment,
										@default,
										CR?garpModel,
										V),
				'The old value will be deleted',
				V?fragment,@nil).
%%

%%
copyToNewElement(FE,Mappings: hash_table, NewMF: modelFragment, New: value):<-
	%gp3 0.2 Copy contents and relations (hypers) to a new assumptionInstance
	%see fragmentElement<<-copyToNewElement for details
	%we fail if required connected objects are missing from the Mappings table
	
	%lets see if the needed other elements are allready copied
	
	NewRef = NewMF<<-copyMF_mapRelatedElement(FE?quantity,FE?quantityRoute,Mappings),
		%fails when this one is not available yet, so will try again later
	%everything checked, we can create the item
	
	New = NewMF<<-addNewValue(NewRef?first,NewRef?second,FE?derivative,
			FE?stateName, FE?valueReference?copy).
%%
:-pce_end_class.
		  
