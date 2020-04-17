/*
Definitie valueReference class
Klasse voor de representatie en verwijzing van waarden op naam.
Door gebruik te maken van een secundaire id kunnen verschillende valueReference objecten
"over dezelfde waarde gaan". Dit is van belang bij het wijzigen van de naam van de value e.d.

Part of garp3. Homer code except where gp3 explicitly mentioned

gp3 1.4 added possibility to be translatable. A valueReference now uses a translatable for its valueName
but will set this one to nonTranslatable when needed. This way, we work transparantly for normal (ie non translatable) values, and also support translations
*/

:-pce_begin_class(
		  valueReference(id,valueName,type),
		  object,
		  "definition of refered values"
		 ).

variable(valueName_translatable,translatable,both). %gp3 1.4 valueName available through accessor methodes below
variable(type,{point,interval,state},both,"Value by type").
variable(id,secondaryID,both,"Value by unique id").

%%
initialise(VR,
	VN : valueName = '[name|translatable]', %for copying purposes it is also allowed to directly send a translatable. A copy is saved
	T : type = [{point,interval,state}],
	Translatable: translatable = [bool], %gp3 1.4: new default: valueReference is by default translatable
	Tra: translator = [translator]	%gp3 1.4: needed only for valueReferences created during creation of model
	):-> 
	
	default(VN,'',Name),
	default(T,state,Type), %we doen maar wat
	default(Tra,@model?translator,TR),
	
	if
		Name->>instance_of(translatable)
	then
		VR->>valueName_translatable(Name?copy)
	else
	(
		VR->>valueName_translatable(?(TR,getTranslatable,unique)),
		VR->>valueName(Name)
	),
	if
		Translatable == @off
	then
		VR?valueName_translatable->>setNonTranslatable,
	VR->>type(Type), 
	VR->>id(new(secondaryID)). %nieuwe verwijzing: verwijst niet naar een andere
%%

%%%%%%%% accessing translatable
%%
valueName(VR,
	N: name):<-

	%gp3 1.4: get valueName
	N = VR?valueName_translatable<<-value.
%	
valueName(VR,
	N : name):->
	"Set valueName slot" ::

	VR?valueName_translatable->>value(N).
%%

name(VR, N: name):<-
	%gp3 0.1 Map ?name on ?valueName, needed for @app->addExportedObject et cetera (expects ?name)
	N = VR<<-valueName.
%%

%%
translatable(VR):->
	%gp3 1.4: succeeds if this value is translatable
	
	\+ VR?valueName_translatable->>isNonTranslatable.
%%

%%
setTranslatable(VR, TR: bool):->
	%gp3 1.4: set the current value to be translatable or not
	
	if
		TR == @on
	then
		VR?valueName_translatable->>setTranslatable
	else
		VR?valueName_translatable->>setNonTranslatable.
%%

%%
copy(VR,
	O : valueReference):->
	"Make receiver equal to given valueReference" ::

	%gp3 1.4: this means copying the translatable
	VR->>valueName_translatable(O?valueName_translatable?copy),
	VR->>type(O?type),
	VR->>id(O?id).
%%


%%
copy(VR,
	C : valueReference):<-
	"Return new valueReference that is a copy of receiver" ::

	C *= valueReference(VR?valueName_translatable, VR?type), %whether or not this one is translatable and the translator used are copied as a prop of valueName
	C->>id(VR?id).
%%


%%
equal(VR,
	O : valueReference):->
	"succeeds if the value contents as well as the id's are the same" ::

	VR->>equalContent(O),
	VR->>sameValue(O).
%%

%%
equalContent(VR,
	O : valueReference):->
	"Succeeds if receiver's value contents are equal to given valueReference (not id)" ::

	%gp3 1.4: we define this as equal in this translation
	VR?valueName->>equalExportName(O?valueName),
	VR?type->>equal(O?type).
%%

%%
sameValue(VR,
	O : valueReference):->
	"Succeeds if receiver refers to same value as given valueReference (by id)" ::

	VR?id->>equal(O?id).
%%

%%
copyContent(VR,
	C: valueReference):<-
		"Return new valueReference that has the same content as the receiver, but a different ID" ::

	%gp3 1.4: This means we have to copy the valueName_translatable
	C *= valueReference('',VR?type),
	C->>valueName_translatable(VR?valueName_translatable?copy). %fixes the translatable option etc
%%

%%
	/*
	CheckNewValues
	check of deze value kan blijven bestaan als NewValues de nieuwe lijst
	mogelijke waarden is
	Door te zoeken naar eenzelfde waarde in die lijst.
	Gebruikt bij checkChange dingen van values en relaties naar QS-elementen
	het resultaat is een naam voor de benodigde actie op de value/relatie + een argument daarbij:
	
same: onbepaald	-> niets veranderen
changeSameType: ValueReference -> naar ValueReference laten wijzen, van zelfde point/interval/state type
changeOtherType: ValueReference ->naar ValueReference laten wijzen, wijziging van point/interval type (van state naar iets anders zou niet moeten voorkomen in NewValues)
internal: ValueReference ->Value is nog goed alleen gegevens zijn gewijzigd, internalUpdateValueRef CR nodig
delete: onbepaald -> het verwijzend object moet weg want het kan niet naar iets anders verwijzen

	Als CanSwitchType @off is kan er niet van interval naar point of v.v. worden gewijzigd, en wordt in het geval van changeOtherType dus delete teruggegeven
*/

checkNewValues(VR,
	NewValues: chain,
	_CanSwitchType: bool,
	Result: tuple):<-

	%1: Precies dezelfde waarde is nog aanwezig, geen probleem
	NewValues->>find(->>(@arg1,equal,VR)),!, %zelfde ID, dus terug te vinden in kopies
	Result *= tuple(same,@nil).
%
checkNewValues(VR,
	NewValues: chain,
	_CanSwitchType: bool,
	Result: tuple):<-
	
	%2: Er is een waarde met zelfde naam en type: er is een change nodig om daar naar te wijzen
	NewVal = NewValues<<-find(->>(@arg1,equalContent,VR)),!,
	Result *= tuple(changeSameType,NewVal).
%
checkNewValues(VR,
	NewValues: chain,
	_CanSwitchType: bool,
	Result: tuple):<-

	%3) Er is een waarde met dezelfde ID, die nog steeds van hetzelfde type is
	%Daar wijzen we al naar, dus moeten we de interne
	%kopie van de gegevens bijwerken
	NewVal = NewValues<<-find(and(->>(@arg1,sameValue,VR),
									@arg1?type == VR?type)),!,
	Result *= tuple(internal,NewVal).
%
checkNewValues(VR,
	NewValues: chain,
	CanSwitchType: bool,
	Result: tuple):<-
	%4) Er is een waarde met dezelfde naam maar ander type: daar gaan we naar wijzen
	%(als ander type mag)
	CanSwitchType = @on,
	NewVal = NewValues<<-find(->>(
			@arg1?valueName,equalExportName,VR?valueName)),!,
	Result *= tuple(changeOtherType,NewVal).
%
checkNewValues(_VR,
	_NewValues: chain,
	_CanSwitchType: bool,
	Result: tuple):<-
%5: in alle andere gevallen moet ie weg
	Result *= tuple(delete,@nil).
%%

	
:-pce_end_class.
		  
