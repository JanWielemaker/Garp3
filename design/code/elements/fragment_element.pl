/*
Definitie fragmentElement class
*/

%Abstracte class.
:-pce_begin_class(
		  fragmentElement,
		  object,
		  "Abstract parent of classes for subelement of model fragments"
		 ).

%ze hebben allemaal een remarks var
variable(remarks_translatable, translatable, both, "Remarks on this object"). %gp3 1.4: added this, remarks is just a get/set method now

%%
initialise(FE,
		  _MF: modelFragment,			%gp3 1.4: added this one for use in translation, not needed now, but might be handy for future use
	   Remarks: remarks = [string]):->
	"Do the abstract part of element creation" ::

	FE->+initialise,
	%create the translatable
	
	FE->>remarks_translatable(?(@model?translator,getTranslatable,empty)),
	default(Remarks,new(string),RM),
	FE->>remarks(RM).
%%

%%%%% wrapping translatables %%%%%%%%
%%gp3 1.4.0

remarks(FE, Remarks: string):->
	"Set a copy of the given string as remarks" ::

	%is now a wrapper around remarks_translatable
	
	FE?remarks_translatable->>value(?(Remarks,strip,both)).
%
remarks(FE,	Remarks : string
       ):<-
	"Return a copy of the remarks" ::
	
	Remarks = FE?remarks_translatable?value<<-copy.
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%relevantComments
%gp3 1.4: return a string with the remarks to show for this one
%when no remarks: return an empty string
%this is the general one. Will do for most elements (see inequality and calculus for an overwrite)

relevantComments(FE,RC: string):<-
	if
		0 = FE?remarks<<-size
	then
		RC *= string
	else
	(
		FR = FE<<-fragment,
		if
			FR->>isInputSystem
		then
			FT = 'SC'
		else
			FT = 'MF',
		RC *= string('%s %s: %s', FT, FR?name,FE?remarks)
	).
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%% ABSTRACT %%%%%%%%%%%%%%
definition(_FE, _: object):<-
	%gp3 1.4: this one is totally abstract. Some element define it to get to the definition object, used in simulate/find_in_design.pl
	
	fail.
%%

%%
isCondition(P):->
	"Succeeds if the element is conditional in its fragment" ::
	%anders dus given

	P?fragment->>isConditionalElement(P).
%%

%%
stateName(P,
	SN : name):<-
	"Returns either 'condition' or 'consequence'" ::

	if
		P->>isCondition
	then
		SN = name('condition')
	else
		SN = name('consequence').
%%

%%
fragment(P,
	F : modelFragment):<-
	"returns the fragment where this element is defined" ::

	F = P<<-hypered(fragment).
%%

%%
checkDeleteElement(_FE,
	_Element: fragmentElement,
	_CR: changeRequestor):->
	%stuur een verwijder subcr als dit element een directe sub van het gegeven element is, het gegeven element een argument van dit element is, of het gegeven element een importedFragment is en onderdeel is van de route naar de verwijzing (super/argument) van dit element 
	% Abstract: slaagt stilzwijgend. Zie subclasses

	true.
%%

%%
relations(FE,
	Class: [name],
	R : chain):<-
	"Return all garpRelation elements (of the given class) that are defined with this element" ::

	AllRelations = FE<<-all_named_hypered(garpRelation),
	if
		Class == @default
	then
		R = AllRelations<<-copy
	else
		R = AllRelations<<-find_all(->>(@arg1,instance_of,Class)),
	AllRelations->>done,
	R->>unique.
%%

%%
copyToNewElement(_FE,_Mappings: hash_table, _NewMF: modelFragment, New: fragmentElement):<-
	%gp3 0.2 this call should be overwritten in every subclass
	%called by modelFragment->>applyChange_copyMF to copy this element.
	%any connected elements are also copied, so the code can find the new objects 
	%in the Mappings table (mapping old to new)
	%when anything is missing right now, this call can fail. copyMF will then continue copying other
	%elements, and come back when that is done. Hopefully, the needed related elements are copied by then
	
	%because failure means "try again", we will not fail, but add a dummy
	
	New *= fragmentElement('Not valid').

:-pce_end_class.
		  
