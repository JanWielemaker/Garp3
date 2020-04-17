/*
Definitie sketchElement class
*/

%Abstracte class.
:-pce_begin_class(
		  sketchElement,
		  object,
		  "Abstract parent of classes for subelement of model sketches"
		 ).

%ze hebben allemaal een remarks var
variable(remarks,string,none,"Remarks on this element").

%%
initialise(FE,
	   Remarks: remarks = [string]):->
	"Do the abstract part of element creation" ::

	FE->+initialise,
	default(Remarks,new(string),RM),
	FE->>remarks(RM).
%%

%%
remarks(FE,
	Remarks : remarks = string
       ):->
	"Set a copy of the given string as remarks" ::

	FE->>slot(remarks,?(Remarks,strip,both)).
%%

%%
remarks(FE,
	Remarks : string
       ):<-
	"Return a copy of the remarks" ::

	RealRemarks = FE<<-slot(remarks),
	Remarks = RealRemarks<<-copy.
%%

%%
isCondition(_P):->
	"Succeeds if the element is conditional in its sketch" ::
	%anders dus given
        fail. % AB, for now, pretend that everything in a sketch is consequence. This distinction should not matter, however.
	% P?sketch->>isConditionalElement(P).
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
sketch(P,
	Sketch : sketch):<-
	"returns the sketch where this element is defined" ::
        % find sketch for sketch element, i.e., conceptMapMF, causalModelMF, etc.
	Sketch = P<<-hypered(sketch). 
%%

%%
checkDeleteElement(_FE,
	_Element: sketchElement,
	_CR: changeRequestor):->
	%stuur een verwijder subcr als dit element een directe sub van het gegeven element is, het gegeven element een argument van dit element is, of het gegeven element een importedSketch is en onderdeel is van de route naar de verwijzing (super/argument) van dit element 
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
copyToNewElement(FE,_Mappings: hash_table, _NewSK: sketch, New: sketchElement):<-
	%gp3 0.2 this call should be overwritten in every subclass
	%called by sketch->>applyChange_copySK to copy this element.
	%any connected elements are also copied, so the code can find the new objects 
	%in the Mappings table (mapping old to new)
	%when anything is missing right now, this call can fail. copySK will then continue copying other
	%elements, and come back when that is done. Hopefully, the needed related elements are copied by then
	
	%because failure means "try again", we will not fail, but write a message to console
	%and add a dummy
	
	@pce->>write_ln('Warning. Class', FE?class_name, 'should have an overwritten implementation of copyToNewElement, but that is missing. Dummy added, not useful.'),
	New *= sketchElement('Not valid').

:-pce_end_class.
		  
