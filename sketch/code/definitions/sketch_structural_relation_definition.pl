/*
Definitie sketchStructuralRelationDefinition class
*/

:-pce_begin_class(
		  sketchStructuralRelationDefinition(name),
		  object,
		  "sketch Structural Relation definition"
		 ).

variable(name,name,get,"Name of the sketch structural relation definition").
variable(remarks,string,none,"Remarks on this sketch structural relation definition").

initialise(D,
	Name : name,
	Remarks : [string]
	):->
	%initialiseer de definitie, zodat ie goed past op CR's..

	D->+initialise,
	D->>name(Name),
	default(Remarks,new(string),RRemarks),
	D->>remarks(RRemarks).
%%

%%
name(D,
     N : name
    ):->
	"Set the name" ::

	D->>slot(name,N?makeGarp).
%%

%%
remarks(D,
	Remarks : remarks = string
       ):->
	"Set a copy of the given string as remarks" ::

	D->>slot(remarks,?(Remarks,strip,both)).
%%

%%
remarks(D,
	Remarks : string
       ):<-
	"Return a copy of the remarks" ::

	RealRemarks = D<<-slot(remarks),
	Remarks = RealRemarks<<-copy.
%%

%%
relations(D,
			C: chain
			):<-
	"Return a new chain with all relations based on this definition" ::
	C = D<<-all_named_hypered(relation).
%%


%
copyToNewElement(D, SK: sketch, New: sketchStructuralRelationDefinition):<-
        "Create a copy of this sketchStructuralRelationDefinition"::
	New *= sketchStructuralRelationDefinition(D?name?copy, D?remarks?copy),
	SK->>insertDefinitionObject(New).
%%


%%%%%%%%%%%%%CHANGES%%%%%%%%%%%%%%
%%

%%
checkChange_addSketchStructuralRelationDef(D,
% checkChange_addRelationDef(D,
	CR:changeRequestor):->
	%checken op naamconflict

	NewName = CR<<-argument(1),
	NewName->>equalExportName(D?name),
	CR->>impossible('The given name is already used. You can type a new name, or reuse an existing relation definition by selecting it from the list.', D).
%%

%%
checkChange_changeSketchStructuralRelationDef(D,
% checkChange_changeRelationDef(D,
			    CR : changeRequestor
			   ):->
	%als we zelf het object van de change zijn checken we of
	%de naam geldig is
	%als we niet zelf het object vd change zijn checken we op
	%naamconflict..

	%1: we zijn het object

	CR->>checkObject(D),!, %we zijn dezelfde
	@model->>norm_relationDef(CR,D). %gp3: replaced code with a norm
%
checkChange_changeSketchStructuralRelationDef(D,CR):->
	%2: we zijn niet het object

	NewName = CR<<-argument(1),
	NewName->>equalExportName(D?name),
	CR->>impossible('The given name is already used. You can type a new name, or reuse an existing relation definition by selecting it from the list.', D).
%
applyChange_changeSketchStructuralRelationDef(D,
			      CR: changeRequestor
			     ):->
	%Deze definitie verandert dus

	D->>name(?(CR,argument,1)),
	D->>remarks(?(CR,argument,2)).
%%

:-pce_end_class.
		  
