/*
Definitie sketchDependencyDefinition class
*/

:-pce_begin_class(
		  sketchDependencyDefinition(name),
		  object,
		  "sketch dependency definition"
		 ).

variable(name,name,get,"Name of the attribute definition").
variable(remarks,string,none,"Remarks on this attribute definition").

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

%%%%%%%%%%%%%CHANGES%%%%%%%%%%%%%%
%%
checkChange_addRelationDef(D,
	CR:changeRequestor):->
	%checken op naamconflict

	NewName = CR<<-argument(1),
	NewName->>equalExportName(D?name),
	CR->>impossible('The given name is already used', D).
%%

%%
checkChange_changeRelationDef(D,
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
checkChange_changeRelationDef(D,CR):->
	%2: we zijn niet het object

	NewName = CR<<-argument(1),
	NewName->>equalExportName(D?name),
	CR->>impossible('The given name is already used',D).
%
applyChange_changeRelationDef(D,
			      CR: changeRequestor
			     ):->
	%Deze definitie verandert dus
	
	D->>name(?(CR,argument,1)),
	D->>remarks(?(CR,argument,2)).
%%

%%
checkChange_addAttributeDef(D,
	CR:changeRequestor):->
	%checken op naamconflict: mag ook niet bij attribute

	NewName = CR<<-argument(1),
	NewName->>equalExportName(D?name),
	CR->>impossible('The given name is already used by a relation', D).
%%

checkChange_changeAttributeDef(D,
			    CR : changeRequestor
			   ):->
	%checken op naamconflict: mag ook niet bij attribute

    NewName = CR<<-argument(1),
	NewName->>equalExportName(D?name), %gelijk?
	CR->>impossible('The given name is already used by a relation',D).
%

:-pce_end_class.
		  
