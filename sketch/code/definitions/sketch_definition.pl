/*
Definitie sketchDefinition class
*/

:-pce_begin_class(
		  sketchDefinition(name),
		  object,
		  "sketch definition"
		 ).

variable(name,name,get,"Name of the sketch definition").

variable(def_sheet, sheet, both, "Sheet with data for definition").
variable(remarks,string,none,"Remarks on this sketch definition").


initialise(D,
	Name : name,
	Remarks : [string]
	):->
	%initialiseer de definitie, zodat ie goed past op CR's..

	D->+initialise,
	D->>name(Name),
	default(Remarks,new(string),RRemarks),
	D->>remarks(RRemarks),
        D->>init_def_sheet.
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
init_def_sheet(_D):->
        writef('This should be specialized in subclass\n',[]).
%%


%%%%%%%%%%%%%CHANGES%%%%%%%%%%%%%%
%%

%%
checkChange_addSketchDef(D,
	CR:changeRequestor):->
	%checken op naamconflict

	NewName = CR<<-argument(1),
	NewName->>equalExportName(D?name),
	CR->>impossible('The given name is already used', D).
%%

%
applyChange_addSketchDef(_D,
			      _CR: changeRequestor
			     ):->
        % this does not seem to work
        debug(sketch(test), 'applyChange_addSketchDef',[]).
%%


%%
checkChange_changeSketchDef(D,
			    CR : changeRequestor
			   ):->
	%als we zelf het object van de change zijn checken we of
	%de naam geldig is
	%als we niet zelf het object vd change zijn checken we op
	%naamconflict..

	%1: we zijn het object

	CR->>checkObject(D). %we zijn dezelfde
        % is this necessary for definitions? AB
	% CR->>checkObject(D),!. %we zijn dezelfde
	% @model->>norm_relationDef(CR,D). %gp3: replaced code with a norm
%
checkChange_changeSketchDef(D,CR):->
	%2: we zijn niet het object

	NewName = CR<<-argument(1),
	NewName->>equalExportName(D?name),
	CR->>impossible('The given name is already used',D).
%
applyChange_changeSketchDef(D,
			      CR: changeRequestor
			     ):->
	%Deze definitie verandert dus
	
	D->>name(?(CR,argument,1)),
	D->>remarks(?(CR,argument,2)),
	D->>def_sheet(?(CR,argument,3)).
%%


:-pce_end_class.
		  
