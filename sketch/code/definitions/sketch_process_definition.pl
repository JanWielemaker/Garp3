/*
Definitie sketchProcessDefinition class
*/

:-pce_begin_class(
		  sketchProcessDefinition(name),
		  sketchDefinition, 
		  "sketch Process definition"
		 ).

variable(name,name,get,"Name of the sketch process definition").

variable(def_sheet, sheet, both, "Sheet with data for process definition").
variable(remarks,string,none,"Remarks on this sketch process definition").


initialise(D,
	Name : name,
	Remarks : [string]
	):->
	D->+initialise(Name, Remarks).
%%


%%
init_def_sheet(D):->
        new(S, sheet(
                     attribute(entities, ''),
                     attribute(quantities, ''),
                     attribute(start_conditions, ''),
                     attribute(effects, ''),
                     attribute(stop_conditions, ''),
                     attribute(assumptions, '')
        )),
	D->>slot(def_sheet,S).
%%


%%%%%%%%%%%%%CHANGES%%%%%%%%%%%%%%
%%

%%
checkChange_addSketchProcessDef(D,
	CR:changeRequestor):->
	%checken op naamconflict

	NewName = CR<<-argument(1),
	NewName->>equalExportName(D?name),
	CR->>impossible('The given name is already used', D).
%%

%
applyChange_addSketchProcessDef(_D,
			      _CR: changeRequestor
			     ):->
        % this does not seem to be called
        debug(sketch(test), 'applyChange_addSketchProcessDef',[]).
%%


%%
checkChange_changeSketchProcessDef(D,
			    CR : changeRequestor
			   ):->
	%als we zelf het object van de change zijn checken we of
	%de naam geldig is
	%als we niet zelf het object vd change zijn checken we op
	%naamconflict..

	%1: we zijn het object

	CR->>checkObject(D). %we zijn dezelfde
        % is this necessary for process definitions? AB
	% CR->>checkObject(D),!. %we zijn dezelfde
	% @model->>norm_relationDef(CR,D). %gp3: replaced code with a norm
%
checkChange_changeSketchProcessDef(D,CR):->
	%2: we zijn niet het object

	NewName = CR<<-argument(1),
	NewName->>equalExportName(D?name),
	CR->>impossible('The given name is already used',D).
%
applyChange_changeSketchProcessDef(D,
			      CR: changeRequestor
			     ):->
	%Deze definitie verandert dus
	
	D->>name(?(CR,argument,1)),
	D->>remarks(?(CR,argument,2)),
	D->>def_sheet(?(CR,argument,3)).
%%


:-pce_end_class.
		  
