/*
Definitie sketchScenarioDefinition class
*/

:-pce_begin_class(
		  sketchScenarioDefinition(name),
		  sketchDefinition, 
		  "sketch Scenario definition"
		 ).

variable(name,name,get,"Name of the sketch scenario definition").

variable(def_sheet, sheet, both, "Sheet with data for scenario definition").
variable(remarks,string,none,"Remarks on this sketch scenario definition").


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
                     attribute(agents, ''),
                     attribute(quantities, ''),
                     attribute(initial_values_and_inequality_statements, ''),
                     attribute(assumptions, '')
        )),
	D->>slot(def_sheet,S).
%%


%%%%%%%%%%%%%CHANGES%%%%%%%%%%%%%%
%%

%%
checkChange_addSketchScenarioDef(D,
	CR:changeRequestor):->
	%checken op naamconflict

	NewName = CR<<-argument(1),
	NewName->>equalExportName(D?name),
	CR->>impossible('The given name is already used', D).
%%

%
applyChange_addSketchScenarioDef(_D,
			      _CR: changeRequestor
			     ):->
        % this does not seem to be called
        debug(sketch(test), 'applyChange_addSketchScenarioDef\n',[]).
%%


%%
checkChange_changeSketchScenarioDef(D,
			    CR : changeRequestor
			   ):->
	%als we zelf het object van de change zijn checken we of
	%de naam geldig is
	%als we niet zelf het object vd change zijn checken we op
	%naamconflict..

	%1: we zijn het object

	CR->>checkObject(D). %we zijn dezelfde
        % is this necessary for scenario definitions? AB
	% CR->>checkObject(D),!. %we zijn dezelfde
	% @model->>norm_relationDef(CR,D). %gp3: replaced code with a norm
%
checkChange_changeSketchScenarioDef(D,CR):->
	%2: we zijn niet het object

	NewName = CR<<-argument(1),
	NewName->>equalExportName(D?name),
	CR->>impossible('The given name is already used',D).
%
applyChange_changeSketchScenarioDef(D,
			      CR: changeRequestor
			     ):->
	%Deze definitie verandert dus
	
	D->>name(?(CR,argument,1)),
	D->>remarks(?(CR,argument,2)),
	D->>def_sheet(?(CR,argument,3)).
%%


:-pce_end_class.
		  
