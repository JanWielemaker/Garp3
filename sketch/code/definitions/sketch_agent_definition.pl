/*
Definitie sketchAgentDefinition class
*/

:-pce_begin_class(
		  sketchAgentDefinition(name),
		  sketchDefinition, 
		  "sketch Agent definition"
		 ).

variable(name,name,get,"Name of the sketch agent definition").

variable(def_sheet, sheet, both, "Sheet with data for agent definition").
variable(remarks,string,none,"Remarks on this sketch agent definition").


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
                     attribute(agent, ''),
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
checkChange_addSketchAgentDef(D,
	CR:changeRequestor):->
	%checken op naamconflict

	NewName = CR<<-argument(1),
	NewName->>equalExportName(D?name),
	CR->>impossible('The given name is already used', D).
%%

%
applyChange_addSketchAgentDef(_D,
			      _CR: changeRequestor
			     ):->
        % this does not seem to be called
        debug(sketch(test), 'applyChange_addSketchAgentDef',[]).
%%


%%
checkChange_changeSketchAgentDef(D,
			    CR : changeRequestor
			   ):->
	%als we zelf het object van de change zijn checken we of
	%de naam geldig is
	%als we niet zelf het object vd change zijn checken we op
	%naamconflict..

	%1: we zijn het object

	CR->>checkObject(D). %we zijn dezelfde
        % is this necessary for agent definitions? AB
	% CR->>checkObject(D),!. %we zijn dezelfde
	% @model->>norm_relationDef(CR,D). %gp3: replaced code with a norm
%
checkChange_changeSketchAgentDef(D,CR):->
	%2: we zijn niet het object

	NewName = CR<<-argument(1),
	NewName->>equalExportName(D?name),
	CR->>impossible('The given name is already used',D).
%
applyChange_changeSketchAgentDef(D,
			      CR: changeRequestor
			     ):->
	%Deze definitie verandert dus
	
	D->>name(?(CR,argument,1)),
	D->>remarks(?(CR,argument,2)),
	D->>def_sheet(?(CR,argument,3)).
%%


:-pce_end_class.
		  
