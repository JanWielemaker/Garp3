/*
Definitie sketchDefinitionElement class
*/

:-pce_begin_class(
		  sketchDefinitionElement(name),
		  sketchElement,
		  "sketch Definition Element definition"
		 ).

variable(name,name,get,"Name of the sketchDefinitionElement").
variable(type,{entity,agent,assumption,undefined,quantity},both,"Type of the Sketch definition element").
variable(def,'sketchDefinition|constant', both,"the sketchDefinition that this element belongs to").


%%
initialise(GI, Name: name = name, Type: {entity,agent,assumption,quantity,undefined}, Remarks : remarks = string, Def) :->
        % Def: sketchDefinition, or still @nil
	GI->+initialise(Remarks),
	GI->>name(Name),
        GI->>slot(type,Type), 
        GI->>slot(def,Def). 
%%


%%
name(GE,
     Name : name = name
    ):->
	"Set a garpified version of the given name as name" ::

	GE->>slot(name,Name?makeGarp).
%%

	

:-pce_end_class.
		  
