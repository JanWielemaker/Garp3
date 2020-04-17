/*
Definitie sketchObjectElement class
*/

:-pce_begin_class(
		  sketchObjectElement(name),
		  sketchElement,
		  "sketch Object definition"
		 ).

variable(name,name,get,"Name of the sketchObjectElement").
variable(type,{undefined,entity,agent,assumption},both,"Type of the Sketch object").


%%
initialise(GI, Name: name = name, Type: {undefined,entity,agent,assumption}, Remarks : remarks = string, Sketch: sketch) :->
	GI->+initialise(Remarks),
	GI->>name(Name),
        GI->>slot(type,Type), 
        % GI->>slot(type,undefined), 
        % get(@model, hypered, structureMF, Sketch),   % AB, april 2006 
        send(GI, hyper(Sketch, sketch, sketchObjectElement)).  % AB, april 2006 
%%


%%
name(GE,
     Name : name = name
    ):->
	"Set a garpified version of the given name as name" ::

	GE->>slot(name,Name?makeGarp).
%%


%%
entity(GE, E: abstractEntity):<-
	"Return the entity this concept belongs to" ::

	E = GE<<-hypered(abstractEntity).
%%

%%
definition(GI, E: abstractEntity):<-
	%gp3 0.1: same as ?entity, exists also in assumptionConcept
	E = GI<<-entity.
%%
	
%%
quantities(GE,
	   Quantities : chain
	  ):<-
	"Return all quantities defined with this concept" ::

	Quantities = GE<<-subelements(garpQuantity).
%%

%%
attributes(GE,
	   Attributes : chain
	  ):<-
	"Return all garpAttributes defined with this concept" ::

	Attributes = GE<<-subelements(garpAttribute).
%%

%%
subelements(GE,
	    Class : [class],
	    Subelements : chain
	   ):<-
	"Return all subelements defined with this concept, or all from the given class" ::

	All = GE<<-all_named_hypered(subelement),
	default(Class,class(object),C),
	Subelements *= chain,
	All->>for_all(if(->>(@arg1,
			     instance_of,
			     C),
			 ->>(Subelements,
			   add,
			     @arg1))).
%%

%%
addSubelement(GE,
	     E: sketchElement
	    ):->
	%maak de hyper die aangeeft dat het element hierbij hoort
	%gp3 0.1: changed call from 'garpAttribute|quantity' to any sketchElement
	%now this is garpAttribute or quantity or assumptionConcept
	GE<<-find_hyper(subelement,@arg3 == E)
	;
	GE->>hyper(E,subelement,sketchObjectElement).
%%

%%
removeSubelement(GE,
		 E : sketchElement %gp3 0.1: changed type to general (see addSubelement)
		):->
	%verwijdert de hyper die het element aan deze concept relateert
	%(bevrijdt het element niet)

	H = GE<<-find_hyper(subelement,
			    @arg3 == E),
	free(H).
%%
	
%%
copyToNewElement(FE,_Mappings: hash_table, NewSK: sketch, New: sketchObjectElement):<-
	%gp3 0.2 Copy contents and relations (hypers) to a new assumptionConcept
	%see sketchElement<<-copyToNewElement for details
	%we fail if required connected objects are missing from the Mappings table
	
	%with sketchObjectElement, no tests are needed to check if the mappings are allready complete enough
	
	%everything checked, we can create the item
	New = NewSK<<-addNewFSketchObject(FE?entity, FE?name, FE?type, FE?remarks).
%%s

:-pce_end_class.
		  
