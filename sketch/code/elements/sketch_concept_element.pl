/*
Definitie sketchConceptElement class
*/

:-pce_begin_class(
		  sketchConceptElement(name),
		  sketchElement,
		  "sketchConceptElement definition"
		 ).

variable(name,name,get,"Name of the sketchConceptElement").

%%
initialise(GI, Name: name = name, Remarks : remarks = string, Sketch: sketch) :->
	GI->+initialise(Remarks),
	GI->>name(Name),
        
        if Sketch = @nil
        then 
        (
           debug(sketch(test), 'Sketch input = @nil for sketchConceptElement: ~w',[Name]), 
           get(@model, hypered, conceptMapMF, Sketch1),   % AB, nov 2006
           send(GI, hyper(Sketch1, sketch, sketchConceptElement))
        )
        else 
           send(GI, hyper(Sketch, sketch, sketchConceptElement)).  % AB, april 2006 
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
	GE->>hyper(E,subelement,sketchConceptElement).
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
copyToNewElement(FE, _Mappings: hash_table, NewSK: sketch, New: sketchConceptElement):<-
	% Copy contents and relations (hypers) to a new sketchConceptElement
	% see sketchElement<<-copyToNewElement for details
	
	% with sketchConceptElement, no tests are needed to check if the mappings are already complete enough
	
	% everything checked, we can create the item
	New = NewSK<<-addNewFConcept(FE?entity,FE?name,FE?remarks).
%%

:-pce_end_class.
		  
