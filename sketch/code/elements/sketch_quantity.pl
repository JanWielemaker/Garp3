/*
Definitie sketchQuantity class
*/

:-pce_begin_class(
		  sketchQuantity(name),
		  sketchElement,
		  "garp sketchQuantity definition"
		 ).

variable(name,name,get,"Name of the sketchQuantity").

%%
initialise(GI, Name: name = name, Remarks : remarks = string) :->
        debug(sketch(test), 'initialise in sketch_quantity.pl in elements directory',[]), 
	GI->+initialise(Remarks),
	GI->>name(Name).
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
	"Return the entity this sketchQuantity belongs to" ::

	E = GE<<-hypered(abstractEntity).
%%

%%
definition(GI, E: abstractEntity):<-
	%gp3 0.1: same as ?entity, exists also in assumptionSketchQuantity
	E = GI<<-entity.
%%
	
%%
quantities(GE,
	   Quantities : chain
	  ):<-
	"Return all quantities defined with this sketchQuantity" ::

	Quantities = GE<<-subelements(garpQuantity).
%%

%%
attributes(GE,
	   Attributes : chain
	  ):<-
	"Return all garpAttributes defined with this sketchQuantity" ::

	Attributes = GE<<-subelements(garpAttribute).
%%

%%
subelements(GE,
	    Class : [class],
	    Subelements : chain
	   ):<-
	"Return all subelements defined with this sketchQuantity, or all from the given class" ::

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
	%now this is garpAttribute or quantity or assumptionSketchQuantity
	GE<<-find_hyper(subelement,@arg3 == E)
	;
	GE->>hyper(E,subelement,sketchQuantity).
%%

%%
removeSubelement(GE,
		 E : sketchElement %gp3 0.1: changed type to general (see addSubelement)
		):->
	%verwijdert de hyper die het element aan deze sketchQuantity relateert
	%(bevrijdt het element niet)

	H = GE<<-find_hyper(subelement,
			    @arg3 == E),
	free(H).
%%
	
%%
copyToNewElement(FE,_Mappings: hash_table, NewSK: sketch, New: sketchQuantity):<-
	%gp3 0.2 Copy contents and relations (hypers) to a new assumptionSketchQuantity
	%see sketchElement<<-copyToNewElement for details
	%we fail if required connected objects are missing from the Mappings table
	
	%with sketchQuantity, no tests are needed to check if the mappings are allready complete enough
	
	%everything checked, we can create the item
	New = NewSK<<-addNewSketchQuantity(FE?entity,FE?stateName,FE?name,FE?remarks).
%%

:-pce_end_class.
		  
