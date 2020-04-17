/*
Definitie sketchQuantityElement class
*/

:-pce_begin_class(
		  sketchQuantityElement(name),
		  sketchElement,
		  "garp sketchQuantityElement definition"
		 ).

variable(name,name,get,"Name of the sketchQuantityElement").



%%
initialise(GI, Name: name = name, Remarks : remarks = string, Sketch: sketch) :->
    % writef('initialise in sketch_quantity_element.pl\n',[]), 
	GI->+initialise(Remarks),
	GI->>name(Name),
        send(GI, hyper(Sketch, sketch, sketchQuantityElement)).% AB, april 2006 
%%



%%
name(GE,
     Name : name = name
    ):->
	"Set a garpified version of the given name as name" ::

	GE->>slot(name,Name?makeGarp).
%%

%%
entity(_D,
       E : 'abstractEntity*'
      ):<-
	"Return top level sketchQuantityElement = sketchQuantityElement" ::
	E = @model<<-hypered(topSketchQuantity).
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
	"Return all quantities defined with this sketchQuantityElement" ::

	Quantities = GE<<-subelements(garpQuantity).
%%

%%
attributes(GE,
	   Attributes : chain
	  ):<-
	"Return all garpAttributes defined with this sketchQuantityElement" ::

	Attributes = GE<<-subelements(garpAttribute).
%%

%%
subelements(GE,
	    Class : [class],
	    Subelements : chain
	   ):<-
	"Return all subelements defined with this sketchQuantityElement, or all from the given class" ::

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
	GE->>hyper(E,subelement,sketchQuantityElement).
%%

%%
removeSubelement(GE,
		 E : sketchElement %gp3 0.1: changed type to general (see addSubelement)
		):->
	%verwijdert de hyper die het element aan deze sketchQuantityElement relateert
	%(bevrijdt het element niet)

	H = GE<<-find_hyper(subelement,
			    @arg3 == E),
	free(H).
%%



%
%  version without (a functional) Mappings argument
copyToNewElement(FE, _Mappings: hash_table, SK: sketch, New: sketchQuantityElement):<-
	% Copy contents and relations (hypers) to a new sketchQuantityElement
	% see sketchElement<<-copyToNewElement for details	
	New = SK<<-addNewSketchQuantity(FE?name,FE?remarks).
	% New = SK<<-addNewSketchQuantity(FE?entity, FE?name,FE?remarks).
%%


/* Old versions - not sure which one works best yet, AB, dec 2006 / jan 2007

%%
copyToNewElement(FE, NewSK: sketch, New: sketchQuantityElement):<-
% copyToNewElement(FE, _Mappings: hash_table, NewSK: sketch, New: sketchQuantityElement):<-
	% Copy contents and relations (hypers) to a new sketchQuantityElement
	% see sketchElement<<-copyToNewElement for details	
	% with sketchConceptElement, no tests are needed to check if the mappings are already complete enough
	
	% everything checked, we can create the item
	New = NewSK<<-addNewSketchQuantity(FE?name,FE?remarks).
	% New = NewSK<<-addNewSketchQuantity(FE?entity,FE?name,FE?remarks).
%%
	
%%
copyToNewElement(FE,_Mappings: hash_table, NewSK: sketch, New: sketchQuantityElement):<-
	%gp3 0.2 Copy contents and relations (hypers) to a new assumptionSketchQuantity
	%see sketchElement<<-copyToNewElement for details
	%we fail if required connected objects are missing from the Mappings table
	
	%with sketchQuantityElement, no tests are needed to check if the mappings are allready complete enough
	
	%everything checked, we can create the item
	New = NewSK<<-addNewSketchQuantity(FE?name,FE?remarks).
	% New = NewSK<<-addNewSketchQuantity(FE?entity,FE?stateName,FE?name,FE?remarks).
	% New = NewSK<<-addNewSketchQuantity(FE?entity,FE?name,FE?remarks).
%%


%
%  version without the Sketch argument
copy(FE, New: sketchQuantityElement):<-
	%gp3 0.2 Copy contents and relations (hypers) to a new assumptionSketchQuantity
	%see sketchElement<<-copyToNewElement for details	
	%everything checked, we can create the item
	New = FE?sketch<<-addNewSketchQuantity(FE?name,FE?remarks).
	% New = FE?sketch<<-addNewSketchQuantity(FE?entity,FE?name,FE?remarks).
%%


*/


:-pce_end_class.
		  
