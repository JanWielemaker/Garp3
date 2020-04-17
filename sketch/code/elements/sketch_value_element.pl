/*
Definitie sketchValueElement class
*/

:-pce_begin_class(
		  sketchValueElement(name),
		  sketchElement,
		  "garp sketchValueElement definition"
		 ).

variable(name,name,get,"Name of the sketchValueElement").



%%
initialise(GI, Name: name = name, Remarks : remarks = string, Sketch: sketch) :->
	GI->+initialise(Remarks),
	GI->>name(Name),
        % get(@model, hypered, stgraphMF, Sketch),   
        send(GI, hyper(Sketch, sketch, sketchValueElement)).% AB, april 2006 
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
	"Return top level sketchValueElement = sketchValueElement" ::
        % there is no topSketchValue, but probably this whole thing is not necessary
	E = @model<<-hypered(topSketchQuantity).
%%



%%
definition(GI, E: abstractEntity):<-
	%gp3 0.1: same as ?entity, exists also in assumptionSketchValue
	E = GI<<-entity.
%%
	
%%
values(GE,
	   Values : chain
	  ):<-
	"Return all values defined with this sketchValueElement" ::

	Values = GE<<-subelements(garpValue).
%%


%%
subelements(GE,
	    Class : [class],
	    Subelements : chain
	   ):<-
	"Return all subelements defined with this sketchValueElement, or all from the given class" ::

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
	%gp3 0.1: changed call from 'garpAttribute|value' to any sketchElement
	%now this is garpAttribute or value or assumptionSketchValue
	GE<<-find_hyper(subelement,@arg3 == E)
	;
	GE->>hyper(E,subelement,sketchValueElement).
%%

%%
removeSubelement(GE,
		 E : sketchElement %gp3 0.1: changed type to general (see addSubelement)
		):->
	%verwijdert de hyper die het element aan deze sketchValueElement relateert
	%(bevrijdt het element niet)

	H = GE<<-find_hyper(subelement,
			    @arg3 == E),
	free(H).
%%
	
%%
copyToNewElement(FE,_Mappings: hash_table, NewSK: sketch, New: sketchValueElement):<-
	%gp3 0.2 Copy contents and relations (hypers) to a new assumptionSketchValue
	%see sketchElement<<-copyToNewElement for details
	%we fail if required connected objects are missing from the Mappings table
	
	%with sketchValueElement, no tests are needed to check if the mappings are allready complete enough
	
	%everything checked, we can create the item
        debug(sketch(save), 'copyToNewElement for sketch_value_element', []), 
	% New = NewSK<<-addNewSketchValue(FE?entity,FE?stateName,FE?name,FE?remarks).
	New = NewSK<<-addNewSketchValue(FE?name,FE?remarks).
%%

:-pce_end_class.
		  
