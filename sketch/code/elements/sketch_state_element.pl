/*
Definitie sketchStateElement class
*/

:-pce_begin_class(
		  sketchStateElement(name),
		  sketchElement,
		  "garp sketchStateElement definition"
		 ).

variable(name,name,get,"Name of the sketchStateElement").
variable(sketchInequalities, chain, both, "sketchInequalities in this state").



%%
initialise(GI, Name: name = name, Remarks : remarks = string, Sketch: sketch) :->
	GI->+initialise(Remarks),
	GI->>name(Name),
        new(Chain, chain), 
	GI->>sketchInequalities(Chain),
        % get(@model, hypered, stgraphMF, Sketch),   % AB, april 2006 
        send(GI, hyper(Sketch, sketch, sketchStateElement)).
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
	"Return the entity this sketchStateElement belongs to" ::

	E = GE<<-hypered(abstractEntity).
%%

%%
definition(GI, E: abstractEntity):<-
	%gp3 0.1: same as ?entity, exists also in assumptionSketchState
	E = GI<<-entity.
%%
	
%%
states(GE,
	   States : chain
	  ):<-
	"Return all states defined with this sketchStateElement" ::

	States = GE<<-subelements(garpState).
%%

%%
attributes(GE,
	   Attributes : chain
	  ):<-
	"Return all garpAttributes defined with this sketchStateElement" ::

	Attributes = GE<<-subelements(garpAttribute).
%%

%%
subelements(GE,
	    Class : [class],
	    Subelements : chain
	   ):<-
	"Return all subelements defined with this sketchStateElement, or all from the given class" ::

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
	%gp3 0.1: changed call from 'garpAttribute|state' to any sketchElement
	%now this is garpAttribute or state or assumptionSketchState
	GE<<-find_hyper(subelement,@arg3 == E)
	;
	GE->>hyper(E,subelement,sketchStateElement).
%%

%%
removeSubelement(GE,
		 E : sketchElement %gp3 0.1: changed type to general (see addSubelement)
		):->
	%verwijdert de hyper die het element aan deze sketchStateElement relateert
	%(bevrijdt het element niet)
        % writef('removeSubelement in sketch_state_element.pl \n',[]), 

	H = GE<<-find_hyper(subelement,
			    @arg3 == E),
	free(H).
%%
	


%%
copyToNewElement(FE,_Mappings: hash_table, NewSK: sketch, New: sketchStateElement):<-
	%gp3 0.2 Copy contents and relations (hypers) to a new assumptionSketchState
	%see sketchElement<<-copyToNewElement for details
	%we fail if required connected objects are missing from the Mappings table
	
	%with sketchStateElement, no tests are needed to check if the mappings are already complete enough
     get(FE, name, StateName), 
     debug(sketch(save), 'copyToNewElement sketch state: ~w\n',[StateName]), 
	%everything checked, we can create the item
        % Create copies of sketchStateInequalities that refer to the right quantities and values (in NewSK)
        get(NewSK, copySketchStateInequalities, FE?sortedSketchStateInequalities, NewIneqs), 
    
	New = NewSK<<-addNewSketchState(FE?entity, FE?name,FE?remarks?copy, NewIneqs),
        debug(sketch(copy), 'copyToNewElement done.%d\n',[]).    
%%


% copy(FE, NewSK: sketch, StateName: name, New: sketchStateElement)
% 
% create New in NewSK as a copy of FE with new name (StateName)
%%
copy(FE, NewSK: sketch, StateName: name, New: sketchStateElement):<-
	% simplified version, with a name as input, and without reference to Mappings, AB, june 2006

        debug(sketch(copy), 'copy sketch state.%d\n',[]),
        % Create copies of sketchStateInequalities that refer to the right quantities and values (in NewSK)
        get(NewSK, copySketchStateInequalities, FE?sortedSketchStateInequalities, NewIneqs), 

	New = NewSK<<-addNewSketchState(FE?entity, StateName, FE?remarks?copy, NewIneqs),
        debug(sketch(copy), 'copy done.%d\n',[]).    
%%

%
updateSketchIneqList(S, Arg: sketchElement):->        
        get(S, sortedSketchStateInequalities, Ineqs), 
	Ineqs->>for_all(->>(@arg1, updateSketchIneqDisplayName, Arg)).
%%


%%
sortedSketchStateInequalities(S, Ineqs: chain):<-
        get(S, sketchInequalities, Ineqs),        
	if 
		@nil = Ineqs
	then
                % do nothing
		send(Ineqs, clear)
	else
        (
                % normal sort is necessary in case multiple inequalities with same displayNameString occur
                Ineqs->>sort,
                Ineqs->>sort(?(@arg1?displayNameString, compare, @arg2?displayNameString))
	).
%

:-pce_end_class.
		  
