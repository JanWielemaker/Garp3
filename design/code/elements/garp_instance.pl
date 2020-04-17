/*
Definitie garpInstance class
*/

:-pce_begin_class(
		  garpInstance(name),
		  fragmentElement,
		  "garp instance definition"
		 ).

variable(name_translatable,translatable,both,"Name of the object"). %gp3 1.4: added this, name is just a get/set method now

%%
initialise(GI, MF: modelFragment, Name: name = name, Remarks : remarks = string) :->
	GI->+initialise(MF, Remarks),
	GI->>name_translatable(?(@model?translator,getTranslatable,unique)),
	GI->>name(Name).
%%

%%%%% wrapping translatables %%%%%%%%
%%
name(MF,Name : name):->
	"Set the name, internal" ::

	%is now a wrapper around name_translatable
	MF?name_translatable->>value(Name?makeGarp).
%
name(MF, Name: name):<-
	%get the name
	Name = MF?name_translatable<<-value.
%%


%%%%%%%%%%%%%%
%%
entity(GE, E: abstractEntity):<-
	"Return the entity this instance belongs to" ::

	E = GE<<-hypered(abstractEntity).
%%

%%
definition(GI, E: abstractEntity):<-
	%gp3 0.1: same as ?entity, exists also in assumptionInstance
	E = GI<<-entity.
%%
	
%%
quantities(GE,
	   Quantities : chain
	  ):<-
	"Return all quantities defined with this instance" ::

	Quantities = GE<<-subelements(garpQuantity).
%%

%%
attributes(GE,
	   Attributes : chain
	  ):<-
	"Return all garpAttributes defined with this instance" ::

	Attributes = GE<<-subelements(garpAttribute).
%%

%%
subelements(GE,
	    Class : [class],
	    Subelements : chain
	   ):<-
	"Return all subelements defined with this instance, or all from the given class" ::

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
	     E: fragmentElement
	    ):->
	%maak de hyper die aangeeft dat het element hierbij hoort
	%gp3 0.1: changed call from 'garpAttribute|quantity' to any fragmentElement
	%now this is garpAttribute or quantity or assumptionInstance
	GE<<-find_hyper(subelement,@arg3 == E)
	;
	GE->>hyper(E,subelement,garpInstance).
%%

%%
removeSubelement(GE,
		 E : fragmentElement %gp3 0.1: changed type to general (see addSubelement)
		):->
	%verwijdert de hyper die het element aan deze instance relateert
	%(bevrijdt het element niet)

	H = GE<<-find_hyper(subelement,
			    @arg3 == E),
	free(H).
%%
	
%%
copyToNewElement(FE,_Mappings: hash_table, NewMF: modelFragment, New: garpInstance):<-
	%gp3 0.2 Copy contents and relations (hypers) to a new assumptionInstance
	%see fragmentElement<<-copyToNewElement for details
	%we fail if required connected objects are missing from the Mappings table
	
	%with garpInstance, no tests are needed to check if the mappings are allready complete enough
	
	%everything checked, we can create the item
	New = NewMF<<-addNewFInstance(FE?entity,FE?stateName,FE?name,FE?remarks).
%%

:-pce_end_class.
		  
