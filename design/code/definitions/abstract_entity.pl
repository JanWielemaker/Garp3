/*
Definitie AbstractEntity class
*/

%Abstracte class.

:-pce_begin_class(
		  abstractEntity,
		  hierarchicalObject,
		  "Abstract parent of entity-like classes (entity and agent)"
		 ).


%%
addInstance(AE,
	    I : garpInstance
	   ):->
	%maak de hyper die aangeeft dat het een instantie van deze is

	AE<<-find_hyper(garpInstance, @arg3 == I)
	;
	AE->>hyper(I,garpInstance,abstractEntity).
%%

%%
removeInstance(AE,
	       I : garpInstance
	      ):->
	%verwijder de hyper die aangeeft dat het een instantie van deze is

	H = AE<<-find_hyper(garpInstance,
			    @arg3 == I),
	free(H).
%%

%%
instances(AE,Instances: chain) :<-
	"Return chain of all instances" ::
	Instances = AE<<-all_named_hypered(garpInstance).
%%

/***********change requestors*****************/
%alles naar hierarchicalObject
:-pce_end_class.
		  
