/*
Definitie Assumption class
*/

:-pce_begin_class(
		  assumption(name),
		  hierarchicalObject,
		  "Assumption definition"
		 ).

%%
addInstance(A,
	    I : assumptionInstance
	   ):->
	%maak de hyper die aangeeft dat het een instantie van deze is

	A<<-find_hyper(assumptionInstance, @arg3 == I)
	;
	A->>hyper(I,assumptionInstance,assumption).
%%

%%
removeInstance(A,
	       I : assumptionInstance
	      ):->
	%verwijder de hyper die aangeeft dat het een instantie van deze is

	H = A<<-find_hyper(assumptionInstance,
			    @arg3 == I),
	free(H).
%%

%%
instances(A,Instances: chain) :<-
	"Return chain of all instances" ::
	Instances = A<<-all_named_hypered(assumptionInstance).
%%

%%
%typeString: gp3 1.4 helper for relevantComments, this one is abstract
typeString(_HO, TS: name):<-
	TS = 'AsH'.
%%
:-pce_end_class.
		  
