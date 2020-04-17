/*
Definition of class sketchConcept
*/

:-pce_begin_class(
		  sketchConcept(name),
		  abstractEntity,
		  "SketchConcept definition"
		 ).



%%
addConcept(AE,
	    I : sketchConceptElement
	   ):->
	%maak de hyper die aangeeft dat het een instantie van deze is

	AE<<-find_hyper(sketchConceptElement, @arg3 == I)
	;
	AE->>hyper(I,sketchConceptElement,abstractEntity).
%%

%%
removeConcept(AE,
	       I : sketchConceptElement
	      ):->
	%verwijder de hyper die aangeeft dat het een instantie van deze is

	H = AE<<-find_hyper(sketchConceptElement,
			    @arg3 == I),
	free(H).
%%

%%
sketchConcepts(AE,Concepts: chain) :<-
	"Return chain of all sketchConcepts" ::
	Concepts = AE<<-all_named_hypered(sketchConceptElement).
%%


:-pce_end_class.
