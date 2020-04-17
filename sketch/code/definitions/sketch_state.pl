/*
Definition of class sketchState
*/

:-pce_begin_class(
		  sketchState(name),
		  abstractEntity,
		  "SketchState definition"
		 ).



%%
addSketchState(AE,
	    I : sketchStateElement
	   ):->
	%maak de hyper die aangeeft dat het een instantie van deze is
	AE<<-find_hyper(sketchStateElement, @arg3 == I)
	;
	AE->>hyper(I,sketchStateElement,abstractEntity).
%%

%%
removeSketchState(AE,
	       I : sketchStateElement
	      ):->
	%verwijder de hyper die aangeeft dat het een instantie van deze is

	H = AE<<-find_hyper(sketchStateElement,
			    @arg3 == I),
	free(H).
%%

%%
sketchStates(AE,SketchStates: chain) :<-
	"Return chain of all sketchStates" ::
	SketchStates = AE<<-all_named_hypered(sketchStateElement).
%%


:-pce_end_class.
