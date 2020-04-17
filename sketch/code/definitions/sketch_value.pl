/*
Definition of class sketchValue
*/

:-pce_begin_class(
		  sketchValue(name),
		  abstractEntity,
		  "SketchValue definition"
		 ).



%%
addSketchValue(AE,
	    I : sketchValueElement
	   ):->
	%maak de hyper die aangeeft dat het een instantie van deze is

	AE<<-find_hyper(sketchValueElement, @arg3 == I)
	;
	AE->>hyper(I,sketchValueElement,abstractEntity).
%%

%%
removeSketchValue(AE,
	       I : sketchValueElement
	      ):->
	%verwijder de hyper die aangeeft dat het een instantie van deze is

	H = AE<<-find_hyper(sketchValueElement,
			    @arg3 == I),
	free(H).
%%

%%
sketchValues(AE,SketchValues: chain) :<-
	"Return chain of all sketchValues" ::
	SketchValues = AE<<-all_named_hypered(sketchValueElement).
%%


:-pce_end_class.
