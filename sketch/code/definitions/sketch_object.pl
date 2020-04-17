/*
Definition of class sketchObject
*/

:-pce_begin_class(
		  sketchObject(name),
		  abstractEntity,
		  "SketchObject definition"
		 ).



%%
addSketchObject(AE,
	    I : sketchObjectElement
	   ):->
	%maak de hyper die aangeeft dat het een instantie van deze is

	AE<<-find_hyper(sketchObjectElement, @arg3 == I)
	;
	AE->>hyper(I,sketchObjectElement,abstractEntity).
%%

%%
removeSketchObject(AE,
	       I : sketchObjectElement
	      ):->
	%verwijder de hyper die aangeeft dat het een instantie van deze is

	H = AE<<-find_hyper(sketchObjectElement,
			    @arg3 == I),
	free(H).
%%

%%
sketchObjects(AE,SketchObjects: chain) :<-
	"Return chain of all sketchObjects" ::
	SketchObjects = AE<<-all_named_hypered(sketchObjectElement).
%%


:-pce_end_class.
