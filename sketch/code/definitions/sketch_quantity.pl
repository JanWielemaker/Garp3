/*
Definition of class sketchQuantity
*/

:-pce_begin_class(
		  sketchQuantity(name),
		  abstractEntity,  
		  "SketchQuantity definition" 
		 ).



%%
addSketchQuantity(AE,
	    I : sketchQuantityElement
	   ):->
	%maak de hyper die aangeeft dat het een instantie van deze is

        % writef('addSketchQuantity in sketch_quantity.pl\n',[]), 
	AE<<-find_hyper(sketchQuantityElement, @arg3 == I)
	;
	AE->>hyper(I,sketchQuantityElement,abstractEntity).
%%

%%
removeSketchQuantity(AE,
	       I : sketchQuantityElement
	      ):->
	%verwijder de hyper die aangeeft dat het een instantie van deze is

        % writef('removeSketchQuantity in sketch_quantity.pl\n',[]), 

	H = AE<<-find_hyper(sketchQuantityElement,
			    @arg3 == I),
	free(H).
%%

%%
sketchQuantities(AE,SketchQuantities: chain) :<-
	"Return chain of all sketchQuantities" ::
        % writef('sketchQuantities in sketch_quantity.pl\n',[]), 
	SketchQuantities = AE<<-all_named_hypered(sketchQuantityElement).
%%


:-pce_end_class.
