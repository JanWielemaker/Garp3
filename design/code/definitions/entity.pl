/*
Definitie entity class
*/

:-pce_begin_class(
		  entity(name),
		  abstractEntity,
		  "Entity definition"
		 ).

%%
%typeString: gp3 1.4 helper for relevantComments, this one is abstract
typeString(_HO, TS: name):<-
	TS = 'EnH'.
%%
:-pce_end_class.
