/*
Definitie agent class
*/

:-pce_begin_class(
		  agent(name),
		  abstractEntity,
		  "Agent definition"
		 ).

%%
%typeString: gp3 1.4 helper for relevantComments, this one is abstract
typeString(_HO, TS: name):<-
	TS = 'AgH'.
%%
:-pce_end_class.
