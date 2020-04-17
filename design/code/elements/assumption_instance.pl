/*
Definitie assumptionInstance class
GARP3: This is homer code, we just made changes (where gp3 mentioned)
*/

:-pce_begin_class(
		  assumptionInstance,
		  fragmentElement,
		  "garp assumption usage definition"
		 ).

%gp3 0.1: changed this object. It is now optionally a subelement of a garpinstance
%(through garpInstance->>addSubelement)
%so we also need the route to the instance
variable(instanceRoute, chain, none). %route through imported fragments to get to the instance

%%
initialise(AI, MF: modelFragment, Remarks: string):->
	%gp3 0.1 Make sure to set the instanceRoute
	
	AI->+initialise(MF, Remarks),
	AI->>instanceRoute(new(chain)).
%%

%%
assumption(AI, A: assumption):<-
	"Return the assumption this instance belongs to" ::

	A = AI<<-hypered(assumption).
%%

%%
name(AI,N: name):<-
	%gp3 0.1: return the name of this assumption instance, which is the name of the assumption
	N = AI?assumption<<-name.
%%

%%
definition(AI, A: assumption):<-
	%gp3 0.1: same as ?assumption, exists also in garpInstance
	A = AI<<-assumption.
%%
	
%%
garpInstance(A,
	 I : garpInstance*
	):<-
	%gp3. Return the associated instance or @nil if none
	(I = A<<-hypered(garpInstance), !)
	;
	I = @nil.
%%

%%
instanceRoute(A,
	R: chain):->
	%gp3: set instanceRoute. Only needed when this assumptioninst is connected to an instance

	A->>slot(instanceRoute,R?copy).
%
instanceRoute(A,
	R: chain):<-
	%gp3: return instanceRoute. Only relevant when this assumptioninst is connected to an instance

	R = ?(A,slot,instanceRoute)<<-copy.
%%

%%
checkDeleteElement(A,
	Element: fragmentElement,	
	CR: changeRequestor):->
	%gp3 0.1 Overwrite: We check whether we should go as well
	
	if
	(
		Element = A<<-garpInstance
	;
		A?instanceRoute->>member(Element)
	)
	then
		CR->>addChangeRequest(changeRequestor(deleteAssumptionInstance,
								A?fragment,
								@default,
								CR?garpModel,
								A),
			string('The assumption instance "%s" in %s "%s" will be deleted because the instance to which it is connected ("%s") is no longer part of the %s',A?name, 
				A?fragment?displayTypeName, A?fragment?name, A?garpInstance?name,
				A?fragment?displayTypeName),
			A?fragment).
%%



%%
copyToNewElement(FE,Mappings: hash_table, NewMF: modelFragment, New: assumptionInstance):<-
	%gp3 0.2 Copy contents and relations (hypers) to a new assumptionInstance
	%see fragmentElement<<-copyToNewElement for details
	%we fail if required connected objects are missing from the Mappings table
	
	%lets see if the needed other elements are allready copied
	
	Instance = FE<<-garpInstance,
	if
		Instance \== @nil
	then
	(
		NewRef = NewMF<<-copyMF_mapRelatedElement(Instance,FE?instanceRoute,Mappings), %fails when this one is not available yet, so will try again later. Receiver can be any MF
		NewInstance = NewRef<<-first,
		NewRoute = NewRef<<-second
	)
	else
	(
		NewInstance = @nil,
		NewRoute = @nil
	),
	%everything checked, we can create the item
	New = NewMF<<-addNewAssumptionInstance(FE?assumption,FE?remarks,NewInstance,NewRoute).
%%


:-pce_end_class.
		  
