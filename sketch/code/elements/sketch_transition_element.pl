/*
Definitie sketchTransitionElement class
*/

:-pce_begin_class(
		  sketchTransitionElement,
		  sketchGenericRelationElement,
		  "definition of transition relation in state-transition graph sketch"
		 ).


%%
initialise(PR, 
			Arg1: sketchStateElement, Arg1Route: chain,
			Arg2: sketchStateElement, Arg2Route: chain,
			Remarks: string, Sketch: sketch) :->
	PR->+initialise(Arg1, Arg1Route, Arg2, Arg2Route, Remarks),
        % get(@model, hypered, stgraphMF, Sketch),   % AB, april 2006 
        send(PR, hyper(Sketch, sketch, sketchTransitionElement)).% AB, april 2006 
%%

%%
displayTypeName(_IS, Type: name):<-
	Type = 'sketch transition'.
%%


%%
name(IS, Name: name):<-
        get(IS?argument1, name, Arg1Name), 
        get(IS?argument2, name, Arg2Name), 
	swritef(Name, 'sketch transition %d->%d', [Arg1Name, Arg2Name]).
%%

%%
checkDeleteElement(QR,
	Element: sketchElement,
	CR: changeRequestor):->
	%overwrite van sketchElement: check onze argumenten
	if
		(
			Element = QR<<-argument1
		;
			QR?argument1Route->>member(Element)
		;
			Element = QR<<-argument2
		;
			QR?argument2Route->>member(Element)
		)
	then
               (
		CR->>addChangeRequest(changeRequestor(deleteSketchTransition,
								QR?sketch,
								@default,
								CR?garpModel,
								QR),
			string('A %s in this %s will also be deleted because one of its arguments is no longer part of the behaviour graph',QR?displayTypeName, QR?sketch?displayTypeName, QR?sketch?displayTypeName),
			QR?sketch)
		).
%%

%%
%%
copyToNewElement(FE,Mappings: hash_table, NewSK: sketch, New: sketchTransitionElement):<-
	%gp3 0.2 Copy contents and relations (hypers) to a new assumptionInstance
	%see sketchElement<<-copyToNewElement for details
	%we fail if required connected objects are missing from the Mappings table
	
	%lets see if the needed other elements are allready copied
	
	NewArg1Info = NewSK<<-copySK_mapRelatedElement(FE?argument1,FE?argument1Route,Mappings),
	NewArg2Info = NewSK<<-copySK_mapRelatedElement(FE?argument2,FE?argument2Route,Mappings),
		%fails when these ones are not available yet, so will try again later
		
	%everything checked, we can create the item
        debug(sketch(save), 'copyToNewElement sketch transition element: ~w\n',[FE]),
	New = NewSK<<-addNewSketchTransition(NewArg1Info?first, NewArg1Info?second, 
					     NewArg2Info?first, NewArg2Info?second,
					     FE?remarks).
%%
:-pce_end_class.
		  
