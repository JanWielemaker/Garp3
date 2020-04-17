/*
Definitie sketchDependencyElement class
*/

:-pce_begin_class(
		  sketchDependencyElement,
		  sketchGenericRelationElement,
		  "definition of quantity proportionality or influence relation in sketches"
		 ).

variable(type,{prop,inf,dep},both,"Type of the Sketch dependency").
% variable(sign,{min,plus,unknown},both,"Sign of the Sketch dependency").
variable(sign,{dep_plus, dep_min, inf_plus, inf_min, prop_plus, prop_min},both,"Sign of the Sketch dependency").

%%
initialise(PR, 	Sign: {dep_plus, dep_min, inf_plus, inf_min, prop_plus, prop_min}, Type: {prop,inf,dep},
% Sign: {min,plus,unknown}
			Arg1: sketchQuantityElement, Arg1Route: chain,
			Arg2: sketchQuantityElement, Arg2Route: chain,
			Remarks: string, 
	                Sketch: sketch) :->
	PR->+initialise(Arg1, Arg1Route, Arg2, Arg2Route, Remarks),
	PR->>sign(Sign),
	PR->>type(Type),
        % when making a copy to save, this does not work anymore - AB, jan 2007
        % get(@model, hypered, causalModelMF, Sketch),   % AB, april 2006 
        send(PR, hyper(Sketch, sketch, sketchDependencyElement)).% AB, april 2006 
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
		CR->>addChangeRequest(changeRequestor(deleteSketchDependency,
								QR?sketch,
								@default,
								CR?garpModel,
								QR),
			string('A sketch dependency in this %s will also be deleted because one of its arguments is no longer part of the dependency', QR?sketch?displayTypeName),
			QR?sketch).
%%

%%


%%
% copyToNewElement(FE, NewSK: sketch, New: sketchDependencyElement):<-
copyToNewElement(FE, Mappings: hash_table, NewSK: sketch, New: sketchDependencyElement):<-
	%gp3 0.2 Copy contents and relations (hypers) to a new assumptionInstance
	%see sketchElement<<-copyToNewElement for details
	%we fail if required connected objects are missing from the Mappings table
	
	%lets see if the needed other elements are allready copied
	
	NewArg1Info = NewSK<<-copySK_mapRelatedElement(FE?argument1,FE?argument1Route,Mappings),
	NewArg2Info = NewSK<<-copySK_mapRelatedElement(FE?argument2,FE?argument2Route,Mappings),
		%fails when these ones are not available yet, so will try again later
		
	%everything checked, we can create the item
	New = NewSK<<-addNewSketchDependency(FE?type,
				% FE?argument1, FE?argument1Route, FE?argument2, FE?argument2Route,
				NewArg1Info?first, NewArg1Info?second, NewArg2Info?first, NewArg2Info?second,
				FE?sign,FE?remarks).
%%


% name(FE, Name):<-
name(FE, Name: name):<-
	get(FE, type, Name).
%%


:-pce_end_class.
		  
