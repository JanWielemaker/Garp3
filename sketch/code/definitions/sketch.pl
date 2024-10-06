/*
Definitie sketch class

based on definition of model fragment

AB - march, 2006
*/

:-pce_begin_class(
		  sketch(name),
		  object,
		  "Definition of sketch"
		 ).

variable(name,name,get,"Name of the object"). %altijd makeGarp
variable(remarks,string,none,"Remarks on this object").
variable(savedSketchName, string, both, "Name of the sketch as it is saved").

% variable(definitions,chain,get,"List of definitions defined in this sketch"). % to delete?, AB nov 2006
variable(elements,chain,get,"List of subelements defined in this sketch").

variable(layOutTable, hash_table, get, "Hash_table for layoutinfo"). %eerste nivo

variable(active, bool := @on, both, "Sketch is used in simulation"). %gp3 0.3.11, modeldefinition 5
/*
het nivo is:
	SK?layOutTable-> key=object
				  value=hash_table -> key=route
							  value=hash_table -> key = infonaam
											  value= any
*/

%%
initialise(SK, Name: name = name,
			Remarks: remarks = [string]):->
	SK->+initialise,
	SK->>name(Name),
	default(Remarks,'',RR),
	SK->>remarks(RR),
	send(SK,slot,elements,new(chain)),
	SK->>slot(layOutTable,new(hash_table)).
%%

%%
unlink(SK):->
	%verwijder de sketch: we maken eventjes wat objecten vrij

        debug(sketch(layout), 'unlinking sketch ~w (elements and layouttable)',[SK]), 
	SK?elements->>for_all(->>(@arg1,free)),    
	SK?layOutTable->>free.
%%

%%
name(SK,Name : name):->
	"Set the name, internal" ::

	SK->>slot(name,Name?makeGarp).
%%

%%
remarks(HO, Remarks: string):->
	"Set a copy of the given string as remarks" ::

	HO->>slot(remarks,?(Remarks,strip,both)).
%
remarks(HO,	Remarks : string
       ):<-
	"Return a copy of the remarks" ::
	RealRemarks = HO<<-slot(remarks),
	if
	RealRemarks == @nil
	then
		Remarks *= string
	else
		Remarks = RealRemarks<<-copy.
%%

%%
systemTypeName(_SK, Type: name):<-
	Type = 'sketch'.
%%

%%
displayTypeName(_IS, Type: name):<-
	%gp3 display name is sketch
	Type = 'sketch'.
%%

%
definitions(_SK, _Definitions: chain):<-
	"Return the definitions involved in this sketch" ::

	debug(sketch(save), 'Method definitions should be specialized in subclass',[]).
%%


%%
layOutInfo(SK,
	   I: infoFor = object,
	   Key : key = name,
	   V : value = any,
		Route: [chain]
	  ):->
	"Save lay_out info with infoFor for access by receiver" ::
	%Het mf slaat dus info op over layout van I in de editor van de SK
	%hierbij kan nog de route van I worden meegegeven wanneer het onderdeel is van een
	%geïmporteerde sketch.
   
   get(I, name, IName), 
   debug(sketch(layout), 'Set layoutInfo in sketch ~w for Item: ~w, Key: ~w, V: ~w',[SK, IName, Key, V]),   
   if catch(send(Key, equal, relPosition), _, fail) 
   then 
   (
    get(V, x, X), 
    get(V, y, Y),
    debug(sketch(layout), 'Set relPosition to: ~w x ~w',[X,Y])
    % debug(sketch(layout), 'Starting trace...',[]),
    % trace
   ),
	default(Route,new(chain),RealRoute),
	if
		ObjectTable = SK?layOutTable<<-member(I)
	then
		OTable = ObjectTable
	else (
	        debug(sketch(layout), 'else clause: notifyUnlink in sketch ~w for Item: ~w, Key: ~w, V: ~w',[SK, IName, Key, V]),   

 		OTable *= hash_table,
		I->>notifyUnlink(SK, if(->>(SK?layOutTable,delete,@arg1)),@on), 
			%altijd slagen door de if. @on betekent: niet runnen als wijzelf (de SK) al aan het unlinken zijn. Zie helpers/unlink_notifier.pl
			%gp3 changed notifyUnlink: the above call now replaces old notifiers
			%(bug in old version kept old ones, growing models all the time)
		SK?layOutTable->>append(I,OTable)
		),
	if
		RTable = OTable<<-find_value(->>(@arg1,equal,RealRoute))
	then
		Table = RTable
	else (
		Table *= hash_table,
		OTable->>append(RealRoute?copy,Table)
		),

	Table->>append(Key,V),
   debug(sketch(layout), 'Set layoutInfo in sketch ~w for Item: ~w, Key: ~w, V: ~w done.',[SK, IName, Key, V]),
   debug(sketch(layout), 'LayoutTable for sketch ~w contains info for the following elements:',[SK]).
   % SK?layOutTable->>for_all(->>(@prolog, write_ln, @arg1)), 
   % SK?layOutTable->>for_all(->>(@prolog, write_ln, @arg1?name)).
%%

%%
layOutInfo(SK,
	   I : infoFor = object,
	   Key : key = name,
		Route: [chain],
		_ThisLevelOnly: [bool],	%gp3 1.0: default @off. When @on do not search for the information in the imported route
	   V : any):<-
	"Get lay_out info with infoFor as accessed by receiver, fail if unavailable" ::

      get(I, name, IName), 
      debug(sketch(layout), 'LayoutTable for sketch ~w contains info for the following elements:',[SK]),   
      % SK?layOutTable->>for_all(->>(@prolog, write_ln, @arg1)), 
      % SK?layOutTable->>for_all(->>(@prolog, write_ln, @arg1?name)), 
      debug(sketch(layout), 'Get layoutInfo for sketch.pl for Item: ~w, Key: ~w, V: ~w',[IName, Key, V]),
      % debug(sketch(layout), 'Starting trace...',[]),   
      % trace,
	default(Route,new(chain),RealRoute),
	%als ie er niet is dan is ie er niet: niet maken
	ObjectTable = SK?layOutTable<<-member(I),
	Table = ObjectTable<<-find_value(->>(@arg1,equal,RealRoute)),
	V = Table<<-member(Key),

   if catch(send(Key, equal, relPosition), _, fail) 
   then 
   (
    get(V, x, X), 
    get(V, y, Y),
    debug(sketch(layout), 'Found relPosition: ~w x ~w',[X,Y])
   )
   else
   (
    debug(sketch(layout), 'Found layOutInfo: ~w x ~w',[Key, V])    
   ).
%

%
layOutInfo(_SK,I,Key,Route,ThisLevelOnly,V):<-
	%hij is niet aanwezig in deze sketch,
	%misschien is ie dan aanwezig in de sketch waaruit hij geïmporteerd komt (als er een route is)
	%het eerste element van Route wijst naar het in deze sketch geïmporteerde sketch

        debug(sketch(layout), 'Get layoutInfo for sketch.pl 2',[]), 
	ThisLevelOnly \== @on, %gp3 1.0: explicitly asked not to recurse?
	Route \== @default,

	OtherRoute = Route<<-copy,
	ImportedFragment = OtherRoute<<-delete_head,
	V = ImportedFragment?referencedFragment<<-layOutInfo(I,Key,OtherRoute),
	OtherRoute->>done.
%%

%%
layOutInfoDefault(SK,
	I : infoFor = object,
	Key : key = name,
	Def : default = any,
	Route: [chain],
	V : any):<-
	%haal lay-out info op, en geef default terug als niet beschikbaar
	%gp3 0.3 rewrite: if the layoutinfo is not available, we use the def
	%AND write the def in the layoutinfo
        debug(sketch(layout), 'layoutInfoDefault in sketch.pl',[]), 
	unless
		V = SK<<-layOutInfo(I,Key,Route)
	do
	(
                debug(sketch(layout), 'layoutInfoDefault in sketch.pl case 2',[]), 
		V = Def,
		SK->>layOutInfo(I,Key,V,Route)
	).
%%

%%
clearLayOutInfo(SK):->
%gp3 0.3: clear all layout info, called by viewEditor before saving it again
%this way outdated information will be destroyed

        debug(sketch(layout), 'clearLayOutInfo for sketch ~w',[SK]), 
	
	%explicit clear and free of member chains and hash_tables
	SK?layOutTable->>for_all(
		and(
			->>(@arg2,for_all,
				and(
					->>(@arg1,clear),
					->>(@arg1,free),
					->>(@arg2,clear),
					->>(@arg2,free)
				),@on),
			->>(@arg2,clear),
			->>(@arg2,free)
			), @on
		),
	SK?layOutTable->>clear,
	SK->>slot(layOutTable,new(hash_table)).
%%


%%
findElements(SK,
	    Class : [name],
	    SubclassOk: [bool],
		Elements : chain):<-
	"Return chain of elements, all or of given class" ::
	%gp3 0.3 added SubclassOk (default: @on). When @off, only exact class, no subclasses
	%class must be given then..
	
	(   Class = @default
	->  C = sketchElement
	;   C = Class
	),
	if
		SubclassOk = @off
	then
		Elements = SK?elements<<-find_all(@arg1?class_name == C)
	else
		Elements = SK?elements<<-find_all(->>(@arg1,instance_of,C)).
%%

	
%%
currentType(_SK,
	Type: {conceptMap, causalModel, structuralModel}):<-
	% Not sure if this is necessary
	Type = conceptMap.

%%	
type(SK,
	Type: name
	):<-
	"Return description of the currentType" ::

	conceptMap = SK<<-currentType,!,
	Type *= name('Concept map').

%
type(_SK,Type):<-
	Type *= name('Sketch of unknown type').	

%%

/***********change requestors*****************/

%newSK: een nieuw sketch

checkChange_newSK(SK,
	CR: changeRequestor):->
	
	%impossible als de naam hetzelfde is
	%als de naam van de ontvanger

	CR?arg1->>equalExportName(SK?name),
	CR->>impossible(string(
		'This name is already used by a %s',SK?systemTypeName),SK).
%%

%%changeSK: een sketch wordt gewijzigd, met veel consequenties


checkChange_changeSK(SK,
	CR: changeRequestor):->
	
		%delegeren naar verschillende calls
		if
			CR->>checkObject(SK)
		then
			@model->>normReservedSKNames(CR?arg1,CR),
		ignore(pl_checkChangeSK_name(SK,CR)).

%
pl_checkChangeSK_name(SK,CR):-
		%impossible als de naam hetzelfde is als de ontvanger
		%en de wijziging gaat over een ander sketch
		
		\+ CR->>checkObject(SK),
		CR?arg1->>equalExportName(SK?name),
		CR->>impossible(string(
			'This name is allready used by a %s',SK?systemTypeName)
				,SK).
	
%
applyChange_changeSK(SK,
	CR: changeRequestor):->
		%dit mf wordt gewijzigd
		
		SK->>name(CR?arg1),
		SK->>remarks(CR?arg2),

	VerwijderdeRefs *= chain,
	% stuff deleted here, AB, march 2006
	CR->>result(VerwijderdeRefs).
%%

%%
%copySK: the sketch will be copied
%gp3 0.2

%no checks needed
applyChange_copySK(SK,
	CR: changeRequestor):->
	%split, because we can also do the children recursively
	
	New = SK<<-createCopy(new(hash_table)), %empty mappings table to start with
	CR->>result(New).
%
createCopy(SK, Mappings: hash_table, New: sketch):<-
        debug(sketch(save), 'createCopy of sketch: ~w...',[SK]), 
	%find a name
	Name= SK<<-name,
	copySK_uniqueName(Name,1,UniqueName),
	%we create an object of the same type (mf/is)
	New = @pce<<-instance(SK?class,UniqueName,SK?remarks?copy), %argumenten!!
	
	%copy all elements
	%this is done as follows: 
	%1. collect all elements
	%2. Fill the mappings table, connecting the old objects to its copies
	%3. Loop over all elements, asking if they can copy themselves now
	%  -> for every element class, the method copyToNewElement exists. As arguments it gets
	% the new SK, the Mappingstable. The method can return a copy of the element
	% or fail, which means: I cannot copy myself right now, please try again
	% (it waits for other objects to be copied first)
	%The mappings table is also send to children that have to be copied 
	%(if CopySingleInheritance = @on), because they might refer to mapped parent references
	%in routes


	%and we are mapped too
	Mappings->>append(SK,New),

        % To do: create specialized methods for relevant subclasses of sketch 
        % causalModel and stgraph do not have definitions
        unless (SK->>instance_of(causalModel) ; SK->>instance_of(stgraph) )
        do 
        (
	 debug(sketch(save), 'copy Definitions...',[]),
	 Definitions = SK?definitions<<-copy,
	 %this call will allways finish (?)
	 copySK_loopDefinitions(SK, Definitions, Mappings, New)
	), 

        if (SK->>instance_of(stgraph) )
        then
        (
	 debug(sketch(save), 'copy sketchQuantities and sketchValues from old sketch: ~w to new sketch ~w.', [SK, New]),
	 % debug(sketch(save), 'starting trace...', []),
	 % trace,
	 get(SK, copy_quantities_simple, New, NewQuantities), 
	 get(SK, copy_values, New, NewValues), 

	 send(New, sketchQuantities, NewQuantities), 
	 send(New, sketchValues, NewValues)
	), 

        Elements = SK?elements<<-copy,
	%this call will allways finish (?)
	copySK_loopElements(SK,Elements,Mappings,New),

	%copy the layouttable (never fail)
	%we add the sketch itself to the mapping, because general layoutinfo (ie size of screen)
	%will be saved there
	Mappings->>append(SK,New),
	
	% debug(sketch(save), 'starting trace...', []),
	% trace,
	SK?layOutTable->>for_some(
		->>(New,copySK_copyLayout,Mappings,@arg1,@arg2)), %implemented for the new one
	 debug(sketch(save), 'createCopy of sketch ~w to ~w done.', [SK, New]).
%%

%
copySK_uniqueName(Name,Number,NewName):-
	%gp3 0.2 helper for copySK: find a unique name
	NewName *= string('%s%s',Name,Number),
	\+ @model?sketches->>find(->>(@arg1?name,equalExportName,NewName)),!.
%
copySK_uniqueName(Name,Number,NewName):-
	%copy Number exists, raise the number
	NewNumber is Number + 1,
	copySK_uniqueName(Name,NewNumber,NewName).

%
copySK_loopDefinitions(_SK,Definitions,_Mappings,_NewSK):-
	%gp3 0.2 helper for copySK: ask all remaining definitions to copy themselves
	Definitions->>empty,!. %done, no backtracking here
%
copySK_loopDefinitions(SK,Definitions,Mappings,NewSK):-
	Definitions?copy->>for_all(
		if(
			->>(Mappings,append,@arg1,?(@arg1,copyToNewElement, NewSK)),
			->>(Definitions,delete,@arg1) %this element is done
		)
	),
	%see if there are any definitions left
	copySK_loopDefinitions(SK,Definitions,Mappings,NewSK).
%%


%
copySK_loopElements(_SK,Elements,_Mappings,_NewSK):-
	%gp3 0.2 helper for copySK: ask all remaining elements to copy themselves
	Elements->>empty,!. %done, no backtracking here
%
copySK_loopElements(SK,Elements,Mappings,NewSK):-
        debug(sketch(save), 'copySK_loopElements for SK: ~w',[SK]), 
        % trace,
        get(Elements, size, L1), 
	Elements?copy->>for_all(
		if(
			->>(Mappings,append,@arg1,?(@arg1,copyToNewElement,Mappings,NewSK)),
			% ->>(Mappings,append,@arg1,?(@arg1,copyToNewElement,NewSK)),
			->>(Elements,delete,@arg1) %this element is done
		)
	),
        get(Elements, size, L2),
        if L1 = L2
        then 
        (
	  debug(sketch(save), 'Something is wrong in copySK_loopElements.',[]),
	  debug(sketch(save), 'Remaining elements: ~w',[Elements]),
	  visualize:write_chain(Elements)
        )
        else 
        (
	% continue until no more elements left
	copySK_loopElements(SK,Elements,Mappings,NewSK)
        ).

%
copySK_copyLayout(NewSK, Mappings: hash_table, Element: any, Layout: hash_table):->
	%gp3 0.2 helper for copySK: copy 1 element of the layout table
	%this has an object (sketch element etc) as key
	%and the value is another lay-out table, where keys are routes, and values are tables containing the real info
	%so we have to go a bit deeper still
	
	%never fail
	Layout->>for_some(->>(NewSK, copySK_copyLayoutRoute, Mappings, Element, @arg1, @arg2)).
%
copySK_copyLayoutRoute(NewSK, Mappings: hash_table, 
			Element: any, Route: chain*, LayoutInfo: hash_table):->
	%see above
	%at this level we can copy
	%alot is possible, so we try to be flexible
	
	(NewElement = Mappings<<-member(Element) ; NewElement = Element),
	if
		Route = @nil
	then
		NewRoute = @nil
	else
	(
		%complete mapping
		NewRoute *= chain,
		Member *= var,
		Route->>for_all(
			->>(NewRoute,append,when(assign(Member,?(Mappings,member,@arg1)),Member,@arg1)))
	),
	%never fail
	LayoutInfo->>for_some(		%key = infoname, value = info value
		->>(NewSK,layOutInfo,NewElement,@arg1,@arg2,NewRoute)).
%
copySK_mapRelatedElement(_SK,OldElement: sketchElement, OldRoute: chain*, Mappings: hash_table, 
			NewElementRef: tuple):<-
	%gp3 0.2 helper for copySK structure.
	%called by copyToNewElement methods in sketchElement subclasses
	%to get connectedElements and their routes mapped to the newly copied elements and/or changed routes
	%when an element is local, we need a copy. When that is not available in Mappings, we fail
	%(which means: try again later when the related element has been copied).
	%when an element is not local, we can use the same element, but have to fix the route
	%because we need newly copied references
	%
	%result is a tuple (newElement, newRoute).
	
	%succeeds correctly for @nil and empty routes (both times returning empty route)
	
	if
	(
		OldRoute = @nil; OldRoute->>empty
	)
	then
	(
		%local object
		NewElement = Mappings<<-member(OldElement), %fail when not yet available
		NewRoute *= chain
	)
	else
	(
		%imported object
		%the head of the route MUST be allready mapped
		Mappings<<-member(OldRoute?head),
		NewRoute *= chain,
		Member *= var,
		OldRoute->>for_all(
			if(
				assign(Member,?(Mappings,member,@arg1)),
				->>(NewRoute,append,Member),
				->>(NewRoute,append,@arg1)
				)),
		%The element itself might also be mapped, when its in a parent sketch that was also copied
		(
			NewElement = Mappings<<-member(OldElement)
		;	
			NewElement = OldElement
		)
	),
	NewElementRef *= tuple(NewElement,NewRoute).
%%

%%deleteSK: de sketch wordt verwijderd
checkChange_deleteSK(SK,
	CR: changeRequestor):->
	
	%1: versie voor zelf verwijderd
	
	%als we zelf verwijderd worden kijken we alleen maar of we inhoud hebben
	%alleen als dat zo is geven we een waarschuwing
	
	CR->>checkObject(SK),!,
	\+ SK?elements->>empty, %er is inhoud
	CR->>warning(string('Warning! You are about to delete the sketch "%s", which already has content.', SK?name),SK).
%
checkChange_deleteSK(_SK,
	_CR: changeRequestor):->
        % not sure if this is used - AB, sept 2006
	debug(sketch(save), 'checkChange_deleteSK. ',[]).

%
applyChange_deleteSK(SK,
	CR: changeRequestor):->
	%onszelf verwijderen. 
	%gp3: Also ask the model to delete any references to this SK
	%from the SKViews tables
	
	@model->>removeDefinitionObject(SK),
	@model->>deleteFromSKViews(SK),
	CR->>freeObject(SK).
%%




% for Concept map - maybe better to move it there?
%
%deleteRelationDef: dat mag niet als we sketch relaties hebben die daarop zijn gebaseerd
checkChange_deleteRelationDef(SK,
	CR: changeRequestor):->

	Def = CR<<-argument(1),
	SRElements = SK<<-findElements(sketchRelationElement),
	if
		SRElements->>find(@arg1?definition == Def) 
	then
		CR->>impossible(string(
			'At least one sketch relation in %s "%s" is based on the removed definition',
			SK?systemTypeName,SK?name),SK),
	SRElements->>done.
%%


%%deleteRelationDef: weg met die sketch relation def
applyChange_deleteRelationDef(SK,
	CR: changeRequestor):->

	C = CR<<-argument(1),
	SK?elements->>delete(C), 
	CR->>freeObject(C).
%%


%%deleteRelation: weg met die sketch relation
applyChange_deleteRelation(SK,
	CR: changeRequestor):->

	C = CR<<-argument(1),
	SK?elements->>delete(C), 
	CR->>freeObject(C).
%%




% for Structure model - maybe better to move it there?
%
%deleteSketchStructuralRelationDef: dat mag niet als we sketch relaties hebben die daarop zijn gebaseerd
checkChange_deleteSketchStructuralRelationDef(SK,
	CR: changeRequestor):->

	Def = CR<<-argument(1),
	SRElements = SK<<-findElements(sketchStructuralRelationElement),
	if
		SRElements->>find(@arg1?definition == Def) 
	then
		CR->>impossible(string(
			'At least one sketch structural relation in %s "%s" is based on the removed definition',
			SK?systemTypeName,SK?name),SK),
	SRElements->>done.
%%


%%deleteSketchStructuralRelationDef: weg met die sketch relation def
applyChange_deleteSketchStructuralRelationDef(SK,
	CR: changeRequestor):->

	C = CR<<-argument(1),
	SK?elements->>delete(C), 
	CR->>freeObject(C).
%%


%%deleteSketchStructuralRelation: weg met die sketch relation
applyChange_deleteSketchStructuralRelation(SK,
	CR: changeRequestor):->

	C = CR<<-argument(1),
	SK?elements->>delete(C), 
	CR->>freeObject(C).
%%





%%%%%%

%%
insertDefinitionObject(SK, O:object):->
	"Insert a definition object into this model" ::
	%dat gaat dus per klasse
	( 
	    % for Sketch, AB, Aug 2006
	    O->>instance_of(sketchRelationDefinition)
	->  SK?sketchRelationDefinitions->>add(O)
	;   O->>instance_of(sketchStructuralRelationDefinition)
	->  SK?sketchStructuralRelationDefinitions->>add(O)

	).
%%



%%%%%%

%%
%deleteHObjectTree
%check of er instanties zijn die van entity moeten veranderen
%dit geldt voor assumptions en abstractEntities
%deze krijgen een nieuwe entity als dat kan, of anders wordt het impossible

checkChange_deleteHObjectTree(_SK, _CR: changeRequestor):->
        % not sure if this is used, AB, sept 2006
        debug(sketch(save), 'checkChange_deleteHObjectTree',[]).
%
%pl_checkDeleteAETree(_SK,_CR):-
%        debug(sketch(save), 'checkChange_deleteAETree',[]).



%%%%%%%%%%%%EXPORT%%%%%%%%%%%%%%%%

export(_SK, _F: file, _LegacyNaming: [bool]):->
	% no export necessary
        true. 

:-pce_end_class.
		  
