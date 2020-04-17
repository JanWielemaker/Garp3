/*
Definitie structuralModel class

based on sketch class (which is based on modelFragment), but with specialized stuff for structural model
AB, feb 2006
modelFragment class was based on homer code, so most comments in dutch. gp3 code only where mentioned
*/

:-pce_begin_class(
		  structuralModel(name),
		  sketch,
		  "Definition of structural models"
		 ).

variable(name,name,get,"Name of the object"). %altijd makeGarp
variable(remarks,string,none,"Remarks on this object").

variable(sketchStructuralRelationDefinitions,chain,get,"All sketchStructuralRelationDefinition objects"). % AB, nov 2006
variable(elements,chain,get,"List of subelements defined in this fragment").

variable(layOutTable, hash_table, get, "Hash_table for layoutinfo"). %eerste nivo

variable(active, bool := @on, both, "Modelfragment is used in simulation"). %gp3 0.3.11, modeldefinition 5
/*
het nivo is:
	SK?layOutTable -> key=object
					  value=hash_table -> key=route
										  value=hash_table -> key = infonaam
															  value= any
*/

%%
initialise(SK, Name: name = name,
			Remarks: remarks = [string]):->
	SK->+initialise(Name, Remarks),
        new(C, chain),
        send(SK, slot, sketchStructuralRelationDefinitions, C).
%%


/***********change requestors*****************/

%%newFSketchObject: een sketchObject in een modelfragment
%%%
%%Of een bepaald object aanwezig mag zijn in een bepaald type sketch wordt momenteel
%bepaald door de editor, en niet door de changeRequestors. Das ook wel zo handig, alhoewel
%de editor het misschien beter door kan sturen?

checkChange_newFSketchObject(SK,
		       CR: changeRequestor
		      ):->
	%de naam moet uniek zijn in het fragment waaraan het
	%wordt toegevoegd, maar niet in fragmenten waarin het
	%betreffende fragment wordt gebruikt
	
	if
	(
		CR->>checkObject(SK),
	        % check for existing sketchObjects with equal name
		NewName = CR<<-argument(2),
		?(SK,findElements,sketchObjectElement)->>find(->>(@arg1?name,
				    equalExportName,
				    NewName))
	)
	then
		CR->>impossible('The given name is already used in this structural model').

%
applyChange_newFSketchObject(SK,
		       CR: changeRequestor
		      ):->
	%de instantie wordt aan dit fragment toegevoegd
	%gp3 0.2: we refer to a helper, so copyToNewElement calls from fragmentElements can also use this
	
	I = SK<<-addNewFSketchObject(CR?arg1,CR?arg2,CR?arg3,CR?arg4), % added type argument, AB, june 2006
	CR->>result(I).
%
%addNewFSketchObject(SK, SketchObject: abstractEntity, Name: name, Remarks: string,I: sketchObjectElement):<-
addNewFSketchObject(SK, SketchObject: abstractEntity, Name: name, Type: {undefined,entity,agent,assumption}, 
		  Remarks: string,I: sketchObjectElement):<-

	%gp3 0.2 Split off from applyChange, can now be called by (fragmentElement subclass)->copyToNewElement as well
	
	I *= sketchObjectElement(Name, Type, Remarks, SK),  % added type argument, AB, june 2006, and SK, AB, nov, 2006

	%zet de configuratieve informatie
	SketchObject->>addSketchObject(I),
	% I think this can be removed, AB, Oct 2006
        % consider this a consequence (given) for now. To be changed to something neutral later. AB, feb 2006
	% SK->>hyper(I,givenElement,fragment),
	SK?elements->>append(I).
%%	

%%changeFSketchObject: Wijzigingen in de meegestuurde instantie.

checkChange_changeFSketchObject(SK,
			    CR: changeRequestor
			   ):->

	%het object checkt op naam uniciteit en geldige entiteitsselectie
	
	CR->>checkObject(SK),
	%er moet wel een entiteit geselecteerd zijn
	if
		@nil = CR<<-arg2
	then
		CR->>impossible('There is no sketchObject selected'),

	NewName = CR<<-argument(3), % was 4
	SketchObject = CR<<-argument(1),

	if
		?(SK,findElements,sketchObjectElement)->>find(and(not(@arg1 == SketchObject),
				    ->>(@arg1?name,
				      equalExportName,
					NewName)))
	then
		CR->>impossible('The given name is already used in this structural model').
	
%
applyChange_changeFSketchObject(_SK,
			    CR: changeRequestor
			   ):->
	%de gewijzigde instantie bevindt zich in dit fragment
	%dus moeten we de boel updaten

	SketchObject = CR<<-argument(1),
	% SketchObject?sketchObject->>removeSketchObject(SketchObject),
	SketchObject?entity->>removeSketchObject(SketchObject),
	CR?arg2->>addSketchObject(SketchObject), %sketchObject-sketchObject relation
	
	% I think this can be removed, AB, Oct 2006
	%state goedzetten:
	% SketchObject->>delete_hypers(fragment),
	%en de nieuwe
	% SK->>hyper(SketchObject,givenElement,fragment),
		
	SketchObject->>name(CR?arg3), % was 4. AB, feb 2006
	SketchObject->>type(CR?arg4), % added type argument, AB, june 2006
	SketchObject->>remarks(CR?arg5). 
%%
	
%%deleteFSketchObject De meegestuurde instantie moet weg

checkChange_deleteFSketchObject(SK,
			    CR: changeRequestor
			   ):->

	%We checken gewoon of we elementen hebben die naar de sketchObject verwijzen
	%die moeten dan ook weg. Maakt niet uit in welk fragment (kan ook in subs natuurlijk)

	SK?elements->>for_some(->>(@arg1,checkDeleteElement,CR?arg1,CR)).
%
applyChange_deleteFSketchObject(SK,
			    CR: changeRequestor
			   ):->
	%de instantie wordt verwijderd

	SketchObject = CR<<-argument(1),
	SK?elements->>delete(SketchObject),
	CR->>freeObject(SketchObject).
%%



%%newSketchStructuralRelation: er komt een relation bij
checkChange_newSketchStructuralRelation(SK,
	CR : changeRequestor):->
	if
		CR->>checkObject(SK)
	then
	(
		if
			@nil = CR<<-arg2
		then
			CR->>impossible('There is no definition for the new relation selected',SK)
	).		
	%laat in alle SK's alle relations en identiteiten checken of dit wel gaat
	/*
	%er tussen uit ivm weghalen normcontroles
	?(SK,findElements,sketchStructuralRelationElement)->>for_some(  % or relation? AB, feb 2006
	% ?(SK,findElements,relation)->>for_some( 
		->>(@arg1,checkChangedRelation, @nil,
				CR?arg2,CR?arg3,CR?arg4,
				CR?arg5,CR?arg6,CR?object,CR)),
	*/

%
applyChange_newSketchStructuralRelation(SK,
	CR: changeRequestor):->
	%maak de nieuwe relation
	%gp3 0.2: we refer to a helper, so copyToNewElement calls from fragmentElements can also use this
	
	C = SK<<-addNewSketchStructuralRelation(CR?arg1,CR?arg2,CR?arg3,CR?arg4,CR?arg5,CR?arg6),
	% C = SK<<-addNewSketchStructuralRelation(CR?arg1,CR?arg2,CR?arg3,CR?arg4,CR?arg5,CR?arg6,CR?arg7),
	CR->>result(C),

        % in creating a new one, the user may have changed the relation def, so 
        % update other instances of the same relation definition, if present- AB, Nov 2006
	?(SK,findElements,sketchStructuralRelationElement)->>for_some( 
		->>(@arg1,updateStructuralRelationsWithSameDefinition, C, CR?arg1, SK)).        



%
addNewSketchStructuralRelation(SK, Def: sketchStructuralRelationDefinition, 
		Arg1: sketchObjectElement, Arg1Route: chain,
		Arg2: sketchObjectElement, Arg2Route: chain,
		Remarks: string,C: sketchStructuralRelationElement):<-

        get(SK, findRelationDef, Def, NewDef), 
	C *= sketchStructuralRelationElement(NewDef, Arg1, Arg1Route, Arg2, Arg2Route, Remarks, SK),
	SK?elements->>append(C).
	% I think this can be removed, AB, Oct 2006
	% SK->>hyper(C,givenElement,fragment).
%%


%
findRelationDef(SK, Def: sketchStructuralRelationDefinition, NewDef: sketchStructuralRelationDefinition):<-
        "Return a sketchStructuralRelationDefinition in sketch with same Name and Remarks as Def"::
        % if it does not exist, create it anew
        if get(SK?sketchStructuralRelationDefinitions, find, 
             and(
                   message(@arg1?name, equal, Def?name),
                   message(@arg1?remarks, equal, Def?remarks)
             ), ExistingDef)
        then 
             NewDef = ExistingDef 
        else
             get(Def, copyToNewElement, SK, NewDef).
%%



%%changeSketchStructuralRelation: een gewijzigde relation
checkChange_changeSketchStructuralRelation(SK,
	CR : changeRequestor):->

	if
		CR->>checkObject(SK)
	then
	(
		if
			@nil = CR<<-arg2 % was arg3 % AB, feb 2006
		then
			CR->>impossible('There is no definition for the new relation selected',SK)
	).	
	%laat in alle SK's alle relations en identiteiten checken of dit wel gaat
	/*
	%er tussen uit ivm weghalen normcontroles
	?(SK,findElements,sketchStructuralRelationElement)->>for_some( % or relation? AB feb 2006
	% ?(SK,findElements,relation)->>for_some( 
		->>(@arg1,checkChangedRelation, CR?arg1,
				CR?arg3,CR?arg4,CR?arg5,
				CR?arg6,CR?arg7,CR?object,CR)),
	*/

%
applyChange_changeSketchStructuralRelation(SK,
	CR: changeRequestor):->
	%wijzig de relation

	C = CR<<-argument(1),

	C->>changeDefinition(CR?arg2),
        % One old Arg is gone, the rest is one number lower now, AB, feb 2006
	C->>argument1(CR?arg3, CR?arg4), 
	C->>argument2(CR?arg5,CR?arg6), 
	C->>remarks(CR?arg7),
	% I think this can be removed, AB, Oct 2006
	% C->>delete_hypers(fragment), %wordt opnieuw aangelegd
	% SK->>hyper(C,givenElement,fragment).

        % update other instances of the same relation definition, if present- AB, Nov 2006
	?(SK,findElements,sketchStructuralRelationElement)->>for_some( 
		->>(@arg1,updateStructuralRelationsWithSameDefinition, C,
				CR?arg2, SK)).        
%%


%%deleteSketchStructuralRelation: weg met die relation
applyChange_deleteSketchStructuralRelation(SK,
	CR: changeRequestor):->

	C = CR<<-argument(1),
	SK?elements->>delete(C), 
	CR->>freeObject(C).
%%



%%
removeDefinitionObject(SK,
		       O: object
		      ):->
	"Remove a definition object from this model" ::

	% for SketchStructuralRelationDefinitions, AB, Nov 2006
	SK?sketchStructuralRelationDefinitions->>delete(O).
%%


	
%%newQuantityRelation: een prop of inf tussen 2 quantities,
%geen check ivm nieuw beleid (zo min mogelijk normen)

applyChange_newQuantityRelation(SK,
	CR: changeRequestor):->
	%we maken de nieuwe relatie
	%gp3 0.2: we refer to a helper, so copyToNewElement calls from fragmentElements can also use this
	
	QR = SK<<-addNewQuantityRelation(CR?arg1,CR?arg2,CR?arg3,CR?arg4,CR?arg5,
							CR?arg6,CR?arg7),
	CR->>result(QR).
%
addNewQuantityRelation(SK, Type: {prop,inf},
			Arg1: garpQuantity, Arg1Route: chain,
			Arg2: garpQuantity, Arg2Route: chain,
			Sign: {min,plus}, Remarks: string, QR):<-
	
	QR *= garpQuantityRelation(Sign, Type, Arg1, Arg1Route, Arg2, Arg2Route, Remarks),
	SK?elements->>append(QR).
	% I think this can be removed, AB, Oct 2006
	% SK->>hyper(QR,givenElement,fragment).
%%

%%changeQuantityRelation:
%geen check ivm nieuw beleid (zo min mogelijk normen)

applyChange_changeQuantityRelation(_SK,
	CR: changeRequestor):->
	
	QR = CR<<-arg1,
	QR->>type(CR?arg2),
	QR->>argument1(CR?arg3, CR?arg4),
	QR->>argument2(CR?arg5, CR?arg6),
	QR->>sign(CR?arg7),
	QR->>remarks(CR?arg8).
%%

%%deleteQuantityRelation: geen check
applyChange_deleteQuantityRelation(SK,
	CR: changeRequestor):->

	SK?elements->>delete(CR?arg1), %hypers blijven nog hangen
	CR->>freeObject(CR?arg1).
%%




%
definitions(SK, Definitions: chain):<-
	"Return the definitions involved in this sketch" ::
        
        get(SK, sortedSketchStructuralRelationDefinitions, Definitions).
%%

%%
sortedSketchStructuralRelationDefinitions(SK,
	C : chain
	):<-
	"Return a name-sorted copy of the sketchStructuralRelationDefinitions variable" ::

	C = SK?sketchStructuralRelationDefinitions<<-copy,
	C->>sort(?(@arg1?name,compare,@arg2?name)).
%%

% for Sketch, AB, Aug 2006
%%
init_structural_relation_definitions(SK):->
        % note: the phrase 'built in' is used in structure_editor.pl to recognize these 
        % as built-in definitions. If you change this remarks field, they can be deleted       
	D1 *= sketchStructuralRelationDefinition('is_a', 
			'X is a subtype of Y: Hierarchical subtype relationship (built in)'), 
	D2 *= sketchStructuralRelationDefinition('contains', 
			'X contains Y: Containment relationship (built in)'), 
	D3 *= sketchStructuralRelationDefinition('part_of', 
			'X is a part of Y: Part-of relationship (built in)'), 
	D4 *= sketchStructuralRelationDefinition('connected_to', 
			'X is connected to Y: Connection relationship (built in)'), 
	SK->>insertDefinitionObject(D1),
	SK->>insertDefinitionObject(D2),
	SK->>insertDefinitionObject(D3),
	SK->>insertDefinitionObject(D4).
%%



%%
getSketchObjectExportNames(SK, ExportTable: lookupTable):->
	%vul de ExportTable (zie export) met de namen van alle instanties
	%relevant of niet
	%we gaan ervanuit dat ze er nog niet instaan 
	%Bij de naamgeving wordt rekening gehouden met door parents/conditionals geexporteerde
	%sketchObjects, en met identities.
	%dit werk wordt gedaan via onze hulp roundUpSketchObjects
	
	AllSketchObjects = SK<<-roundUpSketchObjects,
	
	%dit is een lookupTable met als key tuple(sketchObject,route) en als value een secondaryID
	%alle entries met dezelfde ID krijgen dezelfde naam, en hoeven maar een keer
	%genoemd te worden

	
	OpKey *= lookupTable,
	AllSketchObjects->>for_all(
		if(
			?(OpKey,member,@arg2),
			->>(?(OpKey,member,@arg2),
				append,
				@arg1),
			->>(OpKey,append,@arg2,create(chain,@arg1))
		)
	),
	AllSketchObjects->>done,
		
	%en die lopen we af, hierbij is van belang dat voor elke secondaryID
	%de naam gebaseerd wordt op de meest lokale sketchObject
	
	
	OpKey->>for_all(
		->>(SK,getSketchObjectExportNames_setIDName,@arg2,ExportTable)
	),
	OpKey->>done.
%
getSketchObjectExportNames_setIDName(SK, SketchObjects: chain, ExportTable: lookupTable):->
	%hulp van getSketchObjectExportNames: bepaal een naam voor alle sketchObjects in de chain
	%en zet ze allemaal in de ExportTable
	%de chain bevat tuples sketchObject-route. We baseren de naam op de naam van de
	%meest lokale
	
	%er zit er minstens 1 in de chain, anders was er nooit een id voor geweest
	
	Min *= number(SketchObjects?head?second?size), %tot nu toe de minste grootte
	MostLocal *= var,
	MostLocal->>assign(SketchObjects?head,global), %nu is deze even de meeste lokale
	SketchObjects->>for_all(
		if(
			->>(Min, larger, @arg1?second?size),
			and(
				assign(MostLocal,@arg1,global),
				->>(Min,value,@arg1?second?size)
			)
		)
	),	
	
	Naam = SK<<-exportVar(MostLocal?first,MostLocal?second,MostLocal?first?name,ExportTable,@off,@off),
	%das dan eindelijk de naam
	SketchObjects->>for_all(
		->>(ExportTable,
			append,
			create(tuple,
				@arg1?first,
				@arg1?second?copy
			),
			Naam
		)
	).
%%
roundUpSketchObjects(SK,AllSketchObjects:lookupTable):<-
	%we vinden alle sketchObjects die in het fragment een rol spelen
	%voor de naamgeving en de structurbeschrijving.
	%er wordt rekening gehouden met de door parents/conditionals geexporteerde
	%sketchObjects, en met identities.
	%uiteindelijk komt er een lookupTable uit. Deze mapt
	%tuple(sketchObject,route) op een secondaryID
	%elke secondaryID moet vervolgens een naam krijgen, als we met naamgeving
	%bezig zijn. Meerdere sketchObjects met dezelfde secondaryID zijn via identities
	%hetzelfde geworden: dus zelfde naam en maar 1 keer noemen
	
	AllSketchObjects *= lookupTable,
	
	%en de lokale instanties krijgen allemaal een eigen id
	?(SK,findElements,sketchObjectElement)->>for_all(
		->>(AllSketchObjects,
			append,
			create(tuple,@arg1,create(chain)),
			create(secondaryID)
		)
	).	
/*
@pce->>write_ln('++LOOKUPTABLE VOOR',SK,SK?name),
AllSketchObjects->>for_all(
	and(
		->>(@pce,write,@arg1?first,'=',@arg1?first?name,'Route:'),
		->>(@arg1?second,for_all,->>(@pce,write,' ',@arg1,'=',@arg1?name)),
		->>(@pce,write_ln)
	)
),
*/

:-pce_end_class.
		  
