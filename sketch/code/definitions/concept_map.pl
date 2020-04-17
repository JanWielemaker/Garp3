/*
Definitie conceptMap class

based on sketch class (which is based on modelFragment), but with specialized stuff for concept map
AB, feb 2006
modelFragment class was based on homer code, so most comments in dutch. gp3 code only where mentioned
*/

:-pce_begin_class(
		  conceptMap(name),
		  sketch,
		  "Definition of concept maps"
		 ).

variable(name,name,get,"Name of the object"). %altijd makeGarp
variable(remarks,string,none,"Remarks on this object").

variable(sketchRelationDefinitions,chain,both,"All sketchRelationDefinition objects"). % for Sketch relations, AB, nov 2006

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
        send(SK, slot, sketchRelationDefinitions, C).
%%


/***********change requestors*****************/

%%newFConcept: een concept in een modelfragment
%%%
%%Of een bepaald object aanwezig mag zijn in een bepaald type sketch wordt momenteel
%bepaald door de editor, en niet door de changeRequestors. Das ook wel zo handig, alhoewel
%de editor het misschien beter door kan sturen?

checkChange_newFConcept(SK,
		       CR: changeRequestor
		      ):->

	%de naam moet uniek zijn in het fragment waaraan het
	%wordt toegevoegd, maar niet in fragmenten waarin het
	%betreffende fragment wordt gebruikt
	
	if
	(
		CR->>checkObject(SK),
	        % check for existing concepts with equal name
		NewName = CR<<-argument(2),
		?(SK,findElements,sketchConceptElement)->>find(->>(@arg1?name,
				    equalExportName,
				    NewName))
	)
	then
		CR->>impossible('The given name is already used in this concept map').

%
applyChange_newFConcept(SK,
		       CR: changeRequestor
		      ):->
	%de instantie wordt aan dit fragment toegevoegd
	%gp3 0.2: we refer to a helper, so copyToNewElement calls from fragmentElements can also use this
	
	I = SK<<-addNewFConcept(CR?arg1,CR?arg2,CR?arg3),
	CR->>result(I).
%%

%
addNewFConcept(SK, Concept: abstractEntity, Name: name, Remarks: string,I: sketchConceptElement):<-

	%gp3 0.2 Split off from applyChange, can now be called by (fragmentElement subclass)->copyToNewElement as well
	
	I *= sketchConceptElement(Name,Remarks, SK), % added SK argument, AB, nov, 2006

	%zet de configuratieve informatie
	Concept->>addConcept(I),
	% I think this can be removed, AB, Oct 2006
        % consider this a consequence (given) for now. To be changed to something neutral later. AB, feb 2006
	% SK->>hyper(I,givenElement,fragment),
	SK?elements->>append(I).
%%	

%%changeFConcept: Wijzigingen in de meegestuurde instantie.

checkChange_changeFConcept(SK,
			    CR: changeRequestor
			   ):->

	%het object checkt op naam uniciteit en geldige entiteitsselectie
	
	CR->>checkObject(SK),
	%er moet wel een entiteit geselecteerd zijn
	if
		@nil = CR<<-arg2
	then
		CR->>impossible('There is no concept selected'),

	NewName = CR<<-argument(3), % was 4
	Concept = CR<<-argument(1),

	if
		?(SK,findElements,sketchConceptElement)->>find(and(not(@arg1 == Concept),
				    ->>(@arg1?name,
				      equalExportName,
					NewName)))
	then
		CR->>impossible('The given name is already used in this concept map').
	
%
applyChange_changeFConcept(_SK,
			    CR: changeRequestor
			   ):->
	%de gewijzigde instantie bevindt zich in dit fragment
	%dus moeten we de boel updaten

	Concept = CR<<-argument(1),
	% Concept?concept->>removeConcept(Concept),
	Concept?entity->>removeConcept(Concept),
	CR?arg2->>addConcept(Concept), %concept-concept relation
	
	% I think this can be removed, AB, Oct 2006
	%state goedzetten:
	% Concept->>delete_hypers(fragment),
	%en de nieuwe
	% SK->>hyper(Concept,givenElement,fragment),
		
	Concept->>name(CR?arg3), % was 4. AB, feb 2006
	Concept->>remarks(CR?arg4). % was 5. AB, feb 2006
%%
	
%%deleteFConcept De meegestuurde instantie moet weg

checkChange_deleteFConcept(SK,
			    CR: changeRequestor
			   ):->

	%We checken gewoon of we elementen hebben die naar de concept verwijzen
	%die moeten dan ook weg. Maakt niet uit in welk fragment (kan ook in subs natuurlijk)

	SK?elements->>for_some(->>(@arg1,checkDeleteElement,CR?arg1,CR)).
%
applyChange_deleteFConcept(SK,
			    CR: changeRequestor
			   ):->
	%de instantie wordt verwijderd

	Concept = CR<<-argument(1),
	SK?elements->>delete(Concept),
	CR->>freeObject(Concept).
%%



%%newRelation: er komt een relation bij
checkChange_newRelation(SK,
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
	?(SK,findElements,sketchRelationElement)->>for_some(  % or relation? AB, feb 2006
	% ?(SK,findElements,relation)->>for_some( 
		->>(@arg1,checkChangedRelation, @nil,
				CR?arg2,CR?arg3,CR?arg4,
				CR?arg5,CR?arg6,CR?object,CR)),
	*/

%
applyChange_newRelation(SK,
	CR: changeRequestor):->
	%maak de nieuwe relation
	%gp3 0.2: we refer to a helper, so copyToNewElement calls from fragmentElements can also use this
	C = SK<<-addNewRelation(CR?arg1,CR?arg2,CR?arg3,CR?arg4,CR?arg5,CR?arg6),
	% C = SK<<-addNewRelation(CR?arg1,CR?arg2,CR?arg3,CR?arg4,CR?arg5,CR?arg6,CR?arg7),
	CR->>result(C),

        % in creating a new one, the user may have changed the relation def, so 
        % update other instances of the same relation definition, if present- AB, Nov 2006
	?(SK,findElements,sketchRelationElement)->>for_some( 
		->>(@arg1,updateRelationsWithSameDefinition, C, CR?arg1, SK)).        



%
addNewRelation(SK, Def: sketchRelationDefinition, 
		Arg1: sketchConceptElement, Arg1Route: chain,
		Arg2: sketchConceptElement, Arg2Route: chain,
		Remarks: string,C: sketchRelationElement):<-
      
        get(SK, findRelationDef, Def, NewDef), 
	C *= sketchRelationElement(NewDef, Arg1, Arg1Route, Arg2, Arg2Route, Remarks, SK),
	SK?elements->>append(C).
%%

%
findRelationDef(SK, Def: sketchRelationDefinition, NewDef: sketchRelationDefinition):<-
        "Return a sketchRelationDefinition in sketch with same Name and Remarks as Def"::
        % if it does not exist, create it anew
        if get(SK?sketchRelationDefinitions, find, 
             and(
                   message(@arg1?name, equal, Def?name),
                   message(@arg1?remarks, equal, Def?remarks)
             ), ExistingDef)
        then 
             NewDef = ExistingDef 
        else
             get(Def, copyToNewElement, SK, NewDef).
%%



%%changeRelation: een gewijzigde relation
checkChange_changeRelation(SK,
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
/*
	%laat in alle SK's alle relations en identiteiten checken of dit wel gaat
	%er tussen uit ivm weghalen normcontroles
	?(SK,findElements,sketchRelationElement)->>for_some( 
	% ?(SK,findElements,relation)->>for_some( 
		->>(@arg1,checkChangedRelation, CR?arg1,
				CR?arg2, CR?arg3,CR?arg4,CR?arg5,
				CR?arg6,CR?object,CR)),
*/	

%
applyChange_changeRelation(SK,
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
	?(SK,findElements,sketchRelationElement)->>for_some( 
		->>(@arg1,updateRelationsWithSameDefinition, CR?arg1,
				CR?arg2, SK)).        
%%

%%deleteRelation: weg met die relation
applyChange_deleteRelation(SK,
	CR: changeRequestor):->

	C = CR<<-argument(1),
	SK?elements->>delete(C), 
	CR->>freeObject(C).
%%

%%%%%%%%%%%%%%% Relation definitions %%%%%%%%%%%%%%%%%%%%%%



%%
removeDefinitionObject(SK,
		       O: object
		      ):->
	"Remove a definition object from this model" ::

	% for SketchRelationDefinitions, AB, Nov 2006
	SK?sketchRelationDefinitions->>delete(O).
%%


%
definitions(SK, Definitions: chain):<-
	"Return the definitions involved in this sketch" ::
        
        get(SK, sortedSketchRelationDefinitions, Definitions).
%%

%%
sortedSketchRelationDefinitions(SK,
	C : chain
	):<-
	"Return a name-sorted copy of the sketchRelationDefinitions variable" ::

	C = SK?sketchRelationDefinitions<<-copy,
	C->>sort(?(@arg1?name,compare,@arg2?name)).
%%


%%
getConceptExportNames(SK, ExportTable: lookupTable):->
	%vul de ExportTable (zie export) met de namen van alle instanties
	%relevant of niet
	%we gaan ervanuit dat ze er nog niet instaan 
	%Bij de naamgeving wordt rekening gehouden met door parents/conditionals geexporteerde
	%concepts, en met identities.
	%dit werk wordt gedaan via onze hulp roundUpConcepts
	
	AllConcepts = SK<<-roundUpConcepts,
	
	%dit is een lookupTable met als key tuple(concept,route) en als value een secondaryID
	%alle entries met dezelfde ID krijgen dezelfde naam, en hoeven maar een keer
	%genoemd te worden

	
	OpKey *= lookupTable,
	AllConcepts->>for_all(
		if(
			?(OpKey,member,@arg2),
			->>(?(OpKey,member,@arg2),
				append,
				@arg1),
			->>(OpKey,append,@arg2,create(chain,@arg1))
		)
	),
	AllConcepts->>done,
		
	%en die lopen we af, hierbij is van belang dat voor elke secondaryID
	%de naam gebaseerd wordt op de meest lokale concept
	
	
	OpKey->>for_all(
		->>(SK,getConceptExportNames_setIDName,@arg2,ExportTable)
	),
	OpKey->>done.
%
getConceptExportNames_setIDName(SK, Concepts: chain, ExportTable: lookupTable):->
	%hulp van getConceptExportNames: bepaal een naam voor alle concepts in de chain
	%en zet ze allemaal in de ExportTable
	%de chain bevat tuples concept-route. We baseren de naam op de naam van de
	%meest lokale
	
	%er zit er minstens 1 in de chain, anders was er nooit een id voor geweest
	
	Min *= number(Concepts?head?second?size), %tot nu toe de minste grootte
	MostLocal *= var,
	MostLocal->>assign(Concepts?head,global), %nu is deze even de meeste lokale
	Concepts->>for_all(
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
	Concepts->>for_all(
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
roundUpConcepts(SK,AllConcepts:lookupTable):<-
	%we vinden alle concepts die in het fragment een rol spelen
	%voor de naamgeving en de structurbeschrijving.
	%er wordt rekening gehouden met de door parents/conditionals geexporteerde
	%concepts, en met identities.
	%uiteindelijk komt er een lookupTable uit. Deze mapt
	%tuple(concept,route) op een secondaryID
	%elke secondaryID moet vervolgens een naam krijgen, als we met naamgeving
	%bezig zijn. Meerdere concepts met dezelfde secondaryID zijn via identities
	%hetzelfde geworden: dus zelfde naam en maar 1 keer noemen
	
	AllConcepts *= lookupTable,
	
	%en de lokale instanties krijgen allemaal een eigen id
	?(SK,findElements,sketchConceptElement)->>for_all(
		->>(AllConcepts,
			append,
			create(tuple,@arg1,create(chain)),
			create(secondaryID)
		)
	).	
/*
@pce->>write_ln('++LOOKUPTABLE VOOR',SK,SK?name),
AllConcepts->>for_all(
	and(
		->>(@pce,write,@arg1?first,'=',@arg1?first?name,'Route:'),
		->>(@arg1?second,for_all,->>(@pce,write,' ',@arg1,'=',@arg1?name)),
		->>(@pce,write_ln)
	)
)
*/
%%

:-pce_end_class.
		  
