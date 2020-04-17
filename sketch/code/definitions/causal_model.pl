/*
Definitie causalModel class

based on sketch class (which is based on modelFragment), but with specialized stuff for causal model
AB, feb 2006
modelFragment class was based on homer code, so most comments in dutch. gp3 code only where mentioned
*/

:-pce_begin_class(
		  causalModel(name),
		  sketch,
		  "Definition of causal models"
		 ).

variable(name,name,get,"Name of the object"). %altijd makeGarp
variable(remarks,string,none,"Remarks on this object").

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
	SK->+initialise(Name, Remarks).
%%


/***********change requestors*****************/

%%newSketchQuantity: een sketch_quantity in een modelfragment
%%%
%%Of een bepaald object aanwezig mag zijn in een bepaald type sketch wordt momenteel
%bepaald door de editor, en niet door de changeRequestors. Das ook wel zo handig, alhoewel
%de editor het misschien beter door kan sturen?

checkChange_newSketchQuantity(SK,
		       CR: changeRequestor
		      ):->

	%de naam moet uniek zijn in het fragment waaraan het
	%wordt toegevoegd, maar niet in fragmenten waarin het
	%betreffende fragment wordt gebruikt
	
	if
	(
		CR->>checkObject(SK),
		NewName = CR<<-argument(2), % was 3
	        % check for existing sketchQuantities with name equal to NewName
		?(SK,findElements,sketchQuantityElement)->>find(->>(@arg1?name,
				    equalExportName,
				    NewName))
	)
	then
		CR->>impossible('The given name is already used in this causal model').
		

%
applyChange_newSketchQuantity(SK,
		       CR: changeRequestor
		      ):->
	%de instantie wordt aan dit fragment toegevoegd
	%gp3 0.2: we refer to a helper, so copyToNewElement calls from fragmentElements can also use this
        debug(sketch(save), 'applyChange_newSketchQuantity CR: ~w\n', [CR]),
	I = SK<<-addNewSketchQuantity(CR?arg2,CR?arg3), % removed 1st argument: sketchQuantity, AB, April 2007
	CR->>result(I).
%%

%
addNewSketchQuantity(SK, Name: name, Remarks: string,I: sketchQuantityElement):<-
% addNewSketchQuantity(SK, SketchQuantity: abstractEntity, Name: name, Remarks: string,I: sketchQuantityElement):<-
        % removed 1st argument: sketchQuantity, AB, April 2007
	I *= sketchQuantityElement(Name,Remarks, SK),

	%zet de configuratieve informatie
	% SketchQuantity->>addSketchQuantity(I),
	SK?elements->>append(I).
%%	

%%changeSketchQuantity: Wijzigingen in de meegestuurde instantie.

checkChange_changeSketchQuantity(SK,
			    CR: changeRequestor
			   ):->
	%het object checkt op naam uniciteit en geldige entiteitsselectie
	
	CR->>checkObject(SK),
	%er moet wel een entiteit geselecteerd zijn
	if
		@nil = CR<<-arg2
	then
		CR->>impossible('There is no sketch_quantity selected'),

	NewName = CR<<-argument(3), % was 4
	SketchQuantity = CR<<-argument(1),

	if
		?(SK,findElements,sketchQuantityElement)->>find(and(not(@arg1 == SketchQuantity),
				    ->>(@arg1?name,
				      equalExportName,
					NewName)))
	then
		CR->>impossible('The given name is already used in this causal model').
	
%
applyChange_changeSketchQuantity(_SK,
			    CR: changeRequestor
			   ):->
	%de gewijzigde instantie bevindt zich in dit fragment
	%dus moeten we de boel updaten
        debug(sketch(quantity_props), 'applyChange_changeSketchQuantity CR: ~w\n', [CR]),

	SketchQuantity = CR<<-argument(1),
        % is this necessary?    
	CR?arg2->>addSketchQuantity(SketchQuantity), %quantity-sketch_quantity relation
	
	% I think this can be removed, AB, Oct 2006
	%state goedzetten:
	% SketchQuantity->>delete_hypers(fragment),
	%en de nieuwe
	% SK->>hyper(SketchQuantity,givenElement,fragment),
		
	SketchQuantity->>name(CR?arg3), % was 4. AB, feb 2006
	SketchQuantity->>remarks(CR?arg4). % was 5. AB, feb 2006
%%
	
%%deleteSketchQuantity De meegestuurde instantie moet weg

checkChange_deleteSketchQuantity(SK,
			    CR: changeRequestor
			   ):->

	%We checken gewoon of we elementen hebben die naar de sketch_quantity verwijzen
	%die moeten dan ook weg. Maakt niet uit in welk fragment (kan ook in subs natuurlijk)

	SK?elements->>for_some(->>(@arg1,checkDeleteElement,CR?arg1,CR)).
%
applyChange_deleteSketchQuantity(SK,
			    CR: changeRequestor
			   ):->
	%de instantie wordt verwijderd

	SketchQuantity = CR<<-argument(1),
	SK?elements->>delete(SketchQuantity),
	CR->>freeObject(SketchQuantity).
%%


%%newSketchQuantity: er komt een quantity bij een instantie. 
checkChange_newSketchQuantity(SK,
			 CR : changeRequestor
			):->

	CR->>checkObject(SK),
	%check definities op @nil, en check bestaande sketch_quantities

	if
		@nil = CR<<-arg4
	then
		CR->>impossible('There is no definition for the new sketch_quantity selected',SK)
	else (
		%sketch_quantityspace?
		if
			@nil = CR<<-arg5
		then
				CR->>impossible('There is no sketch_quantity space selected for the new sketch_quantity',SK)
		%als er een qs gegeven is gaan we ervan uit dat hij bij de definition past
		else
			%laat alle sketch_quantities bij de sketch_quantity checken of het wel gaat
			%en eventueel zichzelf verwijderen via een subchange (of een impossible geven)
			CR?arg1?sketch_quantities->>for_some(
				->>(@arg1,checkChangedSketchQuantity,@nil,CR?arg2,CR?arg4,SK,CR))
		).

	
%%newSketchDependency: een prop, inf, of generic dep tussen 2 quantities,
%geen check ivm nieuw beleid (zo min mogelijk normen)

applyChange_newSketchDependency(SK,
	CR: changeRequestor):->
	%we maken de nieuwe relatie
	%gp3 0.2: we refer to a helper, so copyToNewElement calls from fragmentElements can also use this

	QR = SK<<-addNewSketchDependency(CR?arg1,CR?arg2,CR?arg3,CR?arg4,CR?arg5,
							CR?arg6,CR?arg7),
	CR->>result(QR).
%
addNewSketchDependency(SK, Type: {prop,inf,dep},
			Arg1: sketchQuantityElement, Arg1Route: chain,
			Arg2: sketchQuantityElement, Arg2Route: chain,
			Sign: {dep_plus, dep_min, inf_plus, inf_min, prop_plus, prop_min}, Remarks: string, QR):<-
	QR *= sketchDependencyElement(Sign, Type, Arg1, Arg1Route, Arg2, Arg2Route, Remarks, SK),
	SK?elements->>append(QR).
%%

%%changeSketchDependency:
%geen check ivm nieuw beleid (zo min mogelijk normen)

applyChange_changeSketchDependency(_SK,
	CR: changeRequestor):->
	
	QR = CR<<-arg1,
	QR->>type(CR?arg2),
	QR->>argument1(CR?arg3, CR?arg4),
	QR->>argument2(CR?arg5, CR?arg6),
	QR->>sign(CR?arg7),
	QR->>remarks(CR?arg8).
%%

%%deleteSketchDependency: geen check
applyChange_deleteSketchDependency(SK,
	CR: changeRequestor):->

	SK?elements->>delete(CR?arg1), %hypers blijven nog hangen
	CR->>freeObject(CR?arg1).
%%


%%
sortedSketchQuantities(SK, SketchQuantities):<-    
        get(SK?elements, find_all, message(@arg1, instance_of, sketchQuantityElement), 
	    SketchQuantities), 
	send(SketchQuantities, sort, ?(@arg1?name, compare, @arg2?name)).
%


%%
getSketchQuantityExportNames(SK, ExportTable: lookupTable):->
	%vul de ExportTable (zie export) met de namen van alle instanties
	%relevant of niet
	%we gaan ervanuit dat ze er nog niet instaan 
	%Bij de naamgeving wordt rekening gehouden met door parents/conditionals geexporteerde
	%sketch_quantities, en met identities.
	%dit werk wordt gedaan via onze hulp roundUpSketchQuantities
	
	AllSketchQuantities = SK<<-roundUpSketchQuantities,
	
	%dit is een lookupTable met als key tuple(sketch_quantity,route) en als value een secondaryID
	%alle entries met dezelfde ID krijgen dezelfde naam, en hoeven maar een keer
	%genoemd te worden

	
	OpKey *= lookupTable,
	AllSketchQuantities->>for_all(
		if(
			?(OpKey,member,@arg2),
			->>(?(OpKey,member,@arg2),
				append,
				@arg1),
			->>(OpKey,append,@arg2,create(chain,@arg1))
		)
	),
	AllSketchQuantities->>done,
		
	%en die lopen we af, hierbij is van belang dat voor elke secondaryID
	%de naam gebaseerd wordt op de meest lokale sketch_quantity
	
	
	OpKey->>for_all(
		->>(SK,getSketchQuantityExportNames_setIDName,@arg2,ExportTable)
	),
	OpKey->>done.
%
getSketchQuantityExportNames_setIDName(SK, Sketch_Quantities: chain, ExportTable: lookupTable):->
	%hulp van getSketchQuantityExportNames: bepaal een naam voor alle sketch_quantities in de chain
	%en zet ze allemaal in de ExportTable
	%de chain bevat tuples sketch_quantity-route. We baseren de naam op de naam van de
	%meest lokale
	
	%er zit er minstens 1 in de chain, anders was er nooit een id voor geweest
	
	Min *= number(Sketch_Quantities?head?second?size), %tot nu toe de minste grootte
	MostLocal *= var,
	MostLocal->>assign(Sketch_Quantities?head,global), %nu is deze even de meeste lokale
	Sketch_Quantities->>for_all(
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
	Sketch_Quantities->>for_all(
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
roundUpSketch_Quantities(SK,AllSketch_Quantities:lookupTable):<-
	%we vinden alle sketch_quantities die in het fragment een rol spelen
	%voor de naamgeving en de structurbeschrijving.
	%er wordt rekening gehouden met de door parents/conditionals geexporteerde
	%sketch_quantities, en met identities.
	%uiteindelijk komt er een lookupTable uit. Deze mapt
	%tuple(sketch_quantity,route) op een secondaryID
	%elke secondaryID moet vervolgens een naam krijgen, als we met naamgeving
	%bezig zijn. Meerdere sketch_quantities met dezelfde secondaryID zijn via identities
	%hetzelfde geworden: dus zelfde naam en maar 1 keer noemen
	
	AllSketch_Quantities *= lookupTable,
	
	%en de lokale instanties krijgen allemaal een eigen id
	?(SK,findElements,sketchQuantityElement)->>for_all(
		->>(AllSketch_Quantities,
			append,
			create(tuple,@arg1,create(chain)),
			create(secondaryID)
		)
	).
%%


:-pce_end_class.
		  
