/*
Definitie stgraph class

based on sketch class (which is based on modelFragment), but with specialized stuff for stgraph
AB, feb 2006
modelFragment class was based on homer code, so most comments in dutch. gp3 code only where mentioned
*/

:-pce_begin_class(
		  stgraph(name),
		  sketch,
		  "Definition of stgraphs"
		 ).

variable(name,name,get,"Name of the object"). %altijd makeGarp
variable(remarks,string,none,"Remarks on this object").

variable(elements,chain,get,"List of subelements defined in this fragment").

variable(layOutTable, hash_table, get, "Hash_table for layoutinfo"). %eerste nivo
variable(sketchQuantities, chain, both, "The sketch quantities").
variable(sketchValues, chain, both, "The sketch values").
variable(active, bool := @on, both, "Modelfragment is used in simulation"). %gp3 0.3.11, modeldefinition 5
% variable(copy_quantities_from_CM_setting, bool := @on, both, "Copy quantities from Causal Model Setting"). 
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
        new(SQ, chain),
        send(SK, sketchQuantities, SQ),
        new(SV, chain),
        send(SK, sketchValues, SV).
%%



%
definitions(SK, Definitions: chain):<-
	"Return the definitions involved in this sketch" ::
        
        get(SK, sortedSketchQuantities, Definitions).
%%



/***********change requestors*****************/

%%newSketchState: een sketch_state in een modelfragment
%%%
%%Of een bepaald object aanwezig mag zijn in een bepaald type sketch wordt momenteel
%bepaald door de editor, en niet door de changeRequestors. Das ook wel zo handig, alhoewel
%de editor het misschien beter door kan sturen?

checkChange_newSketchState(SK,
		       CR: changeRequestor
		      ):->

	%de naam moet uniek zijn in het fragment waaraan het
	%wordt toegevoegd, maar niet in fragmenten waarin het
	%betreffende fragment wordt gebruikt
	
	if
	(
		CR->>checkObject(SK),
		NewName = CR<<-argument(3),
		?(SK,findElements,sketchStateElement)->>find(->>(@arg1?name,
				    equalExportName,
				    NewName))
	)
	then
		CR->>impossible('The given state name is already used in this behaviour graph').
%%


% this is defined twice - which is used? AB, june 2006
%%newSketchState: er komt een state bij een instantie. 
checkChange_newSketchState(SK,
			 CR : changeRequestor
			):->

	CR->>checkObject(SK),
	%check definities op @nil, en check bestaande sketch_states

	if
		@nil = CR<<-arg4
	then
		CR->>impossible('There is no definition for the new sketch_state selected',SK)
	else (
		%sketch_statespace?
		if
			@nil = CR<<-arg5
		then
				CR->>impossible('There is no sketch_state space selected for the new sketch_state',SK)
		%als er een qs gegeven is gaan we ervan uit dat hij bij de definition past
		else
			%laat alle sketch_states bij de sketch_state checken of het wel gaat
			%en eventueel zichzelf verwijderen via een subchange (of een impossible geven)
			CR?arg1?sketch_states->>for_some(
				->>(@arg1,checkChangedSketchState,@nil,CR?arg2,CR?arg4,SK,CR))
		).




%
applyChange_newSketchState(SK,
		       CR: changeRequestor
		      ):->
	%de instantie wordt aan dit fragment toegevoegd
	%gp3 0.2: we refer to a helper, so copyToNewElement calls from fragmentElements can also use this
	
	I = SK<<-addNewSketchState(CR?arg1,CR?arg2,CR?arg3,CR?arg4),
	CR->>result(I).

%
addNewSketchState(SK, SketchState: abstractEntity, Name: name, Remarks: string, Ineqs: chain, I: sketchStateElement):<-

	%gp3 0.2 Split off from applyChange, can now be called by (fragmentElement subclass)->copyToNewElement as well
	
	I *= sketchStateElement(Name,Remarks, SK),
        send(I, sketchInequalities, Ineqs?copy), 
	%zet de configuratieve informatie
	SketchState->>addSketchState(I),
	SK?elements->>append(I).
%%	

%%changeSketchState: Wijzigingen in de meegestuurde instantie.

checkChange_changeSketchState(SK,
			    CR: changeRequestor
			   ):->

	%het object checkt op naam uniciteit en geldige entiteitsselectie
	
	CR->>checkObject(SK),
	%er moet wel een entiteit geselecteerd zijn
	if
		@nil = CR<<-arg2
	then
		CR->>impossible('There is no sketch_state selected'),

	NewName = CR<<-argument(3), % was 4
	SketchState = CR<<-argument(1),

	if
                % NameVal is not a number
                % not( catch(number(NewName), _, fail) )
                not( catch(atom_number(NewName, _Nr), _, fail) )
	then
		CR->>impossible('The state name should be a unique number'),

	if
		?(SK,findElements,sketchStateElement)->>find(and(not(@arg1 == SketchState),
				    ->>(@arg1?name,
				      equalExportName,
					NewName)))
	then
		CR->>impossible('The given state name is already used in this behaviour graph').
	
	
%
applyChange_changeSketchState(_SK,
			    CR: changeRequestor
			   ):->
	%de gewijzigde instantie bevindt zich in dit fragment
	%dus moeten we de boel updaten

	SketchState = CR<<-argument(1),
	% SketchState?sketch_state->>removeSketchState(SketchState),
	SketchState?entity->>removeSketchState(SketchState),
	CR?arg2->>addSketchState(SketchState), %state-sketch_state relation
	  
        % replace sketchInequalities by the new ones
        % and throw away the old sketchInequalities that have been deleted
        debug(sketch(delete), 'applyChange_changeSketchState CR:~w', [CR]), 
        get(SketchState, sketchInequalities, OldIneqs), 
        get(CR?arg5, copy, NewIneqs), 
        send(OldIneqs, subtract, NewIneqs), 
        % OldIneqs now contains only the old sketchInequalities that have been deleted
        send(OldIneqs, for_all, ->>(@arg1, free)), 
        SketchState->>sketchInequalities(NewIneqs),

	SketchState->>name(CR?arg3), % was 4. AB, feb 2006
	SketchState->>remarks(CR?arg4). % was 5. AB, feb 2006
%%


%%copySketchState: based on changeSketchState and addNewSketchState    
%
checkChange_copySketchState(SK,
			    CR: changeRequestor
			   ):->

	%het object checkt op naam uniciteit en geldige entiteitsselectie
	
	CR->>checkObject(SK),
	%er moet wel een entiteit geselecteerd zijn
	if
		@nil = CR<<-arg2
	then
		CR->>impossible('There is no sketch_state selected'),

	NewName = CR<<-argument(3), % was 4
	SketchState = CR<<-argument(1),

	if
		?(SK,findElements,sketchStateElement)->>find(and(not(@arg1 == SketchState),
				    ->>(@arg1?name,
				      equalExportName,
					NewName)))
	then
		CR->>impossible('The given state name is already used in this behaviour graph').
	
	
%
applyChange_copySketchState(_SK,
			    CR: changeRequestor
			   ):->
	%de gewijzigde instantie bevindt zich in dit fragment
	%dus moeten we de boel updaten

	SketchState = CR<<-argument(1),
	% SketchState?sketch_state->>removeSketchState(SketchState),
	SketchState?entity->>removeSketchState(SketchState),
	CR?arg2->>addSketchState(SketchState), %state-sketch_state relation

	% I think this can be removed, AB, Oct 2006	
	%state goedzetten:
	% SketchState->>delete_hypers(fragment),
	%en de nieuwe
	% SK->>hyper(SketchState,givenElement,fragment),

        SketchState->>sketchInequalities(CR?arg5?copy), % AB, june 2006		
	SketchState->>name(CR?arg3), % was 4. AB, feb 2006
	SketchState->>remarks(CR?arg4). % was 5. AB, feb 2006
%%


	
%%deleteSketchState De meegestuurde instantie moet weg

checkChange_deleteSketchState(SK,
			    CR: changeRequestor
			   ):->

	%We checken gewoon of we elementen hebben die naar de sketch_state verwijzen
	%die moeten dan ook weg. Maakt niet uit in welk fragment (kan ook in subs natuurlijk)

	SK?elements->>for_some(->>(@arg1,checkDeleteElement,CR?arg1,CR)).
%
applyChange_deleteSketchState(SK,
			    CR: changeRequestor
			   ):->
	%de instantie wordt verwijderd

	SketchState = CR<<-argument(1),
	SK?elements->>delete(SketchState),
	CR->>freeObject(SketchState).
%%

	
%%newSketchTransition: een transition tussen 2 states,
%geen check ivm nieuw beleid (zo min mogelijk normen)

applyChange_newSketchTransition(SK,
	CR: changeRequestor):->
	%we maken de nieuwe relatie
	%gp3 0.2: we refer to a helper, so copyToNewElement calls from fragmentElements can also use this
        % removed CR?arg6 (type) & 7 (sign), AB, april 2006
	QR = SK<<-addNewSketchTransition(CR?arg1,CR?arg2,CR?arg3,CR?arg4,CR?arg5), 
	CR->>result(QR).
%
addNewSketchTransition(SK,              
			Arg1: sketchStateElement, Arg1Route: chain,
			Arg2: sketchStateElement, Arg2Route: chain,
			Remarks: string, QR):<-
	
	QR *= sketchTransitionElement(Arg1, Arg1Route, Arg2, Arg2Route, Remarks, SK),
	SK?elements->>append(QR).
	% I think this can be removed, AB, Oct 2006
	% SK->>hyper(QR,givenElement,fragment).
%%

%%changeSketchTransition:
%geen check ivm nieuw beleid (zo min mogelijk normen)

applyChange_changeSketchTransition(_SK,
	CR: changeRequestor):->
	
	QR = CR<<-arg1,
	QR->>argument1(CR?arg2, CR?arg3),
	QR->>argument2(CR?arg4, CR?arg5),
	QR->>remarks(CR?arg6).
%%

%%deleteSketchTransition: geen check
applyChange_deleteSketchTransition(SK,
	CR: changeRequestor):->

	SK?elements->>delete(CR?arg1), %hypers blijven nog hangen
	CR->>freeObject(CR?arg1).
%%



	
%%newSketchInequality: a sketch inequality between 2 quantities,
%geen check ivm nieuw beleid (zo min mogelijk normen)

applyChange_newSketchInequality(SK,
	CR: changeRequestor):->
	%we maken de nieuwe relatie
	%gp3 0.2: we refer to a helper, so copyToNewElement calls from fragmentElements can also use this
	QR = SK<<-addNewSketchInequality(CR?arg1,CR?arg2,CR?arg3,CR?arg4,CR?arg5,CR?arg6),
	CR->>result(QR).
%
addNewSketchInequality(SK, Type: {greater, equal, smaller},
			Arg1: sketchQuantityElement, Arg1Route: chain,
			Arg2: sketchQuantityElement, Arg2Route: chain,
			Remarks: string, QR):<-
	
	% is this code active? AB, April 2007
        debug(sketch(delete), 'addNewSketchInequality in stgraph.pl', []), 
	QR *= sketchInequalityElement(Type, Arg1, Arg1Route, Arg2, Arg2Route, Remarks),
	SK?elements->>append(QR).
%%

%%changeSketchInequality:
%geen check ivm nieuw beleid (zo min mogelijk normen)

applyChange_changeSketchInequality(_SK,
	CR: changeRequestor):->
	% is this code active? AB, April 2007
        debug(sketch(delete), 'applyChange_changeSketchInequality in stgraph.pl CR: ~w', [CR]), 
	QR = CR<<-arg1,
	QR->>type(CR?arg2),
	QR->>argument1(CR?arg3, CR?arg4),
	QR->>argument2(CR?arg5, CR?arg6),
	QR->>remarks(CR?arg7).
%%

%%deleteSketchInequality: geen check
applyChange_deleteSketchInequality(SK,
	CR: changeRequestor):->

	% is this code active? AB, April 2007
        debug(sketch(delete), 'applyChange_deleteSketchInequality in stgraph.pl CR: ~w', [CR]), 
        % this is the important one
        SK?sketchInequalities->>delete(CR?arg1),
        % is this also necessary?
	SK?elements->>delete(CR?arg1), %hypers blijven nog hangen
	CR->>freeObject(CR?arg1).
%%


%%
getSketchStateExportNames(SK, ExportTable: lookupTable):->
	%vul de ExportTable (zie export) met de namen van alle instanties
	%relevant of niet
	%we gaan ervanuit dat ze er nog niet instaan 
	%Bij de naamgeving wordt rekening gehouden met door parents/conditionals geexporteerde
	%sketch_states, en met identities.
	%dit werk wordt gedaan via onze hulp roundUpSketchStates
	
	AllSketchStates = SK<<-roundUpSketchStates,
	
	%dit is een lookupTable met als key tuple(sketch_state,route) en als value een secondaryID
	%alle entries met dezelfde ID krijgen dezelfde naam, en hoeven maar een keer
	%genoemd te worden

	
	OpKey *= lookupTable,
	AllSketchStates->>for_all(
		if(
			?(OpKey,member,@arg2),
			->>(?(OpKey,member,@arg2),
				append,
				@arg1),
			->>(OpKey,append,@arg2,create(chain,@arg1))
		)
	),
	AllSketchStates->>done,
		
	%en die lopen we af, hierbij is van belang dat voor elke secondaryID
	%de naam gebaseerd wordt op de meest lokale sketch_state
	
	
	OpKey->>for_all(
		->>(SK,getSketchStateExportNames_setIDName,@arg2,ExportTable)
	),
	OpKey->>done.
%
getSketchStateExportNames_setIDName(SK, Sketch_States: chain, ExportTable: lookupTable):->
	%hulp van getSketchStateExportNames: bepaal een naam voor alle sketch_states in de chain
	%en zet ze allemaal in de ExportTable
	%de chain bevat tuples sketch_state-route. We baseren de naam op de naam van de
	%meest lokale
	
	%er zit er minstens 1 in de chain, anders was er nooit een id voor geweest
	
	Min *= number(Sketch_States?head?second?size), %tot nu toe de minste grootte
	MostLocal *= var,
	MostLocal->>assign(Sketch_States?head,global), %nu is deze even de meeste lokale
	Sketch_States->>for_all(
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
	Sketch_States->>for_all(
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


%
roundUpSketch_States(SK,AllSketch_States:lookupTable):<-
	%we vinden alle sketch_states die in het fragment een rol spelen
	%voor de naamgeving en de structurbeschrijving.
	%er wordt rekening gehouden met de door parents/conditionals geexporteerde
	%sketch_states, en met identities.
	%uiteindelijk komt er een lookupTable uit. Deze mapt
	%tuple(sketch_state,route) op een secondaryID
	%elke secondaryID moet vervolgens een naam krijgen, als we met naamgeving
	%bezig zijn. Meerdere sketch_states met dezelfde secondaryID zijn via identities
	%hetzelfde geworden: dus zelfde naam en maar 1 keer noemen
	
	AllSketch_States *= lookupTable,
	
	%en de lokale instanties krijgen allemaal een eigen id
	?(SK,findElements,sketchStateElement)->>for_all(
		->>(AllSketch_States,
			append,
			create(tuple,@arg1,create(chain)),
			create(secondaryID)
		)
	).	
%%

%%
sortedSketchQuantities(SK, SketchQuantities):<-                
        get(SK, sketchQuantities, SketchQuantities), 
        % add the sketch quantities from the causal model sketch if desired
        if 
          @on = @app<<-setting(import_quantities_from_causal_model_to_stgraph)
        then 
         (
          get(@model, hypered, causalModelMF, CM), 
          get(CM, sortedSketchQuantities, SketchQuantitiesCM), 
	  % create copies of these quantities, so that they can be edited independently
	  get(SK, copy_quantities, SketchQuantitiesCM, SketchQuantities, QuantityCopies), 
          send(SketchQuantities, union, QuantityCopies)
         )
        else 
         (
          true % do nothing
        ),
        % sort
	send(SketchQuantities, sort, ?(@arg1?name, compare, @arg2?name)).
%%


%
copy_quantities(SK, SketchQuantitiesCM: chain, SketchQuantities: chain, QuantityCopies: chain):<-
        debug(sketch(stg), 'Copying quantities from Causal model to Behaviour graph editor... \n',[]),
        new(QuantityCopies, chain), 
        chain_list(SketchQuantitiesCM, SketchQuantitiesCMList), 
        forall(member(Q, SketchQuantitiesCMList), 
	       (
		  % if not already present
		  if 
		    (
		     pl_quantity_already_present(Q, SketchQuantities)
		    )
		  then
		     % do nothing - a Quantity with this name was already present
		     true
		  else
		    (
		     new(Mappings, hash_table), 
		     get(Q, copyToNewElement, Mappings, SK, Copy), 	
	             send(QuantityCopies, append, Copy)
		    )
	       )
	).
%%


%
copy_quantities_simple(SK, NewSK: stgraph, QuantityCopies: chain):<-       
        debug(sketch(save), 'Copying quantities simple... ~w\n', [SK]),
        new(QuantityCopies, chain), 
        get(SK, sketchQuantities, SketchQuantities), 
        chain_list(SketchQuantities, SketchQuantitiesList), 
        forall(member(Q, SketchQuantitiesList), 
	    (
	     new(Mappings, hash_table), 
	     get(Q, copyToNewElement, Mappings, NewSK, Copy), 	
             send(QuantityCopies, append, Copy)
	    )
	).
%%


%
copy_values(SK, NewSK: stgraph, ValueCopies: chain):<-
        debug(sketch(save), 'Copying values... ~w\n', [SK]),
        % trace,
        new(ValueCopies, chain), 
        get(SK, sketchValues, SketchValues), 
        chain_list(SketchValues, SketchValuesList), 
        forall(member(V, SketchValuesList), 
	    (
	     new(Mappings, hash_table), 
	     get(V, copyToNewElement, Mappings, NewSK, Copy), 	
             send(ValueCopies, append, Copy)
            )
	).
%%

%
copySketchStateInequalities(S, IneqsOrg: chain, NewIneqs: chain):<-
	% S is the new Sketch
        debug(sketch(save), 'copySketchStateInequalities...\n',[]), 
        get(S, sketchQuantities, Quantities),
        get(S, sketchValues, Values),
        new(NewIneqs, chain), 
        % create a copy of each Ineq in IneqsOrg
        chain_list(IneqsOrg, IneqsOrgList), 
        forall(member(Ineq, IneqsOrgList), 
	 (
	   % get NewArg1 (quantity in S) for LHS
	   get(Quantities, find, ->>(@arg1?name, equalExportName, Ineq?argument1?name), NewArg1), 
	   % get NewArg2 for RHS (this can be a quantity or a value in S)	   
	   if Ineq?argument2Type->>equal(sketchQuantityElement)
	   then
	   (
	      get(Quantities, find, ->>(@arg1?name, equalExportName, Ineq?argument2?name), NewArg2)
	   )
	   else
	   (
	      get(Values, find, ->>(@arg1?name, equalExportName, Ineq?argument2?name), NewArg2)
	   ),
	   % create NewIneq with this data
	   new(NewIneq, sketchInequalityElement(NewArg1, Ineq?type, NewArg2, Ineq?argument2Type, Ineq?remarks?copy)),
	   send(NewIneqs, append, NewIneq)
         )
        ).
%%


% pl_quantity_already_present(Q, SketchQuantities)
%
% Q is already present in SketchQuantities chain if it contains Q2 with the same name
%  
pl_quantity_already_present(Q, SketchQuantities):-
	get(Q, name, Qname), 
        chain_list(SketchQuantities, SketchQuantitiesList), 
	member(Q2, SketchQuantitiesList), 
        Q2?name->>equal(Qname).
%%
	
	
% still used in versionpatch!
%
pl_extract_quantities(Ineqs, AllQuantities):-
        new(AllQuantities, chain), 
        Ineqs->>for_all(
	  % extract quantities from each Inequality and add them if necessary
          message(AllQuantities, union, @arg1?extract_quantities)       
	).
%%


%%
sortedSketchValues(SK, SketchValues):<-    
        get(SK, sketchValues, SketchValues),
	send(SketchValues, sort, ?(@arg1?name, compare, @arg2?name)).
%%


%%
pl_extract_values(Ineqs, AllValues):-
        new(AllValues, chain), 
        Ineqs->>for_all(
	  % extract values from each Inequality and add them if necessary
          message(AllValues, union, @arg1?extract_values)       
	).
%%




/***********change requestors*****************/

%%newSketchValue: een sketch_value in een modelfragment
%%%
%%Of een bepaald object aanwezig mag zijn in een bepaald type sketch wordt momenteel
%bepaald door de editor, en niet door de changeRequestors. Das ook wel zo handig, alhoewel
%de editor het misschien beter door kan sturen?

checkChange_newSketchValue(SK,
		       CR: changeRequestor
		      ):->

	%de naam moet uniek zijn in het fragment waaraan het
	%wordt toegevoegd, maar niet in fragmenten waarin het
	%betreffende fragment wordt gebruikt
	
	%er moet wel een entiteit geselecteerd zijn
	if
	(
		CR->>checkObject(SK),
		@nil = CR<<-arg1
	)
	then
		CR->>impossible('There is no entity? or state? selected'),
	if
	(
		CR->>checkObject(SK),
		NewName = CR<<-argument(3),
		?(SK,findElements,sketchValueElement)->>find(->>(@arg1?name,
				    equalExportName,
				    NewName))
	)
	then
		CR->>impossible('The given name is already used in this fragment').

%
applyChange_newSketchValue(SK,
		       CR: changeRequestor
		      ):->
	%de instantie wordt aan dit fragment toegevoegd
	%gp3 0.2: we refer to a helper, so copyToNewElement calls from fragmentElements can also use this
	
	get(CR, arg1, A1),     
	get(CR, arg2, A2), 
	get(CR, arg3, A3), 
    
        debug(sketch(save), 'CR arg1: ~w, arg2: ~w, arg3: ~w\n',[A1,A2,A3]), 
	I = SK<<-addNewSketchValue(CR?arg1,CR?arg2,CR?arg3),
	CR->>result(I).


%
addNewSketchValue(SK, Name: name, Remarks: string, I: sketchValueElement):<-	
        debug(sketch(save), 'addNewSketchValue to sketch: ~w\n', [SK]), 
	I *= sketchValueElement(Name, Remarks, SK),
	SK?sketchValues->>append(I).
	% SK?elements->>append(I).
%%	



%%changeSketchValue: Wijzigingen in de meegestuurde instantie.

checkChange_changeSketchValue(SK,
			    CR: changeRequestor
			   ):->

	%het object checkt op naam uniciteit en geldige entiteitsselectie
	
	CR->>checkObject(SK),
	%er moet wel een entiteit geselecteerd zijn
	if
		@nil = CR<<-arg2
	then
		CR->>impossible('There is no sketch_value selected'),

	NewName = CR<<-argument(3), % was 4
	SketchValue = CR<<-argument(1),

        % To Do: check if here sketchValues should be used instead 
	if
		?(SK,findElements,sketchValueElement)->>find(and(not(@arg1 == SketchValue),
				    ->>(@arg1?name,
				      equalExportName,
					NewName)))
	then
		CR->>impossible('The given name is already used in this fragment').
	
	
%
applyChange_changeSketchValue(_SK,
			    CR: changeRequestor
			   ):->
	%de gewijzigde instantie bevindt zich in dit fragment
	%dus moeten we de boel updaten

	SketchValue = CR<<-argument(1),
	% SketchValue?sketch_value->>removeSketchValue(SketchValue),
	% SketchValue?entity->>removeSketchValue(SketchValue),
	% CR?arg2->>addSketchValue(SketchValue), %value-sketch_value relation
	
	% I think this can be removed, AB, Oct 2006
	%state goedzetten:
	% SketchValue->>delete_hypers(fragment),
	%en de nieuwe
	% SK->>hyper(SketchValue,givenElement,fragment),
		
	SketchValue->>name(CR?arg3), % was 4. AB, feb 2006
	SketchValue->>remarks(CR?arg4). % was 5. AB, feb 2006
%%
	
%%deleteSketchValue De meegestuurde instantie moet weg

checkChange_deleteSketchValue(SK,
			    CR: changeRequestor
			   ):->

	%We checken gewoon of we elementen hebben die naar de sketch_value verwijzen
	%die moeten dan ook weg. Maakt niet uit in welk fragment (kan ook in subs natuurlijk)

	SK?elements->>for_some(->>(@arg1,checkDeleteElement,CR?arg1,CR)).
%
applyChange_deleteSketchValue(SK,
			    CR: changeRequestor
			   ):->
	%de instantie wordt verwijderd

	SketchValue = CR<<-argument(1),
	SK?elements->>delete(SketchValue),
	CR->>freeObject(SketchValue).
%%


%%newSketchValue: er komt een value bij een instantie. 
checkChange_newSketchValue(SK,
			 CR : changeRequestor
			):->

	CR->>checkObject(SK),
	%check definities op @nil, en check bestaande sketch_values

	if
		@nil = CR<<-arg4
	then
		CR->>impossible('There is no definition for the new sketch_value selected',SK)
	else (
		%sketch_valuespace?
		if
			@nil = CR<<-arg5
		then
				CR->>impossible('There is no sketch_value space selected for the new sketch_value',SK)
		%als er een qs gegeven is gaan we ervan uit dat hij bij de definition past
		else
			%laat alle sketch_values bij de sketch_value checken of het wel gaat
			%en eventueel zichzelf verwijderen via een subchange (of een impossible geven)
			CR?arg1?sketch_values->>for_some(
				->>(@arg1,checkChangedSketchValue,@nil,CR?arg2,CR?arg4,SK,CR))
		).




% Quantities

% I don't understand why this wasn't necessary before - AB, 9 jan 2007
%
addNewSketchQuantity(SK, Name: name, Remarks: string,I: sketchQuantityElement):<-
        debug(sketch(save), 'addNewSketchQuantity to sketch: ~w\n', [SK]), 
	I *= sketchQuantityElement(Name,Remarks, SK),
	SK?sketchQuantities->>append(I).
	% SK?elements->>append(I).
	%zet de configuratieve informatie
	% SketchQuantity->>addSketchQuantity(I).
%%	


:-pce_end_class.
		  
