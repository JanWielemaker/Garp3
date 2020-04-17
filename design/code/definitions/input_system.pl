/*
Definitie inputSystem class
*/

:-pce_begin_class(
		  inputSystem,
		  modelFragment,
		  "Description of an input system"
		 ).

%%
systemTypeName(_IS, Type: name):<-
	%overwrite van modelfragment
	Type = 'scenario'.
%%

%%
displayTypeName(_IS, Type: name):<-
	%gp3 display name is either scenario or model fragment
	Type = 'scenario'.
%%

%%
isInputSystem(_IS):->
	true.
%%

%%
currentType(_IS,
	Type: {staticFragment, processFragment, agentFragment,inputSystem}):<-
	%overwrite, we zijn altijd input system
	
	Type = inputSystem.
%%

%%	
type(_IS,
	Type: name
	):<-
	%overwrite

	Type *= name('Input system').
%

%%

%%
%relevantComments
%gp3 1.4: return a string with the remarks to show for this one
%when no remarks: return an empty string

relevantComments(D,RC: string):<-
	if
		0 = D?remarks<<-size
	then
		RC *= string
	else
		RC *= string('SC %s: %s', D?name,D?remarks).
%%

%%%%%%%%%%%%%%%%%%%%%%%CHANGES%%%%%%%%%%%%%%%%%%%

%alle changes die ook van toepassing zijn op MFs (waaronder newInputSystem e.d.)
%staan bij onze superklasse modelFragment

%%changeInputSystem: algemene check staat bij modelFragment
%(want naamcheck gaat over alle fragmenten + systems heen)
applyChange_changeInputSystem(IS,
	CR: changeRequestor):->
		%dit is wordt gewijzigd
		
		IS->>name(CR?arg1),
		IS->>remarks(CR?arg2).
%%

%%deleteInputSystem: het input system wordt verwijderd
checkChange_deleteInputSystem(IS,
	CR: changeRequestor):->
	
	%verwijderen is nooit een probleem, alleen waarschuwen we als er al inhoud is
	
	CR->>checkObject(IS),!,
	\+ IS?elements->>empty, %er is inhoud
	CR->>warning(string('Warning! You are about to delete the scenario "%s", which already has content.', IS?name),IS).
%
applyChange_deleteInputSystem(IS,
	CR: changeRequestor):->
	%onszelf verwijderen.

	@model->>removeDefinitionObject(IS),
	CR->>freeObject(IS).
%%

%%%%%%%%%%%%EXPORT%%%%%%%%%%%%%%%%

export(IS, F: file):->
	%gp3 0.1 This is homer code, but we added code to register the exported instance names
	%with the app, for cross reference in the simulator
	
	%gp3 0.3.16: we added code to account for quantity behaviour (system assumptions)
	%these result in extra given assumptions connected to the instance the quantity is connected to
	
	%schrijf dit inputsystem weg
	%LET OP: OVERWRITE VAN MODELFRAGMENT
	%zie daar voor wat meer commentaar mbt de ExportTable

	ExportTable *= lookupTable,
	
	if
		(\+ 0 = IS?remarks<<-size)
	then
		F->>format('\n%%%s\n%s\n', IS?name,?(IS?remarks,makeRemarks,0))
	else
		F->>format('%%%s\n',IS?name),

	EN = IS?name<<-exportName,
	F->>format('smd(input_system(\'%s\'),\n',EN),

	Counter *= number(1), %gp3 1.4, see comments in modelFragment->>export
	%%instances van entities en agents, assumptions
	IS->>exportSystemElements(consequence,1,ExportTable,F,@on,Counter), %alles hier is consequence, met nonvar instance names
	%gp3 0.1: remember the names used (input systems use the given name made lower case)
	%we just get them out of the ExportTable
	ExportTable->>for_all(
		->>(@app,addExportedObject,in,@arg1?first,IS,@arg2)),
		
	F->>append(',\n'),
	IS->>exportGivenParameters(1,ExportTable,F,Counter),
	F->>append(',\n'),
	IS->>exportValues(consequence,1,ExportTable,F,Counter),
	F->>append(',\n'),
	IS->>exportRelations(consequence,1,ExportTable,F,Counter),
	F->>append(',\n\tsystem_structures([])\n).\n\n').
%%

%%
exportInternalBehaviourFragments(IS, F: file):->
	%gp3 0.3.16. Called from the model when exporting, to export special modelfragments needed
	%for quantity behaviours defined in this inputSystem to the library file
	
	Quantities = IS<<-findElements(garpQuantity),
	Quantity *= var,
	Num *= number(1),
	Quantities->>for_all(
		and(
			assign(Quantity,@arg1,global),
			->>(Quantity?quantityAssumptions,for_all,
			->>(IS,exportInternalBehaviourFragment,Quantity,@arg1,Num,F)
			)
		)
	).
%
exportInternalBehaviourFragment(IS,Quantity: garpQuantity, Behaviour: name, Num: number, F: file):->
	%gp3 0.3.16 export an internal MF for this one
	
	QuantityExport = Quantity?name<<-exportName,
	Instance = Quantity<<-garpInstance,
	InstanceExport = @app<<-findExportedName(Instance,in,IS), %get the name used when creating this instance
	ISExport = IS?name<<-exportName,
	
	F->>format('\n%%Internal model fragment for %s behaviour of %s: %s in scenario %s\n',
		Behaviour, Instance?name, Quantity?name, IS?name),
		
	%this line starts the MF structure and exports some stuff that should in all cases be present in the conditions:
	%1) Check for the right scenario, 2) The behaviour assumption, 3) The instance this applies to (as an atom, same name as used when created in the input system)
	
	F->>format('system_structures(garp_internal_%s_%s_%u(%s),\n\tisa([static]),\n\tconditions([\n\t\tsystem_elements([\n\t\t\tinstance(garp_scenario_%s,garp_internal),\n\t\t\tinstance(Behaviour,%s),\n\t\t\tinstance(%s,%s),\n\t\t\thas_attribute(%s,has_assumption,Behaviour)\n\t\t\t]),\n\t\tparameters([\n'
			,ISExport,Behaviour,Num,InstanceExport,ISExport,Behaviour,InstanceExport,Instance?entity?name?exportName,InstanceExport),
	
	%behaviours that should be used on a conditional quantity:
	%we can use a static varname for the quantity, because it will be the only one inhere
	if
		member(Behaviour,[constant,generate_all_values])
	then
		F->>format('\t\t\t%s(%s,Quantity,_,%s)\n',
			QuantityExport,InstanceExport, Quantity?quantitySpace?name?exportName),
			
	%next general part
	F->>append('\t\t\t]),\n\t\tpar_values([]),\n\t\tpar_relations([]),\n\t\tsystem_structures([])\n\t]),\n\tgivens([\n\t\tsystem_elements([]),\n\t\tparameters([\n'),
	
	%behaviours that should be uses on a given quantity
	%again, we use the same static varname for the quantity, because in the whole MF there will only be
	%one quantity
	
	if
	    (
		design:exogenous_behaviours(Behaviours), % new JL May 2009, made exogenous behaviours explicit
		member(Behaviour,Behaviours) % new FL june 07
	    ) 
	then
		F->>format('\t\t\t%s(%s,Quantity,_,%s)\n',
			QuantityExport,InstanceExport, Quantity?quantitySpace?name?exportName),
	
	%next general part
	F->>append('\t\t\t]),\n\t\tpar_values([\n'),
	
	%for constant, we output the par_values that were given in the scenario again...
	if
		Behaviour == constant
	then
	(
		%values. All values defined with our quantity can be used here, because scenarios cannot be imported
		%in other model fragments: all values are local. There are maximum 2 values (normal + derivative)
		%we have to find our own version of the exportstring for values
		Values = Quantity<<-values,
		(
			QVal = Values<<-find(@arg1?derivative == @off)
		;
			QVal = @nil
		),
		(
			DVal = Values<<-find(@arg1?derivative == @on)
		;
			DVal = @nil
		),
		
		unless
			(QVal == @nil, DVal == @nil)
		do
			F->>format('\t\t\tvalue(Quantity,_,%s,%s)\n',
				when(
					QVal == @nil,
					'_',
					?(IS,exportInternalBehaviourFragment_Value,QVal?valueReference)
				),
				when(
					DVal == @nil,
					'_',
					?(IS,exportInternalBehaviourFragment_Value,DVal?valueReference)
				)
			)
	),
	%next general part
	F->>append('\t\t\t]),\n\t\tpar_relations([\n'),
	
	%for constant, we output the 'local' par_relations the were given in the scenario
	%again we need a helper which is a special version of the getting the right exportstring
	%(less variables)
	if
		Behaviour == constant
	then
	(
		%get all relevant inequalities: local only (no calculi)
		Relations = ?(Quantity,relations,inequality)<<-find_all(
			and(@arg1?argument1 == Quantity, @arg1?argument2 == Quantity)
		),
		unless
			Relations->>empty
		do
		(
			RC *= var,
			RC->>assign('',global), %comma
			Relations->>for_all(
				and(
					->>(F, format,'%s\t\t\t%s(%s,%s)',
						RC,
						?(IS,exportInequalityType,@arg1), %same call as normal MF export
						?(IS,exportInternalBehaviourFragment_IneqArg,@arg1?argument1Type,@arg1?argument1QSPoint),
						?(IS,exportInternalBehaviourFragment_IneqArg,@arg1?argument2Type,@arg1?argument2QSPoint)
						),
					assign(RC,',\n')
				)
			),
			F->>append('\n')
		)
	),

		
	%next general part
	F->>append(']),\n\t\tsystem_structures([])\n\t])\n).\n'),
	Num->>plus(1). %inc counter to make next MF-name unique
%
exportInternalBehaviourFragment_Value(_IS,VR: valueReference,ValueString: string):<-
	%helper for exportInternalBehaviourFragment: get the right string description for a value
	%of the one quantity in the internal mf exports (where quantity is allways Quantity)
	
	%code like modelFragment<<-exportQSValue, but without getting the quantity-name
	if
	(
		interval = VR<<-type
	;
	    get(VR, valueName, ValueName),
	    member(ValueName, ['Zero', 'One']) % 'Minusone'
	)
	then
		ValueString = VR?valueName?exportName
	else
		ValueString *= string('%s(Quantity)',VR?valueName?exportName).
%
exportInternalBehaviourFragment_IneqArg(IS, ArgType: {currentValue,currentDerivative,quantityQSPoint,derivativeZero},
	ArgPoint: 'valueReference*',Arg: string):<-
	%helper for exportInternalBehaviourFragment: get the right string description for a inequality argument
	%in the one quantity in the internal mf exports (where quantity is allways Quantity)
	%this must be local and no calculi
	
	if
		(ArgType == currentValue ; ArgType == currentDerivative)
	then
		Arg *= string('Quantity'),
	if
		ArgType == quantityQSPoint
	then
		Arg = IS<<-exportInternalBehaviourFragment_Value(ArgPoint), %same as for values
	if
		ArgType == derivativeZero
	then
		Arg *= string('zero').
%%

%%
exportText(IS,F: file):->
	%gp3 1.4: added for textual export, just a rewrite of the normal export code
	
	%just output the scenario and the conditions etc
	
	F->>format('\nScenario: %s\n',IS?name),
	if
		(\+ 0 = IS?remarks<<-size)
	then
		F->>format('Remarks:\n%s\n', ?(IS?remarks,tab,1)),
	IS->>exportSystemElements_text(consequence,1,F),
	IS->>exportQuantities_text(consequence,1,F),
	IS->>exportValues_text(consequence,1,F),
	IS->>exportRelations_text(consequence,1,F).
%%
	
:-pce_end_class.
