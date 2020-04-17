/*
Definitie sketchInequalityElement class
*/

:-pce_begin_class(
		  sketchInequalityElement(argument1,type,argument2,argument2Type,remarks),
		  % parRelation,
		  sketchGenericRelationElement,
		  "definition of sketch inequality relation in sketch"
		 ).

% variable(arg1, sketchQuantityElement,both,"First argument").
variable(type,{l,leq,eq,geq,g},both,"Type of inequality").
% variable(argument2, 'sketchValueElement | sketchQuantityElement', both, "Second argument").
% variable(arg2, sketchElement, both, "Second argument").
variable(argument2Type,{sketchQuantityElement,sketchValueElement},both,"Type of second argument").
% variable(remarks,string,both,"Remarks").
variable(displayNameString, string, both,"Display Name String").


%%



%%
initialise(I,
	Arg1 : arg1 = 'sketchQuantityElement',
	% Arg1 is always of type sketchQuantityElement
        % Arg1Route: chain,
	Type: type = {l,leq,eq,geq,g},
	Arg2 : arg2 = 'sketchValueElement | sketchQuantityElement',
	Arg2Type: argument2Type = 
		{sketchValueElement,sketchQuantityElement},
        % Arg2Route: chain,
	Remarks: string
        ):->

        new(C, chain), 
        send(C, clear), 
	I->+initialise(Arg1, C, Arg2, C, Remarks), % C = empty route? 
	I->>type(Type),
	I->>slot(type,Type),
	I->>argument2Type(Arg2Type),
	I->>slot(argument2Type,Arg2Type), 
        I->>displayNameString(I?createDisplayNameString).
%%



%%
updateSketchIneqDisplayName(I, Arg: sketchElement):->
        get(I?displayNameString, value, IName), 
        get(Arg, name, ArgName), 
        debug(sketch(stg), 'updateSketchIneqDisplayName for element: ~w and Arg: ~w',[IName, ArgName]),
        if ( 
	     (I?argument1->>equal(Arg)) 
	     ;
	     (I?argument2->>equal(Arg)) 
	   )
        then 
           (
	     I->>displayNameString(I?createDisplayNameString)
           ). 
%%


%%
extract_quantities(I, Quantities: chain):<-
        new(Quantities, chain), 
        get(I, argument1, LHS_Q), 
        send(Quantities, append, LHS_Q),
        get(I, argument2, RHS), 
        if
           (send(I?argument2Type, equal, sketchQuantityElement))
        then
           send(Quantities, append, RHS).
%%


%
does_not_involve_quantity(I, Q: sketchQuantityElement):->
        get(I, extract_quantities, Quantities), 
        if 
          Quantities->>member(Q)
        then         
          fail
        else
          true.
%%


%
does_not_involve_value(I, V: sketchValueElement):->
        get(I, extract_values, Values), 
        if 
          Values->>member(V)
        then         
          fail
        else
          true.
%%


%%
extract_values(I, Values: chain):<-
        new(Values, chain), 
        get(I, argument2, RHS), 
        if
           (send(I?argument2Type, equal, sketchValueElement))
        then
           send(Values, append, RHS).
%%
  
      
%%
swappedType(I,
	Type : '{l,leq,eq,geq,g}'):<-
	%geeft het type geswapt terug

	CurType = I<<-type,
	pl_swapType_INE(CurType,Type).
%
pl_swapType_INE(l,g).
pl_swapType_INE(leq,geq).
pl_swapType_INE(geq,leq).
pl_swapType_INE(g,l).
pl_swapType_INE(eq,eq).
%%

%%
typeName(I,
	Type: name):<-
	%gp3 0.2
	%give a natlang description of the type
	
	CurType = I<<-type,
	pl_typeName(CurType,Type).
%
swappedTypeName(I,
	Type: name):<-
	%gp3 0.2
	%like typeName, but give the name for the swapped type
	
	CurType = I<<-swappedType,
	pl_typeName(CurType,Type).
%
pl_typeName(l,'smaller').
pl_typeName(leq, 'smaller or equal').
pl_typeName(geq, 'greater or equal').
pl_typeName(g, 'greater').
pl_typeName(eq, 'equal').
%%

createDisplayNameString(I, IneqStr: name):<-
        get(I, argument1, FirstObj), 
        get(I, argument2, SecondObj), 
        get(FirstObj, name, First),   
        get(SecondObj, name, Second),   
        get(I, type, Type), 
        symbol_typeName(Type, Symbol), 
        swritef(IneqStr, '%p %p %p', [First, Symbol, Second]).


%%
checkDeleteElement(I,
	Element: fragmentElement,
	CR: changeRequestor):->
	%overwrite van fragmentElement: check onze argumenten
	if
		(
			Element = I<<-argument1
		;
			I?argument1Route->>member(Element)
		;
			Element = I<<-argument2
		;
			I?argument2Route->>member(Element)
		)
	then
	(
		@pce->>write_ln(I),
		CR->>addChangeRequest(changeRequestor(deleteInequality,
								I?fragment,
								@default,
								CR?garpModel,
								I),
			string('A sketch inequality in this %s will also be deleted because one of its arguments is no longer part of the %s',I?fragment?displayTypeName, I?fragment?displayTypeName),
			I?fragment)
	).
%%

%%
copyToNewElement(FE,Mappings: hash_table, NewMF: modelFragment, New: inequality):<-
	%gp3 0.2 Copy contents and relations (hypers) to a new assumptionInstance
	%see fragmentElement<<-copyToNewElement for details
	%we fail if required connected objects are missing from the Mappings table
	
	%lets see if the needed other elements are allready copied
	debug(sketch(stg), 'copyToNewElement: Is this used?',[]), 
	NewArg1Info = NewMF<<-copyMF_mapRelatedElement(FE?argument1,FE?argument1Route,Mappings),
	NewArg2Info = NewMF<<-copyMF_mapRelatedElement(FE?argument2,FE?argument2Route,Mappings),
		%fails when these ones are not available yet, so will try again later
		
	if
		@nil = FE<<-argument1QSPoint
	then
		NewArgument1QSPoint = @nil
	else
		NewArgument1QSPoint = FE?argument1QSPoint<<-copy,
	if
		@nil = FE<<-argument2QSPoint
	then
		NewArgument2QSPoint = @nil
	else
		NewArgument2QSPoint = FE?argument2QSPoint<<-copy,
		
	%everything checked, we can create the item
	New = NewMF<<-addNewInequality(FE?stateName, FE?type,
				NewArg1Info?first, NewArg1Info?second, FE?argument1Type, NewArgument1QSPoint,
				NewArg2Info?first, NewArg2Info?second, FE?argument2Type, NewArgument2QSPoint,
				FE?remarks).
%%



%%
copySketchInequality(FE, New: sketchInequalityElement):<-
	% simplified version, AB, june 2006
	debug(sketch(stg), 'copySketchInequality: Is this used?',[]), 
        new(New, sketchInequalityElement(FE?argument1, FE?type, FE?argument2, FE?argument2Type, FE?remarks)).
%%



:-pce_end_class.
		  
