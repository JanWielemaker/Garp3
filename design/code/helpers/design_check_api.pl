/************ Model Information API *********************/

%%
getTopEntity(Model, TopEntity) :-
    get(Model, hypered, 'topEntity', TopEntity).

getTopAgent(Model, TopAgent) :-
    get(Model, hypered, 'topAgent', TopAgent).

getTopAssumption(Model, TopAssumption) :-
    get(Model, hypered, 'topAssumption', TopAssumption).

getTopStaticFragment(Model, TopStaticFragment) :-
    get(Model, hypered, 'topStaticFragment', TopStaticFragment).

getTopProcessFragment(Model, TopProcessFragment) :-
    get(Model, hypered, 'topProcessFragment', TopProcessFragment).

getTopAgentFragment(Model, TopAgentFragment) :-
    get(Model, hypered, 'topAgentFragment', TopAgentFragment).

getTopAbstractEntity(Model, Type, AbstractEntity) :-
    (
	Type = 'Entity' ->
	getTopEntity(Model, AbstractEntity)
    ;
	Type = 'Agent' ->
	getTopAgent(Model, AbstractEntity)
    ;
	Type = 'Assumption' ->
	getTopAssumption(Model, AbstractEntity)
    ).


isPredefinedAbstractEntity(Model, AbstractEntity) :-
    (
	getTopEntity(Model, AbstractEntity)
    ;
	getTopAgent(Model, AbstractEntity)
    ;
	getTopAssumption(Model, AbstractEntity)
    ).

isPredefinedModelFragment(ModelFragment) :-
    definitionBelongsToModel(ModelFragment, Model),
    (
	getTopStaticFragment(Model, ModelFragment)
    ;
	getTopProcessFragment(Model, ModelFragment)
    ;
	getTopAgentFragment(Model, ModelFragment)
    ).
%% 


%%
getAllEntityParents(Entity, Parents) :-
    new(Parents, chain),
    getAllEntityParents2(Entity, Parents).
    
getAllEntityParents2(Entity, _Parents) :-
    not(get(Entity, parent, _Parent)).

getAllEntityParents2(Entity, Parents) :-
    get(Entity, parent, Parent),
    send(Parents, append, Parent),
    getAllEntityParents2(Parent, Parents).
%%

/* Returns a complete list of all the abstract entities ordered by depth */
completeEntityList(Model, Type, AllEntities) :-
    getTopAbstractEntity(Model, Type, Entity),
    chain_list(AllEntities, [Entity]),
    %new(AllEntities, chain),
    get(Entity?children, copy, Layer),
    completeEntityList2(Model, Layer, AllEntities).
    %chain_list(AllEntities, AllEntitiesList),
    %format('The complete list is: ~w\n', [AllEntitiesList]).

completeEntityList2(_Model, Layer, _AllEntities) :-
    send(Layer, empty).

completeEntityList2(Model, Layer, AllEntities) :-
    send(Layer, for_all,
	message(AllEntities, append, @arg1)
    ),
    new(NextLayer, chain),
    send(Layer, for_all, 
	message(NextLayer, union, @arg1?children)
    ),
    send(Layer, clear),
    completeEntityList2(Model, NextLayer, AllEntities).

getAllScenarios(Model, AllScenarios) :-
    new(AllScenarios, chain),
    send(Model?modelFragments, for_all,
	if(
	    message(@prolog, ==, @arg1?class_name, 'inputSystem'), 
	    message(AllScenarios, append, @arg1),
	    message(@prolog, true)
	)
    ).


getAllModelFragmentsOfParents(Model, ModelFragmentParentList, AllModelFragmentsList) :-
    new(AllModelFragments, chain),
    chain_list(ModelFragmentParents, ModelFragmentParentList),
    get(Model?modelFragments, find_all, 
	message(ModelFragmentParents, member, @arg1?name?value),
	ModelFragments
    ),
    %chain_list(ModelFragments, ModelFragmentsList),
    % format('De modelfragmenten ~w\n', [ModelFragmentsList]),
    new(Layer, chain),
    send(ModelFragments, for_all,
	message(Layer, union, @arg1?children)
    ),
    getAllModelFragmentsOfParents2(Layer, AllModelFragments),
    chain_list(AllModelFragments, AllModelFragmentsList).


getAllModelFragmentsOfParents2(Layer, _AllModelFragments) :-
    send(Layer, empty).

getAllModelFragmentsOfParents2(Layer, AllModelFragments) :-
    send(Layer, for_all,
	message(AllModelFragments, append, @arg1)
    ),
    new(NextLayer, chain),
    send(Layer, for_all,
	message(NextLayer, union, @arg1?children)
    ),
    send(Layer, clear),
    getAllModelFragmentsOfParents2(NextLayer, AllModelFragments).


getRequiredModelFragments(ModelFragment, RequiredModelFragments) :-
    % All parents
    %getAllMFParents(ModelFragment, ModelFragmentParents),
    % All definitions of imported model fragments 
    getAllImportedModelFragmentsDefinitions(ModelFragment, ImportedModelFragmentDefinitions),
    % Get the parents of the imported model fragment definitions
    /*new(ParentsOfImportedModelFragmentDefinitions, chain),
    forall(
	member(ImportedModelFragmentDefinition, ImportedModelFragmentDefinitions),
	(
	    getAllMFParents(ImportedModelFragmentDefinition, ParentsOfImportedModelFragmentDefinition),
	    forall(
		member(P, ParentsOfImportedModelFragmentDefinition),
		send(ParentsOfImportedModelFragmentDefinitions, append, P)
	    )
	)
    ),
    chain_list(ParentsOfImportedModelFragmentDefinitions, ParentsOfImportedModelFragmentsDefinitionsList),
    flatten([ModelFragmentParents,ImportedModelFragmentDefinitions,ParentsOfImportedModelFragmentsDefinitionsList],
	RequiredModelFragmentsBag),
    */
    %chain_list(ImportedModelFragmentDefinitions, RequiredModelFragments2),
    deletePredefinedModelFragmentsList(ImportedModelFragmentDefinitions, RequiredModelFragments).

%%
getAllMFParents(ModelFragment, ParentsList) :-
    new(AllParents, chain),
    get(ModelFragment, parents, Parents),
    getAllMFParents2(Parents, AllParents),
    chain_list(AllParents, ParentsList).
    
getAllMFParents2(LayerParents, _Parents) :-
    send(LayerParents, empty).

getAllMFParents2(LayerParents, Parents) :-
    send(Parents, union, LayerParents),
    new(NextLayerParents, chain),
    send(LayerParents, for_all,
	message(NextLayerParents, union, @arg1?parents)
    ),
    getAllMFParents2(NextLayerParents, Parents).
%%

%%
getAllMFParentReferences(ModelFragment, ParentReferencesChain) :-
    new(ParentReferencesChain, chain),
    get(ModelFragment, parentReferences, ParentReferences),
    getAllMFParentReferences2(ParentReferences, ParentReferencesChain),
    % Delete IMF's of predefined model fragments 
    send(ParentReferencesChain, for_all,
	if(
	    message(@prolog, isPredefinedModelFragment, @arg1?referencedFragment),
	    message(ParentReferencesChain, delete, @arg1),
	    message(@prolog, true)
	)
    ).

getAllMFParentReferences2(ParentReferences, _ParentReferencesChain) :-
    send(ParentReferences, empty).
getAllMFParentReferences2(ParentReferences, ParentReferencesChain) :-
    send(ParentReferencesChain, union, ParentReferences),
    
    new(NextLayerParentReferences, chain),
    send(ParentReferences, for_all,
	message(NextLayerParentReferences, union, @arg1?referencedFragment?parentReferences)
    ), 
    getAllMFParentReferences2(NextLayerParentReferences, ParentReferencesChain).
%%

%% 
getAllImportedModelFragmentsDefinitions(ModelFragment, AllImportedModelFragmentDefinitionsList) :-
    new(AllImportedModelFragmentDefinitions, chain),
    chain_list(ModelFragmentChain, [ModelFragment]),
    getAllImportedModelFragmentsDefinitions2(ModelFragmentChain, AllImportedModelFragmentDefinitions),
    send(AllImportedModelFragmentDefinitions, delete, ModelFragment),
    send(AllImportedModelFragmentDefinitions, unique),
    chain_list(AllImportedModelFragmentDefinitions, AllImportedModelFragmentDefinitionsList).

getAllImportedModelFragmentsDefinitions2(ImportedModelFragmentsLayer, _AllImportedModelFragmentDefinitions) :-
    send(ImportedModelFragmentsLayer, empty).
getAllImportedModelFragmentsDefinitions2(ImportedModelFragmentsLayer, AllImportedModelFragmentDefinitions) :-
    send(AllImportedModelFragmentDefinitions, union, ImportedModelFragmentsLayer),
    new(NextImportedModelFragmentsDefinitionsLayer, chain),
    send(ImportedModelFragmentsLayer, for_all,
	message(@prolog, getImportedFragmentDefinitions, @arg1, NextImportedModelFragmentsDefinitionsLayer) 
    ),
    getAllImportedModelFragmentsDefinitions2(NextImportedModelFragmentsDefinitionsLayer, AllImportedModelFragmentDefinitions).

%%

%%
getImportedFragmentDefinitions(ModelFragment, ImportedModelFragmentDefinitions) :-
    new(ImportedModelFragments, chain),
    send(ModelFragment?elements, for_all, 
	if(
	    or(
		message(@prolog, ==, @arg1?class_name, 'importedFragment'),
		message(@prolog, ==, @arg1?class_name, 'fragmentRefiner')
	    ),
	    message(ImportedModelFragments, append, @arg1),
	    message(@prolog, true)
	)
    ),
    send(ImportedModelFragments, union, ModelFragment?parentReferences),
    get(ImportedModelFragments, map, @arg1?referencedFragment, ImportedModelFragmentDefinitions2),
    send(ImportedModelFragmentDefinitions, union, ImportedModelFragmentDefinitions2).
%%

%%
deletePredefinedModelFragmentsList(ModelFragmentsList, CleanedModelFragmentsList) :-
    chain_list(ModelFragmentsChain, ModelFragmentsList),
    deletePredefinedModelFragmentsChain(ModelFragmentsChain),
    chain_list(ModelFragmentsChain, CleanedModelFragmentsList).

deletePredefinedModelFragmentsChain(ModelFragmentsChain) :-
    send(ModelFragmentsChain, for_all,
	if(
	    message(@prolog, isPredefinedModelFragment, @arg1),
	    message(ModelFragmentsChain, delete, @arg1),
	    message(@prolog, true)
	)
    ).
%%

%%
definitionBelongsToModel(Definition, Model) :-
    getModels(Models),
    (	
	(
	    send(Definition, instance_of, abstractEntity) ;
	    send(Definition, instance_of, assumption)
	) ->
	get(Models, find,
	    message(@prolog, hasEntityDefinition, @arg1, Definition),
	    Model)
    ;
	send(Definition, instance_of, modelFragment) -> 
	get(Models, find,
	    message(@prolog, hasModelFragmentDefinition, @arg1, Definition),
	    Model)
    ;
	send(Definition, instance_of, configurationDefinition) ->
	get(Models, find,
	    message(@prolog, hasConfigurationDefinition, @arg1, Definition?name),
	    Model
	)
    ;
	send(Definition, instance_of, garpAttributeDefinition) ->
	get(Models, find,
	    message(@prolog, hasAttributeDefinition, @arg1, Definition?name),
	    Model
	)
    ;
	send(Definition, instance_of, garpQuantityDefinition) ->
	get(Models, find,
	    message(@prolog, hasQuantityDefinition, @arg1, Definition?name),
	    Model
	)
    ;
	send(Definition, instance_of, quantitySpace) ->
	get(Models, find,
	    message(@prolog, hasQuantitySpaceDefinition, @arg1, Definition?name),
	    Model
	)
    ).

	    
elementBelongsToModel(Element, Model) :-
    getModels(Models),
    get(Models, find,
	message(@prolog, elementBelongsToModel2, Element, @arg1),
	Model	
    ).
elementBelongsToModel2(Element, Model) :-
    get(Model?modelFragments, find,
	message(@arg1?elements, member, Element),
    _MF).

hasModelFragmentDefinition(Model, ModelFragment) :-
    send(Model?modelFragments, member, ModelFragment).

getModelFragmentDefinitionName(Model, ModelFragmentName, ModelFragment) :-
    get(Model?modelFragments, find,
	message(@prolog, ==, @arg1?name?value, ModelFragmentName),
	ModelFragment).

hasModelFragmentDefinitionName(Model, ModelFragmentName) :-
   getModelFragmentDefinitionName(Model, ModelFragmentName, _ModelFragment).
%%

getModels(Models) :-
   get(@app?modelsTabs, values, Models),
   send(Models, append, @copyBuffer).

/************** INSTANCE CHECKING API ****************/
getRequiredDefinitions(ModelFragments, Entities, Agents, Assumptions, Configurations, Attributes, Quantities, QuantitySpaces, EssentialFlag) :-
    chain_list(ModelFragments, ModelFragmentsList),
    new(AbstractEntities, chain),
    new(Configurations, chain),
    new(Attributes, chain),
    new(Quantities, chain),
    forall(
	member(MF, ModelFragmentsList),
	(
	    getRequiredEntities(MF, RequiredEntities),
	    %printChainNames(RequiredEntities),
	    getRequiredAssumptions(MF, RequiredAssumptions),
	    send(AbstractEntities, union, RequiredEntities),
	    send(AbstractEntities, union, RequiredAssumptions),
	    getRequiredConfigurations(MF, RequiredConfigurations),
	    send(Configurations, union, RequiredConfigurations),
	    getRequiredAttributes(MF, RequiredAttributes),
	    send(Attributes, union, RequiredAttributes),
	    getRequiredQuantities(MF, RequiredQuantities),
	    send(Quantities, union, RequiredQuantities)
	)
    ),
    getRequiredQuantitySpaces(Quantities, QuantitySpaces),
    
    get(ModelFragments, head, MF),
    definitionBelongsToModel(MF, Model),
    %get(AbstractEntities, size, Size),
    %format('AbstractEntities heeft grootte ~w\n', [Size]),
    (
	EssentialFlag == @on ->
	new(EntityChain, chain), % Chain with all the required entities
	send(AbstractEntities, for_all,
	    and(
		message(@prolog, requiredEntities, Model, EntityChain, @arg1),
		message(EntityChain, append, @arg1)
	    )
	),
	send(EntityChain, unique)
	%get(EntityChain, size, Size2),
	%format('EntitityChain heeft grootte ~w\n', [Size2])
    ;
	EntityChain = AbstractEntities
    ),
    separateEntities(EntityChain, Entities, Agents, Assumptions).

/* Add the selected entities and all their parent to a chain */
requiredEntities(Model, EntityChain, Entity) :-
    (
	definitionBelongsToModel(Entity, DefinitionModel),
	isPredefinedAbstractEntity(DefinitionModel, Entity)
    ;
	send(EntityChain, append, Entity),
	get(Entity, parent, Parent),
	send(@prolog, requiredEntities, Model, EntityChain, Parent)	
    ).


testRequired(Name) :-
    getModelFragmentDefinitionName(@model, Name, X),
    getRequiredModelFragments(X, MFList),
    new(Chain, chain),
    forall(member(MF, MFList), send(Chain, append, MF)),
    send(Chain, append, X),
    getRequiredDefinitions(Chain, Entities, Agents, Assumptions, Configurations, Attributes, Quantities, QuantitySpaces, @on),
    format('The entities: \n'),
    printChainNames(Entities),
    format('The agents: \n'),
    printChainNames(Agents),
    format('The assumptions: \n'),
    printChainNames(Assumptions),
    format('The configurations \n'),
    printChainNames(Configurations),
    format('The attributes \n'),
    printChainNames(Attributes),
    format('The quantities \n'),
    printChainNames(Quantities),
    format('The quantity spaces \n'),
    printChainNames(QuantitySpaces).

getRequiredEntities(ModelFragment, RequiredEntities) :-
    %new(RequiredEntitiesInstances, chain),
    get(ModelFragment?elements, find_all,
	message(@prolog, ==, @arg1?class_name, 'garpInstance'),
	AllGarpInstances
    ),
    get(AllGarpInstances, map, @arg1?entity, RequiredEntities),
    send(RequiredEntities, unique).

getRequiredAssumptions(ModelFragment, RequiredAssumptions) :-
    get(ModelFragment?elements, find_all,
	message(@prolog, ==, @arg1?class_name, 'assumptionInstance'),
	AllAssumptions
    ),
    get(AllAssumptions, map, @arg1?definition, RequiredAssumptions),
    send(RequiredAssumptions, unique).


separateEntities(AllEntities, Entities, Agents, Assumptions) :-
    new(Entities, chain),
    new(Agents, chain),
    new(Assumptions, chain),
    send(AllEntities, for_all,
	and(
	    if(
		message(@prolog, hasEntityType, @arg1, 'Entity'),
		message(Entities, append, @arg1),
		message(@prolog, true)
	    ),
	    if(
		message(@prolog, hasEntityType, @arg1, 'Agent'),
		message(Agents, append, @arg1),
		message(@prolog, true)
	    ),
    	    if(
		message(@prolog, hasEntityType, @arg1, 'Assumption'),
		message(Assumptions, append, @arg1),
		message(@prolog, true)
	    )
	)
    ).

hasEntityType(Entity, TypeName) :-
    definitionBelongsToModel(Entity, Model),
    (
	getTopEntity(Model, Entity) ->
	TypeName = 'Entity'
    ;
	getTopAgent(Model, Entity) ->
	TypeName = 'Agent'
    ;
	getTopAssumption(Model, Entity) ->
	TypeName = 'Assumption'
    ;
	get(Entity, parent, Parent),
	hasEntityType(Parent, TypeName)
    ).

getRequiredConfigurations(ModelFragment, RequiredConfigurations) :-
    %new(RequiredConfigurationInstances, chain),
    get(ModelFragment?elements, find_all,
	message(@prolog, ==, @arg1?class_name, 'configuration'),
	AllConfigurationInstances
    ),
    get(AllConfigurationInstances, map, @arg1?definition, RequiredConfigurations),
    send(RequiredConfigurations, unique).

getRequiredAttributes(ModelFragment, RequiredAttributes) :-
    %new(RequiredAttributeInstances, chain),
    get(ModelFragment?elements, find_all,
	message(@prolog, ==, @arg1?class_name, 'garpAttribute'),
	AllAttributeInstances
    ),
    get(AllAttributeInstances, map, @arg1?definition, RequiredAttributes),
    send(RequiredAttributes, unique).

getRequiredQuantities(ModelFragment, RequiredQuantities) :-
    %new(RequiredQuantityInstances, chain),
    get(ModelFragment?elements, find_all,
	message(@prolog, ==, @arg1?class_name, 'garpQuantity'),
	AllQuantityInstances
    ),
    get(AllQuantityInstances, map, @arg1?definition, RequiredQuantities),
    send(RequiredQuantities, unique).

getRequiredQuantitySpaces(Quantities, RequiredQuantitySpaces) :-
    new(RequiredQuantitySpaces, chain),
    send(Quantities, for_all,
	message(RequiredQuantitySpaces, union, @arg1?allowedQuantitySpaces)
    ),
    send(RequiredQuantitySpaces, unique).

sortAggregateElements(Elements) :-
    send(Elements, sort, ?(@prolog, orderCompare, @arg1?class_name, @arg2?class_name) ).

orderCompare(Type1, Type2, OrderIndicator) :-
        Order = [ [1, 'fragmentRefiner'],
                  [2, 'importedFragment'],
		  [3, 'garpInstance'],
                  [4, 'assumptionInstance'],
                  [5, 'garpQuantity'],
                  [6, 'garpAttribute'],
                  [7, 'configuration'],
                  [8, 'garpQuantityRelation'],
                  [9, 'value'],
                  [10,'correspondence'],
                  [11,'calculus'],
                  [12,'inequality'], 
                  [13,'identityRelation'] ],
         member([X,Type1], Order),
         member([Y,Type2], Order),
         if X == Y then OrderIndicator = 0,
         if X < Y then OrderIndicator = -1,
         if X > Y then OrderIndicator = 1.


/************ Model checking API *********************/
hasEntityDefinition(Model, Entity) :-
    (
	send(Model?abstractEntities, member, Entity)
    ;
	send(Model?assumptions, member, Entity)
    ).


hasEntityDefinitionName(Model, EntityName) :-
    getEntityDefinitionName(Model, EntityName, _Entity).
getEntityDefinitionName(Model, EntityName, Entity) :-
    changeToModel(Model, LastModel),
    (
	(
	    get(Model?abstractEntities, find, 
		message(@prolog, ==, @arg1?name, EntityName), 
	    Entity)
	;
	    get(Model?assumptions, find,
		message(@prolog, ==, @arg1?name, EntityName),
	    Entity)
	),
	changeToModel(LastModel, _)
    ;
	changeToModel(LastModel, _), fail
    ).

getEntityType(Model, EntityName, Type) :-
    completeEntityList(Model, 'Agent', AllAgents),
    completeEntityList(Model, 'Entity', AllEntities), !,
    (
	get(AllEntities, find, @arg1?name == EntityName, _Entity), !,
	Type = 'Entity'
    ;
	get(Model?assumptions, find, @arg1?name == EntityName, _Assumption), !,
	Type = 'Assumption'
    ;
	get(AllAgents, find, @arg1?name == EntityName, _Agent), !,
	Type = 'Agent'
    ).

hasConfigurationDefinition(Model, ConfigurationName) :-
    getConfigurationDefinition(Model, ConfigurationName, _Configuration).
getConfigurationDefinition(Model, ConfigurationName, Configuration) :-
    changeToModel(Model, LastModel),
    (
	get(Model?configurationDefinitions, find, @arg1?name == ConfigurationName, Configuration),
	changeToModel(LastModel, _UnimportantLastModel)
    ;
	changeToModel(LastModel, _TheSameUnimportantModel),
	fail
    ).


hasAttributeDefinition(Model, AttributeName) :-
    getAttributeDefinition(Model, AttributeName, _Attribute).
getAttributeDefinition(Model, AttributeName, Attribute) :-
    get(Model?attributeDefinitions, find, @arg1?name == AttributeName, Attribute).

hasQuantityDefinition(Model, QuantityName) :-
    getQuantityDefinition(Model, QuantityName, _QuantityDef).
getQuantityDefinition(Model, QuantityName, QuantityDef) :-
    get(Model?quantityDefinitions, find, @arg1?name == QuantityName, QuantityDef).

hasQuantitySpaceDefinition(Model, QuantitySpaceName) :-
    getQuantitySpaceDefinition(Model, QuantitySpaceName, _QuantitySpaceDef).
getQuantitySpaceDefinition(Model, QuantitySpaceName, QuantitySpaceDef) :-
    get(Model?quantitySpaces, find, @arg1?name == QuantitySpaceName, QuantitySpaceDef).

cloneValues(Values, ClonedValues) :-
    new(ClonedValues, chain),
    chain_list(Values, ValuesList),
    forall(
	member(Value, ValuesList),
	(
	    %gp3 1.4 (jj): changed this to support new features in valueReference
	    %send(ClonedValues,append, Value?copy)
	    
	    get(Value, name, Name),
	    get(Value, type, Type),
	    new(ValueReference, valueReference(Name, Type)),
	    send(ClonedValues, append, ValueReference)
	    
	)
    ),
    send(ClonedValues, protect).

hasSameValues(Values1, Values2, CompareOrder) :-
    get(Values1, size, Size),
    get(Values2, size, Size),
    (
	CompareOrder == @off ->
	hasSameValues(Values1, Values2)
    ;
	CompareOrder == @on ->
	hasSameValuesOrder(Values1, Values2)
    ).

hasSameValues(Values1, Values2) :-
    % If CompareOrder is @on, the order of the values should be identical
    % They should have the same amount of values
    chain_list(Values1, Values1List),
    forall(
	member(Value1, Values1List),
	(
	    nth1(Index, Values1List, Value1),
	    get(Values2, nth1, Index, Value2),
	    get(Value1, name, Name),
	    get(Value2, name, Name),
	    get(Value1, type, Type),
	    get(Value2, type, Type)
	)
    ).

hasSameValuesOrder(Values1, Values2) :-
    % If CompareOrder is @off, the order of the values is not important

    chain_list(Values1, Values1List),
    forall(
	member(Value1, Values1List),
	(
	    get(Value1, name, Name),
	    get(Value1, type, Type),
	    get(Values2, find, 
		and(
		    message(@prolog, ==, @arg1?name, Name),
		    message(@prolog, ==, @arg1?type, Type)
		), _AnotherValue
	    )
	)
    ).

changeToModel(Model, PreviousModel) :-
    (
	% You can never change to the active model (since it is a pointer)
	Model == @model ->
	PreviousModel = @model
    ;
	% Store the old model, and switch to the new one
        get(@model, '_value', PreviousModel),
	send(@model, assign, Model, global)
    ).

/* Find the route to the element if you know the Element and the Main Aggregate which contains it */
findElementByNameInAggregate(ElementName, Aggregate, Element, Route, MainMF) :-
        findElementByNameInAggregate2(ElementName, Aggregate, Element, RouteAndMainMF),
        last(RouteAndMainMF, MainMF),
        delete(RouteAndMainMF, MainMF, RouteList),
        %reverse(RouteListRev, RouteList),
        chain_list(Route, RouteList).
        %format('De route is: '),
        %forall(member(RouteE, RouteList), (RouteEName = RouteE?referencedFragment<<-name, format(' ~w', [RouteEName]))),
        %format('\n').
        
findElementByNameInAggregate2(ElementName, Aggregate, Element, [Aggregate]) :-
                %format('Element: ~w\n', [Element]),
                (
                        get(Aggregate?elements, find, 
			    if(
				message(@arg1, has_get_method, name),
				message(@prolog, ==, @arg1?name, ElementName),
				message(@prolog, fail)
			    ), Element
			)
                        ;
                        get(Aggregate?parentReferences, find, 
			    if(
				message(@arg1, has_get_method, name),
				message(@prolog, ==, @arg1?name, ElementName),
				message(@prolog, fail)
			    ), Element
			)
                ).
        
findElementByNameInAggregate2(ElementName, Aggregate, Element, [ImportedFragment|Route]) :-
                %send(Aggregate?elements, for_all, message(@prolog, format, '~w\n', [@arg1?class_name])),
                (
                        get(Aggregate?elements, find_all, @arg1?class_name == 'importedFragment', ImportedFragmentsChain)
                ;
			get(Aggregate?elements, find_all, @arg1?class_name == 'fragmentRefiner', ImportedFragmentsChain)
		;
                        get(Aggregate?parentReferences, find_all, @arg1?class_name == 'importedFragment', ImportedFragmentsChain)
                ),
                chain_list(ImportedFragmentsChain, ImportedFragments),
                /* Only one of them contains the element */
                member(ImportedFragment, ImportedFragments),
                get(ImportedFragment, referencedFragment, ImportedFragmentDef),
                findElementByNameInAggregate2(ElementName, ImportedFragmentDef, Element, Route). 
