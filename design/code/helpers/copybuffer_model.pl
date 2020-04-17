:-pce_begin_class(
                  copyBufferModel(name),
                  garpModel,
                  "copyBuffer based on garpModel" 
                 ).

variable(selectedEntities,    chain:=new(chain), both, 'The chain with the selected entities').
variable(selectedAgents,      chain:=new(chain), both, 'The chain with the selected agents').
variable(selectedAssumptions, chain:=new(chain), both, 'The chain with the selected assumptions').
variable(mapping, hash_table:=new(hash_table), both, 'This table translates between models').

initialise(CB, N : name = [name]) :->
    send_super(CB, initialise, N),
    send(CB, warnings),
    send_super(CB, modelState, new).

warnings(_CB) :->
    send(@app, setting, nonessential_warnings, @on).
noWarnings(_CB) :->
    send(@app, setting, nonessential_warnings, @off).

% The copy function called from the entity editor
entityCopy(CB, SelectionChain, _Type) :->
    %send(CB, emptySelectedEntities, Type),
    %emptyEntityHierarchy(CB, Type),

    send(CB, clearMapping),
    send(CB, initialiseMapping, @model, @copyBuffer),
    emptyAllHierarchies(@copyBuffer),
   
    new(EntityChain, chain), % Chain with all the required entities
    send(SelectionChain, for_all, 
	message(@prolog, requiredEntities, @copyBuffer, EntityChain, @arg1)
    ),

    send(EntityChain, unique),

    get(EntityChain, size, Size),
    (
	Size > 0 ->
	get(EntityChain, head, FirstEntity),
	definitionBelongsToModel(FirstEntity, SourceModel),
	send(CB, createEntityDefs, SourceModel, @copyBuffer, EntityChain, @nil, @nil, SelectionChain),
	send(@app, modelChanged)
    ;
	true
    ).

/* The paste function called by the entity editor */
entityPaste(CB, SelectedNode, Type, Editor) :->
    send(CB, clearMapping),
    send(CB, initialiseMapping, @copyBuffer, @model),

    ( Type == 'Entity' ->     get(CB, selectedEntities, SelectedEntitiesType)   ;
      Type == 'Agent' ->      get(CB, selectedAgents, SelectedEntitiesType)     ;
      Type == 'Assumption' -> get(CB, selectedAssumptions, SelectedEntitiesType)), 

    completeEntityList(CB, Type, AllEntities),

    send(AllEntities, for_all,
	if(
	    message(SelectedEntitiesType, member, @arg1),
	    message(@prolog, true),
	    message(AllEntities, delete, @arg1)
	)
    ),
    send(CB, createEntityDefs, @copyBuffer, @model, AllEntities, Editor, SelectedNode, SelectedEntitiesType).

createEntityDefs(CB, SourceModel, TargetModel, Entities, Editor, TargetEntity, SelectedEntities) :->
    (
	send(Entities, empty) ->
	true
    ;
	get(Entities, head, Entity),
	hasEntityType(Entity, Type),
	completeEntityList(SourceModel, Type, AllEntities),

	send(AllEntities, for_all,
	    if(
		message(Entities, member, @arg1),
		message(@prolog, true),
		message(AllEntities, delete, @arg1)
	    )
	),
	% format('Je hasEntityDefinition functie moet gewoon een entity instance krijgen i.p.v. een naam. Je definitionBelongsToModel bugt!'),
	send(CB, createEntityDefs2, SourceModel, TargetModel, AllEntities, Editor, TargetEntity, SelectedEntities)
    ).

createEntityDefs2(_CB, _SourceModel, _TargetModel, Entities, _Editor, _TargetEntity, _SelectedEntities) :->
    send(Entities, empty).
createEntityDefs2(CB, SourceModel, TargetModel, Entities, Editor, TargetEntity, SelectedEntities) :->
    send(Entities, for_all,
	if(
	    if(
		message(CB, hasEntityParent, @arg1, SelectedEntities),
		message(CB, createEntityDef, SourceModel, TargetModel, @arg1, Editor, @nil, SelectedEntities),
		message(CB, createEntityDef, SourceModel, TargetModel, @arg1, Editor, TargetEntity, SelectedEntities)
	    ),
	    message(Entities, delete, @arg1),
	    message(@prolog, true)
	)
    ),
    send(CB, createEntityDefs2, SourceModel, TargetModel, Entities, Editor, TargetEntity, SelectedEntities).

/* Check if a parent of Entity is copied */
hasEntityParent(_CB, Entity, Entities) :->
    (
	Entities == @nil ->
	fail
    ;
	true
    ),
    %get(Entity, name, Name), format('Searching for parent of ~w\n', [Name]),
    getAllEntityParents(Entity, Parents),
    get(Parents, find,
	message(Entities, member, @arg1),
    _Parent).
	

createEntityDef(CB, SourceModel, TargetModel, Entity, Editor, TargetEntity, SelectedEntities) :->
    % Sychronize the languages and set the target model language to the source model language
    syncLanguages(SourceModel, TargetModel), 

    get(Entity, name, EntityName),
    getEntityType(SourceModel, EntityName, Type),
    createEntityName(SourceModel, EntityName, TargetModel, NewName), 
    (
	% The target model does not already have an entity with the same name and type
	\+ (
	    getEntityDefinitionName(TargetModel, NewName, AlreadyExistingEntity),
	    get(AlreadyExistingEntity, name, AlreadyExistingEntityName),
	    getEntityType(TargetModel, AlreadyExistingEntityName, Type) 
	) ->
	% Use the selection
	(
	    TargetEntity \== @nil ->
	    Parent = TargetEntity
	;
	    get(Entity, parent, EntityParent),
	    get(EntityParent, name, EntityParentName),
	    getEntityType(SourceModel, EntityParentName, EntityParentType),
	    getClosestParent(TargetModel, EntityParent, EntityParentType, Parent)
	),
	get(Parent, name, ParentName),
	get(Entity?remarks, value, Remarks),
	addEntityDefinition(TargetModel, NewName, ParentName, Remarks, Editor),
	getEntityDefinitionName(TargetModel, NewName, NewEntity),
	addMappingDefinition(CB?mapping, Entity, NewEntity),

	% Copy the names in different languages for each of the elements
	copyLanguagesForElement(Entity, NewEntity),

	% If SelectedEntities is not @nil, check if the entity is one of the selected ones
	(
	    SelectedEntities \== @nil,
	    ( Type == 'Entity',     get(CB, selectedEntities,    SelectedEntitiesType) ;
	      Type == 'Agent',      get(CB, selectedAgents,      SelectedEntitiesType) ;
	      Type == 'Assumption', get(CB, selectedAssumptions, SelectedEntitiesType) ),
	    send(SelectedEntities, member, Entity),
	    getEntityDefinitionName(TargetModel, NewName, NewEntity),
	    send(SelectedEntitiesType, append, NewEntity)
	;
	    true
	)
    ;
	getEntityDefinitionName(TargetModel, NewName, ExistingEntity),
	addMappingDefinition(CB?mapping, Entity, ExistingEntity)
    ).

getClosestParent(TargetModel, Parent, Type, ClosestAncestor) :-
    (
	% Either an entity with the same name
	get(Parent, name, ParentName),
	getEntityDefinitionName(TargetModel, ParentName, ClosestAncestor),
	getEntityType(TargetModel, ParentName, Type) 
    ;
	% Parent is the top node in the source model, so get the top node in the source model
	definitionBelongsToModel(Parent, SourceModel),
	getTopAbstractEntity(SourceModel, Type, Parent),
	getTopAbstractEntity(TargetModel, Type, ClosestAncestor)
    ;
	% Recursive until a parent has been found
	get(Parent, parent, Ancestor),
	getClosestParent(TargetModel, Ancestor, Type, ClosestAncestor)
    ).

% Add a postfix if another entity exist with the same name of a different type
% Otherwise return the same name
createEntityName(SourceModel, EntityName, TargetModel, NewName) :-
    (
	getEntityDefinitionName(TargetModel, EntityName, TargetEntity) ->
	getEntityType(TargetModel, TargetEntity?name, TargetType),
	getEntityType(SourceModel, EntityName, SourceType),
	(
	    TargetType == SourceType ->
	    EntityName = NewName
	;
	    downcase_atom(TargetType, TargetTypeLowerCase),
	    swritef(PostFixNameString, '%w (also %w)', [EntityName, TargetTypeLowerCase]),
	    string_to_atom(PostFixNameString, PostFixName),
	    createEntityName(SourceModel, PostFixName, TargetModel, NewName)
	)
    ;
	NewName = EntityName
    ).


containsElementWithSameName(_CB, Chain, Element) :->
    send(Chain, find, message(@prolog, ==, @arg1?name, Element?name)).

/* Add an element with the same name from Chain1 to Chain2 */
addElementWithName(Chain1, Name, Chain2) :-
    get(Chain1, find, message(@prolog, ==, @arg1?name, Name), Element),
    send(Chain2, append, Element).

emptySelectedEntities(CB, Type) :->
    (
	Type == 'Entity',
        send(CB?selectedEntities, clear)
    ;
	Type == 'Agent',
        send(CB?selectedAgents, clear)
    ;
	Type == 'Assumption',
        send(CB?selectedAssumptions, clear)
    ).
emptyAllSelectedEntities(CB) :->
    send(CB, emptySelectedEntities, 'Entity'),
    send(CB, emptySelectedEntities, 'Agent'),
    send(CB, emptySelectedEntities, 'Assumption').

copyConfigurationDef(CB, Configuration) :->
    %emptyConfigurationHierarchy(CB),
    emptyAllHierarchies(@copyBuffer),
    chain_list(Configurations, [Configuration]), % Extend me!
    send(CB, createConfigurationDefs, @copyBuffer, Configurations, @nil), 
    send(@app, modelChanged).


    %changeToModel(@model, _UnimportantModel). % Debug
    %send(@app, modelChanged).


pasteConfigurationDef(CB, Editor) :->
    get(@copyBuffer, configurationDefinitions, ConfigurationDefinitions),
    send(CB, createConfigurationDefs, @model, ConfigurationDefinitions, Editor).

createConfigurationDefs(CB, TargetModel, Configurations, Editor) :->
    send(Configurations, for_all,
	message(CB, createConfigurationDef, TargetModel, @arg1, Editor)
    ).

createConfigurationDef(CB, TargetModel, Configuration, Editor) :->
    % Sychronize the languages and set the target model language to the source model language
    definitionBelongsToModel(Configuration, SourceModel), 
    syncLanguages(SourceModel, TargetModel), 

    get(Configuration, name, Name),
    (
	getConfigurationDefinition(TargetModel, Name, ExistingConfiguration) ->
	addMappingDefinition(CB?mapping, Configuration, ExistingConfiguration)
    ;	
	get(Configuration?remarks, value, Remarks),
	addConfigurationDefinition(TargetModel, Editor, Name, Remarks),
	getConfigurationDefinition(TargetModel, Name, NewConfiguration),
	addMappingDefinition(CB?mapping, Configuration, NewConfiguration),

	% Copy the names in different languages for the element
	copyLanguagesForElement(Configuration, NewConfiguration)
    ).

copyAttributeDef(CB, AttributeDef) :->
    emptyAllHierarchies(@copyBuffer),
    %emptyAttributeDefinitionHierarchy(CB),

    chain_list(Attributes, [AttributeDef]), % EXTEND ME
    send(CB, createAttributeDefs, @copyBuffer, Attributes, @nil),
    send(@app, modelChanged).
   
pasteAttributeDef(CB, Editor) :->
    get(CB, attributeDefinitions, AttributeDefChain),
    send(CB, createAttributeDefs, @model, AttributeDefChain, Editor).

createAttributeDefs(CB, TargetModel, Attributes, Editor) :->
    send(Attributes, for_all,
	message(CB, createAttributeDef, TargetModel, @arg1, Editor)
    ).

createAttributeDef(CB, TargetModel, Attribute, Editor) :->
    % Sychronize the languages and set the target model language to the source model language
    definitionBelongsToModel(Attribute, SourceModel), 
    syncLanguages(SourceModel, TargetModel), 

    get(Attribute, name, AttributeName),
    get(Attribute, values, AttributeValues),
    get(Attribute, remarks, Remarks),
    new(RemarksS, string(Remarks)),
    (
	% An existing attribute definition exists with the same name
	getAttributeDefinition(TargetModel, AttributeName, _ExistingAttributeDef) ->
	createAttributeOrQuantitySpaceName(Attribute, AttributeName, TargetModel, @off, NewAttributeName),
	(
	    getAttributeDefinition(TargetModel, NewAttributeName, AnotherExistingAttributeDef) ->
	    addMappingDefinition(CB?mapping, Attribute, AnotherExistingAttributeDef)
	;
	    cloneValues(AttributeValues, ClonedValues),
	    addAttributeDefinition(TargetModel, Editor, NewAttributeName, ClonedValues, RemarksS),
	    getAttributeDefinition(TargetModel, NewAttributeName, NewDifferentAttribute),
	    addMappingDefinition(CB?mapping, Attribute, NewDifferentAttribute),

	    % Copy the names in different languages for the element
	    copyLanguagesForElement(Attribute, NewDifferentAttribute),
	    get(NewDifferentAttribute, values, NewValues)
	)
    ;
	cloneValues(AttributeValues, ClonedValues),
	addAttributeDefinition(TargetModel, Editor, AttributeName, ClonedValues, RemarksS),
	getAttributeDefinition(TargetModel, AttributeName, NewAttribute),
	addMappingDefinition(CB?mapping, Attribute, NewAttribute),

	% Copy the names in different languages for the element
	copyLanguagesForElement(Attribute, NewAttribute),
	get(NewAttribute, slot, values, NewValues),
	copyLanguagesForValues(AttributeValues, NewValues)
    ).

% TODO change my names from Attribute to AttributeOrQuantitySpace
createAttributeOrQuantitySpaceName(Attribute, AttributeName, TargetModel, OrderFlag, NewAttributeName) :-
    (
	(
	    getAttributeDefinition(TargetModel, AttributeName, ExistingAttributeDef) ;
	    getQuantitySpaceDefinition(TargetModel, AttributeName, ExistingAttributeDef)
	),
	% If it has the same values, the name is the same
	(
	    get(ExistingAttributeDef, values, ExistingAttributeValues),
	    get(Attribute, values, AttributeValues),
	    hasSameValues(AttributeValues, ExistingAttributeValues, OrderFlag) ->
	    NewAttributeName = AttributeName
	;
	    % Else create an attribute with the postfix '(different values)'
	    string_concat(AttributeName, ' (different values)', NextAttributeNameString),
	    string_to_atom(NextAttributeNameString, NextAttributeName),
	    createAttributeOrQuantitySpaceName(ExistingAttributeDef, NextAttributeName, TargetModel, OrderFlag, NewAttributeName)
	)
    ;
	NewAttributeName = AttributeName
    ).


copyQuantitySpaceDef(CB, QuantitySpaceDef) :->
    send(CB, noWarnings),
    emptyAllHierarchies(@copyBuffer),
    %emptyQuantitySpaceDefinitionHierarchy(@copyBuffer),
    chain_list(QuantitySpaceDefs, [QuantitySpaceDef]),
    send(CB, createQuantitySpaceDefs, @copyBuffer, QuantitySpaceDefs, @nil),
    send(@app, modelChanged),
    send(CB, warnings).

pasteQuantitySpaceDef(CB, Editor) :->
    send(CB, noWarnings),
    get(CB, quantitySpaces, QuantitySpaceDefs),
    send(CB, createQuantitySpaceDefs, @model, QuantitySpaceDefs, Editor),
    send(CB, warnings).

createQuantitySpaceDefs(CB, TargetModel, QuantitySpaceDefs, Editor) :->
    send(QuantitySpaceDefs, for_all,
	message(CB, createQuantitySpaceDef, TargetModel, @arg1, Editor)
    ).

createQuantitySpaceDef(CB, TargetModel, QuantitySpaceDef, Editor) :->
    % Sychronize the languages and set the target model language to the source model language
    definitionBelongsToModel(QuantitySpaceDef, SourceModel), 
    syncLanguages(SourceModel, TargetModel), 

    get(QuantitySpaceDef, name, QuantitySpaceName),
    %QuantitySpaceName \== 'Mzp',
    get(QuantitySpaceDef, values, QuantitySpaceValues),
    get(QuantitySpaceDef, remarks, Remarks),	
    (
	% An existing quantity space definition exists with the same name
	getQuantitySpaceDefinition(TargetModel, QuantitySpaceName, _ExistingQuantitySpaceDef) ->
	createAttributeOrQuantitySpaceName(QuantitySpaceDef, QuantitySpaceName, TargetModel, @on, NewQuantitySpaceName),
	(
	    getQuantitySpaceDefinition(TargetModel, NewQuantitySpaceName, AnotherExistingQuantitySpaceDef) ->
	    addMappingDefinition(CB?mapping, QuantitySpaceDef, AnotherExistingQuantitySpaceDef)
	;
	    cloneValues(QuantitySpaceValues, ClonedValues),
	    addQuantitySpaceDefinition(TargetModel, Editor, NewQuantitySpaceName, ClonedValues, Remarks),
	    getQuantitySpaceDefinition(TargetModel, NewQuantitySpaceName, NewDifferentQuantitySpace),
	    addMappingDefinition(CB?mapping, QuantitySpaceDef, NewDifferentQuantitySpace),

	    % Copy the names in different languages for the element
	    copyLanguagesForElement(QuantitySpaceDef, NewDifferentQuantitySpace),
	    get(NewDifferentQuantitySpace, slot, values, NewValues),
	    copyLanguagesForValues(QuantitySpaceValues, NewValues) 
	)
    ; 
	cloneValues(QuantitySpaceValues, ClonedValues),
	addQuantitySpaceDefinition(TargetModel, Editor, QuantitySpaceName, ClonedValues, Remarks),
	getQuantitySpaceDefinition(TargetModel, QuantitySpaceName, NewQuantitySpace),
	addMappingDefinition(CB?mapping, QuantitySpaceDef, NewQuantitySpace),

	% Copy the names in different languages for the element
	copyLanguagesForElement(QuantitySpaceDef, NewQuantitySpace),
	get(NewQuantitySpace, slot, values, NewValues),
	copyLanguagesForValues(QuantitySpaceValues, NewValues)
    ).

copyQuantityDef(CB, QuantityDefinition) :->
    send(CB, clearMapping),
    send(CB, noWarnings),
    emptyAllHierarchies(@copyBuffer),
    %emptyQuantityDefinitionHierarchy(@copyBuffer),
    chain_list(QuantityDefinitions, [QuantityDefinition]), % Extend me
    send(CB, createQuantityDefs, @copyBuffer, QuantityDefinitions, @nil),
    send(@app, modelChanged),
    send(CB, warnings).


pasteQuantityDef(CB, Editor) :->
    send(CB, noWarnings),
    get(CB, quantityDefinitions, QuantityDefinitionsChain),
    send(CB, createQuantityDefs, @model, QuantityDefinitionsChain, Editor),
    send(CB, warnings).


createQuantityDefs(CB, TargetModel, QuantityDefinitions, Editor) :->
    new(AllQuantitySpaces, chain),
    send(QuantityDefinitions, for_all,
	message(AllQuantitySpaces, union, @arg1?allowedQuantitySpaces)
    ),
    send(CB, createQuantitySpaceDefs, TargetModel, AllQuantitySpaces, Editor),
    send(QuantityDefinitions, for_all,
	message(CB, createQuantityDef, TargetModel, @arg1, Editor)
    ).


createQuantityDef(CB, TargetModel, QuantityDefinition, Editor) :->
    get(QuantityDefinition, name, QuantityName),
    get(QuantityDefinition, allowedQuantitySpaces, SourceAllowedQuantitySpaces),
    get(QuantityDefinition?remarks, value, Remarks),    
    get(SourceAllowedQuantitySpaces, map, ?(@prolog, getMappingDefinition, CB?mapping, @arg1), TargetAllowedQuantitySpaces),

    (
	% If a quantity with the same name already exists
        getQuantityDefinition(TargetModel, QuantityName, ExistingQuantityDefinition) ->
	% Merge them
	get(ExistingQuantityDefinition, allowedQuantitySpaces, ExistingNeededQuantitySpaces),
	send(TargetAllowedQuantitySpaces, union, ExistingNeededQuantitySpaces),
	changeQuantityDefinition(TargetModel, Editor, ExistingQuantityDefinition, QuantityName, TargetAllowedQuantitySpaces, Remarks),
	addMappingDefinition(CB?mapping, QuantityDefinition, ExistingQuantityDefinition)
    ;	
	% Otherwise create a new one
        addQuantityDefinition(TargetModel, Editor, QuantityName, TargetAllowedQuantitySpaces, Remarks),
	getQuantityDefinition(TargetModel, QuantityName, NewQuantityDefinition),
	addMappingDefinition(CB?mapping, QuantityDefinition, NewQuantityDefinition),

	% Copy the names in different languages for the element
	copyLanguagesForElement(QuantityDefinition, NewQuantityDefinition)
    ).



copyScenarioDef(CB, Scenario) :->
    chain_list(Scenarios, [Scenario]), % TODO: Extend me!
    send(CB, noWarnings),
    send(CB, clearMapping),
    emptyAllHierarchies(@copyBuffer),
    send(CB, emptyAllSelectedEntities),
    send(CB, createScenarioDefs, @copyBuffer, Scenarios, @nil, @on),
    send(@app, modelChanged),
    send(CB, warnings).


pasteScenarioDef(CB, Editor) :->
    send(CB, noWarnings),
    send(CB, clearMapping),
    getAllScenarios(@copyBuffer, Scenarios),
    send(CB, createScenarioDefs, @model, Scenarios, Editor, @off),
    send(@app, modelChanged),
    send(CB, warnings).

createScenarioDefs(CB, TargetModel, Scenarios, Editor, EssentialFlag) :->
    get(Scenarios, head, FirstScenario),
    definitionBelongsToModel(FirstScenario, SourceModel),
    send(CB, initialiseMapping, SourceModel, TargetModel),

    getRequiredDefinitions(Scenarios, Entities, Agents, Assumptions, Configurations, Attributes, Quantities, QuantitySpaces, EssentialFlag),
    %format('The entities: \n'),
    %printChainNames(Entities),
    send(CB, createEntityDefs, SourceModel, TargetModel, Entities, @nil, @nil, Entities),
    %printChainNames(Agents),
    send(CB, createEntityDefs, SourceModel, TargetModel, Agents, @nil, @nil, Agents),
    %printChainNames(Assumptions),
    send(CB, createEntityDefs, SourceModel, TargetModel, Assumptions, @nil, @nil, Assumptions),
    %format('The configurations \n'),
    %printChainNames(Configurations),
    send(CB, createConfigurationDefs, TargetModel, Configurations, @nil),
    %format('The attributes \n'),
    %printChainNames(Attributes),
    send(CB, createAttributeDefs, TargetModel, Attributes, @nil),
    %format('The quantities \n'),
    %printChainNames(Quantities),
    send(CB, createQuantityDefs, TargetModel, Quantities, @nil),
    %format('The quantity spaces \n'),
    %printChainNames(QuantitySpaces),
    send(CB, createQuantitySpaceDefs, TargetModel, QuantitySpaces, @nil),

    send(Scenarios, for_all,
	message(CB, createScenarioDef, TargetModel, @arg1, Editor)
    ).

createScenarioDef(CB, TargetModel, Scenario, Editor) :-> 
    get(Scenario, name, Label),
    get(Scenario?remarks, value, Remarks), 
    changeModelFragmentDefinitionName(TargetModel, Label, NewName),
    addScenarioDefinition(TargetModel, Editor, NewName, Remarks),

    getModelFragmentDefinitionName(TargetModel, NewName, NewScenario),
    
    % Copy the names in different languages for the new element
    copyLanguagesForElement(Scenario, NewScenario),

    % Set the correct displayOrigin and editSize
    % get(Scenario, layOutInfo, Scenario, displayOrigin, SourcePoint), (no origin since the new layout patch)
    get(Scenario, layOutInfo, Scenario, editSize, SourceSize),
    % get(SourcePoint, copy, TargetPoint), (no origin since the new layout patch)
    new(TargetSize, size(SourceSize?width,SourceSize?height)),
    % send(NewScenario, layOutInfo, NewScenario, displayOrigin, TargetPoint), (no origin since the new layout patch)
    send(NewScenario, layOutInfo, NewScenario, editSize, TargetSize),

    send(CB, createModelFragmentContent, TargetModel, Scenario, NewScenario).



copyModelFragmentDef(CB, ModelFragments) :->
    send(CB, noWarnings),
    send(CB, clearMapping),
    deletePredefinedModelFragmentsChain(ModelFragments),
    emptyAllHierarchies(@copyBuffer),
    send(CB, emptyAllSelectedEntities),
    chain_list(ModelFragments, ModelFragmentsList),
    new(AllRequiredModelFragments, chain),
    forall(
	member(MF, ModelFragmentsList),
	(
	    send(AllRequiredModelFragments, append, MF),
	    getRequiredModelFragments(MF, RequiredModelFragments),
	    forall(
		member(RMF, RequiredModelFragments),
		send(AllRequiredModelFragments, append, RMF)
	    )
	)
    ),
    %get(AllRequiredModelFragments, map, @arg1?name?value, ModelFragmentNames),
    %send(ModelFragmentNames, for_all, message(@prolog, format, ' ~w ', @arg1)), nl,
    send(CB, createModelFragments, @copyBuffer, AllRequiredModelFragments, @nil, @on),
    send(@app, modelChanged),
    send(CB, warnings).

   
pasteModelFragmentDef(CB, Editor) :->
    send(CB, noWarnings),
    send(CB, clearMapping),
    get(CB?modelFragments, copy, CopiedModelFragments),
    deletePredefinedModelFragmentsChain(CopiedModelFragments),
    %get(CopiedModelFragments, map, @arg1?name?value, ModelFragmentNames),
    %send(ModelFragmentNames, for_all, message(@prolog, format, ' ~w ', @arg1)), nl,
    
    send(CB, createModelFragments, @model, CopiedModelFragments, Editor, @on),
    send(@app, modelChanged),
    send(CB, warnings).


createModelFragments(CB, TargetModel, ModelFragments, Editor, EssentialFlag) :->
    get(ModelFragments, size, Size),
    (
	Size > 0 ->
        get(ModelFragments, head, FirstMF),
	definitionBelongsToModel(FirstMF, SourceModel),
	send(CB, initialiseMapping, SourceModel, TargetModel),

	getRequiredDefinitions(ModelFragments, Entities, Agents, Assumptions, Configurations, Attributes, Quantities, QuantitySpaces, EssentialFlag),
	%format('The entities: \n'),
	%printChainNames(Entities),
	send(CB, createEntityDefs, SourceModel, TargetModel, Entities, @nil, @nil, @nil),
	%printChainNames(Agents),
        send(CB, createEntityDefs, SourceModel, TargetModel, Agents, @nil, @nil, @nil),
	%printChainNames(Assumptions),
        send(CB, createEntityDefs, SourceModel, TargetModel, Assumptions, @nil, @nil, @nil),
	%format('The configurations \n'),
	%printChainNames(Configurations),
        send(CB, createConfigurationDefs, TargetModel, Configurations, @nil),
	%format('The attributes \n'),
	%printChainNames(Attributes),
        send(CB, createAttributeDefs, TargetModel, Attributes, @nil),
	%format('The quantities \n'),
	%printChainNames(Quantities),
        send(CB, createQuantityDefs, TargetModel, Quantities, @nil),
	%format('The quantity spaces \n'),
	%printChainNames(QuantitySpaces),
        send(CB, createQuantitySpaceDefs, TargetModel, QuantitySpaces, @nil),

	createModelFragments2(CB, TargetModel, ModelFragments, Editor)
    ;
	true
    ).

createModelFragments2(_CB, _TargetModel, ModelFragments, _Editor) :-
    send(ModelFragments, empty).

createModelFragments2(CB, TargetModel, ModelFragments, Editor) :-
    send(ModelFragments, for_all,
	if(
	    message(CB, createOrReuseModelFragment, TargetModel, @arg1, Editor),
	    message(ModelFragments, delete, @arg1),
	    message(@prolog, true)
	)
    ),
    createModelFragments2(CB, TargetModel, ModelFragments, Editor).

createOrReuseModelFragment(CB, TargetModel, ModelFragment, Editor) :->
    % Check if the model fragment is ready to be created
    send(CB, hasRequiredModelFragments, ModelFragment),
    (
	send(CB, reuseModelFragment, TargetModel, ModelFragment, Editor)
	;
	send(CB, createModelFragment, TargetModel, ModelFragment, Editor)
    ).

hasRequiredModelFragments(CB, ModelFragment) :->
    getRequiredModelFragments(ModelFragment, RequiredModelFragments), !, 
    forall(
	member(RequiredMF, RequiredModelFragments),
	hasMappingDefinition(CB?mapping, RequiredMF) 
    ).

createModelFragment(CB, TargetModel, ModelFragment, Editor) :->
    get(ModelFragment, name, Name),

    % Check if the model fragment already exists, otherwise change the name
    changeModelFragmentDefinitionName(TargetModel, Name, NewName),

    get(ModelFragment?remarks, value, Remarks),
    get(ModelFragment, parents, ParentsSource),
    get(ParentsSource, map, ?(@prolog, getMappingDefinition, CB?mapping, @arg1), ParentsTarget), 
    addModelFragmentDefinition(TargetModel, Editor, NewName, Remarks, ParentsTarget, @on),
    get(TargetModel?modelFragments, head, NewModelFragment),
    send(@prolog, addMappingDefinition, CB?mapping, @arg1, NewModelFragment),
   
    % Copy the names in different languages for the new element
    copyLanguagesForElement(ModelFragment, NewModelFragment),

    % Set the correct displayOrigin and editSize
    % get(ModelFragment, layOutInfo, ModelFragment, displayOrigin, SourcePoint), (no origin since the new layout patch)
    get(ModelFragment, layOutInfo, ModelFragment, editSize, SourceSize),
    % get(SourcePoint, copy, TargetPoint), (no origin since the new layout patch) 
    new(TargetSize, size(SourceSize?width,SourceSize?height)),
    % send(NewModelFragment, layOutInfo, NewModelFragment, displayOrigin, TargetPoint),(no origin since the new layout patch) 
    send(NewModelFragment, layOutInfo, NewModelFragment, editSize, TargetSize),

    send(CB, createModelFragmentContent, TargetModel, ModelFragment, NewModelFragment).

reuseModelFragment(CB, TargetModel, ModelFragmentSource, _Editor) :->
    /* If the target model does not have a model fragment with the same name, fail */
    get(ModelFragmentSource, name, MFName),
    getModelFragmentDefinitionCopies(TargetModel, MFName, PotentiallyReusableMFs),
    /* Get the potentially reusable model fragment */
    member(ModelFragmentTarget, PotentiallyReusableMFs),
   
    /* At least the model fragments should have the same amount of ingredients */
    get(ModelFragmentSource?elements, copy, MFSourceElementsC),
    get(ModelFragmentTarget?elements, copy, MFTargetElementsC),
    get(MFSourceElementsC, size, SizeElementsSource),
    get(MFTargetElementsC, size, SizeElementsTarget),

    (
	SizeElementsSource > SizeElementsTarget ->
	fail
    ;
	SizeElementsSource = SizeElementsTarget ->
	true
    ;
	SizeElementsSource < SizeElementsTarget ->
        new(WW, warningWindow),
	format(atom(FeedbackString), 
'MF "~w" contains more model ingredients than in the copy buffer, but can potentially be reused. Should this be attempted?\n\nCancelling, or failure of the reuse will create a copy of the model fragment.', [MFName]),
	get(WW, open, 'Should reuse be attempted?', @nil, FeedbackString, @on, 'paste_reuse', Result),
	(
	    Result = @on ->
	    true
	;
	    fail
	)
    ),

    /* And they should contain the same named ingredients (or at least be of the same class) */
    chain_list(MFSourceElementsC, MFSourceElements),
    chain_list(MFTargetElementsC, MFTargetElements),
    forall(
	member(MFSourceElement, MFSourceElements),
	(
	    member(MFTargetElement, MFTargetElements),
	    (
		send(MFSourceElement, has_get_method, name) ->
		send(MFTargetElement, has_get_method, name),
		get(MFSourceElement, name, ElementName),
		get(MFTargetElement, name, ElementName)
	    ;
		get(MFSourceElement, class_name, ElementClass),
		get(MFTargetElement, class_name, ElementClass)
	    )
	)
    ),

    /* If this is correct, try to treat the model fragment as an imported model fragment to be fixed */
    send(MFSourceElementsC, union, ModelFragmentSource?parentReferences),
    deletePredefinedModelFragmentsChain(MFSourceElementsC),
    sortAggregateElements(MFSourceElementsC),
    chain_list(MFSourceElementsC, MFSourceElementsList),

    send(MFTargetElementsC, union, ModelFragmentTarget?parentReferences),
    deletePredefinedModelFragmentsChain(MFTargetElementsC),
    sortAggregateElements(MFTargetElementsC),

    chain_list(Route, []),
    fixImportedElements2(CB, MFSourceElementsList, ModelFragmentSource, Route, MFTargetElementsC, ModelFragmentTarget, Route, @on),
    send(@prolog, addMappingDefinition, CB?mapping, ModelFragmentSource, ModelFragmentTarget).

/* Find potentially reusable model fragments */
getModelFragmentDefinitionCopies(Model, ModelFragmentName, MatchingMFs) :-
    once(get(Model, modelFragments, AllModelFragments)),
    chain_list(AllModelFragments, ModelFragmentsList),
    findall(MatchingMF,
	(
	    member(MatchingMF, ModelFragmentsList),
	    get(MatchingMF, name, MFName),
	    (
		MFName == ModelFragmentName
	    ;
		sub_string(MFName, 0, _, _, ModelFragmentName),
		sub_string(MFName, _, _, _, 'copy')
	    )
	),
	ModelFragments),
	predsort(nameLengthCompare, ModelFragments, MatchingMFs).
%
nameLengthCompare(Delta, Ingredient1, Ingredient2) :-
    get(Ingredient1, name, Ingredient1Name), 
    get(Ingredient2, name, Ingredient2Name),
    string_length(Ingredient1Name, Length1),
    string_length(Ingredient2Name, Length2),
    (
	Length1 == Length2 ->
	Delta = '='
    ;
	Length1 > Length2 ->
	Delta = '>'
    ;
	Length1 < Length2 ->
	Delta = '<'
    ).


changeModelFragmentDefinitionName(TargetModel, Name, NewName) :-
    (
	hasModelFragmentDefinitionName(TargetModel, Name) ->
	atom_concat(Name, ' (copy)', NameSuffix),
	changeModelFragmentDefinitionName(TargetModel, NameSuffix, NewName)
    ;
	NewName = Name
    ).

createModelFragmentContent(CB, TargetModel, ModelFragment, ModelFragmentTarget) :->
    % Start by fixing the content in the parents
    get(ModelFragment, parentReferences, SourceParentReferences),
    get(ModelFragmentTarget, parentReferences, TargetParentReferences),
    chain_list(SourceParentReferences, SourceParentReferencesList),
    chain_list(TargetParentReferences, TargetParentReferencesList),
    forall(
	(
	    member(SourceParentReference, SourceParentReferencesList),
	    member(TargetParentReference, TargetParentReferencesList),
	    % same definition
	    get(SourceParentReference, referencedFragment, SourceParentDef),
	    get(TargetParentReference, referencedFragment, TargetParentDef),
	    getMappingDefinition(CB?mapping, SourceParentDef, TargetParentDef),
	    % Not a predefined MF
	    not(isPredefinedModelFragment(SourceParentDef))
	),
	(
	    new(Route, chain), new(TargetRoute, chain),
	    send(CB, updateLayout, SourceParentReference, ModelFragment, Route, TargetParentReference, ModelFragmentTarget, TargetRoute),
	    addMappingElement(CB?mapping, SourceParentReference, Route, TargetParentReference, TargetRoute),
	    %get(SourceParentDef, name, SourceParentDefName),
	    fixImportedElements(CB, SourceParentReference, ModelFragment, Route, TargetParentReference, ModelFragmentTarget, TargetRoute)
	)
    ),
     
    % Now create the real model content
    get(ModelFragment?elements, copy, Elements),
    deletePredefinedModelFragmentsChain(Elements),
    sortAggregateElements(Elements),
    % Debug
    %get(ModelFragment, name, MFName),
    %format('De gesorteerde elementen in ~w: \n', [MFName]),
    %printChainElements(Elements), nl,
    % End debug
    
    chain_list(Elements, ElementsList),
    forall(
	member(Element, ElementsList),
	(
	    get(Element, class_name, Type),
	    format('Creating element of type: ~w\n',[Type]),
	    (
		Type == 'garpInstance' ->
		send(CB, createGarpInstance, Element, ModelFragment, TargetModel, ModelFragmentTarget)
	    ;
		Type == 'configuration' -> 
		send(CB, createConfigurationInstance, Element, ModelFragment, TargetModel, ModelFragmentTarget)
	    ;
		Type == 'garpQuantity' ->
		send(CB, createQuantityInstance, Element, ModelFragment, TargetModel, ModelFragmentTarget)
	    ;
		Type == 'assumptionInstance' ->
		send(CB, createAssumptionInstance, Element, ModelFragment, TargetModel, ModelFragmentTarget)
	    ;
		Type == 'value' ->
		send(CB, createValueInstance, Element, ModelFragment, TargetModel, ModelFragmentTarget)
	    ;
		Type == 'calculus' ->
		send(CB, createCalculusInstance, Element, ModelFragment, TargetModel, ModelFragmentTarget)
	    ;
		Type == 'fragmentRefiner',
		send(CB, createFragmentRefiner, Element, ModelFragment, TargetModel, ModelFragmentTarget)
	    ;
		Type == 'fragmentRefinerIdentity' 
	    ;
		Type == 'importedFragment' ->
		(
		    get(Element, referencedFragment, ImportedFragmentDef),
		    not(isPredefinedModelFragment(ImportedFragmentDef)) ->
		    send(CB, createImportedFragment, Element, ModelFragment, TargetModel, ModelFragmentTarget)
		;
		    true
		)
	    ;
		Type == 'garpAttribute' ->
		send(CB, createAttributeInstance, Element, ModelFragment, TargetModel, ModelFragmentTarget)
	    ;
		Type == 'garpQuantityRelation' ->
		send(CB, createCausalDependency, Element, ModelFragment, TargetModel, ModelFragmentTarget)
	    ;
		Type == 'correspondence' ->
		send(CB, createCorrespondenceInstance, Element, ModelFragment, TargetModel, ModelFragmentTarget)
	    ;
		Type == 'inequality' ->
		send(CB, createInequalityInstance, Element, ModelFragment, TargetModel, ModelFragmentTarget)
	    ;
		Type == 'identityRelation' ->
		send(CB, createIdentityInstance, Element, ModelFragment, TargetModel, ModelFragmentTarget)
	    ;
		format('A ~w failed to copy, please report this bug and send your model to the developers!\n', [Type])
	    )
	)
    ).

createGarpInstance(CB, GarpInstance, ModelFragment, TargetModel, ModelFragmentTarget) :->
    get(GarpInstance, name, EntityName),
    get(GarpInstance?remarks, value, Remarks),
    get(GarpInstance?stateName, value, ConditionOrConsequence),
    get(GarpInstance, entity, EntityDefinition),
    getMappingDefinition(CB?mapping, EntityDefinition, TargetEntityDef),
    
    addEntityInstance(TargetModel, ModelFragmentTarget, TargetEntityDef, EntityName, Remarks, ConditionOrConsequence, @nil),
    get(ModelFragmentTarget?elements, tail, NewElement),

    % Copy the names in different languages for the new element
    copyLanguagesForElement(GarpInstance, NewElement),

    send(CB, updateLayout, GarpInstance, ModelFragment, new(chain), NewElement, ModelFragmentTarget, new(chain)),
    
    % Since the entity is local to this model fragment, the routes are empty
    addMappingElement(CB?mapping, GarpInstance, new(chain), NewElement, new(chain)).

    

createConfigurationInstance(CB, Configuration, ModelFragment, TargetModel, ModelFragmentTarget) :->
    get(Configuration?remarks, value, Remarks),
    get(Configuration?stateName, value, ConditionOrConsequence),
    get(Configuration, definition, ConfigurationDef),
    getMappingDefinition(CB?mapping, ConfigurationDef, TargetConfigurationDef),
 
    get(Configuration, argument1, Argument1),
    get(Configuration, argument2, Argument2),
    get(Configuration, argument1Route, Argument1Route),
    get(Configuration, argument2Route, Argument2Route),
    getMappingElement(CB?mapping, Argument1, Argument1Route, TargetArg1, TargetArg1Route),
    getMappingElement(CB?mapping, Argument2, Argument2Route, TargetArg2, TargetArg2Route),

    addConfigurationInstance(
	TargetModel,
	ModelFragmentTarget, 
	TargetConfigurationDef, 
	TargetArg1, TargetArg1Route,
	TargetArg2, TargetArg2Route,
	Remarks,
	ConditionOrConsequence,
	@nil),

    get(ModelFragmentTarget?elements, tail, NewElement),

    % Copy the names in different languages for the new element
    copyLanguagesForElement(Configuration, NewElement),

    send(CB, updateLayout, Configuration, ModelFragment, new(chain), NewElement, ModelFragmentTarget, new(chain)),
    addMappingElement(CB?mapping, Configuration, new(chain), NewElement, new(chain)).

createAttributeInstance(CB, Attribute, ModelFragment, TargetModel, ModelFragmentTarget) :->
    get(Attribute?remarks, value, Remarks), 
    get(Attribute, definition, AttributeDef),
    getMappingDefinition(CB?mapping, AttributeDef, TargetAttributeDef),
    
    get(Attribute, garpInstance, Instance),
    get(Attribute, instanceRoute, InstanceRoute),
    getMappingElement(CB?mapping, Instance, InstanceRoute, TargetInstance, TargetInstanceRoute),

    get(Attribute, valueReference, Value),
    get(TargetAttributeDef, values, Values),
    get(Values, find, 
	message(@prolog, ==, @arg1?name, Value?name),
	TargetValue
    ),

    get(Attribute?stateName, value, ConditionOrConsequence),

    addAttributeInstance(
	TargetModel,
	ModelFragmentTarget,
	TargetAttributeDef,
	TargetValue,
	TargetInstance, %AbstractEntity,
	TargetInstanceRoute, %AbstractEntityRoute,
	Remarks,
	ConditionOrConsequence,
	@nil),

    get(ModelFragmentTarget?elements, tail, NewElement),

    % Copy the names in different languages for the new element
    copyLanguagesForElement(Attribute, NewElement),

    send(CB, updateLayout, Attribute, ModelFragment, new(chain), NewElement, ModelFragmentTarget, new(chain)),
    addMappingElement(CB?mapping, Attribute, new(chain), NewElement, new(chain)).



createQuantityInstance(CB, Quantity, ModelFragment, TargetModel, ModelFragmentTarget) :->
    get(Quantity?remarks, value, Remarks),
    get(Quantity, definition, QuantityDef),
    getMappingDefinition(CB?mapping, QuantityDef, TargetQuantityDef),
    get(Quantity, quantitySpace, QuantitySpaceDef),
    getMappingDefinition(CB?mapping, QuantitySpaceDef, TargetQuantitySpaceDef),
    get(Quantity, garpInstance, GarpInstance),
    get(Quantity, instanceRoute, GarpInstanceRoute),
    getMappingElement(CB?mapping, GarpInstance, GarpInstanceRoute, TargetInstance, TargetInstanceRoute),
    get(Quantity?quantityAssumptions, copy, TargetQuantityAssumptions),
    get(Quantity?stateName, value, ConditionOrConsequence),

    addQuantityInstance(
	TargetModel, 
	ModelFragmentTarget,
	TargetQuantityDef,
	TargetQuantitySpaceDef,
	TargetInstance,
	TargetInstanceRoute, 
	TargetQuantityAssumptions,
	Remarks,
	ConditionOrConsequence, 
	@nil),

    get(ModelFragmentTarget?elements, tail, NewElement),

    % Copy the names in different languages for the new element
    copyLanguagesForElement(Quantity, NewElement),

    send(CB, updateLayout, Quantity, ModelFragment, new(chain), NewElement, ModelFragmentTarget, new(chain)),
    send(CB, updateQuantityLayout, Quantity, ModelFragment, new(chain), NewElement, ModelFragmentTarget, new(chain)),
    addMappingElement(CB?mapping, Quantity, new(chain), NewElement, new(chain)).

createAssumptionInstance(CB, Assumption, ModelFragment, TargetModel, ModelFragmentTarget) :->
    get(Assumption?remarks, value, Remarks),
    get(Assumption, definition, AssumptionDef),
    getMappingDefinition(CB?mapping, AssumptionDef, TargetAssumptionDef),
    get(Assumption, garpInstance, GarpInstance),
    (
	GarpInstance \== @nil ->
	get(Assumption, instanceRoute, GarpInstanceRoute),
	getMappingElement(CB?mapping, GarpInstance, GarpInstanceRoute, TargetInstance, TargetInstanceRoute)
    ;
	TargetInstance = @nil,
	TargetInstanceRoute = @nil
    ),
    addAssumptionInstance(TargetModel, ModelFragmentTarget, TargetAssumptionDef, Remarks, TargetInstance, TargetInstanceRoute, @nil),

    get(ModelFragmentTarget?elements, tail, NewElement),

    % Copy the names in different languages for the new element
    copyLanguagesForElement(Assumption, NewElement),

    send(CB, updateLayout, Assumption, ModelFragment, new(chain), NewElement, ModelFragmentTarget, new(chain)),
    addMappingElement(CB?mapping, Assumption, new(chain), NewElement, new(chain)).

createValueInstance(CB, Value, _ModelFragment, TargetModel, ModelFragmentTarget) :->
    get(Value, quantity, Quantity),
    get(Value, quantityRoute, QuantityRoute),
    getMappingElement(CB?mapping, Quantity, QuantityRoute, TargetQuantity, TargetQuantityRoute),
    get(Value?stateName, value, ConditionOrConsequence),
    get(Value, derivative, OnDerivative),
    (
        OnDerivative == @off ->
        get(TargetQuantity, quantitySpace, TargetQuantitySpace)
    ;
        OnDerivative == @on,
        get(TargetModel, dqs, TargetQuantitySpace)
    ),
    get(TargetQuantitySpace, values, ValuesChain),	
    get(ValuesChain, find, @arg1?valueName == Value?valueReference?name, ValueReferenceCopy),

    addValueInstance(
	TargetModel, 
	ModelFragmentTarget,
	ValueReferenceCopy,
	TargetQuantity,
	TargetQuantityRoute,
	OnDerivative, 
	ConditionOrConsequence,
	@nil).

createCalculusInstance(CB, Calculus, ModelFragment, TargetModel, ModelFragmentTarget) :->
    get(Calculus, sign, Sign), 
    get(Calculus, type, QuantityOrDerivative), 

    % First argument
    getInequalityOrCalculusArgument(CB, Calculus, argument1, TargetArgument1, TargetArg1Route, TargetArg1QSPoint),

    % Second argument
    getInequalityOrCalculusArgument(CB, Calculus, argument2, TargetArgument2, TargetArg2Route, TargetArg2QSPoint),

    get(Calculus?remarks, value, Remarks), 

    addCalculusInstance(
	TargetModel,
	ModelFragmentTarget,
	Sign,
	QuantityOrDerivative,
	TargetArgument1, TargetArg1Route, TargetArg1QSPoint,
	TargetArgument2, TargetArg2Route, TargetArg2QSPoint,
	Remarks,
	@nil),

    get(ModelFragmentTarget?elements, tail, NewElement),

    % Copy the names in different languages for the new element
    copyLanguagesForElement(Calculus, NewElement),

    send(CB, updateLayout, Calculus, ModelFragment, new(chain), NewElement, ModelFragmentTarget, new(chain)),
    addMappingElement(CB?mapping, Calculus, new(chain), NewElement, new(chain)).

createInequalityInstance(CB, Inequality, ModelFragment, TargetModel, ModelFragmentTarget) :->
    get(Inequality, type, InequalityType), 
    get(Inequality?remarks, value, Remarks),
    get(Inequality?stateName, value, ConditionOrConsequence),

    % First argument
    getInequalityOrCalculusArgument(CB, Inequality, argument1, TargetArgument1, TargetArg1Route, TargetArg1QSPoint),
    get(Inequality, argument1Type, Arg1Type),

    % Second argument
    getInequalityOrCalculusArgument(CB, Inequality, argument2, TargetArgument2, TargetArg2Route, TargetArg2QSPoint),
    get(Inequality, argument2Type, Arg2Type),
    
    addInequalityInstance(
	TargetModel, 
	ModelFragmentTarget, 
	InequalityType, 
	Remarks, 
	ConditionOrConsequence, 
	TargetArgument1, TargetArg1Route, Arg1Type, TargetArg1QSPoint,
	TargetArgument2, TargetArg2Route, Arg2Type, TargetArg2QSPoint, 
	@nil),

    get(ModelFragmentTarget?elements, tail, NewElement),

    % Copy the names in different languages for the new element
    copyLanguagesForElement(Inequality, NewElement),

    send(CB, updateLayout, Inequality, ModelFragment, new(chain), NewElement, ModelFragmentTarget, new(chain)),
    addMappingElement(CB?mapping, Inequality, new(chain), NewElement, new(chain)).

createCorrespondenceInstance(CB, Correspondence, ModelFragment, TargetModel, ModelFragmentTarget) :->
    get(Correspondence?remarks, value, Remarks),
    get(Correspondence, derivative, IsOnDerivative),
    get(Correspondence, directed, IsDirected),   
    get(Correspondence, mirror, IsInverted),
    get(Correspondence, full, IsFull),

    get(Correspondence, argument1, Argument1),
    get(Correspondence, argument1Route, Arg1Route),
    getMappingElement(CB?mapping, Argument1, Arg1Route, TargetArgument1, TargetArg1Route),
    get(Correspondence, argument1Value, Arg1Value),

    get(Correspondence, argument2, Argument2),
    get(Correspondence, argument2Route, Arg2Route),
    getMappingElement(CB?mapping, Argument2, Arg2Route, TargetArgument2, TargetArg2Route),
    get(Correspondence, argument2Value, Arg2Value),

    
    (
        IsOnDerivative == @off, Arg1Value \== @nil, Arg2Value \== @nil ->
        get(TargetArgument1, quantitySpace, TargetQuantitySpace1),
	get(TargetQuantitySpace1, values, Values1),
	get(Values1, find, 
	    message(@prolog, ==, @arg1?name, Arg1Value?name),
	    TargetArg1QSValue
	),
        get(TargetArgument2, quantitySpace, TargetQuantitySpace2),
	get(TargetQuantitySpace2, values, Values2),
	get(Values2, find, 
	    message(@prolog, ==, @arg1?name, Arg2Value?name),
	    TargetArg2QSValue
	)
    ;
        IsOnDerivative == @on, Arg1Value \== @nil, Arg2Value \== @nil ->
        get(TargetModel, dqs, TargetQuantitySpace),
	get(TargetQuantitySpace, values, DQSValues),
	get(DQSValues, find,
	    message(@prolog, ==, @arg1?name, Arg1Value?name),
	    TargetArg1QSValue
	),
	get(DQSValues, find,
	    message(@prolog, ==, @arg1?name, Arg2Value?name),
	    TargetArg2QSValue
	)
    ;
	TargetArg1QSValue = @nil,
	TargetArg2QSValue = @nil
    ),

    addCorrespondenceInstance(
	TargetModel,
	ModelFragmentTarget, 
	IsDirected,
	IsOnDerivative,
	IsInverted,
	IsFull,
	TargetArgument1, TargetArg1Route, TargetArg1QSValue,
	TargetArgument2, TargetArg2Route, TargetArg2QSValue,
	Remarks, @nil),

    get(ModelFragmentTarget?elements, tail, NewElement),

    % Copy the names in different languages for the new element
    copyLanguagesForElement(Correspondence, NewElement),

    send(CB, updateLayout, Correspondence, ModelFragment, new(chain), NewElement, ModelFragmentTarget, new(chain)),
    addMappingElement(CB?mapping, Correspondence, new(chain), NewElement, new(chain)).

createCausalDependency(CB, CausalDependency, ModelFragment, TargetModel, ModelFragmentTarget) :->
    get(CausalDependency?remarks, value, Remarks),
    get(CausalDependency, type, Type),
    get(CausalDependency, sign, Sign),

    get(CausalDependency, argument1, Argument1),
    get(CausalDependency, argument1Route, Arg1Route),
    getMappingElement(CB?mapping, Argument1, Arg1Route, TargetArgument1, TargetArg1Route),

    get(CausalDependency, argument2, Argument2),
    get(CausalDependency, argument2Route, Arg2Route),
    getMappingElement(CB?mapping, Argument2, Arg2Route, TargetArgument2, TargetArg2Route),

  
    addCausalDependency(
	TargetModel,
	ModelFragmentTarget,
	Type,
	Sign,
	TargetArgument1, TargetArg1Route,
	TargetArgument2, TargetArg2Route,
	Remarks,
	@nil),

    get(ModelFragmentTarget?elements, tail, NewElement),

    % Copy the names in different languages for the new element
    copyLanguagesForElement(CausalDependency, NewElement),

    send(CB, updateLayout, CausalDependency, ModelFragment, new(chain), NewElement, ModelFragmentTarget, new(chain)),
    addMappingElement(CB?mapping, CausalDependency, new(chain), NewElement, new(chain)).

createIdentityInstance(CB, Identity, ModelFragment, TargetModel, ModelFragmentTarget) :->
    get(Identity?remarks, value, Remarks),

    get(Identity, argument1, Argument1),
    get(Identity, argument1Route, Arg1Route),
    getMappingElement(CB?mapping, Argument1, Arg1Route, TargetArgument1, TargetArg1Route),

    get(Identity, argument2, Argument2),
    get(Identity, argument2Route, Arg2Route),
    getMappingElement(CB?mapping, Argument2, Arg2Route, TargetArgument2, TargetArg2Route),

    addIdentity(
	TargetModel,
	ModelFragmentTarget,
	TargetArgument1, TargetArg1Route,
	TargetArgument2, TargetArg2Route,
	Remarks,
	@off,
	@nil),

    get(ModelFragmentTarget?elements, tail, NewElement),

    % Copy the names in different languages for the new element
    copyLanguagesForElement(Identity, NewElement),

    send(CB, updateLayout, Identity, ModelFragment, new(chain), NewElement, ModelFragmentTarget, new(chain)),
    addMappingElement(CB?mapping, Identity, new(chain), NewElement, new(chain)).




createImportedFragment(CB, ImportedMF, ModelFragment, TargetModel, ModelFragmentTarget) :->
    get(ImportedMF?remarks, value, Remarks),
    get(ImportedMF, referencedFragment, ModelFragmentDef),
    getMappingDefinition(CB?mapping, ModelFragmentDef, TargetModelFragmentDef),
    
    addModelFragmentInstance(
	TargetModel,
	ModelFragmentTarget,
	TargetModelFragmentDef,
	Remarks,
	@nil),

    get(ModelFragmentTarget?elements, tail, NewElement),

    % Copy the names in different languages for the new element
    copyLanguagesForElement(ImportedMF, NewElement),

    send(CB, updateLayout, ImportedMF, ModelFragment, new(chain), NewElement, ModelFragmentTarget, new(chain)),
    addMappingElement(CB?mapping, ImportedMF, new(chain), NewElement, new(chain)),
    
    new(Route, chain), new(TargetRoute, chain),
    fixImportedElements(CB, ImportedMF, ModelFragment, Route, NewElement, ModelFragmentTarget, TargetRoute).

createFragmentRefiner(CB, FragmentRefiner, ModelFragment, TargetModel, ModelFragmentTarget) :->
    get(FragmentRefiner?remarks, value, Remarks),
    get(FragmentRefiner, refined, ImportedMF),
    get(FragmentRefiner, refinedRoute, ImportedMFRoute),
    getMappingElement(CB?mapping, ImportedMF, ImportedMFRoute, TargetImportedMF, TargetImportedMFRoute),

    get(FragmentRefiner, referencedFragment, ModelFragmentDef),
    getMappingDefinition(CB?mapping, ModelFragmentDef, TargetModelFragmentDef),

    % TODO - FIX ME: Jochem - It should be the path to the TargetModelFragmentDef, not above it!
    getAllMFParentReferences(TargetModelFragmentDef, TargetRefinementRoute),
    
    addRefinementInstance(
	TargetModel,
	ModelFragmentTarget,
	TargetImportedMF,
	TargetImportedMFRoute,
	TargetModelFragmentDef,
	TargetRefinementRoute,
	Remarks,
	@nil),

    get(ModelFragmentTarget, elements, MFTargetElements),
    get(MFTargetElements, size, Size),
    NewElementIndex is Size - 1,
    get(MFTargetElements, nth1, NewElementIndex, NewElement),

    % Copy the names in different languages for the new element
    copyLanguagesForElement(FragmentRefiner, NewElement),

    send(CB, updateLayout, FragmentRefiner, ModelFragment, new(chain), NewElement, ModelFragmentTarget, new(chain)),
    addMappingElement(CB?mapping, FragmentRefiner, new(chain), NewElement, new(chain)).



fixImportedElements(CB, ImportedMF, ModelFragment, Route, NewImportedMF, ModelFragmentTarget, TargetRoute) :-
    get(ImportedMF?referencedFragment, name, SourceParentDefName),
    format('Fixing elements for ~w\n\n', [SourceParentDefName]),

    % Get the elements
    get(ImportedMF?referencedFragment?elements, copy, ImportedMFElements),
    send(ImportedMFElements, union, ImportedMF?referencedFragment?parentReferences),
    deletePredefinedModelFragmentsChain(ImportedMFElements),
    sortAggregateElements(ImportedMFElements),
    %format('ImportedMFElements\n'),
    %printChainElements(ImportedMFElements),nl, nl,
    chain_list(ImportedMFElements, ImportedMFElementsList),

    get(NewImportedMF?referencedFragment?elements, copy, TargetElements),
    send(TargetElements, union, NewImportedMF?referencedFragment?parentReferences),
    deletePredefinedModelFragmentsChain(TargetElements), 
    %format('TargetElements\n'),
    %printChainElements(TargetElements), nl, nl,


    % Fix the new routes
    send(Route, append, ImportedMF),
    send(TargetRoute, append, NewImportedMF),

    %format('De Route\n'),
    %printChainNames(Route), nl,
    %format('De Target Route\n'),
    %printChainNames(TargetRoute), nl,
    
    format('De lijst met geimporteerde elementen: ~w\n', [ImportedMFElementsList]),
    fixImportedElements2(CB, ImportedMFElementsList, ModelFragment, Route, TargetElements, ModelFragmentTarget, TargetRoute, @off). 

fixImportedElements2(CB, ImportedMFElementsList, ModelFragment, Route, TargetElements, ModelFragmentTarget, TargetRoute, ReuseFlag) :-
    forall(
	member(ImportedMFElement, ImportedMFElementsList),
	(
	    get(ImportedMFElement, class_name, Type),
	    format('Fixing ~w\n', [Type]),
	    (
		Type == 'garpInstance' ->
		get(TargetElements, find_all,
		    message(@prolog, ==, @arg1?class_name, Type),
		PotentialGarpInstances),
		chain_list(PotentialGarpInstances, PotentialGarpInstancesList),
		member(PotentialGarpInstance, PotentialGarpInstancesList),
		% Not yet mapped!
		% Same definition
		get(ImportedMFElement, definition, GarpInstanceDef),
		get(PotentialGarpInstance, definition, TargetGarpInstanceDef),
		getMappingDefinition(CB?mapping, GarpInstanceDef, TargetGarpInstanceDef),
		% Same name
		get(ImportedMFElement, name, GarpInstanceName),
		get(PotentialGarpInstance, name, GarpInstanceName),

		( 
		    ReuseFlag = @on ->
		    true
		;
		    send(CB, updateLayout, ImportedMFElement, ModelFragment, Route, PotentialGarpInstance, ModelFragmentTarget, TargetRoute)
		),
		addMappingElement(CB?mapping, ImportedMFElement, Route, PotentialGarpInstance, TargetRoute)
	    ;
		Type == 'configuration' ->
		get(TargetElements, find_all,
		    message(@prolog, ==, @arg1?class_name, Type),
		PotentialConfigurations),
		chain_list(PotentialConfigurations, PotentialConfigurationsList),
		member(PotentialConfiguration, PotentialConfigurationsList),

		% same definition
		get(ImportedMFElement, definition, ConfigurationDef),
		get(PotentialConfiguration, definition, TargetConfigurationDef),
		getMappingDefinition(CB?mapping, ConfigurationDef, TargetConfigurationDef),

		% same argument 1
		get(ImportedMFElement, argument1, Argument1),
		get(ImportedMFElement, argument1Route, Argument1Route),
		get(PotentialConfiguration, argument1, TargetArgument1),
		get(PotentialConfiguration, argument1Route, TargetArgument1Route),
		getMappingElement(CB?mapping, Argument1, Argument1Route, TargetArgument1, TargetArgument1Route),

		% same argument 2
		get(ImportedMFElement, argument2, Argument2),
		get(ImportedMFElement, argument2Route, Argument2Route),
		get(PotentialConfiguration, argument2, TargetArgument2),
		get(PotentialConfiguration, argument2Route, TargetArgument2Route),
		getMappingElement(CB?mapping, Argument2, Argument2Route, TargetArgument2, TargetArgument2Route),

		( 
		    ReuseFlag = @on ->
		    true
		;
		    send(CB, updateLayout, ImportedMFElement, ModelFragment, Route, PotentialConfiguration, ModelFragmentTarget, TargetRoute)
		),
		addMappingElement(CB?mapping, ImportedMFElement, Route, PotentialConfiguration, TargetRoute)
	    ;
		Type == 'garpQuantity' ->
		get(TargetElements, find_all,
		    message(@prolog, ==, @arg1?class_name, Type),
		PotentialGarpQuantities),
		chain_list(PotentialGarpQuantities, PotentialGarpQuantitiesList),
		member(PotentialGarpQuantity, PotentialGarpQuantitiesList),
		% Not mapped yet
		% Same definition
		get(ImportedMFElement, definition, GarpQuantityDef),
		get(PotentialGarpQuantity, definition, TargetGarpQuantityDef),
		getMappingDefinition(CB?mapping, GarpQuantityDef, TargetGarpQuantityDef),
		% Same instance
		get(ImportedMFElement, garpInstance, SourceGarpInstance),
		get(ImportedMFElement, instanceRoute, SourceInstanceRoute),
		get(PotentialGarpQuantity, garpInstance, TargetGarpInstance),
		get(PotentialGarpQuantity, instanceRoute, TargetInstanceRoute),
		getMappingElement(CB?mapping, SourceGarpInstance, SourceInstanceRoute, TargetGarpInstance, TargetInstanceRoute),
	   
		( 
		    ReuseFlag = @on ->
		    true
		;
		    send(CB, updateLayout, ImportedMFElement, ModelFragment, Route, PotentialGarpQuantity, ModelFragmentTarget, TargetRoute),
		    send(CB, updateQuantityLayout, ImportedMFElement, ModelFragment, Route, PotentialGarpQuantity, ModelFragmentTarget, TargetRoute)
		),
		addMappingElement(CB?mapping, ImportedMFElement, Route, PotentialGarpQuantity, TargetRoute)

	    ;
		Type == 'assumptionInstance',
		get(TargetElements, find_all,
		    message(@prolog, ==, @arg1?class_name, Type),
		PotentialAssumptions),
		chain_list(PotentialAssumptions, PotentialAssumptionsList),
		member(PotentialAssumption, PotentialAssumptionsList),

		% Not mapped yet
		not( hasReverseMappingElement(CB?mapping, PotentialAssumption, TargetRoute) ),

		% Same assumption definition
		get(ImportedMFElement, definition, AssumptionDef),
		get(PotentialAssumption, definition, TargetAssumptionDef),
		getMappingDefinition(CB?mapping, AssumptionDef, TargetAssumptionDef),

		% Same associated entity
		get(ImportedMFElement, garpInstance, SourceGarpInstance),
		( 
		    SourceGarpInstance \== @nil,
		    get(ImportedMFElement, instanceRoute, SourceInstanceRoute),
		    get(PotentialAssumption, garpInstance, TargetGarpInstance),
		    get(PotentialAssumption, instanceRoute, TargetInstanceRoute),
		    getMappingElement(CB?mapping, SourceGarpInstance, SourceInstanceRoute, TargetGarpInstance, TargetInstanceRoute)
		;
		    true
		),

		( 
		    ReuseFlag = @on ->
		    true
		;
		    send(CB, updateLayout, ImportedMFElement, ModelFragment, Route, PotentialAssumption, ModelFragmentTarget, TargetRoute)
		),
		addMappingElement(CB?mapping, ImportedMFElement, Route, PotentialAssumption, TargetRoute)

	    ;
		Type == 'value' % Nothing to do
	    ;
		Type == 'calculus',
		get(TargetElements, find_all,
		    message(@prolog, ==, @arg1?class_name, Type),
		PotentialCalculi),
		chain_list(PotentialCalculi, PotentialCalculiList),
		member(PotentialCalculus, PotentialCalculiList),

		% Same features
		get(PotentialCalculus, type, CalculusType),
		get(ImportedMFElement, type, CalculusType),
		get(PotentialCalculus, sign, CalculusSign),
		get(ImportedMFElement, sign, CalculusSign),


		% Same value
		get(PotentialCalculus?argument1QSPoint, name, Value1),
		get(ImportedMFElement?argument1QSPoint, name, Value1),
		get(PotentialCalculus?argument2QSPoint, name, Value2),
		get(ImportedMFElement?argument2QSPoint, name, Value2),

		% same argument 1
		get(ImportedMFElement, argument1, Argument1),
		get(ImportedMFElement, argument1Route, Argument1Route),
		get(PotentialCalculus, argument1, TargetArgument1),
		get(PotentialCalculus, argument1Route, TargetArgument1Route),
		getMappingElement(CB?mapping, Argument1, Argument1Route, TargetArgument1, TargetArgument1Route),

		% same argument 2
		get(ImportedMFElement, argument2, Argument2),
		get(ImportedMFElement, argument2Route, Argument2Route),
		get(PotentialCalculus, argument2, TargetArgument2),
		get(PotentialCalculus, argument2Route, TargetArgument2Route),
		getMappingElement(CB?mapping, Argument2, Argument2Route, TargetArgument2, TargetArgument2Route),

		( 
		    ReuseFlag = @on ->
		    true
		;
		    send(CB, updateLayout, ImportedMFElement, ModelFragment, Route, PotentialCalculus, ModelFragmentTarget, TargetRoute)
		),
		addMappingElement(CB?mapping, ImportedMFElement, Route, PotentialCalculus, TargetRoute)
	    ;
		Type == 'fragmentRefiner',
		get(TargetElements, find_all,
		    message(@prolog, ==, @arg1?class_name, Type),
		PotentialFragmentRefiners),
		chain_list(PotentialFragmentRefiners, PotentialFragmentRefinersList),
		member(PotentialFragmentRefiner, PotentialFragmentRefinersList),
		% Not mapped yet
		not( hasReverseMappingElement(CB?mapping, PotentialFragmentRefiner, TargetRoute) ),

		% Same def
		get(ImportedMFElement, referencedFragment, ImportedFragmentDef),
		get(PotentialFragmentRefiner, referencedFragment, TargetImportedFragmentDef),
		getMappingDefinition(CB?mapping, ImportedFragmentDef, TargetImportedFragmentDef),

		%send(CB, updateLayout, ImportedMFElement, ModelFragment, Route, PotentialImportedFragment, ModelFragmentTarget, TargetRoute),
		addMappingElement(CB?mapping, ImportedMFElement, Route, PotentialImportedFragment, TargetRoute),

		get(Route, copy, NewRoute),
		get(TargetRoute, copy, NewTargetRoute),
		fixImportedElements(CB, ImportedMFElement, ModelFragment, NewRoute, PotentialFragmentRefiner, ModelFragmentTarget, NewTargetRoute)
	    ;
		Type == 'fragmentRefinerIdentity'
		% Nothing to do!
	    ;
		Type == 'importedFragment' ->
		get(TargetElements, find_all,
		    message(@prolog, ==, @arg1?class_name, Type),
		PotentialImportedFragments),
		chain_list(PotentialImportedFragments, PotentialImportedFragmentsList),
		member(PotentialImportedFragment, PotentialImportedFragmentsList),
		% Not mapped yet
		not( hasReverseMappingElement(CB?mapping, PotentialImportedFragment, TargetRoute) ),
		%not( hasMappingElement(CB?mapping, ImportedMFElement, Route) ),

		% Same def
		get(ImportedMFElement, referencedFragment, ImportedFragmentDef),
		get(PotentialImportedFragment, referencedFragment, TargetImportedFragmentDef),
		getMappingDefinition(CB?mapping, ImportedFragmentDef, TargetImportedFragmentDef),

		%send(CB, updateLayout, ImportedMFElement, ModelFragment, Route, PotentialImportedFragment, ModelFragmentTarget, TargetRoute),
		addMappingElement(CB?mapping, ImportedMFElement, Route, PotentialImportedFragment, TargetRoute),

		get(Route, copy, NewRoute),
		get(TargetRoute, copy, NewTargetRoute),
		fixImportedElements(CB, ImportedMFElement, ModelFragment, NewRoute, PotentialImportedFragment, ModelFragmentTarget, NewTargetRoute)
	    ;
		Type == 'garpAttribute',
		get(TargetElements, find_all,
		    message(@prolog, ==, @arg1?class_name, Type),
		PotentialAttributes),
		chain_list(PotentialAttributes, PotentialAttributesList),
		member(PotentialAttribute, PotentialAttributesList),

		% Same attribute definition
		get(ImportedMFElement, definition, AttributeDef),
		get(PotentialAttribute, definition, TargetAttributeDef),
		getMappingDefinition(CB?mapping, AttributeDef, TargetAttributeDef),
		
		% Same abstract entity instance
		get(ImportedMFElement, garpInstance, SourceGarpInstance),
		get(ImportedMFElement, instanceRoute, SourceInstanceRoute),
		get(PotentialAttribute, garpInstance, TargetGarpInstance),
		get(PotentialAttribute, instanceRoute, TargetInstanceRoute),
		getMappingElement(CB?mapping, SourceGarpInstance, SourceInstanceRoute, TargetGarpInstance, TargetInstanceRoute),

		( 
		    ReuseFlag = @on ->
		    true
		;
		    send(CB, updateLayout, ImportedMFElement, ModelFragment, Route, PotentialAttribute, ModelFragmentTarget, TargetRoute)
		),
		addMappingElement(CB?mapping, ImportedMFElement, Route, PotentialAttribute, TargetRoute)
	    ;
		Type == 'garpQuantityRelation' ->
		get(TargetElements, find_all,
		    message(@prolog, ==, @arg1?class_name, Type),
		PotentialCausalDependencies),
		chain_list(PotentialCausalDependencies, PotentialCausalDependenciesList),
		member(PotentialCausalDependency, PotentialCausalDependenciesList),
		% Same type and sign
		get(PotentialCausalDependency, type, DependencyType),
		get(PotentialCausalDependency, sign, Sign),
		get(ImportedMFElement, type, DependencyType),
		get(ImportedMFElement, sign, Sign),

		% same argument 1
		get(ImportedMFElement, argument1, Argument1),
		get(ImportedMFElement, argument1Route, Argument1Route),
		get(PotentialCausalDependency, argument1, TargetArgument1),
		get(PotentialCausalDependency, argument1Route, TargetArgument1Route),
		getMappingElement(CB?mapping, Argument1, Argument1Route, TargetArgument1, TargetArgument1Route),

		% same argument 2
		get(ImportedMFElement, argument2, Argument2),
		get(ImportedMFElement, argument2Route, Argument2Route),
		get(PotentialCausalDependency, argument2, TargetArgument2),
		get(PotentialCausalDependency, argument2Route, TargetArgument2Route),
		getMappingElement(CB?mapping, Argument2, Argument2Route, TargetArgument2, TargetArgument2Route),

		( 
		    ReuseFlag = @on ->
		    true
		;
		    send(CB, updateLayout, ImportedMFElement, ModelFragment, Route, PotentialCausalDependency, ModelFragmentTarget, TargetRoute)
		),
		addMappingElement(CB?mapping, ImportedMFElement, Route, PotentialCausalDependency, TargetRoute)
	    ;
		Type == 'correspondence' ->
		get(TargetElements, find_all,
		    message(@prolog, ==, @arg1?class_name, Type),
		PotentialCorrespondences),
		chain_list(PotentialCorrespondences, PotentialCorrespondencesList),
		member(PotentialCorrespondence, PotentialCorrespondencesList),
		% Same features
		get(PotentialCorrespondence, derivative, Derivative),
		get(ImportedMFElement, derivative, Derivative),
		get(PotentialCorrespondence, directed, Directed),
		get(ImportedMFElement, directed, Directed),
		get(PotentialCorrespondence, mirror, Mirror),
		get(ImportedMFElement, mirror, Mirror),
		get(PotentialCorrespondence, full, Full),
		get(ImportedMFElement, full, Full),

		% same argument 1
		get(ImportedMFElement, argument1, Argument1),
		get(ImportedMFElement, argument1Route, Argument1Route),
		get(PotentialCorrespondence, argument1, TargetArgument1),
		get(PotentialCorrespondence, argument1Route, TargetArgument1Route),
		getMappingElement(CB?mapping, Argument1, Argument1Route, TargetArgument1, TargetArgument1Route),

		% same argument 2
		get(ImportedMFElement, argument2, Argument2),
		get(ImportedMFElement, argument2Route, Argument2Route),
		get(PotentialCorrespondence, argument2, TargetArgument2),
		get(PotentialCorrespondence, argument2Route, TargetArgument2Route),
		getMappingElement(CB?mapping, Argument2, Argument2Route, TargetArgument2, TargetArgument2Route),

		( 
		    ReuseFlag = @on ->
		    true
		;
		    send(CB, updateLayout, ImportedMFElement, ModelFragment, Route, PotentialCorrespondence, ModelFragmentTarget, TargetRoute)
		),
		addMappingElement(CB?mapping, ImportedMFElement, Route, PotentialCorrespondence, TargetRoute)
	    ;
		Type == 'inequality' ->
		get(TargetElements, find_all,
		    message(@prolog, ==, @arg1?class_name, Type),
		PotentialInequalities),
		chain_list(PotentialInequalities, PotentialInequalitiesList),
		member(PotentialInequality, PotentialInequalitiesList),

		% Same features
		get(PotentialInequality, type, InequalityType),
		get(ImportedMFElement, type, InequalityType),
		get(PotentialInequality, argument1Type, Arg1Type),
		get(ImportedMFElement, argument1Type, Arg1Type),
		get(PotentialInequality, argument2Type, Arg2Type),
		get(ImportedMFElement, argument2Type, Arg2Type),

		% Same value
		get(PotentialInequality?argument1QSPoint, name, Value1),
		get(ImportedMFElement?argument1QSPoint, name, Value1),
		get(PotentialInequality?argument2QSPoint, name, Value2),
		get(ImportedMFElement?argument2QSPoint, name, Value2),

		% same argument 1
		get(ImportedMFElement, argument1, Argument1),
		get(ImportedMFElement, argument1Route, Argument1Route),
		get(PotentialInequality, argument1, TargetArgument1),
		get(PotentialInequality, argument1Route, TargetArgument1Route),
		getMappingElement(CB?mapping, Argument1, Argument1Route, TargetArgument1, TargetArgument1Route),

		% same argument 2
		get(ImportedMFElement, argument2, Argument2),
		get(ImportedMFElement, argument2Route, Argument2Route),
		get(PotentialInequality, argument2, TargetArgument2),
		get(PotentialInequality, argument2Route, TargetArgument2Route),
		getMappingElement(CB?mapping, Argument2, Argument2Route, TargetArgument2, TargetArgument2Route),

		( 
		    ReuseFlag = @on ->
		    true
		;
		    send(CB, updateLayout, ImportedMFElement, ModelFragment, Route, PotentialInequality, ModelFragmentTarget, TargetRoute)
		),
		addMappingElement(CB?mapping, ImportedMFElement, Route, PotentialInequality, TargetRoute)
	    ;
		Type == 'identityRelation',
		get(TargetElements, find_all,
		    message(@prolog, ==, @arg1?class_name, Type),
		PotentialIdentities),
		chain_list(PotentialIdentities, PotentialIdentitiesList),
		member(PotentialIdentity, PotentialIdentitiesList),

		% same argument 1
		get(ImportedMFElement, argument1, Argument1),
		get(ImportedMFElement, argument1Route, Argument1Route),
		get(PotentialIdentity, argument1, TargetArgument1),
		get(PotentialIdentity, argument1Route, TargetArgument1Route),
		getMappingElement(CB?mapping, Argument1, Argument1Route, TargetArgument1, TargetArgument1Route),

		% same argument 2
		get(ImportedMFElement, argument2, Argument2),
		get(ImportedMFElement, argument2Route, Argument2Route),
		get(PotentialIdentity, argument2, TargetArgument2),
		get(PotentialIdentity, argument2Route, TargetArgument2Route),
		getMappingElement(CB?mapping, Argument2, Argument2Route, TargetArgument2, TargetArgument2Route),

		( 
		    ReuseFlag = @on ->
		    true
		;
		    send(CB, updateLayout, ImportedMFElement, ModelFragment, Route, PotentialIdentity, ModelFragmentTarget, TargetRoute)
		),
		addMappingElement(CB?mapping, ImportedMFElement, Route, PotentialIdentity, TargetRoute)
	    ;
		/* If you are trying to do reuse, and finding an element fails, this function should fail */
		(
		    ReuseFlag = @on ->
		    fail
		;
		    format('Problem setting layout of imported element: ~w\n', [Type])
		)
	    )
	)
    ).


getInequalityOrCalculusArgument(CB, Element, WhichArgument, TargetArgument, TargetArgRoute, TargetArgQSPoint) :-
    (
	WhichArgument == argument1 ->
	get(Element, argument1, Argument),
	get(Element, argument1Route, ArgRoute),
	get(Element, argument1QSPoint, ArgQSPoint)

    ;
	WhichArgument == argument2 ->
	get(Element, argument2, Argument),
	get(Element, argument2Route, ArgRoute),
	get(Element, argument2QSPoint, ArgQSPoint)
    ),
    getMappingElement(CB?mapping, Argument, ArgRoute, TargetArgument, TargetArgRoute),
     
    (
	ArgQSPoint \== @nil,
	get(TargetArgument?quantitySpace, values, Values),
	get(Values, find, 
	    message(@prolog, ==, @arg1?valueName, ArgQSPoint?name),
	    TargetArgQSPoint)
    ;
	TargetArgQSPoint = @nil
    ).

updateLayout(_CB, SourceElement, SourceModelFragment, SourceRoute, TargetElement, TargetModelFragment, TargetRoute) :->
    %findRouteInGarp(SourceElement, SourceModelFragment, SourceRouteChain, _SourceMainMF),
    get(SourceModelFragment, layOutInfo, SourceElement, relPosition, SourceRoute, Point), % updated to a relative position
    get(SourceModelFragment, layOutInfo, SourceElement, hidden, SourceRoute, HiddenFlag),

    get(Point, copy, TargetPoint),

    %findRouteInGarp(TargetElement, TargetModelFragment, TargetRouteChain, _TargetMainMF),
    send(TargetModelFragment, layOutInfo, TargetElement, relPosition, TargetPoint, TargetRoute), % updated to a relative position
    send(TargetModelFragment, layOutInfo, TargetElement, hidden, HiddenFlag, TargetRoute).

updateQuantityLayout(_CB, SourceElement, SourceModelFragment, SourceRoute, TargetElement, TargetModelFragment, TargetRoute) :-> 
    /* The layout info of the original quantity */
    get(SourceModelFragment, layOutInfo, SourceElement, qsElementRelPosition, SourceRoute, MagQSPoint), % updated to a relative position
    get(SourceModelFragment, layOutInfo, SourceElement, qsElementHidden, SourceRoute, MagQSHiddenFlag),
    get(SourceModelFragment, layOutInfo, SourceElement, dplusRelPosition, SourceRoute, DerivativePoint), % updated to a relative position
    get(SourceModelFragment, layOutInfo, SourceElement, dplusHidden, SourceRoute, DerivativeHiddenFlag),
    get(SourceModelFragment, layOutInfo, SourceElement, derivativeElementRelPosition, SourceRoute, DerQSPoint), % updated to a relative position
    get(SourceModelFragment, layOutInfo, SourceElement, derivativeElementHidden, SourceRoute, DerQSHiddenFlag),

    /* Create copies of the point objects */
    get(MagQSPoint, copy, MagQSPointTarget),
    get(DerivativePoint, copy, DerivativePointTarget),
    get(DerQSPoint, copy, DerQSPointTarget),

    /* Set the layout of the new quantity */
    send(TargetModelFragment, layOutInfo, TargetElement, qsElementRelPosition, MagQSPointTarget, TargetRoute), % updated to a relative position
    send(TargetModelFragment, layOutInfo, TargetElement, qsElementHidden, MagQSHiddenFlag, TargetRoute),
    send(TargetModelFragment, layOutInfo, TargetElement, dplusRelPosition, DerivativePointTarget, TargetRoute), % updated to a relative position
    send(TargetModelFragment, layOutInfo, TargetElement, dplusHidden, DerivativeHiddenFlag, TargetRoute),
    send(TargetModelFragment, layOutInfo, TargetElement, derivativeElementRelPosition, DerQSPointTarget, TargetRoute), % updated to a relative position
    send(TargetModelFragment, layOutInfo, TargetElement, derivativeElementHidden, DerQSHiddenFlag, TargetRoute).

initialiseMapping(CB, SourceModel, TargetModel) :->
    % Entity
    getTopEntity(SourceModel, EntitySource), 
    getTopEntity(TargetModel, EntityTarget),
    addMappingDefinition(CB?mapping, EntitySource, EntityTarget),

    % Agent
    getTopAgent(SourceModel, AgentSource), 
    getTopAgent(TargetModel, AgentTarget),
    addMappingDefinition(CB?mapping, AgentSource, AgentTarget),

    % Assumption
    getTopAssumption(SourceModel, AssumptionSource), 
    getTopAssumption(TargetModel, AssumptionTarget),
    addMappingDefinition(CB?mapping, AssumptionSource, AssumptionTarget),

    % Static model fragment
    getTopStaticFragment(SourceModel, StaticMFSource),
    getTopStaticFragment(TargetModel, StaticMFTarget), 
    addMappingDefinition(CB?mapping, StaticMFSource, StaticMFTarget),
    
    % Process model fragment
    getTopProcessFragment(SourceModel, ProcessMFSource),
    getTopProcessFragment(TargetModel, ProcessMFTarget), 
    addMappingDefinition(CB?mapping, ProcessMFSource, ProcessMFTarget),
    
    % Agent model fragment
    getTopAgentFragment(SourceModel, AgentMFSource),
    getTopAgentFragment(TargetModel, AgentMFTarget), 
    addMappingDefinition(CB?mapping, AgentMFSource, AgentMFTarget).


addMappingDefinition(Hash, DefinitionSource, DefinitionTarget) :-
    send(Hash, append, prolog(definition(DefinitionSource)), prolog(definition(DefinitionTarget))).
hasMappingDefinition(Hash, DefinitionSource) :-
    getMappingDefinition(Hash, DefinitionSource, _DefinitionTarget).
getMappingDefinition(Hash, DefinitionSource, DefinitionTarget) :-
    get(Hash, find_value,
	message(@prolog, ==, @arg1, prolog(definition(DefinitionSource))),
	DefinitionTargetFunctor),
    arg(1, DefinitionTargetFunctor, DefinitionTarget).

addMappingElement(Hash, ElementSource, RouteSource, ElementTarget, RouteTarget) :-
    chain_list(RouteSource, RouteSourceList),
    chain_list(RouteTarget, RouteTargetList),
    send(Hash, append, prolog(element(ElementSource, RouteSourceList)), prolog(element(ElementTarget, RouteTargetList))).
hasMappingElement(Hash, ElementSource, RouteSource) :-
    getMappingElement(Hash, ElementSource, RouteSource, _ElementTarget, _RouteTarget).
getMappingElement(Hash, ElementSource, RouteSource, ElementTarget, RouteTarget) :-
    chain_list(RouteSource, RouteSourceList),
    get(Hash, find_value,
	message(@prolog, ==, @arg1, prolog(element(ElementSource, RouteSourceList))),
	ElementTargetFunctor),
    arg(1, ElementTargetFunctor, ElementTarget),
    arg(2, ElementTargetFunctor, RouteTargetList),
    chain_list(RouteTarget, RouteTargetList).

hasReverseMappingElement(Hash, ElementTarget, RouteTarget) :- 
    getReverseMappingElement(Hash, ElementTarget, RouteTarget, _ElementSource, _RouteSource).
getReverseMappingElement(Hash, ElementTarget, RouteTarget, ElementSource, RouteSource) :-
    chain_list(RouteTarget, RouteTargetList),
    get(Hash, find_key,
	message(@prolog, ==, @arg2, prolog(element(ElementTarget, RouteTargetList))),
	ElementSourceFunctor),
    arg(1, ElementSourceFunctor, ElementSource),
    arg(2, ElementSourceFunctor, RouteSourceList),
    chain_list(RouteSource, RouteSourceList).


clearMapping(CB) :->
    send(CB?mapping, clear).

:-pce_end_class.

