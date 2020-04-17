/************ New Model Manipulation API *************/

emptyAllHierarchies(Model) :-
    emptyScenarioDefinitionHierarchy(Model),
    emptyModelFragmentDefinitionHierarchy(Model),
    emptyEntityHierarchy(Model, 'Entity'),
    emptyEntityHierarchy(Model, 'Agent'),
    emptyEntityHierarchy(Model, 'Assumption'),
    emptyConfigurationHierarchy(Model),
    emptyAttributeDefinitionHierarchy(Model),
    emptyQuantityDefinitionHierarchy(Model),
    emptyQuantitySpaceDefinitionHierarchy(Model).

emptyEntityHierarchy(Model, Type) :-
    completeEntityList(Model, Type, TopDownChain),
    send(TopDownChain, delete, TopDownChain?head),
    new(BottomUpChain, chain),
    send(TopDownChain, for_all,
	message(BottomUpChain, prepend, @arg1)
    ),
    send(BottomUpChain, for_all,
	message(@prolog, deleteEntityDefinition, Model, @arg1)
    ).
addEntityDefinition(Model, EntityName, ParentName, Comment, Editor) :-
    %format('"~w" "~w" "~w"\n', [EntityName, ParentName, Comment]),
    changeToModel(Model, LastModel),

    % Add the entity to the model
    getEntityDefinitionName(Model, ParentName, Parent),
    %get(Model?abstractEntities, find, @arg1?name == ParentName, Parent),
    string_to_atom(CommentString, Comment),
    send(Model, changeRequest, newHObjectChild,
        Parent, %Parent,
        Editor,  %D?editor,
        EntityName, %D?name_member?selection,
	CommentString), %D?remarks_member?contents).

    changeToModel(LastModel, _).

/* Not done, nor tested! 
changeEntityDefinition(Model, EntityName, ParentName, Comment) :-
    changeToModel(Model, LastModel),
    send(@model, changeRequest, changeHObject,
	    D?element,
	    D?editor,
	    D?name_member?selection,
	    Parent,
	    D?remarks_member?contents),
    changeToModel(LastModel, _).
*/

deleteEntityDefinition(Model, Entity) :-
    changeToModel(Model, LastModel),
    send(Model, changeRequest, deleteHObjectTree,
	Entity, %EE?selectedNode?element,
	@nil %EE
    ),
    changeToModel(LastModel, _).

emptyConfigurationHierarchy(Model) :-
    send(Model?configurationDefinitions, for_all,
	message(@prolog, deleteConfigurationDefinition, Model, @arg1)
    ).

addConfigurationDefinition(Model, Editor, ConfigurationDefName, ConfigurationDefRemarks) :-
    changeToModel(Model, LastModel),
    send(Model, changeRequest, addConfigurationDef,
	Model,
	Editor, % D,
	ConfigurationDefName, % D?name_member?selection,
	ConfigurationDefRemarks %D?remarks_member?contents
    ),
    changeToModel(LastModel, _UnimportantLastModel).

/* Not done, nor tested! 
changeConfigurationDefinition(Model, ConfigurationDef) :-
    changeToModel(Model, LastModel),
    @model->>changeRequest(changeConfigurationDef,
	D?def,
	D,
    	D?name_member?selection,
	D?remarks_member?contents),
    changeToModel(LastModel, _UnimportantLastModel).
*/

emptyAttributeDefinitionHierarchy(Model) :-
    send(Model?attributeDefinitions, for_all,
	message(@prolog, deleteAttributeDefinition, Model, @arg1)
    ).

deleteConfigurationDefinition(Model, ConfigurationDef) :-
    changeToModel(Model, LastModel),
    send(Model, changeRequest, deleteConfigurationDef,
	Model, 
	@nil, % D,
	ConfigurationDef %D?def
    ),
    changeToModel(LastModel, _UnimportantModel).

addAttributeDefinition(Model, Editor, Label, ValueReferencesChain, Remarks) :- 
    changeToModel(Model, LastModel),
    send(Model, changeRequest, addAttributeDef,
        Model, %@model,
        Editor, %D,
        Label, %D?name_member?selection,
        ValueReferencesChain,
        Remarks %D?remarks_member?contents
    ),
    changeToModel(LastModel, _UnimportantModel).

%changeAttributeDefinition(Model, AttributeDef) :-
%    @model->>changeRequest(changeAttributeDef,
%	D?attributeDef,
%	D,
%	D?name_member?selection,
%	Values,
%	D?remarks_member?contents).

deleteAttributeDefinition(Model, AttributeDef) :-
    changeToModel(Model, LastModel),
    send(Model, changeRequest, deleteAttributeDef,
	Model,
	@nil, %D,
	AttributeDef %D?attributeDef),
    ),
    changeToModel(LastModel, _UnimportantModel).

emptyQuantitySpaceDefinitionHierarchy(Model) :-
    send(Model?quantitySpaces, for_all,
	if(
	    message(@prolog, ==, @arg1?name, 'Mzp'),
	    message(@prolog, true),
	    message(@prolog, deleteQuantitySpaceDefinition, Model, @arg1)
	)
    ).

addQuantitySpaceDefinition(Model, Editor, Label, QualitativeValueReferencesChain, Remarks) :-
    changeToModel(Model, LastModel),
    send(Model, changeRequest, addQuantitySpace,
        Model,
        Editor, %D,
        Label, %D?qsName?selection,
        QualitativeValueReferencesChain, % D?qsShow?currentValues,
        Remarks %D?remarks_member?contents)
    ),
    changeToModel(LastModel, _UnimportantModel).

%changeQuantitySpaceDefinition(Model, QuantitySpaceDefinition, Label, QualitativeValueReferencesChain, Remarks) :-

deleteQuantitySpaceDefinition(Model, QuantitySpaceDefinition) :-
    send(Model, changeRequest, deleteQuantitySpace,
	Model,
	@nil,
	QuantitySpaceDefinition).

emptyQuantityDefinitionHierarchy(Model) :-
    send(Model?quantityDefinitions, for_all,
	message(@prolog, deleteQuantityDefinition, Model, @arg1)
    ).

addQuantityDefinition(Model, Editor, Label, QuantitySpacesChain, Remarks) :-
    changeToModel(Model, LastModel),
    send(Model, changeRequest, addQuantityDef,
        Model,
        Editor, %D,
        Label, %D?quantityName?selection,
        QuantitySpacesChain, %D?currentAllowedQS,
        Remarks %D?remarks?contents)
    ),
    changeToModel(LastModel, _UnimportantModel).


changeQuantityDefinition(Model, Editor, Quantity, Label, QuantitySpacesChain, Remarks) :- 
    changeToModel(Model, LastModel),
    send(Model, changeRequest, changeQuantityDef,
	Quantity, %D?quantityDef,
	Editor, %D,
	Label, %D?quantityName?selection,
	QuantitySpacesChain, %D?currentAllowedQS,
	Remarks %D?remarks?contents
    ),
    changeToModel(LastModel, Model).

    


deleteQuantityDefinition(Model, Quantity) :-
    send(Model, changeRequest, deleteQuantityDef,
	Model,
	@nil,
	Quantity).


addModelFragmentDefinition(Model, Editor, Label, Remarks, ParentModelFragmentsChain, ActiveFlag) :-
    changeToModel(Model, LastModel),
    send(Model, changeRequest, newMF,
        Model,
        Editor, % D?editor,
        Label, % D?name_member?selection,
	Remarks, % D?remarks_member?contents,
        ParentModelFragmentsChain, %D?currentParents
        ActiveFlag
    ),
    changeToModel(LastModel, _UnimportantModel).

%changeModelFragmentDefinition(Model, ModelFragment, Label, Remarks, ParentModelFragmentsChain, ActiveFlag) :-
%	@model->>changeRequest(changeMF,
%		Fragment,
%		E,
%		Fragment?name,
%		Fragment?remarks,
%		Fragment?parents,
%		Fragment?active?negate
%		).

deleteModelFragmentDefinition(Model, ModelFragment, Editor) :-
    changeToModel(Model, LastModel),
    emptyModelFragment(Model, ModelFragment),
    send(Model, changeRequest, deleteMF, ModelFragment, Editor),
    changeToModel(LastModel, _UnimportantModel).

emptyModelFragmentDefinitionHierarchy(Model) :-
    get(Model?modelFragments, copy, MFChain),
    %deletePredefinedModelFragmentsChain(MFChain),
    emptyModelFragmentDefinitionHierarchy2(Model, MFChain).

emptyModelFragmentDefinitionHierarchy2(Model, ModelFragments) :-
    (
	get(ModelFragments, size, 3)
    ;
	send(ModelFragments, for_all,
	    if(
		and(
		    not( message(@prolog, isPredefinedModelFragment, @arg1) ),
		    message(@prolog, notImportedModelFragment, @arg1)
		),
		and(
		    message(@prolog, format, 'Deleting MF ~w\n', @arg1?name),
		    message(ModelFragments, delete, @arg1),
		    message(@prolog, deleteModelFragmentDefinition, Model, @arg1, @nil)
		),
		message(@prolog, true)
	    )
	),
	emptyModelFragmentDefinitionHierarchy2(Model, ModelFragments)
    ).

notImportedModelFragment(ModelFragmentDef) :-
    get(ModelFragmentDef, name, Name),
    format('Checking if ~w is conditional in other MFs\n', [Name]),
    get(ModelFragmentDef, conditionalIn, MFChain),
    send(MFChain, empty),
    format('MF ~w is empty\n', [Name]). 

excludesModelFragment(ModelFragment, ModelFragmentDef) :-
    get(ModelFragment?elements, find_all,
	message(@prolog, ==, @arg1?class_name, 'importedFragment'),
	ImportedFragments),
    get(ImportedFragments, map, @arg1?referencedFragment, ImportedFragmentDefs),
    not( send(ImportedFragmentDefs, member, ModelFragmentDef) ).



addScenarioDefinition(Model, Editor, Label, Remarks) :-
    changeToModel(Model, LastModel),
    send(Model, changeRequest, newInputSystem,
	Model,
        Editor,
        Label, %D?name_member?selection,
        Remarks %D?remarks_member?contents
    ),
    changeToModel(LastModel, _UnimportantModel).

%changeScenarioDefinition(Model, Editor, Scenario, Label, Remarks) :-
deleteScenarioDefinition(Model, Scenario, Editor) :-
    changeToModel(Model, LastModel),
    emptyModelFragment(Model, Scenario),
    send(Model, changeRequest, deleteInputSystem,
	Scenario,
	Editor),
    changeToModel(LastModel, Model).

emptyScenarioDefinitionHierarchy(Model) :-
    send(Model?modelFragments, for_all,
	if(
	    message(@prolog, ==, @arg1?class_name, 'inputSystem'),
	    message(@prolog, deleteScenarioDefinition, Model, @arg1, @nil),
	    message(@prolog, true)
	)
    ).

/******************* INSTANCES ***************/

emptyModelFragment(Model, ModelFragment) :-
    changeToModel(Model, LastModel),
    send(ModelFragment?elements, clear),
    send(ModelFragment?parentReferences, clear),
    changeToModel(LastModel, _UnimportantModel).

addFragmentInstance(Model, ModelFragment, ModelFragmentDef, Remarks, Editor) :-
    changeToModel(Model, LastModel),
    send(Model, changeRequest, newConditionalFragment,
	ModelFragment, % D?modelFragment,
	Editor, %D?editor,
	ModelFragmentDef, %D?selected,
	Remarks %D?remarks_member?contents
    ),
    changeToModel(LastModel, _UnimportantModel).

addEntityInstance(Model, ModelFragment, EntityDef, Name, Remarks, ConditionOrConsequence, Editor) :- 
    changeToModel(Model, LastModel),
    send(Model, changeRequest, newFInstance,
	ModelFragment, %D?modelFragment,
        Editor, % D?editor,
        EntityDef, %D?entity,
        ConditionOrConsequence, %D?state,
        Name, %D?instanceName,
        Remarks %D?remarks).
    ),
    changeToModel(LastModel, _UnimportantModel).

addAssumptionInstance(Model, ModelFragment, AssumptionDef, Remarks, AbstractEntityInstance, AbstractEntityInstanceRoute, Editor) :-
    changeToModel(Model, LastModel),
    send(Model, changeRequest, newAssumptionInstance,
        ModelFragment, %Aggregate, %D?modelFragment,
        Editor, %@nil, % D?editor,
        AssumptionDef, %Parent, % D?assumption,
        Remarks, %D?remarks
	AbstractEntityInstance, %D?instance,
	AbstractEntityInstanceRoute %D?instanceRoute
    ),
    changeToModel(LastModel, _UnimportantModel).

addQuantityInstance(Model, ModelFragment, QuantityDef, QuantitySpaceDef, AbstractEntity, AbstractEntityRoute, QuantityAssumptions,
		    Remarks, ConditionOrConsequence, Editor) :-
    changeToModel(Model, LastModel),
    send(Model, changeRequest, newQuantity,
        ModelFragment, %Aggregate, %D?modelFragment,
        Editor, %@nil, %D?editor,
        AbstractEntity, % QuantitySource, %D?instance,
        AbstractEntityRoute, %QuantitySpaceRouteChain, %D?instanceRoute,
        ConditionOrConsequence, %D?state_member?selection,
        QuantityDef, %Parent, %D?selectedQuantityDef,
        QuantitySpaceDef, %D?selectedQuantitySpace,
        Remarks, %D?remarks_member?contents
	QuantityAssumptions
    ),
    changeToModel(LastModel, _UnimportantModel).

addAttributeInstance(Model, ModelFragment, AttributeDef, Value, AbstractEntity, AbstractEntityRoute,
		    Remarks, ConditionOrConsequence, Editor) :-
    changeToModel(Model, LastModel),
    send(Model, changeRequest, newAttribute,
        ModelFragment, %Aggregate, %D?modelFragment,
        Editor, %@nil, %D?editor,
        AbstractEntity, %D?instance,
        AbstractEntityRoute, %D?instanceRoute,
        ConditionOrConsequence, %D?state_member?selection,
        AttributeDef, %D?selectedAttributeDef,
        Value, %D?selectedValue,
        Remarks %D?remarks_member?contents
    ),
    changeToModel(LastModel, _UnimportantModel).

addConfigurationInstance(Model, ModelFragment, ConfigurationDef, Argument1, Arg1Route, Argument2, Arg2Route, Remarks, ConditionOrConsequence, Editor) :-
    changeToModel(Model, LastModel),
    send(Model, changeRequest, newConfiguration,
        ModelFragment, %Aggregate, %D?modelFragment,
        Editor, %@nil, %D?editor,
        ConditionOrConsequence, %D?state_member?selection,
        ConfigurationDef, %Parent, %D?selectedDef,
        Argument1, Arg1Route, Argument2, Arg2Route, %D?arg1,D?arg1Route,D?arg2,D?arg2Route,
        Remarks %D?remarks_member?contents
    ),
    changeToModel(LastModel, _UnimportantModel).


addValueInstance(Model, ModelFragment, ValueReferenceCopy, Quantity, QuantityRoute, OnDerivative, ConditionOrConsequence, Editor) :-
    changeToModel(Model, LastModel),  
    send(Model, changeRequest, setValue,
	ModelFragment, %Aggregate, %VE?fragment,
	Editor, % VE,
	Quantity, % S?fragmentElement,
	QuantityRoute, %S?route, %route staat ook in de subelementen
	OnDerivative, %D,
	ConditionOrConsequence, % consequence,
	ValueReferenceCopy % S?valueRef?copy
    ),
    changeToModel(LastModel, _UnimportantModel).

addCalculusInstance(Model, ModelFragment, Sign, QuantityOrDerivative, 
    Argument1, Arg1Route, Arg1QSPoint, Argument2, Arg2Route, Arg2QSPoint,
    Remarks, Editor) :-
    changeToModel(Model, LastModel),  
    send(Model, changeRequest, newCalculus,
        ModelFragment, %Aggregate, %D?modelFragment,
        Editor, %@nil, % D?editor,
	Sign, %D?sign_member?selection,
	QuantityOrDerivative, %D?type,
	Argument1, Arg1Route, Arg1QSPoint, %D?arg1,D?arg1Route,D?arg1QSPoint,
	Argument2, Arg2Route, Arg2QSPoint, %D?arg2,D?arg2Route,D?arg2QSPoint,
	Remarks % D?remarks_member?contents
    ),
    changeToModel(LastModel, _UnimportantModel).

addInequalityInstance(Model, ModelFragment, InequalityType, Remarks, ConditionOrConsequence, 
    Argument1, Argument1Route, Argument1Type, Argument1QSPoint,
    Argument2, Argument2Route, Argument2Type, Argument2QSPoint, Editor) :-
    changeToModel(Model, LastModel),
    send(Model, changeRequest, newInequality,
	ModelFragment, %Aggregate, % D?modelFragment,
        Editor, % D?editor,
        ConditionOrConsequence, % D?state_member?selection,
	InequalityType, %D?type_member?selection,
	Argument1, Argument1Route, Argument1Type, Argument1QSPoint, %D?arg1,D?arg1Route,D?arg1Type,D?arg1QSPoint,
	Argument2, Argument2Route, Argument2Type, Argument2QSPoint, %D?arg2,D?arg2Route,D?arg2Type,D?arg2QSPoint,
        Remarks %D?remarks_member?contents).
    ),
    changeToModel(LastModel, _UnimportantModel).

addCorrespondenceInstance(Model, ModelFragment, 
    IsDirectedFlag, OnDerivativeFlag, IsInvertedFlag, IsFullFlag,
    Argument1, Argument1Route, Argument1QSPoint,
    Argument2, Argument2Route, Argument2QSPoint,
    Remarks, Editor) :-
    changeToModel(Model, LastModel), 
    send(Model, changeRequest, newCorrespondence,
	ModelFragment, %Aggregate, % D?modelFragment,
        Editor, %@nil, % D?editor,
        IsDirectedFlag, %?(D?directedMenu_member,selected,directed),
        OnDerivativeFlag, %D?derivative, %2.1: derivative?
        IsInvertedFlag, %?(D?mirrorMenu_member,selected,mirror), %2.1
        IsFullFlag, %D?full, %gp3 0.3
        Argument1, Argument1Route, Argument1QSPoint, %D?arg1,D?arg1Route,D?arg1Value,
        Argument2, Argument2Route, Argument2QSPoint, %D?arg2,D?arg2Route,D?arg2Value,
        Remarks % D?remarks_member?contents
    ), 
    changeToModel(LastModel, _UnimportantModel).

addCausalDependency(Model, ModelFragment, Type, Sign, Argument1, Argument1Route, Argument2, Argument2Route, Remarks, Editor) :-
    changeToModel(Model, LastModel), 
    send(Model, changeRequest, newQuantityRelation,
	ModelFragment, %D?modelFragment,
	Editor, %D?editor,
	Type, %D?quantityRelationType,
	Argument1, Argument1Route, %D?arg1, D?arg1Route,
        Argument2, Argument2Route, %D?arg2, D?arg2Route,
	Sign, % D?sign_member?selection,
	Remarks %D?remarks_member?contents
    ),
    changeToModel(LastModel, _UnimportantModel).



addIdentity(Model, ModelFragment, Argument1, Arg1Route, Argument2, Arg2Route, Remarks, GiveWarning, Editor) :-
    changeToModel(Model, LastModel),
    send(Model, changeRequest, newIdentity,
	ModelFragment, %Aggregate, % VE?fragment,
        Editor, % VE,
        Argument1, %First?fragmentElement,
        Arg1Route, %First?route,
        Argument2, % Sec?fragmentElement,
        Arg2Route, %Sec?route,
        Remarks, %new(string)
	GiveWarning
    ),
    changeToModel(LastModel, _UnimportantModel).

addModelFragmentInstance(Model, ModelFragment, ModelFragmentDef, Remarks, Editor) :-
    changeToModel(Model, LastModel),
    send(Model, changeRequest, newConditionalFragment,
	ModelFragment, %Aggregate, % D?modelFragment,
	Editor, %@nil, %D?editor,
	ModelFragmentDef, %D?selected,
	Remarks %D?remarks_member?contents
    ),
    changeToModel(LastModel, _UnimportantModel).

addRefinementInstance(Model, ModelFragment, ImportedMF, ImportedMFRoute, ModelFragmentDef, RefinementRoute, Remarks, Editor) :-
    changeToModel(Model, LastModel),
    send(Model, changeRequest, newFragmentRefiner,
	ModelFragment, %D?modelFragment,
	Editor, % D?editor,
	ImportedMF, %PossiblyTheImportedModelFragment, %D?refined,
	ImportedMFRoute, %PossiblyTheImportedModelFragmentRoute, %D?refinedRoute,
	ModelFragmentDef, %OriginalAggregate, %D?selectedMF,
	RefinementRoute, %D?selectedRoute,  
	Remarks %D?remarks_member?contents
    ),
    changeToModel(LastModel, _UnimportantModel).

deleteRefinementInstance(Model, ModelFragment, Editor, RefinementElement) :-
    changeToModel(Model, LastModel),
    send(Model, changeRequest, deleteFragmentRefiner,
		ModelFragment, %D?modelFragment,
		Editor, %D?editor,
		RefinementElement % D?element
    ),
    changeToModel(LastModel, _UnimportantModel).

/* Helpers */
printChainNames(Chain) :-
    chain_list(Chain, List),
    forall(member(Member, List),
	(
	    get(Member, name, Name),
	    get(Member, class_name, ClassName),
	    format(' ~w/~w/~w\n', [Member, ClassName, Name])
	)
    ).

printListNames(List) :-
    forall(member(Member, List),
	(
	    get(Member, name, Name),
	    get(Member, class_name, ClassName),
	    format(' ~w/~w/~w\n', [Member, ClassName, Name])
	)
    ).

printChainElements(Chain) :-
    chain_list(Chain, List),
    forall(member(Member, List),
	(
	    (
		send(Member, has_get_method, name) ->
		get(Member, name, Name)
	    ;
		Name = 'no name'
	    ),
	    get(Member, class_name, ClassName),
	    format(' ~w/~w/~w\n', [Member, ClassName, Name])
	)
    ).

/* Multiple language support */
syncLanguages(SourceModel, TargetModel) :-
    % Add all the languages in the source model to the target model (if they do not exist yet)
    get(SourceModel?translator, allLanguages, AllSourceLanguagesChain),
    chain_list(AllSourceLanguagesChain, AllSourceLanguages),

    addLanguages(TargetModel, AllSourceLanguages),

    % Set the current language of the target model to the current language of the source model
    get(SourceModel?translator, currentLanguage, CurrentLanguage),
    get(TargetModel?translator?allLanguages, find, message(@prolog, ==, @arg1?value, CurrentLanguage?value), _TargetCurrentLanguage),
    send(TargetModel?translator, setCurrentLanguage, CurrentLanguage?value).
%%

addLanguages(TargetModel, Languages) :-
    forall(
	member(Language, Languages),
	(
	    (
		% The language is already defined in the target model
		get(TargetModel?translator?allLanguages, find, message(@prolog, ==, @arg1?value, Language?value), _TargetLanguage) ->
		true
	    ;
		% Create the language if it is not already defined
		send(TargetModel?translator, addLanguage, Language?value)
	    )
	)
    ).


%%
/* Copy all the language information from an element in the source model to an element in the target model */
copyLanguagesForElement(SourceElement, TargetElement) :-
    % Get the translatable name and remarks
    (
	send(SourceElement, has_get_method, name_translatable) ->
	get(SourceElement, name_translatable, SourceNameT),
	get(TargetElement, name_translatable, TargetNameT),
	SetNames = @on
    ;
	SetNames = @off
    ),
 
    get(SourceElement, remarks_translatable, SourceRemarksT),
    get(TargetElement, remarks_translatable, TargetRemarksT),
   
    get(SourceRemarksT, translator, SourceT),
    get(TargetRemarksT, translator, TargetT),
  
    % Get all the languages in which the source element is defined
    get(SourceT, allLanguages, AllSourceLanguagesC),
    chain_list(AllSourceLanguagesC, AllSourceLanguages),

    % For each language in which the source elements is defined
    forall(
	member(SourceLanguage, AllSourceLanguages),
	(
	    % Store the current language of the target model
	    get(TargetT, currentLanguage, CurrentLanguage), 
	    
	    % Set the language of the target model (model of TargetElement) [@model is the target model]
	    %@model->>changeRequestEx(setCurrentLanguage,@model,chain(silent,@on),@nil,SourceLanguage?value), %set silent attribute
	    send(TargetT, setCurrentLanguage, SourceLanguage?value),

	    get(TargetT?currentLanguage, value, Current),
	    format('The current language is ~w\n', [Current]),

	    % The type of the element 
	    %ClassName = TargetElement<<-class_name, %sometimes interesting

	    /* TODO: Check if the names already exist, and if so change the name */

	    % Update the name
	    (
		SetNames == @on ->
		get(SourceNameT, valueForLang, SourceLanguage?value, SourceName),
		get(TargetNameT, valueForLang, SourceLanguage?value, TargetName),
		(
		    not(TargetName = SourceName) ->
		    %ignore(pl_changeValue(@nil,ClassName, TargetElement, name, SourceName, @off))
		    send(TargetNameT, value, SourceName)
		;
		    true
		)
	    ;
		true
	    ),

	    % Update the remarks
    	    get(SourceRemarksT, valueForLang, SourceLanguage?value, SourceRemarks),
	    get(TargetRemarksT, valueForLang, SourceLanguage?value, TargetRemarks),
	    (
		not(SourceRemarks = TargetRemarks) ->
		%ignore(pl_changeValue(@nil,ClassName, TargetElement, remarks, SourceRemarks, @off))
		send(TargetRemarksT, value, SourceRemarks)
	    ;
		true
	    ),

	    % Revert the language to the current language
	    %@model->>changeRequestEx(setCurrentLanguage,@model,chain(silent,@on),@nil,CurrentLanguage?value)
    	    send(TargetT, setCurrentLanguage, CurrentLanguage?value)

	)
    ).

copyLanguagesForValues(ValuesC, TargetValuesC) :-
    chain_list(ValuesC, Values),
    forall(
	member(Value, Values),
	(
	    get(Value, valueName_translatable, ValueNameT),
	    get(ValuesC, index, Value, Index),
	    get(TargetValuesC, nth1, Index, TargetValueT),
	    get(TargetValueT, valueName_translatable, TargetValueNameT),

	    % Get the translator of the target values
	    get(TargetValueNameT, translator, TargetTranslator),

	    % get all the languages
	    get(ValueNameT, translator, Translator),
	    get(Translator, allLanguages, AllLanguagesC),
	    chain_list(AllLanguagesC, AllLanguages),
	    
	    % For each language set the name of the TargetValue
	    forall(
		member(Language, AllLanguages),
		(
		    % Set the current language of the target translator to the to-be-copied language
		    get(Translator, currentLanguage, CurrentLanguage),
		    send(TargetTranslator, setCurrentLanguage, Language?value),

		    % Set the name of the target value
		    get(ValueNameT, valueForLang, Language?value, SourceName),
		    send(TargetValueNameT, value, SourceName),

		    % Set the language back to the previous language
		    send(TargetTranslator, setCurrentLanguage, CurrentLanguage?value)
		)
	    )
	)
    ).



