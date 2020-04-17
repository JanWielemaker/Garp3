:-pce_begin_class(
                  importExport,
                  object,
                  "Superclass of the import and export functions (shared functions)."
                 ).
variable(currentURI, prolog, both, "The URI of the OWL ontology").
variable(refinements, hash_table:=new(hash_table), both, "A hash_table with the importedFragment and the applying refinement").
variable(gensymIdentifierChain, chain:=new(chain), both, "Storage for the used IURI's.").
variable(identifierHash, hash_table:=new(hash_table), both, "Storage for the Element/Route info and the corresponding URI").
variable(calculiToDoChain, chain:=new(chain), both, "The chain of element which still have to be exported").
variable(garp3_owl_version_number, number:=1, get).
variable(modelFragmentsDone, chain:=new(chain), both, 'The model fragments that have already been exported/imported').

initialise(_IE) :->
    %owl_register_ns(qrm, 'http://www.science.uva.nl/~jliem/ontologies/model.owl#'),
    owl_register_ns(qr,  'http://www.science.uva.nl/~jliem/ontologies/QRvocabulary.owl#'),
    %owl_register_ns(dcns,'http://purl.org/dc/elements/1.1/#'),
    owl_register_ns(dcens, 'http://www.science.uva.nl/~jliem/ontologies/dc-extension.rdf#'),
    owl_register_ns(dctermsns, 'http://purl.org/dc/terms/').

cleanGensym(IE) :->
        get(IE?gensymIdentifierChain, size,ChainLength),

        ( ChainLength >= 1 ->
                numlist(1,ChainLength,IteratorList),
                forall(
                        member(Iterator, IteratorList),
                        (
                        get(IE?gensymIdentifierChain, nth1, Iterator, Identifier),
                        reset_gensym(Identifier)
                        )
                )
        ;
                true
        ).

        
createUniqueIDURI(IE, ElementName, ElementIDURI) :<-
        send(IE?gensymIdentifierChain, add, ElementName),
        gensym(ElementName, ElementID),
        get(IE, add_correct_namespace, ElementID, ElementIDURI).       

/*
findElementInAggregate(IE, Element, AggregateWithElement, ElementURI) :<-
        % Get the URI of the Aggregate
        get(IE?identifierHash, member, AggregateWithElement, AggregateWithElementURI),
                
        % Get the name of the Element
        get(Element, name, ElementName),
        
        % Get the URIs of the hasCondition and hasConsequence relations
        get(IE, add_correct_namespace, 'hasCondition', HasConditionURI),
        get(IE, add_correct_namespace, 'hasConsequence' ,HasConsequenceURI),

        (
                rdf_has(AggregateWithElementURI, HasConditionURI, ElementURI),
		get(IE, get_current_name, ElementURI, ElementName),
		%rdf_has(ElementURI, rdfs:label, literal(ElementName))
        ;
                rdf_has(AggregateWithElementURI, HasConsequenceURI, ElementURI),
		get(IE, get_current_name, ElementURI, ElementName),
		%rdf_has(ElementURI, rdfs:label, literal(ElementName))
        ).
*/

add_correct_namespace(_IE, ElementName, ElementURI) :<-
    % Add the right namespace
    QRElements =  
	['Entity', 
	 'Agent',
	 'Assumption',
	 'Configuration',
	 'Attribute',
	 'hasAttribute',
	 'AttributeValue',
	 'hasAttributeValue',
	 'Quantity',
	 'QuantitySpace',
	 'Point',
	 'Interval',
	 'containsQualitativeValue',
	 'hasInequality',
	 'hasInequalityTarget',
	 'Inequality',
	 'GreaterThan',
	 'GreaterOrEqualTo',
	 'EqualTo',
	 'SmallerOrEqualTo',
	 'SmallerThan',
	 'hasQuantitySpace',
	 'hasMagnitude',
	 'Magnitude',
	 'hasDerivative',
	 'Derivative',
	 'Scenario',
	 'ModelFragment',
	 'hasFact',
	 'hasCondition',
	 'hasConsequence',
	 'hasConfiguration',
	 'hasConfigurationTarget',
	 'hasQuantity',
	 'StaticFragment',
	 'ProcessFragment',
	 'AgentFragment',
	 'PositiveProportionality',
	 'NegativeProportionality',
	 'PositiveInfluence',
	 'NegativeInfluence',
	 'hasCausalDependency',
	 'hasCausalDependencyTarget',
	 'QuantitySpaceCorrespondence',
	 'ValueCorrespondence',
	 'hasCorrespondence',
	 'hasCorrespondenceTarget',
	 'hasLeftHandSide',
	 'hasRightHandSide',
	 'Operator',
	 'Plus',
	 'Minus',
	 'hasValue',
	 'hasValueTarget',
	 'Identity',
	 'hasIdentity',
	 'hasIdentityTarget',
	 'has_xposition_on_screen',
	 'has_yposition_on_screen',
	 'is_hidden_on_screen',
	 'hasEditSizeHeight',
	 'hasEditSizeWidth',
	 'hasDisplayOriginX',
	 'hasDisplayOriginY',
	 'isActive',
	 'isDirected',
	 'isInverted',
	 'isFull',
	 'isRefinedFrom',
	 'hasQuantityAssumption',
	 'ExogenousNone',
	 'ExogenousDecrease',
	 'ExogenousSteady',
	 'ExogenousIncrease',
	 'ExogenousSinusoidal',
	 'ExogenousPositiveParabola', % new FL june 07
	 'ExogenousNegativeParabola', % new FL june 07
	 'ExogenousRandom',
	 'GenerateAllValues',
	 'Constant',
	 'isAssumptionRegarding',
	 'hasSimulationPrefcw_assumption',
	 'hasSimulationPreffree_maxmin_derivative',
	 'hasSimulationPreffree_zero_derivative',
	 'hasSimulationPrefequal_qspace_points',
	 'hasSimulationPrefvalue_branching',
	 'hasSimulationPreffast_path',
	 'hasSimulationPreforder_using_correspondences',
	 'hasSimulationPrefepsilon_ordering',
	 'hasSimulationPrefepsilon_merging',
	 'hasSimulationPrefepsilon_derivative_constraints',
	 'hasSimulationPrefremove_inactive_quantities',
	 'hasSimulationPrefreasoning_assumptions',
	 'hasSimulationPrefassume_conditional_derivatives',
	 'hasSimulationPrefsecond_order_derivatives',
	 'hasSimulationPrefderivative_terminations',
	 'hasSimulationPrefsecond_order_continuity',
	 'hasSimulationPrefapply_continuity_d_inequalities',
	 'hasSimulationPreforder_using_equalities',
	 'hasSimulationPrefequal_intervals',
	 'hasSimulationPrefsolve_extra_combinations',
	 'hasSimulationPreffull_branching_derivative_terminations',
	 'hasSimulationPreffull_branching_equality_terminations',
	 'hasSimulationPreffull_branching_value_terminations',
	 'hasSimulationPrefterminate_weak_relations',
	 'hasSimulationPrefextra_termination_interpreter',
	 'hasSimulationPreforder_epsilon_last',
	 'hasSimulationPrefremove_corresponding_equality',
	 'hasSimulationPrefuse_landmarks',
	 'hasSimulationPrefmax_depth'
	],
    DCElements = 
       [
	'title',
	'creator',
	'contributor',
	'subject',
	'language',
	'rights',
	'description',
	'type'
       ],
    DCQElements = 
	[
	'audience',
	'bibliographicCitation',
	'created',
	'format',
	'modified'
	],
    DCEElements = 
	[
	'domain',
	'keyword',
	'goal',
	'limitation',
	'email',
	'hasVersionNumber',
	'hasOWLVersionNumber'
	],
    (
	member(ElementName, QRElements) ->
	rdf_db:ns(qr, QRURI),
	owl_create_uri(QRURI, ElementName, ElementURI)
    ;
	member(ElementName, DCElements) ->
	rdf_db:ns(dc, DCURI),
	owl_create_uri(DCURI, ElementName, ElementURI)
    ;
	member(ElementName, DCQElements) ->
	rdf_db:ns(dctermsns, DCQURI),
	owl_create_uri(DCQURI, ElementName, ElementURI)
    ;
	member(ElementName, DCEElements) ->
	rdf_db:ns(dcens, DCEURI),
	owl_create_uri(DCEURI, ElementName, ElementURI)
    ;
	rdf_db:ns(qrm, QRMURI),
	owl_create_uri(QRMURI, ElementName, ElementURI)
    ).

translateQuantityAssumption(ME, QuantityAssumption, QuantityAssumptionURI) :-
    QuantityAssumptionMapping = 
	[generate_all_values-'GenerateAllValues',
	 constant-'Constant',
	 exogenous_none-'ExogenousNone',
	 exogenous_decreasing-'ExogenousDecrease',
	 exogenous_steady-'ExogenousSteady',
	 exogenous_increasing-'ExogenousIncrease', 
	 exogenous_sinus-'ExogenousSinusoidal',
	 exogenous_neg_parabola-'ExogenousNegativeParabola', % new FL june 07
	 exogenous_pos_parabola-'ExogenousPositiveParabola', % new FL june 07
	 exogenous_free-'ExogenousRandom'
	],

    (
		var(QuantityAssumptionURI) ->
		member(QuantityAssumption-QuantityAssumptionName, QuantityAssumptionMapping),
		get(ME, add_correct_namespace, QuantityAssumptionName, QuantityAssumptionURI)
    ;
		rdf_split_url(_Base, QuantityAssumptionName, QuantityAssumptionURI),
		member(QuantityAssumption-QuantityAssumptionName, QuantityAssumptionMapping)
    ).


findMainMF(IE, ElementURI, MainMFURI) :<-
    get(IE, add_correct_namespace, 'hasCondition', HasConditionURI),
    get(IE, add_correct_namespace, 'hasConsequence', HasConsequenceURI),
    (
        (
	    owl_has(ContainingMFURI, HasConditionURI, ElementURI)
        ;
	    owl_has(ContainingMFURI, HasConsequenceURI, ElementURI)
	) ->
	get(IE, findMainMF, ContainingMFURI, MainMFURI)
    ;
	MainMFURI = ElementURI
    ).


/* An alternative findRoute function tailored specifically for the model import. This was necessary as the
 * IdentifierHash in the import function is used differently from the export functionality */
findMainMFAndRoute2(IE, ElementURI, MainMF, RouteChain) :-
        get(IE, add_correct_namespace, 'hasCondition', HasConditionURI),
        get(IE, add_correct_namespace, 'hasConsequence', HasConsequenceURI),
        (
                (
                owl_has(ContainingMFURI, HasConditionURI, ElementURI)
                ;
                owl_has(ContainingMFURI, HasConsequenceURI, ElementURI)
                ) 
        ->
                findRoute2(IE, ContainingMFURI, RouteAndMainMFListRev),
        	reverse(RouteAndMainMFListRev, RouteAndMainMFList),
                nth1(1, RouteAndMainMFList, MainMF),
                delete(RouteAndMainMFList, MainMF, Route),
        	%reverse(RouteRev, Route), % REMOVED REVERSE
                chain_list(RouteChain, Route)
        ;
                get(IE, findElementDefInIdentifierHash, ElementURI, MainMF),
                chain_list(RouteChain, [])
        ).
        

findRoute2(IE, MFURI, MFDefList) :-
        get(IE, add_correct_namespace, 'hasCondition', HasConditionURI),
	get(IE, add_correct_namespace, 'isRefinedFrom', IsRefinedFromURI),
        (
	    /* The element is refined */
	    owl_has(MFURI, IsRefinedFromURI, _OriginalMFDef),
	    get(IE, findMainMF, MFURI, MainMFURI),
	    findInstanceInIdentifierHash(IE, MFURI, MFDef, _Route),
            get(IE, findElementDefInIdentifierHash, MainMFURI, MainMFDef),
	    MFDefList = [MFDef, MainMFDef]   
	;
            /* The element is the Defining MF itself */
            not(owl_has(_ContainingMFURI, HasConditionURI, MFURI)),
            get(IE, findElementDefInIdentifierHash, MFURI, MFDef),
	    MFDefList = [MFDef]
            %format('De URI van het model fragment is: ~w met objectID ~w\n', [MFURI, MFDef])
        ).

findRoute2(IE, MFURI, [MF|Route]) :-
        findInstanceInIdentifierHash(IE, MFURI, MF, _Route),
        get(IE, add_correct_namespace, 'hasCondition', HasConditionURI),
        owl_has(ContainingMFURI, HasConditionURI, MFURI),
        findRoute2(IE, ContainingMFURI, Route).


/* Find the route to the element if you know the Element and the Main Aggregate which contains it */
findRouteInGarp(Element, Aggregate, Route, MainMF) :-
        findRouteInGarp2(Element, Aggregate, RouteAndMainMF),
        last(RouteAndMainMF, MainMF),
        delete(RouteAndMainMF, MainMF, RouteList),
        %reverse(RouteListRev, RouteList),
        chain_list(Route, RouteList).
        %format('De route is: '),
        %forall(member(RouteE, RouteList), (RouteEName = RouteE?referencedFragment<<-name, format(' ~w', [RouteEName]))),
        %format('\n').
        
        
findRouteInGarp2(Element, Aggregate, [Aggregate]) :-
                %format('Element: ~w\n', [Element]),
                (
                        get(Aggregate?elements, find, @arg1 == Element, Element)
                        ;
                        get(Aggregate?parentReferences, find, @arg1 == Element, Element)
                ).
        
findRouteInGarp2(Element, Aggregate, [ImportedFragment|Route]) :-
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
                findRouteInGarp2(Element, ImportedFragmentDef, Route). 

/* Find the chain which uniquely defines an element */
findInstanceInIdentifierHash(IE, ElementURI, Element, Route) :-
        get(IE?identifierHash, find_key, @arg2 == ElementURI, ElementAndRouteChain),
        get(ElementAndRouteChain, head, Element),
        get(ElementAndRouteChain, copy, Route),
        send(Route, subtract, [Element]).

findElementDefInIdentifierHash(IE, ElementURI, Element) :<-
        get(IE?identifierHash, find_key, @arg2 == ElementURI, Element).

/* find the URI belonging to the chain which uniquely defines an instance */
findInstanceURIInIdentifierHash(IE, Element, Route, ElementURI) :<-
        get(Route, copy, RouteCopy),
        /* DEBUG  
	chain_list(RouteCopy, RouteList),
	forall(member(RElement, RouteList), (X = RElement<<-class_name, format('Searching ~w/~w\n', [RElement, X]))),
        END DEBUG */
        send(RouteCopy, prepend, Element),
	%send(Route, append, MainMF),
        get(IE?identifierHash, find_value, message(RouteCopy, equal, @arg1), ElementURI).
        %format('Found uri: ~w\n', [ElementURI]).

findElementDefURIInIdentifierHash(IE, Element, ElementURI) :<-
        get(IE?identifierHash, member, Element, ElementURI).
        

/* Store the chain which uniquely defines the element and the URI which which defines it in the OWL file. The chain
 * contains the element itself, the importedFragments, and finally the model fragment in which it is defined. */
 /*
storeInstanceInIdentifierHash(IE, Element, ElementURI) :->
         DEBUG 
        %chain_list(RouteChain, RouteList),
        %format('Stored ~w, with ~w\n', [ElementURI, RouteList]).
         END DEBUG 
*/

/* Alternative version of the storeInstanceInIdentifierHash function for model export */
storeInstanceInIdentifierHash(IE, Element, ElementURI, AggregateNameURI:[any]) :->
        default(AggregateNameURI, '', AggregateNameURIValue),
	% DEBUG ME!!!
	/* Route Exceptions for refinements */
	(
    	    /* If the Element is contained by a refinement that is directly added to the MainMF */
	    not(AggregateNameURIValue == ''),
	    not(send(IE, isModelFragmentDefinition, AggregateNameURI)), 
	    findInstanceInIdentifierHash(IE, AggregateNameURI, Fragment, _FragmentRoute),
	    get(IE?refinements, member, Fragment, Refinement),
	    send(IE, isDirectElementOfMainMF, Refinement, AggregateNameURI) ->
	    /* The route contains only the refinement */
	    %chain_list(RouteChain, [Element, Fragment, Refinement]),
	    chain_list(RouteChain, [Element, Refinement]),
	    send(IE?identifierHash, append, RouteChain, ElementURI)
	    /* Debug */
	    ,printInstanceIdentifier(1, ElementURI, RouteChain)
	    /* No debug */
	;
	    /* If the Element is a refinement that is directly added to the MainMF */
	    get(Element, class_name, 'fragmentRefiner'),
	    send(IE, isDirectElementOfMainMF, Element, AggregateNameURIValue) ->
	    /* The route is empty */
	    chain_list(RouteChain, [Element]),
	    send(IE?identifierHash, append, RouteChain, ElementURI)
	    /* Debug */
	    ,printInstanceIdentifier(2, ElementURI, RouteChain)
	    /* No debug */
	;
	    /* Do the normal storage of the paths */
	    send(IE, storeInstanceInIdentifierHash2, Element, ElementURI, AggregateNameURIValue)
	).

isDirectElementOfMainMF(ME, Element, AggregateURI) :->
    get(ME, findMainMF, AggregateURI, MainMFURI),
    get(ME, findElementDefInIdentifierHash, MainMFURI, MainMF),
    send(MainMF?elements, member, Element).

printInstanceIdentifier(Integer, ElementURI, Route) :-
    (
	get(Route, nth1, 1, Element),
	get(Element, class_name, ClassName), 
	member(ClassName, ['fragmentRefiner', 'importedFragment', 'garpInstance', 'garpQuantity', 'configuration']) ->
	get(Element, name, Name),
	chain_list(Route, RouteList),
	debug(owl(identifier), '~w. Stored ~w/~w with identifier: ', [Integer, ElementURI, Name]),
	forall(
	    member(RouteElement, RouteList),
	    (
		get(RouteElement, name, RouteName),
		get(RouteElement, class_name, RouteClassName),
		debug(owl(identifier), '~w/~w/~w ', [RouteElement,RouteClassName,RouteName])
	    )
	),
	debug(owl(identifier), '\n', [])
    ;   
	true 
    ).


storeInstanceInIdentifierHash2(IE, Element, ElementURI, AggregateNameURI:[any]) :->
        default(AggregateNameURI, '', AggregateNameURIValue),
        (
                AggregateNameURIValue == '' ->
                %format('storeInstance/2 called\n'),
                findMainMFAndRoute2(IE, ElementURI, _MainMF, RouteChain),
                send(RouteChain, prepend, Element),
                
		send(IE?identifierHash, append, RouteChain, ElementURI)
		/* Debug */
		,printInstanceIdentifier(3, ElementURI, RouteChain)
		/* No debug */
        ;
		%format('Searching for ~w\n', [AggregateNameURI]),
                (
                        send(IE, isModelFragmentDefinition, AggregateNameURI) ->
                        chain_list(RouteChain, [])
                ;
                        findInstanceInIdentifierHash(IE, AggregateNameURI, Aggregate, RouteChain),
                        send(RouteChain, append, Aggregate) % Debugged prepend=>append  
                        %findRouteInGarp(, MainMF, GarpInstanceRoute, _MainMFQuestionMark),

                ),
                send(RouteChain, prepend, Element),
                send(IE?identifierHash, append, RouteChain, ElementURI)
		/* Debug */
		,printInstanceIdentifier(4, ElementURI, RouteChain)
		/* No debug */
        ).

isModelFragmentDefinition(_IE, AggregateNameURI) :->
        owl_has(AggregateNameURI, rdf:type, owl:'Class').
        

/* The definitions do not have a route or main model fragment, so they are stored as a couple Element=>URI */
storeElementDefInIdentifierHash(IE, Element, ElementURI) :->
        send(IE?identifierHash, append, Element, ElementURI).

get_element_by_name(_IE, Chain, Name, Element) :<-
    Element = Chain<<-find(@arg1?name == Name).

% TODO: There might be a bug in this function, it does not distinguish between derivative or magnitudes maybe it should
% be an argument like MagOrDer. 
findQuantitySpaceIDFromQuantityID(IE, QuantityURI, MagOrDer, QuantitySpaceURI) :<-
        HasDerivativeURI    = IE<<-add_correct_namespace('hasDerivative'),
	HasMagnitudeURI     = IE<<-add_correct_namespace('hasMagnitude'),
        HasQuantitySpaceURI = IE<<-add_correct_namespace('hasQuantitySpace'),

        % Zoek de valueURI en zet hem in Argument1URI
        (
                MagOrDer == 'magnitude' ->
                owl_has(QuantityURI, HasMagnitudeURI, MagOrDerURI) 
        ;       
                MagOrDer == 'derivative' ->
                owl_has(QuantityURI, HasDerivativeURI, MagOrDerURI)
        ), 
        owl_has(MagOrDerURI, HasQuantitySpaceURI, QuantitySpaceURI).

isDerivativeElement(IE, ElementURI) :->
        DerivativeURI       = IE<<-add_correct_namespace('Derivative'),
        QuantitySpaceURI    = IE<<-add_correct_namespace('QuantitySpace'),
        HasQuantitySpaceURI = IE<<-add_correct_namespace('hasQuantitySpace'),
        QualitativeValueURI = IE<<-add_correct_namespace('QualitativeValue'),
        HasQValueURI        = IE<<-add_correct_namespace('hasQualitativeValue'),

	(
	    /* A derivative itself */
	    owl_has(ElementURI, rdf:type, DerivativeURI)
	;
	    /* Quantity Space belonging to a derivative */
	    owl:j_instance_of(ElementURI, QuantitySpaceURI) ->
	    owl_has(DerivativeElementURI, HasQuantitySpaceURI, ElementURI),
	    owl_has(DerivativeElementURI, rdf:type, DerivativeURI)
	;   
	    /* A value belonging to a quantity of a derivative */
	    owl:j_instance_of(ElementURI, QualitativeValueURI) ->
	    owl_has(QuantitySpaceElementURI, HasQValueURI, ElementURI),
	    owl_has(DerivativeElementURI, HasQuantitySpaceURI, QuantitySpaceElementURI),
	    owl_has(DerivativeElementURI, rdf:type, DerivativeURI)
	).

addRefinementsToRoute(ME, ArgumentRoute) :->
    % If an imported fragment or refinment in the route is refined add the refinement
    % ACTUALLY REPLACE IT BY THE REFINEMENT!
    
    chain_list(ArgumentRoute, ArgumentRouteList),
    forall(
	member(Element, ArgumentRouteList),
	(
	    get(ME?refinements, member, Element, Refinement),
	    %send(ArgumentRoute, replace, Element, Refinement)
	    send(ArgumentRoute, insert_before, Refinement, Element),
	    get(Refinement, fragment, RefinementFragment),
	    findRouteInGarp(Element, RefinementFragment, Route, _RefinementMainMF),
	    %send(ArgumentRoute, delete, Element)
	    send(ArgumentRoute, subtract, Route),
	    
	    get(Element, name, ElementName), 
	    findImportedFragmentInAggregate(ElementName, RefinementFragment, NewIMF, NewIMFRoute),
	    get(ArgumentRoute, index, Refinement, RefinementIndex),
	    get(ArgumentRoute, sub, RefinementIndex, ArgumentRoute?size, ReplaceMe),
	    send(ArgumentRoute, subtract, ReplaceMe),
	    send(ArgumentRoute, merge, NewIMFRoute),
	    send(ArgumentRoute, append, NewIMF)
	;
	    true
	)
    ).

%getOntologyURI(_ME, OntologyURI) :<-
%    owl_has(OntologyURI, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#Ontology'),
%    not(sub_string(OntologyURI, _, _, _, 'QRvocabulary.owl')).

:-pce_end_class.

getSimPrefs(SimPrefs) :- 
    findall(SimPrefName,
	(
	    owl_has(SimulationPrefURI, rdf:'type', owl:'DatatypeProperty'),
	    sub_string(SimulationPrefURI, Position, _, _, '#'),
	    Rest is Position + 1,
	    sub_string(SimulationPrefURI, Rest, _, 0, SimPref),
	    string_concat('hasSimulationPref', SimPrefNameString, SimPref),
	    string_to_atom(SimPrefNameString, SimPrefName)
	),
	SimPrefs
    ).

