:- consult('owl.pl').
:- consult('exportimport_helpers.pl').

:-pce_begin_class(
                  modelImporter,
                  importExport,
                  "Parser which imports a model from an OWL file."
                 ).

variable(impexp_helpers, importexport_helpers, both, 'The helper function for the import and export.').
variable(vocabularyOntology, string, both, 'The URL of the vocabulary ontology').
variable(modelFragmentsToDo, chain:=new(chain), both, 'The Model Fragments which still have to be imported.').
variable(doneRefinements, chain:=new(chain), both, 'The refinements which have already been performed').
variable(currentLanguage, string, both, 'The current language of the model being imported').

initialise(MI) :->
        send_super(MI, initialise),
        MI->>slot(vocabularyOntology, 'http://www.science.uva.nl/~jliem/ontologies/QRvocabulary.owl#'),
        format('Importing Time\n').

import(MI, FileToImport: name) :->
    %free(@model),
    %format('hallo ik ga ~w importeren\n', [FileToImport]),
    /* Create an OWL pleaseWait screen */
    (
	@app<<-hypered(pleaseWait) ; debugging(owl(general)) ->
	true
    ;
	W *= pleaseWait('Please wait,\nImporting OWL file...'),  %see below in this file
	@app->>hyper(W,pleaseWait)
    ),

    (
	rdf_load(ontologies('QRvocabulary.owl')),
	debug(owl(general), 'Loading the model OWL file.', []),
	rdf_load(FileToImport),
	rdf_has(OntologyURI, rdf:'type', owl:'Ontology'),
	sub_string(OntologyURI, _, _, _, 'dynalearn'),
	send(MI, slot, currentURI, OntologyURI),

	send(MI, importLanguages),
	debug(owl(general), 'Importing ENTITIES', []),
	send(MI, importHierarchyDef),
	debug(owl(general), 'Importing CONFIGURATIONS', []),
	send(MI, importConfigurationsDef),
	debug(owl(general), 'Importing ATTRIBUTES', []),
	send(MI, importAttributeDef),
	debug(owl(general), 'Importing QUANTITY SPACES', []),
	send(MI, importQuantitySpaceDef),
	debug(owl(general), 'Importing QUANTITIES', []),
	send(MI, importQuantityDef),
	debug(owl(general), '----------------- Importing SCENARIOS ------------', []),
	send(MI, importScenarioDef),
	debug(owl(general), '----------------- Importing MODEL FRAGMENTS ------------', []),
	send(MI, importModelFragmentDefs),
	debug(owl(general), 'Importing MetaData', []),
	send(MI, importMetaData),
	debug(owl(general), 'Importing Simulation Preferences', []),
	send(MI, importSimPrefs),
	debug(owl(general), 'Importing Unique IDs', []),
	send(MI, importUniqueIDs),

	debug(owl(general), 'Done importing ~w', [FileToImport]),
	free(MI?identifierHash),
	rdf_reset_db,
	send(MI?refinements, clear),
	send(MI?modelFragmentsDone, clear),
	send(@app, thankYou)
    ;
	@app?mainMenu->>msgBox(string('OWL Import failed! Please send this error and your .owl file the developers.'),alarm), !,
	send(@app, thankYou),
	send(MI?refinements, clear),
	send(MI?modelFragmentsDone, clear),
	rdf_reset_db
    ).

importLanguages(MI) :->
    get(MI, add_correct_namespace, 'Entity', EntityURI),
    get(MI, add_correct_namespace, 'Quantity', QuantityURI),

    get(MI, add_correct_namespace, 'language', LanguageURI),
    get(MI, currentURI, OntologyURI),
    rdf_has(OntologyURI, LanguageURI, literal(CurrentLanguageKey)),
    %owl:j_instance_of(AnEntityURI, EntityURI),
    (
	rdf_has(ADefinitionURI, rdfs:subClassOf, EntityURI) ;
	rdf_has(ADefinitionURI, rdfs:subClassOf, QuantityURI)
    ),
    get(@model?translator, getLanguageFromKey, CurrentLanguageKey, CurrentLanguage),

    findall(LanguageString,
	(
	    rdf_has(ADefinitionURI, rdfs:'label', literal(lang(LanguageKey, _Label))),
	    get(@model?translator, getLanguageFromKey, LanguageKey, Language),
	    new(LanguageString, string(Language))
	),
	Languages),

    % Add all the languages to the model
    addLanguages(@model, Languages),
    % Set the correct current language
    send(MI, slot, currentLanguage, CurrentLanguage?value),
    send(@model?translator, setCurrentLanguage, CurrentLanguage?value).


importMetaData(MI) :->
    get(MI, add_correct_namespace, 'title', TitleURI),
    get(MI, add_correct_namespace, 'creator', CreatorURI),
    get(MI, add_correct_namespace, 'contributor', ContributorURI),
    %get(MI, add_correct_namespace, 'subject', SubjectURI),
    get(MI, add_correct_namespace, 'rights', RightsURI),
    get(MI, add_correct_namespace, 'description', DescriptionURI),
    %get(MI, add_correct_namespace, 'type', TypeURI),
    get(MI, add_correct_namespace, 'bibliographicCitation', BibliographicCitationURI), 
    get(MI, add_correct_namespace, 'audience', AudienceURI),
    get(MI, add_correct_namespace, 'created', CreatedOnURI),
    get(MI, add_correct_namespace, 'hasVersionNumber', HasVersionNumberURI),
    get(MI, add_correct_namespace, 'domain', DomainURI),
    get(MI, add_correct_namespace, 'keyword', KeywordURI),
    get(MI, add_correct_namespace, 'goal', GoalURI),
    get(MI, add_correct_namespace, 'limitation', LimitationURI),
    get(MI, add_correct_namespace, 'email', EmailURI),
    get(MI, add_correct_namespace, 'modified', ModifiedURI),
    get(MI, add_correct_namespace, 'format', FormatURI),  

    get(@model, metaData, MetaDataHash),
    get(MetaDataHash, member, remarks, RemarksD),
    get(MetaDataHash, member, modelData, ModelDataD),
    %get(MetaDataHash, member, status, StatusD),
    %get(MetaDataHash, member, misc, MiscD),
    get(MetaDataHash, member, general, GeneralD),

    get(MI, currentURI, Ontology),

    rdf_has(Ontology, TitleURI, literal(Title)),
    rdf_has(Ontology, CreatorURI, literal(Creator)),
    rdf_has(Ontology, ContributorURI, literal(Contributor)),
    rdf_has(Ontology, RightsURI, literal(Rights)),
    rdf_has(Ontology, DescriptionURI, literal(Description)),
    %rdf_has(Ontology, TypeURI, literal(Type)),
    rdf_has(Ontology, BibliographicCitationURI, literal(BibliographicCitation)),
    rdf_has(Ontology, AudienceURI, literal(Audience)),
    rdf_has(Ontology, CreatedOnURI, literal(CreatedOn)),
    rdf_has(Ontology, HasVersionNumberURI, literal(HasVersionNumber)),
    rdf_has(Ontology, DomainURI, literal(Domain)),
    rdf_has(Ontology, KeywordURI, literal(Keyword)),
    rdf_has(Ontology, GoalURI, literal(Goal)),
    rdf_has(Ontology, LimitationURI, literal(Limitation)),
    rdf_has(Ontology, EmailURI, literal(Email)),
    rdf_has(Ontology, ModifiedURI, literal(Modified)),
    rdf_has(Ontology, FormatURI, literal(Format)),
    rdf_has(Ontology, rdfs:comment, literal(Remarks)),

    send(GeneralD, append, title, Title),
    send(GeneralD, append, author, Creator),
    send(GeneralD, append, contributors, Contributor),
    send(GeneralD, append, contact_email, Email),
    send(GeneralD, append, keywords, Keyword),
    send(GeneralD, append, domain, Domain),
    send(GeneralD, append, model_version, HasVersionNumber),
    send(GeneralD, append, known_model_limitations, Limitation),
    send(GeneralD, append, bibliographic_citation, BibliographicCitation),
    send(GeneralD, append, license, Rights),
    new(CreatedOnDate, date), send(CreatedOnDate, convert, CreatedOn),
    send(ModelDataD, append, creationTime, CreatedOnDate),
    %send(ModelDataD, append, last_change, Modified),
    send(@model?lastChangeTime, convert, Modified),
    send(ModelDataD, append, created_in, Format),
    send(RemarksD, append, general_remarks, Remarks),
    send(RemarksD, append, abstract, Description),
    send(RemarksD, append, model_goals, Goal),
    send(RemarksD, append, intended_audience, Audience).

importSimPrefs(MI) :->
    get(MI, currentURI, Ontology),
    get(@model, runPrefs, RunPrefs),
    design:getSimPrefs(SimPrefs),
    forall(
	member(SimPref, SimPrefs),
	(
	    atom_concat('hasSimulationPref', SimPref, SimPrefFull),
	    get(MI, add_correct_namespace, SimPrefFull, SimPrefURI),
	    (
		% If the ontology has simulation preferences defined
		owl_has(Ontology, SimPrefURI, literal(SimPrefBooleanString)) ->
		string_to_atom(SimPrefBooleanString, SimPrefBoolean),
		(
		    member(SimPrefBoolean, [true, false]) ->
		    boolean_to_flag(SimPrefBoolean, SimPrefFlag)
		;
		    SimPrefFlag = SimPrefBoolean
		),
		send(RunPrefs, append, SimPref, SimPrefFlag)
	    ;
		true
	    )
	)
    ).

importUniqueIDs(MI) :->
    get(MI, currentURI, OntologyURI),
    forall(
	rdf_has(OntologyURI, owl:priorVersion, PriorVersionURI),
	(
	    atom_concat('http://www.dynalearn.eu/models/', PartURI, PriorVersionURI),
	    atom_concat(PartURI, '.owl', PriorVersionEncoded),
	    www_form_encode(PriorVersion, PriorVersionEncoded),
	    send(@model?uniqueIDs, append, PriorVersion)
	)
    ),
    atom_concat('http://www.dynalearn.eu/models/', PartURI, OntologyURI),
    atom_concat(PartURI, '.owl', PriorVersionEncoded),
    www_form_encode(PriorVersion, PriorVersionEncoded),
    send(@model?uniqueIDs, append, OntologyURI).

importHierarchyDef(MI) :->
        /* Import the Entity Hierarchy */
        get(MI, add_correct_namespace, 'Entity', EntityURI),
	%Entity = MI<<-get_element_by_name(@model?abstractEntities, 'Entity'),
	getTopEntity(@model, Entity),
        send(MI, importHierarchy2Def, Entity, EntityURI),
        send(MI, storeElementDefInIdentifierHash, Entity, EntityURI), 

        /* Import the Agent Hierarchy */
        get(MI, add_correct_namespace, 'Agent', AgentURI),
	%Agent = MI<<-get_element_by_name(@model?abstractEntities, 'Agent'),
	getTopAgent(@model, Agent),
        send(MI, importHierarchy2Def, Agent, AgentURI),
        send(MI, storeElementDefInIdentifierHash, Agent, AgentURI), 

        /* Import the Assumption Hierarchy */
        get(MI, add_correct_namespace, 'Assumption', AssumptionURI),
	%Assumption = MI<<-get_element_by_name(@model?assumptions, 'Assumption'),
	getTopAssumption(@model, Assumption),
        send(MI, importHierarchy2Def, Assumption, AssumptionURI),
        send(MI, storeElementDefInIdentifierHash, Assumption, AssumptionURI).
        
importHierarchy2Def(MI, Parent, ParentURI) :->
	
        forall(
                /* for each child */
                owl_has(ChildURI, rdfs:subClassOf, ParentURI),
                (
                        /* Get the Label and Comment */
			get(MI, get_current_name, ChildURI, Label),
			get(MI, get_current_comment, ChildURI, Comment),

                        /* and add it to the hierarchy */
			get(Parent, name, ParentName),
			addEntityDefinition(@model, Label, ParentName, Comment, @nil),

			%send(@model, changeRequest, newHObjectChild,
			%                            Parent, %Parent,
			%                           @nil,  %D?editor,
			%                           Label, %D?name_member?selection,
			%                           Comment), %D?remarks_member?contents).
			
                        /* then find the just added object and export the children */
                        (
                                Child = MI<<-get_element_by_name(@model?abstractEntities, Label)
                        ;
                                Child = MI<<-get_element_by_name(@model?assumptions, Label)
                        ),
                        send(MI, storeElementDefInIdentifierHash, Child, ChildURI), 

			% Add the translations (multiple model support)
			send(MI, import_name_languages, ChildURI, Child),
			send(MI, import_remarks_languages, ChildURI, Child),

			% Import the rest of the hierarchy
                        send(MI, importHierarchy2Def, Child, ChildURI)
                )
              ).

importConfigurationsDef(MI) :->
        get(MI, add_correct_namespace, 'Configuration', ConfigurationURI),

        forall(
                /* for each child of configuration */
                owl_has(ChildURI, rdfs:subClassOf, ConfigurationURI),
                
                (
                        /* get the label and comment */
			get(MI, get_current_name, ChildURI, Label),
			get(MI, get_current_comment, ChildURI, Comment),
                        
                        /* add the configuration to the model */
			addConfigurationDefinition(@model, @nil, Label, Comment),

			/*
                        send(@model, changeRequest, addConfigurationDef,
                                                    @model,
                                                    @nil,
                                                    Label,
                                                    Comment
                        ),
			*/

                        /* Find a reference to the configuration and store it */
                        Configuration = MI<<-get_element_by_name(@model?configurationDefinitions, Label),

			% Add the translations (multiple model support)
			send(MI, import_name_languages, ChildURI, Configuration),
			send(MI, import_remarks_languages, ChildURI, Configuration),

                        send(MI, storeElementDefInIdentifierHash, Configuration, ChildURI)
                )
        ).

importAttributeDef(MI) :->
        get(MI, add_correct_namespace, 'Attribute', AttributeURI),

        forall(
                /* for each child of attribute */
                owl_has(ChildURI, rdfs:subClassOf, AttributeURI),
                (
                        /* get the label and comment */
			get(MI, get_current_name, ChildURI, Label),
			get(MI, get_current_comment, ChildURI, Remarks),

                        /* Find the possible values */
                        owl_has(ChildURI, rdfs:subClassOf, Restriction),
                        owl_has(Restriction, owl:allValuesFrom, ValueClass),
                        owl_has(ValueClass, owl:oneOf, ValueList),
                        get_elements_from_list(ValueList, Values),
                        
                        /* The values have to be provided as a chain of valueReferences) */
			get(MI, get_current_language_key, LanguageKey),
                        findall(ValueReference,
                                (
                                        member(Value, Values),
                                        owl_has(Value, rdfs:label, literal(lang(LanguageKey,ValueLabel))),
                                        new(ValueReference, valueReference(ValueLabel, state)),
					
					% Add the translations
					send(MI, import_values_languages, Value, ValueReference)

                                ),
                                ValueReferences),
                        chain_list(ValueReferencesChain, ValueReferences),
                         
                        /* Add the attribute to the model */
			addAttributeDefinition(@model, @nil, Label, ValueReferencesChain, Remarks),

			/*
                        send(@model, changeRequest, addAttributeDef,
                                                    @model,
                                                    @nil, %D,
                                                    Label, %D?name_member?selection,
                                                    ValueReferencesChain,
                                                    Remarks %D?remarks_member?contents
                        ),
			*/

                        /* Find a reference to the AttributeDef and store it */
                        AttributeDef = MI<<-get_element_by_name(@model?attributeDefinitions, Label),
                        send(MI, storeElementDefInIdentifierHash, AttributeDef, ChildURI),

			% Add the translations (multiple model support)
			send(MI, import_name_languages, ChildURI, AttributeDef),
			send(MI, import_remarks_languages, ChildURI, AttributeDef)
                )
        ).

importQuantitySpaceDef(MI) :->
        get(MI, add_correct_namespace, 'QuantitySpace',  QuantitySpaceURI),

        forall(
                /* for each child of Quantity Space */
                owl_has(ChildURI, rdfs:subClassOf, QuantitySpaceURI),

                (
                        /* get the label and comment */
			get(MI, get_current_name, ChildURI, Label),
			get(MI, get_current_comment, ChildURI, Remarks),

                        /* Find the possible values */
			new(QualitativeValueReferencesChain, chain),
                        send(MI, quantitySpaceOrder, ChildURI, QualitativeValueReferencesChain),
			%format('De waarden zijn: '),
			%send(QualitativeValueReferencesChain, for_all,
			%    message(@prolog, format, ' ~w ', [@arg1?valueName])
			%), nl,
			% chain_list(QuantitySpaceValueChain, QuantitySpaceValueListRev),
			% reverse(QuantitySpaceValueListRev, QuantitySpaceValueList),
			% chain_list(QuantitySpaceTypeChain, QuantitySpaceTypeListRev),
			% reverse(QuantitySpaceTypeListRev, QuantitySpaceTypeList),
			%format('De quantity space: ~w\n', [Label]),
			%format('Hallo de values zijn: ~w\n', [QuantitySpaceValueList]),
			%format('met de waarden: ~w\n', [QuantitySpaceTypeList]),
                                                
                        /* If the quantity space is not Mzp (which is already defined) */
                        (
                                not(Label = 'Mzp') ->
                                /* Add the attribute to the model */
				addQuantitySpaceDefinition(@model, @nil, Label, QualitativeValueReferencesChain, Remarks),
				/*
                                send(@model, changeRequest, addQuantitySpace,
                                                            @model,
                                                            @nil, %D,
                                                            Label, %D?qsName?selection,
                                                            QualitativeValueReferencesChain, % D?qsShow?currentValues,
                                                            Remarks %D?remarks_member?contents)
                                ),
				*/
                                get(@model?quantitySpaces, find, @arg1?name == Label, QuantitySpace),
                                send(MI, storeElementDefInIdentifierHash, QuantitySpace, ChildURI),

				% Add the translations (multiple model support)
				send(MI, import_name_languages, ChildURI, QuantitySpace),
				send(MI, import_remarks_languages, ChildURI, QuantitySpace)
                        ;
                                /* The quantity space already exists */
                                get(@model?quantitySpaces, find, @arg1?name == Label, QuantitySpace),
                                send(MI, storeElementDefInIdentifierHash, QuantitySpace, ChildURI)

                         ),
			 % Delete the QualitativeValueReferencesChain
			 free(QualitativeValueReferencesChain)
                )
        ).

quantitySpaceOrder(MI, QuantitySpaceURI, QualitativeValueReferencesChain) :->
    % Get the intervalValueSets (interval + surrounding points)
    get(MI, quantitySpaceSets, QuantitySpaceURI, IntervalValueSetsChain),
    chain_list(IntervalValueSetsChain, IntervalValueSets),

    % Order the intervalsets
    new(Temp, chain),
    quantitySpaceOrder2(IntervalValueSets, Temp, QualitativeValueReferencesChainWithDoubles),
    
    chain_list(QualitativeValueReferencesChainWithDoubles, QualitativeValueReferencesWithDoubles),
    /* DEBUG */
    %format('De value sets: ~w \n', [QualitativeValueReferencesWithDoubles]),
    %forall(
    %	member(ValueReference, QualitativeValueReferencesWithDoubles),
    %	(
    %	    get(ValueReference, valueName, Value),    
    %	    format(' ~w ', [Value])
    %	)
    %), nl,
    /* NO DEBUG */
    
    forall(
	member(QualitativeValue, QualitativeValueReferencesWithDoubles),
	(
	    get(QualitativeValue, valueName, ValueName),
	    (
		send(QualitativeValueReferencesChain, find, @arg1?valueName == ValueName) ->
		true
	    ;
		send(QualitativeValueReferencesChain, append, QualitativeValue)
	    )
	)
    ).

    
quantitySpaceOrder2([], FinalQualitativeValueReferencesChain, FinalQualitativeValueReferencesChain).
quantitySpaceOrder2(IntervalValueSets, QualitativeValueReferencesChain, FinalQualitativeValueReferencesChain) :-
    select(IntervalValueSetChain, IntervalValueSets, IntervalValueSetsRest),
    chain_list(IntervalValueSetChain, IntervalValueSet),
    (
	send(QualitativeValueReferencesChain, empty) ->
	send(QualitativeValueReferencesChain, merge, IntervalValueSetChain),
	quantitySpaceOrder2(IntervalValueSetsRest, QualitativeValueReferencesChain, FinalQualitativeValueReferencesChain)

    ;
	nth1(1, IntervalValueSet, FirstPoint), get(FirstPoint, type, point),
	get(FirstPoint, valueName, FirstPointName),
	get(QualitativeValueReferencesChain, find, @arg1?valueName == FirstPointName, CorrespondingPoint),
	%send(QualitativeValueReferencesChain, insert_after, CorrespondingPoint, IntervalValueSetChain)
	send(QualitativeValueReferencesChain, merge, IntervalValueSetChain),
	quantitySpaceOrder2(IntervalValueSetsRest, QualitativeValueReferencesChain, FinalQualitativeValueReferencesChain)

    ;
	nth1(2, IntervalValueSet, LastPoint), get(LastPoint, type, point),
	get(LastPoint, valueName, FirstPointName),
	get(QualitativeValueReferencesChain, find, @arg1?valueName == FirstPointName, CorrespondingPoint),
	%send(QualitativeValueReferencesChain, insert_before, CorrespondingPoint, IntervalValueSetChain)
	send(IntervalValueSetChain, merge, QualitativeValueReferencesChain),
	quantitySpaceOrder2(IntervalValueSetsRest, IntervalValueSetChain, FinalQualitativeValueReferencesChain)

    ;
	nth1(3, IntervalValueSet, LastPoint), get(LastPoint, type, point),
	get(LastPoint, valueName, FirstPointName),
	get(QualitativeValueReferencesChain, find, @arg1?valueName == FirstPointName, CorrespondingPoint),
	%send(QualitativeValueReferencesChain, insert_before, CorrespondingPoint, IntervalValueSetChain)
	send(IntervalValueSetChain, merge, QualitativeValueReferencesChain),
	quantitySpaceOrder2(IntervalValueSetsRest, IntervalValueSetChain, FinalQualitativeValueReferencesChain)
    ).


% Collect each set of values (interval and possible surrounding points)
quantitySpaceSets(MI, QuantitySpaceURI, IntervalValueSetsChain) :<-
    get(MI, add_correct_namespace, 'containsQualitativeValue', ContainsQVURI),
    get(MI, add_correct_namespace, 'Interval', IntervalURI),
    get(MI, add_correct_namespace, 'Inequality', InequalityURI),

    findall(IntervalValueSetChain,
	(
	    owl_has(QuantitySpaceURI, rdfs:subClassOf, IntervalRestriction),
            owl_has(IntervalRestriction, owl:someValuesFrom, IntersectionClass),
            owl_has(IntersectionClass, owl:intersectionOf, List), 
        
	    % This chain will become a small part of the quantity space 
	    new(IntervalValueSetChain, chain),
	    
            get_elements_from_list(List, ElementsList),
	    % One of the members is an interval 
            member(Interval, ElementsList),
            owl_has(Interval, rdfs:subClassOf, IntervalURI),

	    get(MI, get_current_name, Interval, IntervalLabel),
	    new(IntervalValueReference, valueReference(IntervalLabel, interval)),
	    send(MI, import_values_languages, Interval, IntervalValueReference),

	    send(IntervalValueSetChain, append, IntervalValueReference),
		
	    % Get the points surrounding the interval 
	    forall(
                (
                    member(Element, ElementsList),
                    owl_has(Element, owl:someValuesFrom, InequalityRestriction),
                    owl_has(InequalityRestriction, owl:intersectionOf, InequalityTargetList),
                    get_elements_from_list(InequalityTargetList, InequalityTargetElements)
                ),
		(		    
                    % One of the elements in the inequality
                    member(InequalityType, InequalityTargetElements),
                    owl_has(InequalityType, rdfs:subClassOf, InequalityURI),
                    owl_has(InequalityType, rdfs:label, literal(lang(en, InequalityTypeLabel))),

                    % The other is the target point
                    member(PointRestriction, InequalityTargetElements),
                    owl_has(PointRestriction, owl:someValuesFrom, Point),
		    get(MI, get_current_name, Point, PointLabel),

       		    new(PointValueReference, valueReference(PointLabel, point)),
		    send(MI, import_values_languages, Point, PointValueReference),

		    (
			InequalityTypeLabel == 'Greater Than' ->
                        send(IntervalValueSetChain, insert_after, PointValueReference, IntervalValueReference)
                    ;
			InequalityTypeLabel == 'Smaller Than' ->
                        send(IntervalValueSetChain, insert_before, PointValueReference, IntervalValueReference)
                    )       
		)
	    )
	),
	IntervalValueSets1
    ),
    % It is possible that there is only a single point value 
    (
	IntervalValueSets1 = [] ->
        owl_has(QuantitySpaceURI, rdfs:subClassOf, ContainsRestriction),
        owl_has(ContainsRestriction, owl:onProperty, ContainsQVURI),
        owl_has(ContainsRestriction, owl:someValuesFrom, PointValueURI),
        owl_has(PointValueURI, rdfs:label, literal(PointValueName)),
        new(PointValueReference, valueReference(PointValueName, point)),
	chain_list(IntervalValueSets, [PointValueReference])
    ;
        IntervalValueSets = IntervalValueSets1
    ),
    %format(' ~w ', [IntervalValueSets]),
    chain_list(IntervalValueSetsChain, IntervalValueSets).


importQuantityDef(MI) :->
        get(MI, add_correct_namespace, 'Quantity',  QuantityURI),

        forall(
                /* for each child of attribute */
                owl_has(ChildURI, rdfs:subClassOf, QuantityURI),

                (
                        /* get the label and comment */
			get(MI, get_current_name, ChildURI, Label),
			get(MI, get_current_comment, ChildURI, Remarks),

                        /* Get the possible quantity spaces */
                        owl_has(ChildURI, rdfs:subClassOf, MagnitudeRestriction),
                        owl_has(MagnitudeRestriction, owl:someValuesFrom, MagnitudeAndQuantitySpacesClass),
                        owl_has(MagnitudeAndQuantitySpacesClass, owl:intersectionOf, MagnitudeAndQuantitySpacesList),
                        get_elements_from_list(MagnitudeAndQuantitySpacesList, IntersectionElements),
                        member(QuantitySpacesRestriction, IntersectionElements),
                        owl_has(QuantitySpacesRestriction, owl:allValuesFrom, QuantitySpacesClass),
                        owl_has(QuantitySpacesClass, owl:unionOf, QuantitySpacesList),
                        get_elements_from_list(QuantitySpacesList, QuantitySpacesElements),

                        /* Find all the quantity space definitions */
                        findall(QuantitySpace,
                                (
                                member(QuantitySpaceName, QuantitySpacesElements),
				get(MI, get_current_name, QuantitySpaceName, QSLabel),
                                QuantitySpace = MI<<-get_element_by_name(@model?quantitySpaces, QSLabel)
                                ),
                                QuantitySpaces),                                
                        chain_list(QuantitySpacesChain, QuantitySpaces),
                        
                        /* Add the quantity space definition */
			addQuantityDefinition(@model, @nil, Label, QuantitySpacesChain, Remarks),
			/*
                        @model->>changeRequest(addQuantityDef,
                                               @model,
                                               @nil, %D,
                                               Label, %D?quantityName?selection,
                                               QuantitySpacesChain, %D?currentAllowedQS,
                                               Remarks %D?remarks?contents)
                        ),
			*/
                        get(@model?quantityDefinitions, find, @arg1?name == Label, Quantity),
                        %send(MI?identifierHash, append, Quantity, ChildURI) % REPLACED
                        send(MI, storeElementDefInIdentifierHash, Quantity, ChildURI),

			% Add the translations (multiple model support)
			send(MI, import_name_languages, ChildURI, Quantity),
			send(MI, import_remarks_languages, ChildURI, Quantity)
                )
        ).


importModelFragmentDefs(MI) :->
	%get(MI, add_correct_namespace, 'ModelFragment', ModelFragmentURI),
	get(MI, add_correct_namespace, 'StaticFragment', StaticFragmentURI),
	get(MI, add_correct_namespace, 'ProcessFragment', ProcessFragmentURI),
	get(MI, add_correct_namespace, 'AgentFragment', AgentFragmentURI),

        (
	get(@model?modelFragments, find, @arg1?name == 'Static fragment', StaticFragment);
	get(@model?modelFragments, find, @arg1?name == 'Static', StaticFragment)
        ),
        (
        get(@model?modelFragments, find, @arg1?name == 'Process fragment', ProcessFragment);
        get(@model?modelFragments, find, @arg1?name == 'Process', ProcessFragment)
        ),
        (
	get(@model?modelFragments, find, @arg1?name == 'Agent fragment', AgentFragment);
	get(@model?modelFragments, find, @arg1?name == 'Agent', AgentFragment)
        ),

        send(MI, storeElementDefInIdentifierHash, StaticFragment, StaticFragmentURI),
        send(MI, storeElementDefInIdentifierHash, ProcessFragment, ProcessFragmentURI), 
        send(MI, storeElementDefInIdentifierHash, AgentFragment, AgentFragmentURI),

	forall(
                /* for each child of the model fragment which is not predefined */
                owl_has(MFChildURI, rdfs:subClassOf, StaticFragmentURI),
                (
                    send(MI, possibleToImportMF, MFChildURI),
		    not(send(MI?modelFragmentsDone, member, MFChildURI)) ->
		    send(MI, importModelFragmentDef, MFChildURI)
		;
                    send(MI, possibleToImportMF, MFChildURI),
		    send(MI?modelFragmentsDone, member, MFChildURI) ->
		    true
		;
                    send(MI?modelFragmentsToDo, append, MFChildURI)
                )
        ),
	forall(
	    /* for each child of the model fragment which is not predefined */
	    owl_has(MFChildURI, rdfs:subClassOf, ProcessFragmentURI),
                (
                    send(MI, possibleToImportMF, MFChildURI),
		    not(send(MI?modelFragmentsDone, member, MFChildURI)) ->
		    send(MI, importModelFragmentDef, MFChildURI)
                ;
                    send(MI, possibleToImportMF, MFChildURI),
		    send(MI?modelFragmentsDone, member, MFChildURI) ->
		    true
		;
                    send(MI?modelFragmentsToDo, append, MFChildURI)
                )
	),
	forall(
                /* for each child of the model fragment which is not predefined */
                owl_has(MFChildURI, rdfs:subClassOf, AgentFragmentURI),
                (
                    send(MI, possibleToImportMF, MFChildURI),
		    not(send(MI?modelFragmentsDone, member, MFChildURI)) ->
		    send(MI, importModelFragmentDef, MFChildURI)
                ;
                    send(MI, possibleToImportMF, MFChildURI),
		    send(MI?modelFragmentsDone, member, MFChildURI) ->
		    true
		;
                    send(MI?modelFragmentsToDo, append, MFChildURI)
                )
        ),
        importRemainingMFs(MI).

importRemainingMFs(MI) :-
        send(MI?modelFragmentsToDo, empty).

importRemainingMFs(MI) :-
        send(MI?modelFragmentsToDo, for_some,
                and(
                        if(
                                and(
				    message(MI, possibleToImportMF, @arg1),
				    not(message(MI?modelFragmentsDone, member, @arg1))
				),
                                message(MI, importModelFragmentDef, @arg1),
				if(
				    and(
					message(MI, possibleToImportMF, @arg1),
					message(MI?modelFragmentsDone, member, @arg1)
				    ),
				    message(@prolog, true),
				    message(MI?modelFragmentsToDo, append, @arg1)
				)
                        ),
                        message(MI?modelFragmentsToDo, delete, @arg1)
                )
        ),
        %get(MI?modelFragmentsToDo, size, Hoeveel),
        %format('Nog maar ~w te doen\n', [Hoeveel]),
        importRemainingMFs(MI).


                

possibleToImportMF(MI, ModelFragmentIURI) :->
        get(MI, add_correct_namespace, 'hasCondition', HasConditionURI),
        get(MI, add_correct_namespace, 'ModelFragment', ModelFragmentURI),

        findall(ImportedModelFragmentURI,
                (
                        owl_has(ModelFragmentIURI, HasConditionURI, ImportedModelFragmentURI),
                        owl:j_instance_of(ImportedModelFragmentURI, ModelFragmentURI)
                ),
                ImportedModelFragmentURIs),
        %format('Found ~w\n', [ImportedModelFragmentURIs]),
        forall(
                member(ImportedFragmentIURI, ImportedModelFragmentURIs),
                (
                        owl_has(ImportedFragmentIURI, rdf:type, ImportedFragmentDefURI),
                        %format('Searching for ~w\n', [ImportedFragmentDefURI]),
        	        get(MI, findElementDefInIdentifierHash, ImportedFragmentDefURI, _ModelFragment)
                )
        ).
        


importModelFragmentDef(MI, ModelFragmentURI) :->
	% Do not consider the previous refinements
	send(MI?refinements, clear),

        /* get the label and comment */
	get(MI, get_current_name, ModelFragmentURI, Label),
	get(MI, get_current_comment, ModelFragmentURI, Remarks),

        debug(owl(general), '---- Importing Model Fragment Definition: ~w ----', [Label]),

	findall(ParentModelFragment,
		(
			owl_has(ModelFragmentURI, rdfs:subClassOf, ParentModelFragmentURI),
			get(MI, findElementDefInIdentifierHash, ParentModelFragmentURI, ParentModelFragment)
       		),
		ParentModelFragmentsList),
	chain_list(ParentModelFragmentsChain, ParentModelFragmentsList),

        /* Find out if the model fragment should be active */ 
        get(MI, add_correct_namespace, 'isActive', IsActiveURI),
        owl_has(ModelFragmentURI, IsActiveURI, literal(ActiveBoolean)),
        boolean_to_flag(ActiveBoolean, ActiveFlag),

        /* create the aggregate and acquire a reference to it */
	addModelFragmentDefinition(@model, @nil, Label, Remarks, ParentModelFragmentsChain, ActiveFlag),

        /* Find and store the reference to the model fragment */
        get(MI, get_element_by_name, @model?modelFragments, Label, Aggregate),
        send(MI, storeElementDefInIdentifierHash, Aggregate, ModelFragmentURI),

	% Add the translations (multiple model support)
	send(MI, import_name_languages, ModelFragmentURI, Aggregate),
	send(MI, import_remarks_languages, ModelFragmentURI, Aggregate),

        /* Set the layout Info for the Aggregate */
	send(MI, setAggregateLayout, Aggregate, ModelFragmentURI),

	/*  Acquire and sort all the elements in the aggregate */
        get(MI, findAndSortAllConditionsAndConsequences, ModelFragmentURI, @off, SortedConditionsAndConsequencesList),

        /* Import each element */ 
        forall(
		member(ElementURI, SortedConditionsAndConsequencesList),
                send(MI, importAggregateElement, ElementURI, Aggregate)
        ),

	/* Flag the model fragment URI as done */
	send(MI?modelFragmentsDone, append, ModelFragmentURI),
	
	/* Import the children model fragments */
	forall(
		owl_has(ModelFragmentChildURI, rdfs:subClassOf, ModelFragmentURI),
                (
		    % The MF can be imported, and has not been imported before (due to multiple inheritance)
                    send(MI, possibleToImportMF, ModelFragmentChildURI),
		    not(send(MI?modelFragmentsDone, member, ModelFragmentChildURI)) ->
		    send(MI, importModelFragmentDef, ModelFragmentChildURI)
                ;
                    send(MI, possibleToImportMF, ModelFragmentChildURI),
		    send(MI?modelFragmentsDone, member, ModelFragmentChildURI) ->
		    true
		;
                    send(MI?modelFragmentsToDo, append, ModelFragmentChildURI)
                )
	).

importScenarioDef(MI) :->
        get(MI, add_correct_namespace, 'Scenario',  ScenarioURI),

        forall(
                /* for each child of scenario */
                owl_has(ChildURI, rdfs:subClassOf, ScenarioURI),
                (
                        /* get the label and comment */
			get(MI, get_current_name, ChildURI, Label),
			get(MI, get_current_comment, ChildURI, Remarks),

                        /* create the aggregate and acquire a reference to it */
			addScenarioDefinition(@model, @nil, Label, Remarks),
			get(MI, get_element_by_name, @model?modelFragments, Label, Aggregate), !,

                        /* Store the reference to the model fragment */
                        %send(MI?identifierHash, append, Aggregate, ChildURI), % REPLACED
                        send(MI, storeElementDefInIdentifierHash, Aggregate, ChildURI),

			/* Set the layout Info for the Aggregate */
			send(MI, setAggregateLayout, Aggregate, ChildURI),

			% Add the translations (multiple model support)
			send(MI, import_name_languages, ChildURI, Aggregate),
			send(MI, import_remarks_languages, ChildURI, Aggregate),

                        /*  Acquire and sort all the elements in the aggregate */
                        get(MI, findAndSortAllConditionsAndConsequences, ChildURI, @off, SortedConditionsAndConsequencesList),
			
			%forall(member(CORC,SortedConditionsAndConsequencesList), format('~w\n', [CORC])), nl,
                        /* export each element */
                        forall(
                                member(ElementURI, SortedConditionsAndConsequencesList),
                                (
                                        send(MI, importAggregateElement, ElementURI, Aggregate) 
				)
                        )
                )
        ).

setAggregateLayout(MI, Aggregate, AggregateURI) :->
                        /* Set the layout Info for the Aggregate */
			%get(MI, add_correct_namespace, 'hasDisplayOriginX', HasDisplayOriginX),
			%get(MI, add_correct_namespace, 'hasDisplayOriginY', HasDisplayOriginY),
                        get(MI, add_correct_namespace, 'hasEditSizeHeight', HasEditSizeHeight),
                        get(MI, add_correct_namespace, 'hasEditSizeWidth',  HasEditSizeWidth),

			%owl_has(AggregateURI, HasDisplayOriginX, literal(X)),
			%owl_has(AggregateURI, HasDisplayOriginY, literal(Y)),
                        owl_has(AggregateURI, HasEditSizeHeight, literal(Height)),
                        owl_has(AggregateURI, HasEditSizeWidth,  literal(Width)),

			%new(Point, point(X,Y)),
                        new(Size, size(Width,Height)),

			%send(Aggregate, layOutInfo, Aggregate, displayOrigin, Point),
                        send(Aggregate, layOutInfo, Aggregate, editSize, Size).


importAggregateElement(MI, ElementURI, Aggregate) :->
	get(MI, getType, ElementURI, TypeLabel),
	debug(owl(general), 'Importing element of type ~w: ~w', [TypeLabel, ElementURI]),
        (
                TypeLabel == 'Entity', %format('Importing Entity\n'),
                send(MI, importEntityOrAgent, ElementURI, Aggregate)
        ;
                TypeLabel == 'Agent', %format('Importing Agent\n'),
                send(MI, importEntityOrAgent, ElementURI, Aggregate)
        ;
                TypeLabel == 'ModelFragment', %format('Importing Model Fragment ~w\n', [ElementURI]),
		send(MI, importModelFragment, ElementURI, Aggregate)
        ;
                TypeLabel == 'Assumption', %format('Importing Assumption\n'),
                send(MI, importAssumption, ElementURI, Aggregate)
        ;
                TypeLabel == 'Quantity', %format('Importing Quantity\n'),
		send(MI, importQuantity, ElementURI, Aggregate)
        ;       
                TypeLabel == 'Magnitude'
        ;       
                TypeLabel == 'Derivative'
        ;       
                TypeLabel == 'QuantitySpace'
        ;
                TypeLabel == 'QualitativeValue'
        ;
                TypeLabel == 'Attribute', %format('Importing Attribute\n'),
		send(MI, importAttribute, ElementURI, Aggregate)
        ;
                TypeLabel == 'Configuration', %format('Importing Configuration\n'),
		send(MI, importConfiguration, ElementURI, Aggregate)
        ; 
                TypeLabel == 'CausalDependency', %format('Importing CausalDependency\n'),
		send(MI, importCausalDependency, ElementURI, Aggregate)
        ;
                TypeLabel == 'hasValue', %format('Importing hasValue\n'),
		send(MI, importHasValue, ElementURI, Aggregate)
        ;
                TypeLabel == 'Correspondence', %format('Importing Correspondence\n'), 
                send(MI, importCorrespondence, ElementURI, Aggregate) 
        ;
                TypeLabel == 'Operator', %format('Importing Operator\n'),
                send(MI, importOperator, ElementURI, Aggregate)
        ;
                TypeLabel == 'Inequality', %format('Importing Inequality\n'), 
		% Before importing the first inequality all the calculi have to be imported
		send(MI, importMissingOperator, Aggregate),
		send(MI, importInequality, ElementURI, Aggregate) 
        ;
                TypeLabel == 'Identity', %format('Importing Identity\n'),
                send(MI, importIdentity, ElementURI, Aggregate)
        ;
		@app?mainMenu->>msgBox(string('OWL Import failed: problem importing %s. Please send this error and your OWL file the developers.',
		TypeLabel), alarm), !, 
		fail
        ).

importEntityOrAgent(MI, ElementURI, Aggregate) :->
        /* get the label and comment */
	get(MI, get_current_name, ElementURI, Label),
	get(MI, get_current_comment, ElementURI, Remarks),

        /* The class of which the element is an instance */
        owl_has(ElementURI, rdf:type, ParentURI),
        %get(MI?identifierHash, find_key, @arg2 == ParentURI, Parent), % REPLACED
        get(MI, findElementDefInIdentifierHash, ParentURI, Parent),

        /* Condition or Consequence? */
        get(MI, conditionOrConsequence, ElementURI, ConditionOrConsequence),

        /* Add the Entity or Agent to the Aggregate */
	addEntityInstance(@model, Aggregate, Parent, Label, Remarks, ConditionOrConsequence, @nil),
	/*
        @model->>changeRequest(newFInstance,
                               Aggregate, %D?modelFragment,
                               @nil, % D?editor,
                               Parent, %D?entity,
                               ConditionOrConsequence, %D?state,
                               Label, %D?instanceName,
                               Remarks %D?remarks).
        ),
	*/

        /* Find the Element and store the reference */
        findElementInAggregate(MI, ElementURI, Aggregate, Element),
        %send(MI?identifierHash, append, Element, ElementURI), % REPLACED
        send(MI, storeInstanceInIdentifierHash, Element, ElementURI),
        %format('I searched ~w and found ~w ~w ~w\n', [ElementURI, ElementX, RouteX, MainMFX]),

	% Add the translations (multiple model support)
	send(MI, import_name_languages, ElementURI, Element),
	send(MI, import_remarks_languages, ElementURI, Element),

        /* Update the layout */
        send(MI, updateLayout, Aggregate, Element, ElementURI).
        

importModelFragment(MI, ElementURI, Aggregate) :->
        /* get the comment */
	get(MI, get_current_comment, ElementURI, Remarks),

	/* The Imported Model Fragment Definition*/
	owl_has(ElementURI, rdf:type, ImportedModelFragmentDefURI),
	get(MI, findElementDefInIdentifierHash, ImportedModelFragmentDefURI, ImportedModelFragmentDef), !,

        /* The parents of the Aggregate (in case of inheritance) */
        get(MI, findElementDefURIInIdentifierHash, Aggregate, AggregateURI),
	
	findall(AggregateParentURI,
	    owl_has(AggregateURI, rdfs:subClassOf, AggregateParentURI),
	    AggregateParentURIs),

        /* If the Imported Model Fragment matches the Parent of the Model Fragment, AND the imported model fragment is
         * already imported, the imported model fragment is inheritied */
        (
                member(ImportedModelFragmentDefURI, AggregateParentURIs),
		findElementInAggregate(MI, ElementURI, Aggregate, PossiblyTheImportedModelFragment),
                findRouteInGarp(PossiblyTheImportedModelFragment, Aggregate, PossiblyImportedModelFragmentRoute, _MainMF),
                /* Is not already imported (inherited) */
                not(get(MI, findInstanceURIInIdentifierHash, PossiblyTheImportedModelFragment, 
                                                 PossiblyImportedModelFragmentRoute,
                                                 _AnImportedModelFragmentURI)) ->
                send(MI, storeInstanceInIdentifierHash, PossiblyTheImportedModelFragment, ElementURI)
        ;
		/* The model fragment is actually a refinement */
		get(MI, add_correct_namespace, 'isRefinedFrom', IsRefinedFromURI),
		owl_has(ElementURI, IsRefinedFromURI, _OriginalAggregateURI) ->
		true
	;
		/* Else the imported fragment should be created */
		addFragmentInstance(@model, Aggregate, ImportedModelFragmentDef, Remarks, @nil),

		/* Store the identifier of the newly imported model fragment */
		get(Aggregate?elements, tail, ImportedModelFragment),
                send(MI, storeInstanceInIdentifierHash, ImportedModelFragment, ElementURI),

		% Add the translations (multiple model support)
		send(MI, import_remarks_languages, ElementURI, ImportedModelFragment)
        ),

	/* Set the layout of the aggregate */
	findInstanceInIdentifierHash(MI, ElementURI, ImportedModelFragment, _Route),
        send(MI, updateLayout, Aggregate, ImportedModelFragment, ElementURI),

	/* Find and sort the content of the imported model fragment */
        get(MI, findAndSortAllConditionsAndConsequences, ElementURI, @off, SortedConditionsAndConsequencesList),

	/* Loop through the elements of the imported fragment, attach the right ID's and add the 
	*  right positioning information */
	forall(
	    member(ConditionURI, SortedConditionsAndConsequencesList),
	    (
		send(MI, fixImportedFragmentElement, ConditionURI, ImportedModelFragment?referencedFragment, Aggregate)
       	    )
	).

getRouteFromRefinementToRefined(MI, RefinementURI, RouteChain) :<-
	get(MI, add_correct_namespace, 'isRefinedFrom', IsRefinedFromURI),
	owl_has(RefinementURI, rdf:type, RefinementModelFragmentClass),
	owl_has(RefinementURI, IsRefinedFromURI, OriginalModelFragmentClass),
	getRouteFromRefinementsToRefined(MI, RefinementModelFragmentClass, OriginalModelFragmentClass, RevRoute),
	reverse(RevRoute, Route),
	chain_list(RouteChain, Route).
    
getRouteFromRefinementsToRefined(MI, ChildMFURI, ParentMFURI, [ParentMF]) :-
    owl_has(ChildMFURI, rdfs:subClassOf, ParentMFURI),
    get(MI, findElementDefInIdentifierHash, ParentMFURI, ParentMF).

getRouteFromRefinementsToRefined(MI, ChildMFURI, ParentMFURI, [NotTheParentMF|Route]) :-
    owl_has(ChildMFURI, rdfs:subClassOf, NotTheParentMFURI),
    get(MI, findElementDefInIdentifierHash, NotTheParentMFURI, NotTheParentMF),
    getRouteFromRefinementsToRefined(MI, NotTheParentMFURI, ParentMFURI, [Route]).

/*
findAllRefinedOrigins(MI, ChildMFURI, [Parent|Route]) :-
    owl_has(ChildMFURI, rdfs:subClassOf, ParentURI),
    get(MI, findElementDefInIdentifierHash, ParentURI, Parent),
    findAllParents(MI, ParentURI, Route).

findAllRefinedOrigins(MI, ChildMFURI, [Parent]) :-
    owl_has(ChildMFURI, rdfs:subClassOf, ParentURI),
    get(MI, findElementDefInIdentifierHash, ParentURI, Parent).
*/

findAllImportedFragments(MI, Aggregate, ImportedElements) :<-
    new(ImportedElements, chain),
    findAllImportedFragments2(MI, Aggregate, ImportedElements).
   
findAllImportedFragments2(MI, Aggregate, ImportedElementsChain) :-
    /* Get all the imported fragments in the current MF and store them */
    get(Aggregate?elements, find_all,
	or(
	    message(@prolog, ==, @arg1?class_name?value, 'importedFragment'), 
	    message(@prolog, ==, @arg1?class_name?value, 'fragmentRefiner')
	),
    ImportedElements),
    send(ImportedElementsChain, merge, ImportedElements),
    
    % Get the parents of the imported fragments and the parents of the current MF
    get(ImportedElements, map, @arg1?referencedFragment, ImportedElementsParents),
    get(Aggregate, parents, AggregateParents),
    send(ImportedElementsParents, merge, AggregateParents),
    send(ImportedElementsParents, for_all,
	message(@prolog, findAllImportedFragments2, MI, @arg1, ImportedElementsChain)
    ).
    
%getRefinedImportedFragment(MI, ModelFragment, ParentModelFragment, ImportedFragment) :-
%    ModelFragment
%getRefinedImportedFragment(MI, ModelFragment, ParentModelFragment, ImportedFragment) :-
   
replaceRefinedInRoute(_MI, RefinedRoute, Route, UpdatedRoute) :<-
    new(UpdatedRoute, chain),
    (
	debugging(owl(testModelFragment)) ->
	true
	%,format('De route is '), printChainNames(Route),
	%format('De refined route is '), printChainNames(RefinedRoute)
    ;
	true
    ),
    chain_list(Route, RouteList),
    forall(
	(
	    member(RouteElement, RouteList)
	),
	(
	    % If the element in the route is refined
	    get(RefinedRoute, find,
		if(
		    message(@prolog, ==, @arg1?class_name, 'fragmentRefiner'),
		    message(@prolog, ==, RouteElement, @arg1?refined),
		    message(@prolog, fail)
		),
		Refiner
	    ),
	    get(Refiner, fragment, RefinerFragment),
	    get(RouteElement, name, RouteElementName),
	    findElementByNameInAggregate(RouteElementName, RefinerFragment, NewRouteElement, _NewElementRoute, _MainMF),
	    get(NewRouteElement, fragment, NewRouteElementFragment),
	    getAllMFParentReferences(NewRouteElementFragment, ParentReferencesChain),
	    send(UpdatedRoute, clear),
	    send(UpdatedRoute, merge, ParentReferencesChain)
	;
	    send(UpdatedRoute, append, RouteElement) 
	)
    ).


/* JOCHEM TODO BUG FIXME */
routeSubsetEqual(MI, RouteOfElement, RouteToMain) :->
    %format('De route van het element is: '), printChainNames(RouteOfElement),
    %format('De route to main is: '), printChainNames(RouteToMain),
    get(MI, replaceRefinedInRoute, RouteOfElement, RouteToMain, RouteToMain2),
    %format('De route to main2 is: '), printChainNames(RouteToMain2),
    %format('De route van het element is: '), printChainNames(RouteOfElement),

    get(RouteOfElement, size, RouteOfElementSize), 
    get(RouteToMain2, size, RouteToMainSize),
    SubsetStart is RouteOfElementSize - RouteToMainSize,
    get(RouteOfElement, sub, SubsetStart, RouteOfElementSize, ResultRouteChain), 
    send(ResultRouteChain, equal, RouteToMain2).

	
refineModelFragment(MI, ElementURI, Aggregate) :->
    /* The model fragment is actually a refinement */
    get(MI, add_correct_namespace, 'isRefinedFrom', IsRefinedFromURI),
     (
	owl_has(ElementURI, IsRefinedFromURI, OriginalAggregateURI) ->

	/* Get the route from the refinement model fragment definition to the refined model fragment definition */
	%get(MI, getRouteFromRefinementToRefined, ElementURI, RouteChain),

        % TODO - FIX ME: Jochem - It should be the path to the TargetModelFragmentDef, not above it!	
 	owl_has(ElementURI, rdf:type, TargetRefinementMFURI),
	get(MI, findElementDefInIdentifierHash, TargetRefinementMFURI, TargetRefinementMF),
   
	getAllMFParentReferences(TargetRefinementMF, RouteChain),

	% The RouteChain cannot be garbage collected 
	%send(RouteChain, lock_object, @on),
	send(RouteChain, protect),

	/* To Do get the remarks from the imported fragment/refinements */
	Remarks = '',

	/* Get the original aggregate */
	get(MI, findElementDefInIdentifierHash, OriginalAggregateURI, OriginalAggregate),
	
	once(get(MI, findAllImportedFragments, Aggregate, ImportedElements)),
	chain_list(ImportedElements, ImportedElementsList),
	debug(owl(refinements), 'The possible options are: ~w\n', [ImportedElementsList]),

	member(ImportedElement, ImportedElementsList),

	% Not already refined
    	not(get(MI?refinements, member, ImportedElement, Refinement)),
	
	get(ImportedElement, referencedFragment, OriginalAggregate),

	/* Make sure that this model fragment is not already refined (i.e. you need another model fragment) */
	not(get(MI?refinements, member, ImportedElement, _RefinementElement)),

	once(findRouteInGarp(ImportedElement, Aggregate, ImportedElementRoute, _MainMFUnimportant)),

	get(Aggregate, name, AggregateName), get(Aggregate, class_name, AggregateClass),
	get(ImportedElement, name, ImportedElementName), get(ImportedElement, class_name, ImportedElementClass),
	get(TargetRefinementMF, name, TargetRefinementMFName), get(TargetRefinementMF, class_name, TargetRefinementMFClass),
	
	debug(owl(refinements), '===== Adding newFragmentRefiner to ~w/~w/~w =====', [Aggregate,AggregateClass,AggregateName]),
	debug(owl(refinements), 'Refining ~w/~w/~w with route: ', [ImportedElement, ImportedElementClass, ImportedElementName]),
	( debugging(owl(refinements)) -> printChainNames(ImportedElementRoute) ; true ),
	debug(owl(refinements), ' to ~w/~w/~w\n', [TargetRefinementMF, TargetRefinementMFClass, TargetRefinementMFName]),
	debug(owl(refinements), 'with route: ', []), 
	( debugging(owl(refinements)) -> printChainNames(RouteChain) ; true ), 

	/* Refine this aggregate to what it should become */
	@model->>changeRequest(newFragmentRefiner,
	    Aggregate, %D?modelFragment,
	    @nil, % D?editor,
	    ImportedElement, %PossiblyTheImportedModelFragment, %D?refined,
	    ImportedElementRoute, %PossiblyTheImportedModelFragmentRoute, %D?refinedRoute,
	    TargetRefinementMF, %OriginalAggregate, %D?selectedMF,
	    RouteChain, %D?selectedRoute,
	    Remarks %D?remarks_member?contents
	), 

	% Get the newly created refinement
	get(Aggregate?elements, size, NumberOfElements),
	FinalToLast is NumberOfElements - 1,
	get(Aggregate?elements, nth1, FinalToLast, Refinement),

	% Get the route of the refinement
	once(findRouteInGarp(Refinement, Aggregate, RefinementRouteChain, _Arg1MainMF)),

	/* Make sure the correct imported fragment is refined */
	(
	    /* Save that this imported fragment is refined */
	    send(MI?refinements, append, ImportedElement, Refinement),
	    send(MI, checkSelectedImportedModelFragment, ElementURI, Refinement, RefinementRouteChain, Aggregate) ->
	    true, debug(owl(refinements), 'Refinement correct!!\n', [])
	;
	    % delete the refinement and fail to try another model fragment
	    deleteRefinementInstance(@model, Aggregate, @nil, Refinement),
	    send(MI?refinements, delete, ImportedElement),
	    fail
	),

	% Free RouteChain for garbage collection
	get(RouteChain, protect, _Boolean)

	% Do not store the reference here, search for the refinement when fixing imported model fragments
	%get(MI, findElementDefURIInIdentifierHash, Aggregate, AggregateURI),
	%send(MI, storeInstanceInIdentifierHash, Refinement, ElementURI, AggregateURI)	
    ;
	get(MI, findAndSortAllConditionsAndConsequences, ElementURI, @off, SortedConditionsAndConsequencesList),
	forall(
	    (
		member(MFURI, SortedConditionsAndConsequencesList),
		get(MI, getType, MFURI, 'ModelFragment')
	    ),
	    (
		send(MI, refineModelFragment, MFURI, Aggregate)
	    )
	)
    ).


/* Check if the correct imported model fragment is selected */
checkSelectedImportedModelFragment(MI, ElementURI, Element, ElementRouteChain, Aggregate) :->
    (
	get(MI?refinements, member, Element, Refinement) ->
	findRouteInGarp(Refinement, Aggregate, RefinementRouteChain, _RefinementMainMF),
	send(MI, checkSelectedImportedModelFragment2, ElementURI, Refinement, RefinementRouteChain, Aggregate)
    ;
	send(MI, checkSelectedImportedModelFragment2, ElementURI, Element, ElementRouteChain, Aggregate)
    ).

checkSelectedImportedModelFragment2(MI, ElementURI, Element, ElementRouteChain, Aggregate) :->
    (
	debugging(owl(testModelFragment)) ->
	get(Element, class_name, ElementClass), get(Element, name, ElementName),
	debug(owl(testModelFragment), 'Checking ~w with ~w/~w/~w in ~w with route ', [ElementURI, Element, ElementClass, ElementName, Aggregate]),
	printChainNames(ElementRouteChain)
    ;
	true
    ),

    get(MI, add_correct_namespace, 'hasCondition', HasConditionURI),
    get(MI, add_correct_namespace, 'hasConfiguration', HasConfigurationURI),
    get(MI, add_correct_namespace, 'hasConfigurationTarget', HasConfigurationTargetURI),
    get(MI, add_correct_namespace, 'hasQuantity', HasQuantityURI),
    get(MI, add_correct_namespace, 'isRefinedFrom', IsRefinedFromURI),

    /* For all the entities in the imported model fragment (Garp3) */
    get(Element?referencedFragment, findElements, garpInstance, EntitiesChain),
    get(Element, findElementsInRoute, Aggregate, AllRouteElements),
    %chain_list(AllRouteElements, AllRouteElementsList),
    chain_list(EntitiesChain, Entities),
    debug(owl(testModelFragment), 'The entities are: ~w', [Entities]), 
    forall(
	/* Get the entity and the path relative to the model fragment */
	member(Entity, Entities),
	(
	    get(Entity, name, EntityName),
	    %findRouteInGarp(Entity, Aggregate, EntityRouteChain, _EntityMainMF), % THIS DOES NOT WORK!
	    get(ElementRouteChain, copy, EntityRouteChain),
	    send(EntityRouteChain, append, Element),
	    %format('De route van deze entiteit is: '),printChainNames(EntityRouteChain),
	    %format('En er zou in ieder geval in moeten zitten: '), format('~w\n', [Element]),

	    /* Loop through all the configurations in OWL (related to the entity) 
	     *  and check if they also exist in Garp3 */
	    get(AllRouteElements, find_all, 
		message(@prolog, ==, @arg1?class_name, configuration),
		ConfigurationsChain
	    ),
	    chain_list(ConfigurationsChain, Configurations),
	    debug(owl(testModelFragment), 'The configurations are ~w', [Configurations]),
	    % If the entity is the first argument, it should also be in OWL
	    forall(
		(
		    member(Configuration, Configurations),
		    get(Configuration, argument1, Entity),
		    get(Configuration, argument1Route, ConfigurationArgument1Route),
		    send(MI, routeSubsetEqual, EntityRouteChain, ConfigurationArgument1Route)
		),
		(
		    debug(owl(testModelFragment), '==== Checking configurations 1 ====', []),
		    owl_has(ElementURI, HasConditionURI, EntityURI),
		    get(MI, get_current_name, EntityURI, EntityName),
		    debug(owl(testModelFragment), 'Checking ~w', [EntityURI]),
		    get(Configuration, name, ConfigurationName),
		    owl_has(EntityURI, HasConfigurationURI, ConfigurationURI),
		    get(MI, get_current_name, ConfigurationURI, ConfigurationName),
		    debug(owl(testModelFragment), '==== Done checking configurations 1: ~w ====', [ConfigurationName])
		)
	    ),
	    % If the entity is the second argument, it should also be in OWL
	    
	    forall(
		(
		    member(Configuration, Configurations),
		    get(Configuration, argument2, Entity),
		    get(Configuration, argument2Route, ConfigurationArgument2Route),
		    send(MI, routeSubsetEqual, EntityRouteChain, ConfigurationArgument2Route)
		),
		(
		    debug(owl(testModelFragment), '==== Checking configurations 2 ====', []),
		    owl_has(ElementURI, HasConditionURI, EntityURI),
		    get(MI, get_current_name, EntityURI, EntityName),
    		    debug(owl(testModelFragment), 'Checking ~w', [EntityURI]),
		    get(Configuration, name, ConfigurationName),
		    owl_has(ConfigurationURI, HasConfigurationTargetURI, EntityURI),
		    get(MI, get_current_name, ConfigurationURI, ConfigurationName),
		    debug(owl(testModelFragment), '==== Done checking configurations 2: ~w ====', [ConfigurationName])
		)
	    ),

	    /* Loop through all the quantities related to the entity in OWL 
	     *  related to that specific and check if the quantities also exist
	     *  in Garp3 in the imported model fragment or one of its parents */
	    get(AllRouteElements, find_all, 
		message(@prolog, ==, @arg1?class_name, garpQuantity),
		QuantitiesChain
	    ),
	    chain_list(QuantitiesChain, Quantities),
	    debug(owl(testModelFragment), 'The quantities are ~w', [Quantities]),

	    % If the entity is the first argument, it should also be in OWL
	    forall(
		(
		    member(Quantity, Quantities),
		    get(Quantity, garpInstance, Entity),
		    get(Quantity, instanceRoute, InstanceRoute),
		    send(MI, routeSubsetEqual, EntityRouteChain, InstanceRoute)
		),
		(
		    debug(owl(testModelFragment), '==== Checking quantities ====', []),
		    owl_has(ElementURI, HasConditionURI, EntityURI),
		    get(MI, get_current_name, EntityURI, EntityName),
		    get(Quantity, name, QuantityName),
		    owl_has(EntityURI, HasQuantityURI, QuantityURI),
		    get(MI, get_current_name, QuantityURI, QuantityName),	   	
		    debug(owl(testModelFragment), '==== Done checking quantities: ~w ====', [QuantityName])

		)
	    )
	)
    ),

    /* Check each of the subfragments of this imported fragment to determine if this imported fragment is the correct one */
    forall(
	(
	    owl_has(ElementURI, HasConditionURI, SubFragmentURI),
	    get(MI, getType, SubFragmentURI, 'ModelFragment')
	),
	(
	    /* Search in the model fragment definition of the imported model fragment */
	    /* Find the correct imported fragment */
	    (
		owl_has(SubFragmentURI, IsRefinedFromURI, _OriginalMFURI1) ->
		% Dit gaat ook niet goed op een of andere manier :(
		findElementInAggregate(MI, SubFragmentURI, Aggregate, SubFragment)
	    ;
		get(Element, referencedFragment, MFDefinitionOfIMF),
		findElementInAggregate(MI, SubFragmentURI, MFDefinitionOfIMF, SubFragment)
	    ),
	    
	    /* The route should incorporate the rest of the route */
	    (
		owl_has(SubFragmentURI, IsRefinedFromURI, _OriginalMFURI2) ->
		chain_list(SubFragmentRouteChain, [])
	    ;
	        findRouteInGarp(SubFragment, Aggregate, SubFragmentRouteChain, _Arg1MainMF),
		send(SubFragmentRouteChain, member, Element),
		send(ElementRouteChain, for_all,
		    message(SubFragmentRouteChain, member, @arg1)
		)
	    ),
	    
	    % DO CHECK!
	    debug(owl(testModelFragment), '==== Checking subfragment ~w ====', [SubFragmentURI]),
	    send(MI, checkSelectedImportedModelFragment, SubFragmentURI, SubFragment, SubFragmentRouteChain, Aggregate),
	    debug(owl(testModelFragment), '==== Done checking subfragment ~w ====', [SubFragmentURI])

	)
    ). 



/* TODO: 
* 1. Partially done: Make sure arguments have the same paths in Garp3 & OWL
* 2. There can be multiple inequalities on a single quantity. Perhaps distinguish per type (value/quantity/type)?
* 3. What will probably help is making it impossible to store an inequality if it already exists 
* ( not in IdentifierHash )
*/
fixImportedFragmentElement(MI, ElementURI, ImportedModelFragment, Aggregate) :->
    	get(MI, add_correct_namespace, 'hasCondition', HasConditionURI),
	get(MI, add_correct_namespace, 'hasConsequence', HasConsequenceURI),
	get(MI, add_correct_namespace, 'hasQuantity', HasQuantityURI),
	get(MI, add_correct_namespace, 'hasConfiguration', HasConfigurationURI),
	get(MI, add_correct_namespace, 'hasConfigurationTarget', HasConfigurationTargetURI),
        get(MI, add_correct_namespace, 'hasCorrespondence', HasCorrespondenceURI),
        get(MI, add_correct_namespace, 'hasCorrespondenceTarget', HasCorrespondenceTargetURI),
        %get(MI, add_correct_namespace, 'hasInequality', HasInequalityURI),
        %get(MI, add_correct_namespace, 'hasInequalityTarget', HasInequalityTargetURI),
        get(MI, add_correct_namespace, 'hasMagnitude', HasMagnitudeURI),
        get(MI, add_correct_namespace, 'hasDerivative', HasDerivativeURI),
        get(MI, add_correct_namespace, 'containsQualitativeValue', ContainsQVURI),
        get(MI, add_correct_namespace, 'hasQuantitySpace', HasQuantitySpaceURI),
	get(MI, add_correct_namespace, 'isRefinedFrom', IsRefinedFromURI),

       	get(MI, getType, ElementURI, TypeLabel),
	
	debug(owl(general), 'Fixing ~w/~w', [ElementURI, TypeLabel]),

	%owl_has(ElementURI, rdfs:label, literal(Label)),
        (
                TypeLabel == 'ModelFragment' ->

                /* Find the correct imported fragment */
		(
		    owl_has(ElementURI, IsRefinedFromURI, _OriginalMFURI1) ->
		    ( 
			% Already refined
			findElementInAggregate(MI, ElementURI, Aggregate, Element)
		    ;
			% Try to refine first
			send(MI, refineModelFragment, ElementURI, Aggregate),
			findElementInAggregate(MI, ElementURI, Aggregate, Element)
			/*
			owl_has(OriginalMFURI1, rdfs:label, literal(Name)),		    
			get(ImportedModelFragment, findElements, 'importedFragment', @on, IMFs),
			send(IMFs, merge, Aggregate?parentReferences),
			chain_list(IMFs, IMFsList),
			member(Element, IMFsList),
			get(Element?referencedFragment, name, Name)
			*/
		    ) 
		;
		    send(MI, refineModelFragment, ElementURI, Aggregate),
		    findElementInAggregate(MI, ElementURI, ImportedModelFragment, Element)
		    %get(Aggregate, findElements, 'importedFragment', @off, IMFs),
		    %send(IMFs, merge, Aggregate?parentReferences),
		    %chain_list(IMFs, IMFsList),
		    %member(Element, IMFsList),
		),
         
                /* The Imported Fragment may not be given a URI yet */
		findRouteInGarp(Element, Aggregate, ElementRouteChain, _Arg1MainMF),
	   
		/* DEBUG 
		(
		    get(MI, findInstanceURIInIdentifierHash, Element, ElementRouteChain, _ElementURITest) ->
		    debug(owl(general), 'This element is already assigned!', []),
		    fail
		;
		    true
		),
		 END DEBUG */
		
		% NOt already a mapped imported fragment
		not(get(MI, findInstanceURIInIdentifierHash, Element, ElementRouteChain, _ElementURI1)),
		
		% Not already refined
		not(get(MI?refinements, member, Element, _Refinement)),

		% Debug 
		%chain_list(ElementChain, Element),
		%format('Het element en de route: '),
		%printChainNames(ElementChain),
		%printChainNames(ElementRouteChain), nl,
		% End debug
		get(Element, class_name, ElementClass),
		get(Element, name, ElementName),
		debug(owl(testModelFragment), '!!!! Checking ~w with element ~w/~w/~w !!!!\n', [ElementURI, Element, ElementClass, ElementName]),
    
		send(MI, checkSelectedImportedModelFragment, ElementURI, Element, ElementRouteChain, Aggregate),

		/* If this model fragment is a refinement used as a model fragment, store it as a refinement */
		(
		    get(Element, class_name, 'fragmentRefiner'),
		    not(owl_has(ElementURI, IsRefinedFromURI, _OriginalMFURI3)) ->
		    get(Element, refined, RefinedElement),
		    send(MI?refinements, append, RefinedElement, Element)
		;
		    true
		), 

		!, % NO more backtracking from here on

		/* Store this correct imported model fragment and update its layout */
		(
		    %owl_has(ElementURI, IsRefinedFromURI, OriginalMFURI2), 
		    %owl_has(OriginalMFURI2, rdfs:label, literal(OriginalMFName)),
		    %get(Element, class_name, 'fragmentRefiner'),
		    %not(get(Element, name, OriginalMFName)) ->
		    get(MI, findMainMF, ElementURI, MainMFURI),
		    get(MI?refinements, member, Element, Refinement) ->
		    send(MI, storeInstanceInIdentifierHash, Refinement, ElementURI, MainMFURI),
		    RefinementOrElement = Refinement
		;
		    send(MI, storeInstanceInIdentifierHash, Element, ElementURI),
		    RefinementOrElement = Element
		),

	        send(MI, updateLayout, Aggregate, RefinementOrElement, ElementURI),
                
        	/* Find and sort the content of the imported model fragment */
                get(MI, findAndSortAllConditionsAndConsequences, ElementURI, @off, SortedConditionsAndConsequencesList),
                %format('De lijst met stuff voor ~w is: ~w\n', [ElementURI, SortedConditionsAndConsequencesList]),

		
        	/* Loop through the elements of the imported fragment, attach the right ID's and add the 
	        *  right positioning information */
        	forall(
	            member(ConditionURI, SortedConditionsAndConsequencesList),
        	    (
			% Check that the correct imported model fragment is selected
		        send(MI, fixImportedFragmentElement, ConditionURI, RefinementOrElement?referencedFragment, Aggregate)
       	            )
	        )
        ;
	        (TypeLabel == 'Entity' ; TypeLabel == 'Agent') ->

		% Find an entity/agent with the same name (backtrack if it is the wrong one)
	        findElementInAggregate(MI, ElementURI, ImportedModelFragment, Element),	
		
		% The entity should not be already mapped to an element in OWL 

		% The entity should have the same route in Garp as in OWL 
		findMainMFAndRoute2(MI, ElementURI, _AEMainMF, ElementRouteChainOWL),
		findRouteInGarp(Element, Aggregate, ElementRouteChainGarp, _AEElementMainMF),

		%chain_list(ElementRouteChainOWL, List1),
		%chain_list(ElementRouteChainGarp, List2),
		%format('~w: Comparing ~w with ~w\n', [ElementURI, List1, List2]),

		send(ElementRouteChainOWL, equal, ElementRouteChainGarp),

		% The element should have the same configurations
                get(MI, add_correct_namespace, 'hasConfiguration', HasConfigurationURI),
                get(MI, add_correct_namespace, 'hasConfigurationTarget', HasConfigurationTargetURI),
		%get(Element, relations, RelationsChain),
		%chain_list(RelationsChain, RelationsList),

		/* Improved checks for configurations */
		
		/* Loop through all the configurations in OWL (related to the entity) 
		*  and check if they also exist in Garp3 */
		(	
		    owl_has(ImportedFragmentURI, HasConditionURI, ElementURI)
		;
		    owl_has(ImportedFragmentURI, HasConsequenceURI, ElementURI)
		),
		findInstanceInIdentifierHash(MI, ImportedFragmentURI, IMF, _IMFRoute),
		get(IMF, findElementsInRoute, Aggregate, AllRouteElements),
		
		get(AllRouteElements, find_all, 
		    message(@prolog, ==, @arg1?class_name, configuration),
		    ConfigurationsChain
		),
		chain_list(ConfigurationsChain, Configurations),
		%format('The configurations are ~w\n', [Configurations]),
		% If the entity is the first argument, it should also be in OWL
		forall(
		    (
			member(Configuration, Configurations),
			get(Configuration, argument1, Entity),
			get(Configuration, argument1Route, ConfigurationArgument1Route),
			send(MI, routeSubsetEqual, ElementRouteChainGarp, ConfigurationArgument1Route)
		    ),
		    (
			%format('==== Checking configurations 1 ====\n'),
			get(MI, get_current_name, ElementURI, EntityName),
			get(Configuration, name, ConfigurationName),
			owl_has(EntityURI, HasConfigurationURI, ConfigurationURI),
			get(MI, get_current_name, ConfigurationURI, ConfigurationName)
			%format('The configuration ~w is correct\n', [ConfigurationName])
		    )
		),
		% If the entity is the second argument, it should also be in OWL
		forall(
		    (
			member(Configuration, Configurations),
			get(Configuration, argument2, Entity),
			get(Configuration, argument2Route, ConfigurationArgument2Route),
			send(MI, routeSubsetEqual, ElementRouteChainGarp, ConfigurationArgument2Route)
		    ),
		    (
			%format('==== Checking configurations 2 ====\n'),
			get(MI, get_current_name, ElementURI, EntityName),
			get(Configuration, name, ConfigurationName),
			owl_has(ConfigurationURI, HasConfigurationTargetURI, EntityURI),
			get(MI, get_current_name, ConfigurationURI, ConfigurationName)
			%format('The configuration ~w is correct\n', [ConfigurationName])
		    )
		),

		% If the configurations and paths are the same, save the entity and update the layout
                send(MI, storeInstanceInIdentifierHash, Element, ElementURI),
	        send(MI, updateLayout, Aggregate, Element, ElementURI)
                %format('Stored element ~w with URI: ~w\n', [Element, ElementURI])
	;
                TypeLabel == 'Configuration' ->
                get(MI, add_correct_namespace, 'hasConfiguration', HasConfigurationURI),
                get(MI, add_correct_namespace, 'hasConfigurationTarget', HasConfigurationTargetURI),

		% 1. Search for the arguments of the configuration in OWL
                owl_has(Argument1URI, HasConfigurationURI, ElementURI),
                owl_has(ElementURI, HasConfigurationTargetURI, Argument2URI),

		% 2. Get the paths of these arguments (in OWL)
		findMainMFAndRoute2(MI, Argument1URI, _MainMF11, Arg1RouteChain),
		findMainMFAndRoute2(MI, Argument2URI, _MainMF21, Arg2RouteChain),

		% 3. Get a possible configuration (in Garp3) (backtrack to get others)
		findElementInAggregate(MI, ElementURI, ImportedModelFragment, Element),

		% 4. Get the arguments of this configuration (in Garp3)
	        get(Element, argument1, Argument1),
                get(Element, argument2, Argument2),

		% 5. Get the routes of these arguments (in Garp3)
                findRouteInGarp(Argument1, Aggregate, Argument1RouteChain, _Arg1MainMF1),
                findRouteInGarp(Argument2, Aggregate, Argument2RouteChain, _Arg2MainMF1),

    		% Replace refined imported model fragments in the argument path
		send(MI, addRefinementsToRoute, Argument1RouteChain),
		send(MI, addRefinementsToRoute, Argument2RouteChain),
		
		% 6. The routes in Garp3 and in OWL should be identical
		send(Arg1RouteChain, equal, Argument1RouteChain),
		send(Arg2RouteChain, equal, Argument2RouteChain),

		% If the routes are the same, the configuration is correct, and should be stored and updated
                get(MI, findInstanceURIInIdentifierHash, Argument1, Argument1RouteChain, Argument1URI),              
                get(MI, findInstanceURIInIdentifierHash, Argument2, Argument2RouteChain, Argument2URI),	
		send(MI, storeInstanceInIdentifierHash, Element, ElementURI),
	        send(MI, updateLayout, Aggregate, Element, ElementURI)
        ;
		TypeLabel == 'Quantity' ->

		get(MI, add_correct_namespace, 'hasQuantity', HasQuantityURI),
                findElementInAggregate(MI, ElementURI, ImportedModelFragment, Quantity),
                get(Quantity, garpInstance, GarpInstance),
		get(Quantity, instanceRoute, GarpInstanceRouteChain),
		findRouteInGarp(Quantity, Aggregate, QuantityRouteChain, _QuantityMainMF),
		get(QuantityRouteChain, merge, GarpInstanceRouteChain, GarpInstanceRouteChainFull),

		% Replace refined imported model fragments in the argument path
		send(MI, addRefinementsToRoute, GarpInstanceRouteChainFull),

                get(MI, findInstanceURIInIdentifierHash, GarpInstance, GarpInstanceRouteChainFull, GarpInstanceURI),
                /* Check if you got the correct quantity */
                owl_has(GarpInstanceURI, HasQuantityURI, ElementURI),
                /* Store the quantity URI combination */
                send(MI, storeInstanceInIdentifierHash, Quantity, ElementURI),
                /* Update the layout of the quantity (elements) */
                send(MI, updateLayout, Aggregate, Quantity, ElementURI),
                send(MI, updateQuantityElementsLayout, Quantity, ElementURI, Aggregate)
                
        ;
                TypeLabel == 'CausalDependency' ->
                get(MI, add_correct_namespace, 'hasCausalDependency', HasCausalDependencyURI),
                get(MI, add_correct_namespace, 'hasCausalDependencyTarget', HasCausalDependencyTargetURI),
                findElementInAggregate(MI, ElementURI, ImportedModelFragment, Element),
		findRouteInGarp(Element, Aggregate, ElementRoute, _ElementMainMF),
                get(Element, argument1, Argument1),
		%findRouteInGarp(Argument1, Aggregate, Argument1RouteChain, _Arg1MainMF2),
		get(Element, argument1Route, Argument1RouteChain),
		get(ElementRoute, merge, Argument1RouteChain, Argument1RouteChainFull),
		% Replace refined imported model fragments in the argument path
		send(MI, addRefinementsToRoute, Argument1RouteChainFull),
                get(MI, findInstanceURIInIdentifierHash, Argument1, Argument1RouteChainFull, Argument1URI),
                get(Element, argument2, Argument2),
		%findRouteInGarp(Argument2, Aggregate, Argument2RouteChain, _Arg2MainMF2),
		get(Element, argument2Route, Argument2RouteChain),
		get(ElementRoute, merge, Argument2RouteChain, Argument2RouteChainFull),
		% Replace refined imported model fragments in the argument path
		send(MI, addRefinementsToRoute, Argument2RouteChainFull),
                get(MI, findInstanceURIInIdentifierHash, Argument2, Argument2RouteChainFull, Argument2URI),
                % Check if you have the correct causal dependency
                owl_has(Argument1URI, HasCausalDependencyURI, ElementURI),
                owl_has(ElementURI, HasCausalDependencyTargetURI, Argument2URI),
                send(MI, storeInstanceInIdentifierHash, Element, ElementURI),
	        send(MI, updateLayout, Aggregate, Element, ElementURI)
        ;
                TypeLabel == 'Attribute' -> 
                get(MI, add_correct_namespace, 'hasAttribute', HasAttributeURI),
                %get(MI, add_correct_namespace, 'hasAttributeValue', HasAttributeValueURI),
                findElementInAggregate(MI, ElementURI, ImportedModelFragment, Element),
		findRouteInGarp(Element, Aggregate, ElementRoute, _ElementMainMF4),
                
                get(Element, garpInstance, GarpInstance),
                findRouteInGarp(GarpInstance, Aggregate, GarpInstanceRouteChain, _GarpInstanceMainMF2),
		get(ElementRoute, merge, GarpInstanceRouteChain, GarpInstanceRouteChainFull),
		% Replace refined imported model fragments in the argument path
		send(MI, addRefinementsToRoute, GarpInstanceRouteChainFull),
                get(MI, findInstanceURIInIdentifierHash, GarpInstance, GarpInstanceRouteChain, GarpInstanceURI),

                % Check if you have the correct attribute 
                owl_has(GarpInstanceURI, HasAttributeURI, ElementURI),
                % If so store the attribute and update its layout
                send(MI, storeInstanceInIdentifierHash, Element, ElementURI),
	        send(MI, updateLayout, Aggregate, Element, ElementURI)
        ;
                TypeLabel == 'Correspondence' -> 

                findElementInAggregate(MI, ElementURI, ImportedModelFragment, Element),
                get(Element, argument1, Argument1),
                findRouteInGarp(Argument1, Aggregate, Argument1RouteChain, _Arg1MainMF3),
                get(MI, findInstanceURIInIdentifierHash, Argument1, Argument1RouteChain, Argument1URI),
                get(Element, argument2, Argument2),
                findRouteInGarp(Argument2, Aggregate, Argument2RouteChain, _Arg2MainMF3),
                get(MI, findInstanceURIInIdentifierHash, Argument2, Argument2RouteChain, Argument2URI),
                % Check if you have the correct causal dependency
                (
                        % If it is a quantity space correspondence
                        owl_has(QS1URI, HasCorrespondenceURI, ElementURI),
                        owl_has(ElementURI, HasCorrespondenceTargetURI, QS2URI),
                        owl_has(MagOrDer1URI, HasQuantitySpaceURI, QS1URI),
                        owl_has(MagOrDer2URI, HasQuantitySpaceURI, QS2URI),
                        (owl_has(Argument1URI, HasMagnitudeURI, MagOrDer1URI)
                        ;owl_has(Argument1URI, HasDerivativeURI, MagOrDer1URI)),
                        (owl_has(Argument2URI, HasMagnitudeURI, MagOrDer2URI)
                        ;owl_has(Argument2URI, HasDerivativeURI, MagOrDer2URI))
                ;
                        % If it is a value correspondence
                        owl_has(QV1URI, HasCorrespondenceURI, ElementURI),
                        owl_has(ElementURI, HasCorrespondenceTargetURI, QV2URI),
                        owl_has(QS1URI, ContainsQVURI, QV1URI),
                        owl_has(QS2URI, ContainsQVURI, QV2URI) ->
                        owl_has(MagOrDer1, HasQuantitySpaceURI, QS1URI),
                        owl_has(MagOrDer2, HasQuantitySpaceURI, QS2URI),
                        %format('~w ~w en ~w ~w\n', [Argument1URI, MagOrDer1, Argument2URI, MagOrDer2]),
                        (owl_has(Argument1URI, HasMagnitudeURI, MagOrDer1)
                        ;owl_has(Argument1URI, HasDerivativeURI,MagOrDer1)),
                        (owl_has(Argument2URI, HasMagnitudeURI, MagOrDer2)
                        ;owl_has(Argument2URI, HasDerivativeURI,MagOrDer2))
                        
                ),
                send(MI, storeInstanceInIdentifierHash, Element, ElementURI),
	        send(MI, updateLayout, Aggregate, Element, ElementURI)

        ;
                /* TODO: This function does not differentiate between different Operator relations between items of the
                 * same quantities (quantity, derivative or value) */
                TypeLabel == 'Operator' ->
                findElementInAggregate(MI, ElementURI, ImportedModelFragment, Element),
                get(Element, argument1, Argument1),
                findRouteInGarp(Argument1, Aggregate, Argument1RouteChain, _Arg1MainMF4),
                get(MI, findInstanceURIInIdentifierHash, Argument1, Argument1RouteChain, Argument1URI),
                get(Element, argument2, Argument2),
                findRouteInGarp(Argument2, Aggregate, Argument2RouteChain, _Arg2MainMF4),
                get(MI, findInstanceURIInIdentifierHash, Argument2, Argument2RouteChain, Argument2URI),
                send(MI, checkInequalityOrCalculiArgument(1, Argument1URI, ElementURI)),
                send(MI, checkInequalityOrCalculiArgument(2, Argument2URI, ElementURI)),
                send(MI, storeInstanceInIdentifierHash, Element, ElementURI),
	        send(MI, updateLayout, Aggregate, Element, ElementURI)

        ;
                TypeLabel == 'Inequality' ->
                /* TODO: This function does not differentiate between different inequality relations between items of the
                 * same quantities (quantity, derivative or value) */
		get(MI, add_correct_namespace, 'hasInequality', HasInequalityURI),
		get(MI, add_correct_namespace, 'hasInequalityTarget', HasInequalityTargetURI),

		% Find the arguments in OWL
		owl_has(Argument1URI, HasInequalityURI, ElementURI),
		owl_has(ElementURI, HasInequalityTargetURI, Argument2URI),
		
		% Find the routes in OWL
		findMainMFAndRoute2(MI, Argument1URI, _MainMF12, Arg1RouteChain),
		findMainMFAndRoute2(MI, Argument2URI, _MainMF22, Arg2RouteChain),
		
		% Find a possible inequality (in Garp3) [backtrack]
		findElementInAggregate(MI, ElementURI, ImportedModelFragment, Element),

		% Find its arguments 
		get(Element, argument1, Argument1),
                get(Element, argument2, Argument2),

		% Find the routes of the arguments
		findRouteInGarp(Argument1, Aggregate, Argument1RouteChain, _Arg1MainMF5),
                findRouteInGarp(Argument2, Aggregate, Argument2RouteChain, _Arg2MainMF5),
		
		% The OWL and Garp3 routes should be identical 
		send(Arg1RouteChain, equal, Argument1RouteChain),
		send(Arg2RouteChain, equal, Argument2RouteChain),
	
		%get(MI, findInstanceURIInIdentifierHash, Argument1, Argument1RouteChain, Argument1URI),
		%get(MI, findInstanceURIInIdentifierHash, Argument2, Argument2RouteChain, Argument2URI),
		%send(MI, checkInequalityOrCalculiArgument(1, Argument1URI, ElementURI)),
		%send(MI, checkInequalityOrCalculiArgument(2, Argument2URI, ElementURI)),
	
                send(MI, storeInstanceInIdentifierHash, Element, ElementURI),
	        send(MI, updateLayout, Aggregate, Element, ElementURI)
        ;
                /* TODO: the fact that there can be more assumptions of the same type is ignored! */
                TypeLabel == 'Assumption' ->
                findElementInAggregate(MI, ElementURI, ImportedModelFragment, Element),
                get(Element?assumption, name, AssumptionName),
		get(MI, get_current_name, ElementURI, AssumptionName),

                send(MI, storeInstanceInIdentifierHash, Element, ElementURI),
	        send(MI, updateLayout, Aggregate, Element, ElementURI)

                %format('TODO: Layout of assumptions\n')
        ;
                TypeLabel == 'Identity' ->
                get(MI, add_correct_namespace, 'hasIdentity', HasIdentityURI),
                get(MI, add_correct_namespace, 'hasIdentityTarget', HasIdentityTargetURI),
                
                findElementInAggregate(MI, ElementURI, ImportedModelFragment, Element),                        
                
                get(Element, argument1, Argument1),
                findRouteInGarp(Argument1, Aggregate, Argument1RouteChain, _Arg1MainMF6),
                get(MI, findInstanceURIInIdentifierHash, Argument1, Argument1RouteChain, Argument1URI),
                
                get(Element, argument2, Argument2),
                findRouteInGarp(Argument2, Aggregate, Argument2RouteChain, _Arg2MainMF6),
                get(MI, findInstanceURIInIdentifierHash, Argument2, Argument2RouteChain, Argument2URI),
                
                % Check if you have the correct identity
                owl_has(Argument1URI, HasIdentityURI, ElementURI),
                owl_has(ElementURI, HasIdentityTargetURI, Argument2URI),
                send(MI, storeInstanceInIdentifierHash, Element, ElementURI),
	        send(MI, updateLayout, Aggregate, Element, ElementURI)
        ;
                (
                        TypeLabel == 'hasValue' ;
                        TypeLabel == 'Magnitude' ;
                        TypeLabel == 'Derivative' ;
                        TypeLabel == 'QuantitySpace' ;
                        TypeLabel == 'QualitativeValue' 
                )
        ;
            format('Problem setting layout for ~w in imported fragment\n', [TypeLabel]), break,
	    true
	).

/* TODO: Perhaps add the route of the argument as an extra argument 
*  you should check if the route of the argument in OWL is the same as the route in Garp.
*/
checkInequalityOrCalculiArgument(MI, ArgNumber, ArgumentURI, RelationURI) :->
        get(MI, add_correct_namespace, 'hasInequality', HasInequalityURI),
        get(MI, add_correct_namespace, 'hasInequalityTarget', HasInequalityTargetURI),
        get(MI, add_correct_namespace, 'hasLeftHandSide', HasLeftHandSideURI),
        get(MI, add_correct_namespace, 'hasRightHandSide', HasRightHandSideURI),
        %get(MI, add_correct_namespace, 'QualitativeValue', QualitativeValueURI),
        get(MI, add_correct_namespace, 'Operator', OperatorURI),
        get(MI, add_correct_namespace, 'Quantity', QuantityURI),
        %get(MI, add_correct_namespace, 'hasMagnitude', HasMagnitudeURI),
        %get(MI, add_correct_namespace, 'hasDerivative', HasDerivativeURI),
        (
                /* The Argument is a calculus */
                owl:j_instance_of(ArgumentURI, OperatorURI) ->
                (
                        ArgNumber == 1 ->
                        owl_has(ArgumentURI, HasInequalityURI, RelationURI);
                        owl_has(RelationURI, HasLeftHandSideURI, ArgumentURI)
                ;
                        ArgNumber == 2 ->
                        owl_has(RelationURI, HasInequalityTargetURI, ArgumentURI);
                        owl_has(RelationURI, HasRightHandSideURI, ArgumentURI)
                )
        ;
                /* The Argument is a quantity (in OWL possibly a value) */
                /* FIX ME!!! */
                owl:j_instance_of(ArgumentURI, QuantityURI) ->
                (
                        ArgNumber == 1,
                        owl_has(OWLArgumentURI, HasInequalityURI, RelationURI);
                        owl_has(RelationURI, HasLeftHandSideURI, OWLArgumentURI)
                ;
                        ArgNumber == 2,
                        owl_has(RelationURI, HasInequalityTargetURI, OWLArgumentURI);
                        owl_has(RelationURI, HasRightHandSideURI, OWLArgumentURI)
                )
                /* You only test a operator or a quantity, never a value! 
                (      
                        /* If the OWL argument is a value it's quantity should be the Garp argument */
                        owl:j_instance_of(OWLArgumentURI, QualitativeValueURI) ->
                        get(MI, findQuantityFromValue, OWLArgumentURI, ArgumentURI)
                ;
                        /* Otherwise, it's either a magnitude or derivative, and it's quantity is the Garp argument */
                        (
                                owl_has(ArgumentURI, HasMagnitudeURI, OWLArgumentURI)
                                ;
                                owl_has(ArgumentURI, HasDerivativeURI, OWLArgumentURI)
                        )
                ) 
                */
        ).

findQuantityFromValue(MI, ValueURI, QuantityURI) :<-
        get(MI, add_correct_namespace, 'containsQualitativeValue', ContainsQVURI),
        get(MI, add_correct_namespace, 'hasQuantitySpace', HasQuantitySpaceURI),
        get(MI, add_correct_namespace, 'hasMagnitude', HasMagnitudeURI),
        get(MI, add_correct_namespace, 'hasDerivative', HasDerivativeURI),

        owl_has(QuantitySpaceURI, ContainsQVURI, ValueURI),
        owl_has(MagOrDerURI, HasQuantitySpaceURI, QuantitySpaceURI),
        (
                owl_has(QuantityURI, HasMagnitudeURI, MagOrDerURI)
                ;
                owl_has(QuantityURI, HasDerivativeURI, MagOrDerURI)
        ).

        
importAssumption(MI, ElementURI, Aggregate)  :->
        /* get the remark */
	get(MI, get_current_comment, ElementURI, Remarks),

        % The parent of which the assumption is an instance
        owl_has(ElementURI, rdf:type, ParentURI),
        get(MI, findElementDefInIdentifierHash, ParentURI, Parent),

	% The instance (and route) the assumption is associated with
	get(MI, add_correct_namespace, 'isAssumptionRegarding', IsAssumptionRegardingURI),
	(
	    owl_has(ElementURI, IsAssumptionRegardingURI, InstanceURI) ->
	    findInstanceInIdentifierHash(MI, InstanceURI, Instance, InstanceRouteChain)
	;
	    Instance = @nil,
	    InstanceRouteChain = @nil
	),

	addAssumptionInstance(@model, Aggregate, Parent, Remarks, Instance, InstanceRouteChain, @nil),
        /*
        @model->>changeRequest(newAssumptionInstance,
                Aggregate, %D?modelFragment,
                @nil, % D?editor,
                Parent, % D?assumption,
                Remarks, %D?remarks
		Instance, %D?instance,
		InstanceRouteChain %D?instanceRoute
        ),
	*/  
        /* Store the Assumption */
        %findElementInAggregate(MI, ElementURI, Aggregate, Assumption),
        get(Aggregate?elements, tail, Assumption),
        send(MI, storeInstanceInIdentifierHash, Assumption, ElementURI),

	% Add the translations (multiple model support)
	send(MI, import_remarks_languages, ElementURI, Assumption),

        /* Update the layout information */
        send(MI, updateLayout, Aggregate, Assumption, ElementURI).

importQuantity(MI, ElementURI, Aggregate) :->
	/* get the label and comment */
	get(MI, get_current_comment, ElementURI, Remarks),

        /* Get the Quantity definition element */
        owl_has(ElementURI, rdf:type, ParentURI),
        get(MI, findElementDefInIdentifierHash, ParentURI, Parent), 

        /* Find the entity the quantity belongs to */
        get(MI, add_correct_namespace, 'hasQuantity', HasQuantity),
        owl_has(QuantitySourceURI, HasQuantity, ElementURI),
	findInstanceInIdentifierHash(MI, QuantitySourceURI, QuantitySource, QuantitySourceRouteChain),
	%get(MI, findElementDefInIdentifierHash, QuantitySourceURI, QuantitySource),

        /* Find the route to the quantity source */
	% findMainMFAndRoute2(MI, QuantitySourceURI, _MainMF, QuantitySourceRouteChain),

        /* Find the Quantity Space Def */
        get(MI, findQuantitySpaceIDFromQuantityID, ElementURI, 'magnitude', QuantitySpaceURI),
        owl_has(QuantitySpaceURI, rdf:type, QuantitySpaceDefURI),
        get(MI, findElementDefInIdentifierHash, QuantitySpaceDefURI, QuantitySpaceDef), 

        /* Is the ElementURI a condition or a consequence? */
        get(MI, conditionOrConsequence, ElementURI, ConditionOrConsequence),

	/* Get the correct quantity assumptions */
	get(MI, getQuantityAssumptions, ElementURI, QuantityAssumptions),
	
	%chain_list(QuantityAssumptions, QuantityAssumptionsList),
	%format('1. Do I still live here??\n'),
	%format('The quantity assumptions are: ~w\n', [QuantityAssumptionsList]),
    
        /* Send the change request */
	addQuantityInstance(@model, Aggregate, Parent, QuantitySpaceDef, QuantitySource, QuantitySourceRouteChain, QuantityAssumptions,
		    Remarks, ConditionOrConsequence, @nil),
	/*
        @model->>changeRequest(newQuantity,
                Aggregate, %D?modelFragment,
                @nil, %D?editor,
                QuantitySource, %D?instance,
                QuantitySourceRouteChain, %D?instanceRoute,
                ConditionOrConsequence, %D?state_member?selection,
                Parent, %D?selectedQuantityDef,
                QuantitySpaceDef, %D?selectedQuantitySpace,
                Remarks, %D?remarks_member?contents
		QuantityAssumptions
        ),
	*/
        /* Find and store the new quantity instance */
        findElementInAggregate(MI, ElementURI, Aggregate, Quantity),
        send(MI, storeInstanceInIdentifierHash, Quantity, ElementURI), 

	% Add the translations (multiple model support)
	send(MI, import_remarks_languages, ElementURI, Quantity),

        /* Update the layout of the quantity */
        send(MI, updateLayout, Aggregate, Quantity, ElementURI),

        /* Update the layout of the quantity elements */
        send(MI, updateQuantityElementsLayout, Quantity, ElementURI, Aggregate).

getQuantityAssumptions(MI, QuantityIURI, QuantityAssumptionsChain) :<-
	new(QuantityAssumptionsChain, chain),
        get(MI, add_correct_namespace, 'hasQuantityAssumption', HasQuantityAssumptionURI),
	forall(
	    owl_has(QuantityIURI, HasQuantityAssumptionURI, QuantityAssumptionURI),
	    (
		translateQuantityAssumption(MI, QuantityAssumptionInternal, QuantityAssumptionURI),
		send(QuantityAssumptionsChain, append, QuantityAssumptionInternal)
	    )
	).

updateQuantityElementsLayout(MI, Quantity, ElementURI, Aggregate) :->
        get(MI, add_correct_namespace, 'hasMagnitude', HasMagnitude),
        get(MI, add_correct_namespace, 'hasDerivative', HasDerivative),
        get(MI, add_correct_namespace, 'hasQuantitySpace', HasQuantitySpace),
	%get(MI, add_correct_namespace, 'containsQualitativeValue', ContainsQualitativeValue),
        get(MI, add_correct_namespace, 'has_xposition_on_screen', HasXPositionURI),
        get(MI, add_correct_namespace, 'has_yposition_on_screen', HasYPositionURI),
        get(MI, add_correct_namespace, 'is_hidden_on_screen', IsHiddenURI),

        /* The magnitude, derivative and their quantity spaces */
        owl_has(ElementURI, HasMagnitude, MagnitudeURI),
        owl_has(ElementURI, HasDerivative, DerivativeURI),
        owl_has(MagnitudeURI, HasQuantitySpace, MagQuantitySpaceURI),
        owl_has(DerivativeURI, HasQuantitySpace, DerQuantitySpaceURI),

        /* The Magnitude Quantity Space Layout */
        owl_has(MagQuantitySpaceURI, HasXPositionURI, literal(MagQSX)),
        owl_has(MagQuantitySpaceURI, HasYPositionURI, literal(MagQSY)),
        new(MagQSPoint, point(MagQSX, MagQSY)),
        owl_has(MagQuantitySpaceURI, IsHiddenURI, literal(MagQSHidden)),
        boolean_to_flag(MagQSHidden, MagQSHiddenFlag),

        /* The Derivative Layout */
        owl_has(DerivativeURI, HasXPositionURI, literal(DerivativeX)),
        owl_has(DerivativeURI, HasYPositionURI, literal(DerivativeY)),
        new(DerivativePoint, point(DerivativeX, DerivativeY)),
        owl_has(DerivativeURI, IsHiddenURI, literal(DerivativeHidden)),
        boolean_to_flag(DerivativeHidden, DerivativeHiddenFlag),

        /* The Derivative Quantity Space Layout */
        owl_has(DerQuantitySpaceURI, HasXPositionURI, literal(DerQSX)),
        owl_has(DerQuantitySpaceURI, HasYPositionURI, literal(DerQSY)),
        new(DerQSPoint, point(DerQSX, DerQSY)),
        owl_has(DerQuantitySpaceURI, IsHiddenURI, literal(DerQSHidden)),
        boolean_to_flag(DerQSHidden, DerQSHiddenFlag), 
      
        /* Update the layout info */
        /* FIX ME */
        %get(MI?identifierHash, member, Aggregate, AggregateURI),
        %get(MI, findElementDefURIInIdentifierHash, Aggregate, AggregateURI), 
        findMainMFAndRoute2(MI, ElementURI, MainMF, RouteChain),
        
        send(MainMF, layOutInfo, Quantity, qsElementRelPosition, MagQSPoint, RouteChain),
        send(MainMF, layOutInfo, Quantity, qsElementHidden, MagQSHiddenFlag, RouteChain),
        send(MainMF, layOutInfo, Quantity, dplusRelPosition, DerivativePoint, RouteChain),
        send(MainMF, layOutInfo, Quantity, dplusHidden, DerivativeHiddenFlag, RouteChain),
        send(MainMF, layOutInfo, Quantity, derivativeElementRelPosition, DerQSPoint, RouteChain),
        send(Aggregate, layOutInfo, Quantity, derivativeElementHidden, DerQSHiddenFlag, RouteChain).


%AttachExtraQuantityURIs(MI, Aggregate, Quantity

        

importAttribute(MI, ElementURI, Aggregate) :->
        /* get the remark */
	get(MI, get_current_comment, ElementURI, Remarks),

        get(MI, add_correct_namespace, 'hasAttribute', HasAttributeURI),
        get(MI, add_correct_namespace, 'hasAttributeValue', HasAttributeValueURI),

        % The originating entity or agent
        owl_has(AttributeSourceURI, HasAttributeURI, ElementURI),
        findInstanceInIdentifierHash(MI, AttributeSourceURI, AttributeSource, AttributeSourceRoute),
        
        % The value of the attribute
        owl_has(ElementURI, HasAttributeValueURI, AttributeValueURI),
	get(MI, get_current_name, AttributeValueURI, AttributeValue),
        new(Value, valueReference(AttributeValue, state)),
        % The defining attribute
        owl_has(ElementURI, rdf:type, AttributeDefURI),
        get(MI, findElementDefInIdentifierHash, AttributeDefURI, AttributeDef),        

        /* Is the ElementURI a condition or a consequence? */
        get(MI, conditionOrConsequence, ElementURI, ConditionOrConsequence),

	addAttributeInstance(@model, Aggregate, AttributeDef, Value, AttributeSource, AttributeSourceRoute,
		    Remarks, ConditionOrConsequence, @nil),
	/*
        @model->>changeRequest(newAttribute,
                Aggregate, %D?modelFragment,
                @nil, %D?editor,
                AttributeSource, %D?instance,
                AttributeSourceRoute, %D?instanceRoute,
                ConditionOrConsequence, %D?state_member?selection,
                AttributeDef, %D?selectedAttributeDef,
                Value, %D?selectedValue,
                Remarks %D?remarks_member?contents
        ),
	*/

        /* Store the configuration */
        findElementInAggregate(MI, ElementURI, Aggregate, Attribute),
        send(MI, storeInstanceInIdentifierHash, Attribute, ElementURI),

	% Add the translations (multiple model support)
	send(MI, import_remarks_languages, ElementURI, Attribute),

        /* Update the layout information */
        send(MI, updateLayout, Aggregate, Attribute, ElementURI).
        %format('Importing ~w\n', [Label]).

importConfiguration(MI, ElementURI, Aggregate) :->
        /* get the remark */
	get(MI, get_current_comment, ElementURI, Remarks),

        /* Get the configuration definition element */
        owl_has(ElementURI, rdf:type, ParentURI),
        get(MI, findElementDefInIdentifierHash, ParentURI, Parent), 
       
        /* Get the URI's of the argument elements */
        get(MI, add_correct_namespace, 'hasConfiguration', HasConfiguration),
        get(MI, add_correct_namespace, 'hasConfigurationTarget', HasConfigurationTarget),
        owl_has(Argument1URI, HasConfiguration, ElementURI),
        owl_has(ElementURI, HasConfigurationTarget, Argument2URI),

        /* Get the argument elements */
        findInstanceInIdentifierHash(MI, Argument1URI, Argument1, Arg1RouteChain),
        findInstanceInIdentifierHash(MI, Argument2URI, Argument2, Arg2RouteChain),
    
        /* Is the ElementURI a condition or a consequence? */
        get(MI, conditionOrConsequence, ElementURI, ConditionOrConsequence),

        /* Call the change request */
	addConfigurationInstance(@model, Aggregate, Parent, Argument1, Arg1RouteChain, Argument2, Arg2RouteChain,
	    Remarks, ConditionOrConsequence, @nil),
	/*
        send(@model, changeRequest,
                newConfiguration,
                Aggregate, %D?modelFragment,
                @nil, %D?editor,
                ConditionOrConsequence, %D?state_member?selection,
                Parent, %D?selectedDef,
                Argument1, Arg1RouteChain, Argument2, Arg2RouteChain, %D?arg1,D?arg1Route,D?arg2,D?arg2Route,
                Remarks %D?remarks_member?contents
        ),
	*/

        /* Store the configuration */
        findElementInAggregate(MI, ElementURI, Aggregate, Configuration),
        send(MI, storeInstanceInIdentifierHash, Configuration, ElementURI),

	% Add the translations (multiple model support)
	send(MI, import_remarks_languages, ElementURI, Configuration),

        /* Update the layout information */
        send(MI, updateLayout, Aggregate, Configuration, ElementURI).
        
        %format('Importing ~w\n', [Label]).

importCausalDependency(MI, ElementURI, Aggregate) :->
        /* get the remark */
	get(MI, get_current_comment, ElementURI, Remarks),
 
        get(MI, add_correct_namespace, 'PositiveProportionality', PosProportionalityURI),
        get(MI, add_correct_namespace, 'NegativeProportionality', NegProportionalityURI),
        get(MI, add_correct_namespace, 'PositiveInfluence', PosInfluenceURI),
        get(MI, add_correct_namespace, 'NegativeInfluence', NegInfluenceURI),
        get(MI, add_correct_namespace, 'hasCausalDependency', HasCausalDependencyURI),
        get(MI, add_correct_namespace, 'hasCausalDependencyTarget', HasCausalDependencyTargetURI),
        
        owl_has(ElementURI, rdf:type, CausalDependencyTypeURI),
        (
                CausalDependencyTypeURI = PosProportionalityURI ->
                Type = 'prop', Sign = 'plus'
        ;
                CausalDependencyTypeURI = NegProportionalityURI ->
                Type = 'prop', Sign = 'min'
        ;
                CausalDependencyTypeURI = PosInfluenceURI ->
                Type = 'inf', Sign = 'plus'
        ;
                CausalDependencyTypeURI = NegInfluenceURI ->
                Type = 'inf', Sign = 'min'
        ),

	owl_has(Argument1URI, HasCausalDependencyURI, ElementURI),
	owl_has(ElementURI, HasCausalDependencyTargetURI, Argument2URI),

        findInstanceInIdentifierHash(MI, Argument1URI, Argument1, Argument1Route),        
        findInstanceInIdentifierHash(MI, Argument2URI, Argument2, Argument2Route),         
	
	addCausalDependency(@model, Aggregate, Type, Sign, 
	    Argument1, Argument1Route, Argument2, Argument2Route, Remarks, @nil),
	/*
	@model->>changeRequest(newQuantityRelation,
		Aggregate, %D?modelFragment,
		@nil, %D?editor,
		Type, %D?quantityRelationType,
                Argument1, Argument1Route, %D?arg1, D?arg1Route,
                Argument2, Argument2Route, %D?arg2, D?arg2Route,
		Sign, % D?sign_member?selection,
		Remarks %D?remarks_member?contents
        ),
	*/

        /* Find and store the Causal Dependency */
        get(Aggregate?elements, tail, Element),
        send(MI, storeInstanceInIdentifierHash, Element, ElementURI),

	% Add the translations (multiple model support)
	send(MI, import_remarks_languages, ElementURI, Element),

        /* Set the correct layout */
        send(MI, updateLayout, Aggregate, Element, ElementURI).

importHasValue(MI, ElementURI, Aggregate) :->
	/* Condition Or Consequence */
	get(MI, conditionOrConsequence, ElementURI, ConditionOrConsequence),

	/* The value the hasValue is pointing to */
	get(MI, add_correct_namespace, 'hasValueTarget', HasValueTarget),
	get(MI, add_correct_namespace, 'containsQualitativeValue', ContainsQVURI),
	get(MI, add_correct_namespace, 'hasQuantitySpace', HasQuantitySpaceURI),
	get(MI, add_correct_namespace, 'hasMagnitude', HasMagnitudeURI),
	get(MI, add_correct_namespace, 'hasDerivative', HasDerivativeURI),

	owl_has(ElementURI, HasValueTarget, QualitativeValueURI),
	owl_has(QuantitySpaceURI, ContainsQVURI, QualitativeValueURI),
	owl_has(MagOrDer, HasQuantitySpaceURI, QuantitySpaceURI),
	(
	    owl_has(QuantityURI, HasMagnitudeURI, MagOrDer)
	;
	    owl_has(QuantityURI, HasDerivativeURI, MagOrDer)
	),
	findInstanceInIdentifierHash(MI, QuantityURI, Quantity, Route), 

	/* Should it be @on or @off depending on Magnitude or Derivative? */
        (
                send(MI, belongsToMagnitude, QualitativeValueURI) ->
	        OnDerivative = @off
        ;
                OnDerivative = @on
        ),

	/* The reference to the correct value */
	get(MI, get_current_name, QualitativeValueURI, ValueLabel),
        (
                OnDerivative == @off ->
        	get(Quantity, quantitySpace, QuantitySpace)
        ;
                OnDerivative == @on,
                get(@model, dqs, QuantitySpace)
        ),
        get(QuantitySpace, values, ValuesChain),	
        get(ValuesChain, find, @arg1?valueName == ValueLabel, ValueReferenceCopy),


	/* Add the value */
	addValueInstance(@model, Aggregate, ValueReferenceCopy, Quantity, Route, OnDerivative, ConditionOrConsequence, @nil).
	/*
	@model->>changeRequest(setValue,
		    Aggregate, %VE?fragment,
		    @nil, % VE,
		    Quantity, % S?fragmentElement,
		    Route, %S?route, %route staat ook in de subelementen
		    OnDerivative, %D,
		    ConditionOrConsequence, % consequence,
		    ValueReferenceCopy % S?valueRef?copy
	).
	*/

belongsToMagnitude(MI, ValueURI) :->
	get(MI, add_correct_namespace, 'containsQualitativeValue', ContainsQVURI),
	get(MI, add_correct_namespace, 'hasQuantitySpace', HasQuantitySpaceURI),
	get(MI, add_correct_namespace, 'Magnitude', MagnitudeURI),
	%get(MI, add_correct_namespace, 'hasDerivative', HasDerivativeURI),

        owl_has(QuantitySpaceURI, ContainsQVURI, ValueURI),
        owl_has(MagOrDerURI, HasQuantitySpaceURI, QuantitySpaceURI),
        owl_has(MagOrDerURI, rdf:type, MagnitudeURI).
        

    
importCorrespondence(MI, ElementURI, Aggregate) :->
        /* get the remark */
	get(MI, get_current_comment, ElementURI, Remarks),
        
	/* Condition Or Consequence? (can only be a consequence) */
	%get(MI, conditionOrConsequence, ElementURI, ConditionOrConsequence),
	
        /* Directed or Undirected? */
	get(MI, add_correct_namespace, 'isDirected', IsDirectedURI),
	owl_has(ElementURI, IsDirectedURI, literal(IsDirectedBoolean)),
        boolean_to_flag(IsDirectedBoolean, IsDirectedFlag),

	/* Inversed? */
	get(MI, add_correct_namespace, 'isInverted', IsInvertedURI),
	owl_has(ElementURI, IsInvertedURI, literal(IsInvertedBoolean)),
	boolean_to_flag(IsInvertedBoolean, IsInvertedFlag),

	/* TODO: Full correspondence */
	IsFullFlag = @off,

	/* The arguments */
	get(MI, add_correct_namespace, 'hasCorrespondence', HasCorrespondenceURI),
        get(MI, add_correct_namespace, 'hasCorrespondenceTarget', HasCorrespondenceTargetURI),

	owl_has(Argument1URI, HasCorrespondenceURI, ElementURI),
	owl_has(ElementURI, HasCorrespondenceTargetURI, Argument2URI),

	inequalityArgument(MI, Aggregate, Argument1URI, Argument1, Argument1Route, _Argument1Type, Argument1QSPoint),
	inequalityArgument(MI, Aggregate, Argument2URI, Argument2, Argument2Route, _Argument2Type, Argument2QSPoint),

	/* On Derivative? */
	(
    	    send(MI, isDerivativeElement, Argument1URI),
	    send(MI, isDerivativeElement, Argument2URI) ->
	    OnDerivativeFlag = @on
	;
	    OnDerivativeFlag = @off
	),

        
        /* DEBUG 
        chain_list(Argument1Route, Argument1RouteList),
        chain_list(Argument2Route, Argument2RouteList),
        ARG1CLASS = Argument1<<-class_name,
        ARG2CLASS = Argument2<<-class_name,
        ARG1NAME = Argument1<<-name,
        ARG2NAME = Argument2<<-name,
        QSPoint1C = Argument1QSPoint<<-class_name,
        QSPoint2C = Argument2QSPoint<<-class_name,
     
        format('Importing correspondence ~w\n', [ElementURI]),
        format('Argument ~w ~w/~w ~w/~w met route is ~w\n', [ARG1NAME, Argument1, ARG1CLASS, Argument1QSPoint, QSPoint1C, Argument1RouteList]),
        format('Argument ~w ~w/~w ~w/~w met route is ~w\n', [ARG2NAME, Argument2, ARG2CLASS, Argument2QSPoint, QSPoint2C, Argument2RouteList]),
        END DEBUG */
     
	addCorrespondenceInstance(@model, Aggregate, 
	    IsDirectedFlag, OnDerivativeFlag, IsInvertedFlag, IsFullFlag,
	    Argument1, Argument1Route, Argument1QSPoint,
	    Argument2, Argument2Route, Argument2QSPoint,
	    Remarks, @nil),

	/*
        @model->>changeRequest(newCorrespondence,
                Aggregate, % D?modelFragment,
                @nil, % D?editor,
                IsDirectedFlag, %?(D?directedMenu_member,selected,directed),
                OnDerivativeFlag, %D?derivative, %2.1: derivative?
                IsInvertedFlag, %?(D?mirrorMenu_member,selected,mirror), %2.1
                IsFullFlag, %D?full, %gp3 0.3
                Argument1, Argument1Route, Argument1QSPoint, %D?arg1,D?arg1Route,D?arg1Value,
                Argument2, Argument2Route, Argument2QSPoint, %D?arg2,D?arg2Route,D?arg2Value,
                Remarks % D?remarks_member?contents
        ),
	*/

	/* Find the inequality and store it */
	% The last added element is the correspondence
        get(Aggregate?elements, tail, Element),
        send(MI, storeInstanceInIdentifierHash, Element, ElementURI),

	% Add the translations (multiple model support)
	send(MI, import_remarks_languages, ElementURI, Element),

	/* Set the inequality layout information */
	send(MI, updateLayout, Aggregate, Element, ElementURI).

	%format('Importing ~w\n', [Label]).

importOperator(MI, ElementURI, Aggregate) :->
    (
        /* get the label and comment */
	get(MI, get_current_name, ElementURI, Label),
	get(MI, get_current_comment, ElementURI, Remarks),

	% A plus or a minus 
	(
	    Label == 'Plus' ->
	    Sign = 'plus'
	;
	    Label == 'Minus' ->
	    Sign = 'min'
	),

	/* The arguments */
	get(MI, add_correct_namespace, 'hasLeftHandSide', HasLeftHandSideURI),
        get(MI, add_correct_namespace, 'hasRightHandSide', HasRightHandSideURI),
	owl_has(ElementURI, HasLeftHandSideURI, Argument1URI),
	owl_has(ElementURI, HasRightHandSideURI, Argument2URI),
	%format('A calculus between ~w and ~w\n', [Argument1URI, Argument2URI]),

	inequalityArgument(MI, Aggregate, Argument1URI, Argument1, Argument1Route, Argument1Type, Argument1QSPoint),
	inequalityArgument(MI, Aggregate, Argument2URI, Argument2, Argument2Route, _Argument2Type, Argument2QSPoint) ->

	% Calculus between quantity or derivative?
        (
                member(Argument1Type, ['currentValue', 'quantityQSPoint', 'calculus']) ->
                Type = 'quantity'
        ;
                Type = 'derivative'
        ),

	addCalculusInstance(@model, Aggregate, Sign, Type, 
	    Argument1, Argument1Route, Argument1QSPoint, Argument2, Argument2Route, Argument2QSPoint,
	    Remarks, @nil),
	/*
	@model->>changeRequest(newCalculus,
	    Aggregate, %D?modelFragment,
	    @nil, % D?editor,
	    Sign, %D?sign_member?selection,
	    Type, %D?type,
	    Argument1, Argument1Route, Argument1QSPoint, %D?arg1,D?arg1Route,D?arg1QSPoint,
	    Argument2, Argument2Route, Argument2QSPoint, %D?arg2,D?arg2Route,D?arg2QSPoint,
	    Remarks % D?remarks_member?contents
	),
	*/
	/* Find the Operator and store it */
	% The last added element is the correspondence
        get(Aggregate?elements, tail, Element),
        send(MI, storeInstanceInIdentifierHash, Element, ElementURI),

	% Add the translations (multiple model support)
	send(MI, import_remarks_languages, ElementURI, Element),
        
	/* Set the inequality layout information */
	send(MI, updateLayout, Aggregate, Element, ElementURI)
    ;
	send(MI?calculiToDoChain, append, ElementURI)
    ).


importMissingOperator(MI, Aggregate) :->
    (
	send(MI?calculiToDoChain, empty)
    ;
	send(MI?calculiToDoChain, for_all,
	    and(
		message(MI?calculiToDoChain, delete, @arg1),
		if(
		    message(MI, importOperator, @arg1, Aggregate),
		    message(@prolog, true),
		    message(@prolog, true)
		)
	    )
	),
	send(MI, importMissingOperator, Aggregate)
    ).

importInequality(MI, ElementURI, Aggregate) :->
        /* get the remark */
	get(MI, get_current_comment, ElementURI, Remarks),

	/* Condition Or Consequence? */
	get(MI, conditionOrConsequence, ElementURI, ConditionOrConsequence),
	
	/* The type of inequality */
	owl_has(ElementURI, rdf:type, InequalityTypeURI),

	get(MI, get_current_name, InequalityTypeURI, InequalityType),
	(
	    InequalityType = 'Smaller Than' ->
	    GarpInequalityType = l
	;
	    InequalityType == 'Smaller Or Equal To' ->
	    GarpInequalityType = leq
	;
	    InequalityType == 'Equal To' ->
	    GarpInequalityType = eq
	;
	    InequalityType == 'Greater Or Equal To' ->
	    GarpInequalityType = geq
	;
	    InequalityType == 'Greater Than' ->
	    GarpInequalityType = g
	),

	/* The arguments */
	get(MI, add_correct_namespace, 'hasInequality', HasInequality),
        get(MI, add_correct_namespace, 'hasInequalityTarget', HasInequalityTarget),

	owl_has(Argument1URI, HasInequality, ElementURI),
	owl_has(ElementURI, HasInequalityTarget, Argument2URI),
       
	inequalityArgument(MI, Aggregate, Argument1URI, Argument1, Argument1Route, Argument1Type, Argument1QSPoint),
	inequalityArgument(MI, Aggregate, Argument2URI, Argument2, Argument2Route, Argument2Type, Argument2QSPoint),

	addInequalityInstance(@model, Aggregate, GarpInequalityType, Remarks, ConditionOrConsequence, 
	    Argument1, Argument1Route, Argument1Type, Argument1QSPoint,
	    Argument2, Argument2Route, Argument2Type, Argument2QSPoint, @nil),

	/* Find the inequality and store it */
	% The last added element is the inequality
        get(Aggregate?elements, tail, Element),
	% send(MI?identifierHash, append, Element, ElementURI), % REPLACED
        send(MI, storeInstanceInIdentifierHash, Element, ElementURI),

	% Add the translations (multiple model support)
	send(MI, import_remarks_languages, ElementURI, Element),

	/* Set the inequality layout information */
	send(MI, updateLayout, Aggregate, Element, ElementURI).

inequalityArgument(MI, _Aggregate, ArgumentURI, Argument, ArgumentRoute, ArgumentType, ArgumentQSPoint) :-
	/* Dermine the type of the argument */
	get(MI, add_correct_namespace, 'QuantitySpace', QuantitySpaceURI),
	get(MI, add_correct_namespace, 'Magnitude', MagnitudeURI),
	get(MI, add_correct_namespace, 'Derivative', DerivativeURI),
	get(MI, add_correct_namespace, 'Operator', OperatorURI),
	get(MI, add_correct_namespace, 'containsQualitativeValue', ContainsQVURI),
	get(MI, add_correct_namespace, 'hasQuantitySpace', HasQuantitySpaceURI),
	get(MI, add_correct_namespace, 'hasMagnitude', HasMagnitudeURI),
	get(MI, add_correct_namespace, 'hasDerivative', HasDerivativeURI),

	/* Find the correct route */
        /* FIX ME */
	%get(MI?identifierHash, member, Aggregate, AggregateURI),
        %get(MI, findURIInIdentifierHash, Aggregate, _Route, _MainMF, AggregateURI), 
        %findRouteInGarp(Argument, Aggregate, ArgumentRoute, _MainMF),
	%findMainMFAndRoute(MI, AggregateURI, MI?identifierHash, _MainMF, ArgumentRoute), 

	/* Find the rest of the necessary information */

	(
            owl_has(ArgumentURI, rdf:type, QuantitySpaceParentURI),
            owl_has(QuantitySpaceParentURI, rdfs:subClassOf, QuantitySpaceURI) ->
            ArgumentType = 'I SHOULD NOT BE IMPORTANT',
            owl_has(MagOrDerURI, HasQuantitySpaceURI, ArgumentURI),
            (
                owl_has(QuantityURI, HasMagnitudeURI, MagOrDerURI)
            ;
                owl_has(QuantityURI, HasDerivativeURI, MagOrDerURI)
            ),
	    findInstanceInIdentifierHash(MI, QuantityURI, Quantity, ArgumentRoute),
	    Argument = Quantity,
	    ArgumentQSPoint = @nil
        ;
	    owl_has(ArgumentURI, rdf:type, MagnitudeURI) ->
	    ArgumentType = 'currentValue',
	    owl_has(QuantityURI, HasMagnitudeURI, ArgumentURI),
	    findInstanceInIdentifierHash(MI, QuantityURI, Argument, ArgumentRoute),
	    ArgumentQSPoint = @nil
	;
	    owl_has(ArgumentURI, rdf:type, DerivativeURI) ->
	    ArgumentType = 'currentDerivative',
	    owl_has(QuantityURI, HasDerivativeURI, ArgumentURI),
	    findInstanceInIdentifierHash(MI, QuantityURI, Argument, ArgumentRoute),
	    ArgumentQSPoint = @nil
	;
            owl:j_instance_of(ArgumentURI, OperatorURI) ->
	    ArgumentType = 'calculus',
	    findInstanceInIdentifierHash(MI, ArgumentURI, Argument, ArgumentRoute),
	    ArgumentQSPoint = @nil
	;
	    owl_has(QuantitySpaceIURI, ContainsQVURI, ArgumentURI),
	    owl_has(MagnitudeInstance, HasQuantitySpaceURI, QuantitySpaceIURI),
	    owl_has(MagnitudeInstance, rdf:type, MagnitudeURI) ->
	    ArgumentType = 'quantityQSPoint',
	    owl_has(QuantitySpaceIURI, rdf:type, QuantitySpaceDefURI),
            owl_has(QuantitySpaceDefURI, rdfs:subClassOf, QuantitySpaceURI),
            owl_has(QuantityIURI, HasMagnitudeURI, MagnitudeInstance),
	    findInstanceInIdentifierHash(MI, QuantityIURI, Argument, ArgumentRoute),
	    get(MI, get_current_name, ArgumentURI, ValueLabel),
	    %owl_has(ArgumentURI, rdfs:label, literal(ValueLabel)),
	    %new(ArgumentQSPoint, valueReference(ValueLabel, point))
            get(Argument, quantitySpace, QuantitySpace),
            get(QuantitySpace, values, Values),
            get(Values, find, @arg1?valueName == ValueLabel, ArgumentQSPoint)
	;
	    % An inequality involving the zero of a derivative
	    owl_has(QuantitySpaceIURI, ContainsQVURI, ArgumentURI),
	    owl_has(DerivativeInstance, HasQuantitySpaceURI, QuantitySpaceIURI),
	    owl_has(DerivativeInstance, rdf:type, DerivativeURI) ->
	    ArgumentType = 'derivativeZero',
            owl:j_instance_of(QuantitySpaceIURI, QuantitySpaceURI),
	    owl_has(QuantityIURI, HasDerivativeURI, DerivativeInstance), 
	    findInstanceInIdentifierHash(MI, QuantityIURI, Argument, ArgumentRoute),
	    ArgumentQSPoint = @nil
	).

importIdentity(MI, ElementURI, Aggregate) :->
        /* get the remark */
	get(MI, get_current_comment, ElementURI, Remarks),
        
        /* Get the URI's of the argument elements */
        get(MI, add_correct_namespace, 'hasIdentity', HasIdentityURI),
        get(MI, add_correct_namespace, 'hasIdentityTarget', HasIdentityTargetURI),
        owl_has(Argument1URI, HasIdentityURI, ElementURI),
        owl_has(ElementURI, HasIdentityTargetURI, Argument2URI),

        /* Get the argument elements */
        findInstanceInIdentifierHash(MI, Argument1URI, Argument1, Arg1RouteChain),
        findInstanceInIdentifierHash(MI, Argument2URI, Argument2, Arg2RouteChain),

        /* DEBUG */
        %chain_list(Arg1RouteChain, Arg1Route),
        %chain_list(Arg2RouteChain, Arg2Route),
        %format('~w met Route 1: ', [Argument1URI]),
        %forall(member(Route1E, Arg1Route), (EC1 = Route1E<<-class_name, format(' ~w/~w ', [Route1E, EC1]))),
        %format('\n~w met Route 2: ', [Argument2URI]),
        %forall(member(Route2E, Arg2Route), (EC2 = Route2E<<-class_name, format(' ~w/~w ', [Route2E, EC2]))),
        %format('\n'),
        /* END DEBUG */

	GiveWarning = @off,

	addIdentity(@model, Aggregate, Argument1, Arg1RouteChain, Argument2, Arg2RouteChain, Remarks, GiveWarning, @nil),
	/*
        @model->>changeRequest(newIdentity,
                                Aggregate, % VE?fragment,
                                @nil, % VE,
                                Argument1, %First?fragmentElement,
                                Arg1RouteChain, %First?route,
                                Argument2, % Sec?fragmentElement,
                                Arg2RouteChain, %Sec?route,
                                Remarks, %new(string)
				GiveWarning
        ),
	*/

	/* Find the inequality and store it */
	% The last added element is the identity
        get(Aggregate?elements, tail, Element),
        send(MI, storeInstanceInIdentifierHash, Element, ElementURI),

	% Add the translations (multiple model support)
	send(MI, import_remarks_languages, ElementURI, Element),
        
	/* Set the inequality layout information */
	send(MI, updateLayout, Aggregate, Element, ElementURI).
        
updateLayout(MI, Aggregate, _Element, ElementURI) :->
         /* Correct the layout information of the Element */
        %get(MI, add_correct_namespace, 'hasCondition', HasConditionURI),
        %get(MI, add_correct_namespace, 'hasConsequence', HasConsequenceURI),
        get(MI, add_correct_namespace, 'has_xposition_on_screen', HasXPositionURI),
        get(MI, add_correct_namespace, 'has_yposition_on_screen', HasYPositionURI),
        get(MI, add_correct_namespace, 'is_hidden_on_screen', IsHiddenURI),
        
        owl_has(ElementURI, HasXPositionURI, literal(X)),
        owl_has(ElementURI, HasYPositionURI, literal(Y)),
        owl_has(ElementURI, IsHiddenURI, literal(Hidden)),
        
        /* create a position (of type point) with X and Y */
        new(Point, point(X, Y)),

        /* Convert the boolean to a flag */
        boolean_to_flag(Hidden, HiddenFlag),
        
        /* Get the aggregate the element is defined in and the correct route */
        %(
        %        owl_has(AggregateURI, HasConditionURI, ElementURI)
        %;
        %        owl_has(AggregateURI, HasConsequenceURI, ElementURI)
        %),
	findInstanceInIdentifierHash(MI, ElementURI, Element, RouteChain),
	/* DEBUG 
	chain_list(RouteChain, RouteList),
	format('De route is: '),
	forall(
	    member(RouteElement, RouteList),
	    (
		get(RouteElement, class_name, ClassName),
		get(RouteElement?referencedFragment, name, FragmentName),
		format(' ~w/~w-~w ', [RouteElement, ClassName, FragmentName]) 
	    )
	), nl,
	END DEBUG */
	
        %findMainMFAndRoute(MI, AggregateURI, MI?identifierHash, MainMF, RouteChain),
        
        /* Call the model fragment to change the position */
        send(Aggregate, layOutInfo, Element, relPosition, Point, RouteChain),
        send(Aggregate, layOutInfo, Element, hidden, HiddenFlag, RouteChain).
        %format('Set the layout for element ~w\n', [ElementURI]).
        %send(MI, printLayout, MainMF, Element, RouteChain).

printLayout(_MI, MainMF, Element, RouteChain) :->
        get(MainMF, layOutInfo, Element, relPosition, RouteChain, Point),
        get(MainMF, layOutInfo, Element, hidden, RouteChain, HiddenFlag),
        get(Point, x, X), get(Point, y, Y),
        format('De positie van het element is (~w,~w)-~w\n', [X,Y, HiddenFlag]).

        
findElementInAggregate(MI, ElementURI, Aggregate, Element) :-
        /* The elements have to be reversed, because some of the elements * 
         * have the same name, and you want to change the last one added. */
        %format('Searching for ~w\n', [ElementURI]),
 
        new(ReverseElements, chain),
        send(Aggregate?elements, for_all,
                message(ReverseElements, prepend, @arg1)
        ),

        get(MI, findGarpType, ElementURI, TypeName),
       
        (
                /* If the element is an garpInstance search for something with the same name */
                /* TODO: Attributes should belong in here too */              
                send(MI, namedGarpType, TypeName) ->
		get(MI, get_current_name, ElementURI, Label),
        	get(ReverseElements, find_all, 
                        if(
                                /* TODO: Attributes should belong in here too */
                                message(MI, namedGarpType, @arg1?class_name),
                                ?(@arg1,name) == Label,
                                message(@prolog, fail)
                        ),
                 Elements1Chain)

        ;
                (TypeName == 'importedFragment'; TypeName == 'fragmentRefiner') ->
		get(MI, get_current_name, ElementURI, Label),
        	get(ReverseElements, find_all, 
                        if(
                                or(
				    message(@prolog, ==, @arg1?class_name, 'importedFragment'),
				    message(@prolog, ==, @arg1?class_name, 'fragmentRefiner')
				),
                                ?(@arg1?referencedFragment,name) == Label,
                                message(@prolog, fail)
                        ),
                 Elements1Chain),
	         get(Aggregate?parentReferences, find_all, ?(@arg1?referencedFragment,name) == Label, Elements2Chain),
                 send(Elements1Chain, union, Elements2Chain)
                
        ;
                /* Else, search something of the same type, and check if it is correct elsewhere */
        	get(ReverseElements, find_all, ?(@arg1,class_name) == TypeName, Elements1Chain)
	        %get(Aggregate?parentReferences, find_all, @arg1?class_name == TypeName, Elements2Chain),
                %send(Elements1Chain, union, Elements2Chain)
        ),
        chain_list(Elements1Chain, Elements), !, 
        member(Element, Elements).

namedGarpType(_MI, GarpType) :->
        member(GarpType, ['garpInstance', 'garpQuantity']).
                

findGarpType(MI, ElementURI, GarpType) :<-
        TypeGarpTypeList = [ 
                ['Entity', 'garpInstance'],
                ['Agent', 'garpInstance'],
                ['ModelFragment', 'importedFragment'],
                ['Assumption', 'assumptionInstance'],
                ['Quantity', 'garpQuantity'],
                ['Magnitude', ''],
                ['Derivative', ''],
                ['QuantitySpace', ''],
                ['QualitativeValue', ''],
                ['Attribute', 'garpAttribute'],
                ['Configuration', 'configuration'],
                ['CausalDependency', 'garpQuantityRelation'],
                ['hasValue', 'value'],
                ['Correspondence', 'correspondence'],
                ['Operator', 'calculus'],
                ['Inequality', 'inequality'],
                ['Identity', 'identityRelation']
        ],
        get(MI, getType, ElementURI, Type),
        member([Type, GarpType], TypeGarpTypeList).
          
                
        

        

getAggregateURIFromElementURI(MI, ElementURI, AggregateURI) :<-
        get(MI, add_correct_namespace, 'hasCondition', HasConditionURI),
        get(MI, add_correct_namespace, 'hasConsequence', HasConsequenceURI),
        (
                owl_has(AggregateURI, HasConditionURI, ElementURI)
                ;
                owl_has(AggregateURI, HasConsequenceURI, ElementURI)
        ).


getType(MI, ElementURI, TypeLabel) :<-
	get(MI?vocabularyOntology, value, URIPrefix),
        Types = [ 'Entity',
                  'Agent', 
                  'ModelFragment',
                  'Assumption',
                  'Quantity',
                  'Magnitude',
                  'Derivative',
                  'QuantitySpace',
                  'QualitativeValue',
                  'Attribute',
                  'Configuration',
                  'CausalDependency',
                  'hasValue',
                  'Correspondence',
                  'Operator',
                  'Inequality',
                  'Identity' ],
        member(TypeLabel, Types),
        concat(URIPrefix, TypeLabel, TypeURI), 
        owl:j_instance_of(ElementURI, TypeURI). 




findAndSortAllConditionsAndConsequences(MI, AggregateURI, AddOrRemoveRefinementsBoolean, SortedConditionsAndConsequencesList) :<-
        get(MI, add_correct_namespace, 'hasCondition', HasConditionURI),
        get(MI, add_correct_namespace, 'hasConsequence', HasConsequenceURI),
        findall(ConditionOrConsequence,
                (
                        owl_has(AggregateURI, HasConditionURI, ConditionOrConsequence)
                        ;
                        owl_has(AggregateURI, HasConsequenceURI, ConditionOrConsequence)
                ),
                ConditionsAndConsequences
        ),

	length(ConditionsAndConsequences, CACLength),
	debug(owl(refinements), 'Without refinements ~w: ~w', [CACLength, ConditionsAndConsequences]),
	addOrRemoveRefinements(MI, AggregateURI, AddOrRemoveRefinementsBoolean, ConditionsAndConsequences, ConditionsAndConsequences2),
	length(ConditionsAndConsequences2, CACLength2),
	debug(owl(refinements), 'With refinements ~w: ~w', [CACLength2, ConditionsAndConsequences2]),
        sortConditionsAndConsequences(MI, ConditionsAndConsequences2, SortedConditionsAndConsequencesListAlmost),

    
	/* Make sure that refined imported model fragments are at the head of the list */
	chain_list(SortedChain, SortedConditionsAndConsequencesListAlmost),
	new(Fragments, chain),
	forall(
	    (
		member(COC, SortedConditionsAndConsequencesListAlmost),
		get(MI, getType, COC, 'ModelFragment')
	    ),
	    (
		send(SortedChain, delete, COC),

		(
		    send(MI, isModelFragmentDefinition, AggregateURI) ->
		    get(MI, findElementDefInIdentifierHash, AggregateURI, Aggregate)
		;
		    owl:j_instance_of(AggregateURI, AggregateDefURI),
		    get(MI, findElementDefInIdentifierHash, AggregateDefURI, Aggregate)
		),

		get(MI, get_current_name, COC, COCName),
		(
		    findElementByNameInAggregate(COCName, Aggregate, Element, _Route, _MainMF),
		    get(Element, class_name, 'fragmentRefiner') ->
		    send(Fragments, prepend, COC)
		;
		    send(Fragments, append, COC)
		)
	    )
	),
	send(Fragments, merge, SortedChain),
	chain_list(Fragments, SortedConditionsAndConsequencesList).
	    

addOrRemoveRefinements(MI, AggregateURI, AddOrRemoveRefinementsBoolean, ConditionsAndConsequences, ConditionsAndConsequences2) :-
    %get(MI, add_correct_namespace, 'isRefinedFrom', IsRefinedFromURI),
    (
	AddOrRemoveRefinementsBoolean == @on ->
	chain_list(ConditionsAndConsequencesChain, ConditionsAndConsequences),
	findRefinements(MI, AggregateURI, ConditionsAndConsequencesChain),
	chain_list(ConditionsAndConsequencesChain, ConditionsAndConsequences2)
    ;
	AddOrRemoveRefinementsBoolean == @off ->
	/*
	findall(ConditionOrConsequence,
	    (
		member(ConditionOrConsequence, ConditionsAndConsequences),
		not(owl_has(ConditionOrConsequence, IsRefinedFromURI, _ModelFragmentDef))
	    ),
	    ConditionsAndConsequences2
	)
	*/
	ConditionsAndConsequences2 = ConditionsAndConsequences
    ).

findRefinements(MI, AggregateURI, _ConditionsAndConsequences) :-
    get(MI, add_correct_namespace, 'hasCondition', HasConditionURI),
    not((
	owl_has(AggregateURI, HasConditionURI, ModelFragmentURI),
	get(MI, getType, ModelFragmentURI, 'ModelFragment')
    )).

findRefinements(MI, AggregateURI, ConditionsAndConsequences) :-
    get(MI, add_correct_namespace, 'hasCondition', HasConditionURI),
    get(MI, add_correct_namespace, 'isRefinedFrom', IsRefinedFromURI),
    forall(
	(
	    owl_has(AggregateURI, HasConditionURI, ModelFragmentURI),
	    get(MI, getType, ModelFragmentURI, 'ModelFragment')
	),
	(
	    (
		owl_has(ModelFragmentURI, IsRefinedFromURI, _ModelFragmentDef) ->
		send(ConditionsAndConsequences, append, ModelFragmentURI),
		debug(owl(refinements), 'Found refinement: ~w', [ModelFragmentURI])
	    ;
		true
	    ),
	    findRefinements(MI, ModelFragmentURI, ConditionsAndConsequences)
	)
    ).


sortConditionsAndConsequences(MI, ConditionsAndConsequences, SortedConditionsAndConsequencesSet) :-
	%URIPrefix = 'http://www.science.uva.nl/~jliem/ontologies/qrontologyGARP3.owl#',        
	get(MI?vocabularyOntology, value, URIPrefix),
	get(MI, add_correct_namespace, 'isRefinedFrom', IsRefinedFromURI),
        Order = [ [1, 'ModelFragment'],
		  [2, 'RefinedModelFragment'], % This type does not really exist 
		  [3, 'Entity'],
                  [4, 'Agent'], 
                  [5, 'Assumption'],
                  [6, 'Quantity'],
                  [7, 'Magnitude'],
                  [8, 'Derivative'],
                  [9, 'QuantitySpace'],
                  [10, 'QualitativeValue'],
                  [11,'Attribute'],
                  [12,'Configuration'],
                  [13,'CausalDependency'],
		  [14,'hasValue'],  % JL@20-03-2009: hasValue was commented out for no particular reason? I added it again an incremented the other integers
                  [15,'Correspondence'],
                  [16,'Operator'],
                  [17,'Inequality'],
                  [18,'Identity'] ],

	findall(AugmentedOrderElement,
	    (
		member([Integer, Type], Order),
		atom_concat(URIPrefix, Type, TypeURI),
		AugmentedOrderElement = [Integer, TypeURI]
	    ),
	    AugmentedOrder
	),
	
        findall( RealIndex - (ConditionOrConsequence),
                (
			member(ConditionOrConsequence, ConditionsAndConsequences),
			findall([Index, TypeURI],
			    (
				member([Index, TypeURI], AugmentedOrder),
				%format('Is ~w\n of type ~w\n\n', [ConditionOrConsequence, TypeURI]),
				owl:j_instance_of(ConditionOrConsequence, TypeURI)
			    ),
			    IndexTypeURIs
			),
			nth1(1, IndexTypeURIs, [Index, _TypeURI]),
			(
			    /* If the ModelFragment is refined */
			    Type = 'ModelFragment',
			    owl_has(ConditionOrConsequence, IsRefinedFromURI, _OriginalMF) ->
			    RealIndex is Index + 1
			;
			    RealIndex = Index
			)
                ),
                KeyedConditionsAndConsequences
        ),
        keysort(KeyedConditionsAndConsequences, SortedConditionsAndConsequencesListWithKeys),
        findall(ConditionOrConsequence,
                member(_Key-ConditionOrConsequence, SortedConditionsAndConsequencesListWithKeys),
                SortedConditionsAndConsequencesList),
	list_to_set(SortedConditionsAndConsequencesList, SortedConditionsAndConsequencesSet).
	%format('The results of sorting the conditions and consequences is: ~w\n', [SortedConditionsAndConsequencesSet]).




get_elements_from_list(StartOfList, [Element]) :-
        owl_has(StartOfList, rdf:first, Element),
        owl_has(StartOfList, rdf:rest, rdf:nil).

get_elements_from_list(StartOfList, [ Element | ListElements ]) :-
        owl_has(StartOfList, rdf:first, Element),
        owl_has(StartOfList, rdf:rest, RestOfList),
        get_elements_from_list(RestOfList, ListElements).

conditionOrConsequence(MI, ElementURI, ConditionOrConsequence) :<-
	/* Is it a condition or consequence? */
	get(MI, add_correct_namespace, 'hasCondition', HasCondition),
	get(MI, add_correct_namespace, 'hasConsequence', HasConsequence),
	(
	    owl_has(_AggregateURI1, HasCondition, ElementURI) ->
	    ConditionOrConsequence = 'condition'
	;
	    owl_has(_AggregateURI2, HasConsequence, ElementURI) ->
	    ConditionOrConsequence = 'consequence'
	).

boolean_to_flag(Boolean, Flag) :-
        (
                Boolean = true ->
                Flag = @on
        ;
                Flag = @off
        ).

/* --------- Multiple Language Support --------- */

% Get the current language specified in the owl file
get_current_language_key(MI, LanguageKey) :<-
    get(MI, add_correct_namespace, 'language', LanguageURI),
    get(MI, currentURI, OntologyURI),
    owl_has(OntologyURI, LanguageURI, literal(LanguageKey)).

% Get the name of an element in the current language
get_current_name(MI, ElementURI, CurrentName) :<-
    get(MI, currentLanguage, Language),
    get(@model?translator, getLanguageKey, Language, Key),
    rdf_has(ElementURI, rdfs:'label', literal(lang(Key, CurrentName))).

get_current_comment(MI, ElementURI, CurrentComment) :<-
    get(MI, currentLanguage, Language),
    get(@model?translator, getLanguageKey, Language, Key),
    rdf_has(ElementURI, rdfs:'comment', literal(lang(Key, CurrentComment))).

% Import the names of the element in different languages
import_name_languages(_MI, ElementURI, Element) :->
    get(Element, name_translatable, NameT),
    get(NameT, translator, Translator),

    get(Translator, allLanguages, AllLanguagesC),
    chain_list(AllLanguagesC, AllLanguages),

    % Store the current language
    get(Translator, currentLanguage, CurrentLanguage),

    % For each language in which the source elements is defined
    forall(
	member(Language, AllLanguages),
	(
	    % Translate the language to a language id
	    get(Translator, getLanguageKey, Language, LanguageID),

	    % The name in a specific language
    	    owl_has(ElementURI, rdfs:label, literal(lang(LanguageID, Name))),

	    % Add the translation of the name to the element
    	    send(Translator, setCurrentLanguage, Language),
    	    send(NameT, value, Name)
	)
    ),

    % Set the original language
    send(Translator, setCurrentLanguage, CurrentLanguage).

% Import the remarks of the element in different languages
import_remarks_languages(_MI, ElementURI, Element) :->
    get(Element, remarks_translatable, RemarksT),
    get(RemarksT, translator, Translator),

    get(Translator, allLanguages, AllLanguagesC),
    chain_list(AllLanguagesC, AllLanguages),

    % Store the current language
    get(Translator, currentLanguage, CurrentLanguage),

    % For each language in which the source elements is defined
    forall(
	member(Language, AllLanguages),
	(
	    % Translate the language to a language id
	    get(Translator, getLanguageKey, Language, LanguageID),

	    % The remarks in the specific language
	    owl_has(ElementURI, rdfs:comment, literal(lang(LanguageID, Remarks))),

	    % Add the translation of the remarks to the element
    	    send(Translator, setCurrentLanguage, Language),
	    send(RemarksT, value, Remarks)
	)
    ),

    % Set the original language
    send(Translator, setCurrentLanguage, CurrentLanguage).


import_values_languages(_MI, ValueURI, Value) :->
    get(Value, valueName_translatable, ValueNameT),
    
    % Get the translator of the target values
    get(ValueNameT, translator, Translator),

    % get all the languages
    get(Translator, allLanguages, AllLanguagesC),
    chain_list(AllLanguagesC, AllLanguages),

    % Store the current language
    get(Translator, currentLanguage, CurrentLanguage),
	    
    % For each language set the name of the Value
    forall(
	member(Language, AllLanguages),
	(
	    % Translate the language to a language id
	    get(Translator, getLanguageKey, Language, LanguageID),

	    % The remarks in the specific language
	    owl_has(ValueURI, rdfs:label, literal(lang(LanguageID, ValueName))),

	    % Add the translation of the value to the element
    	    send(Translator, setCurrentLanguage, Language),
	    send(ValueNameT, value, ValueName)
	)
    ),

    % Set the original language
    send(Translator, setCurrentLanguage, CurrentLanguage).


/*
get_all_languages(ElementURI, Languages) :-
    findall(
	Lang, 
	rdf_db:rdf_has('http://www.science.uva.nl/~jliem/ontologies/model.owl#owl_ae_Action', rdfs:label, literal(lang(Lang, _Y))),
	Languages
    ).
*/


:-pce_end_class.
