/* TODO: Make the IdentifierHash and GensymIdentifierChain class fields
 * Then you can exportCalculiToDo */
:- consult('toOWL.pl').
:- consult('owl.pl').
:- consult('exportimport_helpers.pl').
:- consult(library('iso_639.pl')).

:-pce_begin_class(
		  modelExporter,
		  importExport,
		  "Parser which saves a model to an OWL file."
		 ).

%variable(exportPath, string, get, "The path where the OWL file should be put.").
variable(modelFragmentsToDo, chain:=new(chain), both, "The chain of model fragments which still have to be exported").

initialise(ME) :->
    send_super(ME, initialise).
    %ME->>slot(exportPath, Path),
    %format('Ik ga hier een owlfile neerzetten: ~w\n', Path?value).

export(ME, StreamAlias: prolog) :->
    /* Create an OWL pleaseWait screen */
    (
	@app<<-hypered(pleaseWait) ->
	true
    ;
	W *= pleaseWait('Please wait,\nExporting to OWL...'),  %see below in this file
	@app->>hyper(W,pleaseWait)
    ),
    (
	% The current URI of the ontology being exported
	get(@model, currentUniqueID, UniqueID),
	get(ME, generateURI, UniqueID, OntologyURI),
	send(ME, currentURI, OntologyURI),
    	atom_concat(OntologyURI, '#', OntologyURIWithHash),
	format('Test2: ~w\n', [OntologyURIWithHash]),
	% Register the NS (with a # in the end), replace previous ns
	rdf_register_ns(qrm, OntologyURIWithHash, [force(true)]),
	
	/* Load the QR vocabulary ontology */
	rdf_load(ontologies('QRvocabulary.owl')),

	%debug(owl(general), 'Export filename is ~w', FileName?value),
	Modelnaam = @model<<-name,
	create_ontology(OntologyURI, 'http://www.science.uva.nl/~jliem/ontologies/QRvocabulary.owl'),
	create_ontology(OntologyURI, 'http://purl.org/dc/elements/1.1/'),
	debug(owl(general), 'De modelnaam is: ~w', [Modelnaam]), 

	ME->>metadata,
	ME->>saveIDs,

	ME->>simPrefs,
        
	debug(owl(general), 'Exporting Entities', []),
	ME->>entities,
	debug(owl(general), 'Exporting Configurations', []),
	ME->>configurations,
	debug(owl(general), 'Exporting Attributes', []),
	ME->>attributes, % Has OWL syntax bugs
	debug(owl(general), 'Exporting Quantities', []),
	ME->>quantities,
	debug(owl(general), 'Exporting Quantity Spaces', []),
	ME->>quantityspaces,

	debug(owl(general), 'Exporting Aggregates', []),
	ME->>exportAggregates(@model?modelFragments),
    
	/* TODO: Export the elements which still have to exported */
	%ME->>exportCalculiToDo,

	debug(owl(general), 'Processing final changes to OWL formalisation.', []),
	ME->>postExportChanges,

	debug(owl(general), 'Finishing OWL export process.', []),

	ME->>cleanGensym,
	
	/* After exporting the model fragments free the identifier hash */
	free(ME?identifierHash),
	send(ME?refinements, clear),
            
	stream_property(Stream, alias(StreamAlias)),
	rdf_save(stream(Stream), [db(user), encoding(utf8)]),

	rdf_reset_db,
	debug(owl(general), 'Successfully export model ~w', [Modelnaam]),
	send(ME?modelFragmentsDone, clear),
	send(@app, thankYou)
    ;
	send(@app, thankYou),
	rdf_reset_db,
	send(ME?refinements, clear),
	free(ME?identifierHash),
	send(ME?modelFragmentsDone, clear),
	@app?mainMenu->>msgBox(string('OWL Export failed! Save your file as a .hgp file to avoid data loss. Please send this error and your .hgp file the developers.'),alarm), !,
	fail
    ).

metadata(ME) :->
    /* Get the right namespaces */
    get(ME, add_correct_namespace, 'title', TitleURI),
    get(ME, add_correct_namespace, 'creator', CreatorURI),
    get(ME, add_correct_namespace, 'contributor', ContributorURI),
    %get(ME, add_correct_namespace, 'subject', SubjectURI),
    get(ME, add_correct_namespace, 'language', LanguageURI),
    get(ME, add_correct_namespace, 'rights', RightsURI),
    get(ME, add_correct_namespace, 'description', DescriptionURI),
    %get(ME, add_correct_namespace, 'type', TypeURI),
    get(ME, add_correct_namespace, 'bibliographicCitation', BibliographicCitationURI), 
    get(ME, add_correct_namespace, 'audience', AudienceURI),
    get(ME, add_correct_namespace, 'created', CreatedOnURI),
    get(ME, add_correct_namespace, 'hasVersionNumber', HasVersionNumberURI),
    get(ME, add_correct_namespace, 'domain', DomainURI),
    get(ME, add_correct_namespace, 'keyword', KeywordURI),
    get(ME, add_correct_namespace, 'goal', GoalURI),
    get(ME, add_correct_namespace, 'limitation', LimitationURI),
    get(ME, add_correct_namespace, 'email', EmailURI),
    get(ME, add_correct_namespace, 'modified', ModifiedURI),
    get(ME, add_correct_namespace, 'format', FormatURI),  
    get(ME, add_correct_namespace, 'hasOWLVersionNumber', HasOWLVersionNumberURI),  

    get(@model, metaData, MetaDataHash),
    get(MetaDataHash, member, remarks, RemarksD),
    get(MetaDataHash, member, modelData, ModelDataD),
    %get(MetaDataHash, member, status, StatusD),
    %get(MetaDataHash, member, misc, MiscD),
    get(MetaDataHash, member, general, GeneralD),

    %get(MetaDataHash, keys, Keys), chain_list(Keys, KeysList),
    %format('De keys zijn: ~w\n', [KeysList]),

    get(GeneralD, member, title, TitleS), % dc:title
    get(GeneralD, member, author, AuthorS), % dc:creator
    get(GeneralD, member, contributors, ContributorsS), % dc:contributor 
    get(GeneralD, member, contact_email, ContactEmailS),
    get(GeneralD, member, keywords, KeywordsS), % dc:subject
    get(GeneralD, member, domain, DomainS),
    get(GeneralD, member, model_version, ModelVersionS),
    get(GeneralD, member, known_model_limitations, KnownModelLimitationsS),
    get(GeneralD, member, bibliographic_citation, BibliographicCitationS),
    get(GeneralD, member, license, LicenseS), %dc:rights
    get(RemarksD, member, abstract, AbstractS), %dc:description
    get(RemarksD, member, general_remarks, RemarksS), % rdfs:comment
    get(RemarksD, member, model_goals, ModelGoalsS),
    get(RemarksD, member, intended_audience, IntendedAudienceS),

    ( 
	get(ModelDataD, member, creationTime, CreatedOnDate), get(CreatedOnDate, string, CreatedOnS) ; 
	new(CreatedOnS, string('')) 
    ),
    ( 
	get(@model, lastChangeTime, LastChangeDate), get(LastChangeDate, string, LastChangeS) ; 
	new(LastChangeS, string('')) 
    ),
    ( get(ModelDataD, member, creatorProgram, CreatedInS) ; new(CreatedInS, string('')) ),

    % TODO MISSING: created_in,creation_definition_version, NOT NEEDED: current_definition_version, current_program, 

    get(TitleS, value, Title), % dc:title
    get(AuthorS, value, Author), % dc:creator
    get(ContributorsS, value, Contributors), % dc:contributor 
    get(ContactEmailS, value, ContactEmail),
    get(KeywordsS, value, Keywords), % dc:subject
    get(DomainS, value, Domain),
    get(ModelGoalsS, value, ModelGoals),
    get(IntendedAudienceS, value, IntendedAudience),
    get(ModelVersionS, value, ModelVersion),
    get(KnownModelLimitationsS, value, KnownModelLimitations),
    get(BibliographicCitationS, value, BibliographicCitation),
    get(CreatedOnS, value, CreatedOn),
    get(LicenseS, value, License), %dc:rights
    get(AbstractS, value, Abstract), %dc:description
    get(RemarksS, value, Remarks), % rdfs:comment
    get(LastChangeS, value, LastChange),
    get(CreatedInS, value, CreatedIn),
    get(ME?garp3_owl_version_number, value, OWLVersionNumber),

    get(ME, currentURI, OntologyURI), 

    owl_property(OntologyURI, TitleURI, literal(Title)),
    owl_property(OntologyURI, CreatorURI, literal(Author)),
    owl_property(OntologyURI, ContributorURI, literal(Contributors)),
    owl_property(OntologyURI, KeywordURI, literal(Keywords)),
    owl_property(OntologyURI, RightsURI, literal(License)),
    owl_property(OntologyURI, DescriptionURI, literal(Abstract)),
    rdf_assert(  OntologyURI, rdfs:comment, literal(Remarks)),
    owl_property(OntologyURI, BibliographicCitationURI, literal(BibliographicCitation)),
    owl_property(OntologyURI, AudienceURI, literal(IntendedAudience)),
    owl_property(OntologyURI, CreatedOnURI, literal(CreatedOn)),
    owl_property(OntologyURI, HasVersionNumberURI, literal(ModelVersion)),
    owl_property(OntologyURI, DomainURI, literal(Domain)),
    owl_property(OntologyURI, KeywordURI, literal(Keywords)),
    owl_property(OntologyURI, GoalURI, literal(ModelGoals)),
    owl_property(OntologyURI, LimitationURI, literal(KnownModelLimitations)),
    owl_property(OntologyURI, EmailURI, literal(ContactEmail)),
    owl_property(OntologyURI, ModifiedURI, literal(LastChange)),
    owl_property(OntologyURI, FormatURI, literal(CreatedIn)),
    owl_property(OntologyURI, HasOWLVersionNumberURI, literal(OWLVersionNumber)),

    % Set the current active language
    design:getTopEntity(@model,TopEntity),
    get(TopEntity?name_translatable?translator, currentLanguage, CurrentLanguage),
    get(TopEntity?name_translatable?translator, getLanguageKey, CurrentLanguage, LanguageKey),
    owl_property(OntologyURI, LanguageURI, literal(LanguageKey)).
    %owl_property(OntologyURI, TypeURI, literal('Garp3 Qualitative Model')).

saveIDs(ME) :->
    get(@model, uniqueIDs, UniqueIDsChain),
    chain_list(UniqueIDsChain, UniqueIDs),
    % The last ID in the list is the current ID (so not a prior version)
    last(UniqueIDs, CurrentID), select(CurrentID, UniqueIDs, PriorIDs),
    % Export the prior IDs as prior versions
    get(ME, currentURI, CurrentURI),
    forall(
	member(PriorID, PriorIDs),
	(
	    get(ME, generateURI, PriorID, PriorIDURI),
	    rdf_assert(CurrentURI, owl:'priorVersion', PriorIDURI)
	)
    ).

generateURI(_ME, ID, URI) :<-
    www_form_encode(ID, IDEncoded),
    string_concat('http://www.dynalearn.eu/models/', IDEncoded, PartURI),
    string_concat(PartURI, '.owl', URIstring),
    string_to_atom(URIstring, URI).

simPrefs(ME) :->
    get(@model, runPrefs, RunPrefs),
    design:getSimPrefs(SimPrefs),
    forall(
	member(SimPref, SimPrefs),
	(
	    get(RunPrefs, member, SimPref, SimPrefValue),
	    (
		member(SimPrefValue, [@on, @off]) ->
		flag_to_boolean(SimPrefValue, SimPrefConverted)
	    ;
		SimPrefConverted = SimPrefValue
	    ),
	    atom_concat('hasSimulationPref', SimPref, SimPrefFull),
	    get(ME, add_correct_namespace, SimPrefFull, SimPrefURI),
	    get(ME, currentURI, OntologyURI),
	    owl_property(OntologyURI, SimPrefURI, literal(SimPrefConverted))
	)
    ).


entities(ME) :->
    getTopEntity(@model, Entity),
    ME->>abstractEntities(Entity, @nil, @nil, 'Entity'),
    getTopAgent(@model, Agent),
    ME->>abstractEntities(Agent, @nil, @nil, 'Agent'),
    getTopAssumption(@model, Assumption),
    ME->>abstractEntities(Assumption, @nil, @nil, 'Assumption').

attributes(ME) :->
    % Get the list of attributes
    AttributesC = @model?attributeDefinitions,
    chain_list(AttributesC, Attributes),

    % Loop through all attributes and export them
    forall(member(Attribute, Attributes),
	(
	    % Get the Attribute Name and add the prefix
	    get(Attribute, name, AttributeName),
            Prefix = 'owl_a_',
            string_concat(Prefix, AttributeName, AttributeNamePrefix),
	    
	    % Create the attribute in OWL
	    AttributeURI  = ME<<-add_correct_namespace(AttributeNamePrefix),
	    owl_create_class(AttributeURI),

	    % The attribute is a subclass of the Attribute definition
	    AttributeDefURI = ME<<-add_correct_namespace('Attribute'),
	    owl_create_subclass(AttributeURI, AttributeDefURI),

	    % Add the name of the attribute in different languages
	    send(ME, export_name_languages, Attribute, AttributeURI),

	    % Add the comment in different languages
	    send(ME, export_remarks_languages, Attribute, AttributeURI),

	    % Determine disjoints, their URI's, and add to OWL
	    get(AttributesC, map, @arg1?name, AttributeNames),
	    chain_list(AttributeNames, AttributeNameList),
	    delete(AttributeNameList, AttributeName, DisjointsList),
	    findall(DisjointURI,
		(
		    member(Disjoint, DisjointsList),
                    string_concat(Prefix, Disjoint, DisjointPrefix),
		    DisjointURI = ME<<-add_correct_namespace(DisjointPrefix)
		),
		DisjointURIS
	    ),
	    owl_create_disjointclasses(AttributeURI, DisjointURIS),

	    % ----------- Export a general definition for the values ----------- %
	    % Create the Attribute value name
	    string_concat(AttributeName, 'Value', AttributeValueName),
            % Add the value prefix
            AttributeValuePrefix = 'owl_av_',
            string_concat(AttributeValuePrefix, AttributeValueName, AttributeValueNamePrefix),

	    AttributeValueURI = ME<<-add_correct_namespace(AttributeValueNamePrefix),
	    AttributeValueDefURI = ME<<-add_correct_namespace('AttributeValue'),

            %% Add the value definition to OWL
	    owl_create_class(AttributeValueURI, AttributeValueName),
	    owl_create_subclass(AttributeValueURI, AttributeValueDefURI),

	    %% State that the Attribute can only have the AttributeValue as range
	    owl_create_node(Restriction),
	    owl_create_subclass(AttributeURI, Restriction),
	    owl_type(Restriction, 'owl:Restriction'),
	    HasAttributeValue = ME<<-add_correct_namespace('hasAttributeValue'),
	    owl_onProperty(Restriction, HasAttributeValue),
	    owl_allValuesFrom(Restriction, AttributeValueURI),

            % Add the value disjoints  
            findall(DisjointValueURI,
	   	(
	   	    member(DisjointValue, DisjointsList),
	   	    string_concat(DisjointValue, 'Value', DisjointValue1),
                    string_concat(AttributeValuePrefix, DisjointValue1, DisjointValue1Prefix),
	    	    DisjointValueURI = ME<<-add_correct_namespace(DisjointValue1Prefix)
	    	),
	    	DisjointValueURIS
	    ),
	    owl_create_disjointclasses(AttributeValueURI, DisjointValueURIS),

	    % ----------- Export the values of the attribute ----------- %   
	    % Get the values
	    get(Attribute, values, ValuesChain),
	    chain_list(ValuesChain, ValuesListObjects),

	    /* Find all the names */
	    findall(ValueString,
		(
		    member(ValueObject, ValuesListObjects),
		    ValueString = ValueObject?name<<-value % PORT: valueName => name
		),
		ValuesList
	    ),	

	    %% The AttributeValue is OneOf the values
	    findall(ValueURI,
		(
		    member(Value, ValuesList),
                    string_concat(AttributeValuePrefix, Value, ValuePrefix),
		    ValueURI = ME<<-add_correct_namespace(ValuePrefix)
		),
		ValueURIS
	    ),	
	    owl_oneOf(AttributeValueURI, ValueURIS),

	    %% Add the attribute values as individuals
	    forall(
		member(Value, ValuesList),
		(
		    nth1(Index2, ValuesList, Value),
		    nth1(Index2, ValueURIS, ValueURI),
		    owl_create_individual(ValueURI, AttributeValueURI),
		    % Export the value name in different languages Value, 
		    % get the original value
		    nth1(Index2, ValuesListObjects, ValueObject),
		    send(ME, export_values_languages, ValueObject, ValueURI)
		)
	    ),
	    
	    %% State that all individuals are different
	    owl_create_node(AllDifferent),
	    owl_type(AllDifferent, 'owl:AllDifferent'),
	    owl_distinctMembers(AllDifferent, ValueURIS) 
	)
    ).

configurations(ME) :->
    ConfigurationsC = @model?configurationDefinitions,
    chain_list(ConfigurationsC, Configurations),

    % The configuration class
    ConfigurationURI = ME<<-add_correct_namespace('Configuration'),
    forall(
	member(Configuration, Configurations),
	(
	    get(Configuration?name, value, ConfigName),
            % Add a prefix to the configurations
            Prefix = 'owl_c_',
            string_concat(Prefix, ConfigName, ConfigNamePrefix),
            
	    ConfigNameURI = ME<<-add_correct_namespace(ConfigNamePrefix),
	    owl_create_class(ConfigNameURI),
	    
	    % Add the name of the attribute in different languages
	    send(ME, export_name_languages, Configuration, ConfigNameURI),

	    % Add the comment in different languages
	    send(ME, export_remarks_languages, Configuration, ConfigNameURI),

	    % The configuration is a subclass of the general configuration concept
	    owl_create_subclass(ConfigNameURI, ConfigurationURI),

	    % The configuration is disjoint with the other configurations
	    get(ConfigurationsC, map, @arg1?name, ConfigurationNames),
	    chain_list(ConfigurationNames, ConfigurationNamesList),
	    delete(ConfigurationNamesList, ConfigName, ConfigurationDisjoints),
	    findall(ConfigDisjointURI,
		(
		    member(ConfigurationDisjoint, ConfigurationDisjoints),
                    string_concat(Prefix, ConfigurationDisjoint, ConfigurationDisjointPrefix),
		    ConfigDisjointURI = ME<<-add_correct_namespace(ConfigurationDisjointPrefix)
		),
		ConfigurationDisjointsURIS),
	    owl_create_disjointclasses(ConfigNameURI, ConfigurationDisjointsURIS)
	)
    ).
       
quantityspaces(ME) :->
    QuantitySpacesC = @model?quantitySpaces,
    chain_list(QuantitySpacesC, QuantitySpaces),
   
    % Chain of all the values (needed when exporting values)
    AllValuesChain *= chain,
    
    %%% Loop through all quantity spaces and export them %%%
    forall(
	member(QuantitySpace, QuantitySpaces),
	(
	    get(QuantitySpace, name, Name),
	    get(QuantitySpace, values, ValuesChain),
	    	    
	    %%% Export the quantity spaces %%%
            % Add the QS Prefix
            QSPrefix = 'owl_qs_',
            atom_concat(QSPrefix, Name, NamePrefix),

	    % The quantity space name URI and the label
	    NameURI = ME<<-add_correct_namespace(NamePrefix),	
	    owl_create_class(NameURI),

	    % The quantity space is a subclass of Quantity Space
	    QuantitySpaceURI = ME<<-add_correct_namespace('QuantitySpace'),
	    owl_create_subclass(NameURI, QuantitySpaceURI),

	    % Add the name of the quantity space  in different languages
	    send(ME, export_name_languages, QuantitySpace, NameURI),

	    % Add the comment of the quantity space in different languages
	    send(ME, export_remarks_languages, QuantitySpace, NameURI),

	    % The quantity space is disjoint with the other quantity spaces
	    get(QuantitySpacesC, map, @arg1?name, NamesC),
	    chain_list(NamesC, NamesList),
	    delete(NamesList, Name, DisjointList),
	    findall(DisjointURI,
		(
		    member(Disjoint, DisjointList),

                    % Add QS Prefix
                    atom_concat(QSPrefix, Disjoint, DisjointPrefix),
                    
		    DisjointURI = ME<<-add_correct_namespace(DisjointPrefix)
		),
		DisjointURIS),
	    owl_create_disjointclasses(NameURI, DisjointURIS),

	    %%% Loop through the value references and find the names/types %%%
	    ValueNamesChain *= chain,
	    ValueTypesChain *= chain,
	    ValuesChain->>for_all(
		and(
		    message(ValueNamesChain, append, @arg1?valueName),
		    message(ValueTypesChain, append, @arg1?type)
		)
	    ),
	    chain_list(ValueNamesChain, ValueNames),
	    chain_list(ValueTypesChain, ValueTypes),

	    % Preserve the values for later when exporting the values
	    AllValuesChain->>merge(ValuesChain),

	    % restrict the quantity space, so it has the right values in the right order
	    reverse(ValueNames, ValueNamesR),
	    reverse(ValueTypes, ValueTypesR),

	    %%% Add all the values to this chain to add them later	
	    %format('De values: ~w in volgorde: ~w\n', [ValueNamesR, ValueTypesR]),
	    length(ValueNamesR, NumValues),

	    %%% Create the restrictions which specify the ordering of the QS
	    forall(
		member(Qvalue, ValueNamesR),
		(
		    nth1(Index2, ValueNamesR, Qvalue),

                    % Add the qualitative value prefix 
                   QVPrefix = 'owl_qv_',
                   atom_concat(QVPrefix, Qvalue, QvaluePrefix),
                    
		    nth1(Index2, ValueTypesR, Qtype),

		    ContainsQVURI = ME<<-add_correct_namespace('containsQualitativeValue'),
		    QvalueURI = ME<<-add_correct_namespace(QvaluePrefix),

		    owl_create_node(Restriction),
		    owl_create_subclass(NameURI, Restriction),
		    owl_type(Restriction, 'owl:Restriction'),
		    owl_onProperty(Restriction, ContainsQVURI),
		    
		    if
			Qtype == 'point'
		    then
			(
			    owl_someValuesFrom(Restriction, QvalueURI)
			)
		    else
			(
			    rdf_bnode(Class),
			    rdf_assert(Class, rdf:'type', owl:'Class'),
			    owl_create_node(List),			    
			    rdf_assert(Class, owl:'intersectionOf', List),
			    owl_someValuesFrom(Restriction, Class),
                            rdf_assert(QvalueURI, rdf:'type', owl:'Class'),
			    rdf_assert(List, rdf:'type', rdf:'List'),
			    rdf_assert(List, rdf:'first', QvalueURI),
			  
			    if	 % If this is the only value
				NumValues == 1
			    then % There are no restrictions
				(
				    %format('There are no further restrictions!\n'),
				    rdf_assert(List, rdf:'rest', rdf:'nil')
				)
			    else
				(				    
				    if  % If there exists a greater point
					Index2 < NumValues
				    then
					(
					    %format('There exists a greater point\n'),
					    GreaterIndex is Index2 + 1,
					    nth1(GreaterIndex, ValueNamesR, GreaterValue),
                                            % Add the qv prefix
                                            atom_concat(QVPrefix, GreaterValue, GreaterValuePrefix),
					    GreaterValueURI = ME<<-add_correct_namespace(GreaterValuePrefix),
					    HasInequalityURI = ME<<-add_correct_namespace('hasInequality'),
					    SmallerThanURI = ME<<-add_correct_namespace('SmallerThan'), 
					    					   					   
					    % Create Restriction,
					    owl_create_restriction(RestricthasInequalityNode, HasInequalityURI),

					    % Attach to previous list
					    rdf_bnode(RestrictionSmallerList),
					    rdf_assert(RestrictionSmallerList, rdf:'type', rdf:'List'),
					    rdf_assert(RestrictionSmallerList, rdf:'first', RestricthasInequalityNode),
					    rdf_assert(List, rdf:'rest', RestrictionSmallerList),
					    					    					    
					    % The anonymous intersection class
					    rdf_bnode(IntersectionClass),
					    rdf_assert(IntersectionClass, rdf:'type', owl:'Class'),
					   
					    % The Inequality
                                            rdf_assert(SmallerThanURI, rdf:'type', owl:'Class'),
                                            
					    % hasInequalityTarget(Value)
					    HasInequalityTargetURI = ME<<-add_correct_namespace('hasInequalityTarget'),
					    owl_create_restriction(HasInequalityTargetNode, HasInequalityTargetURI),
					
                                            /*** COMMENT: Zero is just a class! ***/
					    owl_someValuesFrom(HasInequalityTargetNode, GreaterValueURI),						

					    % Attach intersection with lists
					    rdf_global_id(rdf:'nil', NIL),
					    owl_create_list(HasInequalityTargetList, HasInequalityTargetNode, NIL),
					    owl_create_list(SmallerThanList, SmallerThanURI, HasInequalityTargetList),
					    % Attach the intersection class and the inequality class
					    rdf_assert(IntersectionClass, owl:'intersectionOf', SmallerThanList),
					    owl_someValuesFrom(RestricthasInequalityNode, IntersectionClass)					    
					)
				    else
					true,

				    if
					(
					    % If there was a greater number
					    % but not a smaller one
					    Index2 < NumValues,
					    Index2 == 1
					)
				    then
					(   % Close the RestrictionSmallerList
					    rdf_assert(RestrictionSmallerList, rdf:'rest', rdf:'nil')
					)
				    else 
					true,

				    if 
					(
					    % If there was no greater number
					    % (but there is a smaller one)
					    Index2 == NumValues
					)
				    then
					(
					    % Attach to the list
					    AttachToMe = List
					)
				    else
					(
					    % Attach to the previous restriction
					    AttachToMe = RestrictionSmallerList
					),
							
				    if
					Index2 > 1
				    then
					(
					    SmallerIndex is Index2 - 1,
					    nth1(SmallerIndex, ValueNamesR, SmallerValue),

                                            % Add the QValue Prefix
                                            atom_concat(QVPrefix, SmallerValue, SmallerValuePrefix),
                                            
					    SmallerValueURI = ME<<-add_correct_namespace(SmallerValuePrefix),
					    GreaterThanURI = ME<<-add_correct_namespace('GreaterThan'), 
					    HasInequalityURI = ME<<-add_correct_namespace('hasInequality'),
					    
					    % Create Restriction,
					    owl_create_restriction(RestricthasInequalityNode2, HasInequalityURI),

					    % Attach to previous list
					    rdf_bnode(RestrictionGreaterList),
					    rdf_assert(RestrictionGreaterList, rdf:'type', rdf:'List'),
					    rdf_assert(RestrictionGreaterList, rdf:'first', RestricthasInequalityNode2),
					    rdf_assert(AttachToMe, rdf:'rest', RestrictionGreaterList),
					    					    
					    % The anonymous intersection class
					    rdf_bnode(IntersectionClass2),
					    rdf_assert(IntersectionClass2, rdf:'type', owl:'Class'),
					   
					    % The Inequality
                                            rdf_assert(GreaterThanURI, rdf:'type', owl:'Class'),
					 
					    % hasInequalityTarget(Value)
					    HasInequalityTargetURI = ME<<-add_correct_namespace('hasInequalityTarget'),
					    owl_create_restriction(HasInequalityTargetNode2, HasInequalityTargetURI),
					    owl_someValuesFrom(HasInequalityTargetNode2, SmallerValueURI),

					    % Attach intersection with lists
					    rdf_global_id(rdf:'nil', NIL),
					    owl_create_list(HasInequalityTargetList2, HasInequalityTargetNode2, NIL),
					    owl_create_list(GreaterThanList, GreaterThanURI, HasInequalityTargetList2),
					    % Attach the intersection class and the inequality class
					    rdf_assert(IntersectionClass2, owl:'intersectionOf', GreaterThanList),
					    owl_someValuesFrom(RestricthasInequalityNode2, IntersectionClass2)

					)
				    else
					true
				)
				
			    )			
			)
		    )
		)
	    ),
	    %%%%%%%%%%%%%%%%%%%%%%%%
	    %Export all the values.%
	    %%%%%%%%%%%%%%%%%%%%%%%%

	    % Create a set of values without duplicates 
	    new(SetOfValuesC, chain),
	    chain_list(AllValuesChain, AllValuesList),
	    forall(
		member(Value, AllValuesList),
		(
		    not(send(SetOfValuesC, find,
			message(Value, equalContent, @arg1))
		    ) ->
		    send(SetOfValuesC, append, Value)
		;
		    true
		)
	    ),
	    chain_list(SetOfValuesC, SetOfValues),
	    
	    %%% Add the values to the OWL file
	    forall(
		member(Value, SetOfValues),
		(
		    get(Value, name, ValueName),
		    get(Value, type, Type),

                    % Add QV Prefix
                    QVPrefix = 'owl_qv_',
                    atom_concat(QVPrefix, ValueName, ValuePrefix),
                    
		    ValueURI = ME<<-add_correct_namespace(ValuePrefix), 
		    owl_create_class(ValueURI),

		    % Add the name of the value in different languages
		    send(ME, export_values_languages, Value, ValueURI),

		    % Determine the subclass and add it
		    if
			Type == 'point'
		    then
			TypeURI = ME<<-add_correct_namespace('Point')
		    else
			TypeURI = ME<<-add_correct_namespace('Interval'),

		    owl_create_subclass(ValueURI, TypeURI),
		    
		    % Find the disjoint values and add them
		    delete(SetOfValues, Value, DisjointValues),
		    findall(DisjointURI,
			(
			    member(DisjointValue, DisjointValues),
			    get(DisjointValue, name, Disjoint),

			    % Add the QS Prefix
			    atom_concat(QVPrefix, Disjoint, DisjointPrefix),
			    DisjointURI = ME<<-add_correct_namespace(DisjointPrefix)
			),
			DisjointURIs),
		    owl_create_disjointclasses(ValueURI, DisjointURIs)
		)
	    ).
	    
quantities(ME) :->
    QuantitiesC = @model?quantityDefinitions,
    chain_list(QuantitiesC, Quantities),

    QuantityURI = ME<<-add_correct_namespace('Quantity'),
    HasMagnitudeURI = ME<<-add_correct_namespace('hasMagnitude'),
    MagnitudeURI = ME<<-add_correct_namespace('Magnitude'),
    HasQuantitySpaceURI = ME<<-add_correct_namespace('hasQuantitySpace'),

    forall(
	member(Quantity, Quantities),
	(
	    get(Quantity, name, QuantityName),

            % Add the Prefix to the Quantity Name
            QPrefix = 'owl_q_',
            atom_concat(QPrefix, QuantityName, QuantityNamePrefix),
            
	    % The URI of the QR Quantity definition
	    QuantityNameURI = ME<<-add_correct_namespace(QuantityNamePrefix),
	    owl_create_class(QuantityNameURI),
	    owl_create_subclass(QuantityNameURI, QuantityURI),
	    
	    % Add the name of the quantity space  in different languages
	    send(ME, export_name_languages, Quantity, QuantityNameURI),

	    % Add the comment of the quantity space in different languages
	    send(ME, export_remarks_languages, Quantity, QuantityNameURI),

	    % Get the Quantity object and get the allowed quantity spaces
	    AllowedQuantitySpaces = Quantity<<-allowedQuantitySpaces,
	    
	    % Create a chain of all the names of allowed quantity spaces and make it a list
	    AllowedQuantitySpacesNames *= chain,
	    AllowedQuantitySpaces->>for_all(
		message(AllowedQuantitySpacesNames, append, @arg1?name)
	    ),
	    chain_list(AllowedQuantitySpacesNames, AllowedQuantitySpacesNamesList),
	
	    % Add the right namespaces to the allowed quantity names
	    findall(AllowedQSURI,
		(
		    member(AllowedQS, AllowedQuantitySpacesNamesList),
                    QSPrefix = 'owl_qs_',
                    atom_concat(QSPrefix, AllowedQS, AllowedQSPrefix),
		    AllowedQSURI = ME<<-add_correct_namespace(AllowedQSPrefix)
		),
		AllowedQSURIS),	
	    
	    %format('The allowed quantity spaces are: ~w\n', [AllowedQuantitySpacesNamesList]), 

	    % The restriction hasMagnitude
	    owl_create_restriction(MagnitudeRestriction, HasMagnitudeURI),
	    owl_create_subclass(QuantityNameURI, MagnitudeRestriction),

	    % The intersection magnitude class node
	    owl_create_node(MagnitudeIntersection),
	    rdf_assert(MagnitudeIntersection, rdf:'type', owl:'Class'),
	    owl_someValuesFrom(MagnitudeRestriction, MagnitudeIntersection),

	    % The magnitude class
	    %owl_create_node(MagnitudeClass),
	    %rdf_assert(MagnitudeClass, rdf:'type', owl:'Class'),
	    %rdf_assert(MagnitudeClass, rdf:'about', literal(MagnitudeURI)),
            rdf_assert(MagnitudeURI, rdf:'type', owl:'Class'),

	    % The list pointing to the magnitude class
	    owl_open_list(MagnitudeListNode, MagnitudeURI),

	    % Attach the list to the Class defining the intersection
	    rdf_assert(MagnitudeIntersection, owl:'intersectionOf', MagnitudeListNode),

	    % Create the unionOf restriction and the corresponding list (and end it)
	    owl_create_restriction(UnionOfRestriction, HasQuantitySpaceURI),
	    owl_open_list(UnionOfRestrictionList, UnionOfRestriction),
	    rdf_assert(UnionOfRestrictionList, rdf:'rest', rdf:'nil'),
	    
	    % Attach the magnitude list to the restriction list
	    owl_close_list(MagnitudeListNode, UnionOfRestrictionList),
	    %rdf_assert(MagnitudeListNode, rdf:'rest', UnionOfRestrictionList),

	    % Create the class is the union of the possible quantity spaces
	    owl_create_node(UnionClass),
	    rdf_assert(UnionClass, rdf:'type', owl:'Class'),
	    owl_unionOf(UnionClass, AllowedQSURIS),
	    
	    % Attach the class to the unionOf restriction
	    owl_allValuesFrom(UnionOfRestriction, UnionClass)
    	)
    ).


exportAggregates(ME, Aggregates) :->
    get(Aggregates, find_all, 
	or(
	    message(@prolog, ==, @arg1?class_name?value, 'inputSystem')
	),
	Scenarios
    ),
    get(Aggregates, find_all, 
	or(
	    message(@prolog, ==, @arg1?name, 'Static'),
	    message(@prolog, ==, @arg1?name, 'Static fragment')
	),
	StaticFragments
    ),
    get(@model?modelFragments, find_all, 
	or(
	    message(@prolog, ==, @arg1?name, 'Process'),
	    message(@prolog, ==, @arg1?name, 'Process fragment')
	),
	ProcessFragments
    ),
    get(@model?modelFragments, find_all, 
	or(
	    message(@prolog, ==, @arg1?name, 'Agent'),
	    message(@prolog, ==, @arg1?name, 'Agent fragment')
	),
	AgentFragments
    ),
    chain_list(Scenarios, ScenariosList),
    chain_list(StaticFragments, StaticFragmentList),
    chain_list(ProcessFragments, ProcessFragmentList),
    chain_list(AgentFragments, AgentFragmentList), 
    flatten([StaticFragmentList, ProcessFragmentList, AgentFragmentList], ModelFragmentParentList),
    list_to_set(ModelFragmentParentList, ModelFragmentParentSet),
    free(Scenarios), free(StaticFragments), free(ProcessFragments), free(AgentFragments),

    /* Export the model fragments */
    forall(
	member(ModelFragmentParent, ModelFragmentParentSet),
	(
	    send(ME, exportAggregateDefinitions, ModelFragmentParent?children)
	)
    ),

    /* Export the scenarios */
    forall(
	member(Scenario, ScenariosList),
	(
	   send(ME, exportAggregateDefinition2, Scenario)
	)
    ),

    exportRemainingModelFragments(ME).

/* Export the model fragments which still have to be exported */
exportRemainingModelFragments(ME) :-
    /* Done if there are no more model fragments in the chain */
    send(ME?modelFragmentsToDo, empty).

exportRemainingModelFragments(ME) :-
    /* If there are model fragments in the chain:
     * 1. create a copy of the model fragments chain
     * 2. delete the model fragments from the todo list
     * 3. export the model fragments
     */
    get(ME?modelFragmentsToDo, copy, ModelFragmentsToDo2),
    send(ME?modelFragmentsToDo, clear),
    send(ME, exportAggregateDefinitions, ModelFragmentsToDo2),
    free(ModelFragmentsToDo2).


exportAggregateDefinitions(ME, Aggregates) :->
    chain_list(Aggregates, AggregateList),

    forall(
	(
	    % For each Aggregate that has not been exported yet
	    member(Aggregate, AggregateList),
	    not(send(ME?modelFragmentsDone, member, Aggregate))
	),
	(
	    /* If there is at least one fragment refinement from a model fragment which
	    * has not been imported yet, then put the aggregate in the todo list */
	    get(ME?identifierHash, keys, IdentifierHashKeys),
	    get(Aggregate?elements, find_all,
		and(message(@prolog, ==, @arg1?class_name, 'fragmentRefiner'),
		not(message(IdentifierHashKeys, member, @arg1?referencedFragment))
		),
		UnexportableElements
	    ),
	    free(IdentifierHashKeys),
	    not(send(UnexportableElements, empty)),
	    get(Aggregate?name, value, AggregateToDo),
	    debug(owl(general), 'Added ~w to Todo list', [AggregateToDo]),
	    send(ME?modelFragmentsToDo, append, Aggregate)				       
        ;
	    /* otherwise export it normally */
	    %format('EXPORTING AGGREGATE: ~w\n', [AggregateName]),
	    free(UnexportableElements),
	    ME->>exportAggregateDefinition2(Aggregate),
	    send(ME?modelFragmentsDone, append, Aggregate)
	)
    ).
    
exportAggregateDefinition2(ME, Aggregate) :->
        /* Let op, je moet iedere subfunctie Aggregate meegeven, want
         * daar staat de layout info in. */
        AggregateName = Aggregate<<-name,
	debug(owl(general), '---------------------------------------------------', []),
	debug(owl(general), '--- Exporting Aggregate Definition ~w ---', [AggregateName]),
	debug(owl(general), '---------------------------------------------------', []),

        % Add aggregate prefix (scenario's and model fragments are the same datastructure)
        AGPrefix = 'owl_ag_',
        atom_concat(AGPrefix, AggregateName, AggregateNamePrefix),
        
        /* Creeer de aggregate klasse in OWL, en het commentaar wat daarbij hoort */
        AggregateNameURI = ME<<-add_correct_namespace(AggregateNamePrefix),
        owl_create_class(AggregateNameURI),

	% Add the name of the aggregate in different languages
	send(ME, export_name_languages, Aggregate, AggregateNameURI),

	% Add the comment of the aggregate in different languages
	send(ME, export_remarks_languages, Aggregate, AggregateNameURI),

        /* Keep track of the aggregate identifier */
        send(ME, storeElementDefInIdentifierHash, Aggregate, AggregateNameURI),

        /* Indicate whether the model fragment is active or not */
        get(Aggregate, active, ActiveFlag),
        flag_to_boolean(ActiveFlag, Active),
        get(ME, add_correct_namespace, 'isActive', IsActiveURI),
        owl_property(AggregateNameURI, IsActiveURI, literal(Active)),

        /* Export the displayOrigin and editSize of the aggregate */
        get(ME, add_correct_namespace, 'hasEditSizeHeight', HasEditSizeHeight),
        get(ME, add_correct_namespace, 'hasEditSizeWidth', HasEditSizeWidth),
        
        get(Aggregate, layOutInfo, Aggregate, editSize, EditSize),
        get(EditSize, height, Height),
        get(EditSize, width, Width),
    
        owl_property(AggregateNameURI, HasEditSizeHeight, literal(Height)),
        owl_property(AggregateNameURI, HasEditSizeWidth, literal(Width)),

        /* Exporteer de parents, en geef aan dat het huidige fragment een kind is van dat fragment */
        Parents = Aggregate<<-parentReferences,
        NumParents = Parents<<-size,
        
        if      
                (
                        NumParents > 0                        
                )
        then
                (
                        numlist(1, NumParents, ParentIteratorList),
                        forall(
                                member(ParentIterator, ParentIteratorList),
                                (
                                        ImportedFragment = Parents<<-nth1(ParentIterator),
                                        % ImportedFragmentKlasse = ImportedFragment<<-class_name,
                                        %format('De parent is van klasse: ~w\n', [ImportedFragmentKlasse]),
                                        Parent = ImportedFragment<<-referencedFragment,

                                        /* Use the same vocabulary as in the ontology */
                                        ParentNameCandidate = Parent<<-name,
                                        ParentName = ME<<-convert_vocabulary(ParentNameCandidate),                            

                                        % Add aggregate Prefix (if not defined in the qr ontology)
                                        (
                                                not(member(ParentName, ['StaticFragment',
                                                                        'ProcessFragment', 
                                                                        'AgentFragment'])),
                                                atom_concat(AGPrefix, ParentName, ParentNamePrefix)
                                        ;
                                                ParentNamePrefix = ParentName
                                        ),
                                                                                        
                                        %format('De parentname is: ~w\n', [ParentName]),
                                        ParentNameURI = ME<<-add_correct_namespace(ParentNamePrefix),
                        
                                        owl_create_subclass(AggregateNameURI, ParentNameURI)

                                )
                        )
                ),

        /* Scenario's should have a parent, eventhough it is not explicitly represented */
        if
                (
                        AggregateType = Aggregate<<-currentType,
                        AggregateType == 'inputSystem'
                )
        then
                (
                        ParentNameURI = ME<<-add_correct_namespace('Scenario'),
                        owl_create_subclass(AggregateNameURI, ParentNameURI)
                ),

        ME->>exportAggregate(Aggregate, AggregateNameURI),
	debug(owl(general), '---------------------------------------------------', []),
	debug(owl(general), '--- Finished Exporting Aggregate Definition ~w --- ', [AggregateName]),
	debug(owl(general), '---------------------------------------------------', []),
	
	/* Clear the stored refinements (so imported MFs in other MFs are not confused to be refined) */
	send(ME?refinements, clear),

	send(ME, exportAggregateDefinitions, Aggregate?children).

      
exportAggregate(ME, Aggregate, AggregateNameURI) :->
	%AggregateName = Aggregate<<-name,
	%format('-------------- Exporting Aggregate ~w ---------------\n', [AggregateName]),

        get(Aggregate?elements, copy, Elements),
        /* Add the parents as imported model fragments to the elements */
        chain_list(PreDefinedMFs, ['Static fragment', 'Process fragment', 'Agent fragment', 'Static', 'Process', 'Agent']),
        get(Aggregate?parentReferences, find_all,
             not(message(PreDefinedMFs, member, @arg1?referencedFragment?name)),
             ParentReferences
        ),
        send(Elements, merge, ParentReferences),
        get(Elements, size, ElementsLength),
        
        if
                ( 
                        ElementsLength > 0
                )
        then
                (
                        /* Sort the elements chain into the export order */                  
                        ME->>sortAggregateElements(Elements),
                        numlist(1, ElementsLength, ElementNumbersList),

                        /* Generate the ID of the Model Fragment they belong to */

                        /* Loop through the chain and export each element */
                        forall(
                                member(ElementIterator, ElementNumbersList),
                                (
                                        Element = Elements<<-nth1(ElementIterator),
                                        
                                        ClassName = Element<<-class_name,
					%Name = Element<<-name,
					debug(owl(general), 'Element: ~w', [ClassName]),
					%format('Het element is van klasse: ~w\n', [ClassName]),
                                        (
						ClassName == 'fragmentRefiner', 
						%get(Element, name, Name),
						%format('Exporting refinement: ~w\n', [Name]),
						ME->>exportImportedFragmentOrRefinement(Element, AggregateNameURI)
					;
						ClassName == 'fragmentRefinerIdentity', 
						%format('FragmentRefinerIdentity\n'),% Superfluous
						true
					;
                                                ClassName == 'importedFragment', 
						%get(Element, name, Name),
						%format('**Exporting importedFragment: ~w**\n', [Name]),
						ME->>exportImportedFragmentOrRefinement(Element, AggregateNameURI)
                                        ;
                                                ClassName == 'garpInstance', 
						%get(Element, name, Name), 
						%format('Exporting garpInstance ~w\n', [Name]),
                                                ME->>exportGarpInstance(Element, AggregateNameURI) 
                                        ;
                                                ClassName == 'garpQuantity', 
						%get(Element, name, Name), format('Exporting garpQuantity ~w\n', [Name]),
                                                ME->>exportGarpQuantity(Element, AggregateNameURI)
                                        ; 
                                                ClassName == 'garpAttribute', 
						%format('Exporting garpAttribute\n'),  
                                                ME->>exportGarpAttribute(Element, AggregateNameURI)
                                        ;
                                                ClassName == 'configuration', 
						%format('Exporting configuration\n'), 
                                                ME->>exportConfiguration(Element, AggregateNameURI)
                                        ;
                                                ClassName == 'garpQuantityRelation', 
						%format('Exporting garpQuantityRelation\n'), 
                                                ME->>exportGarpQuantityRelation(Element, AggregateNameURI)
                                        ;
                                                ClassName == 'correspondence', 
						%format('Exporting correspondence\n'),
                                                ME->>exportCorrespondence(Element, AggregateNameURI)
                                        ;
                                                ClassName == 'calculus', 
						%format('Exporting calculus\n'), 
                                                ME->>exportCalculus(Element, AggregateNameURI)
                                        ;
                                                ClassName == 'inequality', 
						%format('Exporting inequality\n'), 
                                                ME->>exportInequality(Element, AggregateNameURI)
                                        ;       
                                                ClassName == 'value',
						%format('Exporting value\n'), 
                                                ME->>exportValue(Element, AggregateNameURI)
                                        ;
                                                ClassName == 'identityRelation', 
						%format('Exporting identityRelation\n'), 
                                                ME->>exportIdentityRelation(Element, AggregateNameURI)
                                        ;
                                                ClassName == 'assumptionInstance', 
						%format('Exporting assumptionInstance\n'),
                                                ME->>exportAssumptionInstance(Element, AggregateNameURI)
                                        ;
						@app?mainMenu->>msgBox(string('OWL Export failed: problem exporting %s. Save your file as a .hgp file to avoid data loss. Please send this error and your .hgp file the developers.', ClassName),alarm), !,
                                                fail
                                        )                                        
                                )
                        )
                ).

/* Refinements are complex, so pay attention here:
 * If the main MF (not the imported MF) being exported contains a refinement directly 
 * (i.e. MF?elements contains the refinement), than the refinement should be stored
 * and once the refined imported fragment should be exported, the refinement is exported
 * instead.
 * However, if the Main MF contains a refinement through an imported MF (i.e. MF?elements does 
 * not contain the refinement), the refinement should be exported immediately. Once the refined 
 * imported fragment should be exported, it should be ignored.
 */
exportImportedFragmentOrRefinement(ME, Element, AggregateNameURI) :->
    /* Am I an imported model fragment or a refinement */
    get(Element, class_name, ClassName),

    (
	/* If the element is a refinement */
	ClassName == 'fragmentRefiner' ->
	/* Store the refined imported fragment and the refinement ingredient. */
	get(Element, refined, RefinedFragment),
	send(ME?refinements, append, RefinedFragment, Element),
	debug(owl(refinements), 'Stored refinement of ~w/importedFragment? by ~w/fragmentRefiner?', [RefinedFragment, Element])

    ;
	true
    ),

    (
	/* If the element is a refinement and a direct elements of the MainMF */
	ClassName == 'fragmentRefiner',
	send(ME, isDirectElementOfMainMF, Element, AggregateNameURI) ->
	% export the refinement when the refined element should be exported
	true
    ;
	/* Otherwise, the element is an imported model fragment or a 
	 * refiner that is a non-direct element of the MainMF.
         * Treat it as a normal imported fragment and export it here, 
	 * (but do _not_ export the imported fragment (IMF) it refines later!) */
	(
	    % If the IMF or refinement is refined ->
	    get(ME?refinements, member, Element, RefinementElement) ->
	    (
		% If the refinement is a direct element of MainMF
		send(ME, isDirectElementOfMainMF, RefinementElement, AggregateNameURI) ->
		
		% Export the refinement instead of the the IMF/refinement
		get(RefinementElement, refinementName, RefinementName),
		get(RefinementElement, class_name, RefinementElementClass),
		get(RefinementElement?refined?referencedFragment?name, value, OriginalName),
		get(RefinementElement, refined, Refined),
		get(Refined, name, RefinedName),
		get(Refined, class_name, RefinedClass),
	
		debug(owl(general), '*** Exporting Imported Fragment: ~w/~w/~w refined from ~w by ~w/~w/~w ***', [RefinementElement, RefinementElementClass, RefinementName,OriginalName,Refined, RefinedClass, RefinedName]),
		send(ME, exportImportedFragment, RefinementElement, AggregateNameURI),
		debug(owl(general), '*** Done exporting Imported Fragment: ~w/~w/~w refined from ~w by ~w/~w/~w ***', [RefinementElement, RefinementElementClass, RefinementName,OriginalName,Refined, RefinedClass, RefinedName]),

		%get(ME, findMainMF, AggregateNameURI, MainMFURI), % What is happening here?
		%get(ME, findElementDefInIdentifierHash, MainMFURI, _Aggregate), % Am I not needed?
		%get(RefinementElement, refined, Refined),
		%findRouteInGarp(Refined, Aggregate, Route, _MainMF),
		chain_list(Route, []), % A direct refinement has an empty route!
		get(ME, findInstanceURIInIdentifierHash, RefinementElement, Route, RefinementIURI),

		/* Get the model fragment definition from which the original importedFragment was derived */
		get(ME, findElementDefURIInIdentifierHash, RefinementElement?refined?referencedFragment, OriginalMFURI),
		get(ME, add_correct_namespace, 'isRefinedFrom', IsRefinedFromURI),
		owl_property(RefinementIURI, IsRefinedFromURI, OriginalMFURI)
	    ;
		% Otherwise if the refinement is not a direct element of MainMf
		% Do not export it, since it is already exported
		true
	    )
	;
	    % Else (it is an unrefined IMF or a refinement that is not a direct element of MainMF) 
	    % Export it normally
	    get(Element, name, ImportedFragmentName),
    	    debug(owl(general), '*** Exporting Imported Fragment: ~w/~w/~w ***', [Element,ClassName,ImportedFragmentName]),
	    send(ME, exportImportedFragment, Element, AggregateNameURI),
	    debug(owl(general), '*** Done exporting Imported Fragment: ~w ***', [ImportedFragmentName])
	)
    ).

exportImportedFragment(ME, Element, AggregateNameURI) :-> 
    /* Create instance of Model Fragment and add to the Aggregate */
    ImportedFragment = Element<<-referencedFragment,
    ImportedFragmentName = ImportedFragment?name<<-value,
    %format('*** Exporting Imported Fragment: ~w *** \n', [ImportedFragmentName]),

    /* Add the Aggregate Prefix and create the identifier */
    AGPrefix = 'owl_ag_',
    atom_concat(AGPrefix, ImportedFragmentName, ImportedFragmentNamePrefix),
    ImportedFragmentIURI = ME<<-createUniqueIDURI(ImportedFragmentNamePrefix),

    /* Keep track of the importedFragment identifier and its IURI */ 
    send(ME, storeInstanceInIdentifierHash, Element, ImportedFragmentIURI, AggregateNameURI),
    %format('Stored IMF ~w\n', [ImportedFragmentIURI]),
        
    ImportedFragmentURI  = ME<<-add_correct_namespace(ImportedFragmentNamePrefix),
    owl_create_individual(ImportedFragmentIURI, ImportedFragmentURI),

    % Add the name of the imported fragment in different languages
    send(ME, export_name_languages, Element?referencedFragment, ImportedFragmentIURI),

    % Add the comment of the imported fragment in different languages
    send(ME, export_remarks_languages, Element, ImportedFragmentIURI),

    /* Attach the importedFragment to the Aggregate */
    AggregateRelationURI = ME<<-add_correct_namespace('hasCondition'),
    owl_property(AggregateNameURI, AggregateRelationURI, ImportedFragmentIURI),

    % Store the remarks (of the unrefined imported fragment)
    get(Element?remarks, value, Remarks),
    owl_create_comment(ImportedFragmentIURI, Remarks), 
        
    /* Export the ImportedFragment as a normal aggregate, but with the instance as AggregateNameURI */
    ME->>exportAggregate(ImportedFragment, ImportedFragmentIURI),

    /* Export the layout info */
    exportLayoutInfo(ME, ImportedFragmentIURI, AggregateNameURI).

exportGarpInstance(ME, Element, AggregateNameURI) :->
	Name = Element?name<<-value,
		    
        % Add a prefix to elements defined in the model
        Prefix = 'owl_ae_',
        (
                % its not defined in the vocabulary
                not(member(Name, ['Entity', 'Agent', 'Assumption'])) *->
	        atom_concat(Prefix, Name, NamePrefix)
        ;
	        NamePrefix = Name
        ),        

        ElementType = Element?entity?name<<-value,
        % Add the abstract entity prefix
         (
                % its not defined in the vocabulary
                not(member(ElementType, ['Entity', 'Agent', 'Assumption'])) *->
	        atom_concat(Prefix, ElementType, ElementTypePrefix)
        ;
	        ElementTypePrefix = ElementType
        ),        
       
        AggregateRelationURI = ME<<-conditionOrConsequenceURI(Element),

	% Add individual to scenario
        NameURI = ME<<-createUniqueIDURI(NamePrefix),

        /* Store the Element/Route for future reference */
        %findRoute2(AggregateNameURI, Route),
        %IdentifierHash->>append(Element, NameURI),
        send(ME, storeInstanceInIdentifierHash, Element, NameURI, AggregateNameURI),
        
	ElementTypeURI = ME<<-add_correct_namespace(ElementTypePrefix),
        owl_create_individual(NameURI, ElementTypeURI),

	% Add the name of the garpInstance in different languages
	send(ME, export_name_languages, Element, NameURI),

	% Add the comment of the garpInstance in different languages
	send(ME, export_remarks_languages, Element, NameURI),

	owl_property(AggregateNameURI, AggregateRelationURI, NameURI),

        /* Export the layout info */
        exportLayoutInfo(ME, NameURI, AggregateNameURI).

exportGarpQuantity(ME, Element, AggregateNameURI) :->
        Name = Element?name<<-value,

        % Add the Quantity Prefix
        QPrefix = 'owl_q_',
        atom_concat(QPrefix, Name, NamePrefix),
        
        NameURI = ME<<-createUniqueIDURI(NamePrefix),
        QuantityURI = ME<<-add_correct_namespace(NamePrefix),
        owl_create_individual(NameURI, QuantityURI),
        AggregateRelationURI = ME<<-conditionOrConsequenceURI(Element),
        owl_property(AggregateNameURI, AggregateRelationURI, NameURI),

	% Add the name of the garpQuantity in different languages
	send(ME, export_name_languages, Element?definition, NameURI),

	% Add the comment of the garpQuantity in different languages
	send(ME, export_remarks_languages, Element, NameURI),

	/* Add the quantity assumptions */
	get(Element, quantityAssumptions, QuantityAssumptions),
	chain_list(QuantityAssumptions, QuantityAssumptionsList),
	get(ME, add_correct_namespace, 'hasQuantityAssumption', HasQuantityAssumptionURI),
	forall(
	    member(QuantityAssumption, QuantityAssumptionsList),
	    (
		translateQuantityAssumption(ME, QuantityAssumption, QuantityAssumptionURI),
		owl_property(NameURI, HasQuantityAssumptionURI, QuantityAssumptionURI)
	    )
	),

        /* Add Quantity to IdentifierHash */
        send(ME, storeInstanceInIdentifierHash, Element, NameURI, AggregateNameURI),

        /* Attach the quantity to the right entity/agent/assumption */
        GarpInstance      = Element<<-garpInstance,
        GarpInstanceRoute = Element<<-instanceRoute,
        get(ME, findRealRoute, AggregateNameURI, GarpInstanceRoute, RealGarpInstanceRoute),        
        get(ME, findInstanceURIInIdentifierHash, GarpInstance, RealGarpInstanceRoute, GarpInstanceIURI),
        HasQuantityURI = ME<<-add_correct_namespace('hasQuantity'),
        owl_property(GarpInstanceIURI, HasQuantityURI, NameURI),
        
        /* Add the Magnitude and Derivatives */
        Magnitude = 'Magnitude',
        Derivative = 'Derivative',
        MagnitudeURI = ME<<-add_correct_namespace(Magnitude),
        DerivativeURI = ME<<-add_correct_namespace(Derivative),
        MagnitudeIURI = ME<<-createUniqueIDURI(Magnitude),
        DerivativeIURI = ME<<-createUniqueIDURI(Derivative),
        owl_create_individual(MagnitudeIURI, Magnitude, MagnitudeURI),
        owl_create_individual(DerivativeIURI, Derivative, DerivativeURI),

        /* Attach the Magnitude and Derivative to the Quantity */
        HasMagnitude = ME<<-add_correct_namespace('hasMagnitude'),
        HasDerivative = ME<<-add_correct_namespace('hasDerivative'),
        owl_property(NameURI, HasMagnitude, MagnitudeIURI),
        owl_property(NameURI, HasDerivative, DerivativeIURI),

        /* Add Quantity, Magnitude and Derivative to Aggregate Type */
        owl_property(AggregateNameURI, AggregateRelationURI, NameURI),
        owl_property(AggregateNameURI, AggregateRelationURI, MagnitudeIURI),
        owl_property(AggregateNameURI, AggregateRelationURI, DerivativeIURI),  
        
        /** Attach the right Quantity Space to the magnitude **/
        QuantitySpace = Element<<-quantitySpace,
        QSName = QuantitySpace<<-name,
        
        QSPrefix = 'owl_qs_',
        atom_concat(QSPrefix, QSName, QSNamePrefix),
        
        QSNameURI = ME<<-add_correct_namespace(QSNamePrefix),
        QSNameIURI = ME<<-createUniqueIDURI(QSNamePrefix),
        owl_create_individual(QSNameIURI, QSNameURI),

	% Add the name of the garpQuantity in different languages
	send(ME, export_name_languages, QuantitySpace, QSNameIURI),
        
        /* Attach QS to Aggregate */
        owl_property(AggregateNameURI, AggregateRelationURI, QSNameIURI),
        /* Attach QS to Quantity */
        HasQuantitySpaceURI = ME<<-add_correct_namespace('hasQuantitySpace'),
        owl_property(MagnitudeIURI, HasQuantitySpaceURI, QSNameIURI),

        /** Attach the right Quantity Space to the derivative **/
        DerivativeQS = @model<<-dqs,
        DQSName = DerivativeQS<<-name,

        /* DEBUG */
	%QSType = QuantitySpace<<-class_name,
	%DQSType = DerivativeQS<<-class_name,
	%format('De quantity space is van type: ~w\n', [QSType]),
	%format('De derivative quantity space is van type: ~w\n', [DQSType]),
        /* END DEBUG */

        
        % Add the QS Prefix 
        atom_concat(QSPrefix, DQSName, DQSNamePrefix),
        
        %format('De naam van de derivative QS: ~w\n', [DQSName]),
        DQSNameURI = ME<<-add_correct_namespace(DQSNamePrefix),
        DQSNameIURI = ME<<-createUniqueIDURI(DQSNamePrefix),
        owl_create_individual(DQSNameIURI, DQSName, DQSNameURI),
        /* Attach QS to Aggregate */
        owl_property(AggregateNameURI, AggregateRelationURI, DQSNameIURI),
        /* Attach QS to Quantity */
        owl_property(DerivativeIURI, HasQuantitySpaceURI, DQSNameIURI),
        
        /* Add Quantity Spaces to IdentifierHash */
        send(ME, storeInstanceInIdentifierHash, QuantitySpace, QSNameIURI, AggregateNameURI),
        send(ME, storeInstanceInIdentifierHash, DerivativeQS, DQSNameIURI, AggregateNameURI), 
       
        /* Attach the right values to the magnitude quantity space */
        QSValuesC = QuantitySpace<<-values,

        ME->>exportQValues(QSValuesC, QSNameIURI, AggregateNameURI, AggregateRelationURI),
        
        /* Attach the right values to the derivative quantity space */
        DQSValuesC = DerivativeQS<<-values, % PORT: DerivativeQS<<-realValues,
        
        ME->>exportQValues(DQSValuesC, DQSNameIURI, AggregateNameURI, AggregateRelationURI),

        /* Export the layout info */
        exportLayoutInfo(ME, Element, AggregateNameURI, QSNameIURI, DerivativeIURI, DQSNameIURI).
    
        
exportQValues(ME, ValuesC, QuantitySpaceIURI, AggregateNameURI, AggregateRelationURI) :->

        ValuesCLength = ValuesC<<-size,
        numlist(1, ValuesCLength, ValuesCIteratorList),
        %reverse(ValuesCIteratorRList, ValuesCIteratorList),

        %format('The number of values is: ~w\n', [ValuesCLength]),

        forall(
                member(ValueCIterator, ValuesCIteratorList),
                (
                        Value = ValuesC<<-nth1(ValueCIterator),
                        %ValueRef = Value<<-valueReference,
			%TYPE = Value<<-class_name,
			%format('Value is van type ~w\n', [TYPE]),
                        ValueName = Value<<-name, % PORT: valueName => name

                        % Add the QVPrefix
                        QVPrefix = 'owl_qv_',
                        
                        atom_concat(QVPrefix, ValueName, ValueNamePrefix),

                        ValueNameURI = ME<<-add_correct_namespace(ValueNamePrefix),
                        ValueNameIURI = ME<<-createUniqueIDURI(ValueNamePrefix),
                        owl_create_individual(ValueNameIURI, ValueNameURI),

			% Add the name of the attribute in different languages
			send(ME, export_values_languages, Value, ValueNameIURI),

                        %CLASS = Value<<-class_name,
                        %format('De value is ~w van klasse ~w\n', [Value, CLASS]),
                        %IdentifierHash->>append(Value, ValueNameIURI), % REPLACED
                        
                        send(ME, storeInstanceInIdentifierHash, Value, ValueNameIURI, AggregateNameURI),

                        ContainsQualitativeValueURI = ME<<-add_correct_namespace('containsQualitativeValue'),

                        /* Attach the value to the quantity space */
                        owl_property(QuantitySpaceIURI, ContainsQualitativeValueURI, ValueNameIURI),
                        /* Attach the value to the Aggregate */
                        owl_property(AggregateNameURI, AggregateRelationURI, ValueNameIURI)
                                                
                )
        ),

        /* Need another loop through the values, as we need the ID's of the values to create the QS inequalities */

        /*  The URI for the hasInequality Relation */
        HasInequality  = ME<<-add_correct_namespace('hasInequality'),
        HasInequalityTarget = ME<<-add_correct_namespace('hasInequalityTarget'),


        /* Needed to find routes */
        %findMainMFAndRoute2(ME, AggregateNameURI, MainMF, _Route),
        
        forall(
                member(ValueCIterator, ValuesCIteratorList),
                (

                        Value = ValuesC<<-nth1(ValueCIterator),
                        ValueType = Value<<-type,
                        %ValueIURI = IdentifierHash<<-member(Value), REPLACED
                        %findRouteInGarp(Value, MainMF, ValueRoute, _MainMFQuestionMark),
                        findInstanceInIdentifierHash(ME, QuantitySpaceIURI, _QS, QSRoute),
                        get(ME, findInstanceURIInIdentifierHash, Value, QSRoute, ValueIURI),

                        (
                                ValueType == 'interval',
                                (
                                        ValueCIterator > 1,
                                        

                                        /* Create the SmallerThan reificated relation individual, and relate
                                         *  it to the point */
                                        SmallerThanURI  = ME<<-add_correct_namespace('SmallerThan'),
                                        SmallerThanIURI = ME<<-createUniqueIDURI('SmallerThan'),
                                        owl_create_individual(SmallerThanIURI, 'SmallerThan', SmallerThanURI),
                                        owl_property(ValueIURI, HasInequality, SmallerThanIURI),
                                        
                                        /* The point above */
                                        PointAboveIndex is ValueCIterator - 1,
                                        PointAbove = ValuesC<<-nth1(PointAboveIndex),

                                        % Add the QV Prefix 
                                        get(ME, findInstanceURIInIdentifierHash, PointAbove, QSRoute, PointAboveURI),
                                        owl_property(SmallerThanIURI, HasInequalityTarget, PointAboveURI)
                                ;
                                        true
                                ),
                                (
                                        ValueCIterator < ValuesCLength,

                                        /* Create the GreaterThan reificated relation individual, and relate
                                         *  it to the point */
                                        GreaterThanURI  = ME<<-add_correct_namespace('GreaterThan'),
                                        GreaterThanIURI = ME<<-createUniqueIDURI('GreaterThan'),
                                        owl_create_individual(GreaterThanIURI, 'GreaterThan', GreaterThanURI),
                                        owl_property(ValueIURI, HasInequality, GreaterThanIURI),
                                        
                                        /* The point above */
                                        PointBelowIndex is ValueCIterator + 1,
                                        PointBelow = ValuesC<<-nth1(PointBelowIndex),

                                        % Add the QV Prefix
                                        get(ME, findInstanceURIInIdentifierHash, PointBelow, QSRoute, PointBelowURI),

                                        owl_property(GreaterThanIURI, HasInequalityTarget, PointBelowURI)

                                ;
                                        true
                                )
                        ;
                                true
                        )
                )
        ).

exportConfiguration(ME, Element, AggregateNameURI) :->
        ConfigurationName = Element<<-name,

        % Add the configuration prefix
        Prefix = 'owl_c_',
        atom_concat(Prefix, ConfigurationName, ConfigurationNamePrefix),
        
        ConfigurationNameURI = ME<<-add_correct_namespace(ConfigurationNamePrefix),
        ConfigurationNameIURI = ME<<-createUniqueIDURI(ConfigurationNamePrefix),

        %format('Exporting configuration: ~w\n', ConfigurationNameIURI),
        
        %IdentifierHash->>append(Element, ConfigurationNameIURI), REPLACED
        send(ME, storeInstanceInIdentifierHash, Element, ConfigurationNameIURI, AggregateNameURI),

        %format('Ik exporteer de configuratie ~w\n', [ConfigurationNameIURI]),
        %format('2. Please do not blow up!!!\n'),

        % Add the configuration to the owl database
        owl_create_individual(ConfigurationNameIURI, ConfigurationNameURI),

	% Add the name of the attribute in different languages
	send(ME, export_name_languages, Element?definition, ConfigurationNameIURI),

	% Add the comment in different languages
	send(ME, export_remarks_languages, Element, ConfigurationNameIURI),

        % Find the arguments in the right model fragments/scenario
        Argument1 = Element<<-argument1,
        Argument2 = Element<<-argument2,
        Argument1Route = Element<<-argument1Route,
        Argument2Route = Element<<-argument2Route,
        get(ME, findRealRoute, AggregateNameURI, Argument1Route, RealArg1Route),
	debug(owl(realroute), 'The real route 1 is: ', []), 
	( debugging(owl(realroute)) -> printChainNames(RealArg1Route) ; true ),
        get(ME, findRealRoute, AggregateNameURI, Argument2Route, RealArg2Route),
    	debug(owl(realroute), 'The real route 2 is: ', []),
	( debugging(owl(realroute)) -> printChainNames(RealArg2Route) ; true ),

        % Find the right URI for the arguments (depending on the aggregate in which it is defined)
        get(ME, findInstanceURIInIdentifierHash, Argument1, RealArg1Route, Argument1URI),
        get(ME, findInstanceURIInIdentifierHash, Argument2, RealArg2Route, Argument2URI),        
       
        HasConfigurationURI = ME<<-add_correct_namespace('hasConfiguration'),
        HasConfigurationTargetURI = ME<<-add_correct_namespace('hasConfigurationTarget'),
        
        owl_property(Argument1URI, HasConfigurationURI, ConfigurationNameIURI),
        owl_property(ConfigurationNameIURI, HasConfigurationTargetURI, Argument2URI),

        /* Attach configuration to aggregate */
        AggregateRelationURI = ME<<-conditionOrConsequenceURI(Element),
        owl_property(AggregateNameURI, AggregateRelationURI, ConfigurationNameIURI),

        /* Export the layout info */
        exportLayoutInfo(ME, ConfigurationNameIURI, AggregateNameURI).

findRealRoute(ME, AggregateNameURI, ArgumentRoute, RealArgumentRoute) :<-
    %get(ME, add_correct_namespace, 'isRefinedFrom', IsRefinedFromURI),
    (
	send(ME, isModelFragmentDefinition, AggregateNameURI) ->	    
	chain_list(ExtraRoute, [])
    ;
	findInstanceInIdentifierHash(ME, AggregateNameURI, Aggregate, ExtraRoute),
	debug(owl(realroute), 'De ExtraRoute is: ', []), 
	%(
	%    debugging(owl(realroute)) ->
	%    printChainNames(ExtraRoute), nl
	%;
	%    true
	%),
	send(ExtraRoute, append, Aggregate)
	%(
	%    debugging(owl(realroute)) ->
	%    debug(owl(realroute), 'De ExtraRoute is: ', []),
	%    printChainNames(ExtraRoute), nl,
	%    get(Aggregate, name, Name), get(Aggregate, class_name, ClassName), format('The aggregate is: ~w/~w/~w\n', [Aggregate,ClassName,Name])
	%;
	%    true
	%)
    ),
    
    /* Add refinements if necessary */
    get(ExtraRoute, merge, ArgumentRoute, RealArgumentRoute),

    (
	debugging(owl(realroute)) ->
	debug(owl(realroute), 'The real route before is: ', []),
	printChainNames(RealArgumentRoute), nl
    ;
	true
    ),
    send(ME, addRefinementsToRoute, RealArgumentRoute),
    (
	debugging(owl(realroute)) ->
	debug(owl(realroute), 'The real route after is: ', []),
	printChainNames(RealArgumentRoute), nl
    ;
	true
    ).

findImportedFragmentInAggregate(ElementName, Aggregate, Element, Route) :-
    findImportedFragmentInAggregate2(ElementName, Aggregate, Element, RouteList),
    chain_list(Route, RouteList).

findImportedFragmentInAggregate2(ElementName, Aggregate, Element, []) :-
                (
			get(Aggregate?elements, find, 
			    if( message(@arg1, has_get_method, name),
			        message(@prolog, ==, @arg1?name, ElementName),
				message(@prolog, fail)
			    ),
			    Element
			)
                        ;
                        get(Aggregate?parentReferences, find, 
			    if( message(@arg1, has_get_method, name),
			        message(@prolog, ==, @arg1?name, ElementName),
				message(@prolog, fail)
			    ),
			    Element
			)
                ),
		get(Element, class_name, Class),
		member(Class, ['importedFragment', 'fragmentRefiner']).
 
findImportedFragmentInAggregate2(ElementName, Aggregate, Element, Route) :-       
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
		findImportedFragmentInAggregate2(ElementName, ImportedFragmentDef, Element, Route).



    % If there is a refinement, remove the path of the refined element in front of it
    /*
    (
	get(ArgumentRoute, find, message(@prolog, ==, @arg1?class_name, 'fragmentRefiner'), Refiner) ->
	get(ArgumentRoute, index, Refiner, Index),

	debug(owl(refinements), 'The complete thing is: ', []), 
	( debugging(owl(refinements)) -> printChainNames(ArgumentRoute), nl ; true ),
	get(ArgumentRoute, sub, 0, Index, DeleteMe),
	debug(owl(refinements), 'Deleting ', []), 
	( debugging(owl(refinements)) -> printChainNames(DeleteMe), nl ; true ),
	send(ArgumentRoute, subtract, DeleteMe)
    ;
	true
    ).
    */

        %findMainMFAndRoute2(ME, AggregateNameURI, _AggregateMainMF, AggregateRoute),
	%findInstanceInIdentifierHash(ME, AggregateNameURI, Aggregate, Route),
	%findRouteInGarp(Element, Aggregate, Route, MainMF),
        %(
        %        send(ME, isModelFragmentDefinition, AggregateNameURI) ->
        %        ExtraRoute = AggregateRoute
        %;
        %        findInstanceInIdentifierHash(ME, AggregateNameURI, Aggregate, _AggregateRoute1),
        %        get(AggregateRoute, copy, ExtraRoute),
        %        send(ExtraRoute, append, Aggregate)
        %),
        /* DEBUG  
        chain_list(ExtraRoute, ExtraRouteList),
        forall(member(ERLM, ExtraRouteList), (Name = ERLM?referencedFragment<<-name, format('~w ', [Name]))),
        format('\n'),
         END DEBUG */              
        %get(ExtraRoute, merge, ArgumentRoute, RealArgumentRoute).
        
        /* DEBUG 
        chain_list(RealArgumentRoute, RealArgumentRouteList),
        forall(member(RARM, RealArgumentRouteList), (Name = RARM?referencedFragment<<-name, format('~w ', [Name]))),
        format('\n'),
        format('---\n').
        END DEBUG */

exportGarpAttribute(ME, Element, AggregateNameURI) :->
        AttributeValueName = Element?valueReference<<-valueName,
        AttributeValuePrefix = 'owl_av_',
        string_concat(AttributeValuePrefix, AttributeValueName, AttributeValueNamePrefix),
        AttributeValueNameURI = ME<<-add_correct_namespace(AttributeValueNamePrefix),
        AttributeName = Element?definition<<-name,
        AttributePrefix = 'owl_a_',
        atom_concat(AttributePrefix, AttributeName, AttributeNamePrefix),
        AttributeNameURI = ME<<-add_correct_namespace(AttributeNamePrefix),
        AttributeNameIURI = ME<<-createUniqueIDURI(AttributeNamePrefix), 
        
        send(ME, storeInstanceInIdentifierHash, Element, AttributeNameIURI, AggregateNameURI),

        /* Attach the quantity to the right entity/agent/assumption */
        GarpInstance      = Element<<-garpInstance,
        GarpInstanceRoute = Element<<-instanceRoute, 
        get(ME, findRealRoute, AggregateNameURI, GarpInstanceRoute, RealGarpInstanceRoute),
        get(ME, findInstanceURIInIdentifierHash, GarpInstance, RealGarpInstanceRoute, GarpInstanceIURI),

        %findMainMFAndRoute2(ME, AggregateNameURI, MainMF, _Route),
        %findRouteInGarp(GarpInstance, MainMF, GarpInstanceRoute, _MainMFQuestionMark),

        %GarpInstance = Element<<-garpInstance,
        %GarpInstanceName = GarpInstance<<-name,
        %format('Hier kom ik nog ~w\n', [GarpInstance]),
        %GarpInstanceURI = IdentifierHash<<-member(GarpInstance),

        HasAttributeURI = ME<<-add_correct_namespace('hasAttribute'),
        HasAttributeValueURI = ME<<-add_correct_namespace('hasAttributeValue'),

        % Add the attribute to the owl database
        owl_create_individual(AttributeNameIURI, AttributeNameURI),

	% Add the name of the attribute in different languages
	send(ME, export_name_languages, Element?definition, AttributeNameIURI),

	% Add the comment in different languages
	send(ME, export_remarks_languages, Element, AttributeNameIURI),

        /* The abstract entity has an attribute instance */
        owl_property(GarpInstanceIURI, HasAttributeURI, AttributeNameIURI),
        /* The attribute instance has a value */
        owl_property(AttributeNameIURI, HasAttributeValueURI, AttributeValueNameURI),
        /* The model fragment contains an attribute */
        AggregateRelationURI = ME<<-conditionOrConsequenceURI(Element),
        owl_property(AggregateNameURI, AggregateRelationURI, AttributeNameIURI),
        
        /* Export the layout info */
        exportLayoutInfo(ME, AttributeNameIURI, AggregateNameURI).

exportGarpQuantityRelation(ME, Element, AggregateNameURI) :->
        ElementType = Element<<-type,
        ElementSign = Element<<-sign,
        (
                ElementType == 'prop',
                ElementTypeName = 'Proportionality'
        ;
                ElementType == 'inf',
                ElementTypeName = 'Influence'
        ),
        (
                ElementSign == 'min',
                ElementSignName = 'Negative'
        ;
                ElementSign == 'plus',
                ElementSignName = 'Positive'
        ),
        concat(ElementSignName, ElementTypeName, ElementName),
        ElementNameURI = ME<<-add_correct_namespace(ElementName),
        ElementNameIURI = ME<<-createUniqueIDURI(ElementName),
        % IdentifierHash->>append(Element, ElementNameIURI), REPLACED
        send(ME, storeInstanceInIdentifierHash, Element, ElementNameIURI, AggregateNameURI),

        % Add the causal dependency to the owl database
        owl_create_individual(ElementNameIURI, ElementName, ElementNameURI),

    	% Add the comment in different languages
	send(ME, export_remarks_languages, Element, ElementNameIURI),

        % Find the arguments in the right model fragments/scenario
        Argument1 = Element<<-argument1,
        Argument2 = Element<<-argument2,
        Argument1Route = Element<<-argument1Route,
        Argument2Route = Element<<-argument2Route,
        get(ME, findRealRoute, AggregateNameURI, Argument1Route, RealArg1Route),
        get(ME, findRealRoute, AggregateNameURI, Argument2Route, RealArg2Route),
        get(ME, findInstanceURIInIdentifierHash, Argument1, RealArg1Route, Argument1URI),
        get(ME, findInstanceURIInIdentifierHash, Argument2, RealArg2Route, Argument2URI),
        
        HasCausalDependency = ME<<-add_correct_namespace('hasCausalDependency'),
        HasCausalDependencyTarget = ME<<-add_correct_namespace('hasCausalDependencyTarget'),
        
        owl_property(Argument1URI, HasCausalDependency, ElementNameIURI),
        owl_property(ElementNameIURI, HasCausalDependencyTarget, Argument2URI),

        /* Attach Causal Relation to Aggregate */
        AggregateRelationURI = ME<<-conditionOrConsequenceURI(Element),
        owl_property(AggregateNameURI, AggregateRelationURI, ElementNameIURI),

        /* Export the layout info */
        exportLayoutInfo(ME, ElementNameIURI, AggregateNameURI).

exportCorrespondence(ME, Element, AggregateNameURI) :->

        % Find the arguments in the right model fragments/scenario
        Quantity1 = Element<<-argument1,
        Quantity2 = Element<<-argument2,     
        Quantity1Route = Element<<-argument1Route,
        Quantity2Route = Element<<-argument2Route,
        get(ME, findRealRoute, AggregateNameURI, Quantity1Route, RealQuantity1Route),
        get(ME, findRealRoute, AggregateNameURI, Quantity2Route, RealQuantity2Route),
        get(ME, findInstanceURIInIdentifierHash, Quantity1, RealQuantity1Route, Quantity1URI),
        get(ME, findInstanceURIInIdentifierHash, Quantity2, RealQuantity2Route, Quantity2URI),

        OnDerivative = Element<<-derivative,
        QualitativeValue1 = Element<<-argument1Value,
        QualitativeValue2 = Element<<-argument2Value,
        (
                QualitativeValue1 == @nil,
                OnDerivative == @off ->
                Argument1URI = ME<<-findQuantitySpaceIDFromQuantityID(Quantity1URI, 'magnitude')
        ;
                QualitativeValue1 == @nil,
                OnDerivative == @on ->
                Argument1URI = ME<<-findQuantitySpaceIDFromQuantityID(Quantity1URI, 'derivative')
        ;
                QV1Name = QualitativeValue1<<-valueName,
                OnDerivative == @off ->
                Argument1URI = ME<<-findValueIDFromQuantityID(Quantity1URI, 'magnitude', QV1Name)
        ;
                QV1Name = QualitativeValue1<<-valueName,
                OnDerivative == @on ->
                Argument1URI = ME<<-findValueIDFromQuantityID(Quantity1URI, 'derivative', QV1Name)
               
        ),
        (
                QualitativeValue2 == @nil,
                OnDerivative == @off ->
                Argument2URI = ME<<-findQuantitySpaceIDFromQuantityID(Quantity2URI, 'magnitude')
        ;
                QualitativeValue2 == @nil,
                OnDerivative == @on ->
                Argument2URI = ME<<-findQuantitySpaceIDFromQuantityID(Quantity2URI, 'derivative')
        ;
                QV2Name = QualitativeValue2<<-valueName,
                OnDerivative == @off ->
                Argument2URI = ME<<-findValueIDFromQuantityID(Quantity2URI, 'magnitude', QV2Name)
        ;
                QV2Name = QualitativeValue2<<-valueName,
                OnDerivative == @on ->
                Argument2URI = ME<<-findValueIDFromQuantityID(Quantity2URI, 'derivative', QV2Name) 
        ),


        /* Determine whether Quantity Space Correspondence or Value Correspondence */
        /* TODO: Can also be full correspondence!! */
        (
                QualitativeValue1 == @nil,
                QualitativeValue2 == @nil ->
                CorrespondenceType = 'QuantitySpace'
        ;
                CorrespondenceType = 'Value'
        ),
        string_concat(CorrespondenceType, 'Correspondence', FullCorrespondenceNameString),
        string_to_atom(FullCorrespondenceNameString, FullCorrespondenceName),
        FullCorrespondenceNameURI = ME<<-add_correct_namespace(FullCorrespondenceName),
        FullCorrespondenceNameIURI = ME<<-createUniqueIDURI(FullCorrespondenceName),

        %IdentifierHash->>append(Element,FullCorrespondenceNameIURI),
        send(ME, storeInstanceInIdentifierHash, Element, FullCorrespondenceNameIURI, AggregateNameURI),


        % Add the individual to the owl database
        owl_create_individual(FullCorrespondenceNameIURI, FullCorrespondenceName, FullCorrespondenceNameURI),

    	% Add the comment in different languages
	send(ME, export_remarks_languages, Element, FullCorrespondenceNameIURI),
        
	/* IsDirected */
        IsDirectedFlag = Element<<-directed,
        flag_to_boolean(IsDirectedFlag, IsDirected),
        get(ME, add_correct_namespace, 'isDirected', IsDirectedURI),
        owl_property(FullCorrespondenceNameIURI, IsDirectedURI, literal(IsDirected)),

        /* IsInverted */
        IsInvertedFlag = Element<<-mirror,
        flag_to_boolean(IsInvertedFlag, IsInverted),
        get(ME, add_correct_namespace, 'isInverted', IsInvertedURI),
        owl_property(FullCorrespondenceNameIURI, IsInvertedURI, literal(IsInverted)),

      
        /* Attach the first argument to the correspondence */
        HasCorrespondenceURI = ME<<-add_correct_namespace('hasCorrespondence'),
        %format('owl_property(~w, ~w, ~w),', [Argument1URI, HasCorrespondenceURI, FullCorrespondenceNameIURI]),
        owl_property(Argument1URI, HasCorrespondenceURI, FullCorrespondenceNameIURI),

        /* Attach the correspondence to the second argument */
        HasCorrespondenceTargetURI = ME<<-add_correct_namespace('hasCorrespondenceTarget'),
        owl_property(FullCorrespondenceNameIURI, HasCorrespondenceTargetURI, Argument2URI),
        
        /* Attach the aggregate to the correspondence */
        AggregateRelationURI = ME<<-conditionOrConsequenceURI(Element),
        owl_property(AggregateNameURI, AggregateRelationURI, FullCorrespondenceNameIURI),

        /* Export the layout info */
        exportLayoutInfo(ME, FullCorrespondenceNameIURI, AggregateNameURI).


exportCalculus(ME, Element, AggregateNameURI) :->
        % Find the arguments in the right model fragments/scenario
        Argument1        = Element<<-argument1,
        Argument2        = Element<<-argument2,
        Argument1Route   = Element<<-argument1Route,
        Argument2Route   = Element<<-argument2Route,
        Argument1C       = Argument1<<-class_name,
        Argument2C       = Argument2<<-class_name,
        Argument1QSPoint = Element<<-argument1QSPoint,
        Argument2QSPoint = Element<<-argument2QSPoint,
        get(ME, findRealRoute, AggregateNameURI, Argument1Route, RealArg1Route),
        get(ME, findRealRoute, AggregateNameURI, Argument2Route, RealArg2Route),

        /* If either of the arguments does not exist yet, it must be a calculus which
         * is not exported yet. Add it to the chain of elements which still have to be
         * exported. */
        (
                (
                    not(get(ME, findInstanceURIInIdentifierHash, Argument1, RealArg1Route, Argument1IURI))
                    ;
                    not(get(ME, findInstanceURIInIdentifierHash, Argument2, RealArg2Route, Argument2IURI))
                ) ->
                send(ME?calculiToDoChain, append, Element)
        
        ;       /* Otherwise do the normal export */
                get(ME, findInstanceURIInIdentifierHash, Argument1, RealArg1Route, Argument1IURI),
                get(ME, findInstanceURIInIdentifierHash, Argument2, RealArg2Route, Argument2IURI),
                         
                Sign = Element<<-sign,
                Type = Element<<-type,

                /* Create Plus/Minus relation individual */
                (
                        Sign == 'plus',
                        CalculusName = 'Plus'
                ;
                        CalculusName = 'Minus'
                ),
                CalculusNameURI  = ME<<-add_correct_namespace(CalculusName),
                CalculusNameIURI = ME<<-createUniqueIDURI(CalculusName),

                % Add the calculus to the owl database
                owl_create_individual(CalculusNameIURI, CalculusName, CalculusNameURI),
       
		% Add the comment in different languages
		send(ME, export_remarks_languages, Element, CalculusNameIURI),
        
                /* Keep track of a calculus identifier */
                %IdentifierHash->>append(Element, CalculusNameIURI), REPLACED
                send(ME, storeInstanceInIdentifierHash, Element, CalculusNameIURI, AggregateNameURI),

                /* Attach Argument1 to Calculus */
                HasLeftHandSideURI = ME<<-add_correct_namespace('hasLeftHandSide'),
                (
                        /* From a magnitude or derivative */
                        Argument1C == 'garpQuantity',
                        Argument1QSPoint == @nil ->
                        MagOrDer1URI = ME<<-findMagnitudeOrDerivativeFromQuantityID(Argument1IURI, Type),
                        owl_property(CalculusNameIURI, HasLeftHandSideURI, MagOrDer1URI)
                ;
                        /* From a value in a quantity space of a magnitude or derivative */
                        Argument1C == 'garpQuantity' ->
                        Quantity1QSPointValueName = Argument1QSPoint<<-valueName,
                        Quantity1QSPointURI = ME<<-findValueIDFromQuantityID(Argument1IURI, Type, Quantity1QSPointValueName),
                        owl_property(CalculusNameIURI, HasLeftHandSideURI, Quantity1QSPointURI)
                ;
                        /* From another calculus */
                        Argument1C == 'calculus' ->
                        owl_property(CalculusNameIURI, HasLeftHandSideURI, Argument1IURI)              
                ),
        
                /* Attach Calculus to Argument2 */
                HasRightHandSideURI = ME<<-add_correct_namespace('hasRightHandSide'),
                (
                        /* To a magnitude or derivative */
                        Argument2C == 'garpQuantity',
                        Argument2QSPoint == @nil ->
                        MagOrDer2URI = ME<<-findMagnitudeOrDerivativeFromQuantityID(Argument2IURI, Type),
                        owl_property(CalculusNameIURI, HasRightHandSideURI, MagOrDer2URI)
                ;
                        /* To a value in a quantity space of a magnitude or derivative */
                        Argument2C == 'garpQuantity' ->
                        Quantity2QSPointValueName = Argument2QSPoint<<-valueName,
                        Quantity2QSPointURI = ME<<-findValueIDFromQuantityID(Argument2IURI, Type, Quantity2QSPointValueName),
                        owl_property(CalculusNameIURI, HasRightHandSideURI, Quantity2QSPointURI)
                ;
                        /* To another calculus */
                        Argument2C == 'calculus' ->
                        owl_property(CalculusNameIURI, HasRightHandSideURI, Argument2IURI)              
                ),

                /* Attach Calculus to Aggregate */
                AggregateRelationURI = ME<<-conditionOrConsequenceURI(Element),
                owl_property(AggregateNameURI, AggregateRelationURI, CalculusNameIURI),

                /* Export the layout info */
                exportLayoutInfo(ME, CalculusNameIURI, AggregateNameURI)
        ).


exportInequality(ME, Element, AggregateNameURI) :->
        % Find the arguments in the right model fragments/scenario
        Argument1 = Element<<-argument1,
        Argument2 = Element<<-argument2,
        Argument1Route = Element<<-argument1Route,
        Argument2Route = Element<<-argument2Route,

        get(ME, findRealRoute, AggregateNameURI, Argument1Route, RealArg1Route),
        get(ME, findRealRoute, AggregateNameURI, Argument2Route, RealArg2Route),

        /* DEBUG */
	%chain_list(RealArg1Route, RealArg1RouteList),
	%format('De route is: \n'),
	%forall( member(IFragment, RealArg1RouteList), 
	%        (       IFragmentName = IFragment?referencedFragment<<-name, 
	%                format(' ~w ', [IFragmentName]) 
	%        )
	%), format('\n'),
        /* NO DEBUG */
        
        get(ME, findInstanceURIInIdentifierHash, Argument1, RealArg1Route, Arg1IURI),
        get(ME, findInstanceURIInIdentifierHash, Argument2, RealArg2Route, Arg2IURI),
       
        Argument1Type = Element<<-argument1Type,
        Argument2Type = Element<<-argument2Type,
        InequalityType = Element<<-type,
        (
                InequalityType == 'l',
                InequalityName = 'SmallerThan',
                InequalityParentURI = ME<<-add_correct_namespace(InequalityName),
                InequalityIURI = ME<<-createUniqueIDURI(InequalityName)
        ;
                InequalityType == 'leq',
                InequalityName = 'SmallerOrEqualTo',
                InequalityParentURI = ME<<-add_correct_namespace(InequalityName),
                InequalityIURI = ME<<-createUniqueIDURI(InequalityName)
        ;
                InequalityType == 'eq',
                InequalityName = 'EqualTo',
                InequalityParentURI = ME<<-add_correct_namespace(InequalityName),
                InequalityIURI = ME<<-createUniqueIDURI(InequalityName)
        ;
                InequalityType == 'geq',
                InequalityName = 'GreaterOrEqualTo',
                InequalityParentURI = ME<<-add_correct_namespace(InequalityName),
                InequalityIURI = ME<<-createUniqueIDURI(InequalityName)
        ;
                InequalityType == 'g',
                InequalityName = 'GreaterThan',
                InequalityParentURI = ME<<-add_correct_namespace(InequalityName),
                InequalityIURI = ME<<-createUniqueIDURI(InequalityName)
        ), 

        %format('The inequality is of type ~w between ~w and ~w.\n',
        %[InequalityName, Argument1Type, Argument2Type]),

        % Find the real URI's in OWL space of argument 1
        (
                Argument1Type == 'currentValue', !, % Magnitude
                RealArg1IURI = ME<<-findMagnitudeOrDerivativeFromQuantityID(Arg1IURI, 'magnitude')
        ;
                Argument1Type == 'currentDerivative', !, % Derivative
                RealArg1IURI = ME<<-findMagnitudeOrDerivativeFromQuantityID(Arg1IURI, 'derivative')
        ;
                Argument1Type == 'quantityQSPoint', !, % Point quantityspace magnitude
                GenericPoint1 = Element<<-argument1QSPoint,
                GenericPoint1 \== @nil,
                GenericPoint1Name = GenericPoint1<<-valueName,
		%format('1. This is working fine.\n'),
                RealArg1IURI = ME<<-findValueIDFromQuantityID(Arg1IURI, 'magnitude', GenericPoint1Name)                                 
        ;
                Argument1Type == 'derivativeZero', !, % Point quantityspace derivative
		%GenericPoint1 = Element<<-argument1QSPoint,
		%GenericPoint1 \== @nil,
		%GenericPoint1Name = GenericPoint1<<-valueName,
		GenericPoint1Name = 'Zero',
                RealArg1IURI = ME<<-findValueIDFromQuantityID(Arg1IURI, 'derivative', GenericPoint1Name) 
        ;
                Argument1Type == 'calculus', !, % Calculus
                RealArg1IURI = Arg1IURI
        ),

        % Find the real URI's in OWL space of argument 2
        (
                Argument2Type == 'currentValue', !, % Magnitude
                RealArg2IURI = ME<<-findMagnitudeOrDerivativeFromQuantityID(Arg2IURI, 'magnitude')
        ;
                Argument2Type == 'currentDerivative', !, % Derivative
                RealArg2IURI = ME<<-findMagnitudeOrDerivativeFromQuantityID(Arg2IURI, 'derivative')
        ;
                Argument2Type == 'quantityQSPoint', !, % Point quantityspace magnitude
                GenericPoint2 = Element<<-argument2QSPoint,
                GenericPoint2 \== @nil,
                GenericPoint2Name = GenericPoint2<<-valueName,
                RealArg2IURI = ME<<-findValueIDFromQuantityID(Arg2IURI, 'magnitude', GenericPoint2Name)                                 
        ;
                Argument2Type == 'derivativeZero', !, % Point quantityspace derivative
		%GenericPoint2 = Element<<-argument2QSPoint,
		%GenericPoint2 \== @nil,
		%GenericPoint2Name = GenericPoint2<<-valueName,
		GenericPoint2Name = 'Zero',
                RealArg2IURI = ME<<-findValueIDFromQuantityID(Arg2IURI, 'derivative', GenericPoint2Name) 
        ;
                Argument2Type == 'calculus', !, % Calculus
                RealArg2IURI = Arg2IURI
        ),

        % Add the inequality to the the OWL database
        owl_create_individual(InequalityIURI, InequalityName, InequalityParentURI),
        % IdentifierHash->>append(Element, InequalityIURI), REPLACED
        send(ME, storeInstanceInIdentifierHash, Element, InequalityIURI, AggregateNameURI),

    	% Add the comment in different languages
	send(ME, export_remarks_languages, Element, InequalityIURI),

        % Attach Argument1 to the Inequality and the Inequality to Argument2
        HasInequalityURI = ME<<-add_correct_namespace('hasInequality'),
        HasInequalityTargetURI = ME<<-add_correct_namespace('hasInequalityTarget'),
        owl_property(RealArg1IURI, HasInequalityURI, InequalityIURI),
        owl_property(InequalityIURI, HasInequalityTargetURI, RealArg2IURI),

        % Attach the inequality to the aggregate
        AggregateRelationURI = ME<<-conditionOrConsequenceURI(Element),
        owl_property(AggregateNameURI, AggregateRelationURI, InequalityIURI),

        /* Export the layout info */
        exportLayoutInfo(ME, InequalityIURI, AggregateNameURI).


exportValue(ME, Element, AggregateNameURI) :->
        HasValueName = 'hasValue',
        HasValueIURI   = ME<<-createUniqueIDURI(HasValueName),
        HasValueParentURI = ME<<-add_correct_namespace(HasValueName),
        HasValueTargetURI = ME<<-add_correct_namespace('hasValueTarget'),

        % Find the quantity in the right model fragments/scenario
        Quantity = Element<<-quantity,
        QuantityRoute = Element<<-quantityRoute,
        get(ME, findRealRoute, AggregateNameURI, QuantityRoute, RealQuantityRoute),
        get(ME, findInstanceURIInIdentifierHash, Quantity, RealQuantityRoute, ArgIURI),


        GenericPoint = Element<<-valueReference,
        GenericPointName = GenericPoint<<-valueName,
        
        InDerivative = Element<<-derivative,
        (
                InDerivative == @on,
                RealArgIURI = ME<<-findValueIDFromQuantityID(ArgIURI, 'derivative', GenericPointName)
        ;
                RealArgIURI = ME<<-findValueIDFromQuantityID(ArgIURI, 'magnitude', GenericPointName)
        ),

        % Create the hasValue individual and attach it to the value
        owl_create_individual(HasValueIURI, HasValueName, HasValueParentURI),
        owl_property(HasValueIURI, HasValueTargetURI, RealArgIURI),

        % Attach the value assignment to the aggregate
        AggregateRelationURI = ME<<-conditionOrConsequenceURI(Element),
        owl_property(AggregateNameURI, AggregateRelationURI,
        HasValueIURI).

exportAssumptionInstance(ME, Element, AggregateNameURI) :->
        % Find the assumption name 
        Assumption = Element<<-assumption,
        AssumptionName = Assumption?name<<-value,
        
        % Add a prefix assumption
        Prefix = 'owl_ae_',
	atom_concat(Prefix, AssumptionName, AssumptionNamePrefix),
      
        % Create the parent and unique URI's
        AssumptionParentURI = ME<<-add_correct_namespace(AssumptionNamePrefix),
        AssumptionIURI = ME<<-createUniqueIDURI(AssumptionNamePrefix),
        
        % Add the identity relation to the owl database
        owl_create_individual(AssumptionIURI, AssumptionParentURI),

	% Add the name of the assumption instance in different languages
	send(ME, export_name_languages, Element?definition, AssumptionIURI),

	% Add the comment of the assumption instance in different languages
	send(ME, export_remarks_languages, Element, AssumptionIURI),

        % Attach the identity relation to the aggregate
        AggregateRelationURI = ME<<-conditionOrConsequenceURI(Element),
        owl_property(AggregateNameURI, AggregateRelationURI, AssumptionIURI),

	% Attach the assumption to the right entity (if any)
        GarpInstance      = Element<<-garpInstance,
        GarpInstanceRoute = Element<<-instanceRoute,
	(
	    GarpInstance \== @nil ->
	    get(ME, findRealRoute, AggregateNameURI, GarpInstanceRoute, RealGarpInstanceRoute),        
	    get(ME, findInstanceURIInIdentifierHash, GarpInstance, RealGarpInstanceRoute, GarpInstanceIURI),
	    get(ME, add_correct_namespace, 'isAssumptionRegarding', IsAssumptionRegardingURI),
	    owl_property(AssumptionIURI, IsAssumptionRegardingURI, GarpInstanceIURI)
	;
	    true
	),


        % Keep track of the assumption identifier for later
        send(ME, storeInstanceInIdentifierHash, Element, AssumptionIURI, AggregateNameURI),

        /* Export the layout info */
        exportLayoutInfo(ME, AssumptionIURI, AggregateNameURI).



exportIdentityRelation(ME, Element, AggregateNameURI) :->
        % Find the arguments in the right model fragments/scenario
        Arg1 = Element<<-argument1,
        Arg2 = Element<<-argument2,
        Arg1Route = Element<<-argument1Route,
        Arg2Route = Element<<-argument2Route,
        get(ME, findRealRoute, AggregateNameURI, Arg1Route, RealArg1Route),
        get(ME, findRealRoute, AggregateNameURI, Arg2Route, RealArg2Route),
        get(ME, findInstanceURIInIdentifierHash, Arg1, RealArg1Route, Arg1IURI),
        get(ME, findInstanceURIInIdentifierHash, Arg2, RealArg2Route, Arg2IURI),
               
        IdentityName = 'Identity',
        IdentityParentURI = ME<<-add_correct_namespace(IdentityName),
        IdentityIURI = ME<<-createUniqueIDURI(IdentityName),
        HasIdentityURI = ME<<-add_correct_namespace('hasIdentity'),
        HasIdentityTargetURI = ME<<-add_correct_namespace('hasIdentityTarget'),

        %IdentifierHash->>append(Element,IdentityIURI), % REPLACED
        send(ME, storeInstanceInIdentifierHash, Element, IdentityIURI, AggregateNameURI),

        % Add the identity relation to the owl database
        owl_create_individual(IdentityIURI, IdentityName, IdentityParentURI),

    	% Add the comment in different languages
	send(ME, export_remarks_languages, Element, IdentityIURI),

        % Attach the first argument to the identity relation, and the identity to the second argument
        owl_property(Arg1IURI, HasIdentityURI, IdentityIURI),
        owl_property(IdentityIURI, HasIdentityTargetURI, Arg2IURI),

        % Attach the identity relation to the aggregate
        AggregateRelationURI = ME<<-conditionOrConsequenceURI(Element),
        owl_property(AggregateNameURI, AggregateRelationURI, IdentityIURI),

        /* Export the layout info */
        exportLayoutInfo(ME, IdentityIURI, AggregateNameURI).

        
       
findMagnitudeOrDerivativeFromQuantityID(ME, QuantityID, MagOrDer, MagOrDerURI) :<-
        HasDerivativeURI    = ME<<-add_correct_namespace('hasDerivative'),
	HasMagnitudeURI     = ME<<-add_correct_namespace('hasMagnitude'),
        (
                MagOrDer == 'derivative',
                owl_has(QuantityID, HasDerivativeURI, MagOrDerURI)
        ;
                owl_has(QuantityID, HasMagnitudeURI, MagOrDerURI)
        ).


/* Search for the valueURI belonging to a quantityURI */
findValueIDFromQuantityID(ME, QuantityURI, MagOrDerSelector, ValueName, ValueURI) :<-
        HasDerivativeURI    = ME<<-add_correct_namespace('hasDerivative'),
	HasMagnitudeURI     = ME<<-add_correct_namespace('hasMagnitude'),
        ContainsQValueURI   = ME<<-add_correct_namespace('containsQualitativeValue'),
        HasQuantitySpaceURI = ME<<-add_correct_namespace('hasQuantitySpace'),

        %format('Hier kom ik nog!'),
        % Zoek de valueURI en zet hem in Argument1URI
        (
                MagOrDerSelector == 'derivative',
                owl_has(QuantityURI, HasDerivativeURI, MagOrDer)
        ;       
                owl_has(QuantityURI, HasMagnitudeURI, MagOrDer)
        ),
               
        %format('owl_has(~w, ~w, ~w),\n', [MagOrDer, HasQuantitySpaceURI, QuantitySpace]),
        owl_has(MagOrDer, HasQuantitySpaceURI, QuantitySpace),
        %format('owl_has(~w, ~w, ~w),\n', [QuantitySpace, ContainsQValueURI, QVURI]),
        owl_has(QuantitySpace, ContainsQValueURI, QVURI),
        %format("2. ~w BOEM ~w BOEM ~w BOEM!\n", [QuantitySpace1, QV1URI, QV1Name]), 
        %format('owl_has(~w, rdfs:label, ~w),\n', [QVURI, literal(ValueName)]),
        owl_has(QVURI, rdfs:'label', LiteralQVName),
        literal(lang(_Lang, ValueName)) = LiteralQVName,
        %format("3. BOEM ~w BOEM BOEM!\n", [QV1Name]), 
        ValueURI = QVURI.

conditionOrConsequenceURI(ME, Element, AggregateRelationURI) :<-
        /* Condition or Consequence?  What about facts? Not distinguished? */
        (
                Element->>isCondition, 
                AggregateRelationURI = ME<<-add_correct_namespace('hasCondition')
         ; 
                AggregateRelationURI = ME<<-add_correct_namespace('hasConsequence')
        ).


sortAggregateElements(ME, Elements) :->
        Elements->>sort( ?(ME, orderCompare, @arg1?class_name, @arg2?class_name) ).

orderCompare(_ME, Type1, Type2, OrderIndicator) :<-
        Order = [ [1,  'fragmentRefinerIdentity' ],
		  [2,  'fragmentRefiner'],
                  [3,  'importedFragment'],
		  [4,  'garpInstance'],
                  [5,  'assumptionInstance'],
                  [6,  'garpQuantity'],
                  [7,  'garpAttribute'],
                  [8,  'configuration'],
                  [9,  'garpQuantityRelation'],
                  [10, 'value'],
                  [11, 'correspondence'],
                  [12, 'calculus'],
                  [13, 'inequality'], 
                  [14, 'identityRelation'] ],
         member([X,Type1], Order),
         member([Y,Type2], Order),
         if X == Y then OrderIndicator = 0,
         if X < Y then OrderIndicator = -1,
         if X > Y then OrderIndicator = 1.


abstractEntities(ME, Element, Parent, Siblings, Type) :->
    % The name of the element
    ElementName = Element<<-name,

    % Add a prefix to elements defined in the model
    Prefix = 'owl_ae_',

    % its not defined in the vocabulary
    (
	not(member(ElementName, ['Entity', 'Agent', 'Assumption'])),
        atom_concat(Prefix, ElementName, ElementNamePrefix)
    ;
        ElementNamePrefix = ElementName
    ),

    ElementURI = ME<<-add_correct_namespace(ElementNamePrefix),
    
    % Create a OWL representation of the Element
    if % its not defined in the vocabulary
	not(member(ElementName, ['Entity', 'Agent', 'Assumption']))
    then % define it
        (
	    owl_create_class(ElementURI),
	    send(ME, export_name_languages, Element, ElementURI)
        )
    else
	true,

    % The name of the parent
    (
	% A parent exists
	Parent \== @nil ->
	ParentName = Parent<<-name
    ;
	% No parent exists, but the entity is still a subclass  of Type
	Parent == @nil,
	ElementName \== Type -> 
	ParentName = Type
    ;
	% The parent is predefined in the qr vocabulary
	ParentName = @nil
    ),

    if % its not defined in the qr vocabulary
        (
	    not(member(ParentName, ['Entity', 'Agent', 'Assumption', @nil]))
        )
    then % add the prefix 
        (
	    string_concat(Prefix, ParentName, ParentNamePrefix)
        )
    else
	ParentNamePrefix = ParentName,

    if % The parent exists
	ParentName \== @nil
    then % Add the correct namespace and define it as a superclass
	(
	    ParentURI = ME<<-add_correct_namespace(ParentNamePrefix),
	    owl_create_subclass(ElementURI, ParentURI)
	)
    else
	true,

	%%%%%format('Entity Naam: ~w ', [ElementName]),%%%%%
	%%%%%format(' met Parent: ~w ', [ParentName]), %%%%%%%
    if	% If Siblings is a chain (i.e. != @nil)
	(
	    SiblingsClassName = Siblings<<-class_name,
	    SiblingsClassName == 'chain'
	)
    then % Create a new chain with the names of the siblings, 
	 % and convert it to a list
	(
	    SiblingsNameChain *= chain,
	    Siblings->>for_all(
		message(SiblingsNameChain, append, @arg1?name)
	    ),
	    chain_list(SiblingsNameChain, SiblingsList),

	    
	    % Find the element and remove it from the siblingslist 
	    % and add the correct namespaces
	    delete(SiblingsList, ElementName, DisjointList),
	    %format('I\'m going to delete ~w from ~w.\nThis resulted in ~w', [ElementName, SiblingsList, DisjointList]),
	    findall(DisjointURI,
		(
		    member(Disjoint, DisjointList),
                    string_concat(Prefix, Disjoint, DisjointPrefix),
		    DisjointURI = ME<<-add_correct_namespace(DisjointPrefix)
		),
		DisjointURIS),

	    owl_create_disjointclasses(ElementURI, DisjointURIS)	    
	)
    else
	true,

    % Export the remarks in different languages
    send(ME, export_remarks_languages, Element, ElementURI),

    %Remarks = Element?remarks<<-value,
    %%%%%%format('De comments bij dit element zijn: ~w\n', Remarks), %%%%%%
    %owl_create_comment(ElementURI, Remarks),
	
    % Export the children of Element
    Children = Element<<-children,
    Children->>for_all(message(ME, abstractEntities, @arg1, Element, Children)).


/* The imported model fragments should include all ingredients as
 * conditions. This is done after the export for simplicity reasons */
postExportChanges(ME) :->
        ME->>changeImportedMFConsequencesToConditions.

changeImportedMFConsequencesToConditions(ME) :->
        MFURI = ME<<-add_correct_namespace('ModelFragment'),
        findall(       
                ModelFragmentInstance,
                owl:j_instance_of(ModelFragmentInstance, MFURI),                
                ModelFragmentInstances
        ),
        %format('De model fragment instances zijn: ~w\n', [ModelFragmentInstances]).
        
        HasConditionURI = ME<<-add_correct_namespace('hasCondition'),
        HasConsequenceURI = ME<<-add_correct_namespace('hasConsequence'),

        % Update all the consequence relations to condition relations
        rdf_transaction(
                forall(member(ImportedModelFragment, ModelFragmentInstances),
                        (
                                forall(
                                        rdf_has(ImportedModelFragment, HasConsequenceURI, ModelIngredient),
                                        %format('~w -> ~w\n', [ImportedModelFragment, ModelIngredient])
                                        rdf_update(ImportedModelFragment, HasConsequenceURI, ModelIngredient,
                                                predicate(HasConditionURI))
                                        )
                        )
                )
        ).
       
        


convert_vocabulary(_ME, ElementName, NewElementName) :<-
        %format('Elementnaam is: ~w\n', [ElementName]),
        ElementNames = ['Static fragment',
                        'Process fragment',
                        'Agent fragment',
                        'Static',
                        'Process',
                        'Agent'
                       ],
        ElementNewNames = [     'StaticFragment',
                                'ProcessFragment',
                                'AgentFragment',
                                'StaticFragment',
                                'ProcessFragment',
                                'AgentFragment'
                          ],

        if
                ( 
                        member(ElementName, ElementNames) 
                )
        then
                (
                        nth1(Index, ElementNames, ElementName),
                        nth1(Index, ElementNewNames, NewElementName)
                )
        else
                (
                        NewElementName = ElementName
                ).

% Export the names of the element in different languages
export_name_languages(_ME, Element, ElementURI) :->
    get(Element, name_translatable, NameT),
    get(NameT, translator, Translator),

    get(Translator, allLanguages, AllLanguagesC),
    chain_list(AllLanguagesC, AllLanguages),

    % For each language in which the source elements is defined
    forall(
	member(Language, AllLanguages),
	(
	    % The name in a specific language
	    get(NameT, valueForLang, Language?value, Name),
	    % Translate the language to a language id
	    get(Translator, getLanguageKey, Language, LanguageID),
	    % Add the label
	    owl_add_label(ElementURI, LanguageID, Name)
	)
    ).

% Export the remarks of the element in different languages
export_remarks_languages(_ME, Element, ElementURI) :->
    get(Element, remarks_translatable, RemarksT),
    get(RemarksT, translator, Translator),

    get(Translator, allLanguages, AllLanguagesC),
    chain_list(AllLanguagesC, AllLanguages),

    % For each language in which the source elements is defined
    forall(
	member(Language, AllLanguages),
	(
	    % The name in a specific language
	    get(RemarksT, valueForLang, Language?value, Remarks),
	    % Translate the language to a language id
	    get(Translator, getLanguageKey, Language, LanguageID),
	    % Add the label
	    owl_add_comment(ElementURI, LanguageID, Remarks)
	)
    ).

export_values_languages(_ME, Value, ValueURI) :->
    get(Value, valueName_translatable, ValueNameT),
    
    % Get the translator of the target values
    get(ValueNameT, translator, Translator),

    % get all the languages
    get(Translator, allLanguages, AllLanguagesC),
    chain_list(AllLanguagesC, AllLanguages),
	    
    % For each language set the name of the Value
    forall(
	member(Language, AllLanguages),
	(
	    % Set the name of the target value
	    get(ValueNameT, valueForLang, Language?value, NameInLanguage),
	    % Translate the language to a language id
	    get(Translator, getLanguageKey, Language, LanguageID),
	    % Add the label
	    owl_add_label(ValueURI, LanguageID, NameInLanguage)
	)
    ).



:-pce_end_class.

exportLayoutInfo(ME, ElementURI, AggregateNameURI) :-

        %chain_list(Route, RouteList),
        %format('De route is ~w\n', [RouteList]),

        getLayoutInfo(ME, ElementURI, AggregateNameURI, XPosition, YPosition, HiddenFlag),
        /* Put the position information in the RDF database here */
        % ElementURI = IdentifierHash<<-member(Element),

        XposURI = ME<<-add_correct_namespace('has_xposition_on_screen'),
        YposURI = ME<<-add_correct_namespace('has_yposition_on_screen'),
        HiddenURI = ME<<-add_correct_namespace('is_hidden_on_screen'),
        
        owl_property(ElementURI, XposURI, literal(XPosition)),
        owl_property(ElementURI, YposURI, literal(YPosition)),

        flag_to_boolean(HiddenFlag, Hidden),
        owl_property(ElementURI, HiddenURI, literal(Hidden)).

        %format('The position is: [~w, ~w]\n, and the hidden flag is: ~w\n', [XPosition, YPosition, Hidden]).

/* Special exportLayoutInfo for garpQuantities */
exportLayoutInfo(ME, Element, AggregateNameURI, QSNameIURI, DerivativeIURI, DQSNameIURI) :-

        findMainMFAndRoute2(ME, AggregateNameURI, MainMF, _RouteChain),
        findInstanceInIdentifierHash(ME, QSNameIURI, _QS, RouteChain),

        /* DEBUG */
        %format('MainMF = ~w with route:\n', [MainMF]),
        %chain_list(RouteChain, RouteList),
        %forall(member(RE, RouteList), (X = RE<<-class_name, format('~w/~w\n', [RE,X]))),
        /* END DEBUG */

        Position             = MainMF<<-layOutInfo(Element, relPosition, RouteChain, @off),
        HiddenFlag           = MainMF<<-layOutInfo(Element, hidden, RouteChain, @off),
        QSPosition           = MainMF<<-layOutInfo(Element, qsElementRelPosition, RouteChain, @off),
        QSHiddenFlag         = MainMF<<-layOutInfo(Element, qsElementHidden, RouteChain, @off),
        DerivativePosition   = MainMF<<-layOutInfo(Element, dplusRelPosition, RouteChain, @off),
        DerivativeHiddenFlag = MainMF<<-layOutInfo(Element, dplusHidden, RouteChain, @off),
        DQSPosition          = MainMF<<-layOutInfo(Element, derivativeElementRelPosition, RouteChain, @off),
        DQSHiddenFlag        = MainMF<<-layOutInfo(Element, derivativeElementHidden, RouteChain, @off),
        /** TODO: What should I do with this thing? It sometimes even breaks! **/
	%QSValueElementHiddenFlag = MainMF<<-layOutInfo(Element, qsValueElementHidden, RouteChain),

        XPosition            = Position<<-x,
        YPosition            = Position<<-y,
        QSXPosition          = QSPosition<<-x,
        QSYPosition          = QSPosition<<-y,
        DerivativeXPosition  = DerivativePosition<<-x,
        DerivativeYPosition  = DerivativePosition<<-y,
        DQSXPosition         = DQSPosition<<-x,
        DQSYPosition         = DQSPosition<<-y,
        flag_to_boolean(HiddenFlag, Hidden),
        flag_to_boolean(QSHiddenFlag, QSHidden),
        flag_to_boolean(DerivativeHiddenFlag, DerivativeHidden),
        flag_to_boolean(DQSHiddenFlag, DQSHidden),
        /* TODO: What should I do with this thing */
	%flag_to_boolean(QSValueElementHiddenFlag, _QSValueElementHidden),
        

        /* Put the position information in the RDF database here */
        get(ME, findInstanceURIInIdentifierHash, Element, RouteChain, ElementURI),

        XposURI = ME<<-add_correct_namespace('has_xposition_on_screen'),
        YposURI = ME<<-add_correct_namespace('has_yposition_on_screen'),
        HiddenURI = ME<<-add_correct_namespace('is_hidden_on_screen'),
        
        /* the quantity */
        owl_property(ElementURI, XposURI, literal(XPosition)),
        owl_property(ElementURI, YposURI, literal(YPosition)),
        owl_property(ElementURI, HiddenURI, literal(Hidden)),
        
        /* the quantity space of the magnitude */
        owl_property(QSNameIURI,  XposURI, literal(QSXPosition)),
        owl_property(QSNameIURI,  YposURI, literal(QSYPosition)),
        owl_property(QSNameIURI,  HiddenURI, literal(QSHidden)),
      
        /* The derivative */
        owl_property(DerivativeIURI, XposURI, literal(DerivativeXPosition)),
        owl_property(DerivativeIURI, YposURI, literal(DerivativeYPosition)),
        owl_property(DerivativeIURI, HiddenURI, literal(DerivativeHidden)),
        
        /* The quantity space of the derivative */
        owl_property(DQSNameIURI, XposURI, literal(DQSXPosition)),
        owl_property(DQSNameIURI, YposURI, literal(DQSYPosition)),
        owl_property(DQSNameIURI, HiddenURI, literal(DQSHidden)).
        
       
getLayoutInfo(ME, ElementURI, AggregateNameURI, XPosition, YPosition, Hidden) :-
        /* Find the instance and the route */
	findInstanceInIdentifierHash(ME, ElementURI, Element, RouteChain),

        /* Find the MainMF in which everything is defined */
        findMainMFAndRoute2(ME, AggregateNameURI, MainMF, _RouteChain),

	/* DEBUG 
	E_CLASS = Element<<-class_name,
	MainMFName = MainMF<<-name,
	format('What is the layout in \'~w\' for: ~w/~w\n', [MainMFName, Element, E_CLASS]),
	chain_list(RouteChain, RouteList),
	forall(member(RE, RouteList), (RE_CLASS = RE<<-class_name, FragName = RE?referencedFragment<<-name,
	        format('~w/~w-~w ', [RE,RE_CLASS,FragName]))), 
	format('\n'),
	 END DEBUG */
	(
	    /* If the element is a normal element or importedFragment, it has normal layout */
	    get(MainMF, layOutInfo, Element, relPosition, RouteChain, @off, Position),
	    XPosition = Position<<-x,
	    YPosition = Position<<-y,
	    get(MainMF, layOutInfo, Element, hidden, RouteChain, @off, Hidden)
	;
	    /* Otherwise it is a fragmentRefiner and the layout of the original importedFragment should be used. */
	    get(Element, refined, OriginalFragment),

	    /* Get the correct route */
	    get(ME, findElementDefInIdentifierHash, AggregateNameURI, AggregateDef),
	    findRouteInGarp(OriginalFragment, AggregateDef, OriginalRouteChain, _OriginalMainMF),
	    
	    get(MainMF, layOutInfo, OriginalFragment, relPosition, OriginalRouteChain, @off, Position),
	    XPosition = Position<<-x,
	    YPosition = Position<<-y,
	    get(MainMF, layOutInfo, OriginalFragment, hidden, OriginalRouteChain, @off, Hidden)
	).


/*
findMainMFAndRoute(ME, AggregateNameURI, IdentifierHash, MainMF, RouteChain) :-
        findRoute(ME, AggregateNameURI, IdentifierHash, MainMFAndRoute),
        (
                [MainMF|Route] = MainMFAndRoute
        ;
                MainMF = MainMFAndRoute ->
                Route = []
        ),
        chain_list(RouteChain, Route).       
        
        
findRoute(ME, AggregateNameURI, IdentifierHash, CurrentModelFragment) :-
        HasConditionURI = ME<<-add_correct_namespace('hasCondition'),
        not(rdf_has(_ImportedModelFragmentURI, HasConditionURI, AggregateNameURI)) ->
        CurrentModelFragment = IdentifierHash<<-find_key( message(@prolog, =, AggregateNameURI, @arg2) ).

findRoute(ME, AggregateNameURI, IdentifierHash, [Route|CurrentModelFragment]) :-
        CurrentModelFragment = IdentifierHash<<-find_key( message(@prolog, =, AggregateNameURI, @arg2) ),
        HasConditionURI = ME<<-add_correct_namespace('hasCondition'),
        rdf_has(ImportingModelFragmentURI, HasConditionURI, AggregateNameURI) ->
        %format('AggregateNameURI = ~w\n', [AggregateNameURI]), 
        findRoute(ME, ImportingModelFragmentURI, IdentifierHash, Route).
*/

flag_to_boolean(Flag, Boolean) :-
        (
                Flag = @on ->
                Boolean = 'true'
        ;
                Boolean = 'false'
        ).


