/*
Definitie garpModel class

Er is maar 1 instantie van deze class: @model (zie application.pl).
Bevat alle top-elementen van het model, gaat over de topcontrole.
Bevat dus ook veel applicatie achtige dingen
Elke change requestor die een naam bevat wordt ook door het model
gecontroleerd: deze geeft aan dat een naam niet valide is.

Part of Garp3, see copyright notice
Most code old dutch homer code

2000-2005 Jelmer Jellema, Spin in het Web, spininhetweb.nl
*/

%:-free(@model). %bevrijd de enige instantie. (niet meer vanzelf)
:-pce_begin_class(
		  garpModel(name),
		  object,
		  "Complete GARP model. The only instance is @model" % JL: Well not any more its not.
		 ).

variable(name,name,both,"Model name").

variable(filename,name*,both,"The filename of the model"). % JL

variable(modelState,{new,loaded,legacy},both). %is there a model (changed state in @model). % JL

variable(abstractEntities,chain,get,"All entity and agent objects (not assumptions)"). % also Sketch concepts, AB, Aug 2006

variable(assumptions, chain, get, "All assumption objects").

variable(attributeDefinitions,chain,get,"All garpAttributeDefinition objects").

variable(configurationDefinitions,chain,get,"All configurationDefinition objects").

variable(sketchRelationDefinitions,chain,get,"All sketchRelationDefinition objects"). % for Sketch relations, AB, added feb 2006, moved to concept_map.pl in nov 2006, but kept here for reading old models
variable(sketchStructuralRelationDefinitions,chain,get,"All sketchStructuralRelationDefinition objects"). % AB, added june 2006, moved to structure_model_map.pl in nov 2006, but kept here for reading old models
variable(sketchProcessDefinitions,chain,get,"All sketchProcessDefinition objects"). % AB, aug 2006
variable(sketchAgentDefinitions,chain,get,"All sketchAgentDefinition objects"). % AB, aug 2006
variable(sketchScenarioDefinitions,chain,get,"All sketchScenarioDefinition objects"). % AB, aug 2006

variable(quantityDefinitions,chain,get,"All garpQuantityDefinition objects").

variable(quantitySpaces,chain,get,"All quantitySpace objects").

%er is een interne qs voor alle derivatives: @model?dqs
%deze maakt *geen* deel uit van de lijst met quantityspaces.
%er is ook een interne qs @model?mzpqs: deze maakt wel deel uit van de lijst
%en is te gebruiken, maar niet te bewerken
%op deze wijze is mzp zichtbaar, maar niet hetzelfde als dqs (dus niet
%dezelfde valuerefs)

variable(dqs, quantitySpace,get,"Quantity space for all derivatives").
variable(mzpqs, quantitySpace, get, "Basic MZP quantity space").

variable(modelFragments,chain,get,"All modelFragment objects").

variable(sketches,chain,get,"All sketches"). % AB, feb 2006

variable(changed, bool, both, "@on if changed").

variable(exportChecksum_isa, 'string*', both, "Checksum for exported isa file").

variable(exportChecksum_qs, 'string*', both, "Checksum for exported quantity_space file").

variable(exportChecksum_inputsystem, 'string*', both, "Checksum for exported input_system file").

variable(exportChecksum_library, 'string*', both, "Checksum for exported library file").

variable(lastChangeTime, date:=new(date), both, "Last change"). %gp3 0.1 see changeRequest

variable(savedSimulations,'hash_table', both, "Prolog clauses for saved simulations").

variable(savedSketches,'hash_table', both, "Prolog clauses for saved sketches").

variable(runPrefs,'hash_table',both, "Engine runprefs (assumptions) saved in the model"). %gp3 0.3 (model definition version 4)

variable(mfViews,'hash_table',both,"Views saved in mfStructureEditor"). %gp3 0.3 (model definition version 5).

variable(modelDefinitionVersion,int,both, "A version number used to implement version patches"). %gp3 0.3: when a model has a lower version, we might have to patch it

variable(metaData,'hash_table',both,"Model meta data"). %gp3 0.3.13 (model definition 7). A hash table for model meta data. The keys are categories used in the code, values are hash_tables containing key/value data and complex structures

variable(translator, translator, both, "Coordinates translations").

variable(uniqueIDs, chain, both, "A chain of unique IDs for a QR model (current ID and previous unique IDs)").

%model 15: Version saved bij Garp3 1.3.8. This and older versions added a lot of stuff about the collaborative workbench (sketch etc)
% model version 16: Added translatables (JJ)
% model version 17: Changed preferences (FL)
% model version 18: another pref (FL)
% model version 19: and another pref (FL)
% model version 20: more prefs (FL)
% model version 21: and another pref (FL)
% model version 22: Added chain with unique ID's for models (JL)
% model version 23: Added two other prefs (FL)
%NB VERSION 23 IS POST DYNALEARN SO IT DOES NOT FIT WITH DYNALEARN!
%So take care in merging!
gp_modelDefinitionVersion(26). %the version of the current definition.



%%
initialise(M,
	   N : name = [name]
	   ) :->
	%initialiseer de variabelen

	%gp3 1.4: translator slot
	TR *= translator,
	M->>translator(TR),

	default(N,'(noname)',Name),
	M->>slot(name,Name),
	gp_modelDefinitionVersion(Version),
	M->>modelDefinitionVersion(Version), %this one is current
	M->>slot(abstractEntities,new(chain)),
	M->>slot(assumptions,new(chain)),
	M->>slot(attributeDefinitions,new(chain)),
	M->>slot(configurationDefinitions,new(chain)),

        % for Sketch, AB, Aug 2006
	% M->>slot(sketchRelationDefinitions,new(chain)),
	% M->>slot(sketchStructuralRelationDefinitions,new(chain)),
	M->>slot(sketchProcessDefinitions,new(chain)),
	M->>slot(sketchAgentDefinitions,new(chain)),
	M->>slot(sketchScenarioDefinitions,new(chain)),
	M->>slot(sketches,new(chain)), % AB, feb 2006

	M->>slot(quantityDefinitions,new(chain)),
	M->>slot(quantitySpaces,new(chain)),
	M->>slot(modelFragments,new(chain)),

	%maak de quantity space voor afgeleiden aan
	%gp3 1.4: these are nontranslatable
	M->>slot(dqs,quantitySpace(mzp,chain(
				valueReference('Plus',interval,@off,TR),
				valueReference('Zero',point,@off,TR),
				valueReference('Min',interval,@off,TR)),
				'Fixed quantity space for derivatives (internal use only.)',
				translator := TR)),
	M?dqs?name_translatable->>setNonTranslatable, %make sure this one is never translated (unnecessary)
	M?dqs?remarks_translatable->>setNonTranslatable, %make sure this one is never translated (unnecessary)

	%en als kopie van wel zichtbare versie
	%gp3 1.4: the values in this one are also non-translatable, because this one is readonly
	M->>slot(mzpqs,quantitySpace(mzp,chain(
				valueReference('Plus',interval,@off,TR),
				valueReference('Zero',point,@off,TR),
				valueReference('Min',interval,@off,TR)),
				'Fixed quantity space for derivatives. If needed, you can use this one for quantities too. You cannot translate its values.',
				translator := TR)),
	M?mzpqs?name_translatable->>setNonTranslatable, %make sure this one is never translated (very necessary)
	M?mzpqs?remarks_translatable->>setNonTranslatable, %nothing translatable so it will not appear in languaeeditor

	%en die voegen we gewoon toe aan de qs-en. Deze kan niet verwijderd worden
	M->>insertDefinitionObject(M?mzpqs),

	%gp3: savedSimulations slot
	M->>savedSimulations(new(hash_table)),

	% for Sketches, savedSketches slot, analogous to savedSimulations, AB, nov 2006
	M->>savedSketches(new(hash_table)),

	%gp3: mfViews slot
	Views *= hash_table,
	%also add special entries @nil and 'Default view'
	Views->>append(@nil,new(hash_table)),
	Views->>append('Default view',new(hash_table)),
	M->>mfViews(Views),

	%gp3 0.3.13: metaData slot
	%code should be ok with empty hash table
	MetaData *= hash_table,
	send(M, slot, metaData, MetaData),
	%	gp3 0.3.14: we add the modelData metadata to save some (irrelevant) information
	ModelData *= hash_table,
	MetaData->>append(modelData,ModelData),
	ModelData->>append(creationTime,new(date)), %now
	ModelData->>append(creatorProgram,string('%s %s',@app?name,@app?version)),
	ModelData->>append(creationDefinition,Version), %model definition version at time of creation
	send(M, createMissingMetaDataFields),
	send(M, updateMetaDataFields),
	M->>changed(@off),
	M->+initialise,

      %Maak de basis topknopen aan
    %entity
    TopEntity *= entity('Entity', translator := TR),
	M?abstractEntities->>append(TopEntity),
	M->>hyper(TopEntity,topEntity),
       %agent
    TopAgent *= agent('Agent', translator := TR),
	M?abstractEntities->>append(TopAgent),
	M->>hyper(TopAgent,topAgent),
       %assumption
    TopAssumption *= assumption('Assumption', translator := TR),
	M?assumptions->>append(TopAssumption),
	M->>hyper(TopAssumption,topAssumption),
       %staticFragment
    TopStatic *= modelFragment('Static', translator := TR),
    TopStatic?name_translatable->>setNonTranslatable, %make sure this one is never translated
    TopStatic?remarks_translatable->>setNonTranslatable, %make sure this one is never translated
	M?modelFragments->>append(TopStatic),
	M->>hyper(TopStatic,topStaticFragment),
       %processFragment
    TopProcess *= modelFragment('Process', translator := TR),
    TopProcess?name_translatable->>setNonTranslatable, %make sure this one is never translated
    TopProcess?remarks_translatable->>setNonTranslatable, %make sure this one is never translated
	M?modelFragments->>append(TopProcess),
	M->>hyper(TopProcess,topProcessFragment),
       %agentFragment
    TopAgentFragment *= modelFragment('Agent', translator := TR), %gp3 changed name
    TopAgentFragment?name_translatable->>setNonTranslatable, %make sure this one is never translated
    TopAgentFragment?remarks_translatable->>setNonTranslatable, %make sure this one is never translated
	M?modelFragments->>append(TopAgentFragment),
	M->>hyper(TopAgentFragment,topAgentFragment),
        M->>initialise_sketch, % AB, feb 2006
	M->>initRunPrefs, %gp3 0.3

    % Create a new slot for unique IDs JL: Garp3 1.4.10 (29 May 2009)
    M->>slot(uniqueIDs,new(chain)).

%%
generateUniqueID(M, UniqueID:atom) :<-
    "Creates a unique ID based on the current filename and the modified date" ::
    % The first part of a unique ID is the file name
    get(M, name, ModelNameWithExtension),
    sub_string(ModelNameWithExtension, 0, _, 4, ModelName),
    % The second part of the unique ID is the creation date
    get(M?metaData, member, modelData, ModelData),
    get(ModelData, member, creationTime, CreatedDate),
    get(CreatedDate, posix_value, CreatedDateSeconds),
    format_time(atom(TimeUniqueID), '-created%G%m%dat%Hh%Mm%Ss', CreatedDateSeconds),
    atom_concat(ModelName, TimeUniqueID, UniqueID).
%%
currentUniqueID(M, UniqueID:atom) :<-
    "Get the current uniqueID of the model" ::
    get(M?uniqueIDs, tail, UniqueID).
%%
newUniqueID(M) :->
    "Saves a new Unique ID in the model (if it not equivalent to the previous ID)" ::
    get(M, generateUniqueID, UniqueID),
    (
	not( get(M?uniqueIDs, tail, UniqueID) ) ->
	send(M?uniqueIDs, append, UniqueID)
    ;
	true
    ).


/* DO NOT EDIT THIS FUNCTION, IT IS USED TO PATCH MODELS! */
%imho (JJ) this should be a versionPatch
createMissingMetaDataFields(M) :->
    get(M, metaData, MetaDataHash),

    % Create the general fields
   (
       get(MetaDataHash, member, general, General)
   ;
       new(General, hash_table), send(MetaDataHash, append, general, General)
    ),
    GeneralFields =
	[title,author,contributors,contact_email,keywords,domain,model_goals,
	intended_audience,model_version,known_model_limitations,language,
	bibliographic_citation,license],
    forall(
	member(GeneralField, GeneralFields),
	(
	    get(General, member, GeneralField, _GeneralFieldValue)
	;
	    new(String1, string('')),
	    send(General, append, GeneralField, String1)
	)
    ),

    % Create the model data fields
    (
	get(MetaDataHash, member, modelData, ModelData)
    ;
	new(ModelData, hash_table), send(MetaDataHash, append, modelData, ModelData)
    ),

    ModelDataFieldsString =
	[creatorProgram, creationDefinition],
    ModelDataFieldsDate =
	[creationTime],
    forall(
	member(ModelDataFieldString, ModelDataFieldsString),
	(
	    get(ModelData, member, ModelDataFieldString, _ModelDataFieldStringValue)
	;
	    new(String2, string('')),
	    send(ModelData, append, ModelDataFieldString, String2)
	)
    ),
    forall(
	member(ModelDataFieldDate, ModelDataFieldsDate),
	(
	    get(ModelData, member, ModelDataFieldDate, _ModelDataFieldDateValue)
	;
	    new(Date, date),
	    send(ModelData, append, ModelDataFieldDate, Date)
	)
    ),

    % Create remarks data fields
    (
	get(MetaDataHash, member, remarks, Remarks)
    ;
	new(Remarks, hash_table), send(MetaDataHash, append, remarks, Remarks)
    ),

    RemarksFields = [remarks, abstract],
    forall(
	member(RemarksField, RemarksFields),
	(
	    get(Remarks, member, RemarksField, _RemarksFieldValue)
	;
	    new(String3, string('')),
	    send(Remarks, append, RemarksField, String3)
	)
    ),
    % Create status hash
    (
	get(MetaDataHash, member, status, Status)
    ;
	new(Status, hash_table), send(MetaDataHash, append, status, Status)
    ),
    % create misc chain
    (
	get(MetaDataHash, member, misc, Misc)
    ;
	new(Misc, chain), send(MetaDataHash, append, misc, Misc)
    ).
%%

/* DO NOT EDIT THIS FUNCTION, IT IS USED TO PATCH MODELS! */
%imho (JJ) this should be a versionPatch
updateMetaDataFields(M) :->
    get(M, metaData, MetaDataHash),
    get(MetaDataHash, member, general, General),
    get(MetaDataHash, member, remarks, Remarks),

    (
	get(Remarks, member, model_goals, _ModelGoals)
    ;
        get(General, member, model_goals, OldModelGoals),
	send(Remarks, append, model_goals, OldModelGoals),
	send(General, delete, model_goals)
    ),
    (
	get(Remarks, member, intended_audience, _IntendedAudience)
    ;
        get(General, member, intended_audience, OldIntendedAudience),
	send(Remarks, append, intended_audience, OldIntendedAudience),
	send(General, delete, intended_audience)
    ),
    (
	get(Remarks, member, general_remarks, _GeneralRemarks)
    ;
        get(Remarks, member, remarks, OldGeneralRemarks),
	send(Remarks, append, general_remarks, OldGeneralRemarks),
	send(Remarks, delete, remarks)
    ),
    (
	not(get(MetaDataHash, member, misc, _Misc))
    ;
	get(MetaDataHash, member, misc, _MiscPresent),
	send(MetaDataHash, delete, misc)
    ).
%%

getModelNameForEditor(M, String, ModelNameForEditor) :<-
    get(M, name, ModelNameFull),
    (
	ModelNameFull \== '(noname)' ->
	sub_string(ModelNameFull, 0, _, 4, ModelName)
    ;
	ModelName = ModelNameFull
    ),
    %get(M, object_reference, ModelID),
    %sub_string(ModelID, 0, 5, Left, _),
    %Rsub_string(ModelID, 5, Left, _, ModelIDNumber),
    format(atom(ModelNameForEditor), '~w: ~w', [ModelName, String]).


% Anders: Delete me?
convert_old_slot(_M, Slot, OldRelDefs):->
    Slot = relationDefinitions,
    % I thought this could work, also for
    % sketchRelationDefinitions and sketchStructuralRelationDefinitions,
    % but that is now solved in versionPatch.pl
    %
    % convert value OldRelDefs for very old slot relationDefinitions in model
    % the old slot should not be used anymore
    writef('To Do: convert_old_slot relationDefinitions %d \n',[OldRelDefs]).
    % move them to the sketch to which they belong: the concept map
    % unfortunately, this does not work, because at this stage, the sketch hypers don't exist yet
    % get(M, hypered, conceptMapMF, CM),
    % not sure if this is possible to convert to directly...
    % send(CM, slot, sketchRelationDefinitions, OldRelDefs),
    % writef('convert_old_slot relationDefinitions done. \n',[]).
%%


% for Sketch, AB, Aug 2006
% initialise_sketch(M):->
%
% initialise the sketch part of the model.
% currently, the chain sketches includes:
% - conceptMap
% - structuralModel
% - causalModel
% - stgraph
% Each of these include a dummy (process) model fragment
%
initialise_sketch(M):->
    % AB, feb 2006
    new(ConceptMapMF, conceptMap('ConceptMapMF')),
    new(StructureMF, structuralModel('StructureMF')),
    new(CausalModelMF, causalModel('CausalModelMF')), % AB, march 2006
    new(STGraphMF, stgraph('STGraphMF')), % AB, april 2006
    % new(ConceptMapMF, modelFragment('ConceptMapMF')),
    % new(CausalModelMF, modelFragment('CausalModelMF')),
    send(M?sketches, append, ConceptMapMF),
    send(M?sketches, append, StructureMF),
    send(M?sketches, append, CausalModelMF),
    send(M?sketches, append, STGraphMF),
    M->>hyper(ConceptMapMF,conceptMapMF),
    M->>hyper(StructureMF,structureMF),
    M->>hyper(CausalModelMF,causalModelMF),
    M->>hyper(STGraphMF,stgraphMF),
    % Maybe there should be an intermediate level of different sketches, i.e.
    % M?sketches->>append(ConceptMap),
    % M?sketches?conceptMap->>append(ConceptMapMF),
    % Add top level sketchConcept to model (or to concept map?)

    %gp3 1.4.0 (JJ) for some reason these object are subclasses of abstractEntity, so also of hierarchicalObject (apparently a sketchQuantity is a hierarchical object?)
    %. So we have to add the translator here.
    %(@model is not ready yet)
    TopSketchConcept *= sketchConcept('SketchConcept', translator := M?translator),
    M?abstractEntities->>append(TopSketchConcept),
    M->>hyper(TopSketchConcept,topSketchConcept),

    TopSketchObject *= sketchObject('SketchObject', translator := M?translator),
    M?abstractEntities->>append(TopSketchObject),
    M->>hyper(TopSketchObject,topSketchObject),

    TopSketchQuantity *= sketchQuantity('SketchQuantity', translator := M?translator),
    M?abstractEntities->>append(TopSketchQuantity),
    M->>hyper(TopSketchQuantity,topSketchQuantity),

    TopSketchState *= sketchState('SketchState', translator := M?translator),
    M?abstractEntities->>append(TopSketchState),
    M->>hyper(TopSketchState,topSketchState),

    % initialize the list of structural relation definitions
    % M->>init_structural_relation_definitions.
    StructureMF->>init_structural_relation_definitions.


%%
unlink(M):->
	%zorg ervoor dat alle objecten bevrijd worden
	%gp3 1.4: because of incompatibilities between model version and current code
	%we wrap this one in a try/catch block
	catch((
		M?abstractEntities->>for_all(->>(@arg1,free)),
		M?assumptions->>for_all(->>(@arg1,free)),
		M?modelFragments->>for_all(->>(@arg1,free)),
		M?attributeDefinitions->>for_all(->>(@arg1,free)),
		M?configurationDefinitions->>for_all(->>(@arg1,free)),
		M?quantityDefinitions->>for_all(->>(@arg1,free)),
		M?quantitySpaces->>for_all(->>(@arg1,free)),

	        % for Sketch, AB, Aug 2006
		% M?sketchRelationDefinitions->>for_all(->>(@arg1,free)),
		% M?sketchStructuralRelationDefinitions->>for_all(->>(@arg1,free)),
		M?sketchProcessDefinitions->>for_all(->>(@arg1,free)),
		M?sketchAgentDefinitions->>for_all(->>(@arg1,free)),
		M?sketchScenarioDefinitions->>for_all(->>(@arg1,free)),
		M?sketches->>for_all(->>(@arg1,free)), % AB, feb 2006

		%for translator, JJ
		M?translator->>free
	),_,true), %never mind exceptions, just go on
	M->+unlink.
%%



%%
versionPatch(M):->
	"Perform some patches for minor version changes" ::
	%legacy homer, but also gp3 (version numbers are homer except when explicitly called gp3).
	%garp3 made this more general
	%aangeroepen na het inlezen van een saved object (zie mainMenu->load)

	%we know the model version number (M?modelDefinitionVersion) and
	%the current one (@modelDefinitionVersion).
	%for all values between those 2, we call pl_VersionPatch
	%applicatin should check on the minimal model definition available

	gp_modelDefinitionVersion(CurrentVersion),
	ModelVersion = M<<-modelDefinitionVersion,

	/* If the loaded model has a higher version than the current version
	*  saving the model will cause model corruption
	*/
	(
	    ModelVersion > CurrentVersion ->
	    send(@app?mainMenu, msgBox(string('Warning: This model was saved in a later version of Garp3. Saving the model in this version will likely cause model corruption. Please upgrade to the newest version of Garp3 (http://www.garp3.org).'),alarm))
	;
	    true
	),

	%does the model have a version anyway?
	if
		ModelVersion = @nil
	then
		MV = 0
	else
		MV = ModelVersion,

	if
		MV < CurrentVersion then
	(
		@pce->>write_ln('\n\n ***',@app?name,@app?version,' MODEL PATCH ***\n'),
		@pce->>write_ln('The loaded model has definition version',MV,'. Patching it to current definition version',CurrentVersion,'.\n\n'),
		% JL: Already loaded during startup, causes errors on linux
		%ensure_loaded(main(versionpatch)), %special file, only loaded when needed (gp3 0.3.11)
		pl_doVersionPatch(M,CurrentVersion,MV),
		%the model is patched now
		M->>modelDefinitionVersion(CurrentVersion)
	).

%%
doSaveInFile(_M,
	F: file) :->
	%Toegevoegd versie 2.03
        %tussenlaag voor save_in_file
        %check via de app even of er nog editors zijn die hun layout moeten opslaan

        @app?members->>for_all(if(->>(@arg1,has_send_method,saveLayoutInfo),
                                                        ->>(@arg1,saveLayoutInfo))),

        % Make sure the model is saved as @model
        get(@model, '_value', RealModel),
        get(RealModel, object_reference, RealModelName),
        send(@model, assign, @nil, global),
        send(@model, '_free'),
        ModelName = 'model',
        send(RealModel, name_reference, ModelName),
        get(@pce, object_from_reference, ModelName, Model),

        % Save the renamed model
        Model->>save_in_file(F),

        % Recreate @model and set it to the old
        send(Model, name_reference, RealModelName),
        new(@model, var),
        send(@model, assign, RealModel, global),

        % Correct the name of the tab
        get(F, base_name, NewModelTabName),
        get(@app?tabTool, on_top, CurrentModelTab),
        send(CurrentModelTab, realTabName, NewModelTabName). %WHY is this in the model definition instead of in the interface code?
%%

%%
lastChangeTimeValue(M, V: real):<-
	%gp3 0.3: return the posix value of the lastChangeTime
	%or zero if never changed

	if
		@nil = M<<-lastChangeTime
	then
		V = 0
	else
		V = M?lastChangeTime<<-posix_value.
%%

%%
allRelevantElements(M,
	Elements: chain):<-

		%gp3 1.4
		%return a chain of all relevant element
		%relevant as in: needed for change requests, translator requests etc

		Elements *= chain,

		%all objects (copied from changeRequest below, which now uses this call)
		%we do not use any member check or union call: it should be unique already

		 get(M, hypered, conceptMapMF, ConceptMap),
       get(M, hypered, structureMF, StructuralModel),
       get(M, hypered, causalModelMF, CausalModel),
       get(M, hypered, stgraphMF, STGraph),

		chain(
			M, %eerst naar onszelf
			M?abstractEntities,
			M?assumptions,
			M?quantitySpaces,
			M?modelFragments,
			M?quantityDefinitions,
			M?attributeDefinitions,
			M?configurationDefinitions,

			% for Sketch, AB, aug 2006
			% M?sketchRelationDefinitions,
			% M?sketchStructuralRelationDefinitions,
			M?sketchProcessDefinitions,
			M?sketchAgentDefinitions,
			M?sketchScenarioDefinitions,
	                ConceptMap,
	                StructuralModel,
	                CausalModel,
	                STGraph

	       )->>for_all(
		if(->>(@arg1,instance_of,chain),
			->>(Elements,merge,@arg1),   %use merge and append to make sure we keep the order from above list
			->>(Elements,append,@arg1)
			)
		).

%%

%%
changeRequest(M,
	      T : type = name,
	      O : object = object,
	      E: editor = [any*],
	      A : arguments = any ... %prolog lijst
	      ) :->
	"Post change request and handle change. Fails if change not applied" ::
	%dit is de hoofdcall voor changeRequest handling
	%maar de change requestor handelt het verder af

	%die prolog lijst met variabele argumenten moet weer als varargs door
	%naar de changeRequestor, dus moeten we de call dynamisch bouwen

	CRCall =.. [changeRequestor,T,O,E,M|A], % Add a model to the changeRequestor
	CR *= CRCall,

	CR->>go(M?allRelevantElements), %go faalt bij niet apply

	%was this a model fragment or input system change?
	if
	(
		catch(O->>instance_of(modelFragment),_,fail) %mf or is, catch non-existant
	)
	then
		M->>setLastEditedMF(O),
	M->>setChanged.

%%

%%
changeRequestEx(M,
	T: type = name,
	O: object = object,
	Att: attributes = 'hash_table|chain',
	E: editor = [any*],
	A: arguments = any ...) :->

	/*
	gp3 1.4: new extended version of changeRequest. Does the same thing as changeRequest
	only 1 extra argument: attributes, which is a hash_table containing extra information (designed for extendability)
	It can also be a chain for easy access: Every odd element is a key, followed by (the even element) the value
	Current supported features in attributes:

	Key: silent	Value: bool	Default: @off: sets the ?silent property of the changerequestor (when @on: no changeApplied and changeTreeApplied calls). (should be: noupdates, but whel)
	Key: nofeedback Value: bool Default: @off. Sets the ?nofeedback property of the changerequestor (when @on: user is not informed when an action is impossible, and not asked to confirm sub changes, they are just carried out).
	*/

	CRCall =.. [changeRequestor,T,O,E,M|A],
	CR *= CRCall,

	if
		Att->>instance_of(hash_table)
	then
		Attribs = Att
	else
	(
		Attribs *= hash_table,
		%map the chain (key, value, ...) to a hash_table
		while(->>(Attribs,append,Att?delete_head,Att?delete_head))->>forward
	),
	%switches in attributes
	if
		@on = Attribs<<-member('silent')
	then
		CR->>silent(@on)
	else
		CR->>silent(@off),
	if
		@on = Attribs<<-member('nofeedback')
	then
		CR->>nofeedback(@on)
	else
		CR->>nofeedback(@off),
	%end of switches

	CR->>go(M?allRelevantElements), %go faalt bij niet apply

	%was this a model fragment or input system change?
	if
	(
		catch(O->>instance_of(modelFragment),_,fail) %mf or is, catch non-existant
	)
	then
		M->>setLastEditedMF(O),
	M->>setChanged.

%%
setChanged(M):->
	%gp3 0.3: helper for changeRequest and other places: set
	%the model state to changed, sending @app a message etc

	M->>changed(@on),
	%M->>slot(lastChangeTime,new(date)), %save when last changed
	send(M?lastChangeTime, current),
	@app->>modelChanged.
%%

%%
lastEditedIS(G,IS: inputSystem):<-
	%gp3 0.1: return last edited input system, fail if not available
	IS = G<<-hypered(lastEditedIS).
%
lastEditedMF(G,MF: modelFragment):<-
	%gp3 0.1: return last edited model fragment, fail if not available
	MF = G<<-hypered(lastEditedMF).
%
setLastEditedMF(G,MF: modelFragment):->
	%gp3 0.1: set MF as the last edited one, as requested by the object in an applychange
	%or generally in the changeRequest method above (the only one used now)
	%should be called before @app->>modelChanged (last call in changeRequest), because
	%the garp3Menu uses this information
	%MF is either a model fragment or an input system (sub class), so we do a type check

	if
		MF->>isInputSystem
	then
		Hypername = lastEditedIS
	else
		Hypername = lastEditedMF,
	G->>delete_hypers(Hypername),
	G->>hyper(MF,Hypername).

%%
insertDefinitionObject(M, O:object):->
	"Insert a definition object into this model" ::
	%dat gaat dus per klasse
	(   ( O->>instance_of(entity)
	    ; O->>instance_of(agent)
	    )
	->  M?abstractEntities->>add(O)
	;   O->>instance_of(assumption)
	->  M?assumptions->>add(O)
	;   O->>instance_of(garpQuantityDefinition)
	->  M?quantityDefinitions->>add(O)
	;   O->>instance_of(garpAttributeDefinition)
	->  M?attributeDefinitions->>add(O)
	;   O->>instance_of(configurationDefinition)
	->  M?configurationDefinitions->>add(O)
	;   O->>instance_of(quantitySpace)
	->  M?quantitySpaces->>add(O)
	;   O->>instance_of(modelFragment)
	->  M?modelFragments->>add(O)

	    % for Sketch, AB, Aug 2006
%	;   O->>instance_of(sketchRelationDefinition)
%	->  M?sketchRelationDefinitions->>add(O)
%	;   O->>instance_of(sketchStructuralRelationDefinition)
%	->  M?sketchStructuralRelationDefinitions->>add(O)
	;   O->>instance_of(sketchProcessDefinition)
	->  M?sketchProcessDefinitions->>add(O)
	;   O->>instance_of(sketchAgentDefinition)
	->  M?sketchAgentDefinitions->>add(O)
	;   O->>instance_of(sketchScenarioDefinition)
	->  M?sketchScenarioDefinitions->>add(O)

	).
%%



%%
removeDefinitionObject(M,
		       O: object
		      ):->
	"Remove a definition object from this model" ::
	%we proberen het in alle lijsten en falen als het
	%in geen van allen is

	M?abstractEntities->>delete(O)
	;
	M?assumptions->>delete(O)
	;
	M?quantityDefinitions->>delete(O)
	;
	M?attributeDefinitions->>delete(O)
	;
	M?configurationDefinitions->>delete(O)
	;
	M?quantitySpaces->>delete(O)
	;
	M?modelFragments->>delete(O)
	;
	% for Sketch, AB, Aug 2006
%	M?sketchRelationDefinitions->>delete(O)
%	;
%	M?sketchStructuralRelationDefinitions->>delete(O)
%	;
	M?sketchProcessDefinitions->>delete(O)
	;
	M?sketchAgentDefinitions->>delete(O)
	;
	M?sketchScenarioDefinitions->>delete(O).
%%

%%
deleteFromMFViews(M,MF: modelFragment):->
	%gp3: request from a modelfragment to delete it from the mfViews tables
	%called in the applyChange of MF deletion
	%We are so kind as to do this

	M?mfViews->>for_some(->>(@arg2,delete,MF)).
%%

%%
initRunPrefs(M, PrefsHash: [hash_table]):->
	%gp3 0.3: initialise the runPrefs hash and default values
	%defaults same as in engine/als_assumptions_garp3.pl
	%gp3 0.4.6: if PrefsHash is given we will fill that hash with the defaults, but not set the
	%real runPrefs member. Used by runprefsDialog to reset to defaults

	if
		PrefsHash = @default
	then
		RP *= hash_table
	else
		RP = PrefsHash, %just create alias
    RP->>append(cw_assumption,@off),
    RP->>append(cw_assumption_second_order,@off),
    RP->>append(free_maxmin_derivative,@on),
    RP->>append(free_zero_derivative,@off),
    RP->>append(equal_qspace_points,@off),
    RP->>append(value_branching,@off),
    RP->>append(order_using_correspondences,@on),
    RP->>append(epsilon_ordering,@on),
    RP->>append(epsilon_merging,@on),
    RP->>append(epsilon_derivative_constraints,@on),

    RP->>append(reasoning_assumptions,@on),
    RP->>append(assume_conditional_derivatives,@on),
    RP->>append(max_depth, 0),
    RP->>append(second_order_proportionality_propagation,@off),
    RP->>append(order_epsilon_last,@off),

    RP->>append(order_using_equalities,@on),
    RP->>append(full_branching_value_terminations,@off),
    RP->>append(full_branching_equality_terminations,@off),
    %RP->>append(no_subsumption_specify_all,@on),
    RP->>append(use_landmarks,@off),
    %RP->>append(use_old_transition_rules,@off),
    RP->>append(terminate_weak_relations,@off),
    RP->>append(remove_corresponding_equality,@off),
    RP->>append(extra_termination_interpreter,@off),
    RP->>append(remove_inactive_quantities,@off),
    RP->>append(apply_continuity_d_inequalities,@on),
    RP->>append(equal_intervals, @off),
    RP->>append(fast_path, @off),
    RP->>append(full_branching_derivative_terminations, @off),
    RP->>append(solve_extra_combinations, @on),
    %RP->>append(allow_d_assumptions_in_reclassifying_mfs,@off),
    %RP->>append(no_analyse_zero,@off), FL option removed

    % Comparative analysis
    RP->>append(comparative_analysis, @off),
    RP->>append(comparative_analysis_on_proportionalities, @off),
    RP->>append(comparative_analysis_equal_target_quantity_type, @off),
    RP->>append(comparative_analysis_equal_source_quantity_type, @off),
    RP->>append(comparative_analysis_similar_target_entity_type, @off),
    RP->>append(comparative_analysis_similar_source_entity_type, @off),
    RP->>append(comparative_analysis_equal_causal_dependency_sign, @off),

    %different from modelling defaults:
    RP->>append(second_order_derivatives,@off),
    RP->>append(derivative_terminations,@off),
    RP->>append(second_order_continuity,@off),

    % third order
    RP->>append(third_order_derivatives,@off),
    RP->>append(third_order_proportionality_propagation,@off),
    RP->>append(cw_assumption_third_order,@off),



    if
	PrefsHash = @default
    then
	%do the real thing
	M->>runPrefs(RP).
%%
initAdvancedRunPrefs(M, PrefsHash: [hash_table]):->
	%gp3 0.3: initialise the runPrefs hash and default values
	%defaults same as in engine/als_assumptions_garp3.pl
	%gp3 0.4.6: if PrefsHash is given we will fill that hash with the defaults, but not set the
	%real runPrefs member. Used by runprefsDialog to reset to defaults

	if
		PrefsHash = @default
	then
		RP *= hash_table
	else
		RP = PrefsHash, %just create alias
    RP->>append(cw_assumption,@off),
    RP->>append(cw_assumption_second_order,@off),
    RP->>append(free_zero_derivative,@off),
    RP->>append(equal_qspace_points,@off),
    RP->>append(value_branching,@off),
    RP->>append(order_using_correspondences,@on),
    RP->>append(epsilon_ordering,@on),
    RP->>append(epsilon_merging,@on),
    RP->>append(epsilon_derivative_constraints,@on),

    RP->>append(reasoning_assumptions,@on),
    RP->>append(assume_conditional_derivatives,@on),
    RP->>append(max_depth, 0),
    RP->>append(second_order_proportionality_propagation,@off),
   RP->>append(order_epsilon_last,@off),

    RP->>append(order_using_equalities,@on),
    %RP->>append(no_subsumption_specify_all,@on),
    RP->>append(use_landmarks,@off),
    %RP->>append(use_old_transition_rules,@off),
    RP->>append(terminate_weak_relations,@off),
    RP->>append(remove_corresponding_equality,@off),
    RP->>append(extra_termination_interpreter,@off), %obsolete, can we remove here? FL
    RP->>append(remove_inactive_quantities,@off),
    RP->>append(apply_continuity_d_inequalities,@on),
    RP->>append(equal_intervals, @off),
    RP->>append(fast_path, @off),
    RP->>append(solve_extra_combinations, @on),
    %RP->>append(allow_d_assumptions_in_reclassifying_mfs,@off),
    %RP->>append(no_analyse_zero,@off), FL option removed

    % third order
    RP->>append(third_order_derivatives,@off),
    RP->>append(third_order_proportionality_propagation,@off),
    RP->>append(cw_assumption_third_order,@off),

    % Comparative analysis
    RP->>append(comparative_analysis, @off),
    RP->>append(comparative_analysis_on_proportionalities, @off),
    RP->>append(comparative_analysis_equal_target_quantity_type, @off),
    RP->>append(comparative_analysis_equal_source_quantity_type, @off),
    RP->>append(comparative_analysis_similar_target_entity_type, @off),
    RP->>append(comparative_analysis_similar_source_entity_type, @off),
    RP->>append(comparative_analysis_equal_causal_dependency_sign, @off),

    % different from trainer default: FL
    RP->>append(free_maxmin_derivative,@off),
    RP->>append(full_branching_derivative_terminations, @on),
    RP->>append(full_branching_value_terminations,@on),
    RP->>append(full_branching_equality_terminations,@on),

    RP->>append(second_order_derivatives,@on),
    RP->>append(derivative_terminations,@on),
    RP->>append(second_order_continuity,@on),


    if
	PrefsHash = @default
    then
	%do the real thing
	M->>runPrefs(RP).


%%
changeRunPrefs(M,NewPrefs: hash_table):->
	%gp3 0.3: update the runPrefs using the values in the NewPrefs table
	%when done this way from a dialog, the model will check if there is
	%a change, and call @app->>modelChanged if needed

	RP = M<<-runPrefs,
	if
		NewPrefs<<-find_key(@arg2 \== ?(RP,member,@arg1))
	then
	(
		%something changed, update the runprefs, make sure no new keys are added
		NewPrefs->>for_all(if(?(RP,member,@arg1),
						->>(RP,append,@arg1,@arg2))),
		M->>setChanged %does the rest
	).
%%

%%%%%%%%%%%%%%%%%%GESORTEERDE LIJST KOPIEEN%%%%%%%%%%

%%
sortedQuantityDefinitions(M,
	C : chain
	):<-
	"Return a name-sorted copy of the quantityDefinitions variable" ::

	C = M?quantityDefinitions<<-copy,
	C->>sort(?(@arg1?name,compare,@arg2?name)).
%%

%%
sortedQuantitySpaces(M,
	C : chain
	):<-
	"Return a name-sorted copy of the quantitySpaces variabled" ::

	C = M?quantitySpaces<<-copy,
	C->>sort(?(@arg1?name, compare, @arg2?name)).
%%

%%
sortedAttributeDefinitions(M,
	C : chain
	):<-
	"Return a name-sorted copy of the attributeDefinitions variable" ::

	C = M?attributeDefinitions<<-copy,
	C->>sort(?(@arg1?name,compare,@arg2?name)).
%%
sortedConfigurationDefinitions(M,
	C : chain
	):<-
	"Return a name-sorted copy of the configurationDefinitions variable" ::

	C = M?configurationDefinitions<<-copy,
	C->>sort(?(@arg1?name,compare,@arg2?name)).
%%

%%
sortedEntities(M, E: chain):<-
	%gp3 1.4 JJ
	E = M?abstractEntities<<-find_all(->>(@arg1,instance_of,entity)),
	E->>sort(?(@arg1?name,compare,@arg2?name)).
%%

%%
sortedAgents(M, E: chain):<-
	%gp3 1.4 JJ
	E = M?abstractEntities<<-find_all(->>(@arg1,instance_of,agent)),
	E->>sort(?(@arg1?name,compare,@arg2?name)).
%%

%%
sortedAssumptions(M, E: chain):<-
	%gp3 1.4 JJ
	E = M?assumptions<<-copy,
	E->>sort(?(@arg1?name,compare,@arg2?name)).
%%

%%
modelFragmentsOnly(M, C: chain):<-
	%gp3 1.4 jj
	%only the model fragments, not the input systems
	C = M?modelFragments<<-find_all(@arg1?class_name == 'modelFragment').
%%

%%
sortedModelFragmentsOnly(M, C: chain):<-
	C = M<<-modelFragmentsOnly,
	C->>sort(?(@arg1?name,compare,@arg2?name)).
%%

%%
sortedInputSystems(M,
	I: chain):<-
	"Find all input systems in the model and return them sorted" ::

	I = M<<-inputSystems,
	I->>sort(?(@arg1?name,compare,@arg2?name)).
%%
inputSystems(M,
	I: chain):<-
		I = M?modelFragments<<-find_all(->>(@arg1,instance_of,inputSystem)).
%%

% for Sketch, AB, Aug 2006
%%
sortedSketchProcessDefinitions(M,
	C : chain
	):<-
	"Return a name-sorted copy of the sketchProcessDefinitions variable" ::

	C = M?sketchProcessDefinitions<<-copy,
	C->>sort(?(@arg1?name,compare,@arg2?name)).
%%


%%
sortedSketchAgentDefinitions(M,
	C : chain
	):<-
	"Return a name-sorted copy of the sketchAgentDefinitions variable" ::

	C = M?sketchAgentDefinitions<<-copy,
	C->>sort(?(@arg1?name,compare,@arg2?name)).
%%


%%
sortedSketchScenarioDefinitions(M,
	C : chain
	):<-
	"Return a name-sorted copy of the sketchScenarioDefinitions variable" ::

	C = M?sketchScenarioDefinitions<<-copy,
	C->>sort(?(@arg1?name,compare,@arg2?name)).
%%


%%
sortedSketchConcepts(M,
	C : chain
	):<-
	"Return a name-sorted chain of all Sketch Concepts" ::
        get(M, hypered, conceptMapMF, CM),
        get(CM?elements, find_all, message(@arg1, instance_of, sketchConceptElement), SketchConcepts),
	C = SketchConcepts<<-copy,
	C->>sort(?(@arg1?name,compare,@arg2?name)).
%%


%%
sortedSketchObjects(M,
	C : chain
	):<-
	"Return a name-sorted chain of all Sketch Objects" ::
        get(M, hypered, structureMF, SM),
        get(SM?elements, find_all, message(@arg1, instance_of, sketchObjectElement), SketchObjects),
	C = SketchObjects<<-copy,
	C->>sort(?(@arg1?name,compare,@arg2?name)).
%%

%%
sortedSketchObjectsTyped(M,
        Type,
	C : chain
	):<-
	"Return a name-sorted chain of all Sketch Objects of Type" ::
        get(M, hypered, structureMF, SM),
        get(SM?elements, find_all, message(@arg1, instance_of, sketchObjectElement), SketchObjects),
        get(SketchObjects, find_all, message(@arg1?type, equal, Type), SketchTypedObjects),
	C = SketchTypedObjects<<-copy,
	C->>sort(?(@arg1?name,compare,@arg2?name)).
%%

%
sortedSketchDefinitionQuantities(M,
        SourceSketchType,
        C: chain
        ):<-
	"Return a name-sorted chain of all Sketch quantities from SourceSketch" ::
        swritef(DefTypeStr, 'sketch%wDefinitions', [SourceSketchType]),
        string_to_atom(DefTypeStr, DefType),
        get(M, DefType, Defs),
        new(C, chain),
        send(Defs, for_all,
             message(C, union, @arg1?def_sheet?quantity?copy)
        ),
	C->>sort(?(@arg1?name,compare,@arg2?name)).
%%



%%%%%%%%%%%%%%%%%%VIND ALLE..%%%%%%%%%%%%%%%%%%%%%%%
%%
instances(M,
	     A : chain
	    ):<-
	"Find all instances in the model" ::

	%doen we vanuit entities
	A *= chain,
	M?abstractEntities->>for_all(->>(A,
					 merge,
					 @arg1?instances
					) ).
%%

%%
quantities(M,
	      A : chain
	     ):<-
	"Find all quantities in the model" ::
	%niet gesorteerd ofzo

	A *= chain,
	M?quantityDefinitions->>for_all(->>(A,
					merge,
					@arg1?quantities)).
%%

%%
attributes(M,
	A : chain):<-
	"Find all garpAttributes in the model" ::

	A *= chain,
	M?attributeDefinitions->>for_all(->>(A,merge,@arg1?attributes)).
%%

%%%%%%%%%%%%%%% TOPOBJECTEN %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
topEntity(M,
	E: entity):<-
	%geef het root entity object (via hyper)

	E = M<<-hypered(topEntity).
%
topAgent(M,
	A: agent):<-
	%geef het root agent object (via hyper)

	A = M<<-hypered(topAgent).
%
topAssumption(M,
	A: assumption):<-
	%geef het root assumption object (via hyper)

	A = M<<-hypered(topAssumption).
%
topAgentFragment(M,
	AF: modelFragment):<-
	%geef het root agentmf object (via hyper)

	AF = M<<-hypered(topAgentFragment).
%
topProcessFragment(M,
	PF: modelFragment):<-
	%geef het root processmf object (via hyper)

	PF = M<<-hypered(topProcessFragment).
%
topStaticFragment(M,
	SF: modelFragment):<-
	%geef het root staticmf object (via hyper)

	SF = M<<-hypered(topStaticFragment).
%%


%%%%%%%%%%%%%%%%CHANGEREQUESTORS%%%%%%%%%%%%%%%%%%%%%%%%
%Eerst waar alleen de controle op correcte naam-syntax hoeft
%%
checkChange_newHObjectChild(M,
			    CR : changeRequestor
			   ):->
	%check de naam van het kind
	%gp3 0.3: also check if there is a number in the name: impossible

	if
		new(regex('[0-9]'))->>search(CR?arg1)
	then
		CR->>impossible(string('Numbers are not allowed in the name of an %s.',CR?object?class_name)),
	CR->>checkImpossibleName(1,M).

%%

%%
checkChange_changeHObject(M,
			      CR : changeRequestor
			     ):->
	%gp3 0.3: also check if there is a number in the name: impossible

	if
		new(regex('[0-9]'))->>search(CR?arg1)
	then
		CR->>impossible(string('Numbers are not allowed in the name of an %s.',CR?object?class_name)),

	CR->>checkImpossibleName(1,M).
%%

%%
checkChange_newFInstance(M,
			 CR : changeRequestor
			):->
	CR->>checkImpossibleName(3,M).
%%

%%
checkChange_changeFInstance(M,
			    CR : changeRequestor
			   ):->
	CR->>checkImpossibleName(4,M).
%%

%%
% for Sketch, AB, Aug 2006

%%
% AB, feb 2006
checkChange_newFConcept(M,
			 CR : changeRequestor
			):->
	CR->>checkImpossibleName(2,M). % was 3, AB, feb 2006
%%

%%
checkChange_changeFConcept(M,
			    CR : changeRequestor
			   ):->
	CR->>checkImpossibleName(3,M).% was 4, AB, feb 2006
%%



%%
% AB, june 2006
checkChange_newFSketchObject(M,
			 CR : changeRequestor
			):->
	CR->>checkImpossibleName(2,M).
%%

%%
checkChange_changeFSketchObject(M,
			    CR : changeRequestor
			   ):->
	CR->>checkImpossibleName(3,M).
%%



%%
% AB, feb 2006
checkChange_newSketchQuantity(M,
			 CR : changeRequestor
			):->
	CR->>checkImpossibleName(2,M). % was 3, AB, feb 2006
%%

%%
checkChange_changeSketchQuantity(M,
			    CR : changeRequestor
			   ):->
	CR->>checkImpossibleName(3,M).% was 4, AB, feb 2006
%%


%%%%%%%%%%%en nu de complexere + applies


%%
%newMF: een nieuw modelfragment
checkChange_newMF(M,
	CR: changeRequestor):->
	%de naam moet kunnen, maar er zijn ook parent regels
	%
	CR->>checkImpossibleName(1,M),
	M->>normReservedMFNames(CR?arg1,CR), %geen gereserveerde naam?

	Parents = CR<<-arg3,
	if
		Parents->>empty
	then
		CR->>impossible('The model fragment has no parent, it should have at least one',M),
	CheckChain *= chain,
	chain_list(Parents,PL),
	pl_checkParents(PL,CheckChain,@nil,M,CR),
	CheckChain->>done.
%
applyChange_newMF(M,
			   CR: changeRequestor
			  ):->
	%we maken een nieuw modelfragment

	MF *= modelFragment(CR?arg1,CR?arg2),
	CR?arg3->>for_all(->>(MF,addParent,@arg1)), %parents
	MF->>active(CR?arg4), %active? gp3 0.3.11
	M->>insertDefinitionObject(MF),
	%en dit resultaat slaan we op in de changeRequestor
	CR->>result(MF).
%

%%changeMF: een gewijzigd modelfragment, ook naam + inheritance regels checken
checkChange_changeMF(M,
	CR: changeRequestor):->
	%de naam moet kunnen, maar er zijn ook parent regels
	%
	CR->>checkImpossibleName(1,M),
	Parents = CR<<-arg3,
	if
		Parents->>empty
	then
		CR->>impossible('The model fragment has no parent, it should have at least one',M),
	CheckChain *= chain,
	chain_list(Parents,PL),
	pl_checkParents(PL,CheckChain,@nil,M,CR),
	CheckChain->>done.
%
%pl_checkParents: helper voor checkChange_newMF en checkChange_changeMF
pl_checkParents([],_,_,_,_).
pl_checkParents([P|Rest],CheckChain,O,M,CR):-
	R = M<<-norm_FragmentInheritance(P,CheckChain,O),
	pl_checkParentsResult(R,P,M,CR),
	CheckChain->>append(P),
	pl_checkParents(Rest,CheckChain,O,M,CR).
%
pl_checkParentsResult(ok,_P,_M,_CR).
pl_checkParentsResult(isObject,_P,M,CR):-
	CR->>impossible('The model fragment has itself as a parent',M).
pl_checkParentsResult(isChild,P,M,CR):-
	Feedback *= string('The parent "%s" is already a (grand-)child of the model fragment',P?name),
	CR->>impossible(Feedback,M).
pl_checkParentsResult(isParent,P,M,CR):-
	Feedback *= string('The parent "%s" is already a (grand-)parent of the model fragment',
						P?name),
	CR->>impossible(Feedback,M).
pl_checkParentsResult(isParentChild,P,M,CR):-
	Feedback *= string('The parent "%s" is a (grand-)child of one of the parents of the model fragment',P?name),
	CR->>impossible(Feedback,M).
pl_checkParentsResult(isChildParent,P,M,CR):-
	Feedback *= string('The parent "%s" is already a parent of one of the children of the model fragment',P?name),
	CR->>impossible(Feedback,M).
%%

%%
%newInputSystem: een nieuw inputSystem
checkChange_newInputSystem(M,
	CR: changeRequestor):->
	%de naam moet kunnen
	%
	CR->>checkImpossibleName(1,M).
%
applyChange_newInputSystem(M,
			   CR: changeRequestor
			  ):->
	%we maken een nieuw inputsystem

	MF *= inputSystem(CR?arg1,CR?arg2), %speciaal type modelFragment
	M->>insertDefinitionObject(MF),
	%en dit resultaat slaan we op in de changeRequestor
	CR->>result(MF).
%%

%%
checkChange_changeInputSystem(M,
	CR: changeRequestor):->
	%de naam moet kunnen
	%
	CR->>checkImpossibleName(1,M).
%%

%%
applyChange_deleteQuantityDef(M,
				CR: changeRequestor
	):->
	%verwijdering van de quantity def
	Del = CR<<-argument(1),
	M->>removeDefinitionObject(Del),
	CR->>freeObject(Del). %CR zal het object verwijderen als het niet meer nodig is (na changeApplied)
%%

%%
checkChange_addQuantityDef(M,
	CR:changeRequestor):->
	%check of de gegevens voor de nieuwe quantity def ok zijn
	%gedelegeerd naar een norm methode omdat het hetzelfde is als bij changeQuantityDef

	M->>norm_quantityDef(CR,M).
%
applyChange_addQuantityDef(M,
	CR:changeRequestor):->
	%maak de quantitydef en voeg hem toe aan de definities

	NewQD *= garpQuantityDefinition(?(CR,argument,1),
				?(CR,argument,2),
				?(CR,argument,3)), %doorsturen
	M->>insertDefinitionObject(NewQD),
	CR->>result(NewQD).
%%

%%
checkChange_addAttributeDef(M,
	CR:changeRequestor):->
	%check of de gegevens ok zijn

	M->>norm_attributeDef(CR,M).
%
applyChange_addAttributeDef(M,
	CR : changeRequestor):->
	%maak de attribute def aan

	NewAD *= garpAttributeDefinition(?(CR,argument,1),
				?(CR,argument,2), ?(CR,argument,3)), %maakt hem aan
	M->>insertDefinitionObject(NewAD),
	CR->>result(NewAD).
%%

%%
applyChange_deleteAttributeDef(M,
	CR : changeRequestor):->
	%Verwijder de attribute def

	AD = CR<<-argument(1),
	M->>removeDefinitionObject(AD),
	CR->>freeObject(AD). %wordt verwijderd na de changeApplied
%%

%%
checkChange_addConfigurationDef(M,
	CR:changeRequestor):->
	%check of de gegevens ok zijn

	@model->>norm_configurationDef(CR,M).
%
applyChange_addConfigurationDef(M,
	CR : changeRequestor):->
	%maak de configuration def aan

	NewD *= configurationDefinition(?(CR,argument,1),
				?(CR,argument,2)), %maakt hem aan
	M->>insertDefinitionObject(NewD),
	CR->>result(NewD).
%%

%%
applyChange_deleteConfigurationDef(M,
	CR : changeRequestor):->
	%Verwijder de configuration def

	D = CR<<-argument(1),
	M->>removeDefinitionObject(D),
	CR->>freeObject(D). %wordt verwijderd na de changeApplied
%%

%%
checkChange_addQuantitySpace(M,
	CR:changeRequestor):->
	%check of de gegevens voor de qs ok zijn
	%aangezien dit hetzelfde is als voor changeQuantitySpace bij quantitySpace
	%maken we hieronder een norm regel die het doet

	M->>norm_quantitySpace(CR,M).
%
applyChange_addQuantitySpace(M,
	CR:changeRequestor):->
	%maak de qs en voeg hem toe aan de definities

	QS *= quantitySpace(?(CR,argument,1),
			?(CR,argument,2),
			?(CR,argument,3)),
	M->>insertDefinitionObject(QS),
	CR->>result(QS).
%%

%%
checkChange_changeQuantitySpace(M,
	CR: changeRequestor):->
	%controleer of dit niet de interne dqs is: die mag niet wijzigen

	CR->>checkObject(M?dqs),
	CR->>impossible('This quantity space is the fixed quantity space of derivatives and cannot be changed.').
%%

%%
checkChange_deleteQuantitySpace(M,
	CR: changeRequestor):->
	%controleer of dit niet de interne dqs is: die mag niet weg

	CR->>checkArgument(1,M?dqs),
	CR->>impossible('This quantity space is the fixed quantity space of derivatives and cannot be deleted.').
%%

%%
applyChange_deleteQuantitySpace(M,
	CR:changeRequestor):->
	%verwijder de qs

	Del = CR<<-argument(1),
	M->>removeDefinitionObject(Del),
	CR->>freeObject(Del).
%%





% for Sketch, AB, Aug 2006


% To move ?, AB, nov 2006
% Move Relation and structural relation definitions to concept_map.pl and structural_model.pl?
%%
checkChange_addRelationDef(M,
	CR:changeRequestor):->
	%check of de gegevens ok zijn

	debug(sketch(relations), 'checkChange_addRelationDef CR: ~w', [CR]),
	@model->>norm_relationDef(CR,M).
%
applyChange_addRelationDef(_M,
	CR : changeRequestor):->
	%maak de relation def aan

	debug(sketch(relations), 'applyChange_addRelationDef CR: ~w', [CR]),
	NewD *= sketchRelationDefinition(?(CR,argument,1),
				?(CR,argument,2)), %maakt hem aan
	SK = CR?editor<<-sketch,
	SK->>insertDefinitionObject(NewD),
	% M->>insertDefinitionObject(NewD),
	CR->>result(NewD).
%%

% should this method be moved to concept_map.pl? AB, nov 2006
%%
applyChange_deleteRelationDef(_M,
	CR : changeRequestor):->
	%Verwijder de relation def
	D = CR<<-argument(1),
	SK = CR?editor<<-sketch,
	SK->>removeDefinitionObject(D),
	% M->>removeDefinitionObject(D),
	CR->>freeObject(D). %wordt verwijderd na de changeApplied
%%




%%
checkChange_addSketchStructuralRelationDef(M,
	CR:changeRequestor):->
	%check of de gegevens ok zijn

	@model->>norm_sketchStructuralRelationDef(CR,M).
%
applyChange_addSketchStructuralRelationDef(_M,
	CR : changeRequestor):->
	%maak de relation def aan

	NewD *= sketchStructuralRelationDefinition(?(CR,argument,1),
				?(CR,argument,2)), %maakt hem aan
	SK = CR?editor<<-sketch,
	SK->>insertDefinitionObject(NewD),
	% M->>insertDefinitionObject(NewD),
	CR->>result(NewD).
%%


%%
applyChange_deleteSketchStructuralRelationDef(_M,
	CR : changeRequestor):->
	%Verwijder de relation def

	D = CR<<-argument(1),
	SK = CR?editor<<-sketch,
	SK->>removeDefinitionObject(D),
	% M->>removeDefinitionObject(D),
	CR->>freeObject(D). %wordt verwijderd na de changeApplied
%%





% Sketch Definitions (Processes, Agents, and Scenarios) (Sketch)
%%
%% this method may make the ones for Processes, Agents, and Scenarios obsolete
%% todo: check if that's indeed the case!
%%
applyChange_deleteSketchDef(M,
	CR : changeRequestor):->
	%Verwijder de sketch def

	D = CR<<-argument(1),
	M->>removeDefinitionObject(D),
	CR->>freeObject(D). %wordt verwijderd na de changeApplied
%%


% Processes (Sketch)
%
%%
checkChange_addSketchProcessDef(M,
	CR:changeRequestor):->
	%check of de gegevens ok zijn

	% @model->>norm_processDef(CR,M).
	CR->>checkImpossibleName(1,M),
	M->>normReservedMFNames(CR?arg1,CR). %geen gereserveerde naam?

%%
applyChange_addSketchProcessDef(M,
	CR : changeRequestor):->
	%maak de process def aan

	NewD *= sketchProcessDefinition(?(CR,argument,1),
				?(CR,argument,2)), %maakt hem aan
        % add def sheet data
	NewD->>def_sheet(?(CR,argument,3)),

	M->>insertDefinitionObject(NewD),
	CR->>result(NewD).
%%

%%
applyChange_deleteSketchProcessDef(M,
	CR : changeRequestor):->
	%Verwijder de process def

	D = CR<<-argument(1),
	M->>removeDefinitionObject(D),
	CR->>freeObject(D). %wordt verwijderd na de changeApplied
%%

/* End Processes (Sketch) */



% Agents (Sketch)
%

%%
checkChange_addSketchAgentDef(M,
	CR:changeRequestor):->
	%check of de gegevens ok zijn

	% @model->>norm_agentDef(CR,M).
	CR->>checkImpossibleName(1,M),
	M->>normReservedMFNames(CR?arg1,CR). %geen gereserveerde naam?

%%
applyChange_addSketchAgentDef(M,
	CR : changeRequestor):->
	%maak de agent def aan

	NewD *= sketchAgentDefinition(?(CR,argument,1),
				?(CR,argument,2)), %maakt hem aan
        % add def sheet data
	NewD->>def_sheet(?(CR,argument,3)),

	M->>insertDefinitionObject(NewD),
	CR->>result(NewD).
%%

%%
applyChange_deleteSketchAgentDef(M,
	CR : changeRequestor):->
	%Verwijder de agent def

	D = CR<<-argument(1),
	M->>removeDefinitionObject(D),
	CR->>freeObject(D). %wordt verwijderd na de changeApplied
%%

/* End Agents (Sketch) */



% Scenarios (Sketch)
%

%%
checkChange_addSketchScenarioDef(M,
	CR:changeRequestor):->
	%check of de gegevens ok zijn

	% @model->>norm_scenarioDef(CR,M).
	CR->>checkImpossibleName(1,M),
	M->>normReservedMFNames(CR?arg1,CR). %geen gereserveerde naam?

%%
applyChange_addSketchScenarioDef(M,
	CR : changeRequestor):->
	%maak de scenario def aan

	NewD *= sketchScenarioDefinition(?(CR,argument,1),
				?(CR,argument,2)), %maakt hem aan
        % add def sheet data
	NewD->>def_sheet(?(CR,argument,3)),

	M->>insertDefinitionObject(NewD),
	CR->>result(NewD).
%%

%%
applyChange_deleteSketchScenarioDef(M,
	CR : changeRequestor):->
	%Verwijder de scenario def

	D = CR<<-argument(1),
	M->>removeDefinitionObject(D),
	CR->>freeObject(D). %wordt verwijderd na de changeApplied
%%

/* End Scenarios (Sketch) */


%%%%%%%%% LANGUAGE %%%%%%%%%%%%%
checkChange_addLanguage(M,CR: changeRequestor):->
	%new language, only when not already defined

	CR->>checkImpossibleName(1,M),
	if
		M?translator?languages<<-find(->>(@arg1, equal, CR?arg1, @on))
	then
		CR->>impossible('This language is already defined').
%
applyChange_addLanguage(M,CR: changeRequestor):->
	M?translator->>addLanguage(CR?arg1).
%%

%%
checkChange_deleteLanguage(M,CR: changeRequestor):->
	%a language is deleted

	unless
		M?translator?languages<<-find(->>(@arg1, equal, CR?arg1, @on))
	do
		CR->>impossible(string('The language %s is not defined',CR?arg1))
	else
	(
		if
			1 = M?translator?languages<<-size
		then
			CR->>impossible('There has to be at least one language defined.')
		else
		(
			if
				M?translator?currentLanguage->>equal(CR?arg1,@on)
			then
			(
				OtherLanguage = M?translator?languages<<-find(@arg1 \== M?translator?currentLanguage),
				CR->>addChangeRequest(changeRequestor(setCurrentLanguage, M, @default, CR?garpModel, OtherLanguage),
						string('The current language of the model will be changed to %s',OtherLanguage),M)
				)
		)
	).
%
applyChange_deleteLanguage(M, CR: changeRequestor):->
	M?translator->>deleteLanguage(CR?arg1). %notifies the related translatables through an internal path
%%

%%
checkChange_setCurrentLanguage(M, CR: changeRequestor):->
	%make sure the asked language is available

	unless
		M?translator?languages<<-find(->>(@arg1, equal, CR?arg1, @on))
	do
		CR->>impossible(string('The language %s is not defined',CR?arg1)).
%
applyChange_setCurrentLanguage(M, CR: changeRequestor):->
	M?translator->>setCurrentLanguage(CR?arg1).
%%

%%
checkChange_changeLanguageName(M, CR: changeRequestor):->
	%changing the name: make sure the language is available, and the new name is not used

	unless
		M?translator?languages<<-find(->>(@arg1, equal, CR?arg1, @on))
	do
		CR->>impossible(string('The language %s is not defined',CR?arg1)),
	if
		M?translator?languages<<-find(->>(@arg1, equal, CR?arg2, @on))
	then
		CR->>impossible(string('The language %s is already defined',CR?arg2)),
	CR->>checkImpossibleName(2,M).
%
applyChange_changeLanguageName(M, CR: changeRequestor):->
	M?translator->>changeLanguageName(CR?arg1,CR?arg2).
%%

%%%%%%%Interne CR's

%%
applyChange_internalUpdateValueRef(_M,
	CR : changeRequestor):->
	%Altijd een subchange bij een of andere waarde/type verandering (changeQuantitySpace e.d.)
	%zorgt ervoor dat na deze wijziging refererende waarden (value e.d) de naamdata enzo weer goed
	%hebben

	?(CR,argument,1)->>copy(?(CR,argument,2)).
%%
%%%%%%%%%%%%%%%NORMEN%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%helpers voor normen, worden vanaf verschillende plekken in het programma aangeroepen
%om in ieder geval een soort algemene plek voor het afchecken van dingen te hebben

%%
/*
norm_FragmentInheritance: checkt MI-regels bij modelfragmenten. Deze norm checkt of een bepaalde
nieuwe parent mag gegeven een lijst met huidige parents. Dit in combinatie met een gegeven (bestaand), of onbekend (nieuw, nog niet bestaand) object waar het allemaal over gaat.
Voor het doorchecken van een heel rijtje parents moet een loopje gemaakt worden.
*/
norm_FragmentInheritance(_M, NewParent: modelFragment, Parents: chain, Object: [modelFragment*],
	Result: {ok,isObject,isChild,isParent,isParentChild,isChildParent,cyclic}):<-

	%resultaat: ok, isObject( NewParent is gelijk aan het gegeven object), isChild (NewParent is child van gegeven object), isParent (NewParent is al parent, of rechtlijnige voorvader van een element uit de Parents chain), isParentChild (NewParent is een child van een element uit de Parents chain), isChildParent(NewParent is een rechtstreekse parent van één van de children van het object), cyclic (NewParent heeft het object direct of indirect als conditie, dus dat zou een cycle geven)
	%alleen resultaat ok geeft aan dat het binnen onze mi-norm valt (zie ook datamodel bij ModelFragment)

	default(Object,@nil,RealObject),

	pl_normFragmentInheritance(NewParent,Parents,RealObject,Result).
%
pl_normFragmentInheritance(O,_,O,isObject):-!.
pl_normFragmentInheritance(NP,_,Object,isChild):-
	Object \== @nil,
	Object->>checkAllDown( @arg1 == NP, @off, @on),!.
pl_normFragmentInheritance(NP,Parents,_,isParent):-
	Parents->>find(->>(@arg1,checkAllUp,@arg1 == NP, @on, @on)),!.
pl_normFragmentInheritance(NP,Parents,_,isParentChild):-
	Parents->>find(->>(@arg1,checkAllDown, @arg1 == NP, @off, @on)),!.
pl_normFragmentInheritance(NP,_Parents,Object,isChildParent):-
	Object \== @nil,
	Object?children->>find(->>(@arg1?parents, find, @arg1 == NP)),!.
pl_normFragmentInheritance(_,_,_,ok).
%%

%%
norm_attributeDef(_M,CR:changeRequestor, O: requestor = object):->
	"check addAttributeDef and changeAttributeDef change data" ::
	%aangeroepen door model->checkChange_addAttributeDef en door
	%garpAttributeDefinition->checkChange_changeAttributeDef (als het de AD zelf betreft).
	%O is het object dat bij een impossible wordt meegestuurd naar de CR

	%De data
	Values = CR<<-argument(2),

	%a) Is de naam ok?
	CR->>checkImpossibleName(1,O), %check de naam

	%gp3: the name can not be has_assumption, as this is a reserved configuration name
	if
		CR?arg1?exportName->>equal('has_assumption')
	then
		CR->>impossible('The name you entered is reserved for internal purposes'),

	%b) Zijn er values?
	if
		Values->>empty
	then
		CR->>impossible('The attribute definition has no possible values defined',O),

	%c) Zijn de namen van de values geldig?
	AllNames = Values<<-map(@arg1?valueName),
	if
		AllNames->>find(not(->>(@model,validName,@arg1)))
	then
		CR->>impossible('At least one of the given values has an invalid name',O),

	%d) Zijn de value namen uniek?
	AllNames->>unique,
	S = AllNames<<-size,

	if
		(not S = Values<<-size)
	then
		CR->>impossible('At least one value is named twice' ,O),

	AllNames->>done.
%%

%%
norm_configurationDef(_M,CR: changeRequestor, O: requestor = object):->
	%gp3 0.1
	%called by model->checkChange_addConfigurationDef and
	%configurationDef->checkChange_changeConfigurationDef

	CR->>checkImpossibleName(1,O),
	if
		CR?arg1?exportName->>equal('has_assumption')
	then
		CR->>impossible('The name you entered is reserved for internal purposes').
%%

% for Sketch, AB, Aug 2006

%%
norm_relationDef(_M,CR: changeRequestor, O: requestor = object):->
	%gp3 0.1
	%called by model->checkChange_addRelationDef and
	%relationDef->checkChange_changeRelationDef

	CR->>checkImpossibleName(1,O),
	if
		CR?arg1?exportName->>equal('has_assumption')
	then
		CR->>impossible('The name you entered is reserved for internal purposes').
%%



%%
norm_sketchStructuralRelationDef(_M,CR: changeRequestor, O: requestor = object):->
	%gp3 0.1
	%called by model->checkChange_addSketchStructuralRelationDef and
	%sketchStructuralRelationDef->checkChange_changeSketchStructuralRelationDef

	CR->>checkImpossibleName(1,O),
	if
		CR?arg1?exportName->>equal('has_assumption')
	then
		CR->>impossible('The name you entered is reserved for internal purposes').
%%
% end Sketch

%%
norm_quantityDef(_M,CR:changeRequestor, O: requestor = object):->
	"Check addQuantityDef and changeQuantityDef change data" ::
	%aangeroepen door model->checkChange_addQuantityDef
	%en door garpQuantityDefinition->checkChange_changeQuantityDef (als het de QD zelf betreft)
	%O is het object dat bij een impossible wordt meegestuurd naar de CR
	%gp3 0.13: no numbers allowed in any quantity def
	CR->>checkImpossibleName(1,O),
	if
		new(regex('[0-9]'))->>search(CR?arg1)
	then
		CR->>impossible('Numbers are not allowed in the name of a quantity definition.'),
	if
		?(CR,argument,2)->>empty
	then
		CR->>impossible('The quantity definition has no allowed quantity spaces defined',O).
%%

%%
norm_quantitySpace(_M,CR:changeRequestor, O : requestor = object):->
	"Check addQuantitySpace and changeQuantitySpace change data" ::
	%aangeroepen door model->checkChange_addQuantitySpace
	%en door quantitySpace->checkChange_changeQuantitySpace (als het de QS zelf betreft)
	%O is het object dat bij een impossible wordt meegestuurd naar de CR

	%wat data
	Values = CR<<-argument(2),

	%a) Check of de naam geldig is
	CR->>checkImpossibleName(1,O), %check naam

	%b) Check of er wel values zijn:
	if
		Values->>empty
	then
		CR->>impossible('The quantity space has no values defined',O),

	%c) Meer dan één zero punt
	ZeroPoints = Values<<-find_all(and(
						@arg1?valueName?makeGarp == 'Zero',
						@arg1?type == point)),
	NumZeroPoints = ZeroPoints<<-size,
	if
		NumZeroPoints > 1
	then
		CR->>impossible('Point zero is defined as value more than once.',O),
	ZeroPoints->>done,

	%d) Zero interval
	if
		Values->>find(and(
				@arg1?valueName?makeGarp == 'Zero',
				@arg1?type == interval))
	then
		CR->>impossible('At least one interval value is defined as zero (should be point).', O),

	%e) Namen valide?
	if
		Values->>find(not(
				->>(@model,validName,@arg1?valueName?makeGarp)))
	then
		CR->>impossible('At least one qs value has an invalid name.',O),

	%f) Vaker voorkomende namen, niet zero (wordt apart gecheckt)
	NotZero = Values<<-find_all(@arg1?valueName?makeGarp \== 'Zero'),
	NotZeroNamesUn = NotZero<<-map(@arg1?valueName?makeGarp),
	NotZeroNamesUn->>unique,
	S = NotZeroNamesUn<<-size,
	if
		(not S = NotZero<<-size)
	then
		CR->>impossible('At least one qs value has the same name as another one.',O),
	NotZero->>done,
	NotZeroNamesUn->>done,

	%g) Wel een of meer puntwaarden, maar geen zero
	(
	    NumZeroPoints == 0,
	    Values->>find(@arg1?type == point) ->
	    get(@app, setting, nonessential_warnings, @on),
	    CR->>warning('The quantity space contains points, but not zero. Most quantity spaces that contain points, contain point zero.')
	;
	    true
	),

	% JL: If a quantity space has one or minusone, give a warning
	(
	    (
		Values->>find(@arg1?name == 'One') 
	    %;
	    %	Values->>find(@arg1?name == 'Minusone') 
	    ) ->
	    CR->>warning('Use 1 and -1 only in the context of multiplication or division.')
	;
	    true
	),

	% JL: If a quantity space has one or minusone, but no zero give a warning
	(
	    not(Values->>find(@arg1?name == 'Zero')),
	    Values->>find(@arg1?name == 'One')
	    %Values->>find(@arg1?name == 'Minusone') 
	    ->
	    CR->>warning('Quantity space uses 1 and -1, but lacks point zero which is necessary for multiplication and division.')
	;
	    true

	).




%%

%%
validName(_M,N:name):->
	%slaagt waneer de meegestuurde naam aan vastgestelde normen voor een naam voldoet
	%oa gebruikt door changeRequestor->checkImpossibleName

	RealName = N<<-makeGarp,
	\+ RealName?exportName?downcase->>equal(nil),
	Re *= regex('[(a-zA-Z][\sa-zA-Z0-9()]*'), %min 1 karakter, eerste een letter, etc
	S = RealName<<-size,
	S = Re<<-match(RealName),
	%gp3 0.3.16: any name starting with 'garp_' is banned, we use it for internal stuff like creating hidden assumptions etc
	\+ RealName?exportName->>prefix('garp_').
%%

%%
normReservedMFNames(M, N: name, CR: changeRequestor):->
	%check of de naam niet botst met een gereserveerde garp name
	%zoja: geef een impossible op de CR
	%slaagt altijd
	%dat is dus in het vreemde geval dat iemand een mf bijvoorbeeld
	%description - view noemt. Dit wordt uiteindelijk description_view.

	%gp3 0.1
	%we added some impossible names, legacy garp topnodes etc
	%our own topnodes are already save, because that would cause a conflict
	%but anyway


	Name = N?makeGarp?exportName?downcase<<-value,
	if
		member(Name,[static,process,agent,qualitative_state, description_view, composition_view,
        decomposition_view, view, state]) %list from engine/class.pl ->get_parents
	then
		CR->>impossible('The given name is a reserved internal name',M).
%%


%%%%%%%%%%EXPORTEREN%%%%%%%%%%%%%%%%%%%


export_all_internal(M, F: file):->
	%GARP3
	%0.1
	%exports to a tmpfile using the export calls, the engine will read it
	%we refill the exportedObjects map of @app mapping the ids generated to objects

	%fill the exportedObjects mapping first
	M?modelFragments->>for_all(->>(@app,addExportedObject,mf,@arg1)), %also contain scenarios

	M?abstractEntities->>for_all(->>(@app,addExportedObject,en,@arg1)),
	M?assumptions->>for_all(->>(@app,addExportedObject,en,@arg1)),

	M?configurationDefinitions->>for_all(->>(@app,addExportedObject,cd,@arg1)),
	M?attributeDefinitions->>for_all(
		->>(M,export_all_internal_attributeDefinition,@arg1)), %we need a helper

	%we also need a helper for quantity spaces, again because of the values
	M?quantitySpaces->>for_all(
		->>(M,export_all_internal_quantitySpace,@arg1)),

	%quantity definitions are not explicitly exported, but we do need the mapping
	M?quantityDefinitions->>for_all(->>(@app,addExportedObject,qd,@arg1)),

	%and exporting
	M->>exportIsa(F,internalexport), %gp3: also exports runPrefs
	M->>exportQS(F,internalexport),
	M->>exportInputSystem(F,internalexport),
	M->>exportLibrary(F,internalexport).
%
export_all_internal_attributeDefinition(_M, AD: garpAttributeDefinition):->
	%gp3 0.1 helper

	%we register the AD itself and it values, the names that is
	@app->>addExportedObject(ad,AD),
	AD?values->>for_all(
		->>(@app,addExportedObject,adv,@arg1,AD)). %in the context of the attribute definition
%
export_all_internal_quantitySpace(_M,QS: quantitySpace):->
	%gp3 0.1 helper

	%we register the QS and the names of its values
	@app->>addExportedObject(qs,QS),
	QS?values->>for_all(
		->>(@app,addExportedObject,qsv,@arg1,QS)). %context: the QS

exportIsa(M, Isa: file, Modelname: name):->
	%exporteer de isa file
	%also exports the runprefs ( = system prefs = algorithm assumptions)
	%gp3 0.3.15: we make sure all system assumptions are exported to the isa tree as well
	Isa->>format('/*\nGarp-model: %s\nisa\nAutomatically generated by %s\n*/\n\n',
				Modelname, @app?name),

	Isa->>append('%%%Entities%%%\n'),
	M?topEntity->>export(nil,Isa),
	Isa->>append('\n%%%Agents%%%\n'),
	M?topAgent->>export(nil,Isa),
	Isa->>append('\n%%%Assumptions%%%\n'),
	M?topAssumption->>export(nil,Isa),
	%lets see which system assumptions are not defined by the user: (gp3 0.3.15)
	SysAssums *= chain(quantity_behaviour,constant,generate_all_values,exogenous,exogenous_decreasing,exogenous_free,exogenous_increasing,exogenous_sinus,exogenous_steady,exogenous_neg_parabola,exogenous_pos_parabola), % new FL june 07: parabolas
	M?assumptions->>for_all(
		->>(SysAssums,delete_all,@arg1?name?exportName?value)), %make sure its a name
	unless
		SysAssums->>empty
	do
	(
		Isa->>append('\n\n%%%Quantity behaviour (system assumptions)\n'),
		SysAssums->>for_all(
			and(
				if(@arg1 == quantity_behaviour,
					->>(Isa,append,'isa(quantity_behaviour,assumption).\n'),
					if(or(@arg1 == constant, @arg1 == generate_all_values, @arg1 == exogenous),
						->>(Isa,format,'isa(%s,quantity_behaviour).\n',@arg1),
						->>(Isa,format,'isa(%s,exogenous).\n',@arg1)
					)
				)
			)
		)
	),
	%gp3 0.3.16: added garp_internal as an internal assumption
	Isa->>append('\n%%%Special assumption for internal garp use:\nisa(garp_internal,assumption).\n'),
	Isa->>append('\n\n%%%Simulation Preferences (Algorithm Assumptions)\n'),
	M?runPrefs->>for_all(
		->>(Isa,format,'algorithm_option_switch(%s,%s).\n',
				@arg1,when(@arg2 == @on,on, when(@arg2 == @off, off, @arg2)))).
%
exportQS(M, QS: file, Modelname: name):->
	%exporteer de quantity_space file

	QS->>format('/*\nGarp-model: %s\nquantity_space\nAutomatically generated by %s %s\n*/\n\n',
			Modelname, @app?name, @app?version),

	M?sortedQuantitySpaces->>for_all(->>(@arg1,export,QS)).

%
exportInputSystem(M, IS: file, Modelname: name):->
	%exporteer de input_system file

	IS->>format('/*\nGarp-model: %s\ninput_system\nAutomatically generated by %s %s\n*/\n\n',
			Modelname,@app?name, @app?version),

	M?sortedInputSystems->>for_all(->>(@arg1,export,IS)).
%
exportLibrary(M, Lib: file, Modelname: name, LegacyNaming: [bool]):->
	%exporteer de library file

	%gp3 added LegacyNaming, used when exporting to legacy files
	%it then uses description_view etc instead of static, process and agent
	%see modelFragment->>export

	Lib->>format('/*\nGarp-model: %s\nlibrary\nAutomatically generated by %s %s\n*/\n\n',
			Modelname, @app?name, @app?version),
	%we willen een logische volgorde: een soort breedte eerst per type modelfragment
	%maar we moeten voorkomen dat er dubbele komen, dus houden we dat bij

	MFGedaan *= chain,
	MFTeDoen *= chain,

	MFTeDoen->>merge(M?topStaticFragment?sortedChildren),
	MFTeDoen->>merge(M?topProcessFragment?sortedChildren),
	MFTeDoen->>merge(M?topAgentFragment?sortedChildren),

	pl_exportLibrary(MFTeDoen,MFGedaan,Lib,LegacyNaming),

	%%gp3 0.3.16: now export all special model fragments needed for quantity behaviours defined in
	%%inputsystems

	M?sortedInputSystems->>for_all(->>(@arg1,exportInternalBehaviourFragments,Lib)).
%
pl_exportLibrary(MFTeDoen,MFGedaan,Lib,LegacyNaming):-
	if
	(
		Huidige = MFTeDoen<<-delete_head %all done
	)
	then
	(
		unless
			MFGedaan->>member(Huidige) %this one done
		do
		(
			Huidige->>export(Lib,LegacyNaming),
			MFGedaan->>append(Huidige),
			NewToDo = Huidige<<-sortedChildren,
			pl_exportLibrary(NewToDo,MFGedaan,Lib,LegacyNaming)
		),
		%next
		pl_exportLibrary(MFTeDoen,MFGedaan,Lib,LegacyNaming)
	).
%%

/* TEXT EXPORT (gp3 1.4) */
%this is a sort of different version of export_all_internal, just export to a text file

exportText(M,Filename: char_array):->

	F *= file(Filename, text),
	F->>open(write),
	F->>format('Garp3-model: %s\nExported by: %s %s\n\n', M?name,@app?name, @app?version),

	%%meta data, we cannot do this automatically
	%%and also only the most important stuff
	%%feel free to add or remove
	MD_M = M?metaData<<-member(modelData),
	MD_G = M?metaData<<-member(general),
	MD_R = M?metaData<<-member(remarks),

	F->>format('** ABOUT THIS MODEL **\n'),
	%make a line for all simple fields that should be shown:
	F->>format('Creation time: %s\n',?(MD_M,member,creationTime)),
	F->>format('Created in: %s\n',?(MD_M,member,creatorProgram)),
	F->>format('Model version: %s\n',?(MD_G,member,model_version)),
	F->>format('Title: %s\n',?(MD_G,member,title)),
	F->>format('Author: %s\n',?(MD_G,member,author)),
	F->>format('\nGeneral remarks:\n%s\n',?(MD_R,member,general_remarks)),

	%the containerobjects will do their own work (more than in prolog export)
	M->>exportText_Isa(F),
	M->>exportText_AD(F),
	M->>exportText_CD(F),
	M->>exportText_QD(F),
	M->>exportText_QS(F),
	M->>exportText_Scenario(F),
	M->>exportText_Library(F),

	F->>close.
%
exportText_Isa(M,F: file):->
	%gp3 1.4: export the isa hierarchy to text

	F->>format('\n** ENTITIES **\n\n'),
	M?topEntity->>exportText(F,0),
	F->>format('\n** AGENTS **\n\n'),
	M?topAgent->>exportText(F,0),
	F->>format('\n** ASSUMPTIONS **\n\n'),
	M?topAssumption->>exportText(F,0).
%
exportText_AD(M,F: file):->
	%gp3 1.4: export the attribute definitions to text
	F->>format('\n** ATTRIBUTE DEFINITIONS **\n'),
	M?sortedAttributeDefinitions->>for_all(->>(@arg1,exportText,F)).
%
exportText_CD(M,F: file):->
	%gp3 1.4: export the configuration definitions to text
	F->>format('\n** CONFIGURATION DEFINITIONS **\n'),
	M?sortedConfigurationDefinitions->>for_all(->>(@arg1,exportText,F)).
%
exportText_QD(M,F: file):->
	%gp3 1.4: export the quantity definitions to text
	F->>format('\n** QUANTITY DEFINITIONS **\n'),
	M?sortedQuantityDefinitions->>for_all(->>(@arg1,exportText,F)).
%
exportText_QS(M,F: file):->
	%gp3 1.4: export the quantity spaces to text
	F->>format('\n** QUANTITY SPACES **\n'),
	M?sortedQuantitySpaces->>for_all(->>(@arg1,exportText,F)).

exportText_Scenario(M,F: file):->
	F->>format('\n** SCENARIOS **\n'),
	M?sortedInputSystems->>for_all(->>(@arg1,exportText,F)).

exportText_Library(M,F: file):->
	F->>format('\n** MODEL FRAGMENTS **\n'),
	%same order as exportLibrary (old code copied)

	MFGedaan *= chain,
	MFTeDoen *= chain,

	MFTeDoen->>merge(M?topStaticFragment?sortedChildren),
	MFTeDoen->>merge(M?topProcessFragment?sortedChildren),
	MFTeDoen->>merge(M?topAgentFragment?sortedChildren),

	pl_exportText_Library(MFTeDoen,MFGedaan,F).
%
pl_exportText_Library(MFTeDoen,MFGedaan,F):-
	if
	(
		Huidige = MFTeDoen<<-delete_head %all done
	)
	then
	(
		unless
			MFGedaan->>member(Huidige) %this one done
		do
		(
			Huidige->>exportText(F),
			MFGedaan->>append(Huidige),
			NewToDo = Huidige<<-sortedChildren,
			pl_exportText_Library(NewToDo,MFGedaan,F)
		),
		%next
		pl_exportText_Library(MFTeDoen,MFGedaan,F)
	).
%%

:-pce_end_class.



