/*
Garp3
2005 Jelmer Jellema - Spin in het Web

Definition of garp3 class: the application (@app)
Defines state, is the application for all frames, and add helper calls like openViewEditor etc.

pleaseWait Dlg is also here

See copyright notice.
*/

:-pce_begin_class(
		  garp3,
		  application,
		  "The garp3 application"
		 ).

variable(fileDir,directory,both,"The last used directory for opening, saving or exporting").
%variable(filename,name*,get,"The opened model filename"). %@nil means: none loaded/new model
variable(exportedObjects, hash_table, get, "Mapping of exported names to their design objects").
variable(exportedElements, hash_table, get, "Mapping of exported MF lines to their design objects"). %gp3 1.4
%variable(modelState,{new,loaded,legacy},both). %is there a model (changed state in @model).
variable(mustReloadModel,bool,both). %should we reload the model into the engine when simulating?
variable(settings,hash_table,both). %contains the settings the app loads and saves, use setting to get or set them by name
variable(engineArchPath,name,both). %just the path the engine loaded it shared objects from. Nice to show when needed (about?) %gp3 0.4.8 as of this version there are no shared objects left. We do keep this tree for future use

% Open multiple models in Garp3 % JL
variable(tabTool, managed_tab_stack:=new(managed_tab_stack), both, 'The toolbar with the tabs'). % JL
variable(modelsTabs, hash_table:=new(hash_table), both, 'The models and related tabs'). %JL
variable(maximumTabs, number:=4, get).

%gp3: we do have a real version now: major.sub.minor
variable(version_major,number:=1,get). 
variable(version_sub,number:=5,get).
variable(version_minor,number:=2,get).

%gp3: minimal pl version because of features
variable(min_pl_version,number:=60200,get).

%gp3 1.4: minimal model definition version we support (through versionpatch)
variable(min_model_version, number:=15,get).


initialise(G):->
	G->+initialise('Garp3'),
	/*** ATTRIBUTES *****/
	G->>attribute(file_extension, ['hgp','owl']),
	G->>attribute(legacy_extension, 'gp'), %gp3
	G->>slot(exportedObjects,new(hash_table)),
	G->>slot(exportedElements,new(hash_table)),
	G->>fileDir('.'), %default: current
	%G->>modelState(new),
	G->>mustReloadModel(@off), %no model yet
	G->>loadSettings,
	@pce->>exit_message(->>(G,saveSettings)),
	@pce->>console_label(string('%s %s',G?name,G?version)).
%%	

%%
initialiseTabs(G) :-> %JL new function
    send(@model, modelState, new),
    get(@model, '_value', RealModel),
    get(RealModel, object_reference, NewModelName),
    new(NewTab, tabButton(NewModelName)), 
    send(G?tabTool,append,NewTab),
    send(G?modelsTabs, append, NewTab, RealModel),
    send(G?tabTool, on_top, NewTab),
    %send(G, show_copy_buffer), % Show the copy buffer
    send(@copyBuffer, changed, @on),
    G?mainMenu->>updateVisual.
%%

%%
show_copy_buffer(G) :->
    new(CopyTab, tabButton('CopyBuffer')),
    send(G?tabTool,append,CopyTab),
    send(G?modelsTabs, append, CopyTab, @copyBuffer).

%%
version(G, V:char_array):<-
	%gp3 0.3: get the current app version string
	
	V *= string('%s.%s.%s',G?version_major,G?version_sub,G?version_minor).
%%

%%
checkPLVersion(G):->
	%gp3 0.3: check the current pl version to see if that is ok
	
	current_prolog_flag(version,V),
	if
		integer(V)
	then
		PLVersion = V
	else
		PLVersion = 0,
	if
		G?min_pl_version->>larger(PLVersion)
	then
	(
		VersionNum = G?min_pl_version<<-value,
		Major is VersionNum // 10000,
		Minor is (VersionNum - Major * 10000) // 100,
		Patch is VersionNum - Major * 10000 - Minor * 100,
		G?mainMenu->>msgBox(string('Warning: this software needs SWI-Prolog version %s.%s.%s or higher. You are using an older version, which can result in errors. Please download a newer version from www.swi-prolog.org.',Major,Minor,Patch),alarm)
	).
%%

%%
loadSettings(G):->
%gp3 0.2
	%are there saved settings?
	File *= file('~/.garp3_settings'),
	if
	(
		File->>access(read),
		File->>check_object
	)
	then
		S = File<<-object
	else
		S *= hash_table,
	%default settings are read when needed (see ?setting)
	G->>settings(S).
%%

%%
saveSettings(G):->
	%gp3 0.2
	%save our settings in a user file
	File *= file('~/.garp3_settings'),
	if
		File->>exists
	then
		File->>access(write), %then we must be able to write to it
	G?settings->>save_in_file(File).
%%

%%
setting(G,Key: any, Value: any):->
	%gp3 0.2
	%save new (value of existing) setting in memory
	
	G?settings->>append(Key,Value).
%%

%%
setting(G,Key: any, Value: any):<-
	%gp3 0.2
	%load a setting from the settings in memory
	%or if missing, from defaults
	
	unless
		Value = G?settings<<-member(Key)
	do
	(
		%first, load it from the default file, which is consulted
		ensure_loaded(main(appsettings)), %needed for <<-setting
		appsetting(Key,Value)
	).
%%

%%
openHelp(_G,ID: name*):->
	%gp3 0.3.13 open context sensitive help
	
	if
		ID == @nil
	then
		web:open(help,noid)
	else
		web:open(help,ID).
%%

visigarp(G, VisiGarp) :<-
    VisiGarp = G?members<<-find(->>(@arg1,instance_of,visigarp)).


%%pleaseWait: start a pleaseWait window

pleaseWait(G,Type: name):->
	%type is loading or run
	unless
		G<<-hypered(pleaseWait)
	do
	(
		if
			Type = loading
		then
			W *= pleaseWait('Please wait,\nGarp3 is loading...')  %see below in this file
		else
			W *= pleaseWait('Please wait,\nGarp3 is processing...'),
		G->>hyper(W,pleaseWait)
	).
%
%thankYou: destroy a pleaseWait window
thankYou(G):->
	if
		W = G<<-hypered(pleaseWait)
	then
		catch(W->>destroy,_,true),
	G->>delete_hypers(pleaseWait).
%%

%%
%model: return the model (@model) as a garpModel
%instead of the @model global var
%(needed for delayed use in functions)

model(_G, M: var):<-  % JL changed the return type
	M = @model.
%%

%%aboutBox: show an aboutBox
%gp3 0.2
aboutBox(G):->
	G?mainMenu->>msgBox(string('%s version %s.\n\nGarp3 is developed as part of the NaturNet-Redime project (004074). This project is co-funded by the European Commission within the Sixth Framework Programme (2002-2006).\n\nPartners in the development of Garp3:\n\nHuman Computer Studies Laboratory\nInformatics Institute, Faculty of Science,\nUniversity of Amsterdam\nAmsterdam, The Netherlands\nWWW: hcs.science.uva.nl\n\nSpin in het Web\nApeldoorn, The Netherlands\nWWW: www.spininhetweb.nl\n\nLouter Vormgeving\nAmsterdam, The Netherlands\nWWW: www.louter.info\n',@app?name,@app?version),notification,@on). %gp3 0.3: no label needed
	
/****** main menu *********/
mainMenu(G,M: mainMenu):->
	%register the menu as our current mainmenu
	G->>delete_hypers(mainMenu),
	G->>hyper(M, mainMenu).

mainMenu(G,M: mainMenu):<-
	%find our current mainmenu
	M = G<<-hypered(mainMenu).

/***** exported object mapping **********/
%%
%Clear all elements in exportedObjects and exportedElements
clearExportedObjects(G):->
	G?exportedObjects->>clear,
	G?exportedElements->>clear.

%%
%Add an object to the exportedObjects map, registering its named type (mf,en,cd, qs,qd,in and whatever)
%object should be able to deliver ?name?exportName
%mainly used bij garpModel->>export_all_internal
%Context used for example to add instances in the context of a scenario or model fragment

addExportedObject(G, Type: name, O: object, Context: [object], UseName: [name]):->
	default(Context,@nil,DContext),
	default(UseName,O?name?exportName,UsedName),
	Key *= string('%s/%s/%s',UsedName,Type,DContext?object_reference),
	G?exportedObjects->>append(Key?value,O).
	
%%


%find the exported object for a given name (no structs allowed, any ( or ) is interpreted as part of the name), given the type and possibly the context
%type should match the registered type for the object


findExportedObject(G, Type: name, Name: name, Context: [object], O: object):<-
	default(Context,@nil,DContext),
	Key *= string('%s/%s/%s',Name,Type,DContext?object_reference),
	O = G?exportedObjects<<-member(Key?value).


%%
findExportedName(G,O: object, Type: name, Context: object, UsedName: name):<-
	%gp3 0.3.16: the other way round
	%we give the garp3 design object, the right type specifier and context
	%and we find the name used in the export
	%used by input system to export internal model fragments for quantity behaviour concerning
	%a fixed instance (nonvar), that should have the same name as in the input system
	
	%fails if not found
	
	PartialKey = string('/%s/%s',Type,Context?object_reference)<<-value,
	Key = G?exportedObjects<<-find_key(
		and(
			@arg2 == O,
			->>(@arg1,suffix,PartialKey)
		)
	),
	UsedName = ?(Key,split,'/')<<-head.
%%
	
%%
%%gp3 1.4: just like addExportedObject, we also have exportedElement
%%these are saved in the ExportedElements hash_table
%%the key is made of the exportname of the MF the object is an element of, followed by an element number
%%so a key of flow/7 would point to the seventh element (system_element, parameter, value, relation or structure) 
%%from the top of the definition, so it could be a par_relation object (for example an influence) in the conditions of the model fragment whose exportname is 'flow', or an instance in the consequences etc. Just count on in the obvious order.

%%When scanning active model fragments in simulate, we can find back the right object by just counting, considering the order in the modelfragment does not change, but the unification does.

addExportedElement(G,MF: modelFragment, Count: number, O: object):->
	Key *= string('%s/%d', MF?name?exportName,Count),
	G?exportedElements->>append(Key?value,O).

%other way round: give the object
findExportedElement(G, MFName: name, Count: int, O: object):<-
	Key = string('%s/%d', MFName, Count)<<-value,
	O = G?exportedElements<<-member(Key).
%%

%get the name used for the object in the design model.
%this uses findExportedObject to get the object, and than ?name to get its name
%when its not able to find it for some reason, it returns the given name
%no structure allowed: only name

designName(G,Type: name, Name: name, Context: [object], DName: name):<-
	O = G<<-findExportedObject(Type,Name, Context),
	catch(DName = O<<-name,_,fail).
%
designName(_G,_Type: name, Name: name, _Context: [object], DName: name):<-
	%return the name only

	DName = Name.
%%

%%
/* currentScenario:
	Gives the design object for the input system (scenario) the engine has currently loaded
	or @nil if this cannot be found
	gp3 0.1
*/
currentScenario(G,IS: inputSystem):<-
	engine:state(_,smd(input_system(ISName),_,_,_,_,_,_)), %any state will do
	IS = G<<-findExportedObject(mf,ISName),!.
%
currentScenario(_G,IS):<-
	IS = @nil.


%%%%%%%%% CHANGES %%%%%%%%
modelChanged(G):->
	%the model has changed, called by garpModel->changeRequest
	
	G->>mustReloadModel(@on), %engine version of model is out of date now
	G->>closeVisualize, %maybe different later on, but for now: simulation is invalid
	%tell our mainmenu about this
	G?mainMenu->>modelChanged.
	
setModelChanged(G, ConceptualChange: [bool]):->
	%request from other objects to set the changed state of the model to @on
	%this is the way to do it, because the @app will also update the mainMenu etc
	%when ConceptualChange is given & @on, we will also note that the current
	%simulation is out of date
	
	unless
		get(@model, modelState, legacy)
		%legacy = G<<-modelState
	do
	(
		@model->>changed(@on),
		if
			ConceptualChange == @on
		then
		(
			G->>mustReloadModel(@on), %engine version is out of date now
			G->>closeVisualize
		),
		G?mainMenu->>modelChanged
	).
		
%%%%%%% OPEN EDITORS %%%%%%%%%%
%open an editor for an object. Copy of locally defined code. We made it global only where needed.
%gp3: entityEditor can now be opened with a selected entity. Does not work when the editor is allready open

openEntities(_G, SelectEntity: [entity]):->
    	get(@model, all_hypers, HyperChain),
	chain_list(HyperChain, HyperList),
	(
	    member(Hyper, HyperList),
	    get(Hyper, to, EntityDefEditor),
	    get(EntityDefEditor, class_name, 'entityEditor'),
	    get(EntityDefEditor, type, 'entity') ->
	    send(EntityDefEditor, expose)
	;
	    new(EntityDefEditor, entityEditor(?(@model,hypered,topEntity),SelectEntity)),
	    new(_PartOfHyper, partof_hyper(@model, EntityDefEditor)),
	    send(EntityDefEditor, expose)
	).

openAgents(_G, SelectAgent: [agent]):->
	get(@model, all_hypers, HyperChain),
	chain_list(HyperChain, HyperList),
	(
	    member(Hyper, HyperList),
	    get(Hyper, to, AgentDefEditor),
	    get(AgentDefEditor, class_name, 'entityEditor'),
	    get(AgentDefEditor, type, 'agent') ->
	    send(AgentDefEditor, expose)
	;
	    new(AgentDefEditor, entityEditor(?(@model,hypered,topAgent),SelectAgent)),
	    new(_PartOfHyper, partof_hyper(@model, AgentDefEditor)),
	    send(AgentDefEditor, expose)
	).

openAssumptions(_G, SelectAssumption: [assumption]):->
	get(@model, all_hypers, HyperChain),
	chain_list(HyperChain, HyperList),
	(
	    member(Hyper, HyperList),
	    get(Hyper, to, AssumptionDefEditor),
	    get(AssumptionDefEditor, class_name, 'entityEditor'),
	    get(AssumptionDefEditor, type, 'assumption') ->
	    send(AssumptionDefEditor, expose)
	;
	    new(AssumptionDefEditor, entityEditor(?(@model,hypered,topAssumption),SelectAssumption)),
	    new(_PartOfHyper, partof_hyper(@model, AssumptionDefEditor)),
	    send(AssumptionDefEditor, expose)
	).

openViewEditor(_G, MForIS):-> % JL Major Rewrite
	%open an existing or new viewEditor
	get(@model, all_hypers, HyperChain),
	chain_list(HyperChain, HyperList),
	(
	    member(Hyper, HyperList),
	    get(Hyper, to, ScenarioEditor),
	    get(ScenarioEditor, class_name, 'viewEditor'),
	    get(ScenarioEditor, fragment, MForIS) ->
	    send(ScenarioEditor, expose)
	;
	    new(ScenarioEditor, viewEditor(MForIS)),
	    new(_PartOfHyper, partof_hyper(@model, ScenarioEditor)),
	    send(ScenarioEditor, expose)
	).
		
%%
resaveAllMFLayouts(_G):->
	%gp3 0.3 This is a debugmessage. Call @app->>resaveAllMFLayouts
	%and all mfs and scenarios will be opened in its editor, and then closed
	%this way layoutinfo will be resaved, removing old (no longer needed) information
	
	@model?modelFragments->>for_some(
			->>(create(viewEditor,@arg1),destroy)),
	@pce->>write_ln('Now save the model please.').
%%

%%
removeAllMFLayouts(_G):->
	%gp3 0.3 This is a debugmessage. Call @app->>removeAllMFLayouts
	%and all layoutinfo will be removed
	%WARNING: this will destroy all nice layouts and mess them up real good
	
	@model?modelFragments->>for_some(
			->>(@arg1,clearLayOutInfo)).
%%
%%
openModelFragments(_G):-> %JL Major Rewrite
	%open an existing or new model fragment overview (structure editor)
	%gp3 0.3: new class, mfStructureEditor, replaces old list
	get(@model, all_hypers, HyperChain),
	chain_list(HyperChain, HyperList),
	(
	    member(Hyper, HyperList),
	    get(Hyper, to, ModelFragmentEditor),
	    get(ModelFragmentEditor, class_name, 'mfStructureEditor') ->
	    send(ModelFragmentEditor, expose)
	;
	    new(ModelFragmentEditor, mfStructureEditor),
	    new(_PartOfHyper, partof_hyper(@model, ModelFragmentEditor)),
	    send(ModelFragmentEditor, open)
	).

openScenarios(_G, DPickMode:[bool]):-> % JL Major Rewrite
	%open an existing or new scenario list (inputSystemList)
	%when needed, the pick mode of an existing window will be changed (gp3 0.2)
	
	default(DPickMode,@off,PickMode),
	%hard to find the editor, because it is a dialog in a frame, and the frame is the member of the app

	get(@model, all_hypers, HyperChain),
	chain_list(HyperChain, HyperList),
	(
	    member(Hyper, HyperList),
	    get(Hyper, to, ScenarioList),
	    get(ScenarioList, class_name, 'inputSystemList') ->
	    %ScenarioListDlg = ScenarioList?members<<-find(->>(@arg1,instance_of,inputSystemList)), 
	    send(ScenarioList, expose, PickMode)
	;
	    new(ScenarioList, inputSystemList),
	    new(_PartOfHyper, partof_hyper(@model, ScenarioList)),
	    send(ScenarioList, open, PickMode)
	).

openAttributeDefinitions(_G, Def):-> % JL Major Rewrite
	%open a new attribute definition list (or an open one)
	get(@model, all_hypers, HyperChain),
	chain_list(HyperChain, HyperList),
	(
	    member(Hyper, HyperList),
	    get(Hyper, to, AttrDefEditor),
	    get(AttrDefEditor, class_name, 'attributeDefEditor') ->
	    send(AttrDefEditor, expose)
	;
	    new(AttrDefEditor, attributeDefEditor),
	    new(_PartOfHyper, partof_hyper(@model, AttrDefEditor)),
	    send(AttrDefEditor, edit, Def)
	).
	
openConfigurationDefinitions(_G, Def):->  % Major Rewrite JL 
	%open a new configuration definition list (or an open one)
	get(@model, all_hypers, HyperChain),
	chain_list(HyperChain, HyperList),
	(
	    member(Hyper, HyperList),
	    get(Hyper, to, ConfigDefEditor),
	    get(ConfigDefEditor, class_name, 'configurationDefEditor') ->
	    send(ConfigDefEditor, expose)
	;
	    new(ConfigDefEditor, configurationDefEditor),
	    new(_PartOfHyper, partof_hyper(@model, ConfigDefEditor)),
	    send(ConfigDefEditor, edit, Def)
	).
	
openQuantityDefinitions(_G, Def: [garpQuantityDefinition]):-> % JL Major Rewrite
	%open a new quantity definition list (or an existing one)
	%When Def is not @default, the it will be selected in the list
	get(@model, all_hypers, HyperChain),
	chain_list(HyperChain, HyperList),
	(
	    member(Hyper, HyperList),
	    get(Hyper, to, QuantityDefEditor),
	    get(QuantityDefEditor, class_name, 'quantityDefEditor') ->
	    send(QuantityDefEditor, expose)
	;
	    new(QuantityDefEditor, quantityDefEditor),
	    new(_PartOfHyper, partof_hyper(@model, QuantityDefEditor)),
	    send(QuantityDefEditor, edit, Def)
	).

openQuantitySpaces(_G):-> % JL Major Rewrite
	%open a new quantity space list (or an existing one)
    	get(@model, all_hypers, HyperChain),
	chain_list(HyperChain, HyperList),
	(
	    member(Hyper, HyperList),
	    get(Hyper, to, QuantitySpaceDefEditor),
	    get(QuantitySpaceDefEditor, class_name, 'quantitySpaceEditor') ->
	    send(QuantitySpaceDefEditor, expose)
	;
	    new(QuantitySpaceDefEditor, quantitySpaceEditor),
	    new(_PartOfHyper, partof_hyper(@model, QuantitySpaceDefEditor)),
	    send(QuantitySpaceDefEditor, edit)
	).

openMetadata(_G):-> % JL Major Rewrite
	%gp3 0.3.13
	%open the metadata editor for the model 
	%hard to find the editor, because it is a dialog in a frame, and the frame is the member of the app
	get(@model, all_hypers, HyperChain),
	chain_list(HyperChain, HyperList),
	(
	    member(Hyper, HyperList),
	    get(Hyper, to, MetaDataDefEditor),
	    get(MetaDataDefEditor, class_name, 'metadataEditor') ->
	    send(MetaDataDefEditor, expose)
	;
	    new(MetaDataDefEditor, metadataEditor),
	    new(_PartOfHyper, partof_hyper(@model, MetaDataDefEditor)),
	    send(MetaDataDefEditor, open)
	).
%%

openLanguageEditor(_G):-> 
	%gp3 1.4.0 jj, based on code by JL (multimodel implementation should be rewriten, an editor should get the model as an argument to new)
	%open the metadata editor for the model 
	%hard to find the editor, because it is a dialog in a frame, and the frame is the member of the app
	get(@model, all_hypers, HyperChain),
	chain_list(HyperChain, HyperList),
	(
	    member(Hyper, HyperList),
	    get(Hyper, to, LanguageEditor),
	    get(LanguageEditor, class_name, 'languageEditor') ->
	    send(LanguageEditor, expose)
	;
	    new(LanguageEditor, languageEditor),
	    new(_PartOfHyper, partof_hyper(@model, LanguageEditor)),
	    send(LanguageEditor, open)
	).
%%

startVisualize(G,Scenario: [inputSystem]):->
	%start the visualizer on a given scenario or display a list to choose from
	%assumes visualizer is gone

	G->>closeVisualize,
	G->>checkReloadModel, %possbily reload the engine model
	if
		Scenario == @default
	then
	(
		%if legacy-mode we use legacy visigarp code
		%otherwise we just open the scenario list
		%or a scenario if there is only one)
		if
			get(@model, modelState, legacy)
		then
			visualize:start_visualize
		else
		(
			if
				1 = @model?inputSystems<<-size
			then
				G->>startVisualize(@model?inputSystems?head)
			else
				G->>openScenarios(@on)
		)
	)
	else
	(
		InternalName = Scenario?name?exportName<<-value, %make sure its a name
		visualize:start_visualize(InternalName)
	).

reopenVisualize(G):->
	%check the way the simulator can be reopened. This is either: reopen the window (when there is some simulation going)
	%or open the savedSimulation window	
	
	if
		Visi = G?members<<-find(->>(@arg1,instance_of,visigarp))
	then
		Visi->>expose
	else
	(
		if
			@off = G<<-mustReloadModel
		then
			visualize:reopen_visualize
		else
		(
			unless
				0 = @model?savedSimulations<<-size
			do
				G->>loadSimulation(G?mainMenu)
		)
	).
%
	
startFullSimulation(G, Scenario: inputSystem):->
	%start the visualizer on a given scenario (no default) and start full simulation
	G->>closeVisualize,
	G->>checkReloadModel, %possbily reload the engine model
	InternalName = Scenario?name?exportName<<-value, %make sure its a name
	visualize:start_full(InternalName).

%%
openTracer(_G):->
	%gp3 0.3
	%open the trace window. You can do this at any time
	
	@tracer->>open.
%%

%%
runPrefs(_G, Caller: frame):->
	%gp3 0.3
	%open runprefs dialog	
	
	% Open the simulation preferences (bottomleft corner on bottomleft corner of main screen)
	get(@app?mainMenu, position, MMPosition),
	get(@app?mainMenu?size, height, MMHeight),
	get(MMPosition, y, Y),
	NewY is Y + MMHeight  - 271, % 271 determined empirically
	new(Position, point(MMPosition?x - 2, NewY)),

	new(RunPrefsDialog, runPrefsDialog(Caller)),
	RunPrefsDialog?frame->>open(position:=Position).
%%

%%
checkReloadModel(G):->
	%gp3 0.1
	%does the engine still have the right model?
	if
		@on = G<<-mustReloadModel
	then
	(
		enginectrl:command(loadmodel),
		G->>mustReloadModel(@off)
	).
	
visualizing(G,Scenario: name):->
	%gp3 0.1
	%visigarp global is telling us it is visualizing Scenario now
	%we save this as the last edited input system, so the menu will show quick links for it
	
	unless
		get(@model, modelState, legacy)
	do
	(
		Object = G<<-findExportedObject(mf,Scenario),
		@model->>setLastEditedMF(Object),
		%we do not set de changed flag in @model, not important enough
		G?mainMenu->>modelChanged %but the menu should update
	).

closeVisualize(G):->
	%gp3 0.1
	%close all visualize windows
	%we only close the main window, all other windows are connected to that one
	%(either because they are transient for it, or because the ->>destroy of the visigarp clas
	%will know they are here)

	
	if
		Visi = G?members<<-find(->>(@arg1,instance_of,visigarp))
	then
		Visi->>destroy.
		
%%%%%model loading and saving etc

%%%
checkChanged(G):-> % JL Only close windows belonging to the model
	% check if the user wants to save the changes
	% Fails if canceled
	% side-effect: close all windows except mainmenu (JL: Only of the current model)
	
	%gp3 0.3: changed the close code: windows that have attribute noCloseOnModelEvent stay open
	M = G<<-mainMenu,
	get(@model, all_hypers, HyperedChain),
	%(   G?members->>for_all(if(and(@arg1 \== M,not(?(@arg1,attribute,noCloseOnModelEvent))),
	%			      ->>(@arg1,destroy))),
	(   
	    HyperedChain->>for_all(
		if(
		    and(
			@arg1?to \== M,
			not(?(@arg1?to,attribute,noCloseOnModelEvent)),
			message(@arg1, instance_of, 'frame')
		    ),
		    ->>(@arg1?to,destroy))),
		(
			get(@model, modelState, legacy)    
		; 
			@off = @model<<-changed
		)
	->  true
	;   C *= saveChangesDlg(M),
	    R = C<<-confirm_centered(M?area?center),
	    C->>destroy,
	    pl_checkChanged(G,R)
	).
%
pl_checkChanged(G,@on):-
	%save changes
	% @nil = G<<-filename,!, %use saveas % JL
	get(@model, filename, @nil),
	%@nil = @model<<-filename, % JL
	G->>saveAs,
	@off = @model<<-changed. %fail if still no good
%
pl_checkChanged(G,@on):-
	G->>save,!, %save it
	@off = @model<<-changed. %else: problem
%
pl_checkChanged(_G,@off):-
	%we are allowed to go on
	true.
%
pl_checkChanged(_G,@nil):-
	%user canceled
	!, fail.
%%%

%%
toManyModels(G) :->
    get(G?modelsTabs, keys, Tabs),
    get(Tabs, size, NumberOfTabs),
    get(G?maximumTabs, value, MaxTabs),
    (
	NumberOfTabs < MaxTabs ->
	true
    ;
	G?mainMenu->>msgBox(string('Maximum of %s open models. Cancelled action.', MaxTabs),alarm),
	fail
    ).


%%
newModel(G):->  % Just replace entire function JL
	%clear the model
	%G->>checkChanged,
	
	%G->>slot(filename,@nil), % JL
	gensym(model, NewModelName),

	send(G, toManyModels),
	@app->>mustReloadModel(@on), %engine has wrong model loaded
	new(NewModel, garpModel),
	send(NewModel, name_reference, NewModelName),

	send(@model, assign, NewModel, global),
	send(@model, modelState, new), % JL
	@model->>slot(filename,@nil), % JL

	new(NewTab, tabButton(NewModelName)), 
	send(G?tabTool,append,NewTab),
	send(G?modelsTabs, append, NewTab, NewModel),
	send(G?tabTool, on_top, NewTab),

	%send(G?tabTool, update_tabnames),
	G?mainMenu->>updateVisual.
%%

%%
closeCurrentModel(G) :->
    % Get the top tab and corresponding model
    get(G?tabTool, on_top, Tab),

    % Check if anything in the model has changed
    G->>checkChanged,
    
    % Then remove the tab from the tool bar and modelTabs, and free the model and tab
    send(G, closeModel, Tab),

    % Now set @model to the model corresponding to the active tab
    (   %if there are other tabs 
	get(@app?tabTool, on_top, NewActiveTab) ->
	get(G?modelsTabs, member, NewActiveTab, NewActiveModel),
	send(@model, assign, NewActiveModel, global)
    ;
	send(@app?tabTool?graphicals, empty),
	send(G, newModel)
    ),
    G?mainMenu->>updateVisual.
%%

%%
closeModel(G, Tab) :->
    /* Search the model corresponding to the tab.*/
    get(G?modelsTabs, member, Tab, ModelInTab),

    /* The viewEditor, mfStructureEditor, and the Sketch Editors have to be closed before unlinking @model */
    get(ModelInTab, all_named_hypered, @default, @default, HyperedChain), 
    chain_list(HyperedChain, HyperedList),
    forall(
	member(Hypered, HyperedList),
	(
	    get(Hypered, class_name, ClassName),

	    member(ClassName, ['viewEditor', 'mfStructureEditor', 
			       'concept_map_editor', 'structure_editor', 
			       'causal_model_editor', 'stgraphEditor',
			       'processesEditor', 'agentsEditor', 
			       'scenariosEditor', 'metadataEditor']) ->
	    send(Hypered, destroy)
	;
	    true
	)
    ),

    /* Destroy both the tab and the model */
    send(G?modelsTabs, delete, Tab),
    free(ModelInTab),
    send(G?tabTool, erase, Tab),
    free(Tab).

%%

%%
changeTab(G, Tab) :-> % JL
	send(G?tabTool, on_top, Tab),
	send(Tab, status, on_top),
	%send(Tab, active, @on),
	get(G?modelsTabs, member, Tab, ActiveModel),
	send(@model, assign, ActiveModel, global),

	/* If a legacy mode model is load the model again */
	( 
	    get(@model, modelState, legacy) ->
	    send(@app, loadLegacy3, @model?filename)
	;
	    true
	),

	send(G?mainMenu, modelChanged),
	send(G, mustReloadModel, @on),
	send(G?mainMenu, updateVisual).

%% 
selectCorrectTab(G, WindowObject) :->
	% Select the tab (model) corresponding to the window (editor)
	%get(G?modelsTabs, values, ModelsChain),
	%chain_list(ModelsChain, Models),
	%member(Model, Models),

	%get(Model, all_hypers, AllHypersChain),
	%chain_list(AllHypersChain, AllHypers),
	%member(Hyper, AllHypers),
	%get(Hyper, class_name, partof_hyper),
	%get(Hyper, to, WindowObject),

	get(G?modelsTabs, find_key, message(@prolog, ==, @arg2, WindowObject?garpModel), Tab),
	send(G, changeTab, Tab).	
%%

%%
editorBelongsToSameModel(_G, ChangeRequestEditor1, Editor1) :->
    % First check which model ChangeRequestEditor belongs to
    % Then check if Editor also belongs to this model.
    
    % Fail if either editor is @nil
    (
	(ChangeRequestEditor1 = @nil ; Editor1 = @nil) ->
	fail
    ;
	Editor = Editor1,
	ChangeRequestEditor = ChangeRequestEditor1
    ),

    %get(Editor, class_name, X),
    %get(ChangeRequestEditor, class_name, Y),
    %format('Editor is ~w and ChangeRequestEditor is ~w\n', [X, Y]),

    % Get the model the change request editor belongs to
    design:getModels(AllModels),
    chain_list(AllModels, AllModelsList),

    new(ChangeRequestEditorModels, chain),
    forall(
	member(Model, AllModelsList),
	(
	    get(Model, all_named_hypered, @default, message(@prolog, ==, @arg2?class_name, partof_hyper), AllEditorsOfModel),
	    %get(Model,all_named_hypered,@default,@arg2?class_name == partof_hyper) or even message(@arg1,instance_of,partof_hypeR)
	    (
		send(AllEditorsOfModel, member, ChangeRequestEditor),
		send(ChangeRequestEditorModels, append, Model)
	    ;
		true
	    )
	)
    ),
    %chain_list(ChangeRequestEditorModels, TheModels),
    %format('Ik hoor bij ~w\n', [TheModels]),

    % Check if Editor belongs to the the ChangeRequestEditorModel
    get(ChangeRequestEditorModels, nth1, 1, ChangeRequestModel),
    get(ChangeRequestModel, all_named_hypered, @default, message(@prolog, ==, @arg2?class_name, partof_hyper), AllEditorsOfModel),
    send(AllEditorsOfModel, member, Editor).
%%

%%
loadModel(G):->
	%load a new model in the designer
	%code from homer 2.1 (hence dutch comments)
	
	%G->>checkChanged, %sluit ook alle editors behalve deze
	(
	    /* If a file is selected */
	    FileName = @garp3_finder<<-file(G?mainMenu, 'Select a Garp3 model', @on,G?file_extension,G?fileDir) ->
	    string_length(FileName, Length),
	    LengthMinusExtension is Length - 3,
	    sub_atom(FileName, LengthMinusExtension, 3, _After, Extension),
	    (
		/* If the model is already loaded */
		get(G?modelsTabs, keys, TabsChain),
		chain_list(TabsChain, Tabs),
		member(Tab, Tabs),
		/* Create a temp file to get the base name */
		new(TempFile, file(FileName)),
		get(TempFile, base_name, FileNameBase),
		free(TempFile),
		get(Tab, realTabName, TabName),
		sub_string(TabName, 0, _, 4, TabNameBase),
		sub_string(FileNameBase, 0, _, 4, TabNameBase) ->
		/* Give a warning and cancel the loading */
		G?mainMenu->>msgBox(string('This file is already loaded. Loading cancelled.'),alarm)
	    ;
		/* Otherwise load the model */
		(
		    /* Check if the model can be loaded in the top tab */
		    get(G?tabTool, on_top, Tab),
		    get(G?modelsTabs, member, Tab, ActiveModel),
		    get(ActiveModel, filename, @nil),
		    get(ActiveModel, changed, @off) ->
		    (
			Extension = 'hgp' ->
			G->>loadModelFromHGPFile(FileName) %helper
	    	    ;
			Extension = 'owl' ->
			G->>loadModelFromOWLFile(FileName)
		    )
		;
		    /* If not, check if a new tab can be opened and load it if possible */
		    send(G, toManyModels),
		    (
			Extension = 'hgp' ->
			G->>loadModelFromHGPFile(FileName) %helper
		    ;
			Extension = 'owl' ->
			G->>loadModelFromOWLFile(FileName)
		    )
		),
		get(@model, name, X),
		format('In loadModel - The loaded model is ~w\n', [X])
	    )
	;
	    true
	).

%%
loadModelFromHGPFile(G, FileName: name):->
	%gp3 0.3 Helper for loadModel, can also be used by revert etc
	%code from homer hence dutch comments

	% Make sure the current model survives
	%gp3 1.4: save it for reassigning on failure
	CurrentModel = @model<<-'_value',
	send(@model, assign, @nil, global),
	
	File *= file(FileName),
	File->>check_object,
	%gp3 1.4: moved this a bit early, to make sure we check for too old model definitions before assigning to @model
	%this will overwrite @model
	_Instance = File<<-object,
	%gp3 1.4: check model version 
	ModelVersion = @model<<-modelDefinitionVersion,
	%does the model have a version anyway?
	if
		ModelVersion = @nil
	then
		MV = 0
	else
		MV = ModelVersion,
	MinVersion = G<<-min_model_version,
	if 
		MinVersion->>larger(MV)
	then
	(
		%reset the @model var to its old value
		catch(free(@model),_,true),
		@model *= var,
		@model->>assign(CurrentModel, global),
		G?mainMenu->>msgBox('Warning. This model\'s version number is too low to load into this version of Garp3. Please use an older version of Garp3 (1.3.8) to open and save this model, and then try again in this version.',alarm),
		!, fail
	),
	
	
	G->>mustReloadModel(@on), %engine has wrong model loaded

	%Instance == @model,
	
    	/* Rename the @model reference to @modelX, where X is a number */
	gensym(model, NewLoadedModelName),
	send(@model, name_reference, NewLoadedModelName),
	get(@pce, object_from_reference, NewLoadedModelName, NewLoadedModel),

	get(File, base_name, TabName),
	(
	    /* Check if the current tab is a new unchanged model */
	    get(G?tabTool, on_top, Tab),
	    get(G?modelsTabs, member, Tab, ActiveModel),
	    get(ActiveModel, filename, @nil),
	    get(ActiveModel, changed, @off) ->
	    /* In that case, close the current tab */
	    send(G, closeModel, Tab)
	;
	    true
	),
	
	new(LoadedModelTab, tabButton(TabName)), 
	send(G?tabTool,append,LoadedModelTab),
	send(G?modelsTabs, append, LoadedModelTab, NewLoadedModel),
	send(G?tabTool, on_top, LoadedModelTab),

	/* Destroy the overwritten @model, and recreate the @model var */
	free(@model),
	new(@model, var),
	/* Assign the cloned loaded model to @model */
	send(@model, assign, NewLoadedModel, global),

	@model->>name(File?base_name),
	@model->>slot(filename,FileName), % JL	
	send(@model, modelState, loaded), 
	
	@model->>changed(@off), %expliciet op @off zetten (want kan met @on opgeslagen zijn)
	%en zet de dirs
	G->>fileDir(File?directory_name),
	File->>close,
	@model->>versionPatch,
	M = G<<-mainMenu,
	M->>modelChanged,
	%make sure the modelTool is closed, and build and simualate tools are opened
	M->>collapse_expand_dlg(M?modelTool,@off),
	M->>collapse_expand_dlg(M?buildTool,@on),
	M->>collapse_expand_dlg(M?simulateTool,@on).
%%

%%

loadModelFromOWLFile(G, FileName: name):->
	%gp3 0.3 Helper for loadModel, can also be used by revert etc
	%code from homer hence dutch comments

	G->>mustReloadModel(@on), %engine has wrong model loaded
	(
	    /* Check if the current tab is a new unchanged model */
	    get(G?tabTool, on_top, Tab),
	    get(G?modelsTabs, member, Tab, ActiveModel),
	    get(ActiveModel, filename, @nil),
	    get(ActiveModel, changed, @off) ->
	    /* In that case, close the current tab */
	    send(G, closeModel, Tab)
	;
	    true
	),
	send(G, newModel),
	new(MI, modelImporter),
	send(@app, setting, nonessential_warnings, @off),
        send(MI, import, FileName),
	send(@app, setting, nonessential_warnings, @on),
	free(MI), 

	send(@model, modelState, new), 

	string_length(FileName, Length),
	LengthMinusExtension is Length - 3,
	sub_atom(FileName, 0, LengthMinusExtension, _After, BaseName),
	string_concat(BaseName, 'hgp', HGPName),
	file_base_name(HGPName, BaseHGPName),

	@model->>name(BaseHGPName),
	@model->>slot(filename, BaseHGPName), % JL	
	get(G?tabTool, on_top, ModelTab),
	send(ModelTab, slot, realTabName, BaseHGPName),

	send(@model, changed, @on), % an OWL model is a new changed model 

        M = G<<-mainMenu,
        M->>modelChanged,

	%make sure the modelTool is closed, and build and simualate tools are opened
	M->>collapse_expand_dlg(M?modelTool,@off),
	M->>collapse_expand_dlg(M?buildTool,@on),
	M->>collapse_expand_dlg(M?simulateTool,@on).
%%

%%
checkLoadFile(G, FileName) :-> % JL
    chain_list(G?tabTool?graphicals, Tabs),
    forall( member(Tab, Tabs),
	(
	    get(G?modelsTabs, member, Tab, Model),
	    get(Model, filename, FileNameLoaded),
	    not(FileName == FileNameLoaded)
	;
	    format('Warning: This file is already loaded! Aborting!\n'),
	    fail
	)
    ).
%%

%%
revert(G):->
	%unpublished method, gp3 0.3
	%reload the model. Use @app->>revert to get it
	
	get(@model, filename, FN),  % JL
	FN \== @nil,
	
	M = G<<-mainMenu,
	G?members->>for_all(if(and(@arg1 \== M,not(?(@arg1,attribute,noCloseOnModelEvent))),
				      ->>(@arg1,destroy))),
	string_length(FN, Length),
	LengthMinusExtension is Length - 3,
	sub_atom(FN, LengthMinusExtension, 3, _After, Extension),
    	(
	    Extension = 'hgp' ->
	    G->>loadModelFromHGPFile(FN) %helper
	;
	    Extension = 'owl' ->
	    G->>loadModelFromOWLFile(FN)
	).
%%
	
%%
save(G):->
	%rewrite of homer method
	
	%gp 0.2: save even if the model indicates is has not changed
	%(lay-out changes)
	%maybe we shoud use saveAs?
	get(@model, filename, FileName),
	(
	    /* If filename is unknown, do saveas */
	    FileName = @nil->
	    G->>saveAs
	;
	    /* If the filename is known save the model in that file */
	    (
		string_length(FileName, Length),
		LengthMinusExtension is Length - 3, %except when saving as .savedtext or anything other than a 3 char extension, we are not using msdos, are we?
		sub_atom(FileName, LengthMinusExtension, 3, _After, Extension),
		(
		    /* If the model is a normal HGP file, do a normal save */
		    Extension = 'hgp' ->
		    @model->>doSaveInFile(FileName) %slaat nog even de layout van de open editors op,
		;
		    /* If the model is an OWL file, do an OWL export save */
		    Extension = 'owl' ->
		    new(ME, modelExporter),
		    open(FileName, write, Fd, []), set_stream(Fd, alias(owl_target)),
		    close(Fd),
		    send(ME, export, owl_target),
		    free(ME) 
		),
		@model->>changed(@off), %dat staat natuurlijk fout opgeslagen,
		send(@model, modelState, loaded),
		%G->>modelState(loaded), %allways
		G?mainMenu->>modelChanged
	    )
	).
%%

%%
saveAs(G):->
	%homer method (dutch comments)
	%save-as gebeurt met de oude gegevens als beschikbaar,
	%en anders gewoon in de huidige dir
	%net als save mag het gewoon met nog dingen open

	(
		get(@model, filename, @nil)
		%@nil = @model<<-filename % JL
		->
		(
			DefDir = G<<-fileDir,
			DefFile = @default
		)
		;
		(
			%File *= file(G?filename),
    			File *= file(@model?filename), % JL
			DefDir = File<<-directory_name,
			DefFileWithExtension = File<<-base_name,
			string_length(DefFileWithExtension, Length),
			LengthMinusExtension is Length - 4,
			sub_atom(DefFileWithExtension, 0, LengthMinusExtension, _DefFileLength, DefFile)
		)
	),

	%get the mainMenu
	M = G<<-mainMenu,
	if
		FileName = @garp3_finder<<-file(M,'Save this model',@off,G?file_extension,DefDir,DefFile) %file hoeft niet te bestaan
	then
	(
		%gp3 0.3.12: above call now fails with cancel
		%so, now we can save
		string_length(FileName, Length2),
		LengthMinusExtension2 is Length2 - 3,
		sub_atom(FileName, LengthMinusExtension2, 3, _After, Extension),
		(
		    Extension = 'hgp' ->
		    % Set the filename in the mdoel
		    @model->>name(file(FileName)?base_name),
		    % The new creation date is now
		    get(@model?metaData, member, modelData, ModelData),
		    send(ModelData, append, creationTime, new(date)),
		    % Create a new UniqueID
		    send(@model, newUniqueID),
		    @model->>doSaveInFile(FileName), %slaat nog even de layout van de open editors op,
		    %G->>slot(filename,FileName),
		    %@model->>slot(filename,FileName), % JL
		    send(@model, slot, filename, FileName),
		    G->>fileDir(file(FileName)?directory_name),
		    @model->>changed(@off), %vanaf nu ongewijzigd
		    send(@model, modelState, loaded),
		    %G->>modelState(loaded),
		    %en zet de dirs
		    M->>modelChanged
		;
		    Extension = 'owl' ->
		    new(ME, modelExporter),
		    open(FileName, write, Fd, []), set_stream(Fd, alias(owl_target)),
		    send(ME, export, owl_target),
		    close(Fd),
		    free(ME) 
		;
		    not(member(Extension, ['hgp','owl'])) ->
		    @app?mainMenu->>msgBox(string('Save-as failed. Unknown extension. Please do not use period symbols (dots) in your filename.', Extension),alarm), !,
		    fail
		)
	).
%%

%%
exportLegacy(G):->
	%export the current model to legacy code. We let a specialized legacy dialog do its thing
	unless
	    get(@model, modelState, legacy)	
	do
	(
		ensure_loaded(main(export_dialog)),
		new(exportDialog(G?mainMenu))->>openDialog
	).
%%

%%
loadLegacy(G):->
	%load a legacy model.
	%lots of legac code with dutch comments
	
	% G->>checkChanged, %close all windows except mainmenu Multiple Model Support. No need to save now
	if
	    FileName = @garp3_finder<<-file(G?mainMenu, 'Select a legacy model', @on,G?legacy_extension,G?fileDir)
	then
	(
	    %ok, we go to legacy mode, closing the model and all
	    (
		/* If the model is already loaded */
		get(G?modelsTabs, keys, TabsChain),
		chain_list(TabsChain, Tabs),
		member(Tab, Tabs),
		/* Create a temp file to get the base name */
		new(TempFile, file(FileName)),
		get(TempFile, base_name, FileNameBase),
		free(TempFile),
		get(Tab, realTabName, FileNameBase) ->
		/* Give a warning and cancel the loading */
		G?mainMenu->>msgBox(string('This file is already loaded. Loading cancelled.'),alarm)
	    ;
		/* Otherwise load the model */
		(
		    /* Check if the model can be loaded in the top tab */
		    get(G?tabTool, on_top, Tab),
		    get(G?modelsTabs, member, Tab, ActiveModel),
		    get(ActiveModel, filename, @nil),
		    get(ActiveModel, changed, @off) ->
		    send(G, loadLegacy2, FileName)
		;
		    /* If not, check if a new tab can be opened and load it if possible */
		    send(G, toManyModels) ->
		    send(G, loadLegacy2, FileName)
		)
	    )
	).
loadLegacy2(G, FileName):->
    G->>mustReloadModel(@on), %engine has wrong model loaded
    (
	/* Check if the current tab is a new unchanged model */
	get(G?tabTool, on_top, Tab),
	get(G?modelsTabs, member, Tab, ActiveModel),
	get(ActiveModel, filename, @nil),
	get(ActiveModel, changed, @off) ->
	/* In that case, close the current tab */
	send(G, closeModel, Tab)
    ;
	true
    ),
    send(G, newModel),
    send(@model, modelState, legacy),
    G->>mustReloadModel(@off), %no reloading required
    send(@model, slot, filename, FileName),
    send(G, loadLegacy3, FileName).


loadLegacy3(G, FileName):->
    G->>mustReloadModel(@off), %no reloading required
    File *= file(FileName),
    Dir = File<<-directory_name,
    %format('De directory is: ~w\n', [Dir]),
    G->>fileDir(File?directory_name),
    File->>close,
    %just load all models
    G?mainMenu->>modelChanged,
    G?mainMenu->>collapse_expand_dlg(G?mainMenu?modelTool,@off),
    G?mainMenu->>collapse_expand_dlg(G?mainMenu?buildTool,@off),
    G?mainMenu->>collapse_expand_dlg(G?mainMenu?simulateTool,@on),
    %nothing to do but:
    (
	_Visi = G?members<<-find(->>(@arg1,instance_of,visigarp))
    ;
	enginectrl:command(loadLegacy(Dir)),
	G->>startVisualize
    ).

%%
exportText(G):->
	%gp3 1.4: export to text file
	
	(
		get(@model, filename, @nil)
		%@nil = @model<<-filename % JL
		->
		(
			DefDir = G<<-fileDir,
			DefFile = @default
		)
		;
		(
			%File *= file(G?filename),
    			File *= file(@model?filename), % JL
			DefDir = File<<-directory_name,
			DefFileWithExtension = File<<-base_name,
			string_length(DefFileWithExtension, Length),
			LengthMinusExtension is Length - 4, %again: we assume all extensions are 3 chards, like they were in 1980?
			sub_atom(DefFileWithExtension, 0, LengthMinusExtension, _DefFileLength, DefFile)
		)
	),

	%get the mainMenu
	M = G<<-mainMenu,
	if
		FileName = @garp3_finder<<-file(M,'Export to text file',@off,'txt',DefDir,DefFile) %file hoeft niet te bestaan
	then
		@model->>exportText(FileName).
%%

%%
saveSimulation(G, Caller: frame):->
	%gp3 rewrite of old visigarp code
	%save simulation data in the model
	%(visigarp did this in a file ofcourse)
	
	\+ get(@model, modelState, legacy), 
	%legacy = @app<<-modelState,
	%ask a name through the savedSimulationList
	L *= savedSimulationList(save, Caller),
	if
		Name = L<<-confirm_centered
	then
	(
		L->>destroy,
		G->>pleaseWait(run),
		G->>saveSimulation2(Name),
		G->>thankYou,
		G->>setModelChanged %no conceptual change
	).

saveSimulation2(_G, Name: name) :->
	Saved *= hash_table, %gp3 0.3: we save in a hash_table in order te have room for metadata
	Saved->>append(modelVersion,number(@model?lastChangeTimeValue)),
	Data *= chain,
	visualize:save_in_chain(Data), %rewrite in globals.pl
	Saved->>append(data,Data),
	@model?savedSimulations->>append(Name,Saved). %possibly overwriting old one


loadSimulation(G, Caller: frame):->
	%gp3 rewrite of old visigarp code
	%save simulation data in a file. You can reload the simulation data and continue the
	%simulation, but you will have to reload the same model as well

	\+ get(@model, modelState, legacy), 
	%legacy = @app<<-modelState,
	%get the name from the list
	L *= savedSimulationList(load, Caller),
	if
	(
		Name = L<<-confirm_centered,
		Saved = @model?savedSimulations<<-member(Name),
		Data = Saved<<-member(data) %just get the chain containing state info (gp3 0.3)
	)
	then
	(
		G->>pleaseWait(run),
		L->>destroy,
		G->>closeVisualize, %close old one
		%clear all data that could be overwritten in the same way as did visigarp:
		%by reloading the model
		enginectrl:command(loadmodel),
		G->>mustReloadModel(@off),
		visualize:load_from_chain(Data), %rewrite in globals.pl,
		visualize:reopen_visualize,
		if
			engine:state(_,smd(input_system(ISName),_,_,_,_,_,_)) %any state will do
		then
			G->>visualizing(ISName)
		else
			G?mainMenu->>modelChanged, %but the menu should update anyway
		G->>thankYou
	).
%
removeSavedSimulation(G,Name: name):->
	%gp3
	%delete a saved simulation from the model, thereby saving a lot of diskspace
	\+ get(@model, modelState, legacy), 
	%legacy = @app<<-modelState,
	if
		@model?savedSimulations<<-member(Name)
	then
	(
		@model?savedSimulations->>delete(Name),
		G->>setModelChanged %no conceptual change
	).
%%

%%%%%%% Sketch File handling: open, save, remove

%%
saveSketch(G, Caller: frame, Type: name):->
	%save sketch data in the model
	
	\+ get(@model, modelState, legacy), 
	%legacy = @app<<-modelState,
	%ask a name through the savedSketchList
	L *= savedSketchList(save, Caller, Type),
	if
		Name = L<<-confirm_centered
	then
	(
		L->>destroy,
		G->>pleaseWait(run),
		Saved *= hash_table, %gp3 0.3: we save in a hash_table in order to have room for metadata
		Saved->>append(modelVersion,number(@model?lastChangeTimeValue)),
		% Data *= chain,

                % Save actual content, AB, nov 2006
                get(@model, hypered, Type, Data), 

	        Copy = Data<<-createCopy(new(hash_table)), %empty mappings table to start with
                Saved->>append(data, Copy),
                Saved->>append(type, Type),
		@model?savedSketches->>append(Name,Saved), %possibly overwriting old one
	        Caller?sketch->>savedSketchName(Name), % set savedSketchName
	        Caller->>updateWindowLabel, % update the label of the sketch editor
		G->>thankYou,
		G->>setModelChanged %no conceptual change
	).
	
loadSketch(G, Caller: frame, Type: name):->
	%load sketch data from the model. 

	\+ get(@model, modelState, legacy), 
	%legacy = @app<<-modelState,
	%get the name from the list
	L *= savedSketchList(load, Caller, Type),
	if
	(
		Name = L<<-confirm_centered,
		Saved = @model?savedSketches<<-member(Name),
		Data = Saved<<-member(data) % get the chain containing the info
	)
	then
	(
		G->>pleaseWait(run),
		L->>destroy,

                % To Do: Close all sketches (of the current sketch type) 
                % G->>closeSketch
		G->>mustReloadModel(@off),
                get(@model, hypered, Type, SK), 
                get(@model, sketches, Sketches), 
                % remove SK from Sketches chain
                send(Sketches, delete_all, SK), % always succeeds, even if Sketches does not contain SK
                debug(sketch(stg), 'loadSketch free SK ~w etc.\n',[SK]), 
                send(SK, free), 
                
                % Load actual content, AB, nov 2006

                % Perhaps this should be done with a change request
	        Sketch = Data<<-createCopy(new(hash_table)), %empty mappings table to start with
                % Sketch = Data<<-copy, 
                send(@model, hyper, Sketch, Type, Type), 
                send(Sketches, append, Sketch),
	        Caller->>slot(sketch, Sketch), % set the new Sketch as the content for the editor
	        Caller?sketch->>savedSketchName(Name), % set savedSketchName
	        Caller->>updateWindowLabel, % update the label of the sketch editor

	        SkipSaveLayout = @on, 
                send(Caller, update_visualisation, SkipSaveLayout),

		% is this always true?
		G?mainMenu->>modelChanged, %but the menu should update anyway
		G->>thankYou
	).
%
removeSavedSketch(G,Name: name):->
	%gp3
	%delete a saved sketch from the model, thereby saving a lot of diskspace
	\+ get(@model, modelState, legacy), 
	%legacy = @app<<-modelState,
	if
		@model?savedSketches<<-member(Name)
	then
	(
		@model?savedSketches->>delete(Name),
		G->>setModelChanged %no conceptual change
	).
%%

%%%%%%% End of Sketch file handling


:-pce_end_class.

:-pce_begin_class(
		  pleaseWait,
		  dialog
		 ).
%used by @app in pleaseWait, pleaseWaitStatus and thankYou
%only windowdef, behaviour in @app, see also main file startup.pl

initialise(D,Status: char_array, Frame: [frame]):->
	%Display this window
	%if Frame is given, the window will center around this
	
	D->+initialise,
	D->>background(white),
	D->>application(@app),
	%D->>modal(application), %gp3 0.2: splash is not modal, because pce will not fully restore event functionality of windows created while this one is modal. This way it works fine (window is only shown when busy)
	
	% This windows should also be minimizable
	%D->>kind(popup),
	D->>label('Please wait...'),
	
	D->>gap(size(0,0)),
	D->>border(size(6,6)),
	D->>pen(0),
	General *= label(general,Status,font(helvetica,bold,14)),
	General->>width(0), %make sure it is minimal
	D->>display(General,point(18,18)), %12 points into the box

	get_image(helpers,'pleasewait',Img),
	LogoLabel *= label(logo,Img),
	D->>display(LogoLabel, point(General?right_side + 18,6)),
	
	%box behind the text	
	Box *= box(General?width + 24, LogoLabel?height), %spacing
	Box->>pen(0),
	BG = new(colour)<<-convert('#cccccc'), %buggy: cannot use the hex value directly
	Box->>fill_pattern(BG),
	D->>display(Box,point(6,6)),
	Box->>hide, %behind text
	General->>set(y:=Box?top_side + (Box?height - General?height) / 2),
	if
		Frame = @default
	then
		D->>open_centered
	else
		D->>open_centered(Frame?area),
	@display->>busy_cursor,
	sleep(0.01), %gp3 1.0.0 Make sure the window will be created before flushing the display
	D->>synchronise,
        % put window on top
        send(D?frame, expose).
%%

%%
destroy(D):->
	%gp3 0.3: remove busy_cursor
	@display->>busy_cursor(@nil),
	D->+destroy.
%%

:-pce_end_class.
%%
