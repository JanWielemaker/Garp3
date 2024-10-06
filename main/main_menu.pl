/*
Garp3 0.2
The application mainmenu + helper classes

Part of Garp3 - see copyright notice
*/

:-pce_begin_class(
		  mainMenu,
		  frame,
		  "The garp3 main menu"
		 ).

variable(fixedWidth,int,both). %width of the dialogs inside the frame
variable(sideMargin,int,both). %how much space left and right inside tools
variable(holder,dialog,both).
variable(helpTool,dialog_group,both).
variable(modelTool,dialog_group,both).
variable(buildTool,dialog_group,both).
variable(simulateTool,dialog_group,both).
variable(aboutTool,dialog_group,both).
variable(tabToolSize, size:=size(10,17),both). % JL Size of TabTool

initialise(G):->
	%Display the main menu
	G->>fixedWidth(405),
	G->>sideMargin(10),

	G->+initialise(@app?name),
	G->>application(@app),
	G->>kind(toplevel),
	G->>can_resize(@on), % Set me to @on to allow users to minimize
	@app->>mainMenu(G), %register ourself with the app
	G->>background('#FFFFFF'),
	G->>done_message(->>(G,onDestroy)),
	G->>confirm_done(@off), %niet vragen
	make_icon(windows,main,Icon), %create the icon
	G->>icon(Icon),

	Holder *= dialog,
	G->>holder(Holder),
	Holder->>pen(0),
	Holder->>background('#FFFFFF'),
	G->>append(Holder),
	HT = G<<-createHelpTool,
	Holder->>display(HT,point(G?sideMargin,0)),
	MT = G<<-createModelTool,
	Holder->>display(MT,point(G?sideMargin,HT?bottom_side)),
	BT = G<<-createBuildTool,
	Holder->>display(BT,point(G?sideMargin,MT?bottom_side + 1)),
	ST = G<<-createSimulateTool,
	Holder->>display(ST,point(G?sideMargin,BT?bottom_side + 1)),
	AT = G<<-createAboutTool,
	Holder->>display(AT,point(G?sideMargin,ST?bottom_side + 1)),
	%G->>layout,
	G->>width(G?fixedWidth + 2 * G?sideMargin),
	%collapse all tools except model
	G->>toggleTool(BT),
	G->>toggleTool(ST),
	G->>updateVisual, %set title etc
	@app->>initialiseTabs.
%%

%%%
onDestroy(G):-> % JL
	%check if we can destroy this frame
	%set as done_message

	% @app->>checkChanged,!, %ok, we can close
	/* Close all active models (automatically checks if they are changed) */
	get(@app?modelsTabs, keys, Tabs),
	get(Tabs, size, NumberOfTabs),
	numlist(1, NumberOfTabs, TabsToClose),
	forall(
	    member(_TabToClose, TabsToClose),
	    send(@app, closeCurrentModel)
	),

	% First close the open
	G->>wm_delete, %default done_message of this frame
	%only die if not in debug
	(
		runMode(debug)
	;
		die
	).

die :-
	thread_self(main),
	!,
	halt.
die :-
	thread_signal(main, halt).

%%


%%
modelChanged(G):->
	%message from the app (@app) that the design-model has changed
	G->>updateVisual.
%%

%%
reorderTools(G):->
	%gp3 0.2 Make sure the tools are nicely below each other
	G?modelTool->>set(y:= G?helpTool?bottom_side),
	G?buildTool->>set(y:= G?modelTool?bottom_side + 1),
	G?simulateTool->>set(y:= G?buildTool?bottom_side + 1),
	G?aboutTool->>set(y:=G?simulateTool?bottom_side + 1),
	%fix main size
	G?holder->>height(G?aboutTool?bottom_side).
%%

%%
toggleTool(G, Tool: dialog_group):->
	%gp3 0.2 hide or show the content part of a tool

	C= Tool<<-content,
	if
		@on = C<<-displayed
	then
	(
		Tool->>height(Tool?collapsedHeight),
		C->>displayed(@off)
	)
	else
	(
		C->>displayed(@on),
		Tool->>height(C?bottom_side)
	),
	%reorder all tools
	G->>reorderTools.
%%

%%
collapse_expand_dlg(G, Tool: dialog_group, OnOrOff: bool):->
	%helper for application, who closes some stuff with us
	%(but we use it took in initialise)
	%this is for legacy reasons, can be changed in next version

	catch(C = Tool<<-content,_,fail),
	unless
		OnOrOff = C<<-displayed
	do
		G->>toggleTool(Tool).
%%

%%
updateVisual(G):->
	%update all status and member activation
	send(@app?tabTool,layout_dialog),
	send(@app?tabTool, update_tabnames), % JL: make the tabs smaller if neccessary
	send(@app?tabTool, layout_labels),
	send(@app?tabTool,redraw, @default), % JL: update the tab toolbar

	G->>updateModelTool,
	G->>updateBuildTool,
	G->>updateSimulateTool,
	%and global
	if
		get(@model, modelState, loaded) %JL
	then
		G->>label(string('%s - %s %s',@model?name,@app?name,@app?version))
	else
		G->>label(string('%s %s',@app?name,@app?version)).
%%

%%
typed(D, E: event_id):->
	%catch some keys just for debug
	(E == 120
	->
		@pce->>expose_console
	;
		true
	),
	(E == 122
	->
		@pce->>show_console(hidden)
	;
		true
	),
	D->+typed(E).
%%

%%%%% HELPTOOL %%%%%
createHelpTool(G, HT: dialog_group):<-
	%gp3 0.2 Create the logotool

	HT *= dialog_group(helpTool,group),
	G->>helpTool(HT),
	HT->>width(G?fixedWidth),
	get_image(mainmenu,'00',HelpImg),
	Help *= clickImage(help,HelpImg,->>(@app,openHelp,'Main'),tooltipMsg := 'Open main help page'),
	get_image(mainmenu,'01',CloseImg),
	% Close *= clickImage(quit,CloseImg,->>(G,onDestroy),tooltipMsg := 'Quit Garp3'), % JL Adapt to close model
	Close *= clickImage(close,CloseImg,->>(@app,closeCurrentModel),tooltipMsg := 'Close Model'), % JL New Close button behaviour
	%right to left
	FromRight *= number(G?fixedWidth),
	HT->>display(Close,point(FromRight - Close?width,0)), %just hidden, they donot want it *now* % JL We do now. :)
	FromRight->>minus(Close?width),
	HT->>display(Help,point(FromRight - Help?width,0)),
	HT->>height(Help?bottom_side),
	HT->>display(@app?tabTool,point(0,Help?bottom_side-17)). % JL: Tabs - stolen from Joram. :) %works great if you change the tabtool ???
%%

/* MODELTOOL */
createModelTool(G, MT: dialog_group):<-
	MT *= dialog_group(modelTool,group),
	G->>modelTool(MT),
	MT->>width(G?fixedWidth),
	get_image(mainmenu,'a1',FileImg),
	FileIcon *= clickImage(file,FileImg,->>(G,toggleTool,MT),
		tooltipMsg := when(MT?content?displayed == @on,'Hide file tasks','Show file tasks')),
	MT->>display(FileIcon,point(1,1)),
	Status *= label(status,'Empty model',font(helvetica,roman,10)),
	MT->>display(Status,point(FileIcon?right_side + 2,FileIcon?top_side + FileIcon?height / 2)),
	%logo
	get_image(mainmenu,'logo',LogoImg),
	Logo *= clickImage(logo,LogoImg,->>(G,onLogo),tooltipMsg := 'Open main page QRM portal'),
	MT->>display(Logo,point(G?fixedWidth - Logo?width -1,1)), %space for box
	%box
	Box *= box(G?fixedWidth,FileIcon?bottom_side + 2), %zero-based + below icons
	Box->>pen(1),
	Box->>colour('#D4D4D4'),
	MT->>display(Box,point(0,0)),
	MT->>attribute(collapsedHeight,Box?bottom_side),

	%content for the modelTool
	C *= dialog_group(content,group),
	MT->>attribute(content,C),
	C->>width(G?fixedWidth),
	get_image(mainmenu,'b1',OpenImg),
	get_image(mainmenu,'b1g',OpenImg_Grey),
	Open *= clickImage(open,OpenImg,->>(@app,loadModel),OpenImg_Grey,'Open model from file'),
	C->>display(Open,point(1,1)),
	get_image(mainmenu,'b2',SaveImg),
	get_image(mainmenu,'b2g',SaveImgG),
	Save *= clickImage(save,SaveImg,->>(@app,save),SaveImgG,'Save current model to file'),
	C->>display(Save,point(Open?right_side + 1, 1)),
	get_image(mainmenu,'b3',SaveAsImg),
	get_image(mainmenu,'b3g',SaveAsImgG),
	SaveAs *= clickImage(saveAs,SaveAsImg,->>(@app,saveAs),SaveAsImgG,'Save current model to new file'),
	C->>display(SaveAs,point(Save?right_side + 1, 1)),
	get_image(mainmenu,'b4',NewImg),
	get_image(mainmenu,'b4g',NewImgG),
	New *= clickImage(new,NewImg,->>(@app,newModel),NewImgG,'Start new model'),
	C->>display(New,point(SaveAs?right_side + 1, 1)),
	Line *= line(New?right_side + 1,0, New?right_side + 1,Open?bottom_side + 2),
	Line->>pen(1),
	Line->>colour(Box?colour),
	C->>display(Line),
	get_image(mainmenu,'b5',OpenLegacyImg),
	get_image(mainmenu,'b5g',OpenLegacyImgG),
	OpenLegacy *= clickImage(openLegacy,OpenLegacyImg,->>(@app,loadLegacy),OpenLegacyImgG,'Open model in legacy mode'),
	C->>display(OpenLegacy,point(Line?right_side + 1, 1)),
	get_image(mainmenu,'b6',ExportLegacyImg),
	get_image(mainmenu,'b6g',ExportLegacyImgG),
	ExportLegacy *= clickImage(exportLegacy,ExportLegacyImg,->>(@app,exportLegacy),ExportLegacyImgG,'Save model in legacy mode'),
	C->>display(ExportLegacy,point(OpenLegacy?right_side + 1, 1)),
	CBox *= box(G?fixedWidth,Open?bottom_side + 2),
	CBox->>pen(1),
	CBox->>colour('#D4D4D4'),
	C->>display(CBox,point(0,0)),
	C->>height(CBox?bottom_side),

	MT->>display(C,point(0,Box?bottom_side - 1)), %overlap

	MT->>height(C?bottom_side),
	G->>updateModelTool.
%%


%%
updateModelTool(G):->
	MT = G<<-modelTool,
	get(@model, modelState, S), % JL
	%and show what must be shown
	doUpdateModelTool(MT,S,Status, Save, SaveAs,ExportLegacy),
	MT?status_member->>selection(Status),
	MT?content?save_member->>active(Save),
	MT?content?saveAs_member->>active(SaveAs),
	MT?content?exportLegacy_member->>active(ExportLegacy).
%
doUpdateModelTool(_MT,new,Status,@off,SaveAs,@off):-!,
	%new model, not saved. Changed?
	Changed = @model<<-changed,
	if
		Changed == @on
	then
	(
		%gp3 1.4.0: add language if there are more than 1
		/*
		%gp3 1.4.1: BB requested this to be turned off..
		if
			1 = @model?translator?languages<<-size
		then
			Lang = ''
		else
			Lang *= string(' [%s] ', @model?translator?currentLanguage),
		Status *= string('New model %s- changed',Lang),
		*/
		Status = 'New model - changed',
		SaveAs = @on
	)
	else
	(
		Status = 'Empty model',
		SaveAs = @off
	).

doUpdateModelTool(_MT,loaded,Status,@on,@on,@on):-!,
	%existing model. Changed?
	%(save is allways on to give the opportunity to save layout changes)
	Changed = @model<<-changed,

	%gp3 1.4.0: add language if there are more than 1

	/*
	%gp3 1.4.1: BB requested this to be turned off..

	if
		1 = @model?translator?languages<<-size
	then
		Lang = ''
	else
		Lang *= string(' [%s] ', @model?translator?currentLanguage),
	if
		Changed == @on
	then
	(
		Status *= string('%s %s- changed',@model?name,Lang)
	)
	else
	(
		Status *= string('%s %s- unchanged',@model?name,Lang)
	)
	*/
	if
		Changed == @on
	then
	(
		Status *= string('%s - changed',@model?name)
	)
	else
	(
		Status *= string('%s - unchanged',@model?name)
	).

doUpdateModelTool(_MT,legacy,Status,@off,@off,@off):-!,
	%legacy model, we cannot save, but we can open and create a new one
	Status = 'Legacy model loaded'.

%%
onLogo(_G):->
	web:open(garp3). %main page
%%

/* BUILDTOOL */
createBuildTool(G, BT: dialog_group):<-
	BT *= dialog_group(modelTool,group),
	G->>buildTool(BT),
	BT->>width(G?fixedWidth),
	get_image(mainmenu,'c1',BuildImg),
	Build *= clickImage(build,BuildImg,->>(G,toggleTool,BT),
		tooltipMsg := when(BT?content?displayed == @on,'Hide build tasks','Show build tasks')),
	BT->>display(Build,point(1,1)),
	Status *= label(status,'No change',font(helvetica,roman,10)),
	BT->>display(Status,point(Build?right_side + 2,Build?top_side + Build?height / 2)),
	%box
	Box *= box(G?fixedWidth,Build?bottom_side + 2), %zero-based + below icons
	Box->>pen(0),
	Colour = new(colour)<<-convert('#c3d1d4'), %buggy: cannot use the hex value directly in fill_pattern, nor in colour constructor
	Box->>fill_pattern(Colour),
	BT->>display(Box,point(0,0)),
	Box->>hide, %to the back
	BT->>attribute(collapsedHeight,Box?bottom_side),

	%content for the buildTool
	C *= dialog_group(content,group),
	BT->>attribute(content,C),
	C->>width(G?fixedWidth),
	get_image(mainmenu,'d1',EntitiesImg),
	get_image(mainmenu,'d1g',EntitiesImgG),
	Entities *= clickImage(entities,EntitiesImg,->>(@app,openEntities),EntitiesImgG,'Open entity hierarchy editor'),
	C->>display(Entities,point(2,2)), %space to create a border using CBOx below
	EntitiesL *= label(entitiesCount,'0',font(helvetica,roman,10)),
	EntitiesL->>width(0), %recompute size
	C->>display(EntitiesL,point(Entities?right_side - EntitiesL?width,Entities?top_side)),
	get_image(mainmenu,'d2',AttImg),
	get_image(mainmenu,'d2g',AttImgG),
	Attributes *= clickImage(attributes,AttImg,->>(@app,openAttributeDefinitions),AttImgG,'Open attribute definitions editor'),
	C->>display(Attributes,point(Entities?right_side, 2)),
	AttributesL *= label(attributesCount,'0',font(helvetica,roman,10)),
	AttributesL->>width(0), %recompute size
	C->>display(AttributesL,point(Attributes?right_side - AttributesL?width,Attributes?top_side)),
	get_image(mainmenu,'d3',ConfImg),
	get_image(mainmenu,'d3g',ConfImgG),
	Confs *= clickImage(configurations,ConfImg,->>(@app,openConfigurationDefinitions),ConfImgG,'Open configuration definitions editor'),
	C->>display(Confs,point(Attributes?right_side, 2)),
	ConfsL *= label(confsCount,'0',font(helvetica,roman,10)),
	ConfsL->>width(0), %recompute size
	C->>display(ConfsL,point(Confs?right_side - ConfsL?width,Confs?top_side)),
	get_image(mainmenu,'d4',QImg),
	get_image(mainmenu,'d4g',QImgG),
	QD *= clickImage(quantities,QImg,->>(@app,openQuantityDefinitions),QImgG,'Open quantity definitions editor'),
	C->>display(QD,point(Confs?right_side, 2)),
	QDL *= label(qdCount,'0',font(helvetica,roman,10)),
	QDL->>width(0), %recompute size
	C->>display(QDL,point(QD?right_side - QDL?width,QD?top_side)),
	get_image(mainmenu,'d5',QSImg),
	get_image(mainmenu,'d5g',QSImgG),
	QS *= clickImage(quantitySpaces,QSImg,->>(@app,openQuantitySpaces),QSImgG,'Open quantity space definitions editor'),
	C->>display(QS,point(QD?right_side, 2)),
	QSL *= label(qsCount,'0',font(helvetica,roman,10)),
	QSL->>width(0), %recompute size
	C->>display(QSL,point(QS?right_side - QSL?width,QS?top_side)),
	get_image(mainmenu,'d6',AboutImg),
	get_image(mainmenu,'d6g',AboutImgG),
	About *= clickImage(aboutModel,AboutImg,->>(@app,openMetadata),AboutImgG,'About this model and Sketch'),
	C->>display(About,point(QS?right_side + 2, 2)),	%extra space

	%second row
	get_image(mainmenu,'e1',ScenariosA),
	get_image(mainmenu,'e1g',ScenariosI),
	Scenarios *= clickImage(scenarios,ScenariosA,->>(@app,openScenarios),ScenariosI,'Open scenarios editor'),
	C->>display(Scenarios,point(2,Entities?bottom_side)),
	ScenariosL *= label(scenariosCount,'0',font(helvetica,roman,10)),
	ScenariosL->>width(0), %recompute size
	C->>display(ScenariosL,point(Scenarios?right_side - ScenariosL?width,Scenarios?top_side)),
	get_image(mainmenu,'e2',CScenarioA),
	get_image(mainmenu,'e2g',CScenarioI),
	CurrentSc *= clickImage(currentScenario,CScenarioA,->>(@app,openViewEditor, @app?model?lastEditedIS),CScenarioI,create(string,'Edit scenario: \'%s\'', @app?model?lastEditedIS?name), 'Edit last changed scenario'),
	C->>display(CurrentSc,point(Scenarios?right_side,Scenarios?top_side)),
	get_image(mainmenu,'e3',ModelfragmentsA),
	get_image(mainmenu,'e3g',ModelfragmentsI),
	Modelfragments *= clickImage(modelfragments,ModelfragmentsA,->>(@app,openModelFragments),ModelfragmentsI,'Open model fragments editor'),
	C->>display(Modelfragments,point(CurrentSc?right_side,Entities?bottom_side)),
	ModelfragmentsL *= label(modelfragmentsCount,'0',font(helvetica,roman,10)),
	ModelfragmentsL->>width(0), %recompute size
	C->>display(ModelfragmentsL,point(Modelfragments?right_side - ModelfragmentsL?width,Modelfragments?top_side)),
	get_image(mainmenu,'e4',CMFA),
	get_image(mainmenu,'e4g',CMFI),
	CurrentMF *= clickImage(currentModelfragment,CMFA,->>(@app,openViewEditor, @app?model?lastEditedMF),CMFI,create(string,'Edit model fragment: \'%s\'', @app?model?lastEditedMF?name),'Edit last changed model fragment'),
	C->>display(CurrentMF,point(Modelfragments?right_side,Scenarios?top_side)),
	get_image(mainmenu,'e5',AgentsA),
	get_image(mainmenu,'e5g',AgentsI),
	Agents *= clickImage(agents,AgentsA,->>(@app,openAgents),AgentsI,'Open agent hierarchy editor'),
	C->>display(Agents,point(CurrentMF?right_side,Entities?bottom_side)),
	AgentsL *= label(agentsCount,'0',font(helvetica,roman,10)),
	AgentsL->>width(0), %recompute size
	C->>display(AgentsL,point(Agents?right_side - AgentsL?width,Agents?top_side)),
	get_image(mainmenu,'e6',AssumptionsA),
	get_image(mainmenu,'e6g',AssumptionsI),
	Assumptions *= clickImage(assumptions,AssumptionsA,->>(@app,openAssumptions),AssumptionsI,'Open assumptions hierarchy editor'),
	C->>display(Assumptions,point(Agents?right_side,Entities?bottom_side)),
	AssumptionsL *= label(assumptionsCount,'0',font(helvetica,roman,10)),
	AssumptionsL->>width(0), %recompute size
	C->>display(AssumptionsL,point(Assumptions?right_side - AssumptionsL?width,Assumptions?top_side)),
	CBox *= box(G?fixedWidth, Scenarios?bottom_side),
	CBox->>pen(0),
	CBox->>fill_pattern(Colour),
	C->>display(CBox,point(0,0)),
	CBox->>hide, %to the back
	C->>height(CBox?bottom_side + 1),

	BT->>display(C,point(0,Box?bottom_side + 1 )), %space

	BT->>height(C?bottom_side),
	G->>updateBuildTool.
%%

%%
updateBuildTool(G):->
	BT = G<<-buildTool,
	get(@model, modelState, S), % JL
	%and show what must be shown
	doUpdateBuildTool(BT,S, Status, Ent, EntCount, Attr, AttrCount,
			Conf, ConfCount, QD, QDCount, QS, QSCount, About,
			Sce, SceCount, CurSc, MF, MFCount, CurMF,
			Ag, AgCount, Ass, AssCount,AboutModel ),
	BT?status_member->>selection(Status),
	G->>buildSetElement(BT,entities,Ent,entitiesCount,EntCount),
	G->>buildSetElement(BT,attributes,Attr,attributesCount,AttrCount),
	G->>buildSetElement(BT,configurations,Conf,confsCount,ConfCount),
	G->>buildSetElement(BT,quantities,QD,qdCount,QDCount),
	G->>buildSetElement(BT,quantitySpaces,QS,qsCount,QSCount),
	G->>buildSetElement(BT,aboutModel,About),
	G->>buildSetElement(BT,scenarios,Sce,scenariosCount,SceCount),
	G->>buildSetElement(BT,currentScenario,CurSc),
	G->>buildSetElement(BT,modelfragments,MF,modelfragmentsCount,MFCount),
	G->>buildSetElement(BT,currentModelfragment,CurMF),
	G->>buildSetElement(BT,agents,Ag,agentsCount,AgCount),
	G->>buildSetElement(BT,assumptions,Ass,assumptionsCount,AssCount),
	G->>buildSetElement(BT,aboutModel,AboutModel).
%
doUpdateBuildTool(_BT,legacy,'You cannot edit a legacy model',
	@off,'',@off,'',
	@off,'',@off,'',@off,'',@off,
	@off,'',@off,@off,'',@off,
	@off,'',@off,'',@off).
%
doUpdateBuildTool(_BT,_,Status,@on,EntCount,@on,AttrCount,
	@on, ConfCount, @on, QDCount, @on, QSCount, @off,
	@on, SceCount, CurSc, @on, MFCount, CurMF,
	@on, AgCount, @on, AssCount, @on
		):-

	LT = @model<<-lastChangeTime,
	if
		LT = @nil
	then
		Status = ''
	else
	(
		if
			0 = LT<<-difference(new(date),day) %today
		then
			Status *= string('Last change: %u:%02u', LT?hour,LT?minute)
		else
			Status *= (string('Last change: %s %s %s',LT?month_name,LT?day,LT?year))
	),
	EntCount = ?(@model?abstractEntities, find_all, ->>(@arg1, instance_of,entity))<<-size,
	AttrCount = @model?attributeDefinitions<<-size,
	ConfCount = @model?configurationDefinitions<<-size,
	QDCount = @model?quantityDefinitions<<-size,
	QSCount = @model?quantitySpaces<<-size,
	SceCount = ?(@model?modelFragments, find_all, ->>(@arg1, isInputSystem))<<-size,
	if
		@model<<-lastEditedIS
	then
		CurSc = @on
	else
		CurSc = @off,
	MFCount = ?(@model?modelFragments, find_all, not(or(->>(@arg1, isInputSystem),@arg1?parents?size == 0)))<<-size,
	if
		@model<<-lastEditedMF
	then
		CurMF = @on
	else
		CurMF = @off,
	AgCount = ?(@model?abstractEntities, find_all, ->>(@arg1, instance_of,agent))<<-size,
	AssCount = @model?assumptions<<-size.
%
buildSetElement(_G, BT: dialog_group, ElName: name, Active: bool, CountElName: [name], Count: [char_array]):->
	%helper to set elements
	C = BT<<-content,
	(El = BT<<-member(ElName) ; El = C<<-member(ElName)),
	El->>active(Active),
	unless
		CountElName = @default
	do
	(
		(CountEl = BT<<-member(CountElName)
		 ;
		 CountEl = C<<-member(CountElName)
		),
		CountEl->>displayed(Active), %hide when inactive
		CountEl->>selection(Count),
		CountEl->>width(0), %recompute
		CountEl->>set(x:= El?right_side - CountEl?width)
	).
%%

/* * SIMULATETOOLS * */
createSimulateTool(G, ST: dialog_group):<-
	ST *= dialog_group(simulateTool,group),
	G->>simulateTool(ST),
	ST->>width(G?fixedWidth),
	get_image(mainmenu,'f1',SimImg),
	Sim *= clickImage(simulate,SimImg,->>(G,toggleTool,ST),
		tooltipMsg := when(ST?content?displayed == @on,'Hide simulate tasks','Show simulate tasks')),
	ST->>display(Sim,point(1,1)),
	Status *= label(status,'No simulation',font(helvetica,roman,10)),
	Status->>colour('#FFFFFF'),
	ST->>display(Status,point(Sim?right_side + 2,Sim?top_side + Sim?height / 2)),
	%box
	Box *= box(G?fixedWidth,Sim?bottom_side + 2), %zero-based + below icons
	Box->>pen(0),
	Colour = new(colour)<<-convert('#5E3950'), %buggy: cannot use the hex value directly in fill_pattern, nor in colour constructor
	Box->>fill_pattern(Colour),
	ST->>display(Box,point(0,0)),
	Box->>hide, %to the back
	ST->>attribute(collapsedHeight,Box?bottom_side),

	%content for the simulate tool
	C *= dialog_group(content,group),
	ST->>attribute(content,C),
	C->>width(G?fixedWidth),
	get_image(mainmenu,'g1',SceA),
	get_image(mainmenu,'g1g',SceI),
	%Sce *= clickImage(scenarios,SceA,->>(@app,startVisualize),SceI,when(@app?modelState == legacy, 'Select a scenario to simulate', when(@app?model?inputSystems?size == 1, create(string,'Simulate scenario: \'%s\'' , @app?model?inputSystems?head?name), 'Select a scenario to simulate')), 'Select a scenario to simulate'), % JL
	Sce *= clickImage(scenarios,SceA,->>(@app,startVisualize),SceI,when(@app?model?modelState == legacy, 'Select a scenario to simulate', when(@app?model?inputSystems?size == 1, create(string,'Simulate scenario: \'%s\'' , @app?model?inputSystems?head?name), 'Select a scenario to simulate')), 'Select a scenario to simulate'), % JL. Fixed by JJ: I saw there was no tooltip
	C->>display(Sce,point(1,1)),
	get_image(mainmenu,'g2',CSceA),
	get_image(mainmenu,'g2g',CSceI),
	CSce *= clickImage(currentScenario,CSceA,->>(@app,startVisualize,@app?model?lastEditedIS),CSceI,create(string,'Simulate scenario: \'%s\'', @app?model?lastEditedIS?name),'Simulate current scenario'),
	C->>display(CSce,point(Sce?right_side + 1, 1)),
	get_image(mainmenu,'g3',FullA),
	get_image(mainmenu,'g3g',FullI),
	Full *= clickImage(full,FullA,->>(@app,startFullSimulation,@app?model?lastEditedIS),FullI,create(string,'Full simulation scenario: \'%s\'', @app?model?lastEditedIS?name), 'Full simulation'),
	C->>display(Full,point(CSce?right_side + 1, 1)),
	get_image(mainmenu,'g4',OpenSimA),
	get_image(mainmenu,'g4g',OpenSimI),
	Open *= clickImage(openSim,OpenSimA,->>(@app,reopenVisualize),OpenSimI,'Open the simulator to its current state or a saved simulation'),
	C->>display(Open,point(Full?right_side + 1, 1)),
	Line *= line(Open?right_side + 1,0, Open?right_side + 1,Open?bottom_side + 2),
	Line->>pen(1),
	Line->>colour('#FFFFFF'),
	C->>display(Line),
	get_image(mainmenu,'g5',TracerA),
	get_image(mainmenu,'g5g',TracerI),
	Tracer *= clickImage(tracer,TracerA,->>(@app,openTracer),TracerI,'Open trace window'),
	C->>display(Tracer,point(Line?right_side + 1, 1)),
	get_image(mainmenu,'g6',PrefsA),
	get_image(mainmenu,'g6g',PrefsI),
	Prefs *= clickImage(prefs,PrefsA,->>(@app,runPrefs,G),PrefsI,'Simulation preferences'),
	C->>display(Prefs,point(Tracer?right_side + 1, 1)),

	CBox *= box(G?fixedWidth, Prefs?bottom_side),
	CBox->>pen(0),
	CBox->>fill_pattern(Colour),
	C->>display(CBox,point(0,0)),
	CBox->>hide, %to the back

	C->>height(CBox?bottom_side),

	ST->>display(C,point(0,Box?bottom_side + 1)),

	ST->>height(C?bottom_side),
	G->>updateSimulateTool.
%%

%%
updateSimulateTool(G):->
	ST = G<<-simulateTool,
	get(@model, modelState, S), % JL
	%and show what must be shown
	doUpdateSimulateTool(ST,S,Status,Sce,CSce,Full,Open,Prefs),
	ST?status_member->>selection(Status),
	ST?content?scenarios_member->>active(Sce),
	ST?content?currentScenario_member->>active(CSce),
	ST?content?full_member->>active(Full),
	ST?content?openSim_member->>active(Open), %in legacy when there are states, otherwise when there are scenarios (need to be able to go to saved states)
	%tracer is just always available (?)
	ST?content?tracer_member->>active(@on),
	ST?content?prefs_member->>active(Prefs).
%
doUpdateSimulateTool(_ST,legacy,'Legacy mode',@on,@off,@off,Open,@off):-
	if
		engine:state(_,_)
	then
		Open = @on
	else
		Open = @off.
%
doUpdateSimulateTool(_ST, _, 'No scenarios', @off, @off, @off,@off,@on):-
	0 = @model?inputSystems<<-size,!.
%
doUpdateSimulateTool(_ST,_,'Ready',@on,CSce,Full,Open,@on):-
	if
		@model<<-lastEditedIS
	then
	(
		CSce = @on,
		Full = @on
	)
	else
	(
		CSce = @off,
		Full = @off
	),
	if
		@off = @app<<-mustReloadModel
	then
		Open = @on %can reopen the simulate window
	else
	(
		if
			0 = @model?savedSimulations<<-size
		then
			Open = @off
		else
			Open = @on %can choose a saved simulation
	).

%%

%%%%% HELPTOOL %%%%%
createAboutTool(G, AT: dialog_group):<-
	%gp3 0.2 Create the logotool

	AT *= dialog_group(aboutTool,group),
	G->>aboutTool(AT),
	AT->>width(G?fixedWidth),
	get_image(mainmenu,'about',AboutImg),
	About *= clickImage(about,AboutImg,->>(@app,aboutBox),tooltipMsg := string('About the %s software',@app?name)),
	AT->>display(About,point(G?fixedWidth - About?width,0)),
	AT->>height(About?bottom_side).
%%


:-pce_end_class.

