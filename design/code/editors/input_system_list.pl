/*
definitie inputSystemList klasse.
Editor voor het bewerken van inputSystems

based on homer code, so most comments in dutch. gp3 code only where mentioned
2000-2005 Jelmer Jellema, Spin in het Web, spininhetweb.nl
*/

:-pce_begin_class(inputSystemList,
		  assistanceDialog,
		  "Input System List"
		 ).

variable(inSimulatePick,bool,both). %are we in simulate-pick mode (see initialise, open and expose)
%%
initialise(D):->
	%gp3. This is legacy homer code with some changes
	%we added the SimulatePick variable.
	%this will be set in open and expose (using setSimulatePick)
	%setSimulatePick will set title etcetera
	%When SimulatePick is @on
	%the editor is opened for selecting a scenario for simulate
	%no difference in behaviour, except that double-clicking on 
	%a scenario will start the simulator instead of the viewEditor
	
	"Initialise the editor" ::
	get(@model, getModelNameForEditor, 'Scenario Definitions Editor - Build', ModelNameForEditorLabel),
	D->+initialise(ModelNameForEditorLabel,'Build_Scenarios'), %gp3 0.3.13 added helpid
	D->>icon(@build_icon),
	D->>application(@app),
	D->>kind(toplevel),

	%de onderdelen
	%De plaatsing doen we zelf op coordinaten, we gebruiken wel de standaard "gap"
	GapX = D?gap<<-width,
	GapY = D?gap<<-height,

	DefList *= extendedListBrowser(height := 20, width := 40), 
	DefList->>name(defList),		
	DefList->>label('Scenarios:'),
	DefList->>show_label(@on),
	DefList->>select_message(->>(D,
				       onDefSelection)),
	DefList->>style(ok,style(colour:='#cc3366')), %ok mf, paarsig	
	DefList->>style(incomplete,style(colour:='#ff0099')), %structuurfout
	D->>display(DefList,point(GapX,D?topY)), %using topY because of helpbutton

	%gp3 new button for simulate
	Simulate *= imgButton(simulate, tt:='Simulate selected scenario'),
	
	Add *= imgButton(add,tt:='Add scenario'),

	Edit *= imgButton(edit, img:='edit_scenario', tt:='Edit selected scenario'),

	Props *= imgButton(properties, tt:='Show scenario properties'),
	
	%gp3 new button voor copy
	Copy *= imgButton(copy, tt:='Copy selected scenario'),
	
	Remove *= imgButton(remove, tt:='Delete selected scenario'),

	%gp3: Instead of 4 buttons (3 gaps) we have 6 buttons (5 gaps)
	%code changed that way
	
	%buttons komen rechts van de lijst, en New helemaal boven, Remove onder
	%dus de lijst moet ook hoog genoeg zijn en bovendien moet New niet bij de bovenkant
	%van de lijst maar bij de bovenkant van het window in de lijst (anders bij label)

	TotalHeight *= number(Add?height + Simulate?height + Edit?height + Props?height + Copy?height + Remove?height +
					5 * GapY),
					
	DefList->>bottom_side(DefList?list_top + TotalHeight),

	%now we can add the buttons just using GapY
	
	%Add komt dus niet op de top_side van DefList, maar van het lijstgedeelte
	D->>display(Add,point(DefList?right_side + GapX,
						DefList?list_top)),
	
	D->>display(Simulate,point(Add?left_side,
					Add?bottom_side + GapY)),
	D->>display(Edit,point(Add?left_side,
					Simulate?bottom_side + GapY)),
	D->>display(Props, point(Add?left_side,
					Edit?bottom_side + GapY)),
	D->>display(Copy, point(Add?left_side,
					Props?bottom_side + GapY)),
	D->>display(Remove,point(Add?left_side,
				Copy?bottom_side + GapY)),
	D->>updateSpacers, %gp3 0.3.13 needed when assistanceDialog is used, but displayContent is not used to fill the dialog
	D->>assign_accelerators, %nodig voor de accels als je niet append gebruikt	
	D->>confirm_done(@off),
	%some stuff will be set after open/expose, using setSimulatePick (gp3 0.2)
	%so we set a default, just to be sure
	D->>setSimulatePick(@off),
	
	D->>fillDefList,
	D->>onDefSelection,

	%minimal size:
	D->>minimalSize(size(Remove?right_side,Remove?bottom_side)), %abs min

	/* Multiple models */
	new(Garp3EditorFrame, garp3EditorFrame(ModelNameForEditorLabel)),
	get(@model, '_value', GarpModel),
	send(Garp3EditorFrame, associateModel, GarpModel),
	send(Garp3EditorFrame, append, D),

	send(D, init_commands).
%%

%%
init_commands(D) :->
	"Initialise command objects" ::

	send(D, command, 'ScenarioCopy',  key := '\\ec', keystring := '[Alt + C]'),
	send(D, command, 'ScenarioCopy',  key := '\\C-c', keystring := '[CTRL + C]'),
	send(D, command, 'ScenarioPaste', key := '\\ev', keystring := '[Alt + V]'),
	send(D, command, 'ScenarioPaste', key := '\\C-v', keystring := '[CTRL + V]').
%%	

%%
onResize(D, Difference: size):->
	%gp3 0.2: 

	D?defList_member->>pixelWidth(D?defList_member?pixelWidth + Difference?width),
	D?defList_member->>pixelHeight(D?defList_member?pixelHeight + Difference?height),
	D?add_member->>set(x:= D?add_member?left_side + Difference?width),
	D?simulate_member->>set(x:= D?add_member?left_side, y:= D?simulate_member?top_side + Difference?height * 1/5),	
	D?edit_member->>set(x:= D?add_member?left_side, y:= D?edit_member?top_side + Difference?height * 2/5),		
	D?properties_member->>set(x:= D?add_member?left_side, y:= D?properties_member?top_side + Difference?height * 3/5),	
	D?copy_member->>set(x:= D?add_member?left_side, y:= D?copy_member?top_side + Difference?height * 4/5),
	D?remove_member->>set(x:= D?add_member?left_side, y:= D?remove_member?top_side + Difference?height).		
%%

%%
open(D, SimulatePick: bool):->
	%gp3 0.2: open a new dialog, setting the simulatePick mode to the given
	
	D->>setSimulatePick(SimulatePick),
	D->+open.
%%

%%
expose(D, SimulatePick: bool):->
	%gp3 0.2: expose a given dialog, setting the simulatePick mode to the given
	
	D->>setSimulatePick(SimulatePick),
	D->+expose.
%%	

%%
setSimulatePick(D, SP: bool):->
	%gp3 0.2 : which mode, what will be the label and open message?
	%this way, it can be changed to resuse this window
	D->>inSimulatePick(SP),
	if
		SP == @on
	then
	(
		get(@model, getModelNameForEditor, 'Scenario Definitions Editor - Simulate', ModelNameForEditorLabel),
		D->>label(ModelNameForEditorLabel),
		D?defList_member->>open_message(->>(D,simulate)),
		D?simulate_member->>default_button(@on),
		D?simulate_member->>keyboard_focus(@on)
	)
	else
	(
		get(@model, getModelNameForEditor, 'Scenario Definitions Editor - Build', ModelNameForEditorLabel),
		D->>label(ModelNameForEditorLabel),
		D?defList_member->>open_message(->>(D,edit)),
		D?edit_member->>default_button(@on),
		D?edit_member->>keyboard_focus(@on)
	).
%%

%%
label(D,L: name):->
	%zorg ervoor dat het framelabel wordt gezet, niet het window_decorator label
	D?frame->>label(L).
%
label(D, L: name):<-
	L = D?frame<<-label.
%%

%%
fillDefList(D):->
	"Fill the list with inputSystems" ::

	List = D<<-defList_member,
	List->>clear,
	%gp3: changed to account for tooltips
	@model?sortedInputSystems->>for_all(
			->>(D,appendDefListItem,@arg1,List)),
	D->>updateDisplay. %style en label via updateDisplay
%
appendDefListItem(_D,IS: inputSystem, List: list_browser):->
	%grp3 1.4 create a new dict_item and add a tooltip
	%we use a static tooltip because its harder to make it calculated for a dict_item
	%AND because the list gets refilled when something is changed
	
	DI *= dict_item(IS,'',IS),
	C = IS<<-relevantComments,
	if
		0 = C<<-size
	then
		S = string('(no comments)')
	else
	(
		S = C<<-strip
	),
	DI->>tooltip(S,model),
	List->>append(DI).
%%


%%
refillDefList(D):->
	%gewoon de lijst opnieuw vullen en proberen de selectie te bewaren
	(
		IS = D<<-selectedIS
	;
		IS = @nil
	),

	D->>fillDefList,

	%we moeten de oude selectie herstellen
	if
		Sel = D?defList_member?members<<-find(@arg1?object == IS)
	then
		D?defList_member->>selection(Sel),
	D->>onDefSelection. %sowieso buttons bijwerken
%%

%%
onDefSelection(D):->
	%we moeten even kijken wat zoal geactiveerd mag zijn
	if
		D<<-selectedIS
	then
		Active = @on
	else
		Active = @off,

	D?simulate_member->>active(Active),
	D?properties_member->>active(Active),
	D?edit_member->>active(Active),
	D?copy_member->>active(Active),
	D?remove_member->>active(Active).

%%
add(D):->
	%we voegen een nieuwe toe
	new(inputSystemPropsDlg(D))->>newObject.
%%

%%
simulate(D):->
	%gp3. Start simulating, closing this editor if it was opened for picking a simulation
	@app->>startVisualize(D?selectedIS),
	if
		@on = D<<-inSimulatePick
	then
		D->>destroy.
%%
properties(D):->
	%openen de properties dialoog voor de selectie
	%check wel even of het mag (of de properties knop active is)
	%vanwege dat enter ook werkt
	@on = D?properties_member<<-active,
	new(inputSystemPropsDlg(D))->>editObject(D?selectedIS).
%%

%%
edit(D):->
	"Edit button pressed..." ::
	MForIS = D<<-selectedIS,

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

%%
copy(D):->
	%gp3, copy this IS, through a helper (that will not show)
    CHD *= mfCopyHelperDlg(D,D?selectedIS),
    ignore(CHD->>doCopy),
    object(CHD),
    CHD->>destroy.
%%

%%
remove(D):->
	"Remove button pressed..." ::

	%CR checkt de gevolgen wel en waarschuwt eventueel

	@model->>changeRequest(deleteInputSystem,D?selectedIS,D).
%%

%%%%%%%% Copy Paste%%%%%%%%%%%%%%%
onScenarioCopy(D) :->
    send(@copyBuffer, copyScenarioDef, D?selectedIS).
onScenarioPaste(D) :->
    send(@copyBuffer, pasteScenarioDef, D).

%%

%%%%%%%%%%Helpers%%%%%%%%%%%%%%%%
%%
updateDisplay(D):->
	%werk alle items bij qua naam en style
	%zonder ze te verwijderen e.d.
	
	D?defList_member?members->>for_all(
		->>(D,updateElement,@arg1)).
%
updateElement(_D,Item: dict_item):->
	%helper voor updateDisplay
	
	Item->>label(string('%s%s',Item?object?name,
				when(Item?object?remarks?size > 0, '*',''))),
	Item->>style(when(->>(Item?object,isIncomplete),'incomplete','ok')).
%%

%%
selectedIS(D,
	IS: inputSystem):<-
	%faalt als niet geselecteerd

	DI = D?defList_member<<-selection,
	DI \== @nil,
	IS = DI<<-object.
%%
%%%%%%%%%CHANGES%%%%%%%%%%%%%%%%
%%
changeApplied_newInputSystem(D,
	CR:changeRequestor):->
	%het nieuwe inputSystem wordt toegevoegd aan de lijst met defs
	D->>refillDefList,	%bewaart selectie
	if	
	(
	    CR->>checkEditor(D)
	)
	then
	(
	    D?defList_member->>selection(CR?result),
	    D->>onDefSelection
	).
%%

%%
changeApplied_copyMF(D,
	CR:changeRequestor):->
	%gp3 0.2
	%when the copied MF is an input system, we add it to the list
	if 
	    CR?result->>isInputSystem
	then
	(
	    D->>refillDefList,
	    if
		CR->>checkEditor(D)
	    then
	    (
		D?defList_member->>selection(CR?result),
		D->>onDefSelection
	    )
	).
%%
				
%%
changeApplied_changeInputSystem(D,
		_CR:changeRequestor):->
	%gp3 1.4: changed this back to version without editorBelongstoSameModel, that did not work
	%and was removed in the trunk as well
	%gewoon hervullen maar
	D->>refillDefList.
%%

%%
changeApplied_deleteInputSystem(D,
	_CR : changeRequestor
	):->
	%gewoon hervullen maar
	D->>refillDefList.
%%

%%
changeApplied_setCurrentLanguage(D,
	CR: changeRequestor):->
	
	%our super wants to destroy, but we dont if we are not in simulatepick
	if
		@on = D<<-inSimulatePick
	then
		D->+changeApplied_setCurrentLanguage(CR).
	%else do nothing
%%
	
%%
%%
%%de algemene changeApplied gebruiken we voor het bijwerken van de info
%%over de structuurregel
%%Das nogal inefficient, maar wel de enige handige manier om te programmeren

changeApplied(D,
	_CR: changeRequestor):->
	D->>updateDisplay.
%%

%WAT KEYAFHANDELING
event(D,E: event):->
	%vang wat keys op, voor de eigen afhandeling
	(
		E->>is_a(keyboard),
		K = E<<-key,
		pl_key_ISL(D,K)
	)
	;
		D->+event(E).
%	

pl_key_ISL(D, 'DEL'):-
	@on = D?remove_member<<-active,
	D->>remove.

pl_key_ISL(D,'ENTER'):-
	%gp3: this will be either simulate or edit
	@on = D<<-inSimulatePick,
	@on = D?simulate_member<<-active,
	D->>simulate.
%
pl_key_ISL(D,'ENTER'):-
	%gp3: this will be either simulate or edit
	@off = D<<-inSimulatePick,
	@on = D?edit_member<<-active,
	D->>edit.

	
pl_key_ISL(D,'\\C-a'):-	%ctrl-a = add child / new
	@on = D?add_member<<-active,
	D->>add.
	
pl_key_ISL(D,'\\C-j'):-	%ctrl-enter = properties (new in gp3)
	@on = D?properties_member<<-active,
	D->>properties.

:-pce_end_class.
