/*
savedSimulationList
Helper dialog to show saved simulations in the model and give the opportunity to save a new one
GARP3
*/

%gp3 0.3.13 changed base class from extendedDialog to assistanceDialog
:-pce_begin_class(savedSimulationList,assistanceDialog
		 ).

variable(type,{load,save},both).

%%
initialise(D, Type: {load,save}, Frame: frame):->
	%gp3 added Frame
	D->+initialise('Saved simulations',later), %gp3 0.3.13: helpid will be set, but later
	D->>icon(@simulate_icon),

	D->>type(Type),
	if
		Type = load
	then
	(
		D->>label('Open a saved simulation'),
		D->>helpId('Sim_File_OpenSavedSimulation')
	)
	else
	(
		D->>label('Save a simulation in the model'),
		D->>helpId('Sim_File_SaveSimulationInModel')
	),
	D->>application(@app),
	%D->>modal(application),
	D->>transient_for(Frame),
	D->>modal(transient),
	D->>kind(transient),

	GapX = D?gap<<-width,
	GapY = D?gap<<-height,

	DefList *= extendedListBrowser(height := 15, width := 30), 
	DefList->>name(list),
	DefList->>label('Saved simulations:'),
	DefList->>show_label(@on),
	DefList->>select_message(->>(D,onSelection)),
	if
		Type = load
	then
		DefList->>open_message(->>(D,simulate)) %dblclick
	else
		DefList->>open_message(->>(D,save,DefList?selection?key)),
	%gp3 0.3: add some styles to show the mapping on the model
	DefList->>style(current,style(colour:=black)),
	DefList->>style(old,style(colour:=grey30)),
	D->>display(DefList,point(GapX,D?topY)), %gp3 0.3.13: use topY instead of GapY

	ButtonHeight *= number(0),
	
	if
		Type= load
	then
	(
		Simulate *= imgButton(simulate, tt:= 'Open selected saved simulation'),
		Simulate->>active(@off),
		ButtonHeight->>plus(Simulate?height)
	),
	
	Remove *= imgButton(remove, img:= remove, tt:= 'Delete selected saved simulation'),
	Remove->>active(@off),
	ButtonHeight->>plus(Remove?height),
	
	Cancel *= imgButton(cancel, img:=undo, tt:= 'Cancel'),
	ButtonHeight->>plus(Cancel?height),
	
	ListBottom *= number(DefList?bottom_side),
	ListBottom->>maximum(DefList?list_top + 
					ButtonHeight +
					2 * GapY), 
	DefList->>bottom_side(ListBottom),

	if
		Type = load
	then
		D->>display(Simulate,point(DefList?right_side + GapX,
						DefList?list_top)),
	
	%we have remove halfway (even if Simulate is not shown)
	D->>display(Remove,point(DefList?right_side + GapX,
					DefList?list_top + ((ListBottom - DefList?list_top) / 2) -
							(Remove?height / 2))),

	%and Cancel below
	D->>display(Cancel,point(DefList?right_side + GapX,
			ListBottom - Cancel?height)),
	%Save the simulation?
	if
		Type = save
	then
	(
		Msg *= label(msg,'Select a saved simulation to overwrite or type a new name.\nClicking save will save the simulation state in the model.\nYou will have to save the model as well.',
			font(helvetica,oblique,10)),
		D->>display(Msg,point(GapX,ListBottom + GapY)),
		Name *= eventTextItem(name), 
		Name->>label('Name:'),
		Name->>afterKey(->>(D,checkSave)), %check if we can enable the save button
		Name->>keyboard_focus(@on),
		D->>display(Name,point(GapX,Msg?bottom_side + GapY)),
		Name->>right_side(DefList?right_side),
		Save *= imgButton(save, tt:='Save simulation in model'),
		Save->>active(@off),
		D->>display(Save,point(Name?right_side + GapX,Name?top_side)),
		Below = Save?bottom_side
	)
	else
		Below = Cancel?bottom_side,
		
	D->>assign_accelerators,	
	D->>confirm_done(@off),

	D->>fillList,
	D->>updateSpacers, %gp3 0.3.13 needed for assistanceDialogs
	D->>minimalSize(size(Cancel?right_side,Below)).
%%

%%
onResize(D, Difference: size):->
	%gp3 0.2: 
	
	D?list_member->>right_side(D?list_member?right_side + Difference?width),
	D?list_member->>bottom_side(D?list_member?bottom_side + Difference?height),
	if
		Simulate = D<<-member(simulate)
	then
		Simulate->>set(x:= Simulate?left_side + Difference?width),
	D?remove_member->>set(x:= D?remove_member?left_side + Difference?width,
							y:= D?remove_member?top_side + Difference?height / 2),
	D?cancel_member->>set(x:= D?cancel_member?left_side + Difference?width,
							y:= D?cancel_member?top_side + Difference?height),
	if
		Msg = D<<-member(msg)
	then
		Msg->>set(y:= Msg?top_side + Difference?height),
	if
		Name = D<<-member(name)
	then
	(
		Name->>set(y:= Name?top_side + Difference?height),
		Name->>right_side(Name?right_side + Difference?width)
	),
	
	if
		Save = D<<-member(save)
	then
		Save->>set(x:= Save?left_side + Difference?width,
					y:= Save?top_side + Difference?height).

%%
label(D,L: name):->
	%make sure the frame label is set, not the decorator label
	D?frame->>label(L).
%
label(D, L: name):<-
	L = D?frame<<-label.
%%

%%
fillList(D):->
	%fill the list of saved simulation names

	List = D<<-list_member,
	List->>clear,
	DI *= var,
	@model?savedSimulations->>for_all(
		and(
			assign(DI,create(dict_item,@arg1,object := @arg2)),
			if(
				->>(?(@arg2,member,modelVersion),equal,number(@model?lastChangeTimeValue)),
				->>(DI,style,current),
				and(
					->>(DI,style,old),
					->>(DI,tooltip,'Warning: after this simulation was saved, changes were made to the model.')
				)
			),
			->>(List,append,DI)
		)).
%%
%%
onSelection(D):->
	%something selected in the list
	if
		load = D<<-type
	then
		D?simulate_member->>active(@on),
	D?remove_member->>active(@on),
	if
		save = D<<-type
	then
	(
		D?name_member->>selection(D?list_member?selection?key),
		D->>checkSave
	).
%%

%%
simulate(D):->
	%just return the selection, we assume there is one
	Simulation = D?list_member?selection<<-key,
	D->>return(Simulation).
%%

%%
remove(D):->
	%remove the selection from the model, return @nil
	S = D?list_member?selection<<-key,
	if
		D->>msgBox(string('This will permanently remove the saved simulation \'%s\' from the model',
					S),
				confirm) %see helpers/object_extensions
	then
	(
		@app->>removeSavedSimulation(S),
		D->>fillList,
		D?simulate_member->>active(@off),
		D?remove_member->>active(@off)
	).
%%


%%
save(D,SelectedName: [name]):->
	TypedName = D?name_member<<-selection,
	default(SelectedName, TypedName, Name),
	if
	(
		@model?savedSimulations<<-member(Name)
	)
	then
	(
		C *= confirmOverwriteDlg(D,string('This will overwrite the already existing saved simulation \'%s\'.',Name)),
		Overwrite = C<<-confirm_centered,
		C->>destroy,
		Overwrite = @on
	),
	D->>return(Name).
%%

%%
cancel(D):->
	D->>destroy. %makes D<<-confirm fail
%%

%%
checkSave(D):->
	%check whether we should enable the save button
	if
		0 = D?name_member?selection<<-size
	then
		D?save_member->>active(@off)
	else
		D?save_member->>active(@on).
%%
:-pce_end_class.
