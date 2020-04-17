/*
savedSketchList
Helper dialog to show saved sketches in the model and give the opportunity to save a new one
GARP3
*/

% based on class savedSimulationList, AB - Nov 2006
:-pce_begin_class(savedSketchList,assistanceDialog
		 ).

variable(type,{load,save},both).
variable(typeOfSketch,name,both).

%%
initialise(D, Type: {load,save}, Frame: frame, TypeOfSketch: name):->
	%gp3 added Frame
    	get(@model, getModelNameForEditor, 'Saved sketches', ModelNameForEditorLabel),
	D->+initialise(ModelNameForEditorLabel,later), %gp3 0.3.13: helpid will be set, but later
        % should this be something else? AB, nov 2006
	D->>icon(@simulate_icon),

	D->>type(Type),
	D->>typeOfSketch(TypeOfSketch),
	if
		Type = load
	then
	(
		D->>label('Open a saved sketch'),
		D->>helpId('Sketch_File_OpenSavedSketch')
	)
	else
	(
		D->>label('Save a sketch in the model'),
		D->>helpId('Sketch_File_SaveSketchInModel')
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
	DefList->>label('Saved sketches:'),
	DefList->>show_label(@on),
	DefList->>select_message(->>(D,onSelection)),
	if
		Type = load
	then
		DefList->>open_message(->>(D,sketch_open_saved)) %dblclick
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
	        % use the edit_changes button for Open selected saved sketch, or something else? AB, jan 2007
		Sketch_Open_Saved *= imgButton(sketch_open_saved, tt:= 'Open selected saved sketch'),
		Sketch_Open_Saved->>active(@off),
		ButtonHeight->>plus(Sketch_Open_Saved?height)
	),
	
	Remove *= imgButton(remove, img:= remove, tt:= 'Delete selected saved sketch'),
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
		D->>display(Sketch_Open_Saved,point(DefList?right_side + GapX,
						DefList?list_top)),
	
	%we have remove halfway (even if Sketch_Open_Saved is not shown)
	D->>display(Remove,point(DefList?right_side + GapX,
					DefList?list_top + ((ListBottom - DefList?list_top) / 2) -
							(Remove?height / 2))),

	%and Cancel below
	D->>display(Cancel,point(DefList?right_side + GapX,
			ListBottom - Cancel?height)),
	%Save the sketch?
	if
		Type = save
	then
	(
		Msg *= label(msg,'Select a saved sketch to overwrite or type a new name.\nClicking save will save the sketch state in the model.\nYou will have to save the model as well.',
			font(helvetica,oblique,10)),
		D->>display(Msg,point(GapX,ListBottom + GapY)),
		Name *= eventTextItem(name), 
		Name->>label('Name:'),
		Name->>afterKey(->>(D,checkSave)), %check if we can enable the save button
		Name->>keyboard_focus(@on),
		D->>display(Name,point(GapX,Msg?bottom_side + GapY)),
		Name->>right_side(DefList?right_side),
		Save *= imgButton(save, tt:='Save sketch in model'),
		Save->>active(@off),
		D->>display(Save,point(Name?right_side + GapX,Name?top_side)),
		Below = Save?bottom_side
	)
	else
		Below = Cancel?bottom_side,
		
	D->>assign_accelerators,	
	D->>confirm_done(@off),

	D->>fillList(TypeOfSketch),
	D->>updateSpacers, %gp3 0.3.13 needed for assistanceDialogs
	D->>minimalSize(size(Cancel?right_side,Below)),
	get(@model, '_value', Model),
	send(D, associateModel, Model).
%%

%%
onResize(D, Difference: size):->
	%gp3 0.2: 
	
	D?list_member->>right_side(D?list_member?right_side + Difference?width),
	D?list_member->>bottom_side(D?list_member?bottom_side + Difference?height),
	if
		Sketch_Open_Saved = D<<-member(sketch_open_saved)
	then
		Sketch_Open_Saved->>set(x:= Sketch_Open_Saved?left_side + Difference?width),
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
fillList(D, TypeOfSketch: name):->
	%fill the list of saved sketch names

	List = D<<-list_member,
	List->>clear,
	DI *= var,
        % only include sketches of the right type
	@model?savedSketches->>for_some(
		and(
			assign(DI,create(dict_item,@arg1,object := @arg2)),
			->>(?(@arg2,member,type),equal,TypeOfSketch),
			->>(DI,style,current),
			->>(List,append,DI)
		)
         ).
%%

%%
onSelection(D):->
	%something selected in the list
	if
		load = D<<-type
	then
		% D?sketch_open_saved_member->>active(@on),
		D?sketch_open_saved_member->>active(@on),
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
sketch_open_saved(D):->
	%just return the selection, we assume there is one
	Sketch = D?list_member?selection<<-key,
	D->>return(Sketch).
%%

%%
remove(D):->
	%remove the selection from the model, return @nil
	S = D?list_member?selection<<-key,
	if
		D->>msgBox(string('This will permanently remove the saved sketch \'%s\' from the model',
					S),
				confirm) %see helpers/object_extensions
	then
	(
		@app->>removeSavedSketch(S),
		D->>fillList(D?typeOfSketch),
		D?sketch_open_saved_member->>active(@off),
		D?remove_member->>active(@off)
	).
%%


%%
save(D,SelectedName: [name]):->
	TypedName = D?name_member<<-selection,
	default(SelectedName, TypedName, Name),
	if
	(
		@model?savedSketches<<-member(Name)
	)
	then
	(
		C *= confirmOverwriteDlg(D,string('This will overwrite the already existing saved sketch \'%s\'.',Name)),
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
