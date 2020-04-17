/*
definition of class sketchDefinitionEditor
Intended to be a superclass for the three Editors for
creating and editing of sketch processes, external influences, and scenarios

edit kan aangeroepen met een bestaande definition voor het bewerken ervan.
Wanneer edit wordt gebruikt zonder argumenten wordt in new mode geopend.

2006 Anders Bouwer, Universiteit van Amsterdam,
Based on Garp3 Attribute definition editor
Still includes some comments in Dutch
*/


:-pce_begin_class(sketchDefinitionEditor,
		  assistanceDialogWithMenuBar,
		  "General Sketch Definition editor"
		 ).

variable(def,sketchDefinition*,get,
		"the edited sketchDefinition"). %@nil betekent: nieuwe

%%
initialise(D, OpenedBy: [object], Label: name, HelpID: name):->
	"Initialise the editor" ::
	%gp3 0.1: added openedBy, will be a hyper to the given object, so others can ask voor ?openedBy

	D->+initialise(Label, HelpID),
	D->>application(@app),
	D->>kind(toplevel),
	D->>icon(@build_icon),
	D->>init_commands,
	D->>init_menu,

	%gp3: saved openedBy in a hyper
	default(OpenedBy,@nil,Opener),
	D->>hyper(Opener,openedBy),

	%de onderdelen
	%De plaatsing doen we zelf op coordinaten, we gebruiken wel de standaard "gap"
	GapX = D?gap<<-width,
	GapY = D?gap<<-height,
	MaxX *= number(0), %houden we de maximale X coordinaat in bij zodat we rechts kunnen uitlijnen

	DefList *= extendedListBrowser(width := 53, height := 10), % width was 40
	DefList->>name(defList),
	DefList->>label('General definitions:'), % will be renamed in subclasses

	DefList->>show_label(@on),
	DefList->>select_message(->>(D,
				       onDefSelection,
						@arg1?object)),

	D->>display(DefList,point(GapX,D?topY)), %topY because of help button
	D->>fillDefList,

        Add = D<<-add_Add_button,
        Remove = D<<-add_Remove_button,

	%buttons komen rechts van de lijst, en New helemaal boven, Remove onder
	%dus de lijst moet ook hoog genoeg zijn en bovendien moet New niet bij de bovenkant
	%van de lijst maar bij de bovenkant van het window in de lijst (anders bij label)

	ListBottom *= number(DefList?bottom_side),
	ListBottom->>maximum(DefList?top_side + DefList?image?top_side + %=bovenkant vd lijst
					Add?height + Remove?height +
					GapY), %hieruit volgt de onderkant van de lijst (minimaal ruimte van label + buttons)
	DefList->>bottom_side(ListBottom),

	%Add komt dus niet op de top_side van DefList, maar van het lijstgedeelte
	%DefList?image?top_side is de bovenkant van het lijstgedeelte tov de hele lijst
	D->>display(Add,point(DefList?right_side + GapX,
						DefList?top_side + DefList?image?top_side)),
	MaxX->>maximum(Add?right_side),

	%en Remove komt keurig onder
	D->>display(Remove,point(Add?left_side,
			ListBottom - Remove?height)),
	MaxX->>maximum(Remove?right_side),

	%Een lijntje
	Line1 *= line(GapX,ListBottom + GapY,0,ListBottom + GapY),
	Line1->>name(line1),
	%deze lijn wordt later goed naar rechts uitgelijnd
	Line1->>pen(2),
	D->>display(Line1),

	Name *= eventTextItem(name), %kan net zo goed text_item zijn
	Name->>label('Name:'),
	Name->>length(DefList?width),
	D->>display(Name,point(GapX,Line1?bottom_side + GapY)),

	MaxX->>maximum(Name?right_side),

	Remarks *= editor(height := 4, width := 73), % width was 60
	Remarks->>name(remarks),
	Remarks->>label('Remarks:'),
	Remarks->>show_label(@on),
	Remarks->>fill_mode(@on),
	Remarks->>font(Name?font),

	D->>display(Remarks,point(GapX,	Name?bottom_side + GapY)),
	MaxX->>maximum(Remarks?right_side),

        TabTool = D<<-add_tabtool,

	%en nu weer een lijntje

	Line2 *= line(GapX,Remarks?bottom_side + GapY,0,Remarks?bottom_side + GapY ),
	Line2->>name(line2),
	Line2->>pen(2),
	D->>display(Line2), %rechterkant wordt hieronder wel gezet, als MaxX bekend is

        D->>display(TabTool, point(GapX,Line2?bottom_side + GapY )),

	%en nu weer een lijntje

	Line3 *= line(GapX,TabTool?bottom_side + GapY,0,TabTool?bottom_side + GapY ),
	Line3->>name(line3),
	Line3->>pen(2),
	D->>display(Line3), %rechterkant wordt hieronder wel gezet, als MaxX bekend is

	Save *= imgButton(save, img:=sketch_save, tt:='Save changes'),
	Cancel *= imgButton(cancel, img:=sketch_undo, tt:='Undo changes'),
	% Save *= imgButton(save, img:=save_sketch_def_to_model, tt:='Save changes'),
	% Cancel *= imgButton(cancel, img:=cancel_sketch_def, tt:='Undo changes'),

	D->>display(Save,point(GapX, Line3?bottom_side + GapY)),

	%deze komt rechts van save
	D->>display(Cancel,point(Save?right_side + GapX,
				Save?top_side)),

	%sluiten moet rechts komen, dus dat moet ook nog uitgezocht
	Close *= imgButton(close,->>(D,onDestroy),tt:='Close this editor'),

	MaxX->>maximum(Cancel?right_side + GapX + Close?width),

	%oftewel: eventueel wordt de dlg nog breder om deze button te laten passen
	%hoe dan ook helemaal rechts

	D->>display(Close,point(
		MaxX - Close?width,
		Save?top_side)),

	%Nog meer van het een en ander moet nog mooi naar rechts
	Add->>set(x:= MaxX - Add?width),
	Remove->>set(x:= MaxX - Remove?width),
	DefList->>right_side(Add?left_side - GapX),
	Line1->>end_x(MaxX),
	Name->>right_side(MaxX),
	Remarks->>right_side(MaxX),
	Line2->>end_x(MaxX),
	Line3->>end_x(MaxX),
	D->>assign_accelerators, %nodig voor de accels als je niet append gebruikt

	%we roepen onDestroy aan om te checken of het wel mag
	%dit gaat via done_message vh frame, die roept gewoonlijk frame->>wm_delete aan
	D->>done_message(->>(D,onDestroy)),
	D->>confirm_done(@off), %niet vragen
	D->>updateSpacers, %gp3 0.3.13 needed when assistanceDialog is used, but displayContent is not used to fill the dialog
		%minimal size:
	D->>minimalSize(size(MaxX,Close?bottom_side)), %abs min
	D->>fillCurrentDefData,

	get(@model, '_value', Model),
	send(D, associateModel, Model).
%%



%
init_menu(VE
	  ) :->

	send_list(VE?menubar, append, %zie assistanceDialogWithMenubar
		  [
		   new(File,popup('File')),
		   new(Import,popup('Import'))
		  ]),
	send_list(File,append,
		    [
			menuCommand(saveModel,'SaveModel','Save model to disk')
		    ]),
	Import->>colour(black),
        send(VE, init_import_menu, Import).

%
init_import_menu(_VE, _Import):->
        writef('init_import_menu: This should be specialized in subclasses\n',[]).
%%



add_tabtool(_D, _TabTool):<-
        writef('add_tabtool called: This should be specialized in subclasses\n',[]).



default_sketchDefName(_D, Name: name):<-
        Name = 'New sketch general definition - this should be specialized in subclass'.


% init_editor_tab(D, Tab, TypeID)
%
% creates a tab with a text editor
%
init_editor_tab(D, Tab, TypeID):->
	GapX = D?gap<<-width,
	GapY = D?gap<<-height,
	Placement *= point(GapX,GapY),
	Editor *= editor(width:=68, height:=10),
        swritef(EditorName, 'editor_for_%w',[TypeID]),
	Editor->>name(EditorName),
	Editor->>label(EditorName),
	Editor->>show_label(@off),
	Editor->>fill_mode(@on),
	Editor->>font(D?remarks_member?font),
	% Editor->>right_side(FixedWidth),
	Editor->>attribute(dataField,@on),
	Tab->>display(Editor,Placement),
	Placement->>set( y:= Editor?bottom_side + GapY), %for future extensions, and for the next line
	% Tab->>height(Placement?y),
        swritef(TabEditorID, '%dTabEditor',[TypeID]),
	D->>hyper(Editor, TabEditorID).


% init_table_tab(D, Tab, TypeID)
%
% creates a tab with an editable table containing Name and Remarks columns
%
init_table_tab(D, Tab, TypeID):->
	%create the misc dialog group

	%we use display and points etc to fit everything

	Misc *= dialog_group(misc,group),
	D->>hyper(Misc, misc),
	send(Tab, append, Misc),
	% send(Tab, display, Misc, point(10,0)),

	FixedWidth = 450, % fixed with
	FixedHeight = 180, % fixed height
	Misc->>width(FixedWidth),
	GapY = Misc?gap<<-height,
	GapX = Misc?gap<<-width,

	Box *= box,
	Box->>fill_pattern(?(new(colour),convert,'#c3d1d4')),
	Box->>pen(0),
	Misc->>display(Box,point(0,0)), %starts on the top
	Box->>right_side(FixedWidth),
	Box->>bottom_side(GapX + GapY), %some space
	Box->>hide,

	%we make a table
	Grid *= picture,
	send(Grid, background, colour(white)),
	% send(Grid, background, D?background),
	swritef(GridName, 'grid_for_%w',[TypeID]),
	Grid->>name(GridName),
	D->>hyper(Grid, GridName),
	% Grid->>name(grid),
	% Misc->>attribute(grid,Grid), %have to save it like this, because of window_decorator becoming the <-member
	Misc->>attribute(grid,Grid), %have to save it like this, because of window_decorator becoming the <-member
	Grid->>pen(0),
	GapY = D?gap<<-height,

	Grid->>width(FixedWidth - 20),
	Grid->>layout_manager(new(Table, tableWithRowSelection)),
	Table->>cell_padding(size(0,0)), % 2,2
	Table->>cell_spacing(0),
	% Table->>cell_spacing(size(1,1)),
	Table->>border(0),
	Table->>frame(void),
	Table->>rules(none),
	%save the widths we chose
	if
		SB = Grid<<-vertical_scrollbar
	then
		ScrollbarWidth = SB<<-width
	else
		ScrollbarWidth = 0,

	Grid->>attribute(nameFieldWidth, (Grid?width - ScrollbarWidth) / 3 - 2 * Table?cell_padding?width),
	Grid->>attribute(remarksFieldWidth, ((Grid?width - ScrollbarWidth) * 2/3 - 2 * Table?cell_padding?width)),

	Tab->>display(new(LBName,label(selection := 'Name', font := bold)),point(GapX, GapY)),
	LBName->>width(0), %make fit
	Tab->>display(new(LBRemarks,label(selection := 'Remarks', font := bold)),point(10 + Grid?nameFieldWidth + 3 * Table?cell_padding?width,LBName?top_side)),
	LBRemarks->>width(0),

	Tab->>display(Grid,point(10,LBRemarks?bottom_side)),
	Grid->>height(FixedHeight - LBRemarks?bottom_side - 10),


	%buttongroup

	swritef(ButtonName, 'new_%w',[TypeID]),
	BtnNew *= imgButton(new,->>(D,newRow, Table, Grid),ButtonName,@nil,sketch_buttonbars,'Add a new item below the currently selected one'),
	% BtnUp *= imgButton(up,->>(D,moveRow, Table, Grid, -1),misc_row_up,@nil,metadata,'Move the selected item up'),
	% BtnDown *= imgButton(down,->>(D,moveRow, Table, Grid, 1),misc_row_down,@nil,metadata,'Move the selected item down'),
	BtnDelete *= imgButton(delete,->>(D,deleteRow, Table),delete,@nil,sketch_buttonbars,'Delete the selected row'),


        send(Tab, display, BtnNew, point(Grid?right_side + GapX, LBRemarks?bottom_side)),
        send(Tab, display, BtnDelete, point(BtnNew?left_side, BtnNew?bottom_side + 10)),
        % send(Tab, display, BtnUp, point(Grid?right_side + GapX, BtnDelete?bottom_side)),
        % send(Tab, display, BtnDown, point(Grid?right_side + GapX, BtnUp?bottom_side)),

	Tab->>height(180),
        swritef(TabTableID, '%dTabTable',[TypeID]),
	D->>hyper(Table, TabTableID).
%%



%%%%%%%% functionality for buttons to add and delete items and remarks

%%
selectedRow(_F,RowField: eventTextItem):->
	%a field in the page was selected, colour the row

	%text_items deligates to its table_cell when its there
	Table = RowField<<-(table),
	Row = RowField?(table)<<-row(RowField?row),
	%is there a currently selected row?
	Old = Table<<-currentRow,
	unless
		Old = @nil
	do
		Old->>background(Row?background), %whatever is the default
	%get the colour, (blueish grey)
	% C = colour<<-convert('#c3d1d4'),
	C = colour(white),
	Row->>background(C),
	Table->>currentRow(Row).
%%

%%
newRow(F, Table: table, Grid: picture, KeyText: [char_array], ValueText: [char_array], FixedRowIndex: [int]):->
	%create a new row in the page and make it current
	%optionally fill it with information and or choose the row index
	%we create the row ourselves and afterwards insert it

	NewRow *= table_row,
	NewRow->>append(new(Key,eventTextItem(key))),
	NewRow->>append(new(Value,eventTextItem(value))),
	unless
		KeyText = @default
	do
		Key->>selection(KeyText),
	unless
		ValueText = @default
	do
		Value->>selection(ValueText),
	Key->>show_label(@off),

	Key->>border(0),
	Value->>border(0),
	Key->>pen(0),
	Value->>pen(0),
	% Key->>value_font(bold),

	Key->>value_width(Grid?nameFieldWidth),
	Value->>show_label(@off),
	Value->>value_width(Grid?remarksFieldWidth),
	OnFocus = ->>(F,selectedRow,@arg1),
	Key->>onFocus(OnFocus),
	Value->>onFocus(OnFocus),

	%insert the row
	if
		FixedRowIndex = @default
	then
	(
		if
			@nil = Table<<-currentRow
		then
			NewRowIndex = Table?rows?high_index + 1 %not evaluated yet (no problem)
		else
			NewRowIndex = Table?currentRow?index + 1
	)
	else
		NewRowIndex = FixedRowIndex,
        % get(NewRowIndex, value, Temp1),
	% writef('Inserting row at index: %w\n',[Temp1]),
	Table->>insert_row(NewRowIndex,NewRow),
	% F->>sortRowTabbing(Table),
	Grid->>normalise(Key),
	Key->>keyboard_focus(@on), %and give it focus + make it current
	send(Table, changedContents, @on).
%%


%%
sortRowTabbing(_F, Table: table):->
	%the tab order in the page is weird, it depends on the order on which rows were added
	%thats why we order them by looping over the cells in the table

	Rows = Table<<-rows,
	%we loop backwards, so number 1 will get the highest order in the end
	%but the vectors containing the rows, and the cells in each row, are themselves ordered backwards
	Rows->>for_all(
		->>(@arg1,for_all,->>(@arg1?image, expose))).
%%

%%
deleteRow(_F, Table: tableWithRowSelection):->
	%delete the currentrow if any
	Row = Table<<-currentRow,
	unless
		Row = @nil
	do
	(
		% Table = Row<<-table,
		Index = Row<<-index,
		Table->>delete(Row),
		Table->>currentRow(@nil),
		unless
			0 = Table?rows<<-size
		do
		(
			unless
				NewCurrent = Table<<-row(Index,@off)
			do
				NewCurrent = Table<<-row(Index - 1,@off),
			Field = NewCurrent?head?image,
			Field->>keyboard_focus(@on) %activates, so we get the onFocus event
		)
	),
	send(Table, changedContents, @on).
%%

%%
moveRow(F, Table: tableWithRowSelection, Grid: picture, Direction: int):->
	%move the current row up (Direction < 0) or down
	%for some reason it just does not work with moving the whole row
	%so we create a new row with the old values
	Row = Table<<-currentRow,
	Row \== @nil, %or fail
	RowIndex = Row<<-index,
	NewRowIndex is RowIndex + Direction,
	NewRowIndex > 0, %or fail
	NumRows = Row?(table)?rows<<-size,
	NewRowIndex =< NumRows,
	KeyText = Row?head?image<<-selection,
	ValueText = Row?tail?image<<-selection,
	Row?(table)->>delete(Row),
	F->>newRow(Table, Grid, KeyText, ValueText, NewRowIndex),
	send(Table, changedContents, @on).
%%


%%%%%%%% end functionality for buttons to add and delete items and remarks



%
clear_tab(D, TypeID):->
        swritef(TabEditorID, '%dTabEditor',[TypeID]),
	get(D, hypered, TabEditorID, Editor),
        send(Editor, clear).
%%

%
clear_table_tab(D, TypeID):->
        swritef(TabTableID, '%dTabTable',[TypeID]),
	get(D, hypered, TabTableID, Table),
        send(Table, clear).
%%

%
fill_tab(D, TypeID):->
        swritef(TabEditorID, '%dTabEditor',[TypeID]),
	get(D, hypered, TabEditorID, Editor),
        get(D, def, Def),
        get(Def, def_sheet, S),
        get(S, TypeID, Value),
        % clear Editor and append Text in Value
        send(Editor, clear),
        send(Editor, append, Value).
%%

%
fill_table_tab(D, TypeID):->
        swritef(TabTableID, '%dTabTable',[TypeID]),
	get(D, hypered, TabTableID, Table),
        get(D, def, SketchDef),
        get(SketchDef, def_sheet, S),
        get(S, TypeID, Items),
        swritef(GridName, 'grid_for_%w',[TypeID]),
	get(D, hypered, GridName, Grid),
        send(Items, for_all,
	     message(D, newRow, Table, Grid, @arg1?name, @arg1?remarks, @default)
	),
	send(Table, changedContents, @off).
%%


%%
onResize(D, Difference: size):->
	%gp3 0.2:

	D?defList_member->>pixelWidth(D?defList_member?pixelWidth + Difference?width),
	D?defList_member->>pixelHeight(D?defList_member?pixelHeight + Difference?height),
	D?add_member->>set(x:= D?add_member?left_side + Difference?width),
	D?remove_member->>set(x:= D?add_member?left_side, y:= D?remove_member?top_side + Difference?height),

	D?line1_member->>right_side(D?line1_member?right_side + Difference?width),
	D?line1_member->>set(y:= D?line1_member?top_side + Difference?height),

	D?name_member->>right_side(D?name_member?right_side + Difference?width),
	D?name_member->>set(y:= D?name_member?top_side + Difference?height),

	D?remarks_member->>right_side(D?name_member?right_side),
	D?remarks_member->>set(y:=D?remarks_member?top_side + Difference?height),

	D?line2_member->>right_side(D?line1_member?right_side),
	D?line2_member->>set(y:= D?line2_member?top_side + Difference?height),

        % this does not work yet
        % D->>resize_tabs(D?tabtool_member?width + Difference?width),
        D?tabtool_member->>set(y:= D?tabtool_member?top_side + Difference?height),

	D?line3_member->>right_side(D?line1_member?right_side),
	D?line3_member->>set(y:= D?line2_member?top_side + Difference?height),

	D?save_member->>set(y:=D?save_member?top_side + Difference?height),
	D?cancel_member->>set(y:=D?save_member?top_side),
	D?close_member->>set(x:= D?close_member?left_side + Difference?width, y:=D?save_member?top_side).
%%



%
resize_tabs(D, Width):->
	get(D?tabtool_member, graphicals, Tabs),
        send(Tabs, for_all,
	        and(
		 message(@arg1?editor_member, width, Width),
		 message(@arg1, width, Width)
	        )
	     ).
%%


%%%
onDestroy(D):->
	%check of destroy wel mag
	%zie initialise waarbij dit als done_message wordt gezet

	D->>checkSaveSelection,!, %ok er mag doorgegaan worden
	D->>wm_delete. %default done_message vh frame
%%

%%
openedBy(D, OpenedBy: object*):<-
	%gp3, give the object that opened this window or @nil
	OpenedBy = D<<-hypered(openedBy)
	;
	OpenedBy = @nil.
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
edit(D,
	Def :  [sketchDefinition]
	):->
	"Open the editor for editing the given (or the first) sketch definition" ::

	D->>fillDefList,

	if
		0 = D?defList_member<<-length
	then
		D->>slot(def,@nil)
	else (
		if
			Def == @default
		then (
			I = D?defList_member?members<<-head,
			D?defList_member->>selection(I),
			D->>slot(def,I?object)
			)
		else (
			D?defList_member->>selection(Def),
			D->>slot(def,Def)
			)
		),
	D->>fillCurrentDefData,
	D->>open.
%%

%
fillDefList(D):->
	"Fill the list with general definitions" ::

	D?defList_member->>clear,
        writef('fillDefList called: This should be specialized in subclass\n').
%%

%
fillDefList_helper(D,Def):->
	%voeg deze toe aan onze deflist
	if
		0 = Def?remarks<<-size
	then
		L *= string('%s',Def?name)
	else
		L *= string('%s*',Def?name),
	DI *= dict_item(Def,L,Def),
	D?defList_member->>append(DI).
%%

%%
onDefSelection(D,
	Def : sketchDefinition*):->
	"Callback when definition selected" ::
	/*
	Het probleem is altijd dat hier nu al de nieuwe staat geselecteerd terwijl de mapping
	in het dataobject nog op de oude staat ivm opslaan...
	*/
	%clause 1: op dezelfde regel geklikt

	Def = D<<-def,!.
%
onDefSelection(D,
	_Def):->
	%clause 2: we mogen niet door ivm de vorige selectie

	\+ D->>checkSaveSelection,!, %faalt dus terug op oude selectie

	if
		@nil = D<<-def %we waren in new modus
	then
		D?defList_member->>selection(@nil)
	else
		D?defList_member->>selection(D?def).
%
onDefSelection(D,Def):->
	%clause 3: we mogen door met de nieuwe selectie dus initialiseren we de data
	D->>slot(def,Def),
	D->>fillCurrentDefData.
%%

%%
add(D):->
	"Add button pressed..." ::

	D->>checkSaveSelection,
	D->>slot(def,@nil), %betekent: nieuwe
	D?defList_member->>selection(@nil),
	D->>fillCurrentDefData,
	D?name_member->>activate(@on),
	D->>caret(D?name_member).
%%

%%
remove(D):->
	"Remove button pressed..." ::

	%CR checkt de gevolgen wel en waarschuwt eventueel

	if
		not(@nil = D<<-def)
	then
		@model->>changeRequest(deleteSketchDef,
			@model,
			D,
			D?def).
%%



%%
fillCurrentDefData(D):->
	"fill the editor with saved data for current general definition selection" ::
	%clause 1: niets geselecteerd dus in nieuw modus
	@nil = D<<-def,!,
	D?remove_member->>active(@off), %er valt niets te verwijderen
	get(D, default_sketchDefName, Name),
	D?name_member->>selection(Name),
	D?remarks_member->>contents(new(string)),
        D->>clearDefSheet.

%
fillCurrentDefData(D):->
	%clause 2: wel een def geselecteerd
	Def = D<<-def,
	D?remove_member->>active(@on),
	D?name_member->>default(Def?name),
	D?remarks_member->>contents(Def?remarks),
        D->>clearDefSheet,
        D->>fillDefSheet(Def).
%%



clearDefSheet(_D):->
        writef('clearDefSheet called: This should be specialized in subclasses\n',[]).

fillDefSheet(_D, _Def):->
        writef('fillDefSheet called: This should be specialized in subclasses\n',[]).

create_data_sheet(_D, _S:sheet):<-
        writef('create_data_sheet called: This should be specialized in subclasses\n',[]).

%
create_def_chain(D, TypeID, Chain):<-
        swritef(TabTableID, '%dTabTable',[TypeID]),
        get(D, hypered, TabTableID, Table),
        new(Chain, chain),
        send(Table?rows, for_all,
	  and(
	      message(D, create_def_and_add_to_chain, @arg1, TypeID, Chain)
	     )
	).
%%

%
create_def_and_add_to_chain(D, Row, TypeID, DefsChain: chain):->
	get(Row?head?image, selection, Name),
	get(Row?tail?image, selection, Remarks),
        get(D, def, SketchDef),
        Def *= sketchDefinitionElement(Name, TypeID, Remarks, SketchDef),
        send(DefsChain, append, Def).
%%



%%%%%%%%%%%%%SAVE EN CANCEL E.D%%%%%%%%%%%%%%%%%%%%%%
%%
checkSaveSelection(D):->
	"Check changed state of current selection and ask for saving, fail if change canceled or save failed" ::
	D->>notChanged,!. %niets aan de hand
%
checkSaveSelection(D):->
	%ok, er is iets gewijzigd, dus moet er misschien opgeslagen worden

	Dlg *= dialog('Confirm changes'),
	Dlg->>application(@app),
	Dlg->>append(text('You made changes to this definition. Do you want to save them?')),
	SC *= imgButton(save_changes,
			    ->>(Dlg,return,save), img:=sketch_save, tt:='Save changes'),
	Dlg->>append(SC),
	CC *= imgButton(cancel_changes,->>(Dlg,return,cancel), img:=sketch_undo, tt:='Cancel changes'),
	Dlg->>append(CC),
	EC *= imgButton(edit_changes,->>(Dlg,return,edit), img:=sketch_edit_changes, tt:='Edit changes'),
	Dlg->>append(EC),
	Dlg->>transient_for(D),
	Dlg->>modal(transient),
	Dlg->>kind(toplevel), %wel een titelbalk
	Answer = Dlg<<-confirm_centered(D?frame?area?center),
	Dlg->>destroy,
	if
		Answer = save
	then
		D->>saveDef
	else
		Answer = cancel. %falen bij edit
%%

%%
notChanged(D):->
	% slaag als de definitie niet gewijzigd is
	%1: versie voor een nieuwe definitie: alles moet er nog precies
	%zo bijstaan als ingevuld door fillCurrentDefData

	@nil = D<<-def,!,
	get(D, default_sketchDefName, Name),
	D?name_member?selection->>equal(Name),
	0 = D?remarks_member?contents<<-size,
        D->>emptyDefSheet.


notChanged(D):->
	%2: Versie voor een bestaande definitie
	%we beschouwen het als gewijzigd als het sowieso anders, is, geen gedoe met makeGarp op dit nivo

	\+ @nil = D<<-def,
	D?name_member?selection->>equal(D?def?name),
	D?remarks_member?contents->>equal(D?def?remarks),
        D->>notChangedDefSheet.
%%

%
emptyDefSheet(D):->
        get(D, tabtool_member, TabTool),
        get(TabTool, graphicals, Tabs),
        % for all tabs with an editor
        get(Tabs, find_all,
	    (
	     message(@arg1?graphicals, find, message(@arg1, instance_of, editor))
	    ), EditorTabs),
        send(EditorTabs, for_all,
	     message(D, emptyDefEditorTab, @arg1)
	),

        % for all tabs with a table (found via hypers, actually)
        get(D, all_hypers, @on, Hypers),
        get(Hypers, find_all, message(@arg1?to, instance_of, table), TableHypers),
        send(TableHypers, for_all,
	     message(@arg1?to?changedContents, equal, @off)
	).
%%

%
emptyDefEditorTab(_D, Tab):->
        E = Tab?graphicals<<-find(->>(@arg1, instance_of, editor)),
	send(E?text_buffer?contents?value, equal, '').
%%




%
notChangedTabContents(D, TypeID):->
          D->>notChangedEditorTabContents(TypeID)
        ;
          D->>notChangedTableTabContents(TypeID).

%
notChangedEditorTabContents(D, TypeID):->
        % for Tabs with an editor
        swritef(TabEditorID, '%dTabEditor',[TypeID]),
	get(D, hypered, TabEditorID, Editor), !,
        get(D, def, Def),
        get(Def, def_sheet, S),
        % clear Editor and append Text in Value
        get(Editor?text_buffer, contents, TextInEditor),
        get(S, TypeID, TextInSheet),
        TextInEditor?value->>equal(TextInSheet?value).
%%

%
notChangedTableTabContents(D, TypeID):->
        % for Tabs with a Table
        swritef(TabTableID, '%dTabTable',[TypeID]),
        get(D, hypered, TabTableID, Table),

        get(Table, changedContents, Bool),
        if
         (
	   % To Do: this should be one correct condition
	   (Bool == @on)
	   ;
	   not( D->>notChangedCurrentTable(Table, TypeID) )
	 )
        then
          (
           % writef('Table %d has changed\n',[Table]),
           fail
	  )
        else
         (
           true
	 ).

notChangedCurrentTable(D, Table: table, TypeID):->
        get(D, def, Def),
        if (Def = @nil)
        then
          true
        else
          (
	   send(D, allItemsInTableOccurAlsoInDef, Def, Table, TypeID),
	   send(D, allItemsInDefOccurAlsoInTable, Def, Table, TypeID)
	  ).

allItemsInTableOccurAlsoInDef(D, Def, Table, TypeID):->
	get(Def?def_sheet, TypeID, Defs),
        send(Table?rows, for_all,
	     and(
		 % currently, I only check the name and remarks separately, but actually,
		 % they should cooccur in the same item
		 message(D, itemNameOccursInDef, @arg1?head?image?value_text?string?value, Defs),
		 message(D, itemRemarksOccursInDef, @arg1?tail?image?value_text?string?value, Defs)
		)
	     ).


itemNameOccursInDef(_D, Name, Defs):->
        send(Defs, find(->>(Name, equal, @arg1?name))).

itemRemarksOccursInDef(_D, Remarks, Defs):->
        send(Defs, find(->>(Remarks, equal, @arg1?remarks))).


allItemsInDefOccurAlsoInTable(_D, _Def, _Table, _TypeID):->
        % to do. for now, assume yes.
        true.


%
notChangedQuantity(_D, QDef, QDefsChain):->
        chain_list(QDefsChain, QuantityDefs),
        % find a QDef2 in QuantityDefs
        member(QDef2, QuantityDefs),
        % with identical name and remarks
        QDef2?name->>equal(QDef?name),
        QDef2?remarks->>equal(QDef?remarks).
%%



%%
cancel(D):->
	"Cancel button" ::
	%wanneer we in new modus waren (def = @nil), dan gaan we daar uit door
	%de onderste def te selecteren...

	if
		@nil = D<<-def
	then	(
		%als er geen entries in de lijst zijn, blijft het gewoon @nil
		%als er wel entries zijn, dan selecteren we de laatste
		if
			( \+ D?defList_member?members->>empty )
		then	(
			Item = D?defList_member?members<<-tail,
			D->>slot(def,Item?object),
			D?defList_member->>selection(Item)
			)
		),
	%in alle gevallen opnieuw data inlezen
	D->>fillCurrentDefData.
%%

%%
save(D):->
	"Save button" ::
	D->>saveDef.
	% D->>fillCurrentDefData. %opnieuw inlezen
%%

%
saveDef(_D):->
        writef('This should be specialized in subclasses\n',[]).
%%

%%
/******************** MULTIPLE MODELS ***********************/ %JL

/* Make the right model active */
input_focus(T, Boolean) :->
	send(@app, selectCorrectTab, T), % T is the window object (of the editor)
	send_super(T, input_focus, Boolean).


%%%%%%%%%%Helpers%%%%%%%%%%%%%%%%
%%%%%%%%%CHANGES%%%%%%%%%%%%%%%%
%%
changeApplied_addSketchDef(D,
	CR:changeRequestor):->
	%de nieuwe configuratiedef wordt toegevoegd aan de lijst met defs
	%als dat in deze editor is gebeurd zonder dat we naar een andere zijn gesprongen, dan
	%selecteren we hem
	/*
	Net als bij changeSketchDef kan  het opslaan gebeuren doordat de boel
	in deze editor wordt
	opgeslagen nadat een andere def is geselecteerd: het gaat bij het herselecteren dan om
	de oude selectie en niet om de opgeslagen D?def
	*/

	List = D<<-defList_member,
	OldSelection = List<<-selection,
	if
		OldSelection = @nil
	then
		OldDef = @nil
	else
		OldDef = OldSelection<<-object,

	D->>fillDefList,
	if
	(
		CR->>checkEditor(D),
		OldSelection = @nil %en we zijn nog braaf in new modus
	)
	then
	(
		List->>selection(CR?result),
		D->>slot(def,CR?result), %de nieuwe selectie
		D->>fillCurrentDefData
	)
	else
	(
		%we moeten dus de oude selectie herstellen
		if
			(OldDef \== @nil)
		then
			List->>selection(OldDef)
	).
%%

%%
changeApplied_changeSketchDef(D,
		_CR:changeRequestor):->
	%we weten zeker dat de geselecteerde def nog steeds bestaat, dus daar hoeven we niet op te checken
	%het enige dat we doen is de lijst hervullen en de oude selectie herstellen
	%wanneer de thans geselecteerde def dezelfde is als de interne "huidige" def, en deze is gewijzigd
	%dan lezen we de gegevens opnieuw in.
	%Dit gebeurt dus niet op het nivo van de <-editor van de CR, maar op de test: is degene die is afgebeeld gewijzigd
	%(bij selecteren van een andere def wordt op deze manier hier geen data opnieuw weergegeven, omdat de opgeslagen
	% def wel de huidige <-def is, maar niet de geselecteerde in de lijst)

	%CLAUSE 1: er is geen selectie
	@nil = D?defList_member<<-selection,!,
	D->>fillDefList.
%
changeApplied_changeSketchDef(D,CR):->
	%CLAUSE 2: er is wel een selectie, dus die is er nog na de change
	Selected= D?defList_member?selection<<-object,
	D->>fillDefList,
	D?defList_member->>selection(Selected),

	%ok, als Selected gelijk is aan het object van de CR �n aan de interne <-def
	%dan moeten we opnieuw inlezen

	if
		(	CR->>checkObject(Selected),
			Selected = D<<-def
		)
	then
		(D->>fillCurrentDefData).
%%

%%
changeApplied_deleteSketchDef(D,
	CR : changeRequestor
	):->
	/*
	Het kan dus zijn dat de geselecteerde def weg is. In dat geval kiezen we een andere
	*/

	List = D<<-defList_member,
	Deleted = CR<<-argument(1),
	if
		(Deleted = D<<-def)
	then (
		(
			New = List?nextItem<<-object
		;	New = List?prevItem<<-object
		;	New = @nil
		),

		List->>selection(New),
		D->>slot(def,New),
		D->>fillCurrentDefData
		),
	List->>delete(Deleted).
%%

/******************COMMANDS**************************/
%%
init_commands(_VE) :->
	"Initialise command objects" ::
        writef('init_commands: This should be specialized in subclasses\n',[]).
%%


/***********************************/

%%saveModel
%%
checkSaveModel(_VE):->
    %Always save the model
    true.
    %@on = @model<<-changed.
%
onSaveModel(_VE):->
	%gp3 just tell the app we want to save
	@app->>save.
%%


/****** IMPORT Functionality *********/
%

%
onImportConcepts(VE):->
	% Open a dialog with options for ImportConcepts
        send(VE, importConcepts, concept_map, entity).
%%

%
onImportEntities(VE):->
	% Open a dialog with options for ImportConcepts
        send(VE, importConcepts, structural_model, entity).
%%

%
onImportAgents(VE):->
	% Open a dialog with options for ImportConcepts
        send(VE, importConcepts, structural_model, agent).
%%

%
onImportAssumptions(VE):->
	% Open a dialog with options for ImportConcepts
        send(VE, importConcepts, structural_model, assumption).
%%

%
onImportQuantities(VE):->
	% Open a dialog with options for ImportConcepts
        send(VE, importConcepts, causal_model, quantity).
%%


%
importConcepts(VE, SourceSketch, SourceType) :->
	"Display window for importing concepts from Concept map, Causal Model, or Structural model"::

        if (SourceSketch = concept_map)
        then HelpID = 'Sketch_ImportConceptsFromConceptMapToSketchDefinition',
        if  (SourceSketch = causal_model)
        then HelpID = 'Sketch_ImportQuantitiesFromCausalModelToSketchDefinition',
        if  (SourceSketch = structural_model)
        then HelpID = 'Sketch_ImportConceptsFromStructuralModelToSketchDefinition',

	new(Dialog, assistanceDialog('Import', HelpID)),
	send(Dialog, transient_for, VE),
	send(Dialog, border, size(20,20)),

	MaxX *= number(0), %houden we de maximale X coordinaat in bij zodat we rechts kunnen uitlijnen
	% GapX = Dialog<<-gapX,
	GapY = Dialog<<-gapY,

	% create a listbrowser for choosing which concepts should be imported from the Concept map

	DefList *= extendedListBrowser(width := 53),
	DefList->>name(defList),
	% DefList->>show_label(@off),
	DefList->>multiple_selection(@on),

        if (SourceSketch = concept_map)
        then
             Label = 'Import the following concepts from the Concept map:',
        if (SourceSketch = causal_model)
        then
        (
             get(VE, createSourceTypePluralStr, SourceType, SourceTypeStr),
	     get(SourceTypeStr, value, SourceTypeStrVal),
             swritef(Label, 'Import the following %w from the Causal model:',[SourceTypeStrVal])
	),
        if (SourceSketch = structural_model)
        then
        (
             get(VE, createSourceTypePluralStr, SourceType, SourceTypeStr),
	     get(SourceTypeStr, value, SourceTypeStrVal),
             swritef(Label, 'Import the following %w from the Structural model:',[SourceTypeStrVal])
	),
        DefList->>label(Label),
	send(Dialog, append, DefList, below),
	% Dialog->>display(DefList, point(100, 200)),
	% Dialog->>display(DefList, point(10*GapX, Dialog?topY+200)),
	MaxX->>maximum(DefList?right_side),

	% Buttons for Select all and none
	%
        new(AllButton, imgButton(all,
			message(DefList, selection, DefList?members),
			img:=select_all,
			tt:= 'Select all'
			)
	),
        new(NoneButton, imgButton(none,
			message(DefList, selection, @nil),
			img := select_none,
			tt := 'Select none'
			)
	),
	send(Dialog, append, new(BTS, dialog_group(buttons, group)), right),
	send(BTS, gap, size(0, 0)),
	send(BTS, append, new(T1, text('Select')), below),
	send(T1, font, bold),
	send(BTS, append, AllButton, below),
	send(BTS, append, NoneButton, below),

	ListBottom *= number(DefList?bottom_side),
	ListBottom->>maximum(DefList?top_side + DefList?image?top_side + %=bovenkant vd lijst
					% EditDef?height +
					GapY), %hieruit volgt de onderkant van de lijst (minimaal ruimte van label + buttons)
	DefList->>bottom_side(ListBottom),


        if (SourceSketch = concept_map)
        then	VE->>fillDefListForType(Dialog, concept, SourceType),
        if (SourceSketch = causal_model)
        then	VE->>fillDefListForType(Dialog, sketchQuantity, SourceType),
        if (SourceSketch = structural_model)
        then	VE->>fillDefListForType(Dialog, sketchObject, SourceType),

	% create a menu for choosing what the concepts/objects should become in the new context
        get(VE, create_import_type_menu, SourceType, M1),

	send(Dialog, append, M1, below),
	% Dialog->>display(M1, point(Dialog?gapX, (DefList?bottom_side+Dialog?gapY)) ),

	Ok *= imgButton(import_ok, ->>(VE, import_ok, Dialog), img:=sketch_save, tt:='Apply'),
	Dialog->>append(Ok,below),
	% Dialog->>display(Ok,point(GapX, (M1?bottom_side + GapY)) ),
	Ok->>default_button(@on),
	%Ok->>keyboard_focus(@on),

	Close *= imgButton(close,->>(Dialog,destroy),tt:='Close this window'),
	Dialog->>append(Close, right),
	% Dialog->>display(Close,point((Dialog?right_side - Close?width),Ok?top_side)),
	send(Dialog, open).
%%

%
add_tabtool(_D, _TabTool):<-
        writef('add_tabtool called: This should be specialized in subclasses\n',[]).
%%

%
create_import_type_menu(_D, _SourceType, _M1):<-
        writef('create_import_type_menu: This should be specialized in subclasses\n',[]).
%%

%
createSourceTypePluralStr(_D, SourceTypeSingular: string, SourceTypeStr: string):<-
        get(SourceTypeSingular, value, SourceType),
        if SourceType = agent
        then swritef(SourceTypeStr, 'agents', []),
        if SourceType = quantity
        then swritef(SourceTypeStr, 'quantities', []),
        if SourceType = entity
        then swritef(SourceTypeStr, 'entities', []),
        if SourceType = assumption
        then swritef(SourceTypeStr, 'assumptions', []),
        if not(member(SourceType, [agent, quantity, entity, assumption]))
        then swritef(SourceTypeStr, 'unknown', []).
%%

%
import_type(_D, _Type):<-
        writef('import_type called: This should be specialized in subclasses\n',[]).
%%


%
import_ok(VE, D):->
	"Ok pressed: save changes" ::
        % for all selected elements from the Concept map, copy them to the right type

        get(VE, import_type, Type),
        if (@nil = VE<<-def)
        then
           send(VE, report_import_error)
        else
          (
	   get(D, member, defList, DefList),
	   get(DefList, selection, Sel),
	   send(Sel, for_all, message(@prolog, pl_import_for_sketch_definition, VE, DefList, @arg1, Type)),

	   send(VE, fillCurrentDefData)
	   ),

	D->>return.
%%

%
report_import_error(_VE):->
        writef('report_import_error called: This should be specialized in subclasses\n',[]).
%%



%
show_import_result(_VE, List, Def, NewName, Type):->
	"Show feedback about the result of what has been imported to what"::
	get(Def, name, Name),
	% get(NewNameStr, value, NewName),
	swritef(ReportTxt, '%w --> %w: %w',[Name, Type, NewName]),
        % VE->>msgBox(string(ReportTxt)),
	get(List?members, find, ->>(@arg1?name, equal, Name), El),
	send(El, label, ReportTxt).
%%



%
pl_import_for_sketch_definition(VE, List, Def, Type):-
        % writef('Import %d to type %d\n',[Def, Type]),
	get(Def?name, copy, DefName),
	pl_create_unique_name_SD(VE, DefName, Type, 1, Name),
	get(Def?remarks, copy, Remarks),
        % or should I use the copyBuffer, or an importBuffer?
	get(Name, value, NameV),
        get(VE, def, SketchDef),
        send(VE, importConcept(Name, Type, Remarks, SketchDef)),
	send(VE, show_import_result(List, Def, NameV, Type)).
%%


% create a unique name based on Name
%
pl_create_unique_name_SD(VE, Name, Type, _Number, Name):-
	not(pl_existing_name_SD(VE?def, Type, Name)),!.
%
pl_create_unique_name_SD(VE, Name, Type, Number, NewName):-
	if
             Number = 1
	then
	     NewName *= string('%s (imported)', Name)
        else
	     NewName *= string('%s (imported %s)', Name, Number),
	not(pl_existing_name_SD(VE?def, Type, NewName)),!.
%
pl_create_unique_name_SD(VE, Name, Type, Number, NewName):-
	%copy Number exists, raise the number
	NewNumber is Number + 1,
	pl_create_unique_name_SD(VE, Name, Type, NewNumber, NewName).
%%



%
pl_existing_name_SD(Def, Type, Name):-
        % check for existing sketchDefinitionElements of same type with equal name
	get(Def?def_sheet, Type, Elements),
        get(Elements, find, message(@arg1?name, equal, Name), _ElementWithSameName).
%%


%
importConcept(_VE, ConceptName, Type, Remarks, SketchDef):->
        % It would be nicer if changeRequest system was used to check names etc.
        new(Element, sketchDefinitionElement(ConceptName, Type, Remarks, SketchDef)),
        get(SketchDef?def_sheet, Type, Elements),
        send(Elements, append, Element).
%%


%
fillDefListForType(_VE, D, Type, ObjectType):->
	"(re)fill the definitions list with concepts of the concept map" ::

	List = D<<-defList_member,
	List->>clear,
	if (Type == concept)
        then Defs = @model<<-sortedSketchConcepts,
	if (Type == sketchQuantity)
        then
        (
          get(@model, hypered, causalModelMF, CM),
	  Defs = CM<<-sortedSketchQuantities
	),
	if (Type == sketchObject)
        then Defs = @model<<-sortedSketchObjectsTyped(ObjectType),
	CreateItem *= create(dict_item,
						@arg1,
						@arg1?name,
						@arg1),
	Defs->>for_all(->>(List,
			    append,
				CreateItem)),
	CreateItem->>done,
	Defs->>done.
%%



:-pce_end_class.
