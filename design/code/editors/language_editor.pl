/*
gp3 1.4.0: language_editor
Dialog for managing languages and creating translations
This editor contains all information for filling and changing. all ChangeRequestors regarding the change are now here
8-3-2007 Spin in het Web - Jelmer Jellema
*/


:-pce_begin_class(languageEditor,
		  assistanceDialog
		 ).

variable(lastCell,'table_cell*',both). %cell edited
variable(lastValue,char_array,both). %last value before change
%%
initialise(D):->
	%gp3. This is legacy homer code with some changes
	
	"Initialise the editor" ::
	
	D->+initialise('Language Editor','Build_Language'), 
	D->>application(@app),
	D->>kind(toplevel),
	
	/* Multiple models */
	%JJ: seems to be a nasty patch for multiple models. Why not put this in assistanceDialog and propertyDialog classes?
	%(not my pakkie on) (it removes the icons too)
	% JL: Somehow the changeApplied for delete language only works when there is a frame, therefore this
	% garpEditorFrame is still needed
    	get(@model, getModelNameForEditor, 'Language Editor - Build', ModelNameForEditorLabel),
	new(Garp3EditorFrame, garp3EditorFrame(ModelNameForEditorLabel)),
	get(@model, '_value', Model),
	send(Garp3EditorFrame, associateModel, Model),
	send(Garp3EditorFrame, append, D),

	D->>icon(@build_icon).
%%

%%
displayContent(D, TopY: int):->

	%gp3 Split off from initialise, called by assistanceDialog->initialise, display all stuff
	%TopY tells us where to start on the Y-axis
	
	%de onderdelen
	%De plaatsing doen we zelf op coordinaten, we gebruiken wel de standaard "gap"
	GapX = D?gap<<-width,
	GapY = D?gap<<-height,
	
	%add language combos
	%language combos only choosing, no typing
	Lang1 *= eventTextItem(lang1),
	Lang1->>onKey(->>(@pce,fail)),
	Lang1->>show_label(@off),
	Lang1->>length(20),
	Lang1->>style(combo_box),
	Lang1->>editable(@on),
	Lang1->>message(->>(D,onChangeLanguage)),
	Lang1->>value_set(@model?translator?allLanguages), %function is saved, so list will contain all values even after adding or removing one
	Lang1->>value(@model?translator?currentLanguage),
	D->>display(Lang1,point(GapX + 100, TopY)), %space for description column
	
	Lang2 *= eventTextItem(lang2),
	Lang2->>onKey(->>(@pce,fail)),
	Lang2->>show_label(@off),
	Lang2->>length(20),
	Lang2->>style(combo_box),
	Lang2->>editable(@on),
	Lang2->>message(->>(D,onChangeLanguage)),
	Lang2->>value_set(@model?translator?allLanguages),
	unless
		1 = @model?translator?allLanguages<<-size
	do
		Lang2->>value(?(@model?translator?allLanguages,find,not(->>(@arg1,equal,@model?translator?currentLanguage)))),
	
	D->>display(Lang2,point(GapX + 100 + 200,TopY)), %above col3
	
	
	%translation table
	ValDev *= window,
	%window is not found as a member, because of issues with window_decorator
	%so we hyper the lot
	%also: top_side is allways 0 for window, use window?decoration
	D->>hyper(ValDev,transwin),
	ValDev->>width(500), %default
	ValDev->>height(300),
	ValDev->>vertical_scrollbar(@on),
	
	Translation *= table,
	%hyper too
	D->>hyper(Translation,transtable),
	Translation->>cell_spacing(size(0,0)),
	Translation->>cell_padding(size(2,2)),
	Translation->>border(1),
	Translation->>frame(void),
	Translation->>rules(all),
	
	%we wants clicks on the table
	ValDev->>recogniser(click_gesture(left, message := ->>(D,onCellClick,Translation,@arg1))),
	Translation->>width(ValDev?width),

	Col1 = Translation<<-column(1,@on),
	Col1->>width(100),
	Col1->>fixed(@on),
	Col1->>halign(stretch),
	
	Col2 = Translation<<-column(2,@on), 
	Col2->>width(200),
	Col2->>halign(stretch),
	
	Col3 = Translation<<-column(3,@on), 
	Col3->>width(200),
	Col3->>halign(stretch),


	ValDev->>layout_manager(Translation),

	D->>display(ValDev,point(GapX,Lang1?bottom_side + GapY)),
	
	
	Data *= dialog_group(data,group),
	Value *= editor(height := 5, width := 40),
	Value->>name(value),
	Value->>show_label(@off),
	Value->>fill_mode(@on),
	Value->>font(normal),
	Value->>recogniser(new(handler(keyboard,->>(D,checkNav,@arg1)))), %cannot get it working using ->>binding
	Data->>display(Value,point(0,0)),
	
	Change *= imgButton(change, D->>checkSaveCellValue,img:=save, tt:= 'Apply changes'),
	Change->>default_button(@on),
	Data->>display(Change,point(ValDev?width - Change?width,0)),

	Value->>right_side(Change?left_side - GapX),
	
	Defs *= imgButton(editLanguage, ->>(D,onEditLanguage), img:=edit_changes, tt:='Edit languages'),
	Data->>display(Defs,point(0,Value?bottom_side + GapY)),
	
	ChangeAllMenu *= menu(changeAllMenu,toggle),
	ChangeAllMenu->>show_label(@off),
	ChangeAllMenu->>append(new(_CA,menu_item(changeAll, label:='Change all occurences (if possible)'))),

	Data->>display(ChangeAllMenu,point(Defs?right_side + GapX, Defs?top_side)),
	
	ChangeAllResult *= label(changeAllResult,''),
	Data->>display(ChangeAllResult,point(ChangeAllMenu?left_side,ChangeAllMenu?bottom_side + GapY)),
	
	Close *= imgButton(close,->>(D,destroy), tt:='Close this editor'), 
	Data->>display(Close,point(Change?left_side, Defs?top_side)),
	
	D->>display(Data,point(GapX,ValDev?decoration?bottom_side + GapY) ),	

	D->>fillTranslationMap, %fill up
		%minimal size:
	D->>minimalSize(size(ValDev?right_side + 2*GapX,Data?bottom_side + 2*GapY)). %abs min.
%%

%%
onEditLanguage(D):->
	Dlg *= languageChangeDlg(D),
	Dlg<<-confirm_centered(D?area?center),
	Dlg->>destroy.
%%


%%
onChangeLanguage(D):->
	D->>fillTranslationMap. %just do it all over again.
%%

%%
checkSaveCellValue(D):->
	%check if we have to save a changed value. This implies using a CR to change something, and maybe change the language for a while
	
			
	%remove old feedback
	D?data_member?changeAllResult_member->>selection(''),
		
	Cell = D<<-lastCell,
	NewValue = D?data_member?value_member<<-contents,
	OldValue = D<<-lastValue,
	if
	(
		Cell \== @nil,
		\+ NewValue->>equal(OldValue) %changed!
	)
	then
	(	
		Language = Cell<<-hypered(language),
		Object = Cell<<-hypered(object),
		Tag = Cell<<-hypered(tag),
		
		
		Translator = @model<<-translator,
		
		%temporarily change language
		CurrentLanguage = Translator<<-currentLanguage,
		
		%check if this will involve a language change
		%this boolean will also be sent to the real change as "silent": there is no redraw needed when the change involves another language.
		if
			CurrentLanguage->>equal(Language)
		then
			ChangeLanguage = @off
		else
			ChangeLanguage = @on,
			
		%to change the language for a while, we do not use an internal method, but the "official" CR
		%this is because the actual value changing will also be involving CRs,
		%so there might be some repainting needed following the value changing CR (depending on whether or not this is a temporary language chain)
		%this is why there is now a "silent" option to changeRequest
		
	
		if
			ChangeLanguage = @on
		then
			@model->>changeRequestEx(setCurrentLanguage,@model,chain(silent,@on),D,Language), %set silent attribute
		
		%for the change, we have a whole set of prologclauses below
		ClassName = Object<<-class_name, %sometimes interesting
	
		%do we change all cells containing the old value? If so, we process all
		%and do not quit if a changerequestor fails (that one keeps the old value then)
		
		if
			@on = D?data_member?changeAllMenu_member<<-selected(changeAll)
		then
		(
			%first this one:
			LocalNew = NewValue<<-copy,
			ignore(pl_changeValue(D,ClassName, Object,Tag,LocalNew,ChangeLanguage,@off)), %no need to send language or translatable
			%changed succesfully?
			unless
				?(Cell,hypered,translatable)?value->>equal(OldValue)
			do
			(
				ExtraChangesOk *= number(0),
				ExtraChangesFailed *= number(0),
				D->>doChangeAllValues(?(D,hypered,transtable), OldValue, NewValue, ChangeLanguage, Cell?column, 1, ExtraChangesOk, ExtraChangesFailed), %no feedback, just try it
				D?data_member?changeAllResult_member->>selection(string('Extra changes: success: %d failure: %d',ExtraChangesOk, ExtraChangesFailed))
			)
		)
		else
			ignore(pl_changeValue(D,ClassName, Object,Tag,NewValue,ChangeLanguage,@off)), %no need to send language or translatable
		%and reset language, we need a CR now
		
		if
			ChangeLanguage = @on
		then
			@model->>changeRequestEx(setCurrentLanguage,@model,chain(silent,@on),D,CurrentLanguage),
		
		%change the data
		Cell?image->>clear,
		Cell?image->>cdata(?(?(Cell,hypered,translatable),valueForLanguage,Language)), %pl_changeValue may have changed the translatable connected
		?(D,hypered,transwin)->>redraw, %force redraw
		D->>updateValue
	).
%
doChangeAllValues(D,T: table, Old: char_array, New: char_array, Silent: bool, X: int, Y: int,ExtraOk: number, ExtraFail:number):->
	%helper for checkSaveCellValue. Loop over all cells in column X, changing value to New if it equals Old
	
	%if we cannot get row number Y, we are done
	if
		Row = T<<-row(Y,@off)
	then
	(
		%see if this row has a column X, that contains a translatable which value is Old
		%(language is allready changed)
		if
		(
			Cell = Row<<-cell(X),
			Trans = Cell<<-hypered(translatable),	
			Old->>equal(Trans?value)
		)		
		then
		(
			%change it!
			Object = Cell<<-hypered(object),
			Classname = Object<<-class_name, %spell it out, we are going to use prolog
			Tag = Cell<<-hypered(tag),
			NewValue = New<<-copy,
			if
				pl_changeValue(D,Classname, Object, Tag, NewValue, Silent, @on) %no feedback, just try
			then
				ExtraOk->>plus(1)
			else
				ExtraFail->>plus(1),
			%change the data to whatever pl_changeValue has made of it
			Cell?image->>clear,
			Cell?image->>cdata(Trans?value) %redraw happens in chackSaveCellValue
			
		),
		%continue
		NewY is Y  + 1,
		D->>doChangeAllValues(T, Old, New, Silent, X, NewY,ExtraOk,ExtraFail)
	).
	
	
%%

checkNav(D,E: event):->
	%check Event in value box
	%we catch ctrl+cursor key, cannot get it working using editor->>binding
	E->>has_modifier('c'),
	K = E<<-key,
	(
		(K = 'cursor_up', D->>doNav(0,-1))
	;
		(K = 'cursor_down', D->>doNav(0,1))
	;
		(K = 'cursor_left', D->>doNav(-1,0))
	;
		(K = 'cursor_right', D->>doNav(1,0))
	).
%%

%%
doNav(D, X: int, Y: int):->
	%navigate to a different cell
	Table = D<<-hypered(transtable),
	Current = D<<-currentCell,
	if
		New = Table<<-cell(Current?column + X, Current?row + Y) %fails if not a valid value, thats ok
	then
	(
		%translatable?
		if 
			New<<-hypered(translatable)
		then
			D->>selectCell(New)
		else
		(
			%yes! Recursion!
			(
				(X <0, NewX is X - 1)
			;
				(X = 0, NewX = X)
			;
				(X > 0, NewX is X + 1)
			),
			(
				(Y <0, NewY is Y - 1)
			;
				(Y = 0, NewY = Y)
			;
				(Y > 0, NewY is Y + 1)
			),			
			D->>doNav(NewX,NewY)
		)
	).
%%
	
%%


%%
onResize(D, Difference: size):->
	Transwin = D<<-hypered(transwin),
	Transwin->>width(Transwin?width + Difference?width),
	Transwin->>height(Transwin?height + Difference?height),
	Transtable = D<<-hypered(transtable),
	Transtable->>width(Transwin?width),
	
	Data = D<<-data_member,
	Data?change_member->>set(x:= Data?change_member?left_side + Difference?width),
	Data?value_member->>right_side(Data?value_member?right_side + Difference?width),
	Data?close_member->>set(x:= Data?change_member?left_side),
	
	Data->>set(y:= Data?top_side + Difference?height),
	D->>resizeColumns. %recalc them, also done after filling
%%

%%
resizeColumns(D):->
	%recalc columns sizes
	
	Transwin = D<<-hypered(transwin),
	Transtable = D<<-hypered(transtable),
	
	Col1 = Transtable<<-column(1),
	Col1->>width(100),
	

	Col2 = Transtable<<-column(2),
	Col2->>width((Transwin?width - Col1?width) / 2),
	Col3 = Transtable<<-column(3),
	Col3->>width((Transwin?width -  Col1?width) / 2),

	D?lang1_member->>set(x:= D?gap?width + Col1?width),
	D?lang2_member->>set(x:=  D?lang1_member?left_side + Col2?width).
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
onCellClick(D,Table: table, E: event):->
	%click in table
	%select cell and fill data
		
	Cell = Table<<-cell_from_position(E),
	Cell->>instance_of(table_cell), %can also be a point
	D->>selectCell(Cell).
%%

%%
selectCell(D,Cell: table_cell):->
	\+ Cell = D<<-lastCell,
	D->>checkSaveCellValue, %try to save the current value
	D->>lastCell(@nil),
	if
		 Cell<<-hypered(translatable)
	then
	(
		%remove old selection bg
		Table = D<<-hypered(transtable),
		ignore(Table?selection->>for_some(
			->>(@arg1,background,@default))),
		%add new one
		Cell->>background(grey),
		Table->>selection(Cell),
		Table?device->>normalise(Cell?area,y),
		D->>lastCell(Cell),
		V = D?data_member<<-member(value),
		V->>keyboard_focus(@on),
		D->>updateValue,
		V->>selection(0,V?text_buffer?size,active)
	).
%%

%%
currentCell(D, Cell: table_cell):<-
	%get the currently selected cell or fail if none
	%assumption: single selection
	
	Table = D<<-hypered(transtable),
	Cell = Table?selection<<-head.
%%

%%
%updateValue: fill the value field with the currently selected value

updateValue(D):->
	Data = D<<-member(data),
	if
	(
		Cell = D<<-lastCell, %we save the cell, this is the last one clicked
		Cell \== @nil
	)
	then
	(
		%get the translatable etc
		T = Cell<<-hypered(translatable),
		L = Cell<<-hypered(language),
		%we allways get the info from the translatable
		
		(
			Val = T<<-valueForLanguage(L)
		;
			Val = @nil
		),
		if
			Val = @nil
		then
			Value = ''
		else
			Value = Val,
		Data?value_member->>contents(Value),
		D->>lastValue(Value),
		Data?change_member->>active(@on),
		Data?changeAllMenu_member->>active(@on)
	)
	else
	(
		Data?value_member->>contents(''),
		D->>lastValue(''), %better safe than sorry
		Data?change_member->>active(@off),
		Data?changeAllMenu_member->>active(@off)
	).
%%
		
	

%%fillTranslationMap: create the translationmap
%main function is fillTranslationMap_Element

fillTranslationMap(D):->
	%first, get some one time information
	(Lang1 = @model?translator?languages<<-find(->>(@arg1,equal,D?lang1_member?value)); Lang1 = @nil),
	(Lang2 = @model?translator?languages<<-find(->>(@arg1,equal,D?lang2_member?value)); Lang2 = @nil),
	Table = D<<-hypered(transtable),
	
	%display
	Table->>delete_rows,
	D->>lastCell(@nil), 
	D->>updateValue,
	
	%ok, this is just hardwired stuff now
	
	%entities
	%this one is allways there, so no need to check for empty set
	D->>fillTranslationMap_Category(Table, 'Entities'),
	@model?sortedEntities->>for_all(
			->>(D,fillTranslationMap_Element,Table,Lang1,Lang2,@arg1,@arg1?name?capitalise,
						'Name',name,@arg1?name_translatable,
						'Remarks',remarks, @arg1?remarks_translatable)),
						
	%attributes, this is a special call because of the values
	D->>fillTranslationMap_Category(Table, 'Attribute definitions'),
	@model?sortedAttributeDefinitions->>for_all(
			->>(D,fillTranslationMap_attributeDef,Table,Lang1,Lang2,@arg1)),	
	
	%configurationdefs
	D->>fillTranslationMap_Category(Table, 'Configuration definitions'),
	@model?sortedConfigurationDefinitions->>for_all(
			->>(D,fillTranslationMap_Element,Table,Lang1,Lang2,@arg1,@arg1?name?capitalise,
						'Name',name,@arg1?name_translatable,
						'Remarks',remarks, @arg1?remarks_translatable)),
			
	%quantities
	D->>fillTranslationMap_Category(Table, 'Quantity definitions'),
	@model?sortedQuantityDefinitions->>for_all(
			->>(D,fillTranslationMap_Element,Table,Lang1,Lang2,@arg1,@arg1?name?capitalise,
						'Name',name,@arg1?name_translatable,
						'Remarks',remarks, @arg1?remarks_translatable)),
										
	%qs, this is a special call because of the values
	D->>fillTranslationMap_Category(Table, 'Quantity spaces'),
	@model?sortedQuantitySpaces->>for_all(											%we just skip @model?dqs, nontranslatable
			->>(D,fillTranslationMap_qs,Table,Lang1,Lang2,@arg1)),	
							
	%modelfragments and scenarios are more complicated, because of their elements
				
	D->>fillTranslationMap_Category(Table,'Scenarios'),
	@model?sortedInputSystems->>for_all(
			->>(D,fillTranslationMap_Modelfragment,Table,Lang1,Lang2,@arg1)),	
			
			
	D->>fillTranslationMap_Category(Table,'Modelfragments'),
	@model?sortedModelFragmentsOnly->>for_all(
			->>(D,fillTranslationMap_Modelfragment,Table,Lang1,Lang2,@arg1)),

			
	%agents
	D->>fillTranslationMap_Category(Table, 'Agents'),
	@model?sortedAgents->>for_all(
			->>(D,fillTranslationMap_Element,Table,Lang1,Lang2,@arg1,@arg1?name?capitalise,
						'Name',name,@arg1?name_translatable,
						'Remarks',remarks, @arg1?remarks_translatable)),	
				
	%assumptions
	D->>fillTranslationMap_Category(Table, 'Assumptions'),
	@model?sortedAssumptions->>for_all(
			->>(D,fillTranslationMap_Element,Table,Lang1,Lang2,@arg1,@arg1?name?capitalise,
						'Name',name,@arg1?name_translatable,
						'Remarks',remarks, @arg1?remarks_translatable)),	
																
	D->>resizeColumns. %recompute column sizes needed after refill, otherwise we get garbage, dont know why

fillTranslationMap_Category(_D, Table: table, Category: name):->
	%start a new category
	Table->>next_row(@on),
	CatCel *= table_cell(text(Category)),
	CatCel->>col_span(3),
	CatCel->>background(orange),
	Table->>append(CatCel).
	
fillTranslationMap_Element(D, Table: table, Lang1: 'string*', Lang2: 'string*', Element: object, 
													 ContainerLabel: 'char_array*', Entries: 'any...'):->
													 
	%helper: fill for this element, showing Entries (Label, tag, translatable|function,...)
	%the translatable arguments (nr3 of every set) are used to get the current value
	%for some objects you have to use some quote_function(...) construct, because the translatable itself is replaced all the time
	%see attributedef and quantityspacedef
	
	%we start with the container, if not @nil
	unless
		ContainerLabel == @nil
	do
	(
		Table->>next_row,
		ContCel *= table_cell(text(ContainerLabel)),
		ContCel->>col_span(3),
		ContCel->>background(skyblue),
		Table->>append(ContCel)
	),
	%now for the entries, yet another call
	D->>fillTranslationMap_Entries(Table,Lang1,Lang2, Element, Entries, 1).
%

fillTranslationMap_Entries(_D, _Table: table, _Lang1: 'string*', _Lang2: 'string*', _Element: object, 
													Entries: vector, Index: int):->
	%display 1 entry containing three elements from the Entries vector, and continue
	%stopclause
	
	\+ Entries<<-element(Index), %no more entries
	!.
%	
fillTranslationMap_Entries(D, Table: table, Lang1: 'string*', Lang2: 'string*', Element: object, 
													Entries: vector,Index: int):->	
	Label = Entries<<-element(Index),
	Tag = Entries<<-element(Index + 1),
	Translatable = Entries<<-element(Index + 2), %can be a function too
	
	%20070419: by request, we show even values that are empty in both languages
	

	%skip if nontranslatable
	unless
		Translatable->>isNonTranslatable
	do
	(
		Table->>next_row,
		LabelCell *= parbox,
		LabelCell->>cdata(Label),
		Table->>append(LabelCell),
		
		%1st value
		Lang1Cell *= table_cell(new(parbox)),
		unless
			Lang1 == @nil
		do
		(
			Lang1Cell->>hyper(Lang1,language),
			Lang1Cell->>hyper(Element,object),
			Lang1Cell->>hyper(Tag,tag),
			Lang1Cell->>hyper(Translatable, translatable), %can be a function too
			Lang1Cell?image->>cdata(?(Translatable,valueForLanguage,Lang1)) %must be the real language object!

		),
		Table->>append(Lang1Cell),
		
		%2nd value
		Lang2Cell *= table_cell(new(parbox)),
		unless 
			Lang2 == @nil
		do
		(
			Lang2Cell->>hyper(Lang2,language),
			Lang2Cell->>hyper(Element,object),
			Lang2Cell->>hyper(Tag,tag),
			Lang2Cell->>hyper(Translatable, translatable),
			Lang2Cell?image->>cdata(?(Translatable,valueForLanguage,Lang2)) %must be the real language object!
		),
		Table->>append(Lang2Cell)
	),
	%and continue
	D->>fillTranslationMap_Entries(Table,Lang1,Lang2, Element, Entries, Index + 3).
%%



%%
fillTranslationMap_attributeDef(D,Table: table, Lang1: 'string*', Lang2: 'string*', AD: garpAttributeDefinition):->
	%call for filling map for 1 attributeDefinition
	
	D->>fillTranslationMap_Element(Table,Lang1,Lang2,AD,AD?name?capitalise,
						'Name',name,AD?name_translatable,
						'Remarks',remarks, AD?remarks_translatable),
	%values
	AD?values->>for_all(
		->>(D,fillTranslationMap_attributeDefValue,Table,Lang1,Lang2,AD,@arg1)). %yet another helper
%
fillTranslationMap_attributeDefValue(D,Table: table, Lang1: 'string*', Lang2: 'string*', AD: garpAttributeDefinition, Val: valueReference):->
	%we use a function to find the value by id, because every change created a new valueReference, so we cannot just remember the object
	%we have to remember its id
	ID = Val<<-id, %make it a non-object value
	D->>fillTranslationMap_Element(Table,Lang1,Lang2,tuple(AD,ID),@nil,'Value',attribDefValue,
						quote_function(?(AD?values,find,@arg1?id == ID)?valueName_translatable)).
%%

%%
fillTranslationMap_qs(D,Table: table, Lang1: 'string*', Lang2: 'string*', QS: quantitySpace):->
	%call for filling map for 1 quantitySpace
	
	%do not even show when nothing is translatable
	unless
	(
		QS?name_translatable->>isNonTranslatable,
		QS?remarks_translatable->>isNonTranslatable,
		\+ QS?values->>find(->>(@arg1,translatable))
	)
	do
	(	
		D->>fillTranslationMap_Element(Table,Lang1,Lang2,QS,QS?name?capitalise,
						'Name',name,QS?name_translatable,
						'Remarks',remarks, QS?remarks_translatable),
		%values, another helper
		QS?values->>for_all(
			->>(D,fillTranslationMap_QSValue,Table,Lang1,Lang2,QS,@arg1))
	).
%
fillTranslationMap_QSValue(D,Table: table, Lang1: 'string*', Lang2: 'string*', QS: quantitySpace, Val: valueReference):->
	%we use a function to find the value by id, because every change created a new valueReference, so we cannot just remember the object
	%we have to remember its id
	ID = Val<<-id, %make it a non-object value
	D->>fillTranslationMap_Element(Table,Lang1,Lang2,tuple(QS,ID),@nil,'Value',qsValue,
						quote_function(?(QS?values,find,@arg1?id == ID)?valueName_translatable)).
%%
	
%%
fillTranslationMap_Modelfragment(D,Table: table, Lang1: 'string*', Lang2: 'string*', MF: modelFragment):->
	%call for filling map for 1 modelfragment
	
	D->>fillTranslationMap_Element(Table,Lang1,Lang2,MF,MF?name?capitalise,
						'Name',name,MF?name_translatable,
						'Remarks',remarks, MF?remarks_translatable),
	

	%Instances
	Instances = MF<<-findElements(garpInstance),
	Instances->>sort(?(@arg1?name,compare,@arg2?name)),
	Instances->>for_all(
		->>(D,fillTranslationMap_Element,Table,Lang1,Lang2,@arg1,@nil,
					'Instance - name',name,@arg1?name_translatable,
					'   - remarks', remarks, @arg1?remarks_translatable)),
					
	%assumption instances
	AssumptionInstances = MF<<-findElements(assumptionInstance),
	AssumptionInstances->>sort(?(@arg1?name,compare,@arg2?name)),
	AssumptionInstances->>for_all(
		->>(D,fillTranslationMap_Element,Table,Lang1,Lang2,@arg1,@nil,
					create(string,'Assumption \'%s\' - remarks',@arg1?name), remarks, @arg1?remarks_translatable)),
					
	%attributes
	Attributes = MF<<-findElements(garpAttribute),
	Attributes->>sort(?(@arg1?name,compare,@arg2?name)),
	Attributes->>for_all(
		->>(D,fillTranslationMap_Element,Table,Lang1,Lang2,@arg1,@nil,
					create(string,'Attribute \'%s\' - remarks',@arg1?name), remarks, @arg1?remarks_translatable)),
					
	%configurations
	Configurations = MF<<-findElements(configuration),
	Configurations->>for_all(
		->>(D,fillTranslationMap_Element,Table,Lang1,Lang2,@arg1,@nil,
					create(string,'Configuration \'%s\' - remarks',@arg1?name), remarks, @arg1?remarks_translatable)),

	%quantities
	Quantities = MF<<-findElements(garpQuantity),
	Quantities->>for_all(
		->>(D,fillTranslationMap_Element,Table,Lang1,Lang2,@arg1,@nil,
					create(string,'Quantity \'%s\' - remarks',@arg1?name), remarks, @arg1?remarks_translatable)),
					
	%inequealities
	Inequalities = MF<<-findElements(inequality),
	Inequalities->>for_all(
		->>(D,fillTranslationMap_Element,Table,Lang1,Lang2,@arg1,@nil,
					'Inequality - remarks', remarks, @arg1?remarks_translatable)),
						
	%correspondences:
	Correspondences = MF<<-findElements(correspondence),
	Correspondences->>for_all(
		->>(D,fillTranslationMap_Element,Table,Lang1,Lang2,@arg1,@nil,
					'Correspondence - remarks', remarks, @arg1?remarks_translatable)),
					
	%quantity relations
	QuantityRelations = MF<<-findElements(garpQuantityRelation),
	QuantityRelations->>for_all(
		->>(D,fillTranslationMap_Element,Table,Lang1,Lang2,@arg1,@nil,
					create(string,'%s - remarks',when(@arg1?type == 'prop', 'Proportionality','Influence')), remarks, @arg1?remarks_translatable)),
															
	%calculi
	Calculi = MF<<-findElements(calculus),
	Calculi->>for_all(
		->>(D,fillTranslationMap_Element,Table,Lang1,Lang2,@arg1,@nil,
					'Calculus - remarks', remarks, @arg1?remarks_translatable)),
						
	%fragmentRefiners and importedFragments (not parents)
	ImportedFragments = MF<<-findElements(importedFragment),
	ImportedFragments->>sort(?(@arg1?name,compare,@arg2?name)),
	ImportedFragments->>for_all(
		->>(D,fillTranslationMap_Element,Table,Lang1,Lang2,@arg1,@nil,
					create(string,'Model fragment \'%s\' - remarks',@arg1?name), remarks, @arg1?remarks_translatable)),
					
	%identities				
	Identities = MF<<-findElements(identityRelation,@off), %no subclasses (fragmentRefinerIdentity)
	Identities->>for_all(
		->>(D,fillTranslationMap_Element,Table,Lang1,Lang2,@arg1,@nil,
					'Identity - remarks', remarks, @arg1?remarks_translatable)).
										
%%%%CHANGING VALUE

%this is called from onChangeValue above
%pl_changeValue(D,ClassName, Object,Tag,NewValue,Silent, Nofeedback): call the right CR for this change

%Hierarchical Object
pl_changeValue(D,_Classname,Object,name,NewValue,Silent, Nofeedback):-
	Object->>instance_of(hierarchicalObject),
	@model->>changeRequestEx(changeHObject,Object,chain(silent,Silent,nofeedback,Nofeedback), D,NewValue,@nil,Object?remarks).
%
pl_changeValue(D,_,Object,remarks,NewValue,Silent, Nofeedback):-
	Object->>instance_of(hierarchicalObject),
	@model->>changeRequestEx(changeHObject,Object,chain(silent,Silent,nofeedback,Nofeedback), D,Object?name,@nil,NewValue).
%%

%attribute definition
pl_changeValue(D,garpAttributeDefinition,Object,name,NewValue,Silent, Nofeedback):-
	@model->>changeRequestEx(changeAttributeDef, Object,chain(silent,Silent,nofeedback,Nofeedback),  D, NewValue, Object?values, Object?remarks).

%attribute definition
pl_changeValue(D,garpAttributeDefinition,Object,remarks,NewValue,Silent, Nofeedback):-
	@model->>changeRequestEx(changeAttributeDef, Object,chain(silent,Silent,nofeedback,Nofeedback),  D, Object?name, Object?values, NewValue).
	
pl_changeValue(D,tuple,Object,attribDefValue,NewValue,Silent, Nofeedback):-
	%this one is complicated. We want a new valueReference containing a copy of the old translatable, with only the current language changed
	%object is a tuple: the definition + the id of the changed value
	
	AD = Object<<-first,
	Values = AD<<-values, %gives a copy
	ThisOne = Values<<-find(@arg1?id ==Object?second),
	ThisOne->>valueName(NewValue), %only this language
	@model->>changeRequestEx(changeAttributeDef,AD,chain(silent,Silent,nofeedback,Nofeedback), D,AD?name,Values,AD?remarks).
%%
	
%%configurationDefinition
pl_changeValue(D,configurationDefinition,Object,name,NewValue,Silent, Nofeedback):-
	@model->>changeRequestEx(changeConfigurationDef, Object,chain(silent,Silent,nofeedback,Nofeedback),  D, NewValue, Object?remarks).

%%configurationDefinition
pl_changeValue(D,configurationDefinition,Object,remarks,NewValue,Silent, Nofeedback):-
	@model->>changeRequestEx(changeConfigurationDef, Object,chain(silent,Silent,nofeedback,Nofeedback),  D, Object?name, NewValue).
	
%%quantitydef
pl_changeValue(D,garpQuantityDefinition,Object,name,NewValue,Silent, Nofeedback):-
	@model->>changeRequestEx(changeQuantityDef, Object,chain(silent,Silent,nofeedback,Nofeedback),  D, NewValue, Object?allowedQuantitySpaces, Object?remarks).

%%quantitydef
pl_changeValue(D,garpQuantityDefinition,Object,remarks,NewValue,Silent, Nofeedback):-
	@model->>changeRequestEx(changeQuantityDef, Object,chain(silent,Silent,nofeedback,Nofeedback),  D, Object?name, Object?allowedQuantitySpaces, NewValue).
%%

%%quantityspace
pl_changeValue(D,quantitySpace,Object,name,NewValue,Silent, Nofeedback):-
	@model->>changeRequestEx(changeQuantitySpace, Object,chain(silent,Silent,nofeedback,Nofeedback),  D, NewValue, Object?values, Object?remarks).
%
%quantityspace
pl_changeValue(D,quantitySpace,Object,remarks,NewValue,Silent, Nofeedback):-
	@model->>changeRequestEx(changeQuantitySpace, Object,chain(silent,Silent,nofeedback,Nofeedback),  D, Object?name, Object?values, NewValue).
%	
pl_changeValue(D,tuple,Object, qsValue,NewValue,Silent, Nofeedback):-
	%this one is complicated. We want a new valueReference containing a copy of the old translatable, with only the current language changed
	%object is a tuple: the definition + the id of the changed value
	
	QS = Object<<-first,
	Values = QS<<-values, %gives a copy
	ThisOne = Values<<-find(@arg1?id ==Object?second),
	ThisOne->>valueName(NewValue), %only this language
	@model->>changeRequestEx(changeQuantitySpace,QS,chain(silent,Silent,nofeedback,Nofeedback), D,QS?name,Values,QS?remarks).
%	
	
%modelfragments and scenarios
pl_changeValue(D,modelFragment,Object,name,NewValue,Silent, Nofeedback):-
	@model->>changeRequestEx(changeMF,Object,chain(silent,Silent,nofeedback,Nofeedback), D,NewValue,Object?remarks,Object?parents,Object?active).
%
pl_changeValue(D,modelFragment,Object,remarks,NewValue,Silent, Nofeedback):-
	@model->>changeRequestEx(changeMF, Object,chain(silent,Silent,nofeedback,Nofeedback),  D, Object?name, NewValue, Object?parents, Object?active).
%
pl_changeValue(D,inputSystem,Object,name,NewValue,Silent, Nofeedback):-
	@model->>changeRequestEx(changeInputSystem,Object,chain(silent,Silent,nofeedback,Nofeedback), D,NewValue,Object?remarks).
%
pl_changeValue(D,inputSystem,Object,remarks,NewValue,Silent, Nofeedback):-
	@model->>changeRequestEx(changeInputSystem, Object,chain(silent,Silent,nofeedback,Nofeedback),  D, Object?name, NewValue).
%%elements
pl_changeValue(D,garpInstance,Object,name,NewValue,Silent, Nofeedback):-
	@model->>changeRequestEx(changeFInstance,Object?fragment,chain(silent,Silent,nofeedback,Nofeedback),  D, Object, Object?entity, Object?stateName,NewValue,Object?remarks).
%
pl_changeValue(D,garpInstance,Object,remarks,NewValue,Silent, Nofeedback):-
	@model->>changeRequestEx(changeFInstance,Object?fragment,chain(silent,Silent,nofeedback,Nofeedback),  D, Object, Object?entity, Object?stateName,Object?name,NewValue).
%
pl_changeValue(D,assumptionInstance,Object,remarks,NewValue,Silent, Nofeedback):-
	@model->>changeRequestEx(changeAssumptionInstance,Object?fragment,chain(silent,Silent,nofeedback,Nofeedback),  D, Object, Object?assumption, NewValue).
%
pl_changeValue(D,garpAttribute,Object,remarks,NewValue,Silent, Nofeedback):-
	@model->>changeRequestEx(changeAttribute,Object?fragment,chain(silent,Silent,nofeedback,Nofeedback),  D, Object, Object?stateName, Object?definition,Object?valueReference, NewValue).
%
pl_changeValue(D,garpQuantity,Object,remarks,NewValue,Silent, Nofeedback):-
	@model->>changeRequestEx(changeQuantity,Object?fragment,chain(silent,Silent,nofeedback,Nofeedback),  D, Object, Object?stateName, Object?definition,Object?quantitySpace, NewValue, Object?quantityAssumptions).
%
pl_changeValue(D,inequality,Object,remarks,NewValue,Silent, Nofeedback):-
	@model->>changeRequestEx(changeInequality,Object?fragment,chain(silent,Silent,nofeedback,Nofeedback),  D, Object, Object?stateName, Object?type, Object?argument1, Object?argument1Route, Object?argument1Type, Object?argument1QSPoint, Object?argument2, Object?argument2Route, Object?argument2Type, Object?argument2QSPoint, NewValue).
%
pl_changeValue(D,correspondence,Object,remarks,NewValue,Silent, Nofeedback):-
	@model->>changeRequestEx(changeCorrespondence,Object?fragment,chain(silent,Silent,nofeedback,Nofeedback),  D, Object, Object?directed, Object?derivative,Object?mirror,Object?full, Object?argument1, Object?argument1Route, Object?argument1Value, Object?argument2, Object?argument2Route, Object?argument2Value, NewValue).
%
pl_changeValue(D,_,Object,remarks,NewValue,Silent, Nofeedback):-
	Object->>instance_of(garpQuantityRelation), %prop/inf
	@model->>changeRequestEx(changeQuantityRelation, Object?fragment,chain(silent,Silent,nofeedback,Nofeedback),  D, Object, Object?type, Object?argument1, Object?argument1Route, Object?argument2, Object?argument2Route, Object?sign, NewValue).
%
pl_changeValue(D,calculus,Object,remarks,NewValue,Silent, Nofeedback):-
	@model->>changeRequestEx(changeCalculus,Object?fragment,chain(silent,Silent,nofeedback,Nofeedback),  D, Object, Object?sign, Object?type, Object?argument1, Object?argument1Route, Object?argument1QSPoint, Object?argument2, Object?argument2Route, Object?argument2QSPoint, NewValue).
%
pl_changeValue(D,configuration,Object,remarks,NewValue,Silent, Nofeedback):-
	@model->>changeRequestEx(changeConfiguration,Object?fragment,chain(silent,Silent,nofeedback,Nofeedback),  D, Object, Object?stateName, Object?definition, Object?argument1, Object?argument1Route, Object?argument2,Object?argument2Route,NewValue).
%
pl_changeValue(D,_,Object,remarks,NewValue,Silent, Nofeedback):-
	Object->>instance_of(importedFragment), %also: refiner
	@model->>changeRequestEx(changeConditionalFragment, Object?fragment,chain(silent,Silent,nofeedback,Nofeedback),  D, Object,NewValue).
%
pl_changeValue(D,identityRelation,Object,remarks,NewValue,Silent, Nofeedback):-
	@model->>changeRequestEx(changeConditionalFragment, Object?fragment,chain(silent,Silent,nofeedback,Nofeedback),  D, Object,NewValue).			
%

%%
%%%CHANGE REQUESTORS

changeApplied_setCurrentLanguage(_D,
	_CR: changeRequestor):->
	
	%we do nothing (overwriting superclass)
	true.
%%
	
changeApplied_deleteLanguage(D, CR: changeRequestor):->
	if
	    D?lang1_member?value->>equal(CR?arg1)
	then
	(
	    D?lang1_member->>value(@model?translator?currentLanguage), %should always be there
	    D->>fillTranslationMap
	),
	if
	    D?lang2_member?value->>equal(CR?arg1)
	then
	(
	    D?lang2_member->>value(''), 
	    D->>fillTranslationMap
	).
%%

%%
changeApplied_addLanguage(D,CR: changeRequestor):->
	%auto select
	D?lang2_member->>value(CR?arg1),
	D->>fillTranslationMap.
%%

%%
changeApplied_changeLanguageName(D,CR: changeRequestor):->
	%just change the name
	if
	    D?lang1_member?value->>equal(CR?arg1)
	then
	(
	    D?lang1_member->>value(CR?arg2), 
	    D->>fillTranslationMap
	),
	if
	    D?lang2_member?value->>equal(CR?arg1)
	then
	(
	    D?lang2_member->>value(CR?arg2), 
	    D->>fillTranslationMap
	).

:-pce_end_class.

:-pce_begin_class(languageChangeDlg,
		  assistanceDialog
		 ).
%helper dialog for changing language settings

initialise(D, Editor: editor = languageEditor):->
	D->+initialise('Language settings','Build_Language_settings'), 
	D->>application(@app),
	D->>transient_for(Editor),
	D->>hyper(Editor,editor),
	D->>modal(transient),
	D->>kind(toplevel),
	D->>can_resize(@off),
	D->>scrollbars(none), %not needed (gp3 0.3.13)
	D->>icon(@build_icon),

	/* Multiple model support */
    	get(@model, getModelNameForEditor, 'Language Settings - Build', ModelNameForEditorLabel),
	new(Garp3EditorFrame, garp3EditorFrame(ModelNameForEditorLabel)),
	get(@model, '_value', Model),
	send(Garp3EditorFrame, associateModel, Model),
	send(Garp3EditorFrame, append, D).
%% 
		
%%
displayContent(D, TopY: int):->

	%gp3 Split off from initialise, called by assistanceDialog->initialise, display all stuff
	%TopY tells us where to start on the Y-axis
	
	%de onderdelen
	%De plaatsing doen we zelf op coordinaten, we gebruiken wel de standaard "gap"
	GapX = D?gap<<-width,
	GapY = D?gap<<-height,
	
	CurrentLanguage *= eventTextItem(currentLanguage),
	CurrentLanguage->>onKey(->>(@pce,fail)),
	CurrentLanguage->>label('Set current language:'),
	CurrentLanguage->>length(20),
	CurrentLanguage->>style(combo_box),
	CurrentLanguage->>editable(@on),
	CurrentLanguage->>value_set(@model?translator?allLanguages), %function is saved, so list will contain all values even after adding or removing one
	CurrentLanguage->>value(@model?translator?currentLanguage),
	D->>display(CurrentLanguage,point(0,TopY)),	
	CL_go *= button(clgo,->>(D,onChangeCurrentLanguage)),
	CL_go->>label('go'),
	CL_go->>height(CurrentLanguage?height),
		
	AddLanguage *= eventTextItem(addLanguage),
	AddLanguage->>onKey(->>(@pce,fail)),
	AddLanguage->>label('Activate language:'),
	AddLanguage->>length(20),
	AddLanguage->>style(combo_box),
	AddLanguage->>editable(@on),
	AddLanguage->>value_set(@model?translator?iso639Languages), %function is saved, so list will contain all values even after adding or removing one
	AddLanguage->>value(@model?translator?iso639Languages?head?value),
	D->>display(AddLanguage,point(0,CurrentLanguage?bottom_side + GapY)),	
	AL_go *= button(algo,->>(D,onAddLanguage)),
	AL_go->>label('go'),
	AL_go->>height(AddLanguage?height),
	
	DeleteLanguage *= eventTextItem(deleteLanguage),
	DeleteLanguage->>onKey(->>(@pce,fail)),
	DeleteLanguage->>label('Deactivate language:'),
	DeleteLanguage->>length(20),
	DeleteLanguage->>style(combo_box),
	DeleteLanguage->>editable(@on),
	DeleteLanguage->>value_set(@model?translator?allLanguages), %function is saved, so list will contain all values even after adding or removing one
	DeleteLanguage->>value(@model?translator?currentLanguage),
	D->>display(DeleteLanguage,point(0,AddLanguage?bottom_side + GapY)),	
	DL_go *= button(dlgo,->>(D,onDeleteLanguage)),
	DL_go->>label('go'),
	DL_go->>height(DeleteLanguage?height),
	
	RenameLanguage *= eventTextItem(renameLanguage),
	RenameLanguage->>onKey(->>(@pce,fail)),
	RenameLanguage->>label('Rename language:'),
	RenameLanguage->>length(20),
	RenameLanguage->>style(combo_box),
	RenameLanguage->>editable(@on),
	RenameLanguage->>value_set(@model?translator?allLanguages), %function is saved, so list will contain all values even after adding or removing one
	RenameLanguage->>value(@model?translator?currentLanguage),
	%D->>display(RenameLanguage,point(0,DeleteLanguage?bottom_side + GapY)), % No more renaming
	RenamedLanguage *= eventTextItem(renamedLanguage),
	RenamedLanguage->>label('to:'),
	RenamedLanguage->>length(20),
	RenamedLanguage->>editable(@on),
	%D->>display(RenamedLanguage,point(0,RenameLanguage?bottom_side + GapY)), % No more renaming
	RL_go *= button(algo,->>(D,onRenameLanguage)),
	RL_go->>label('go'),
	RL_go->>height(RenamedLanguage?height),
	
	%align labels:		
	LangLabels *= number(0),
	LangLabels->>maximum(CurrentLanguage?label_width),
	LangLabels->>maximum(AddLanguage?label_width),
	LangLabels->>maximum(DeleteLanguage?label_width),
	LangLabels->>maximum(RenameLanguage?label_width),  
	LangLabels->>maximum(RenamedLanguage?label_width), 
	
	CurrentLanguage->>label_width(LangLabels),
	D->>display(CL_go,point(CurrentLanguage?right_side + GapX,CurrentLanguage?top_side)),
	AddLanguage->>label_width(LangLabels),
	D->>display(AL_go,point(CL_go?left_side,AddLanguage?top_side)), %CL_go is more to the right, because CurrentLanguage is a combo
	DeleteLanguage->>label_width(LangLabels),
	D->>display(DL_go,point(CL_go?left_side,DeleteLanguage?top_side)).
	%RenameLanguage->>label_width(LangLabels), % No more renaming
	%RenamedLanguage->>label_width(LangLabels), % No more renaming
	%D->>display(RenamedLanguage,point(CL_go?left_side,RenameLanguage?top_side)),
	%D->>display(RL_go,point(CL_go?left_side,RenamedLanguage?top_side)). % No more renaming
%%

%%
onChangeCurrentLanguage(D):->
	Lang = D?currentLanguage_member<<-selection,
	if
	(
		Lang \== @nil
	)
	then
	(
		@model->>changeRequest(setCurrentLanguage,@model,?(D,hypered,editor),Lang)
	).
%%

%%
onDeleteLanguage(D):->
	Lang = D?deleteLanguage_member<<-selection,
	if
	(
		Lang \== @nil,
		D->>msgBox('When you delete a language all data (names, remarks) in that language are gone. Are you sure you want to delete this language?',confirm,@on)
	)
	then
	(
		@model->>changeRequest(deleteLanguage,@model,?(D,hypered,editor),Lang)
	).
%%
	
%%
onAddLanguage(D):->
	Lang = D?addLanguage_member<<-selection,
	if
	(
		Lang \== @nil
	)
	then
	(
		@model->>changeRequest(addLanguage,@model,?(D,hypered,editor),Lang)
	).
%%

onRenameLanguage(D):->
	LangOud = D?renameLanguage_member<<-value,
	LangNieuw = D?renamedLanguage_member<<-value,
	if
	(
		LangOud \== @nil
	;
		LangNieuw \== @nil
	)
	then
	(
		@model->>changeRequest(changeLanguageName,@model,?(D,hypered,editor),LangOud,LangNieuw)
	).
%%

%%%%% CHANGE REQUESTORS %%%%%%
changeApplied_setCurrentLanguage(_D,
	_CR: changeRequestor):->
	
	%we do nothing (overwriting superclass)
	true.
%%

changeApplied_deleteLanguage(D, CR: changeRequestor):->
	if
	    D?currentLanguage_member?value->>equal(CR?arg1)
	then
	    D?currentLanguage_member->>value(@model?translator?currentLanguage),
	if
	    D?deleteLanguage_member?value->>equal(CR?arg1)
	then
	    D?deleteLanguage_member->>value(''),
	if
	    D?renameLanguage_member?value->>equal(CR?arg1)
	then
	    D?renameLanguage_member->>value('').
%%

%%
changeApplied_changeLanguageName(D,CR: changeRequestor):->
	%just change the name
	if
	    D?currentLanguage_member?value->>equal(CR?arg1)
	then
	    D?currentLanguage_member->>value(CR?arg2),
	if
	    D?deleteLanguage_member?value->>equal(CR?arg1)
	then
	    D?deleteLanguage_member->>value(CR?arg2),
	if
	    D?renameLanguage_member?value->>equal(CR?arg1)
	then
	    D?renameLanguage_member->>value(CR?arg2).



:-pce_end_class.
