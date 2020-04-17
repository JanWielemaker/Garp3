/*
metadataEditor
An editor for showing / editing the metadata @model

Part of Garp3 - see copyright notice
2005 Jelmer Jellema, Spin in het Web, spininhetweb.nl
gp3 0.3.13

This is a complex dialog that just does not work fine with the auto layout features of pce
So, once again, we use display etc to do all the stuff.

*/

:-pce_begin_class(metadataEditor,frame).
variable(currentReportId,name*,both). %saves currently displayed status report id
variable(currentMiscRow,table_row*,both). %currently selected row in the misc page
variable(garpModel, garpModel, both, 'The model associated with this editor').
%%
initialise(F):->
    	get(@model, getModelNameForEditor, 'About this model and Sketch', ModelNameForEditorLabel),
	F->+initialise(ModelNameForEditorLabel),
	F->>icon(@build_icon),
	F->>application(@app),
	F->>kind(toplevel), %just a toplevel window
	F->>can_resize(@off), %currently not resizable due to problems with subdialog members etc
	send(F, slot, garpModel, @model).
		
	%the rest will be done in open because failuse during initialise results in a very certain pce death
%%

%%
open(F, Point: [point], Normalise: normalise = [bool]):->
	%fill the dialog and open it
		
	Holder *= extendedDialog(F?label), %holds the different pages
	Holder->>name(holder),
	Holder->>background(colour(white)),
	F->>append(Holder),
	
	%we spell out all stretching and shrinking to make sure it does what we want
	Holder->>hor_stretch(100),
	Holder->>ver_stretch(100),
	Holder->>hor_shrink(100),
	Holder->>ver_shrink(100),
	
	%page-buttons, we put close in the same row
	
	ButtonRow *= dialog,
	ButtonRow->>name(buttons),
	ButtonRow->>pen(0),
	ButtonRow->>gap(size(0,0)),
	ButtonRow->>border(size(0,0)),
	ButtonRow->>ver_stretch(0),
	ButtonRow->>hor_stretch(100),
	ButtonRow->>ver_shrink(0),
	ButtonRow->>hor_shrink(100),	
	
	%we just append a spacer for height and then all buttons
	
	ButtonTopSpacer *= graphical(width := 1, height := 4), 
	ButtonRow->>append(ButtonTopSpacer),		
	
	BGeneral *= toggleImgButton(bGeneral,->>(F, showPage,general),general,@nil,metadata,'General information'),
	BGeneral->>value(@on),
	BGeneral->>below(ButtonTopSpacer),

        % renamed button to BAbstract, but kept the name remarks, AB 2 nov 2006 
	BAbstract *= toggleImgButton(bAbstract,->>(F, showPage,remarks),remarks,@nil,metadata,'Abstract, model goals, intended audience and general remarks'),
	BAbstract->>group(BGeneral), %same group
	BAbstract->>right(BGeneral),	

        % Add extra horizontal space between related button groups 
        HSWidth is 15,
        get(BGeneral, height, HSHeight), 
        % HSHeight is 42,
        new(HSpacerColour, colour(dark_grey)), % or black?
	% HSpacer1 *= graphical(width := HSWidth, height := HSHeight), 
	HSpacer1 *= box(width := HSWidth, height := HSHeight), 
	HSpacer1->>pen(0), 
        HSpacer1->>fill_pattern(HSpacerColour),
	ButtonRow->>append(HSpacer1, right),
	
	BStatus *= toggleImgButton(bStatus,->>(F, showPage,status),status,@nil,metadata,'Model status and bug reports'),
	BStatus->>group(BGeneral), %same group
	BStatus->>right(HSpacer1),
	% BStatus->>right(BAbstract),

	%other buttons just to the right
	%0.4.6 moved data to second place
	BData *= toggleImgButton(bData,->>(F, showPage,data),data,@nil,metadata,'Model data'),
	BData->>group(BGeneral), %same group
	BData->>right(BStatus),	
	
        % Add extra horizontal space between related button groups 
	% HSpacer2 *= graphical(width := HSWidth, height := HSHeight), 
	HSpacer2 *= box(width := HSWidth, height := HSHeight), 
	HSpacer2->>pen(0), 
        HSpacer2->>fill_pattern(HSpacerColour),
	ButtonRow->>append(HSpacer2, right),

        % Concept Map Sketch Screen
	BConceptMap *= toggleImgButton(bSketchConceptMap,->>(F, showPage,sketchConceptMap),
				       sketchConceptMap,@nil,metadata,'Concept map'),
	BConceptMap->>group(BGeneral), %same group
	BConceptMap->>right(HSpacer2),
	% BConceptMap->>right(BData),


        % Add extra horizontal space between related button groups 
	% HSpacer3 *= graphical(width := HSWidth, height := HSHeight), 
	HSpacer3 *= box(width := HSWidth, height := HSHeight), 
	HSpacer3->>pen(0), 
        HSpacer3->>fill_pattern(HSpacerColour),
	ButtonRow->>append(HSpacer3, right),

        % Structural Model Sketch Screen
	BStructuralModel *= toggleImgButton(bSketchStructuralModel,->>(F, showPage,sketchStructuralModel),
					    sketchStructuralModel,@nil,metadata,'Structural model'),
	BStructuralModel->>group(BGeneral), %same group
	BStructuralModel->>right(HSpacer3),
	% BStructuralModel->>right(BConceptMap),


        % Add extra horizontal space between related button groups 
	% HSpacer4 *= graphical(width := HSWidth, height := HSHeight), 
	HSpacer4 *= box(width := HSWidth, height := HSHeight), 
	HSpacer4->>pen(0), 
        HSpacer4->>fill_pattern(HSpacerColour),
	ButtonRow->>append(HSpacer4, right),

        % Processes Sketch Screen
	BProcesses *= toggleImgButton(bSketchProcesses,->>(F, showPage,sketchProcesses),
					    sketchProcesses,@nil,metadata,'Processes'),
	BProcesses->>group(BGeneral), %same group
	BProcesses->>right(HSpacer4),
	% BProcesses->>right(BStructuralModel),

        % Agents Sketch Screen
	BAgents *= toggleImgButton(bSketchAgents,->>(F, showPage,sketchAgents),
					    sketchAgents,@nil,metadata,'Actions and external influences'),
	BAgents->>group(BGeneral), %same group
	BAgents->>right(BProcesses),

        % Causal Model Sketch Screen
	BCausalModel *= toggleImgButton(bSketchCausalModel,->>(F, showPage,sketchCausalModel),
					    sketchCausalModel,@nil,metadata,'Causal model'),
	BCausalModel->>group(BGeneral), %same group
	BCausalModel->>right(BAgents),

        % Add extra horizontal space between related button groups 
	% HSpacer5 *= graphical(width := HSWidth, height := HSHeight), 
	HSpacer5 *= box(width := HSWidth, height := HSHeight), 
	HSpacer5->>pen(0), 
        HSpacer5->>fill_pattern(HSpacerColour),
	ButtonRow->>append(HSpacer5, right),


        % Scenarios Sketch Screen
	BScenarios *= toggleImgButton(bSketchScenarios,->>(F, showPage,sketchScenarios),
					    sketchScenarios,@nil,metadata,'Scenarios'),
	BScenarios->>group(BGeneral), %same group
	BScenarios->>right(HSpacer5),
	% BScenarios->>right(BCausalModel),

        % BehaviourGraph Sketch Screen
	BBehaviourGraph *= toggleImgButton(bSketchBehaviourGraph,->>(F, showPage,sketchBehaviourGraph),
					    sketchBehaviourGraph,@nil,metadata,'Behaviour graph'),
	BBehaviourGraph->>group(BGeneral), %same group
	BBehaviourGraph->>right(BScenarios),


	ButtonRow->>append(graphical(width := BAbstract?width, height := BAbstract?height),right),
	
	
	%assistance
	
	Assistance *= dialog,
	Assistance->>name(assistance),
	Assistance->>pen(0),
	Assistance->>gap(size(0,0)),
	Assistance->>border(size(0,0)),
	Assistance->>ver_stretch(0),
	Assistance->>hor_stretch(0),
	Assistance->>ver_shrink(0),
	Assistance->>hor_shrink(0),
	Help *= helpButton(help,'Build_AboutThisModel'),

	%we display this one, calculating the space needed above (assuming all buttons have same size)
	
	Assistance->>append(new(AssistanceSpacer,graphical(width:=Help?width, height := ButtonTopSpacer?height + BGeneral?height - Help?height))),
	Help->>below(AssistanceSpacer),
	
	Assistance->>right(ButtonRow),
	Assistance->>above(Holder),

	Holder?tile?root->>border(0),
	

	%spacers, needed because of problems with setting tile borders
	SpaceLeft *= window,
	SpaceLeft->>width(4),
	SpaceLeft->>pen(0),
	SpaceLeft->>background(ButtonRow?background),
	SpaceLeft->>hor_stretch(0),
	SpaceLeft->>ver_stretch(100),
	SpaceLeft->>hor_shrink(0),
	SpaceLeft->>ver_shrink(100),
	SpaceLeft->>left(Holder),
	SpaceLeft?tile?root->>border(0),
	
	SpaceRight *= window,
	SpaceRight->>width(4),
	SpaceRight->>pen(0),
	SpaceRight->>background(ButtonRow?background),
	SpaceRight->>hor_stretch(0),
	SpaceRight->>ver_stretch(100),
	SpaceRight->>hor_shrink(0),
	SpaceRight->>ver_shrink(100),
	SpaceRight->>right(Holder),
	SpaceRight?tile?root->>border(0),
	

	SpaceBelow *= window,
	SpaceBelow->>height(4),
	SpaceBelow->>pen(0),
	SpaceBelow->>background(ButtonRow?background),
	SpaceBelow->>hor_stretch(100),
	SpaceBelow->>ver_stretch(0),
	SpaceBelow->>hor_shrink(100),
	SpaceBelow->>ver_shrink(0),
	SpaceBelow->>below(Holder),
	SpaceBelow?tile?root->>border(0),
	
		
	%dialog_groups
	
	FixedWidth = 750, %we send a fixed with, can be used for lay out etc
	%(we do not use auto layout stuff)
	
	General = F<<-createGeneral(FixedWidth), 
	General->>attribute(isPage,@on),
	Holder->>display(General,point(Holder?gapX,Holder?gapY)),

	Abstract = F<<-createAbstract(FixedWidth),
	Abstract->>attribute(isPage,@on),
	Holder->>display(Abstract,point(Holder?gapX, Holder?gapY)),
	Abstract->>displayed(@off),

	Status = F<<-createStatus(FixedWidth),
	Status->>attribute(isPage,@on),	
	Holder->>display(Status,point(Holder?gapX, Holder?gapY)), %same spot but...
	Status->>displayed(@off),
	
	ModelData = F<<-createModelData(FixedWidth),
	ModelData->>attribute(isPage,@on),
	Holder->>display(ModelData,point(Holder?gapX, Holder?gapY)),
	ModelData->>displayed(@off),
		
	ConceptMap = F<<-createSketchRemarks(sketchConceptMap, 'Concept Map', FixedWidth),
	ConceptMap->>attribute(isPage,@on),
	Holder->>display(ConceptMap,point(Holder?gapX, Holder?gapY)),
	ConceptMap->>displayed(@off),

	StructuralModel = F<<-createSketchRemarks(sketchStructuralModel, 'Structural Model', FixedWidth),
	StructuralModel->>attribute(isPage,@on),
	Holder->>display(StructuralModel,point(Holder?gapX, Holder?gapY)),
	StructuralModel->>displayed(@off),

	Processes = F<<-createSketchRemarks(sketchProcesses, 'Processes', FixedWidth),
	Processes->>attribute(isPage,@on),
	Holder->>display(Processes,point(Holder?gapX, Holder?gapY)),
	Processes->>displayed(@off),
 
	Agents = F<<-createSketchRemarks(sketchAgents, 'Actions and External Influences', FixedWidth),
	Agents->>attribute(isPage,@on),
	Holder->>display(Agents,point(Holder?gapX, Holder?gapY)),
	Agents->>displayed(@off),

	CausalModel = F<<-createSketchRemarks(sketchCausalModel, 'Causal Model', FixedWidth),
	CausalModel->>attribute(isPage,@on),
	Holder->>display(CausalModel,point(Holder?gapX, Holder?gapY)),
	CausalModel->>displayed(@off),

	Scenarios = F<<-createSketchRemarks(sketchScenarios, 'Scenarios', FixedWidth),
	Scenarios->>attribute(isPage,@on),
	Holder->>display(Scenarios,point(Holder?gapX, Holder?gapY)),
	Scenarios->>displayed(@off),

	BehaviourGraph = F<<-createSketchRemarks(sketchBehaviourGraph, 'Behaviour Graph', FixedWidth),
	BehaviourGraph->>attribute(isPage,@on),
	Holder->>display(BehaviourGraph,point(Holder?gapX, Holder?gapY)),
	BehaviourGraph->>displayed(@off),
	
	%make sure the size fits
	MaxSize *= size(0,0),
	
	Holder->>for_all(@default,
		and(
			if(@arg1?width > MaxSize?width, ->>(MaxSize,width,@arg1?width)),
			if(@arg1?height > MaxSize?height, ->>(MaxSize,height,@arg1?height))
		)),
	Holder->>for_all(@default,->>(@arg1,size,MaxSize)),	
	
	%we display close and save in a dialog, below
	GlobalButtons *= extendedDialog(globalButtons),
	GlobalButtons->>pen(0),
	
	BtnSave *= imgButton(save,->>(F,saveValues), tt := 'Close this editor, saving changes in the model (status reports are saved seperately)'),
	BtnClose *= imgButton(close,->>(F,destroy), tt:='Close this editor without saving changes'),
	GlobalButtons->>display(BtnSave,point((FixedWidth - BtnSave?width - GlobalButtons?gapX - BtnClose?width) / 2,GlobalButtons?gapY)),
	GlobalButtons->>display(BtnClose,point(BtnSave?right_side + GlobalButtons?gapX,BtnSave?top_side)),
	GlobalButtons->>below(Holder),
	
	F->>loadValues,	

	F->+open(Point,Normalise).
%%

%%
createGeneral(_F,FixedWidth: int, General: dialog_group):<-
	%create the general dialog group

	%we use display and points etc to fit everything
			
	General *= dialog_group(general,group),
	General->>width(FixedWidth),
	GapY = General?gap<<-height,
	%GapX = General?gap<<-width,
	
	Placement *= point(0,0),
	MaxLabelWidth *= number(0),
	%we can just add all other fields and add an attribute to recognize them at once:
	forall(member(Fieldname,
	[title,author,contributors,contact_email,keywords,domain,model_version,known_model_limitations,bibliographic_citation,license]), % Removed language from this list, since we now have multi-language support 
			(
				General->>display(new(Field,text_item(Fieldname)),Placement),
				Placement->>set(y := Field?bottom_side + GapY), %for the next one
				%we also want to find the most right aligned field (because of label), to get it all right
				%in the second pass (yes, we are rewriting pce code here, but anyway)
				MaxLabelWidth->>maximum(Field?label_width), %save the max one
				Field->>attribute(dataField,@on)
			)
	),
	%second pass: we now know the max label width, so we move all fields to the right if their label width is less
	%this way, the fields will be aligned
	
	General->>for_all(@default,
		if(
			?(@arg1,attribute,dataField), %this is a datafield
			and(
				->>(@arg1,set,x := @arg1?left_side + (MaxLabelWidth - @arg1?label_width)),
				%and set right side as well 
				->>(@arg1,right_side,FixedWidth)
			)
		)
	),		
	General->>height(Placement?y). %this has to fit
%%

%%
createStatus(F, FixedWidth: int, Status: dialog_group):<-
	%create the status dialog group
	%same diy method as in createGeneral
	
	Status *= dialog_group(status,group),
	Status->>width(FixedWidth),
	GapY = Status?gap<<-height,
	GapX = Status?gap<<-width,
	
	%this list is placed spacely, because we show a box behind it
	Placement *= point(0,GapY), %extra Y-space
	MaxLabelWidth *= number(0),
	
	%list
	Status->>display(new(List,eventTextItem(report_ID)),Placement),
	%we make it an eventTextItem so we can have it editable but stop the user from typing (just choosing)
	List->>onKey(->>(@pce,fail)), %simply fail
	List->>style(combo_box),
	List->>editable(@on),
	List->>message(->>(F,onChangeReportId)),
	
	MaxLabelWidth->>maximum(List?label_width), %save it for alignment later on
	Placement->>set(y := List?bottom_side + GapY),
	
	%we place the 4 buttons centered below the list, but because we have to align the text fields later on
	%we cannot do that now, so we just place them to the left

	Status->>display(new(BtnNew,imgButton(new,->>(F,newReportId),new_report,@nil,metadata,'Start a new status report after saving the current one in the model')), Placement),
	Status->>display(new(BtnSave,imgButton(save,->>(F,onBtnSave),save_report,@nil,metadata,'Save current status report in the model')), point(BtnNew?right_side + GapX, Placement?y)),	
	Status->>display(new(BtnUndo,imgButton(undo,->>(F,updateStatusReport),cancel_report,@nil,metadata,'Cancel all changes to this status report and show the unchanged values')), point(BtnSave?right_side + GapX, Placement?y)),	
	Status->>display(new(BtnDelete,imgButton(delete,->>(F,deleteReportId),delete_report,@nil,metadata,'Delete this status report')), point(BtnUndo?right_side + GapX, Placement?y)),	

	%Display a box behind the stuff above
	Box *= box,
	Box->>fill_pattern(?(new(colour),convert,'#c3d1d4')),
	Box->>pen(0),
	Status->>display(Box,point(0,0)), %starts on the top
	Box->>bottom_side(BtnDelete?bottom_side + GapY), %some space
	Box->>right_side(FixedWidth),
	Box->>hide, %behind other stuff
	
	Placement->>set(0,Box?bottom_side + GapY), %for the rest
	
	%gp3 0.4.9 added name field
	forall(member(Fieldname,
				[name,priority,date,reported_by,status,to_do,fixes]),
			(
				Status->>display(new(Field,text_item(Fieldname)),Placement),
				Field->>attribute(dataField,@on),
				Placement->>set(y := Field?bottom_side + GapY), %for the next one
				MaxLabelWidth->>maximum(Field?label_width) %save the max one
			)
	),
	
	%all text items are done now
	%(description will folow below)
	%align them:
	
	Status->>for_all(@default,
		if(
			?(@arg1,attribute,dataField), %this is a datafield
			and(
				->>(@arg1,set,x := @arg1?left_side + (MaxLabelWidth - @arg1?label_width)),
				%and set right side as well 
				->>(@arg1,right_side,FixedWidth)
			)
		)
	),			
	
	%list does not have the dataField attribute, so we have to do this seperately (and set right_site differently)
	List->>set(x:= List?left_side + (MaxLabelWidth - List?label_width)),
	List->>right_side(FixedWidth - 10), %we want to be able to see the box..
	
	%now the list is aligned, we can align the buttons, centered below the list (without the label):
	BtnNew->>set( x:= List?left_side + List?label_width + (List?right_side - (List?left_side + List?label_width) - (BtnDelete?right_side - BtnNew?left_side)) / 2),
	BtnSave->>set( x:= BtnNew?right_side + GapX),
	BtnUndo->>set( x:= BtnSave?right_side + GapX),
	BtnDelete->>set( x:= BtnUndo?right_side + GapX),
	
	%description
	
	Description *= editor(height:=7),
	Description->>name(description),
	Description->>label('Description:'),
	Description->>fill_mode(@on),
	Description->>font(List?font),
	Status->>display(Description,Placement),
	Description->>right_side(FixedWidth),
	Description->>attribute(dataField,@on),
	
	Placement->>set( y:= Description?bottom_side + GapY), %for future extensions, and for the next line
	
	Status->>height(Placement?y).
%%

%%
createMisc(F,FixedWidth: int, FixedHeight: int, Misc: dialog_group):<-
	%create the misc dialog group

	%we use display and points etc to fit everything
			
	Misc *= dialog_group(misc,group),
	Misc->>width(FixedWidth),
	GapY = Misc?gap<<-height,
	GapX = Misc?gap<<-width,
	%buttongroup
	
	Misc->>display(new(LBExplanation,label(selection:='Use these buttons to create and manipulate custom fields:',font:=italic)),point(GapX,GapY)),
	LBExplanation->>width(0), %make it fit
	LBExplanation->>set(x:= (FixedWidth - LBExplanation?width) / 2),
	
	BtnNew *= imgButton(new,->>(F,newMiscRow),new_misc_row,@nil,metadata,'Create a new custom field below the currently selected one'),
	BtnUp *= imgButton(up,->>(F,moveMiscRow,-1),misc_row_up,@nil,metadata,'Move the selected custom field up'),
	BtnDown *= imgButton(down,->>(F,moveMiscRow,1),misc_row_down,@nil,metadata,'Move the selected custom field down'),
	BtnDelete *= imgButton(delete,->>(F,deleteMiscRow),delete_misc_row,@nil,metadata,'Delete the selected custom field'),
	Misc->>display(BtnNew, point((FixedWidth - (BtnNew?width + BtnUp?width + BtnDown?width + BtnDelete?width + 3 * GapX)) / 2,LBExplanation?bottom_side + GapY)),
	Misc->>display(BtnUp, point(BtnNew?right_side + GapX,BtnNew?top_side)),
	Misc->>display(BtnDown, point(BtnUp?right_side + GapX,BtnNew?top_side)),
	Misc->>display(BtnDelete, point(BtnDown?right_side + GapX,BtnNew?top_side)),
	
	Box *= box,
	Box->>fill_pattern(?(new(colour),convert,'#c3d1d4')),
	Box->>pen(0),
	Misc->>display(Box,point(0,0)), %starts on the top
	Box->>right_side(FixedWidth),
	Box->>bottom_side(BtnNew?bottom_side + GapY), %some space
	Box->>hide,
	
	%we make a table
	Grid *= picture,
	Grid->>name(grid),
	Misc->>attribute(grid,Grid), %have to save it like this, because of window_decorator becoming the <-member
	Grid->>pen(0),
	Grid->>width(FixedWidth - 20),
	Grid->>layout_manager(new(Table,table)),
	Table->>cell_padding(size(2,4)),
	Table->>cell_spacing(0),
	%save the widths we chose
	if
		SB = Grid<<-vertical_scrollbar
	then
		ScrollbarWidth = SB<<-width
	else
		ScrollbarWidth = 0,
		
	Grid->>attribute(nameFieldWidth, (Grid?width - ScrollbarWidth) / 3 - 2 * Table?cell_padding?width),
	Grid->>attribute(valueFieldWidth, ((Grid?width - ScrollbarWidth) * 2/3 - 2 * Table?cell_padding?width)),
	
	Misc->>display(new(LBName,label(selection := 'name', font := bold)),point(10 + Table?cell_padding?width,Box?bottom_side + GapY)),
	LBName->>width(0), %make fit
	Misc->>display(new(LBVal,label(selection := 'value', font := bold)),point(10 + Grid?nameFieldWidth + 3 * Table?cell_padding?width,LBName?top_side)),
	LBVal->>width(0),
	
	Misc->>display(Grid,point(10,LBVal?bottom_side)),
	Grid->>height(FixedHeight - LBVal?bottom_side - 10).
%%

%%
createModelData(_F,FixedWidth: int, ModelData: dialog_group):<-
	%create the model data dialog group

	%we use display and points etc to fit everything
	%all fields in the model data group are read only
	%see createGeneral for logic behind this code
			
	ModelData *= dialog_group(data,group),
	ModelData->>width(FixedWidth),
	GapY = ModelData?gap<<-height,
	%GapX = General?gap<<-width,
	
	Placement *= point(0,0),
	MaxLabelWidth *= number(0),
	
	forall(member(Fieldname,
				[creation_date,created_in,creation_definition_version, last_change, current_program, current_definition_version]),
			(
				ModelData->>display(new(Field,eventTextItem(Fieldname)),Placement),
				Placement->>set(y := Field?bottom_side + GapY), %for the next one
				%we also want to find the most right aligned field (because of label), to get it all right
				%in the second pass (yes, we are rewriting pce code here, but anyway)
				MaxLabelWidth->>maximum(Field?label_width), %save the max one
				Field->>attribute(dataField,@on),
				Field->>onKey(->>(@pce,fail)) %looks better than disabled
			)
	),
	%second pass: we now know the max label width, so we move all fields to the right if their label width is less
	%this way, the fields will be aligned
	
	ModelData->>for_all(@default,
		if(
			?(@arg1,attribute,dataField), %this is a datafield
			and(
				->>(@arg1,set,x := @arg1?left_side + (MaxLabelWidth - @arg1?label_width)),
				%and set right side as well 
				->>(@arg1,right_side,FixedWidth)
			)
		)
	),		
	ModelData->>height(Placement?y), %this has to fit
	%we add the values here, because this is a one time thing
	%
	%these values *might* be saved by the model in the metadata variable under the key modelData
	
	catch(MD = @model<<-metaData,_,fail), %make sure model exists etc
	unless
		Data = MD<<-member(modelData)
	do
	(
		Data *= hash_table,
		MD->>append(modelData,Data)
	),
	
	ModelData?creation_date_member->>selection(when(?(Data,member,creationTime), ?(Data,member,creationTime)?print_name,'?')),
	ModelData?created_in_member->>selection(when(?(Data,member,creatorProgram), ?(Data,member,creatorProgram),'?')),
	ModelData?creation_definition_version_member->>selection(when(?(Data,member,creationDefinition), ?(Data,member,creationDefinition),'?')),
	%lastChangetime is different, it is relevant data so it is a variable
	ModelData?last_change_member->>selection(when(@model?lastChangeTime == @nil, '?', @model?lastChangeTime?print_name)),
	%information about current state, we get them elsewhere
	ModelData?current_program_member->>selection(string('%s %s',@app?name,@app?version)),
	ModelData?current_definition_version_member->>selection(@model?modelDefinitionVersion). %current version is allways saved in the model after conversion.
	
%%

%%
createAbstract(F,FixedWidth: int, AbstractGroup: dialog_group):<-
	%create the remarks dialog group for Abstract, Model goals, Intended audience, and General remarks

	%we use display and points etc to fit everything
			
	AbstractGroup *= dialog_group(remarks,group),
	AbstractGroup->>width(FixedWidth),

        % create a TabTool, with 4 tabs: Abstract, Intended audience, Model goals, and General remarks

        GapX = 0, 
        GapY = 10, 
        TabTool = F<<-add_tabtool, 
        AbstractGroup->>display(TabTool, point(GapX,GapY )),
        % AbstractGroup->>display(TabTool, point(GapX,Remarks?bottom_side + GapY )),
	
	AbstractGroup->>height(TabTool?bottom_side). %this has to fit
%%


%
add_tabtool(D, TabTool):<-
 	new(TabTool, 
		 tab_stack(
			  new(AbstractTab, tab('Abstract')),
			  new(IntendedAudienceTab, tab('Intended audience')),
			  new(ModelGoalsTab, tab('Model goals')),
			  new(GeneralRemarksTab, tab('General remarks'))
		 )
	),
 	TabTool->>name(tabtool), 

        % init tabs
        D->>init_tab(AbstractTab, abstract), 
        D->>init_tab(IntendedAudienceTab, intended_audience), 
        D->>init_tab(ModelGoalsTab, model_goals), 
        D->>init_tab(GeneralRemarksTab, general_remarks). 
%%


%
init_tab(D, Tab, TypeID):->
        GapX is 10, 
        GapY is 10, 
	% GapX = D?gap<<-width,
	% GapY = D?gap<<-height,
	Placement *= point(GapX,GapY), 
    
	Editor *= editor(width:=100, height:=23),
        swritef(EditorName, '%w_editor',[TypeID]),
	Editor->>name(EditorName),
	Editor->>label(EditorName),
	Editor->>show_label(@off),
	Editor->>fill_mode(@on),
	Editor->>font(normal),
	% Editor->>font(D?abstract_member?font),
	% Editor->>right_side(FixedWidth),
	Editor->>attribute(dataField,@on),	
	Tab->>display(Editor,Placement),
	Placement->>set( y:= Editor?bottom_side + GapY), %for future extensions, and for the next line	
	Tab->>height(Placement?y),
        swritef(TabEditorID, '%dTabEditor',[TypeID]), 
	D->>hyper(Editor, TabEditorID).
%%



%
clear_tab(D, TypeID):->
        swritef(TabEditorID, '%dTabEditor',[TypeID]), 
	get(D, hypered, TabEditorID, Editor), 
        send(Editor, clear).
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


%%
createSketchRemarks(_F, SketchID: name, LabelID: name, FixedWidth: int, SketchGroup: dialog_group):<-
	%create a dialog group named SketchID

	%we use display and points etc to fit everything
			
	SketchGroup *= dialog_group(SketchID,group),
	SketchGroup->>width(FixedWidth),

        % this has or will become obsolete
	Sketch *= editor(height := 12),
	Sketch->>name(SketchID),
        swritef(LabelStr, 'Remarks about the %w sketch:', [LabelID]), 
	Sketch->>label(LabelStr),
	Sketch->>fill_mode(@on),
	Sketch->>font(normal),
	Sketch->>show_label(@on),
        % do not show the editor - AB, oct 2006
	% SketchGroup->>display(Sketch,point(0,0)),
	Sketch->>right_side(FixedWidth),
	Sketch->>attribute(dataField,@on),
	SketchGroup->>height(Sketch?bottom_side). %this has to fit
%%


%%
loadValues(F):->
	%load the values from the metaData in the model

	catch(MD = @model<<-metaData,_,fail), %make sure model exists etc

	%general
	Holder = F<<-member(holder),
	GT = Holder<<-member(general),
	unless
		General = MD<<-member(general)
	do
	(
		General *= hash_table, %just an empty one, so all values will be empty
		MD->>append(general,General)
	),
	GT->>for_some(@default,
		if(
			?(@arg1,attribute,dataField),
			if(
				assign(new(Value,var),?(General,member,@arg1?name)),
				->>(F,setContents,@arg1,Value), %we have to use a value because different classes use different calls to set contents....
				->>(F,setContents,@arg1,'')
			)
		)
	),
	
	%status for reuse in a different call:
	F->>reloadStatus, %does the rest       
	
        % Abstract, Model goals, Intended audience, General Remarks

        swritef(AbstractTabEditorID, '%dTabEditor',[abstract]), 
	get(F, hypered, AbstractTabEditorID, AbstractEditor),

        swritef(ModelGoalsTabEditorID, '%dTabEditor',[model_goals]), 
	get(F, hypered, ModelGoalsTabEditorID, ModelGoalsEditor),

        swritef(IntendedAudienceTabEditorID, '%dTabEditor',[intended_audience]), 
	get(F, hypered, IntendedAudienceTabEditorID, IntendedAudienceEditor),

        swritef(GeneralRemarksTabEditorID, '%dTabEditor',[general_remarks]), 
	get(F, hypered, GeneralRemarksTabEditorID, GeneralRemarksEditor),


        % note: the remarks field in the model metadata 
        % contains Abstract, Model Goals, Intended Audience, and General remarks
	unless
		Abstract = MD<<-member(remarks)
	do
	(
		Abstract *= hash_table,
		MD->>append(remarks,Abstract)
	),
	AbstractEditor->>contents(when(?(Abstract,member,abstract),?(Abstract,member,abstract),'')),
	ModelGoalsEditor->>contents(when(?(Abstract,member,model_goals),?(Abstract,member,model_goals),'')),
	IntendedAudienceEditor->>contents(when(?(Abstract,member,intended_audience),?(Abstract,member,intended_audience),'')),
	GeneralRemarksEditor->>contents(when(?(Abstract,member,general_remarks),?(Abstract,member,general_remarks),'')).




%%
saveValues(F):->
	%save the values from the dialogs in the model, and close the dialog
	%check for changes
	
	catch(MD = @model<<-metaData,_,fail), %make sure model exists etc
	Holder = F<<-member(holder),
	
	Changed *= var(value := @off),
	
	%general:
	GT = Holder<<-member(general),
	General = MD<<-member(general),
	%loop over all fields
	GT->>for_some(@default,
		if(
			and(
				?(@arg1,attribute,dataField), %we tagged this one as a data field
				assign(new(Content,var),?(F,getContents,@arg1)), %use a helper to get contents for different classes
				not(->>(?(General,member,@arg1?name),equal,Content)) %can fail on missing key as well
			),
			and(
				assign(Changed, @on, global),
				->>(General,append,@arg1?name,Content)
			)
		)
	),
	
	%status reports are done seperately, we just save the current one:
	F->>saveCurrentReport,

	% Abstract etc.
	% 
        % note: the remarks field in the model metadata contains four things:
	% abstract, model_goals, intended_audience, general_remarks
        % 
        swritef(AbstractTabEditorID, '%dTabEditor',[abstract]), 
	get(F, hypered, AbstractTabEditorID, AbstractEditor),

        swritef(ModelGoalsTabEditorID, '%dTabEditor',[model_goals]), 
	get(F, hypered, ModelGoalsTabEditorID, ModelGoalsEditor),

        swritef(IntendedAudienceTabEditorID, '%dTabEditor',[intended_audience]), 
	get(F, hypered, IntendedAudienceTabEditorID, IntendedAudienceEditor),

        swritef(GeneralRemarksTabEditorID, '%dTabEditor',[general_remarks]), 
	get(F, hypered, GeneralRemarksTabEditorID, GeneralRemarksEditor),

	Abstract = MD<<-member(remarks),

	unless
	(
		?(Abstract,member,abstract)->>equal(AbstractEditor?contents),
		?(Abstract,member,model_goals)->>equal(ModelGoalsEditor?contents),
		?(Abstract,member,intended_audience)->>equal(IntendedAudienceEditor?contents),
		?(Abstract,member,general_remarks)->>equal(GeneralRemarksEditor?contents)
	)
	do
	(
		%changed!
		Changed->>assign(@on,global),
		Abstract->>append(abstract, AbstractEditor?contents),
		Abstract->>append(model_goals, ModelGoalsEditor?contents),
		Abstract->>append(intended_audience, IntendedAudienceEditor?contents),
		Abstract->>append(general_remarks, GeneralRemarksEditor?contents)
	),

	%changed?
	if
		@on = Changed<<-'_value'
	then
		@app->>setModelChanged, %not conceptual	
	F->>destroy.
%%	
	
	

%%
reloadStatus(F):->
	%status contains report ids for keys and a hash_table for each report
	%we just load the id's into the list and tell frame to update it
	
	catch(MD = @model<<-metaData,_,fail), %make sure model exists etc

	unless
		Status = MD<<-member(status)
	do
	(
		Status *= hash_table,
		MD->>append(status,Status)
	),
	Holder = F<<-member(holder),
	ST = Holder<<-member(status),
	
	List = ST<<-report_ID_member,
	F->>fillReportIDs(List,Status),

	unless
		List?value_set->>empty
	do
	(
		First = List?value_set<<-head,
		List->>selection(First),
		F->>currentReportId(First?split?head) %only part up till space
	)
	else
	(
		ST?report_ID_member->>selection('<new>'),
		F->>currentReportId(@nil) %meaning: new
	),
	F->>updateStatusReport. %new or existing (may need to clear currently filled fields)
	
%%

%%
fillReportIDs(_F,List: text_item, Data: hash_table):->
	%fill the combo with report id's, adding some data to the keys to make it more informative
	
	Items *= chain,
	Name *= var, %added in gp3 0.4.9
	Reporter *= var,
	Status *= var,
	Data?keys->>for_all(
		and(
			if(
				not(assign(Name,?(?(Data,member,@arg1),member,name))),
				assign(Name,'?')
			),

			if(
				not(assign(Reporter,?(?(Data,member,@arg1),member,reported_by))),
				assign(Reporter,'?')
			),
			if(
				not(assign(Status,?(?(Data,member,@arg1),member,status))),
				assign(Status,'?')
			),			
			->>(Items,append,create(string,'%s Name: %s By: %s Status: %s',@arg1,Name, Reporter,Status))
		)
	),
	Items->>sort(?(@arg2?print_name,compare,@arg1?print_name)), %reverse sort
	List->>value_set(Items).
%%


%%
showPage(F, PageName: name):->
	%show the given page, hide all others
        % special case for sketch 
	PageName = sketch,!,
	Holder = F<<-member(holder),
	Holder->>for_all(@default,
		if(
			?(@arg1,attribute,isPage),
			->>(@arg1,displayed,@off))),
	?(Holder,member,PageName)->>displayed(@on),    
        % open sketchbook sketch main tabs

        if 
            % sketchbook already exists?
            (
	    get(@model, hypered, sketchbook, SB)
	     )
	then
            send(SB, expose)
        else
           (
            % create new sketchbook
            get(@model, hypered, conceptMapMF, MF1),
            new(SB, sketchMainTabs(MF1))
	   ).


%%
showPage(F, PageName: name):->
	%show the given page, hide all others
        % special case for sketch pages
	member(PageName, [sketchConceptMap, 
			  sketchStructuralModel, 
			  sketchCausalModel, 
			  sketchProcesses, 
			  sketchAgents, 
			  sketchScenarios, 
			  sketchBehaviourGraph]),!,
	Holder = F<<-member(holder),
	Holder->>for_all(@default,
		if(
			?(@arg1,attribute,isPage),
			->>(@arg1,displayed,@off))),
	?(Holder,member,PageName)->>displayed(@on),    
        % open sketchbook sketch main tabs

        if 
            % sketch editor (SE) already exists?
            (
	    get(@model, hypered, PageName, SE)
	     )
	then
            send(SE, expose)
        else
           (
            F->>openSketchEditor(PageName)
	   ).

%%
showPage(F,PageName: name):->
	%show the given page, hide all others
	Holder = F<<-member(holder),
	Holder->>for_all(@default,
		if(
			?(@arg1,attribute,isPage),
			->>(@arg1,displayed,@off))),
	?(Holder,member,PageName)->>displayed(@on).
%%


%
openSketchEditor(_F, SketchID: name):->
        SketchID = sketchConceptMap,!,
        get(@model, hypered, conceptMapMF, MF1),                 
        new(SCM, concept_map_editor(MF1)), 
        new(_PartOfHyper, partof_hyper(@model, SCM, SketchID)),
        send(SCM, open).

openSketchEditor(_F, SketchID: name):->    
        SketchID = sketchStructuralModel,!,
        get(@model, hypered, structureMF, MF1),                 
        new(SCM, structure_editor(MF1)), 
        new(_PartOfHyper, partof_hyper(@model, SCM, SketchID)),
        send(SCM, open).

openSketchEditor(_F, SketchID: name):->
        SketchID = sketchCausalModel,!,
        get(@model, hypered, causalModelMF, MF1),                 
        new(SCM, causal_model_editor(MF1)), 
        new(_PartOfHyper, partof_hyper(@model, SCM, SketchID)),
        send(SCM, open).

openSketchEditor(_F, SketchID: name):->
        SketchID = sketchBehaviourGraph,!,
        get(@model, hypered, stgraphMF, MF1),                 
        new(SCM, stgraphEditor(MF1)), 
        new(_PartOfHyper, partof_hyper(@model, SCM, SketchID)),
        send(SCM, open).

openSketchEditor(_F, SketchID: name):->
        SketchID = sketchProcesses,!,
        % get(@model, hypered, processesSketch, PSK),                 
        new(SCM, processesEditor), 
        % new(SCM, processesEditor(PSK)), 
        new(_PartOfHyper, partof_hyper(@model, SCM, SketchID)),
        send(SCM, open).

openSketchEditor(_F, SketchID: name):->
        SketchID = sketchAgents,!,
        % get(@model, hypered, externalInfluencesSketch, _EXSK),                 
        new(SCM, agentsEditor), 
        new(_PartOfHyper, partof_hyper(@model, SCM, SketchID)),
        send(SCM, open).

openSketchEditor(_F, SketchID: name):->
        SketchID = sketchScenarios,!,
        % get(@model, hypered, scenariosSketch, _SSK),                 
        new(SCM, scenariosEditor), 
        new(_PartOfHyper, partof_hyper(@model, SCM, SketchID)),
        send(SCM, open).
%%



%%
updateStatusReport(F):->
	%request to update the status report
	%we get the report id from our member, not from the list
	
	catch(MD = @model<<-metaData,_,fail), %make sure model exists etc
	Status = MD<<-member(status), %if needed, this one was created in loadValues above

	Holder = F<<-member(holder),
	ST = Holder<<-member(status),
	
	unless
		Report = Status<<-member(F?currentReportId)
	do
		Report *= hash_table, %just an empty one..
	%fill all matching fields
	ST->>for_some(@default,
		if(
			?(@arg1,attribute,dataField),
			if(
				assign(new(Value,var),?(Report,member,@arg1?name)),
				->>(F,setContents,@arg1,Value),
				->>(F,setContents,@arg1,'')
			)
		)
	),
	%special
	if
		@nil = F<<-currentReportId
	then
	(
		ST?date_member->>selection(new(date)?string)
	).
%%

%%
newReportId(F):->
	%used hit the button for a new report id
	%we have to save the old one
	
	F->>saveCurrentReport,
	Holder = F<<-member(holder),
	ST = Holder<<-member(status),
	ST?report_ID_member->>selection('<new>'),
	F->>currentReportId(@nil), %new one
	F->>updateStatusReport.
%%

%%
onChangeReportId(F):->
	%used chose a different report id from the list
	F->>saveCurrentReport,
	%get the selection
	Holder = F<<-member(holder),
	ST = Holder<<-member(status),
	F->>currentReportId(ST?report_ID_member?selection?split?head), %only part up till space	
	F->>updateStatusReport.
%%

%%
onBtnSave(F):->
	%save button: save the current repport and display the right id
	F->>saveCurrentReport,
	Holder = F<<-member(holder),
	ST = Holder<<-member(status),
	if
		@nil = F<<-currentReportId
	then
		ST?report_ID_member->>selection('<new>')
	else
		ST?report_ID_member->>selection(F?currentReportId). %no reporter or status, but that does not matter


%%
saveCurrentReport(F):->
	%save the current report
	
	catch(MD = @model<<-metaData,_,fail), %make sure model exists etc
	Status = MD<<-member(status), %should be there (see loadValues)

	%do we allready have an ID?
	Holder = F<<-member(holder),
	ST = Holder<<-member(status),
	
	if
		@nil = F<<-currentReportId
	then
	(
		%this bug has no name yet, we will create one
		SaveID = F<<-newReportID(Status),
		NewReport = @on,
		CurrentData *= hash_table
	)
	else
	(
		NewReport = @off,
		unless
			CurrentData = Status<<-member(F?currentReportId)
		do
			CurrentData *= hash_table,
		SaveID = F<<-currentReportId
	),

	Changed *= var(value := @off),
	
	%we have to save if:
	%the report allready existed and there is a changed value
	%or: the report did not exist and there is a non-empty field
	ST->>for_some(@default,
		if(
			and(
				?(@arg1,attribute,dataField), %we tagged this one as a data field
				assign(new(Content,var),?(F,getContents,@arg1)), %again, use a helper to get contents for different classes
				or(
					and(
						NewReport == @off,
						not(->>(?(CurrentData,member,@arg1?name),equal,Content)) %can fail on missing key as well
					),
					and(
						NewReport == @on,
						@arg1?name \== date, %we skip date here because it does not count as changed
						not(->>(Content,equal,''))
					)
				)
			),
			and(
				assign(Changed,@on,global),
				->>(CurrentData,append,@arg1?name,Content)
			)
		)
	),
	
	%something changed?
	if
		@on = Changed<<-'_value'
	then
	(
		%something changed. Above we skipped the special field date, because a change in date in a new report
		%does not count as a change. But we do have to save its value
		CurrentData->>append(date,ST?date_member?selection),
		Status->>append(SaveID,CurrentData),
		@app->>setModelChanged, %not conceptual
		F->>currentReportId(SaveID),
		%but we do not set it as the selection of the list (might just be chosen a different one)
		F->>fillReportIDs(ST?report_ID_member,Status)
	).
%%

%%
newReportID(F,Status: hash_table, Counter: [number],ID: name):<-
	%create a new report id, using current date and a counter
	default(Counter,new(number(1)),C),
	Now *= date,
	NewID *= string('%d%02d%02d%02d',Now?year,Now?month,Now?day,C),
	if
		Status<<-member(NewID?value) %allways as a name
	then
	(
		C->>plus(1),
		ID = F<<-newReportID(Status,C) %try again with higher counter, not very efficient but well
	)
	else
		ID = NewID<<-value.
%%

%%
deleteReportId(F):->
	%delete the current report id (no warning)

	if
		@nil = F<<-currentReportId
	then
		F->>newReportId %the same as just creating a new one
	else
	(
		catch(MD = @model<<-metaData,_,fail), %make sure model exists etc
		Status = MD<<-member(status), %should be there (see loadValues)
		ignore(Status->>delete(F?currentReportId)),
		F->>reloadStatus
	).
%%

%%
setContents(_F, Field: graphical, Content: char_array):->
	%set the content for a field, text_item or something like that
	%depending on the class, we have to use different calls...
	
	Class = Field<<-class_name,
	pl_content_method(Class,Method),
	send(Field,Method,Content).
%
getContents(_F, Field: graphical, Content: char_array):<-
	%get the content for a field. See setContent

	Class = Field<<-class_name,
	pl_content_method(Class,Method),
	get(Field,Method,Content).
%
%default is selection, so we do diffences first
pl_content_method(editor,contents):-!.
pl_content_method(_,selection).
%%


/*
%%
selectedMiscRow(F,RowField: eventTextItem):->
	%a field in the misc page was selected, colour the row
	
	%text_items deligates to its table_cell when its there
	Row = RowField?table<<-row(RowField?row),
	%is there a currently selected row?
	Old = F<<-currentMiscRow,
	unless
		Old = @nil
	do
		Old->>background(Row?background), %whatever is the default
	%get the colour
	C = colour<<-convert('#c3d1d4'),
	Row->>background(C),
	F->>currentMiscRow(Row).
%%

%%
newMiscRow(F, KeyText: [char_array], ValueText: [char_array], FixedRowIndex: [int]):->
	%create a new row in the misc page and make it current
	%optionally fill it with information and or choose the row index
	
	Holder = F<<-member(holder),
	Misc = Holder<<-member(misc),
	Grid = Misc<<-grid, %get it through attribute, member does not work because of window_decorator
	Table = Grid<<-layout_manager,
	
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
	Key->>value_font(bold),
	Key->>value_width(Grid?nameFieldWidth),
	Value->>show_label(@off),
	Value->>value_width(Grid?valueFieldWidth),
	OnFocus = ->>(F,selectedMiscRow,@arg1),
	Key->>onFocus(OnFocus),
	Value->>onFocus(OnFocus),
	
	%insert the row
	if
		FixedRowIndex = @default
	then
	(
		if
			@nil = F<<-currentMiscRow
		then
			NewRowIndex = Table?rows?high_index + 1 %not evaluated yet (no problem)
		else
			NewRowIndex = F?currentMiscRow?index + 1
	)
	else
		NewRowIndex = FixedRowIndex,
	
	Table->>insert_row(NewRowIndex,NewRow),
	F->>sortMiscRowTabbing(Table),
	Grid->>normalise(Key),
	Key->>keyboard_focus(@on). %and give it focus + make it current
%%

%%
sortMiscRowTabbing(_F, Table: table):->
	%the tab order in the misc page is weird, it depends on the order on which rows were added
	%thats why we order them by looping over the cells in the table
	
	Rows = Table<<-rows,
	%we loop backwards, so number 1 will get the highest order in the end
	%but the vectors containing the rows, and the cells in each row, are themselves ordered backwards
	Rows->>for_all(
		->>(@arg1,for_all,->>(@arg1?image,expose))).
%%

%%
deleteMiscRow(F):->
	%delete the currentrow if any
	Row = F<<-currentMiscRow,
	unless
		Row = @nil
	do
	(
		Table = Row<<-table,
		Index = Row<<-index,
		Table->>delete(Row),
		F->>currentMiscRow(@nil),
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
	).
%%

%%
moveMiscRow(F,Direction: int):->
	%move the current misc row up (Direction < 0) or down
	%for some reason it just does not work with moving the whole row
	%so we create a new row with the old values
	Row = F<<-currentMiscRow,
	Row \== @nil, %or fail
	RowIndex = Row<<-index,
	NewRowIndex is RowIndex + Direction,
	NewRowIndex > 0, %or fail
	NumRows = Row?table?rows<<-size,
	NewRowIndex =< NumRows,
	KeyText = Row?head?image<<-selection,
	ValueText = Row?tail?image<<-selection,
	Row?table->>delete(Row),
	F->>newMiscRow(KeyText,ValueText,NewRowIndex).
%%

*/
    
/******************** MULTIPLE MODELS ***********************/ % JL
/* Make the right model active */
input_focus(T, Boolean) :->
	send(@app, selectCorrectTab, T), % T is the window object (of the editor)
	send_super(T, input_focus, Boolean).
%%


:-pce_end_class.
