/*
Garp3
2005 Jelmer Jellema - Spin in het Web
Helper dialog classes used by main menu and elsewhere

gp3 0.3
*/

%%
:-pce_begin_class(
		  saveChangesDlg,
		  dialog
		 ).
%used by @app to ask if the user wants to save changed made to the model
%only windowdef

initialise(D,Frame: frame):->
	%Display this window
	%again, we do not use append, because we cannot reason with sizes then before creation
	D->+initialise,
	D->>application(@app),
	D->>transient_for(Frame),
	D->>modal(transient),
	%%gp3 0.3 changed
	%D->>kind(popup),
	%in:
	D->>kind(transient),
	D->>label('Save changes?'),
	%
	
	D->>gap(size(0,0)),
	D->>border(size(6,6)),
	D->>pen(0),
	D->>background(white),
	
	get_image(helpers,'alarm',Img),
	Logo *= label(logo,Img),
	D->>display(Logo,point(18,18)), %12 points into the box, which will be at 6,6
	% Add the name of the active model to the warning
	get(@app?modelsTabs, find_key, message(@prolog, ==, @arg2, @model), ModelTab),
	get(ModelTab, realTabName, ModelName),
	swritef(Warning, 'Model: \'%w\' has changed.\nSave it?', [ModelName]),
	Text *= text(Warning, left, font(helvetica,bold,14)),
	Text->>name(lbl),
	Text->>margin(400,wrap),
	D->>display(Text,point(Logo?right_side + 6, Logo?top_side)),
	MaxY *= number(Logo?bottom_side),
	MaxY->>maximum(Text?bottom_side),
	
	%box behind it all	
	Box *= box(Text?right_side - Logo?left_side + 24, MaxY - Logo?top_side + 24), %spacing
	Box->>pen(0),
	BG = new(colour)<<-convert('#fb0f0c'), %buggy: cannot use the hex value directly
	Box->>fill_pattern(BG),
	D->>display(Box,point(6,6)),
	Box->>hide,

	%we use clickImage object, no buttons, like on the mainmenu
	get_image(helpers,'save',SaveImg),
	Save *= clickImage(save,SaveImg,->>(D,return,@on), tooltipMsg:='Save the model'),
	get_image(helpers,'cancelchanges',SkipImg),
	Skip *= clickImage(cancelchanges,SkipImg,->>(D,return,@off), tooltipMsg:='Skip changes'),
	get_image(helpers,'cancel',CancelImg),
	Cancel *= clickImage(cancel,CancelImg,->>(D,return,@nil), tooltipMsg:='Cancel'),
	D->>display(Save,point(Box?left_side,Box?bottom_side + 6)),
	D->>display(Skip,point(Box?left_side + (Box?width - Skip?width) / 2,Box?bottom_side + 6)),
	D->>display(Cancel,point(Box?right_side - Cancel?width,Box?bottom_side + 6)).
%%
:-pce_end_class.


%%
:-pce_begin_class(
		  confirmOverwriteDlg,
		  dialog
		 ).
%used by @app and legacy export to ask if the user wants to overwrite something
%windowdef, text is variable

initialise(D,Frame: frame, DisplayText: char_array):->
	%Display this window
	%again, we do not use append, because we cannot reason with sizes then before creation
	D->+initialise,
	D->>application(@app),
	D->>transient_for(Frame),
	D->>modal(transient),
	% D->>kind(popup), %gp3 0.3 changed this to:
	D->>kind(transient),
	D->>label('Please confirm'),
	%
	D->>gap(size(0,0)),
	D->>border(size(6,6)),
	D->>pen(0),
	D->>background(white),
	
	get_image(helpers,'alarm',Img),
	Logo *= label(logo,Img),
	D->>display(Logo,point(18,18)), %12 points into the box, which will be at 6,6
	Text *= text(DisplayText,left, font(helvetica,bold,14)),
	Text->>name(lbl),
	Text->>margin(400,wrap),
	D->>display(Text,point(Logo?right_side + 6, Logo?top_side)),
	MaxY *= number(Logo?bottom_side),
	MaxY->>maximum(Text?bottom_side),
	
	%box behind it all	
	Box *= box(Text?right_side - Logo?left_side + 24, MaxY - Logo?top_side + 24), %spacing
	Box->>pen(0),
	BG = new(colour)<<-convert('#fb0f0c'), %buggy: cannot use the hex value directly
	Box->>fill_pattern(BG),
	D->>display(Box,point(6,6)),
	Box->>hide,

	%we use clickImage object, no buttons, like on the mainmenu
	get_image(helpers,'overwrite',SaveImg),
	Save *= clickImage(save,SaveImg,->>(D,return,@on), tooltipMsg:='Overwrite existing'),
	get_image(helpers,'cancel',CancelImg),
	Cancel *= clickImage(cancel,CancelImg,->>(D,return,@off), tooltipMsg:='Cancel'),
	D->>display(Save,point(Box?left_side,Box?bottom_side + 6)),
	D->>display(Cancel,point(Box?right_side - Cancel?width,Box?bottom_side + 6)).
%%
:-pce_end_class.


%%
:-pce_begin_class(
		  notificationDlg,
		  dialog
		 ).
%used by frame->>msgBox (zie object_extension.pl helper code)
%gp3 0.3 added confirm type

initialise(D,Frame: frame, Type: {notification,alarm,confirm}, DisplayText: char_array, Smallfont: [bool]):->
	%Display this window
	%again, we do not use append, because we cannot reason with sizes then before creation
	D->+initialise,
	D->>application(@app),
	D->>transient_for(Frame),
	D->>modal(transient),
	%D->>kind(popup), %gp3 0.3 changed this to
	D->>kind(transient),
	
	D->>gap(size(0,0)),
	D->>border(size(6,6)),
	D->>pen(0),
	D->>background(white),
	
	get_image(helpers,Type,Img),
	Logo *= label(logo,Img),
	D->>display(Logo,point(18,18)), %12 points into the box, which will be at 6,6
	if
		Smallfont = @on %gp3
	then
		Fontsize = 12
	else
		Fontsize = 14,
	Text *= text(DisplayText,left, font(helvetica,bold,Fontsize)),
	Text->>name(lbl),
	Text->>margin(500,wrap),
	D->>display(Text,point(Logo?right_side + 6, Logo?top_side)),
	MaxY *= number(Logo?bottom_side),
	MaxY->>maximum(Text?bottom_side),
	
	%box behind it all	
	Box *= box(Text?right_side - Logo?left_side + 24, MaxY - Logo?top_side + 24), %spacing
	Box->>pen(0),
	if
		Type = notification
	then
	(
		BG = new(colour)<<-convert('#E7EAEA'), %buggy: cannot use the hex value directly
		D->>label('Message')
	),
	if
		Type = alarm
	then
	(
		D->>label('Warning'),
		BG = new(colour)<<-convert('#fb0f0c')
	),
	if
		Type = confirm
	then
	(
		D->>label('Please confirm'),
		BG = new(colour)<<-convert('#E7EAEA')
	),
		
	Box->>fill_pattern(BG),
	D->>display(Box,point(6,6)),
	Box->>hide,

	%we use clickImage object, no buttons, like on the mainmenu
	get_image(helpers,'ok',OkImg),
	Ok *= clickImage(ok,OkImg, ->>(D,onOk), tooltipMsg:='OK'),
	if
		Type = confirm
	then
	(
		get_image(helpers,'cancel',CancelImg),
		Cancel *= clickImage(cancel,CancelImg, ->>(D,onCancel), tooltipMsg:='Cancel'),
		D->>display(Ok,point(Box?left_side,Box?bottom_side + 6)),
		D->>display(Cancel,point(Box?right_side - Cancel?width,Box?bottom_side + 6))
	)
	else
		D->>display(Ok,point(Box?left_side + (Box?width - Ok?width)/2,Box?bottom_side + 6)).
%%

%%
onOk(D):->
	%ok pushed
	
	D->>return(@on).
%%

%%
onCancel(D):->
	%cancel pushed

	D->>return(@off).
%%

:-pce_end_class.

