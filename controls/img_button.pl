/*
imgButton class
part of GARP3

A button with an image as label, and possibly another one for inactive.
There is also a toggleImgButton, just like there is a toggleCommandButton (almost the same, but different)
So the tree is:

imgButton
 - commandButton
 --- toggleCommandButton
 - toggleImgButton
 
Cannot find a better way to do this
2000-2005 Jelmer Jellema, Spin in het Web, spininhetweb.nl
*/

:-pce_begin_class(imgButton,
		  button
		 ).

variable(activeLabel, image, get). %when active
variable(inactiveLabel, image*, get). %the inactive label
variable(tooltipMsg, [char_array|function]*,both).
variable(inactiveTooltipMsg,  'char_array|function*',both).
%%
initialise(B, Name: name=name, Msg: message = [code], ImgName : img = [name],
	InactiveName: inactImg = '[name*]', ImgCategory: imgCategory = [name], 
		TT: tt = [char_array|function],
		ITT: itt = [char_array|function]):->
/*
	gp3 0.2 Create the button. The images are found using get_image (icons.pl)
	with the given names (without path or .gif) and in the given category.
	Defaults are normally ok:
	
	img: same as name
	inactImg: uses the imagename followed by _inactive when available (checked once)
				otherwise @nil: use the same image
	imgCategory: 'actions'
	tt = tooltip: you can use a static name or function
	itt: tooltip when inactive. Default is the same as tt. You can also use 'none' to tell the button not to show a tooltip when inactive
*/

	B->+initialise(Name,Msg),
	default(ImgName, Name,RImgName),
	B->>setImage(RImgName,InactiveName,ImgCategory),
	default(TT,@nil,Tooltip),
	B->>tooltipMsg(Tooltip),
	default(ITT,@nil,ITooltip),
	B->>inactiveTooltipMsg(ITooltip),
	B->>tooltip(@default). %use tooltipContent get-method
%%

%%
setImage(B, ImgName : img = name,
	InactiveName: inactImg = '[name*]', ImgCategory: imgCategory = [name]):->
	%set new image(s), same way as in initialise, though now the active image is not optional
	%when InactImg is not given, the _inactive variant is again checked
	
	default(ImgCategory, actions, RImgCat),
	
	get_image(RImgCat,ImgName,Img),
	
	if
		InactiveName = @default
	then
		RInactiveName = string('%s_inactive',ImgName)<<-value
	else
		RInactiveName = InactiveName,
		
	%inactive image might be missing
	if
		RInactiveName = @nil
	then
		InactImg = @nil
	else
	(
		unless
			get_image(RImgCat,RInactiveName,InactImg)
		do
			InactImg = @nil
	),
	B->>slot(activeLabel,Img),
	B->>slot(inactiveLabel,InactImg),
	B->>updateLabel.
%%

%%
tooltipContent(B, Msg: char_array*):<-
	if
		@on = B<<-active
	then
	(
		TTMsg = B<<-tooltipMsg
	)
	else
	(
		ITM = B<<-inactiveTooltipMsg,
		(
			ITM = none, TTMsg = @nil,!
			;
			ITM = @nil, TTMsg = B<<-tooltipMsg,!
			;
			TTMsg = ITM
		)
	),
	if
		TTMsg->>'_instance_of'(function)
	then
		Msg = TTMsg<<-forward(B)
	else
		Msg = TTMsg.
%%

%%
active(B, A: bool):->
	%set active state
	%do nothing if not changed
	unless
		A = B<<-active
	do
	(
		B->+active(A),
		B->>updateLabel
	).
%%

%%
updateLabel(B):->
	%set active or inactive
	if
		@on = B<<-active
	then
		B->>label(B?activeLabel)
	else
	(
		if
			@nil = B<<-inactiveLabel
		then
			B->>label(B?activeLabel)
		else
			B->>label(B?inactiveLabel)
	).
%%

:-pce_end_class.


:-pce_begin_class(toggleImgButton, imgButton).
%a imgButton that has a @on @off value (pressed/released)

variable(value,	bool := @off, get).

%%
initialise(B, Name: name=name, Msg: message = [code], ImgName : img = [name],
	InactiveName: inactImg = '[name*]', ImgCategory: imgCategory = [name], 
		TT: tt = [char_array|function],
		ITT: itt = [char_array|function],
		V: value = [bool]):-> 
	%just one extra arg: the value, default @off
	
	B->+initialise(Name,Msg,ImgName,InactiveName,ImgCategory,TT,ITT),
	default(V,@off,Val),
	B->>value(Val).
%%

%%
group(TIB, Other: toggleImgButton):->
	%add this button to a group together with Other
	%or add it to the group of other

	%do we allready have a group?
	if
		Group = Other<<-hypered(tibGroup)
	then
		TIB->>hyper(Group,tibGroup)
	else
	(
		%new group
		Group *= object, %just to hold it together
		TIB->>hyper(Group,tibGroup),
		Other->>hyper(Group,tibGroup)
	),
	%make sure only one has value @on
	if
		FirstOn = Group<<-hypered(tibGroup,@arg3?value == @on)
	then
		FirstOn->>doGroupSync.
%%

%%
groupSync(TIB, Selected: toggleImgButton):->
	%gp3: the group is synced: Selected is the only one that is @on
	%(it allready knows that)
	
	unless
		TIB = Selected
	do
		TIB->>value(@off). %not this one
%%	
	
%%
doGroupSync(TIB):->
	%sync the group (if any) that this one is @on
	if
		Group = TIB<<-hypered(tibGroup)
	then
		ignore(Group->>send_hyper(tibGroup,groupSync,TIB)).
%%
	
%%
groupValue(TIB, Value: name):<-
	%returns the name of the button in the group (if any) that has value @on
	
	Group = TIB<<-hypered(tibGroup),
	FirstOn = Group<<-hypered(tibGroup,@arg3?value == @on),
	Value = FirstOn<<-name.
%%

%%
value(TIB, Value:bool) :->
	%gp3 set value (and change displayed status)
	unless
		Value = TIB<<-value %allready ok
	do
	(
		TIB->>slot(value,Value),
		TIB->>setValueDisplay
	),
	if
		Value = @on
	then
		TIB->>doGroupSync. %if we are grouped, sync it
%%


%%
setValueDisplay(TIB):->
	%make sure the status is displayed correctly, needed after some calls
	if
		@on = TIB<<-value
	then
		TIB->>status(execute)
	else
		TIB->>status(inactive),
	TIB->>flush. %update display now
%%

%%
reset(TIB) :->
	%gp3: make sure the displayed status is remained after a mousemove off the button etc

	TIB->>setValueDisplay.
%%

%%
execute(TIB):->
	%gp3: make sure the display status remains ok
	%we do execute, also if we are allready @on (like pce toggle menu)
	
	TIB->>value(TIB?value?negate),
	TIB->+execute,
	TIB->>setValueDisplay.
%%

:-pce_end_class.