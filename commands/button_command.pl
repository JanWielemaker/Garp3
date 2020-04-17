/*
button_command.pl
gp3 0.2
Defines commandButtonHolder and commandButton

Class commandButtonHolder.
This is a very simple implementation of a dialog that might
contain 0 or more commandButton objects. It works with framedWindow.
The only differences
with a normal dialog are:
- some layout is preset
- when visible, and in an exposed framedWindow, it sends checkActive calls to all members and does so recursively for members that are instance_of device. Code there can check if the graphical should be active or not. This is used in commandButton.
Because of the recursive calls of checkActive, you can use dialog_group etc.
- Some code to add buttons using the normal garp3 way

Class commandButton.
This is a button that can be used with a command object. When put on a commandButtonHolder (or a member of a commandButtonHolder), it will respons to the automatic checkActive calls of the holder, but it will allways work without it (thought you must set the active state manually).
The button can have a tooltip (expects the tooltip.pl helper) and 2 labels (active/inactive). Labels are switched when executing ->active. (button will also be greyed out).
Very basic command implementation: only canRun and execute, no info types. The button gets it static labels when initialising.

Class toggleCommandButton
This is a subclass of commandButton: only difference: it has a <->value and will display either pressed (value=@on) or release (value=@off). The command will execute when the button is clicked, never mind the value. When the condition for the command fails, the button value can not be changed.

the toggleCommandButton can also be used as a grouped button. The button can be made part of a group using ->group(other ToggleCommandButton). When the button is part of a group, it reacts a bit different: Setting the value to @on will make all the other buttons in the same group set value to @off. Clicking a released button will set value to @on and execute command, clicking a pressed button will not do anything.

Part of Garp3 see copyright notice

2000-2005 Jelmer Jellema, Spin in het Web, spininhetweb.nl
*/


:- module(button_command, []).


:- pce_begin_class(commandButtonHolder, dialog). %holds the buttonbar

initialise(CBH, Label: [name], Size:[size], Display:[display]) :->
	CBH->+initialise(Label,Size,Display),
	CBH->>pen(0),
	CBH->>border(size(0,0)),
	CBH->>gap(size(0,0)).
%%

%%%%placement
%some special calls to add graphical buttons using commands
%this is a shortcut that is helpful most of the time
%but you can also just use ->display or even ->append etc

%with add you may send placement info, used to place relative to another button
%otherwise it is append
%context is de context as used in get_image (helpers/icons.pl)
%we use this call with the labelnames to get an image
%when Toggle is @on, the button will be a toggleCommandButton

add(CBH, Name: name, Command: name, Context: name, ActiveLabel: name, InactiveLabel: [name*],
	Tooltip: [char_array|function],Placement: ['{below,right}'], RelativeTo: '[graphical|name]', Toggle: toggle = [bool],
		InactiveTooltip : inactivateTooltip = [char_array|function], B : commandButton):<-
	
	
	if
		Toggle = @on
	then
		Class = toggleCommandButton
	else
		Class = commandButton,
		
	B = @pce<<-instance(Class,Name,Command,ActiveLabel,InactiveLabel,Context,Tooltip, InactiveTooltip),
	
	B->>alignment(left), %set as default
	if
		Placement = @default
	then
		CBH->>append(B)
	else
	(
		if
			RelativeTo->>instance_of(graphical)
		then
			Button = RelativeTo
		else
			Button = CBH<<-member(RelativeTo),	
		send(B,Placement,Button) %below, right is the message name
	).
%%

%
addWithRepeat(CBH, Name: name, Command: name, Context: name, ActiveLabel: name, InactiveLabel: [name*],
	Tooltip: [char_array|function],Placement: ['{below,right}'], RelativeTo: '[graphical|name]', Toggle: toggle = [bool],
 		InactiveTooltip : inactivateTooltip = [char_array|function], Repeat: repeat = [bool], B : commandButton):<-

        B = CBH<<-add(Name, Command, Context, ActiveLabel, InactiveLabel, Tooltip, Placement, RelativeTo, Toggle, InactiveTooltip), 
	B->>slot(repeat, Repeat).        
%%


%%Activation
updateStatus(CBH):->
	%called by framedWindow (or other) when buttons should check their status
	if
		@on = CBH<<-displayed
	then
	(
		CBH?graphicals->>for_some(->>(CBH,doUpdateMember,@arg1))
	).

%
doUpdateMember(CBH, M: graphical):->
	%helper for doUpdate
	
	if
		M->>has_send_method(doUpdate)
	then
		M->>doUpdate,
	if
		M->>instance_of(device) %has members
	then
		M?graphicals->>for_some(->>(CBH,doUpdateMember,@arg1)).
%%


:- pce_end_class.

:- pce_begin_class(commandButton,imgButton). %button using command objects. See controls/img_button

variable(commandname,name,get).

initialise(B, Name: name = name, C: commandname = name,
	L1: activeLabel = [name], L2: inactiveLabel = ['name*'],
		Cat: category = [name],
		TT: tooltip = [char_array|function],
		ITT: inactiveTooltip = [char_array|function]):->
		
	%initialise the button
	%most arguments go to imgButton, see there
	%tooltip is a char_array or a function (forwarded with @arg1 = the receiver)
	
	B->+initialise(Name,->>(B,executeCommand),L1,L2,Cat, TT, ITT),
	B->>slot(commandname,C).
%%

%%
doUpdate(B):->
	%call from commandButtonHolder to recheck our active state etc
	if
		B->>executeCondition
	then
		B->>active(@on)
	else
		B->>active(@off).
%%

%%
executeCommand(B) :->
	%get the command (using getCommand from command_helpers)
	%and execute it

	%just use our frame
	getCommand(B?frame,B?commandname,Command),
	%recheck
	if
		Command->>canRun(B)
	then
		Command->>execute(B).
	%allways succeed
%%

%%
executeCondition(B) :->
	%get the command (using getCommand from command_helpers)
	%and check if it could run

	getCommand(B?frame,B?commandname,Command),
	Command->>canRun(B).
%%

:-pce_end_class.

:-pce_begin_class(toggleCommandButton, commandButton).
%a commandButton that has a @on @off value (pressed/released)

variable(value,	bool := @off, get).
variable(repeat, bool := @off, get). % added for StateMode button in Simulate, AB, june 2007

%%
group(TCB, Other: toggleCommandButton):->
	%add this button to a group together with Other
	%or add it to the group of other

	%do we allready have a group?
	if
		Group = Other<<-hypered(tcbGroup)
	then
		TCB->>hyper(Group,tcbGroup)
	else
	(
		%new group
		Group *= object, %just to hold it together
		TCB->>hyper(Group,tcbGroup),
		Other->>hyper(Group,tcbGroup)
	),
	%make sure only one has value @on
	if
		FirstOn = Group<<-hypered(tcbGroup,@arg3?value == @on)
	then
		FirstOn->>doGroupSync.
%%

%%
groupSync(TCB, Selected: toggleCommandButton):->
	%gp3: the group is synced: Selected is the only one that is @on
	%(it allready knows that)
	
	unless
		TCB = Selected
	do
		TCB->>value(@off). %not this one
%%	
	
%%
doGroupSync(TCB):->
	%sync the group (if any) that this one is @on
	if
		Group = TCB<<-hypered(tcbGroup)
	then
		ignore(Group->>send_hyper(tcbGroup,groupSync,TCB)).
%%
	
%%
groupValue(TCB, Value: name):<-
	%returns the name of the button in the group (if any) that has value @on
	
	Group = TCB<<-hypered(tcbGroup),
	FirstOn = Group<<-hypered(tcbGroup,@arg3?value == @on),
	Value = FirstOn<<-name.
%%

%%
value(TCB, Value:bool) :->
	%gp3 set value (and change displayed status)
	unless
		Value = TCB<<-value %allready ok
	do
	(
		TCB->>slot(value,Value),
		TCB->>setValueDisplay
	),
	if
		Value = @on
	then
		TCB->>doGroupSync. %if we are grouped, sync it
%%

%%
doUpdate(B):->
	%call from commandButtonHolder to recheck our active state etc
	%we try and get our value (when active)
	%using the 'Value' infotype
	
	B->+doUpdate,
	if
	(
		B->>executeCondition
	)
	then
	(
		getCommand(B?frame,B?commandname,Command),
		Value = Command<<-getInfo('Value',B,B?value),
		B->>value(Value)
	).
%%


%%
executeCommand(TCB):->
	%gp3 overwrite: make sure we change the value
	%if we are grouped, we do nothing if we are already @on, unless repeat = @on

	unless
	(
		TCB<<-hypered(tcbGroup),
		@on = TCB<<-value,
	        if @on = TCB<<-repeat
	        then 
	        (
	          % executeCommand, but don't negate the value-switch
		  TCB->+executeCommand
                )
	)
	do
	(
		TCB->>value(TCB?value?negate),
		TCB->+executeCommand
	).
%%


%%
setValueDisplay(TCB):->
	%make sure the status is displayed correctly, needed after some calls
	if
		@on = TCB<<-value
	then
		TCB->>status(execute)
	else
		TCB->>status(inactive),
	TCB->>flush. %update display now
%%

%%
reset(TCB) :->
	%gp3: make sure the displayed status is remained after a mousemove off the button etc

	TCB->>setValueDisplay.
%%

%%
execute(TCB):->
	%gp3: make sure the display status remains ok
	TCB->+execute,
	TCB->>setValueDisplay.
%%

:-pce_end_class.



