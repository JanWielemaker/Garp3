/*
clickImage class
part of GARP3

A clickable label that is showing an image
Not a button. This is not part of the img_button code. You need to set the image yourself.
2000-2005 Jelmer Jellema, Spin in het Web, spininhetweb.nl
*/
:-pce_begin_class(
		  clickImage,
		  label
		 ).

%gp3 0.2
%clickable image, different bitmap for active:@off possible
variable(activeImage,image,both).
variable(inactiveImage,image*,both).
variable(rec,recogniser,both).
variable(active,bool,get).
variable(tooltipMsg, 'char_array|function*',both).
variable(inactiveTooltipMsg,  'char_array|function*',both).
variable(inAction, bool, both). %to make sure an action the action is not called for a second time, while the first one is not done yet
%to create a clickImage, set the image and optionally other arguments.
%to change tooltip, use TooltipMsg, not tooltip (sorry)

initialise(CI,
	Name: name,
	Image: image,
	Message: msg = [code], %message to run when clicked
	InactiveImage: inactiveImage = [image], %image to shown when active: @off (default: same as Image)
	TooltipMsg: tooltipMsg = [char_array|function], %only a text or function (forwarded with @arg1 = the clickImage)
	InactTooltipMsg: inactiveTooltipMsg = [char_array|function] %when not given: use same as tooltipMsg. When 'none': do not show
	):-> 

	CI->+initialise(Name),
	CI->>border(0),
	CI->>activeImage(Image),
	default(InactiveImage,@nil,II),
	CI->>inactiveImage(II),
	CI->>inAction(@off),
	CI->>rec(click_gesture(left, 
		message:= 
			if( CI?inAction == @off,
				and(
					->>(CI,inAction,@on),
					->>(@display,busy_cursor),
					if(->>(Message,forward)), %make sure it succeeds
					->>(@display,busy_cursor,@nil),
					->>(CI,inAction,@off)
					)
				)
			)),
	CI->>recogniser(CI?rec), %and set it
	default(TooltipMsg,@nil,TT),
	CI->>tooltipMsg(TT),
	default(InactTooltipMsg, @nil, InactTT),
	CI->>inactiveTooltipMsg(InactTT),
	CI->>tooltip(@default), %use tooltipContent get-method
	CI->>active(@on).
%%

%%
active(CI, A: bool):->
	%set active state
	CI->>slot(active,A),
	if
		A == @on
	then
	(
		CI->>selection(CI?activeImage),
		CI->>cursor(hand2),
		CI?rec->>active(@on)
	)
	else
	(
		II = CI<<-inactiveImage,
		unless
			II = @nil
		do
			CI->>selection(II),
		CI->>cursor(@nil),
		CI?rec->>active(@off)
	).
%%

%%
tooltipContent(CI, Msg: char_array*):<-
	if
		@on = CI<<-active
	then
	(
		TTMsg = CI<<-tooltipMsg
	)
	else
	(
		ITM = CI<<-inactiveTooltipMsg,
		(
			ITM = none, TTMsg = @nil,!
			;
			ITM = @nil, TTMsg = CI<<-tooltipMsg,!
			;
			TTMsg = ITM
		)
	),
	
	if
		TTMsg->>'_instance_of'(function)
	then
		Msg = TTMsg<<-forward(CI)
	else
		Msg = TTMsg.
%%

:-pce_end_class.