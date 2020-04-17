/*
Definitie attributeElement class:
figuurtje om een garpAttribute mee af te beelden

Part of Garp3, see copyright notice
Most code old dutch homer code

2000-2005 Jelmer Jellema, Spin in het Web, spininhetweb.nl
*/

:-pce_begin_class(attributeElement,
		  visualElement,
		  "Display of attribute"
		 ).

initialise(AE,
	   Fragment : modelFragment,
	   Attribute: garpAttribute,
		Device: device,
		IE: instanceElement,
		FragmentState: '[{normal,imported,parent}]',
		Route: [chain] %zie visualElement
	  ):->
	%maken van het figuurtje en teken hem

	AE->+initialise(Fragment,Attribute,IE,FragmentState,Route),

	AttribText *= text(''),
	AttribText->>name(attribText),
	AttribText->>font(italic),
	ValueText *= text(''),
	ValueText->>name(valueText),
	ValueText->>font(bold),
	
	Bitmap *= psBitmap(AE?stateImage),
	Bitmap->>name(bitmap),
	Bitmap->>psdef(AE?imageName),

	AE->>display(Bitmap),
	AE->>display(AttribText,
			point(Bitmap?right_side + 1,
			      Bitmap?center_y - AttribText?height)),
	AE->>display(ValueText,
			point(AttribText?x,
			      AttribText?bottom_side)),
	AE->>initHandles(Bitmap?area, centerRight := @off),
	%display
	AE->>doDisplay(IE,Device, strategy := visibleSub), %gp3 0.4.11, changed placement strategy
	AE->>updateDisplay,
	%gp3 0.2: this element has a tooltip with dynamic text
	AE->>tooltip(@default,model).
%%

%%
basename(_AI,N:name):<-
	%gp3 0.2
	N = attribute.
%%

%%
updateDisplay(AE):->
	"Update the display information" ::

	Attribute = AE<<-fragmentElement,

	(   0 = Attribute?remarks<<-size
	->  Val = Attribute?valueReference<<-valueName
	;   Val = Attribute?valueReference?valueName<<-append('*')
	),

	AE?attribText_member->>string(Attribute?name),
	AE?valueText_member->>string(Val),

	AE?bitmap_member->>image(AE?stateImage),
	AE?bitmap_member->>psdef(AE?imageName).
%%

%%
tooltipContent(VE,S: string):<-
	%gp3 1.4 rewrite of the tooltip
	
	El = VE<<-fragmentElement,
	S = VE<<-checkComments(El?definition?relevantComments,El?relevantComments).
%%
:-pce_end_class.
