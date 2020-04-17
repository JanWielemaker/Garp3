/*
Definitie quantityElement class:
figuurtje om een garpQuantity mee af te beelden

Part of Garp3, see copyright notice
Most code old dutch homer code

2000-2005 Jelmer Jellema, Spin in het Web, spininhetweb.nl
*/

:-pce_begin_class(quantityElement,
		  visualElement,
		  "Display of quantity"
		 ).

initialise(PE,
	   Fragment : modelFragment,
	   Quantity: garpQuantity,
		Device: device,
		IE: instanceElement, 
		FragmentState: '[{normal,imported,parent}]',
		Route: [chain] %zie visualElement
	  ):->
	%maken en tekenen van het figuurtje
    	PE->+initialise(Fragment,Quantity,IE,FragmentState,Route),

	NameText *= text(''),
	NameText->>name(nameText),
	NameText->>font(italic),

	Bitmap *= psBitmap(PE?stateImage),
	Bitmap->>psdef(PE?imageName),
	Bitmap->>name(bitmap),
	PE->>display(Bitmap),
	PE->>display(NameText,
		     point(Bitmap?right_side + 1,
			   Bitmap?center_y - (NameText?height / 2))),
	PE->>initHandles(Bitmap?area, isCircle := @on, centerRight := @off),

	%display
	PE->>doDisplay(IE,Device, strategy := visibleSub), %gp3 0.4.11 Use a different strategy and send IE as argument
	PE->>updateDisplay,
	%gp3 0.2: this element has a tooltip with dynamic text
	PE->>tooltip(@default,model).

%%

%%
basename(_PE,N:name):<-
	%gp3 0.2
	N = quantity.
%%

	
%%
updateDisplay(PE):->
	"Update the display information" ::

	Quantity = PE<<-fragmentElement,
	Name *= string('%s',Quantity?name),
	unless
		0 = Quantity?remarks<<-size
	do
		Name->>append('*'),

	%gp3 0.3.16: if we have special quantity behaviours, we add an exclamation
	unless
		Quantity?quantityAssumptions->>empty
	do
		Name->>append(' !'),
		
	PE?nameText_member->>string(Name),
	
	PE?bitmap_member->>image(PE?stateImage),
	PE?bitmap_member->>psdef(PE?imageName).
%%

%%
tooltipContent(VE,S: string):<-
	%gp3 1.4 rewrite of the tooltip
	
	El = VE<<-fragmentElement,
	S = VE<<-checkComments(El?definition?relevantComments,El?relevantComments).
%%
:-pce_end_class.
