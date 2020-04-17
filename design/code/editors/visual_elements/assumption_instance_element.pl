/*
Definitie assumptionInstanceElement class:
figuurtje om een assumptie instance mee af te beelden in een view

Part of Garp3, see copyright notice
Most code old dutch homer code

2000-2005 Jelmer Jellema, Spin in het Web, spininhetweb.nl
*/

:-pce_begin_class(
		  assumptionInstanceElement,
		  visualElement,
		  "Display of assumptionInstances"
		 ).

initialise(AI,
	   Fragment : modelFragment,
	   Instance: assumptionInstance,
		Device: device,
		Super: visualElement*, 
		FragmentState: '[{normal,imported,parent}]',
		Route: [chain] %zie visualElement
	   ):->	
	   %we maken het figuurtje even en beelden het af
	   %gp3 0.2 Changed display code: no box, image dependant on state

	   AI->+initialise(Fragment,
			   Instance,Super,FragmentState,Route),

	   ClassText *= text(''),
	   ClassText->>name(classText),
	   %weer een poging om een fijn fontje te maken
	   %zelfde family, verder zelf
	   ClassText->>font(italic),
	   
	
	   Bitmap *= psBitmap(AI?stateImage),
	   Bitmap->>psdef(AI?imageName),
	   Bitmap->>name(bitmap),
	   AI->>display(Bitmap), %onze bitmap
	   AI->>display(ClassText,
			point(Bitmap?right_side + 1,
			      Bitmap?center_y - ( ClassText?height / 2))),
	   AI->>initHandles(Bitmap?area,
	   		topLeft := @off, topRight := @off, 
	   		bottomLeft := @off, bottomRight := @off, centerRight := @off),
		AI->>updateDisplay,
		VisArea = Device<<-visible,
		%gp3 0.4.11: strategy used for placement depends on whether we have a super element
		if
			Super->>instance_of(visualElement)
		then
			AI->>doDisplay(Super,Device,strategy := visibleSub)
		else
			AI->>doDisplay(point(VisArea?left_side,
						VisArea?bottom_side - AI?height),Device,strategy := spot), %just find a spot
		%gp3 0.2: this element has a tooltip with dynamic text
		AI->>tooltip(@default,model).
%%

%%
basename(_AI,N:name):<-
	%gp3 0.2
	N = assumption.
%%

%%
updateDisplay(AI):->
	"Update the displayed information" ::
	%we zetten de texten en kleuren goed

	Instance = AI<<-fragmentElement,
	ClassText = AI<<-member(classText),
	Bitmap = AI<<-member(bitmap),

	(   0 = Instance?remarks<<-size
	->  CName = Instance?assumption<<-name
	;   CName = Instance?assumption?name<<-append('*')
	),
	ClassText->>string(CName),
	Bitmap->>image(AI?stateImage),
	Bitmap->>psdef(AI?imageName).
%%	

%%
tooltipContent(VE,S: string):<-
	%gp3 1.4 rewrite of the tooltip
	
	El = VE<<-fragmentElement,
	S = VE<<-checkComments(El?definition?relevantComments,El?relevantComments).
%%
:-pce_end_class.
