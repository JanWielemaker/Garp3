/*
Definitie instanceElement class:
figuurtje om een instance mee af te beelden in een view

Part of Garp3, see copyright notice
Most code old dutch homer code

2000-2005 Jelmer Jellema, Spin in het Web, spininhetweb.nl
*/

:-pce_begin_class(
		  instanceElement,
		  visualElement,
		  "Display of garpInstance"
		 ).

initialise(IE,
		Fragment : modelFragment,
		Instance: garpInstance,
		Device: device,
		IMF: importedMFElement*, %gp3 0.4.11: we send the super element, not a placement point, 1.0: needed for general use as well
		FragmentState: '[{normal,imported,parent}]',
		Route: [chain] %zie visualElement
		):->
	   %we maken het figuurtje even en beelden het af
	   IE->+initialise(Fragment,
			   Instance,IMF,FragmentState,Route),
	   ClassText *= text(''),
	   ClassText->>name(classText),
	   %weer een poging om een fijn fontje te maken
	   %zelfde family, verder zelf
	   ClassText->>font(italic),
	   
	   NameText *= text(''),
	   NameText->>name(nameText),
	   NameText->>font(bold),
	   
	   Bitmap *= psBitmap(IE?stateImage,@on),
	   Bitmap->>psdef(IE?imageName),
	   Bitmap->>name(bitmap),
	   IE->>display(Bitmap), %onze bitmap
	   IE->>display(ClassText,
			point(Bitmap?right_side + 1,
			      Bitmap?center_y - ClassText?height)),
	   IE->>display(NameText,
			point(ClassText?x,
			      ClassText?bottom_side)),
	   
		if
			Instance?entity->>instance_of(entity)
		then
			%this will allways remain an entity, so we can create the handles now
	   		IE->>initHandles(Bitmap?area, centerRight := @off, isCircle := @on)
	   	else
	   		%agent: only the 4 center points
	  	   	IE->>initHandles(Bitmap?area,
	   			topLeft := @off, topRight := @off, 
	   			bottomLeft := @off, bottomRight := @off, centerRight := @off),

		IE->>updateDisplay,
		%gp3 0.4.11: strategy used for placement depends on whether we have a superfragment
		if
			IMF->>instance_of(importedMFElement)
		then
			IE->>doDisplay(IMF,Device,strategy := visibleSub)
		else
			IE->>doDisplay(@nil,Device,strategy := spot), %just find a spot
		%gp3 0.2: this element has a tooltip with dynamic text
		IE->>tooltip(@default,model).
%%

%%
basename(AI,N:name):<-
	%gp3 0.2
	N = AI?fragmentElement?entity<<-class_name.
%%

%%
updateDisplay(IE):->
	"Update the displayed information" ::
	%we zetten de texten en kleuren goed
	%en zorgen dat de subs kloppen

	Instance = IE<<-fragmentElement,
	ClassText = IE<<-member(classText),
	NameText = IE<<-member(nameText),
	Bitmap = IE<<-member(bitmap),

	ClassText->>string(Instance?entity?name),
	(   0 = Instance?remarks<<-size
	->  DName = Instance<<-name
	;   DName = Instance?name<<-append('*')
	),
	NameText->>string(DName),
	Bitmap->>image(IE?stateImage),
	Bitmap->>psdef(IE?imageName).
%%	

%%
tooltipContent(VE,S: string):<-
	%gp3 1.4 rewrite of the tooltip
	
	El = VE<<-fragmentElement,
	S = VE<<-checkComments(El?definition?relevantComments,El?relevantComments).
%%
:-pce_end_class.
