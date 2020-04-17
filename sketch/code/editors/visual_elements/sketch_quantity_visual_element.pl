/*
Definitie sketchQuantityVisualElement class:
based on instanceElement class, AB, feb 2006
figuurtje om een sketchQuantityElement mee af te beelden in een view

Part of Garp3, see copyright notice
Most code old dutch homer code

2000-2005 Jelmer Jellema, Spin in het Web, spininhetweb.nl
*/

:-pce_begin_class(
		  sketchQuantityVisualElement,
		  sketchVisualElement,
		  "Display of sketchQuantityElement"
		 ).

initialise(IE,
		Sketch : sketch,
		SketchQuantityElement: sketchQuantityElement,
		Device: device,
		IMF: importedMFElement*, %gp3 0.4.11: we send the super element, not a placement point, 1.0: needed for general use as well
		% DisplayPoint: point,
		SketchState: '[{normal,imported,parent}]',
		Route: [chain] %zie sketchVisualElement
		):->
	   %we maken het figuurtje even en beelden het af

	   IE->+initialise(Sketch,
			   SketchQuantityElement, IMF, SketchState, Route),

	   % Don't display ClassText, AB, feb 2006
	   % ClassText *= text(''),
	   % ClassText->>name(classText),
	   %weer een poging om een fijn fontje te maken
	   %zelfde family, verder zelf
	   % ClassText->>font(italic),
	   
	   NameText *= text(''),
	   NameText->>name(nameText),
	   NameText->>font(bold),
	   Bitmap *= psBitmap(IE?stateImage,@on),
	   Bitmap->>psdef(IE?imageName),
	   Bitmap->>name(bitmap),
	   IE->>display(Bitmap), %onze bitmap
    
           % position text on the right side of the image, vertically centered
	   IE->>display(NameText,
	   	 point(Bitmap?right_side + 2, Bitmap?center_y - NameText?height)),
	   new(_, constraint(Bitmap, NameText, identity(center_y))), % center text vertically

	   IE->>updateDisplay,
   	   %gp3 0.4.11: strategy used for placement depends on whether we have a superfragment
           % To do: Sketches do not contain importedMFElements, so irrelevant code can be removed - AB
	   if
		IMF->>instance_of(importedMFElement)
	   then
		IE->>doDisplay(IMF,Device,strategy := visibleSub)
	   else
		IE->>doDisplay(@nil,Device,strategy := spot), %just find a spot
	   %gp3 0.2: this element has a tooltip with dynamic text
	   IE->>tooltip(@default).
%%


%%
imageName(_VE,N: name):<-
	%gp3 0.2 get the image name, overridable, 
        % overrides definition in sketchVisualElement
	
        swritef(Name, 'sketch_quantity',[]), 
	N = Name<<-value.
%%


%%
basename(AI,N:name):<-
	%gp3 0.2
	N = AI?sketchElement?entity<<-class_name. % should entity be sketchQuantityElement? AB, feb 2006
	% N = AI?sketchElement?sketchQuantityElement<<-class_name. 
%%

%%
updateDisplay(IE):->
	"Update the displayed information" ::
	%we zetten de texten en kleuren goed
	%en zorgen dat de subs kloppen

	SketchQuantityElement = IE<<-sketchElement,
	% ClassText = IE<<-member(classText),
	NameText = IE<<-member(nameText),
	% Bitmap = IE<<-member(bitmap),

	% ClassText->>string(SketchQuantityElement?entity?name), % should entity be sketchQuantityElement? AB, feb 2006
	(   0 = SketchQuantityElement?remarks<<-size
	->  DName = SketchQuantityElement<<-name
	;   DName = SketchQuantityElement?name<<-append('*')
	),
	NameText->>string(DName),
	IE->>init_handles.
	% Bitmap->>image(IE?stateImage),
	% Bitmap->>psdef(IE?imageName).
%%	

%%
tooltipContent(VE,S: string):<-
	%gp3 0.2 define the tooltip content
	
	El = VE<<-sketchElement,
	if
		El?entity->>instance_of(sketchQuantity) % should entity be sketchQuantity? AB, feb 2006
	then
		Type = 'Sketch Quantity'
	else
		Type = 'Unknown',
        S *= string('%s (%s)', El?name, Type), % should entity be sketchQuantity? AB, feb 2006
	unless
		0 = El?remarks<<-size
	do
		S->>ensure_nl(El?remarks).
%%
:-pce_end_class.
