/*
Definitie sketchObjectVisualElement class:
based on instanceElement class, AB, feb 2006
figuurtje om een sketch Object mee af te beelden in een view

Part of Garp3, see copyright notice
Most code old dutch homer code

2000-2005 Jelmer Jellema, Spin in het Web, spininhetweb.nl
*/

:-pce_begin_class(
		  sketchObjectVisualElement,
		  sketchVisualElement,
		  "Display of sketchObjectElement"
		 ).


initialise(IE,
		Sketch : sketch,
		Concept: sketchObjectElement,
		Device: device,
		IMF: importedMFElement*, %gp3 0.4.11: we send the super element, not a placement point, 1.0: needed for general use as well
		% DisplayPoint: point,
		SketchState: '[{normal,imported,parent}]',
		Route: [chain] %zie sketchVisualElement
		):->
	   %we maken het figuurtje even en beelden het af

	   IE->+initialise(Sketch,
	    		   Concept, IMF, SketchState,Route),

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
    
           % position text below, horizontally centered
	   % IE->>display(NameText,
	   % 	 point(0, Bitmap?bottom_side)),
	   % new(_, constraint(Bitmap, NameText, identity(center_x))), % center text horizontally

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
imageName(VE,N: name):<-
	%gp3 0.2 get the image name, overridable, 
        % overrides definition in sketchVisualElement
	
        Type = VE?sketchElement<<-type,
        swritef(Name, 'sketch_%d',[Type]), 
	N = Name<<-value.
%%

%%
basename(AI,N:name):<-
	%gp3 0.2
	N = AI?sketchElement?entity<<-class_name. % should entity be sketchObject? AB, feb 2006
	% N = AI?sketchElement?sketchObject<<-class_name. 
%%

%%
updateDisplay(IE):->
	"Update the displayed information" ::
	%we zetten de texten en kleuren goed
	%en zorgen dat de subs kloppen

	Concept = IE<<-sketchElement,
	% ClassText = IE<<-member(classText),
	NameText = IE<<-member(nameText),
	Bitmap = IE<<-member(bitmap),

	% ClassText->>string(Concept?entity?name), % should entity be sketchObject? AB, feb 2006
	(   0 = Concept?remarks<<-size
	->  DName = Concept<<-name
	;   DName = Concept?name<<-append('*')
	),
	NameText->>string(DName),

        if Bitmap?image->>equal(IE?stateImage)
        then 
           % image was already up to date
           true
        else
           (
           % update image
 	   Bitmap->>image(IE?stateImage),
	   Bitmap->>psdef(IE?imageName),
           IE->>repositionText(NameText, Bitmap)
        ),
        % update connection point handles
	IE->>init_handles.
%%

%
positionText(IE, NameText: text, Bitmap: psBitmap):->
           % position text on the right side of the image, vertically centered
	   IE->>display(NameText,
	   	 point(Bitmap?right_side + 2, Bitmap?center_y - NameText?height)),
	   new(_, constraint(Bitmap, NameText, identity(center_y))). % center text vertically
%%	

%
repositionText(IE, NameText: text, Bitmap: psBitmap):->
           % repositionText for sketch object
           % position text on the right side of the image, 
           % no need to change y, because the center constraint is still active
	   IE->>display(NameText,
	   	 point(Bitmap?right_side + 2, NameText?y)).
%%	


%
tooltipContent(VE,S: string):<-
	%gp3 0.2 define the tooltip content
	
	El = VE<<-sketchElement,
        Type = El<<-type,
        if
		Type = undefined
        then
               (
	 	swritef(TypeName, 'Sketch undefined object',[])
	       )
	else
	       ( 
                 swritef(TypeName, 'Sketch %d',[Type])
		),

        S *= string('%s (%s)', El?name, TypeName), % should entity be sketchObject? AB, feb 2006
	unless
		0 = El?remarks<<-size
	do
		S->>ensure_nl(El?remarks).
%%
:-pce_end_class.

