/*
Definitie sketchStateVisualElement class:
based on instanceElement class, AB, feb 2006
figuurtje om een sketchStateElement mee af te beelden in een view

Part of Garp3, see copyright notice
Most code old dutch homer code

2000-2005 Jelmer Jellema, Spin in het Web, spininhetweb.nl
*/

:-pce_begin_class(
		  sketchStateVisualElement,
		  sketchVisualElement,
		  "Display of sketchStateElement"
		 ).

initialise(IE,
		Sketch : sketch,
		SketchStateElement: sketchStateElement,
		Device: device,
		IMF: importedMFElement*, %gp3 0.4.11: we send the super element, not a placement point, 1.0: needed for general use as well
		% DisplayPoint: point,
		SketchState: '[{normal,imported,parent}]',
		Route: [chain] %zie sketchVisualElement
		):->
	   %we maken het figuurtje even en beelden het af

	   IE->+initialise(Sketch,
			   SketchStateElement, IMF, SketchState, Route),
         
	   Box *= figure,
           send(Box, name, box), 
           send(Box, border, 3), 
           send(Box, pen, 1), 
           send(IE, border, 0), 
           send(IE, pen, 0), 
           send(IE, display, Box, point(0,0)),

	   ClassText *= text(''),
	   ClassText->>name(classText),
	   % weer een poging om een fijn fontje te maken
	   % zelfde family, verder zelf
	   ClassText->>font(italic),

	   NameText *= text(''),
	   NameText->>name(nameText),
	   NameText->>font(bold),

	   Bitmap *= psBitmap(IE?stateImage,@on),
	   Bitmap->>psdef(IE?imageName),
	   Bitmap->>name(bitmap),
	   IE->>display(Bitmap), %onze bitmap
    
	   Box->>display(NameText,
	   	point(2, 2)),
	   % IE->>display(ClassText,
	   %	point(NameText?right_side+10,
	   %	      NameText?bottom_side-8)),
	   Box->>display(ClassText,
		point(NameText?right_side+2,
		      NameText?bottom_side+1)),
	   
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
	
        swritef(Name, 'sketch_state',[]), 
	N = Name<<-value.
%%

%%
basename(AI,N:name):<-
	%gp3 0.2
	N = AI?sketchElement?entity<<-class_name. % should entity be sketchStateElement? AB, feb 2006
	% N = AI?sketchElement?sketchStateElement<<-class_name. 
%%

%%
updateDisplay(IE):->
	"Update the displayed information" ::
	%we zetten de texten en kleuren goed
	%en zorgen dat de subs kloppen

	SketchStateElement = IE<<-sketchElement,
	Box = IE<<-member(box),
	% ClassText = Box<<-member(classText),
	NameText = Box<<-member(nameText),
	Bitmap = IE<<-member(bitmap),

	% ClassText->>string(SketchStateElement?entity?name), % should entity be sketchStateElement? AB, feb 2006
	(   0 = SketchStateElement?remarks<<-size
	->  DName = SketchStateElement<<-name
	;   DName = SketchStateElement?name<<-append('*')
	),
	NameText->>string(DName),
        send(IE, displaySketchInequalities, SketchStateElement?sortedSketchStateInequalities),
        % round off corners of the box
        send(Box, radius, 5), 
        % update connection point handles
        send(IE, init_handles),
	Bitmap->>image(IE?stateImage),
	Bitmap->>psdef(IE?imageName),
        % send(IE?, display, Bitmap, point(0, IE?height)).
        % send(IE, display, Bitmap, point(0, ClassText?height+NameText?height+7)).
        send(IE, display, Bitmap, point(0, Box?height-1)).
%%	


%%
displaySketchInequalities(IE, Ineqs):->
	"Update the sketch inequalities" ::
	% place text for inequalities

	% SketchStateElement = IE<<-sketchElement,
	Box = IE<<-member(box),
	ClassText = Box<<-member(classText),
	% ClassText = IE<<-member(classText),
	send(ClassText, clear), 
       
        % for all elements in Ineqs, add it to the state's text 
        % Ineqs \= @nil, 
	% L *= string('%s',Def?name)
	% L *= string('%s',Def?value),
	% Ineqs->>for_all(->>(NameText, append, @arg1)).
	% send(Ineqs, for_all, message(NameText, append, @arg1)).
	send(Ineqs, for_all, and(
				 message(ClassText, append, @arg1?displayNameString),
				 message(ClassText, append, when(@arg1 == Ineqs?tail, '', '\n'))
				 )
        ).
%	



%%
tooltipContent(VE,S: string):<-
	%gp3 0.2 define the tooltip content
	
	El = VE<<-sketchElement,
	if
		El?entity->>instance_of(sketchState) % should entity be sketchState? AB, feb 2006
	then
		Type = 'SketchState'
	else
		Type = 'Unknown',
        S *= string('%s (%s)', El?name, Type), % should entity be sketchState? AB, feb 2006
	unless
		0 = El?remarks<<-size
	do
		S->>ensure_nl(El?remarks).
%%


%
init_handles(IE):->
	 % Box = IE<<-member(box),
         % this does not work after updating
         % IE->>initHandles(Box?area, 
         IE->>initHandles(IE?area, 
                                   topLeft := @on, 
                                   bottomLeft := @on, 
                                   centerLeft := @on,
                                   topRight := @on, 
                                   bottomRight := @on, 
                                   centerRight := @on).
%%


:-pce_end_class.
