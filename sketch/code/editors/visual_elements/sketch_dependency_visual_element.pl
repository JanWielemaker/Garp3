/*
Definitie sketchDependencyVisualElement class:
figuurtje om een quantity relation (proportionality of influence) mee af te beelden
Part of Garp3, see copyright notice
Most code old dutch homer code

2000-2005 Jelmer Jellema, Spin in het Web, spininhetweb.nl*/

:-pce_begin_class(sketchDependencyVisualElement,
		  sketchGenericRelationVisualElement,
		  "Display of sketchDependencyElement"
		 ).

%overgenomen van correspondence_element

%%
initialise(SKDVE,
	   Sk : sketch,
	   SDE: sketchDependencyElement,
		Device: device,
		Arg1: [sketchVisualElement],
		Arg2: [sketchVisualElement],
		FragmentState: '[{normal,imported,parent}]',
		Route: [chain] %zie sketchVisualElement
	  ):->
	%maken van de display
	%gp3 0.2 changed this code
	
	SKDVE->+initialise(Sk,SDE,Arg1,Arg2,FragmentState,Route),

	Bitmap *= psBitmap(SKDVE?stateImage),
	Bitmap->>psdef(SKDVE?imageName),
	Bitmap->>name(bitmap),
	SKDVE->>display(Bitmap),
	
	CommentText *= text('*'),
	CommentText->>font(normal),
	CommentText->>colour(gray35),
	CommentText->>name(comment),
	SKDVE->>display(CommentText,point(0,0)), %wordt allemaal nog goed gezet in updateDisplay
	
	SKDVE->>initHandles(Bitmap, isCircle := @on),
	SKDVE->>updateDisplay,
	SKDVE->>doDisplay(Device), %relatie doet het zonder punt
	%gp3 0.2: this element has a tooltip with dynamic text
	SKDVE->>tooltip(@default).
%%

%%
basename(SKDVE,N:name):<-
	%gp3 0.2
	%depends on the type
	
	% Name *= string('sketch_%s_%s',SKDVE?sketchElement?type, SKDVE?sketchElement?sign),
	Name *= string('sketch_%s',SKDVE?sketchElement?sign),
	N = Name<<-value.
%%

%%
updateDisplay(SKDVE):->
	"Update the display information" ::

	SKDVE?bitmap_member->>image(SKDVE?stateImage),
	SKDVE?bitmap_member->>psdef(SKDVE?imageName),
	if
		0 = SKDVE?sketchElement?remarks<<-size
	then
		SKDVE?comment_member->>displayed(@off)
	else
	(
		SKDVE->>display(SKDVE?comment_member,point(SKDVE?bitmap_member?right_side + 2,
				SKDVE?bitmap_member?top_side))
	),
	SKDVE->+updateDisplay. %doet de relaties (MOET!)
%%

%%
tooltipContent(VE,S: string):<-
	%gp3 0.2 define the tooltip content
	
	El = VE<<-sketchElement,
        get(El, sign, Sign), 
        pl_dependencyName(Sign, Label), 
	S *= string('%s ', Label), 
	unless
		0 = El?remarks<<-size
	do
		S->>ensure_nl(El?remarks).
%%

pl_dependencyName(inf_plus, 'positive influence'). 
pl_dependencyName(inf_min, 'negative influence'). 
pl_dependencyName(prop_plus, 'positive proportionality'). 
pl_dependencyName(prop_min, 'negative proportionality'). 


:-pce_end_class.

