/*
Definitie correspondenceElement class:
figuurtje om een correspondence mee af te beelden

Part of Garp3, see copyright notice
Most code old dutch homer code

2000-2005 Jelmer Jellema, Spin in het Web, spininhetweb.nl
*/

:-pce_begin_class(correspondenceElement,
		  visualRelationElement,
		  "Display of correspondence"
		 ).

%overgenomen van configuration_element

%%
initialise(CE,
	   Fragment : modelFragment,
	   Cor: correspondence,
		Device: device,
		Arg1: [visualElement],
		Arg2: [visualElement],
		FragmentState: '[{normal,imported,parent}]',
		Route: [chain] %zie visualElement
	  ):->
	%maken van de display
	%gp3 changed this: we use an image now instead of a text
	
	CE->+initialise(Fragment,Cor,Arg1,Arg2,FragmentState,Route),

	Bitmap *= psBitmap(CE?stateImage),
	Bitmap->>name(bitmap),
	Bitmap->>psdef(CE?imageName),
	CE->>display(Bitmap), %onze bitmap
	CommentText *= text('*'),
	CommentText->>font(normal),
	CommentText->>colour(gray35),
	CommentText->>name(comment),
	CE->>display(CommentText,point(0,0)), %wordt allemaal nog goed gezet in updateDisplay

	CE->>initHandles(Bitmap,isCircle := @on),
	CE->>updateDisplay,
	CE->>doDisplay(Device), %relatie doet het zonder punt
	%gp3 0.2: this element has a tooltip with dynamic text
	CE->>tooltip(@default,model).
%%

%%
basename(AI,N:name):<-
	%gp3 0.2
	%the image name depends on a lot of stuff

	FE = AI<<-fragmentElement,
	if
		@on = FE<<-mirror
	then
		Rev = '_rev'
	else
		Rev = '',
		
	if
		@on = FE<<-derivative
	then
		Der = 'd'
	else
		Der = '',
		
	if
		@on = FE<<-full
	then
		Type = 'f' %allready here for gp3 0.3
	else
	(
		if
			FE->>isVCorrespondence
		then
			Type = 'v'
		else
			Type = 'q'
	),
	N *= string('corr%s%s%s',Der,Type,Rev).
%%

%%
updateDisplay(CE):->
	%changed in gp3 0.2
	CE?bitmap_member->>image(CE?stateImage),
	CE?bitmap_member->>psdef(CE?imageName),
	if
		0 = CE?fragmentElement?remarks<<-size
	then
		CE?comment_member->>displayed(@off)
	else
	(
		CE->>display(CE?comment_member,point(CE?bitmap_member?right_side + 2,
				CE?bitmap_member?top_side))
	),
	CE->+updateDisplay. %doet de relaties (MOET!)
%%

%%
connectFirstArgument(CE,
	Arg: graphical
	):->
	%overrule. Bij een directed correspondence geen pijl,
	%bij undirected wel

	if
		@on = CE?fragmentElement<<-directed
	then
		Link *= link(link,link,line(0,0,0,0,none))
	else
		Link *= link(link,link,line(0,0,0,0,first)),

	Link->>colour(CE?stateColour),
	Arg->>connect(CE,Link).
%%

%%
tooltipContent(VE,S: string):<-
	%gp3 1.4 rewrite of the tooltip
	
	El = VE<<-fragmentElement,
	S = VE<<-checkComments(El?relevantComments).
%%
:-pce_end_class.

