/*
Definitie quantityRelationElement class:
figuurtje om een quantity relation (proportionality of influence) mee af te beelden
Part of Garp3, see copyright notice
Most code old dutch homer code

2000-2005 Jelmer Jellema, Spin in het Web, spininhetweb.nl*/

:-pce_begin_class(quantityRelationElement,
		  visualRelationElement,
		  "Display of garpQuantityRelation"
		 ).

%overgenomen van correspondence_element

%%
initialise(QRE,
	   Fragment : modelFragment,
	   QR: garpQuantityRelation,
		Device: device,
		Arg1: [visualElement],
		Arg2: [visualElement],
		FragmentState: '[{normal,imported,parent}]',
		Route: [chain] %zie visualElement
	  ):->
	%maken van de display
	%gp3 0.2 changed this code
	
	QRE->+initialise(Fragment,QR,Arg1,Arg2,FragmentState,Route),

	Bitmap *= psBitmap(QRE?stateImage),
	Bitmap->>psdef(QRE?imageName),
	Bitmap->>name(bitmap),
	QRE->>display(Bitmap),
	
	CommentText *= text('*'),
	CommentText->>font(normal),
	CommentText->>colour(gray35),
	CommentText->>name(comment),
	QRE->>display(CommentText,point(0,0)), %wordt allemaal nog goed gezet in updateDisplay
	
	QRE->>initHandles(Bitmap, isCircle := @on),
	QRE->>updateDisplay,
	QRE->>doDisplay(Device), %relatie doet het zonder punt
	%gp3 0.2: this element has a tooltip with dynamic text
	QRE->>tooltip(@default,model).
%%

%%
basename(QRE,N:name):<-
	%gp3 0.2
	%depends on the type
	
	Name *= string('%s_%s',QRE?fragmentElement?type, QRE?fragmentElement?sign),
	N = Name<<-value.
%%

%%
updateDisplay(QRE):->
	"Update the display information" ::

	QRE?bitmap_member->>image(QRE?stateImage),
	QRE?bitmap_member->>psdef(QRE?imageName),
	if
		0 = QRE?fragmentElement?remarks<<-size
	then
		QRE?comment_member->>displayed(@off)
	else
	(
		QRE->>display(QRE?comment_member,point(QRE?bitmap_member?right_side + 2,
				QRE?bitmap_member?top_side))
	),
	QRE->+updateDisplay. %doet de relaties (MOET!)
%%


%%
tooltipContent(VE,S: string):<-
	%gp3 1.4 rewrite of the tooltip
	
	El = VE<<-fragmentElement,
	S = VE<<-checkComments(El?relevantComments).
%%

:-pce_end_class.

