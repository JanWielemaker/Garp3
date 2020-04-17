/*
Definitie importedMFElement class:
figuurtje om een geimporteerd modelfragment mee af te beelden in een view

Part of Garp3, see copyright notice
Most code old dutch homer code

2000-2005 Jelmer Jellema, Spin in het Web, spininhetweb.nl
*/

:-pce_begin_class(
		  importedMFElement,
		  visualElement,
		  "Display of modelfragment in modelfragment"
		 ).

variable(subMFsShown,bool,both). %gp3 0.3: @on when we show symbols for the submfs

initialise(IMF,
		ContainerFragment : modelFragment,
		ImportedFragment: importedFragment,
		Device: device,
		Super: importedMFElement*,
		DisplayPoint: point,
		FragmentState: '[{normal,imported,parent}]',
		Route: [chain] %zie visualElement
		):->
	   %we maken het figuurtje even en beelden het af

	   IMF->+initialise(ContainerFragment,
			   ImportedFragment,Super,FragmentState,Route),

	   SavedSubMFsShown = IMF?fragment<<-layOutInfoDefault(IMF?fragmentElement,
					subMFsShown,@on,IMF?route),
	   IMF->>subMFsShown(SavedSubMFsShown), %default: do not show	
	   default(Route,new(chain),RRoute),
	   FullRoute = RRoute<<-copy,
	   FullRoute->>append(ImportedFragment),
	   
	   NameText *= text(''),
	   NameText->>name(nameText),
	   %weer een poging om een fijn fontje te maken
	   %zelfde family, verder zelf
	   NameText->>font(italic),
	   
	   Bitmap *= psBitmap(IMF?stateImage),
	   Bitmap->>psdef(IMF?imageName),
	   Bitmap->>name(bitmap),
	   IMF->>display(Bitmap), %onze bitmap
	   IMF->>display(NameText,
			point(Bitmap?right_side + 1,
			      Bitmap?center_y - ( NameText?height / 2))),
	   IMF->>initHandles(Bitmap?area,
	   		@off,@off,@off,@on,@on,@on,@on,@on), %only middle and bottomside

		IMF->>updateDisplay,
		IMF->>doDisplay(DisplayPoint,Device),
		%gp3 0.2: this element has a tooltip with dynamic text
		IMF->>tooltip(@default,model).

%%

%%
updateDisplay(IMF):->
	"Update the displayed information" ::

	%we zetten de texten en kleuren goed
	%gp3: show the (refined) name this element has in modelfragments above
	%this one, and add any refinement
	
	El = IMF<<-fragmentElement,
	if
	(
		El->>instance_of(fragmentRefiner),
		FR = El<<-fragment,
		FR = IMF<<-fragment
	)
	then
		Text *= string('%s --> %s',El?refinedName,El?refinementName)
	else
		Text *= string('%s',IMF?fragmentElement?name),
	unless
		0 = IMF?fragmentElement?remarks<<-size
	do
		Text->>append(' *'),
	IMF?nameText_member->>string(Text),
	IMF?bitmap_member->>image(IMF?stateImage),
	IMF?bitmap_member->>psdef(IMF?imageName).
%%	

%%
basename(_IMF,N:name):<-
	%gp3 0.2
	N = modelfragment.
%%

%%


%%
connectSub(VE,Sub: sub = visualElement,
			M : moveable = [bool] %default @on
			):-> 
	%gp3 0.3 overwrite for importedMFElement
	%anything that is connected to a mf symbol as a sub
	%should disconnect from anything else
	%this is needed when hiding or displaying submf symbols
	%--> object will connect to another symbol
	
	%remove the hypers and links in the sub, connected to super
	if
		Super = Sub<<-super
	then
	(
		Sub->>delete_hypers(super),
		Sub->>delete_hypers(moveable_by_parent),
		Sub->>disconnect(to:=Super)
	),
	%gp3 1.0 connectSub no longer sets the super-sub hyper, so we have to do this here
	VE->>hyper(Sub,sub,super),
	VE->+connectSub(Sub, M). %do the rest
%%

%%
saveLayoutInfo(VE):->
	%gp3 0.3: save special layout info for this element, and call parent
	%to save the standard info
	
	VE?fragment->>layOutInfo(VE?fragmentElement,
			    subMFsShown,
			    VE?subMFsShown,
				VE?route),
	VE->+saveLayoutInfo.
%%

%%
tooltipContent(VE,S: string):<-
	%gp3 1.4 rewrite of the tooltip
	
	El = VE<<-fragmentElement,
	S = VE<<-checkComments(El?relevantComments).
%%

%%
connectRefined(VE, RefinedElement: importedMFElement, Direct: bool):->
	%gp3 0.3 Make a identity link from the refiner element to the refined
	%Direct is @on if the link is to the refined element, or @off if its to its parent
	
	%disconnect old ones
	if
		Old = VE<<-hypered(connectedRefined)
	then
		VE->>disconnect(to:=Old),
	Link *= link(link,link,line(0,0,0,0,second)),
	if
		Direct = @on
	then
		Link->>colour(yellow)
	else
		Link->>colour(gray70),
	Link?line->>pen(2),	
	VE->>connect(RefinedElement,Link),
	VE->>hyper(RefinedElement,connectedRefined,connectedRefiner).
%%

:-pce_end_class.
