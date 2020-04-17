/*
Definitie calculusElement class:
afbeelden van een calculus in een modelfragment

Let op: Ook al lijkt een calculusElement op een relatie, het is geen subclass van relationElement. Het wordt
dus gezien als een los object dat gerelateerd is aan zijn twee argumenten en zelf onderdeel van een relatie
kan zijn. Hierdoor geldt alle relatie functionaliteit niet (zoals tussen argumenten blijven)

Part of Garp3, see copyright notice
Most code old dutch homer code

2000-2005 Jelmer Jellema, Spin in het Web, spininhetweb.nl
*/

:-pce_begin_class(
		  calculusElement,
		  visualElement,
		  "Display of calculus"
		 ).

initialise(C,
	   Fragment : modelFragment,
	   Calculus: calculus,
		Device: device,
		Arg1: visualElement,
		Arg2: visualElement,
		FragmentState: '[{normal,imported,parent}]',
		Route: [chain] %zie visualElement
	   ):->	
	   %we maken het figuurtje even en beelden het af


	   C->+initialise(Fragment,
			   Calculus,@nil,FragmentState,Route),

	%even de argumenten zetten
	C->>hyper(Arg1,arg1,calculus),
	C->>hyper(Arg2,arg2,calculus),

	Arg1->>checkImportant,
	Arg2->>checkImportant,

	%gp3: changed layout
	
	Bitmap *= psBitmap(C?stateImage),
	Bitmap->>psdef(C?imageName),
	Bitmap->>name(bitmap),
	C->>display(Bitmap),


	
	CommentText *= text('*'),
	CommentText->>font(normal),
	CommentText->>colour(gray35),
	CommentText->>name(comment),

	C->>display(CommentText,point(Bitmap?right_side - 10,
				Bitmap?top_side - 10)), %see updateDisplay
	
	CLeft = C?area<<-left_side, %calculate current situation
	CTop = C?area<<-top_side,
	%we hebben speciale handles en roepen dus niet initHandles van de superklasse aan
		%gp3: BB wants the args in the icon, this breaks when icon changes
	C->>handle(handle(4 - CLeft,Bitmap?area?center?y - CTop, arg1)), %alleen voor en min
	C->>handle(handle(4 - CLeft,Bitmap?area?center?y - CTop,arg)), %voor plus
	C->>handle(handle(20 - CLeft,Bitmap?area?center?y - CTop,arg2)), %voor min
	C->>handle(handle(20 - CLeft,Bitmap?area?center?y - CTop,arg)), %voor plus
	C->>handle(handle(34 - CLeft,Bitmap?area?center?y - CTop)), %standaard handle voor calculus als argument in relatie

	C->>updateDisplay,

	%berekening punt tussen de twee args (overgenomen van visualRelationElement)
	Arg1Point = Arg1<<-absolute_position,
	Arg2Point = Arg2<<-absolute_position,

	%ok, we zetten de calculus er precies tussen in
	X1 = Arg1Point<<-x,
	Y1 = Arg1Point<<-y,
	X2 = Arg2Point<<-x,
	Y2 = Arg2Point<<-y,
	(   X1 < X2
	->  NewX is X1 + ((X2 - X1) // 2)
	;   NewX is X2 + ((X1 - X2) // 2)
	),
	(   Y1 < Y2
	->  NewY is Y1 + ((Y2 - Y1) // 2)
	;   NewY is Y2 + ((Y1 - Y2) // 2)
	),

	C->>doDisplay(point(NewX,NewY),Device),
	C->>checkDisplayed,
	%gp3 0.2: this element has a tooltip with dynamic text
	C->>tooltip(@default,model).
%%

%%
basename(C,N:name):<-
	%gp3 0.2
	%depends on the type
	
	Name *= string('math_%s',C?fragmentElement?sign),
	N = Name<<-value.
%%

%%
destroy(C):->
	%even de argumenten op de hoogte brengen
	C->>removeArgs,
	C->+destroy.
%%

%%
updateDisplay(C):->
	%Wordt aangeroepen voor het tekenen van de juiste bitmap

	%gp3: lot of changes

	C?bitmap_member->>image(C?stateImage),
	C?bitmap_member->>psdef(C?imageName),
	%gp3: the easiest way is not to hide the comment mark
	%but to set it to empty text
	%otherwise, bitmap placement changes and handles are a mess
	if
		0 = C?fragmentElement?remarks<<-size
	then
		C?comment_member->>string('')
	else
	(
		C?comment_member->>string('*')
	),
	C->>updateArgumentLinks.
%%	

%%
updateArgumentLinks(C):->
	%opnieuw koppelen van de argument links
	%dus eerst weg

	C->>disconnect(from_kind := arg),
	C->>disconnect(from_kind := arg1),
	C->>disconnect(from_kind := arg2),

	%en opnieuw maken, hangt van het soort calculus af
	if
		plus = C?fragmentElement<<-sign
	then (
		To_kind1 = arg,
		To_kind2 = arg %maakt niet uit waar de link heen loopt
		)
	else (
		To_kind1 = arg1,
		To_kind2 = arg2
		),

	Link1 *= link(link,To_kind1,line(0,0,0,0,second)),
	Link1->>colour(black),
	Link2 *= link(link,To_kind2,line(0,0,0,0,second)),
	Link2->>colour(black),
	C?arg1->>connect(C,Link1),
	C?arg2->>connect(C,Link2).
%%

%%
setArguments(C,
	Arg1: visualElement,
	Arg2: visualElement):->
	%Zet nieuwe argumenten voor deze relatie


	C->>removeArgs,

	C->>hyper(Arg1,arg1,calculus),
	Arg1->>checkImportant,	
	C->>hyper(Arg2,arg2,calculus),
	Arg2->>checkImportant,
	C->>checkDisplayed,
	C->>updateDisplay.
%%

%%
removeArgs(C):->
	%verwijder de argumenten die er zijn
	if
		OldArg1 =C<<-arg1
	then (
		C->>delete_hypers(arg1),
		OldArg1->>checkImportant
		),

	if
		OldArg2 = C<<-arg2
	then (
		C->>delete_hypers(arg2),
		OldArg2->>checkImportant
		).
%%

%%
arg1(C,Arg1: visualElement):<-
	Arg1 = C<<-hypered(arg1).
%%

%%
arg2(C,Arg2: visualElement):<-
	Arg2 = C<<-hypered(arg2).
%%

%%
saveLayoutInfo(C):->
	%gp3 1.0: overwrite of visualElement saveLayoutInfo: calculi save their position relative to
	%mid(arg1,arg2), to make it indepenend of the order of arguments

	if
		Arg1 = C<<-arg1
	then
		Arg1Point = Arg1<<-absolute_position
	else
		Arg1Point *= point(C?device?visible?left_side,C?device?visible?top_side),

	if
		Arg2 = C<<-arg2
	then
		Arg2Point = Arg2<<-absolute_position
	else
		Arg2Point *= point(C?device?visible?right_side,C?device?visible?bottom_side),	
		
	
	C?fragment->>layOutInfo(C?fragmentElement,
			    relPosition,
			    ?(C?absolute_position,minus,?(Arg1Point,mid,Arg2Point)),
				C?route),
	C?fragment->>layOutInfo(C?fragmentElement,
				hidden,
				C?hidden,
				C?route).
%
savedPosition(C,
	      Position: point
	     ):<-
	%gp3 1.0: overwrite of visualElement
	%gp3 1.0: could be relative to mid(arg1,arg2) (new style) or just absolute (old style)
	
	%we expect an Arg1 and Arg2, otherwise we just fail
	
	Position = ?(C?arg1?absolute_position,mid,C?arg2?absolute_position)<<-plus(?(C?fragment,layOutInfo,C?fragmentElement, relPosition,C?route)).
%%

%%
canHide(_C):->
	%calculi kunnen altijd verborgen worden
	true.
%%

%%
checkDisplayed(C):->
	%hide wanneer één vd twee argumenten hidden is of wanneer onze hidden flag staat
	if (
		@on = C?arg1<<-displayed ,
		@on = C?arg2<<-displayed,
		@off = C<<-hidden
		)
	then 
		Displayed = @on
	else
		Displayed = @off,

	if
		(\+ Displayed = C<<-displayed)
	then (
		C->>displayed(Displayed),
		C->>updateRelationMarker,
		ignore(C?arg1->>checkHiddenRelations),
		ignore(C?arg2->>checkHiddenRelations)
		),
	%en aangezien er weer relaties en calculi aan deze kunnen hangen sturen we het door
	C?calc_and_relations->>for_all(->>(@arg1,checkDisplayed)).
%%

%%
ensureVisible(C):->
	%zorgt dat we zichtbaar zijn, dat betekent bij een calculus dat beide argumenten zichtbaar moeten zijn
	%en onze hidden flag uit moet staan

	if
		@off = C<<-displayed
	then (
		C->>setHidden(@off),
		C?arg1->>ensureVisible,
		C?arg2->>ensureVisible
		).
%%

%%
checkDragMove(C,
	Change: point,
	Moved: chain):->
	
	%check of de calc moet moven: zitten we zelf niet, en onze argumenten wel in 
	%Moved?
	
	%gp3 1.0: fails if no new movement, for looping in viewEditor->dragMultiMove
	\+ Moved->>member(C),
	Moved->>member(C?arg1),
	Moved->>member(C?arg2),
	%dus als hier gekomen: toevoegen aan moved, en verplaatsen
	Moved->>append(C),
	ignore(C->>relative_move(Change)).
%%


%%
tooltipContent(VE,S: string):<-
	%gp3 1.4 rewrite of the tooltip
	
	El = VE<<-fragmentElement,
	S = VE<<-checkComments(El?relevantComments).
%%


:-pce_end_class.