/*
Definitie staticMarker class: abstracte class voor weergave van statische markeringen
(valueMarker en hiddenRelationMarker)

updateDisplay en doDisplay e.d. doen het niet gebruik setMarker voor tekenen. Natuurlijk kan bij subklasse eventueel
een updateDisplay worden gemaakt
*/


:-pce_begin_class(staticMarker,
		  visualElement,
		  "abstract class for display of markers next to elements"
		 ).
%%
%%
doDisplay(_SM,
	_Point : default_point = point,
	_Device : device = [device]
       ):->
	"Not to be used with this element" ::

	fail.
%%


%%savedPosition faalt (maar savedHidden wordt niet overschreven)
savedPosition(_SM,
	      _Position: point):<-
	fail.
%%

%%
setMarker(SM,
	NextTo: visualElement,
	Position: '{left,right}'):->
	"Point  Marker to the given element" ::

	%eventueel oude dus weg
	if
		Old = SM<<-hypered(markerConstraint)
	then
		free(Old), %weg ermee

	Y = 	NextTo?area?center?y - (SM?height / 2),

	if
		Position = left
	then
		X = NextTo?left_side - SM?width - 2 %gp3: added some spacing for left side
	else
		X = NextTo?right_side,

	NextTo?device->>display(SM),
	SM->>position(point(X,Y)),
	Constraint = NextTo<<-constrainSub(SM), %visualElement
				%SM wordt een "sub" van NextTo in de hierarchie van constraint objecten
				%zie isMoveTogetherSub bij visualElement en viewEditor->getMultiMove

	SM->>hyper(Constraint,markerConstraint),
	%een een gewone sub
	SM->>delete_hypers(super),
	%gp3 1.0: we have to set the hyper here, subAdded will not do this any more
	SM->>hyper(NextTo,super,sub),
	NextTo->>subAdded(SM,@off).
%%

:-pce_end_class.
