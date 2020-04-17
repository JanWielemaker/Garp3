/*****************************************************************************************************
Garp3 0.1 Based on main source of visigarp (main.pl). This file is split in parts and edited.
All visiualize source based on visigarp 2.05, designed and implemented by Anders Bouwer 18/08/1999
Original copyright: (C) 2004 University of Amsterdam. All rights reserved.

Now:
PART OF Garp3. SEE COPYRIGHT NOTICE.
*******************************************************************************************************/

%loaded into module (namespace) visualize

:- pce_begin_class(my_sketch_connection(node, node, sketchElement, link, handle, handle), my_connection).

initialise(MyC, FromNode:node, ToNode:node, SE:sketchElement, LName:link) :->
	"Create from name"::
   
	MyC->>hyper(SE, sketchElement),
	send(MyC, send_super, initialise, FromNode, ToNode, LName).


dragMove(_MyC, _, _, _):->
        true.



%%
sketchElement(VE,
		FE : any
	       ):<-
	"Return the associated sketch element" ::

	FE = VE<<-hypered(sketchElement).
%%


%%
isMoveTogetherSub(_MyC,
	_Other: sketchVisualElement):->
	"Succeeds when argument will move when receiver moves (moveable sub or related sub)" ::
	%gebruikt door vieweditor, dus moveable sub of moveTogetherSub
        true.


saveLayoutInfo(_MyC):->
        true.


:- pce_end_class.

%
