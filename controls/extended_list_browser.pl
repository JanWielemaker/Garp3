/*
Definitie uitgebreide implementatie van een list_browser 
Verschillen:
-geen deselectie
<-selection geeft @nil bij geen selectie ipv falen
-Handelt keyup en keydown af, stuurt daarbij de select_message
-verschillende helpers, zoals <-memberObjects
-changeable: standaard @on, als op @off gezet wordt elke change_selection geweigerd

Part of garp3
Most code homer, some gp3 extensions
*/

:-pce_begin_class(
		  extendedListBrowser,
		  list_browser,
		  "extended list_browser with some different behaviour"
		 ).

variable(changeable,bool,both,"Can change selection state").		 
%%
initialise(ELB, D: dict=[dict], W : width=[int], H : height=[int]):->
	ELB->>slot(changeable,@on),
	ELB->+initialise(D,W,H).

%%
list_top(ELB,
	T : int):<-
	"Return the top_side of the list instead of that of the list_browser" ::
	%maar wel in het parent device

	TS = ELB<<-top_side,
	ITS = ELB?image<<-top_side, %tov de bovenkant vd lijst
	T is TS + ITS.
%%

%%
pixelWidth(ELB, W:int):<-
	%gp3 0.2
	%get the width in pixels
	
	W = ELB?area<<-width.
%%

%%
pixelWidth(ELB, W:int):->
	%gp3 0.2
	%set the width in pixels
	
	ELB->>'_size'(size(W,ELB?pixelHeight)).
%%

%%
pixelHeight(ELB, H:int):<-
	%gp3 0.2
	%get the height in pixels
	
	H = ELB?area<<-height.
%%

%%
pixelHeight(ELB, H:int):->
	%gp3 0.2
	%set the height in pixels
	
	ELB->>'_size'(size(ELB?pixelWidth,H)).
%%

%%
change_selection(ELB,
	 A : action = {set,toggle,extend,clear, cancel}, 
	 C : context=[dict_item]):->
	"Skip clear" ::

	@on = ELB<<-changeable,
	
	if
		A \== clear
	then
		ELB->+change_selection(A,C).	
%%

%%
selection(ELB,
	S : 'chain|dict_item*'
	):<-
	"Return @nil when nothing selected" ::

	S = ELB+<-selection
	;
	S = @nil.
%%

%%
prevItem(ELB,
	I : dict_item):<-
	"Single selection: return the item before the selection, fail if unavailable" ::

	S = ELB+<-selection, %faal als er niet is
	S->>instance_of(dict_item), %geen chain
	I = ELB<<-itemBefore(S).
%%

%%
itemBefore(ELB,
	I : dict_item,
	B : dict_item):<-

	"Return the item before the given item, fail if unavailable" ::

	ELB->>member(I),
	Index = ELB?members<<-index(I),
	Index > 1,
	B = ELB?members<<-nth1(Index - 1).
%%

%%
nextItem(ELB,
	I : dict_item):<-
	"Single selection: return the item after the selection, fail if unavailable" ::

	S = ELB+<-selection, %faal als er niet is
	S->>instance_of(dict_item), %geen chain
	I = ELB<<-itemAfter(S).
%%

%%
itemAfter(ELB,
	I : dict_item,
	A : dict_item):<-
	"Return the item after the given item, fail if unavailable" ::

	ELB->>member(I),
	Index = ELB?members<<-index(I),
	Num = ELB?members<<-size,
	Index < Num,
	A = ELB?members<<-nth1(Index + 1).
%%
	
%%
insert_before(ELB,
	B : before = dict_item,
	Item : dict_item):->
	"Insert first arg before second arg. Like dict->insert_after" ::

	if
		Prev = ELB<<-itemBefore(Item) %als het item niet de eerste is dan
	then
		ELB->>insert_after(B,Prev)	%dan achter de vorige
	else
		ELB->>insert_after(B,@nil). %anders als eerste
%%

%%
memberObjects(ELB,O: chain):<-
	%geeft een chain met alle objects in de lijst terug
	%itt members, dat een chain met alle dict_items teruggeeft
	
	O = ELB?members<<-map(@arg1?object).
%%

%%
key(ELB,K: name):->
	%check op een paar keys
	pl_key_ELB(ELB,K)
	;
	ELB->+key(K).
%
pl_key_ELB(ELB,cursor_up):-
	ELB->>selection(ELB?prevItem),
	if
		(\+ @nil = ELB<<-select_message)
	then
	(
		@receiver->>assign(ELB,local),
		ELB?select_message->>forward(ELB?selection)
	).
%
pl_key_ELB(ELB,cursor_down):-
	ELB->>selection(ELB?nextItem),
	if
		(\+ @nil = ELB<<-select_message)
	then
	(
		@receiver->>assign(ELB,local),
		ELB?select_message->>forward(ELB?selection)
	).

%%
:-pce_end_class.
		  

