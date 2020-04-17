/*
Garp3: based on object_extensions.pl in old homer versions.
Now it contains just some stuff, some needed for a specific part of the app (design, engine, visualize),
some global.

object_extensions.pl: Uitbreidingen van klasse object (of subclasses) om
de zaken wat soepeler te laten verlopen.
Sommige uitbreidingen staan ook bij de betreffende helper file (idle/
layout_hyper).

*/
:-module(object_extensions,[]).

:-pce_extend_class(char_array).

tab(C,Num: int, NewC: string):<-
	"Return a string with every line in the orignal tabed by the number of tabs" ::
	%en geen \n op het eind
	
	Tab *= string,
	Tab->>insert_character('\t',times:= Num),
	
	Lines = C<<-split('\n'),
	NewC *= string,
	EndLine *= var,
	EndLine->>assign('',global),
	
	Lines->>for_all(
		and(
			->>(NewC,append,EndLine),
			->>(NewC,append,Tab),
			->>(NewC,append,@arg1),
			assign(EndLine,'\n',global)
			)
		),
	Lines->>done.
%%

:-pce_end_class.

%en een helper voor string: opsplitsen voor meerdere regels
:-pce_extend_class(string).

split_lines(S,
	MaxChars: int,
	Split: string):<-

	MaxChars > 1,
	%beetje flauwe manier van aanroepen
	WorkString *= string,
	pl_split_lines(S,WorkString,MaxChars),
	%en aan het begin zit nu een newline: eraf
	Split = WorkString<<-sub(1).	
%
pl_split_lines(String,Split,MaxChars):-
	Split->>newline, %sowieso nodig
	S = String<<-size,
	if
		S =< MaxChars
	then
		Split->>append(String) %klaar
	else (
		if
			LastSpace = String<<-rindex(' ',MaxChars - 1)
		then (
			Split->>append(?(String,sub,0,LastSpace)),
			object_extensions:pl_split_lines(?(String,sub,LastSpace + 1),Split,MaxChars)
			)
		else ( %maar gewoon ophakken dan
			Split->>append(?(String,sub,0,MaxChars)),
			object_extensions:pl_split_lines(?(String,sub,MaxChars),Split,MaxChars)
			)
		).
%%
:-pce_end_class.

/*************chain aanpassingen*******************/

:-pce_extend_class(chain).
/*
for_all_pairs: voor elk paar uit de twee chains wordt code uitgevoerd, gestopt als het faalt.
Hierbij is @arg1 natuurlijk element uit ontvanger en @arg2 element uit andere lijst.
Faalt wanneer de tweede chain korter is dan de eerste
*/

for_all_pairs( C,
	O : other_chain = chain,
	Code :  code):->
	"Succeeds if code succeeds for every pair at the same index in the other chain" ::

	I *= number(1),
	C->>for_all(and(->>(Code,forward,@arg1,?(O,nth1,I)),
				->>(I,plus,1))).

/*
is_tail(C,O: other_chain).
gp3 0.13: succeeds of the ordered members of O are at the tail of C.
(needed for route checking)
*/

is_tail(C, O: other_chain = chain):->
	"Succeeds if the members of other_chain are at the end of receiver" ::
	
	%copy them
	CC = C<<-copy,
	OC = O<<-copy,
	%and run some code
	pl_is_tail(CC,OC).
%
pl_is_tail(_CC,OC):-
	OC->>empty. %done
%
pl_is_tail(CC,OC):-
	EL = CC<<-tail, 
	EL = OC<<-tail, %same
	CC->>delete_tail,
	OC->>delete_tail,
	pl_is_tail(CC,OC). %continue
%%

%%
is_head(C,O: other_chain = chain):->
	"Succeeds if hte members of other_chain are at the beginning of receiver" ::
	
	\+ O->>empty,
	HeadChain = C<<-sub(0,O?size),
	HeadChain->>equal(O).
%%
	
/*
is_subchain(C, O: other_chain).
gp3 0.3: Succeeds if the elements of the other_chain form a sub chain of the receiver.
(needed for route checking)
*/
is_subchain(C, O: other_chain = chain):->
	"Succeeds if the elements of the other_chain form a sub chain of the receiver - assumes no duplicates" ::
	%assumes no duplicates!
	
	%get the first one
	First = C<<-index(O?head),
	I *= number(First),
	%size ok?
	Needed *= number(I + O?size - 1),
	Needed->>less_equal(C?size),
	O->>for_all(
		and(
			?(C,nth1,I) == @arg1,
			->>(I,plus,1)
		)
	).
%%

%%
join(C,Sep: separator = char_array, Joined: string):<-
	%gp3 1.4: create a string of the members of this chain (should be char_arrays), with the given separator between them
	%chain(1,2,3)<<-join(', ') will return '1, 2, 3'
	
	Joined *= string,
	SepVar *= var(value:=''),
	C->>for_all(
		and(
			->>(Joined,append,SepVar),
			->>(Joined,append,@arg1),
			assign(SepVar,Sep,global)
		)
	).
%%
	
:-pce_end_class.

/*****************object aanpassingen*******************/
:-pce_extend_class(object).

/**********all_named_hypers*************
Geeft een chain met daarin alle hypers met opgegeven naam
die voldoen aan de opgegeven testcode. Testcode kan @arg1
bevatten voor een hyper, verder niet.
*****************************************/

:-pce_group(meta).

all_named_hypers(Object,N : hyper_name = [name], T : test = [code], C : chain) :<-
	"Return chain with all hypers with given name" ::

	new(C,chain),
	%bouw de test message op, daar moeten we aan sleutelen
	CheckMessage = not(message(C,member,@arg2)),
	(   T == @default
	->  Test = CheckMessage
	;   Test = and(CheckMessage,T)
	),
	send(Object,all_named_hypers_sub,N,Test,C).

all_named_hypers_sub(Object,Name,Test,Chain):->
	"Internal" ::
	(   get(Object,find_hyper,Name,Test,Hyper)
	->  send(Chain,append,Hyper),
	    send(Object,all_named_hypers_sub,Name,Test,Chain)
	;   true
	).

/**********all_named_hypered************
Geeft als resultaat een chain met daarin alle via de opgegeven naam (en 
eventueel testcode) gehyperde objecten. Testcode kan weer @arg1 bevatten
voor een hyper en verder niet.
***************************************/
:-pce_group(meta).

all_named_hypered(Object,N : hyper_name = [name], T : test = [code], C : chain) :<-
	"Return chain with all <- hypered objects" ::

	get(Object,all_named_hypers,N,T,Hypers),
	new(C,chain),
	send(Hypers,for_all,if(@arg1?to == Object,
			       ->>(C,append,@arg1?from),
			       ->>(C,append,@arg1?to))).


/************hyper********************
send(Object,hyper,To,FName,BName) is hetzelfde als 
new(_,hyper(Object,To,FName,BName)), alleen logischer opgeschreven (vind ik).
***************************************/

%%
hyper(Object,
      To: to=object,
      Forward: forward = name, 
      Backward: backward = [name]
      ):->
	"Attach a hyper from receiver to other object" ::

	new(_,hyper(Object,To,Forward,Backward)).
%%

%%
%convert_old_slot: this is a heavy patch done
%to make sure information in old slots is available to code from
%garpModel->versionPatch, but is not saved again we just put all old slots in the old_slots attribute
%so we put all old slots we find while loading (a model I guess) 
%in a hash_table linking the objectref (not the object)
%to a hash_table of slot-value pairs, which now is defined in startup.pl
%@old_slots

%1.4 JJ

convert_old_slot(O,Slot,OldValue):->
	unless
		T = @old_slots<<-member(O?object_reference)
	do
	(
		T *= hash_table,
		@old_slots->>append(O?object_reference,T)
	),
	T->>append(Slot,OldValue),
	@pce->>write_ln('Saved old slot value',O,'->',Slot,'in @old_slots under',O?object_reference).
	
		
:-pce_end_class.

/****************hyper aanpassingen*******************
Het probleem is dat een hyper een referentie naar een object geeft
en dat dat object dus niet bevrijdt wordt omdat er nog een hyper aan zit.
Met unreferenced_object kan gecheckt worden of dat zo is. In dat geval kan de hyper
dus bevrijdt worden (en gaat het vrije object dus wel mee).
************************************************************/
:-pce_extend_class(hyper).
%%

unreferenced_object(H):->
	"Succeeds if one of the two connected object only exists because of the hyper"::

	H->>unreferenced_to
	;
	H->>unreferenced_from.
%
unreferenced_to(H):->
	"Succeeds if the to part of the hyper only exists because of the hyper" ::
	pl_unreferenced_object(H?to).
%
unreferenced_from(H):->
	"Succeeds if the from part of the hyper only exists because of the hyper" ::
	pl_unreferenced_object(H?from).
%
pl_unreferenced_object(Object):-
	get(Object,references,1),
	get(Object,lock_object,@off),
	get(Object,protect,@off).
%%

:-pce_end_class.

/*****************frame aanpassingen*********************
Enkele handige tooltjes voor frames
*********************************************************/
:-pce_extend_class(frame).

%%
msgBox(F, 
       T: text = char_array,
       Type: [{notification,alarm,confirm}],
       SmallFont: [bool]
       ) :->
	"Show modal message box" ::

	%gp3 changed this to helper_dialog notificationDlg
	%no label needed anymore
	%gp3 0.3: added confirm.
	%when ok clicked this call succeeds, when something else clicked
	%(cancel in confirm mode) this call fails
	default(Type,notification,RealType),
	Dlg *= notificationDlg(F,RealType,T,SmallFont),
	Result = Dlg<<-confirm_centered(F?area?center),
	Dlg->>destroy,
	!, Result = @on.
%%	

:-pce_end_class.


/*******************tuple aanpassingen***********************/

:-pce_extend_class(tuple).

equal(T,To: tuple):->
	"Succeeds if both elements are equal" ::
	
	T?first->>equal(To?first),
	T?second->>equal(To?second).

:-pce_end_class.
	
/**************** hash_table ******************************/
%gp3

:-pce_extend_class(hash_table).

keys(H,Keys: chain):<-
	"Return a new chain containing all keys in the table (garp3)" ::
	%gp3 03.14
	
	Keys *= chain,
	H->>for_all(->>(Keys,append,@arg1)).
%%

%%
values(H, Values: chain):<-
	"Return a new chain containing all values in the table (garp3)" ::
	%gp3 03.14
	
	Values *= chain,
	H->>for_all(->>(Values,append,@arg2)).
%%
:-pce_end_class.
