/*
Wat prolog helpertjes

Some prolog helpers. NO MODULE ALL GLOBAL
Also some stuff defined by goal_expansion, and by op.
*/

%the todo predicate. Gives a message with the file and line where something is todo and succeeds.
%done by goal_expansion to make sure the calling clause has a choice point and therefore an existing frame.

goal_expansion(todo,(!,prolog_helpers:do_todo;true)). %create choice point in calling predicate

%if then else is er wel, doch als goal_expansion gedefinieerd

:-op(900,fy,not). %dan is ie er tenminste

:-op(750,fx,user:if).
:-op(740,xfx,user:then).
:-op(750,fx,user:unless).
:-op(740,xfx,user:do).
:-op(730,xfy,user:else).

%%%%%%%%if/then/else%%%%%%%%%%
%if/then/else gaat via goal_expansion, zodat het niet interfereert met modules
%en vooral met goals in de TrueAction en FalseAction (die niet expanded worden
%wanneer if/then/else als clause is gedefinieerd)

user:goal_expansion(if(then(Condition,else(TrueAction,FalseAction))),
	(Condition -> TrueAction ; FalseAction)):-!.
user:goal_expansion(if(then(Condition,TrueAction)),
	(Condition -> TrueAction ; true)).
	
%unless/do/else is the same, but then do action if not true
user:goal_expansion(unless(do(Condition,else(FalseAction,TrueAction))),
	(Condition -> TrueAction ; FalseAction)):-!.
user:goal_expansion(unless(do(Condition,FalseAction)),
	(Condition -> true ; FalseAction)).
	
%%%%%%%chain_member%%%%%%%%%%%%
%helper voor backtrackbaarheid over pce-chain members als de var voor de member niet ground is
%doet hetzelfde als member op een prolog lijst
chain_member(Element,ChainExpression):-
	ground(Element),!,
	ChainExpression->>member(Element).

chain_member(Element,ChainExpression):-
	%niet ground, dus de lange manier
	chain_list(ChainExpression,List),!,
	member(Element,List).

%%%%%%%%%constrainDistance(G1,G2,C)
%maak een pce constraint om 2 pce graphicals altijd samen te laten bewegen
%de /3 versie geeft als laatste de constraint terug, de /2 versie doet dat niet

constrainDistance(G1,G2,C):-
	%we berekenen het huidige verschil in afstand, en dat wordt
	%dus het relatieve verschil

	X1 = G1<<-left_side,
	X2 = G2<<-left_side,
	Y1 = G1<<-top_side,
	Y2 = G2<<-top_side,

	%dus de constraint wordt dan heel simpel:
	C *= constraint(G1,G2,spatial(xref = x, yref = y,
							xref = x + (X1 - X2), yref = y + (Y1 - Y2))).
%
constrainDistance(G1,G2):-
	constrainDistance(G1,G2,_).
%%

%%%%%list_chain(List,Chain)
%%Zet een lijst (bijvoorbeeld met argumenten uit een varaantal arglist)
%%om in een nieuwe chain

list_chain(List,Chain):-
	NewChainCall =.. [chain|List],
	Chain *= NewChainCall.
%%

% first in list

first(H, [H|_]).
% last is system predicate in SWI prolog


%FL July 2004: for prolog versions older then 5.20 reverse A and B arguments.
%TODO: this should be in some helpers fil
common_last(A, B):-
    last(A, B).

common_select(A, B, C) :-
    select(B, A, C).

common_shell(Command):-
    shell(Command).

common_shell(Command, R):-
    shell(Command, R).

recorded_erase(Key, Structure):-
    recorded(Key, Structure, Reference), 
    erase(Reference).
    
%%%%%%%%min/max%%%%%%55
%min/3 and max/3 seem not te be defined??

%
%vg: max is already defined in ISO-prolog
%
%gp0.1 SIHW 20050506: well, not in mine. We redefine
 max(X,Y,Z):-
 	Z is max(X,Y).
 min(X,Y,Z):-
 	Z is min(X,Y).

%%%%%%%todo%%%%%%
%%slaag. Geeft duidelijk aan dat het nog moet gebeuren
%%

do_todo:-
	prolog_current_frame(CF),
	prolog_frame_attribute(CF,parent,PF),
	prolog_frame_attribute(PF,clause,CL),
	prolog_frame_attribute(PF,goal,Goal),
	clause_property(CL,file(FileName)),
	clause_property(CL,line_count(Line)),!,
	writef('TODO: check goal %w\nFile %w:%w\n',[Goal,FileName,Line]).

do_todo:-!,
	writeln('TODO! (Search for todo or put a spypoint on do_todo/0, could not determine the spot myself'),
	true.

%%where(pred/par).
%%prints sourcefile

where(Mod:Pred/Par):-!,
	length(PL,Par),
	P =.. [Pred|PL],
	source_file(Mod:P,F),
	write_ln(F).
	
where(Pred/Par):-
	Pred \= (_:_),
	where(user:Pred/Par). 


% String functions
string_replace(Input, Target, ReplaceBy, Output) :-
    string_replace_instance(Input, Target, ReplaceBy, AlmostOutput),
    (
	string_replace(AlmostOutput, Target, ReplaceBy, Output)
    ;
	Output = AlmostOutput
    ).

string_replace_instance(Input, Target, ReplaceBy, Output) :-
    Target \== ReplaceBy, % Prevent endless loops
    sub_string(Input, Start, Length, After, Target),
    sub_string(Input, 0, Start, _, First),
    LastStart is Start+Length,
    sub_string(Input, LastStart, After, _, Last),
    string_concat(First, ReplaceBy, Head),
    string_concat(Head, Last, Output).

