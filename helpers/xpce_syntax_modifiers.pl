/*
Modified pce syntax.
Object->>Message : send(Object,Message)
Object->>Message(Arg1,Arg2) : send(Object,Message(Arg1,Arg2))
Var = Object <<- Message : get(Object,Message,Var)
Var = Object <<- Message(Arg1,Arg2) : get(Object,Message(Arg1,Arg2),Var)
Object <<- Message : get(Object,Message,_)
Object <<- Message(Arg1,Arg2) : get(Object,Message(Arg1,Arg2),_)
Object->+Message : send_super(Object,Message) (idem met args)
Var = Object +<- Message: get_super(Object,Message,Var) (idem met args en/of zonder Var)
Object *= class : new(Object,class)
Object *= class(Arg1,Arg2) : new(Object,class(Arg1,Arg2))

Alle operatoren zijn via goal_expansion gedefinieerd en je ziet ze dus niet
in de tracer. Alle operatoren behalve ->+ en +<- zijn er ook voor de prompt/
commandline: aparte clauses.
*/

:-op(710,xfx,->>).
:-op(710,xfx,<<-).
:-op(710,xfx,->+).
:-op(710,xfx,+<-).
:-op(710,xfx,*=).

%Object->>Message: via goal_expansion voor compileren en een rechtstreekse versie voor op de prompt
goal_expansion(->>(Object,Message),send(Object,Message)).
->>(Object,Message):-
	%Object->>Message
	Object->>Message. %wordt nu in bovenstaande versie gecompileerd


%Var = Object<<-Message of Object<<-Message
%wederom via goal_expansion en rechtstreeks

goal_expansion(<<-(Assign,Message),get(Object,Message,Var)):-
	nonvar(Assign),
	Assign = ( Var = Object),!.
goal_expansion(<<-(Object,Message),get(Object,Message,_)).

<<-(Assign,Message):-
	nonvar(Assign),
	Assign = (Var = Object),!,
	get(Object,Message,Var).
<<-(Object,Message):-
	Object<<-Message. %gecompileerd

%Object->+Message etc.: send_super(Object,Message), dus alleen in methode
%geen versie voor commandline

goal_expansion(Object->+Message,send_super(Object,Message)).

%Var = Object+<- Message etc: get_super(Object,Message,Var). Geen commandline
%versie

goal_expansion(+<-(Assign,Message),get_super(Object,Message,Var)):-
	nonvar(Assign),
	Assign = ( Var = Object),!.
goal_expansion(+<-(Object,Message),get_super(Object,Message,_)).

%Var *= Class , Var *= Class(Args): via goal_expansion en rechtstreeks

goal_expansion(Var *= NewCall, new(Var,NewCall)).
*=(Var,NewCall):-
	Var *= NewCall. %compileert via hierboven

/***************PCE HELPERS***************/
:-pce_begin_class(
		  ->>,
		  message,
		  "Different symbol for message"
		 ).
:-pce_end_class.
