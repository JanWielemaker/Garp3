/*
Definitie garpRelation class
*/

%Abstracte class.
:-pce_begin_class(
		  garpRelation,
		  fragmentElement,
		  "Abstract parent of classes for relation in model fragments"
		 ).

variable(argument1Route,chain,none). %route van geïmporteerde fragmenten om vanuit
									%deze relatie bij het eerste argument te komen
variable(argument2Route,chain,none). %route van geïmporteerde fragmenten om vanuit
									%deze relatie bij het tweede argument te komen

%%
initialise(GR, MF: modelFragment,  Arg1: argument1 = fragmentElement, %gp3 made this more general
			Arg1Route: chain,
			Arg2 : argument2 = fragmentElement, %gp3 made this more general
			Arg2Route: chain,
			Remarks: string):->

	GR->+initialise(MF, Remarks),
	GR->>argument1(Arg1,Arg1Route),
	GR->>argument2(Arg2,Arg2Route).
%%

argument1(GR,Arg : fragmentElement, %gp3 made this more general
			ArgRoute: [chain]
	 ):->
	"Set argument 1 of the relation" ::
	%eigenlijk natuurlijk abstract

	%eerst de oude maar eens weg
	( H = GR<<-find_hyper(argument1)
        -> free(H)
        ; true
        ),
	GR->>hyper(Arg,argument1,garpRelation),
	if
		ArgRoute == @default
	then
		GR->>argument1Route(new(chain))
	else
		GR->>argument1Route(ArgRoute).
%%

%%
argument2(GR, Arg: fragmentElement, %gp3 made this more general
			ArgRoute: [chain]
	):->
	"Set argument 2 of the relation" ::

	( H = GR<<-find_hyper(argument2)
	-> free(H)
	;  true
	),
	GR->>hyper(Arg,argument2,garpRelation),
	if
		ArgRoute == @default
	then
		GR->>argument2Route(new(chain))
	else
		GR->>argument2Route(ArgRoute).
%%

%%
isArgument(GR,
	   Arg: fragmentElement, %gp3 made this more general
		ArgRoute: [chain]
          ):->
	"Succeeds if the argument is part of this relation" ::
	%Als de route niet is meegestuurd hoeft ie ook niet leeg te zijn,
	%hij wordt dan helemaal niet gecontroleerd

	( Arg = GR<<-argument1,
	  if
		(ArgRoute \== @default)
	  then
		ArgRoute->>equal(GR?argument1Route)
	)
	;
	( Arg = GR<<-argument2,
	  if
		(ArgRoute \== @default)
	  then
		ArgRoute->>equal(GR?argument2Route)
	).
%%


%%
argument1(GR,
	  Arg : fragmentElement* %gp3 made this more general
	 ):<-
	"Returns 1st argument or @nil" ::

	Arg = GR<<-hypered(argument1)
	;
	Arg = @nil.
%%

%%
argument2(GR,
	  Arg : fragmentElement* %gp3 made this more general
	 ):<-
	"Returns 2nd argument or @nil" ::

	Arg = GR<<-hypered(argument2)
	;
	Arg = @nil.
%%

%%
argument1Route(GR,
	R : chain):->
	"Set argument1route" ::

	GR->>slot(argument1Route,R?copy).
%
argument1Route(GR,
	R : chain):<-
	"Get argument1Route" ::

	R = ?(GR,slot,argument1Route)<<-copy.
%%

%%
argument2Route(GR,
	R : chain):->
	"Set argument2route" ::

	GR->>slot(argument2Route,R?copy).
%
argument2Route(GR,
	R : chain):<-
	"Get argument2Route" ::

	R = ?(GR,slot,argument2Route)<<-copy.
%%

%%
otherArgument(GR,
	Arg :  fragmentElement, %gp3 made this more general
	ArgRoute: [chain],
	Other:  fragmentElement*):<-
	"Return the other argument given one" ::
	
	Arg = GR<<-argument1,
	(
		ArgRoute == @default
	;
		ArgRoute->>equal(GR?argument1Route)
	),
	Other = GR<<-argument2.
%
otherArgument(GR,
	Arg,
	ArgRoute,
	Other):<-
	
	Arg = GR<<-argument2,
	(
		ArgRoute == @default
	;
		ArgRoute->>equal(GR?argument2Route)
	),
	Other = GR<<-argument1.
%%

%%
otherArgumentRoute(GR,
	Arg :  fragmentElement, %gp3 made this more general
	ArgRoute: [chain],
	OtherRoute: chain):<-
	"Return the route of the other argument given one" ::
	
	Arg = GR<<-argument1,
	(
		ArgRoute == @default
	;
		ArgRoute->>equal(GR?argument1Route)
	),
	OtherRoute = GR?argument2Route.
%
otherArgumentRoute(GR,
	Arg,
	ArgRoute,
	OtherRoute):<-
	
	Arg = GR<<-argument2,
	(
		ArgRoute == @default
	;
		ArgRoute->>equal(GR?argument2Route)
	),
	OtherRoute = GR?argument1Route.
%%


%%%
/*
isMoreGeneralRelation(Arg1,Route1,Arg2,Route2):
gp3 0.2:
succeeds if the given relation (as defined thru its arguments because the relation might not
exist yes)more general than receiver. It is a relation between exactly
the same arguments as receiver (same route, up to the more general MF and the MF of receiver imports the
arguments through the same route). NB we are talking about instances of instances (when you import
the same MF 3 times, you have 3 instances of each instance. So you need the routes to see which is which).
*/

isMoreGeneralRelation(R, Arg1_in: fragmentElement, Route1_in: chain,
						  Arg2_in: fragmentElement, Route2_in: chain
					  ):->
	%gp3 0.2
	%the arguments must be the same object, but this can be reversed
	(
		Arg1_in = R<<-argument1,
		Arg2_in = R<<-argument2,
		
		Arg1 = Arg1_in,
		Arg2 = Arg2_in,
		Route1 = Route1_in,
		Route2 = Route2_in
	;
		%other way round?
		Arg1_in = R<<-argument2,
		Arg2_in = R<<-argument1,
		
		Arg1 = Arg2_in,
		Arg2 = Arg1_in,
		Route1 = Route2_in,
		Route2 = Route1_in
	),

	MyRoute1 = R<<-argument1Route,
	MyRoute2 = R<<-argument2Route,
	MyRoute1->>is_tail(Route1), %we point to the same version of the argument
	MyRoute2->>is_tail(Route2),
	SpecializedRoute1 = MyRoute1<<-sub(0,MyRoute1?size - Route1?size), %the part before Route1
	SpecializedRoute2 = MyRoute2<<-sub(0,MyRoute2?size - Route2?size),
	\+ SpecializedRoute1->>empty, %not empty, than we would be in the same MF
	SpecializedRoute1->>equal(SpecializedRoute2). %same path to the 2 arguments at the level of specialized relation (receiver)

%%%

/*
isSpecializedRelation(Arg1,Route1,Arg2,Route2):
gp3 0.2:
succeeds if the given relation is more specialized than receiver, but between exactly the same arguments. This is the isMoreGeneralRelation inversed.
*/

isSpecializedRelation(R, Arg1_in: fragmentElement, Route1_in: chain,
						  Arg2_in: fragmentElement, Route2_in: chain
					  ):->
	%gp3 0.2
	%the arguments must be the same object, but this can be reversed
	(
		Arg1_in = R<<-argument1,
		Arg2_in = R<<-argument2,
		
		Arg1 = Arg1_in,
		Arg2 = Arg2_in,
		Route1 = Route1_in,
		Route2 = Route2_in
	;
		%other way round?
		Arg1_in = R<<-argument2,
		Arg2_in = R<<-argument1,
		
		Arg1 = Arg2_in,
		Arg2 = Arg1_in,
		Route1 = Route2_in,
		Route2 = Route1_in
	),

	MyRoute1 = R<<-argument1Route,
	MyRoute2 = R<<-argument2Route,
	Route1->>is_tail(MyRoute1),
	Route2->>is_tail(MyRoute2),
	SpecializedRoute1 = Route1<<-sub(0,Route1?size - MyRoute1?size),
	SpecializedRoute2 = Route2<<-sub(0,Route2?size - MyRoute2?size),
	\+ SpecializedRoute1->>empty, %not empty, than we would be in the same MF
	SpecializedRoute1->>equal(SpecializedRoute2). %same path to the 2 arguments at the level of specialized relation (arguments)
	
/*
sameArguments(Arg1,Route1,Arg2,Route2):
gp3 0.2
succeeds if the receiver and the given relation (as defined by arguments + routes) have 
exactly the same arguments, including the routes
*/

sameArguments(R, Arg1_in: fragmentElement, Route1_in: chain,
						  Arg2_in: fragmentElement, Route2_in: chain
					  ):->
	%gp3 0.2
		Arg1_in = R<<-argument1,
		Arg2_in = R<<-argument2,
		Route1_in->>equal(R?argument1Route),
		Route2_in->>equal(R?argument2Route)
	;
		Arg1_in = R<<-argument2,
		Arg2_in = R<<-argument1,
		Route1_in->>equal(R?argument2Route),
		Route2_in->>equal(R?argument1Route).


:-pce_end_class.
