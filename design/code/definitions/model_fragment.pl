/*
Definitie modelFragment class

based on homer code, so most comments in dutch. gp3 code only where mentioned
*/

:-pce_begin_class(
		  modelFragment,
		  object,
		  "Definition of model fragments"
		 ).

variable(name_translatable,translatable,both,"Name of the object"). %gp3 1.4: added this, name is just a get/set method now
variable(remarks_translatable, translatable, both, "Remarks on this object"). %gp3 1.4: added this, remarks is just a get/set method now


variable(elements,chain,get,"List of subelements defined in this fragment").

variable(parentReferences,chain,none,"List of importedFragment objects referencing parents"). %gp3 made this none and made a get call for copy: we had bugs after refering to the real chain

variable(layOutTable, hash_table, get, "Hash_table for layoutinfo"). %eerste nivo

variable(active, bool := @on, both, "Modelfragment is used in simulation"). %gp3 0.3.11, modeldefinition 5

/*
het nivo is:
	MF?layOutTable -> key=object
					  value=hash_table -> key=route
										  value=hash_table -> key = infonaam
															  value= any
*/

%%
initialise(MF, Name: name = name,
			Remarks: remarks = [string], T: translator = [translator]):->

	%gp3 1.4 added translator argument, only needed when initializing model fragments withoud a @model
	MF->+initialise,
	default(T,@model?translator,TR),
	MF->>name_translatable(?(TR,getTranslatable,unique)),
	MF->>name(Name),
	default(Remarks,'',RR),
	MF->>remarks_translatable(?(TR,getTranslatable,empty)),
	MF->>remarks(RR),
	send(MF,slot,elements,new(chain)),
	MF->>slot(parentReferences,new(chain)),
	MF->>slot(layOutTable,new(hash_table)).
%%

%%
unlink(MF):->
	%verwijder het fragment: we maken eventjes wat objecten vrij

	MF?elements->>for_all(->>(@arg1,free)),
	MF?parentReferences->>for_all(->>(@arg1,free)),
	MF?layOutTable->>free.
%%

%%%%% wrapping translatables %%%%%%%%
%%
name(MF,Name : name):->
	"Set the name, internal" ::

	%is now a wrapper around name_translatable
	MF?name_translatable->>value(Name?makeGarp).
%
name(MF, Name: name):<-
	%get the name
	Name = MF?name_translatable<<-value.
%%


%%
remarks(MF, Remarks: string):->
	"Set a copy of the given string as remarks" ::

	%is now a wrapper around remarks_translatable

	MF?remarks_translatable->>value(?(Remarks,strip,both)).
%
remarks(MF,	Remarks : string
       ):<-
	"Return a copy of the remarks" ::

	Remarks = MF?remarks_translatable?value<<-copy.
%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
systemTypeName(_MF, Type: name):<-
	Type = 'model fragment'.
%%

%%
displayTypeName(_IS, Type: name):<-
	%gp3 display name is either scenario or model fragment
	Type = 'model fragment'.
%%

%%
isInputSystem(_MF):->
	%faalt: overschreven in subclass inputSystem
	fail.
%%

addParent(MF, Parent: modelFragment):->
	"Add a parent to the list of parents" ::
	%natuurlijk ingepakt in een importedFragment
	%dit is de MULTIPLE INHERITANCE VERSIE (zie datamodel)
	%maar hij zet de single inheritance parent hyper

	IF *= importedFragment(MF, Parent,new(string)),
	%get the real one
	PR = MF<<-slot(parentReferences),
	PR->>append(IF),
	%en de parent krijgt een hyper naar dit kind
	Parent->>hyper(MF,child,parent).
%%


%%
addChild(MF, Child: modelFragment):->
	%voeg een kind toe, voor compabiliteit met hierarchicalObject

	Child->>addParent(MF).
%%

%%
removeParent(MF, Parent: modelFragment):->
	%verwijder alle importedFragment elementen in parentReferences
	%die naar deze parent verwijderen, faal als er geen een is
	%zie removeParentReference voor de hyperverwijdering

	Aantal = MF?parentReferences<<-size,
	MF?parentReferences->>for_all(
		if(@arg1?referencedFragment == Parent,
			->>(MF,removeParentReference,@arg1)
		)),
	\+ Aantal = MF?parentReferences<<-size. %faal als ongewijzigd
%%

%%
removeParentReference(MF, Ref: importedFragment):->
	%verwijder de opgegeven importedFragment uit de lijst met parentReferences, en verwijder de parent/child hyper

	?(MF,slot,parentReferences)->>delete(Ref), %work on the slot, not the copy
	H = Ref?referencedFragment<<-find_hyper(child,@arg3==MF), %via child, want parent is alleen voor de single-inheritance versie
	free(H).
%%

%%
children(MF, Children: chain):<-
	%geef lijst met alle child modelfragmenten terug
	%wanneer een MF tweemaal kind is, wordt het hier tweemaal gegeven

	Children = MF<<-all_named_hypered(child).
%%

sortedChildren(MF, Children: chain):<-
	%geeft dezelfde lijst als children terug, maar dan op naam gesorteerd

	Children = MF<<-children,
	Children->>sort(?(@arg1?name,compare,@arg2?name)).
%%

%%
parentReferences(MF, PR: chain):<-
	%gp3 0.3: return a copy instead of the slot

	PR = ?(MF,slot,parentReferences)<<-copy.
%%

%%
parents(MF, Parents: chain):<-
	%geeft lijst met alle parent modelfragmenten terug
	%wanneer een fragment tweemaal parent is, wordt ie ook tweemaal teruggegeven
	%voor de importedFragment objecten moet parentReferences gebruikt worden

	Parents = MF?parentReferences<<-map(@arg1?referencedFragment).
%%

%%
conditionalIn(MF,
	Fragments: chain):<-
	%geeft een lijst met alle fragmenten die dit fragment conditioneel inporteren
	%wanneer een fragment dat tweemaal doet, staat het tweemaal in de lijst
	%we vinden ze door alle referenties naar dit MF te checken of ze een fragment hyper hebben
	%want dat hebben parent-child links niet, maar conditionele importedFragments wel.

	Fragments *= chain,
	?(MF,all_named_hypered,fragmentReference)->>for_all(
		if(->>(Fragments,append,?(@arg1,hypered,fragment)))). %binnen if voor altijd slagen
%%

%%
isIncomplete(MF):->
	%slaagt als er een probleem is mbt de structuurregel: alle instanties (lokaal of geïmporteerd) van ENTITIES moeten met elkaar verbonden zijn (niet via agents) met configuraties of identities

	\+ MF->>isComplete. %omdraaien
%

isComplete(MF):->
	%helper bij isIncomplete, deze slaagt als dit fragment goed bevonden is
	%(dat betekent niet dat een parent of applies_to niet fout kan zijn)

	TeCheckenConditionals = ?(MF,findElements, garpInstance)<<-find_all(
			and(
				->>(@arg1?entity,instance_of,entity),
				->>(@arg1,isCondition)
				)),
	DirectImports = MF?parentReferences<<-find_all(
		not(->>(@arg1?referencedFragment?parentReferences,empty))),
	DirectImports->>merge(?(MF,findElements,importedFragment)),


	%maar alleen de MFs die ook zelf weer instanties hebben of importeren
	%zijn van belang
	TeCheckenConditionals->>merge(
		?(DirectImports,find_all,
			->>(@arg1?referencedFragment,
				checkAllUp,
				->>(@arg1?elements,find,
					and(
						->>(@arg1,instance_of,garpInstance),
						->>(@arg1?entity,instance_of,entity)
						)
					),
				@on, @off %deze zelf ook, niet alleen parents
				))),

	%TeCheckenConditionals bevat nu conditionele instanties en objecten die er toe doen, en uiteindelijk in GevondenConditionals moeten komen

	if
		(\+ TeCheckenConditionals->>empty)
	then
	(
		GevondenConditionals *= chain,
				%bevat straks alle conditionals die goed zijn bevonden
		MF->>'_findConnected'(TeCheckenConditionals?head,@on,GevondenConditionals),
		%Gecheckt bevat alle gevonden instances en modelfragments, is dat ok?
		%zijn dat ze allemaal?
		TeCheckenConditionals->>for_all(
			->>(GevondenConditionals,member,@arg1))
	),

	%hetzelfde nu voor de givens

	TeCheckenGivens = TeCheckenConditionals<<-merge(?(?(MF,findElements, garpInstance),find_all,
			and(
				->>(@arg1?entity,instance_of,entity),
				not(->>(@arg1,isCondition))
				))),

	if
		(\+ TeCheckenGivens->>empty)
	then
	(
		GevondenGivens *= chain,
				%bevat straks alle givens die goed zijn bevonden
		MF->>'_findConnected'(TeCheckenGivens?head,@off,GevondenGivens),
		%Gecheckt bevat alle gevonden instances en modelfragments, is dat ok?
		%zijn dat ze allemaal?
		TeCheckenGivens->>for_all(
			->>(GevondenGivens,member,@arg1))
	).


'_findConnected'(MF,
	Instance: 'garpInstance|importedFragment',
	ConditionalOnly: bool,
	Found: chain):->
	%helper bij isComplete

	Found->>append(Instance), %deze is binnen (ook agents e.d.)
	Direct = MF<<-'_directConnected'(Instance,ConditionalOnly),
	Direct->>for_all(
		if(
			not(->>(Found,member,@arg1)),
				->>(MF,'_findConnected',@arg1,ConditionalOnly,Found)
			)),
	Direct->>done.
%
'_directConnected'(MF,
	InstanceOrImportedFragment: 'garpInstance|importedFragment',
	ConditionalOnly: bool,
	Result: chain):<-

	%helper bij isComplete
	%alle direct verbonden garpInstances (entity) of importedFragments
	%als een configuratie of identity deze instantie/IF als argument heeft
	%dan moet het andere argument in het resultaat: als dit een lokale instantie
	%is, dan de instantie, en anders het fragment dat we importeren (das dan weer verbonden)
	%als ConditionalOnly @on is, worden alleen instances gevonden die via
	%conditionele paden zijn verbonden. Dit maakt de instances zelf ook conditional
	%(of imported wat ook goed was)

	Result *= chain,
	MF?elements->>for_all(
		if(
			and(
				or(
					ConditionalOnly == @off,
					->>(@arg1,isCondition)
				),
				or(
					->>(@arg1,instance_of,configuration),
					->>(@arg1,instance_of,identityRelation)
				)
			),
			and(
				if(
					and(
						or(
							%importedFragment variant (mag dieper zitten)
							->>(@arg1?argument1Route,member,
								InstanceOrImportedFragment),
							%lokale instance variant:
							and(
								@arg1?argument1 == InstanceOrImportedFragment,
								->>(@arg1?argument1Route,empty)
								)
						)
					),
					->>(Result,append,
						when(
							->>(@arg1?argument2Route,empty),
							@arg1?argument2,
							@arg1?argument2Route?head
							)
						)
				),
				%of het andere argument?
				if(
					and(
						or(
							%importedFragment variant (mag dieper zitten)
							->>(@arg1?argument2Route,member,
								InstanceOrImportedFragment),
							%lokale instance variant:
							and(
								@arg1?argument2 == InstanceOrImportedFragment,
								->>(@arg1?argument2Route,empty)
								)
						)

					),
					->>(Result,append,
						when(
							->>(@arg1?argument1Route,empty),
							@arg1?argument1,
							@arg1?argument1Route?head
							)
						)
				)
			)
		)
	).

%%

%%
activated(MF):->
	%gp3 0.3.11
	%Succeed if this mf and all mfs it depends on are active (recursively)

	@on = MF<<-active,
	MF?parents->>for_all(->>(@arg1,activated)),
	?(MF,findElements,importedFragment)->>for_all(
		->>(@arg1?referencedFragment,activated)
	).
%%

%%
layOutInfo(MF,
	   I: infoFor = object,
	   Key : key = name,
	   V : value = any,
		Route: [chain]
	  ):->
	"Save lay_out info with infoFor for access by receiver" ::
	%Het mf slaat dus info op over layout van I in de editor van de MF
	%hierbij kan nog de route van I worden meegegeven wanneer het onderdeel is van een
	%geïmporteerd fragment.

	default(Route,new(chain),RealRoute),
	if
		ObjectTable = MF?layOutTable<<-member(I)
	then
		OTable = ObjectTable
	else (
		OTable *= hash_table,
		I->>notifyUnlink(MF, if(->>(MF?layOutTable,delete,@arg1)),@on),
			%altijd slagen door de if. @on betekent: niet runnen als wijzelf (de MF) al aan het unlinken zijn. Zie helpers/unlink_notifier.pl
			%gp3 changed notifyUnlink: the above call now replaces old notifiers
			%(bug in old version kept old ones, growing models all the time)
		MF?layOutTable->>append(I,OTable)
		),

	if
		RTable = OTable<<-find_value(->>(@arg1,equal,RealRoute))
	then
		Table = RTable
	else (
		Table *= hash_table,
		OTable->>append(RealRoute?copy,Table)
		),

	Table->>append(Key,V).
%%

%%
layOutInfo(MF,
	   I : infoFor = object,
	   Key : key = name,
		Route: [chain],
		_ThisLevelOnly: [bool],	%gp3 1.0: default @off. When @on do not search for the information in the imported route
	   V : any):<-
	"Get lay_out info with infoFor as accessed by receiver, fail if unavailable" ::

	default(Route,new(chain),RealRoute),
	%als ie er niet is dan is ie er niet: niet maken
	ObjectTable = MF?layOutTable<<-member(I),
	Table = ObjectTable<<-find_value(->>(@arg1,equal,RealRoute)),
	V = Table<<-member(Key).
%
layOutInfo(_MF,I,Key,Route,ThisLevelOnly,V):<-
	%hij is niet aanwezig in dit fragment,
	%misschien is ie dan aanwezig in het fragment waaruit hij geïmporteerd komt (als er een route is)
	%het eerste element van Route wijst naar het in dit fragment geïmporteerde fragment

	ThisLevelOnly \== @on, %gp3 1.0: explicitly asked not to recurse?
	Route \== @default,

	OtherRoute = Route<<-copy,
	ImportedFragment = OtherRoute<<-delete_head,
	V = ImportedFragment?referencedFragment<<-layOutInfo(I,Key,OtherRoute),
	OtherRoute->>done.
%%

%%
layOutInfoDefault(MF,
	I : infoFor = object,
	Key : key = name,
	Def : default = any,
	Route: [chain],
	V : any):<-
	%haal lay-out info op, en geef default terug als niet beschikbaar
	%gp3 0.3 rewrite: if the layoutinfo is not available, we use the def
	%AND write the def in the layoutinfo

	unless
		V = MF<<-layOutInfo(I,Key,Route)
	do
	(
		V = Def,
		MF->>layOutInfo(I,Key,V,Route)
	).
%%

%%
clearLayOutInfo(MF):->
%gp3 0.3: clear all layout info, called by viewEditor before saving it again
%this way outdated information will be destroyed

	%explicit clear and free of member chains and hash_tables
	MF?layOutTable->>for_all(
		and(
			->>(@arg2,for_all,
				and(
					->>(@arg1,clear),
					->>(@arg1,free),
					->>(@arg2,clear),
					->>(@arg2,free)
				),@on),
			->>(@arg2,clear),
			->>(@arg2,free)
			), @on
		),
	MF?layOutTable->>clear,
	MF->>slot(layOutTable,new(hash_table)).
%%


%%
isConditionalElement(MF, E: fragmentElement):->
	"Succeeds if the given element is conditional in this fragment" ::

	MF<<-hypered(conditionalElement,
		     @arg3 == E).
%%

%%
findElements(MF,
	    Class : [name],
	    SubclassOk: [bool],
		Elements : chain):<-
	"Return chain of elements, all or of given class" ::
	%gp3 0.3 added SubclassOk (default: @on). When @off, only exact class, no subclasses
	%class must be given then..

	(   Class = @default
	->  C = fragmentElement
	;   C = Class
	),
	if
		SubclassOk = @off
	then
		Elements = MF?elements<<-find_all(@arg1?class_name == C)
	else
		Elements = MF?elements<<-find_all(->>(@arg1,instance_of,C)).
%%

%%
findParentElements(MF,
		Class: [class],
		Elements : chain):<-
	"Return chain of elements in parent-chain, all or of given class" ::

	Elements *= chain,
	%we moeten echter dit nivo negeren
	MF?parents->>for_all(and(
						->>(Elements,merge,?(@arg1,findElements,Class)),
						->>(Elements,merge,?(@arg1,findParentElements,Class)))).
%%

%%
containsConditionalFragments(MF):->
	%gp3 0.3
	%succeeds if this mf or one of its ancestors have at least 1 importedFragment as condition (used by viewEditor to quickly find out possible actions)

	MF?elements->>find(->>(@arg1,instance_of,importedFragment))
	;
	MF?parents->>find(->>(@arg1,containsConditionalFragments)).
%%

%%
checkRouteUp(MF,
	Route: chain, %de import route die wordt gecontroleerd, gezien vanuit dit fragment
	Code: code,
	ThisOneToo: [bool],
	SucceededFragment: modelFragment):<-


	/*Controleer het slagen van de code voor een gegeven fragmentroute (die dus de importroute van het een of
	andere element in dit fragment weergeeft)
	Werking: In elk fragment op de route wordt de code uitgevoerd, met @arg1 = het modelfragment,
	@arg2 = de import route gezien vanuit dat fragment (een kopie)
	Zodra de code slaagt slaagt checkRouteUp met het modelfragment waarin de code slaagde. Wanneer de code
	nergens slaagt faalt checkRouteUp.
	Als ThisOneToo @on is wordt het ook in dit fragment zelf gecheckt. Default = @off.
	Volgorde: bottom-up, dwz eerst in dit fragment, dan in de parent/conditioneel fragment, dan daarboven etc.
	*/

	(
		@on == ThisOneToo,
		NewRoute = Route<<-copy,
		%gp3 0.3 For some reason forward *send* method gives problems on function objects
		if
			Code->>'_instance_of'(function)
		then
			Code<<-forward(MF,NewRoute)
		else
			Code->>forward(MF,NewRoute),
		!,
		SucceededFragment = MF
	)
	;
	(
		%goed, niet bij dit fragment, dan bij het fragment waaruit dit fragment volgens de route importeert?
		%dat is de voorste uit de route
		NewRoute = Route<<-copy,
		Head = NewRoute<<-delete_head,
		SucceededFragment = Head?referencedFragment<<-checkRouteUp(NewRoute,Code,@on) %hier ThisOneToo altijd @on
	).
%%

%%
getRouteUp(MF,
	Route: chain,
	Function: function,
	ThisOneToo: [bool],
	Result: any):<-

	/*Als checkRouteUp, maar dan wordt het resultaat van de functie als ie slaagt teruggegeven
	  in plaats van het modelfragment. Zelfde @arg1,@arg2
	*/

	Var *= var,
	MF<<-checkRouteUp(Route,assign(Var,Function,global),ThisOneToo),
	Result = Var<<-'_value'.
%%

%%
checkRouteDown(MF,
	Route: chain,
	Code: code,
	ThisOneToo: [bool],
	SucceededFragment: modelFragment):<-

	/*
	Het omgekeerde van checkRouteUp:
	Werking: In elk fragment dat dit fragment importeert (mf als parent of conditioneel)
	 wordt de code uitgevoerd, met @arg1 = het modelfragment,
	@arg2 = de import route gezien vanuit dat fragment (dus uitgebreid,een kopie). De meegegeven importroute
	slaat dus op de route naar het een of andere element waar de code iets bij checkt in de importerende fragmenten
	Zodra de code slaagt slaagt checkRouteDown met het modelfragment waarin de code slaagde. Wanneer de code
	nergens slaagt faalt checkRouteDown.
	Als ThisOneToo @on is wordt het ook in dit fragment zelf gecheckt. Default = @off.
	Volgorde: "depth-first top-down". Voor elk fragment dat dit fragment importeert gaan we helemaal de diepte in
	voordat we doorgaan met het volgende fragment. Wanneer een fragment dit fragment tweemaal importeert wordt
	de code daar ook tweemaal aangeroepen, waarbij de route wel steeds anders is..
	*/

	(
		@on == ThisOneToo,
		Code->>forward(MF,Route?copy),!,
		SucceededFragment = MF
	)
	;
	(

		%Niet bij dit fragment, dus misschien bij een fragment dat dit
		%fragment importeert. Eerst maar eens al die verwijzingen vinden.
		%We moeten de kinderen vinden en de fragmenten die dit fragment importeren en dan moeten
		%we in dit fragmenten via de conditionals en de parentreferences de importedFragments vinden die
		%naar dit fragment wijzen. Voor elke fragmentreference moet de code gerund worden, en wel met die
		%fragmentreference als head van de nieuwe import route

		AllReferences *= chain,
		CheckMF *= var,

		Children = MF<<-children,
		Children->>unique, %ook al is het tweemaal kind, maar eenmaal opnemen hier

		Children->>for_all(and(
			assign(CheckMF,@arg1),
			->>(
				@arg1?parentReferences,
				for_all,
				if(@arg1?referencedFragment == MF, %als ie verwijst naar dit fragment
					->>(AllReferences,append,create(tuple,CheckMF,@arg1)))))), %dan slaan we het fragment + ref op

		%nu nog de modelfragmenten waarin dit fragment conditie is
		Importing = MF<<-conditionalIn,
		Importing->>unique, %ook maar eenmaal opnemen hier
		Importing->>for_all(and(
			assign(CheckMF,@arg1),
			->>(
				?(@arg1,findElements,importedFragment),
				for_all,
				if(@arg1?referencedFragment == MF,
					->>(AllReferences,append,create(tuple,CheckMF,@arg1)))))), %idem

		%goed, nu kunnen we het af gaan lopen, met een prolog helper doen we dat
		CheckMF->>done,
		pl_checkRouteDown(AllReferences,Route,Code,SucceededFragment)
	).
%
pl_checkRouteDown(AllReferences,Route,Code,SucceededFragment):-
	%goed, recursiefje op pce chain

	Tuple = AllReferences<<-delete_head, %faalt bij lege chain
	NewRoute = Route<<-copy,
	NewRoute->>prepend(Tuple?second),
	(
		SucceededFragment = Tuple?first<<-checkRouteDown(NewRoute,Code,@on) %hier ThisOneToo altijd @on
	;
		( NewRoute->>done,
		 pl_checkRouteDown(AllReferences,Route,Code,SucceededFragment)
		) %of anders door met de volgende in de lijst als die er is
	). %en anders gewoon falen
%%

%%
getRouteDown(MF,
	Route: chain,
	Function: function,
	ThisOneToo: [bool],
	Result: any):<-

	/*Als checkRouteDown, maar dan wordt het resultaat van de functie als ie slaagt teruggegeven
	  in plaats van het modelfragment. Zelfde @arg1,@arg2
	KLOPT DIT?
	*/

	Var *= var,
	MF<<-checkRouteDown(Route,assign(Var,Function,global),ThisOneToo),
	Result = Var<<-'_value'.
%%

%%
checkAllUp(MF, Code: code,
				ThisOneToo:  [bool], %def = @off
				_ParentsOnly:  [bool] %def = @off
			):->

	/*run de code bij alle geïmporteerde fragmenten. Slaagt als de code ergens
		slaagt. De code wordt bij alle parents en alle conditionele fragmenten
		gerunt, en recursief weer bij al hun parents/conditionele fragmenten.
		Er hoort hier geen cycle in te zitten.
		Als ThisOneToo = @on wordt ook bij deze zelf de code gerunt.
		Als ParentsOnly = @on wordt niet bij conditionele gerunt
		@arg1 wordt gebonden aan het betreffende fragment (niet de importedFragment stuff)
		*/

	ThisOneToo == @on,
	Code->>forward(MF),!. %deze zelf
%
checkAllUp(MF, Code: code, _ThisOneToo: [bool], ParentsOnly: [bool]):->
	%we maken een chain voor alle geïmporteerde objecten

	Imported = MF<<-parents,
	if
		ParentsOnly \== @on
	then
		?(MF,findElements,importedFragment)->>for_all(
						->>(Imported,append,@arg1?referencedFragment)),
	Imported->>find(->>(@arg1,checkAllUp,Code,@on,ParentsOnly)).
%%

%%
checkAllDown(MF,
			Code: code,
			ThisOneToo: [bool], %def: @off
			_ChildrenOnly: [bool] %def: @off
			):->

	/*
	Als checkAllUp, maar dan voor alle modelfragmenten die dit fragment "importeren" (en
	recursief), dwz alle children en fragmenten waarin dit fragment conditioneel is.
	Slaagt wanneer Code bij één van die fragmenten slaagt. @arg1 is het fragment.
	Dit dus verder naar beneden/boven (het is maar hoe je het ziet)
	Als ThisOneToo = @on wordt ook dit fragment zelf gecheckt.
	Als ChildrenOnly = @on worden niet conditioneel importerende fragmenten gecheckt
	%%DATAMODEL%%
	*/

	ThisOneToo == @on,
	Code->>forward(MF),!. %deze zelf
%
checkAllDown(MF, Code: code, _ThisOneToo: [bool], ChildrenOnly: [bool]):->
	%een chain met alle importerende objecten

	Importing = MF<<-children,
	if
		ChildrenOnly \== @on
	then
		Importing->>merge(MF?conditionalIn),
	Importing->>find(->>(@arg1,checkAllDown,Code,@on,ChildrenOnly)).
%%

%%
currentType(MF,
	Type: {staticFragment, processFragment, agentFragment,inputSystem}):<-
	%geeft het huidige type volgens de parent(s). Bij meerdere parents
	%geldt de hoogste (agent). Bij geen parents is het een static
	%inputSystem alleen bij de overwrite bij inputSystem

	MF->>checkAllUp(
			@arg1 == @model?topAgentFragment, @on, @on),!,
	Type = agentFragment.
%
currentType(MF,
	Type: {staticFragment, processFragment, agentFragment}):<-

	MF->>checkAllUp(
			@arg1 == @model?topProcessFragment, @on, @on),!,
	Type = processFragment.
%
currentType(_MF,
	Type: {staticFragment, processFragment, agentFragment}):<-

	Type = staticFragment.
%%

%%
type(MF,
	Type: name
	):<-
	"Return description of the currentType" ::

	staticFragment = MF<<-currentType,!,
	Type *= name('Static fragment').
%
type(MF,Type):<-
	processFragment = MF<<-currentType,!,
	Type *= name('Process fragment').
%
type(MF,Type):<-
	agentFragment = MF<<-currentType,!,
	Type *= name('Agent fragment').
%
type(_MF,Type):<-
	Type *= name('Fragment of unknown type').
%%

%%findInequality wordt gebruikt als helper voor CRs newInequality en changeInequality
%slaagt met een gevonden gelijke inequality als die er is, faalt anders
findInequality(MF,
		Type: '{l,leq,eq,geq,g}',
		Arg1: 'garpQuantity | calculus',
		Arg1Type: name,
		Arg1Point: 'valueReference*',
		Arg2: 'garpQuantity | calculus',
		Arg2Type: name,
		Arg2Point: 'valueReference*',
		FoundInequality: inequality):<-

	%bestaat er al zon inequality tussen precies dezelfde argumenten?
	AllInequalities = MF<<-findElements(inequality),
	Relevant = AllInequalities<<-find_all(
				and(->>(@arg1,isArgument,Arg1),
					->>(@arg1,isArgument,Arg2))),

	%ok, als ie al letterlijk bestaat dan mag ie niet dus
	FoundInequality = Relevant<<-find(or(
				and(@arg1?argument1 == Arg1,
					@arg1?argument1Type == Arg1Type,
					@arg1?argument2 == Arg2,
					@arg1?argument2Type == Arg2Type,
					@arg1?type == Type,
					if(Arg1Type == quantityQSPoint, %mag pas na type controle
					->>(@arg1?argument1QSPoint,sameValue,Arg1Point)),
					if(Arg2Type == quantityQSPoint,
					->>(@arg1?argument2QSPoint,sameValue,Arg2Point))),
				and(@arg1?argument1 == Arg2,
					@arg1?argument1Type == Arg2Type,
					@arg1?argument2 == Arg1,
					@arg1?argument2Type == Arg1Type,
					@arg1?swappedType == Type,
					if(Arg2Type == quantityQSPoint, %mag pas na type controle
					->>(@arg1?argument1QSPoint,sameValue,Arg2Point)),
					if(Arg1Type == quantityQSPoint,
					->>(@arg1?argument2QSPoint,sameValue,Arg1Point))))).
%%

%%
refinementFor(MF,
	IF: importedFragment, Route: any,
	FR: fragmentRefiner):<-

	%gp 0.3 Get a refiner in this fragment for the given IF
	%fail if there is none
	Refiners = MF<<-findElements(fragmentRefiner),
	FR = Refiners<<-find(and(@arg1?refined == IF,
				->>(@arg1?refinedRoute,equal,Route))),!.

%%

%%
%relevantComments
%gp3 1.4: return a string with the remarks to show for this one
%when no remarks: return an empty string

relevantComments(MF,RC: string):<-
	if
		0 = MF?remarks<<-size
	then
		RC *= string
	else
		RC *= string('MF %s: %s', MF?name,MF?remarks).

	%%this version does not display comments for parents
	%%this can be done easily: just remove comment signs below
	/*
	%parents
	Parents = MF<<-parents,
	Parents->>unique,
	Parents->>for_all(
		->>(RC,ensure_nl(MF?relevantComments))).
	*/
%%

/***********change requestors*****************/

%newMF: een nieuw modelfragment

checkChange_newMF(MF,
	CR: changeRequestor):->

	%impossible als de naam hetzelfde is
	%als de naam van de ontvanger

	CR?arg1->>equalExportName(MF?name),
	CR->>impossible(string(
		'This name is already used by a %s',MF?systemTypeName),MF).
%%

%%changeMF: een modelfragment wordt gewijzigd, met veel consequenties


checkChange_changeMF(MF,
	CR: changeRequestor):->

		%delegeren naar verschillende calls
		if
			CR->>checkObject(MF)
		then
			@model->>normReservedMFNames(CR?arg1,CR),
		ignore(pl_checkChangeMF_name(MF,CR)),
		ignore(pl_checkChangeMF_type(MF,CR)),
		ignore(pl_checkChangeMF_imports(MF,CR)),
		ignore(pl_checkChangeMF_elements(MF,CR)).
%
pl_checkChangeMF_name(MF,CR):-
		%impossible als de naam hetzelfde is als de ontvanger
		%en de wijziging gaat over een ander modelfragment

		\+ CR->>checkObject(MF),
		CR?arg1->>equalExportName(MF?name),
		CR->>impossible(string(
			'This name is already used by a %s',MF?systemTypeName)
				,MF).
%
pl_checkChangeMF_type(MF,CR):-
		%als het dit object is: check of dit betekent dat we een lager
		%type worden (zo ja: waarschuwen)

		CR->>checkObject(MF),
		(
			CR?arg3->>find(
				->>(MF,checkChange_changeMF_checkParentType,@arg1))
			;
			CR->>warning(string('Warning: the %s "%s" may already contain elements that are inconsistent with it\'s new type. Check the content of the fragment before changing it\'s type.',MF?systemTypeName,MF?name),MF)
		).
%
pl_checkChangeMF_imports(MF,CR):-
/*
	Als wij het gewijzigde MF als conditie hebben (parent, importerend of we zijn het), en wij zijn conditioneel in één van de parents, dan is er een cycle.
	Voor cycle detectie is het voldoende om alleen te letten op direct conditioneel: indirect conditioneel verloopt toch altijd via een direct conditionele met een cycle. (scheelt feedback, zie ook checkChange_newConditionalFragment
*/
	Object = CR<<-object,
	Object->>checkAllDown(
		->>(@arg1?conditionalIn,member,MF),@on,@off), %het object of iets wat daar van afhankelijk is is conditioneel bij ons
	Cycles = CR?arg3<<-find_all(->>(@arg1,checkAllUp,@arg1 == MF, @on, @off)),

	Cycles->>for_all(
		->>(CR,impossible,
			create(string,
				'This would create a cycle in %s "%s" because %s "%s" is a condition in "%s"',
				@arg1?systemTypeName,@arg1?name,
				Object?systemTypeName,Object?name,MF?name),
			MF)).
%
checkChange_changeMF_checkParentType(MF,
	Parent):->
	%slaag als het type van de parent zelfde of hoger is
	%dan het type voor wijziging van de MF (zie hierboven)

	PType = Parent<<-currentType,
	Type = MF<<-currentType,
	pl_checkParentType(Type,PType).
%
pl_checkParentType(Same,Same).
pl_checkParentType('staticFragment',_).
pl_checkParentType('processFragment','agentFragment').
%
pl_checkChangeMF_elements(MF,CR):-
	/*
	Zijn er elementen in dit MF die niet meer geldig zijn omdat er een parent weg is? Waar het hierbij omgaat is of er in dit MF elementen zitten die via hun route gerelateerd zijn aan parents van het object van de CR die straks geen parents meer zijn
	*/

	VerwijderdeParents =
		CR?object?parentReferences<<-find_all(
			not(->>(CR?arg3,member,@arg1?referencedFragment))),
	%bevat nu dus alle importedFragment objecten die verwijzen naar
	%een te verwijderen parent

	%als we elementen hebben die in hun route dit ding noemen: weg ermee
	VerwijderdeParents->>for_all(
		->>(MF,checkChangeMF_elements_checkParent,
			@arg1,CR)).
%
checkChangeMF_elements_checkParent(MF,
	ExParent: importedFragment, CR: changeRequestor):->
	%helper: laat alle elementen checken of ze aan het importedFragment
	%gerelateerd zijn. Zoja: laat ze zich verwijderen via een subCR

	MF?elements->>for_some(->>(@arg1,checkDeleteElement,ExParent,CR)).

%
applyChange_changeMF(MF,
	CR: changeRequestor):->
		%dit mf wordt gewijzigd

		MF->>name(CR?arg1),
		MF->>remarks(CR?arg2),
		/*de parents, dat komt precies
		alle parents die blijven bestaan moeten dezelfde importedFragment link houden, vanwege routes van objecten in het fragment. Uitgangspunt: een specifieke parent kan maar 1 keer parent zijn.
Alle parentReferences elementen die verwijderd worden slaan we trouwens op in het result van de CR (handig voor de editors).
*/

	OudeParents = MF<<-parents,
	NieuweParents = CR<<-arg3,
	VerwijderdeRefs *= chain,
	MF?parentReferences->>for_all(	%work on a copy first
		if(
			not(->>(NieuweParents,member,@arg1?referencedFragment)),
			and(
				->>(VerwijderdeRefs,append,@arg1),
				->>(MF,removeParent,@arg1?referencedFragment)
				)
			)), %weg zijn alle niet-langer-parents

	NieuweParents->>for_all(if(
		not(->>(OudeParents,member,@arg1)),
			->>(MF,addParent,@arg1))),
		%en toegevoegd de nieuwe
	OudeParents->>done,
	NieuweParents->>done,

	%gp3 0.3.11: set active state
	MF->>active(CR?arg4),
	CR->>result(VerwijderdeRefs).
%%

%%
%copyMF: the modelfragment (of InputSystem) will be copied
%gp3 0.2

%no checks needed
applyChange_copyMF(MF,
	CR: changeRequestor):->
	%split, because we can also do the children recursively

	New = MF<<-createCopy(CR?arg1,CR?arg2,new(hash_table)), %empty mappings table to start with
	CR->>result(New).
%
createCopy(MF, CopySingleInheritance: bool, CopyMultipleInheritance: bool, Mappings: hash_table,
									New: modelFragment):<-
	%find a name
	Name= MF<<-name,
	copyMF_uniqueName(Name,1,UniqueName),
	%we create an object of the same type (mf/is)
	New = @pce<<-instance(MF?class,UniqueName,MF?remarks?copy), %argumenten!!

	%copy active state (gp3 0.3.11)
	New->>active(MF?active),

	%copy all elements
	%this is done as follows:
	%1. collect all elements
	%2. Fill the mappings table, connecting the old objects to its copies
	%3. Loop over all elements, asking if they can copy themselves now
	%  -> for every element class, the method copyToNewElement exists. As arguments it gets
	% the new MF, the Mappingstable. The method can return a copy of the element
	% or fail, which means: I cannot copy myself right now, please try again
	% (it waits for other objects to be copied first)
	%The mappings table is also send to children that have to be copied
	%(if CopySingleInheritance = @on), because they might refer to mapped parent references
	%in routes

	%first, map all parents of the fragment, if any


	MF?parentReferences->>for_all(
		->>(MF,copyMF_copyParent,@arg1,Mappings,New)),

	%and we are mapped too
	Mappings->>append(MF,New),

	Elements = MF?elements<<-copy,
	%this call will allways finish (?)
	copyMF_loopElements(MF,Elements,Mappings,New),

	%copy the layouttable (never fail)
	%we add the fragment itself to the mapping, because general layoutinfo (ie size of screen)
	%will be saved there
	Mappings->>append(MF,New),

	MF?layOutTable->>for_some(
		->>(New,copyMF_copyLayout,Mappings,@arg1,@arg2)), %implemented for the new one

	%add to model
	@model->>insertDefinitionObject(New),

	%are we gonna go on like this?
	%(save loop)
	if
		CopySingleInheritance = @on
	then
	(
		if
			CopyMultipleInheritance = @on
		then
			Children = MF?children<<-copy %copy to get save loop
		else
			Children = MF?children<<-find_all(@arg1?parents?size == 1),
		Children->>for_all(
				?(@arg1,createCopy,CopySingleInheritance,CopyMultipleInheritance,Mappings))
	).
%
copyMF_uniqueName(Name,Number,NewName):-
	%gp3 0.2 helper for copyMF: find a unique name
	NewName *= string('%s (copy %s)',Name,Number),
	\+ @model?modelFragments->>find(->>(@arg1?name,equalExportName,NewName)),!.
%
copyMF_uniqueName(Name,Number,NewName):-
	%copy Number exists, raise the number
	NewNumber is Number + 1,
	copyMF_uniqueName(Name,NewNumber,NewName).
%
copyMF_loopElements(_MF,Elements,_Mappings,_NewMF):-
	%gp3 0.2 helper for copyMF: ask all remaining elements to copy themselves
	Elements->>empty,!. %done, no backtracking here
%
copyMF_loopElements(MF,Elements,Mappings,NewMF):-
	Elements?copy->>for_all(
		if(
			->>(Mappings,append,@arg1,?(@arg1,copyToNewElement,Mappings,NewMF)),
			->>(Elements,delete,@arg1) %this element is done
		)
	),
	%see if there are any elements left
	copyMF_loopElements(MF,Elements,Mappings,NewMF).
%
copyMF_copyParent(MF,OldRef: importedFragment,Mappings: hash_table,NewMF: modelFragment):->
	%gp3 0.2 helper for copyMF: create a new importedFragment parentReference
	%to make the new MF get the same parent. Save the mapping
	%
	%the parent itself might be mapped to a new MF (already copied)

	(
		Parent = Mappings<<-member(OldRef?referencedFragment)
		;
		%not mapped
		Parent = OldRef<<-referencedFragment
	),
	NewRef *= importedFragment(MF,Parent, OldRef?remarks),
	?(NewMF,slot,parentReferences)->>append(NewRef), %work on the real thing
	%en de parent krijgt een hyper naar dit kind
	Parent->>hyper(NewMF,child,parent),
	Mappings->>append(OldRef,NewRef).
%
copyMF_copyLayout(NewMF, Mappings: hash_table, Element: any, Layout: hash_table):->
	%gp3 0.2 helper for copyMF: copy 1 element of the layout table
	%this has an object (fragment element etc) as key
	%and the value is another lay-out table, where keys are routes, and values are tables containing the real info
	%so we have to go a bit deeper still

	%never fail
	Layout->>for_some(->>(NewMF, copyMF_copyLayoutRoute, Mappings, Element, @arg1, @arg2)).
%
copyMF_copyLayoutRoute(NewMF, Mappings: hash_table,
			Element: any, Route: chain*, LayoutInfo: hash_table):->
	%see above
	%at this level we can copy
	%alot is possible, so we try to be flexible

	(NewElement = Mappings<<-member(Element) ; NewElement = Element),
	if
		Route = @nil
	then
		NewRoute = @nil
	else
	(
		%complete mapping
		NewRoute *= chain,
		Member *= var,
		Route->>for_all(
			->>(NewRoute,append,when(assign(Member,?(Mappings,member,@arg1)),Member,@arg1)))
	),
	%never fail
	LayoutInfo->>for_some(		%key = infoname, value = info value
		->>(NewMF,layOutInfo,NewElement,@arg1,@arg2,NewRoute)).
%
copyMF_mapRelatedElement(_MF,OldElement: fragmentElement, OldRoute: chain*, Mappings: hash_table,
			NewElementRef: tuple):<-
	%gp3 0.2 helper for copyMF structure.
	%called by copyToNewElement methods in fragmentElement subclasses
	%to get connectedElements and their routes mapped to the newly copied elements and/or changed routes
	%when an element is local, we need a copy. When that is not available in Mappings, we fail
	%(which means: try again later when the related element has been copied).
	%when an element is not local, we can use the same element, but have to fix the route
	%because we need newly copied references
	%
	%result is a tuple (newElement, newRoute).

	%succeeds correctly for @nil and empty routes (both times returning empty route)

	if
	(
		OldRoute = @nil; OldRoute->>empty
	)
	then
	(
		%local object
		NewElement = Mappings<<-member(OldElement), %fail when not yet available
		NewRoute *= chain
	)
	else
	(
		%imported object
		%the head of the route MUST be already mapped
		Mappings<<-member(OldRoute?head),
		NewRoute *= chain,
		Member *= var,
		OldRoute->>for_all(
			if(
				assign(Member,?(Mappings,member,@arg1)),
				->>(NewRoute,append,Member),
				->>(NewRoute,append,@arg1)
				)),
		%The element itself might also be mapped, when its in a parent fragment that was also copied
		(
			NewElement = Mappings<<-member(OldElement)
		;
			NewElement = OldElement
		)
	),
	NewElementRef *= tuple(NewElement,NewRoute).
%%

%%deleteMF: het modelfragment wordt verwijderd
checkChange_deleteMF(MF,
	CR: changeRequestor):->

	%1: versie voor zelf verwijderd

	%als we zelf verwijderd worden kijken we alleen maar of we inhoud hebben
	%alleen als dat zo is geven we een waarschuwing

	CR->>checkObject(MF),!,
	\+ MF?elements->>empty, %er is inhoud
	CR->>warning(string('Warning! You are about to delete the modelfragment "%s", which already has content.', MF?name),MF).
%
checkChange_deleteMF(MF,
	CR: changeRequestor):->

	%2: het is een ander modelfragment dat wordt verwijderd
	%is dat een directe parent van ons?
	if
		MF?parents->>member(CR?object)
	then
	(
		%houden we nog parents over, of niet
		OtherParents = MF<<-parents,
		OtherParents->>delete_all(CR?object),
		%nog over?
		if
			OtherParents->>empty
		then
			%weg met dit fragment
			CR->>addChangeRequest(changeRequestor(deleteMF, MF, CR?editor, CR?garpModel), % added CR?editor JL
				string('The child fragment "%s" will be deleted too', MF?name),
				MF,@nil)
		else
			%alleen deze parent gaat eraan
			CR->>addChangeRequest(changeRequestor(changeMF,
									MF,
									CR?editor, %@default, JL
									CR?garpModel,
									MF?name,
									MF?remarks,
									OtherParents),
				string('The %s "%s" will no longer have the deleted model fragment as it\'s parent', MF?systemTypeName,MF?name),
				MF)
	),

	%en is het te verwijderen fragment een applies-to in dit modelfragment?
	ImportedFragments = MF<<-findElements(importedFragment),
	ImportedFragments->>for_all(
		if(@arg1?referencedFragment == CR?object,
			->>(CR,
				addChangeRequest,
				create(changeRequestor,
					deleteConditionalFragment,
					MF,
					CR?editor, %@default,
					CR?garpModel,
					@arg1),
				string('The deleted %s will also be deleted as a condition in model fragment "%s"', MF?systemTypeName,MF?name),
					MF,
					create(
						chain,
						create(
							changeRequestor,
							deleteMF,
							MF,
							CR?editor,
							CR?garpModel) % Added CR?editor JL
						)
				))).
%
applyChange_deleteMF(MF,
	CR: changeRequestor):->
	%onszelf verwijderen. We verwijderen expliciet info over onze parents,
	%zodat die parents ons niet meer als child zien (want de echte free is natuurlijk
	%weer uitgesteld)
	%gp3: Also ask the model to delete any references to this MF
	%from the MFViews tables
	@model->>removeDefinitionObject(MF),
	@model->>deleteFromMFViews(MF),

	MF?parents->>for_some(->>(MF,removeParent,@arg1)), %for_some, want als er 2 keer dezelfde parent is, haalt de 1e call naar removeParent beide weg, en faalt de 2e call
	CR->>freeObject(MF).
%%

%newInputSystem: een nieuw inputsystem
%(check hier, apply bij garpModel)

checkChange_newInputSystem(MF,
	CR: changeRequestor):->

	%impossible als de naam hetzelfde is
	%als de naam van de ontvanger

	CR?arg1->>equalExportName(MF?name),
	CR->>impossible(string(
		'This name is already used by a %s',MF?systemTypeName),MF).
%

%%
%changeInputSystem: de basisgegevens van een inputsystem worden gewijzigd
%(check hier, apply bij inputSystem)

checkChange_changeInputSystem(MF,
	CR: changeRequestor):->

	%impossible als de naam hetzelfde is als de ontvanger (dus een object
	%van onze subclass inputSystem)
	%en de wijziging gaat over een ander modelfragment

	\+ CR->>checkObject(MF),
	CR?arg1->>equalExportName(MF?name),
	CR->>impossible(string(
		'This name is already used by a %s',MF?systemTypeName)
			,MF).
%%

%%
%deleteHObjectTree
%check of er instanties zijn die van entity moeten veranderen
%dit geldt voor assumptions en abstractEntities
%deze krijgen een nieuwe entity als dat kan, of anders wordt het impossible

checkChange_deleteHObjectTree(MF, CR: changeRequestor):->

	%we splitten eerst even op klasse, omdat hier assumptions zo anders gaan
	%dan andere HObject-instanties
	if
		CR?object->>instance_of(abstractEntity)
	then
		pl_checkDeleteAETree(MF,CR)
	else
		pl_checkDeleteAssumptionTree(MF,CR).
%
pl_checkDeleteAETree(MF,CR):-

	Instances = MF<<-findElements(garpInstance), %entities en agents
	Relevant = Instances<<-find_all(or(
			->>(CR,checkObject,@arg1?entity),
			->>(@arg1?entity,isUp,CR?object))),
	\+ Relevant->>empty, %anders niets aan de hand
	Parent = CR?object<<-parent,
	(
		if (
				\+ Parent = @model<<-hypered(topAgent),
				\+ Parent = @model<<-hypered(topEntity) %niet aan de top
			)
		then
			%voor alle relevante instances moet de parent opnieuw gezet worden
			Relevant->>for_all(->>(CR,addChangeRequest,
						create(changeRequestor,changeFInstance,
								MF,
								@default,
								CR?garpModel,
								@arg1,
								Parent,
								@arg1?stateName,
								@arg1?name,
								@arg1?remarks),
						create(string,
							'The instance "%s" in %s "%s" wil become an instance of %s',
							@arg1?name,MF?systemTypeName,MF?name,Parent?name),
						MF))
		else
			%blijkbaar is de parent een topnode, mag niet
			Relevant->>for_all(->>(CR,
				impossible,
				create(string,
					'A new %s for instance "%s" in %s "%s" (now an instance of %s) cannot automatically be determined.',
					CR?object?class_name,
					@arg1?name, MF?systemTypeName,MF?name,
					@arg1?entity?name)
				)
			)
	).
%
pl_checkDeleteAssumptionTree(MF,CR):-
	%variant van checkDeleteHObjectTree voor assumptions
	%die worden verwijderd
	Instances = MF<<-findElements(assumptionInstance),
	Relevant = Instances<<-find_all(or(
			->>(CR,checkObject,@arg1?assumption),
			->>(@arg1?assumption,isUp,CR?object))),
	Relevant->>for_all(->>(CR,addChangeRequest,
							create(changeRequestor,deleteAssumptionInstance,
								MF, @default, CR?garpModel, @arg1),
							create(string,
								'The assumption "%s" in %s "%s" will be deleted.',
								@arg1?assumption?name, MF?systemTypeName,MF?name),
							MF)).




%%changeQuantityDef: check of hierdoor quantities ongeldig zouden worden doordat ze een verkeerde QS hebben..
checkChange_changeQuantityDef(MF,
	CR: changeRequestor):->

	Definition = CR<<-object,
	QSAllowed = CR<<-argument(2),
	Quantities = MF<<-findElements(garpQuantity),

	Quantities->>for_all(if(
					and(
						@arg1?definition == Definition, %als de definitie gebruikt wordt
						not(->>(QSAllowed,member,@arg1?quantitySpace))), %en de qs van de quantity mag niet meer volgens de gewijzigde def
					->>(CR,impossible,
						create(string,
							'A quantity in %s "%s" uses a no longer allowed quantity space',MF?systemTypeName,MF?name),@arg1))),
	Quantities->>done.
%%

%%
%deleteQuantityDef: mag niet wanneer er quantities op die definitie zijn gebaseerd

checkChange_deleteQuantityDef(MF,
	CR: changeRequestor):->

	Definition = CR<<-argument(1), %niet ?object
	Quantities = MF<<-findElements(garpQuantity),
	if
		Quantities->>find(@arg1?definition == Definition)
	then
		CR->>impossible(string(
			'At least one quantity in %s "%s" is based on the removed definition',
			MF?systemTypeName,MF?name),
			MF),
	Quantities->>done.
%%

%%
%changeAttributeDef: check of attributes hierdoor een ongeldige waarde krijgen

checkChange_changeAttributeDef(MF,
	CR : changeRequestor):->

	Attributes = MF<<-findElements(garpAttribute),
	Values = CR<<-argument(2),
	Attributes->>for_some(if(@arg1?definition == CR?object,
							->>(@arg1,checkChangedValues,Values,MF,CR))).
%%

%%
%deleteAttributeDef: mag niet wanneer er quantities op die definitie zijn gebaseerd

checkChange_deleteAttributeDef(MF,
	CR: changeRequestor):->

	Definition = CR<<-argument(1), %niet ?object
	Attributes = MF<<-findElements(garpAttribute),
	if
		Attributes->>find(@arg1?definition == Definition)
	then
		CR->>impossible(string(
			'At least one attribute in %s "%s" is based on the removed definition',
			MF?systemTypeName,MF?name),
			MF),
	Attributes->>done.
%%

%%
%deleteConfigurationDef: dat mag niet als we configuraties hebben die daarop zijn gebaseerd
checkChange_deleteConfigurationDef(MF,
	CR: changeRequestor):->

	Def = CR<<-argument(1),
	Configs = MF<<-findElements(configuration),
	if
		Configs->>find(@arg1?definition == Def)
	then
		CR->>impossible(string(
			'At least one configuration in %s "%s" is based on the removed definition',
			MF?systemTypeName,MF?name),MF),
	Configs->>done.
%%

%%
%changeQuantitySpace: dit kan invloed hebben op values, relaties en calculi.
%Die wijzigingen moeten dan doorgevoerd worden

checkChange_changeQuantitySpace(MF,
			    CR : changeRequestor
			   ):->

	%we moeten alle values, inequalities, correspondences en calculi
	%laten checken of ze hun waarde moeten aanpassen

	NewValues = CR?arg2,

	%1) Values
	?(MF,findElements,value)->>for_some(
		if(
			->>(CR,checkObject,@arg1?quantity?quantitySpace),
			->>(@arg1,checkChangedQS,@arg1?quantity?name,NewValues,CR)
		)),

	%2) Inequalities
	?(MF,findElements,inequality)->>for_some(
		->>(@arg1,checkChangedQS,
			when(
				and(
					@arg1?argument1Type == quantityQSPoint,
					->>(CR,checkObject,@arg1?argument1?quantitySpace)
					), @on, @off),
			when(
				and(
					@arg1?argument2Type == quantityQSPoint,
					->>(CR,checkObject,@arg1?argument2?quantitySpace)
					), @on, @off), %als 2 keer @off dan doet de call gewoon niets
			NewValues,MF,CR)),

	%3) voor calculi geldt zo ongeveer hetzelfde
	?(MF,findElements,inequality)->>for_some(
		->>(@arg1,checkChangedQS,
			when(
				and(
					@arg1?argument1QSPoint \== @nil,
					->>(CR,checkObject,@arg1?argument1?quantitySpace)
					), @on, @off),
			when(
				and(
					@arg1?argument2QSPoint \== @nil,
					->>(CR,checkObject,@arg1?argument2?quantitySpace)
					), @on, @off),
			NewValues,MF,CR)),

	%3.5) JL: These checks should also be done for calculi
	?(MF,findElements,calculus)->>for_some(
		->>(@arg1,checkChangedQS,
			when(
				and(
					@arg1?argument1QSPoint \== @nil,
					->>(CR,checkObject,@arg1?argument1?quantitySpace)
					), @on, @off),
			when(
				and(
					@arg1?argument2QSPoint \== @nil,
					->>(CR,checkObject,@arg1?argument2?quantitySpace)
					), @on, @off),
			NewValues,MF,CR)),

	%4) Hetzelfde voor correspondenties
	%gp3 0.3 changed this: also set an argument to check when not a value correspondence, because of qs-correspondence checks (so set checkArg to @on whenever the qs for that argument has changed)

	?(MF,findElements,correspondence)->>for_some(
		->>(@arg1,checkChangedQS,
			when(
				->>(CR,checkObject,@arg1?argument1?quantitySpace)
				, @on, @off),
			when(
				->>(CR,checkObject,@arg1?argument2?quantitySpace)
					, @on, @off),
			NewValues,MF,CR)).
%%

%%
%deleteQuantitySpace: mag niet wanneer er quantities zijn die die quantity space gebruiken

checkChange_deleteQuantitySpace(MF,
	CR:changeRequestor):->

	QS = CR<<-argument(1),
	Quantities = MF<<-findElements(garpQuantity),
	if
		Quantities->>find(@arg1?quantitySpace == QS)
	then
		CR->>impossible(string('At least one quantity in %s "%s" uses the quantity space',MF?systemTypeName,MF?name),MF),
	Quantities->>done.
%%

%%newFInstance: een instance in een modelfragment
%%%
%%Of een bepaald object aanwezig mag zijn in een bepaald type modelFragment wordt momenteel
%bepaald door de editor, en niet door de changeRequestors. Das ook wel zo handig, alhoewel
%de editor het misschien beter door kan sturen?

checkChange_newFInstance(MF,
		       CR: changeRequestor
		      ):->

	%de naam moet uniek zijn in het fragment waaraan het
	%wordt toegevoegd, maar niet in fragmenten waarin het
	%betreffende fragment wordt gebruikt

	%gp3 0.3 added check for refiners. If a MF has relevant refiners
	%we create an internal subCR for recreating the fragmentRefinerIdentity objects
	%we use to map instances between refined and refinement

	%er moet wel een entiteit geselecteerd zijn
	if
	(
		CR->>checkObject(MF),
		@nil = CR<<-arg1
	)
	then
		CR->>impossible('There is no entity or agent selected'),
	if
	(
		CR->>checkObject(MF),
		NewName = CR<<-argument(3),
		?(MF,findElements,garpInstance)->>find(->>(@arg1?name,
				    equalExportName,
				    NewName))
	)
	then
		CR->>impossible('The given name is already used in this fragment'),

	?(MF,findElements,fragmentRefiner)->>for_some(->>(@arg1,checkRecreateIdentities,CR?object,CR)).

%
applyChange_newFInstance(MF,
		       CR: changeRequestor
		      ):->
	%de instantie wordt aan dit fragment toegevoegd
	%gp3 0.2: we refer to a helper, so copyToNewElement calls from fragmentElements can also use this

	I = MF<<-addNewFInstance(CR?arg1,CR?arg2,CR?arg3,CR?arg4),
	CR->>result(I).
%
addNewFInstance(MF, Entity: abstractEntity, State: name, Name: name, Remarks: string,I: garpInstance):<-

	%gp3 0.2 Split off from applyChange, can now be called by (fragmentElement subclass)->copyToNewElement as well

	I *= garpInstance(MF,Name,Remarks),

	%zet de configuratieve informatie
	Entity->>addInstance(I),
	(   State = 'condition'
	->  MF->>hyper(I,conditionalElement,fragment)
	;   MF->>hyper(I,givenElement,fragment)
	),
	MF?elements->>append(I).
%%

%%changeFInstance: Wijzigingen in de meegestuurde instantie.

checkChange_changeFInstance(MF,
			    CR: changeRequestor
			   ):->

	%het object checkt op naam uniciteit en geldige entiteitsselectie

	CR->>checkObject(MF),
	%er moet wel een entiteit geselecteerd zijn
	if
		@nil = CR<<-arg2
	then
		CR->>impossible('There is no entity or agent selected'),

	NewName = CR<<-argument(4),
	Instance = CR<<-argument(1),

	if
		?(MF,findElements,garpInstance)->>find(and(not(@arg1 == Instance),
				    ->>(@arg1?name,
				      equalExportName,
					NewName)))
	then
		CR->>impossible('The given name is already used in this fragment'),

	%en er mogen geen conditionele subelementen zijn als de instance cosnequence wordt...
	%dat zijn alleen quantities en attributes, dieper zoeken niet nodig
	%gp3: subelements are now also assumptionInstance, same story
	if
	(
			consequence = CR<<-argument(3),
			Instance?subelements->>find(
				and(
					@arg1?fragment == MF,
					->>(@arg1,isCondition))
					)
	)
	then
		CR->>impossible('The instance cannot become a consequence when it has conditional sub elements'),
	%idem voor relaties
	if
	(
			consequence = CR<<-argument(3),
			Instance?relations->>find(
				and(
					@arg1?fragment == MF,	%lokaal
					->>(@arg1,isCondition)
					))
	)
	then
		CR->>impossible('The instance cannot become a consequence when it has conditional relations').

%
applyChange_changeFInstance(MF,
			    CR: changeRequestor
			   ):->
	%de gewijzigde instantie bevindt zich in dit fragment
	%dus moeten we de boel updaten

	Instance = CR<<-argument(1),
	Instance?entity->>removeInstance(Instance),
	CR?arg2->>addInstance(Instance), %entity-instance relation

	%state goedzetten:
	Instance->>delete_hypers(fragment),
	%en de nieuwe
	if
		condition = CR<<-argument(3)
	then
		MF->>hyper(Instance,conditionalElement,fragment)
	else
		MF->>hyper(Instance,givenElement,fragment),

	Instance->>name(CR?arg4),
	Instance->>remarks(CR?arg5).
%%

%%deleteFInstance De meegestuurde instantie moet weg

checkChange_deleteFInstance(MF,
			    CR: changeRequestor
			   ):->

	%We checken gewoon of we elementen hebben die naar de instance verwijzen
	%die moeten dan ook weg. Maakt niet uit in welk fragment (kan ook in subs natuurlijk)

	MF?elements->>for_some(->>(@arg1,checkDeleteElement,CR?arg1,CR)).
%
applyChange_deleteFInstance(MF,
			    CR: changeRequestor
			   ):->
	%de instantie wordt verwijderd

	Instance = CR<<-argument(1),
	MF?elements->>delete(Instance),
	CR->>freeObject(Instance).
%%

%%newAssumptionInstance: er komt een assumptie bij
checkChange_newAssumptionInstance(MF,
	CR: changeRequestor):->

	CR->>checkObject(MF),
	%er moet wel een assumption geselecteerd zijn
	if
		@nil = CR<<-arg1
	then
		CR->>impossible('There is no assumption selected'),
	%gp3: check on double assumptionInstances in the same context
	%(connected to the same garpInstance or Free)
	?(MF,findElements,assumptionInstance)->>for_some(
		->>(@arg1,checkAssumptionConflict,
			@nil, CR?arg4, CR?arg5, MF, CR)).

applyChange_newAssumptionInstance(MF,
	CR: changeRequestor
	):->
	%gp3: there are new arguments in the CR (3 and 4) which are not @nil
	%when the assumption instance is connected to a garpInstance

	%gp3 0.2: we refer to a helper, so copyToNewElement calls from fragmentElements can also use this

	AI = MF<<-addNewAssumptionInstance(CR?arg1,CR?arg2,CR?arg3,CR?arg4),
	CR->>result(AI).
%
addNewAssumptionInstance(MF,Assumption: assumption, Remarks: string,
		Instance: garpInstance*, InstanceRoute: chain*, I: assumptionInstance):<-
	%gp3 0.2 Split off from applyChange, can now be called by (fragmentElement subclass)->copyToNewElement as well

	I *= assumptionInstance(MF, Remarks),
	Assumption->>addInstance(I), %registreren bij de assumptie
	%en bij ons is het een conditioneel element, tenzij we een inputSystem zijn
	if
		MF->>isInputSystem
	then
		MF->>hyper(I, givenElement, fragment)
	else
		MF->>hyper(I, conditionalElement, fragment),
	MF?elements->>append(I),

	%gp3: of there is a garpInstance, register the new AI as a subelement
	unless
		@nil = Instance
	do
	(
		Instance->>addSubelement(I), %register instance/subelement
		I->>instanceRoute(InstanceRoute)
	).
%%

%%changeAssumptionInstance: een assumptie wijzigt
checkChange_changeAssumptionInstance(MF,
	CR: changeRequestor):->

	CR->>checkObject(MF),
	%er moet wel een assumption geselecteerd zijn
	if
		@nil = CR<<-arg2
	then
		CR->>impossible('There is no assumption selected'),
	%gp3: check on double assumptionInstances in the same context
	%(connected to the same garpInstance or Free)
	?(MF,findElements,assumptionInstance)->>for_some(
		->>(@arg1,checkAssumptionConflict,
			@nil, CR?arg4, CR?arg5, MF, CR)).

applyChange_changeAssumptionInstance(_MF,
	CR: changeRequestor):->

	AI = CR<<-arg1,
	AI?assumption->>removeInstance(AI), %info weg
	CR?arg2->>addInstance(AI),
	AI->>remarks(CR?arg3).

%%deleteAssumptionInstance: een assumptie wordt verwijderd
applyChange_deleteAssumptionInstance(MF,
	CR: changeRequestor):->

	AI = CR<<-arg1,
	MF?elements->>delete(AI),
	CR->>freeObject(AI).
%%

%%newAttribute: er komt een attribute bij een instantie.
checkChange_newAttribute(MF,
			 CR : changeRequestor
			):->

	CR->>checkObject(MF),
	%we kijken of die instantie al een attribuut heeft met die definitie
	%+ wat standaard dingen

	%definitie ingevuld?
	if
		@nil = CR<<-arg4
	then
		CR->>impossible('There is no definition for the new attribute selected',MF)
	else (
		%value ingevuld?
		if
			@nil = CR<<-arg5
		then
			CR->>impossible('There is no value selected for the new attribute',MF),

		%laat alle attributes bij de instance checken of het wel gaat
		%(en eventueel zichzelf verwijderen):

		CR?arg1?attributes->>for_some(
			->>(@arg1,checkChangedAttribute,@nil,CR?arg2,CR?arg4,MF,CR))
	).
%
applyChange_newAttribute(MF,
			 CR: changeRequestor
			):->
	%het gaat over ons
	%gp3 0.2: we refer to a helper, so copyToNewElement calls from fragmentElements can also use this

	I = MF<<-addNewAttribute(CR?arg1,CR?arg2,CR?arg3,CR?arg4, CR?arg5, CR?arg6),
	CR->>result(I).
%
addNewAttribute(MF, Instance: garpInstance, Route: chain, State: name,
				Def: garpAttributeDefinition*, Val: valueReference, Remarks: string, A):<-

	A *= garpAttribute(MF, Def,Val, Remarks),
	Instance->>addSubelement(A), %dat doet de instantie
	A->>instanceRoute(Route),
		%de route naar de eventueel in het fragment geïmporteerde instance

	%onze configuratieve informatie
	(   State = 'condition'
	->  MF->>hyper(A,conditionalElement,fragment)
	;   MF->>hyper(A,givenElement,fragment)
	),
	%en in de lijst
	MF?elements->>append(A).
%%

%%changeAttribute: Een bestaande attribute wordt gewijzigd
checkChange_changeAttribute(MF,
			 CR : changeRequestor
			):->

	%lijkt erg op newAttribute

	CR->>checkObject(MF),
	%we kijken of die instantie al een attribuut heeft met die definitie
	%+ wat standaard dingen


	%definitie ingevuld?
	if
		@nil = CR<<-arg3
	then
		CR->>impossible('There is no definition for the attribute selected',MF)
	else (
		%value ingevuld?
		if
			@nil = CR<<-arg4
		then
			CR->>impossible('There is no value selected for the attribute',MF),

		%laat alle attributes bij de instance checken of het wel gaat
		%(en eventueel zichzelf verwijderen):

		CR?arg1?garpInstance?attributes->>for_some(
			->>(@arg1,checkChangedAttribute,
				CR?arg1,CR?arg1?instanceRoute,CR?arg3,MF,CR))

	).
%
applyChange_changeAttribute(MF,
			 CR: changeRequestor
			):->
	%het gaat over ons

	Attribute = CR<<-argument(1),
	NewDefinition = CR<<-argument(3),
	Value = CR<<-argument(4),
	Remarks = CR<<-argument(5),

	Attribute->>changeDefinition(NewDefinition,Value),
	Attribute->>remarks(Remarks),
	%onze configuratieve informatie
	Attribute->>delete_hypers(fragment), %wordt opnieuw aangelegd
	(   CR->>checkArgument(2,'condition')
	->  MF->>hyper(Attribute,conditionalElement,fragment)
	;   MF->>hyper(Attribute,givenElement,fragment)
	).
%%

%%deleteAttribute: een attribute wordt verwijderd
applyChange_deleteAttribute(MF,
	CR: changeRequestor):->

	A = CR<<-arg1,
	MF?elements->>delete(A),
	CR->>freeObject(A).
%%

%%newQuantity: er komt een quantity bij een instantie.
checkChange_newQuantity(MF,
			 CR : changeRequestor
			):->

	CR->>checkObject(MF),
	%check definities op @nil, en check bestaande quantities

	if
		@nil = CR<<-arg4
	then
		CR->>impossible('There is no definition for the new quantity selected',MF)
	else (
		%quantityspace?
		if
			@nil = CR<<-arg5
		then
				CR->>impossible('There is no quantity space selected for the new quantity',MF)
		%als er een qs gegeven is gaan we ervan uit dat hij bij de definition past
		else
			%laat alle quantities bij de instance checken of het wel gaat
			%en eventueel zichzelf verwijderen via een subchange (of een impossible geven)
			CR?arg1?quantities->>for_some(
				->>(@arg1,checkChangedQuantity,@nil,CR?arg2,CR?arg4,MF,CR))
		).

%

applyChange_newQuantity(MF,
			 CR: changeRequestor
			):->
	%het gaat over ons
	%gp3 0.2: we refer to a helper, so copyToNewElement calls from fragmentElements can also use this
	%gp3 0.3.16: added arg7, quantityAssumptions
	Q = MF<<-addNewQuantity(CR?arg1,CR?arg2,CR?arg3,CR?arg4,CR?arg5,CR?arg6,CR?arg7),
	CR->>result(Q).
%
addNewQuantity(MF,Instance: garpInstance, Route: chain, State: name, Def: garpQuantityDefinition,
				QS: quantitySpace, Remarks: string, Assumptions: chain, Q: garpQuantity):<-

	Q *= garpQuantity(MF,Def,QS, Remarks),
	Instance->>addSubelement(Q), %dat doet de instantie
	Q->>instanceRoute(Route), %de route naar de eventueel in het fragment geïmporteerde instance

	%gp3 0.3.16: we added the quantity assumptions
	Q->>quantityAssumptions(Assumptions?copy), %copy just in case

	%onze configuratieve informatie
	(   State = 'condition'
	->  MF->>hyper(Q,conditionalElement,fragment)
	;   MF->>hyper(Q,givenElement,fragment)
	),
	%en in de lijst
	MF?elements->>append(Q).
%%

%%changeQuantity: Een quantity wordt gewijzigd
checkChange_changeQuantity(MF,
			 CR : changeRequestor
			):->
%check definities, kijk of er subelementen/relaties zijn die conditie zijn terwijl
%dit element given wordt, en kijk of values/relaties nog naar bestaande QS-elementen wijzen

	CR->>checkObject(MF),

	Quantity = CR<<-argument(1),
	Instance = Quantity<<-garpInstance,
	NewDefinition = CR<<-argument(3),
	NewQS = CR<<-argument(4),

	%
	%conditionele values terwijl de quantity given wordt?
	if
	(
			consequence = CR<<-argument(2),
			Quantity?values->>find(
				and(
					@arg1?fragment == MF,
					->>(@arg1,isCondition))
				)
	)
	then
		CR->>impossible('The quantity cannot become a consequence when it has conditional values'),
	%idem voor relaties
	if
	(
			consequence = CR<<-argument(2),
			Quantity?relations->>find(
				and(
					@arg1?fragment == MF,	%lokaal
					->>(@arg1,isCondition)
					))
	)
	then
		CR->>impossible('The quantity cannot become a consequence when it has conditional relations'),

	if
		NewDefinition == @nil
	then
		CR->>impossible('There is no definition for the quantity selected',MF)
	else (
		%quantityspace?
		if
			NewQS == @nil
		then
				CR->>impossible('There is no quantity space selected for the quantity',MF)
		%als er een qs gegeven is gaan we ervan uit dat hij bij de definition past
		else
		(
			%laat alle quantities bij de instance checken of het wel gaat
			%en eventueel zichzelf verwijderen via een subchange (of een impossible geven)
			Instance?quantities->>for_some(
				->>(@arg1,checkChangedQuantity,Quantity,Quantity?instanceRoute,
						NewDefinition,MF,CR)),

				%laat alle values en relations checken of ze moeten wijzigen
				%omdat de qs is gewijzigd

				Quantity?values->>for_some(
					->>(@arg1,checkChangedQS,NewDefinition?name,NewQS?values,CR)),

				Quantity?calculi->>for_some(
					->>(@arg1,checkChangedQS,
						when(@arg1?argument1 == Quantity,@on,@off),
						when(@arg1?argument2 == Quantity,@on,@off),
						NewQS?values,
						MF,CR)),

				?(Quantity,relations,inequality)->>for_some(
					->>(@arg1,checkChangedQS,
						when(@arg1?argument1 == Quantity,@on,@off),
						when(@arg1?argument2 == Quantity,@on,@off),
						NewQS?values,
						MF,CR)),

				?(Quantity,relations,correspondence)->>for_some(
					->>(@arg1,checkChangedQS,
						when(@arg1?argument1 == Quantity,@on,@off),
						when(@arg1?argument2 == Quantity,@on,@off),
						NewQS?values,
						MF,CR))
		)
	).


%

applyChange_changeQuantity(MF,
			 CR: changeRequestor
			):->
	%het gaat over ons

	Quantity = CR<<-argument(1),
	NewDefinition = CR<<-argument(3),
	NewQS = CR<<-argument(4),
	NewRemarks = CR<<-argument(5),

	Quantity->>changeDefinition(NewDefinition,NewQS),
	Quantity->>remarks(NewRemarks),

	%gp3 0.3.16: we added the quantityAssumptions. Just set them to the new list
	Quantity->>quantityAssumptions(CR?arg6?copy),

	%onze configuratieve informatie
	Quantity->>delete_hypers(fragment), %wordt opnieuw aangelegd
	(   CR->>checkArgument(2,'condition')
	->  MF->>hyper(Quantity,conditionalElement,fragment)
	;   MF->>hyper(Quantity,givenElement,fragment)
	).
%%

%%deleteQuantity De meegestuurde quantity moet weg

checkChange_deleteQuantity(MF,
			    CR: changeRequestor
			   ):->

	%We checken gewoon of we elementen hebben die naar de quantity verwijzen
	%die moeten dan ook weg. Dit doen we in elk fragment

	MF?elements->>for_some(->>(@arg1,checkDeleteElement,CR?arg1,CR)).
%
applyChange_deleteQuantity(MF,
			    CR: changeRequestor
			   ):->
	%de quantity wordt verwijderd

	Quantity = CR<<-argument(1),
	MF?elements->>delete(Quantity),
	CR->>freeObject(Quantity).
%%


%%setValue: een value bij quantity wordt gezet
checkChange_setValue(MF,
	CR : changeRequestor):->

	CR->>checkObject(MF),
	%laten we alle values bij de quantity (ook die uit andere MF's)
	%maar eens laten checken of ze niet weg moeten

	CR?arg1?values->>for_some(
		->>(@arg1,checkSetValue,CR?arg2,CR?arg3,CR?arg4,CR?arg5,CR?object,CR)).
%
applyChange_setValue(MF,
	CR : changeRequestor):->
	%oude value wordt vanzelf verwijderd (zie checkChange), dus wij maken gewoon de nieuwe
	%gp3 0.2: we refer to a helper, so copyToNewElement calls from fragmentElements can also use this

	V = MF<<-addNewValue(CR?arg1,CR?arg2,CR?arg3,CR?arg4,CR?arg5),
	CR->>result(V).
%
addNewValue(MF, Q : garpQuantity, QR: chain, D : bool,
					State: {condition, consequence}, Val : valueReference, V: value):<-

	V *= value(MF,Q,D,Val),
	%onze configuratieve informatie
	if
		State = 'condition'
	then
		MF->>hyper(V,conditionalElement,fragment)
	else
		MF->>hyper(V,givenElement,fragment),
	V->>quantityRoute(QR),
	%en in de lijst
	MF?elements->>append(V).
%%

%%deleteValue: value gaat weg. Geen check
applyChange_deleteValue(MF,
	CR : changeRequestor):->

	V = CR<<-argument(1),
	MF?elements->>delete(V), %(maar de hypers blijven)
	CR->>freeObject(V).
%%

%%

%%newConfiguration: er komt een configuratie bij
checkChange_newConfiguration(MF,
	CR : changeRequestor):->

	if
		CR->>checkObject(MF)
	then
	(
		if
			@nil = CR<<-arg2
		then
			CR->>impossible('There is no definition for the new configuration selected',MF)
	),


	%laat in alle MF's alle configuraties en identiteiten checken of dit wel gaat
	/*
	%er tussen uit ivm weghalen normcontroles
	?(MF,findElements,configuration)->>for_some(
		->>(@arg1,checkChangedConfiguration, @nil,
				CR?arg2,CR?arg3,CR?arg4,
				CR?arg5,CR?arg6,CR?object,CR)),
	*/
	?(MF,findElements,identityRelation)->>for_some(
		->>(@arg1,checkChangedConfiguration,
				CR?arg3,CR?arg4,
				CR?arg5,CR?arg6,
				CR?object,CR)).
%
applyChange_newConfiguration(MF,
	CR: changeRequestor):->
	%maak de nieuwe configuratie
	%gp3 0.2: we refer to a helper, so copyToNewElement calls from fragmentElements can also use this

	C = MF<<-addNewConfiguration(CR?arg1,CR?arg2,CR?arg3,CR?arg4,CR?arg5,CR?arg6,CR?arg7),
	CR->>result(C).
%
addNewConfiguration(MF,State: name, Def: configurationDefinition,
		Arg1: garpInstance, Arg1Route: chain,
		Arg2: garpInstance, Arg2Route: chain,
		Remarks: string,C: configuration):<-

	C *= configuration(	MF, Def, Arg1, Arg1Route, Arg2, Arg2Route, Remarks),
	MF?elements->>append(C),
	if
		State = 'condition'
	then
		MF->>hyper(C,conditionalElement,fragment)
	else
		MF->>hyper(C,givenElement,fragment).
%%

%%changeConfiguration: een gewijzigde configuratie
checkChange_changeConfiguration(MF,
	CR : changeRequestor):->

	if
		CR->>checkObject(MF)
	then
	(
		if
			@nil = CR<<-arg3
		then
			CR->>impossible('There is no definition for the new configuration selected',MF)
	),

	%laat in alle MF's alle configuraties en identiteiten checken of dit wel gaat
	/*
	%er tussen uit ivm weghalen normcontroles
	?(MF,findElements,configuration)->>for_some(
		->>(@arg1,checkChangedConfiguration, CR?arg1,
				CR?arg3,CR?arg4,CR?arg5,
				CR?arg6,CR?arg7,CR?object,CR)),
	*/
	?(MF,findElements,identityRelation)->>for_some(
		->>(@arg1,checkChangedConfiguration,
				CR?arg4,CR?arg5,
				CR?arg6,CR?arg7,
				CR?object,CR)).
%
applyChange_changeConfiguration(MF,
	CR: changeRequestor):->
	%wijzig de configuratie

	C = CR<<-argument(1),
	C->>changeDefinition(CR?arg3),
	C->>argument1(CR?arg4,CR?arg5),
	C->>argument2(CR?arg6,CR?arg7),

	C->>remarks(CR?arg8),

	C->>delete_hypers(fragment), %wordt opnieuw aangelegd
	if
		CR->>checkArgument(2,'condition')
	then
		MF->>hyper(C,conditionalElement,fragment)
	else
		MF->>hyper(C,givenElement,fragment).
%%

%%deleteConfiguration: weg met die configuratie
applyChange_deleteConfiguration(MF,
	CR: changeRequestor):->

	C = CR<<-argument(1),
	MF?elements->>delete(C),
	CR->>freeObject(C).
%%

%%newInequality: er komt een inequality bij
%geen check: mogen altijd meer bij
%
applyChange_newInequality(MF,
	CR: changeRequestor):->
	%maak de nieuwe inequality
	%gp3 0.2: we refer to a helper, so copyToNewElement calls from fragmentElements can also use this

	I = MF<<-addNewInequality(CR?arg1,CR?arg2,CR?arg3,CR?arg4,CR?arg5,CR?arg6,
		CR?arg7,CR?arg8,CR?arg9,CR?arg10,
		CR?arg11),
	CR->>result(I).
%
addNewInequality(MF, State: {condition, consequence},
		Type: type = {l,leq,eq,geq,g},
		Arg1 : argument1 = 'garpQuantity | calculus',
		Arg1Route: chain,
		Arg1Type: argument1Type =
			{currentValue, currentDerivative, quantityQSPoint,derivativeZero,calculus},
		Arg1QSPoint: argument1QSPoint = 'valueReference*',
		Arg2 : argument2 = 'garpQuantity | calculus',
		Arg2Route: chain,
		Arg2Type: argument2Type =
			{currentValue, currentDerivative, quantityQSPoint,derivativeZero,calculus},
		Arg2QSPoint: argument2QSPoint = 'valueReference*',
		Remarks: string, I: inequality):<-

	I *= inequality(MF, Type,
		Arg1, Arg1Route, Arg1Type, Arg1QSPoint,
		Arg2, Arg2Route, Arg2Type, Arg2QSPoint,
		Remarks),
	MF?elements->>append(I),
	if
		State = 'condition'
	then
		MF->>hyper(I,conditionalElement,fragment)
	else
		MF->>hyper(I,givenElement,fragment).
%%

%%changeInequality: Een inequality wordt gewijzigd
%geen check

%
applyChange_changeInequality(MF,
	CR: changeRequestor):->
	%Wijzig de inequality die in dit modelFragment zit

	I = CR<<-argument(1),
	I->>changeArguments(
					CR?arg4,
					CR?arg5,
					CR?arg6,
					CR?arg7,
					CR?arg8,
					CR?arg9,
					CR?arg10,
					CR?arg11),
	I->>type(CR?arg3), %g,l,leq,eq etc
	I->>remarks(CR?arg12),

	I->>delete_hypers(fragment), %wordt opnieuw aangelegd
	if
		CR->>checkArgument(2,'condition')
	then
		MF->>hyper(I,conditionalElement,fragment)
	else
		MF->>hyper(I,givenElement,fragment).
%%

%%deleteInequality: inequality gaat weg. Geen check
applyChange_deleteInequality(MF,
	CR : changeRequestor):->

	MF?elements->>delete(CR?arg1), %(maar de hypers blijven)
	CR->>freeObject(CR?arg1).
%%

%%newCalculus: nieuwe calculus, geen check nodig.
applyChange_newCalculus(MF,
	CR: changeRequestor):->
	%gp3 0.2: we refer to a helper, so copyToNewElement calls from fragmentElements can also use this

	C = MF<<-addNewCalculus(CR?arg1,CR?arg2,CR?arg3,CR?arg4,CR?arg5,CR?arg6,CR?arg7,CR?arg8,CR?arg9),
	CR->>result(C).
%
addNewCalculus(MF,Sign: {min,plus,mult,diw}, Type: {quantity, derivative},
		Arg1: 'garpQuantity | calculus', Arg1Route: chain, Arg1QSPoint: valueReference*,
		Arg2: 'garpQuantity | calculus', Arg2Route: chain, Arg2QSPoint: valueReference*,
		Remarks: string, C: calculus):<-

	C *= calculus(MF,Sign, Type, Arg1, Arg1Route, Arg1QSPoint, Arg2, Arg2Route, Arg2QSPoint, Remarks),
	MF?elements->>append(C),
	MF->>hyper(C,calculus,fragment).
%%


%%changeCalculus: de calculus wijzigt
applyChange_changeCalculus(_MF,
		CR: changeRequestor):->

	CR?arg1->>sign(CR?arg2),
	CR?arg1->>changeArguments(CR?arg3,
					CR?arg4, CR?arg5,
					CR?arg6, CR?arg7,CR?arg8,CR?arg9),
	CR?arg1->>remarks(CR?arg10).
%%

%%
checkChange_deleteCalculus(MF,
		CR: changeRequestor):->

	%laat alle objecten in alle MFs gewoon checken of ze dit element gebruiken
	%en zich ook verwijderen

	MF?elements->>for_some(->>(@arg1,checkDeleteElement,CR?arg1,CR)).

%
applyChange_deleteCalculus(MF,
	CR: changeRequestor):->
	%verwijderen maar

	MF?elements->>delete(CR?arg1),
	CR->>freeObject(CR?arg1).
%%

%%newCorrespondence
%gp3 0.3: we do have a check now: if this is a qs or full correspondence
%there should be a correct mapping on the quantityspaces
%this is also checked in the interface, but things might change while the dlg is open

checkChange_newCorrespondence(MF,
	CR: changeRequestor):->

	%if:
	CR->>checkObject(MF),
	CR->>checkArgument(2,@off), %derivative allways has a correct mapping
	CR->>checkArgument(7,@nil), %value corr
	%so this is a qs or a full correspondence
	\+ CR?arg5->>checkQSCorrCompatibility(CR?arg8,CR?arg3),
	%then:
	CR->>impossible('The two quantity spaces are not compatible for use in this kind of correspondence.').
%

applyChange_newCorrespondence(MF,
	CR: changeRequestor):->
	%maak de nieuwe correspondence
	%gp3 0.2: we refer to a helper, so copyToNewElement calls from fragmentElements can also use this

	C = MF<<-addNewCorrespondence(CR?arg1,CR?arg2,CR?arg3,CR?arg4,CR?arg5,
							CR?arg6,CR?arg7,CR?arg8,CR?arg9,CR?arg10,CR?arg11),
	CR->>result(C).
%
addNewCorrespondence(MF,Directed: bool, Derivative: bool, Mirror: bool, Full: bool,
				Arg1: garpQuantity,Arg1Route: chain, Arg1Value: 'valueReference*',
				Arg2: garpQuantity,	Arg2Route: chain,Arg2Value: 'valueReference*',
					Remarks: string, C: correspondence):<-

	C *= correspondence(MF, Directed, Derivative, Mirror, Full, Arg1, Arg1Route, Arg1Value,
					Arg2, Arg2Route, Arg2Value, Remarks),
	MF?elements->>append(C),
	MF->>hyper(C,givenElement,fragment).
%%

%%changeCorrespondence
%gp3: check added like newCorrespondence, just to be sure of correct mapping
checkChange_changeCorrespondence(MF,
	CR: changeRequestor):->

	%if:
	CR->>checkObject(MF),
	CR->>checkArgument(3,@off), %derivative allways has a correct mapping
	CR->>checkArgument(6,@nil), %value corr
	\+ CR?arg6->>checkQSCorrCompatibility(CR?arg9,CR?arg4),
	%then:
	CR->>impossible('The two quantity spaces are not compatible for use in this kind of correspondence.').

applyChange_changeCorrespondence(_MF,
	CR: changeRequestor):->
	%wijzigen maar
	%gp3:

	Corr = CR<<-argument(1),
	Corr->>directed(CR?arg2),
	Corr->>derivative(CR?arg3),
	Corr->>mirror(CR?arg4),
	Corr->>full(CR?arg5),
	Corr->>changeArguments(CR?arg6,CR?arg7,
								CR?arg8,CR?arg9,CR?arg10,CR?arg11),
	Corr->>remarks(CR?arg12).
%%

%%deleteCorrespondence: correspondence gaat weg. Geen check
applyChange_deleteCorrespondence(MF,
	CR : changeRequestor):->

	MF?elements->>delete(CR?arg1), %(maar de hypers blijven)
	CR->>freeObject(CR?arg1).
%%

%%newQuantityRelation: een prop of inf tussen 2 quantities,
%geen check ivm nieuw beleid (zo min mogelijk normen)

applyChange_newQuantityRelation(MF,
	CR: changeRequestor):->
	%we maken de nieuwe relatie
	%gp3 0.2: we refer to a helper, so copyToNewElement calls from fragmentElements can also use this

	QR = MF<<-addNewQuantityRelation(CR?arg1,CR?arg2,CR?arg3,CR?arg4,CR?arg5,
							CR?arg6,CR?arg7),
	CR->>result(QR).
%
addNewQuantityRelation(MF, Type: {prop,inf},
			Arg1: garpQuantity, Arg1Route: chain,
			Arg2: garpQuantity, Arg2Route: chain,
			Sign: {min,plus,mult,diw}, Remarks: string, QR):<-

	QR *= garpQuantityRelation(MF,Sign, Type, Arg1, Arg1Route, Arg2, Arg2Route, Remarks),
	MF?elements->>append(QR),
	MF->>hyper(QR,givenElement,fragment).
%%

%%changeQuantityRelation:
%geen check ivm nieuw beleid (zo min mogelijk normen)

applyChange_changeQuantityRelation(_MF,
	CR: changeRequestor):->

	QR = CR<<-arg1,
	QR->>type(CR?arg2),
	QR->>argument1(CR?arg3, CR?arg4),
	QR->>argument2(CR?arg5, CR?arg6),
	QR->>sign(CR?arg7),
	QR->>remarks(CR?arg8).
%%

%%deleteQuantityRelation: geen check
applyChange_deleteQuantityRelation(MF,
	CR: changeRequestor):->

	MF?elements->>delete(CR?arg1), %hypers blijven nog hangen
	CR->>freeObject(CR?arg1).
%%

%%newConditionalFragment
%kijk of er sprake is van een cyclus

checkChange_newConditionalFragment(MF,
	CR: changeRequestor):->

	Object = CR<<-object,
	TeImporteren = CR<<-arg1,

	if
	(
		MF == Object,
		TeImporteren = @nil
	)
	then
		CR->>impossible('There is no model fragment selected')
	else
	(
		if
		(
			MF == Object,
			TeImporteren = MF
		)
		then
			CR->>impossible('You cannot add a model fragment to itself',MF)
		else
		(
			if
			(
				MF == Object,
				TeImporteren?parents->>empty
			)
			then
				CR->>impossible('You cannot add a top model fragment as a condition', MF)
			else
			(
				if
				(
					%1 het MF is afhankelijk van het object
					MF->>checkAllUp(@arg1 == Object, @on, @off),
					%2 Te Importeren is afhankelijk van de MF
					TeImporteren->>checkAllUp(@arg1 == MF, @off, @off)
				)
				then
					CR->>impossible(
						string('This would create a cycle because model fragment "%s" is a condition for "%s", and that fragment would now become a condition for "%s"',
							MF?name,
							TeImporteren?name,
							MF?name
							)
						,MF)
			)
		)
	).
%
applyChange_newConditionalFragment(MF,
	CR: changeRequestor):->
	%gp3 0.2: we refer to a helper, so copyToNewElement calls from fragmentElements can also use this

	CF = MF<<-addNewConditionalFragment(CR?arg1,CR?arg2),
	CR->>result(CF).
%
addNewConditionalFragment(MF, Ref: modelFragment, Remarks: string, IF: importedFragment):<-
	%maak de nieuwe importedFragment
	%gp3 0.2 Split off from applyChange, can now be called by (fragmentElement subclass)->copyToNewElement as well

	IF *= importedFragment(MF,Ref,Remarks),
	MF?elements->>append(IF),
	MF->>hyper(IF,conditionalElement,fragment).
%%

%%changeConditionalFragment: kan alleen het commentaar zijn
%gp3 0.3: also used for changing comment for fragmentRefiner (is_a importedFragment)
applyChange_changeConditionalFragment(_MF,
	CR: changeRequestor):->

	CR?arg1->>remarks(CR?arg2).
%%

%%deleteConditionalFragment: controleer of we geen subelementen hebben die weg moeten
checkChange_deleteConditionalFragment(MF,
	CR: changeRequestor):->

	MF?elements->>for_some(->>(@arg1,checkDeleteElement,CR?arg1,CR)).
%
applyChange_deleteConditionalFragment(MF,
	CR: changeRequestor):->

	%gp3 0.3: we also delete hypers fragmentReference and fragment,
	%to make sure conditionalIn works ok after this call but before
	%freeObject

	MF?elements->>delete(CR?arg1), %hypers blijven nog hangen
	CR?arg1->>delete_hypers(fragmentReference),
	CR?arg1->>delete_hypers(fragment),
	CR->>freeObject(CR?arg1).
%%
%newFragmentRefiner
checkChange_newFragmentRefiner(MF,
	CR: changeRequestor):->
	%mostly copied from newImportedFragment
	%no checks on elements

	Object = CR<<-object,
	Refinement = CR<<-arg3,
	%RefinementRoute = CR<<-arg4,
	Refined = CR<<-arg1,
	RefinedRoute = CR<<-arg2,

	%most of these rules are checked by Object
	if
	(
		MF == Object
	)
	then
	(
		%if the Refined element already is refined in this element or
		%above, we block
		if
		(
			UP = MF<<-checkRouteUp(RefinedRoute,
				?(@arg1,refinementFor,Refined,@arg2),@on)
		)
		then
			CR->>impossible(string('There is already a refinement for this model fragment in %s %s. Further refinements should be done through that refinement',UP?displayTypeName,UP?name),MF),

		%if the refined element is a parent reference instead of a conditional reference, we cannot refine it
		if
		(
			\+ Refined<<-fragment
		)
		then
			CR->>impossible('You cannot refine a reference to a parent model fragment.'),
		%if the refined element is element in this Object, we block
		if
		(
			MF = Refined<<-fragment
		)
		then
			CR->>impossible('You cannot refine a direct conditional model fragment',MF),


		if
		(
			Refinement = MF
		)
		then
			CR->>impossible('This would add a copy of the model fragment to itself',MF),

		if
		(
			\+ Refined?referencedFragment->>checkAllDown(
				@arg1 == Refinement,@off,@on)
		)
		then
			CR->>impossible('The refinement should be a specialisation (child) of the model fragment, it is not', MF)
	),

	if
	(
		%1 het MF is afhankelijk van het object
		MF->>checkAllUp(@arg1 == Object, @on, @off),
		%2 Te Importeren is afhankelijk van de MF
		Refinement->>checkAllUp(@arg1 == MF, @off, @off)
	)
	then
		CR->>impossible(
			string('This would create a cycle because model fragment "%s" is a condition for "%s", and that fragment would now become a condition for "%s"',
				MF?name,
				Refinement?name,
				MF?name
				)
			,MF),

	%any refinements already refining part of this?
	Refiners = MF<<-findElements(fragmentRefiner),
	Refiners->>for_some(->>(@arg1,checkReRefinement,Object,Refined,RefinedRoute,CR)),

	%always warn:
	if
		CR->>checkObject(MF)
	then
	    (
		get(@app, setting, nonessential_warnings, @on),
		CR->>warning(string('Warning: A refinement is a shortcut. By using it, you bypass the normal parent-child hierarchy. This way, it is possible to create contradicions that will not work in the simulator.'), MF)
	    ;
		true
	    ).
%
applyChange_newFragmentRefiner(MF,
	CR: changeRequestor):->
	%gp3 0.3: we refer to a helper, so copyToNewElement calls from fragmentElements can also use this

	FR = MF<<-addNewFragmentRefiner(CR?arg1,CR?arg2,CR?arg3,CR?arg4,CR?arg5,@on),
	CR->>result(FR).
%
addNewFragmentRefiner(MF, Refined: 'importedFragment',
				RefinedRoute: chain,
				Refinement: modelFragment,
				RefinementRoute: chain,
				Remarks: string,
				CreateIdentities: bool,
								FR: fragmentRefiner):<-
	%create the new fragmentRefiner; to be used in applyChanged and copyToNewElement
	%a refiner is always a conditional element
	%when CreateIdentities = @on, we will also add the conditional fragmentRefinerIdentity objects to link the instances in Refined to those in Refinement. This should be @on when adding a new one, and @off when copying an existing one, because in the latter case the existing identities will be automatically copied

	FR *= fragmentRefiner(MF, Refinement,RefinementRoute,Refined,RefinedRoute,Remarks),
	MF?elements->>append(FR),
	MF->>hyper(FR,conditionalElement,fragment),
	if
		CreateIdentities = @on
	then
		FR->>createIdentities. %does the work through MF->addNewIdentity
%%

%%deleteFragmentRefiner: exactly the same code as conditionalfragment
%(for now). We kept it apart. Later on, we might check if there is a way
%to move elements that are connected to the refiner to the original mf.

checkChange_deleteFragmentRefiner(MF,
	CR: changeRequestor):->

	MF?elements->>for_some(->>(@arg1,checkDeleteElement,CR?arg1,CR)).
%
applyChange_deleteFragmentRefiner(MF,
	CR: changeRequestor):->

	%gp3 0.3: we also delete hypers fragmentReference and fragment,
	%to make sure conditionalIn works ok after this call but before
	%freeObject

	MF?elements->>delete(CR?arg1), %a refiner is also a conditional element
	CR?arg1->>delete_hypers(fragmentReference),
	CR?arg1->>delete_hypers(fragment),
	CR->>freeObject(CR?arg1).
%%

%%helper for fragmentRefiner, internalCreateRefinerIdentities. See fragmentRefiner: checkRecreateIdentities
applyChange_internalCreateRefinerIdentities(_MF,
	CR: changeRequestor):->
	%gp3 0.3: a request from one of our refiners to let it re-create its identities
	%this assumes they are already deleted through another subcr

	CR?arg1->>createIdentities.
%%

%%newIdentity
checkChange_newIdentity(MF,
	CR: changeRequestor):->

	%het -->object geeft een waarschuwing en alle modelfragmenten checken
	%of er geen configuraties in de weg zitten (die gaan dan weg)

	if
		CR->>checkObject(MF)
	then
	    (
		get(@app, setting, nonessential_warnings, @on),
		CR->>warning('Warning: By using an identity relation it is possible to create contradictions in the subelements of both instances.', MF)
	    ;
		true
	    ),


	%identities zijn geen probleem, kunnen alleen in hetzelfde fragment
	%zitten,

	if
		CR->>checkObject(MF)
	then
	(
		if
			?(MF,findElements,identityRelation)->>find(
				and(
					->>(@arg1,isArgument,CR?arg1,CR?arg2),
					->>(@arg1,isArgument,CR?arg3,CR?arg4)
					))
		then
			CR->>impossible('There is already an identity between these instances.',MF)
	),

	%configuraties kunnen wel in andere fragmenten zitten, zij het alleen
	%in deze en lager. Dat regelen ze zelf
	?(MF,findElements,configuration)->>for_some(
		->>(@arg1,checkNewIdentity,
				CR?arg1,CR?arg2,
				CR?arg3,CR?arg4,
				CR?object,CR)).
%
applyChange_newIdentity(MF,
	CR: changeRequestor):->
	%maak de nieuwe identity
	%gp3 0.2: we refer to a helper, so copyToNewElement calls from fragmentElements can also use this

	I = MF<<-addNewIdentity(CR?arg1,CR?arg2,CR?arg3,CR?arg4,CR?arg5, identityRelation),
	CR->>result(I).
%
addNewIdentity(MF,Arg1: garpInstance, Arg1Route: chain,
				Arg2: garpInstance, Arg2Route: chain, Remarks: string,
				Class: class, I: identityRelation):<-

	%gp3 0.3 added the class argument to be able to create subclass elements
	%in copyToNewElement (see identity_relation). Usualy this is just an identityRelation
	I = @pce<<-instance(Class,MF,Arg1, Arg1Route, Arg2, Arg2Route, Remarks),
	MF?elements->>append(I),
	MF->>hyper(I,conditionalElement,fragment).
%%

%%changeIdentity: kan alleen het commentaar zijn
applyChange_changeIdentity(_MF,
	CR: changeRequestor):->

	CR?arg1->>remarks(CR?arg2).
%

%%
applyChange_deleteIdentity(MF,
	CR: changeRequestor):->

	MF?elements->>delete(CR?arg1), %hypers blijven nog hangen
	CR->>freeObject(CR?arg1).

%%%%%%%%%%%%EXPORT%%%%%%%%%%%%%%%%

export(MF, F: file, LegacyNaming: [bool]):->
	%schrijf dit modelfragment weg
	%LET OP: OVERWRITE DOOR INPUTSYSTEM

	/*
	We gebruiken een exporttable om variabelenamen te onthouden.
	Deze tabel verbindt de tuple (pce-instance,route) met een name die een prolog var weergeeft
	De route is gezien vanuit het exporterende modelfragment
	Deze exportTable is een lookupTable (eigen helper klasse, zie helpers/lookup_table.pl).
	Die werkt als een hash_table, maar is het niet: hij werkt met chains
	en is daardoor deterministisch doch traag. hash_table heeft geen vaste
	volgore in for_all loops e.d, lookup_table wel waardoor de export van
	een model 2x achter elkaar ook hetzelfde zal zijn.
	*/

	%gp3: when LegacyNaming = @on, we use the old garp names like description_view etc
	%otherwise we use Agent, Process and Static (so just the names of the top-nodes)

	%gp3: when MF->>activated fails, this MF or one of its parents /conditionalfragments is not active. We do not export then, but write a comment

	%gp3 1.4: we added a counter, because any valid element will be saved in the exportedElements hash_table in the app using the counter as a key. This way simulate can find the object that belongs to a certain line in the MF definition.

	ExportTable *= lookupTable,

	if
		(\+ 0 = MF?remarks<<-size)
	then
		F->>format('\n%%%s\n%s\n', MF?name,?(MF?remarks,makeRemarks,0))
	else
		F->>format('\n%%%s\n',MF?name),

	/*We moeten echt allerlei dingen weten voordat we ook maar iets kunnen wegschrijven
	Ten eerste: Variabelen voor alle instanties die we in- en exporteren
	Dat doen we via getInstanceExportNames, die we de ExportTable laten vullen
	*/

	unless
		MF->>activated
	do
	(
		if
			@off = MF<<-active
		then
			F->>append('%%This fragment is not exported, because it was set to inactive\n\n')
		else
			F->>append('%%This fragment is not exported, because one of its parents or conditional model fragments was set to inactive\n\n')
	)
	else
	(
		MF->>getInstanceExportNames(ExportTable),
		F->>format('system_structures(%s,\n',
							?(MF,exportStructure,new(chain),ExportTable)),
		Comma *= var,

		/*
		Onze parent(s), ook weer met die instances
		Als de topknoop voor static fragments erbij is doen we iets speciaals:
		we kijken of we description_view of composition_view zijn
		*/

		%gp3 0.1 changed this: we only check for description_view / composition_view
		%when  LegacyNaming = @on
		%otherwise, just use exportStructure

		Comma->>assign('',global),
		F->>append('\tisa(['),
		ParentReferences = MF<<-parentReferences, %get a copy
		ParentReferences->>sort(?(@arg1?referencedFragment?name,
			compare,
			@arg2?referencedFragment?name)),
		ParentReferences->>for_all(
			and(
				->>(F,format,
						'%s%s',
						Comma,
						when(
							and(
								LegacyNaming == @on, %gp3
								->>(@arg1?referencedFragment?parents,empty),
								@arg1?referencedFragment?currentType == staticFragment
							),
							MF?exportStaticType,
							?(@arg1?referencedFragment,exportStructure,
								create(chain,@arg1),ExportTable,LegacyNaming)
						)
				),
				assign(Comma,',\n\t\t',global)
			)
		),

		%nog een probleem: als alle parents description_view zijn (volgens Garp) en wij
		%zouden composition_view zijn, dan moet die ineens als parent erbij:
		%(maar niet als we staticFragment als directe parent hebben, want dan is het hierboven
		% al geregeld)
		%gp3: only relevant in legacy mode
		if
		(
			LegacyNaming = @on,
			staticFragment = MF<<-currentType,	%oftewel: alle parents zijn static
			composition_view = MF<<-exportStaticType,	%en wij zouden als composition_view te boek staan
			\+ MF?parents->>find(or(
				->>(@arg1?parents,empty),
				@arg1?exportStaticType == composition_view)) %maar geen parent die zelf staticFragment is, of al composition_view
		)
		then
			F->>format('%s%s',Comma,composition_view),
		F->>append(']),\n'),
		ParentReferences->>done,

		Counter *= number(1), %gp3 1.4

		%%%%%%%CONDITIES

		F->>append('\tconditions([\n'),
		MF->>exportSystemElements(condition,2,ExportTable, F,@off, Counter),
		F->>append(',\n'),
		MF->>exportConditionalParameters(2,ExportTable,F, Counter),
		F->>append(',\n'),
		MF->>exportValues(condition,2,ExportTable,F, Counter),
		F->>append(',\n'),
		MF->>exportRelations(condition,2,ExportTable,F,Counter),
		F->>append(',\n'),
		MF->>exportSystemStructures(2,ExportTable,F, Counter),
		F->>append('\n\t]),\n'),
		%%%%%%%GIVENS
		F->>append('\tgivens([\n'),
		MF->>exportSystemElements(consequence,2,ExportTable,F,@off, Counter),
		F->>append(',\n'),
		MF->>exportGivenParameters(2,ExportTable,F, Counter),
		F->>append(',\n'),
		MF->>exportValues(consequence,2,ExportTable,F, Counter),
		F->>append(',\n'),
		MF->>exportRelations(consequence,2,ExportTable,F, Counter),
		F->>append(',\n\t\tsystem_structures([])\n\t])\n).\n')
	).

%
exportVar(_MF,O: object, Route: chain, _N: name, ExportTable: lookupTable, _ReadOnly: bool,
	_NonVarName : nonVarName = bool,	Var: name):<-
	%geef een prolog varnaam terug voor het gegeven object
	%door op te zoeken in de exportTable, of anders een unieke naam te kiezen
	%en die aan de exportTable toe te voegen (als ReadOnly niet @on is)
	%als ReadOnly @on is en de naam komt niet voor, dan faalt deze call

	%nonVarName is @on bij inputsystem voor instances. In dit geval wordt
	%er niet er niet een variabele, maar een atoom gemaakt, verder hetzelfde
	%consequent meegeven: werkt alleen bij eerste keer vragen bij een instance
	%dit is verder alleen geimplementeerd waar het nodig is voor het bepalen
	%van de namen van instances in input_systems. Waar naar de namen verwezen wordt
	%via exportVar (bijv. bij parameters) wordt er stilzwijgend vanuit gegaan dat de
	%instance al genoemd is in de exportTabel

	%1: we kennen hem al

	Key *= tuple(O,Route?copy), %om zeker te zijn van goede readonly (want er wordt aan gerommeld in tuple, ik weet niet waarom)
	Var = ExportTable<<-find_value(->>(@arg1,equal,Key)),!.

		%tuple->equal gewijzigd (zie helpers/object_extensions.pl)
%
exportVar(_MF,O: object, Route: chain, N: name, ExportTable: lookupTable, ReadOnly: bool,
	NonVarName : nonVarName = bool, Var: name):<-
	%2: de naam komt nog niet voor

	ReadOnly \== @on,
	if
		NonVarName == @on
	then
		Var = N<<-exportName
	else
		Var = N<<-makePrologName,
	\+ ExportTable<<-find_key(->>(@arg2, equal, Var)),
	ExportTable->>append(tuple(O,Route?copy),Var),!.
%
exportVar(_MF,O: object, Route: chain, N: name, ExportTable: lookupTable, ReadOnly: bool,
	NonVarName : nonVarName = bool, Var: name):<-
	%3: de naam komt voor, we zetten er een teller achter

	%gp3: we create unique names with the same ref-id (if needed)


	ReadOnly \== @on,
	pl_exportVar_findName(ExportTable,N,2,NonVarName,Var),
	ExportTable->>append(tuple(O,Route?copy),Var).
%
pl_exportVar_findName(Table,Name,Num,NonVarName,Result):-
	NewName *= string('%s%s',Name,Num),
	if
		NonVarName == @on
	then
		Result = NewName<<-exportName
	else
		Result = NewName<<-makePrologName,
	\+ Table<<-find_key(->>(@arg2,equal,Result)),!.
%
pl_exportVar_findName(Table,Name,Num,NonVarName,Result):-
	NewNum is Num + 1,
	pl_exportVar_findName(Table,Name,NewNum,NonVarName,Result).
%%

%%
getInstanceExportNames(MF, ExportTable: lookupTable):->
	%vul de ExportTable (zie export) met de namen van alle instanties
	%relevant of niet
	%we gaan ervanuit dat ze er nog niet instaan
	%Bij de naamgeving wordt rekening gehouden met door parents/conditionals geexporteerde
	%instances, en met identities.
	%dit werk wordt gedaan via onze hulp roundUpInstances

	AllInstances = MF<<-roundUpInstances,

	%dit is een lookupTable met als key tuple(instance,route) en als value een secondaryID
	%alle entries met dezelfde ID krijgen dezelfde naam, en hoeven maar een keer
	%genoemd te worden


	OpKey *= lookupTable,
	AllInstances->>for_all(
		if(
			?(OpKey,member,@arg2),
			->>(?(OpKey,member,@arg2),
				append,
				@arg1),
			->>(OpKey,append,@arg2,create(chain,@arg1))
		)
	),
	AllInstances->>done,

	%en die lopen we af, hierbij is van belang dat voor elke secondaryID
	%de naam gebaseerd wordt op de meest lokale instance


	OpKey->>for_all(
		->>(MF,getInstanceExportNames_setIDName,@arg2,ExportTable)
	),
	OpKey->>done.
%
getInstanceExportNames_setIDName(MF, Instances: chain, ExportTable: lookupTable):->
	%hulp van getInstanceExportNames: bepaal een naam voor alle instances in de chain
	%en zet ze allemaal in de ExportTable
	%de chain bevat tuples instance-route. We baseren de naam op de naam van de
	%meest lokale

	%er zit er minstens 1 in de chain, anders was er nooit een id voor geweest

	Min *= number(Instances?head?second?size), %tot nu toe de minste grootte
	MostLocal *= var,
	MostLocal->>assign(Instances?head,global), %nu is deze even de meeste lokale
	Instances->>for_all(
		if(
			->>(Min, larger, @arg1?second?size),
			and(
				assign(MostLocal,@arg1,global),
				->>(Min,value,@arg1?second?size)
			)
		)
	),

	Naam = MF<<-exportVar(MostLocal?first,MostLocal?second,MostLocal?first?name,ExportTable,@off,@off),
	%das dan eindelijk de naam
	Instances->>for_all(
		->>(ExportTable,
			append,
			create(tuple,
				@arg1?first,
				@arg1?second?copy
			),
			Naam
		)
	).
%%
roundUpInstances(MF,AllInstances:lookupTable):<-
	%we vinden alle instances die in het fragment een rol spelen
	%voor de naamgeving en de structuurbeschrijving.
	%er wordt rekening gehouden met de door parents/conditionals geexporteerde
	%instances, en met identities.
	%uiteindelijk komt er een lookupTable uit. Deze mapt
	%tuple(instance,route) op een secondaryID
	%elke secondaryID moet vervolgens een naam krijgen, als we met naamgeving
	%bezig zijn. Meerdere instances met dezelfde secondaryID zijn via identities
	%hetzelfde geworden: dus zelfde naam en maar 1 keer noemen

	AllInstances *= lookupTable,

	%eerste elke parent: we pakken hun roundUpInstances
	%en passen in elk key ervan de route aan (ons importobject ervoor)
	%voor de rest slaan we hem lokaal op: zelfde instance natuurlijk,
	%en zelfde secondaryID
	Imported *= var,
	Route *= var,

	MF?parentReferences->>for_all(
		and(
			assign(Imported,@arg1),
			->>(
				Imported?referencedFragment?roundUpInstances,
				for_all,
					and(
						assign(Route,@arg1?second?copy),
						->>(Route,prepend,Imported),
						->>(AllInstances,append,
							create(tuple,@arg1?first,Route),
							@arg2
						)
					)
			)
		)
	),

	%idem voor de imports
	?(MF,findElements,importedFragment)->>for_all(
		and(
			assign(Imported,@arg1),
			->>(
				Imported?referencedFragment?roundUpInstances,
				for_all,
					and(
						assign(Route,@arg1?second?copy),
						->>(Route,prepend,Imported),
						->>(AllInstances,append,
							create(tuple,@arg1?first,Route),
							@arg2
						)
					)
			)
		)
	),

	%en de lokale instanties krijgen allemaal een eigen id
	?(MF,findElements,garpInstance)->>for_all(
		->>(AllInstances,
			append,
			create(tuple,@arg1,create(chain)),
			create(secondaryID)
		)
	),

	%nu gaan we alle identities aflopen
	?(MF,findElements,identityRelation)->>for_all(
		->>(MF,roundUpInstances_checkIdentity,@arg1, AllInstances)
	).
%
roundUpInstances_checkIdentity(_MF,I: identityRelation, L: lookupTable):->
	%helper van roundUpInstances
	%check de 2 argumenten van de identity in de AllInstances hash
	%en zorg dat alle objecten met de bijbehorende IDs dezelfde ID krijgen
	%want ze zijn allemaal verbonden


	Arg1Tuple *= tuple(I?argument1,I?argument1Route),
	Arg2Tuple *= tuple(I?argument2,I?argument2Route), %lokale route
	%ze zijn er gegarandeerd, anders is er iets mis
	ID1 = L<<-find_value(->>(@arg1,equal,Arg1Tuple)), %zelfde inhoud, ander object
	ID2 = L<<-find_value(->>(@arg1,equal,Arg2Tuple)),

	%alle objecten met ID2 moeten nu ID1 als id krijgen
	L->>for_all(
		if(
			@arg2 == ID2,
			->>(L,append,@arg1,ID1)
		)
	).
%%

%%
exportStructure(MF, CurrentRoute: chain, ExportTable: lookupTable, LegacyNaming: [bool], S: string):<-
	%geef de structuurbeschrijving van deze MF terug
	%en gebruik voor de instantievariabelen de namen uit
	%ExportTable. Deze moet al eerder via getInstanceExportNames gevuld zijn
	%de CurrentRoute is nodig omdat in de ExportTable de route van de instanties
	%vanuit een (wellicht ander) aanroepen fragment staat opgeslagen

	%gp3 0.1: We added LegacyNaming. Only when @on, we will export top nodes as their
	%legacy names (description_view etc). See ->>export
	%when not @on (@off or @default), we will export top nodes using their ?name
	%(static, process etC)

	%we bepalen welke variabelen zijn er voor ons van belang?

	AllInstances = MF<<-roundUpInstances,

	%dit is een lookupTable met als key tuple(instance,route) en als value een secondaryID
	%per secondaryID hebben we eigenlijk maar 1 element nodig, want ze hebben
	%als het goed is in getInstanceExportNames al allemaal dezelfde naam gekregen
	%we draaien het dus om en maken een nieuwe lookupTable op id->tuple (eerste)
	%route daarin is nog lokaal bezien

	Collected *= lookupTable,
	AllInstances->>for_all(
		if(
			not(?(Collected,member,@arg2)),
			->>(Collected,append,@arg2,@arg1)
		)
	),

	AllInstances->>done,

	/*********** VASTE NAAMGEVING **************
	de topfragmenten hebben speciale exportnamen, die staan op 2 plekken:
	1. Hier
	2. Bij @model->>normReservedMFNames daar wordt het gebruik van gereserveerde
		garp-namen (ook composition_view) afgevangen
	********************************************/

	if
	(
		LegacyNaming = @on, %gp3: only when legacynaming is @on
		MF?parents->>empty
	)
	then
	(
		Type = MF<<-currentType,
		(
			(Type = staticFragment, Name = description_view)
		;
			(Type = processFragment, Name = process)
		;
			(Type = agentFragment, Name = qualitative_state)
		)
	)
	else
		Name = MF?name<<-exportName, %anders standaard

	S *= string('%s',Name),
	NumCollected = Collected<<-size,
	if
		NumCollected > 0
	then
		S->>append('('),
	if
		NumCollected > 1
	then
		S->>append('('),

	Comma *= var,
	Comma->>assign('',global),

	%zoek de namen van de instanties die met hun lokale route in Collected zitten
	%op, en zet CurrentRoute nog even snel voor die route
	Collected->>for_all(
		and(
			->>(S,append,
					create(string,'%s%s',
						Comma,
						?(MF,exportVar,
							@arg2?first,?(CurrentRoute,merge,@arg2?second),@arg2?first?name,
							ExportTable,@on,@off) %readonly, hoort er door de hoofdaanroep
							%van getInstanceExportNames (in export) te zijn
					)
			),
			assign(Comma,',',global)
		)
	),

	Collected->>done,

	if
		NumCollected > 0
	then
		S->>append(')'),
	if
		NumCollected > 1
	then
		S->>append(')').

%%
exportSystemElements(MF,StateName: {condition, consequence},BasisTab: int,
		ExportTable: lookupTable, F: file, NVN: nonVarInstanceName = bool, Counter: number):->

	%schrijf de system elements van een bepaalde state weg
	%nonVarInstanceName: als deze @on is (bij inputsystems...)
	%wordt voor de *instances* exportVar aangeroepen met nonVarName = @on
	%(dit worden dus atomen)

	%gp3 1.4: Counter is just a counter, we use it to save the elements in @app->>addExportedElement
	Tab *= string,
	Tab->>insert_character('\t',times:= BasisTab),
	%%instances van entities en agents, assumptions
	F->>format('%ssystem_elements([',Tab),

	Instances = ?(MF,findElements,garpInstance)<<-find_all(
					@arg1?stateName == StateName),
	Instances->>sort(?(@arg1?name,compare,@arg2?name)),
	EntityInstances = Instances<<-find_all(->>(@arg1?entity,instance_of,entity)),
	AgentInstances = Instances<<-find_all(->>(@arg1?entity,instance_of,agent)),
	AssumptionInstances = ?(MF,findElements,assumptionInstance)<<-find_all(
					@arg1?stateName == StateName),

	Comma *= var,
	Comma->>assign('\n',global),

	if
		(\+ EntityInstances->>empty)
	then
	(
		F->>format('%s%s\t%%Entity Instances:\n',Comma,Tab),
		Comma->>assign('',global),
		EntityInstances->>for_all(
			and(
				->>(F,format,
					'%s%s\t%sinstance(%s,%s)',
					Comma,
					when(
						@arg1?remarks?size > 0,
						create(string,'%s\n',
							?(@arg1?remarks,makeRemarks,BasisTab + 2)),
						''
					),
					Tab,
					?(MF,exportVar,@arg1,create(chain),@arg1?name,ExportTable,@off,
						NVN),
					?(@arg1?entity?name,exportName)
				),
				assign(Comma,',\n',global),
				%gp3 1.4: export by counter
				->>(@app,addExportedElement,MF,Counter,@arg1),
				->>(Counter,plus,1)
			)
		)
	),

	if
		(\+ AgentInstances->>empty)
	then
	(
		F->>format('%s%s\t%%Agent Instances:\n',Comma,Tab),
		Comma->>assign('',global),
		AgentInstances->>for_all(
			and(
				->>(F,format,
					'%s%s\t%sinstance(%s,%s)',
					Comma,
					when(
						@arg1?remarks?size > 0,
						create(string,'%s\n',
							?(@arg1?remarks,makeRemarks,BasisTab + 2)),
						''
					),
					Tab,
					?(MF,exportVar,@arg1,create(chain),@arg1?name,ExportTable,@off,
						NVN),
					?(@arg1?entity?name,exportName)
				),
				assign(Comma,',\n',global),
				%gp3 1.4: export by counter
				->>(@app,addExportedElement,MF,Counter,@arg1),
				->>(Counter,plus,1)
			)
		)
	),

	if
		(\+ AssumptionInstances->>empty)
	then
	(
		F->>format('%s%s\t%%Assumption Instances:\n',Comma,Tab),
		Comma->>assign('',global),
		AssumptionInstances->>for_all(
			and(
				->>(F,format,
					'%s%s\t%sinstance(%s,%s)',
					Comma,
					when(
						@arg1?remarks?size > 0,
						create(string,'%s\n',?(@arg1?remarks,makeRemarks,BasisTab + 2)),
						''
					),
					Tab,
					?(MF,exportVar,@arg1,
						create(chain),@arg1?assumption?name,ExportTable, @off,
						NVN),
					?(@arg1?assumption?name,exportName)
				),
				assign(Comma,',\n',global),
				%gp3 1.4: export by counter
				->>(@app,addExportedElement,MF,Counter,@arg1),
				->>(Counter,plus,1),

				%gp3: any assumption connected to an instance will
				%result in exporting a has_attribute(has_assumption):
				if(
					@arg1?garpInstance \== @nil,
					and(
						->>(F,format,
						'%s%s\n\t%shas_attribute(%s,has_assumption,%s)',
							Comma,
							?(string('This assumption is connected to an instance.'),
								makeRemarks,BasisTab + 2),
							Tab,
							?(MF,exportVar,@arg1?garpInstance,@arg1?instanceRoute,
								@arg1?garpInstance?name,ExportTable,@off,@off),
							?(MF,exportVar,@arg1,create(chain),@arg1?name,ExportTable,@off,@off)
						),
						%gp3 1.4: export by counter. This has_attribute is not exported by counter
						%but the linecount is incremented
						->>(Counter,plus,1)
					)
				)
			)
		)
	),

	%atrributes (+ configuraties) horen er ook bij
	Configurations = ?(MF,findElements,configuration)<<-find_all(@arg1?stateName == StateName),
	Configurations->>sort(?(@arg1?name,compare,@arg2?name)),

	if
		(\+ Configurations->>empty)
	then
	(
		F->>format('%s%s\t%%Configurations:\n',Comma,Tab),
		Comma->>assign('',global),
		Configurations->>for_all(
			and(
				->>(F,format,
					'%s%s\t%shas_attribute(%s,%s,%s)',
					Comma,
					when(
						@arg1?remarks?size > 0,
						create(string,'%s\n',?(@arg1?remarks,makeRemarks,BasisTab + 2)),
						''
					),
					Tab,
					?(MF,exportVar,@arg1?argument1,@arg1?argument1Route,
									@arg1?argument1?name,ExportTable,@off,@off),
					@arg1?name?exportName,
					?(MF,exportVar,@arg1?argument2,@arg1?argument2Route,
							@arg1?argument2?name,ExportTable,@off,@off)
				),
				assign(Comma,',\n',global),
				%gp3 1.4: export by counter
				->>(@app,addExportedElement,MF,Counter,@arg1),
				->>(Counter,plus,1)
			)
		)
	),

	Attributes = ?(MF,findElements,garpAttribute)<<-find_all(@arg1?stateName == StateName),
	Attributes->>sort(?(@arg1?name,compare,@arg2?name)),
	if
		(\+ Attributes->>empty)
	then
	(
		F->>format('%s%s\t%%Attributes:\n',Comma,Tab),
		Comma->>assign('',global),
		Attributes->>for_all(
			and(
				->>(F,format,
					'%s%s\t%shas_attribute(%s,%s,%s)',
					Comma,
					when(
						@arg1?remarks?size > 0,
						create(string,'%s\n',?(@arg1?remarks,makeRemarks,BasisTab + 2)),
						''
					),
					Tab,
					?(MF,exportVar,@arg1?garpInstance,@arg1?instanceRoute,
							@arg1?garpInstance?name,ExportTable,@off,@off),
					@arg1?name?exportName,
					@arg1?valueReference?valueName?exportName	%without id
				),
				assign(Comma,',\n',global),
				%gp3 1.4: export by counter
				->>(@app,addExportedElement,MF,Counter,@arg1),
				->>(Counter,plus,1)
			)
		)
	),


	%gp3 0.3.16: quantity behaviours should be given if we are exporting a scenario
	%we also generate an extra assumption for the scenario name
	if
		MF->>isInputSystem
	then
	(
		F->>format('%s%s\t%%Identify this scenario for internal use:\n',Comma,Tab),
		F->>format('%s\tinstance(garp_scenario_%s,garp_internal)',Tab,MF?name?exportName),
		Counter->>plus(1), %not exported, but linecount increments
		Comma->>assign(',\n',global),
		QBNum *= number(1),
		Quantities = MF<<-findElements(garpQuantity),
		Quantity *= var,
		Quantities->>for_all(
			if(
				not(->>(@arg1?quantityAssumptions,empty)),
				and(
					->>(F,format,'%s%s\t%%Quantity behaviour for %s: %s\n',Comma,Tab,@arg1?garpInstance?name,@arg1?name),
					assign(Comma,'',global),
					assign(Quantity,@arg1,global),
					->>(Quantity?quantityAssumptions,for_all,
						and(
							->>(F,format,'%s%s\tinstance(garp_generated_%s_%u,%s),\n%s\thas_attribute(%s,has_assumption,garp_generated_%s_%u)',
								Comma,Tab,@arg1,QBNum,@arg1,Tab,
								?(MF,exportVar,Quantity?garpInstance,Quantity?instanceRoute,
									Quantity?garpInstance?name,ExportTable,@off,@off),
								@arg1,QBNum),
							assign(Comma,',\n',global),
							->>(Counter, plus, 1), %not exported, but linecount increments
							->>(QBNum,plus,1) %next number
						)
					)
				)
			)
		)
	),
	F->>format('\n%s\t])',Tab).
%%

%%
exportConditionalParameters(MF,BasisTab: int, ExportTable: lookupTable, F: file, Counter: number):->

	%schijft de parameters weg die als conditioneel beschouwd kunnen worden
	%dit zijn de lokale conditionele parameters + álle relevante geïmporteerde
	%parameters, die dus gebruikt worden in dit MF bij conditionele of consequentiele
	%values en relaties.

	%gp3 1.4: Counter is just a counter, we use it to save the elements in @app->>addExportedElement

	%vinden van alle relevante quantities

	%1. lokaal, conditionele zoeken
	Local *= chain,
	?(MF,findElements,garpQuantity)->>for_all(
		if(
			->>(@arg1,isCondition),
			->>(Local,append,create(tuple,@arg1,create(chain)))
		)
	),
	Local->>sort(?(@arg1?first?garpInstance?name,compare,@arg2?first?garpInstance?name)),

	%2. geïmporteerd: altijd checken op route (want niet geïmporteerde laten we
	%hier zitten

	Imported *= chain,
	Tuple *= var,

	%2a: values
	?(MF,findElements,value)->>for_all(
		if(
			and(
				not(->>(@arg1?quantityRoute,empty)),
				assign(Tuple,create(tuple,@arg1?quantity,@arg1?quantityRoute)),
				not(->>(Imported,find,->>(@arg1,equal,Tuple)))
			),
			->>(Imported,append,Tuple)
		)
	),

	?(MF,findElements,parRelation)->>for_all(	%doet ook de verbonden calculi
		->>(MF,exportParameters_CollectQuantities,@arg1,Imported,create(chain))
	),

	Imported->>sort(?(@arg1?first?garpInstance?name,compare,@arg2?first?garpInstance?name)),

	%in Local en Imported zitten nu de juiste quantities (met hun route)
	%die moeten we nog eventjes schrijven

	%we gaan de code twee keer runnen, dus slaan we hem op in een variabele
	Tab *= string,
	Tab->>insert_character('\t',times:= BasisTab),

	Comma *= var,
	Comma->>assign('\n',global),

	Loop *=
		and(
			->>(F,format,
				'%s%s\t%s%s(%s,%s,_,%s)',
				Comma,
				when(
					and(
						->>(@arg1?second,empty),	%alleen commentaar bij lokale quantity
						@arg1?first?remarks?size > 0
					),
					create(string,'%s\n',?(@arg1?first?remarks,makeRemarks, BasisTab + 2)),
					''
				),
				Tab,
				@arg1?first?name?exportName,
				?(MF,exportVar,@arg1?first?garpInstance,
					?(@arg1?second,merge,@arg1?first?instanceRoute),
					@arg1?first?garpInstance?name,ExportTable,@off,@off),
				?(MF,exportVar,@arg1?first,@arg1?second,
							@arg1?first?name,ExportTable,@off,@off),
				@arg1?first?quantitySpace?name?exportName
			),
			assign(Comma,',\n',global),
			%gp3 1.4: export by counter
			%only export-by-counter the quantity when local, then export the quantity, not the tuple
			if(
				->>(@arg1?second,empty),
				->>(@app,addExportedElement,MF,Counter,@arg1?first)
			),
			%allways increment the counter
			->>(Counter,plus,1)
		),

	F->>format('%sparameters([',Tab),

	if
		(\+ Local->>empty)
	then
	(
		F->>format('%s%s\t%%%%local conditional parameters:\n',Comma,Tab),
		Comma->>assign('',global),
		Local->>for_all(Loop)
	),

	if
		(\+ Imported->>empty)
	then
	(
		F->>format('%s%s\t%%%%imported relevant parameters:\n',Comma,Tab),
		Comma->>assign('',global),
		Imported->>for_all(Loop)
	),

	F->>format('\n%s\t])',Tab).

%
exportParameters_CollectQuantities(MF,R: 'parRelation|calculus',Quantities: chain, BasisRoute: chain):->
	%helper bij exportParameters: peutert de geïmporteerde quantities die bij (lokale) relaties horen
	%eruit en kijkt of ze niet al geexporteerd zijn. Zoniet, dan gaat quantity/route tuple
	%in Quantities

	TotalRoute1 = BasisRoute<<-merge(R?argument1Route),
	if
		R?argument1->>instance_of(garpQuantity)
	then
	(
		Q1 *= tuple(R?argument1,TotalRoute1),
		if
		(
			\+ TotalRoute1->>empty,	%alleen geïmporteerde quantities (volgens de hele route vanwege calculi)
			\+ Quantities->>find(->>(@arg1,equal,Q1))
		)
		then
			Quantities->>append(Q1)
	)
	else %het is een calculus
		MF->>exportParameters_CollectQuantities(R?argument1,Quantities,
					TotalRoute1),

	TotalRoute2 = BasisRoute<<-merge(R?argument2Route),
	if
		R?argument2->>instance_of(garpQuantity)
	then
	(
		Q2 *= tuple(R?argument2,TotalRoute2),
		if
		(
			\+ TotalRoute2->>empty,
			\+ Quantities->>find(->>(@arg1,equal,Q2))
		)
		then
			Quantities->>append(Q2)
	)
	else %het is een calculus
		MF->>exportParameters_CollectQuantities(R?argument2,Quantities,
					TotalRoute2).
%%

%%
%%
exportGivenParameters(MF,BasisTab: int,ExportTable: lookupTable, F: file, Counter: number):->

	%schijft de parameters weg die als given beschouwd kunnen worden
	%dit zijn dus alleen lokale consequentiele parameters
	%gp3 1.4: Counter is just a counter, we use it to save the elements in @app->>addExportedElement

	Tab *= string,
	Tab->>insert_character('\t',times:= BasisTab),
	F->>format('%sparameters([',Tab),

	Comma *= var,
	Comma->>assign('\n',global),

	Parameters = ?(MF,findElements,garpQuantity)<<-find_all(not(->>(@arg1,isCondition))),
	Parameters->>sort(?(@arg1?garpInstance?name,compare,@arg2?garpInstance?name)),
	Parameters->>for_all(
		and(
			->>(F,format,
				'%s%s\t%s%s(%s,%s,_,%s)',
				Comma,
				when(
					@arg1?remarks?size > 0,
					create(string,'%s\n',?(@arg1?remarks,makeRemarks, BasisTab + 2)),
					''
				),
				Tab,
				@arg1?name?exportName,
				?(MF,exportVar,@arg1?garpInstance,
					@arg1?instanceRoute,
					@arg1?garpInstance?name,ExportTable,@off,@off),
				?(MF,exportVar,@arg1,create(chain),
							@arg1?name,ExportTable,@off,@off),
				@arg1?quantitySpace?name?exportName
			),
			assign(Comma,',\n',global),
			%gp3 1.4: export by counter
			->>(@app,addExportedElement,MF,Counter,@arg1),
			->>(Counter,plus,1)
		)
	),
	F->>format('\n%s\t])',Tab).
%%

exportValues(MF,StateName: {condition, consequence},BasisTab: int,
		ExportTable: lookupTable, F: file, Counter: number):->
	%schrijf de values van een bepaalde state weg
	%gp3 1.4: Counter is just a counter, we use it to save the elements in @app->>addExportedElement

	Tab *= string,
	Tab->>insert_character('\t',times:= BasisTab),
	F->>format('%spar_values([',Tab),
	Values = ?(MF,findElements,value)<<-find_all(
					@arg1?stateName == StateName),
	%eerst maar eens verzamelen welke quantities er toen doen
	Relevant *= chain,
	Current *= var,

	%gp3 1.4: we do have a problem here, because there can be 2 value objects in the same exported line
	%so which one do we save with addExportedElement?
	%it does not matter much because values are of little interest, the do not have comments with them etc
	%
	%so for now, we dont do anything: we increment the counter but do NOT use it in addExportedElement
	%this is easy to change, for example by using a chain of values in addExportedElement

	Values->>for_all(
		and(
			assign(Current,@arg1,global),
			if(not(->>(Relevant,find,
				and(
					->>(@arg1?first,equal,Current?quantity),
					->>(@arg1?second,equal,Current?quantityRoute)
				))),
				->>(Relevant,append,create(tuple,Current?quantity,Current?quantityRoute))
			)
		)
	),

	%nu nog wegschrijven
	Comma *= var,
	Comma->>assign('\n',global),
	Relevant->>sort(?(@arg1?first?name,compare,@arg2?first?name)),
	Relevant->>for_all(
		and(
			assign(Current,@arg1,global),
			->>(F,format,
				'%s\t%svalue(%s,_,%s,%s)',
				Comma,Tab,
				?(MF,exportVar,Current?first,Current?second,
							Current?first?name,ExportTable,@off,@off),
				when(
					->>(Values,find,and(@arg1?derivative == @off,
						@arg1?quantity == Current?first,
						->>(@arg1?quantityRoute,equal, Current?second))),
					?(MF,exportQSValue,Values?current?valueReference,
							Current?first,Current?second,ExportTable),
					'_'
				),
				when(
					->>(Values,find,and(@arg1?derivative == @on,
						@arg1?quantity == Current?first,
						->>(@arg1?quantityRoute,equal, Current?second))),
					?(MF,exportQSValue,Values?current?valueReference,
							Current?first,Current?second,ExportTable),
					'_'
				)
			),
		assign(Comma,',\n',global),
		%gp3 1.4: export by counter
		%which we do not do right now, see comments above
		%->>(@app,addExportedElement,MF,Counter,@arg1),
		->>(Counter,plus,1)
		)
	),
	F->>format('\n%s\t])',Tab).
%%

%%
exportRelations(MF,StateName: {condition, consequence},BasisTab: int,
		ExportTable: lookupTable, F: file, Counter: number):->
	%schrijf de relations van een bepaalde state weg
	%gp3 1.4: Counter is just a counter, we use it to save the elements in @app->>addExportedElement

	Tab *= string,
	Tab->>insert_character('\t',times:= BasisTab),
	F->>format('%spar_relations([',Tab),

	Comma *= var,
	Comma->>assign('\n',global),

	Relations =  ?(MF,findElements,parRelation)<<-find_all(@arg1?stateName == StateName),
	Relations->>for_all(
		and(
			->>(F,format,
				'%s%s\t%s%s',
				Comma,
				when(
					@arg1?remarks?size > 0,
					create(string,'%s\n',?(@arg1?remarks,makeRemarks,BasisTab + 2)),
					''
				),
				Tab,
				?(MF,exportRelation,@arg1,ExportTable)
			),
			assign(Comma,',\n',global),
			%gp3 1.4: export by counter
			->>(@app,addExportedElement,MF,Counter,@arg1),
			->>(Counter,plus,1)
		)
	),
	F->>format('\n%s\t])',Tab).
%
%
exportRelation(MF, R: parRelation, ExportTable: lookupTable, S: string):<-
	%helper voor het wegschrijven van een relatie

	%1: inequality
	R->>instance_of(inequality),
	S *= string('%s(%s,%s)',
		?(MF,exportInequalityType,R),
		?(MF,exportInequalityArg,R?argument1,R?argument1Route,
				R?argument1Type,R?argument1QSPoint,ExportTable),
		?(MF,exportInequalityArg,R?argument2,R?argument2Route,
				R?argument2Type,R?argument2QSPoint,ExportTable)
	).
%

exportRelation(MF, R: parRelation, ExportTable: lookupTable, S: string):<-

	%2: valuecorrespondence
	/*LET OP: ARGUMENTEN ZIJN ANDERSOM:
	In vgarp: wijst de pijl van de beïnvloedende naar de beïnvloede
	In garp: is het eerste argument de beïvloede..
	2.1: het kan ook nog eens van een derivative zijn
	2.1: het kan ook nog eens gemirrored zijn
	*/
	R->>instance_of(correspondence),
	\+ @nil = R<<-argument1Value,

	S *= string('%s%sv_correspondence(%s,%s,%s,%s)',
		when(
			R?directed == @on,
			'dir_',
			''
		),
		when(
			R?derivative == @on,
			'd',
			''
		),
		?(MF,exportVar,
			R?argument2,R?argument2Route,R?argument2?name,ExportTable,@off,@off),
		?(MF,exportQSValue,
			R?argument2Value,R?argument2,R?argument2Route,ExportTable),
		?(MF,exportVar,
			R?argument1,R?argument1Route,R?argument1?name,ExportTable,@off,@off),
		?(MF,exportQSValue,
			R?argument1Value,R?argument1,R?argument1Route,ExportTable)
	).
%
exportRelation(MF, R: parRelation, ExportTable: lookupTable, S: string):<-

	%3: qscorrespondence
	/*LET OP: ARGUMENTEN ZIJN ANDERSOM:
	In vgarp: wijst de pijl van de beïnvloedende naar de beïnvloede
	In garp: is het eerste argument de beïvloede..
	2.1: het kan ook nog eens van een derivative afkomen
	*/
	R->>instance_of(correspondence),
	@nil = R<<-argument1Value,
	@off = R<<-full, %not full
	S *= string('%s%s%sq_correspondence(%s,%s)',
		when(
			R?directed == @on,
			'dir_',
			''
		),
		when(
			R?mirror == @on,
			'mirror_',
			''
		),
		when(
			R?derivative == @on,
			'd',
			''
		),
		?(MF,exportVar,
			R?argument2,R?argument2Route,R?argument2?name,ExportTable,@off,@off),
		?(MF,exportVar,
			R?argument1,R?argument1Route,R?argument1?name,ExportTable,@off,@off)
	).
%
exportRelation(MF, R: parRelation, ExportTable: lookupTable, S: string):<-

	%4: full correspondence
	%like qs correspondence we asume we have to swap the arguments

	R->>instance_of(correspondence),
	@on = R<<-full, %not full
	S *= string('%sfull_correspondence(%s,%s)',
		when(
			R?directed == @on,
			'dir_',
			''
		),
		?(MF,exportVar,
			R?argument2,R?argument2Route,R?argument2?name,ExportTable,@off,@off),
		?(MF,exportVar,
			R?argument1,R?argument1Route,R?argument1?name,ExportTable,@off,@off)
	).
%
exportRelation(MF, R: parRelation, ExportTable:lookupTable, S: string):<-

	%5: garpQuantityRelation (prop en inf)
	/*LET OP: ARGUMENTEN ZIJN ANDERSOM:
	In vgarp: wijst de pijl van de beïnvloedende naar de beïnvloede
	In garp: is het eerste argument de beïvloede..
	*/
	R->>instance_of(garpQuantityRelation),

	% Rewrite to add prop_mult and prop_diw
	get(R, sign, Sign),
	(
	    Sign == plus ->
	    SignEngine = pos
	;
	    Sign == min ->
	    SignEngine = neg
	;
	    SignEngine = Sign
	),


	S *= string('%s_%s%s(%s,%s)',
		R?type,
		SignEngine,
		%when(
		%		R?sign == plus,
		%		'pos',
		%		'neg'),
		when(
			R?type == inf,
			'_by',
			''
		),
		?(MF,exportVar,
			R?argument2,R?argument2Route,R?argument2?name,ExportTable,@off,@off),
		?(MF,exportVar,
			R?argument1,R?argument1Route,R?argument1?name,ExportTable,@off,@off)
	).

%


exportInequalityType(_MF,I: inequality, Type: string):<-
	%geef het type terug

	AT = I<<-argument1Type,
	Type *= string,

	if
	(
		AT == currentDerivative
	;
		AT == derivativeZero
	;
		(
			AT == calculus,
			derivative = I?argument1<<-type
		)
	)
	then
		Type->>append('d_'),

	T = I<<-type,
	(
		( T == l, Type->>append('smaller'))
	;
		( T == leq, Type->>append('smaller_or_equal'))
	;
		( T == eq, Type->>append('equal'))
	;
		( T == geq, Type->>append('greater_or_equal'))
	;
		( T == g, Type->>append('greater'))
	).
%
exportInequalityArg(MF,
	Argument: 'garpQuantity|calculus',
	ArgumentRoute: chain,
	ArgumentType: {currentValue,currentDerivative,quantityQSPoint,derivativeZero,calculus},
	_ArgumentQSPoint: 'valueReference*',
	ExportTable: lookupTable,
	Arg: string):<-
	%geef een exportstring voor het argument terug

	(
		ArgumentType == currentValue
	;
		ArgumentType == currentDerivative
	),!,
	Arg = MF<<-exportVar(Argument,ArgumentRoute,Argument?name,ExportTable,@off,@off).
%
exportInequalityArg(MF,Argument,Route,ArgumentType,ArgumentQSPoint,ExportTable,Arg):<-
	ArgumentType == quantityQSPoint,
	Arg = MF<<-exportQSValue(ArgumentQSPoint,Argument,Route,ExportTable).
%
exportInequalityArg(_MF,_Argument,_Route,ArgumentType,_ArgumentQSPoint,_ExportTable,Arg):<-
	ArgumentType == derivativeZero,
	Arg *= string('zero').
%
exportInequalityArg(MF,Argument,Route,ArgumentType,_ArgumentQSPoint,ExportTable,Arg):<-
	ArgumentType == calculus,
	Arg = MF<<-exportCalculus(Argument,Route,ExportTable).
%

exportCalculus(MF, Calc: calculus, Route: chain, ExportTable: lookupTable, Export: string):<-
	%export de calculus naar een garpstring

	Export *= string('%s(%s,%s)',
		Calc?sign,
		when(
			->>(Calc?argument1,instance_of,garpQuantity),
			when(
				Calc?argument1QSPoint == @nil,
				?(MF,exportVar,Calc?argument1,?(Route, merge, Calc?argument1Route),
						Calc?argument1?name,ExportTable,@off,@off),
				?(MF,exportQSValue,Calc?argument1QSPoint,Calc?argument1,
						?(Route, merge, Calc?argument1Route),ExportTable)
			),
			?(MF,exportCalculus,Calc?argument1,
					?(Route, merge, Calc?argument1Route),ExportTable)
		),
		when(
			->>(Calc?argument2,instance_of,garpQuantity),
			when(
				Calc?argument2QSPoint == @nil,
				?(MF,exportVar,Calc?argument2,?(Route, merge, Calc?argument2Route),
					Calc?argument2?name,ExportTable,@off,@off),
				?(MF,exportQSValue,Calc?argument2QSPoint,Calc?argument2,
					?(Route, merge, Calc?argument2Route),ExportTable)
			),
			?(MF,exportCalculus,Calc?argument2,
					?(Route, merge, Calc?argument2Route),ExportTable)
		)
	).
%

%%

%%
exportQSValue(MF,VR: valueReference, Q: garpQuantity, Route: chain, ExportTable: lookupTable,
		ValueString: string):<-
	%export een qs-waarde, eventueel met argument
	if
	(
		interval = VR<<-type
	;
		get(VR, valueName, ValueName),
		member(ValueName, ['Zero', 'One']) % 'Minusone'
	)
	then
		ValueString = VR?valueName?exportName
	else
		ValueString *= string('%s(%s)',
								VR?valueName?exportName,
								?(MF,exportVar,Q,Route,Q?name,ExportTable,@off,@off)).
%%

%%
exportSystemStructures(MF,BasisTab: int,ExportTable: lookupTable,F: file, Counter: number):->
	%schrijf de system_structures (altijd conditioneel) weg
	%gp3 1.4: Counter is just a counter, we use it to save the elements in @app->>addExportedElement

	Tab *= string,
	Tab->>insert_character('\t',times:= BasisTab),
	F->>format('%ssystem_structures([',Tab),

	Comma *= var,
	Comma->>assign('\n',global),

	ImportedFragments = MF<<-findElements(importedFragment),
	ImportedFragments->>sort(?(@arg1?referencedFragment?name,
		compare,
		@arg2?referencedFragment?name)),

	ImportedFragments->>for_all(
		and(
			->>(F,format,
				'%s%s\t%s%s',
				Comma,
				when(
					@arg1?remarks?size > 0,
					create(string,'%s\n',?(@arg1?remarks,makeRemarks,BasisTab + 2)),
					''
				),
				Tab,
				?(@arg1?referencedFragment,exportStructure,
					 create(chain,@arg1),ExportTable)
			),
			assign(Comma,',\n',global),
			%gp3 1.4: export by counter
			->>(@app,addExportedElement,MF,Counter,@arg1),
			->>(Counter,plus,1)
		)
	),
	F->>format('\n%s\t])',Tab).
%%

%%
exportStaticType(MF,
	Name: {description_view, composition_view}):<-
	%Als dit MF een static modelfragment zou zijn, zou het dan als description_view of als
	%composition_view te boek staan. Aangeroepen in export, om voor evt static fragment parents
	%te bepalen welke het moet zijn
	%(en eventueel composition_view als parent toe te voegen)

	%gp3 this is now legacy code. It is only used when modelFragment->>export was called
	%with legacyNaming = @on. (export to legacy model).
	%otherwise, both kinds of modelfragment are called: static

	if
		MF?parents->>find(@arg1?exportStaticType == composition_view)
	then
		Name = composition_view
	else
	(
		%hebben we zelf meerdere instanties?
		if
		(
			Num *= number(0),
			MF->>checkAllUp(and(
				->>(Num,plus,?(@arg1,findElements,garpInstance)?size),
				->>(Num,larger,1)
				),
				@on, @off
			)
		)
		then
			Name = composition_view
		else
			Name = description_view
	).


%%%%%%
%%
exportText(MF,F: file):->
	%gp3 1.4: added for textual export, just a rewrite of the normal export code

	%just output the givens and the conditions etc

	F->>format('\nModel fragment: %s\n',MF?name),
	F->>format('Type: %s\n',MF?type),
	F->>format('Status: %s\n', when(MF?active,'active','inactive')),

	if
		(\+ 0 = MF?remarks<<-size)
	then
		F->>format('Remarks:\n%s\n', ?(MF?remarks,tab,1)),

	%parents:
	Parents = MF?parentReferences<<-map(@arg1?referencedFragment?name),
	F->>format('Parents: %s\n', ?(Parents,join,', ')),
	F->>format('Conditions:\n'),
	MF->>exportConditionalFragments_text(1,F),
	MF->>exportSystemElements_text(condition,1,F),
	MF->>exportIdentities_text(1,F),
	MF->>exportQuantities_text(condition,1,F),
	MF->>exportValues_text(condition,1,F),
	MF->>exportRelations_text(condition,1,F),

	F->>format('Givens:\n'),
	MF->>exportSystemElements_text(consequence,1,F),
	MF->>exportQuantities_text(consequence,1,F),
	MF->>exportValues_text(consequence,1,F),
	MF->>exportRelations_text(consequence,1,F).
%
exportConditionalFragments_text(MF, Tab: int, F: file):->
	ImportedFragments = MF<<-findElements(importedFragment), %also: refiner
	unless
		ImportedFragments->>empty
	do
	(
		ImportedFragments->>sort(?(@arg1?referencedFragment?name,
			compare,
			@arg2?referencedFragment?name)),
		F->>format(?('Model fragments:',tab, Tab)),
			F->>append('\n'), %using \n in ?tab ed strings causes problems
		ImportedFragments->>for_all(
			->>(MF,exportConditionalFragments_text_mf,@arg1, Tab + 1, F))
	).
%
exportConditionalFragments_text_mf(_MF, CondMF: importedFragment, Tab: int, F: file):->

%helper: display conditional fragment
%might also be a refiner, but the refined one is then never a condition
%in this mf (but in a parent or condition)

	Info *= string('%s',CondMF?referencedFragment?name),
	if
		(\+ 0 = CondMF?remarks<<-size)
	then
	(
		Info->>append('\n'),
		Info->>append(?(string('--- Remarks:\n%s\n---',CondMF?remarks),tab,1))
	),
	F->>append(?(Info,tab,Tab)),
	F->>append('\n'). %not in the tabbed string, because that gives problems in pce
%
exportIdentities_text(MF, Tab: int, F: file):->
	Identities = ?(MF,findElements,identityRelation)<<-find_all(
			not(->>(@arg1,instance_of,fragmentRefinerIdentity))),
	unless
		Identities->>empty
	do
	(
		F->>format(?('Identities:',tab, Tab)),
		F->>append('\n'),
		Identities->>for_all(
			->>(MF,exportIdentities_text_id,@arg1,Tab + 1, F))
	).
%
exportIdentities_text_id(_MF, Id: identityRelation,Tab: int, F: file):->
	Info *= string('%s', Id?argument1?name),
	unless
		Id?argument1Route->>empty
	do
		Info->>append(string(' (%s)',Id?argument1Route?head?referencedFragment?name)),
	Info->>append(string(' == %s',Id?argument2?name)),
	unless
		Id?argument2Route->>empty
	do
		Info->>append(string(' (%s)',Id?argument2Route?head?referencedFragment?name)),
	if
		(\+ 0 = Id?remarks<<-size)
	then
	(
		Info->>append('\n'),
		Info->>append(?(string('--- Remarks:\n%s\n---',Id?remarks),tab,1))
	),
	F->>append(?(Info,tab,Tab)),
	F->>append('\n'). %not in the tabbed string, because that gives problems in pce
%
exportSystemElements_text(MF, StateName: {condition, consequence}, Tab: int, F: file) :->
	%helper for exportText: exports conditional / consequential instances to text

	Instances = ?(MF,findElements,garpInstance)<<-find_all(
					@arg1?stateName == StateName),
	Instances->>sort(?(@arg1?name,compare,@arg2?name)),
	EntityInstances = Instances<<-find_all(->>(@arg1?entity,instance_of,entity)),
	AgentInstances = Instances<<-find_all(->>(@arg1?entity,instance_of,agent)),
	AssumptionInstances = ?(MF,findElements,assumptionInstance)<<-find_all(
					@arg1?stateName == StateName),
	AssumptionInstances->>sort(?(@arg1?name,compare,@arg2?name)),

	unless
		EntityInstances->>empty
	do
	(
		F->>format(?('Entity Instances:',tab, Tab)),
		F->>append('\n'), %using \n in ?tab ed strings causes problems
		EntityInstances->>for_all(
			->>(MF,exportSystemElements_text_Element, @arg1, F, @on, Tab + 1) %helper
		)
	),

	unless
		AgentInstances->>empty
	do
	(
		F->>format(?('Agent Instances:',tab, Tab)),
		F->>append('\n'), %using \n in ?tab ed strings causes problems
		AgentInstances->>for_all(
			->>(MF,exportSystemElements_text_Element, @arg1, F, @on, Tab + 1) %helper
		)
	),

	unless
		AssumptionInstances->>empty
	do
	(
		F->>format(?('Assumptions:',tab, Tab)),
		F->>append('\n'), %using \n in ?tab ed strings causes problems
		AssumptionInstances->>for_all(
			->>(MF,exportSystemElements_text_Element, @arg1, F, @off, Tab + 1) %helper
		)
	),

	Attributes = ?(MF,findElements,garpAttribute)<<-find_all(@arg1?stateName == StateName),
	Attributes->>sort(?(@arg1?name,compare,@arg2?name)),
	unless
		Attributes->>empty
	do
	(
		F->>format(?('Attributes:',tab,Tab)),
		F->>append('\n'),
		Attributes->>for_all(
			->>(MF,exportSystemElements_text_attr,@arg1,F,Tab+1)
		)
	),

	Configurations = ?(MF,findElements,configuration)<<-find_all(@arg1?stateName == StateName),
	Configurations->>sort(?(@arg1?name,compare,@arg2?name)),
	unless
		Attributes->>empty
	do
	(
		F->>format(?('Configuations:',tab,Tab)),
		F->>append('\n'),
		Configurations->>for_all(
			->>(MF,exportSystemElements_text_conf,@arg1,F,Tab+1)
		)
	).
%
exportSystemElements_text_Element(_MF, Element: object, F: file, GetEntity: bool, Tab: int) :->
	%helper for displaying one element

	Info *= string('%s',Element?name),
	if
		GetEntity == @on
	then
		Info->>append(string(' (%s)',Element?entity?name)),
	if
		(\+ 0 = Element?remarks<<-size)
	then
	(
		Info->>append('\n'),
		Info->>append(?(string('--- Remarks:\n%s\n---',Element?remarks),tab,1))
	),
	F->>append(?(Info,tab,Tab)),
	F->>append('\n'). %not in the tabbed string, because that gives problems in pce
%
exportSystemElements_text_conf(_MF, Conf: configuration, F: file,Tab: int) :->
	%add information about route when needed

	Info *= string,
	Info->>append(Conf?argument1?name),
	if
		Importing1 = Conf?argument1Route<<-head
	then
	(
		Info->>append(string(' (%s)', Importing1?referencedFragment?name))
	),
	Info->>append(string(' - %s - %s', Conf?name, Conf?argument2?name)),
	if
		Importing2 = Conf?argument2Route<<-head
	then
	(
		Info->>append(string(' (%s)', Importing2?referencedFragment?name))
	),

	if
		(\+ 0 = Conf?remarks<<-size)
	then
	(
		Info->>append('\n'),
		Info->>append(?(string('--- Remarks:\n%s\n---',Conf?remarks),tab,1))
	),
	F->>append(?(Info,tab,Tab)),
	F->>append('\n'). %not in the tabbed string, because that gives problems in pce
%
exportSystemElements_text_attr(_MF, A: garpAttribute, F: file,Tab: int) :->
	%add information about route when needed

	Info *= string,
	Info->>append(A?garpInstance?name),
	if
		Importing = A?instanceRoute<<-head
	then
	(
		Info->>append(string(' (%s)', Importing?referencedFragment?name))
	),
	Info->>append(string(': %s = %s', A?name, A?valueReference?valueName)),

	if
		(\+ 0 = A?remarks<<-size)
	then
	(
		Info->>append('\n'),
		Info->>append(?(string('--- Remarks:\n%s\n---',A?remarks),tab,1))
	),
	F->>append(?(Info,tab,Tab)),
	F->>append('\n'). %not in the tabbed string, because that gives problems in pce
%
exportQuantities_text(MF, StateName: {condition, consequence}, Tab: int, F: file):->
	%helper for exporting quantities. We only export the local ones here, not imported stuff

	Quantities = ?(MF,findElements,garpQuantity)<<-find_all(
					@arg1?stateName == StateName),
	unless
		Quantities->>empty
	do
	(
		F->>format(?('Quantities:',tab, Tab)),
		F->>append('\n'), %using \n in ?tab ed strings causes problems


		Quantities->>sort(?(@arg1?name,compare,@arg2?name)),
		Quantities->>for_all(
			->>(MF, exportQuantities_text_q,@arg1,Tab + 1, F)
		)
	).
%
exportQuantities_text_q(_MF,Q: garpQuantity,  Tab: int, F: file):->

	Info *= string('%s',Q?garpInstance?name),
	if
		Importing = Q?instanceRoute<<-head
	then
	(
		Info->>append(string(' (%s)', Importing?referencedFragment?name))
	),
	Info->>append(string(': %s', Q?name)),

	unless
		(Q?quantityAssumptions->>empty)
	do
	(
		Info->>append('\n'),
		Info->>append(?(string('Quantity behaviour: %s',?(Q?quantityAssumptions, join,', ')), tab, Tab + 1)),
		Info->>append('\n')
	),

	if
		(\+ 0 = Q?remarks<<-size)
	then
	(
		Info->>append('\n'),
		Info->>append(?(string('--- Remarks:\n%s\n---',Q?remarks),tab,1))
	),
	F->>append(?(Info,tab,Tab)),
	F->>append('\n'). %not in the tabbed string, because that gives problems in pce
%
exportValues_text(MF, StateName: {condition, consequence}, Tab: int, F: file):->
	%helper for exporting values.

	Values = ?(MF,findElements,value)<<-find_all(
					@arg1?stateName == StateName),
	Values->>sort(?(@arg1?quantity?name,compare,@arg2?quantity?name)),
	unless
		Values->>empty
	do
	(
		F->>format(?('Quantity values:',tab, Tab)),
		F->>append('\n'), %using \n in ?tab ed strings causes problems
		Values->>for_all(
			->>(MF, exportValues_text_v,@arg1,Tab + 1, F)
		)
	).
%
exportValues_text_v(_MF,V: value,  Tab: int, F: file):->

	Q = V<<-quantity,
	QN *= string('%s',Q?name),
	if
		ImportingQ = V?quantityRoute<<-head
	then
	(
		QN->>append(string(' (%s)', ImportingQ?referencedFragment?name))
	),

	I = Q<<-garpInstance,
	IN *= string('%s', I?name),
	if
	(
		V?quantityRoute->>empty, %otherwise: do not show import route of instance
		ImportingI = Q?instanceRoute<<-head
	)
	then
	(
		IN->>append(string(' (%s)', ImportingI?referencedFragment?name))
	),

	Info *= string('%s - %s: %s = %s', IN, QN, when(V?derivative == @off, 'value', 'derivative'), V?valueReference?valueName),
	F->>append(?(Info,tab,Tab)),
	F->>append('\n'). %not in the tabbed string, because that gives problems in pce
%
exportRelations_text(MF, StateName: {condition, consequence}, Tab: int, F: file):->
	%helper for exporting relations.

	Relations =  ?(MF,findElements,parRelation)<<-find_all(@arg1?stateName == StateName),
	unless
		Relations->>empty
	do
	(
		F->>format(?('Quantity relations:',tab, Tab)),
		F->>append('\n'), %using \n in ?tab ed strings causes problems
		Relations->>for_all(
			->>(MF, exportRelations_text_r,@arg1,Tab + 1, F)
		)
	).
%
exportRelations_text_r(MF,R: parRelation,  Tab: int, F: file):->

	%split by type
	%we do not show any remarks for calculi...

	Info *= string('%s', ?(MF,exportRelations_text_bytype,R)),
	if
		(\+ 0 = R?remarks<<-size)
	then
	(
		Info->>append('\n'),
		Info->>append(?(string('--- Remarks:\n%s\n---',R?remarks),tab,1))
	),
	F->>append(?(Info,tab,Tab)),
	F->>append('\n'). %not in the tabbed string, because that gives problems in pce
%
exportRelations_text_bytype(MF, R: parRelation, N: string):<-
	%1: inequality
	R->>instance_of(inequality),!,
	N *= string('Inequality: '),
	Q1 = R<<-argument1,
	Q1R = R<<-argument1Route,
	Q2 = R<<-argument2,
	Q2R = R<<-argument2Route,

	N->>append(?(MF,exportRelations_text_arg,Q1,Q1R)),
	T1 = R<<-argument1Type,
	(
		T1 == currentDerivative, A1 = ': derivative'
	;
		T1 == quantityQSPoint, A1 = string(': point %s', R?argument1QSPoint?valueName)
	;
		T1 == derivativeZero, A1 = ': zero derivative'
	;
		A1 = ''
	),
	N->>append(string(A1)),
	T = R<<-type,
	nth1(TI, [l,leq,eq,geq,g], T),
	nth1(TI, [' < ',' =< ',' = ',' => ',' > '], AT),
	N->>append(AT),
	unless
	(
		Q1 == Q2,
		Q1R->>equal(Q2R)
	)
	do
		N->>append(?(MF,exportRelations_text_arg,R?argument2,R?argument2Route)),
	T2 = R<<-argument2Type,
	(
		T2 == currentDerivative, A2 = ':derivative'
	;
		T2 == quantityQSPoint, A2 = string(':value:%s', R?argument2QSPoint?valueName)
	;
		T2 == derivativeZero, A2 = ':derivative:zero'
	;
		A2 = ''
	),
	N->>append(A2).
%
exportRelations_text_bytype(MF, R: parRelation, N: string):<-
	%2: correspondence
	R->>instance_of(correspondence),
	!,
	Q1 = R<<-argument1,
	Q1R = R<<-argument1Route,
	Q1V = R<<-argument1Value,
	Q2 = R<<-argument2,
	Q2R = R<<-argument2Route,
	Q2V = R<<-argument2Value,

	N *= string('%s%s%sCorrespondence: %s%s %s %s%s',
		when(R?full == @on, 'Full ',''),
		when(R?mirror == @on, 'Inverse ',''),
		when(R?derivative == @on, 'Derivative ',''),
		?(MF,exportRelations_text_arg,Q1,Q1R),
		when(Q1V == @nil,'',create(string,':%s',Q1V?valueName)),
		when(R?directed == @on, '-->','<->'),
		?(MF,exportRelations_text_arg,Q2,Q2R),
		when(Q2V == @nil,'',create(string,':%s',Q2V?valueName))
	).

exportRelations_text_bytype(MF, R: parRelation, N: string):<-
	%3: garpQuantityRelation
	R->>instance_of(garpQuantityRelation),
	!,
	Q1 = R<<-argument1,
	Q1R = R<<-argument1Route,
	Q2 = R<<-argument2,
	Q2R = R<<-argument2Route,

	N *= string('%s %s: from %s to %s',
		when(R?sign == 'plus', 'Positive','Negative'),
		when(R?type == 'inf', 'Influence', 'Proportionality'),
		?(MF,exportRelations_text_arg,Q1,Q1R),
		?(MF,exportRelations_text_arg,Q2,Q2R)
	).

exportRelations_text_bytype(_,_,N):<-
	N = '?'.
%
exportRelations_text_arg(_MF,Arg: object, ArgRoute: chain, N: string):<-
	%get the right description for the arg
	%1: quantity
	Arg->>instance_of(garpQuantity),

	QN *= string('%s',Arg?name),
	if
		ImportingQ = ArgRoute<<-head
	then
	(
		QN->>append(string(' (%s)', ImportingQ?referencedFragment?name))
	),

	I = Arg<<-garpInstance,
	IN *= string('%s', I?name),
	if
	(
		ArgRoute->>empty, %otherwise: do not show import route of instance
		ImportingI = Arg?instanceRoute<<-head
	)
	then
	(
		IN->>append(string(' (%s)', ImportingI?referencedFragment?name))
	),

	N *= string('%s:%s', IN, QN).
%
exportRelations_text_arg(MF,Arg: object, _ArgRoute: chain, N: string):<-
	%get the right description for the arg
	%2: calculus
	Arg->>instance_of(calculus),
	N *= string('('),
	N->>append(?(MF,exportRelations_text_arg,Arg?argument1,Arg?argument1Route)),
	if
		Arg?argument1->>instance_of(garpQuantity)
	then
	(
		if
			derivative = Arg<<-type
		then
			N->>append(': derivative'),
		unless
			@nil = Arg<<-argument1QSPoint
		do
			N->>append(string(': point %s', Arg?argument1QSPoint?valueName))
	),

	if
		plus = Arg<<-sign
	then
		N->>append(' + ')
	else
		N->>append(' - '),

	N->>append(?(MF,exportRelations_text_arg,Arg?argument2,Arg?argument2Route)),
	if
		Arg?argument2->>instance_of(garpQuantity)
	then
	(
		if
			derivative = Arg<<-type
		then
			N->>append(': derivative'),
		unless
			@nil = Arg<<-argument1QSPoint
		do
			N->>append(string(': point %s', Arg?argument2QSPoint?valueName))
	),
	N->>append(')').
:-pce_end_class.

