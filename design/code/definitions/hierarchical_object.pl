/*
Definitie hierarchicalObject class
*/

%Abstracte class.
:-pce_begin_class(
		  hierarchicalObject,
		  object,
		  "Abstract super of classes of hierarchically structured objects (single inheritance)"
		 ).

variable(name_translatable,translatable,both,"Name of the object"). %gp3 1.4: added this, name is just a get/set method now
variable(remarks_translatable, translatable, both, "Remarks on this object"). %gp3 1.4: added this, remarks is just a get/set method now

%%
initialise(HO, Name: name = name, Remarks : remarks = [string], T: translator = [translator]):->
			
	%gp3 1.4 added translator argument, only needed when initializing model fragments withoud a @model
	send(HO,send_super,initialise),
	default(T,@model?translator,TR),
	HO->>name_translatable(?(TR,getTranslatable,unique)),
	HO->>name(Name),

	default(Remarks,'',RRemarks),
	HO->>remarks_translatable(?(TR,getTranslatable,empty)),
	HO->>remarks(RRemarks).
%%

%%%%% wrapping translatables %%%%%%%%
%%
name(HO,Name : name):->
	"Set the name, internal" ::

	%is now a wrapper around name_translatable
	HO?name_translatable->>value(Name?makeGarp).
%
name(HO, Name: name):<-
	%get the name
	Name = HO?name_translatable<<-value.
%%

%%
remarks(HO, Remarks: string):->
	"Set a copy of the given string as remarks" ::

	%is now a wrapper around remarks_translatable
	
	HO?remarks_translatable->>value(?(Remarks,strip,both)).
%
remarks(HO,	Remarks : string
       ):<-
	"Return a copy of the remarks" ::
	
	Remarks = HO?remarks_translatable?value<<-copy.
%%

%%
%relevantComments
%gp3 1.4: return a string with the remarks to show for this one
%when no remarks: return an empty string

relevantComments(HO,RC: string):<-
	if
		0 = HO?remarks<<-size
	then
		RC *= string
	else
		RC *= string('%s %s: %s', HO?typeString,HO?name,HO?remarks).

	%%this version does not display comments for parents
	%%this can be done easily: just remove comment signs below
	%%and find a solution in find_in_design.pl to make sure definitions are not added twice
	%%(when an instance is added in a state while in MF 1 it is an instance of Entity 1, and in MF 2 of a parent entity)
	/*
	%parents
	if 
		HO<<-parent
	then
		RC->>ensure_nl(HO?parent?relevantComments).
	*/
%%

%%
%typeString: gp3 1.4 helper for relevantComments, this one is abstract
typeString(_HO, TS: name):<-
	TS = 'HO'.
%%

%%%%%%


addChild(HO,
	 Child : child = hierarchicalObject
	):->
	"Register the argument as child of the receiver, fail if not of the same class" ::
	%maak de hyper die aangeeft dat het een kind van deze is
	%class moet PRECIES gelijk zijn
	%oude hyper wordt verwijderd

	Child?class->>equal(HO?class),
	Child->>delete_hypers(parent), %de oude weg
	HO->>hyper(Child,child,parent).
%%

%%
parent(HO,Parent : hierarchicalObject) :<-
	"Return the parent of this object" ::
	Parent = HO<<-hypered(parent).
%%

%%
children(HO, Children : chain) :<-
	"Return chain of all children" ::
	Children = HO<<-all_named_hypered(child).
%%

%%
for_all_up(HO,Msg: action =  code):->
	"Run code on this object and all objects up the hierarchy, like chain->for_all" ::
	%@arg1 is de entiteit, net als chain->for_all

	HO->>for_all_up_internal(Msg,new(chain)). %die doet de rest
%
for_all_up_internal(HO,Msg: action = code, Done : chain):->
	"Internal implementation of for_all_up" ::
	(   Done->>member(HO) %deze hebben we al gedaan
	->  true
	;   Done->>append(HO), %zodat we hem niet nog een keer doen
	    Msg->>forward(HO), %wijzelf
		(
			(
				P = HO<<-parent,!,
				P->>for_all_up_internal(Msg,Done)
			);
			true
		)
	).
%%

%%
for_some_up(HO,Msg: action = code):->
	"Run code on this entity and all entities up, like chain->for_some" ::
	
	HO->>for_some_up_internal(Msg,new(chain)).
%
for_some_up_internal(HO,Msg: action = code, Done : chain):->
	"Internal implementation for for_some_up" ::
	(   Done->>member(HO) %al gedaan
	->  true
	;   Done->>append(HO),
	    ignore(Msg->>forward(HO)), %wijzelf
		(
			(
				P = HO<<-parent,!,
				P->>for_some_up_internal(Msg,Done)
			);
			true
		)
	).
%%

%%
for_all_down(HO,Msg: action =  code):->
	"Run code on this entity and all entities down, like chain->for_all" ::
	%@arg1 is het object, net als chain->for_all

	HO->>for_all_down_internal(Msg,new(chain)). %die doet de rest
%
for_all_down_internal(HO,Msg: action = code, Done : chain):->
	"Internal implementation of for_all_down" ::
	(   Done->>member(HO) %deze hebben we al gedaan
		->  true
	;	Done->>append(HO), %zodat we hem niet nog een keer doen
	    Msg->>forward(HO), %wijzelf
	    HO?children->>for_all(->>(@arg1,
				      for_all_down_internal,
				      Msg,
				      Done))
	).
%%

for_some_down(HO,Msg: action = code):->
	"Run code on this entity and all entities down, like chain->for_some" ::
	
	HO->>for_some_down_internal(Msg,new(chain)).
%
for_some_down_internal(HO,Msg: action = code, Done : chain):->
	"Internal implementation for for_some_down" ::
	(   Done->>member(HO) %al gedaan
		->  true
	;	Done->>append(HO),
	    ignore(Msg->>forward(HO)), %wijzelf
	    HO?children->>for_some(->>(@arg1,
				       for_some_down_internal,
				       Msg,
				       Done))
	).
%%

%%
isUp(HO,
	P : hierarchicalObject
	):->
	"Succeeds if the argument is an (indirect) parent of this object" ::

	HO \== P,
	N *= number(0),
	HO->>for_all_up(if(@arg1 == P,
                    ->>(N,plus,1))),
	N->>larger(0). %slaagt dus als er een parent is gevonden.
%%

%%
isDown(HO,
	P : hierarchicalObject
	):->
	"Succeeds if the argument is an (indirect) child of this object" ::

	HO \== P,
	N *= number(0),
	HO->>for_all_down(if(@arg1 == P,
						->>(N,plus,1))),
	N->>larger(0).
%%

%%%%%%%%%%%%%CHANGE%%%%%%%%%%%%%%%
checkChange_newHObjectChild(HO,
			   CR: changeRequestor
			   ):->
	%impossible als de naam hetzelfde is als de naam van de ontvanger

    CR?arg1->>equalExportName(HO?name), 
	S *= string('This name is already used by an %s', HO?class_name),
	CR->>impossible(S,HO).
%
applyChange_newHObjectChild(HO,
			   CR: changeRequestor
			  ):->
	%De nieuwe wordt ons kind
	Child = @pce<<-instance(HO?class,
				CR?arg1,CR?arg2),
	@model->>insertDefinitionObject(Child),
	HO->>addChild(Child),
	%en dit resultaat slaan we op in de changeRequestor
	CR->>result(Child).
%%

checkChange_changeHObject(HO,
			      CR : changeRequestor
			     ):->
	%check op naamconflict en overervingsrecursie
	
	
	%1. name change is impossible als de nieuwe naam hetzelfde is als van de ontvanger en de ontvanger niet zelf het object is dat gewijzigd wordt
	% het hoeft niet dezelfde class tezijn: HObjects zijn over alle objecten uniek benoemd
	\+ HO = CR<<-object, %niet dezelfde (want dan geen probleem)
	NewName = CR<<-argument(1),
	NewName->>equalExportName(HO?name), %gelijk?
	S *= string('This name is already used by an %s', HO?class_name), %mazzel met de drie termen met elk een klinker aan het begin
	CR->>impossible(S,HO).
%
checkChange_changeHObject(HO,
			      CR : changeRequestor
			     ):->

	%2. Er is een cyclus: het object van de CR is een parent van dit object
	%en wordt een kind
	
	CR->>checkArgument(2,HO), %dit wordt de directe parent van het object
	HO->>isUp(CR?object), %terwijl hij nu een (klein-)kind ervan is
	CR->>impossible('A child would be the parent', HO).
%
%
applyChange_changeHObject(HO,
	CR: changeRequestor):->
	%dit object moet bijwerken
	
	HO->>name(CR?arg1),
	%gp3: arg2 can be nil for no change (i.e. when there is no parent for a top node)
	unless
		@nil = CR<<-arg2
	do
		CR?arg2->>addChild(HO),
	HO->>remarks(CR?arg3).
%%

%deleteHObjectTree
checkChange_deleteHObjectTree(HO,
	CR: changeRequestor):->
	%als het over dit object gaat, en hij heeft kinderen, dan waarschuwen we
	
	CR->>checkObject(HO),
	\+ HO?children->>empty,
	CR->>warning('This will delete the whole subtree', HO).
%
applyChange_deleteHObjectTree(HO,
	CR: changeRequestor):->
	%deze moet weg, maar dus ook alle children: we melden het model dat hij er niet meer is
	%en laten de CR weten dat ie weg moet
	HO->>for_some_down(and(
		->>(@model,removeDefinitionObject,@arg1),
		->>(CR,freeObject,@arg1))).

%%%%%%%%%%%%EXPORT%%%%%%%%%%%%%%%%
export(HO,ParentName: name, Isa: file):->
	%schrijf onze plaats in de hierarchie weg, en daarna die van onze kinderen
	
	ExportName = HO?name<<-exportName,
	
	Isa->>format('isa(%s,%s).',ExportName,ParentName),
	if
		(\+ 0 = HO?remarks<<-size)
	then
		Isa->>format('\n%s\n',?(HO?remarks,makeRemarks,1))
	else
		Isa->>append('\n'),
	C = HO<<-children,
	C->>sort(?(@arg1?name,compare,@arg2?name)),
	C->>for_all(->>(@arg1,export,ExportName,Isa)).
%%

%%
exportText(HO,F: file, T: int):->
	%gp3 1.4: added for textual export
	
	F->>append(?(HO?name,tab,T)),
	if
		(\+ 0 = HO?remarks<<-size)
	then
		F->>format(?(string('\n--- Remarks:\n%s\n---',HO?remarks),tab,T)),
	F->>append('\n'), %clear any tabbing that pce forgot to fix
	NewT is T + 1,
	C = HO<<-children,
	C->>sort(?(@arg1?name,compare,@arg2?name)),
	C->>for_all(->>(@arg1,exportText,F,NewT)).	
:-pce_end_class.
  
