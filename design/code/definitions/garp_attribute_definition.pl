/*
Definitie garpAttributeDefinition class
*/

:-pce_begin_class(
		  garpAttributeDefinition(name),
		  object,
		  "garp attribute definition"
		 ).

variable(name_translatable,translatable,both,"Name of the object"). %gp3 1.4: added this, name is just a get/set method now
variable(remarks_translatable, translatable, both, "Remarks on this object"). %gp3 1.4: added this, remarks is just a get/set method now

variable(values,chain,none,"Values allowed in this attribute definition"). %(valueRefence objects --> translatable)

initialise(AD,
	Name : name,
	Values : chain,
	Remarks : [string]
	):->
	%initialiseer de definitie, zodat ie goed past op CRs..

	AD->+initialise,
	TR = @model<<-translator,
	AD->>name_translatable(?(TR,getTranslatable,unique)),
	AD->>name(Name),

	default(Remarks,'',RRemarks),
	AD->>remarks_translatable(?(TR,getTranslatable,empty)),
	AD->>remarks(RRemarks),
	AD->>slot(values,new(chain)),
	AD->>values(Values).
%%

%%%%% wrapping translatables %%%%%%%%
%%
name(D,Name : name):->
	"Set the name, internal" ::

	%is now a wrapper around name_translatable
	D?name_translatable->>value(Name?makeGarp).
%
name(D, Name: name):<-
	%get the name
	Name = D?name_translatable<<-value.
%%

%%
remarks(D, Remarks: string):->
	"Set a copy of the given string as remarks" ::

	%is now a wrapper around remarks_translatable
	
	D?remarks_translatable->>value(?(Remarks,strip,both)).
%
remarks(D,	Remarks : string
       ):<-
	"Return a copy of the remarks" ::
	
	Remarks = D?remarks_translatable?value<<-copy.
%%
%%%%%%%%%%


%%
values(AD,
	Values : chain
	):->
	"Set the list of values as a copy of the given values" ::

	NewValues = Values<<-map(@arg1?copy), %Maar weer een kopie
	AD->>slot(values,NewValues).
%%

%%
values(AD,
	Values : chain):<-
	"Return a new list containing copies of the values (with same ID)" ::

	RV = AD<<-slot(values),
	Values = RV<<-map(@arg1?copy).
%%

%%
attributes(AD,
			A: chain
			):<-
	"Return a new chain with all attributes based on this definition" ::
	A = AD<<-all_named_hypered(garpAttribute).
%%

%%
%relevantComments
%gp3 1.4: return a string with the remarks to show for this one
%when no remarks: return an empty string

relevantComments(D,RC: string):<-
	if
		0 = D?remarks<<-size
	then
		RC *= string
	else
		RC *= string('AD %s: %s', D?name,D?remarks).
%%

%%%%%%%%%%%%%CHANGES%%%%%%%%%%%%%%
%%
checkChange_addAttributeDef(AD,
	CR:changeRequestor):->
	%checken op naamconflict

	NewName = CR<<-argument(1),
	NewName->>equalExportName(AD?name),
	CR->>impossible('The given name is already used', AD).
%%

checkChange_changeAttributeDef(AD,
			    CR : changeRequestor
			   ):->
	%als we zelf het object van de change zijn checken we of
	%alle data correct is (via een norm methode bij het model)
	%als we niet zelf het object vd change zijn checken we op
	%naamconflict..

	%1: we zijn het object
	(   CR->>checkObject(AD) %we zijn dezelfde
	->  @model->>norm_attributeDef(CR,AD)
	;   %2: we zijn niet het object
	    NewName = CR<<-argument(1),
		NewName->>equalExportName(AD?name), %gelijk?
	    CR->>impossible('The given name is already used',AD)
	).
%
applyChange_changeAttributeDef(AD,
			      CR: changeRequestor
			     ):->
	%Deze definitie verandert dus
	
	AD->>name(?(CR,argument,1)),
	AD->>values(?(CR,argument,2)),
	AD->>remarks(?(CR,argument,3)).
%%

checkChange_addConfigurationDef(AD,
	CR:changeRequestor):->
	%checken op naamconflict: configuratie + attributen delen één namespace

	NewName = CR<<-argument(1),
	NewName->>equalExportName(AD?name),
	CR->>impossible('The given name is already used by an attribute', AD).
%%

%%
checkChange_changeConfigurationDef(AD,
			    CR : changeRequestor
			   ):->
	%checken op naamconflict bij gewijzigde configuratie
	
	NewName = CR<<-argument(1),
	NewName->>equalExportName(AD?name),
	CR->>impossible('The given name is already used by an attribute',AD).

%%%%%%%%%%%%%%%%% EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
exportText(AD,F: file):->
	%gp3 1.4: added for textual export
	
	F->>format('\nAttribute definition: %s\n',AD?name),
	if
		(\+ 0 = AD?remarks<<-size)
	then
		F->>format('Remarks:\n%s\n',?(AD?remarks,tab,1)),

	%values: just map them to their names
	ValueNames = AD?values<<-map(@arg1?valueName),
	F->>format('Values:\n%s\n',?(?(ValueNames,join,', '),tab,1)).
%%
:-pce_end_class.
		  
