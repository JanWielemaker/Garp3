/*
Definitie garpQuantityDefinition class
*/

:-pce_begin_class(
		  garpQuantityDefinition(name),
		  object,
		  "garp quantity definition"
		 ).

variable(name_translatable,translatable,both,"Name of the object"). %gp3 1.4: added this, name is just a get/set method now
variable(remarks_translatable, translatable, both, "Remarks on this object"). %gp3 1.4: added this, remarks is just a get/set method now

initialise(QD,
	Name : name,
	AllowedQS : chain,
	Remarks : [string]
	):->
	%initialiseer de definitie, zodat ie goed past op CRs..

	QD->+initialise,
	TR = @model<<-translator,
	QD->>name_translatable(?(TR,getTranslatable,unique)),
	QD->>name(Name),

	default(Remarks,'',RRemarks),
	QD->>remarks_translatable(?(TR,getTranslatable,empty)),
	QD->>remarks(RRemarks),

	AllowedQS->>for_all(->>(QD,hyper,@arg1,allowedQuantitySpace,quantityDefinition)).
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
quantities(QD,
			Quantities: chain
			):<-
	Quantities = QD<<-all_named_hypered(garpQuantity).
%%

%%
quantitySpaceAllowed(QD,
	QS : quantitySpace
	):->
	"Succeeds if the given qs is allowed with this definition" ::

	QD<<-hypered(allowedQuantitySpace,
					@arg3 == QS).
%%

%%
allowedQuantitySpaces(QD,
	C : chain
	):<-
	"Returns name-sorted chain of all quantity spaces that are allowed with this definition" ::

	C = QD<<-all_named_hypered(allowedQuantitySpace),
	C->>sort(?(@arg1?name,compare,@arg2?name)).
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
		RC *= string('QD %s: %s', D?name,D?remarks).
%%

%%%%%%%%CHANGES%%%%%%%%%%%%%%%%%%%%%%
checkChange_changeQuantityDef(QD,
			    CR : changeRequestor
			   ):->
	%als we zelf het object van de change zijn checken we of
	%alle data correct is (via een norm methode bij het model)
	%als we niet zelf het object vd change zijn checken we op
	%naamconflict..

	%1: we zijn het object

	(   CR->>checkObject(QD)
	->  @model->>norm_quantityDef(CR,QD)
	;   NewName = CR<<-argument(1),
		NewName->>equalExportName(QD?name), %gelijk?
	    CR->>impossible('The given name is already used',QD)
	).
%
applyChange_changeQuantityDef(QD,
			      CR: changeRequestor
			     ):->
	%Deze definitie verandert dus
	
	NewName = CR<<-argument(1),
	QD->>name(NewName),
	AllowedQS = CR<<-argument(2),
	%oude hypers weg
	QD->>delete_hypers(allowedQuantitySpace),
	AllowedQS->>for_all(->>(QD,hyper,@arg1,allowedQuantitySpace,quantityDefinition)),
	NewRemarks = CR<<-argument(3),
	QD->>remarks(NewRemarks).
%%

%%
checkChange_addQuantityDef(QD,
	CR:changeRequestor):->
	%checken op naamconflict

	NewName = CR<<-argument(1),
	NewName->>equalExportName(QD?name),
	CR->>impossible('The given name is already used', QD).
%%

%%
checkChange_deleteQuantitySpace(QD,
	CR:changeRequestor):->
	%checken of dit een toegestane QS is bij deze definitie.
	%als dat zo is hangt het er nog van af of er nog QS-en over zijn na de verwijdering..

	QS = CR<<-argument(1),
	if
		QD->>quantitySpaceAllowed(QS)
	then (
		if
			1 = QD?allowedQuantitySpaces<<-size
		then
			%ok, dan mag het gewoon niet
			CR->>impossible(string('The quantity definition "%s" has no other allowed quantity spaces',
									QD?name),
							QD)
		else
			%mag wel, doch wel waarschuwen
			CR->>warning(
				string('The quantity space will be removed from the list of the quantity definition "%s"',
							QD?name),
				QD)
		).
%%

%%%%%%%%%%%%%%%%% EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
exportText(QD,F: file):->
	%gp3 1.4: added for textual export
	
	F->>format('\nQuantity definition: %s\n',QD?name),
	if
		(\+ 0 = QD?remarks<<-size)
	then
		F->>format('Remarks:\n%s\n',?(QD?remarks,tab,1)),
	AllQS = QD?allowedQuantitySpaces<<-map(@arg1?name),
	F->>format('Allowed quantity spaces: %s\n',?(AllQS,join,', ')).
%%
		
:-pce_end_class.
		  
