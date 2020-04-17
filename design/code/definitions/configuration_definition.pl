/*
Definitie configurationDefinition class
*/

:-pce_begin_class(
		  configurationDefinition(name),
		  object,
		  "garp configuration definition"
		 ).

variable(name_translatable,translatable,both,"Name of the object"). %gp3 1.4: added this, name is just a get/set method now
variable(remarks_translatable, translatable, both, "Remarks on this object"). %gp3 1.4: added this, remarks is just a get/set method now

initialise(D,
	Name : name,
	Remarks : [string]
	):->
	%initialiseer de definitie, zodat ie goed past op CRs..

	D->+initialise,
	TR = @model<<-translator,
	D->>name_translatable(?(TR,getTranslatable,unique)),
	D->>name(Name),

	default(Remarks,'',RRemarks),
	D->>remarks_translatable(?(TR,getTranslatable,empty)),
	D->>remarks(RRemarks).
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
configurations(D,
			C: chain
			):<-
	"Return a new chain with all configurations based on this definition" ::
	C = D<<-all_named_hypered(configuration).
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
		RC *= string('CD %s: %s', D?name,D?remarks).
%%

%%%%%%%%%%%%%CHANGES%%%%%%%%%%%%%%
%%
checkChange_addConfigurationDef(D,
	CR:changeRequestor):->
	%checken op naamconflict

	NewName = CR<<-argument(1),
	NewName->>equalExportName(D?name),
	CR->>impossible('The given name is already used', D).
%%

%%
checkChange_changeConfigurationDef(D,
			    CR : changeRequestor
			   ):->
	%als we zelf het object van de change zijn checken we of
	%de naam geldig is
	%als we niet zelf het object vd change zijn checken we op
	%naamconflict..

	%1: we zijn het object

	CR->>checkObject(D),!, %we zijn dezelfde
	@model->>norm_configurationDef(CR,D). %gp3: replaced code with a norm
%
checkChange_changeConfigurationDef(D,CR):->
	%2: we zijn niet het object

	NewName = CR<<-argument(1),
	NewName->>equalExportName(D?name),
	CR->>impossible('The given name is already used',D).
%
applyChange_changeConfigurationDef(D,
			      CR: changeRequestor
			     ):->
	%Deze definitie verandert dus
	
	D->>name(?(CR,argument,1)),
	D->>remarks(?(CR,argument,2)).
%%

%%
checkChange_addAttributeDef(D,
	CR:changeRequestor):->
	%checken op naamconflict: mag ook niet bij attribute

	NewName = CR<<-argument(1),
	NewName->>equalExportName(D?name),
	CR->>impossible('The given name is already used by a configuration', D).
%%

checkChange_changeAttributeDef(D,
			    CR : changeRequestor
			   ):->
	%checken op naamconflict: mag ook niet bij attribute

    NewName = CR<<-argument(1),
	NewName->>equalExportName(D?name), %gelijk?
	CR->>impossible('The given name is already used by a configuration',D).
%

%%%%%%%%%%%%%%%%% EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
exportText(CD,F: file):->
	%gp3 1.4: added for textual export
	
	F->>format('\nConfiguration definition: %s\n',CD?name),
	if
		(\+ 0 = CD?remarks<<-size)
	then
		F->>format('Remarks:\n%s\n',?(CD?remarks,tab,1)).
%%
:-pce_end_class.
		  
