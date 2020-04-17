/*
Definitie quantitySpace class

2000 - 2006 Jelmer Jellema, spininhetweb.nl
*/

:-pce_begin_class(
		  quantitySpace(name),
		  object,
		  "quantitySpace definition"
		 ).

:-pce_group('Model data').

variable(name_translatable,translatable,both,"Name of the object"). %gp3 1.4: added this, name is just a get/set method now
variable(remarks_translatable, translatable, both, "Remarks on this object"). %gp3 1.4: added this, remarks is just a get/set method now

variable(values,chain,none,"List of all values in the quantitySpace"). 
%de eerste in de chain is de hoogste

:-pce_group(@default).
initialise(QS, Name: name = name,
			Values: chain,
			Remarks: remarks = [string],
			T: translator = [translator] %added by gp3 1.4 Only needed when called from model initialisation
	) :->
	send(QS,send_super,initialise),
	default(T,@model?translator,TR),
	QS->>name_translatable(?(TR,getTranslatable,unique)),
	QS->>name(Name),

	default(Remarks,'',RRemarks),
	QS->>remarks_translatable(?(TR,getTranslatable,empty)),
	QS->>remarks(RRemarks),
	%gp3 1.4: check for translatability (see comments with applyChange_changeQuantitySpace)
	%in initialise we assume the creator knows best, except for Zero, which is never translatable
	RealValues = Values<<-map(@arg1?copy),
	RealValues->>for_all(
		if(and(@arg1?type == point, ->>(@arg1?valueName,equal,'Zero')),
			->>(@arg1,setTranslatable,@off)
		)),
	QS->>slot(values,RealValues).
%%

%%%%% wrapping translatables %%%%%%%%
%%
name(QS,Name : name):->
	"Set the name, internal" ::

	%is now a wrapper around name_translatable
	QS?name_translatable->>value(Name?makeGarp).
%
name(QS, Name: name):<-
	%get the name
	Name = QS?name_translatable<<-value.
%%

%%
remarks(QS, Remarks: string):->
	"Set a copy of the given string as remarks" ::

	%is now a wrapper around remarks_translatable
	
	QS?remarks_translatable->>value(?(Remarks,strip,both)).
%
remarks(QS,	Remarks : string
       ):<-
	"Return a copy of the remarks" ::
	
	Remarks = QS?remarks_translatable?value<<-copy.
%%


%%%%%%
%%
values(QD,
	V : chain):<-
	"Return a new list containing copies of the values (with same ID)" ::

	RV = QD<<-slot(values),
	V = RV<<-map(@arg1?copy).
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
		RC *= string('QS %s: %s', D?name,D?remarks).
%%
%%%%%%%%CHANGES%%%%%%%%%%%%%%%%%%%%%%
checkChange_changeQuantitySpace(QS,
			    CR : changeRequestor
			   ):->
	%als we zelf het object van de change zijn checken we of
	%alle data correct is.
	%als we niet zelf het object vd change zijn checken we op
	%naamconflict..

	%1: we zijn het object: de norm staat bij het model (ivm addQuantitySpace)
	CR->>checkObject(QS),!,
	@model->>norm_quantitySpace(CR,QS).
%
checkChange_changeQuantitySpace(QS,CR):->
	%2: we zijn niet het object
	
	NewName = CR<<-argument(1),
	NewName->>equalExportName(QS?name), %gelijk?
	CR->>impossible('The given name is already used',QS).
%
applyChange_changeQuantitySpace(QS,
			      CR: changeRequestor
			     ):->
	%Deze qs verandert dus
	%gp3 1.4: Check values for translatability. We asume: point value zero is nontranslatable, any other value that is changeable is also translatable. This means: only the built-in quantityspaces are nontranslatable, and there are never changed thru this cr
	
	NewName = CR<<-argument(1),
	QS->>name(NewName),
	NewValues = CR<<-argument(2),
	RealValues = NewValues<<-map(@arg1?copy),
	%gp3 1.4: check for translatability
	RealValues->>for_all(
		if(and(@arg1?type == point, ->>(@arg1?valueName,equal,'Zero')),
			->>(@arg1,setTranslatable,@off),
			->>(@arg1,setTranslatable,@on)
		)),
	QS->>slot(values,RealValues), %dus een kopie variant
	NewRemarks = CR<<-argument(3),
	QS->>remarks(NewRemarks).
%%

%%
checkChange_addQuantitySpace(QS,CR:changeRequestor):->
	%naam conflict checken dus
	NewName = CR<<-argument(1),
	NewName->>equalExportName(QS?name),
	CR->>impossible('The given name is already used',QS).
%%

%%%helpers

%%%%%%%%%%%%EXPORT%%%%%%%%%%%%%%%%%%
export(QS,F: file):->
	%export deze qs

	if
		(\+ 0 = QS?remarks<<-size)
	then
		F->>format('\n%s\n',?(string('%s\n%s\n', QS?name,QS?remarks),makeRemarks,0))
	else
		F->>format('\n%%%s\n',QS?name),
		
	%is er een punt dat niet zero is
	if
		QS?values->>find(and(
			@arg1?type == point,
			@arg1?valueName \== 'Zero'))
	then
		ContextVar = 'X'
	else
		ContextVar = '_',
		
	%
	EN = QS?name<<-exportName,
	F->>format('quantity_space(%s,%s,[\n'
				,EN,ContextVar),
				
	%waarden, die willen we van laag naar hoog
	new(Reversed, chain),
	send(QS?values, for_all, message(Reversed,prepend,@arg1)),

	new(Comma, var),
	send(Comma, assign('',global)),
	send(Reversed, for_all,
	    and(
		message(F,format,'%s\t',Comma),
		assign(Comma,',\n',global),
		if(
		    message(chain('Zero', 'One'), member, @arg1?valueName), % 'Minusone' 
		    message(F,format,'point(%s)', @arg1?valueName?exportName),
		    if(
			@arg1?type == interval,
			message(F,append,@arg1?valueName?exportName),
			message(F,format,'point(%s(X))', @arg1?valueName?exportName)
		    )
		)
	    )
	),
	send(F, append, '\n]).\n').
%%

%%
exportText(QS,F: file):->
	%gp3 1.4: added for textual export
	
	F->>format('\nQuantity space: %s\n',QS?name),
	if
		(\+ 0 = QS?remarks<<-size)
	then
		F->>format('Remarks:\n%s\n',?(QS?remarks,tab,1)),

	%we want the values low to high, as in export
	Reversed *= chain,
	QS?values->>for_all(
		->>(Reversed,prepend,@arg1?valueName)),
	F->>format('Values:\n%s\n',?(?(Reversed,join,', '),tab,1)).
%%

:-pce_end_class.
		  
