/*
definitie proportionalityPropsDlg klasse.
Samen met influencePropsDlg subklasse van qRelPropsDlg, waarin het meeste
geregeld is.
*/

:-pce_begin_class(proportionalityPropsDlg,
		  qRelPropsDlg
		 ).

%%
newObject(D,
	Arg1: garpQuantity,
	Arg1Route: chain,
	Arg2: garpQuantity,
	Arg2Route: chain
	):->
	%open dialoog voor nieuwe proportionality
	get(@model, getModelNameForEditor, 'Add a new proportionality - Build', ModelNameForEditor),
	D->>label(ModelNameForEditor),
	D->>helpId(string('Build_%s_AddNewProportionality',D?containerType)), %gp3 0.3.13
	D->+newObject(Arg1, Arg1Route, Arg2, Arg2Route).
%%

%%
editObject(D,
	QR : garpQuantityRelation,
	ReadOnly: bool
	):->
	
	%open dialoog voor bestaande proportionality
	prop = QR<<-type,
	get(@model, getModelNameForEditor, 'Proportionality properties - Build', ModelNameForEditor),
	D->>label(ModelNameForEditor),
	D->>helpId(string('Build_%s_ProportionalityProps',D?containerType)), %gp3 0.3.13
	D->+editObject(QR, ReadOnly).
%% 

%%
quantityRelationType(_D,
	N: name):<-
	%geef het type terug voor nieuwe relaties die met deze dialoog worden gemaakt

	N = prop.
%%
:-pce_end_class.
