/*
definitie influencePropsDlg klasse.
Samen met proportionalityPropsDlg subklasse van qRelPropsDlg, waarin het meeste
geregeld is.
*/

:-pce_begin_class(influencePropsDlg,
		  qRelPropsDlg
		 ).

%%
newObject(D,
	Arg1: garpQuantity,
	Arg1Route: chain,
	Arg2: garpQuantity,
	Arg2Route: chain
	):->

	%open dialoog voor nieuwe influence
    	get(@model, getModelNameForEditor, 'Add a new influence - Build', ModelNameForEditor),
	D->>label(ModelNameForEditor),
	D->>helpId(string('Build_%s_AddNewInfluence',D?containerType)), %gp3 0.3.13
	D->+newObject(Arg1,Arg1Route,Arg2,Arg2Route).
%%

%%
editObject(D,
	QR : garpQuantityRelation,
	ReadOnly: bool
	):->

	%open dialoog voor bestaande influence
	inf = QR<<-type,
    	get(@model, getModelNameForEditor, 'Influence properties - Build', ModelNameForEditor),
	D->>label(ModelNameForEditor),
	D->>helpId(string('Build_%s_InfluenceProperties',D?containerType)), %gp3 0.3.13
	D->+editObject(QR, ReadOnly).
%% 

%%
quantityRelationType(_D,
	N: name):<-
	%geef het type terug voor nieuwe relaties die met deze dialoog worden gemaakt

	N = inf.
%%

:-pce_end_class.
