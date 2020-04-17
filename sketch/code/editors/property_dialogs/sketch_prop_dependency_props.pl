/*
definition of class sketchPropDependencyPropsDlg, 
the dialog with features of proportionality SketchDependencies
Based on class proportionalityPropsDlg
*/

:-pce_begin_class(sketchPropDependencyPropsDlg,
		  sketchGenericDependencyPropsDlg
		 ).

%%
newObject(D,
	Arg1: sketchQuantityElement,
	Arg1Route: chain,
	Arg2: sketchQuantityElement,
	Arg2Route: chain,
	Sign: {dep_plus, dep_min, inf_plus, inf_min, prop_plus, prop_min}
	% Sign: {plus,min}
	):->
	%open dialoog voor nieuwe proportionality	
	get(@model, getModelNameForEditor, 'Add a new causal dependency - Sketch', ModelNameForEditor),
	D->>label(ModelNameForEditor),
	D->>helpId('Sketch_PropDependencyProps'), %gp3 0.3.13
	D->+newObject(Arg1, Arg1Route, Arg2, Arg2Route, Sign).
%%

%%
editObject(D,
	QR : sketchDependencyElement,
	ReadOnly: bool
	):->
	
	%open dialoog voor bestaande proportionality
	prop = QR<<-type,
	get(@model, getModelNameForEditor, 'Causal dependency properties - Sketch', ModelNameForEditor),
	D->>label(ModelNameForEditor),
	D->>helpId('Sketch_PropDependencyProps'), %gp3 0.3.13
	D->+editObject(QR, ReadOnly).
%% 



:-pce_end_class.
