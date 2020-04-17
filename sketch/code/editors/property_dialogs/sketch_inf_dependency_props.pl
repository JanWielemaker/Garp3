/*
definition of class sketchInfDependencyPropsDlg, 
the dialog with features of influence SketchDependencies
Based on class influencePropsDlg
*/

:-pce_begin_class(sketchInfDependencyPropsDlg,
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
	%open dialoog voor nieuwe sketch influence
	get(@model, getModelNameForEditor, 'Add a new causal dependency - Sketch', ModelNameForEditor),
	D->>label(ModelNameForEditor),
	D->>helpId('Sketch_InfDependencyProps'), %gp3 0.3.13
	D->+newObject(Arg1, Arg1Route, Arg2, Arg2Route, Sign).
%%

%%
editObject(D,
	QR : sketchDependencyElement,
	ReadOnly: bool
	):->	
	%open dialoog voor bestaande sketch influence
	inf = QR<<-type,
	get(@model, getModelNameForEditor, 'Causal dependency properties - Sketch', ModelNameForEditor),
	D->>label(ModelNameForEditor),
	D->>helpId('Sketch_InfDependencyProps'), %gp3 0.3.13
	D->+editObject(QR, ReadOnly).
%% 


:-pce_end_class.
