/*
definition of class sketchDependencyPropsDlg, 
the dialog with features of unknown dependency SketchDependencies
Based on class influencePropsDlg
*/

:-pce_begin_class(sketchDependencyPropsDlg,
		  sketchGenericDependencyPropsDlg
		 ).

%%
newObject(D,
	Arg1: sketchQuantityElement,
	Arg1Route: chain,
	Arg2: sketchQuantityElement,
	Arg2Route: chain,
%	Sign: {plus,min,unknown}
	Sign: {dep_plus, dep_min, inf_plus, inf_min, prop_plus, prop_min}
       ):->
	%open dialoog voor nieuwe sketch dependency

	get(@model, getModelNameForEditor, 'Add a new dependency - Sketch', ModelNameForEditor),
	D->>label(ModelNameForEditor),
	D->>helpId('Sketch_DependencyProps'), %gp3 0.3.13
	D->+newObject(Arg1, Arg1Route, Arg2, Arg2Route, Sign).
%%

%%
editObject(D,
	QR : sketchDependencyElement,
	ReadOnly: bool
	):->
	
	%open dialoog voor bestaande sketch dependency
	dep = QR<<-type,
	D->>label('Dependency properties - Sketch'),
	D->>helpId('Sketch_DependencyProps'), %gp3 0.3.13
	D->+editObject(QR, ReadOnly).
%% 



:-pce_end_class.
