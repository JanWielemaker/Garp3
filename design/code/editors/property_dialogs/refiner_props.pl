/*
gp3 0.3
Definition refinedPropsDlg class: The property dialog for fragment refiners.
For adding new objects, and for changing existing ones (comments and removal only).

Part of Garp3, see copyright notice
Some code copied from other old homer classes, so comments there in dutch

2000-2005 Jelmer Jellema, Spin in het Web, spininhetweb.nl
*/

:-pce_begin_class(refinerPropsDlg,
		  propertyDialog).

variable(refined,importedFragment,both). %associated object that is refined
variable(refinedRoute,chain,both). %route to the refined object

%%
initialise(D, F: frame):->
	"Initialise the properties dialog" ::
	%gp3 changed buttons to imgButtons
	
	D->+initialise('Fragment refinement properties - Build',F,later),

	%De plaatsing doen we zelf op coordinaten, we gebruiken wel de standaard "gap"
	GapX = D?gap<<-width,
	GapY = D?gap<<-height,

	RefineText *= label(refineText),
	D->>display(RefineText,point(GapX,D?topY)),
	
	DefList *= extendedListBrowser(height:=15, width := 60), 
	DefList->>name(mfList),
	DefList->>label('Possible refinements:'),
	DefList->>show_label(@on),
	DefList->>select_message(->>(D,
				       onMFSelection)),
	DefList->>style(root,style(colour:='#757575')), %niet te editen grijs
	DefList->>style(ok,style(colour:='#cc3366')), %ok mf, paarsig	

	D->>display(DefList,point(GapX,RefineText?bottom_side + GapY)),

	Remarks *= editor(height := 5, width := 60),
	Remarks->>name(remarks),
	Remarks->>label('Remarks:'),
	Remarks->>show_label(@on),
	Remarks->>fill_mode(@on),
	Remarks->>font(DefList?font),
	D->>display(Remarks,point(GapX,DefList?bottom_side + GapY)), 
	
	Ok *= imgButton(ok, img:=save, tt:='Apply changes'),
	D->>display(Ok,point(GapX,Remarks?bottom_side + GapY)),
	Ok->>default_button(@on),
	
	Delete *= imgButton(delete, ->>(D,onDelete),img:=remove, tt:='Delete this fragment refinement'),
	D->>display(Delete,point(Remarks?left_side + (Remarks?width - Delete?width) / 2, Ok?top_side)),
	
	Cancel *= imgButton(cancel, img:=undo, tt:= 'Cancel changes'),
	D->>display(Cancel,point(Remarks?right_side - Cancel?width, Ok?top_side)),
	D->>updateSpacers, %gp3 0.3.13
	D->>assign_accelerators,
		%minimal size:
	D->>minimalSize(size(Cancel?right_side,Cancel?bottom_side)), %abs min

	% Multiple model support
	get(@model, getModelNameForEditor, 'Fragment refinement properties - Build', ModelNameForEditor),
	new(Garp3EditorFrame, garp3EditorFrame(ModelNameForEditor)),
	get(@model, '_value', Model),
	send(Garp3EditorFrame, associateModel, Model),
	send(Garp3EditorFrame, append, D),
	send(Garp3EditorFrame, transient_for, F).
%%

%%
onResize(D, Difference: size):->
	%gp3 0.2: 
	
	DefList = D<<-mfList_member,
	DefList->>pixelWidth(DefList?pixelWidth + Difference?width),
	DefList->>pixelHeight(DefList?pixelHeight + Difference?height),

	D?remarks_member->>right_side(DefList?right_side),
	D?remarks_member->>set(y:=D?remarks_member?top_side + Difference?height),
	D?ok_member->>set(y:=D?ok_member?top_side + Difference?height),
	D?delete_member->>move(point(DefList?left_side + (DefList?pixelWidth - D?delete_member?width) / 2, D?ok_member?top_side)),
	D?cancel_member->>move(point(DefList?right_side - D?cancel_member?width, D?ok_member?top_side)).
%%

%%
newObject(D,Refined: importedFragment,
		RefinedRoute: chain):->
	%open the dialog for a new refinement

	get(@model, getModelNameForEditor, 'Add a new model fragment refinement - Build', ModelNameForEditor),
	D->>label(ModelNameForEditor),
	D->>helpId(string('Build_%s_AddNewMFRefinement',D?containerType)),
	D->>element(@nil),
	D->>refined(Refined),
	D->>refinedRoute(RefinedRoute?copy),
	D->>setRefinedInfo,
	D->>fillMFList,
	D->>onMFSelection,
	D?delete_member->>displayed(@off),
	D->>openDialog.
%%

%%
editObject(D,FR: fragmentRefiner):->
	%open the dialog for an existing refinement
	get(@model, getModelNameForEditor, 'Model fragment refinement properties - Build', ModelNameForEditor),
	D->>label(ModelNameForEditor),
	D->>helpId(string('Build_%s_MFRefinementProperties',D?containerType)),
	D->>element(FR),
	D->>refined(FR?refined),
	D->>refinedRoute(FR?refinedRoute), %is a copy
	D->>setRefinedInfo,
	D->>fillMFList,
	D?mfList_member->>changeable(@off),
	D?remarks_member->>contents(FR?remarks),
	D->>openDialog.
%%

%%
setRefinedInfo(D):->
	%gp3 0.3 Set info about refined object
	
	%get the content fragment instead of the referencedFragment
	HigherName = D?refined<<-name,
	Info *= string('In %s: refine %s (defined in %s)', D?modelFragment?name,HigherName,
		D?refinedRoute?head?referencedFragment?name), 
	D?refineText_member->>selection(Info).
%%

%%
fillMFList(D):->
	
	List = D<<-member(mfList),
	
	List->>clear,

	%the currently refined fragment is the root of possibilities
	D->>fillMFListRecursive(List,D?refined?referencedFragment,root,new(chain),0,0),
	%can we set the selection?
	
	if
	(
		FR = D<<-element,
		FR \== @nil,
		Item = List?members<<-find( and(
			@arg1?object?first == FR?referencedFragment,
			->>(@arg1?object?second,equal,FR?refinementRoute))
		)
	)
	then
		List->>selection(Item).
%
fillMFListRecursive(D,
	List: list_browser,
	Current: modelFragment,
	Style: {root, ok},
	RefinementRoute: chain, %route from refinement to refined
	Tab: int,
	Key: number):->
	%recursief hulpje

	Name *= string,
	Name->>insert_character('-',0,Tab),
	Name->>append(Current?name),
	
	Key->>plus(1), %unique key, thats all
	
	List->>append(dict_item(Key?value,Name,tuple(Current,RefinementRoute),Style)),
	
	NewTab is Tab + 1,
	%run all children
	Current?sortedChildren->>for_all(
		->>(D,fillMFListRecursive_child,
			List,Current,@arg1,RefinementRoute,NewTab,Key)).
%
fillMFListRecursive_child(D,
	List: list_browser,Current: modelFragment, Child: modelFragment,
	RefinementRoute: chain,
	Tab: int, Key: number):->
	%just a continuation helper
	
	AllRefs = Child?parentReferences<<-find_all(@arg1?referencedFragment == Current),
	NewRoute *= var,
	AllRefs->>for_all(
		and(
			assign(NewRoute,RefinementRoute?copy),
			->>(NewRoute,prepend,@arg1), %first in the new route to the refined
			->>(D,fillMFListRecursive,List,Child,ok,NewRoute,Tab,Key)
		)
	).
%%

%%
onMFSelection(D):->
	%make sure the ok button is only available when the selection is ok
	
	MF = D<<-selectedMF,
	if
	(
		MF = @nil
	;
		D?selectedRoute->>empty %root
	;
		D?modelFragment->>checkAllDown(@arg1 == MF, @on, @on)
				%deze ook, alleen onze children (recursief)
	)
	then
		D?ok_member->>active(@off)
	else
		D?ok_member->>active(@on).

%%
	
%%
onDelete(D):->
	%post a delete CR
	@model->>changeRequest(deleteFragmentRefiner,
		D?modelFragment,
		D?editor,
		D?element).
%%

%%
saveNewElement(D):->
	/* Debug
	get(D, modelFragment, Aggregate), get(D, refined, ImportedElement),
	get(D, refinedRoute, ImportedElementRoute), get(D, selectedMF, TargetRefinementMF), 
	get(D, selectedRoute, RouteChain),
	get(Aggregate, name, AggregateName), get(Aggregate, class_name, AggregateClass),
	get(ImportedElement, name, ImportedElementName), get(ImportedElement, class_name, ImportedElementClass),
	get(TargetRefinementMF, name, TargetRefinementMFName), get(TargetRefinementMF, class_name, TargetRefinementMFClass),
	
	format('Adding newFragmentRefiner to ~w/~w/~w\n', [Aggregate,AggregateClass,AggregateName]),
	format('Refining ~w/~w/~w with route: ', [ImportedElement, ImportedElementClass, ImportedElementName]),
	printChainNames(ImportedElementRoute),
	format(' to ~w/~w/~w\n', [TargetRefinementMF, TargetRefinementMFClass, TargetRefinementMFName]),
	format('with route: '), printChainNames(RouteChain),
	*/

	%set the new refinement
	@model->>changeRequest(newFragmentRefiner,
		D?modelFragment,
		D?editor,
		D?refined,
		D?refinedRoute,
		D?selectedMF,
		D?selectedRoute,
		D?remarks_member?contents).

	/* This does not work (see bug 92) 
	% The refiner should have the same position as the refined MF
	get(D?modelFragment, layOutInfo, D?refined, relPosition, D?refinedRoute, @on, Position),
	get(Position, x, X),
	get(Position, y, Y),
	format('Position ~w/~w\n', [X, Y]),
	%new(Position, point(4000,4000)),
	get(D?modelFragment?elements, size, Size),
	Index is Size - 1,
	get(D?modelFragment?elements, nth1, Index, Refinement),
	send(D?modelFragment, layOutInfo, Refinement, relPosition, Position, D?selectedRoute).
	*/

%%

%%
%%
saveChangedElement(D):->
	%Only comment can change, and since a fragmentRefiner is a conditionalfragment,
	%we reuse the CR:
	@model->>changeRequest(changeConditionalFragment,
		D?modelFragment,
		D?editor,
		D?element,
		D?remarks_member?contents).
%%


%%
notChanged(D):->
	%definitieobject hetzelfde?
	
	%only thing that can change is remarks
	%remarks hetzelfde?
	D?remarks_member?contents->>equal(D?element?remarks).
%%

%%DATA MAPPINGS%%
%%
selectedMF(D,
	MF: modelFragment):<-
	%gives @nil when nothing selected


	DI = D?mfList_member<<-selection,
	if
		DI == @nil
	then
		MF = @nil
	else
		MF = DI?object<<-first.
%%

%%
selectedRoute(D,
	Route: chain):<-
	%gives @nil when nothing selected

	DI = D?mfList_member<<-selection,
	if
		DI == @nil
	then
		Route = @nil
	else
		Route = DI?object<<-second.
%% 

%%%%%%%%%%%%%%%%%%Change Requestors
%%

changeApplied_newMF(D,
	_CR: changeRequestor):->

	%alleen de lijst met alle fragmenten kan wijzigen
	D->>fillMFList.
%%

changeApplied_changeMF(D,
	_CR: changeRequestor):->
	
	%we sluiten
	D->>cancel.

%%

changeApplied_deleteMF(D,
	_CR: changeRequestor):->
	
	D->>cancel. %sluiten
%%

changeApplied_changeConditionalFragment(D,
	CR: changeRequestor):->

		\+ CR->>checkEditor(D?editor), %niet als deze editor (anders dubbel sluiten)
		CR->>checkArgument(1,D?element),
		D->>return.
%%

%%
changeApplied_deleteFragmentRefiner(D,
	CR: changeRequestor):->
	
		CR->>checkArgument(1,D?element),
		D->>return.
%%

%%
changeApplied_deleteConditionalFragment(D,
	_CR: changeRequestor):->
	
		%to be on the save side, just close
		D->>return.
%%

:-pce_end_class.
