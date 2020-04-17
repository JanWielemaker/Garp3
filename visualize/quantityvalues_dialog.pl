/*
quantityValuesDialog
Dialog for showing quantity values in a state

Rewrite of show_quantity_values in visigarp. Now we have our own class for this dialog
We made it assistanceDialog so we could handle helpbutton (assistance bar), resize etc

Part of Garp3 - see copyright notice
2006 Jelmer Jellema, Spin in het Web, spininhetweb.nl
*/


%loaded into module (namespace) visualize

show_quantity_values(F, N):-
	%old call is used as a wrapper

	new(_,quantityValuesDialog(N,F)).
%

:-pce_begin_class(quantityValuesDialog,assistanceDialog
		 ).

%%
initialise(D, State: int, Frame: frame):->
	%most code copied from old show_quantity_values call, but split of resize calls

	D->+initialise(string('Quantity values in state %s - Simulate',State),'Sim_QuantityValuesInState'),
	D->>icon(@simulate_icon),

	D->>kind(toplevel),
	D->>display(new(B,extendedListBrowser),point(D?gapX,D?topY)),
	B->>name(list),
	B->>width(20),
	B->>tab_stops(vector(250)),

	findall(Pred/Name/Entity/Val/QS/Der/SOD/TOD,
		(
			%gp3 1.4 added Name to the list of interesting stuff, we can use it to get design-time comments
			find_quantity_details(State,Name,Pred,_Type,Entity,_CVal,ValStruct,QS,Der),
			find_2nd_derivative(State,Name, SOD, TOD), % SOD = 2nd der, TOD = 3rd der
			%strip the parameter name of the value struct (if nescessary) [visigarp code]
			term_to_atom(ValTerm,ValStruct),
			strip_atomize(ValTerm,Val)
		),
		QList),
	sort(QList,SortedQList),
	%we need the inputsystem again
	IS = @app<<-currentScenario,

	Tab *= number(0),
	AfterTab *= number(0),
	Font = B<<-font,
	%we want sizes in pixels, for the stuff before the tab and after

	%gp3 1.4: added State
	show_quantity_values_fill_list(B,State,SortedQList,IS,Font,Tab,AfterTab),

	text_margin_spaces(_Left,Right),
	%how much in pixels?
	Space *= string,
	Space->>insert_character(' ',times:=Right),
	SpacePixels = Font<<-width(Space),
	B->>tab_stops(vector(Tab + SpacePixels)),
	% _size: give the size in pixels
	B->>'_size'(size(Tab + SpacePixels + AfterTab + SpacePixels + B?scroll_bar?width,10)), %height will be reset below

	% set height to nr of items, or 25 at max
	get(B, length, NrItems),
	min(NrItems, 25, Height),
	send(B, height, Height),

	Close *= imgButton(close,->>(D,destroy), tt:='Close this window'),
	DesignMsg *= ->>(@app, openQuantityDefinitions,B?selection?object),
	Design *= imgButton(design,DesignMsg, img:= edit_quantity, tt:='Edit selected quantity'),
	Design->>active(@off), %only active when non-legacy and something selected

	D->>display(Close,point(D?gapX + ((B?pixelWidth - (Close?width + D?gapX + Design?width)) / 2),B?bottom_side + D?gapY)),
	D->>display(Design,point(Close?right_side + D?gapX,Close?top_side)),

	D->>updateSpacers, %needed in assistanceDialog
	unless
		get(@model, modelState, legacy) % JL
	do
	(
		B->>open_message(DesignMsg),
		B->>select_message(->>(Design,active,@on))
	),
	D->>default_button(close),
	%always open in initialise
	D->>open,
	D->>transient_for(Frame), %gp3: after creation
	%gp3: get mimimalsize
	D->>minimalSize(size(B?right_side, Close?bottom_side)).
%%

%%
%show_quantity_values_fill_list is a helper for show_quantity_values
%fill the listbrowser with the information
%this is a gp3 rewrite of visigarps show_quantity_values_list
%we use more information and get the design time info
%gp3 1.4: added State number, and added Name to the quantity information (see above)

show_quantity_values_fill_list(_,_,[],_,_,_,_). %done
%
show_quantity_values_fill_list(List,State,[Pred/Name/Entity/Val/QS/Der/SOD/TOD|Rest],IS,Font,Tab,AfterTab):-
	visualize(Der,VDer), %old visigarp code, interface.pl
	dd_text(SOD, VDDer),
	dd_text(TOD, VDDDer),
	%lets find design time names in a safe way, hopefully also works in legacy mode

	if
		QDef = @app<<-findExportedObject(qd,Pred)
	then
	(
		PredName = QDef<<-name
		%Remarks = QDef<<-remarks
	)
	else
	(
		PredName = Pred,
		QDef = @default %this way when openQuantityDefinitions is called for design, it will open without a selection
		%Remarks *= string

	),

	%instance name:
	if
		Instance = @app<<-findExportedObject(in,Entity,IS)
	then
	(
		InstanceName = Instance<<-name
	)
	else
	(
		InstanceName = Entity
	),

	%Value should come from the QS, so do we have the qs?
	if
		QSDef = @app<<-findExportedObject(qs,QS)
	then
		ValName = @app<<-designName(qsv,Val,QSDef)
	else
	(
		ValName = Val
	),
	SBefore *= string('%s (%s):',PredName,InstanceName),
	Tab->>maximum(?(Font,width,SBefore)), %maybe we have to move the tab

	HODonoff = @app<<-setting(second_order_derivatives),  %FL new feb 2012: this now has the meaning: higher order derivatives
	%printing 2nd derivatives also depends on the setting for computing them
	%printing 3rd derivatives depends on the setting for computing them
	flag(second_order_derivatives, SODtruefail, SODtruefail),
	flag(third_order_derivatives, TODtruefail, TODtruefail),
	(
	  HODonoff == @off, SODtruefail = true
	->
	  % no printing of higher order derivatives (or no calculation of at least 2nd order derivatives)
	  SAfter *= string('%s, %s',ValName,VDer)
	;
	  (
	    TODtruefail = true
	  ->
	    SAfter *= string('%s, %s, (%s), (%s)', ValName, VDer, VDDer, VDDDer)
	  ;
	    SAfter *= string('%s, %s, (%s)', ValName, VDer, VDDer)
	  )
	),

	AfterTab->>maximum(?(Font,width,SAfter)),
	DI *= dict_item(PredName,string('%s\t%s',SBefore,SAfter),QDef),

	%gp3 1.4: new tooltip code, using the full pred

	SearchTerm =.. [Pred,_,Name,_,_], %back to a predicate
	relevant_comments(SearchTerm,State,'',Tooltip),
	%gp3 1.4: because of readability etc, we also add any comments  for QS
			if
			(
				nonvar(QSDef), %found somewhere above?
				QSRemarks = QSDef<<-relevantComments,
				(\+ 0 = QSRemarks<<-size)
			)
			then
			(
				Tooltip->>ensure_nl(?(QSRemarks,split_lines,120))
			),
			%still empty?
			if
				0 = Tooltip<<-size
			then
				Tooltip->>append('(no comments)'),
	/*
	%gp3 0.2: add tooltip, same as in dependency view
	Tooltip *= string('Quantity %s\nInstance: %s', PredName, InstanceName),
	if
	(	\+ 0 = Remarks<<-size)
	then
		Tooltip->>ensure_nl(Remarks),
	*/
	DI->>tooltip(Tooltip,model), %works since 0.2.15 because of patch in tooltip.pl
	List->>append(DI),
	show_quantity_values_fill_list(List,State,Rest,IS,Font,Tab,AfterTab).
%%

%%
onResize(D, Difference: size):->
	%see extendedDialog
	%make sure buttons stay centered and list gets bigger/smaller

	B = D<<-member(list),
	B->>right_side(B?right_side + Difference?width),
	B->>bottom_side(B?bottom_side + Difference?height),

	Close = D<<-member(close),
	Design = D<<-member(design),

	Close->>set(x:= Close?left_side + Difference?width / 2, y:= Close?top_side + Difference?height),
	Design->>set(x:= Design?left_side + Difference?width / 2, y:= Close?top_side).
%%
:-pce_end_class.
