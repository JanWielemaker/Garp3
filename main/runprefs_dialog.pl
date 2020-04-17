/*
runPrefsDialog
Helper dialog for setting simulation preferences in @model

Part of Garp3 - see copyright notice
2005 Jelmer Jellema, Spin in het Web, spininhetweb.nl
*/

:-pce_begin_class(runPrefsDialog,assistanceDialog
		 ).

%%
initialise(D, Frame):->
	%gp3 0.3.13 rewrite because of assistancebar above
	%we cannot use append etc (again)
	D->+initialise('Simulation preferences','Sim_SimulationPreferences', new(colour(white))), %gp3 0.3.13: helpid + background
	D->>icon(@simulate_icon),
	D->>application(@app),
	%D->>modal(application), %gp3 0.3.12 changed this (problem with other modal dialogs owned by this one)
	D->>transient_for(Frame),
	D->>modal(transient),
	D->>kind(transient),
	D->>can_resize(@off),


	%Basic Menu:
	BasicLabel *= label(basicLabel,'Basic preferences for running the simulator:',italic),
	D->>display(BasicLabel,point(D?gapX,D?topY)),
	new(Basic, menu(basicMenu, toggle)),
	Basic->>show_label(@off),
	Basic->>layout(vertical),
	Basic->>columns(2),
	%Basic->>gap(size(0,4)),

	%% BASIC ITEMS:
	new(CWA, menu_item(cw_assumption, label:= 'Assume unknown influences to be zero')),
	send(Basic, append, CWA),
	tttext(cw_assumption, CWATip), % defined below
	send(CWA, tooltip,  CWATip), %This is how tooltips should be done in Garp3 preferences (not in DL)

	%this one displays checked when the internal value is @off and vice versa
	FMD *= menu_item(free_maxmin_derivative, label:= 'Apply quantity space constraints on extreme values'),
	FMD->>attribute(negate_value,@on),
	Basic->>append(FMD),
	%this one displays checked when the internal value is @off and vice versa
	FZD *= menu_item(free_zero_derivative, label:= 'Apply quantity space constraints on zero as extreme value       '),
	FZD->>attribute(negate_value,@on),
	Basic->>append(FZD),
	Basic->>append(menu_item(value_branching, label:= 'Generate all values for calculated quantities')),
	Basic->>append(menu_item(fast_path, label:= 'Apply Fastest Path Heuristic')),
	Basic->>append(menu_item(epsilon_ordering, label:= 'Apply epsilon ordering')),
	Basic->>append(menu_item(epsilon_merging, label:= 'Apply epsilon merging of immediate terminations')),
	Basic->>append(menu_item(epsilon_derivative_constraints, label:= 'Apply epsilon derivative continuity constraints')),
	Basic->>append(menu_item(remove_inactive_quantities, label:= 'Remove inactive quantities after transition')), %gp3 0.3.12 moved this one to basic
	send(Basic, value_width, 455),
	D->>display(Basic,point(D?gapX*2,BasicLabel?bottom_side + D?gapY)),


	%Intermediate Menu:
	IntermediateLabel *= label(intermediateLabel,'Advanced preferences for running the simulator:',italic),
	D->>display(IntermediateLabel,point(D?gapX,Basic?bottom_side + D?gapY + D?gapY)),
	new(Intermediate, menu(intermediateMenu, toggle)),
	Intermediate->>show_label(@off),
	Intermediate->>layout(vertical),
	Intermediate->>columns(2),
	%Intermediate->>gap(size(0,4)),

	%% INTERMEDIATE ITEMS:
	Intermediate->>append(menu_item(second_order_derivatives, label:= 'Calculate 2nd order derivatives')),
	Intermediate->>append(menu_item(cw_assumption_second_order, label:= 'Assume unknown 2nd order influences to be zero')),
	Intermediate->>append(menu_item(second_order_proportionality_propagation, label:= 'Propagate 2nd order derivatives over proportionalities')), % Sign-Equality Assumption
	Intermediate->>append(menu_item(derivative_terminations, label:= 'Generate derivative terminations (based on 2nd order derivatives)')),
	Intermediate->>append(menu_item(second_order_continuity, label:= 'Apply 2nd order derivative continuity constraints')),
	Intermediate->>append(menu_item(full_branching_equality_terminations, label:= 'Assume inequality terminations')),
	Intermediate->>append(menu_item(full_branching_value_terminations, label:= 'Assume value terminations')),
	Intermediate->>append(menu_item(full_branching_derivative_terminations, label:= 'Terminate ambiguous derivatives')),
	Intermediate->>append(menu_item(order_using_correspondences, label:= 'Use correspondences in ordering')),
	Intermediate->>append(menu_item(equal_qspace_points, label:= 'Assume equal quantity spaces have equal points')),
	send(Intermediate, value_width, 455),
	D->>display(Intermediate,point(D?gapX*2,IntermediateLabel?bottom_side + D?gapY)),


	%Expert menu:
	AdvancedLabel *= label(advancedLabel,'Expert preferences for running the simulator:',italic),
	D->>display(AdvancedLabel,point(D?gapX,Intermediate?bottom_side + D?gapY + D?gapY)),
%	Advb *= button(advanced, ->>(D,onAdvanced)),
%	Advb->>label('Advanced >>'),
%	D->>display(Advb,point(D?gapX,Basic?bottom_side + D?gapY)),
	new(Advanced, menu(advancedMenu, toggle)),
	Advanced->>show_label(@off),
	Advanced->>layout(vertical),
	Advanced->>columns(2),

	%% DEPTH LIMIT FIELD
	Depth *= text_item(depth),
	Depth ->> label(''),
	Depth ->> value_width(30),
	DepthCaption *= label(depthCaption, 'Maximum Inequality Reasoning Depth: '),
	D->>display(DepthCaption,point(D?gapX*2,AdvancedLabel?bottom_side + D?gapY)),
	D->>display(Depth,point(DepthCaption?right_side,AdvancedLabel?bottom_side + D?gapY)),
	%Depth->>displayed(@off), %not showing now
	DepthLabel *= label(depthLabel,'Integers only, 0 is Off',italic),
	D->>display(DepthLabel,point(Depth?right_side + D?gapX, AdvancedLabel?bottom_side + D?gapY)),
	%DepthLabel->>displayed(@off), %not showing now

	%% ADVANCED ITEMS:
	Advanced->>append(menu_item(third_order_derivatives, label:= 'Calculate 3rd order derivatives')),
	Advanced->>append(menu_item(cw_assumption_third_order, label:= 'Assume unknown 3rd order influences to be zero')),
	Advanced->>append(menu_item(third_order_proportionality_propagation, label:= 'Propagate 3rd order derivatives over proportionalities')), % Sign-Equality Assumption for third order
	Advanced->>append(menu_item(comparative_analysis, label:= 'Compare Derivatives (CD) of similar quantity pairs')),
	Advanced->>append(menu_item(comparative_analysis_on_proportionalities, label:= 'Extend CD: include proportionalities')),
	Advanced->>append(menu_item(comparative_analysis_equal_target_quantity_type, label:= 'Refine CD: equal target quantity type')),
	Advanced->>append(menu_item(comparative_analysis_equal_source_quantity_type, label:= 'Refine CD: equal source quantity type')),
	Advanced->>append(menu_item(comparative_analysis_similar_target_entity_type, label:= 'Refine CD: similar target entity type')),
	Advanced->>append(menu_item(comparative_analysis_similar_source_entity_type, label:= 'Refine CD: similar source entity type')),
	Advanced->>append(menu_item(comparative_analysis_equal_causal_dependency_sign, label:= 'Refine CD: equal causal dependency sign')),
	Advanced->>append(menu_item(apply_continuity_d_inequalities, label:= 'Apply continuity on derivative inequalities')),
	Advanced->>append(menu_item(reasoning_assumptions, label:= 'Allow reasoning assumptions')),
	Advanced->>append(menu_item(assume_conditional_derivatives, label:= 'Allow reasoning assumptions on derivatives')),
	Advanced->>append(menu_item(order_using_equalities, label:= 'Use constants in ordering')),
	Advanced->>append(menu_item(equal_intervals, label:= 'Assume equal length intervals')),
	Advanced->>append(menu_item(terminate_weak_relations, label:= 'Generate terminations for >= & =<')),
	Advanced->>append(menu_item(solve_extra_combinations, label:= 'Extra thorough inequality reasoning')),
	Advanced->>append(menu_item(remove_corresponding_equality, label:= 'Remove terminations to unequal for full corresponding quantities')),
	Advanced->>append(menu_item(use_landmarks, label:= 'Constrain interaction between possible worlds (derive landmark relations)')), % old: 'Derive landmark relations')),

	%% END ADVANCED ITEMS

	send(Advanced, value_width, 455),
	D->>display(Advanced,point(D?gapX*2,Depth?bottom_side + D?gapY/3)), %make member, put on right spot, but do not display:
%	Advanced->>displayed(@off), %not showing now




	Save *= imgButton(save, ->>(D,onSave),tt:='Save changes to model'),
	Save->>alignment(left),
	D->>display(Save,point(D?gapX,Advanced?bottom_side + D?gapY*2)),

	Defaults *= button(defaults, ->>(D,onDefaults)),
	Defaults->>label('Default training settings'),
	Defaults->>tooltip('Restore default simulation preferences for modelling exploration: simple behavior'), %gp3 0.4.6
	D->>display(Defaults,point(D?gapX + (Advanced?right_side - D?gapX - Defaults?width) / 3,Save?bottom_side - Defaults?height)),

	AdvancedDefaults *= button(advancedDefaults, ->>(D,onAdvancedDefaults)),
	AdvancedDefaults->>label('Default modelling settings'),
	AdvancedDefaults->>tooltip('Restore default simulation preferences for modelling: full behavior and constraints'), %gp3 0.4.6
	D->>display(AdvancedDefaults,point(D?gapX + ((Advanced?right_side - D?gapX - Defaults?width) * 2 )/ 3,Save?bottom_side - Defaults?height)),

	Close *= imgButton(close,->>(D,destroy), tt:='Close this editor'),
	D->>display(Close,point(Advanced?right_side - Close?width, Save?top_side)),

	D->>updateSpacers,
	D->>loadRunPrefs(@model?runPrefs).

	%% REMOVED ITEMS
	%Advanced->>append(menu_item(extra_termination_interpreter, label:= 'Use extra termination rules')), %obsolete!
	%Advanced->>append(menu_item(order_epsilon_last, label:= 'Apply epsilon ordering last')), %obsolete!
	%Advanced->>append(menu_item(allow_d_assumptions_in_reclassifying_mfs, label:= 'Allow assumptions on derivatives in reclassifying MFs')),
	%this one is also negated
	%NAZ *= menu_item(no_analyse_zero, label:= 'Application of analyse zero equality technique'),
	%NAZ->>attribute(negate_value,@on),
	%Advanced->>append(NAZ),
	%Advanced->>append(menu_item(no_subsumption_specify_all, label:= 'Specify and Match instead of Subsumption')),
	%Advanced->>append(menu_item(use_old_transition_rules, label:= 'Use complete rule based transition procedure')),




loadRunPrefs(D, Prefs: hash_table):->
	%gp3 0.4.6
	%split code from initialize, we need it too in onDefaults below
	%there we use a different hash_table, not the real prefs, so thats why we use the hash_table as an argument here
	%instead of just using @model?runPrefs

	%set the right values
	Item *= var,
	Basic = D<<-basicMenu_member,
	Intermediate = D<<-intermediateMenu_member,
	Advanced = D<<-advancedMenu_member,
	Prefs->>for_all(
		and(
			if(assign(Item,?(Basic,member,@arg1)),
				->>(Basic,selected,@arg1,
					when(?(Item,attribute,negate_value),@arg2?negate,@arg2))),
			if(assign(Item,?(Intermediate,member,@arg1)),
				->>(Intermediate,selected,@arg1,
					when(?(Item,attribute,negate_value),@arg2?negate,@arg2))),
			if(assign(Item,?(Advanced,member,@arg1)),
				->>(Advanced,selected,@arg1,
					when(?(Item,attribute,negate_value),@arg2?negate,@arg2)))
		)
	),
	Depth = D<<-member(depth),
	Max = Prefs<<-member(max_depth),
	Depth->>value(Max).
%%

/* FL nov 2011 removed advanced button
%%
onAdvanced(D):->
	%gp3 0.3 Make sure the advanced menu is hidden of shown and update helpid and display

	Advanced = D<<-member(advancedMenu),
	Depth = D<<-member(depth),Depth = D<<-member(depth),
	DepthLabel = D<<-member(depthLabel),
	if
		@on = Advanced<<-displayed
	then
	(
		Advanced->>displayed(@off),
		D?advanced_member->>label('Advanced >>'),
		D->>helpId('Sim_SimulationPreferences'),
	        Depth->>displayed(@off),
	        DepthLabel->>displayed(@off)
	)
	else
	(
		Dlg *= notificationDlg(D,confirm, 'Advanced preferences should in principle not be changed! Correct use of these preferences requires expert knowledge of the Garp3 engine.',@on),
		Result = Dlg<<-confirm_centered(D?frame?area?center),
		Dlg->>destroy,
		if
			Result = @on
		then
		(
			Advanced->>displayed(@on),
			D?advanced_member->>label('Advanced <<'),
			D->>helpId('Sim_SimulationPreferencesAdvanced'),
		        DepthLabel->>displayed(@on),
			Depth->>displayed(@on)
		)
	),

	%update display

	if
		@on = Advanced<<-displayed
	then
	(
		NextY =	DepthLabel<<-bottom_side,
		MaxX = Advanced<<-right_side
	)
	else
	(
		NextY = D?advanced_member<<-bottom_side,
		MaxX = D?basicMenu_member<<-right_side
	),
	%place close and save
	D?save_member->>set(y:=NextY + D?gapY),
	D?defaults_member->>set(x:= D?gapX + (MaxX - D?gapX - D?defaults_member?width) / 2, y:= D?save_member?bottom_side - D?defaults_member?height),
	D?close_member->>set(x:= MaxX - D?close_member?width, y:= NextY + D?gapY),
	%D->>updateSpacers, %update spacing helpers, see assistanceDialog
	D->>fit.
%%
	*/


%%
onSave(D):->
	new(ExpertPrefs, hash_table),
	D?advancedMenu_member?members->>for_all(
		->>(ExpertPrefs,append,@arg1?value,
			when(?(@arg1,attribute,negate_value),
				@arg1?selected?negate,
				@arg1?selected))),
	new(IntermediatePrefs, hash_table),
	D?intermediateMenu_member?members->>for_all(
		->>(IntermediatePrefs,append,@arg1?value,
			when(?(@arg1,attribute,negate_value),
				@arg1?selected?negate,
				@arg1?selected))),
	get(D, checkMenuChanged, ExpertPrefs, ExpertChanged),
	get(D, checkMenuChanged, IntermediatePrefs, IntermediateChanged),

	%gp3 0.3: save the prefs
	NewPrefs *= hash_table,
	D?advancedMenu_member?members->>for_all(
		->>(NewPrefs,append,@arg1?value,
			when(?(@arg1,attribute,negate_value),
				@arg1?selected?negate,
				@arg1?selected))),
	D?intermediateMenu_member?members->>for_all(
		->>(NewPrefs,append,@arg1?value,
			when(?(@arg1,attribute,negate_value),
				@arg1?selected?negate,
				@arg1?selected))),
	D?basicMenu_member?members->>for_all(
		->>(NewPrefs,append,@arg1?value,
			when(?(@arg1,attribute,negate_value),
				@arg1?selected?negate,
				@arg1?selected))),
	Depth = D<<-member(depth),
	Value = Depth<<-value,

	% first check the depth value
	(
	  checkValue(Value, Max)
	->
	  NewPrefs ->>append(max_depth, Max),
	  % value is ok, check logical combinations:
	  (
	    D->>checkChoices(NewPrefs, IntermediateChanged, ExpertChanged) % FL: added change indicators
	  ->
	    % combinations ok: save and close
	    @model->>changeRunPrefs(NewPrefs),
	    %gp3 0.3.13: this might close this dialog, so we check before destroy
	    if
		object(D)
	    then
		D->>destroy
	  ;
	    true % save cancelled by user because of illogical combination
	  )

	;
	   % wrong value: notify
	  Dlg *= notificationDlg(D, notification, 'Please fill in a valid number in the depth field',@on),
	  Dlg<<-confirm_centered(D?frame?area?center),
	  Dlg->>destroy
	).




checkMenuChanged(D, MenuPrefs, Answer):<-
	(
	  send(MenuPrefs, for_all, message(D, checkMenuItemUnchanged, @arg1, @arg2))
	%send(MenuPrefs, for_all, message(?(?(OldPrefs,member,@arg1),value), equals, @arg2))
	%send(MenuPrefs, for_all, message(OldPrefs?member(@arg1)?value, equals, @arg2))
	 ->
	  Answer = @off
         ;
	  Answer = @on
	).

checkMenuItemUnchanged(_D, Key, NewValue):->
	get(@model?runPrefs, member(Key), OldValue),
	OldValue == NewValue.


%%
onDefaults(D):->
	%gp3 0.4.6: reset to defaults

	Defaults *= hash_table,
	@model->>initRunPrefs(Defaults), %fill Defaults instead of the real thing
	D->>loadRunPrefs(Defaults).
%%
onAdvancedDefaults(D):->
	%gp3 0.4.6: reset to defaults

	Defaults *= hash_table,
	@model->>initAdvancedRunPrefs(Defaults), %fill Defaults instead of the real thing
	D->>loadRunPrefs(Defaults).
%%

checkChoices(D, NewPrefs, IntermediateChanged, ExpertChanged):-> % check if all switch choices are logical combinations
	D->>checkSure(IntermediateChanged, ExpertChanged),
	D->>checkEpsilon(NewPrefs),
%	D->>checkIncompatible(NewPrefs),
	D->>check2ndOrder(NewPrefs),
	D->>check3rdOrderNeeds2ndOrder(NewPrefs),
	send(D, check3rdOrderCWA, NewPrefs).
	%D->>checkGenerate(NewPrefs). %not really a true pair...


checkSure(D, IntermediateChanged, ExpertChanged):->
	(
	  IntermediateChanged = @off, ExpertChanged = @off
	->
	  true % no action
	;
	  (
	    ExpertChanged = @on
	  ->
	    %expert or both on
	    Dlg *= notificationDlg(D, confirm, 'Changing Expert preferences changes simulation behaviour very strongly.\nWould you like to continue saving?',@on),
	    Result = Dlg<<-confirm_centered(D?frame?area?center),
	    Dlg->>destroy,
	    (
	      Result = @off
	    ->
	      fail % cancel save (fail method)
	    ;
	      true % continue
	    )
	  ;
	    % Only IntermediateChanged must be @on
	    Dlg *= notificationDlg(D, confirm, 'Changing Advanced preferences changes simulation behaviour strongly.\nWould you like to continue saving?',@on),
	    Result = Dlg<<-confirm_centered(D?frame?area?center),
	    Dlg->>destroy,
	    (
	      Result = @off
	    ->
	      fail % cancel save (fail method)
	    ;
	      true % continue
	    )
	  )
	).



checkGenerate(D, NewPrefs):->
	V = NewPrefs<<-member(full_branching_equality_terminations),
	E = NewPrefs<<-member(full_branching_value_terminations),
	(
	 E = V
	->
	 true % everything ok
	;
	 % unequal: notify
	  Dlg *= notificationDlg(D,confirm, 'Please consider turning the following preferences on or off simultaneously since they are conceptually related:\n\n - Assume inequality terminations\n - Assume value terminations\n\nWould you like to continue saving?',@on),
	  Result = Dlg<<-confirm_centered(D?frame?area?center),
	  Dlg->>destroy,
	  (
	    Result = @off
	  ->
	    fail % cancel save (fail method)
	  ;
	    true % continue
	  )
	).
/* FL nov 2011 removed epsilon last, check obsolete
checkIncompatible(D, NewPrefs):->
	V = NewPrefs<<-member(full_branching_derivative_terminations),
	E = NewPrefs<<-member(order_epsilon_last),
	(
	 (E = @on, V = E)
	->
	 % incompatible options turned on: notify
	  Dlg *= notificationDlg(D,confirm, 'The following preferences are incompatible:\n\n - Terminate ambiguous derivatives\n - Apply epsilon last\n\nSimulation results may be incorrect\n\nWould you like to continue saving?',@on),
	  Result = Dlg<<-confirm_centered(D?frame?area?center),
	  Dlg->>destroy,
	  (
	    Result = @off
	  ->
	    fail % cancel save (fail method)
	  ;
	    true % continue
	  )
	;
	  true % no problems with incompatible options.
	).
*/

check2ndOrder(D, NewPrefs):->
	EO = NewPrefs<<-member(second_order_derivatives),
	EM = NewPrefs<<-member(derivative_terminations),
	EDC = NewPrefs<<-member(second_order_continuity),
	(
	 EO = EM,
	 EM = EDC
	->
	 true % everything ok
	;
	 % unequal: notify
	  Dlg *= notificationDlg(D,confirm, 'Please consider turning the following preferences on or off simultaneously since they are conceptually related:\n\n - Calculate 2nd order derivatives\n - Generate derivative terminations (based on 2nd order derivatives)\n - Apply 2nd order derivative continuity constraints\n\nWould you like to continue saving?',@on),
	  Result = Dlg<<-confirm_centered(D?frame?area?center),
	  Dlg->>destroy,
	  (
	    Result = @off
	  ->
	    fail % cancel save (fail method)
	  ;
	    true % continue
	  )
	).


check3rdOrderNeeds2ndOrder(D, NewPrefs):->
	get(NewPrefs, member(second_order_derivatives), SOD),
	get(NewPrefs, member(third_order_derivatives), TOD),
	(
	 SOD = @off,
	 TOD = @on
	->
	  % 2nd order off: notify
	  Dlg *= notificationDlg(D,confirm, 'Calculation of 2nd order derivatives is needed to calculate 3rd order derivatives.\nPlease consider turning on the following preferences:\n\n - Calculate 2nd order derivatives (required)\n - Propagate 2nd order derivatives over proportionalities (recommended)\n\nWould you like to continue saving?',@on),
	  Result = Dlg<<-confirm_centered(D?frame?area?center),
	  Dlg->>destroy,
	  (
	    Result = @off
	  ->
	    fail % cancel save (fail method)
	  ;
	    true % continue
	  )
	;
	  true % no 3rd without 2nd
	).


check3rdOrderCWA(D, NewPrefs):->
	get(NewPrefs, member(cw_assumption), FOD),
	get(NewPrefs, member(cw_assumption_second_order), SOD),
	get(NewPrefs, member(cw_assumption_third_order), TOD),
	(
	  TOD = @on,
	 (FOD = @off;SOD = @off)
	 %SOD = @off %not checking for tod, is it a different case?
	->
	  % 2nd or 1st order cwa off with 3rd on: notify
	  Dlg *= notificationDlg(D,confirm, 'You have activated the preference: Assume unknown 3rd order influences to be zero. Please consider turning on the following preferences as well:\n\n - Assume unknown influences to be zero\n - Assume unknown 2nd order influences to be zero\n\nWould you like to continue saving?',@on),
	  Result = Dlg<<-confirm_centered(D?frame?area?center),
	  Dlg->>destroy,
	  (
	    Result = @off
	  ->
	    fail % cancel save (fail method)
	  ;
	    true % continue
	  )
	;
	  true % no 3rd without 2nd/1st
	).


checkEpsilon(D, NewPrefs):->
	EO = NewPrefs<<-member(epsilon_ordering),
	EM = NewPrefs<<-member(epsilon_merging),
	EDC = NewPrefs<<-member(epsilon_derivative_constraints),
	(
	 EO = EM,
	 EM = EDC
	->
	 true % everything ok
	;
	 % unequal: notify
	  Dlg *= notificationDlg(D,confirm, 'Please consider turning the following preferences on or off simultaneously since they are conceptually related:\n\n - Apply epsilon ordering\n - Apply epsilon merging of immediate terminations\n - Apply epsilon continuity constraints\n\nWould you like to continue saving?',@on),
	  Result = Dlg<<-confirm_centered(D?frame?area?center),
	  Dlg->>destroy,
	  (
	    Result = @off
	  ->
	    fail % cancel save (fail method)
	  ;
	    true % continue
	  )
	).


checkValue('', 0):-
	!, % an empty text field...
	fail.
checkValue(Value, X):-
	atom_codes(Value, Codes),
	all_number_codes(Codes),
	number_codes(X, Codes),
	!.

all_number_codes([]).
all_number_codes([H|T]):-
	H > 47,
	H < 58,
	all_number_codes(T).


%tttext = tooltiptext
tttext(cw_assumption, @nil):-!.  %'text like this can be used' or @nil
tttext(_, @nil):-!.


:-pce_end_class.



