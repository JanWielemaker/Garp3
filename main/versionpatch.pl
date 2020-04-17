/*
versionpatch.pl

Version patch code. Used by garpModel->versionPatch when a model is loaded that uses an older modelDefinitionVersion than the current one.

Loaded only when needed.

This file defined pl_doVersionPatch(Model, CurrentVersion, ModelVersion) to get Model from ModelVersion to CurrentVersion.
For every step (from version 0 to 1, 1 to 2 etc) is has code, so we can patch any old version.

8-12-2005 Jelmer Jellema (Spin in het Web)
Part of GARP3
See copyright notice

%gp3 1.4: Due to heavy changes in data structure, we no longer support models with a version below 15. Users should load and save their version in Garp3 version 1.3.8 before using 1.4.

%gp3 1.4: We made some changes. pl_doVersionPatch(M,CurrentVersion,LowerVersion) is now a main call. It first executes a series of calls looping from CurrentVersion back to LowerVersion + 1. These new calls pl_versionPatch_data(M,Version), are meant to set newly added variables. It was invented to add the translator to the model in version 15. This is needed, because the version patch for version 8 (!!) initialises data that in the current code needs this translator. After this, the old pl_versionPatch(M,OldVersion) is still used to loop from LowerVersion to CurrentVersion.

So:
- use pl_versionPatch_data(M,Version) to define and ininitialise variables and other data that were added in Version.
- use pl_versionPatch(M,OldVersion) to manipulate old data (probably in @old_slots) to get to the new version. All elements initialises in pl_versionPatch_addElements are there...
*/

:-discontiguous pl_versionPatch/2, pl_versionPatch_addElements/2.
%

pl_doVersionPatch(M,CurrentVersion,LowerVersion):-
	pl_doVersionPatch_data(M,CurrentVersion,LowerVersion),
	pl_doVersionPatch_patches(M,CurrentVersion,LowerVersion),
	(
	  LowerVersion < CurrentVersion % patch was done
	->
	  get(@app?version, value, Version),
	  format(atom(String),
		 'Model definition was updated from version ~w to ~w.\n\nPlease save your model if you intend to keep working in GARP ~w.',
		 [LowerVersion, CurrentVersion, Version]),
	  Dlg *= notificationDlg(@app?mainMenu, notification, String,@on),
	  Dlg<<-confirm_centered(@app?mainMenu?frame?area?center),
	  Dlg->>destroy
	  %@app->>setModelChanged(@on) % FL patching is a change, right?  But towards the user it's a bit too agressive...
	;
	  true  % no patch: LC = CV (or greater)
	).

pl_doVersionPatch_data(M,CurrentVersion,LowerVersion):-
	%gp3 1.4. Loop back from CurrentVersion to LowerVersion + 1, initialising any data added in CurrentVersion
	LowerVersion < CurrentVersion,!,
	% This version patch rolls back over version numbers, so the patch does not necessary exist. Sort of scary.
	% For this reason, the patch can fail, requiring the fail to be ignored
	ignore(pl_versionPatch_data(M,CurrentVersion)),

	PrevVersion is CurrentVersion - 1,
	pl_doVersionPatch_data(M,PrevVersion,LowerVersion).
%
pl_doVersionPatch_data(_,_,_). %done.
%%

pl_doVersionPatch_patches(M,CurrentVersion,LowerVersion):-
	%gp3
	%check if there is a patch for models being at a lower definition level
	LowerVersion < CurrentVersion,!,
	(
	    pl_versionPatch(M,LowerVersion), !
	;
	    format(atom(String),
		 'Version data patch from ~w to ~w failed! Do NOT save your model, as it will potentially cause corruption. Please report this issue to the developers, and send them your model.',
		 [LowerVersion, CurrentVersion]),
	    Dlg *= notificationDlg(@app?mainMenu, notification, String,@on),
	    Dlg<<-confirm_centered(@app?mainMenu?frame?area?center),
	    Dlg->>destroy,
	    fail
	),
	NextVersion is LowerVersion + 1,
	pl_doVersionPatch_patches(M,CurrentVersion,NextVersion).
%
pl_doVersionPatch_patches(_,_,_). %done
%%

/*
pl_versionPatch_data(M,NewVersion)
	%gp3 1.4: This one is called for every version level starting at current and looping back
	%use it to initialise data that is new in NewVersion. This way pl_versionPatch calls can asume
	%all current *code* runs fine, because all variables are there. Without it, a patch at level 8 can fail
	%because a class definition uses data added at level 9.
*/

pl_versionPatch(M,25):-
        % From 24 to 25 added 4 preferences, renamed 1 prerence, FL
	RP = M<<-runPrefs,
	get(RP, member, comparative_analysis, CA),
	get(RP, member, comparative_analysis_no_strict_constraints, Bool),
	(
	  CA = @on, Bool = @off % if old model used CA, but without strict constraints, set new replacing setting to @on
	->
	  New = @on
	;
	  New = @off  %all other cases off, of course...
	),
	send(RP, append, comparative_analysis_equal_target_quantity_type, New),
	send(RP, delete, comparative_analysis_no_strict_constraints),
	send(RP, append, comparative_analysis_equal_source_quantity_type, @off),
	send(RP, append, comparative_analysis_similar_target_entity_type, @off),
	send(RP, append, comparative_analysis_similar_source_entity_type, @off),
	send(RP, append, comparative_analysis_equal_causal_dependency_sign, @off),
	@pce->>write_ln('25 --> 26: Added 4 new simulation preferences.').

pl_versionPatch(M,24):-
        % From 23 to 24 added yet three other preferences FL
	RP = M<<-runPrefs,
	RP->>append(comparative_analysis,@off),
	RP->>append(comparative_analysis_on_proportionalities,@off),
	RP->>append(comparative_analysis_no_strict_constraints,@off),
	@pce->>write_ln('24 --> 25: Added 3 new simulation preferences.').


pl_versionPatch(M,23):-
        % From 23 to 24 added yet three other preferences FL
	RP = M<<-runPrefs,
	RP->>append(third_order_derivatives,@off),
	RP->>append(cw_assumption_third_order,@off),
	RP->>append(third_order_proportionality_propagation,@off),
	@pce->>write_ln('23 --> 24: Added 3 new simulation preferences.').


pl_versionPatch(M,22):-
        % From 22 to 23 added yet two other preferences FL
	RP = M<<-runPrefs,
	RP->>append(cw_assumption_second_order,@off),
	RP->>append(second_order_proportionality_propagation,@on),
	@pce->>write_ln('22 --> 23: Added 2 new simulation preferences.').

pl_versionPatch(M,21):-
	% From 21 to 22 Added a chain for unique IDs of models
	send(M, slot, uniqueIDs, new(chain)),
	send(M, newUniqueID).

pl_versionPatch(M,20):-
        % From 20 to 21 added yet another preference FL
	RP = M<<-runPrefs,
	RP->>append(solve_extra_combinations,@on),
       @pce->>write_ln('20 --> 21: Added 1 new simulation preference.').

pl_versionPatch(M,19):-
        % From 19 to 20 added yet another preference FL
	RP = M<<-runPrefs,
	RP->>append(full_branching_derivative_terminations,@off),
       @pce->>write_ln('19 --> 20: Added 1 new simulation preference.').

pl_versionPatch(M,18):-
        % From 18 to 19 added yet another preference FL
	RP = M<<-runPrefs,
	RP->>append(fast_path,@off),
       @pce->>write_ln('18 --> 19: Added 1 new simulation preference.').

pl_versionPatch(M,17):-
        % From 17 to 18 we another preference FL
	RP = M<<-runPrefs,
	RP->>append(equal_intervals,@off),
       @pce->>write_ln('17 --> 18: Added 1 new simulation preference.').

pl_versionPatch(M,16):-
        % From 16 to 17 we added some preferences and removed some... FL
	RP = M<<-runPrefs,
	RP->>append(equal_qspace_points,@off),
	RP->>append(epsilon_derivative_constraints,@on),
	RP->>append(reasoning_assumptions,@on),
	RP->>append(assume_conditional_derivatives,@on),
	RP->>append(max_depth, 0),
	RP->>append(second_order_derivatives,@on),
	RP->>append(derivative_terminations,@on),
	RP->>append(second_order_continuity,@on),
	RP->>append(order_epsilon_last,@off),
	RP->>append(value_branching,@off),
	ignore((
	        RP->>delete(no_subsumption_specify_all),
		RP->>delete(use_old_transition_rules),
		RP->>delete(no_analyse_zero),
		RP->>delete(allow_d_assumptions_in_reclassifying_mfs)
	      )),
        @pce->>write_ln('16 --> 17: Added 10 new simulation preferences, and removed 4 old ones.').


pl_versionPatch_data(M,16):-
	@pce->>write_ln('16: translator added'),
	M->>translator(new(translator)).
%%


/*
pl_versionPatch(M,OldVersion)
	gp3 0.3 rewrite of old versionpatch
	this one is called for every version level between the current one
	and the model's one
	pl_versionPatch(M,1) means: patch M so that it current version (version 1) goes to the next one (version 2)
	so we can implement patches for any version wanted
	Version 0 is the code we used to patch stuff before explicit versioning
	So here we have to do checks about the content
*/

pl_versionPatch(M,15):-
	%gp3 1.4 (JJ). Add translation support

	% add model fragments in the hierarchy to @model?modelFragments if they are not there already
	reRegisterModelFragments(@model),

	% JL: There are sometimes datastructure remants of an old Sketch in the abstractEntities that should be removed
	removeOldSketchDatastructures(@model),

	%a lot of conversion from old char_array values to translatables, using the @old_slots hash
	%(see object_extensions.pl: object->convert_old_slot)

	@pce->>write_ln('15 --> 16: Add translation support'),

	TR = M<<-translator, %added through pl_versionPatch_data above.

	%%a lot of objectsd have name and remarks translatable, we do one loop

	%%sketch stuff is also in subclasses of abstractEntity, it appears (did I allready ask why?)
	Obs *= chain,
	chain(M?modelFragments,M?abstractEntities,M?assumptions,M?configurationDefinitions,M?attributeDefinitions,M?quantityDefinitions,M?quantitySpaces,M?dqs)->>for_all(
		if(->>(@arg1,instance_of,chain),
			->>(Obs,merge,@arg1),
			->>(Obs,append,@arg1)
		)
	),

	Obs->>for_all(
		and(
			->>(@arg1,name_translatable,?(TR,getTranslatable, unique)),
			->>(@arg1,name,?(?(@old_slots,member,@arg1?object_reference),member,name)),
			->>(@pce,write_ln,@arg1,?(@arg1,name)),
			->>(@arg1,remarks_translatable,?(TR,getTranslatable, empty)),
			->>(@arg1,remarks,?(?(@old_slots,member,@arg1?object_reference),member,remarks))
			)),

	%%Values for attributeDefinitions
	M?attributeDefinitions->>for_all(
		->>(?(@arg1,slot,values),for_all,  %use slot, because it is wrapped thru a copy action
			and(
				->>(@arg1,valueName_translatable,?(TR,getTranslatable,unique)),
				->>(@arg1,valueName,?(?(@old_slots,member,@arg1?object_reference),member,valueName)),
				->>(@pce,write_ln,'Attrib value:',@arg1?valueName)
			)
		)
	),

	%%values for quantityspaces
	QS = M?quantitySpaces<<-copy,
	QS->>append(M?dqs),
	QS->>for_all(
		->>(?(@arg1,slot,values),for_all,
			and(
				->>(@arg1,valueName_translatable,?(TR,getTranslatable,unique)),
				->>(@arg1,valueName,?(?(@old_slots,member,@arg1?object_reference),member,valueName)),
				%value Zero is nontranslatable:
				if(->>(@arg1?valueName,equal,'Zero'),->>(@arg1,setTranslatable,@off)),
				->>(@pce,write_ln,'QS value:',@arg1?valueName)
			)
		)
	),

	%2 special quantityspaces are nontranslatable (name and remarks allready set above)
	chain(M?dqs, M?mzpqs)->>for_all(
		and(
			->>(@arg1?name_translatable,setNonTranslatable),
			->>(@arg1?remarks_translatable,setNonTranslatable),
			->>(?(@arg1,slot,values),for_all,->>(@arg1,setTranslatable,@off)) %again, we use the slot, because otherwise we ll get a copy
		)
	),

	%model fragment elements

	M?modelFragments->>for_all(
		and(
			->>(?(@arg1?elements,merge,@arg1?parentReferences),for_all,
				and(
					->>(@pce,write,@arg1),
					if(?(@arg1,slot,name_translatable),						%has name_translatable
						and(
							->>(@arg1,name_translatable,?(TR,getTranslatable,unique)),
							->>(@arg1,name,?(?(@old_slots,member,@arg1?object_reference),member,name)),
							->>(@pce,write,' name',@arg1?name)
							)
						),
					if(?(@arg1,slot,remarks_translatable),						%has remarks_translatable
						and(
							->>(@arg1,remarks_translatable,?(TR,getTranslatable,empty)),
							->>(@arg1,remarks,?(?(@old_slots,member,@arg1?object_reference),member,remarks)),
							->>(@pce,write,' remarks')
							)
						),
					%all other elements containing valueReference objects
					%(@arg3 is the slot name, @arg4 is the object in the slot)
					->>(@arg1,for_slot_reference,
						if(and(@arg2 == 'slot', ->>(@arg4,instance_of,valueReference)),
							and(
								->>(@arg4,valueName_translatable,?(TR,getTranslatable,unique)),
								->>(@arg4,valueName,?(?(@old_slots,member,@arg4?object_reference),member,valueName)),
								->>(@pce,write,' valueReference:', @arg3,'=',@arg4?valueName)
							)
						)
					),
					->>(@pce,write_ln)
					)
				)
			)),
	%special for top nodes
	M?topStaticFragment?name_translatable->>setNonTranslatable,
	M?topStaticFragment?remarks_translatable->>setNonTranslatable,
	M?topProcessFragment?name_translatable->>setNonTranslatable,
	M?topProcessFragment?remarks_translatable->>setNonTranslatable,
	M?topAgentFragment?name_translatable->>setNonTranslatable,
	M?topAgentFragment?remarks_translatable->>setNonTranslatable,

	@pce->>write_ln('15 --> 16: Finished adding translation support').
%%
