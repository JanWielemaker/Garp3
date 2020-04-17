/* Jochem Liem November 2007 
 *
 * This file is meant for code that fixed corrupted models and should reference the correct bug it fixes. This code
 * might be called from version patches 
*/

/* removeMFsInLimbo(+Model) removes unsuccessfully deleted model fragments (or model fragments that are not in
 * @model?modelFragments for another reason. It loops through all model fragments, removes children (and whatever they
 * hyper to) that are not in @model?modelFragments.
*/

%:-ensure_loaded('../design/code/helpers/design_check_api.pl').

/* reRegisterModelFragment(+@model) adds model fragments in the MF hierarchies which are missing in
 * @model?modelFragments back to this datastructure */
reRegisterModelFragments(Model) :-
    design:getTopStaticFragment(Model, TopStaticFragment),
    design:getTopProcessFragment(Model, TopProcessFragment),
    design:getTopAgentFragment(Model, TopAgentFragment),
    new(AllMFs, chain(TopStaticFragment, TopProcessFragment, TopAgentFragment)),
    reRegisterModelFragments2(AllMFs).

reRegisterModelFragments2(AllMFs) :- send(AllMFs, empty).
reRegisterModelFragments2(AllMFs) :-
    % Get the first MF and remove it from the chain
    get(AllMFs, head, MF),
    send(AllMFs, delete, MF),

    % The children of the MF 
    get(MF, children, ChildrenC),
    chain_list(ChildrenC, Children),

    % For each of the children of the model fragment
    forall(
	(
	    member(Child, Children),
	    send(AllMFs, append, Child)
	),
	(
	    % If the Child is a registered model fragment 
	    send(@model?modelFragments, member, Child) ->
	    true
	;
	    send(@model?modelFragments, append, Child)
	)
    ),

    % Recurse until all modelfragments have been tested 
    %get(AllMFs, size, Size),
    %format('The size is: ~w\n', [Size]),
    reRegisterModelFragments2(AllMFs).

/* forceDeleteMF(+@model, +ModelFragment) removes a model fragment that cannot be deleted through normal means. 
 * It: 1. removes the hypers from the parent to the model fragment, 2. deletes the hypers and whatever it hypers to and
 * 3. frees the model fragment itself */ 
forceDeleteMF(@model, ModelFragment) :-
    % Remove the hypers from the parents to the model fragment
    get(ModelFragment, parents, ParentsC),
    chain_list(ParentsC, Parents),

    forall(
	member(Parent, Parents),
	(
	    get(Parent, find_hyper, @default, message(@prolog, ==, @arg2?to, ModelFragment), MFHyper),
	    free(MFHyper)
	)
    ),

    % Remove all the hypers in the model fragment
    get(ModelFragment, all_hypers, AllHypers),
    send(AllHypers, for_all,
	and(
	    message(@arg1?to, free),
	    message(@arg1, free)
	)
    ),

    % Free the model fragment
    free(ModelFragment).


/*  removeOldSketchDatastructures(+Model) removes old Sketch datastructures
*   (sheets in @modelabstractEntities) that corrupt models (makes it 
*   impossible to open Sketches. TODO: the new sketch datastructures should
*   also not be in @model?abstractEntities, as it mixes the build and sketch
*   environment. This seems like a potential cause for bugs. */
removeOldSketchDatastructures(Model) :-
    send(Model?abstractEntities, for_all, 
	if(
	    message(@prolog, ==, @arg1?class_name, sheet),
	    and(
		message(Model?abstractEntities, delete, @arg1),
		message(@arg1, free)
	    ),
	    message(@prolog, true)
	)
    ).



