/*
gp3 0.3
Definition fragmentRefiner class: contains information about refinement of
an importedFragment element of another (parent or conditional) model fragment.
Also in this file: the fragmentRefinerIdentity helper class.

A refinement is modeled as a new conditional fragment in a modelfragment. So this is a subclass of importedFragment. This way, most code does not have to make a diffence between conditional fragments and refiners: they are both just conditional elements. Ofcourse: interface code must make this difference.

A refiner is a new conditional fragment that partly "exports" the same instances and agents as the refined fragment. We connect these matching elements through fragmentRefinerIdentity relations, which is an identityRelation subclass. By also adding these relations to the conditions of the fragment, again there is no need for rewriting export and reasoning code: the elements all fit in the current framework. Again: interface code must make the difference, by not trying to draw these internal identities.

When seen as a conditional fragment this fragmentRefiner has a ?referencedFragment: the fragment it symbolises.
Because it is a refinement, it also has a reference to the importedFragment it refines (the element in ?refined, the route to it in ?refinedRoute).
There might in some akward way of dreadful multiple inheritance circumstances be a possibility where you can refine a modelfragment in more than one way, i.e. when a modelfragment is two times a parent of another fragment. Thats why we also keep the ?refinementRoute: the way to get from ?referencedFragment to the fragment of ?refined throught the parent-child hierarchy.

Thats all. Kid does laundry as we say in dutch.

Part of Garp3, see copyright notice
Some code copied from other old homer classes, so comments there in dutch

2000-2005 Jelmer Jellema, Spin in het Web, spininhetweb.nl
*/

:-pce_begin_class(
		  fragmentRefiner,
		  importedFragment
		 ).

variable(refinedRoute, chain, none). %route to the refined importedFragment
variable(refinementRoute, chain, none). %child-parent route from refinement MF to refined MF
%%
initialise(FR, MF: modelFragment, Refinement: modelFragment, RefinementRoute: chain,
		Refined: importedFragment, RefinedRoute: chain,
		Remarks : remarks = [string]) :->
	%init this refiner.
	

	FR->+initialise(MF, Refinement, Remarks),
	FR->>refinementRoute(RefinementRoute),
	FR->>refinedRoute(RefinedRoute),
	FR->>hyper(Refined,refined,refiner).
%%

%%
refined(FR, IF: importedFragment):<-
	%returns the importedFragment/fragmentRefiner element this refiner is about
	IF = FR<<-hypered(refined).
%%
%%
refinementRoute(FR,
	R: chain):->
	%set refinementRoute

	FR->>slot(refinementRoute,R?copy).
%
refinementRoute(FR,
	R: chain):<-
	%return refinementRoute

	R = ?(FR,slot,refinementRoute)<<-copy.
%%

%%
refinedRoute(FR,
	R: chain):->
	%set refinedRoute

	FR->>slot(refinedRoute,R?copy).
%
refinedRoute(FR,
	R: chain):<-
	%return refinedRoute

	R = ?(FR,slot,refinedRoute)<<-copy.
%%

%%
name(FR, N: name):<-
	%helper: we would like all elements to understand ?name
	N = FR<<-refinementName.
%%

%%
refinementName(FR,N: name):<-
	%name of the refinement
	N = FR?referencedFragment<<-name.
%%

%%
refinedName(FR, N: name):<-
	%name of the refined fragment (of refinement)
	
	N = FR?refined<<-name.
%%

%%
checkDeleteElement(FR,
	Element: fragmentElement,	
	CR: changeRequestor):->
	%gp3 0.1 Overwrite: We check whether we should go as well
	
	if
	(
		Element = FR<<-refined
	;
		FR?refinedRoute->>member(Element)
	)
	then
		CR->>addChangeRequest(changeRequestor(deleteFragmentRefiner,
								FR?fragment,
								@default,
								CR?garpModel,
								FR),
			string('The fragment refinement "%s" for model fragment "%s" in %s "%s" will be deleted because the refined model fragment is no longer part of the %s',FR?refinementName,FR?refinedName,
				FR?fragment?displayTypeName, FR?fragment?name,
				FR?fragment?displayTypeName),
			FR?fragment),
	if
	(
		Element = FR<<-referencedFragment
	;
		FR?refinementRoute->>member(Element)
	)
	then
		CR->>addChangeRequest(changeRequestor(deleteFragmentRefiner,
								FR?fragment,
								@default,
								CR?garpModel,
								FR),
			string('The fragment refinement "%s" for model fragment "%s" in %s "%s" will be deleted because "%s" is no longer available as a specialisation for "%s"',FR?refinementName,FR?refinedName,
				FR?fragment?displayTypeName, FR?fragment?name,
				FR?refinementName,FR?refinedName),
			FR?fragment).	
%%

%%
createIdentities(FR):->
	%request from our fragment to create the fragmentRefinerIdentity objects
	%with which we map instances in refined to instances in refinement, so 
	%all reasoning code "thinks" they are just instances with identities.
	
	%all instances (agents and entities) in ?refined are also available in refinement. So we map them.
	%This is all instances in ?refined and higher
	
	RouteToRefined = FR<<-refinedRoute, %a copy
	RouteToRefined->>append(FR?refined),
	RouteToRefiner *= chain(FR), %we are directly in the fragment
	RouteToRefiner->>merge(FR?refinementRoute), %so this brings us to the same level
	Start = FR<<-refined,
	FR->>createIdentities_level(Start,RouteToRefined,RouteToRefiner).
%
createIdentities_level(FR,Current: importedFragment,RouteToRefined: chain,RouteToRefiner: chain):->
	%helper: add identities for this level, and then go on with parents and conditionals
	%always add the identities to the fragment of FR
	%this level
	AllInstances = Current?referencedFragment<<-findElements(garpInstance),
	AllInstances->>for_all(
		and(
			->>(FR,hyper,
					?(FR?fragment,addNewIdentity,@arg1,RouteToRefined,
						@arg1,RouteToRefiner,'',fragmentRefinerIdentity),
					identity,refiner) %same object, other route, and hyper it
		)
	),
	
	%next level:
	AllNext = Current?referencedFragment<<-parentReferences, %this is a copy (now)
	AllNext->>merge(?(Current?referencedFragment,findElements,importedFragment)), %also other refiners
	AllNext->>for_all(->>(FR,createIdentities_next,@arg1,RouteToRefined,RouteToRefiner)).
%
createIdentities_next(FR,NewCurrent: importedFragment,RouteToRefined: chain,RouteToRefiner: chain):->
	%continue by adding NewCurrent to the routes. This is the same for both as we are working in two copies of the same modelfragmen
	NewRefined = RouteToRefined<<-copy,
	NewRefiner = RouteToRefiner<<-copy,
	NewRefined->>append(NewCurrent),
	NewRefiner->>append(NewCurrent),
	FR->>createIdentities_level(NewCurrent,NewRefined,NewRefiner).
%%


%%
checkReRefinement(FR,
	Fragment: modelFragment,
	Refined: importedFragment,
	RefinedRoute: chain,
	CR: changeRequestor):->
	
	%check if a new refiner refines something on our path, will not allow that
	
	NewFullPath = RefinedRoute<<-copy,
	NewFullPath->>append(Refined),
	
	OurFullPath = FR<<-refinedRoute, %is a copy
	OurFullPath->>append(FR?refined),
	
	if
		pl_checkReRefinement_helper(Fragment,NewFullPath,OurFullPath)
	then
		CR->>impossible(string('The new refinement is incompatible with an existing refinement in %s "%s"',
				FR?fragment?displayTypeName,
				FR?fragment?name)
			,FR?fragment).
%
pl_checkReRefinement_helper(Fragment,NewFullPath,OurFullPath):-
	%if OurFullPath starts with a reference to Fragment (the fragment of the new
	%refiner) and then follows NewFullPath, this is a problem
	%because that means our ?refined of ?refinedRoute is being refined
	
	Fragment = OurFullPath?head<<-referencedFragment,
	%ok, try on a copy
	?(OurFullPath,sub,1,NewFullPath?size + 1)->>equal(NewFullPath),!. %done
%
pl_checkReRefinement_helper(Fragment,NewFullPath,OurFullPath):-
	%otherwise, see if we find the NewFullPath further on in our full path
	
	OurFullPath->>delete_head, %we work on a copy anyway
	pl_checkReRefinement_helper(Fragment,NewFullPath,OurFullPath).
%%

%%
checkRecreateIdentities(RF, _Object: modelFragment, CR: changeRequestor):->
	%called by our fragment to check if a new instance in Object is
	%reason for us te recreate our fragmentRefinerIdentity relations

	%for now, we just always do it, I think that is quicker than
	%doing a check. When this poses a problem
	%do the check: check if MF is available in refined (is refined, is a parent of refined, is a condition in refined or recursively so for all parents and conditional fragments).
	
	%we just do it: this means add subrequests for deleting current identities
	%and a special one for recreating them
	
	FR = RF<<-fragment,
	?(RF,all_named_hypered,identity)->>for_all(
		->>(CR,addChangeRequest,
			create(changeRequestor,deleteIdentity,
								FR,
								@default,
								CR?garpModel,
								@arg1),
			'Automatic update of refiner identity data: delete old identity',@nil,@nil)
		),
								
	CR->>addChangeRequest(changeRequestor(internalCreateRefinerIdentities,
								RF?fragment,
								@default,
								CR?garpModel,
								RF),
			string('Automatic updating of refiner identity data',RF),@nil,@nil).
	
%%

copyToNewElement(FR,Mappings: hash_table, NewMF: modelFragment, New: fragmentRefiner):<-
	%copy contents and relations (hypers) to a new fragmentRefiner
	%see fragmentElement<<-copyToNewElement for details
	%we fail if required connected objects are missing from the Mappings table
	
	%in our case we use mf<-addNewFragmentRefiner to create it, without creating the identities right now, these copy themself en rehyper to the new refiner
	
	%we assume the refined etc do not change their fragment (these still refer to the same modelfragment, I mean). This is how it works now anyway.

	RefinedInfo = NewMF<<-copyMF_mapRelatedElement(FR?refined, FR?refinedRoute,Mappings),
	%fails if not available yet, which means: try again later
	
	%refinement and refinementroute do not change if the fragment of refined does
	%not change..

	New = NewMF<<-addNewFragmentRefiner(RefinedInfo?first, RefinedInfo?second,
		FR?referencedFragment,FR?refinementRoute, FR?remarks,@off). %do not create identities
%%

:-pce_end_class.
		  
:-pce_begin_class(
		  fragmentRefinerIdentity,
		  identityRelation
		 ).
%internal element, see top of file

%some overruling of interface like code, to make sure we stay quiet as a mouse
%%
checkDeleteElement(I,
	Element: fragmentElement,
	CR: changeRequestor):->
	if
		(
			Element = I<<-argument1
		;
			I?argument1Route->>member(Element)
		;
			Element = I<<-argument2
		;
			I?argument2Route->>member(Element)
		)
	then
		%just a deleteIdentity, but quiet
		CR->>addChangeRequest(changeRequestor(deleteIdentity,
								I?fragment,
								@default,
								CR?garpModel,
								I),
			string('Automatic removal of internal element (%s) used in representing the refinement',I),@nil,@nil). 
%%

%%
checkChangedConfiguration(I,
	Arg1: garpInstance,
	Arg1Route: chain,
	Arg2: garpInstance,
	Arg2Route: chain,
	OtherMF: modelFragment,
	CR: changeRequestor):->
	
	%this should not happen, because drawing code would prevent both arguments
	%to be visible. 
	%to be sure, we forbid it with some cryptic message
	
	if
	(
		I->>isArgument(Arg1),
		I->>isArgument(Arg2),
		(
			OtherMF = I<<-fragment
			;
			(
				Arg1Route->>find(@arg1?referencedFragment == I?fragment),
				Arg2Route->>find(@arg1?referencedFragment == I?fragment)
			)
			;
			(
				I?argument1Route->>find(
			@arg1?referencedFragment == OtherMF), %we
				I?argument2Route->>find(
			@arg1?referencedFragment == OtherMF)
			)
		)
	)
	then
		CR->>impossible(
			string('These instances represent the same element in a refined modelfragment.'),I?fragment).
%%

%%
copyToNewElement(FE,Mappings: hash_table, NewMF: modelFragment, New: fragmentRefinerIdentity):<-

	%we do almost the same as the more general version in identityRelation,
	%only we check if there is allready a mapping for the fragmentRefiner we are related to. Only if so, we continue, and rehyper also
	
	Refiner = FE<<-hypered(refiner),
	NewRefiner = NewMF<<-copyMF_mapRelatedElement(Refiner,@nil,Mappings), %fails if not yet done
	New = FE+<-copyToNewElement(Mappings,NewMF), %above call does a trick with class_name etc, so it returns an instance of the right class
	NewRefiner->>hyper(New,identity,refiner).
	
:-pce_end_class.
