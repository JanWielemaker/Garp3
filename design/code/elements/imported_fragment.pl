/*
Definitie importedFragment class: verwijzing naar een ander fragment dat conditioneel is in dit fragment of
parent is van dit fragment
Vooral gebruikt om bij subelementen in het huidige fragment te kunnen verwijzen naar een element in het
geïmporteerde fragment (via de route informatie)

Part of Garp3, see copyright notice
Most code old dutch homer code

2000-2005 Jelmer Jellema, Spin in het Web, spininhetweb.nl
*/

:-pce_begin_class(
		  importedFragment(referencedFragment),
		  fragmentElement
		 ).

%%
initialise(IF, MF: modelFragment, Referenced: modelFragment, Remarks : remarks = string) :->
	IF->+initialise(MF, Remarks),
	IF->>hyper(Referenced,referencedFragment,fragmentReference).
%%

%%
referencedFragment(IF,
	F: modelFragment):<-
	%geef het fragment waar naar wordt verwezen

	F = IF<<-hypered(referencedFragment).
%%

%%
name(IF, N: name):<-
	%gewoon helpertje omdat we graag willen dat veel objecten ?name snappen
	N = IF?referencedFragment<<-name.
%%

%%
copyToNewElement(FE,_Mappings: hash_table, NewMF: modelFragment, New: importedFragment):<-
	%gp3 0.2 Copy contents and relations (hypers) to a new importedFragment
	%see fragmentElement<<-copyToNewElement for details
	%we fail if required connected objects are missing from the Mappings table
	
	%only used for conditional fragments (elements), not for parents
	%with importedFragment, no tests are needed to check if the mappings are allready complete enough
	
	%everything checked, we can create the item
	New = NewMF<<-addNewConditionalFragment(FE?referencedFragment, FE?remarks).
%%

%%
findElementsInRoute(IF, MainModelFragment, ElementsChain) :<-
    % Find the elements in the route of this imported modelfragment
    findRouteInGarp(IF, MainModelFragment, IFRouteChain, _IFMainMF),
    send(IFRouteChain, append, IF),

    /* Add the elements inherited from the model fragments in which an imported fragment was refined */
    chain_list(IFRouteChain, IFRouteChainList),
    forall(
	(
	    member(Refinement, IFRouteChainList),
	    get(Refinement, class_name, 'fragmentRefiner')
	),
	(
	    get(Refinement, fragment, FragmentWithRefinement),
	    getAllMFParentReferences(FragmentWithRefinement, ParentReferencesChain),
	    send(IFRouteChain, merge, ParentReferencesChain)
	)
    ),

    %format('De route is: '), printChainNames(IFRouteChain), nl,
    /* Create a list with all the elements */
    new(ElementsChain, chain),
    send(IFRouteChain, for_all,
	message(ElementsChain, merge, @arg1?referencedFragment?elements)
    ).


%%
:-pce_end_class.
		  
