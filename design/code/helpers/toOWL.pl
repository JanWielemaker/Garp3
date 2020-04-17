:- use_module(owl).
:- use_module(library('semweb/rdf_db')).

% Create a blank node
owl_create_node(Node) :-
    rdf_bnode(Node).


% Assert an objectproperty to the rdf database
create_objectproperty(Range, Property, Domain) :-
    rdf_assert(Property, rdf:type, owl:'ObjectProperty'),
    rdf_assert(Property, rdfs:range, Range),
    rdf_assert(Property, rdfs:domain, Domain),
    rdf_assert(Property, owl:inverseOf, wordtGegeten).


% Create a class 
% owl_create_class(+Class, -Node)
owl_create_class(Class, Label) :-
    term_to_atom(Label, Label2),
    atom_length(Label2, Length),
    End is Length - 2,
    sub_atom(Label2, 1, End, _Foo, Label3),
    rdf_assert(Class, rdf:'type', owl:'Class'),
    rdf_assert(Class, rdfs:'label', literal(lang('en', Label3))).

owl_create_class(Class) :-
    rdf_assert(Class, rdf:'type', owl:'Class').

owl_add_label(URI, LanguageID, Label) :-
    rdf_assert(URI, rdfs:'label', literal(lang(LanguageID, Label))).

owl_add_comment(URI, LanguageID, Comment) :-
    rdf_assert(URI, rdfs:'comment', literal(lang(LanguageID, Comment))).

owl_create_subclass(Node, Subclass) :-
    rdf_assert(Node, rdfs:'subClassOf', Subclass).

% Add disjoint classes to a node
% owl_create_disjointclasses(+Node, +[DisjointClasses])
owl_create_disjointclasses(Node, DisjointClasses) :-
    forall( 
	member(DisjointClass, DisjointClasses),
	(
	    rdf_global_id(DisjointClass, DisjointClassExpanded),
	    rdf_assert(Node, owl:'disjointWith', DisjointClassExpanded)
	)
    ).

owl_create_comment(Node, Comment) :-
    rdf_assert(Node, rdfs:'comment', literal(Comment)).

owl_register_ns(NS, NSURL) :-
    rdf_register_ns(NS, NSURL).

owl_create_uri(NS, Label, URI) :-
    new(S, string('%s', Label)),
    send(S, translate, ' ', '_'),
    send(S, prepend, NS),
    get(S, value, URI),
    free(S).

% Assert a class to the rdf database
create_class(Class, Subclass, DisjointClasses) :-
    rdf_global_id(Class, ClassExpanded),
    rdf_assert(ClassExpanded, rdf:'type', owl:'Class'),
    rdf_assert(ClassExpanded, rdfs:'label', literal(lang('en', Class))),
    rdf_assert(ClassExpanded, rdfs:'subClassOf', Subclass),
    forall( 
	member(DisjointClass, DisjointClasses),
	(
	rdf_global_id(DisjointClass, DisjointClassExpanded),
	rdf_assert(ClassExpanded, owl:'disjointWith', DisjointClassExpanded)
	)
	).

owl_type(Node, Type) :-
    rdf_assert(Node, rdf:'type', Type).

owl_property(Domain, Property, Range) :-
    rdf_assert(Domain, Property, Range).

owl_onProperty(Node, Property) :-
    rdf_assert(Node, owl:'onProperty', Property).

owl_create_restriction(Node, Property) :-
    rdf_bnode(Node),
    rdf_assert(Node, rdf:'type', owl:'Restriction'),
    owl_onProperty(Node, Property).

owl_allValuesFrom(Restriction, Class) :-
    rdf_assert(Restriction, owl:'allValuesFrom', Class).

owl_someValuesFrom(Restriction, Class) :-
    rdf_assert(Restriction, owl:'someValuesFrom', Class).

owl_hasValue(Restriction, Individual) :-
    rdf_assert(Restriction, owl:'hasValue', Individual).

owl_cardinality(Node, Integer) :-
    rdf_assert(Node, owl:'cardinality', literal(Integer)).

owl_minCardinality(Node, Integer) :-
    rdf_assert(Node, owl:'minCardinality', literal(Integer)).

owl_maxCardinality(Node, Integer) :-
    rdf_assert(Node, owl:'maxCardinality', literal(Integer)).

owl_oneOf(Node, ClassesList) :-
    rdf_bnode(NewNode),
    rdf_assert(Node, owl:'oneOf', NewNode),
    owl_create_list_thing(NewNode, ClassesList).

owl_intersectionOf(Node, ClassesList) :-
    rdf_bnode(NewNode),
    rdf_assert(Node, owl:'intersectionOf', NewNode),
    owl_create_list_class(NewNode, ClassesList).

owl_unionOf(Node, ClassesList) :-
    rdf_bnode(NewNode),
    rdf_assert(Node, owl:'unionOf', NewNode),
    owl_create_list_class(NewNode, ClassesList).

owl_distinctMembers(Node, IndividualsList) :-
    rdf_bnode(NewNode),
    rdf_assert(Node, owl:'distinctMembers', NewNode),
    owl_create_list_thing(NewNode, IndividualsList).

owl_create_individual(IndividualURI, ClassURI) :-
    rdf_assert(IndividualURI, rdf:'type', ClassURI).

owl_create_individual(IndividualURI, Label, ClassURI) :-
    rdf_assert(IndividualURI, rdf:'type', ClassURI),
    rdf_assert(IndividualURI, rdfs:'label', literal(lang('en', Label))).

owl_open_list(Node, First) :-
    rdf_bnode(Node),
    rdf_assert(Node, rdf:'type', rdf:'List'),
    rdf_assert(Node, rdf:'first', First).

owl_close_list(Node, Rest) :-
    rdf_assert(Node, rdf:'rest', Rest).
    
owl_create_list(Node, First, Rest) :-
    rdf_bnode(Node),
    rdf_assert(Node, rdf:'type', rdf:'List'),
    rdf_assert(Node, rdf:'first', First),
    rdf_assert(Node, rdf:'rest', Rest).

%owl_create_set(ClassName, [SetElement|SetElements]) :-
%    create_class(ClassName, '#Entity', []),
%    rdf_bnode(Set),
%    rdf_assert(Set, rdf:'type', rdf:'List'), 
%    rdf_assert(Set, rdf:'first', SetElement),
%    rdf_assert(Set, rdf:'rest', rdf:'nil'),  
%    rdf_assert(ClassName, owl:'unionOf', Set),
%    owl_create_list_items(Set, SetElements).

%owl_create_list(_, []).

owl_create_list_class(Node, [Element|RestList]) :-
    rdf_assert(Node, rdf:'type', rdf:'List'),
    %rdf_bnode(ListTarget),
    %rdf_assert(ListTarget, rdf:'about', literal(Element)),
    %rdf_assert(ListTarget, rdf:'type', owl:'Class'), 
    rdf_assert(Element, rdf:'type', owl:'Class'),
    rdf_assert(Node, rdf:'first', Element),
    rdf_assert(Node, rdf:'rest', rdf:'nil'),
    %rdf_assert(Node, rdf:'rest', rdf:'nil').
    (
    	RestList \== [], !, 
    	rdf_bnode(NewNode),
    	rdf_update(Node, rdf:'rest', rdf:'nil', object(NewNode)),
    	owl_create_list_class(NewNode, RestList)
    )
    ;
    (
	% End Statement
	true
    ).

owl_create_list_thing(Node, [Element|RestList]) :-
    rdf_assert(Node, rdf:'type', rdf:'List'),
    %rdf_node(ListTarget),
    %rdf_assert(listTarget, rdf:'about', literal(Element)),
    %rdf_assert(Element, rdf:'type', owl:'Thing'), 
    rdf_assert(Node, rdf:'first', Element),
    rdf_assert(Node, rdf:'rest', rdf:'nil'),
    %rdf_assert(Node, rdf:'rest', rdf:'nil').
    (
    	RestList \== [], !, 
    	rdf_bnode(NewNode),
    	rdf_update(Node, rdf:'rest', rdf:'nil', object(NewNode)),
    	owl_create_list_thing(NewNode, RestList)
    )
    ;
    (
	% End Statement
	true
    ).

create_ontology(OntologyURI, ImportURL) :-
    rdf_assert(OntologyURI, rdf:'type', owl:'Ontology'),
    rdf_assert(OntologyURI, owl:'imports', ImportURL).
