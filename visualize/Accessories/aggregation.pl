/*
    This file includes routines for aggregation,
    to be used in VisiGarp
    Anders Bouwer, 5/12/2001

    Last updated 4 May 2005
*/


% :- load_files(['aggregation_path_level', 'aggregation_local_level'], [silent(true)]).

:- dynamic
        rec_aggr_transition/2,
        rec/1,
        edge_deleted/1,
        temp_event_counter/2,
        trigger_conditions/3.
:- multifile
        rec/1,
        trigger_conditions/3.


% reduce_digraph(Graph)
%
% transitive reduction of Graph: remove all transitive
% edges T (A->B) for which there is a directed path A to B
% which does not include T.
%
% the resulting (remaining) edges are asserted as
% rec_aggr_transition(A, B)
% which stands for recorded aggregated transition.
%
reduce_digraph(Graph):-
    % delete previous edges
    writef('Performing transitive reduction... \n', []),
    retractall(rec_aggr_transition(_, _)),
    retractall((rec(aggregation, _))),
    % copy all edges to rec_aggr_transition
    forall(rec_transition(A, B),
         assertz(rec_aggr_transition(A, B))
    ),
    % delete transitive edges
    forall(transitive_edge(Graph, A, B),
         (writef('Transitive edge found from %d to %d. \n', [A, B]),
          retract(rec_aggr_transition(A, B))
         )
    ),
    asserta(rec(aggregation, reduce_digraph)),
    writef('Completed transitive reduction. \n', []).




% reduce_digraph_input(Graph)
%
% transitive reduction of input-state transitions in Graph:
% remove all transitive edges T (Input->B) for
% which there is a directed path Input to B
% which does not include T.
%
reduce_digraph_input(Graph):-
    % delete previous edges
    writef('Performing transitive reduction of input transitions... \n', []),
    retractall(rec_aggr_transition(_, _)),
    retractall((rec(aggregation, _))),
    % copy all edges to rec_aggr_transition
    forall(rec_transition(A, B),
         assertz(rec_aggr_transition(A, B))
    ),
    % delete transitive edges
    % writef('Reduce digraph input 2 \n', []),
    A0 = input,
    forall(transitive_edge(Graph, A0, B),
         (
          % writef('Transitive edge from %d to %d found. \n', [A0, B]),
          retract(rec_aggr_transition(A0, B))
         )
    ),
    asserta(rec(aggregation, reduce_digraph_input)),
    writef('Completed transitive reduction of input transitions. \n', []).



aggregate_digraph(Graph):-
    % Find a group of paths P1 to Pn with the same
    % begin point A and end point B for which holds:
    %  P1 to Pn contain the same events, (or events
    %  which can be abstracted into the same higher
    %  level events)
    %
    % do transitive reduction first
    % (does it make sense not to do this?)
    OriginalGraph = state_graph(original, input),
    reduce_digraph_input(OriginalGraph),
    %
    writef('Performing aggregation of orderings on original simulation... \n', []),
    % repeat by failure
    repeat,
    % if equivalent paths can be found, which can be aggregated,
    % then repeat by failure
    ((min_equivalent_paths(A, B, Paths),
     aggregate_digraph(Graph, A, B, Paths))
     ->
       fail
     ;
       true
    ),
    asserta(rec(aggregation, aggregate_digraph)),
    writef('Completed aggregation of orderings on original simulation. \n', []).



% for paths starting from input, don't do anything
aggregate_digraph(_Graph, input, _B, _Paths):-
    !, fail.


aggregate_digraph(Graph, A, B, Paths):-
    % set temporary variable to false
    retractall(edge_deleted(_)),
    assertz(edge_deleted(false)),
    % for every Path in Paths
    forall(member(Path, Paths),
	% for every edge in Path
        forall(sublist([X,Y], Path),
	  (
	   % if the edge does not come after an incoming,
	   % or before an outgoing branch, and it is not
	   % the shortcut edge from A -> B, delete it
	   (not(after_incoming_branch(Graph, X, Y, Path)),
            not(before_outgoing_branch(Graph, X, Y, Path)),
	    not((A == X, B == Y))
	   )
	   ->
	      % record edges to be retracted
	      % don't retract yet, otherwise the recognition
              % of branches will be disturbed
	      assertz(to_be_retracted(rec_aggr_transition(X, Y))),
	      retractall(edge_deleted(_)),
	      assertz(edge_deleted(true))
	      % writef('Deleted edge %p -> %p  \n', [X, Y])
	   ;
	      true
	  )
	)
    ),
    % if at least one edge was deleted, add shortcut if necessary;
    % otherwise, fail
    (edge_deleted(true)
     ->
        % add shortcut edge A -> B
        writef('Constructing shortcut %p -> %p for paths: %p \n', [A, B, Paths]),
	% record which paths are aggregated in A->B
        asserta(rec(aggregated_paths(A->B, Paths))),
        (rec_aggr_transition(A, B)
        ->
        % edge already existed
           true
        ;
	   assertz(rec_aggr_transition(A, B)),
           % record events for transition which aggregates Paths
           record_events(A->B, Paths)

           % writef('Constructed shortcut %p -> %p  \n', [A, B])
        ),
        % retract all edges to be retracted (this may
	% include the shortcut which was supposed to be added!)
        forall(to_be_retracted(Clause),
	       retractall(Clause)
	),
        retractall(rec(upper_bound_path_length(Graph, _Max))),
        retractall(to_be_retracted(_))
     ;
        fail
    ).



% aggregate_digraph(_Graph).



% graph nodes in aggregated state graph
%
% without input node
graph_node(state_graph(aggregated, noinput), N):-
    graph_node(state_graph(original, noinput), N),
    from_or_to_node(state_graph(aggregated, noinput), N).

% including input node
% same as above
graph_node(state_graph(aggregated, input), N):-
    graph_node(state_graph(aggregated, noinput), N).

% plus input node
% graph_node(state_graph(aggregated, input), input).
graph_node(state_graph(aggregated, input), 0).

% both original and aggregated graph nodes
%
% for the moment, the original nodes are a superset of the
% aggregated ones, so the latter can be neglected here
graph_node(state_graph(both, InputOrNot), N):-
    graph_node(state_graph(original, InputOrNot), N).




from_or_to_node(Graph, N):-
    % the state appears as a from-node in a transition
    graph_edge(Graph, N, _B, to),!.

from_or_to_node(Graph, N):-
    % the state appears as a to-node in a transition
    graph_edge(Graph, _A, N, to),!.







% upper_bound_path_length(Graph, Max)
%
% Returns Max as upper bound of path length
% in Graph
% Very rough estimate - how can this be optimized?
% It currently doesn't take into account that nodes
% become isolated
%
% if it was recorded, retrieve it
upper_bound_path_length(Graph, Max):-
       rec(upper_bound_path_length(Graph, Max)), !.

% if not, calculate it, and record for future use
upper_bound_path_length(Graph, Max):-
    findall(Node,
       (graph_node(Graph, Node),
        in_degree(Graph, Node, Din),
        out_degree(Graph, Node, Dout),
        % check if it still has a link in or out
        (Din > 0; Dout > 0)
       ), NodesList),
    list_to_set(NodesList, Nodes),
    length(Nodes, Max),
    % writef('New upper bound on longest path length: %d \n', [Max]),
    assertz(rec(upper_bound_path_length(Graph, Max))).










% equivalent_paths(A, B, ListOfPaths)
%
% returns a ListOfPaths consisting of paths between
% A and B in state_graph
%
equivalent_paths(A, B, [P|ListOfPaths], MaxLength):-
    Graph = state_graph(aggregated, noinput),
    path(A, B, Graph, P, _L),
    % reset previous recorded nodes
    retractall(rec(node_already_in_path_set(_Node))),
    % writef('path set erased... trying path from %d to %d: %p \n', [A,B,P]),
    forall(member(PNode, P),
              (PNode == A;
               PNode == B;
               % writef('node in path set: %d \n', [PNode]),
               asserta(rec(node_already_in_path_set(PNode)))
              )
    ),

    % Neglect length variable - paths may be of different length

    setof(Path,
           Length^(path(A, B, Graph, Path, Length),
             % make sure that no node (except A & B) occurs more
             % than once in the whole set of paths
             Path \== P,
             forall(member(Node, Path),
              (
               % writef('checking node: %d \n', [Node]),
               Node == A;
               Node == B;
               not(rec(node_already_in_path_set(Node))),
               % writef('node in path set: %d \n', [Node]),
               asserta(rec(node_already_in_path_set(Node)))
               )
              ),
              contain_same_events(P, Path)
              % contain_same_rules(P, Path)

           ), ListOfPaths
    ),
    calculate_max_length([P|ListOfPaths], MaxLength).





% min_equivalent_paths(A, B, ListOfPaths)
%
% returns a ListOfPaths consisting of minimal equivalent paths
% between A and B
%
min_equivalent_paths(A, B, ListOfPaths):-
    Graph = state_graph(aggregated, noinput),
    % reset previous recorded nodes
    retractall(rec(node_already_in_path_set(_Node))),
    % find outbranch as beginpoint A
    out_degree(Graph, A, Dout),
    Dout > 1,
    % find inbranch as endpoint B
    in_degree(Graph, B, Din),
    Din > 1,
    % find set of equivalent paths between A and B
    equivalent_paths(A, B, ListOfPaths, _L),
    length(ListOfPaths, N),
    N > 1.



/*
% previous clause failed
%
min_equivalent_paths(A, B, ListOfPaths):-
    retractall(rec(node_already_in_path_set(_Node))),
    writef('equivalent PathSet between %d and %d was not minimal: \n', [A,B]),
    my_write_list(ListOfPaths), fail, !.
*/



% after_incoming_branch(Graph, A, B, Path)
%
% succeeds when the edge A -> B occurs after an incoming
% branch in Path
%
after_incoming_branch(Graph, A, B, Path):-
    % divide Path in PathBefore, A, B, PathAfter
    conc(PathBefore, [A,B|_PathAfter], Path),
    conc(PathBefore, [A], [_First|PathBeforeB]),
    % A->B occurs after an incoming branch when
    % a node in PathBeforeB has in-degree > 1
    % except when this is the first node
    contains_in_branch(Graph, PathBeforeB, _Node).


% before_outgoing_branch(Graph, A, B, Path)
%
% succeeds when the edge A -> B occurs before an outgoing
% branch in Path
%
before_outgoing_branch(Graph, A, B, Path):-
    % divide Path in PathBefore, A, B, PathAfter
    conc(_PathBefore, [A,B|PathAfter], Path),
    % A->B occurs before an outgoing branch when
    % a node in [B|PathAfter] has out-degree > 1
    contains_out_branch(Graph, [B|PathAfter], Node),
    % except the last node?!
    not(last([B|PathAfter], Node)). % order of arguments changed, 28/04/2003


contains_in_branch(Graph, Path, Node):-
    contains_in_branch(Graph, Path, Node, _InDegree),!.

contains_in_branch(Graph, Path, Node, InDegree):-
    member(Node, Path),
    in_degree(Graph, Node, InDegree),
    InDegree > 1.
    % writef('Incoming branch at %d of degree %d \n', [Node, InDegree]).

contains_out_branch(Graph, Path, Node):-
    contains_out_branch(Graph, Path, Node, _OutDegree),!.

contains_out_branch(Graph, Path, Node, OutDegree):-
    member(Node, Path),
    out_degree(Graph, Node, OutDegree),
    OutDegree > 1.
    % writef('Outgoing branch at %d of degree %d \n', [Node, OutDegree]).




% contain_different_edges(P1, P2):-
%
% succeeds whenever path P1 and P2 share no edges
%
contain_different_edges([], _L).

contain_different_edges([_], _L).

contain_different_edges([A,B|R1], P2):-
    not(sublist([A,B], P2)),
    contain_different_edges([B|R1], P2).








% transitive_edge(Graph, A, B)
%
% an edge A->B in Graph is transitive when
% there is a path in Graph from A to B which
% does not include the edge A->B.
%
transitive_edge(Graph, A, B):-
    rec_transition(A, B),
    longer_path(A,B, Graph, _Path, _L).


% longer_path(A, B, Graph, Path, L):-
%
% succeeds when there is a Path between A & B
% which does not include the transition A->B,
% but does include the same rules as this
% transition. The second condition is dropped
% for transitions from input, because it's
% irrelevant in that case.
%
longer_path(A, B, Graph, Path, L):-
    path(A, B, Graph, Path, L),
    not(sublist([A,B], Path)),
    (A == input
     ;
    contain_same_events([A,B], Path)
    % contain_same_rules([A,B], Path)
    ),!.




contain_same_rules(P1, P2):-
    contains_rules(P1, Rules1),
    % writef('P1: %p, Rules1: %p \n', [P1, Rules1]),

    contains_rules(P2, Rules2),
    % writef('P2: %p, Rules2: %p \n', [P2, Rules2]),

    subset(Rules1, Rules2),
    subset(Rules2, Rules1),
    length(Rules1, L1),
    length(Rules2, L1).
    % writef('P1: %p & P2: %p contain same rules: %p \n', [P1, P2, Rules2]).
    % checking subsets can go wrong in rare? cases
    % where events are repeated, but different events...
    % like Rules1 = [A,B,C,A,A,A] & Rules2 = [A,B,C,C,C,C]
    % permutation would be better, but takes too much time
    % permutation(Rules1, Rules2),



% in case of merge_correspondence(A,B) or fuseer_correspondentie(A,B)
% the above fails
%
%
contain_same_rules(P1, P2):-
    contains_rules(P1, Rules1),
    writef('Trying to match P1: %p and P2: %p \n', [P1, P2]),

    contains_rules(P2, Rules2),
    writef('P2: %p, Rules2: %p \n', [P2, Rules2]),

    engine:difference(Rules1, Rules2, Rules1min2),
    writef('Rules1min2: %p \n', [Rules1min2]),
    engine:difference(Rules2, Rules1, Rules2min1),
    writef('Rules2min1: %p \n', [Rules2min1]),
    possible_to_merge(Rules1min2, Rules2min1),
    length(Rules1, L1),
    length(Rules2, L1),
    writef('P1: %p & P2: %p contain same rules: %p \n', [P1, P2, Rules2]).
    % checking subsets can go wrong in rare? cases
    % where events are repeated, but different events...
    % like Rules1 = [A,B,C,A,A,A] & Rules2 = [A,B,C,C,C,C]
    % permutation would be better, but takes too much time
    % permutation(Rules1, Rules2),

% possible_to_merge(Rules1, Rules2)
%
% succeeds when Rules1 contains rules which can
% be merged with rules in Rules2 and vice versa
%
possible_to_merge([],[]).

possible_to_merge([R1|RestRules1], Rules2):-
    % writef('Trying to find corresponding rule in English\n', []),
    R1 = merge_correspondence(A,B),
    R2 = merge_correspondence(B,A),
    member(R2, Rules2),
    engine:difference(Rules2, [R2], RestRules2), !,
    possible_to_merge(RestRules1, RestRules2).


possible_to_merge([R1|RestRules1], Rules2):-
    % writef('Trying to find corresponding rule in Dutch \n', []),
    R1 = fuseer_correspondentie(A,B),
    R2 = fuseer_correspondentie(B,A),
    member(R2, Rules2),
    % writef('R2: %p found to correspond with R1: %p \n', [R2, R1]),
    engine:difference(Rules2, [R2], RestRules2), !,
    % writef('RestRules2: %p \n', [RestRules2]),
    possible_to_merge(RestRules1, RestRules2).


contains_rules([],[]).

contains_rules([_],[]).

contains_rules([input|Rest],RestRules):-
    contains_rules(Rest, RestRules),!.

contains_rules([A,B|Rest], Rules):-
    find_transition_details(A, B,
              CausesList, _ConditionsList, _ResultsList, _Status),
    contains_rules([B|Rest], RestRules),
    conc(RestRules, CausesList, Rules).



% contain_same_events(P1, P2)
%
% succeeds whenever paths P1 and P2 contain the same
% events, as specified below
%
% the same paths contain the same events
contain_same_events(P1, P1):-!.
%     writef('P1 == P2 == %p. \n', [P1]), !.

contain_same_events(P1, P2):-
%     writef('Do P1: %p & P2: %p contain same events? %p \n', [P1, P2, Events2]),

      settings(event_types, EventTypes),
      EventTypes \== [],!,
      forall(member(Type, EventTypes),
        (
         % writef('checking if %p and %p contain the same %d events\n',
         %    [P1, P2, Type]),
         contain_same_events(P1, P2, Type)
        )
      ).

contain_same_events(_P1, _P2):-
      true.
      % writef('No event types selected \n',[]),
      % writef('No check required if %p and %p contain the same events \n',
      %     [P1, P2]).
      % contain_same_rules(P1, P2).

/*
%    contain_same_rules(P1, P2).
    contain_same_events(P1, P2, val_der_events),
   contain_same_events(P1, P2, inequality_events).
 %    contain_same_events(P1, P2, parameters),
 %    contain_same_events(P1, P2, par_values).
 %    contain_same_events(P1, P2, par_relations),
 %    contain_same_events(P1, P2, causal_effects),
%    contain_same_events(P1, P2, causal_events),
%    contain_same_events(P1, P2, mf_events).
 %    contain_same_events(P1, P2, system_elements),
 %    contain_same_events(P1, P2, system_structures).
*/


% contain_same_events(P1, P2, EventType):-
%
% first, check if P1 and P2 are already known to contain
% the same events
contain_same_events(P1, P2, EventType):-
    rec(contain_same_events(P1, P2, EventType)),!.

% also, check if P1 and P2 are already known to contain
% different events - if so, fail. No need to look further!
contain_same_events(P1, P2, EventType):-
    rec(contain_diff_events(P1, P2, EventType)),!,
    fail.

% check if P1 and P2 contain the same events and
% record it for later use
contain_same_events(P1, P2, EventType):-
    contains_events1(local, P1, EventType, _, Events1),
    % writef('P1: %p, Events1: \n', [P1]),
    % write_events(Events1),

    contains_events1(local, P2, EventType, _, Events2), !,
    % writef('P2: %p, Events2: \n', [P2]),
    % write_events(Events2),

    (equal_event_sets(Events1, Events2)
    ->
      asserta(rec(contain_same_events(P1, P2, EventType))),
      writef('P1: %p & P2: %p contain same %d \n', [P1, P2, EventType])
    ;
      asserta(rec(contain_diff_events(P1, P2, EventType))),
      writef('P1: %p & P2: %p contain different %d  \n', [P1, P2, EventType]),
      fail, !
    ).

    % checking subsets can go wrong in rare? cases
    % same as with rules above

% equal_event_sets(Events1, Events2)
%
% returns true when Events1 contains the same events as Events2
%

equal_event_sets(Events1, Events2):-
    % inefficient version
    event_subset(Events1, Events2),
    event_subset(Events2, Events1),!.


/*
equal_event_sets(Events1, Events2):-
    length(Events1, K1
*/


event_subset([], _Events2).



% event_subset(Events1, Events2)
%
% is true when Events1 is a subset of Events2
%
event_subset([E|Events1], Events2):-
    member(E, Events2),
    !, event_subset(Events1, Events2).



% special case for structured events like added(...) and removed(...)
% which contain multiple things to be checked
%
event_subset([E|Events1], Events2):-
    E = removed(A->B, Things),
    forall(member(X, Things),
        (member(E2, Events2),
         E2 = removed(_A2->_B2, Things2),
         member(X, Things2)
	 ;
         writef('Event not present in both paths: removed(%p) in %p->%p \n', [X,A,B]),
         !, fail
        )
    ),
    !, event_subset(Events1, Events2).


event_subset([E|Events1], Events2):-
    E = added(A->B, Things),
    forall(member(X, Things),
        (member(E2, Events2),
         E2 = added(_A2->_B2, Things2),
         member(X, Things2)
	 ;
         writef('Event not present in both paths: added(%p) in %p->%p \n', [X,A,B]),
         !, fail
        )
    ),
    !, event_subset(Events1, Events2).



% event_subset(Events1, Events2)
%
% for some types of events, the state nrs are included
% in the event representation. This is taken care of in this
% clause.
%
event_subset([E1|Events1], Events2):-
    E1 =.. [EventHead, (_A1->_B1)| Rest],
    E2 =.. [EventHead, (_A2->_B2)| Rest],
    % writef('\n Searching %p from %d->%d in Events 2:  \n', [E1, A1, B1]),
    member(E2, Events2),
    % writef('   Found     %p from %d->%d in Events 2:  \n', [E1, A1, B1]),
    !,
    event_subset(Events1, Events2).


% event_subset(Events1, Events2)
%
% when the previous clause fails, this one
% presents the event that caused it.
%
event_subset([E1|_Events1], _Events2):-
    E1 =.. [_EventHead, (A1->B1)|_Rest],
    writef('\n %p is only included in %d->%d. \n', [E1, A1, B1]),
    % writef('Events 2: \n', []),
    % write_events(_Events2),
    !,
    % trace,
    fail.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% contains_events1(local, Path, EventType, Subjects, Events)
%
% checks whether local events of EventType are recorded,
% and returns them; if not, it will determine the events
% first, and then record and return them
%
contains_events1(local, [], _, [], []).

contains_events1(local, [_], _, [], []).

% when input state is included in selected statelist,
% all new interesting information in the first state
% should be considered an event too. Input state = 0.
%
contains_events1(local, [input|Rest], EventType, Subjects, RestEvents):-
    contains_events1(local, [0|Rest], EventType, Subjects, RestEvents),!.
%
contains_events1(local, [A,B|Rest], EventType, Subjects, Events):-
    contains_events(A->B, _ListOfPaths, EventType, ABSubjects, ABEvents),
    contains_events1(local, [B|Rest], EventType, RestSubjects, RestEvents),
    conc(ABEvents, RestEvents, Events),
    conc(ABSubjects, RestSubjects, Subjects),!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


contains_events([], _, [], []).

contains_events([_], _, [], []).

% stop condition if subjects are specified
contains_events([_], _, _, []).

% when input state is included in selected statelist,
% all new interesting information in the first state
% should be considered an event too. Input state = 0.
%
contains_events([input|Rest], EventType, Subjects, RestEvents):-
    contains_events([0|Rest], EventType, Subjects, RestEvents),!.
%
% OR
%
% when input state is included in selected statelist,
% neglect it to determine events
% contains_events([input|Rest], EventType, Subj, RestEvents):-
%   contains_events(Rest, EventType, Subj, RestEvents),!.


% value and derivative events together
%
contains_events([A,B|Rest], val_der_events, Quantities, Events):-

%    findall(Event,
%            contains_der_event([A,B],
%            derivative_events, Event),
%            ABDerivativeEvents),
%    findall(Event,
%            contains_value_event([A,B],
%            value_events, Event),
%            ABValueEvents),

    % if subject quantities were given, check
    % whether the events apply to them
    % if not, skip the membership check
    (nonvar(Quantities) ->
       findall(Event,
           (
            member(Q, Quantities),
            contains_value_event([A,B],
              Val_and_Der_events, Q, Event)
           ),
           ABEvents),
       contains_events([B|Rest], val_der_events, Quantities, RestEvents),
       conc(ABEvents, RestEvents, Events),!
    ;
       findall(Event,
           (
            contains_value_event([A,B],
              Val_and_Der_events, Q, Event)
           ),
           ABEvents),
       % Make a list of all quantities involved in the events
       findall(Q,
            member(value_event(_T, Q, _V1, _D1, _Event, _V2, _D2), ABEvents),
            ABQuantities),
       contains_events([B|Rest], val_der_events, RestQuantities, RestEvents),
       conc(ABQuantities, RestQuantities, Quantities),
       conc(ABEvents, RestEvents, Events),!
    ).





% contains_events(Path, derivative_events, Quantities, Events)
%
% special clause for derivative events, derived from
% par_value events
%
%
contains_events([A,B|Rest], derivative_events, Quantities, Events):-
    % if subject quantities were given, check
    % whether the events apply to them
    % if not, skip the membership check
    (nonvar(Quantities) ->
       findall(Event,
           (
            member(Q, Quantities),
            contains_value_event([A,B],
              derivative_events, Q, Event)
           ),
           ABEvents),
    contains_events([B|Rest], derivative_events, Quantities, RestEvents),
    conc(ABEvents, RestEvents, Events),!
    ;
       findall(Event,
           (
            contains_value_event([A,B],
              derivative_events, Q, Event)
           ),
           ABEvents),
       % Make a list of all quantities involved in the events
       findall(Q,
            member(value_event(_T, Q, _V1, _D1, _Event, _V2, _D2), ABEvents),
            ABQuantities),
       contains_events([B|Rest], derivative_events, RestQuantities, RestEvents),
       conc(ABQuantities, RestQuantities, Quantities),
       conc(ABEvents, RestEvents, Events),!
    ).





/* this should be useless?
*/
% contains_events(Path, value_events, Events)
%
% special clause for value events, derived from
% par_value events
%
%
contains_events([A,B|Rest], value_events, Quantities, Events):-
    % if subject quantities were given, check
    % whether the events apply to them
    % if not, skip the membership check
    (nonvar(Quantities) ->
       findall(Event,
           (
            member(Q, Quantities),
            contains_value_event([A,B],
              value_events, Q, Event)
           ),
           ABEvents),
       contains_events([B|Rest], value_events, Quantities, RestEvents),
       conc(ABEvents, RestEvents, Events),!
    ;
       findall(Event,
           (
            contains_value_event([A,B],
              value_events, Q, Event)
           ),
           ABEvents),
       % Make a list of all quantities involved in the events
       findall(Q,
            member(value_event(_T, Q, _V1, _D1, _Event, _V2, _D2), ABEvents),
            ABQuantities),
       contains_events([B|Rest], value_events, RestQuantities, RestEvents),
       conc(ABQuantities, RestQuantities, Quantities),
       conc(ABEvents, RestEvents, Events),!
    ).







% contains_events(Path, causal_events, _, Events)
%
% special clause for causal events
%
%
contains_events([A,B|Rest], causal_events, _, Events):-
    findall(Event,
            contains_causal_event([A,B],
            causal_events, _CausalRel, Event),
            ABEvents),
    contains_events([B|Rest], causal_events, _, RestEvents),
    conc(ABEvents, RestEvents, Events),!.




% contains_events(Path, inequality_events, _, Events)
%
% special clause for (in)equality events, derived from
% par_relation events
%
%
contains_events([A,B|Rest], inequality_events, _, Events):-
    findall(Event,
            contains_inequality_event([A,B],
            inequality_events, Event),
            ABEvents),
    contains_events([B|Rest], inequality_events, _, RestEvents),
    conc(ABEvents, RestEvents, Events),!.


% contains_events(Path, corr_events, _, Events)
%
% special clause for correspondence events, derived from
% par_relation events
%
%
contains_events([A,B|Rest], corr_events, _, Events):-
    findall(Event,
            contains_corr_event([A,B],
            corr_events, Event),
            ABEvents),
    contains_events([B|Rest], corr_events, _, RestEvents),
    conc(ABEvents, RestEvents, Events),!.



% contains_events(Path, corr_calc_events, _, Events)
%
% special clause for the effects of value events
% propagated by correspondence calculations
%
contains_events([A,B|Rest], corr_calc_events, _, Events):-
    findall(Event,
            contains_corr_calculation([A,B],
            corr_calculation, Event),
            ABEvents),
    contains_events([B|Rest], corr_calc_events, _, RestEvents),
    conc(ABEvents, RestEvents, Events),!.


% contains_events(Path, mf_events, _, Events)
%
% special clause for model fragment events
%
%
contains_events([A,B|Rest], mf_events, _, Events):-
    findall(Event,
            contains_mf_event([A,B],
            mf_events, Event),
            ABEvents),
    contains_events([B|Rest], mf_events, _, RestEvents),
    conc(ABEvents, RestEvents, Events),!.



contains_events(StateList, dependency_events, _, Events):-
    contains_events(StateList, par_relations, _, Events),!.


% special clause for structure events. Events is
% the combination of system_element events
% (Entities and Relations) and Quantity events
%
contains_events(Path, structure_events, _, Events):-
    contains_events(Path, system_elements, _, EventsER),
    contains_events(Path, parameters, _, EventsQ),
    conc(EventsER, EventsQ, Events),!.


% all other types of events
%
% does not work with lists anymore - other code
% may need updating!
%
% special case: semi-transition from input 0->B
% neglect removal events
contains_events([0,B|Rest], EventType, _, Events):-
    compare_states_by_type(0, B, EventType,
                            [EventType/_AminB],
                            [EventType/BminA], _AandB),
    % findall(removed(0->B, EventType, Removed),
    %          member(Removed, AminB),
    %          RemovalEvents),
    findall(added(0->B, EventType, Added),
              member(Added, BminA),
              AdditionEvents),
    conc([], AdditionEvents, ABEvents),
    contains_events([B|Rest], EventType, _, RestEvents),
    conc(ABEvents, RestEvents, Events).
%
% all other transitions
%
contains_events([A,B|Rest], EventType, _, Events):-
    compare_states_by_type(A, B, EventType,
                            [EventType/AminB],
                            [EventType/BminA], _AandB),
    findall(removed(A->B, EventType, Removed),
              member(Removed, AminB),
              RemovalEvents),
    findall(added(A->B, EventType, Added),
              member(Added, BminA),
              AdditionEvents),
    conc(RemovalEvents, AdditionEvents, ABEvents),
    contains_events([B|Rest], EventType, _, RestEvents),
    conc(ABEvents, RestEvents, Events).


/*

removed(1->2, [mf1(x1,x2), mf2(x1), mf3(x4)])
added(1->2, [mf2(x1,x2), mf3(x1)])

removed(1->2, [
mf1(x1,x2),
mf2(x1),
mf3(x4)])

added(1->2, [
mf4(x1,x2),
mf3(x1),
])

Now, does mf3(x1) replace mf2(x1), or mf3(x4), or both?
*/


% contains_mf_event([A,B], mf_events, Event):-

/*
% special case for 'transition' between input state (0) and
% another state B
contains_mf_event([A,B], mf_events, Event):-
    A == 0,!,
    contains_events([A,B], system_structures, _,
       [removed(A->B, Removed), added(A->B, Added)]),
    % writef('Removed %p->%p: %p \n', [A, B, Removed]),
    % writef('Added   %p->%p: %p \n', [A, B, Added]),
    sort_mfs_by_variables(Removed, SortedRemoved),
    sort_mfs_by_variables(Added, SortedAdded),

    writef('Sorted Removed %p->%p: %p \n', [A, B, SortedRemoved]),
    writef('Sorted Added   %p->%p: %p \n', [A, B, SortedAdded]),

    mf_event([A,B], SortedRemoved, SortedAdded, Event).
*/


contains_mf_event([A,B], mf_events, Event):-
    compare_states_by_type(A, B, system_structures,
                            [system_structures/Removed],
                            [system_structures/Added], _Same),
    % writef('Removed %p->%p: %p \n', [A, B, Removed]),
    % writef('Added   %p->%p: %p \n', [A, B, Added]),

    sort(Added, SortedAdded),
    sort(Removed, SortedRemoved),
    sort_mfs_by_variables(SortedRemoved, AssortedRemoved),
    sort_mfs_by_variables(SortedAdded, AssortedAdded),

    % writef('Assorted Removed %p->%p: %p \n', [A, B, AssortedRemoved]),
    % writef('Assorted Added   %p->%p: %p \n', [A, B, AssortedAdded]),

    mf_event([A,B], AssortedRemoved, AssortedAdded, Event).


% replacement of a list of model fragments by another list
%
mf_event([A,B], SortedRemoved, SortedAdded, MFEvent):-
    member(mfs_with_same_variables(Variables, MFList1), SortedRemoved),
    member(mfs_with_same_variables(Variables, MFList2), SortedAdded),
    MFEvent = replaced(A->B, mf, MFList1, MFList2).
    % writef('MFEvent: %p \n', [MFEvent]).


% addition of a list of model fragments
%
mf_event([A,B], SortedRemoved, SortedAdded, MFEvent):-
    member(mfs_with_same_variables(Variables, MFList2), SortedAdded),
    not(member(mfs_with_same_variables(Variables, _MFList1), SortedRemoved)),
    MFEvent = added(A->B, mf, MFList2).
    % writef('MFEvent: %p \n', [MFEvent]).


% removal of a list of model fragments
%
mf_event([A,B], SortedRemoved, SortedAdded, MFEvent):-
    member(mfs_with_same_variables(Variables, MFList1), SortedRemoved),
    not(member(mfs_with_same_variables(Variables, _MFList2), SortedAdded)),
    MFEvent = removed(A->B, mf, MFList1).
    % writef('MFEvent: %p \n', [MFEvent]).



mfs_with_same_variables(MFList,
         mfs_with_same_variables(MFVariables, MFsWithSameVariables)):-
    member(MF, MFList),
    % writef('MF: %p \n', [MF]),
    MF =.. [_MFName|MFVariables],
    % writef('MF Variables: %p \n', [MFVariables]),
    findall(MF1, (member(MF1, MFList),
                  MF1 =.. [_MF1Name|MFVariables]
                 ), MFsWithSameVariables).


sort_mfs_by_variables([], []).

sort_mfs_by_variables(MFList, SortedMFList):-
    MFList \== [],
    % writef('MFList: %p \n', [MFList]),
    setof(MFsWithSameVariables,
            mfs_with_same_variables(MFList,
                                    MFsWithSameVariables),
            SortedMFList).



% explain_mf_event([A,B], added((A->B), mf, MF), ExplanationEventsTree):-
%
% Two options?
% 1. find all dependencies which have just become true: triggers
% 2. look at all depenencies, and find events which made them true
% For the moment, I only do option 1.
%
explain_mf_event([A,B], added((A->B), mf, MF), Triggers):-
    contains_events([A,B], system_structures, _,
		[removed(A->B, _RemovedMFs), added(A->B, AddedMFs)]),
    member(MF, AddedMFs),
    writef('\n Explaining addition of MF: %p \n', [MF]),

    % find conditions of MF in state B

    index_pred_state(B, system_structures,
      system_structures(MF, isa(SuperTypeMF),
			conditions(Conditions), givens(_Results))),

    write_events(Conditions),
    writef('Supertype MF: %p \n', [SuperTypeMF]),

    forall(member(ConditionsTerm, Conditions),
	  (ConditionsTerm =.. [Type, TypedConditions],
	   contains_events([A,B], Type, _,
		[removed(A->B, _Removed), added(A->B, Added)]),
	   findall(Condition,
	    (member(Condition, TypedConditions),
	     member(Condition, Added)
	    ),
	   TypedTriggers),
	   asserta(trigger_conditions(added((A->B), mf, MF), Type, TypedTriggers))
	  )
    ),


    findall(Type/TypedTriggers,
	    trigger_conditions(added((A->B), mf, MF), Type, TypedTriggers),
	    Triggers),
    writef('Trigger conditions: \n', []),
    write_events(Triggers),
    retractall(trigger_conditions(_, _, _)).


% explain_mf_event([A,B], removed((A->B), mf, MF), ExplanationEventsTree):-
%
% For removals, option 1 is the only meaningful option.
% 1. find all conditions which have just become false: triggers
%
explain_mf_event([A,B], removed((A->B), mf, MF), Triggers):-
    contains_events([A,B], system_structures, _,
		[removed(A->B, RemovedMFs), added(A->B, _AddedMFs)]),
    member(MF, RemovedMFs),
    writef('\n Explaining removal of MF: %p \n', [MF]),

    % find conditions of MF in state A

    index_pred_state(A, system_structures,
      system_structures(MF, isa(SuperTypeMF),
			conditions(Conditions), givens(_Results))),

    write_events(Conditions),
    writef('Supertype MF: %p \n', [SuperTypeMF]),

    forall(member(ConditionsTerm, Conditions),
	  (ConditionsTerm =.. [Type, TypedConditions],
	   contains_events([A,B], Type, _,
		[removed(A->B, Removed), added(A->B, _Added)]),
	   findall(Condition,
	    (member(Condition, TypedConditions),
	     member(Condition, Removed)
	    ),
	   TypedTriggers),
	   asserta(trigger_conditions(removed((A->B), mf, MF), Type, TypedTriggers))
	  )
    ),
    findall(Type/TypedTriggers,
	    trigger_conditions(removed((A->B), mf, MF), Type, TypedTriggers),
	    Triggers),
    writef('Trigger conditions: \n', []),
    write_events(Triggers),
    retractall(trigger_conditions(_, _, _)).



    % When looking at events involving the other conditions,
    % they should take place in A->B, or before!
    %
    % find events causing these conditions to become true or false












% Value events


% To do: look for passages, as Mallory does.
% look at QSpace to classify events as increase
% decrease, from interval to point and vice versa
% Specify value events without a derivative change,
% as Mallory does?
% otherwise it would be a derivative event

% contains_value_event([A,B], value_events, Q, Event):-

% special case for 'transition' between input state (0) and
% another state B
contains_value_event([0,B], value_events, Q, Event):-
    A = 0,
    % contains_events([A,B], par_values, _,
    %   [removed(A->B, _RemovedValues), added(A->B, AddedValues)]),
    % this should ideally be replaced by a procedure
    % that checks whether the value was already specified
    % in the input scenario or not. This is not trivial, because
    % the name for the quantity may not yet be instantiated in
    % the input state.
    % member(value(Q, _, Value1, _Der), RemovedValues),
    % member(value(Q, _, Value2, Der2), AddedValues),
    index_pred_state(B, par_values, value(Q, _Real, Value2, Der2)),
    value_event(_Value1, Value2, ValueChange),
    Event = value_event(A->B, Q, _, _, ValueChange, Value2, Der2).



contains_value_event([A,B], value_events, Q, Event):-
    % contains_events([A,B], par_values, _,
    %   [removed(A->B, RemovedValues), added(A->B, AddedValues)]),
    % member(value(Q, _, Value1, Der1), RemovedValues),
    % member(value(Q, _, Value2, Der2), AddedValues),
    index_pred_state(A, par_values, value(Q, _Real, Value1, Der1)),
    index_pred_state(B, par_values, value(Q, _Real2, Value2, Der2)),
    value_event(Value1, Value2, ValueChange),
    Event = value_event(A->B, Q, Value1, Der1, ValueChange, Value2, Der2).

/*
% This may not be necessary anymore!
% special case for when both? derivatives are unknown
%
contains_value_event([A,B], value_events, Q, Event):-
    % contains_events([A,B], par_values, _,
    %    [removed(A->B, RemovedValues), added(A->B, AddedValues)]),
    % member(value(Q, _, Value1, UnknownDer), RemovedValues),

    index_pred_state(A, par_values, value(Q, _Real, Value1, UnknownDer)),
    index_pred_state(B, par_values, value(Q, _Real2, Value2, Der2)),

    var(UnknownDer),
    writef('Der: %p, Value1: %p \n', [UnknownDer, Value1]),
    % member(value(Q, _, Value2, Der2), AddedValues),
    value_event(Value1, Value2, ValueChange),
    Event = value_event(A->B, Q, Value1, UnknownDer, ValueChange, Value2, Der2).
*/



% Derivative events

% contains_value_event([A,B], derivative_events, Events):-
% special case for 'transition' between input state (0) and
% another state B
contains_value_event([0,B], derivative_events, Q, Event):-
    A = 0,
    % contains_events([A,B], par_values, _,
    %    [removed(A->B, _RemovedValues), added(A->B, AddedValues)]),
    % this should ideally be replaced by a procedure
    % that checks whether the derivative was already specified
    % in the input scenario or not. This is not trivial, because
    % the name for the quantity may not yet be instantiated in
    % the input state.
    % member(value(Q, _, Value1, DerA), RemovedValues),
    % member(value(Q, _, Value2, DerB), AddedValues),
    % for now, we assume it was zero, so any change
    % will be recorded as a start to in/decrease
    index_pred_state(B, par_values, value(Q, _Real, Value2, Der2)),
    Der1 = assumed_zero,
    derivative_event(Der1, Der2, Value2, DerChange),
    Event = value_event(A->B, Q, _Value1, _Der1, DerChange, Value2, Der2).
    % write_ln(derivative_change(A->B, Q, Der1->Der2)).


contains_value_event([A,B], derivative_events, Q, Event):-
    % contains_events([A,B], par_values, _,
    %   [removed(A->B, RemovedValues), added(A->B, AddedValues)]),
    % member(value(Q, _, Value1, DerA), RemovedValues),
    % member(value(Q, _, Value2, DerB), AddedValues),
    index_pred_state(A, par_values, value(Q, _Real, Value1, Der1)),
    index_pred_state(B, par_values, value(Q, _Real2, Value2, Der2)),
    derivative_event(Der1, Der2, Value2, DerChange),
    Event = value_event(A->B, Q, Value1, Der1, DerChange, Value2, Der2).
    % write_ln(derivative_change(A->B, Q, Der1->Der2)).



% special case for transition from 0->1
%
% derivative stays unknown - no event, so fail!
derivative_event(assumed_zero, UnknownDer2, _Value, _NoEvent):-
    var(UnknownDer2),!,
    fail.
derivative_event(assumed_zero, plus, Value, start_to_increase_from(Value)):-!.
derivative_event(assumed_zero, min, Value, start_to_decrease_from(Value)):-!.
derivative_event(assumed_zero, _Der2, _Value, _NoEvent):-!,
    fail.


% if variables are given as input, this results in special
% types of events:
%
% derivative becomes unknown
derivative_event(Der1, UnknownDer2, Value, der_becomes_unknown_at(Value)):-
    nonvar(Der1),
    Der1 \== assumed_zero,
    var(UnknownDer2),!.

% derivative becomes known
derivative_event(UnknownDer1, Der2, Value, der_becomes_known_at(Der2, Value)):-
    nonvar(Der2),
    var(UnknownDer1),!.

% derivative stays unknown - no event, so fail!
derivative_event(UnknownDer1, UnknownDer2, _Value, der_stays_unknown):-
    var(UnknownDer1),
    var(UnknownDer2),!,
    fail.




%
% normal cases of a derivative changing from one known
% derivative value to another
%
derivative_event(zero, plus, Value, start_to_increase_from(Value)):-!.
derivative_event(zero, min, Value, start_to_decrease_from(Value)):-!.
derivative_event(plus, zero, Value, increase_stops_at(Value)):-!.
derivative_event(min, zero, Value, decrease_stops_at(Value)):-!.
derivative_event(X, Y, _Value, der_change(X->Y)):-
    X \== Y,!.
% derivative_event(X, Y, _Value, no_change(X->Y)).









% if variables are given as input, this results in special
% types of events:
%
% value becomes unknown
value_event(Value1, UnknownValue2, becomes_unknown):-
    not(unknown_value(Value1)),
    unknown_value(UnknownValue2),!.

% value becomes known
value_event(UnknownValue1, Value2, becomes_known(Value2)):-
    not(unknown_value(Value2)),
    unknown_value(UnknownValue1),!.

% value stays unknown - no event, so fail!
value_event(UnknownValue1, UnknownValue2, stays_unknown):-
    unknown_value(UnknownValue1),
    unknown_value(UnknownValue2),!,
    fail.

% normal cases of a value changing from one known
% value value to another
%
value_event(X, Y, value_change(X->Y)):-
    X \== Y,!.
% value_event(X, Y, no_change(X->Y)).


unknown_value(Value):-
    var(Value).

unknown_value(unk).

unknown_value(unknown).

unknown_value('?').

unknown_value(?).








% inequality events

% contains_inequality_event([A,B], inequality_events, Events):-

% special case for 'transition' between input state (0) and
% another state B
contains_inequality_event([A,B], inequality_events, Event):-
    A == 0,!,
    compare_states_by_type(A, B, par_relations,
                            [par_relations/_RemovedRels],
                            [par_relations/AddedRels], _SameRels),
    inequality_relation_member(AddedRels, InequalityRel),
    % for now, we assume any inequality relation is introduced
    Event = inequality_introduced((A->B), InequalityRel).


contains_inequality_event([A,B], inequality_events, Event):-
    compare_states_by_type(A, B, par_relations,
                            [par_relations/RemovedRels],
                            [par_relations/AddedRels], _SameRels),
    inequality_relation_member(RemovedRels, InequalityRelA),
    inequality_relation_member(AddedRels, InequalityRelB),
    inequality_event(InequalityRelA, InequalityRelB, InequalityEvent),
    Event = inequality_event((A->B), InequalityEvent).



contains_inequality_event([A,B], inequality_events, Event):-
    compare_states_by_type(A, B, par_relations,
                            [par_relations/RemovedRels],
                            [par_relations/AddedRels], _SameRels),
    inequality_relation_member(RemovedRels, InequalityRelA),
    % if there is no pair RelA & RelB which makes an event,
    % then RelA has been removed
    not(
      (inequality_relation_member(AddedRels, InequalityRelB),
       inequality_event(InequalityRelA, InequalityRelB, _Event))
    ),
    Event = inequality_removed((A->B), InequalityRelA).



contains_inequality_event([A,B], inequality_events, Event):-
    compare_states_by_type(A, B, par_relations,
                            [par_relations/RemovedRels],
                            [par_relations/AddedRels], _SameRels),
    inequality_relation_member(AddedRels, InequalityRelB),
    % if there is no pair RelA & RelB which makes an event,
    % then RelB was newly introduced
    not(
      (inequality_relation_member(RemovedRels, InequalityRelA),
       inequality_event(InequalityRelA, InequalityRelB, _Event))
    ),
    Event = inequality_introduced((A->B), InequalityRelB).







% inequality relationships between values
%
inequality_relation_member(ParRels, equal(X, Y)):-
      member(equal(X, Y), ParRels).
inequality_relation_member(ParRels, greater(X, Y)):-
      member(greater(X, Y), ParRels).
inequality_relation_member(ParRels, smaller(X, Y)):-
      member(smaller(X, Y), ParRels).
inequality_relation_member(ParRels, greater_or_equal(X, Y)):-
      member(greater_or_equal(X, Y), ParRels).
inequality_relation_member(ParRels, smaller_or_equal(X, Y)):-
      member(smaller_or_equal(X, Y), ParRels).


% idem for inequality relationships between derivatives
%
inequality_relation_member(ParRels, d_equal(X, Y)):-
      member(d_equal(X, Y), ParRels).
inequality_relation_member(ParRels, d_greater(X, Y)):-
      member(d_greater(X, Y), ParRels).
inequality_relation_member(ParRels, d_smaller(X, Y)):-
      member(d_smaller(X, Y), ParRels).
inequality_relation_member(ParRels, d_greater_or_equal(X, Y)):-
      member(d_greater_or_equal(X, Y), ParRels).
inequality_relation_member(ParRels, d_smaller(X, Y)):-
      member(d_smaller_or_equal(X, Y), ParRels).




% if variables are given as input, this results in special
% types of events:
%
% inequalities between values
%
inequality_event(equal(X,Y), greater(X,Y),
    between_val(from(X, =, Y), to(X, >, Y))):-!.

inequality_event(equal(X,Y), greater_or_equal(X,Y),
    between_val(from(X, =, Y), to(X, >=, Y))):-!.

inequality_event(equal(X,Y), smaller(X,Y),
    between_val(from(X, =, Y), to(X, <, Y))):-!.

inequality_event(equal(X,Y), smaller_or_equal(X,Y),
    between_val(from(X, =, Y), to(X, <=, Y))):-!.


inequality_event(greater(X,Y), equal(X,Y),
    between_val(from(X, >, Y), to(X, =, Y))):-!.

inequality_event(greater(X,Y), greater_or_equal(X,Y),
    between_val(from(X, >, Y), to(X, >=, Y))):-!.

inequality_event(greater(X,Y), smaller_or_equal(X,Y),
    between_val(from(X, >, Y), to(X, <=, Y))):-!.


inequality_event(greater_or_equal(X,Y), equal(X,Y),
    between_val(from(X, >=, Y), to(X, =, Y))):-!.

inequality_event(greater_or_equal(X,Y), greater(X,Y),
    between_val(from(X, >=, Y), to(X, >, Y))):-!.

inequality_event(greater_or_equal(X,Y), smaller(X,Y),
    between_val(from(X, >=, Y), to(X, <, Y))):-!.



inequality_event(smaller(X,Y), equal(X,Y),
    between_val(from(X, <, Y), to(X, =, Y))):-!.

inequality_event(smaller(X,Y), smaller_or_equal(X,Y),
    between_val(from(X, <, Y), to(X, <=, Y))):-!.

inequality_event(smaller(X,Y), greater_or_equal(X,Y),
    between_val(from(X, <, Y), to(X, >=, Y))):-!.



inequality_event(smaller_or_equal(X,Y), equal(X,Y),
    between_val(from(X, <=, Y), to(X, =, Y))):-!.

inequality_event(smaller_or_equal(X,Y), smaller(X,Y),
    between_val(from(X, <=, Y), to(X, <, Y))):-!.

inequality_event(smaller_or_equal(X,Y), greater(X,Y),
    between_val(from(X, <=, Y), to(X, >, Y))):-!.



% inequalities between derivatives
inequality_event(d_equal(X,Y), d_greater(X,Y),
    between_der(from(X, =, Y), to(X, >, Y))):-!.

inequality_event(d_equal(X,Y), d_greater_or_equal(X,Y),
    between_der(from(X, =, Y), to(X, >=, Y))):-!.

inequality_event(d_equal(X,Y), d_smaller(X,Y),
    between_der(from(X, =, Y), to(X, <, Y))):-!.

inequality_event(d_equal(X,Y), d_smaller_or_equal(X,Y),
    between_der(from(X, =, Y), to(X, <=, Y))):-!.


inequality_event(d_greater(X,Y), d_equal(X,Y),
    between_der(from(X, >, Y), to(X, =, Y))):-!.

inequality_event(d_greater(X,Y), d_greater_or_equal(X,Y),
    between_der(from(X, >, Y), to(X, >=, Y))):-!.

inequality_event(d_greater(X,Y), d_smaller_or_equal(X,Y),
    between_der(from(X, >, Y), to(X, <=, Y))):-!.


inequality_event(d_greater_or_equal(X,Y), d_equal(X,Y),
    between_der(from(X, >=, Y), to(X, =, Y))):-!.

inequality_event(d_greater_or_equal(X,Y), d_greater(X,Y),
    between_der(from(X, >=, Y), to(X, >, Y))):-!.

inequality_event(d_greater_or_equal(X,Y), d_smaller(X,Y),
    between_der(from(X, >=, Y), to(X, <, Y))):-!.


inequality_event(d_smaller(X,Y), d_equal(X,Y),
    between_der(from(X, <, Y), to(X, =, Y))):-!.

inequality_event(d_smaller(X,Y), d_smaller_or_equal(X,Y),
    between_der(from(X, <, Y), to(X, <=, Y))):-!.

inequality_event(d_smaller(X,Y), d_greater_or_equal(X,Y),
    between_der(from(X, <, Y), to(X, >=, Y))):-!.


inequality_event(d_smaller_or_equal(X,Y), d_equal(X,Y),
    between_der(from(X, <=, Y), to(X, =, Y))):-!.

inequality_event(d_smaller_or_equal(X,Y), d_smaller(X,Y),
    between_der(from(X, <=, Y), to(X, <, Y))):-!.

inequality_event(d_smaller_or_equal(X,Y), d_greater(X,Y),
    between_der(from(X, <=, Y), to(X, >, Y))):-!.


% rest case
inequality_event(RelA, RelB, unknown_inequality_event(RelA, RelB)):-
       RelA =.. [Rel1, X, Y],
       RelB =.. [Rel2, X, Y],
       Rel1 \== Rel2,!.

% Examples of unknown inequality events.
% These examples should not be considered events, in my opinion,
% but if some unforeseen combination should be, it should
% be added to the list above. When the test phase is over,
% this clause can be deleted.
%
% inequality_event((14->15), unknown_inequality_event(greater(number_of3, zero), d_equal(number_of3, zero)))
% inequality_event((14->15), unknown_inequality_event(d_smaller(born3, zero), equal(born3, zero)))
% inequality_event((14->15), unknown_inequality_event(d_smaller(dead3, zero), equal(dead3, zero)))
% inequality_event((14->15), unknown_inequality_event(d_smaller(number_of3, zero), equal(number_of3, zero)))






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Correspondence events
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% contains_corr_event([A,B], corr_events, Events):-
%
% for the correspondences we only distinguish
% two kinds of events: corr_introduced and corr_removed
%
contains_corr_event([A,B], corr_events, Event):-
    compare_states_by_type(A, B, par_relations,
                            [par_relations/_RemovedRels],
                            [par_relations/AddedRels], _SameRels),
    corr_relation_member(AddedRels, CorrRel),
    % for now, we assume any corr relation is introduced,
    % also for the begin states
    Event = corr_introduced((A->B), CorrRel).


contains_corr_event([A,B], corr_events, Event):-
    compare_states_by_type(A, B, par_relations,
                            [par_relations/RemovedRels],
                            [par_relations/_AddedRels], _SameRels),
    corr_relation_member(RemovedRels, CorrRelA),
    Event = corr_removed((A->B), CorrRelA).



% correspondence events by comparing two correspondence
% relationships doesn't really make sense; only introduce
% and remove correspondence should be considered events
% (maybe transformation of a directed to an undirected
% correspondence could be considered an event, but this
% is neglected for now).



corr_relation_member(ParRels, v_correspondence(Q1, V1, Q2, V2)):-
      member(v_correspondence(Q1, V1, Q2, V2), ParRels).
corr_relation_member(ParRels, dir_v_correspondence(Q1, V1, Q2, V2)):-
      member(dir_v_correspondence(Q1, V1, Q2, V2), ParRels).
corr_relation_member(ParRels, q_correspondence(Q1, Q2)):-
      member(q_correspondence(Q1, Q2), ParRels).
corr_relation_member(ParRels, dir_q_correspondence(Q1, Q2)):-
      member(dir_q_correspondence(Q1, Q2), ParRels).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% When is a correspondence active within a state?
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% q_correspondence from Q1 to Q2
%
contains_corr_calculation(A, corr_calculation, corr_calc(A, Q1, V1, q_correspondence(Q2, Q1), Q2, V2)):-
    index_pred_state(A, par_relations, q_correspondence(Q2, Q1)),
    index_pred_state(A, par_values, value(Q1, _RealVal1, V1, _D1)),
    index_pred_state(A, par_values, value(Q2, _RealVal2, V2, _D2)).


% q_correspondence from Q2 to Q1 (undirected can be considered bidirectional)
%
contains_corr_calculation(A, corr_calculation, corr_calc(A, Q1, V1, q_corr_rev(Q1, Q2), Q2, V2)):-
    index_pred_state(A, par_relations, q_correspondence(Q1, Q2)),
    index_pred_state(A, par_values, value(Q1, _RealVal1, V1, _D1)),
    index_pred_state(A, par_values, value(Q2, _RealVal2, V2, _D2)).


% dir_q_correspondence from Q1 to Q2
%
contains_corr_calculation(A, corr_calculation, corr_calc(A, Q1, V1, dir_q_correspondence(Q2, Q1), Q2, V2)):-
    index_pred_state(A, par_relations, dir_q_correspondence(Q2, Q1)),
    index_pred_state(A, par_values, value(Q1, _RealVal1, V1, _D1)),
    index_pred_state(A, par_values, value(Q2, _RealVal2, V2, _D2)).


% v_correspondence from Q1 to Q2
%
contains_corr_calculation(A, corr_calculation, corr_calc(A, Q1, V1, v_correspondence(Q2, V2, Q1, V1), Q2, V2)):-
    index_pred_state(A, par_relations, v_correspondence(Q2, V2, Q1, V1)),
    index_pred_state(A, par_values, value(Q1, _RealVal1, V1, _D1)),
    index_pred_state(A, par_values, value(Q2, _RealVal2, V2, _D2)).


% v_correspondence from Q2 to Q1 (undirected can be considered bidirectional)
%
contains_corr_calculation(A, corr_calculation, corr_calc(A, Q1, V1, v_corr_rev(Q1, V1, Q2, V2), Q2, V2)):-
    index_pred_state(A, par_relations, v_correspondence(Q1, V1, Q2, V2)),
    index_pred_state(A, par_values, value(Q1, _RealVal1, V1, _D1)),
    index_pred_state(A, par_values, value(Q2, _RealVal2, V2, _D2)).


% dir_v_correspondence from Q1 to Q2
%
contains_corr_calculation(A, corr_calculation, corr_calc(A, Q1, V1, dir_v_correspondence(Q2, V2, Q1, V1), Q2, V2)):-
    index_pred_state(A, par_relations, dir_v_correspondence(Q2, V2, Q1, V1)),
    index_pred_state(A, par_values, value(Q1, _RealVal1, V1, _D1)),
    index_pred_state(A, par_values, value(Q2, _RealVal2, V2, _D2)).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% The effects of calculating via correspondences
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% q_correspondence from Q1 to Q2
%
contains_corr_calculation([A,B], corr_calculation, corr_calc((A->B), Q1, E1, q_correspondence(Q2, Q1), Q2, E2)):-
    index_pred_state(B, par_relations, q_correspondence(Q2, Q1)),
    % value event E1 happening to Q1
    % value event E2 happening to Q2
    contains_value_event([A,B], value_events, Q1, value_event((A->B), Q1, _V1a, _D1a, E1, _V1b, _D1b)),
    contains_value_event([A,B], value_events, Q2, value_event((A->B), Q2, _V2a, _D2a, E2, _V2b, _D2b)).


% q_correspondence from Q2 to Q1 (undirected can be considered bidirectional)
%
contains_corr_calculation([A,B], corr_calculation, corr_calc((A->B), Q1, E1, q_corr_rev(Q1, Q2), Q2, E2)):-
    index_pred_state(B, par_relations, q_correspondence(Q1, Q2)),
    % value event E1 happening to Q1
    % value event E2 happening to Q2
    contains_value_event([A,B], value_events, Q1, value_event((A->B), Q1, _V1a, _D1a, E1, _V1b, _D1b)),
    contains_value_event([A,B], value_events, Q2, value_event((A->B), Q2, _V2a, _D2a, E2, _V2b, _D2b)).



% dir_q_correspondence from Q1 to Q2
%
contains_corr_calculation([A,B], corr_calculation, corr_calc((A->B), Q1, E1, dir_q_correspondence(Q2, Q1), Q2, E2)):-
    index_pred_state(B, par_relations, dir_q_correspondence(Q2, Q1)),
    % value event E1 happening to Q1
    % value event E2 happening to Q2
    contains_value_event([A,B], value_events, Q1, value_event((A->B), Q1, _V1a, _D1a, E1, _V1b, _D1b)),
    contains_value_event([A,B], value_events, Q2, value_event((A->B), Q2, _V2a, _D2a, E2, _V2b, _D2b)).



% v_correspondence from Q1 to Q2
%
contains_corr_calculation([A,B], corr_calculation, corr_calc((A->B), Q1, E1, v_correspondence(Q2, V2b, Q1, V1b), Q2, E2)):-
    index_pred_state(B, par_relations, v_correspondence(Q2, V2b, Q1, V1b)),
    % value event E1 happening to Q1
    % value event E2 happening to Q2
    contains_value_event([A,B], value_events, Q1, value_event((A->B), Q1, _V1a, _D1a, E1, V1b, _D1b)),
    contains_value_event([A,B], value_events, Q2, value_event((A->B), Q2, _V2a, _D2a, E2, V2b, _D2b)).


% v_correspondence from Q2 to Q1 (undirected can be considered bidirectional)
%
contains_corr_calculation([A,B], corr_calculation, corr_calc((A->B), Q1, E1, v_corr_rev(Q1, V1b, Q2, V2b), Q2, E2)):-
    index_pred_state(B, par_relations, v_correspondence(Q1, V1b, Q2, V2b)),
    % value event E1 happening to Q1
    % value event E2 happening to Q2
    contains_value_event([A,B], value_events, Q1, value_event((A->B), Q1, _V1a, _D1a, E1, V1b, _D1b)),
    contains_value_event([A,B], value_events, Q2, value_event((A->B), Q2, _V2a, _D2a, E2, V2b, _D2b)).



% dir_v_correspondence from Q1 to Q2
%
contains_corr_calculation([A,B], corr_calculation, corr_calc((A->B), Q1, E1, dir_v_correspondence(Q2, V2b, Q1, V1b), Q2, E2)):-
    index_pred_state(B, par_relations, dir_v_correspondence(Q2, V2b, Q1, V1b)),
    % member(q_correspondence(Q1, Q2), ParRels),
    % value event E1 happening to Q1
    % value event E2 happening to Q2
    contains_value_event([A,B], value_events, Q1, value_event((A->B), Q1, _V1a, _D1a, E1, V1b, _D1b)),
    contains_value_event([A,B], value_events, Q2, value_event((A->B), Q2, _V2a, _D2a, E2, V2b, _D2b)).






% introduction of a correspondence triggers a value event in Q2
%

% q_correspondence from Q1 to Q2
%
contains_corr_calculation([A,B], corr_calculation, corr_calc((A->B), Q1, corr_introduced((A->B), CorrRel), CorrRel, Q2, E2)):-
    contains_corr_event([A,B], corr_events, corr_introduced((A->B), CorrRel)),
    % writef('corr intro calc %p->%p: %p \n', [A, B, CorrRel]),
    (
     CorrRel = q_correspondence(Q2, Q1)
     ;
     CorrRel = q_correspondence(Q1, Q2)
     ;
     CorrRel = dir_q_correspondence(Q2, Q1)
     ;
     CorrRel = v_correspondence(Q2, V2b, Q1, V1b)
     ;
     CorrRel = v_correspondence(Q1, V1b, Q2, V2b)
     ;
     CorrRel = dir_v_correspondence(Q2, V2b, Q1, V1b)
    ),
    % writef('corr intro calc Q1: %p  Q2: %p \n', [Q1, Q2]),
    % value of Q1 in state B. There does not need to be an event,
    % because in those cases the event will already trigger - this
    % may even create doubles, so maybe these cases should be excluded
    % here (or there)!
    index_pred_state(B, par_values, value(Q1, _RealVal, V1b, _D1b)),
    % value event E2 happening to Q2
    contains_value_event([A,B], value_events, Q2, value_event((A->B), Q2, _V2a, _D2a, E2, V2b, _D2b)).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% The effects of value events & calculating inequalities
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% To Do!

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Causal events
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% contains_causal_event([A,B], causal_events, CausalRel, Events):-

% special case for 'transition' between input state (0) and
% another state B
contains_causal_event([A,B], causal_events, causal_relation(Q1, R, Q2), Event):-
    A == 0,!,
    % contains_events([A,B], causal_effects, _,
    %   [removed(A->B, _Removed), added(A->B, Added)]),
    % this should ideally be replaced by a procedure
    % that checks whether the derivative was already specified
    % in the input scenario or not. This is not trivial, because
    % the name for the quantity may not yet be instantiated in
    % the input state.
    % member(effect_status(causal_relation(Q1, R, Q2), Dir, EffStatus), Removed),
    % member(effect_status(causal_relation(Q1, R, Q2), Dir, EffStatus), Added),
    effect_status(B, causal_relation(Q1, R, Q2), Dir, EffStatus),
    % for now, we assume there was no causal relation so it
    % will be recorded as a newly introduced causal relation
    Event = introduced(A->B, effect_status(causal_relation(Q1,R,Q2),
                        Dir, EffStatus)).



% contains_causal_event([A,B], causal_events, CausalRel, Event):-
%
% succeeds when there is an Event happening to CausalRel in A->B
%
%
contains_causal_event([A,B], causal_events, causal_relation(Q1, R, Q2), Event):-
    effect_status(A, causal_relation(Q1, R, Q2), DirA, EffA),
    check_causal_event([A,B], causal_relation(Q1, R, Q2), DirA, EffA, Event).



% special case for newly introduced causal effects
%
contains_causal_event([A,B], causal_events, causal_relation(Q1, R, Q2), Event):-
    effect_status(B, causal_relation(Q1, R, Q2), DirB, EffB),
    not(effect_status(A, causal_relation(Q1, R, Q2), _DA, _EA)),
    Event = introduced(A->B, effect_status(causal_relation(Q1,R,Q2), DirB, EffB)).

/*
% This one is not necessary - taken care of by check_causal_event
%
% special case for disappearing causal effects
%
contains_causal_event([A,B], causal_events, causal_relation(Q1, R, Q2), Event):-
    effect_status(A, causal_relation(Q1, R, Q2), DirA, EffA),
    not(effect_status(B, causal_relation(Q1, R, Q2), _DB, _EffB)),
    Event = removed(A->B, effect_status(causal_relation(Q1,R,Q2), DirA, EffA)).
*/



% causal relation has changed in status from state A to B
%
check_causal_event([A,B], causal_relation(Q1, R, Q2), DirA, EffA, Event):-
     effect_status(B, causal_relation(Q1, R, Q2), DirB, EffB),!,
     (same_effect(DirA, EffA, DirB, EffB), !, fail
     ;
     Event = changed(A->B, effect_status(causal_relation(Q1,R,Q2),
        from(DirA, EffA), to(DirB, EffB)))
     ).


% final case: causal relation must be removed in state B
%
check_causal_event([A,B], causal_relation(Q1, R, Q2), DirA, EffA, Event):-
     % not(effect_status(B, causal_relation(Q1, R, Q2), _DB, _EffB)),
     Event = removed(A->B, effect_status(causal_relation(Q1,R,Q2), DirA, EffA)).




% same_effect(DirA, EffA, DirB, EffB)
%
% only returns true if both Dir and Eff are the same for A and B.
%
same_effect(Dir, Eff, Dir, Eff).


% This is the old version, without the CausalRel as subject
%
contains_causal_event([A,B], causal_events, Event):-
    effect_status(A, causal_relation(Q1, R, Q2), DirA, EffA),
    effect_status(B, causal_relation(Q1, R, Q2), DirB, EffB),
    not(same_effect(DirA, EffA, DirB, EffB)),
    % old version
    % EffA \== EffB,
    Event = changed(A->B, effect_status(causal_relation(Q1,R,Q2),
                     from(DirA, EffA), to(DirB, EffB))).


% special case for disappearing causal effects
%
contains_causal_event([A,B], causal_events, Event):-
    effect_status(A, causal_relation(Q1, R, Q2), DirA, EffA),
    not(effect_status(B, causal_relation(Q1, R, Q2), _DB, _EffB)),
    Event = removed(A->B, effect_status(causal_relation(Q1,R,Q2), DirA, EffA)).


% special case for newly introduced causal effects
%
contains_causal_event([A,B], causal_events, Event):-
    effect_status(B, causal_relation(Q1, R, Q2), DirB, EffB),
    not(effect_status(A, causal_relation(Q1, R, Q2), _DA, _EA)),
    Event = introduced(A->B, effect_status(causal_relation(Q1,R,Q2), DirB, EffB)).


% record_events
%
% records local level events of all types
% for all transitions
%
record_events:-
    forall(rec_transition(A, B),
	   record_events(A->B, [[A,B]])
    ).


% record_events(A->B, ListOfPaths):-
% records local level events of all types
% for a transition A->B (aggregrated or not)
%
% special case for input state
record_events(input->B, ListOfPaths):-
    record_events(0->B, ListOfPaths).
%
record_events(A->B, ListOfPaths):-
    record_events_type(A->B, ListOfPaths, structure_events),
    record_events_type(A->B, ListOfPaths, value_events),
    record_events_type(A->B, ListOfPaths, derivative_events),
    record_events_type(A->B, ListOfPaths, val_der_events),
    record_events_type(A->B, ListOfPaths, inequality_events),
    record_events_type(A->B, ListOfPaths, dependency_events),
    record_events_type(A->B, ListOfPaths, causal_events),
    record_events_type(A->B, ListOfPaths, corr_calc_events),
    record_events_type(A->B, ListOfPaths, mf_events).



% record_events_type(A->B, ListOfPaths, EventType)
%
% records local level events of a type EventType
% for one transition A->B, in which case ListOfPaths is [[A,B]],
% or an aggregated transition, in which case ListOfPaths contains
% all paths aggregated into the transition
%
record_events_type(A->B, [Path1|ListOfPaths], EventType):-
    % all paths contain the same events, so just look at the first
    findall(EventsAB,
            (pair_member(X/Y, Path1),
              contains_events(X->Y, ListOfPathsXY, EventType, SubjectsAB, EventsAB)
%             this only works for real transitions, not abstracted
%             contains_events([X,Y], EventType, SubjectsAB, EventsAB)
            ), EventsAll
    ),
    findall(SubjectsAB,
            (pair_member(X/Y, Path1),
              contains_events(X->Y, ListOfPathsXY, EventType, SubjectsAB, EventsAB)
%             this only works for real transitions, not abstracted
%             contains_events([X,Y], EventType, SubjectsAB, EventsAB)
            ), SubjectsAll
    ),
    flatten(EventsAll, Events),
    flatten(SubjectsAll, Subjects),
    asserta(rec(contains_events(A->B, [Path1|ListOfPaths],
                            EventType, Subjects, Events))).


% if events are known (asserted) already, return them
%
contains_events(A->B, ListOfPaths, EventType, Subjects, Events):-
    rec(contains_events(A->B, ListOfPaths, EventType, Subjects, Events)),!.


% if events are not known (asserted) yet, record them first, and try again.
%
contains_events(A->B, ListOfPaths, EventType, Subjects, Events):-
    (
      var(ListOfPaths),
      ListOfPaths = [[A,B]]
    ;
      % nonvar(ListOfPaths),
      true
    ),

%   Nothing is recorded, so determine the low level events
    contains_events([A,B], EventType, Subjects, Events),
%   assert the events for future use
    asserta(rec(contains_events(A->B, ListOfPaths,
                         EventType, Subjects, Events))),!.

%   This old version creates a loop
%    record_events_type(A->B, ListOfPaths, EventType),
%    rec(contains_events(A->B, ListOfPaths, EventType, Subjects, Events)),!.






% the blank variable is
% for the subjects of the events
test_events(Path, EventType):-
    contains_events(Path, EventType, _Subjects, Events),
    write_events(Events).


test_events(Path, EventType, Subjects):-
    contains_events(Path, EventType, Subjects, Events),
    write_events(Events).

% test_results:-
%
% a procedure to test the results of the aggregation techniques
% by giving some quantitative data about the simulation before
% and after aggregation
%
test_results:-
    reset_aggregation,
    OriginalGraph = state_graph(original, input),
    Graph = state_graph(aggregated, input),
    % # States
    findall(N, engine:state(N,_), L), length(L, NrStates),
    %
    % # Full Paths
    findall(Path, full_path(_A, _B, OriginalGraph, Path, _Length), Paths),
    length(Paths, NrPaths),

    % # Facts
    findall(N/Fact, index_pred_state(N, system_elements, Fact), FactsStructure),
    length(FactsStructure, NrFactsStructure),

    findall(N/Fact, index_pred_state(N, parameters, Fact), FactsQuantities),
    length(FactsQuantities, NrFactsQuantities),

    findall(N/Fact, index_pred_state(N, par_values, Fact), FactsValDer),
    length(FactsValDer, NrFactsValDer),

    findall(N/Fact, index_pred_state(N, par_relations, Fact), FactsDependencies),
    length(FactsDependencies, NrFactsDependencies),

    findall(N/Fact, (graph_node(OriginalGraph, N),
                     index_pred_state(N, causal_effects, Fact)
                    ), FactsCausalEffects),
    length(FactsCausalEffects, NrFactsCausalEffects),

    findall(N/Fact, index_pred_state(N, system_structures, Fact), FactsModelFragments),
    length(FactsModelFragments, NrFactsModelFragments),


    findall(_Transition, graph_edge(OriginalGraph, _A2, _B2, to), Transitions),
    length(Transitions, NrTransitions),

    % Path Segments of maximum length in original graph
    findall(PathSegment,
            max_path_segment(A, B, OriginalGraph, PathSegment, _Length2), Segments),
    length(Segments, NrSegments),

    writef("Before aggregation:  \n", []),
    writef("# states: %p \n", [NrStates]),
    writef("# transitions: %p \n", [NrTransitions]),
    writef("# paths:  %p \n", [NrPaths]),
    writef("# path segments:  %p \n", [NrSegments]),

    NrStructuralFacts is NrFactsStructure + NrFactsQuantities,
    NrFacts is NrStructuralFacts + NrFactsValDer + NrFactsDependencies + NrFactsModelFragments,

    writef("# structure (E,R) facts:  %p \n", [NrFactsStructure]),
    writef("# quantities facts:  %p \n", [NrFactsQuantities]),
    writef("# structural (E,R,Q) facts:  %p \n", [NrStructuralFacts]),
    writef("# value/derivative facts:  %p \n", [NrFactsValDer]),
    writef("# dependencies facts:  %p \n", [NrFactsDependencies]),
    writef("# model fragment facts:  %p \n", [NrFactsModelFragments]),

    writef("# facts in total:  %p \n", [NrFacts]),

    writef("After aggregation on level 1 (system state level):  \n", []),

    findall(effect_status(N, Rel, Eff, effect),
            (graph_node(OriginalGraph, N),
             index_pred_state(N, causal_effects,
                                 effect_status(Rel, Eff, effect))
            ),
            FactsEffectiveCausalEffects),
    length(FactsEffectiveCausalEffects, NrFactsEffectiveCausalEffects),
    findall(effect_status(N, Rel, Eff, balanced),
            (graph_node(OriginalGraph, N),
             index_pred_state(N, causal_effects,
                                 effect_status(Rel, Eff, balanced))
            ),
            FactsBalancedCausalEffects),
    length(FactsBalancedCausalEffects, NrFactsBalancedCausalEffects),
    findall(effect_status(N, Rel, Eff, submissive),
            (graph_node(OriginalGraph, N),
             index_pred_state(N, causal_effects,
                                 effect_status(Rel, Eff, submissive))
            ),
            FactsSubmissiveCausalEffects),
    length(FactsSubmissiveCausalEffects, NrFactsSubmissiveCausalEffects),
    findall(effect_status(N, Rel, Eff, none),
            (graph_node(OriginalGraph, N),
             index_pred_state(N, causal_effects,
                                 effect_status(Rel, Eff, none))
            ),
            FactsInactiveCausalEffects),
    length(FactsInactiveCausalEffects, NrFactsInactiveCausalEffects),


    writef("# causal effects effective:  %p \n", [NrFactsEffectiveCausalEffects]),
    writef("# causal effects balanced:  %p \n", [NrFactsBalancedCausalEffects]),
    writef("# causal effects submissive:  %p \n", [NrFactsSubmissiveCausalEffects]),
    writef("# causal effects inactive:  %p \n", [NrFactsInactiveCausalEffects]),
    writef("# total causal dependencies:  %p \n", [NrFactsCausalEffects]),

    writef("After aggregation on level 2 (local event level):  \n", []),


    % for every event type, count the nr of events in all path segments
    forall(member(EventType, [val_der_events,
                              derivative_events,
                              par_relations,
                              causal_events,
                              inequality_events,
                              corr_events,
                              corr_calc_events,
                              structure_events,
                              system_structures,
                              mf_events]),
       count_events_in_paths(Segments, EventType, _NrEvents6)
    ),
    temp_event_counter(val_der_events, NrValDerEvents),
    % temp_event_counter(derivative_events, NrDerivativeEvents),
    % temp_event_counter(par_relations, NrDependencyEvents),
    temp_event_counter(causal_events, NrCausalEvents),
    temp_event_counter(inequality_events, NrIneqEvents),
    % temp_event_counter(corr_events, NrCorrEvents),
    temp_event_counter(corr_calc_events, NrCorrCalcEvents),
    temp_event_counter(structure_events, NrStructureEvents),
    temp_event_counter(mf_events, NrMFEvents),
    NrEvents is NrValDerEvents + NrCausalEvents +
                NrIneqEvents + NrCorrCalcEvents +
                NrStructureEvents + NrMFEvents,
    writef("# events in total:  %p \n", [NrEvents]),

    writef("After aggregation on level 5 (global level):  \n", []),

    % For different aggregated versions of the graph
    %
    % Transitive Reduction
    %
    reset_aggregation,
    reduce_digraph(OriginalGraph),

    % trace,
    findall(TrState, graph_node(Graph, TrState), TrStatesAfterTR),
    length(TrStatesAfterTR, NrTrStatesAfterTR),

    findall(graph_edge(A->B), graph_edge(Graph, A, B, to), TrEdgesAfterTR),
    length(TrEdgesAfterTR, NrTrEdgesAfterTR),

    findall(PathSegment,
            max_path_segment(A, B, Graph, PathSegment, _Length3), SegmentsAfterTR),
    length(SegmentsAfterTR, NrSegmentsAfterTR),
    findall(Path, full_path(_A3, _B3, Graph, Path, _Length4), TrPathsAfterTR),
    length(TrPathsAfterTR, NrTrPathsAfterTR),



    writef("After transitive reduction:  \n", []),
    writef("# states: %p \n", [NrTrStatesAfterTR]),
    writef("# transitions: %p \n", [NrTrEdgesAfterTR]),
    writef("# paths:  %p \n", [NrTrPathsAfterTR]),
    writef("# path segments:  %p \n", [NrSegmentsAfterTR]),


    % Aggregation of Alternative Orderings
    %
    % Value and Derivative Events
    %
    reset_aggregation,
    retractall(settings(event_types, _RecEventTypes)),
    asserta(settings(event_types, [val_der_events])),
    aggregate_digraph(Graph),

    writef("After aggregation of orderings based on val_der_events:  \n", []),

    findall(TrState, graph_node(Graph, TrState), TrStatesAfterAggr1),
    length(TrStatesAfterAggr1, NrTrStatesAfterAggr1),

    findall(rec_aggr_transition(A->B), rec_aggr_transition(A, B), TrEdgesAfterAggr1),
    length(TrEdgesAfterAggr1, NrTrEdgesAfterAggr1),

    findall(PathSegment,
            max_path_segment(A, B, Graph, PathSegment, _Length5), SegmentsAfterAggr1),
    length(SegmentsAfterAggr1, NrSegmentsAfterAggr1),

    findall(Path, full_path(_A4, _B4, Graph, Path, _Length6), TrPathsAfterAggr1),
    length(TrPathsAfterAggr1, NrTrPathsAfterAggr1),

    % for every event type, count the nr of events in all path segments
    forall(member(EventType, [val_der_events,
                              derivative_events
                              ]),
       count_events_in_paths(SegmentsAfterAggr1, EventType, _NrEvents2)
    ),

    writef("# states: %p \n", [NrTrStatesAfterAggr1]),
    writef("# transitions: %p \n", [NrTrEdgesAfterAggr1]),
    writef("# paths:  %p \n", [NrTrPathsAfterAggr1]),
    writef("# path segments:  %p \n", [NrSegmentsAfterAggr1]),

    temp_event_counter(val_der_events, NrValDerEventsAfterAggr1),
    temp_event_counter(derivative_events, NrDerivativeEventsAfterAggr1),
    writef("# val_der events:     %p \n", [NrValDerEventsAfterAggr1]),
    writef("# derivative events:  %p \n", [NrDerivativeEventsAfterAggr1]),


    % Causal Events
    %
    reset_aggregation,
    retractall(settings(event_types, _RecEventTypes2)),
    asserta(settings(event_types, [causal_events])),
    aggregate_digraph(Graph),

    writef("After aggregation of orderings based on causal_events:  \n", []),

    findall(TrState, graph_node(Graph, TrState), TrStatesAfterAggr2),
    length(TrStatesAfterAggr2, NrTrStatesAfterAggr2),

    findall(rec_aggr_transition(A->B), rec_aggr_transition(A, B), TrEdgesAfterAggr2),
    length(TrEdgesAfterAggr2, NrTrEdgesAfterAggr2),

    findall(PathSegment,
            max_path_segment(A, B, Graph, PathSegment, _Length7), SegmentsAfterAggr2),
    length(SegmentsAfterAggr2, NrSegmentsAfterAggr2),

    findall(Path, full_path(_A5, _B5, Graph, Path, _Length8), TrPathsAfterAggr2),
    length(TrPathsAfterAggr2, NrTrPathsAfterAggr2),

    % for every event type, count the nr of events in all path segments
    forall(member(EventType, [causal_events
                              ]),
       count_events_in_paths(SegmentsAfterAggr2, EventType, _NrEvents3)
    ),

    writef("# states: %p \n", [NrTrStatesAfterAggr2]),
    writef("# transitions: %p \n", [NrTrEdgesAfterAggr2]),
    writef("# paths:  %p \n", [NrTrPathsAfterAggr2]),
    writef("# path segments:  %p \n", [NrSegmentsAfterAggr2]),

    temp_event_counter(causal_events, NrCausalEventsAfterAggr2),
    writef("# causal events:     %p \n", [NrCausalEventsAfterAggr2]),



    % MF Events
    %
    reset_aggregation,
    retractall(settings(event_types, _RecEventTypes3)),
    asserta(settings(event_types, [mf_events])),
    aggregate_digraph(Graph),

    writef("After aggregation of orderings based on MF_events:  \n", []),

    findall(TrState, graph_node(Graph, TrState), TrStatesAfterAggr3),
    length(TrStatesAfterAggr3, NrTrStatesAfterAggr3),

    findall(rec_aggr_transition(A->B), rec_aggr_transition(A, B), TrEdgesAfterAggr3),
    length(TrEdgesAfterAggr3, NrTrEdgesAfterAggr3),

    findall(PathSegment,
            max_path_segment(A, B, Graph, PathSegment, _Length9), SegmentsAfterAggr3),
    length(SegmentsAfterAggr3, NrSegmentsAfterAggr3),

    findall(Path, full_path(_A6, _B6, Graph, Path, _Length10), TrPathsAfterAggr3),
    length(TrPathsAfterAggr3, NrTrPathsAfterAggr3),

    % for every event type, count the nr of events in all path segments
    forall(member(EventType, [system_structures,
                              mf_events
                              ]),
       count_events_in_paths(SegmentsAfterAggr3, EventType, _NrEvents4)
    ),

    writef("# states: %p \n", [NrTrStatesAfterAggr3]),
    writef("# transitions: %p \n", [NrTrEdgesAfterAggr3]),
    writef("# paths:  %p \n", [NrTrPathsAfterAggr3]),
    writef("# path segments:  %p \n", [NrSegmentsAfterAggr3]),

    temp_event_counter(system_structures, NrSSEventsAfterAggr3),
    temp_event_counter(mf_events, NrMFEventsAfterAggr3),
    writef("# MF events:     %p \n", [NrMFEventsAfterAggr3]),
    writef("# MFs involved:     %p \n", [NrSSEventsAfterAggr3]),




    % Value & Der, Causal, and MF Events
    %
    reset_aggregation,
    retractall(settings(event_types, _RecEventTypes4)),
    asserta(settings(event_types, [val_der_events, causal_events, mf_events])),
    aggregate_digraph(Graph),

    writef("After aggregation of orderings based on ValDer, Causal & MF_events:  \n", []),

    findall(TrState, graph_node(Graph, TrState), TrStatesAfterAggr4),
    length(TrStatesAfterAggr4, NrTrStatesAfterAggr4),

    findall(rec_aggr_transition(A->B), rec_aggr_transition(A, B), TrEdgesAfterAggr4),
    length(TrEdgesAfterAggr4, NrTrEdgesAfterAggr4),

    findall(PathSegment,
            max_path_segment(A, B, Graph, PathSegment, _Length11), SegmentsAfterAggr4),
    length(SegmentsAfterAggr4, NrSegmentsAfterAggr4),

    findall(Path, full_path(_A7, _B7, Graph, Path, _Length12), TrPathsAfterAggr4),
    length(TrPathsAfterAggr4, NrTrPathsAfterAggr4),

    % for every event type, count the nr of events in all path segments
    forall(member(EventType, [val_der_events, causal_events, mf_events
                              ]),
       count_events_in_paths(SegmentsAfterAggr4, EventType, _NrEvents5)
    ),

    writef("# states: %p \n", [NrTrStatesAfterAggr4]),
    writef("# transitions: %p \n", [NrTrEdgesAfterAggr4]),
    writef("# paths:  %p \n", [NrTrPathsAfterAggr4]),
    writef("# path segments:  %p \n", [NrSegmentsAfterAggr4]),

    temp_event_counter(mf_events, NrMFEventsAfterAggr4),
    temp_event_counter(val_der_events, NrValDerEventsAfterAggr4),
    temp_event_counter(derivative_events, NrDerivativeEventsAfterAggr4),
    temp_event_counter(causal_events, NrCausalEventsAfterAggr4),
    writef("# val_der events:     %p \n", [NrValDerEventsAfterAggr4]),
    writef("# derivative events:  %p \n", [NrDerivativeEventsAfterAggr4]),
    writef("# causal events:     %p \n", [NrCausalEventsAfterAggr4]),
    writef("# mf events:     %p \n", [NrMFEventsAfterAggr4]),
    NrEventsAggr4 is NrValDerEventsAfterAggr4 + NrCausalEventsAfterAggr4 +
                     NrMFEventsAfterAggr4,
    writef("# events in total:  %p \n", [NrEventsAggr4]),


    % No Events, Maximum Aggregation
    %
    reset_aggregation,
    retractall(settings(event_types, _RecEventTypes5)),
    asserta(settings(event_types, [])),
    aggregate_digraph(Graph),

    writef("After aggregation of orderings while neglecting events (maximum aggregation):  \n", []),


    findall(TrState, graph_node(Graph, TrState), TrStatesAfterAggr5),
    length(TrStatesAfterAggr5, NrTrStatesAfterAggr5),

    findall(rec_aggr_transition(A->B), rec_aggr_transition(A, B), TrEdgesAfterAggr5),
    length(TrEdgesAfterAggr5, NrTrEdgesAfterAggr5),

    findall(PathSegment,
            max_path_segment(A, B, Graph, PathSegment, _Length13), SegmentsAfterAggr5),
    length(SegmentsAfterAggr5, NrSegmentsAfterAggr5),

    findall(Path, full_path(_A8, _B8, Graph, Path, _Length14), TrPathsAfterAggr5),
    length(TrPathsAfterAggr5, NrTrPathsAfterAggr5),

    writef("# states: %p \n", [NrTrStatesAfterAggr5]),
    writef("# transitions: %p \n", [NrTrEdgesAfterAggr5]),
    writef("# paths:  %p \n", [NrTrPathsAfterAggr5]),
    writef("# path segments:  %p \n", [NrSegmentsAfterAggr5]).






% count_events_in_paths(Paths, EventType, NrEvents)
%
% counts all (currently, only local) events that take place in Path
% and prints the results on the screen
%
count_events_in_paths(Paths, EventType, NrEvents):-
    retractall(temp_event_counter(EventType, _)),
    asserta(temp_event_counter(EventType, 0)),
    forall(member(Path, Paths),
       (
        count_events(Path, EventType, NrEventsInPath),
        retract(temp_event_counter(EventType, Counter)),
        NewCounter is Counter + NrEventsInPath,
        asserta(temp_event_counter(EventType, NewCounter))
       )
    ),
    temp_event_counter(EventType, NrEvents),
    % retract(temp_event_counter(EventType, NrEvents)),
    writef("# events (%p): %d \n", [EventType, NrEvents]).



% count_events(Path, EventType, NrEvents)
%
% counts all (currently, only local) events that take place in Path
%
count_events(Path, EventType, NrEvents):-
    % Events
    % Value and derivative events
    contains_events(Path, EventType, _Subjects, Events),
    length(Events, NrEvents).





% write_events(Events)
%
% write procedure for a list of Events
%
write_events([]).

write_events([E|RestEvents]):-
    writef('%p \n', [E]),
    write_events(RestEvents).


% filter_differences(N, Type, L, StateInfoFilter, global_view, FilteredL)
%
% StateInfoFilter = all: do nothing, return L
%
filter_differences(_N, _Type, L, all, global_view, L).

% StateInfoFilter = new_facts_only
%
filter_differences(_N, _Type, [], new_facts_only, global_view, []).
%
%
% Fact was already present in all predecessors; continue with the rest
%
filter_differences(N, Type, [Fact|L], new_facts_only, global_view, FilteredL):-
    % there is at least one predecessor
    edge_to_be_shown(_M, N, bigstate),
    index_pred_state(N, Type, Fact),
    % for all predecessors, the same fact holds
    forall(edge_to_be_shown(M, N, bigstate),
         (
           index_pred_state(M, Type, Fact2),
           % require exact match to prevent Variables being matched
           % this seems to have the opposite effect
           % Fact == Fact2
           Fact = Fact2
         )
    ),!,
    filter_differences(N, Type, L, new_facts_only, global_view, FilteredL).
%
% Fact was not present in one of its predecessors; include in Filtered List
%
filter_differences(N, Type, [Fact|L], new_facts_only, global_view, [Fact|FilteredL]):-
    filter_differences(N, Type, L, new_facts_only, global_view, FilteredL).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% determine_forbidden(quantity, ForbiddenQuantities):-
%
% determine forbidden quantities
%
% if settings have been defined, use them
%
determine_forbidden(entity, ForbiddenEntities):-
    settings(forbidden(entity, ForbiddenEntities)),!.
%
determine_forbidden(quantity, ForbiddenQuantities):-
    settings(forbidden(quantity, ForbiddenQuantities)),!.

% if the above two fail, return empty list
% for both quantity and entity
determine_forbidden(_, []).


% filter(-Quantities1, -EventList1, -Quantities2, +EventList2)
%
% filters EventList1 for those events that involve
% quantities from Quantities2, resulting in EventList2
%
% empty list of events: stop condition
filter(_Quantities1, [], _Quantities2, []):-!.

/*
 * this also lets through mf events which should not be filtered
 * by quantities!
% empty list of filters: let through all events
filter(_Quantities1, Events, [], Events):-!.
*/

% for parameters
filter(_Quantities1, Stuff, FilterQuantities, FilteredList):-
    Stuff = [parameters/ParList],
    filter(_Quantities1b, ParList, FilterQuantities, FilteredList1),
    determine_forbidden(quantity, ForbiddenQuantities),
    filter_out(FilteredList1, ForbiddenQuantities, FilteredList).


% for values
filter(_Quantities1, Stuff, FilterQuantities, FilteredList):-
    Stuff = [par_values/ValueList],
    filter(_Quantities1b, ValueList, FilterQuantities, FilteredList1),
    determine_forbidden(quantity, ForbiddenQuantities),
    filter_out(FilteredList1, ForbiddenQuantities, FilteredList).

% for dependencies
filter(_Quantities1, Stuff, FilterQuantities, FilteredList):-
    Stuff = [par_relations/ParRelList],
    filter(_Quantities1b, ParRelList, FilterQuantities, FilteredList1),
    determine_forbidden(quantity, ForbiddenQuantities),
    filter_out(FilteredList1, ForbiddenQuantities, FilteredList).

% for causal effects
filter(_Quantities1, Stuff, FilterQuantities, FilteredList):-
    Stuff = [causal_effects/CEList],
    filter(_Quantities1b, CEList, FilterQuantities, FilteredList1),
    determine_forbidden(quantity, ForbiddenQuantities),
    filter_out(FilteredList1, ForbiddenQuantities, FilteredList).

% for system_structures (model fragments), filter MFs
filter(_Quantities1, Stuff, FilterQuantities, system_structures/FilteredMFList):-
    Stuff = [system_structures/MFList],
    filter_system_structures(MFList, _A, FilterQuantities, FilteredMFList).

% for other kinds
filter(_Quantities1, Stuff, FilterQuantities, FilteredList):-
    Stuff = [_EventType/Events],
    _NewStuff = Events,
    filter(_Quantities1b, Events, FilterQuantities, FilteredList1),
    determine_forbidden(quantity, ForbiddenQuantities),
    filter_out(FilteredList1, ForbiddenQuantities, FilteredList).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
% specific for types of events?
% value event
filter(Quantities1, [Event|Rest1], Quantities2, [Event|Rest2]):-
    Event = value_event((_A->_B), Q, _V1, _D1, _ValueEvent, _V2, _D2),
    member(Q, Quantities2), !,
    filter(Quantities1, Rest1, Quantities2, Rest2).
*/


% specific for mf_events
%
% Quantity Q occurs in an MF that has been added in Event
filter(Quantities1, [Event|Rest1], Quantities2, [FilteredEvent|Rest2]):-
    Event = added((A->B), mf, MFList),
    filter_MFs(MFList, B, Quantities2, FilteredMFList),
    FilteredMFList \== [],
    FilteredEvent = added((A->B), mf, FilteredMFList),
    filter(Quantities1, Rest1, Quantities2, Rest2),!.
%
%
% Quantity Q does not occur in any MF that has been added in Event
filter(Quantities1, [Event|Rest1], Quantities2, Rest2):-
    % no need to look further if it's an MF addition event
    Event = added((_A->_B), mf, _MFList), !,
    filter(Quantities1, Rest1, Quantities2, Rest2),!.


%
%
% Quantity Q occurs in an MF that has been removed in Event
filter(Quantities1, [Event|Rest1], Quantities2, [FilteredEvent|Rest2]):-
    Event = removed((A->B), mf, MFList),
    filter_MFs(MFList, A, Quantities2, FilteredMFList),
    FilteredMFList \== [],
    FilteredEvent = removed((A->B), mf, FilteredMFList),
    filter(Quantities1, Rest1, Quantities2, Rest2),!.
%
%
% Quantity Q does not occur in any MF that has been removed in Event
filter(Quantities1, [Event|Rest1], Quantities2, Rest2):-
    % no need to look further if it's an MF removal event
    Event = removed((_A->_B), mf, _MFList), !,
    filter(Quantities1, Rest1, Quantities2, Rest2),!.


%
%
% Case 1: Quantity Q occurs in at least one MF that's involved
% in replacement Event.
% The lists of MFs are filtered
%
filter(Quantities1, [Event|Rest1], Quantities2, [FilteredEvent|Rest2]):-
    Event = replaced((A->B), mf, MFList1, MFList2),
    filter_MFs(MFList1, A, Quantities2, FilteredMFList1),
    filter_MFs(MFList2, B, Quantities2, FilteredMFList2),
    % check whether both are empty lists
    not(FilteredMFList1 == FilteredMFList2), !,
    FilteredEvent = replaced((A->B), mf, FilteredMFList1, FilteredMFList2),
    filter(Quantities1, Rest1, Quantities2, Rest2),!.
%
%
% Case 2: Q does not occur in MFList1 or 2 in Event; try the rest
filter(Quantities1, [Event|Rest1], Quantities2, Rest2):-
    Event = replaced((_A->_B), mf, _MFList1, _MFList2), !,
    filter(Quantities1, Rest1, Quantities2, Rest2),!.





% generic filtering method, but perhaps not the most
% efficient. May let through too much, when quantities
% are named the same as entities or model fragments.
%
filter(Quantities1, [Event|Rest1], Quantities2, [Event|Rest2]):-
    Event =.. EventAsList,
    member(Q, Quantities2),
    destruct_member(Q, EventAsList),!,
    filter(Quantities1, Rest1, Quantities2, Rest2).


% the first member did not match, so try the rest
filter(Quantities1, [_Event|Rest1], Quantities2, Rest2):-
    filter(Quantities1, Rest1, Quantities2, Rest2).


% no filter (for test purposes)
% filter_MFs(MFList, _StateNr, _FilterList, MFList).


% empty list
filter_system_structures([], _A, _FilterQuantities, []).

% for a single system_structure (model fragment), filter MFs
filter_system_structures([First|MFList], _A, FilterQuantities, Filtered):-
    First = system_structures(MF, _Isa, _Cond, _Res),
    filter_MFs([MF], _Ab, FilterQuantities, FilteredFirst),
    filter_system_structures(MFList, _Ac, FilterQuantities, FilteredRest),
    conc(FilteredFirst, FilteredRest, Filtered).


% filter_out(List, ForbiddenStuff, FilteredList):-
%
% This filters out everything that contains any element from ForbiddenStuff
%
filter_out([], _ForbiddenStuff, []).

filter_out([X|List], ForbiddenStuff, FilteredList):-
    % if X contains anything from ForbiddenStuff, delete X
    member(F, ForbiddenStuff),
    destruct_member(F, X),!,
    filter_out(List, ForbiddenStuff, FilteredList).

filter_out([X|List], ForbiddenStuff, [X|FilteredList]):-
    % if X does not contains anything from ForbiddenStuff, keep X
    filter_out(List, ForbiddenStuff, FilteredList).




% filter_MFs(+MFList, +StateNr, +FilterList, -FilteredMFList):-
%
% return a FilteredMFSet of all MFs
%
filter_MFs(MFList, StateNr, FilterList, FilteredMFSet):-
    settings(mf_filter_type, FilterType),
    assertion(FilterType == quantities),
    filter_MFs_by_quantities(MFList, StateNr, FilterList, FilteredMFSet).


% filter_MFs_by_quantities(+MFList, +StateNr, +FilterList, -FilteredMFList):-
%
% return a FilteredMFSet of all MFs which contain a reference
% to a member quantity Q of FilterList in conditions or results
%
filter_MFs_by_quantities(MFList, StateNr, FilterList, FilteredMFSet):-
    findall(MF,
     (
      member(MF, MFList),
      index_pred_state(StateNr, system_structures,
                     system_structures(MF, _Isa, conditions(C), givens(R))),
      member(Q, FilterList),
      % check if Q occurs in either the conditions or results of MF
      (destruct_member(Q, C)
      ;
      destruct_member(Q, R)
      )
     ), FilteredMFList),
     sort(FilteredMFList, FilteredMFListSorted),
     list_to_set(FilteredMFListSorted, FilteredMFSet).  %gp3 changed this (was proprietary clause that did the same)



% compare_causal_models(StateList)
%
compare_causal_models([]).

compare_causal_models(StateList):-
    causal_model_intersection(StateList, CMIntersection),
    writef('All states, intersection of dependencies: %p\n',[CMIntersection]),
    compare_causal_models(StateList, CMIntersection).

compare_causal_models([], _CMIntersection).

compare_causal_models([State|RestStates], CMIntersection):-
    get_causal_dependencies(State, StateSet),
    my_difference(StateSet, CMIntersection, DiffSet),
    writef('State %p, contains extra dependencies: %p\n',[State, DiffSet]),
    compare_causal_models(RestStates, CMIntersection).


% causal_model_intersection(StateList, CausalDependenciesList).
%
% start with first state: take all dependencies in the working set
% for each next state, delete those dependencies from the working set which
% are not included in that state. What you end up with is the set
% that exists in all states.
%
causal_model_intersection([], []).

causal_model_intersection([State|RestStates], Dependencies):-
    get_causal_dependencies(State, WorkingSet),
%    writef('Initializing working set \n',[]),
%    writef('State %p, causal model consists of: %p\n',[State, WorkingSet]),
    delete_nonexisting(RestStates, WorkingSet, Dependencies).

% delete_nonexisting(StateList, WorkingSet, ResultingSet)
%
% delete from WorkingSet those dependencies which are
% absent in any of the states in StateList. The result
% the ResultingSet.
%
delete_nonexisting([], WorkingSet, WorkingSet).

delete_nonexisting([State|RestStates], WorkingSet, ResultSet):-
    get_causal_dependencies(State, StateSet),
%    writef('State %p, causal model consists of: %p\n',[State, StateSet]),
    my_difference(WorkingSet, StateSet, DiffSet),
%    writef('Not present in state %p: %p\n',[State, DiffSet]),
    my_difference(WorkingSet, DiffSet, NewWorkingSet),
%    writef('Deleted. New Working Set: %p: \n',[NewWorkingSet]),
    delete_nonexisting(RestStates, NewWorkingSet, ResultSet).

get_causal_dependencies(State, CausalDependencies):-
    findall(causal_dependency(VA, VR, VB),
            (causal_model(State, R, A, B),
            % (math_model(State, R, A, B),
             visualize(R, A, B, VR, VA, VB, _Dir)
            ),
            CausalDependencies).





/*
these predicates are in the file interface.pl
I copied them here just for inspiration

describe_state(Nr, ListOfAllTypes):-
	findall(Type/L,
	index_state(Nr, Type, L),
	ListOfAllTypes).

compare_states(N1, N2, N1minN2, N2minN1, N1andN2):-
	% get description of state N1
	describe_state(N1, ListOfAllTypes1),

	% get description of state N2
	describe_state(N2, ListOfAllTypes2),

	% compare state descriptions
	difference_types(ListOfAllTypes1, ListOfAllTypes2, N1minN2),
	difference_types(ListOfAllTypes2, ListOfAllTypes1, N2minN1),
	difference_types(ListOfAllTypes1, N1minN2, N1andN2).

*/




% permutation(L1, L2).
%
% returns true when L1 is a permutation of L2,
% in other words contain the same elements
%
permutation([], []).

permutation([X|L], P):-
    permutation(L,L1),
    insert(X, L1, P).


% del(X, L, L1)
%
% deleting X from L gives L1
%
del(X, [X|Rest], Rest).

del(X, [Y|Rest], [Y|Rest1]):-
    del(X, Rest, Rest1).


insert(X, List, BiggerList):-
    del(X, BiggerList, List).



calculate_max_length([Path|ListOfPaths], MaxLength):-
	calculate_max_length(ListOfPaths, Length1),
	length(Path, Length2),
	max(Length1, Length2, MaxLength).

calculate_max_length([], 0).


% fill_variables(-List, -Fill, +ListWithoutVariables)
%
% fills the uninstantiated variables in List with Fill
%
fill_variables([], _, []).

fill_variables([X|List], Fill, [X|ListWithoutVariables]):-
    nonvar(X),!,
    fill_variables(List, Fill, ListWithoutVariables).

fill_variables([_X|List], Fill, [Fill|ListWithoutVariables]):-
    fill_variables(List, Fill, ListWithoutVariables).



% my_flatten(-List, +AtomizedList)
%
% tears apart List completely into all its
% individual atoms, and puts them into AtomizedList
%
my_flatten([], []).

% variables are replaced by 'var' to
% prevent unwanted matches
my_flatten([X|List], [var|AtomList]):-
    var(X),!,
    my_flatten(List, AtomList).

my_flatten([X|List], [X|AtomList]):-
    atomic(X),!,
    my_flatten(List, AtomList).

my_flatten([X|List], AtomList):-
    is_list(X),!,
    my_flatten(X, Y),
    my_flatten(List, AtomList1),
    conc(Y, AtomList1, AtomList).

my_flatten([X|List], AtomList):-
    X =.. XAsList,!,
    my_flatten(XAsList, Y),
    my_flatten(List, AtomList1),
    conc(Y, AtomList1, AtomList).

my_flatten(X, List):-
    not(is_list(X)),!,
    my_flatten([X], List).



% destruct_member(-X, -UnknownStructure)
%
% tears apart UnknownStructure completely into all its
% individual atoms, and succeeds if X is one of them
%
% X may be:
%
% - a variable: fail (currently, this doesn't happen, see 1st clause)
% - an atom or int: check if it matches, as below
% more unpredictable results are produced when X is a predicate or list.
% if X contains variables, these may be filled, e.g.,
% when X is pred(a,b,C), it will match pred(a,b,c).
%
% UnknownStructure may be:
%
% - a variable: fail.
% - equal to X (== X): succeed.
% - unequal to X, and
%   - an atom or int: fail.
%   - a list: check the first member as UnknownStructure,
%             if yes, succeed, otherwise, try rest of list
%   - a predicate: transform predicate to list
%                  and check the list
%
% if X is a variable, fail
% to prevent unwanted matches
% this clause is turned off to improve efficiency
% destruct_member(X, _Y):-
%     var(X), !, fail.

% if U is a variable, also fail
% to prevent unwanted matches
destruct_member(_X, Y):-
    var(Y), !, fail.

% if X is equal to U, succeed.
destruct_member(X, X).

% now we know that X \== Y
%
% if Y is atomic (atom or int), X can never match Y anymore
destruct_member(_X, Y):-
    atomic(Y), !, fail.

% if U is an empty list: fail.
destruct_member(_X, []):-
    !, fail.

% if U is a list: check the first member as UnknownStructure,
% if yes, succeed.
destruct_member(X, [Y|_List]):-
    destruct_member(X, Y),!.

% U is a list, but the first member did not match,
% so try the rest of the list
destruct_member(X, [_Y|List]):-
    !, destruct_member(X, List).

% we know now that U is not a list
% if U is a predicate: transform predicate to list
% and check the list
destruct_member(X, U):-
    U =.. UList,
    destruct_member(X, UList),!.



