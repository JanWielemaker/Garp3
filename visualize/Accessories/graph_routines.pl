/*  
    This file includes some routines used in the visualize module of Garp3 
    Anders Bouwer, 18/08/1999

    Last updated: March 2007
*/


% :-consult('block_search.pl').

% path(A, B, state_graph, Path, N)
%
% This should not be used anymore - rather use the 
% more specific version, with Aggregation and Input options
% This one is kept for backwards compatibility
%
path(A, B, state_graph, Path, N):-
  path(A, B, state_graph(both, input), Path, N).



% path(A, B, state_graph(TypeOfEdges, InputOrNot), Path, N).
%
% finds any path of length N edges (including cycle) 
% between A and B in Graph. 
%
% TypeOfEdges = original/aggregated/both
%   specifies which types of edges may be included
%
% InputOrNot = input/noinput
%   specifies whether edges from the input-state should be included or not. 
%
%
% if N is a variable, it will be instantiated with the length of the shortest path
% from a startNode A to an endnode B
%
path(StartNodes, EndNodes, _Graph, Path, N):-
    var(N),!,
    single_bfs_path(StartNodes, EndNodes, Path),
    % bfs_path(StartNodes, EndNodes, Graph, Path),
    length(Path, N).


/*
path(A, B, Graph, Path, N):-
    var(N),!,
    bfs_path(A, [B], Graph, Path),
    length(Path, N).
*/


/*
multi_bfs(GoalNodes, Path):-
    block_multi_bfs(GoalNodes, Path).
*/


% single_bfs_path
%
% breadth first search that finds the shortest path 
% from one of the StartNodes to one of the GoalNodes
%
single_bfs_path(StartNodes, GoalNodes, Path):-
    findall(CandidatePath, 
	    (
	     member(S, StartNodes),
	     CandidatePath = [S]
	     ), 
	    CandidatePaths),
    % writef('Current list of candidate paths: \n', []),
    % my_write_list(CandidatePaths),
    conc(CandidatePaths, Z, CP),
    single_bfs(CP-Z, GoalNodes, Path).


% multi_bfs
%
% breadth first search that finds the shortest path 
% through multiple goal states
%
multi_bfs(GoalNodes, Path):-
    findall(CandidatePath, 
	    (
	     member(G, GoalNodes),
	     CandidatePath = [G]
	     ), 
	    CandidatePaths),
    % writef('Current list of candidate paths: \n', []),
    % my_write_list(CandidatePaths),
    conc(CandidatePaths, Z, CP),
    multi_bfs(CP-Z, GoalNodes, Path).



% multi_bfs_full_path
%
% breadth first search that finds the shortest full path 
% through multiple goal states
%
multi_bfs_full_path(GoalNodes, Path):-
    Graph = state_graph(original, noinput), 
    findall(A, start_node(Graph, A), StartNodes), 
    findall(B, end_node(Graph, B), EndNodes),
    % if there are no EndNodes, fail
    EndNodes \== [],
    findall(CandidatePath, 
	    (
	     member(S, StartNodes),
	     CandidatePath = [S]
	     ), 
	    CandidatePaths),
    % writef('Current list of candidate paths: \n', []),
    % my_write_list(CandidatePaths),
    conc(CandidatePaths, Z, CP),
    multi_bfs_full_path(CP-Z, GoalNodes, EndNodes, Path).



% multi_bfs_path_ending_in_cycle
%
% breadth first search that finds the shortest path 
% through multiple goal states ending in a cycle
%
multi_bfs_path_ending_in_cycle(GoalNodes, PathEndingInCycle):-
    Graph = state_graph(original, noinput), 
    findall(A, start_node(Graph, A), StartNodes), 
    findall(CandidatePath, 
	    (
	     member(S, StartNodes),
	     CandidatePath = [S]
	     ), 
	    CandidatePaths),
    % writef('Current list of candidate paths: \n', []),
    % my_write_list(CandidatePaths),
    conc(CandidatePaths, Z, CP),
    multi_bfs_path_ending_in_cycle(CP-Z, GoalNodes, Path),
    last(Path, Last),
    graph_edge(state_graph(both, noinput), Last, Next, _Rel),
    % Next was already included, so adding this creates a loop at the end
    member(Next, Path),
    append(Path, [Next], PathEndingInCycle).



/*
multi_bfs(GoalNodes, Path):-
    block_multi_bfs(GoalNodes, Path).
*/


% cycle_bfs(GoalNodes, Cycle)  
% 
% returns Cycle, a minimal cyclic path through all GoalNodes
% cycle_bfs(GoalNodes, Cycle)
%
cycle_bfs(GoalNodes, Cycle):-
    cycle_multi_bfs(GoalNodes, [First|Path]),
    last(Path, Last),
    graph_edge(state_graph(both, noinput), Last, First, _Rel),
    append([First|Path], [First], Cycle).
 


% cycle_multi_bfs
%
% adaptation of multi_bfs for cycles - stop searching when you reach a start node.
%
cycle_multi_bfs(GoalNodes, Path):-
    % just take one - since it will become a cycle, it does not matter which one
    member(G, GoalNodes),!,
    CandidatePaths = [[G]],
    % writef('Current list of candidate paths: \n', []),
    % my_write_list(CandidatePaths),
    conc(CandidatePaths, Z, CP),
    cycle_multi_bfs(CP-Z, GoalNodes, Path).



% cycle_multi_bfs(CandidatePaths-Z, GoalNodes, Path)
% 
% adaptation of multi_bfs for cycles - stop searching when you reach a start node.
% 
% All GoalNodes appear in Path: done!
%
cycle_multi_bfs([Path|_CandidatePaths]-_Z, GoalNodes, Path):-
    subset(GoalNodes, Path).


% FirstPath has not reached all GoalNodes yet, so extend it
% unless the first is a start node (in which case it is unreachable)
%
cycle_multi_bfs([FirstPath|CandidatePaths]-Z, GoalNodes, Path):-
    FirstPath = [A|_Rest], 
    not(real_start_node(state_graph, A)), !,
    extend_forwards(FirstPath, ExtensionsOfFirstPath),!,

    % if ExtensionsOfFirstPath = []
    % then
    % ( 
    %   writef('cycle bfs: FirstPath %w could not be extended\n',[FirstPath])
    % ),

    conc(ExtensionsOfFirstPath, Z1, Z),!,
    CandidatePaths \== Z1,

    % writef('Cycle_multi_bfs: Current list of candidate paths (extended): \n', []),
    % my_write_list(ExtensionsOfFirstPath),!,

    cycle_multi_bfs(CandidatePaths-Z1, GoalNodes, Path).


cycle_multi_bfs([FirstPath|_CandidatePaths]-_Z, _GoalNodes, _Path):-
    FirstPath = [A|_Rest], 
    real_start_node(state_graph, A), !,
    % writef('start node: %d - do not extend this path\n',[A]), 
    fail.


%%%%%%% single_bfs

% single_bfs(CandidatePaths-Z, GoalNodes, Path)
% 
% returns Path, a minimal solution path to one of the specified GoalNodes.
% It works with difference lists, inspired by Bratko, 2001.
% 
%% One of the GoalNodes appears in Path: done!
% Path ends in one of the GoalNodes: done!

single_bfs([Path|_CandidatePaths]-_Z, GoalNodes, Path):-
    member(G, GoalNodes),
    last(Path, G).
    % member(G, Path).


% FirstPath has not reached a GoalNode yet, so extend it
%
single_bfs([FirstPath|CandidatePaths]-Z, GoalNodes, Path):-
    extend_forwards(FirstPath, ExtensionsOfFirstPath),

    % if ExtensionsOfFirstPath = []
    % then
    % ( 
    %   writef('single bfs: FirstPath %w could not be extended\n',[FirstPath])
    % ),
    
    conc(ExtensionsOfFirstPath, Z1, Z),
    CandidatePaths \== Z1,
    
    % writef('Single bfs: Current list of candidate paths (extended): \n', []),
    % my_write_list(ExtensionsOfFirstPath),

    single_bfs(CandidatePaths-Z1, GoalNodes, Path).

%%%%%%% end single_bfs


% multi_bfs(CandidatePaths-Z, GoalNodes, Path)
% 
% returns Path, a minimal solution path which contains all GoalNodes.
% It works with difference lists, inspired by Bratko, 2001.
% This is an adaptation of breadth-first-search which finds the shortest path 
% through multiple nodes, not just two nodes.
% 
% All GoalNodes appear in Path: done!

multi_bfs([Path|_CandidatePaths]-_Z, GoalNodes, Path):-
    subset(GoalNodes, Path).


% FirstPath has not reached all GoalNodes yet, so extend it
%
multi_bfs([FirstPath|CandidatePaths]-Z, GoalNodes, Path):-
    extend_forwards(FirstPath, ExtensionsOfFirstPath),

    % if ExtensionsOfFirstPath = []
    % then
    % ( 
    %   writef('multi bfs: FirstPath %w could not be extended\n',[FirstPath])
    % ),
    
    conc(ExtensionsOfFirstPath, Z1, Z),
    CandidatePaths \== Z1,
    
    % writef('Multi bfs: Current list of candidate paths (extended): \n', []),
    % my_write_list(ExtensionsOfFirstPath),

    multi_bfs(CandidatePaths-Z1, GoalNodes, Path).




% multi_bfs_full_path(CandidatePaths-Z, GoalNodes, EndNodes, Path)
% 
% returns Path, a minimal solution path which contains all GoalNodes and 
% ends in one of the EndNodes.
% It works with difference lists, inspired by Bratko, 2001.
% This is an adaptation of breadth-first-search which finds the shortest path 
% through multiple nodes, not just two nodes.
% 
% Path contains all GoalNodes and ends with an EndNode: done!

multi_bfs_full_path([Path|_CandidatePaths]-_Z, GoalNodes, EndNodes, Path):-
    subset(GoalNodes, Path),    
    last(Path, EndNode),
    member(EndNode, EndNodes).

% FirstPath has not reached all GoalNodes yet, so extend it
%
multi_bfs_full_path([FirstPath|CandidatePaths]-Z, GoalNodes, EndNodes, Path):-
    extend_forwards(FirstPath, ExtensionsOfFirstPath),

    % if ExtensionsOfFirstPath = []
    % then
    % ( 
    %   writef('multi bfs: FirstPath %w could not be extended\n',[FirstPath])
    % ),
    
    conc(ExtensionsOfFirstPath, Z1, Z),
    CandidatePaths \== Z1,
    
    % writef('Multi bfs: Current list of candidate paths (extended): \n', []),
    % my_write_list(ExtensionsOfFirstPath),

    multi_bfs_full_path(CandidatePaths-Z1, GoalNodes, EndNodes, Path).

%%%%%%%% end multi_bfs_full_path




% multi_bfs_path_ending_in_cycle(CandidatePaths-Z, GoalNodes, Path)
% 
% returns Path, a minimal solution path which contains all GoalNodes and 
% ends in a cycle.
% 
% Path contains all GoalNodes: done!

multi_bfs_path_ending_in_cycle([Path|_CandidatePaths]-_Z, GoalNodes, Path):-
    subset(GoalNodes, Path).

% FirstPath has not reached all GoalNodes yet, so extend it
%
multi_bfs_path_ending_in_cycle([FirstPath|CandidatePaths]-Z, GoalNodes, Path):-
    extend_forwards(FirstPath, ExtensionsOfFirstPath),

    % if ExtensionsOfFirstPath = []
    % then
    % ( 
    %   writef('multi bfs: FirstPath %w could not be extended\n',[FirstPath])
    % ),
    
    conc(ExtensionsOfFirstPath, Z1, Z),
    CandidatePaths \== Z1,
    
    % writef('Multi bfs: Current list of candidate paths (extended): \n', []),
    % my_write_list(ExtensionsOfFirstPath),

    multi_bfs_path_ending_in_cycle(CandidatePaths-Z1, GoalNodes, Path).

%%%% end multi_bfs_path_ending_in_cycle


% extend Path forwards by one, return a list of all resulting paths: Extensions
extend_forwards(Path, Extensions):-
    last(Path, Last), 
    findall(NewPath, 
	  (graph_edge(state_graph(both, noinput), Last, Next, _Rel),
           % Next should not create a cycle
           not(member(Next, Path)), 
	   append(Path, [Next], NewPath)           
	  ), Extensions).
    % writef('Extend %w forwards: \n', [Path]),
    % my_write_list(Extensions),!.



% bfs_path(StartNodes, GoalNodes, Graph, Path)
% 
% this version of breadth-first-search returns the shortest Path from 
% one of the StartNodes to the nearest node of the GoalNodes
%
% bfs_path(StartNodes, GoalNodes, Graph, Path) returns a Path from a StartNode A to one of the GoalNodes
% breadth_first builds up a path backwards, so this needs to be reversed to get Path
%
bfs_path(StartNodes, GoalNodes, Graph, Path):-
    findall([A], member(A, StartNodes), StartPaths), 
    % breadth_first([[A]], GoalNodes, Graph, RevPath),
    breadth_first(StartPaths, GoalNodes, Graph, RevPath),
    reverse(RevPath, Path).


% breadth_first(ListOfPaths, GoalNodes, Graph, RevPath) returns RevPath given ListOfPaths to start with
%
% stop condition: fail if GoalNodes = []
breadth_first([[GoalNode|Path]|_], [], _Graph, [GoalNode|Path]):-
    !,
    fail.

% stop condition: succeeded
breadth_first([[GoalNode|Path]|_], GoalNodes, _Graph, [GoalNode|Path]):-
    member(GoalNode, GoalNodes).


/*

% for cycles: first node is a start node: skip this path
breadth_first([[A|_Path]|Paths], GoalNodes, Graph, Solution):-
    real_start_node(Graph, A),!, % bla
    % writef('start node: %d - do not extend this path\n',[A]),
    breadth_first(Paths, GoalNodes, Graph, Solution).
*/


% Path does not contain goal node yet, so add extensions to the end of the list, and try again
breadth_first([Path|Paths], GoalNodes, Graph, Solution):-
    extend(Path, Graph, NewPaths),!,
    conc(Paths, NewPaths, Paths1),!,
    breadth_first(Paths1, GoalNodes, Graph, Solution).

extend([Node|Path], Graph, NewPaths):-
    bagof([NewNode, Node|Path], 
     (graph_edge(Graph, Node, NewNode, _Rel),
     not(member(NewNode, [Node|Path]))
     ), NewPaths),
    !.

extend(_Path, _Graph, []).


% cyclic_path(A, A, Graph, [A|Path], N):-
% 
% for a cyclic path, add a cycle-making edge to a path.
%
/*
cyclic_path(A, A, Graph, [A|Path], N):-
    graph_edge(Graph, A, B, _Rel),
    bfs_path(B, [A], Graph, Path),
    length(Path, Nmin1),
    N is Nmin1 + 1.
*/
cyclic_path(A, A, _Graph, CyclicPath, N):-
    single_bfs_path([A], [B], Path),
    rec_transition(B, A), 
    % graph_edge(Graph, B, A, _Rel),
    conc(Path, [A], CyclicPath),
    length(CyclicPath, N).

  

% path_max may become obsolete, but let's keep it for now.
%
% path_max(A, B, Graph, Path, Max, N)
%
% finds paths of all lengths N up to Max
%
path_max(_A, _B, _Graph, _Path, Max, _N):-
    Max =< 0, !, fail.

% find shorter paths first
%
path_max(A, B, Graph, Path, Max, N):-
    NewMax is Max - 1,
    % writef('NewMax: %d \n', [NewMax]),
    path_max(A, B, Graph, Path, NewMax, N).

% find maximum length path last
%
path_max(A, B, Graph, Path, Max, Max):-
    % but only when there is at least one path of length Max-1
    NewMax is Max - 1,
    % If NewMax < 1, don't bother
    (NewMax > 0
    -> 
       path(A, _, Graph, Path1, NewMax),!,
       writef('Path found of length Max - 1 = %d: %w \n', [NewMax, Path1])
    ; 
       true
    ), 
    writef('Searching for a path from %d to %d of length up to Max: %d\n', [A,B,Max]),
    path(A, B, Graph, Path, Max),
    writef('Path found of length %d: %w found! \n', [Max, Path]).



% full_path finds all paths between start nodes A 
% and end nodes B in Graph, of Length, plus all paths which contain 
% a cycle making edge as the last edge
%
% from start to end node
full_path(A, B, Graph, Path, Length):-
    start_node(Graph, A),
    end_node(Graph, B), 
    path(A, B, Graph, Path, Length).

% path with a cycle made by the last edge
%
full_path(A, B, Graph, Path, Length):-
    start_node(Graph, A),
    path(A, B, Graph, Path, Length),
    % B was already present earlier in the path
    reverse(Path, [B|AllButLast]), 
    member(B, AllButLast).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Causal model dependencies graph
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
% c_path(A, B, Graph, Path, N)
%
% finds any causal path of length N edges (including cycles) 
% between A and B in Graph  
%
%
% if N is a variable, then check paths of all lengths up to 
% the maximum of the number of nodes in the graph. 
%
c_path(A, B, Graph, Path, N):-
    var(N),!,
    findall(Node, graph_node(Graph, Node), NodesList),
    list_to_set(NodesList, Nodes),
    length(Nodes, Max), 
    c_path_max(A, B, Graph, Path, Max, N).

% N is not a variable
%
c_path(A, B, Graph, Path, N):-
    c_path1(A, [B], Graph, Path, N).



% c_path1(A, Z, Graph, Path, Length)
%
% after Bratko (1990, p. 234)
%
% returns a Path which may be cyclic, but only 
% when A = Z, so no subpath in Path is cyclic.
% Length is measured in the number of edges.
%
c_path1(_A, _B, _Graph, _Path, N):-
    N < 0, !, fail.

c_path1(A, [A|Path1], _Graph, [A|Path1], 0).

c_path1(A, [C|Path1], Graph, Path, N):-
    graph_edge(Graph, B, C, Rel),
    not(member(C, Path1)),   % no cycle except when A = Z
    % not(member(B, Path1)), % no cycles at all
    NewN is N - 1, 
    c_path1(A, [B, Rel, C|Path1], Graph, Path, NewN).



% c_path_max(A, B, Graph, Path, Max, N)
%
% finds paths of all lengths N up to Max
%
c_path_max(_A, _B, _Graph, _Path, Max, _N):-
    Max =< 0, !, fail.

% find shorter paths first
%
c_path_max(A, B, Graph, Path, Max, N):-
    NewMax is Max - 1,
    c_path_max(A, B, Graph, Path, NewMax, N).

% find maximum length path last
%
c_path_max(A, B, Graph, Path, Max, Max):-
    c_path(A, B, Graph, Path, Max).


% full_path finds all paths between start nodes A 
% and end nodes B in Graph, of Length
%
full_c_path(A, B, Graph, Path, Length):-
    start_node(Graph, A),
    end_node(Graph, B), 
    c_path(A, B, Graph, Path, Length).




% real start node is a node without incoming edges
%
real_start_node(Graph, A):-
    graph_node(Graph, A), 
    not(graph_edge(Graph, _From, A, _Rel)).



% kept for backwards compatibility
%
start_node(state_graph, A):-
    state_to_be_shown(A, state_graph),
    start_node(state_graph(both, _), A).


% in the state-transition graph, all nodes which are connected
% to the input state are start nodes
%
start_node(state_graph(TypeOfEdges, _), A):-
    graph_node(state_graph(TypeOfEdges, noinput), A), 
    graph_edge(state_graph(TypeOfEdges, input), input, A, to).



% in all other graphs, a start node is a node without 
% incoming edges
%
start_node(Graph, A):-
    Graph \= state_graph,
    Graph \= state_graph(_,_),
    graph_node(Graph, A), 
    not(graph_edge(Graph, _From, A, _Rel)).


% end_node(Graph, A) returns all nodes A in Graph 
% which have no outgoing edges
%
end_node(Graph, A):-
    graph_node(Graph, A), 
    not(graph_edge(Graph, A, _B, _Rel)).


graph_stats:-
    Graph = state_graph(original, noinput), 
    findall(InDegree, 
	 (
	   graph_node(Graph, N), 
	   in_degree(Graph, N, InDegree),
  	   InDegree > 0
	  ),
	    InDegrees), 
    average(InDegrees, AvInDegree),

    findall(OutDegree, 
	 (
	   graph_node(Graph, N), 
	   out_degree(Graph, N, OutDegree),
	   OutDegree > 0
	  ),
	    OutDegrees), 
    average(OutDegrees, AvOutDegree),
    findall(Node, 
	    graph_node(Graph, Node), 
	    Nodes), 
    findall(EndNode, 
	    end_node(Graph, EndNode), 
	    EndNodes), 
    findall(StartNode, 
	    start_node(Graph, StartNode), 
	    StartNodes), 
    findall(RealStartNode, 
	    real_start_node(Graph, RealStartNode), 
	    RealStartNodes), 
    length(Nodes, L), 
    length(StartNodes, LS), 
    length(RealStartNodes, RLS), 
    length(EndNodes, LE), 

    findall(edge(A,B), 
	    graph_edge(Graph, A, B, _Rel), 
	    Edges), 
    length(Edges, L2), 

    writef('Number of nodes       : %w\n',[L]), 
    writef('Number of start nodes : %w\n',[LS]), 
    writef('Number of real start nodes (no incoming edges): %w\n',[RLS]), 
    writef('Number of end nodes   : %w\n',[LE]), 
    writef('Number of edges : %w\n',[L2]), 
    writef('Average in-degree per node (leaving out degrees 0) : %w\n',[AvInDegree]), 
    writef('Average out-degree per node (leaving out degrees 0): %w\n',[AvOutDegree]).


% prevent division by zero
average([], 0):- !.

average(ListOfInts, Average):-
    sum(ListOfInts, Sum), 
    length(ListOfInts, N), 
    Average is Sum/N.


sum([], 0).

sum([N|ListOfInts], Sum):-
    sum(ListOfInts, SumSoFar), 
    Sum is SumSoFar + N.


% path_segment(A, B, Graph, PathSegment, Length)
%
% returns each PathSegment in Graph of length Length between A and B
% i.e., each path which does not contain outgoing branching points 
% between A and B
%
path_segment(A, B, Graph, [A|Path], Length):-
    path(A, B, Graph, [A|Path], Length), 
    % strip off A and B at the outer ends
    reverse(Path, [_LastNode|AllButLastAndFirst]), 
    forall(member(Node, AllButLastAndFirst), 
         (
          in_degree(Graph, Node, 1),
          out_degree(Graph, Node, 1)
         )
    ).




% max_path_segment(A, B, Graph, Path, Length)
%
% returns each longest Path Segment in Graph of 
% length Length between A and B
% which does not contain incoming or outgoing branches 
% except possibly at the start and end point
%
max_path_segment(A, B, Graph, Path, Length):-
    path_segment(A, B, Graph, Path, Length),
    impossible_to_expand(Graph, A, B).



impossible_to_expand(Graph, A, B):-
    impossible_to_expand_left(Graph, A),
    impossible_to_expand_right(Graph, B),!.
    

% impossible to expand path segment to the left 
% if A is outgoing branching point
%
impossible_to_expand_left(Graph, A):-    
    out_degree(Graph, A, Degree), 
    Degree > 1.

% impossible to expand path segment to the left 
% if A is starting point
%
impossible_to_expand_left(Graph, A):-
    real_start_node(Graph, A).

% impossible to expand path segment to the left 
% if A is the input state
%
impossible_to_expand_left(_Graph, input).
% impossible_to_expand_left(Graph, 0).


% impossible to expand path segment to the right 
% if B is outgoing branching point
%
impossible_to_expand_right(Graph, B):-
    out_degree(Graph, B, Degree), 
    Degree > 1.

% impossible to expand path segment to the right 
% if B is incoming branching point
%
impossible_to_expand_right(Graph, B):-
    in_degree(Graph, B, Degree), 
    Degree > 1.

% impossible to expand path segment to the right
% if B is end point
%
impossible_to_expand_right(Graph, B):-
    end_node(Graph, B).




% connections(Graph, Node, InConnections, OutConnections)
%
% returns lists of Inconnections and OutConnections connected 
% to Node in Graph
%
connections(Graph, Node, InConnections, OutConnections):-
    % in_connections(Graph, Node, InConnections),
    % out_connections(Graph, Node, OutConnections).
    in_connections_short(Graph, Node, InConnections),
    out_connections_short(Graph, Node, OutConnections).


in_connections(Graph, Node, InConnections):-
    graph_node(Graph, Node), 
    findall(graph_edge(Graph, A, Node, Rel), 
              graph_edge(Graph, A, Node, Rel), InConnections).

out_connections(Graph, Node, OutConnections):-
    graph_node(Graph, Node), 
    findall(graph_edge(Graph, Node, B, Rel), 
              graph_edge(Graph, Node, B, Rel), OutConnections).


in_connections_short(Graph, Node, InConnections):-
    graph_node(Graph, Node), 
    findall(A, 
              graph_edge(Graph, A, Node, _Rel), InConnections).

out_connections_short(Graph, Node, OutConnections):-
    graph_node(Graph, Node), 
    findall(B, 
              graph_edge(Graph, Node, B, _Rel), OutConnections).


% same_connections(Graph, A, B)
%
% returns each pair of nodes A & B for which 
% the connections are identical
%
same_connections(Graph, A, B):-    
    connections(Graph, A, In, Out),
    connections(Graph, B, In, Out),
    A \== B.



% same_end_node(Graph, A, B)
%
% returns each pair of nodes A & B which 
% result in the same end_node
%
same_end_node(Graph, A, B):-    
    end_node(Graph, EndNode), 
    path(A, EndNode, Graph, _PathA, _LengthA),
    path(B, EndNode, Graph, _PathB, _LengthB).


% possible_end_nodes(Graph, Node, EndNodes)
%
% returns all possible EndNodes in Graph, which 
% can be reached from Node
%
possible_end_nodes(Graph, Node, EndNodes):-
    graph_node(Graph, Node),
    setof(EndNode, 
          (end_node(Graph, EndNode), 
           % P^L^path(Node, EndNode, Graph, P, L)
           connected(Graph, Node, EndNode)
          ), EndNodes).




% possible_successor_nodes(Graph, Node, SuccNodes)
%
% returns all possible Successor Nodes in Graph, which 
% can be reached from Node
%
possible_successor_nodes(Graph, Node, SuccNodes):-
    graph_node(Graph, Node),
    setof(SuccNode, 
          (graph_node(Graph, SuccNode), 
           connected(Graph, Node, SuccNode)
          ), SuccNodes).





% connected only checks for one path, to speed things up 
% a little...
% 
connected(Graph, A, B):-
    graph_node(Graph, A), 
    graph_node(Graph, B), 
    path(A, B, Graph, _P, _L),!.

   

% in_degree(Graph, Node, Degree)
%
% determines Degree, the number of incoming links 
% from Node in Graph
%
in_degree(Graph, Node, Degree):-
    findall(_, graph_edge(Graph, _FromNode, Node, _Rel), L), 
    length(L, Degree).
 


% out_degree(Graph, Node, Degree)
%
% determines Degree, the number of outgoing links 
% from Node in Graph
%
out_degree(Graph, Node, Degree):-
    findall(_, graph_edge(Graph, Node, _ToNode, _Rel), L), 
    length(L, Degree).
 

:-dynamic(graph_node/2).
:-dynamic(graph_edge/4).
:-multifile(graph_node/2).
:-multifile(graph_edge/4).


% state graph 
%
% default, included for backwards compatibility
% better to use the more specific form below
%
graph_node(state_graph, Node):-
    graph_node(state_graph(original, input), Node).
%
%
% graph_node(state_graph(TypeOfEdges, InputOrNot), Node)
%
% TypeOfEdges = original/aggregated/both
%   specifies which types of edges are considered, and hence, which nodes
%
% InputOrNot = input/noinput
%   specifies whether the input-state should be included or not. 
%
%
% original state graph
%
% no input, just the states defined in garp
graph_node(state_graph(original, noinput), Node):-
    engine:state(Node, _).

% including input
graph_node(state_graph(original, input), Node):-
    graph_node(state_graph(original, noinput), Node).

% input = 0 
graph_node(state_graph(original, input), 0).



% default is both
% included for backwards compatibility
%
graph_edge(state_graph, A, B, to):-
    graph_edge(state_graph(both, input), A, B, to).

% original state graph, including transitions from input
%  
graph_edge(state_graph(original, input), A, B, to):-
    rec_transition(A, B).

% original state graph, without transitions from input
% 
graph_edge(state_graph(original, noinput), A, B, to):-
    rec_real_transition(A, B).


% aggregated state graph
%  
graph_edge(state_graph(aggregated, input), A, B, to):-
    rec_aggr_transition(A, B).
%
% no input
graph_edge(state_graph(aggregated, noinput), A, B, to):-
    rec_aggr_transition(A, B),
    A \== input,
    A \== 0.

% both original and aggregated state graph
%  
graph_edge(state_graph(both, InputOrNot), A, B, to):-
    graph_edge(state_graph(original, InputOrNot), A, B, to).

graph_edge(state_graph(both, InputOrNot), A, B, to):-
    graph_edge(state_graph(aggregated, InputOrNot), A, B, to), 
    % don't include edges already included by the previous clause
    not(graph_edge(state_graph(original, InputOrNot), A, B, to)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% causal_graph
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


graph_node(causal_graph(N), Node):-
    find_quantity_value(N, QName, _RealVal, _QVal, _Der),
    Node = QName.
%    Node = quantity_val_der(QName, QVal, Der).

% only consider causal relations which have an actual effect
%
graph_edge(causal_graph(N), A, B, CausalRel):-
    effect_status(N, causal_relation(A, CausalRel, B), Dir, Status),
    Dir \== none, 
    Status \== none,
    Status \== submissive.
    

% causal_graphB
%
graph_node(causal_graphB(N), Node):-
    find_quantity_value(N, QName, _RealVal, _QVal, _Der),
    Node = QName.
%    Node = quantity_val_der(QName, QVal, Der).

% consider all causal relations, regardless of their effect-status
%
graph_edge(causal_graphB(N), A, B, CausalRel):-
    effect_status(N, causal_relation(A, CausalRel, B), _Dir, _Status).
%    Dir \== none, 
%    Status \== none,
%    Status \== submissive.
    


% Entity-Relationship-Attribute graph
% 
graph_node(er_graph(N), Node):-
    find_instance(N, Node, _Type).

graph_edge(er_graph(N), A, B, type(relation, A, Rel, B)):-
    find_entity_attribute(N, A, Rel, B).

% it may be handy to include the edge in reverse direction too
% 
% graph_edge(er_graph(N), B, A, type(relation, A, Rel, B)):-
%    find_entity_attribute(N, A, Rel, B).


graph_edge(er_graph(N), A, B, type(attribute, A, Attr, B)):-
    find_other_attribute(N, A, Attr, B).


test_er(N):-
    graph_node(er_graph(N), A),
    graph_edge(er_graph(N), A, B, type(_Type, A, AttrOrRel, B)),
    writef('%d %d %d\n', [A, AttrOrRel, B]), 
    fail.
    
test_er(_N).



% entity_isa_graph
%
% this way, the top node (nil?) is not found!
graph_node(entity_isa, Node):-
    engine:isa(Node, _).


graph_edge(entity_isa, A, B, isa):-
    engine:isa(A, B).


%gp3 rewrite for mf_isa_graph because we do not (usualy) use recorded(library_index,..)
%data anymore, so just use system_structures

graph_node(mf_isa,Name):-
	engine:system_structures(Name,_Supertype,_Cond,_Givens).
%
graph_node(mf_isa, Name):-
	%gp3 0.1: we added Garp3 names (static, process, agent, with process moving to front)
	%to head of the list of possible topnodes
	%but we left the old names as well for legacy etc  
	
    member(Name, [static, process, agent,
    	  description_view, 
		  composition_view, 
		  decomposition_view, 
		  qualitative_state ]).
%
graph_edge(mf_isa, A, B, isa):-
    engine:system_structures(A, isa(Parents), _Cond, _Givens),
    memberchk(B, Parents),
    graph_node(mf_isa, B). 


/* 
 * isa_predecessor(X, Y, N)
 * 
 * succeeds iff X is a isa_predecessor of Y, in terms of 
 * N number of 'isa' links.
 *
 */

isa_predecessor(_X, _Y, N):-
    nonvar(N), 
    N =< 0, !, fail.

isa_predecessor(X, Y, 1):-
    engine:isa(X, Y).

isa_predecessor(X, Z, N):-
    engine:isa(X, Y),
    isa_predecessor(Y, Z, NewN),
    N is NewN + 1.


%gp3 rewrite for my_apply graph because we do not (allways) use recorded(library_index,..)
%data anymore, so just use system_structures

% mf_applies_to_graph
%
graph_node(mf_apply, Name):-
    % nodes are the same as mf_isa_graph except the MF categories
    engine:system_structures(Name, _Supertype, _Cond, _Givens).


graph_edge(mf_apply, A, B, applies_to):-
    engine:system_structures(B, _Supertype, conditions(Cond), _Givens),
    % Relation: Parent B mentions Child A in conditions
    member(system_structures(S), Cond), 
    member(A, S),
    graph_node(mf_apply, A).


% sublist(S, L) 
%
% finds any sublist S of L, preserving the order of elements
% 
sublist(S, L):-
    conc(_L1, L2, L),
    conc(S, _L3, L2).


% sublist_index(List, Index, N, SubList) 
%
% finds any Sublist of List, starting at Index, with N elements, 
% preserving the order of elements
% 
sublist_index(List, Index, Length, SubList):-
    conc(L1, L2, List),
    conc(SubList, _L3, L2),
    length(L1, Index), 
    length(SubList, Length). 

% sublist_index1(List, Index, N, SubList) 
%
% finds only the first Sublist of List, starting at Index, with N elements, 
% preserving the order of elements
% 
sublist_index1(List, Index, Length, SubList):-
    sublist_index(List, Index, Length, SubList),!.


% sublist_skip(S, L) 
%
% finds any sublist S of L, preserving the order of elements, 
% but allowing skips in S between elements of L, e.g., 
% sublist_skip([1,3], [1,2,3,4]).
%
%
% the empty list is a sublist of any list
%
sublist_skip([], _L).

% first element of SubList is contained in List, 
% so continue with the rest of both lists
%
sublist_skip([X|RestSubList], [X|RestList]):-
    sublist_skip(RestSubList, RestList).

% first element of SubList is not contained in List, 
% so continue with the same Sublist and the rest of List
%
sublist_skip([X|SubList], [Y|RestList]):-
    X \== Y,
    sublist_skip([X|SubList], RestList).


% branching(Graph)
%
% succeeds when Graph contains a right branching node
%
branching(Graph):-
	% check if there are two edges from the same node
	graph_edge(Graph, A, B, _Rel1), 
	graph_edge(Graph, A, C, _Rel2),
	B \== C.
 


