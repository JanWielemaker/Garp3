:- open('trace.out', write, traceout).
:- at_halt(close(traceout)).

prolog_trace_interception(Port, Frame, _PC, _) :-
	prolog_frame_attribute(Frame, goal, Goal),
	tracing(Goal),
	numbervars(Goal, '$VAR', 0, _),
	format(traceout, '~w: ', [Port]),
	print(traceout, Goal),
/*	write_term(traceout, Goal,
		   [ quoted(true),
		     numbervars(true),
		     portray(true)
		   ]), */
	nl(traceout),
	fail.
prolog_trace_interception(_Port, _Frame, _PC, continue).

tracing(list_map(_,_)).
tracing(map_list(_,_)).
tracing(map_union(_,_,_)).
tracing(map_union_unique(_,_,_)).
tracing(map_intersection(_,_,_)).
tracing(map_without_intersection(_, _,_,_)).
tracing(map_count(_,_)).
tracing(map_replace(_,_,_,_)).
tracing(map_difference(_,_,_)).
%tracing(add_relation(_,_,_)).

portray(Map) :-
	string(Map),
	map_list(Map, List),
	write(map(List)).
