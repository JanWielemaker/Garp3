/*  File:    pllib
    Purpose: Some non specific predicates
    Author:  Martin Reinders & Bert Bredeweg
    Date:    August 1989
    Part-of: GARP (version 1.0)

    Copyright (c) 1989, University of Amsterdam. All rights reserved.

*/

% merge +list 1 with +list 2 to +set3 (no elements double)

smerge(L1, L2, R):-
	list_to_set(L1, SL1), 
	list_to_set(L2, SL2), 
	union(SL1, SL2, R).

% -arg3 is +list1 without any element that is also in +list2

without([], _, []).

without([H|T], L2, L3):-
	memberchk(H, L2), 
	!, 
	without(T, L2, L3).

without([H|T], L2, [H|L3]):-
	without(T, L2, L3).

% first in list

first(H, [H|_]).
% last is system predicate in SWI prolog

% replace element +Arg1 in +Arg2 with +Arg3 to -Arg4

replace_nth1(1, [_|T], New, [New|T]):- !.
replace_nth1(N, [H|T], New, [H|NT]):-
	N > 1, 
	NN is N - 1, 
	replace_nth1(NN, T, New, NT).

% all elements of L1 in same order in L2 ?

sub_list([], _).
sub_list([H|T1], [H|T2]) :- sub_list(T1, T2).
sub_list(L, [_|T2]) :- sub_list(L, T2).

% all elements of L1 in same order and contiguous in L2 ?

part_list(L1, L2):- first_part(L1, L2).
part_list(L1, [_|L2]) :- part_list(L1, L2).

first_part([], _).
first_part([H|T1], [H|T2]):- first_part(T1, T2).

% all elements of L1 in L2 ?

sub_set([], _).
sub_set([H|T], L) :- common_select(L, H, NL), sub_set(T, NL).

% do_once performs the call once, asserts it if succes (result instantiated), 
% fails, thus cleaning term stack and retracts the result on backtracking

do_once(C):- once(C), recorda(do_once_result, C), fail.
do_once(C):- recorded(do_once_result, C, Reference), erase(Reference), !.

% member for a comma list (a, b, c, etc) or (a)

comma_member(A, (A, _)).
comma_member(A, (_, B)):- comma_member(A, B).
comma_member(A, A):- A \= (_, _).

pr_l_tab(_, []).
pr_l_tab(N, [H|T]):- tab(N), write_ln(H), pr_l_tab(N, T).

% a real subset

% take first and nothing of tail
a_subset([H], [H|_]).
% take first and something of tail
a_subset([H|T1], [H|T2]):- a_subset(T1, T2).
% don't take first, but something of tail
a_subset(T1, [_|T2]):- a_subset(T1, T2).

% append lists and remove duplicates

append_sort(L1, L2, L3):- append(L1, L2, LT), sort(LT, L3).
% sort remove duplicates

% difference between two sets

difference([], L, L).
difference([H|T], L, NT):-
	common_select(L, H, NL), 
	!, 
	difference(T, NL, NT).	
difference([H|T], L, [H|NT]):-
	difference(T, L, NT).

