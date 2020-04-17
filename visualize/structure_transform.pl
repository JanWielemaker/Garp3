/*****************************************************************************************************
Garp3 0.1 Based on main source of visigarp (main.pl). This file is split in parts and edited.
All visiualize source based on visigarp 2.05, designed and implemented by Anders Bouwer 18/08/1999
Original copyright: (C) 2004 University of Amsterdam. All rights reserved.

Now:
PART OF Garp3. SEE COPYRIGHT NOTICE.
*******************************************************************************************************/

%loaded into module (namespace) visualize

% From 7 May 2001, some GARP ontological categories 
% are/should be renamed as follows: 
% system_elements are called entities & relations? 
% parameters are called quantities
% parameter values are called quantity values
% parameter relations are called dependencies
% system_structures are called model fragments
% attributes are called relations (this is not 
% optimal yet - attributes within an entity 
% should be called attributes, and attributes 
% involving multiple entities should be called 
% relations
rename_garp_category('system_elements', 'entities and relations'):-!.
rename_garp_category('parameters', 'quantities'):-!.
rename_garp_category('par_values', 'quantity values'):-!.
rename_garp_category('par_relations', 'dependencies'):-!.
rename_garp_category('system_structures', 'model fragments'):-!.
rename_garp_category(Str, Str).


% sl(CS, StrList, Tab, Max):-
%
% transforms an unknown, possibly complex structure 
% (which can be a list or a term, or atom) into a 
% list of strings, indented for every term-structure
%
	
% CS is an empty list
sl([], [], _Tab, 0).

% CS is an atom
sl(CS, [Str], Tab, Max):-
	atom(CS),!,
	tab_str2(Tab, CS, Str),
	string_length(Str, Max).

% CS is a non-empty list:
% look at the first element (which may have a complex structure itself)
% do the rest
% and then merge the results into one list
%
sl([FirstCS|Rest], StrList, Tab, Max):-
        % writef('sl 1: %w \n', [FirstCS]), 
        % my_write_list(Rest),
	sl(FirstCS, FirstStrList, Tab, FirstMax),
	sl(Rest, RestStrList, Tab, RestMax),
	conc(FirstStrList, RestStrList, StrList),!,
	max(FirstMax, RestMax, Max).


% CS is a term with a list as argument:
% take the title and let the rest indent
%
sl(CS, [TitleStr|RestStrList], Tab, NewMax):-
	CS =.. [OldTitle, Rest],
	is_list(Rest),!,
	rename_garp_category(OldTitle, Title),
	% add a colon after the title
 	get(string('%s:', Title),
		value, TitleStr0),
	tab_str2(Tab, TitleStr0, TitleStr),
%	string_length(TitleStr, L), 
%	text_margin_spaces(_Left, Right), 
%	NewTab is L + Right,
	NewTab is Tab + 1,
	sl(Rest, RestStrList, NewTab, NewMax).


% CS is a term with a number of arguments:
% For events, this is specified in further detail
%
% Value event: change - should be increase or decrease to/from point/interval!
sl(CS, [Str], Tab, Max):-
	CS = value_event((_A->_B), Q1, V1, _D1, value_change(_VC), V2, _D2), 
	% create a string
        swritef(Str0, '%d changes value:   %d -> %d ', [Q1, V1, V2]),
	tab_str2(Tab, Str0, Str),
	string_length(Str, Max).

% Value event: becomes known 
sl(CS, [Str], Tab, Max):-
	CS = value_event((_A->_B), Q1, _V1, _D1, becomes_known(_VC), V2, _D2), 
	% create a string
        swritef(Str0, '%d is now known to be %d ', [Q1, V2]),
	tab_str2(Tab, Str0, Str),
	string_length(Str, Max).

% Value event: becomes unknown 
sl(CS, [Str], Tab, Max):-
	CS = value_event((_A->_B), Q1, V1, _D1, becomes_unknown, _V2, _D2), 
	% create a string
        swritef(Str0, '%d was %d but becomes unknown ', [Q1, V1]),
	tab_str2(Tab, Str0, Str),
	string_length(Str, Max).

% Derivative event: start to increase
sl(CS, [Str], Tab, Max):-
	CS = value_event((_A->_B), Q1, _V1, _D1, start_to_increase_from(V2), _V2, _D2), 
	% create a string
        swritef(Str0, '%d is %d and starts to increase ', [Q1, V2]),
	tab_str2(Tab, Str0, Str),
	string_length(Str, Max).

% Derivative event: start to decrease
sl(CS, [Str], Tab, Max):-
	CS = value_event((_A->_B), Q1, _V1, _D1, start_to_decrease_from(V2), _V2, _D2), 
	% create a string
        swritef(Str0, '%d is %d and starts to decrease ', [Q1, V2]),
	tab_str2(Tab, Str0, Str),
	string_length(Str, Max).

% Derivative event: increase stops
sl(CS, [Str], Tab, Max):-
	CS = value_event((_A->_B), Q1, _V1, _D1, increase_stops_at(V2), _V2, _D2), 
	% create a string
        swritef(Str0, '%d reaches %d and stops increasing ', [Q1, V2]),
	tab_str2(Tab, Str0, Str),
	string_length(Str, Max).

% Derivative event: decrease stops
sl(CS, [Str], Tab, Max):-
	CS = value_event((_A->_B), Q1, _V1, _D1, decrease_stops_at(V2), _V2, _D2), 
	% create a string
        swritef(Str0, '%d reaches %d and stops decreasing ', [Q1, V2]),
	tab_str2(Tab, Str0, Str),
	string_length(Str, Max).

% Derivative event: becomes known 
sl(CS, [Str], Tab, Max):-
	CS = value_event((_A->_B), Q1, _V1, _D1, der_becomes_known_at(V2, D2), V2, D2),
        % create a string for derivative
        derivative_str(D2, D2Str),
        swritef(Str0, '%d is %d and now known to %d', [Q1, V2, D2Str]),
	tab_str2(Tab, Str0, Str),
	string_length(Str, Max).

% Derivative event: becomes unknown 
sl(CS, [Str], Tab, Max):-
	CS = value_event((_A->_B), Q1, _V1, D1, der_becomes_unknown_at(V2), V2, _D2),
        % create a string for derivative
        derivative_str(D1, D1Str),
        swritef(Str0, '%d is %d but not longer known to %d ', [Q1, V2, D1Str]),
	tab_str2(Tab, Str0, Str),
	string_length(Str, Max).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Inequality Events
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Inequality between derivatives introduced
% It's not certain that the second argument is a derivative!
sl(CS, [Str], Tab, Max):-
	CS = inequality_introduced((_A->_B), RelTerm),
        RelTerm =.. [_Rel, _Q1, _Q2orValorFormula],
        % visualize(Rel, Q1, Q2orValorFormula, _VRel0, V1, V2, _Dir),
        % derivative_relation(Rel, VRel),!, % gives <=> instead of d<=>
        visualize_formula(RelTerm, RelStr),
        % create a string 
        swritef(Str0, 'introduced: %w ', [RelStr]),
        % swritef(Str0, 'introduced: d(%d) %d d(%d) ', [V1, VRel, V2]),
	tab_str2(Tab, Str0, Str),
	string_length(Str, Max).

% Inequality between values introduced
% It's not certain that the second argument is a value!
sl(CS, [Str], Tab, Max):-
	CS = inequality_introduced((_A->_B), RelTerm),
        RelTerm =.. [_Rel, _Q1, _Q2orValorFormula],
        visualize_formula(RelTerm, RelStr),
        % visualize(Rel, Q1, Q2orValorFormula, VRel, V1, V2, _Dir),
        % create a string 
        swritef(Str0, 'introduced: %w ', [RelStr]),
        % swritef(Str0, 'introduced: %d %d %d ', [V1, VRel, V2]),
	tab_str2(Tab, Str0, Str),
	string_length(Str, Max).


% Inequality between derivatives removed
% It's not certain that the second argument is a derivative!
sl(CS, [Str], Tab, Max):-
	CS = inequality_removed((_A->_B), RelTerm),
        RelTerm =.. [_Rel, _Q1, _Q2orValorFormula],
        visualize_formula(RelTerm, RelStr),
%        visualize(Rel, Q1, Q2orValorFormula, _VRel0, V1, V2, _Dir),
%        derivative_relation(Rel, VRel),!, % gives <=> instead of d<=>
        % create a string 
%        swritef(Str0, 'removed: d(%w) %w d(%w) ', [V1, VRel, V2]),
        swritef(Str0, 'removed: %w ', [RelStr]),
	tab_str2(Tab, Str0, Str),
	string_length(Str, Max).
/*
% Inequality between values removed
% It's not certain that the second argument is a value!
sl(CS, [Str], Tab, Max):-
	CS = inequality_removed((_A->_B), RelTerm),
        RelTerm =.. [Rel, Q1, Q2orValorFormula],
        visualize(Rel, Q1, Q2orValorFormula, VRel, V1, V2, _Dir),
        % create a string 
        swritef(Str0, 'removed: %d %d %d ', [V1, VRel, V2]),
	tab_str2(Tab, Str0, Str),
	string_length(Str, Max).
*/


% Inequality event between values: X R1 Y  ->  X R2 Y
sl(CS, [Str], Tab, Max):-
	CS = inequality_event((_A->_B), between_val(from(Q1, R1, Q2), to(Q1, R2, Q2))),
        % create a string 
        swritef(Str0, 'changed: %d %d %d  ->  %d %d %d ', [Q1, R1, Q2, Q1, R2, Q2]),
	tab_str2(Tab, Str0, Str),
	string_length(Str, Max).


% Inequality event between derivatives: dX R1 dY  ->  dX R2 dY
sl(CS, [Str], Tab, Max):-
	CS = inequality_event((_A->_B), between_der(from(Q1, R1, Q2), to(Q1, R2, Q2))),
        % create a string 
        swritef(Str0, 'changed: d(%d) %d d(%d)  ->  d(%d) %d d(%d) ', [Q1, R1, Q2, Q1, R2, Q2]),
	tab_str2(Tab, Str0, Str),
	string_length(Str, Max).

% rest case: unknown inequality event?


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% causal events
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% changed: effect status of causal relation 
sl(CS, [Str], Tab, Max):-
	CS = changed((_A->_B), effect_status(causal_relation(Q1, R1, Q2), 
                                 from(Dir1, Status1), to(Dir2, Status2))),
        % create a string 
        visualize_effect_status_change(from(Dir1, Status1), 
                                       to(Dir2, Status2), EffStr),
        swritef(Str0, 'changed: %w %w %w %w ', 
		[Q1, R1, Q2, EffStr]),
	tab_str2(Tab, Str0, Str),
	string_length(Str, Max).


% introduced: causal relation with its effect status
sl(CS, [Str], Tab, Max):-
	CS = introduced((_A->_B), 
               effect_status(causal_relation(Q1, R1, Q2), 
                               Dir1, Status1)), 
        % create a string 
        visualize_effect_dir_status(Dir1, Status1, DirStatusStr),
        swritef(Str0, 'introduced: %w %w %w: %w ', 
		[Q1, R1, Q2, DirStatusStr]),
	tab_str2(Tab, Str0, Str),
	string_length(Str, Max).

% removed: causal relation with its effect status
sl(CS, [Str], Tab, Max):-
	CS = removed((_A->_B), 
               effect_status(causal_relation(Q1, R1, Q2), 
                               Dir1, Status1)), 
        % create a string 
        visualize_effect_dir_status(Dir1, Status1, DirStatusStr),
        swritef(Str0, 'removed: %w %w %w: %w ', 
		[Q1, R1, Q2, DirStatusStr]),
	tab_str2(Tab, Str0, Str),
	string_length(Str, Max).



% Correspondence calculation events 
% 
sl(CS, [Str], Tab, Max):-
	CS = corr_calc((_A->_B), _Q1, _VC1, CorrRelTerm, _Q2, _VC2), 
	% create a string
        visualize_formula(CorrRelTerm, CorrRelStr), 
        % visualize value changes
        % visualize_value_change(Q1, VC1, QVC1Str), 
        % visualize_value_change(Q2, VC2, QVC2Str), 
        swritef(Str0, '%w ', [CorrRelStr]),
	tab_str2(Tab, Str0, Str),
	string_length(Str, Max).










% dependency events
%
% removed: causal relation with its effect status
sl(CS, [Str], Tab, Max):-
	CS = removed((_A->_B), par_relations, RelTerm),
        % create a string 
        visualize_formula(RelTerm, RelStr), 
        swritef(Str0, 'removed: %w ', [RelStr]),
	tab_str2(Tab, Str0, Str),
	string_length(Str, Max).

% introduced: causal relation with its effect status
sl(CS, [Str], Tab, Max):-
	CS = added((_A->_B), par_relations, RelTerm),
        % create a string 
        visualize_formula(RelTerm, RelStr), 
        swritef(Str0, 'introduced: %w ', [RelStr]),
	tab_str2(Tab, Str0, Str),
	string_length(Str, Max).


% model fragment events
%
% removed: MFList
sl(CS, [Str|MFStrList], Tab, Max):-
	CS = removed((_A->_B), mf, MFList),
        % create a string 
	NewTab is Tab + 3,
        sl(MFList, MFStrList, NewTab, _), 		   
        swritef(Str0, 'removed: ', []),
	tab_str2(Tab, Str0, Str),
	string_length(Str, Max).

% introduced: MFList
sl(CS, [Str|MFStrList], Tab, Max):-
	CS = added((_A->_B), mf, MFList),
        % create a string 
	NewTab is Tab + 3,
        sl(MFList, MFStrList, NewTab, _), 		   
        swritef(Str0, 'introduced: ', []),
	tab_str2(Tab, Str0, Str),
	string_length(Str, Max).

% replaced: MFList1 by MFList2
sl(CS, StrList, Tab, Max):-
	CS = replaced((_A->_B), mf, MFList1, MFList2), 
        % create a string 
	NewTab is Tab + 3,
        sl(MFList1, MFStrList1, NewTab, _), 		   
        sl(MFList2, MFStrList2, NewTab, _), 		   
        % add tab to the string for 'replaced by <MFList2>'
	% tab_str2(Tab, MFStr2, MFStr2Tab),
        swritef(Str1a, 'replaced: ', []),
	tab_str2(Tab, Str1a, Str1),
        swritef(Str2a, 'by:       ', []),
	tab_str2(Tab, Str2a, Str2),
        conc([Str1|MFStrList1], [Str2|MFStrList2], StrList),
	string_length(Str1, Max).





% Structure events 

% introduced: entity, attribute or relation
sl(CS, [Str], Tab, Max):-
	CS = added((_A->_B), system_elements, Term),
        % create a string 
        visualize_system_elements(Term, TermStr), 
        swritef(Str0, 'introduced: %w ', [TermStr]),
	tab_str2(Tab, Str0, Str),
	string_length(Str, Max).

% introduced: quantity
sl(CS, [Str], Tab, Max):-
	CS = added((_A->_B), parameters, QuantityTerm),
        % create a string 
        visualize_quantity(QuantityTerm, QTermStr),
        swritef(Str0, 'introduced: %w ', [QTermStr]),
	tab_str2(Tab, Str0, Str),
	string_length(Str, Max).

% removed: entity, attribute or relation
sl(CS, [Str], Tab, Max):-
	CS = removed((_A->_B), system_elements, Term),
        % create a string 
        visualize_system_elements(Term, TermStr), 
        swritef(Str0, 'removed: %w ', [TermStr]),
	tab_str2(Tab, Str0, Str),
	string_length(Str, Max).

% removed: quantity
sl(CS, [Str], Tab, Max):-
	CS = removed((_A->_B), parameters, QuantityTerm),
        % create a string 
        visualize_quantity(QuantityTerm, QTermStr),
        swritef(Str0, 'removed: %w ', [QTermStr]),
	tab_str2(Tab, Str0, Str),
	string_length(Str, Max).




/* To do: other kinds of events
% v causal events 
% v dependency events
% v mf events
% - corr (calc) events
%
%  event
sl(CS, [Str], Tab, Max):-
	CS = 
        % create a string 
        swritef(Str0, 'd%d %d d%d  ->  d%d %d d%d ', [Q1, R1, Q2, Q1, R2, Q2]),
	tab_str2(Tab, Str0, Str),
	string_length(Str, Max).

*/

/*
% lists: call sl recursively
%
sl([X|CS], [Str], Tab, Max):-
	% atomize into a string
	atomize(X, Str0), 
        sl(CS, [Str1], Tab, _Max2), 
        swritef(Str, '%w, %w', [Str0, Str1]),
	tab_str2(Tab, Str, TabStr),
	string_length(TabStr, Max).
*/


/*
% question generated by question generator:
% peel off 'question' 
%
%sl(CS, [QuestionStr, AnswerStr], _Tab, Max):-
% sl(CS, [QuestionStr, answer(AnswerStr)], _Tab, Max):-
sl(CS, [QuestionStr1, answer(AnswerStr1)], _Tab, Max):-
	CS =.. [question, TypeNr, Question, Answer],
	% atomize into a string
	% tab_str2(Tab, Question, QuestionStr0),
	% tab_str2(Tab, Answer, AnswerStr0),
        swritef(QuestionStr1, '  %w: %w', [TypeNr, Question]),
        swritef(AnswerStr1,   '         %w', [Answer]),
%       this is no longer necessary!
%	insert_newlines(QuestionStr1, MaxLength, chartab(6), QuestionStr),
%	insert_newlines(AnswerStr1, MaxLength, chartab(10), AnswerStr),
	% atomize([TypeNr, Question, Answer], Str0), !,
        % this way to calculating string length does not really make sense!
%        swritef(Str, '%w\n%w', [QuestionStr, AnswerStr]),     
        swritef(Str, '%w\n%w', [QuestionStr1, AnswerStr1]),     
	string_length(Str, Max).
*/



% all other terms:
% don't decompose any further - just atomize into a string
%
sl(CS, [Str], Tab, Max):-
	CS =.. [_Title| _Rest],
	% atomize into a string
	atomize(CS, Str0), !,
	tab_str2(Tab, Str0, Str),
	string_length(Str, Max).

% tab_str(N, Str, TabStr)
%
% adds N spaces in front of Str to form TabStr
%
tab_str(N, Str, TabStr):-
	% construct empty string of N spaces
	empty_str(N, EmptyStr),
	% put empty string in front of the content string
 	get(string('%s%s', EmptyStr, Str),
		value, TabStr).


test_sl(CS, Max):-
	sl(CS, StrList, 0, Max),
	my_write_list(StrList).


tab_str3(N, Str, TabStr):-
	% construct empty string of N*20 spaces
	M is N * 20, 
	sformat(EmptyStrCode, '~w~w~w', ['~t~', M, '|']),
	sformat(EmptyStr, EmptyStrCode, []),	
	% put empty string in front of the first string
 	get(string('%s%s', EmptyStr, Str),
		value, TabStr).

tab_str2(0, Str, Str).


tab_str2(N, Str, TabStr):-
	% construct empty string of N tabs
	empty_tab_str(N, EmptyStr),
	% put empty string in front of the first string
 	get(string('%s%s', EmptyStr, Str),
		value, TabStr).


% empty_tab_str(N, Str)
%
% construct empty string of N spaces 
%
empty_tab_str(N, _Str):-
	N < 0, !, 
	fail.

empty_tab_str(0, '').

empty_tab_str(N, Str):-
	N > 0, !, 
	M is N - 1, 
	empty_tab_str(M, Str1),
	get(string('\t%s', Str1), value, Str).

	
	
% empty_str(N, Str)
% 
% create empty string of N spaces
%
empty_str(N, _Str):-
	N < 0, !, 
	fail.

empty_str(0, '').

empty_str(N, Str):-
	N > 0, !, 
	M is N - 1, 
	empty_str(M, Str1),
	get(string('%s ', Str1), value, Str).
