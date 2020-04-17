/*
equation_history_view.pl
PART OF GARP 3
Legacy visigarp code. Changed only where gp3 mentioned.
Original code written by Anders Bouwer, 2005
Moved some declarations (dynamic..) to visualize_startup_garp3
*/

% gen_equation_history_menu(F)
%
gen_equation_history_menu(F):-
	%gp3 0.2 changed the buttons to imgButtons or toggleImgButtons
	%and did a bit of a rewrite in the left hand bar: no menus, just one dlg of  buttons
	%gp3 0.3.13 Added assistance bar (help button) + stretching/shrinking settings (just type them all out and it works fine)
	
	
	get_selected_states_or_path(F, InputStateList),
	delete(InputStateList, 0, StateList),
	StateList \== [],!,
	atomize(StateList, StateListStr),  %gp3 moved this one a bit up
	new(F2, my_frame), 
	F2->>icon(@simulate_icon),
	send(F2, label, 'Equation history view'), 
	
	%%%%COMMENT: SHOULD THIS NOT BE A ONE TIME FLAG?? (listing(preferred_equation_type) after some runs gives quite a list) [but not our pakkie on]
	
        % default: preferred_equation_type(F2, no_der)
        asserta(preferred_equation_type(F2, no_der)),
        % default: preferred_equation_format(F2, short)
        asserta(preferred_equation_format(F2, long)), %gp3: long is non-legacy, we prefer that

	send(F2, append, new(P, picture)),
	P->>hor_stretch(100),
	P->>ver_stretch(100),
	P->>hor_shrink(100),
	P->>ver_shrink(100),
	
	
	send(new(D2, dialog), below, P), 
	send(D2, hor_stretch, 100),
	D2->>hor_shrink(100),
	D2->>ver_stretch(0),
	D2->>ver_shrink(0),
	new(PSButton, imgButton(postscript, message(F2, postscript), tt:= 'Save Diagram to EPS file')),
	new(CloseButton, imgButton(close, message(F2, destroy),tt:='Close this window')),

        new(D, dialog(string('Graph View'))),
	D->>hor_stretch(0),
	D->>hor_shrink(0),
	D->>ver_stretch(100),
	D->>ver_shrink(100),
	
	send(D, append, new(B, list_browser)),
	send(B, multiple_selection, @on),
	send(B, width, 35),
	send(B, height, 15), 

        preferred_equation_type(F2, TypeOfEquations),
        preferred_equation_format(F2, LongOrShort),
	fill_equations(B, StateList, TypeOfEquations, LongOrShort),
	send(B, open_message,
			message(B?selection, for_all, 
					message(@prolog, gen_equation_history_table, 
						F, F2, P, @arg1?object
					)
			)
	),
	

	%gp3 0.2: we removed the choice menus and used grouped toggleImgButtons instead

    send(D, append, new(BTS, dialog_group(buttons, group))), 
	send(BTS, gap, size(0, 0)),
	BTS->>alignment(left),
	BTS->>append(new(DispEq,text('Display equations')), below),
	DispEq->>font(bold),
	
	LongBtn *= toggleImgButton(long,
					message(@prolog, set_equation_format, B, StateListStr, F2, long), %legacy vg
					tt := 'Show long names'),
	LongBtn->>value(@on),
	LongBtn->>alignment(left),
	BTS->>append(LongBtn, below),
	
	%gp3
	%there is a short view, which just shows the quantity names
	%so in non-legacy mode, this short view shows the design time quantity names
	%while in legacy mode, the short view show the legacy names (ofcourse) and there is
	%no legacy view
	%gp3: also remove the TypeOfEquations argument from set_equation_format.
	%the type of equation (der or non-der) should be considered when set_equation_format
	%is run, not when the menu is build
	
	if
		get(@model, modelState, legacy) % JL 
	then
	(
		ShortBtn *= toggleImgButton(short,
					message(@prolog, set_equation_format, B, StateListStr, F2, legacy), %legacy vg
					tt := 'Show short names'),
		ShortBtn->>group(LongBtn),
		BTS->>append(ShortBtn, right)
	)
	else
	(
		ShortBtn *= toggleImgButton(short,
					message(@prolog, set_equation_format, B, StateListStr, F2, short), %legacy vg
					tt := 'Show short names'),
		ShortBtn->>group(LongBtn),
		BTS->>append(ShortBtn, right),
		LegacyBtn *= toggleImgButton(legacy,
					message(@prolog, set_equation_format, B, StateListStr, F2, legacy), %legacy vg
					tt := 'Show names in legacy mode'),
		LegacyBtn->>group(LongBtn),
		BTS->>append(LegacyBtn, right)
	),

	BTS->>append(text(''), below),
	BTS->>append(new(DerEq,text('Derivative equations')),below),
	DerEq->>font(bold),
	
	%one button for derivative view
	DerivativeBtn *= toggleImgButton(derivative_equation_btn,
		message(@prolog, set_equation_type, B, StateListStr, F2, @receiver?value),
		img:= derivative_equation),
	DerivativeBtn->>value(@off),
	DerivativeBtn->>tooltipMsg(when(DerivativeBtn?value == @off, 'Include derivative equations', 'Exclude derivative equations')),
	DerivativeBtn->>alignment(left),
	BTS->>append(DerivativeBtn, below),

	
	BTS->>append(text(''),below),
	
	%this is vg code (except for the imgButton features and putting them next to eachother)

        new(AllButton, imgButton(all, 
			message(B, selection, B?members),
			img:=select_all,
			tt:= 'Select all'
			)
	),
        new(NoneButton, imgButton(none, 
			message(B, selection, @nil),
			img := select_none,
			tt := 'Select none'
			)
	),
        new(DrawButton, imgButton(draw, 
				message(B?selection, for_all, 
					message(@prolog, gen_equation_history_table, 
						F, F2, P, @arg1?object)
				),
				tt := 'Draw equation history'
			)
	),
        new(ClearButton, imgButton(clear, 
			and( 
				message(P, clear),
				message(P, scroll_to, point(0, 0))
			),
			tt:='Clear screen')
	),


	send(BTS, append, new(T1, text('Select')), below),
	send(T1, font, bold), 

	AllButton->>alignment(left),
	send(BTS, append, AllButton, below),
	send(BTS, append, NoneButton, right),
	
	send(BTS, append, new(_TGap, text('')), below),
	send(BTS, append, new(T2, text('Graph')), below),
	send(T2, font, bold), 

	DrawButton->>alignment(left),
	send(BTS, append, DrawButton, below),
	send(BTS, append, ClearButton, right), %gp3 0.2: moved frm D2 to BTS
	
	send(D2, append, PSButton),
	send(D2, append, CloseButton), 
	send(D2, layout_dialog),


	send(D, left, P), 
	send(D2, hor_stretch, 100),
	
	%gp3 0.3.13: add assistance stuff
	LeftOfAssistance *= dialog,
	LeftOfAssistance->>pen(0),
	LeftOfAssistance->>gap(size(0,0)),
	LeftOfAssistance->>border(size(0,0)), %does nothing, just stretching
	LeftOfAssistance->>ver_stretch(0),
	LeftOfAssistance->>hor_stretch(100),
	LeftOfAssistance->>ver_shrink(0),
	LeftOfAssistance->>hor_shrink(100),	
	LeftOfAssistance->>height(1), %need some height to overrule default
	
	Assistance *= dialog,
	Assistance->>name(assistance),
	Assistance->>pen(0),
	Assistance->>gap(size(0,0)),
	Assistance->>border(size(0,0)),
	Assistance->>ver_stretch(0),
	Assistance->>hor_stretch(0),
	Assistance->>ver_shrink(0),
	Assistance->>hor_shrink(0),
	Help *= helpButton(help,'Sim_QuantityEquationHistoryView'),
	Assistance->>display(Help),
	Assistance->>right(LeftOfAssistance),
	Assistance->>above(P),
	
	%make sure all tiles fit neatly
	P?tile?root->>border(0),
	
	send(F2, open),
	send(F2, transient_for, F).




fill_equations_str(B, StateListStr, TypeOfEquations, ViewMode):-
        term_to_atom(StateList, StateListStr), 
        fill_equations(B, StateList, TypeOfEquations, ViewMode).



% fill_equations(ListBrowser, StateList, TypeOfEquations, LongOrShort):-
%
% sorts the parameters in the listbrowser according to:
% first, the name of their entity, and 	
% second, the name of the parameter itself
%
%gp3: last argument was LongOrShort, rename to viewmode, because we now have
%long, short and legacy
fill_equations(B, StateList, TypeOfEquations, ViewMode):-
	send(B, clear),	
        findall(binary_ineq_rel(From, To, EHVRel), 
		(      
			% iterate over states in StateList
			nth0(_Index, StateList, N),

		        visualize:show_ineq_model(N, VRel, From, To, Dir),
		        test_equation_type(show_ineq_model(N, VRel, From, To, Dir), TypeOfEquations),
		        visualize_dep_for_Eq_history(VRel, question_mark, EHVRel)
		), 
		EquationList),
	%eliminate_doubles_rev(EquationList, EquationSet), 
	%gp3 changed the above line to the built-in:
	list_to_set(EquationList,EquationSet),
	sort(EquationSet, SortedEquations),

	forall(member(binary_ineq_rel(From, To, VRel), SortedEquations), 
		( 
		 % append item with Equation as key, and 
		 % Equation as label and object

		 create_equation_str(From, ViewMode, To, FromStr), %gp3, added 3rd argument (Other)
		 create_equation_str(To, ViewMode, From, ToStr), 

		 swritef(Str, '%w %w %w ', [FromStr, VRel, ToStr]),
		 atomize(binary_ineq_rel(From, To, VRel), Str2),
		 % append item with key, label, object:
		 % key = Str
		 % label = Str
		 % object = Str2 = binary_ineq_rel(From, To, VRel), 
		 send(B, append, dict_item(Str, Str, Str2))
		)
	).


% create_equation_str(Quantity, LongOrShort, Other, QuantityStr):-
%changed gp3: added legacy mode (same as old short mode) and made short mode differ depending on legacy mode of the model
%changed gp3: added 3rd argument (Other): the other quantity or whatever. We sometimes need this to get the
%non-designtime name of a value

create_equation_str(X, legacy, _Other, XShort):-!,
        strip_atomize(X, XShort).
%
create_equation_str(Quantity,ShortOrLong, Other, QuantityStr):-
	%designtime
	%short is allways non-legacy, otherwise the "short" button points to clause above (type = legacy)
	%but this code should also work for legacy
	create_equation_str_designName(Quantity,ShortOrLong, Other,QuantityStr).
%or else fall through: 
% 
create_equation_str(NoQuantity, _ShortOrLong, _Other, NoQuantity).

/*
create_equation_str_designName(Argument,ShortOrLong, Otherargument,Str)
new call gp3: get the designtime name of Argument
This can be a quantity name, or a value from its or another qs, or a dvalue, or a complex thing like
plus(value(q1),min(q2,value(q3))).
We make the best of it
*/

%some constant values for Zero (qs/dqs)
create_equation_str_designName(zero,_ShortOrLong,_Other,'Zero'):- !.
%

create_equation_str_designName(Argument,ShortOrLong,_Other,DesignName):-
	%Argument is the name of a quantity
	atom(Argument),
	find_quantity_entity(_,Argument,Pred,_,_,Instance),!,
	PredName = @app<<-designName(qd,Pred),
	if
		ShortOrLong = short
	then
		DesignName = PredName
	else
		DesignName = string('%s (%s)',?(@app,designName,qd,Pred),?(@app,designName,in,Instance,@app?currentScenario))<<-value.

%
create_equation_str_designName(Argument,_ShortOrLong,_Other,Argument):-
	%when not a quantity name, no dqs value or zero and still atomic, we dont know
	atom(Argument),!.
%
create_equation_str_designName(Argument,_ShortOrLong, Other,DesignValue):-
	%a value from its own QS
	Argument =.. [Value,Other], %like immigrated1 > max(immigrated1)
	%get the qs and the given value
	find_quantity_entity(_,Other,_Pred,_,QSName,_), !,
	(
		QS = @app<<-findExportedObject(qs,QSName),!
	;
		QS = @nil
	),

	%we dont need to name the quantity, because it is in the right context
	DesignValue = @app<<-designName(qsv,Value,QS).
%
create_equation_str_designName(Argument,ShortOrLong, Other,Str):-
	%a value from another qs
	Argument =.. [Value,Quantity2], %like immigrated1 > max(emigrated1)
	Quantity2 \== Other, !,
	%get the quantity designname qs and the given value
	find_quantity_entity(_,Quantity2,Pred,_,QSName,Instance), !,
	(
		QS = @app<<-findExportedObject(qs,QSName)
	;
		QS = @nil
	),
	DesignName = @app<<-designName(qd,Pred), %the other
	DesignValue = @app<<-designName(qsv,Value,QS),
	if
		ShortOrLong = short
	then
		EntityName = ''
	else
		EntityName = string(' (%s)',@app<<-designName(in,Instance,@app?currentScenario)),
	Str = string('%s%s: %s',DesignName,EntityName,DesignValue)<<-value.
%
create_equation_str_designName(Argument,_ShortOrLong,_Other,Str):-
	%fallback for arity=1 Arguments
	functor(Argument,_,1),
	term_to_atom(Argument,Str).
%
create_equation_str_designName(Argument,ShortOrLong,Other,Str):-
	%any structure with arity > 1 is a complex one
	functor(Argument,_,A),
	A > 1,
	Argument =.. [Functor|Args],
	findall(
		ArgStr,
		(
			member(Arg,Args),
			create_equation_str_designName(Arg,ShortOrLong,Other,ArgStr)
		),
		ArgStrings),
	NewTerm =.. [Functor|ArgStrings],
	term_to_atom(NewTerm,Str).
	
	


% visualize_dep_for_Eq_history(VRel, NormalOrQuestionMark, EHVRel) 
% 
% EHVRel: visualization of dependency in Equation History
        
% display derivative dependencies dR as R
%
visualize_dep_for_Eq_history(VRel, normal, VRel_without_d):-
        ineq_relation(Rel, VRel),
        derivative_relation(Rel, VRel_without_d).
%
% display non-derivative dependencies as usual
%
visualize_dep_for_Eq_history(VRel, normal, VRel):-
        ineq_relation(Rel, VRel),
        not_derivative_rel(Rel).
 
% display derivative dependencies as d?
%
visualize_dep_for_Eq_history(VRel, question_mark, 'd?'):-
        ineq_relation(Rel, VRel),
        derivative_relation(Rel, _VRel_without_d).
%
% display non-derivative dependencies as ?
%
visualize_dep_for_Eq_history(VRel, question_mark, '?'):-
        ineq_relation(Rel, VRel),
        not_derivative_rel(Rel).
 


set_equation_format(B, StateListStr, F, LongOrShort):-
		%gp3: removed TypeOfEquations as an argument, we should get it now
		%from the last time the user changed this:
		preferred_equation_type(F,TypeOfEquations),
        retractall(preferred_equation_format(F, _)),
        asserta(preferred_equation_format(F, LongOrShort)),
        fill_equations_str(B, StateListStr, TypeOfEquations, LongOrShort).


%gp3 0.2: changed last argument, is now @on (all) or @off (no_der)
set_equation_type(B, StateListStr, F, ShowAll):-
        retractall(preferred_equation_type(F, _)),
        if
        	ShowAll == @on
        then
        	TypeOfEquations = all
        else
        	TypeOfEquations = no_der,
        asserta(preferred_equation_type(F, TypeOfEquations)),
        preferred_equation_format(F, LongOrShort),
        fill_equations_str(B, StateListStr, TypeOfEquations, LongOrShort).


% determine_equation_type(EHVRel, TypeOfEquation)
%
determine_equation_type('d?', der).
determine_equation_type('?', no_der).




% gen_equation_history_table(F, F2, P, Selection):-
%
% Shows a table with equations for 
% Quantity for the states in StateList - this StateList
% may have been changed since the menu popped up, so check 
% again in frame F!
%	
gen_equation_history_table(F, F2, P, Selection):-
        preferred_equation_format(F2, LongOrShort),
        term_to_atom(binary_ineq_rel(From, To, Rel), Selection),
	get_selected_states_or_path(F, InputStateList), 
	delete(InputStateList, 0, StateList),

        determine_equation_type(Rel, TypeOfEquations),
	equation_history(From, To, StateList, TypeOfEquations, EquationList), 
	% get bounding box area of all graphicals in Picture P
	get(P, bounding_box, area(_X, Y, _W, H)),

	text_margin(LeftMargin, TopMargin), 
	% Y1 is Y + H + TopMargin, 
	Y1 is Y + H, 
	X2 is 2 * LeftMargin, 
	Y2 is 2.8 * TopMargin, 
	new(Fig, figure), 

        % atomize(To, ToStr), 
	create_equation_str(From, LongOrShort, To, FromStr),  %gp3 added 3rd argument for the other end of the equation
	create_equation_str(To, LongOrShort, From, ToStr), 

	swritef(Str, '%w %w %w ', [FromStr, Rel, ToStr]),

	send(P, display, Fig, point(0, Y1)), 
	send(Fig, display, 
		new(_Text, text(Str)), point(LeftMargin, TopMargin)),
	send(Fig, display, 
		new(Fig2, figure), point(X2, Y2)),

	draw_equations(Fig2, EquationList, TypeOfEquations),
	% update scrollbars
	send(P, normalise, Fig2).
	% send(P, scroll_to, point(0, Y1)).



% equation_history(From, To, PathStateList, TypeOfEquations, EquationList) 
%
% show a history table of equations in (path of) states
%
equation_history(From, To, PathStateList, TypeOfEquations, EquationList) :-
        % writef('equation_history for Path: %p.\n',[PathStateList]), 
        findall(show_ineq_model(N, VRel, From, To, Dir), 
		(      
			% iterate over states in StateList
			nth0(_Index, PathStateList, N),

		        visualize:show_ineq_model(N, VRel, From, To, Dir),
		        test_equation_type(show_ineq_model(N, VRel, From, To, Dir), TypeOfEquations)

		), 
		EquationList).



% draw_equations(P, EquationList, TypeOfEquations)
%
draw_equations(P, EquationList, TypeOfEquations):-
    	% add text to the right of the graph 

        new(D, device),
        send(D, layout_manager, new(T, table)),
        get(T, column, 1, @on, Col),
	send(Col, halign, center),
        forall(nth0(Index, EquationList, show_ineq_model(N, Rel, From, To, Dir)), 
	      ( 
	       test_equation_type(show_ineq_model(N, Rel, From, To, Dir), TypeOfEquations)
	       ->
		 visualize_dep_for_Eq_history(Rel, normal, EHVRel),
	         % writef('Equation in state %d: %d %d %d\n', 
		 %     [N, From, Rel, To]),
	         swritef(Str, '%w', [EHVRel]),
	         swritef(StrN, '%d', [N]),
       
		 % make a table cell
	         new(TableCell, text(Str, center)),
	         new(TableCellN, text(StrN, center)),
	         X is 1,
	         send(T, append, TableCell, Index, X),
	         send(T, append, TableCellN, Index, X+1),
		 send(TableCell, halign, center), 
		 send(TableCellN, halign, center)

		;
		 % do nothing
		 true
                )

	),
        send(T, border, 1), 
        % rules = all: show all gridlines in the table
        % rules = none: show no gridlines in the table
        send(T, rules, none),
        send(T, cell_spacing, size(-1,-1)),
        send(T, cell_padding, size(4,2)),
	send(T?columns, for_all, message(@arg1, halign, center)),

	send(P, display, D).

  
% test_equation_type(show_ineq_model(N, Rel, From, To, Dir), TypeOfEquations):-
%    
% TypeOfEquations = no_der: no equations of the form Q d? X
% where X is a (derivative) value or quantity
%
test_equation_type(show_ineq_model(_N, VRel, From, To, _Dir), no_der):-
               ineq_relation(Rel, VRel),
               not_derivative_rel(Rel),
	       is_quantity(From), 
	       is_quantity_or_val_or_dval(To, From),!. 
    
% TypeOfEquations = der: only equations of the form Q d? X
% where X is a (derivative) value or quantity
%
test_equation_type(show_ineq_model(_N, VRel, From, To, _Dir), der):-
               ineq_relation(Rel, VRel),
               derivative_relation(Rel, _VRel_without_d),
	       is_quantity(From), 
	       is_quantity_or_val_or_dval(To, From),!. 


% TypeOfEquations = all: all equations of the form Q ? X or Q d? X
% where X is a (derivative) value or quantity
%
test_equation_type(show_ineq_model(_N, _Rel, From, To, _Dir), all):-
	       is_quantity(From), 
	       is_quantity_or_val_or_dval(To, From),!. 


% is_quantity_or_val_or_dval(X, Q)
%
% Check whether X is a quantity, value or derivative value
%
% X is a quantity
is_quantity_or_val_or_dval(X, _Q):-
               is_quantity(X).


% X is a value in Q's quantity space
is_quantity_or_val_or_dval(X, Q):-
	       engine:qspace(Q, _Predicate, QSpaceList, _Fail), 
               qspace_member(X, QSpaceList, _Index). 


% X is a derivative value 
is_quantity_or_val_or_dval(X, _Q):-
	       member(X, [min, zero, plus]).

