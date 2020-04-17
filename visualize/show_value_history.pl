/*****************************************************************************************************
Garp3 0.1 Based on main source of visigarp (main.pl). This file is split in parts and edited.
All visiualize source based on visigarp 2.05, designed and implemented by Anders Bouwer 18/08/1999
Original copyright: (C) 2004 University of Amsterdam. All rights reserved.

Now:
PART OF Garp3. SEE COPYRIGHT NOTICE.
%code was visigarp, only changed where gp3 mentioned.
*******************************************************************************************************/
%code to generate the value history for (a) state(s).

% gen_history_menu(F, _ParameterList):-
gen_history_menu(F):-
	%gp3 0.2: changed buttons to imgButton of toggleImgButton
	get_selected_states_or_path(F, InputStateList),
	delete(InputStateList, 0, StateList),
	StateList \== [],!,
	new(F2, my_frame), %frame with postscript method (gp3 comment: this is the only place where its used so we might rewrite this)
	F2->>icon(@simulate_icon),

	send(F2, label, 'Quantity value history view'),

	send(F2, append, new(P, picture)),
	P->>hor_stretch(100),
	P->>ver_stretch(100),
	P->>hor_shrink(100),
	P->>ver_shrink(100),

	send(new(D2, dialog), below, P),

	new(PSButton, imgButton(postscript, message(F2, postscript),tt:= 'Save Diagram to EPS file')),
	new(CloseButton, imgButton(close, message(F2, destroy),tt:='Close this window')),

    new(D, extendedDialog(string('Graph View'))), %changed dialog to extendedDialog
	send(D, append, new(B, list_browser)),
	send(B, multiple_selection, @on),
	send(B, width, 35),
	send(B, height, 15),
	D->>hor_stretch(0),
	D->>hor_shrink(0),
	D->>ver_stretch(100),
	D->>ver_shrink(100),

	% default option: fill and sort parameters in
	% listbrowser by parameter name
	show_value_history_fill_list(B,parameter),
	send(B, open_message,
			message(B?selection, for_all,
					message(@prolog, gen_history_table,
						F, P, @arg1?key,@arg1?tooltip    %gp3 1.4: added tooltip to show also in parameter display
					)
			)
	),

	%gp3: remove the menu with the 2 buttons for sort by, and added them to the same dlg
	%the other buttons belong to, using toggleImgButton

		new(QuantitySort, toggleImgButton(quantity,
				message(@prolog, show_value_history_fill_list, B, parameter), %legacy vg code
				img := list_quantities,
				tt := 'Sort by quantity')),
		QuantitySort->>value(@on),

		new(EntitySort, toggleImgButton(entity,
				message(@prolog, show_value_history_fill_list, B, entity), %legacy vg code
				img := list_entities,
				tt := 'Sort by entity')),
		EntitySort->>group(QuantitySort),

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
					message(@prolog, gen_history_table,
						F, P, @arg1?key,@arg1?tooltip)
				),
				tt := 'Draw value history'
			)
	),

        new(ClearButton, imgButton(clear,
			and(
				message(P, clear),
				message(P, scroll_to, point(0, 0))
			),
			tt:='Clear screen')
	),

	send(D, append, new(BTS, dialog_group(buttons, group))),
	send(BTS, gap, size(0, 0)),

	send(BTS, append, new(TSort, text('Sort by')), below), %gp3 added this, used to be menu
	TSort->>font(bold),
	BTS->>append(QuantitySort, below),
	BTS->>append(EntitySort, right),
	BTS->>append(text(''), below), %gap

	send(BTS, append, new(T1, text('Select')), below),
	send(T1, font, bold),

	send(BTS, append, AllButton, below),
	send(BTS, append, NoneButton, right),

	send(BTS, append, new(_TGap, text('')), below),
	send(BTS, append, new(T2, text('Graph')), below),
	send(T2, font, bold),

	send(BTS, append, DrawButton, below),
	send(BTS, append, ClearButton, right), %gp3 0.2 moved this from D2 to BTS

	send(D2, append, PSButton),
	send(D2, append, CloseButton),

	send(D2, layout_dialog),
	send(D2, hor_stretch, 100),
	D2->>hor_shrink(100),
	D2->>ver_stretch(0),
	D2->>ver_shrink(0),

	send(D, left, P),

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
	Help *= helpButton(help,'Sim_QuantityValueHistoryView'),
	Assistance->>display(Help),
	Assistance->>right(LeftOfAssistance),
	Assistance->>above(P),

	%make sure all tiles fit neatly
	P?tile?root->>border(0),

	send(F2, open),
	send(F2, transient_for, F).



% if the above goes wrong, in case there are no states, just succeed.
%
gen_history_menu(_F).

%show_value_history_fill_list. This is the gp3 (0.1) rewrite of sort_by_parameter / sort_by_entity and helpers
show_value_history_fill_list(B,ParameterOrEntity):-
	%gp3: fill the list browser, displaying parameter first or entity first
	%rewrite using old visigarp algoritms
	B->>clear,
	B->>sort_by(@default), %gp3 sort by display label

	findall(Pred/Entity/ParInstance/ParName/QS, %gp3: we added more information
	(
		engine:state(_,SMD),
		engine:smd_slots(SMD,_,_,ParList,_,_,_,_),
		member(SomePar, ParList),
		engine:parameter(SomePar,_,Pred,Entity,ParInstance,_,QS), %gp3 this is different from original
		engine:parameter_visible_item(SomePar, ParName)
	),
		Parameters),
	list_to_set(Parameters,ParameterSet), %gp3 changed this (was proprietary clause that did the same)
	%gp3: we do not sort here, we do that when adding to the list
	%do we have a design object for the IS?
	IS = @app<<-currentScenario,

	forall(member(Pred/Entity/ParInstance/ParPlusEntity/QS,ParameterSet),
		(
			%gp3 1.4, use findExportedObject to find the QDef, needed for tooltip
			%because this list is over more than one state, we can only show the comments for the definition
			%we also added the QS information, to add to the tooltip
			Tooltip *= string,

			if
				QDef = @app<<-findExportedObject(qd,Pred)
			then
			(
				PredName = QDef<<-name,
				QDComment = QDef<<-relevantComments,
				%we add an *, because we show the comment for the definition
				unless
					0 = QDComment<<-size
				do
				(
					Tooltip->>ensure_nl(?(QDComment,split_lines,120))
				)
			)
			else
			(
				PredName = Pred,
				Tooltip *= string
			),

			if
				QSDef = @app<<-findExportedObject(qs,QS)
			then
			(
				QSComment = QSDef<<-relevantComments,
				%we add an *, because we show the comment for the definition
				unless
					0 = QSComment <<- size
				do
				(
					Tooltip->>ensure_nl(?(QSComment,split_lines,120))
				)
			),

			InsName = @app<<-designName(in,Entity,IS),
			if
				ParameterOrEntity = parameter
			then
				Label *= string('%s (%s)', PredName,InsName)
			else
				Label *= string('%s: %s', InsName,PredName),

			DI *= dict_item(ParInstance,Label,ParPlusEntity),
			%gp3 1.4 tooltip

			%still empty?
			if
				0 = Tooltip<<-size
			then
				Tooltip->>append('(no comments)'),
			DI->>tooltip(Tooltip,model),
			B->>insert(DI) %gp3: we kept the information the same as visigarp,except for the label
		)
	).

% gen_history_table(F, P, Parameter):-
%
% Shows a table with values and derivatives for
% Parameter for the states in StateList - this StateList
% may have been changed since the menu popped up, so check
% again in frame F!
%
% this is visigarp code where we added some gp3 stuff where mentioned
%gp3 1.4: added tooltip argument
gen_history_table(F, P, Parameter,Tooltip):-
	get_selected_states_or_path(F, InputStateList),
	delete(InputStateList, 0, StateList),
	par_value_history(Parameter, StateList, IndexNValueDerivativeList),
	par_2nd_der_history(Parameter, 0, StateList, DDList),
	engine:qspace(Parameter, Par, QSpace, _),
	engine:parameter(Par, _, Pred, Entity, Parameter, _, QSName), %gp3: added Pred and QSName

	%%%%gp3 code
	%we try to get the design names or objects
	%we need the inputsystem again
	IS = @app<<-currentScenario,

	QuantityName = @app<<-designName(qd,Pred),
	InsName = @app<<-designName(in,Entity,IS),

	%and get the qsdef object, if we can find it
	(
		QS = @app<<-findExportedObject(qs,QSName)
	;
		QS = @nil
	),
	Label *= string('%s: %s', InsName, QuantityName),
	%%%%end gp3 code

	% get bounding box area of all graphicals in Picture P
	get(P, bounding_box, area(_X, Y, _W, H)),

	text_margin(LeftMargin, TopMargin),
	Y1 is Y + H + TopMargin,
	X2 is 3 * LeftMargin,
	Y2 is 3.5 * TopMargin,
	new(Fig, figure),
	send(P, display, Fig, point(0, Y1)),
	send(Fig, display,
		new(_Text, text(Label)), point(LeftMargin, TopMargin)),
	send(Fig, display,
		new(Fig2, figure), point(X2, Y2)),

	%gp3 1.4: tooltip
	Fig->>tooltip(Tooltip,model),

	draw_quantity_space_grid(Fig2, QSpace, QS, StateList),	%gp3 added argument QS: the qs def

	draw_value_derivative_points(Fig2, StateList,
				IndexNValueDerivativeList, DDList, QSpace),
	% update scrollbars
	send(P, normalise, Fig2).
	% send(P, scroll_to, point(0, Y1)).


draw_quantity_space_grid(P, QSpace, QS, StateList):- %gp3: added QS argument for qs design object
	% draw a grid with the StateList on the X-axis
	% and the QSpace on the Y-axis.

%	% create Grid of size StateList x QSpace-1 grid blocks
%	% create Grid of size StateList x QSpace+1 grid blocks
	% create Grid of size StateList x 2*NrOfIntervals+1 grid blocks

	length(StateList, XB),
	nr_of_intervals(QSpace, NrIntervals),

	YB is 2 * NrIntervals,
	grid_block_size(XF, YF),
	X is XF * XB,
	Y is YF * YB,
	send(P, display,
		new(GraphArea, box(X, Y)), point(0,0)),
	send(GraphArea, colour, grey),
	send(GraphArea, fill_pattern, colour(light_grey)),

	send(P, display,
		new(LeftLine, line(0, 0, 0, Y, none))),
	send(LeftLine, colour, grey),
	send(P, display,
		new(RightLine, line(X, 0, X, Y, none))),
	send(RightLine, colour, grey),
%	send(P, display,
%		new(TopLine, line(0, 0, X, 0, none))),
%	send(TopLine, colour, grey),
%	send(P, display,
%		new(BottomLine, line(0, Y, X, Y, none))),
%	send(BottomLine, colour, grey),


	forall(member(PointOrInterval, QSpace),
		draw_gridline(P, PointOrInterval, QSpace, XB, YB, QS) %gp3: added QS argument (qs design object)
	),
	show_state_list(P, StateList, QSpace, XB, YB).


% nr_of_intervals(QSpace, NrIntervals):-
%
% determines the number of intervals in QSpace
%
nr_of_intervals([], 0).

nr_of_intervals([point(_Point)|RestQSpace], NrIntervals):-
	nr_of_intervals(RestQSpace, NrIntervals),!.

nr_of_intervals([_Interval|RestQSpace], N):-
	nr_of_intervals(RestQSpace, RN),
	N is RN + 1.




%
% draw_gridline(P, PointOrInterval, QSpace, XB, YB)
%
% draws a gridline for a point value in QSpace in
% picture P, of size XB by YB gridblocks
%
draw_gridline(P, point(Point), QSpace, XB, _YB, QS):- %gp3 added QS argument (qs design object)
	strip_atomize(Point, PointAtom),!,
	% point value: draw gridline
	% get index of Point in QSpace
	qspace_member(Point, QSpace, N),
%	nth0(N, QSpace, point(Point)),
	% Point is the Nth value from below.
	length(QSpace, L),
	M1 is L - N,
	% Or, the M1th = L - Nth value from above.
	adjust_for_interval_at_end(QSpace, M1, M),

	% Multiply M by the gridblock YFactor to
	% find out Y coordinate of gridline
	grid_block_size(XF, YF),
	Y is M * YF,
	% Multiply XB by the gridblock XFactor to
	% find out X coordinate of gridline endpoint
	X is XB * XF,
	send(P, display,
		new(_GridLine, line(0, Y, X, Y, none))),
	% add text to the right of the graph
	text_margin(LeftMargin, _TopMargin),
	X2 is X + LeftMargin,
	send(P, display,
		new(PointText, text(?(@app,designName,qsv,PointAtom,QS)))), %gp3 rewrite to get design valuename
	send(PointText, center, point(X2, Y)),
	send(PointText, x, X2).




% in case of an interval value: just write the value name
draw_gridline(P, Interval, QSpace, XB, _YB, QS):- %gp3 added QS argument
	strip_atomize(Interval, IntervalAtom),!,
	% get index of Interval in QSpace
	qspace_member(Interval, QSpace, N),
	% so, Point is the Nth value from below.
	length(QSpace, L),
	M1 is L - N,
	% Or, the M1th = L - Nth value from above.
	adjust_for_interval_at_end(QSpace, M1, M),

	% Multiply M by the gridblock YFactor to
	% find out Y coordinate of gridline
	grid_block_size(XF, YF),
	Y is M * YF,
	% Multiply XB by the gridblock XFactor to
	% find out X coordinate of gridline endpoint
	X is XB * XF,
%	send(P, display,
%		new(_GridLine, line(0, Y, X, Y, none))),
	% add text to the right of the graph
	text_margin(LeftMargin, _TopMargin),
	X2 is X + LeftMargin,
	send(P, display,
		new(IntervalText, text(?(@app,designName,qsv,IntervalAtom,QS)))),
	send(IntervalText, center, point(X2, Y)),
	send(IntervalText, x, X2).




% draw_value_derivative_points(P, StateList,
%				IndexNValDerList, QSpace)
%
% draw points from IndexNValDerList on the grid in picture P
%
draw_value_derivative_points(_P, _StateList, [], [], _QSpace).

draw_value_derivative_points(P, StateList,
				[Index/N/Val/Der|RestValDerList], [Index/SOD/TOD|DDList], QSpace):-
	draw_value_derivative(P, N, Index, Val/Der/SOD/TOD, QSpace),
	draw_value_derivative_points(P, StateList,
					RestValDerList, DDList, QSpace).





% draw_value_derivative(P, N, X, Value/Derivative, QSpace)
%
% draw a point on the grid on picture P, with as X the index of
% State N in StateList and Y as the index of Value in QSpace
%
draw_value_derivative(P, N, XB, Value/Derivative/SOD/TOD, QSpace):-
	% check whether Value is known
	nonvar(Value),
	% Value is the th value from below.
	% Or, the YBth = L - Kth value from above
	qspace_member(Value, QSpace, K),
	length(QSpace, L),
	YB1 is L - K,
	adjust_for_interval_at_end(QSpace, YB1, YB),
	draw_point(P, XB, YB, Derivative, SOD, TOD, N).


draw_value_derivative(_P, _N, _XB, Value/_Derivative/_DDer/_TOD, _QSpace):-
	% if Value is unknown, do nothing!
	var(Value),!.

draw_value_derivative(_P, _N, _XB, nonexistent/_Derivative/_DDer/_TOD, _QSpace).
	% if Value is nonexistent, do nothing!


adjust_for_interval_at_end(QSpace, M, N):-
	last(QSpace, Last), % order of attributes changed, 28/4/2003
	% if the highest value in QSpace is a point,
	(Last = point(_Point),
	 N is M
	;
	% if the highest value in QSpace is an interval,
	 % Last = _Interval,
	 % For intervals, move node a block down to middle of interval
	 N is M + 1
	).



adjust_for_interval_at_begin(QSpace, Margin):-
	first(First, QSpace),
	% if the lowest value in QSpace is a point,
	% return extra margin
	(First = point(_Point),
	 graph_margin(_MarginX, MarginY),
	 Margin is MarginY
	;
	% if the lowest value in QSpace is an interval,
	% First = _Interval,
	 Margin is 0
	).



draw_point(Picture, XB, YB, Derivative, SOD, TOD, N):-
	% one grid block is XF by YF
	grid_block_size(XF, YF),
	graph_margin(XMargin, _YMargin),
	X is XF * XB + XMargin,
	Y is YF * YB,
	new(PointNode, graph_node(' ', graph_point, N)),
	create_derivative_graphical(Derivative, DerGr),
	send(Picture, display, PointNode),
	send(PointNode, center, point(X, Y)),
	send(Picture, display, DerGr),
        position_point(DerGr, PointNode, Derivative),
	HODonoff = @app<<-setting(second_order_derivatives), %FL new feb 2012: this now has the meaning: higher order derivatives
	(
	  HODonoff == @on
	->
	  draw_2nd_derivative(X, Y, Picture, SOD, TOD)
	;
	  true
	).

draw_2nd_derivative(_X, _Y, _Picture, DDer, _):-
	var(DDer),
	!.

draw_2nd_derivative(X, Y, Picture, neg, TOD):-
	!,
	DDX is X + 9,
	DDY is Y + 7,
	draw_2nd_der(DDX, DDY, Picture, neg),
	draw_3rd_derivative(DDX, DDY, Picture, TOD).

draw_2nd_derivative(X, Y, Picture, pos, TOD):-
	!,
	DDX is X + 9,
	DDY is Y - 5,
	draw_2nd_der(DDX, DDY, Picture, pos),
	draw_3rd_derivative(DDX, DDY, Picture, TOD).

draw_2nd_derivative(X, Y, Picture, neg_zero, TOD):-
	!,
	DDX is X + 9,
	DDY is Y + 6,
	draw_2nd_der(DDX, DDY, Picture, neg),
	draw_2nd_der(DDX, Y, Picture, zero),
	draw_3rd_derivative(DDX, DDY, Picture, TOD).

draw_2nd_derivative(X, Y, Picture, pos_zero, TOD):-
	!,
	DDX is X + 9,
	DDY is Y - 5,
	draw_2nd_der(DDX, DDY, Picture, pos),
	draw_2nd_der(DDX, Y, Picture, zero),
	draw_3rd_derivative(DDX, DDY, Picture, TOD).

draw_2nd_derivative(X, Y, Picture, DDer, TOD):-
	!,
	DDX is X + 9,
	draw_2nd_der(DDX, Y, Picture, DDer),
	draw_3rd_derivative(DDX, Y, Picture, TOD).

draw_2nd_der(DDX, DDY, Picture, DDer):-
	create_2nd_derivative_graphical(DDer, DDerGr),
	send(Picture, display, DDerGr),
	send(DDerGr, center, point(DDX, DDY)).


% Same for 3rd derivatives...
% only draw something when it is really known,
draw_3rd_derivative(_X, _Y, _Picture, DDer):-
	var(DDer),
	!.

draw_3rd_derivative(X, Y, Picture, neg):-
	!,
	DDX is X + 6,
	DDY is Y + 4,
	draw_3rd_der(DDX, DDY, Picture, neg).

draw_3rd_derivative(X, Y, Picture, pos):-
	!,
	DDX is X + 6,
	DDY is Y - 4,
	draw_3rd_der(DDX, DDY, Picture, pos).

draw_3rd_derivative(X, Y, Picture, neg_zero):-
	!,
	DDX is X + 6,
	DDY is Y + 4,
	draw_3rd_der(DDX, DDY, Picture, neg),
	draw_3rd_der(DDX, Y, Picture, zero).

draw_3rd_derivative(X, Y, Picture, pos_zero):-
	!,
	DDX is X + 6,
	DDY is Y - 4,
	draw_3rd_der(DDX, DDY, Picture, pos),
	draw_3rd_der(DDX, Y, Picture, zero).

draw_3rd_derivative(X, Y, Picture, DDer):-
	!,
	DDX is X + 6,
	DDY is Y + 0,
	draw_3rd_der(DDX, DDY, Picture, DDer).

draw_3rd_der(DDX, DDY, Picture, DDer):-
	create_3rd_derivative_graphical(DDer, DDerGr),
	send(Picture, display, DDerGr),
	send(DDerGr, center, point(DDX, DDY)).





% position_point(DerGr, PointNode, plus)
%
% for some reason, positioning of nodes was not perfect, especially in the generated eps-file, AB, 17/07/2006
%
position_point(DerGr, PointNode, plus):-
	send(DerGr, center, PointNode?center),!.

position_point(DerGr, PointNode, min):-
	send(DerGr, center, point(PointNode?center_x, PointNode?center_y+1)),!.

position_point(DerGr, PointNode, _Derivative):-
	send(DerGr, center, PointNode?center),!.


% show_state_list(P, StateList, QSpace, XB, YB).
%
% writes the state numbers in StateList below the graph
% in picture P, of size XB * YB gridblocks
%
show_state_list(P, StateList, QSpace, _XB, YB):-
	grid_block_size(XF, YF),
	graph_margin(XMargin, YMargin),
	text_margin(_LeftMargin, TopMargin),
	adjust_for_interval_at_begin(QSpace, IntervalMargin),
	Y is YB * YF + TopMargin + YMargin + IntervalMargin,
	forall(nth0(Index, StateList, StateNr),
		(
			X is Index * XF + XMargin,
			send(P, display,
				new(StateText, text(StateNr))),
			send(StateText, center, point(X, Y))
		)
	).
