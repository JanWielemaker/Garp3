/*****************************************************************************************************
Garp3 0.1 Based on main source of visigarp (main.pl). This file is split in parts and edited.
All visiualize source based on visigarp 2.05, designed and implemented by Anders Bouwer 18/08/1999
Original copyright: (C) 2004 University of Amsterdam. All rights reserved.

Now:
PART OF Garp3. SEE COPYRIGHT NOTICE.
Code is old visigarp code unless gp3 is mentioned
*******************************************************************************************************/

%loaded into module (namespace) visualize

% for input state, do nothing
%
gen_model_fragments(_F, 0):-!.

gen_model_fragments(F, N) :-
        % Show list of model fragments for state N
    %gp3 0.2: We skip explicit creation of frame
    show_model_fragment_list(F,N).


show_model_fragment_list(F, N):-
	%changed in garp3 0.1: parsing the systemstructures, we create a nice name, and save the design object,
	
	%gp3 0.2: F is no longer the frame to create the dialog *in*, but the one the dialog is transient for
        % Show system structures for state N inside F
    %gp3 0.3.13: made this a assistanceDialog with helpbutton. So we changed this code from
    %an autolayout dialog to a fixed layout dialog
    new(D, assistanceDialog(string('Simulate - Model fragments in state %s', N),'Sim_ModelFragmentsInState')),
    D->>icon(@simulate_icon),
    
    B *= list_browser,
	send(B, width, 60), 
	send(B, height, 15), 
    D->>display(B,point(D?gapY,D?topY)),
    
	%gp3: make sure we do a nice sort in the list_browser
	B->>sort_by(@default),
	
	%legacy?
	if
		get(@model, modelState, legacy) % JL 
	then
		InLegacy = true
	else
		InLegacy = false,
		
	forall(show_system_structure(N, StructureName),
		( 
		 %gp3 0.1:  find design information
		 
		 %gp3 0.3.16: skip all names starting with 'garp_', these are internal (show them in legacy mode)
		 unless
		 	(InLegacy = false, StructureName->>prefix('garp_'))
		 do
		 (
			 %we need the structure anyway, so lets get it now
			 term_to_atom(Term,StructureName),
			 Term =.. [GarpName|Args],
			 Label *= string,
			 if
			 	MF = @app<<-findExportedObject(mf,GarpName) 
			 then
			 (
			 	Object = MF,
			 	JustName = MF<<-name,
			 	Tooltip = MF<<-relevantComments %gp3 1.4
			 )
			 else
			 (
			 	Object = @nil,
			 	JustName = GarpName,
			 	Tooltip = ''
			 ),
			 
			 Label->>append(JustName),
			 if
			 	Args \= []
			 then
			 (
			 	Label->>append(':'),
			 	forall(member(Argterm,Args),
					(
				 	Label->>append(' '),
				 	show_model_fragment_list_arg(Label,Argterm,@app?currentScenario) %gp3: append designtime names for args
				 	%term_to_atom(Argterm,Arg), %TODO: split these args, create nice names (from scenario)
				 	%Label->>append(Arg)
					)
				)
			 ),
			 
			/*
			%also get the supertype (gp3 1.4: not needed any more: not shown in comment)
			find_system_structure_supertype(N, Term, Supertype),
	    	term_to_atom(SupertypeTerm,Supertype),
	    	SupertypeTerm =.. [GarpSupername|_SuperArgs],
	    	SuperMF = @app<<-designName(mf,GarpSupername),
	    */
			 DI *= dict_item(StructureName,Label,Object),
			 %gp3 0.2: tooltip now possible
			 %gp3 0.4: rewrote tooltip
			 if 
			 	0 = Tooltip<<-size
			 then
			 	DI->>tooltip('(no comments)',model)
			 else
			 (
				 	DI->>tooltip(Tooltip,model)
			 	),
			 send(B, insert, DI)
		)
	   )
	),

	%gp3 0.2: we changed this to just a row of 3 toggleImgButtons
	%no dialog groups or whatever

	TextMI *= toggleImgButton(text, message(@prolog, 
						show_model_fragment_details, 
						D, text, B?selection?key, B?selection?label, N),
						img := text_modelfragment, inactImg := @nil,
						tt:='Show model fragment in legacy mode', value := @on),
	D->>display(TextMI,point(D?gapX,B?bottom_side + D?gapY)),
	GraphMI *= toggleImgButton(graphics, message(@prolog, 
						show_model_fragment_details, 
						D, graphics, B?selection?key,B?selection?label, N),
						img := graphics_modelfragment, inactImg := @nil,
						 tt:='Show model fragment in context'),
	GraphMI->>group(TextMI),
	D->>display(GraphMI,point(TextMI?right_side + D?gapX,TextMI?top_side)),
	%gp3
	%we always create the design button, but make it invisible if legacy mode
	%that makes life easier in resizing
	EditMI *=  toggleImgButton(design, message(@prolog, 
					show_model_fragment_design, 
					B?selection?object),img := edit_modelfragment, inactImg := @nil,
					tt:='Edit selected model fragment'),
	EditMI->>group(TextMI),
	D->>display(EditMI,point(GraphMI?right_side + D?gapX, GraphMI?top_side)),
	if
		get(@model, modelState, legacy) % JL 
	then
		EditMI->>displayed(@off),

	%and the closebutton
	Close *= imgButton(close, message(D, destroy), tt:='Close this window'),
	D->>display(Close,point(B?right_side - Close?width, TextMI?top_side)),
	
	%gp3 0.2: changed the open message
	%we decide on the basis of the groupValue (that we can ask of any of the buttons)
	
	B->>open_message(
		if(
			TextMI?groupValue == design,
			->>(@prolog,show_model_fragment_design,@arg1?object),
			->>(@prolog,show_model_fragment_details,D,TextMI?groupValue,@arg1?key,
					@arg1?label,N))), %gp3: changed F to D
	D->>updateSpacers, %needed in assistanceDialog	
	%gp3 0.2
	%resize stuff
	D->>minimalSize(size(Close?right_side, Close?bottom_side)),
	D->>onResizeMessage(and(
    	->>(B,right_side,B?right_side + @arg1?width),
    	->>(B,bottom_side, B?bottom_side + @arg1?height),
       	->>(TextMI,set,y:= TextMI?top_side + @arg1?height),
       	->>(GraphMI,set,y:= GraphMI?top_side + @arg1?height),
       	->>(EditMI,set,y:= EditMI?top_side + @arg1?height),
    	->>(Close,set, x:= Close?left_side + @arg1?width,
    						y:= Close?top_side + @arg1?height)
	)),
	%we open ourself and set transient_for after (wm wont know, pce will)
	D->>open,
	D->>transient_for(F).
% 
%show_model_fragment_list_arg(Label,Argterm,IS)
%gp3 0.1 Helper to get the designnames of the instances in Argterm
%this might be a (...,...,...) complex term, or just atomic
%NB: if A = (1,2,3), then A=(B,C) will unify with B = 1, C = (2,3)

show_model_fragment_list_arg(Label,(FirstArgterm, Rest),IS):-!,
	show_model_fragment_list_arg(Label,FirstArgterm,IS),
	Label->>append(', '),
	show_model_fragment_list_arg(Label,Rest,IS).
%
show_model_fragment_list_arg(Label,AtomicArgterm,IS):-
	%clause for atomic arg, no (...)
	Label->>append(?(@app,designName,in,AtomicArgterm,IS)).

%garp3: open the design editor for the selected model fragment
show_model_fragment_design(MFObject):-
	%this code is from structure_editor.pl in design (method edit)
	if
		MFObject \== @nil
	then
		@app->>openViewEditor(MFObject).


% show_model_fragment_details(F, TextOrGraphics, Name, N)
%
% shows the contents of model fragment Name in state N, in 
% text, or graphics format. If text is chosen, a new window 
% pops up to enable comparing different model fragments; if 
% graphics is chosen, only one window is opened - this is 
% reused whenever a model fragment is chosen. In this case, 
% the layout does not change - so you have to adjust it only 
% once. 
%
%gp3: no changes for the *content* of text mode details, but we did add a label
%and made the dialog resizable

show_model_fragment_details(F, text, Name, Label, N):- %gp3: added label argument
        % Show additional details about model fragment Name in state N:
       %gp3 0.3.13: changed the dialog to assistanceDialog, added helpid and rewrote layout code
       %because assistanceDialog likes fixed layouts
       
	% isa (supertype), conditions and givens
	input_to_int(State, N),
        new(D, assistanceDialog(string('Model fragment %s in state %s - Simulate', ?(@app,designName,mf,Name), State),
        	'Sim_ModelFragmentLegacyMode')),
        D->>icon(@simulate_icon),
	term_to_atom(NameTerm, Name),
	send(@prolog, find_system_structure_supertype, N, 
              NameTerm, Supertype),
              
    %gp3, get the design time description of this Supertype
    term_to_atom(SupertypeTerm,Supertype),
    SupertypeTerm =.. [GarpSupername|SuperArgs],
    SuperLabel *= string,
    if
    	SuperMF = @app<<-findExportedObject(mf,GarpSupername)
    then
    	SuperLabel->>append(SuperMF?name)
    else
    (
    	SuperLabel->>append(GarpSupername),
    	SuperMF = @nil
    ),
    
	if 
		SuperArgs \= []
	then
	(
		SuperLabel->>append(':'),
		forall(member(SuperArgTerm,SuperArgs),
			(
			SuperLabel->>append(' '),
			show_model_fragment_list_arg(SuperLabel,SuperArgTerm,@app?currentScenario)
			)
		)
	),
	
	D->>display(new(LMF,label(mfname,Label,font(helvetica,bold,12))),point(D?gapX,D?topY)),
	D->>display(new(LSMF,label(mfsupername,string('Is a %s',SuperLabel),font(helvetica,oblique,10))), point(D?gapX,LMF?bottom_side + D?gapY)),

	B *= list_browser,
	B2 *= list_browser,
	%we do not display yet, because we want to get the sizes first
	
	send(B, tab_stops, vector(75, 150, 225, 300, 375, 450, 525, 600)),
	send(B2, tab_stops, vector(75, 150, 225, 300, 375, 450, 525, 600)),

	index_pred_state(N, system_structures, system_structures(
                        NameTerm, _Isa, 
			conditions(Conditions), givens(Givens))), 

	get(string('Conditions'), value, CString),
	send(B, style, conditions, style(colour:= red)), % was blue before v.2.02
	send(B2, style, results, style(colour:= blue)), % was red before v.2.02
	show_list_new(B, CString, Conditions, conditions, MaxLengthC), 
	get(string('Results'), value, GString),
	show_list_new(B2, GString, Givens, results, MaxLengthG), 
	text_margin_spaces(_Left,Right),
	max(MaxLengthC, MaxLengthG, Max), 
	MaxLengthPlus is Max + Right,
	get(B, width, CurrentWidth),
	max(MaxLengthPlus, CurrentWidth, NewMax), 
	NewMax1 is NewMax + 25, % extra margin - better solution?!!
	min(NewMax1, 150, Width),
	get(B, length, NrItems), 
	min(NrItems, 25, Height),
	get(B2, length, NrItemsB), 
	min(NrItemsB, 25, HeightB),
	send(B, request_geometry, @default, @default, Width, Height),
	send(B2, request_geometry, @default, @default, Width, HeightB),
	%gp3: now we can display
	
	D->>display(B,point(D?gapX,LSMF?bottom_side + D?gapY)),
	D->>display(B2,point(D?gapX,B?bottom_side + D?gapY)),
	
	Close *= imgButton(close,->>(D,destroy),tt:='Close this window'),
	D->>display(Close,point(D?gapX + (B?right_side - D?gapX - Close?width) / 2,B2?bottom_side + D?gapY)),
    
	send(D, default_button, close),
    %gp3: resize stuff
    D->>updateSpacers, %needed for assistanceDialogs
    D->>minimalSize(size(400,300)),
    D->>onResizeMessage(and(
    	->>(B,right_side,B?right_side + @arg1?width),
    	->>(B,bottom_side, B?bottom_side + @arg1?height / 2),
    	->>(B2,set,y:= B2?top_side + @arg1?height / 2),
    	->>(B2,right_side,B2?right_side + @arg1?width),
    	->>(B2,bottom_side, B2?bottom_side + @arg1?height / 2),   
    	->>(D?close_member,set, x:= D?close_member?left_side + @arg1?width / 2,
    						y:= D?close_member?top_side + @arg1?height)
    )),
	send(D, open),
	send(D, transient_for, F). %gp3: after open to trick wm


show_model_fragment_details(F, graphics, Name, Label, N):- %gp3 added Label argument
        % Highlight details of system structure Name in state N:
	% conditions in red, givens in blue (was reversed before v. 2.02)
	% 
	% get the graphics window GW through the hyper link
	%
	( get(F, hypered, graphics_window, GW)
	->  
	    % graphics window found; associate state nr N with it
    	    send(GW, state_nr, N),
	    send(GW, expose),
    	    send(GW, label, string('Model fragment %s in state %s - Simulate', Label, N))
	;   
	    % no graphics window present yet; create one
	    new(GW, causal_graph_viewer(parameter_relations,'Sim_ModelFragmentInContext')), %gp3 0.3.13 added helpid
    	    send(GW, state_nr, N),
	    send(GW, gen_entity_causal_model_for_mf),
	    gen_derivatives(GW, N),
	    show_hide(GW, derivatives, @on),
	    show_hide(GW, attributes, @on),
	    send(GW?picture_member, selection_feedback, @nil),
	    new(_H, hyper(F, GW, graphics_window, list_window)),
    	    send(GW, label, string('Model fragment %s in state %s - Simulate', Label, N)),
	    send(GW, expose),
	    send(GW, open),
	   	send(GW, transient_for, F) %gp3: moved after open
	),
	
	% first reset all colours 
	%
	reset_colours(GW, light_grey),
	

	term_to_atom(NameTerm, Name),
	
	% find conditions, colour them red (was blue before v.2.02)
	%
        CondTerm = 
		visualize:find_system_structure_detail(N, 
							NameTerm, 
							conditions,
							Type, 
							Condition),
	forall(user:CondTerm, 
		(
		highlight(GW, N, Condition, Type, red,c) %gp3 added elementState c
		)
	),

	% find givens, colour them blue (was red before v.2.02) 
	%
        GivenTerm = 
		visualize:find_system_structure_detail(N, 
							NameTerm, 
							givens,
							Type, 
							Given),
	forall(user:GivenTerm, 
		(
		highlight(GW, N, Given, Type, blue,g) %gp3 added the elementState g
		)
	),
	%%gp3 0.3.13: resize picture does not work at all. It aimed to reason about the needed size
	%%of the picture, but it just did not work. So now we use a standard size picture
	%%which has the same effect: automatic_zoom messes the graph up a bit
	%%so we might consider disabling automatic_zoom as well

	%resize_picture(GW),
	scroll_picture(GW).
	% scale_picture(GW).

%highlight code
%gp3 0.2 Added extra argument for named elementState (c=condition, g=given, i=imported)
%to be send to the code that does the colouring, which might send it to the tagged connection and through that one to the dependency_icon that might use it to
%create the right bitmap. A bit of a dirty fix, but the only way to do it
%breaking existing code
	
% highlight par_relation link in state N in Colour

%  
highlight(F, N, Detail, par_relations, Colour, ElementState):-
	term_to_atom(DetailTerm, Detail), 
	DetailTerm=..[Rel, A, B], 
	visualize(Rel, A, B, VRel, VA, VB, _Dir), 
	colour_dependency_in_ent(F, N, VRel, VA, VB, Colour,ElementState). %gp3 added last arg

% highlight par_relation link in state N in Colour
%  
highlight(F, N, Detail, par_relations, Colour,ElementState):-
	term_to_atom(DetailTerm, Detail), 
	DetailTerm=..[Rel, A, B], 
	visualize(Rel, A, B, VRel, VA, VB, _Dir), 
	colour_math_rel(F, N, VRel, VA, VB, Colour,ElementState).



% highlight (v_)correspondence link in state N in Colour 
%  
highlight(F, N, Detail, par_relations, Colour,ElementState):-
	term_to_atom(DetailTerm, Detail), 
	DetailTerm=..[Rel, A, AV, B, BV], 
        ATerm =.. [A, AV],
        BTerm =.. [B, BV],
	visualize(Rel, ATerm, BTerm, VRel, VA, VB, _Dir), 
	colour_math_rel(F, N, VRel, VA, VB, Colour,ElementState).



% highlight par_relation node of type immigrated1 > zero 
% in state N in Colour
%  
highlight(F, N, Detail, par_relations, Colour,_ElementState):-
	term_to_atom(DetailTerm, Detail), 
	DetailTerm=..[Rel, A, B], 
	not(derivative_relation(Rel, _VRel)), 
	atomize(A, AAtom), 
	strip_atomize(B, BAtom), 
	B =.. [BAtom|Rest], 
	% prevent 'equal(growth_rate1, min(inflow1, outflow1)' from 
	% being interpreted as 'growth_rate1 = min'
	length(Rest, L), 
	L < 2,
	find_par_node(F, N, AAtom, NA), 
	
	% also, highlight the values involved
	% check whether a node for value B exists
	% get the quantity_space node within entity node NA
	get(NA, member, quantity_space, QSNode),

	( get(QSNode, member, BAtom, ValNode)
	->
		send(ValNode, colour, Colour),
		send(ValNode, expose)
	; 
	        % do nothing
		true
	),
	
	math_relation(Rel, VRel), 
	construct_code_str(A, B, VRel, ACodeStr), 
%	construct_code_str(A, B, Rel, ACodeStr), 

	% check whether a node for this relation exists
	% get the par_relations group node within entity node NA
	get(NA, member, par_relations, ParRelGroupNode),
	( get(ParRelGroupNode, member, ACodeStr, ParRelNode)
	->
		send(ParRelNode, colour, Colour),
		send(ParRelNode, expose)
	; 
	        % do nothing
		true
	).



% highlight par_relation node of type derivative-relation: 
% number_of1 d> zero in state N in Colour
%  
highlight(F, N, Detail, par_relations, Colour,_ElementState):-
	term_to_atom(DetailTerm, Detail), 
	DetailTerm=..[Rel, A, B], 
	derivative_relation(Rel, _VRel),
	atomize(A, AAtom), 
	strip_atomize(B, BAtom), 
	B =.. [BAtom|Rest], 
	% prevent 'equal(growth_rate1, min(inflow1, outflow1)' from 
	% being interpreted as 'growth_rate1 = min'
	length(Rest, L), 
	L < 2,
	find_par_node(F, N, AAtom, NA), 
	
	math_relation(Rel, VRel), 
	construct_code_str(A, B, VRel, ACodeStr), 
%	construct_code_str(A, B, Rel, ACodeStr), 
        % send(@prolog, write_ln, string('highlight search for:', ACodeStr)),

	% check whether a node for this relation exists
	% get the par_relations group node within entity node NA
	get(NA, member, par_relations, ParRelGroupNode),
%        send(@prolog, write_ln, string('par rel group:', ParRelGroupNode)),
	( get(ParRelGroupNode, member, ACodeStr, ParRelNode)
	->
%	        send(@prolog, write_ln, string('par rel node exists:', 
%							ParRelNode)),
		send(ParRelNode, colour, Colour),
		send(ParRelNode, expose)
	; 
	        % do nothing
		% send(@prolog, write_ln, string('par rel node did not exist:', 
		% 					ACodeStr))
		true
	).




% highlight par_relation node of type growth_rate = inflow - outflow 
% in state N in Colour
%  
highlight(F, N, Detail, par_relations, Colour,_ElementState):-
	term_to_atom(DetailTerm, Detail), 
        % send(@prolog, write_ln, string('highlight par rel node:', Detail)),
	DetailTerm=..[Rel, A, B], 
	atomize(A, AAtom), 
	find_par_node(F, N, AAtom, NA), 
	
	math_relation(Rel, VRel), 
	construct_code_str(A, B, VRel, ACodeStr), 
%	construct_code_str(A, B, Rel, ACodeStr), 
        % send(@prolog, write_ln, string('highlight search for:', ACodeStr)),

	% check whether a node for this relation exists
	% get the par_relations group node within entity node NA
	get(NA, member, par_relations, ParRelGroupNode),
%        send(@prolog, write_ln, string('par rel group:', ParRelGroupNode)),
	( get(ParRelGroupNode, member, ACodeStr, ParRelNode)
	->
%	        send(@prolog, write_ln, string('par rel node exists:', 
%							ParRelNode)),
		send(ParRelNode, colour, Colour),
		send(ParRelNode, expose)
	; 
	        % do nothing
		% send(@prolog, write_ln, string('par rel node did not exist:', 
		% 					ACodeStr))
		true
	).



% highlight par_value node in state N in Colour
%  
highlight(F, N, Detail, par_values, Colour,_ElementState):-
	term_to_atom(DetailTerm, Detail), 
        % send(@prolog, write_ln, string('highlight par val node:', Detail)),
	DetailTerm=..[value, A, _RealVal, QualVal, _Der], 
	% strip off parameter name if necessary
	strip_atomize(QualVal, Value), 
	find_par_node(F, N, A, NA), 

%        send(@prolog, write_ln, string('highlight search for:', Value)),

	% check whether a node for this value exists
	% get the par_relations group node within entity node NA
	get(NA, member, quantity_space, QSNode),
        % send(@prolog, write_ln, string('QSNode:', QSNode)),
	( get(QSNode, member, Value, ValNode)
	->
%	        send(@prolog, write_ln, string('ValNode:', 
%							ValNode)),
		send(ValNode, colour, Colour),
		send(ValNode, expose)
	; 
	        % do nothing
		% send(@prolog, write_ln, string('ValNode did not exist:', 
		% 					Value))
		true
	).



% highlight parameter in state N in Colour
%  
highlight(F, N, Detail, parameters, Colour,_ElementState):-
	term_to_atom(DetailTerm, Detail), 
	DetailTerm=..[_Par, EntityTerm, Parameter, _Type, _QSpace], 
	atomize(EntityTerm, EntityStr), 
	% get corresponding entity node
	get(F, node, EntityStr, entity, N, NE),	
	% get quantity node
   	get(NE, member, Parameter, NP),
	send(NP, colour, Colour),
	send(NP, expose),
	send(NP, redraw).




% highlight entity in state N in Colour
%  
highlight(F, N, Detail, system_elements, Colour,_ElementState):-
	term_to_atom(DetailTerm, Detail), 
	DetailTerm=..[instance, Entity, _EntityType], 
	% get entity node
	get(F, node, Entity, entity, N, NE),	
	send(NE, colour, Colour),
	send(NE, expose),
	send(NE, redraw).


% highlight attribute links in state N in Colour
%  
highlight(F, _N, Detail, system_elements, Colour,_ElementState):-
	term_to_atom(DetailTerm, Detail), 
	DetailTerm=..[has_attribute, Entity, Attribute, AttrArg], 
	create_attr_link_string(Entity, Attribute, AttrArg, Str), 
	get(F, member, picture, P),  %this is the graphics window for modelfragments is my guess (JJ). Maybe this should be real OO to make it more readable for future generations of garp-adepts or their programmers?
	get(P, find, @default, (@arg1?name == Str), GraphLink),		
	send(GraphLink, colour, Colour).



% highlight attribute nodes in state N in Colour
%  
highlight(F, N, Detail, system_elements, Colour,_ElementState):-
	term_to_atom(DetailTerm, Detail), 
	DetailTerm=..[has_attribute, Entity, Attribute, AttrArg], 
	% get entity node
	get(F, node, Entity, entity, N, NE),	
	send(NE, colour_attribute, Attribute, AttrArg, Colour).

	

% highlight a complete model fragment in state N in Colour
%
highlight(GW, N, Name, system_structures, Colour,ElementState):-
        % Highlight details of system structure Name in state N:
	% both conditions and givens have to be coloured in Colour now
	% (this should be red (was blue before 2.02), 
        % because MF's should only be included 
	% as conditions, not as results, although it's syntactically 
	% possible)
	% get the graphics window GW through the hyper link
	%
	% send(@prolog, write_ln, string('highlight MF: ', Name)),
	
	term_to_atom(NameTerm, Name),
	
	% find conditions, colour them in Colour 
	%
        CondTerm = 
		visualize:find_system_structure_detail(N, 
							NameTerm, 
							conditions,
							Type, 
							Condition),
	forall(user:CondTerm, 
		(
		highlight(GW, N, Condition, Type, Colour, ElementState)
		)
	),

	% find givens, colour them in Colour 
	%
        GivenTerm = 
		visualize:find_system_structure_detail(N, 
							NameTerm, 
							givens,
							Type, 
							Given),
	forall(user:GivenTerm, 
		(
		highlight(GW, N, Given, Type, Colour,ElementState)
		)
	).



% highlight information of a certain Type in state N in Colour
%
% To Do: There may be a few things which are not highlighted yet
% This does not cause big problems, though.
%
highlight(_Frame, _N, _Detail, _Type, _Colour,_ElementState).
