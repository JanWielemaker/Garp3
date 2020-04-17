/*****************************************************************************************************
Garp3 0.1 Based on main source of visigarp (main.pl). This file is split in parts and edited.
All visiualize source based on visigarp 2.05, designed and implemented by Anders Bouwer 18/08/1999
Original copyright: (C) 2004 University of Amsterdam. All rights reserved.

Now:
PART OF Garp3. SEE COPYRIGHT NOTICE.
*******************************************************************************************************/

%loaded into module (namespace) visualize

% create a menu for toggling different types of relations 
% so that they are shown or hidden
%
create_relations_toggle_menu(F):-
	%gp3 0.3.13: there is no need to get the picture member, so we do not send it either
	%get(F, member, picture, P),
     create_relations_toggle_menu(F, dependencies_view_menu).

% create a menu for toggling different types of relations 
% so that they are shown or hidden
%
create_relations_toggle_menu(F, MenuType):-
	%gp3 0.3.13: the left_dialog itself is now created by the causal_graph_viewer itself (see initialise), so we 
	%just have to ask for it
	
	Dialog = F<<-member(left_dialog),
    create_menus(F,Dialog, MenuType), %gp3: now a buttonbar, but anyway. Removed a lot of arguments
    Dialog->>layout. %gp3 0.3.13 added this one
 
create_menus(F, Dialog, MenuType):-
		%GP3 0.2 We changed this to a buttonbar. Without using the buttonbar code
		%(we do not have a dedicated framedWindow)
		%so we use toggleImgButtons here
		%and some patches to make it work
		%we do not use the two calls for the menu
		
		State = F<<-state_nr, %make it a prolog number now
		
		%gp3 0.3.13: code to set layout for the Dialog has moved to causal_graph_viewer->initialise
		
		%because we do not use a menu structure, nor a dedicated class
		%(legacy VG made this all @prolog calls)
		%we create a new generic message this will act a bit like
		%the legacy select_extra / show_hide code used as a default message
		%in the old menu
		

		ViewMsg *= ->>(@prolog,show_relations_viewbar_click, F, Dialog, @receiver),

%%%% begin changes  - AB, 24/09/2005
		% 3 column layout 
		%1
		Dialog->>append(
			imgButton(empty, ->>(@prolog,true),
				img := empty,
				inactImg:= @nil, imgCategory:=buttonbars, tt:='This button does nothing yet'), next_row),
		%2
		Dialog->>append(
			toggleImgButton(entities, ViewMsg, inactImg:= @nil, imgCategory:=buttonbars,
				tt:= when(@arg1?value == @on, 'Hide entities', 'Show entities')), right),

		%3
		Dialog->>append(
			toggleImgButton(attributes, ViewMsg, inactImg:= @nil, imgCategory:=buttonbars, tt:= when(@arg1?value == @on, 'Hide configurations and attributes', 'Show configurations and attributes')), right),		

		%1
		Dialog->>append( %also prolog helper
			imgButton(view_select_all, ->>(@prolog,show_relations_bar_set_all,F,State, Dialog,view,@on),
				img := select_all,
				inactImg:= @nil, imgCategory:=buttonbars, tt:='Select all context'), next_row),

		%2
		Dialog->>append(
			toggleImgButton(quantities, ViewMsg, inactImg:= @nil, imgCategory:=buttonbars, tt:= when(@arg1?value == @on, 'Hide quantities', 'Show quantities')), right),

		%3
		Dialog->>append(
			toggleImgButton(q_spaces, ViewMsg, inactImg:= @nil, imgCategory:=buttonbars, tt:= when(@arg1?value == @on, 'Hide quantity spaces', 'Show quantity spaces')), right),	


		%1
		Dialog->>append(
			imgButton(view_select_none, ->>(@prolog,show_relations_bar_set_all,F,State, Dialog,view,@off),
				img := select_none,
				inactImg:= @nil, imgCategory:=buttonbars, tt:='Deselect all context'), next_row),
		%2
		Dialog->>append(
			toggleImgButton(values, ViewMsg, inactImg:= @nil, imgCategory:=buttonbars, tt:= when(@arg1?value == @on, 'Hide values', 'Show values')), right),
		%3
		Dialog->>append(
			toggleImgButton(derivatives, ViewMsg, inactImg:= @nil, imgCategory:=buttonbars, tt:= when(@arg1?value == @on, 'Hide derivative values', 'Show derivative values')), right),	

		%old vg code:
		settings(MenuType, on(OnList), off(OffList)),
		forall(member(Off, OffList), 
				ignore(?(Dialog,member,Off)->>value(@off)) %gp3 changed way of setting
	        ),
		forall(member(On, OnList), 
	           ignore(?(Dialog,member,On)->>value(@on))	%gp3
	         ),
			
	    %next: the former rel_type_menu
	    %default message
	    %we can skip an extra prolog clause here, the old one does still work
	    %(only select_extra has gone, no extras needed)
	    %we keep using execute_rel_type instead of gen_causal_model_rel
	    %because some new buttons are aggregated old concepts
	    
	 	RelMsg *= ->>(@prolog,execute_rel_type,F,State,@receiver?name, @receiver?value),
	
	 	%causal model
	 	Dialog->>append(graphical(0,0,0,8)),
		%1
		Dialog->>append(
			imgButton(cm_select_all, ->>(@prolog,show_relations_bar_set_all,F,State, Dialog,cm,@on),
				img := select_all,
				inactImg:= @nil, imgCategory:=buttonbars, tt:='Select all causal'), next_row),
		%2
		Dialog->>append(
			toggleImgButton('I+', RelMsg, imgCategory:=buttonbars, img:=i_plus,inactImg:= @nil, 
				tt:= when(@arg1?value == @on, 'Hide positive influences', 'Show positive influences'), value := @on), right),
		%3
		Dialog->>append(
			toggleImgButton('I-', RelMsg, imgCategory:=buttonbars, img := i_min,inactImg:= @nil,  tt:= when(@arg1?value == @on, 'Hide negative influences', 'Show negative influences'), value := @on), right),		

		%1
		Dialog->>append(
			imgButton(cm_select_none, ->>(@prolog,show_relations_bar_set_all,F,State, Dialog,cm,@off),
				img := select_none,
				inactImg:= @nil, imgCategory:=buttonbars, tt:='Deselect all causal'), next_row),

		%2
		Dialog->>append(
			toggleImgButton('P+', RelMsg, imgCategory:=buttonbars, img:=p_plus, inactImg:= @nil, tt:= when(@arg1?value == @on, 'Hide positive proportionalities', 'Show positive proportionalities'), value := @on), right),
		%3
		Dialog->>append(
			toggleImgButton('P-', RelMsg, imgCategory:=buttonbars, img:=p_min,inactImg:= @nil,  tt:= when(@arg1?value == @on, 'Hide negative proportionalities', 'Show negative proportionalities'), value := @on), right),	


		%inequalities
	 	Dialog->>append(graphical(0,0,0,8)),


		%1
		Dialog->>append(
			toggleImgButton('<', RelMsg,
				imgCategory:=buttonbars, img := ineq_s,inactImg:= @nil,
				tt:= when(@arg1?value == @on, 'Hide smaller', 'Show smaller'), value := @on), next_row),	

		%2
		Dialog->>append(
			toggleImgButton('=', RelMsg,
				imgCategory:=buttonbars, img := ineq_equal,inactImg:= @nil,
				tt:= when(@arg1?value == @on, 'Hide equal', 'Show equal'), value := @on), right),	
		%3
		Dialog->>append(
			toggleImgButton('>', RelMsg,
				imgCategory:=buttonbars, img := ineq_l,inactImg:= @nil,
				tt:= when(@arg1?value == @on, 'Hide larger', 'Show larger'), value := @on), right),		

		%1
		Dialog->>append(
			toggleImgButton('d<', RelMsg, 
				imgCategory:=buttonbars, img:=ineq_d_s,inactImg:= @nil, 
				tt:= when(@arg1?value == @on, 'Hide derivative smaller', 'Show derivative smaller'), value := @on), next_row),

		%2
		Dialog->>append(
			toggleImgButton('d=', RelMsg, 
				imgCategory:=buttonbars, img:=ineq_d_equal,inactImg:= @nil, 
				tt:= when(@arg1?value == @on, 'Hide derivative equal', 'Show derivative equal'), value := @on), right),

		%3
		Dialog->>append(
			toggleImgButton('d>', RelMsg, 
				imgCategory:=buttonbars, img:=ineq_d_l,inactImg:= @nil, 
				tt:= when(@arg1?value == @on, 'Hide derivative larger', 'Show derivative larger'), value := @on), right),


		%1
		Dialog->>append(
			imgButton(ineq_select_all, ->>(@prolog,show_relations_bar_set_all,F,State, Dialog,ineq,@on),
				img := select_all,
				inactImg:= @nil, imgCategory:=buttonbars, tt:='Select all inequalities'), next_row),

		%2
		Dialog->>append(
			toggleImgButton('<=', RelMsg,
				imgCategory:=buttonbars, img := ineq_se,inactImg:= @nil,
				tt:= when(@arg1?value == @on, 'Hide smaller or equal', 'Show smaller or equal'), value := @on), right),	

		%3 
		Dialog->>append(
			toggleImgButton('>=', RelMsg,
				imgCategory:=buttonbars, img := ineq_le,inactImg:= @nil,
				tt:= when(@arg1?value == @on, 'Hide larger or equal', 'Show larger or equal'), value := @on), right),	


		%1
		Dialog->>append(
			imgButton(ineq_select_none, ->>(@prolog,show_relations_bar_set_all,F,State, Dialog,ineq,@off),
				img := select_none,
				inactImg:= @nil, imgCategory:=buttonbars, tt:='Deselect all inequalities'), next_row),
		%2
		Dialog->>append(
			toggleImgButton('d<=', RelMsg, 
				imgCategory:=buttonbars, img:=ineq_d_se,inactImg:= @nil, 
				tt:= when(@arg1?value == @on, 'Hide derivative smaller or equal', 'Show derivative smaller or equal'), value := @on), right),

		%3
		Dialog->>append(
			toggleImgButton('d>=', RelMsg, 
				imgCategory:=buttonbars, img:=ineq_d_le,inactImg:= @nil, 
				tt:= when(@arg1?value == @on, 'Hide derivative larger or equal', 'Show derivative larger or equal'), value := @on), right),



		%correspondences
	 	Dialog->>append(graphical(0,0,0,8)),


		%1
		Dialog->>append(
			toggleImgButton('F', RelMsg,
				imgCategory:=buttonbars, img := fq,inactImg:= @nil,
				tt:= when(@arg1?value == @on, 'Hide full correspondences', 'Show full correspondences'), value := @on), next_row),	
%		?(Dialog,member,'F')->>alignment(column), %in the same col als mQ

		%2
		Dialog->>append(
			toggleImgButton('dQ', RelMsg, 
				imgCategory:=buttonbars, img:=dq,inactImg:= @nil, 
				tt:= when(@arg1?value == @on, 'Hide derivative correspondences', 'Show derivative correspondences'), value := @on), right),
		%3
		Dialog->>append(
			toggleImgButton('Q', RelMsg,
				imgCategory:=buttonbars, img := q,inactImg:= @nil,
				tt:= when(@arg1?value == @on, 'Hide quantity correspondences', 'Show quantity correspondences'), value := @on), right),

		%1
		Dialog->>append(
			imgButton(corr_select_all, ->>(@prolog,show_relations_bar_set_all,F,State, Dialog,corr,@on),
				img := select_all,
				inactImg:= @nil, imgCategory:=buttonbars, tt:='Select all correspondences'), next_row),

		%2
		Dialog->>append(
			toggleImgButton('dV', RelMsg, 
				imgCategory:=buttonbars, img:=dv,inactImg:= @nil, 
				tt:= when(@arg1?value == @on, 'Hide derivative value correspondences', 'Show derivative value correspondences'), value := @on), right),
		%3
		Dialog->>append(
			toggleImgButton('V', RelMsg,
				imgCategory:=buttonbars, img := v,inactImg:= @nil,
				tt:= when(@arg1?value == @on, 'Hide value correspondences', 'Show value correspondences'), value := @on), right),

		%1
		Dialog->>append(
			imgButton(corr_select_none, ->>(@prolog,show_relations_bar_set_all,F,State, Dialog,corr,@off),
				img := select_none,
				inactImg:= @nil, imgCategory:=buttonbars, tt:='Deselect all correspondences'), next_row),

		%2
		Dialog->>append(
			toggleImgButton('mdQ', RelMsg, 
				imgCategory:=buttonbars, img:=mdq,inactImg:= @nil, 
				tt:= when(@arg1?value == @on, 'Hide inverse derivative correspondences', 'Show inverse derivative correspondences'), value := @on), right),
		%3
		Dialog->>append(
			toggleImgButton('mQ', RelMsg,
				imgCategory:=buttonbars, img := mq,inactImg:= @nil,
				tt:= when(@arg1?value == @on, 'Hide inverse quantity correspondences', 'Show inverse quantity correspondences'), value := @on), right).
%%%% end of changes - AB, 24/09/2005


show_relations_viewbar_click(F,Dialog, Button):-
	%gp3 0.2: This is the generic prolog call for the buttons in the view-part
	%of the buttonbar. This replaces the general call used in the old menu-object
	%Purpose: make sure everything needed is selected as well
	%and make sure the right items are shown
	
	%show_relations_viewbar_select_extra is the replacement for the legacy select_extra
	%viewbar part
	
	Type = Button<<-name,
	Value = Button<<-value, %@on or @off
	show_relations_viewbar_select_extra(Type, Value, OnList, OffList),
	forall(
		member(On, OnList),
		?(Dialog,member,On)->>value(@on)
	),
	forall(
		member(Off, OffList),
		?(Dialog,member,Off)->>value(@off)
	),
	%call show_hide
	show_hide(F,Type,Value).
%

%items to turn off or on when another item is turned on or off
% if values is turned on, turn on quantities and q_spaces too
show_relations_viewbar_select_extra(values,@on,[quantities,q_spaces],[]).
% if derivatives is turned on, turn on quantities
show_relations_viewbar_select_extra(derivatives,@on,[quantities],[]).
% if quantity_spaces is turned on, turn on quantities
show_relations_viewbar_select_extra(q_spaces,@on,[quantities],[]).
% if attributes is turned on, turn on entities too
show_relations_viewbar_select_extra(attributes,@on,[entities],[]).
% if q_spaces is turned off, turn off values too	
show_relations_viewbar_select_extra(q_spaces,@off,[],[values]).
% if entities is turned off, turn off attributes too
show_relations_viewbar_select_extra(entities,@off,[],[attributes]).
% if quantities is turned off, turn off quantity spaces, values and derivatives too
show_relations_viewbar_select_extra(quantities,@off,[],[q_spaces,values,derivatives]).
% in all other cases, no extra action is required
show_relations_viewbar_select_extra(_,_,[],[]).

%%
/*
show_relations_bar_set_all(F, State, Dialog,Type,Value)
	%gp3 0.2 Set all buttons in the 'TYPE' part of the buttonbar to Value
	%and update status (using show_hide or execute_rel_type)
	%replaces execute message of old all and none menu items + helper code
*/
	
show_relations_bar_set_all(F, _State, Dialog,view, Value):-
%view part
	forall(member(B,[entities,attributes,quantities,q_spaces,values,derivatives]),
		(
			?(Dialog,member,B)->>value(Value),
			show_hide(F,B,Value)
		)
	).
%
show_relations_bar_set_all(F, State, Dialog,cm, Value):-
%cm part
%we keep using execute_rel_type instead of gen_causal_model_rel
%because some new buttons are aggregated old concepts
	forall(member(B,['I+','I-','P+','P-']),
		(
			?(Dialog,member,B)->>value(Value),
			ignore(execute_rel_type(F,State,B,Value))
		)
	).
%
show_relations_bar_set_all(F, State, Dialog,ineq, Value):-
%inequalities part
	forall(member(B,['d>','>','d>=','>=','d=','=','d<=','<=','d<','<']),
		(
			?(Dialog,member,B)->>value(Value),
			ignore(execute_rel_type(F,State,B,Value))
		)
	).
%
show_relations_bar_set_all(F, State, Dialog,corr, Value):-
%correspondences part
	forall(member(B,['dQ','Q','dV','V','mdQ','mQ','F']),
		(
			?(Dialog,member,B)->>value(Value),
			ignore(execute_rel_type(F,State,B,Value))
		)
	).



%gp3 0.2: all old special cases are gone, but we have some new 'agregate types'
execute_rel_type(F, N,'dQ', OnOrOff):-
	!,
	ignore(gen_causal_model_rel(F,N, 'dQ', OnOrOff)),
	ignore(gen_causal_model_rel(F,N, 'dQ^', OnOrOff)).
%
execute_rel_type(F, N,'Q', OnOrOff):-
	!,
	ignore(gen_causal_model_rel(F,N, 'Q', OnOrOff)),
	ignore(gen_causal_model_rel(F,N, 'Q^', OnOrOff)).
%
execute_rel_type(F, N,'dV', OnOrOff):-
	!,
	ignore(gen_causal_model_rel(F,N, 'dV', OnOrOff)),
	ignore(gen_causal_model_rel(F,N, 'dV^', OnOrOff)).
%
execute_rel_type(F, N,'V', OnOrOff):-
	!,
	ignore(gen_causal_model_rel(F,N, 'V', OnOrOff)),
	ignore(gen_causal_model_rel(F,N, 'V^', OnOrOff)).
%
execute_rel_type(F, N,'mQ', OnOrOff):-
	!,
	ignore(gen_causal_model_rel(F,N, 'mQ', OnOrOff)),
	ignore(gen_causal_model_rel(F,N, 'mQ^', OnOrOff)).
%
execute_rel_type(F, N,'mdQ', OnOrOff):-
	!,
	ignore(gen_causal_model_rel(F,N, 'mdQ', OnOrOff)),
	ignore(gen_causal_model_rel(F,N, 'mdQ^', OnOrOff)).
%
execute_rel_type(F, N,'F', OnOrOff):-
	!,
	ignore(gen_causal_model_rel(F,N, 'F', OnOrOff)),
	ignore(gen_causal_model_rel(F,N, 'F^', OnOrOff)).

% Individual dependency types
%
execute_rel_type(F, N, RelTypeItem, OnOrOff):-
        gen_causal_model_rel(F, N, RelTypeItem, OnOrOff).

