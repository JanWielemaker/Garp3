/*****************************************************************************************************
Garp3 0.1 Based on main source of visigarp (main.pl). This file is split in parts and edited.
All visiualize source based on visigarp 2.05, designed and implemented by Anders Bouwer 18/08/1999
Original copyright: (C) 2004 University of Amsterdam. All rights reserved.

Now:
PART OF Garp3. SEE COPYRIGHT NOTICE.
%code was visigarp, only changed where gp3 mentioned.
*******************************************************************************************************/

%loaded into module (namespace) visualize
% for the input state, do nothing!
%
%used in class_graph_node: click_graphical
show_quantity_details(_Name, 0):-!. 


show_quantity_details(Name, N):- 
        % Show additional details about node in state N
        new(D, dialog(string('Details about %s in state %s', Name, N))),
        D->>icon(@simulate_icon),
	find_quantity_details(N, Name, QuantityPred, Type, Entity, 
					RealVal, QualVal, QSpace, Der),
	atomize(Name, NameStr), 
        send(D, append, new(_T0, text_item(name, NameStr))),
	atomize(Entity, EntityStr),
        send(D, append, new(_T1, text_item(entity, EntityStr))),
        send(D, append, new(_T2, text_item(predicate, QuantityPred))),
        send(D, append, new(_T3, text_item(type, Type))),
        send(D, append, new(_T4, text_item('real value', RealVal))),
        send(D, append, new(_T5, text_item('qual.value', QualVal))),
        send(D, append, new(_T6, text_item(qspace, QSpace))),
        send(D, append, new(_T7, text_item(derivative, Der))),
        send(D, append, imgButton(close, message(D, destroy), tt:= 'Close this window')),
        send(D, default_button, close),
        send(D, open).

