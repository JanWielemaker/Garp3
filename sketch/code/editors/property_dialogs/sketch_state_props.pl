/*
definitie sketchStatePropsDlg klasse.

De eigenschappen dialoog van sketchState, met 
editor voor het bewerken van sketchInequalities

Initialise kan aangeroepen met een bestaande sketchIneq voor het bewerken ervan.
Wanneer initialise wordt gebruikt zonder argumenten wordt in new mode geopend.

based on homer code, so most comments in dutch. gp3 code only where mentioned
2000-2005 Jelmer Jellema, Spin in het Web, spininhetweb.nl
2006, Anders Bouwer.

*/

:-pce_begin_class(sketchStatePropsDlg,
		  % assistanceDialog,
		  propertyDialog,
		  "sketchState properties dialog"
		 ).


default_sketchStateName('0').
defaultLHSName('NewQuantity'). 
defaultRHSName('NewValue'). 

variable(startType,{sketchState},both).
variable(prevName,name,both). %voor de standaardnaam
variable(previousValueName,name,both). 
variable(previousValue,any*,both). 
variable(previousQuantityL,any*,both). 
variable(previousQuantityNameL,name,both). 
variable(previousQuantityR,any*,both). 
variable(previousQuantityNameR,name,both). 
variable(element,any*,both,"The edited sketch state element. @nil means: new one").
variable(sketchInequalities,chain,both,"The edited sketch inequalities. @nil means: new one").
variable(sketchValues,chain,both,"The edited sketch values").
variable(sketchQuantities,chain,both,"The edited sketch quantities").
variable(sketchInequality,object,both,"The currently selected sketch inequality. @nil means: new one").

variable(garpModel, garpModel, both, 'The model associated to this editor').


%%
initialise(D, OpenedBy:[object]):->
	"Initialise the editor" ::
	%gp3 0.1: added openedBy, will be a hyper to the given object, so others can ask voor ?openedBy
	
	D->+initialise('Sketch State properties - Sketch', OpenedBy, 'Sketch_StateProps'),
	% D->+initialise('Sketch State properties - Sketch', 'Sketch_StateProps'),
	D->>icon(@build_icon),
	D->>application(@app),
	D->>kind(toplevel),
        default_sketchStateName(DefaultName), 
        D->>prevName(DefaultName), 
        new(Ineqs, chain), 
        D->>sketchInequalities(Ineqs),

        D->>previousValueName(''),
        D->>previousValue(@nil),
        D->>previousQuantityNameL(''),
        D->>previousQuantityL(@nil),
        D->>previousQuantityNameR(''),
        D->>previousQuantityR(@nil),
        % set sketchValues
        % new(Values, chain), 
        % D->>sketchValues(Values),
        get(@model, hypered, stgraphMF, STGraphMF), 
        get(STGraphMF, sortedSketchValues, SketchValues), 
        % copy for editing purposes         
        send(D, sketchValues, SketchValues?copy), 

        % set sketchQuantities
        get(STGraphMF, sortedSketchQuantities, SketchQuantities), 
        % copy for editing purposes
        send(D, sketchQuantities, SketchQuantities?copy), 

	%gp3: saved openedBy in a hyper
	default(OpenedBy,@nil,Opener),
	D->>hyper(Opener,openedBy),
	
	%lay-out:
	%we doen alles met de hand op coordinaten, anders
	%wordt de lay-out niet ok...

	%voor de plaatsing gebruiken we de standard "gap"
	GapX = D?gap<<-width,
	GapY = D?gap<<-height,
	MaxX *= number(0),

	%de onderdelen

        % Listbrowser with Sketch Inequalities for the selected State
	SketchIneqList *= extendedListBrowser(width := 30),
	SketchIneqList->>name(sketchIneqList),
	SketchIneqList->>label('Value and (in)equality statements:'),
	SketchIneqList->>show_label(@on),
	SketchIneqList->>select_message(->>(D,
				       onSketchIneqSelection,
						@arg1?object)),
	SketchIneqList->>style(normaal,style(colour:=black)), 
	SketchIneqList->>style(readonly,style(colour:=grey50)), 


        % State ID = Name 
	Name *= eventTextItem(stateID), 
	Name->>label('State ID:'),
	Name->>length(SketchIneqList?width),
        % Generate a State ID (an integer which is not in use yet)
        get(D, generate_state_ID, StateID), 
        Name->>value(StateID),
	D->>display(Name, point(GapX, D?topY + GapY)),
	% D->>display(Name, point(GapX, Line?bottom_side + GapY)),
	MaxX->>maximum(Name?right_side),
 
	%een lijntje
	Line *= line(GapX,Name?bottom_side + GapY,0,Name?bottom_side + GapY ),
	Line->>name(line1),
	Line->>pen(2), 
	D->>display(Line), %rechterkant wordt hieronder wel gezet, als MaxX bekend is

	D->>display(SketchIneqList,point(GapX,Line?bottom_side + GapY)), %geen align

	%display vd buttons is lastig: ten eerste moet de bovenste precies bij de bovenkant vd lijst
	%maar het moet ook mooi passen dus eventueel moet de lijst hoger
	%daarom maken we eerst alle knoppen voordat we ze plaatsen

	AddSketchIneq *= imgButton(addSketchIneq, img:=sketch_add_statement, tt:='Add statement'),
	CopySketchIneq *= imgButton(copySketchIneq, img:=sketch_copy_statement, tt:='Copy selected statement'),
	RemoveSketchIneq *= imgButton(removeSketchIneq, img:=sketch_remove_statement, tt:='Delete selected statement'),

	%ok: aangezien alles gerelateerd is aan de bovenkant en de onderkant van de lijst
	%moeten we even kijken of dat wel gaat passen

	ListBottom *= number(SketchIneqList?bottom_side),
	ListBottom->>maximum(SketchIneqList?top_side + SketchIneqList?image?top_side + 
					 AddSketchIneq?height + 
					 CopySketchIneq?height + 
				     RemoveSketchIneq?height + 
					 2 * GapY), %ruimte ingenomen door label vd lijst + buttons

	SketchIneqList->>bottom_side(ListBottom),

	%weergave Add button precies bij de bovenkant vd lijst:
	%SketchIneqList?image is het echte lijst window (text_image)
	%die de list_browser als device gebruikt.
	%Dus geeft SketchIneqList?image?top_side de afstand van bovenin
	%de list_browser tot de bovenkant vh window (dus de hoogte
	%van het label). Dit opgeteld bij de bovenkant van de
	%list_browser geeft dus de y-coordinaat in termen van de
	%dialoog.

	D->>display(AddSketchIneq,
		point(SketchIneqList?right_side + GapX,
			SketchIneqList?top_side+ SketchIneqList?image?top_side)), %hebben we nu list_top voor

	MaxX->>maximum(AddSketchIneq?right_side),


	%deze moet dus halverwege komen..
	D->>display(CopySketchIneq,point(AddSketchIneq?left_side,
				AddSketchIneq?top_side + ((SketchIneqList?bottom_side - AddSketchIneq?top_side) / 2 )
					- (CopySketchIneq?height / 2))),

	MaxX->>maximum(CopySketchIneq?right_side),

	
	D->>display(RemoveSketchIneq,point(AddSketchIneq?left_side,
		SketchIneqList?bottom_side - RemoveSketchIneq?height)), %precies onder

	MaxX->>maximum(RemoveSketchIneq?right_side),


	%een lijntje
	Line2 *= line(GapX,SketchIneqList?bottom_side + GapY,0,SketchIneqList?bottom_side + GapY ),
	Line2->>name(line2),
	Line2->>pen(2), 
	D->>display(Line2), %rechterkant wordt hieronder wel gezet, als MaxX bekend is


        % SketchQuantitiesBrowser 
	SketchQuantitiesBrowser *= sketchQuantityBrowser(width := 24, height:=8), 
	SketchQuantitiesBrowser->>name(sketchQuantitiesBrowser),
	% SketchQuantitiesBrowser->>multiple_selection(@on), 
	SketchQuantitiesBrowser->>label('Quantities:'),
	SketchQuantitiesBrowser->>select_message(->>(D, onQuantitySelect)),
	D->>display(SketchQuantitiesBrowser,point(GapX, Line2?bottom_side + 5*GapY)),
	D->>fillSketchQuantitiesBrowser(SketchQuantitiesBrowser),

	%Type menu in een midden kolom
	%hun gezamelijke breedte doet er dus toe
	% Type *= menu(type,marked),
	Type *= menu(type,cycle, ->>(D, onTypeSelection)),
	% Type *= menu(type,choice, ->>(D, onTypeSelection)),
	% Type->>label('Type:'),
	Type->>show_label(@off),
	Type->>layout(horizontal),
	% Type->>layout(vertical),

        % Inequality sign strings - for cycle menu
        % For >= and =<, mathematical characters are used. 
        % See XPCE Demo programs - Font viewer, Unicode chart 
        % 'mathematical operators' for char codes in different fonts
        % Usage: \u + hex code for a specific char
	Type->>append(menu_item(g,@default,'>')),
	% Type->>append(menu_item(geq,@default,'>=')),
	Type->>append(menu_item(geq,@default,'\u2265')), % >= char
	Type->>append(menu_item(eq,@default,'=')),
	% Type->>append(menu_item(leq,@default,'=<')),
	Type->>append(menu_item(leq,@default,'\u2264')), % \u + hex code for =< char
	Type->>append(menu_item(l,@default,'<')),

	MiddenBreedte *= number(Type?width),
        Type->>selection(eq),
	% Type->>message(->>(D, onTypeSelection)),
	D->>display(Type,point(SketchQuantitiesBrowser?right_side + GapX +
					((MiddenBreedte / 2) -
					(Type?width / 2)), SketchQuantitiesBrowser?top_side)),
        % create choice menu for Values or Quantities for the RHS of inequality
	get(D, initButtonbar, BB), 
	D->>display(BB,point(SketchQuantitiesBrowser?right_side+GapX, Line2?bottom_side + 5*GapY-1)),

        % SketchValuesBrowser
	SketchValuesBrowser *= sketchQuantityBrowser(width := 24, height:=8), 
	SketchValuesBrowser->>name(sketchValuesBrowser),
	SketchValuesBrowser->>label('Values:'),
	SketchValuesBrowser->>show_label(@off),
	SketchValuesBrowser->>select_message(->>(D, onValueSelect)),
	% D->>display(SketchValuesBrowser,point(Type?right_side-Type?width/2+GapX, Line2?bottom_side + 5*GapY)),
	% D->>display(SketchValuesBrowser,point(SketchQuantitiesBrowser?right_side+GapX, Line2?bottom_side + 5*GapY)),
	D->>display(SketchValuesBrowser,point(SketchQuantitiesBrowser?right_side+GapX, BB?bottom_side)),
	D->>fillSketchValuesBrowser(SketchValuesBrowser),

        % SketchQuantitiesBrowser2
	SketchQuantitiesBrowser2 *= sketchQuantityBrowser(width := 24, height:=8), 
	SketchQuantitiesBrowser2->>name(sketchQuantitiesBrowser2),
	SketchQuantitiesBrowser2->>label('Quantities:'),
	SketchQuantitiesBrowser2->>show_label(@off),
	SketchQuantitiesBrowser2->>select_message(->>(D, onQuantitySelect2)),
        % display at same place as the values browser, just expose one or the other
	D->>display(SketchQuantitiesBrowser2,point(SketchQuantitiesBrowser?right_side+GapX, BB?bottom_side)),
	D->>fillSketchQuantitiesBrowser(SketchQuantitiesBrowser2),

	%  quantity name, value name and sign below inequality list

	% ValueType *= menu(zeroMenu,toggle, ->>(D,onZero)),
	% ValueType->>label('LHS:'),
	% ValueType->>layout(horizontal),
	% ValueType->>append(zero),

	QuantityName *= eventTextItem(quantityName),
	% QuantityName->>label('LHS:'), 
	QuantityName->>show_label(@off), 
	QuantityName->>length(10), %will be recalculated
	% QuantityName->>message(->>(D, onQuantityNameLR, left)),
	QuantityName->>afterKey(->>(D, onQuantityNameLR, left)),

	Sign *= eventTextItem(sign),
	Sign->>editable(@off), 
	Sign->>show_label(@off), 
	Sign->>length(5), %will be recalculated
	Sign->>message(->>(D, onSign)),
	% Sign->>afterKey(->>(D, onSign)),

	RHSName *= eventTextItem(rhsName),
	% RHSName->>label('RHS:'), 
	RHSName->>show_label(@off), 
	RHSName->>length(10), %will be recalculated
	% RHSName->>message(->>(D, onRHSName)),
	RHSName->>afterKey(->>(D, onRHSName)),

	
	% D->>display(ValueType,point(GapX, SketchQuantitiesBrowser?bottom_side + GapY)),
	% D->>display(QuantityName, point(ValueType?right_side + GapX, SketchQuantitiesBrowser?bottom_side + GapY)),
	D->>display(QuantityName, point(GapX, Line2?bottom_side + GapY)),
	% D->>display(Sign, point(SketchQuantitiesBrowser?right_side + GapX +
	%				((MiddenBreedte / 2) -
	%				(Type?width / 2)), Line2?bottom_side + GapY)),
	D->>display(Type, point(SketchQuantitiesBrowser?right_side - (Type?width/2) + GapX +
					((MiddenBreedte / 2) -
					(Type?width / 2)), Line2?bottom_side + GapY)),
        % display these two fields at the same point, just expose one or the other
	D->>display(RHSName, point(Type?right_side+GapX, Line2?bottom_side + GapY)),
	% D->>display(QuantityName2, point(Type?right_side+GapX, Line2?bottom_side + GapY)),

        % Add and delete buttons for Quantities (LHS)
	AddQuantity *= imgButton(addSketchQuantityLeft, img:=element_new, tt:='Add sketch quantity'),
	DeleteQuantity *= imgButton(deleteSketchQuantityLeft, img:=element_delete, tt:='Delete selected sketch quantity'),
        D->>display(AddQuantity, point(GapX, SketchQuantitiesBrowser?bottom_side)),
        D->>display(DeleteQuantity, point(AddQuantity?right_side, SketchQuantitiesBrowser?bottom_side)),

        % Add and delete buttons for Values/Quantities (RHS)
	AddRHS *= imgButton(addSketchValueOrQuantity, img:=element_new, tt:='Add sketch value/quantity'),
	DeleteRHS *= imgButton(deleteSketchValueOrQuantity, img:=element_delete, tt:='Delete selected sketch value/quantity'),
        D->>display(AddRHS, point(SketchValuesBrowser?left_side, SketchValuesBrowser?bottom_side)),
        D->>display(DeleteRHS, point(AddRHS?right_side, SketchValuesBrowser?bottom_side)),    

	Remarks *= editor(height := 4, width := 40),
	Remarks->>name(remarks),
	Remarks->>label('Remarks:'),
	Remarks->>show_label(@on),
	Remarks->>fill_mode(@on),
	Remarks->>font(Name?font),
	% D->>display(Remarks,point(GapX, ValueType?bottom_side + GapY)),
	% D->>display(Remarks,point(GapX,	SketchQuantitiesBrowser?bottom_side + GapY)),
	D->>display(Remarks,point(GapX,	AddQuantity?bottom_side + GapY)),
	MaxX->>maximum(Remarks?right_side),
	%en nu weer een lijntje

	Line3 *= line(GapX,Remarks?bottom_side + GapY,0,Remarks?bottom_side + GapY ),
	Line3->>name(line3),
	Line3->>pen(2), 
	D->>display(Line3), %rechterkant wordt hieronder wel gezet, als MaxX bekend is

	Save *= imgButton(save, img:= sketch_save, tt:='Save changes'),

	D->>display(Save,point(GapX,
							Line3?bottom_side + GapY)),

	Cancel *= imgButton(cancel, img:= sketch_undo, tt:='Undo changes'),
	%deze komt rechts van save
	D->>display(Cancel,point(Save?right_side + GapX,
				Save?top_side)),

	%sluiten moet rechts komen, dus dat moet ook nog uitgezocht
	Close *= imgButton(close,->>(D,onDestroy), tt:='Close this editor'), 
	MaxX->>maximum(Cancel?right_side + GapX + Close?width),
	MaxX->>maximum(SketchValuesBrowser?right_side),
	%oftewel: eventueel wordt de dlg nog breder om deze button te laten passen
	%hoe dan ook helemaal rechts

	D->>display(Close,point(
		MaxX - Close?width,
		Save?top_side)),

	%Het een en ander moet nog mooi naar rechts
	%gp3 0.2: we do a lot more about placement here, because that makes resize easier
	AddSketchIneq->>set(x:= MaxX - AddSketchIneq?width),
	CopySketchIneq->>set(x:= MaxX - CopySketchIneq?width),
	RemoveSketchIneq->>set(x:= MaxX - RemoveSketchIneq?width),
	SketchIneqList->>right_side(AddSketchIneq?left_side - GapX),
	
	Line->>end_x(MaxX),
	Name->>right_side(MaxX),
	
	QuantityName->>right_side(SketchQuantitiesBrowser?right_side - (Type?width/2) ),
	RHSName->>right_side(SketchValuesBrowser?right_side),
	% QuantityName2->>right_side(SketchValuesBrowser?right_side),
	Remarks->>right_side(SketchValuesBrowser?right_side),
	
	Line2->>end_x(MaxX),
	Line3->>end_x(MaxX),

	D->>updateSpacers, %gp3 0.3.13 needed when assistanceDialog is used, but displayContent is not used to fill the dialog
	D->>assign_accelerators, %nodig voor de accels als je niet append gebruikt	

	%we roepen onDestroy aan om te checken of het wel mag
	%dit gaat via done_message vh frame, die roept gewoonlijk frame->>wm_delete aan
	D->>done_message(->>(D,onDestroy)),
	D->>confirm_done(@off), %niet vragen
				%minimal size:
	D->>minimalSize(size(MaxX,Close?bottom_side)), %abs min

        % Turn off remove and copy buttons as long as no Inequality is selected
	D?removeSketchIneq->>active(@on),
	D?copySketchIneq->>active(@on),

	D->>selectMode(values),        

        % set keyboard focus to quantityName field
	D->>caret(D?quantityName_member),

	% Multiple model support
	get(@model, getModelNameForEditor, 'Sketch State properties - Sketch', ModelNameForEditor),
	new(Garp3EditorFrame, garp3EditorFrame(ModelNameForEditor)),
	get(@model, '_value', Model),
	send(Garp3EditorFrame, associateModel, Model),
	send(Garp3EditorFrame, append, D),
	send(Garp3EditorFrame, transient_for, OpenedBy).
%%



%%
initButtonbar(D, CM: menu):<-
	new(CM, menu(select)),
        send(CM, show_label, @off), 
	send(CM, layout, horizontal), 
	send(CM, alignment, left), 
	send(CM, kind, choice),
	% why doesn't this work? 
	% send(CM, append, menu_item(values, message(D, selectMode, values), label(values, 'Values:', font:=bold))),
	% send(CM, append, menu_item(quantities, message(D, selectMode, quantities), label(quantities, 'Quantities:', font:=bold))),
	send(CM, append, menu_item(values, message(D, selectMode, values))),
	send(CM, append, menu_item(quantities, message(D, selectMode, quantities))),
	% send(CM, gap, size(0,10)), % difference between gap & border?
	send(CM, gap, size(0,0)), % difference between gap & border?
	send(CM, border, 0),       % difference between gap & border?
	Width is 30,
	send(CM, value_width, Width), 
	% send(CM, height, Height), % this does not seem to work.
	send(CM, format, center).
%%


%%
selectMode(D, Mode: {values, quantities}):<-
	B = D<<-select_member,
	get(B, selection, Sel), 
	% B = D<<-buttonBar,
	if
		% @on = B?quantitiesMode_member<<-value
		Sel = quantities
	then
		Mode = quantities
	else
		Mode = values.
%
selectMode(D,Mode: {values, quantities}):->
	%change the the internal mode as reported by <-selectMode
	%also changes interface buttons, but no side-effects	
	if
		Mode = values
	then
        (
	        D->>checkSaveSketchQuantity(right),!,

                % reset everything about the right-hand side Quantities list browser
                send(D?sketchQuantitiesBrowser2, selection, @nil),
                D->>previousQuantityNameR(''),
                D->>previousQuantityR(@nil),

		% D?buttonBar?valuesMode_member->>value(@on)
		send(D?select_member, selection, 'values'),

	        D?sketchValuesBrowser->>expose,
                if D?sketchValuesBrowser?selection->>equal(@nil)
                then 
	           D?rhsName->>clear
                else
                (
                   D?rhsName->>value(D?sketchValuesBrowser?selection?name)
                )
        )
	else
        (
                % Mode = quantities
	        D->>checkSaveSketchValue,!,       

                % reset everything about Values
                send(D?sketchValuesBrowser, selection, @nil),
                D->>previousValueName(''),
                D->>previousValue(@nil),

		% D?buttonBar?quantitiesMode_member->>value(@on).
		send(D?select_member, selection, 'quantities'),
	        D?sketchQuantitiesBrowser2->>expose,
                if D?sketchQuantitiesBrowser2?selection->>equal(@nil)
                then 
	           D?rhsName->>clear
                else
                (
                   D?rhsName->>value(D?sketchQuantitiesBrowser2?selection?name)
                )
        ).
%%



%%
generate_state_ID(_D, StateID):<-
	"Generate an integer as state ID" ::
        get(@model, hypered, stgraphMF, STGraphMF), 
        get(STGraphMF, elements, Elements), 
	chain_list(Elements, EList), 
        findall(N, 
		(
		    member(E, EList), 
		    send(E, instance_of, sketchStateElement),
		    new(Number, number(E?name)),
		    get(Number, value, N)
		), 
		Nrs),
        list_max(Nrs, Temp), 
        max(0, Temp, TempMax), 
        StateID is TempMax + 1.
%	

list_max([], 0).

list_max(Nrs, Max):-
        sort(Nrs, Sorted),
        last(Sorted, Max).


/* does not work correctly yet...
%%
onResize(D, Difference: size):->
	%gp3 0.2: 
	       
	D?remarks_member->>right_side(D?remarks_member?right_side + Difference?width),
	D?remarks_member->>set(y:=D?remarks_member?top_side + Difference?height),
	D?remarks_member->>set(height:=D?remarks_member?height + Difference?height),

	D?save_member->>set(y:=D?save_member?top_side + Difference?height),
	D?cancel_member->>set(y:=D?save_member?top_side),
	D?close_member->>set(x:= D?close_member?left_side + Difference?width, y:=D?save_member?top_side).
%%
*/

%%
onDestroy(D):->
	%check of destroy wel mag
	%zie initialise waarbij dit als done_message wordt gezet
	D->>checkSaveSketchQuantity(left),!,       
	D->>checkSaveSketchQuantity(right),!,       
	D->>checkSaveSketchValue,!,       
	D->>checkSaveSelection,!,    % ok, proceed
        send(D, wm_delete).
%%

%%
openedBy(D, OpenedBy: object*):<-
	%gp3, give the object that opened this window or @nil
	OpenedBy = D<<-hypered(openedBy)
	;
	OpenedBy = @nil.
%%

%%
label(D,L: name):->
	%zorg ervoor dat het framelabel wordt gezet, niet het window_decorator label
	D?frame->>label(L).
%
label(D, L: name):<-
	L = D?frame<<-label.
%%

%%
edit(D,
	SketchIneq : [sketchInequality]
	):->
	"Open the editor for editing the given (or the first) sketchInequality" ::
	D->>fillSketchIneqList,
	D->>fillSketchQuantitiesBrowser,
	pl_setSketchIneqSelection(SketchIneq,D,D?sketchIneqList),
	D->>open.
%
pl_setSketchIneqSelection(@default,D,List):-
	%alleen bij het openen: zet de zaken even goed.
	0 =List<<-length,!,
	List->>selection(@nil), %toevoegen
	D->>slot(sketchInequality,@nil),
	D->>fillCurrentSketchIneqData.
%
pl_setSketchIneqSelection(@default,D, List):-!,
	Q = List?members<<-head,
	List->>selection(Q),
	D->>slot(sketchInequality,Q?object),
	D->>fillCurrentSketchIneqData.
%
pl_setSketchIneqSelection(SketchIneq,D,List):-!,
	Item = List<<-member(SketchIneq),
	List->>selection(Item),
	D->>slot(sketchInequality,SketchIneq),
	D->>fillCurrentSketchIneqData.
%%


%%
fillSketchIneqList(D):->
	"Fill the list with temporary sketchInequalities" ::

        get(D, sortedSketchInequalities, Ineqs), 
        Ineqs \= @nil, 
	D?sketchIneqList->>clear,
	Ineqs->>for_all(->>(D,fillSketchIneqList_helper,@arg1)).
%



%
fillSketchIneqList(D):->
	"There is no state yet, so no inequalities either" ::

	D?sketchIneqList->>clear.
%%

%
fillSketchIneqList_helper(D,Def):->
	%voeg deze toe aan onze deflist
	L *= string('%s',Def?displayNameString),
	DI *= dict_item(L, L, Def, normaal),
	D?sketchIneqList->>append(DI).
%%


%%
updateAllSketchIneqLists(D, Arg: sketchElement):->
	"Update the display names of the sketchInequalities which include Arg as argument" ::

        get(D, sketchIneqList, Browser), 
        % get the selection to restore later
        get(Browser, selection, Sel), 

        % for each sketchStateElement, update its SketchIneqList
        ?(D?sketch,findElements,sketchStateElement)->>for_all(
 	        ->>(@arg1, updateSketchIneqList, Arg)
	 					      ),
	Browser->>clear,
        D->>fillSketchIneqList,
        if Sel \= @nil
        then 
        (
            % restore selection if possible
            if (catch(send(Browser?dict, member, Sel),_,fail))
            then 
            (
               send(Browser, select, Sel)
            )
            else
            (
               debug(sketch(stg), 'Could not restore selection to ~w in Ineq browser\n',[Sel])
            )        
        ).

%%

%
fillSketchQuantitiesBrowser(D, Browser):->
	"Fill the list with sketchQuantities" ::
        % get the selection to restore later
        get(Browser, selection, Sel), 
        if Sel \= @nil
        then 
           get(Sel?object, name, SelName),

	Browser->>clear,
        % get(@model, hypered, causalModelMF, CM), 
        get(D, sortedSketchQuantities, SketchQuantities), 
	SketchQuantities->>for_all(->>(D,fillSketchQuantitiesBrowser_helper,Browser, @arg1)),

        if Sel \= @nil
        then 
        (
            % restore selection if possible
            if get(Browser, member, SelName, _SelItem)
            then 
            (
               send(Browser, select, SelName)
            )
            else
            (
               debug(sketch(stg), 'Could not restore selection to ~w in Quantity browser',[SelName])
            )        
        ).
%%

%
fillSketchQuantitiesBrowser_helper(_D, Browser, Def):->
	%voeg deze toe aan onze deflist
	L *= string('%s',Def?name),
	DI *= dict_item(L, L, Def, normaal),
	Browser->>append(DI).
%%


%%
fillSketchValuesBrowser(D, Browser):->
	"Fill the list with sketchValues" ::
	Browser->>clear,
        get(D, sortedSketchValues, SketchValues), 
	SketchValues->>for_all(->>(D,fillSketchValuesBrowser_helper, Browser, @arg1)).
%

fillSketchValuesBrowser_helper(_D, Browser, SketchValueElement):->
% fillSketchValuesBrowser_helper(_D, Browser, ValueStr):->
	%voeg deze toe aan onze list
	get(SketchValueElement, name, ValueStr),
	L *= string('%s', ValueStr),
	% DI *= dict_item(ValueStr, L, ValueStr),
	DI *= dict_item(ValueStr, L, SketchValueElement),
	Browser->>append(DI).
%%



%
addSketchQuantityToList(D, SketchQuantity):->
	"Add a sketch quantity to the list with sketchQuantities and update browser" ::
        send(D?sketchQuantities, append, SketchQuantity), 
        send(D, fillSketchQuantitiesBrowser, D?sketchQuantitiesBrowser),
	send(D, fillSketchQuantitiesBrowser, D?sketchQuantitiesBrowser2).
%%

%
removeSketchQuantityFromList(D, SketchQuantity):->
	"Remove a sketch quantity from the list with sketchQuantities and update browser" ::
        if 
        (
	    D?sketchQuantities->>member(SketchQuantity)
	)
        then
        (
            % remove it and refill
            send(D?sketchQuantities, delete, SketchQuantity)
        )
        else
        (
	    % quantity was not saved yet, so refill will remove it
	    debug(sketch(stg), 'remove sketchQuantity ~w from Browser\n',[SketchQuantity])
        ),
	send(D, fillSketchQuantitiesBrowser, D?sketchQuantitiesBrowser),
	send(D, fillSketchQuantitiesBrowser, D?sketchQuantitiesBrowser2),
	debug(sketch(stg), 'removed sketchQuantity ~w from D\n',[SketchQuantity]).
%%

%
removeSketchValueFromList(D, SketchValue, Browser):->
	"Remove a sketch value from the list with sketchValues and update browser" ::
        if 
        (
	    D?sketchValues->>member(SketchValue)
	)
        then
        (
            % remove it and refill
            send(D?sketchValues, delete, SketchValue),
	    send(D, fillSketchValuesBrowser, Browser),
	    debug(sketch(stg), 'removed sketchValue ~w from D\n',[SketchValue])
        )
        else
        (
	    % value was not saved yet, so refill will remove it
	    send(D, fillSketchValuesBrowser, Browser),
	    debug(sketch(stg), 'removed sketchValue ~w from Browser\n',[SketchValue])
        ).
%%

%
addSketchValueToList(D, SketchValue, Browser):->
	"Add a sketch value to the list with sketchValues and update browser" ::
        send(D?sketchValues, append, SketchValue), 
	send(D, fillSketchValuesBrowser, Browser).
%%



%
onSketchIneqSelection(D,
	SketchIneq : sketchInequality*):->
	"Callback when sketchInequality selected" ::
	/*
	Het probleem is altijd dat hier nu al de nieuwe staat geselecteerd terwijl de mapping
	in het dataobject (<-sketchInequality) nog op de oude staat ivm opslaan..
	*/
	%clause 1: op dezelfde regel geklikt
	SketchIneq = D<<-sketchInequality,!,
        % The data may have changed in the mean time
	D->>slot(sketchInequality,SketchIneq),
	D->>fillCurrentSketchIneqData.
%%

%
onSketchIneqSelection(D, _SketchIneq):->
	%clause 2: we mogen niet door ivm de vorige selectie

	% \+ D->>checkSaveSketchIneqSelection,!, %faalt dus terug op oude selectie
	\+ D->>checkSaveAllParts,!, %faalt dus terug op oude selectie

	if 
		@nil = D<<-sketchInequality %we waren in new modus
	then
		D?sketchIneqList->>selection(@nil)
	else
		D?sketchIneqList->>selection(D?sketchInequality?displayNameString).
%
onSketchIneqSelection(D, SketchIneq):->
	%clause 3: we mogen door met de nieuwe selectie dus initialiseren we de data
	D->>slot(sketchInequality,SketchIneq),
	D->>fillCurrentSketchIneqData,
	D?removeSketchIneq->>active(@on),
	D?copySketchIneq->>active(@on).
%%


%%
addSketchIneq(D):->
	"Add button pressed.." ::
	D->>checkSaveAllParts,
	% D->>checkSaveSketchIneqSelection,
	D->>slot(sketchInequality,@nil), %betekent: nieuwe
	D?sketchQuantitiesBrowser->>selection(@nil),
	D?sketchQuantitiesBrowser2->>selection(@nil),
	D?sketchValuesBrowser->>selection(@nil),
	D?sketchIneqList->>selection(@nil),
        % set the RHS to values, as default mode
	D->>selectMode(values),
	defaultLHSName(DefLHSName), 
	defaultRHSName(DefRHSName), 
        % set the inequality sign to =, as default
	D?type_member->>selection(eq),
        D?quantityName->>value(DefLHSName),
	D?rhsName->>value(DefRHSName), 
        D->>previousQuantityNameL(''),
        D->>previousQuantityL(@nil),
        D->>previousQuantityNameR(''),
        D->>previousQuantityR(@nil),
        D->>previousValueName(''),
        D->>previousValue(@nil),
	D->>caret(D?quantityName_member).
%%



%%
copySketchIneq(D):->
	"Copy button pressed.." ::
	%lijkt op addSketchIneq: we maken gewoon een nieuwe met dezelfde quantities als de geselecteerde
	%maar wijzigen de naam en zet naam/type-kopien van de quantities (andere id) in de sketchQuantitiesBrowser
	D->>checkSaveAllParts,
	% D->>checkSaveSketchIneqSelection,
	CurrentSketchIneq = D<<-sketchInequality,
	CurrentSketchIneq \== @nil,
	% D->>fillCurrentSketchIneqData, %vult dus voor de huidige selectie (en zet save uit)
	% NewName *= string('%s (copy)',CurrentSketchIneq?value),
	NewName *= string('%s (copy)',CurrentSketchIneq?displayNameString),

        get(D, sortedSketchInequalities, Ineqs), 
        get(CurrentSketchIneq, type, Type), 
        get(CurrentSketchIneq, argument1, Arg1), 
        get(CurrentSketchIneq, argument2, Arg2), 
        get(CurrentSketchIneq, argument2Type, Arg2Type), 

        new(NewIneq, sketchInequalityElement(Arg1, Type, Arg2, Arg2Type, '')),
        send(NewIneq, displayNameString, NewName), 
        send(Ineqs, append, NewIneq),     
        % send(Ineqs, append, NewName),     
	D->>fillSketchIneqList, % opnieuw inlezen

	% D->>slot(sketchInequality,@nil), %maar de bewerkte is een nieuwe
        % set new copy as the current SketchInequality
	D->>slot(sketchInequality,NewIneq), 

        D->>previousQuantityNameL(''),
        D->>previousQuantityL(@nil),
        D->>previousQuantityNameR(''),
        D->>previousQuantityR(@nil),
        D->>previousValueName(''),
        D->>previousValue(@nil),

	D?sketchIneqList->>selection(@nil),
	D?sketchIneqList->>selection(NewIneq?displayNameString),
	D?removeSketchIneq->>active(@off),
	D?copySketchIneq->>active(@off),
	D?save_member->>active(@on).
%%
	
%%
removeSketchIneq(D):->
	"Remove button pressed.." ::

	%CR checkt de gevolgen wel en waarschuwt eventueel
	if
		not(@nil = D<<-sketchInequality)
	then
                % this line should be taken care of somewhere else.
                % the changeRequestor now does nothing, until the applyChange stage.
                D?sketchInequalities->>delete(D?sketchInequality),
		@model->>changeRequest(deleteSketchInequality,
			@model,
			D,
			D?sketchInequality),
                D->>fillSketchIneqList,  % opnieuw inlezen
	        D->>slot(sketchInequality,@nil),
	        send(D, resetSelections).
%%



%
addSketchQuantityLeft(D):->
	"Add a sketch quantity to the list with sketchQuantities and update browser" ::
        send(D, addSketchQuantity, left).
%%

%%
addSketchQuantity(D, LR):->
	"Add button pressed.." ::
        % put 'NewQuantity' in Quantity field

	D->>checkSaveSketchQuantity(LR),
	% D->>checkSaveSketchIneqSelection,

        get(D, sketchQuantitiesBrowserLR, LR, SQB), 
        % reset the selection
	SQB->>selection(@nil),

        defaultLHSName(DefLHSName), 
        if LR = left
        then 
	(
           D?quantityName->>value(DefLHSName),
           D->>previousQuantityNameL(''),
           D->>previousQuantityL(@nil),
	   D->>caret(D?quantityName_member)
        )
        else
	(
           D?rhsName->>value(DefLHSName),
           D->>previousQuantityNameR(''),
           D->>previousQuantityR(@nil),
	   D->>caret(D?rhsName_member)
        ).
%%



%
deleteSketchQuantityLeft(D):->
	"Delete button pressed.." ::
	send(D, deleteSketchQuantity, left).
%%


%
deleteSketchQuantity(D, LR):->
	"Delete button pressed.." ::
        get(D, sketchQuantitiesBrowserLR, LR, SQB), 
	% there is no selection, so do nothing
        get(SQB, selection, SelectedQuantity), 
	SelectedQuantity = @nil,!,
        D->>present_message(warning, 
	       'No quantity selected to delete\n').
%%


%
deleteSketchQuantity(D, LR):->
        get(D, sketchQuantitiesBrowserLR, LR, SQB), 
        if LR = left
        then 
        (
          get(D, quantityName, SelectedQuantityName)
        )
        else
        (
          get(D, rhsName, SelectedQuantityName)
        ),

        % Selection \= @nil
        get(SQB?selection, object, SelectedQuantity), 
        if 
        (
	 % if none of the inequalities in none of the states involve the selected quantity
	 get(D?sketch,findElements,sketchStateElement, SketchStates), 
         SketchStates->>for_all(->>(@arg1?sortedSketchStateInequalities, for_all, (->>(@arg1, does_not_involve_quantity, SelectedQuantity))))
	)
        then 
        (
	 if D?sketchQuantities->>delete_all(SelectedQuantity)
	 then 
	 (
           get(@model, hypered, stgraphMF, STGraphMF), 

/*
           get(STGraphMF, sketchQuantities, STGraphQuantities), 
           chain_list(STGraphQuantities, STGraphQuantitiesList), 
           % findall(STGraphQuantity, 
                    (member(STGraphQuantity, STGraphQuantitiesList), 
                     STGraphQuantity == SelectedQuantity 
                    QuantitiesToDelete), 
*/

           % delete all occurrences (should be only one) of SelectedQuantity from the chain
           STGraphMF?sketchQuantities->>delete_all(SelectedQuantity),
           % delete the sketchQuantityElement itself
           send(SelectedQuantity, free), 

           % update sketchQuantitiesBrowser        
	   D->>fillSketchQuantitiesBrowser(D?sketchQuantitiesBrowser),
	   D->>fillSketchQuantitiesBrowser(D?sketchQuantitiesBrowser2),
           % reset the selection
	   SQB->>selection(@nil),
	   SelectedQuantityName->>clear, 
           if LR = left
	   then 
           (
              D->>previousQuantityNameL(''),
	      D->>previousQuantityL(@nil),
	      D->>caret(D?quantityName_member)
           )
           else
           (
              D->>previousQuantityNameR(''),
	      D->>previousQuantityR(@nil),
	      D->>caret(D?rhsName_member)
           )
	 )
	 else
	 ( 
	  debug(sketch(stg), 'sketchQuantity could not be found\n',[])
	  )
        )
        else
        (
         % give warning
         get(SelectedQuantityName, value, SelectedQuantityNameStr), 
	 swritef(Str, 'Quantity %d is used in at least one (in)equality statement and could therefore not be deleted\n',
		[SelectedQuantityNameStr]),
         D->>present_message(warning, Str)
	).
%%


%
present_message(D, Type: string, Msg: string):->
        get(Type, value, TypeStr), 
        get(Msg, value, MsgStr), 
        debug(sketch(stg), '~w: ~w\n',[TypeStr, MsgStr]),
	Dlg *= dialog(Type),
	Dlg->>application(@app),
	Dlg->>append(text(Msg)),
	Ok *= imgButton(ok,
			    ->>(Dlg,return,ok), img:=close, tt:='Ok'),
	Dlg->>append(Ok),
	Dlg->>transient_for(D),
	Dlg->>modal(transient),
	Dlg->>kind(toplevel), %wel een titelbalk
	Answer = Dlg<<-confirm_centered(D?frame?area?center),
	Dlg->>destroy,
	if
		Answer = ok
	then
		D->>saveSketchValue
	else 
               (
		debug(sketch(stg), 'This should not happen', [])
	       ).
%%


%%
addSketchValueOrQuantity(D):->
	"Add button pressed.." ::
	if D?selectMode->>equal(values)
        then 
           D->>addSketchValue
        else
           D->>addSketchQuantity(right).
%%

%%
addSketchValue(D):->
	"Add button RHS pressed in values mode.." ::
        % put 'NewValue' in Value field

	D->>checkSaveSketchValue,
	% D->>checkSaveSketchIneqSelection,
	D->>selectMode(values),

        % reset the selection
	D?sketchValuesBrowser->>selection(@nil),
	% D?rhsName->>clear, 
	defaultRHSName(DefRHSName), 
	% D?type_member->>selection(eq),
        D?rhsName->>value(DefRHSName),
        D->>previousValueName(''),
        D->>previousValue(@nil),
	D->>caret(D?rhsName_member).        
%%


%
deleteSketchValueOrQuantity(D):->
	"Delete button pressed.." ::
        if D?selectMode->>equal(values)
        then 
           D->>deleteSketchValue
        else
           D->>deleteSketchQuantity(right).

%%
deleteSketchValue(D):->
        % nothing selected, so do nothing
        D?sketchValuesBrowser?selection->>equal(@nil),!,
        D->>present_message(warning, 
	       'No value selected to delete\n').
%
deleteSketchValue(D):->
        get(D, sketchValuesBrowser, SVB), 
        get(D, rhsName, SelectedValueName), 
        get(SVB?selection, object, SelectedValue), 
        if 
        (
	 % if none of the inequalities in none of the states involve the selected value
	 get(D?sketch,findElements,sketchStateElement, SketchStates), 
         SketchStates->>for_all(->>(@arg1?sortedSketchStateInequalities, for_all, (->>(@arg1, does_not_involve_value, SelectedValue))))
	)
        then 
        (
	 if D?sketchValues->>delete_all(SelectedValue)
	 then 
	 (
           get(@model, hypered, stgraphMF, STGraphMF), 

           % delete all occurrences (should be only one) of SelectedQuantity from the chain
           STGraphMF?sketchValues->>delete_all(SelectedValue),
           % delete the sketchValueElement itself
           send(SelectedValue, free), 

           % update sketchValuesBrowser        
	   D->>fillSketchValuesBrowser(SVB),
           % reset the selection
	   SVB->>selection(@nil),
           SelectedValueName->>clear, 
           D->>previousValueName(''),
	   D->>previousValue(@nil),
	   D->>caret(D?rhsName_member)
	 )
	 else
	 ( 
	  debug(sketch(stg), 'sketchValue could not be found\n',[])
	  )
        )
        else
        (
         % give warning
         get(SelectedValueName, value, SelectedValueNameStr), 
	 debug(sketch(stg), 'Value ~w is used in at least one (in)equality statement and could therefore not be deleted\n',
		[SelectedValueNameStr]),
	 swritef(Str, 'Value %d is used in at least one (in)equality statement and could therefore not be deleted\n',
		[SelectedValueNameStr]),
         D->>present_message(warning, Str)
	).
%%






%%
fillCurrentSketchIneqData(D):->
	"Fill the dialog with the saved data for the current sketchIneq" ::
	%clause 1: geen sketchIneq geselecteerd: een nieuwe dus

	@nil = D<<-sketchInequality,!,
	%de remove en copy knop gaat dan uit, save mag aan
	D?removeSketchIneq->>active(@off),
	D?copySketchIneq->>active(@off),
	D?save_member->>active(@on),
	% default_sketchStateName(Name), %zie bovenin deze file
	% D?sketchIneqList->>selection(Name),
	send(D, resetSelections).

%
fillCurrentSketchIneqData(D):->
	%clause 2: wel een sketchIneq geselecteerd        
	SketchIneq = D<<-sketchInequality,
	D?copySketchIneq->>active(@on),
	% SketchIneq = D<<-sketchInequality,
	D?sketchIneqList->>selection(SketchIneq?displayNameString),
        % set Quantities Browser to LHS quantity in the selected Inequality
        get(SketchIneq, argument1, LHSQuantity), 
	SketchQuantitiesBrowser = D<<-sketchQuantitiesBrowser,
	SketchQuantitiesBrowser2 = D<<-sketchQuantitiesBrowser2,
	SketchValuesBrowser = D<<-sketchValuesBrowser,

        if 
           get(SketchQuantitiesBrowser, member, LHSQuantity?name, _LHSItem)
        then 
        ( 
           send(SketchQuantitiesBrowser, select, LHSQuantity?name),         
           D?quantityName->>value(D?sketchQuantitiesBrowser?selection?name),
           D->>previousQuantityNameL(D?sketchQuantitiesBrowser?selection?name),
           D->>previousQuantityL(D?sketchQuantitiesBrowser?selection?object)
        ), 
        % set Sign menu and Sign text item to the right sign
        get(SketchIneq, type, Sign),
        send(D?type_member, selection, Sign),
        % symbol_typeName(Sign, TypeStr),  
        % send(D?sign, value, TypeStr), 
        % send(D?sign_member, selection, Sign),
        
        % set Values Browser to RHS value in the selected Inequality, if applicable
        get(SketchIneq, argument2, RHS),
        get(SketchIneq, argument2Type, RHSType),
        if (RHSType == sketchValueElement) 
        then
          (
           % RHS is a sketchValueElement; set Values Browser selection to RHS, reset QuantitiesBrowser2
           D->>selectMode(values), 
           send(SketchValuesBrowser, select, RHS?name),                    
           send(SketchQuantitiesBrowser2, selection, @nil),
           D?rhsName->>value(D?sketchValuesBrowser?selection?name),
           D->>previousValueName(D?sketchValuesBrowser?selection?name),
           D->>previousValue(D?sketchValuesBrowser?selection?object),
           D->>previousQuantityNameR(''),
           D->>previousQuantityR(@nil)
          )
        else
          (
           % RHS is a sketchQuantity; set QuantitiesBrowser2 selection to RHS, reset ValuesBrowser
           D->>selectMode(quantities), 
           send(D?rhsName, value, RHS?name),
           send(SketchValuesBrowser, selection, @nil),
           send(SketchQuantitiesBrowser2, select, RHS?name),
           D->>previousValueName(''),
           D->>previousValue(@nil),
           D->>previousQuantityNameR(D?sketchQuantitiesBrowser2?selection?name),
           D->>previousQuantityR(D?sketchQuantitiesBrowser2?selection?object)
          ).
%%


%%
onQuantitySelect(D):->
	"Quantity selected on the left-hand side" ::

	D->>checkSaveSketchQuantity(left),       
	D->>selectQuantityName,
        % fill in the selected quantity in the quantityName field
        % Set quantity as current and previous quantity
        D?quantityName->>value(D?sketchQuantitiesBrowser?selection?name),
        D->>previousQuantityL(D?sketchQuantitiesBrowser?selection?object),
        D->>previousQuantityNameL(D?sketchQuantitiesBrowser?selection?name).
%%


%
onQuantitySelect2(D):->
	"Quantity2 selected on the right-hand side" ::
	D->>checkSaveSketchQuantity(right),     
	D->>selectMode(quantities),        
	D->>selectQuantityName2,

        % reset everything about Values
        send(D?sketchValuesBrowser, selection, @nil),
        D->>previousValueName(''),
        D->>previousValue(@nil),

        % fill in the selected quantity in the quantityName field (= default)
	% Set quantity as current and previous quantity
        D?rhsName->>value(D?sketchQuantitiesBrowser2?selection?name),
        D->>previousQuantityR(D?sketchQuantitiesBrowser2?selection?object),
        D->>previousQuantityNameR(D?sketchQuantitiesBrowser2?selection?name).
%%


%%
onValueSelect(D):->
	"Value selected" ::

	D->>checkSaveSketchValue,       
	D->>selectMode(values),        
	D->>selectValueName,
    
        % reset everything about the right-hand side Quantities list browser
        send(D?sketchQuantitiesBrowser2, selection, @nil),
        D->>previousQuantityNameR(''),
        D->>previousQuantityR(@nil),

	% Set quantity as current and previous quantity
        D?rhsName->>value(D?sketchValuesBrowser?selection?name),
        D->>previousValue(D?sketchValuesBrowser?selection?object),
        D->>previousValueName(D?sketchValuesBrowser?selection?name).
%%

	

%%
onQuantityNameLR(D, LR):->
	"Quantity name changed" ::
        % get(D?quantityName), selection, QuantityName), 
        if LR = left
        then 
        (
          get(D?quantityName, value, QuantityName),
          get(D, sketchQuantitiesBrowser, SQB)
        )
        else 
        (
          get(D?rhsName, value, QuantityName),
          get(D, sketchQuantitiesBrowser2, SQB)
        ),
    	QuantityNameCap = QuantityName<<-makeGarp,
    	get(QuantityNameCap, value, QuantityNameAfter),

        % writef('name before: %d and after: %d\n',[QuantityName, QuantityNameAfter]), 

        % check if the name is valid
	if
	(
	        @model->>validName(QuantityNameAfter)
	)
	then
        (
	        % writef('The quantity name %d seems to be valid\n',[QuantityNameAfter]),

	        % check if the name is unique    
	        if
	        (
		   get(D, sketchQuantities, SQs), 
		   get(SQs, find(@arg1?name == QuantityNameAfter), Q2)
	        )
	        then
                (
		   % writef('The given quantity name %d is already used\n',[QuantityNameAfter]),
		   if 
		   (
		     % the quantity was selected itself
		     get(SQB, selection, Q1Item), 
		     Q1Item \== @nil,
		     get(Q1Item, object, Q1), 
		     % writef('Q1:  %d\n',[Q1]),
		     % writef('Q2:  %d\n',[Q2]),
		     Q2 == Q1		     
		   )
		   then
		   (
		     debug(sketch(stg), 'The quantity was selected itself, so do not remove it\n',[]) 
		   )		  
		   else
		   (
                    debug(sketch(stg), 'Existing quantity with same name: ~w\n',[Q2]),
	            send(D, onQuantityNameContinueDoubleName, LR, QuantityName)		    
		   )

                )
                else
                (
                   debug(sketch(stg), 'The quantity name ~w seems to be unique\n',[QuantityNameAfter]),
                   send(D, onQuantityNameContinueValidName, LR, QuantityName)
	        )
        )
        else
        (
		debug(sketch(stg), 'The given quantity name ~w is invalid\n',[QuantityNameAfter]),
	        send(D, onQuantityNameContinueInvalidName, LR, QuantityName)
	).
%%
		

notAcceptableSketchQuantity(D, LR):->
	"succeeds if Quantity name is unacceptable" ::
        debug(sketch(stg), 'check if Q name on ~w-hand side is acceptable\n',[LR]), 
        get(D, sketchQuantitiesBrowserLR, LR, SQB),
        if LR = left
        then 
        (
           get(D?quantityName, value, QuantityName),
           get(D, previousQuantityL, PrevQ)
        )
        else
        (
           get(D?rhsName, value, QuantityName),
           get(D, previousQuantityR, PrevQ)
        ),   
    	QuantityNameCap = QuantityName<<-makeGarp,
    	get(QuantityNameCap, value, QuantityNameAfter),

        debug(sketch(stg), 'name before: ~w and after: ~w\n',[QuantityName, QuantityNameAfter]), 

        % check if the name is valid
	if
	(
	        @model->>validName(QuantityNameAfter)
	)
	then
        (
	        debug(sketch(stg), 'The quantity name ~w seems to be valid\n',[QuantityNameAfter]),

	        % check if the name is unique    
	        if
	        (
		   get(D, sketchQuantities, SQs), 
		   get(SQs, find(@arg1?name == QuantityName), Q2)
	        )
	        then
                (
		   debug(sketch(stg), 'The given quantity name ~w is already used\n',[QuantityName]),
		   if 
		   (
		     % the quantity was selected itself

		     get(SQB, selection, QItem), % current selection - this may have changed already!
		     QItem \== @nil,
		     get(QItem, object, Q), 
                     get(Q, name, QName), 
		     debug(sketch(stg), 'Selected quantity Q name: ~w\n',[QName]),

                     get(PrevQ, name, PrevQName), 
		     debug(sketch(stg), 'Previous quantity name: ~w\n',[PrevQName]),
                     get(Q2, name, Q2Name), 
		     debug(sketch(stg), 'Found quantity Q2 name: ~w\n',[Q2Name]),
                    
		     PrevQ \== @nil,
		     Q2 == PrevQ		     
		   )
		   then
		   (
		     debug(sketch(stg), 'The quantity was selected itself, so it is ok\n',[]),
		     fail
		   )		  
		   else
		   (
                    debug(sketch(stg), 'The quantity name existed already: ~w\n',[Q2]),
		    true
		   )

                )
                else
                (
                   debug(sketch(stg), 'The quantity name ~w seems to be unique\n',[QuantityName]),
                   fail
	        )
        )
        else
        (
		debug(sketch(stg), 'The given quantity name ~w is invalid\n',[QuantityNameAfter]),
	        true
	).
%%



notAcceptableSketchValue(D):->
	"succeeds if Value name is unacceptable" ::
        % writef('check if Value name  is acceptable\n',[]), 
        get(D, sketchValuesBrowser, SVB),
        get(D?rhsName, value, ValueName),
    	ValueNameCap = ValueName<<-makeGarp,
    	get(ValueNameCap, value, ValueNameAfter),

        % writef('name before: %d and after: %d\n',[ValueName, ValueNameAfter]), 

        % check if the name is valid
	if
	(
	        @model->>validName(ValueNameAfter)
	)
	then
        (
	        % writef('The value name %d seems to be valid\n',[ValueNameAfter]),

	        % check if the name is unique    
	        if
	        (
		   get(D, sketchValues, SVs), 
		   get(SVs, find(@arg1?name == ValueName), V2)
	        )
	        then
                (
		   % writef('The given value name %d is already used\n',[ValueName]),
		   if 
		   (
		     % the value was selected itself

		     get(SVB, selection, V0Item), % current selection - this may have changed already!
		     V0Item \== @nil,
		     get(V0Item, object, V0), 
                     get(V0, name, V0Name), 
		     debug(sketch(stg), 'Selected value V0 name: ~w\n',[V0Name]),

                     get(D, previousValue, V1), 
                     get(V1, name, V1Name), 
		     debug(sketch(stg), 'Previous value V1 name: ~w\n',[V1Name]),
                     get(V2, name, V2Name), 
		     debug(sketch(stg), 'Found value V2 name: ~w\n',[V2Name]),
                    
		     V1 \== @nil,
		     V2 == V1		     
		   )
		   then
		   (
		     % writef('The value was selected itself, so it is ok\n',[]),
		     fail
		   )		  
		   else
		   (
                    % writef('The value name existed already: %d\n',[V2]),
		    true
		   )

                )
                else
                (
                   % writef('The value name %d seems to be unique\n',[ValueName]),
                   fail
	        )
        )
        else
        (
		% writef('The given value name %d is invalid\n',[ValueNameAfter]),
	        true
	).
%%


						
%
onQuantityNameContinueValidName(D, LR, QuantityName):->
    	 QuantityNameCap = QuantityName<<-makeGarp,
         get(D, sketchQuantitiesBrowser, SQB1), 
         get(D, sketchQuantitiesBrowser2, SQB2), 
         if LR = left
         then
         (
            SQB = SQB1, 
            get(D, quantityName, QNameField)
         )
         else
         (
            SQB = SQB2, 
            get(D, rhsName, QNameField)
         ),        

	 get(SQB, selection, SelectedQuantity), 
	 % if there was a quantity selected, change it
	 if (@nil \= SelectedQuantity)
	 then
	 (
          send(SelectedQuantity, name, QuantityNameCap),
	  D->>fillSketchQuantitiesBrowser(SQB1),
	  D->>fillSketchQuantitiesBrowser(SQB2),
	  SQB->>select(QuantityNameCap),
	  send(QNameField, selection, QuantityNameCap)       
	 )
	 else
	 (
          % create new sketchQuantityElement SQ, with empty remarks
          get(@model, hypered, stgraphMF, STGraphMF), 
          new(SQ, sketchQuantityElement(QuantityName, '', STGraphMF)), 
          % append to elements only when saving the whole state
	  % SK?elements->>append(SQ),
          % add SQ to quantity list 
          send(D, addSketchQuantityToList, SQ),
	  SQB->>select(SQ?name),
	  send(QNameField, selection, SQ?name)       
	).
%%


onQuantityNameContinueInvalidName(D, LR, QuantityName):->
         get(D, sketchQuantitiesBrowserLR, LR, SQB),
	 get(SQB, selection, SelectedQuantity), 
	 % if there was a quantity selected, remove it from the list and clear selection
	 if (@nil \= SelectedQuantity)
	 then
	 (
          send(D, removeSketchQuantityFromList, SelectedQuantity?object),
	  debug(sketch(stg), 'Removed selected invalid quantity name ~w from list \n',[QuantityName]),

	  SQB->>selection(@nil)
	 )
	 else
	 (
          % do nothing
	  true
	 ).
%%
	
onQuantityNameContinueDoubleName(D, LR, QuantityName):->
         get(D, sketchQuantitiesBrowserLR, LR, SQB),
	 get(SQB, selection, SelectedQuantity), 
	 % if there was a quantity selected, remove it from the list
	 if (@nil \= SelectedQuantity)
	 then
	 (
          send(D, removeSketchQuantityFromList, SelectedQuantity?object),
	  debug(sketch(stg), 'Removed double quantity ~w from list \n',[QuantityName]),
          % and select the one with same name, or not?
	  % SQB->>select(QuantityName)
	  SQB->>selection(@nil)
	 )
	 else
	 (
          % do nothing
	  true
	 ),

    	 QuantityNameCap = QuantityName<<-makeGarp,
         if LR = left
         then
         (
            get(D, quantityName, QNameField)
         )
         else
         (
            get(D, rhsName, QNameField)
         ),        
         % update text field with garpified name
         send(QNameField, selection, QuantityNameCap).
%%


%%
onRHSName(D):->
        if D?selectMode->>equal(quantities)
        then 
        ( 
           % writef('D?selectMode = quantities\n',[]),
           D->>onQuantityNameLR(right)
        )
        else
        (
           % writef('D?selectMode = values\n',[]),
           D->>onValueName
        ).

%%
onValueName(D):->
	"Value name changed" ::
	% D?sketchValuesBrowser->>changeCurrentValue(D?rhsName?selection).
        % get(D?rhsName, selection, ValueName), 
        get(D?rhsName, value, ValueName), 
    	ValueNameCap = ValueName<<-makeGarp,

    	get(ValueNameCap, value, ValueNameAfter),
        % writef('name before: %d and after: %d\n',[ValueName, ValueNameAfter]), 

        % check if the name is valid
	if
	(
	        @model->>validName(ValueNameAfter)
	)
	then
        (
	        debug(sketch(stg), 'The Value name ~w seems to be valid\n',[ValueNameAfter]),

	        % check if the name is unique    
	        if
	        (
		   get(D, sketchValues, SVs), 
		   get(SVs, find(@arg1?name == ValueNameAfter), V2)
	        )
	        then
                (
		   debug(sketch(stg), 'The given value name %d is already used\n',[ValueNameAfter]),
		   if 
		   (
		     % the value was selected itself
		     get(D?sketchValuesBrowser, selection, V1Item), 
		     V1Item \== @nil,
		     get(V1Item, object, V1), 
		     % writef('V1:  %d\n',[V1]),
		     % writef('V2:  %d\n',[V2]),
		     V2 == V1		     
		   )
		   then
		   (
		     debug(sketch(stg), 'The value was selected itself, so do not remove it\n',[]) 
		   )		  
		   else
		   (
                    debug(sketch(stg), 'Existing value with same name: ~w\n',[V2]),
	            send(D, onValueNameContinueDoubleName, ValueName)		    
		   )

                )
                else
                (
                   debug(sketch(stg), 'The value name ~w seems to be unique\n',[ValueNameAfter]),
                   send(D, onValueNameContinueValidName, ValueName)
	        )
        )
        else
        (
		debug(sketch(stg), 'The given value name ~w is invalid\n',[ValueNameAfter]),
	        send(D, onValueNameContinueInvalidName, ValueName)
	).
%%


						
%
onValueNameContinueValidName(D, ValueName):->
    	 ValueNameCap = ValueName<<-makeGarp,

	 get(D?sketchValuesBrowser, selection, SelectedValue), 
	 % if there was a value selected, change it
	 if (@nil \= SelectedValue)
	 then
	 (
          send(SelectedValue, name, ValueNameCap),
	  D->>fillSketchValuesBrowser(D?sketchValuesBrowser),
	  D?sketchValuesBrowser->>select(ValueNameCap),
	  send(D?rhsName, selection, ValueNameCap)       
	 )
	 else
	 (
          % create new sketchValueElement SV, with empty remarks
          new(SV, sketchValueElement(ValueName, '', D?sketch)), 
          % append to elements only when saving the whole state
	  % SK?elements->>append(SV),
          send(D, addSketchValueToList, SV, D?sketchValuesBrowser),
	  D?sketchValuesBrowser->>select(SV?name),
	  send(D?rhsName, selection, SV?name)       
	).
%%


onValueNameContinueInvalidName(D, ValueName):->
	 get(D?sketchValuesBrowser, selection, SelectedValue), 
	 % if there was a value selected, remove it from the list and clear selection
	 if (@nil \= SelectedValue)
	 then
	 (
          send(D, removeSketchValueFromList, SelectedValue?object, D?sketchValuesBrowser),
	  debug(sketch(stg), 'Removed selected invalid value name ~w from list \n',[ValueName]),

	  D?sketchValuesBrowser->>selection(@nil)
	 )
	 else
	 (
          % do nothing
	  true
	 ).
%%
	
onValueNameContinueDoubleName(D, ValueName):->
	 get(D?sketchValuesBrowser, selection, SelectedValue), 
	 % if there was a value selected, remove it from the list
	 if (@nil \= SelectedValue)
	 then
	 (
          send(D, removeSketchValueFromList, SelectedValue?object, D?sketchValuesBrowser),
          % and select the one with same name, or not?
	  % D?sketchValuesBrowser->>select(ValueName)
	  D?sketchValuesBrowser->>selection(@nil)
	 )
	 else
	 (
          % do nothing
	  true
	 ),
         % update text field with garpified name
         get(D, rhsName, VNameField),
    	 ValueNameCap = ValueName<<-makeGarp,
         send(VNameField, selection, ValueNameCap).
%%




%%
onTypeSelection(_D):->
	"Sign selected" ::
        true.
        % no further action necessary 
        % get(D, type, Type), 
        % symbol_typeName(Type, TypeStr).  
        % send(D?sign, value, TypeStr), 
        % send(D?sign_member, selection, Sign),
	% D?sketchValuesBrowser->>changeCurrent(D?sign?selection).
%


%%
onSign(_D):->
	"Sign changed" ::
        true.
        % no further action necessary 
%





%%%%%%%%%%%%%SAVE EN CANCEL E.D%%%%%%%%%%%%%%%%%%%%%%



%%
notChangedIneqSign(D):->
	% no sketchInequality was selected, so do not consider the sign to be changed.    
        D?sketchInequality->>equal(@nil).

notChangedIneqSign(D):->
        D?sketchInequality?type->>equal(D?type).
%%


%%
notChangedIneqRHS(D):->
	% no sketchInequality was selected, so not changed.    
        D?sketchInequality->>equal(@nil).
%%
	
%%
notChangedIneqRHS(D):->
	get(D, sketchInequality, SketchIneq),          
        get(SketchIneq, argument2, RHS),
        get(SketchIneq, argument2Type, RHSType),
        if (RHSType == sketchValueElement) 
        then
          (
           ( 
             % RHS is a sketchValueElement - is the rhsName still equal?
             D?rhsName?value->>equal(RHS?name),
             debug(sketch(stg), 'RHS name is still equal.\n',[])
           ; % OR
             % or is rhsName empty, in Quantities mode, but still the same value selected in the value browser?
             % in that case, we also consider it not changed
             D?rhsName?value->>equal(''),
             D?selectMode->>equal(quantities), 
             debug(sketch(stg), 'check if RHS name is still equal.\n',[]),
             get(D?sketchValuesBrowser, selection, SelRHS), 
             ( 
               % no value selected
               SelRHS = @nil
             ; 
               % or still the same value selected
               catch(D?sketchValuesBrowser?selection?object->>equal(RHS), _, fail) % or RHS?name ?           
             ),
             debug(sketch(stg), 'RHS name is empty, but still same (or no) value selected in browser, so assume not changed.\n',[])
           )
          )
        else
          (
	   % RHS is a sketchQuantity and its name in the rhsName field has not been changed
           D?rhsName?value->>equal(RHS?name),
           D?rhsName?value->>equal(D?previousQuantityNameR)
          ).
%%




%%
notChangedSketchQuantity(D, LR):->
	% no sketchQuantity was selected, so not changed.    
        debug(sketch(stg), 'notChangedSketchQuantity on the %d-hand side? \n',[LR]), 
        get(D, sketchQuantitiesBrowserLR, LR, SQB),
        SQB?selection->>equal(@nil).
%%

%%
notChangedSketchQuantity(D, LR):->
	% no sketchQuantity was recorded, so not changed.    
        if LR = left
        then
        (
          D?previousQuantityNameL->>equal('')
        )
        else
        (
          D?previousQuantityNameR->>equal('')
        ).
%%
	
%%
notChangedSketchQuantity(D, LR):->
        % the name in the quantity field is identical to the previous name (when last selected in the browser)
        if LR = left
        then
        (
          D?previousQuantityNameL->>equal(D?quantityName?value)
        )
        else
        (
          D?previousQuantityNameR->>equal(D?rhsName?value)
        ).
%%




%%
notChangedSketchValue(D):->
	% no sketchValue was selected, so not changed.    
        D?sketchValuesBrowser?selection->>equal(@nil).
%%

%%
notChangedSketchValue(D):->
	% no sketchValue was recorded, so not changed.    
        D?previousValueName->>equal('').
%%
	
%%
notChangedSketchValue(D):->
        % the name in the value field is identical to the previous name (when last selected in the browser)
        D?previousValueName->>equal(D?rhsName?value).
%%

/*
% I think this clause is not necessary anymore, given that we now know the selectMode 
%
notChangedSketchValue(D):->
	% the RHS of the Ineq is a quantity, not a value - ignore any changes for now
	SketchIneq = D<<-sketchInequality,
	SketchIneq \== @nil,
        get(SketchIneq, argument2Type, RHSType),
        (RHSType == sketchQuantityElement).
%%
*/

%%
saveNewSketchValueElement(D):->
	@model->>changeRequest(newSketchValue,
		D?sketch,
		D?editor,
		% D?sketchValueElement,
		D?entity,
		D?sketchValueName,
		D?remarks).
%%

%%
saveChangedSketchValueElement(D):->
	@model->>changeRequest(changeSketchValue,
		D?sketch,
		D?editor,
		D?element,
		% D?sketchValue,
		D?entity,
		D?sketchValueName,
		D?remarks).
%%





%%
notChangedIneqLHS(D):->
	% no sketchInequality was selected, so not changed.    
        D?sketchInequality->>equal(@nil).

%%

%%
notChangedIneqLHS(D):->
	%bij bewerkt element iets gewijzigd?
	get(D, sketchInequality, SketchIneq),          
        get(SketchIneq, argument1, LHSQuantity), 
	SketchQuantitiesBrowser = D<<-sketchQuantitiesBrowser,
        get(D?quantityName, value, SelQName), 
        get(SketchQuantitiesBrowser, member, SelQName, SelQItem), 
        get(SelQItem, object, SelectedQuantity),         
        % get(SketchQuantitiesBrowser?selection, object, SelectedQuantity),         
        SelectedQuantity = LHSQuantity.	
	% D?sketchQuantityName->>equal(D?element?name),
	% D?sketchQuantity->>equal(D?element?sketchQuantityElement),
%%

%%
saveNewSketchQuantityElement(D):->
	@model->>changeRequest(newSketchQuantity,
		D?sketch,
		D?editor,
		% D?sketchQuantityElement,
		D?entity,
		D?sketchQuantityName,
		D?remarks).
%%

%%
saveChangedSketchQuantityElement(D):->
	@model->>changeRequest(changeSketchQuantity,
		D?sketch,
		D?editor,
		D?element,
		% D?sketchQuantity,
		D?entity,
		D?sketchQuantityName,
		D?remarks).
%%

%
checkSaveAllParts(D):->
	D->>checkSaveSketchValue,!,       
	D->>checkSaveSketchQuantity(left),!,       
	D->>checkSaveSketchQuantity(right),!,       
	D->>checkSaveSketchIneqSelection.
%%


%
checkSaveSketchValue(D):->
	"Check changed state of current Ineq selection and ask for saving, fail if change canceled or save failed" ::
	D->>notChangedSketchValue,!. %niets aan de hand

checkSaveSketchValue(D):->
	%ok, er is iets gewijzigd, dus moet er misschien opgeslagen worden

	Dlg *= dialog('Confirm changes'),
	Dlg->>application(@app),
	Dlg->>append(text('You made changes to a sketch Value, which may also be used \nelsewhere in this sketch. Do you want to save your changes?')),
	SC *= imgButton(save_changes,
			    ->>(Dlg,return,save), img:=sketch_save, tt:='Save changes to model'),
	Dlg->>append(SC),
	CC *= imgButton(cancel_changes,->>(Dlg,return,cancel), img:=sketch_undo, tt:='Cancel changes'),
	Dlg->>append(CC),
	EC *= imgButton(edit_changes,->>(Dlg,return,edit), img:=sketch_edit_changes, tt:='Edit changes'),
	Dlg->>append(EC),
	Dlg->>transient_for(D),
	Dlg->>modal(transient),
	Dlg->>kind(toplevel), %wel een titelbalk
	Answer = Dlg<<-confirm_centered(D?frame?area?center),
	Dlg->>destroy,
	if
		Answer = save
	then
		D->>saveSketchValue
	else 
               (
		Answer = cancel,
                D->>cancelSketchValue
	       ).
%%



%
checkSaveSketchQuantity(D, LR):->
	"Check changed state of current Ineq selection and ask for saving, fail if change canceled or save failed" ::
	D->>notChangedSketchQuantity(LR),!. %niets aan de hand
%
checkSaveSketchQuantity(D, LR):->
        % something has changed, but the name is not valid, or a duplicate
	D->>notAcceptableSketchQuantity(LR),!, 

         % give warning
	 debug(sketch(stg), 'Invalid quantity name: ~w\n',
		[SelectedQuantityName]),
	 swritef(Str, 'Invalid quantity name: %d\n',
		[SelectedQuantityName]),
         D->>present_message(warning, Str).
        % offer cancel?
%    
checkSaveSketchQuantity(D, LR):->
	%ok, er is iets gewijzigd, dus moet er misschien opgeslagen worden
	Dlg *= dialog('Confirm changes'),
	Dlg->>application(@app),
	Dlg->>append(text('You made changes to a sketch Quantity, which may also be used \nelsewhere in this sketch. Do you want to save your changes?')),
	SC *= imgButton(save_changes,
			    ->>(Dlg,return,save), img:=sketch_save, tt:='Save changes to model'),
	Dlg->>append(SC),
	CC *= imgButton(cancel_changes,->>(Dlg,return,cancel), img:=sketch_undo, tt:='Cancel changes'),
	Dlg->>append(CC),
	EC *= imgButton(edit_changes,->>(Dlg,return,edit), img:=sketch_edit_changes, tt:='Edit changes'),
	Dlg->>append(EC),
	Dlg->>transient_for(D),
	Dlg->>modal(transient),
	Dlg->>kind(toplevel), %wel een titelbalk
	Answer = Dlg<<-confirm_centered(D?frame?area?center),
	Dlg->>destroy,
	if
		Answer = save
	then
		D->>saveSketchQuantity(LR)
	else 
               (
		Answer = cancel,
                D->>cancelSketchQuantity(LR)
	       ).
%%


%
revertSketchQuantity(D, LR):->
	"Check changed state of current Quantity and cancel if necessary" ::
	D->>notChangedSketchQuantity(LR),!. %niets aan de hand

revertSketchQuantity(D, LR):->
        D->>cancelSketchQuantity(LR).
%%

revertSketchValue(D):->
	"Check changed state of current Value and cancel if necessary" ::
	D->>notChangedSketchValue,!. %niets aan de hand

revertSketchValue(D):->
        D->>cancelSketchValue.



checkSaveSketchIneqSelection(D):->
	"Check changed state of current Ineq selection and ask for saving, fail if change canceled or save failed" ::
	D->>notChangedSketchIneq,!. %niets aan de hand
%
checkSaveSketchIneqSelection(D):->
	%ok, er is iets gewijzigd, dus moet er misschien opgeslagen worden

	Dlg *= dialog('Confirm changes'),
	Dlg->>application(@app),
	Dlg->>append(text('You made changes to a sketch Inequality. Do you want to save them?')),
	SC *= imgButton(save_changes,
			    ->>(Dlg,return,save), img:=sketch_save, tt:='Save changes to model'),
	Dlg->>append(SC),
	CC *= imgButton(cancel_changes,->>(Dlg,return,cancel), img:=sketch_undo, tt:='Cancel changes'),
	Dlg->>append(CC),
	EC *= imgButton(edit_changes,->>(Dlg,return,edit), img:=sketch_edit_changes, tt:='Edit changes'),
	Dlg->>append(EC),
	Dlg->>transient_for(D),
	Dlg->>modal(transient),
	Dlg->>kind(toplevel), %wel een titelbalk
	Answer = Dlg<<-confirm_centered(D?frame?area?center),
	Dlg->>destroy,
	if
		Answer = save
	then
		D->>saveSketchIneq
	else 
		Answer = cancel. %falen bij edit
%%


%%
checkSaveSelection(D):->
	"Check changed state of current selection and ask for saving, fail if change canceled or save failed" ::
        debug(sketch(stg), 'checkSaveSelection\n',[]), 
	D->>notChanged,!. %niets aan de hand
%
checkSaveSelection(D):->
	%ok, er is iets gewijzigd, dus moet er misschien opgeslagen worden

	Dlg *= dialog('Confirm changes'),
	Dlg->>application(@app),
	Dlg->>append(text('You made changes to this sketch State. Do you want to save them?')),
	SC *= imgButton(save_changes,
			    ->>(Dlg,return,save), img:=sketch_save, tt:='Save changes to model'),
	Dlg->>append(SC),
	CC *= imgButton(cancel_changes,->>(Dlg,return,cancel), img:=sketch_undo, tt:='Cancel changes'),
	Dlg->>append(CC),
	EC *= imgButton(edit_changes,->>(Dlg,return,edit), img:=sketch_edit_changes, tt:='Edit changes'),
	Dlg->>append(EC),
	Dlg->>transient_for(D),
	Dlg->>modal(transient),
	Dlg->>kind(toplevel), %wel een titelbalk
	Answer = Dlg<<-confirm_centered(D?frame?area?center),
	Dlg->>destroy,
	if
		Answer = save
	then
		D->>save_no_destroy
	else 
		Answer = cancel. %falen bij edit
%%




%%
changed(D):->
	"Succeeeds when the sketchIneq is changed by the user" ::

	%dit draaien we even om
	\+ D->>notChanged.
%%




%%
notChanged(D):->
	% succeed when nothing has changed
	% 1: new definition, is everything still equal to defaults?
  debug(sketch(stg), 'notChanged?',[]), 
        % State ID
	% D?stateID_member?selection->>equal(D?prevName),
	% default_sketchStateName(DefaultName), %zie bovenin deze file
	D?stateID_member?selection->>equal(D?generate_state_ID),

        % Current Sketch Inequality
        D->>notChangedSketchIneq, 
        % Current Inequality
	% @nil = D<<-sketchInequality,!,
        
        % Remarks
	0 = D?remarks_editor?contents<<-size,
    
        % No longer true: 
        % Values and Quantities are removed automatically when they are not used in any Statement, 
        % so it makes no sense to check for changes here, because they are not saved anyway. 
        % In case this is changed, the two code fragments below (for Values and Quantities) may 
        % become useful again.
        % 
        % Values
        get(@model, hypered, stgraphMF, STG), 
	D?sketchValues->>equal(STG?sketchValues), 
        % 
        % Quantities
        % get(@model, hypered, causalModelMF, CM), 
	% D?sketchQuantities->>equal(CM?sortedSketchQuantities),
	D?sortedSketchQuantities->>equal(STG?sortedSketchQuantities), 

        % Inequalities
        D?sketchInequalities->>empty.



%%
notChanged(D):->
	% succeed when nothing has changed
	% 2: changed definition, is everything still as it was?

        % State ID
	D?stateID_member?selection->>equal(D?prevName),
	% D?stateID_member?selection->>equal(Name),

        % Current Sketch Inequality
        D->>notChangedSketchIneq, 
	% @nil = D<<-sketchInequality,!,
        
        % Remarks
	D?element?remarks->>equal(D?remarks_editor?contents),

        % No longer true: 
        % Values and Quantities are removed automatically when they are not used in any Statement, 
        % so it makes no sense to check for changes here, because they are not saved anyway. 
        % In case this is changed, the two code fragments below (for Values and Quantities) may 
        % become useful again.
        % 
        % Quantities 
        get(@model, hypered, stgraphMF, STG), 
	D?sortedSketchQuantities->>equal(STG?sortedSketchQuantities), 
        % 
        % Values
	D?sketchValues->>equal(STG?sketchValues),

        % Inequalities
	D?sortedSketchInequalities->>equal(D?element?sortedSketchStateInequalities).
%%

%%
notChangedSketchIneq(D):->
	% succeed when nothing has changed to the selected Sketch Inequality
        D->>notChangedIneqLHS,
        D->>notChangedIneqSign,
        D->>notChangedIneqRHS.
%%


%%
cancel(D):->
	"Cancel button" ::
	% reset all data related to the current sketch state
	% should another inequality be selected?

        D->>revertSketchQuantity(left),
        % always do both V and Q2, or only one of them, based on selectMode?
        D->>revertSketchQuantity(right),
        D->>revertSketchValue,
        send(D, reset_sketch_state_data).
%%

%%
reset_sketch_state_data(D):->
        % get last saved sketch inequalities for this state element
        get(D, element, SketchState), 
        @nil \= SketchState,
	get(SketchState, sortedSketchStateInequalities, Ineqs), 
	send(D, sketchInequalities, Ineqs?copy), 

	D->>fillSketchIneqList,
        % reset state name
	D?stateID_member->>selection(SketchState?name),
        % or D?prevName? 
        % D->>prevName(SketchState?name),	
	% reset Remarks 
	D?remarks_member->>contents(SketchState?remarks),
        % reset current sketch inequality data
        D->>slot(sketchInequality, @nil), 
	D->>fillCurrentSketchIneqData.
%%

reset_sketch_state_data(D):->
        get(D, element, SketchState), 
        @nil = SketchState,
        send(D, resetSelections).
%%

%%
save(D):->
	"Save button" :: 
     debug(sketch(stg), 'save in sketch_state_props.pl',[]),
	D->>saveSketchQuantity(left),
        % always do both V and Q2, or only one of them, based on selectMode?
	D->>saveSketchQuantity(right),
	D->>saveSketchValue,       
        
        % saveSketchIneq is also done in save_no_destroy, but seems necessary anyhow
	D->>saveSketchIneq, 
	D->>save_no_destroy.
%%

%
save_no_destroy(D):->
	(   @nil = D<<-element
	->  
	    D->>saveNewElement 
	;  
	    D->>saveChangedElement
	).
%%


%
saveSketchIneq(D):->
        % there is no SketchInequality to save because LHS and RHS have not changed (still default name)
        get(D?quantityName, value, LHSName), 
        get(D?rhsName, value, RHSName), 
	LHSName = '',
	RHSName = '',!,
        swritef(Str, 'Sketch inequality does not have to be saved. No quantity or value has been selected or entered.\n',[]),
        debug(sketch(stg), '~w',[Str]).
%
saveSketchIneq(D):->
        % there is no SketchInequality to save because LHS has not changed (still default name)
        get(D?quantityName, value, LHSName), 
	defaultLHSName(DefLHSName), 
        LHSName = DefLHSName,
        get(D?rhsName, value, RHSName), 
	defaultRHSName(DefRHSName), 
        RHSName = DefRHSName,!,
        swritef(Str, 'Sketch inequality can not be saved. No quantity or value has been selected or entered.\n',[]),
        D->>present_message(warning, Str),!,
        fail.
%
saveSketchIneq(D):->
        % there is no SketchInequality to save because RHS has not changed (still default name)
        get(D?rhsName, value, RHSName), 
	defaultRHSName(DefRHSName), 
        RHSName = DefRHSName,
        swritef(Str, 'Sketch inequality can not be saved. Nothing has been selected or entered on the right-hand side.\n',[]),
        D->>present_message(warning, Str),!,
        fail.
%
saveSketchIneq(D):->
        % there is no SketchInequality to save because there is nothing on the LHS
        get(D?quantityName, value, LHSName), 
	LHSName = '',
        swritef(Str, 'Sketch inequality can not be saved. There is nothing on the left-hand side.\n',[]),
        D->>present_message(warning, Str),!,
        fail.
%
saveSketchIneq(D):->
        % there is no SketchInequality to save because LHS has not changed (still default name)
        get(D?quantityName, value, LHSName), 
	defaultLHSName(DefLHSName), 
        LHSName = DefLHSName,
        swritef(Str, 'Sketch inequality can not be saved. Nothing has been selected or entered on the left-hand side.\n',[]),
        D->>present_message(warning, Str),!,
        fail.
%
saveSketchIneq(D):->
        % impossible to save because the LHS is unacceptable
        D->>notAcceptableSketchQuantity(left),!, % no need to save
        swritef(Str, 'Sketch inequality can not be saved. Invalid name for quantity on the left-hand side.\n',[]),
        D->>present_message(warning, Str),!,
        fail.
%
saveSketchIneq(D):->
        % there is no SketchInequality to save because there is nothing on the RHS
        get(D?rhsName, value, RHSName), 
	RHSName = '',
        swritef(Str, 'Sketch inequality can not be saved. There is nothing on the right-hand side.\n',[]),
        D->>present_message(warning, Str),!,
        fail.
%
%
saveSketchIneq(D):->
        % there is no SketchInequality to save because RHS has not changed (still default name)
        get(D?rhsName, value, RHSName), 
	defaultRHSName(DefRHSName), 
        RHSName = DefRHSName,
        swritef(Str, 'Sketch inequality can not be saved. Nothing has been selected or entered on the right-hand side.\n',[]),
        D->>present_message(warning, Str),!,
        fail.
%
saveSketchIneq(D):->
        % impossible to save because the RHS is unacceptable
        if D?selectMode->>equal(quantities)
        then 
        (
           D->>notAcceptableSketchQuantity(right),
           swritef(Str, 'Sketch inequality can not be saved. Invalid name for quantity on the right-hand side.\n',[]),
           D->>present_message(warning, Str)
        )
        else
        (
           D->>notAcceptableSketchValue,           
           swritef(Str, 'Sketch inequality can not be saved. Invalid name for value on the right-hand side.\n',[]),
           D->>present_message(warning, Str)
        ),!,
        fail.
%
% case of new inequality of the form Q ? V
% 
saveSketchIneq(D):->
        % save new SketchInequality
	@nil = D<<-sketchInequality, 
	%kee, nu moeten we een CR bouwen
	%hangt af of we in nieuw modus zijn of niet

        % the case of a Q ? V, and a value being selected
        D?selectMode->>equal(values), 
        get(D?sketchValuesBrowser, selection, Sel), 
        Sel \== @nil, !,
 
        debug(sketch(stg), 'save new SketchIneq of the form Q ? V \n',[]), 
        get(D, quantityName, LHSName), 
        get(D?sketchQuantitiesBrowser, member, LHSName?value, LHSItem), 
        get(LHSItem, object, FirstObj),
        % get(D?sketchQuantitiesBrowser?selection, object, FirstObj), 
        get(D?sketchValuesBrowser?selection, object, SecondObj),
        % get(FirstObj, name, First), 
        % get(SecondObj, name, Second),   
        get(D?type_member, selection, Type), 
        % symbol_typeName(Type, Symbol), 
        % writef('%p %d %p\n', [First, Symbol, Second]), 
        % swritef(IneqStr, '%p %d %p', [First, Symbol, Second]), 
        get(D, sortedSketchInequalities, Ineqs), 
        new(NewIneq, sketchInequalityElement(FirstObj, Type, SecondObj, sketchValueElement, '')), 
        send(Ineqs, append, NewIneq),     
        % send(Ineqs, append, IneqStr),     
	D->>slot(sketchInequality, NewIneq),
	D->>fillSketchIneqList,  %opnieuw inlezen
	D?sketchIneqList->>selection(NewIneq?displayNameString).
%
% case of new inequality of the form Q1 ? Q2
% 
saveSketchIneq(D):->
        % save new SketchInequality
	@nil = D<<-sketchInequality, !,
	%kee, nu moeten we een CR bouwen
	%hangt af of we in nieuw modus zijn of niet

        D?selectMode->>equal(quantities), 
        debug(sketch(stg), 'save new SketchIneq of the form Q1 ? Q2 \n',[]), 
        get(D, quantityName, LHSName), 
        get(D, rhsName, RHSName), 
        get(D?sketchQuantitiesBrowser, member, LHSName?value, LHSItem), 
        get(LHSItem, object, FirstObj),
        get(D?sketchQuantitiesBrowser, member, RHSName?value, RHSItem), 
        get(RHSItem, object, SecondObj),
        % get(FirstObj, name, First), 
        % get(SecondObj, name, Second),   
        get(D?type_member, selection, Type), 
        % symbol_typeName(Type, Symbol), 
        % writef('%p %d %p\n', [First, Symbol, Second]), 
        % swritef(IneqStr, '%p %d %p', [First, Symbol, Second]), 
        get(D, sortedSketchInequalities, Ineqs), 
        new(NewIneq, sketchInequalityElement(FirstObj, Type, SecondObj, sketchQuantityElement, '')), 
        send(Ineqs, append, NewIneq),     
        % send(Ineqs, append, IneqStr),     
	D->>slot(sketchInequality, NewIneq),
	D->>fillSketchIneqList,  %opnieuw inlezen
	D?sketchIneqList->>selection(NewIneq?displayNameString).
%
% case of changed inequality of the form Q ? V
%
saveSketchIneq(D):->
        % save changed SketchInequality
	get(D, sketchInequality, SketchIneq), 

        % the case of a Q ? V, and a value being selected
        D?selectMode->>equal(values), 
        get(D?sketchValuesBrowser, selection, Sel), 
        Sel \== @nil, !,
        debug(sketch(stg), 'save changed SketchIneq of the form Q ? V \n',[]), 
        get(D, quantityName, QName), 
        % garpify Name
    	LHSName = QName<<-makeGarp,
        get(D?sketchQuantitiesBrowser, member, LHSName?value, LHSItem), 
        get(LHSItem, object, FirstObj),
        % get(D?sketchQuantitiesBrowser?selection, object, FirstObj), 
        get(D?sketchValuesBrowser?selection, object, SecondObj),  
        % get(FirstObj, name, First), 
        % get(SecondObj, name, Second),   
        get(D?type_member, selection, Type), 
        % symbol_typeName(Type, Symbol), 
        % writef('%p %d %p\n', [First, Symbol, Second]), 
        % swritef(IneqStr, '%p %d %p', [First, Symbol, Second]), 
        get(D, sortedSketchInequalities, Ineqs), 
        new(NewIneq, sketchInequalityElement(FirstObj, Type, SecondObj, sketchValueElement, '')), 
        % send(Ineqs, append, NewIneq), 
        send(Ineqs, replace, SketchIneq, NewIneq), 
        % send(Ineqs, append, IneqStr),     
	D->>slot(sketchInequality, NewIneq),
	D->>fillSketchIneqList,  %opnieuw inlezen
	D?sketchIneqList->>selection(NewIneq?displayNameString).
%
% case of changed inequality of the form Q1 ? Q2
% 
saveSketchIneq(D):->
        % save changed SketchInequality
	get(D, sketchInequality, SketchIneq), 
        D?selectMode->>equal(quantities), 
        debug(sketch(stg), 'save changed SketchIneq of the form Q1 ? Q2 \n',[]), 
        get(D, quantityName, LHSName), 
        get(D, rhsName, RHSName), 
        get(D?sketchQuantitiesBrowser, member, LHSName?value, LHSItem), 
        get(LHSItem, object, FirstObj),
        get(D?sketchQuantitiesBrowser, member, RHSName?value, RHSItem), 
        get(RHSItem, object, SecondObj),
        % get(FirstObj, name, First), 
        % get(SecondObj, name, Second),   
        get(D?type_member, selection, Type), 
        % symbol_typeName(Type, Symbol), 
        % writef('%p %d %p\n', [First, Symbol, Second]), 
        % swritef(IneqStr, '%p %d %p', [First, Symbol, Second]), 
        get(D, sortedSketchInequalities, Ineqs), 
        new(NewIneq, sketchInequalityElement(FirstObj, Type, SecondObj, sketchQuantityElement, '')), 
        % send(Ineqs, append, NewIneq), 
        send(Ineqs, replace, SketchIneq, NewIneq), 
        % send(Ineqs, append, IneqStr),     
	D->>slot(sketchInequality, NewIneq),
	D->>fillSketchIneqList,  %opnieuw inlezen
	D?sketchIneqList->>selection(NewIneq?displayNameString).

/*
        % To Do - use changeRequest system
        % this would be a nicer way to do it, instead of some of the clauses above
	if 
		@nil = D<<-sketchInequality 
	then 
		@model->>changeRequest(addSketchInequality,
				@model,
				D,
				D?sketchIneqName?selection,
				D?sketchQuantitiesBrowser?selection?object,
				D?Type?selection,
				D?sketchValuesBrowser?selection?object,
				D?remarks_editor?contents)
	else
		@model->>changeRequest(changeSketchInequality,
				D?sketchInequality,
				D,
				D?sketchIneqName?selection,
				D?sketchQuantitiesBrowser?selection?object,
				D?Type?selection,
				D?sketchValuesBrowser?selection?object,
				D?remarks_editor?contents),
	D->>fillSketchIneqList. %opnieuw inlezen
*/
%%	


resetSelections(D):->
        D?sketchIneqList->>selection(@nil),
        D?sketchQuantitiesBrowser->>selection(@nil),
        D?sketchValuesBrowser->>selection(@nil),
        D?quantityName->>clear,
        D?rhsName->>clear,
        D?type_member->>selection(eq),
        D->>previousQuantityNameL(''),
        D->>previousQuantityL(@nil),
        D->>previousQuantityNameR(''),
        D->>previousQuantityR(@nil),
        D->>previousValueName(''),
        D->>previousValue(@nil).
        % D?sign->>clear.



symbol_typeName(l, '<').
symbol_typeName(leq, '\u2264'). % =< char
% symbol_typeName(leq, '=<').
symbol_typeName(eq, '=').
% symbol_typeName(geq, '>='). 
symbol_typeName(geq, '\u2265'). % >= char
symbol_typeName(g, '>').



%%%%%%%wat handige verwijzingen naar members%%%%%%%%%%%%%%
%%
sketchIneqList(D,
			L : list_browser
			):<-
	"Mapping on sketchIneq list member" ::

	L = D<<-member(sketchIneqList).
%%

%%
copySketchIneq(D,
	B : button):<-
	"Mapping on copySketchIneq button member" ::

	B = D<<-member(copySketchIneq).
%%

%%
removeSketchIneq(D,
	B : button):<-
	"Mapping on removeSketchIneq button member" ::

	B = D<<-member(removeSketchIneq).
%%

/*
%%
sketchIneqName(D,
		N : text_item
		):<-
	"Mapping on sketchIneq name item" ::
	N = D<<-member(sketchIneqName).
%%
*/



%
sketchQuantitiesBrowserLR(D, LR, 
	Q : sketchQuantityBrowser
	):<-
	"Mapping on either the left or right sketchQuantitiesBrowser list member" ::
        if LR = left
        then 
	   Q = D<<-member(sketchQuantitiesBrowser)
        else
	   Q = D<<-member(sketchQuantitiesBrowser2). 

%%

%
sketchQuantitiesBrowser(D,
	% Q : list_browser
	Q : sketchQuantityBrowser
	):<-
	"Mapping on sketchQuantitiesBrowser list member" ::
	Q = D<<-member(sketchQuantitiesBrowser).
%%

%
sketchQuantitiesBrowser2(D,
	% Q : list_browser
	Q : sketchQuantityBrowser
	):<-
	"Mapping on sketchQuantitiesBrowser2 list member" ::
	Q = D<<-member(sketchQuantitiesBrowser2).
%%

%
sketchValuesBrowser(D,
	% Q : list_browser
	Q : sketchQuantityBrowser
	):<-
	"Mapping on sketchValuesBrowser list member" ::
	Q = D<<-member(sketchValuesBrowser).
%%


%%
remarks_editor(D,
	R : editor
	):<-
	"Mapping on remarks editor member" ::

	R = D<<-member(remarks).
%%

%%
quantityName(D,
	T : text_item):<-
	"Mapping on quantity name item" ::

	T = D<<-member(quantityName).
%%

%
rhsName(D,
	T : text_item):<-
	"Mapping on value name item" ::

	T = D<<-member(rhsName).
%%


sign(D,
	T : text_item):<-
	"Mapping on sign name item" ::

	T = D<<-member(sign).
%%



type(D,
	T : name):<-
	"Mapping on TypeMenu item" ::

	TypeMenu = D<<-member(type),
        get(TypeMenu, selection, T).
%%


%%%%%%%%%%Helpers%%%%%%%%%%%%%%%%
%%
selectQuantityName(D):->
	"Make quantity name entry selected" ::

	D?quantityName->>activate(@on),
	D->>caret(D?quantityName),
        D->>previousQuantityNameL(D?sketchQuantitiesBrowser?selection?name),
        D->>previousQuantityL(D?sketchQuantitiesBrowser?selection?object).
%%
%
selectQuantityName2(D):->
	"Make quantity name 2 entry selected" ::

	D?rhsName->>activate(@on),
	D->>caret(D?rhsName),
        D->>previousQuantityNameR(D?sketchQuantitiesBrowser2?selection?name),
        D->>previousQuantityR(D?sketchQuantitiesBrowser2?selection?object).
%%

%%
selectValueName(D):->
	"Make value name entry selected" ::

	D?rhsName->>activate(@on),
	D->>caret(D?rhsName),
        D->>previousValueName(D?sketchValuesBrowser?selection?name),
        D->>previousValue(D?sketchValuesBrowser?selection?object).
%%




%%deleteSketchInequality: geen check
applyChange_deleteSketchInequality(D,
	CR: changeRequestor):->

        D?sketchInequalities->>delete(CR?arg1).
%%


%%
changeApplied_deleteSketchInequality(D,
	CR : changeRequestor
	):->
	/*
	Het kan dus zijn dat de geselecteerde SketchIneq weg is. In dat geval kiezen we een andere
	*/
        debug(sketch(delete), 'changeApplied_deleteSketchInequality in sketch_state_props.pl', []), 
	DeletedSketchIneq = CR<<-argument(1),
	ListBox = D<<-sketchIneqList,
	List = D<<-sketchInequalities,
        % delete from the list of sketchInequalities in the editor (used for temporary storage)
        % they are definitively deleted only when the state is saved
        List->>delete_all(DeletedSketchIneq), 

	if 
		(DeletedSketchIneq = D<<-sketchInequality)
	then (
		% reset selection
		(
		 	NewSketchIneq = @nil
		),	        
		ListBox->>selection(NewSketchIneq),
		D->>slot(sketchInequality, NewSketchIneq),
	        send(D, resetSelections)
		% D->>fillCurrentSketchIneqData
		),
        send(D, resetSelections).
%%		


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% :-pce_end_class.
%
% the rest comes from the old version, based on property dialog
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%
sketch(D,SK: sketch):<-
	%geef de bewerkte sketch terug, dit is altijd de sketch van de editor

	SK=D?editor<<-sketch.
%%



%%
newObject(D,
	  StartType : {sketchState} % this variable could be removed. AB, feb 2006
	 ):->
	"Open the dialog for a new sketchState" ::

	D->>startType(StartType),	
	D->>element(@nil),

        % clear sketch inequalities
	send(D?sketchInequalities, clear), 
	
	%gp3 0.2: set the right defbutton
	if
		StartType = sketchState
	then
	(
	    	get(@model, getModelNameForEditor, 'Add a new state - Sketch', ModelNameForEditor),
		D->>label(ModelNameForEditor),
		D->>helpId('Build_MF_AddNewSketchState') %gp3 0.3.13
	),

	D->>openDialog.
%%
	
	  
%%
editObject(D,
 	    SketchState : sketchStateElement,
	    ReadOnly : bool
	  ):->
	"Open the dialog for editing an existing sketchStateElement"::
	D->>element(SketchState),

	D->>startType(sketchState),
	get(@model, getModelNameForEditor, string('Sketch State properties%s - Sketch',when(ReadOnly == @on,' [Read Only]',''))?value, ModelNameForEditor),
	D->>label(ModelNameForEditor),
	D->>helpId('Build_MF_SketchStateProperties'), %gp3 0.3.13

        % get sketch inequalities for this state
	get(SketchState, sortedSketchStateInequalities, Ineqs), 
        % writef('Ineqs in S: %d \n',[Ineqs]), 
	% Ineqs->>for_all(->>(@prolog, write, @arg1?displayNameString)),
        % copy for editing purposes
	send(D, sketchInequalities, Ineqs?copy), 

        % get(D, sortedSketchInequalities, Ineqs2), 
        % writef('Ineqs in D: %d \n',[Ineqs2]), 
	% Ineqs2->>for_all(->>(@prolog, write, @arg1?displayNameString)),
        % writef('D: %d\nIneqs: %d\n',[D, Ineqs]),

	D->>fillSketchIneqList,

	D?stateID_member->>selection(SketchState?name),
	D?stateID_member->>editable(ReadOnly?negate),
        % record existing name as prevName
        D->>prevName(SketchState?name),	
	D?remarks_member->>contents(SketchState?remarks),

	D->>openDialog.	
%%



	  
%%
copyObject(D,
 	    SketchState : sketchStateElement,
	    ReadOnly : bool
	  ):->
	"Open the dialog for editing an existing sketchStateElement"::
    
        % Generate a State ID (an integer which is not in use yet)
        get(D, generate_state_ID, StateID), 

        get(SketchState, copy, D?sketch, StateID, SketchStateCopy), 
	D->>element(SketchStateCopy),
        % set element to @nil, so SketchStateCopy will be treated as a new one
	% D->>element(@nil),
	D?editor->>update_visualisation,  % this may be in the wrong place, here - AB, june 2006
	% D?editor->>mustUpdateVisualisation,  % this may be in the wrong place, here - AB, june 2006

	D->>startType(sketchState),
	get(@model, getModelNameForEditor, string('Sketch State properties (copy) %s - Sketch',when(ReadOnly == @on,' [Read Only]',''))?value, ModelNameForEditor),
	D->>label(ModelNameForEditor),
	D->>helpId('Build_MF_SketchStateProperties'), %gp3 0.3.13

        % get sketch inequalities for this state

	get(SketchStateCopy, sortedSketchStateInequalities, Ineqs), 
        % writef('Ineqs in S: %d \n',[Ineqs]), 
	% Ineqs->>for_all(->>(@prolog, write, @arg1?displayNameString)),

        % copy for editing purposes
	send(D, sketchInequalities, Ineqs?copy), 

        % get(D, sortedSketchInequalities, Ineqs2), 
        % writef('Ineqs in D: %d \n',[Ineqs2]), 
	% Ineqs2->>for_all(->>(@prolog, write, @arg1?displayNameString)),
        % writef('D: %d\nIneqs: %d\n',[D, Ineqs]),

	D->>fillSketchIneqList,

	D?stateID_member->>selection(SketchStateCopy?name),
	D?stateID_member->>editable(ReadOnly?negate),
        % record existing name as prevName
        D->>prevName(SketchStateCopy?name),	
	D?remarks_member->>contents(SketchStateCopy?remarks),
        % there is no ok_member - save, maybe?
%	D?ok_member->>active(ReadOnly?negate),
	D->>openDialog.
%%



%
saveSketchValue(D):->
        D->>notChangedSketchValue,!, % no need to save
        % writef('save sketch Value - not changed, so no need to save\n',[]), 
        % if a name was entered which can be selected, select it
        get(D?rhsName, value, RHSName), 
        % writef('entered value %d in text field\n',[RHSName]), 
        if D?sketchValuesBrowser->>member(RHSName)
        then 
        ( 
            % writef('select value %d in browser\n',[RHSName]), 
            D?sketchValuesBrowser->>select(RHSName), 
            D->>previousValueName(D?sketchValuesBrowser?selection?name),
            D->>previousValue(D?sketchValuesBrowser?selection?object)
        )
        else
        (
            true 
        ).
%
saveSketchValue(D):->
        D->>notAcceptableSketchValue,!, % no need to save
         % give warning
	 debug(sketch(stg), 'Invalid value name\n',
		[]),
	 swritef(Str, 'Invalid value name\n',
		[]),
         D->>present_message(warning, Str),!.
        % offer cancel?
%
saveSketchValue(D):->
        % The name of the value itself has already been changed, no need to save
        % Need to update relevant inequalities too 
        get(D, previousValue, Arg), 
        unless
           Arg = @nil
        do 
        (
           D->>updateAllSketchIneqLists(Arg)
        ),
        D->>previousValueName(D?sketchValuesBrowser?selection?name),
        D->>previousValue(D?sketchValuesBrowser?selection?object),
	D?editor->>update_visualisation.
%


%
cancelSketchValue(D):->
        % The name of the value has been changed, but this should not be saved
        % Need to update relevant inequalities too 
        get(D, previousValueName, OldName), 
        % writef('Revert back to previous sketch value name: %d \n',[OldName]),
        get(D, previousValue, Arg), 
        % no need to updateAllSketchIneqLists?
        unless
           Arg = @nil
        do 
        (
           Arg->>name(OldName),
           D->>updateAllSketchIneqLists(Arg)
        ),
        % update sketchValuesBrowser        
        get(D?sketchValuesBrowser?selection, object, Sel),
	D->>fillSketchValuesBrowser(D?sketchValuesBrowser),
        send(D?sketchValuesBrowser, select, Sel?name),
	D?rhsName->>value(Sel?name),
	D?editor->>update_visualisation.
%%


%%
saveSketchQuantity(D, LR):->
        D->>notChangedSketchQuantity(LR),!, % no need to save
        % writef('Sketch Quantity on the %d has not changed, so no need to save.\n',[LR]),
        % if a name was entered which can be selected, select it in the appropriate browser
        get(D, sketchQuantitiesBrowserLR, LR, SQB), 
        if LR = left
        then
        (
           get(D?quantityName, value, QName)
        )
        else
        (
           if D?selectMode->>equal(quantities)
           then 
           (
               get(D?rhsName, value, QName)
           )
           else
           (
               % make this clause succeed without doing anything
               QName = '' 
           )
        ),
        % writef('entered quantity name %d in text field\n',[QName]), 
        if SQB->>member(QName)
        then 
        ( 
            % writef('select quantity %d in browser\n',[QName]), 
            SQB->>select(QName)
        )
        else
        (
            true 
        ),
        unless SQB?selection->>equal(@nil)
        do 
        ( 
          % record selection as previous quantity 
          if LR = left
          then
          (
            D->>previousQuantityNameL(SQB?selection?name),
            D->>previousQuantityL(SQB?selection?object)
          )
          else
          (

            D->>previousQuantityNameR(SQB?selection?name),
            D->>previousQuantityR(SQB?selection?object)
          )
        ).


saveSketchQuantity(D, LR):->
        D->>notAcceptableSketchQuantity(LR),!, % no need to save
         % give warning
	 debug(sketch(stg), 'Invalid (or already used) quantity name\n',
		[]),
	 swritef(Str, 'Invalid (or already used) quantity name\n',
		[]),
         D->>present_message(warning, Str),!.
        % offer cancel?


%%
saveSketchQuantity(D, LR):->
        % The name of the quantity itself has already been changed, no need to save
        % Need to update relevant inequalities too 

        get(D, sketchQuantitiesBrowserLR, LR, SQB), 
        if LR = left
        then
        (
           get(D, previousQuantityL, Arg), 
           unless
              Arg = @nil
           do 
           (
              D->>updateAllSketchIneqLists(Arg)
           ),
           D->>previousQuantityNameL(SQB?selection?name),
           D->>previousQuantityL(SQB?selection?object)
        )
        else
        (
           get(D, previousQuantityR, Arg), 
           unless
              Arg = @nil
           do 
           (
              D->>updateAllSketchIneqLists(Arg)
           ),
           D->>previousQuantityNameR(SQB?selection?name),
           D->>previousQuantityR(SQB?selection?object)
        ),
	D?editor->>update_visualisation.
%


%%
cancelSketchQuantity(D, LR):->
        % The name of the quantity has been changed, but this should not be saved
        % Need to update relevant inequalities too 
        get(D, sketchQuantitiesBrowser, SQB1), 
        get(D, sketchQuantitiesBrowser2, SQB2), 
        if LR = left
        then
        (
          SQB = SQB1,
          get(SQB?selection, object, Sel),
          get(D, previousQuantityNameL, OldName), 
          % writef('Revert back to previous sketch quantity name: %d \n',[OldName]),
          get(D, previousQuantityL, Arg), 
          % no need to updateAllSketchIneqLists?
          unless
             Arg = @nil
          do 
          (
             Arg->>name(OldName),
             D->>updateAllSketchIneqLists(Arg)
          ),
          D?quantityName->>value(Sel?name)
        )
        else
        (
          % LR = right
          SQB = SQB2,
          get(SQB?selection, object, Sel),
          get(D, previousQuantityNameR, OldName), 
          % writef('Revert back to previous sketch quantity name 2: %d \n',[OldName]),
          get(D, previousQuantityR, Arg), 
          % no need to updateAllSketchIneqLists?
          unless
             Arg = @nil
          do 
          (
             Arg->>name(OldName),
             D->>updateAllSketchIneqLists(Arg)
          ),
          D?rhsName->>value(Sel?name)
        ),
        % update sketchQuantitiesBrowser        
	D->>fillSketchQuantitiesBrowser(SQB1),
	D->>fillSketchQuantitiesBrowser(SQB2),
        send(SQB, select, Sel?name),
	D?editor->>update_visualisation.
%%



%%
saveSketchValues(D):->
        get(D, sortedSketchValues, SketchValues), 
        get(@model, hypered, stgraphMF, STGraphMF), 
        send(STGraphMF, sketchValues, SketchValues). 
%

%%
saveSketchQuantities(D):->
        get(D, sortedSketchQuantities, SketchQuantities), 
        get(@model, hypered, stgraphMF, STGraphMF), 
        % to add them also to the causal model sketch, uncomment the next lines
        % get(@model, hypered, causalModelMF, CM), 
        % add only new SketchQuantities to elements of CM
        % this does not work, because there is no such chain called sketchQuantities in CM
        % send(CM, sketchQuantities, SketchQuantities). 
        % send(CM?elements, union, SketchQuantities). 
        % send(STGraphMF?elements, union, SketchQuantities). 
        send(STGraphMF?sketchQuantities, union, SketchQuantities). 
%


%%
saveNewElement(D):->
	"Save the changes that were made" ::
	D->>notChanged,!. %niets te doen

%%
saveNewElement(D):->
        % save SketchValues if necessary
        send(D, saveSketchValues), 
        % save SketchQuantities if necessary
        send(D, saveSketchQuantities), 
        % save SketchInequality if necessary
        send(D, saveSketchIneq), 
	@model->>changeRequest(newSketchState,
		D?sketch,
		D?editor,
		D?entity,
		D?sketchStateName,
		D?remarks, 
		D?sortedSketchInequalities).
%



%%
saveChangedElement(D):->
	"Save the changes that were made" ::
	D->>notChanged,!. %niets te doen

saveChangedElement(D):->
        % save SketchValues if necessary
        send(D, saveSketchValues), 
        % save SketchQuantities if necessary
        send(D, saveSketchQuantities), 
        % save SketchInequality if necessary
        send(D, saveSketchIneq), 
	@model->>changeRequest(changeSketchState,
		D?sketch,
		D?editor,
		D?element,
		D?entity,
		D?sketchStateName,
		D?remarks, 
		D?sortedSketchInequalities).
%%


% It is strange to call this 'entity', but instead of renaming it, 
% maybe it could be deleted altogether. This would require other files 
% to be changed as well, though.
%%
entity(_D,
       E : 'abstractEntity*'
      ):<-
	"Return top level sketchStateElement = sketchStateElement" ::
	E = @model<<-hypered(topSketchState).
%%


%%
sketchState(D,
       E : 'sketchStateElement*'
      ):<-
	"Return the currently selected sketchState or @nil" ::
        get(D, element, E). 
%%

%%
sketchStateName(D,
	     N : name
	    ):<-
	"Return the given name" ::

	Name = D<<-member(stateID),
	N = Name<<-selection.
%%

%%
remarks(D,
	R : char_array
       ):<-
	"Return the given remarks" ::

	Remarks = D<<-member(remarks),
	R = Remarks<<-contents.
%%
	



%%%%%%%%%CHANGES%%%%%%%%%%%%%%%%


% After creating a new State, set it as the current state
changeApplied_newSketchState(D,
	CR: changeRequestor):->
	% \+ CR->>checkEditor(D?editor), %niet als deze editor (anders dubbel sluiten)
	D->>slot(element, CR?result), 
        % record current name of sketchState as prevName
        D->>prevName(D?element?name).
%%


%%Bij wijzigingen van onze instantie sluiten we
changeApplied_changeSketchState(D,
	CR: changeRequestor):->
        % record current name of sketchState as prevName
        D->>prevName(D?element?name),
	\+ CR->>checkEditor(D?editor), %niet als deze editor (anders dubbel sluiten)
	CR->>checkArgument(1,D?element),
	D->>return.
%%


% After copying a sketch state, close the window
changeApplied_copySketchState(D,
	CR: changeRequestor):->
	\+ CR->>checkEditor(D?editor), %niet als deze editor (anders dubbel sluiten)
	CR->>checkArgument(1,D?element),
	D->>return.
%%


%%
changeApplied_deleteSketchState(D,
	CR: changeRequestor):->
	
	CR->>checkArgument(1,D?element),
	D->>return.
%%


%%
sortedSketchInequalities(D, Ineqs: chain):<-
        get(D, sketchInequalities, Ineqs),        
        % writef('sortedSketchInequalities \n',[]),
        % writef('Ineqs: %d \n',[Ineqs]), 
        % visualize:write_chain(Ineqs), 
	if 
		@nil = Ineqs
	then
                % do nothing
		send(Ineqs, clear)
	else
        (
                % normal sort is necessary in case multiple inequalities with same displayNameString occur
                Ineqs->>sort,
                Ineqs->>sort(?(@arg1?displayNameString, compare, @arg2?displayNameString))
        ).
%

%%
sortedSketchQuantities(D, SketchQuantities: chain):<-
        get(D, sketchQuantities, SketchQuantities),        
        SketchQuantities->>sort(?(@arg1?name, compare, @arg2?name)).
%



%%
sortedSketchValues(D, Values: chain):<-
        get(D, sketchValues, Values),        
        % writef('sortedSketchValues \n',[]),
        % writef('Values: %d \n',[Values]), 
        % visualize:write_chain(Values), 
        Values->>sort(?(@arg1?name, compare, @arg2?name)).
%



/*******************CHANGE REQUESTORS****************/


%%
changeApplied_newSketchValue(VE,
			    CR:changeRequestor
			   ):->
	%onze reaktie bij een nieuwe instantie: we moeten hem maken
	%en afbeelden
	%gp3 0.3: no selection change even when this editor initiated the change

	VE->>update_visualisation, %need to do it right now
	
	IE = VE<<-findFirstElement(CR?result),
	unless
		CR->>checkEditor(VE)
	do
		VE->>checkHideNewElement(IE,CR).
%%


%%
changeApplied_newSketchValueElement(VE,
			    CR:changeRequestor
			   ):->
	%onze reaktie bij een nieuwe instantie: we moeten hem maken
	%en afbeelden
	%gp3 0.3: no selection change even when this editor initiated the change

	VE->>update_visualisation, %need to do it right now
	
	IE = VE<<-findFirstElement(CR?result),
	unless
		CR->>checkEditor(VE)
	do
		VE->>checkHideNewElement(IE,CR).


%%
changeApplied_changeSketchValue(VE,
			      CR: changeRequestor
			     ):->
	%gp3 0.2: changed to only update the relevant elements
	VE->>updateElement(CR?arg1).
%%



%%
changeApplied_deleteSketchValue(VE,
			      _CR:changeRequestor
			     ):->
	VE->>mustUpdateVisualisation. %gp3, changed this to update when all done
%%


:-pce_end_class.


