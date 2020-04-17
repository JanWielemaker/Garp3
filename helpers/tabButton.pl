/*
Garp3 0.2
The application mainmenu + helper classes

Part of Garp3 - see copyright notice
*/

:-pce_begin_class(
		    tabButton,
		    tab,
		    "A tab which is also a button"
		 ).

variable(realTabName, name, both). % The real name of the currently loaded file

initialise(T, Name) :->
    send(T, slot, realTabName, Name),
    send_super(T, initialise, Name).

label_event(T, Event) :->
    get(Event, id, EventType),
    %format('Het event is van type: ~w\n', [EventType]),
    (
	EventType = 'ms_left_down',
	get(Event, receiver, Tab),
	send(@app, changeTab, Tab)
    ;
	true
    ),
    send_super(T, label_event, Event).

get_tabname(T, Size, TruncatedName) :<-
    get(T?realTabName, value, TabName),
    string_length(TabName, StringLength),
    (
	StringLength < Size ->
	sub_string(TabName, 0, StringLength, _X, TruncatedName)
    ;
	sub_string(TabName, 0, Size, _Y, TruncatedName)
    ).

update_tabname(T, Size) :->
    get(T, get_tabname, Size, TruncatedName),
    send(T, name, TruncatedName).



:-pce_end_class.

