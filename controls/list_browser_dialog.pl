/*
listBrowserDialog class
part of GARP3

2006 Jelmer Jellema - Spin in het Web

%gp3 0.3.13
%listBrowserDialog is an assistanceDialog subclass that just shows a list_browser (or to be exact: an extendedListBrowser) for use as a data view (used a lot in visualize part)

%it does nothing except for showing a close and a details button.
%To set open messages for the list use Dialog<<-list to get it

%to make double clicking on the listbrowser do something, set detailsMessage to some code. @arg1 will be the key of the selected item in the listbrowser, @arg2 will be the dialog

%To show the details button, set ShowDetails to @on. The button will call the same detailsMessage as the list browser


*/

:-pce_begin_class(listBrowserDialog,assistanceDialog %we use an assistance bar (help button), hence assistanceDialog
		 ).

variable(detailsMessage,code*, both). %the message called when double clicking the list or clicking the details button

initialise(D, Label: char_array, ListWidth: int, ListHeight: int, ShowDetails: bool, HelpId: [name]*):->
	D->+initialise(Label, HelpId),
	D->>icon(@simulate_icon), %default, overrule when needed
	B *= extendedListBrowser(width:= ListWidth, height := ListHeight),
	B->>name(list),
	B->>open_message(->>(D,onDetails)),
	D->>display(B,point(D?gapX,D?topY)),
	Close *= imgButton(close,->>(D,destroy), tt:='Close this window'),
		
	if
		ShowDetails = @on 
	then
	(	
		Details *= imgButton(details,->>(D,onDetails),
					img:=zoom_in, tt:= 'Zoom in on details'),
		D->>display(Details,point(D?gapX + ((B?pixelWidth - (Details?width + D?gapX + Close?width)) / 2), B?bottom_side + D?gapY)),
		CloseX = Details?right_side + D?gapX %functions, but thats ok
	)
	else
	(
		CloseX = D?gapX + ((B?pixelWidth - Close?width)) / 2
	),
	D->>display(Close,point(CloseX,B?bottom_side + D?gapY)),
	D->>updateSpacers, %needed in assistanceDialog	
	D->>default_button(close),
	D->>minimalSize(size(B?right_side, Close?bottom_side)).
%%

%%
onResize(D, Diff: size):->
	%when resized
	
	B = D<<-list,
	B->>right_side(B?right_side + Diff?width),
	B->>bottom_side(B?bottom_side + Diff?height),
	
	if
		Details = D<<-member(details)
	then
	(
		Details->>set(x:= Details?left_side + Diff?width / 2, y:= Details?top_side + Diff?height)
	),
	
	Close = D<<-member(close),
	Close->>set( x:= Close?left_side + Diff?width / 2, y:= Close?top_side + Diff?height).
%%

%%
list(D, List: extendedListBrowser):<-
	List = D<<-member(list). %just a shortcut
%%

%%
onDetails(D):->
	%called from open_message in the list or the details button
	%we call the detailsMessage if its there, with @arg1 is the selection and @arg2 the dialog
	%so check: message and selection
	
	unless
	(
		@nil = D<<-detailsMessage
	;
		@nil = D?list<<-selection
	)
	do
		ignore(D?detailsMessage->>forward(D?list?selection?key,D)).
%%
	
:-pce_end_class.