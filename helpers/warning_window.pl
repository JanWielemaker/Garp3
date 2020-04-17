:-pce_begin_class(
		  warningWindow,
		  object,
		  "A generic warning window"
		 ).


open(WW,
	     Label: name,
	     Prompt: name*, %gp3 0.3.11 changed this to also nil possible
	     Feedback: string,
	     CanApply: bool,
	     HelpId: name, %gp3 0.3.13 added this
	     Result: bool
	    ):<-
	"Internal: show overview dialog"::
	%toon de dialoog en geef @on terug bij ok
	%en @off bij cancel. CanApply = @off betekent dat de apply button disabeled is


	D *= assistanceDialog(Label,HelpId), %gp3 0.3.13 changed this to extended with help
	D->>can_resize(@off),
	D->>application(@app),
	D->>modal(application),
	
	%layout
	GapX = D?gap<<-width,
	GapY = D?gap<<-height,
	
	if
		Prompt = @nil
	then
		ListTop = D?topY
	else
	(	
		LabelText *= label(labeltext, 'Labeltext'),
		LabelText->>selection(Prompt),
		D->>display(LabelText,point(GapX,D?topY)),
		ListTop = LabelText?bottom_side + GapY
	),
	List *=	extendedListBrowser(width:= 80, height:= 10),
	D->>display(List,point(GapX, ListTop)),
	List->>style(impossible,style(colour:='red')), %onmogelijkheden rood

	send(WW, fillFeedbackList, List, Feedback),

	Apply *= imgButton(apply,->>(D,return,@on),img:=save,tt:='Apply changes'),
	Apply->>active(CanApply),
	D->>display(Apply,point(GapX, List?bottom_side + GapY)),

	Cancel *= imgButton(cancel,->>(D,return,@off), img:=undo,tt:='Cancel changes'),
	D->>display(Cancel,point(List?right_side - GapY - Cancel?width, Apply?top_side)),
	D->>updateSpacers, %gp3 0.3.13
	Cancel->>default_button(@on),
	Cancel->>keyboard_focus(@on),
	%en tonen
	@display->>bell,
	D->>assign_accelerators,
	D->>input_focus(@on), %iets nodig om hem de focus te geven
	
	Result = D<<-confirm_centered,
	
	D->>destroy.
%%

%%
fillFeedbackList(_WW,
		 List : list_browser,
		 Feedback : string
		):->
	S *= string,
	send(S, append, Feedback),
	Split = S<<-split_lines(List?width),
	List->>append(Split).
%%




:-pce_end_class.

