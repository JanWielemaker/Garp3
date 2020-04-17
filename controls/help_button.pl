/*
helpButton class
part of GARP3

A click_image for showing help. It works with @app->>openWeb to open its ?helpId

2000-2006 Jelmer Jellema, Spin in het Web, spininhetweb.nl
*/
:-pce_begin_class(
		   helpButton,
		  clickImage
		 ).

%gp3 0.3.13
%clickable image, different bitmap for active:@off possible
variable(helpId,name*,both). %gp3 0.3.13: set in initialise for subclasses, used in onHelp

initialise(HB,
	Name: name,
	ID: [name]*
	):-> 

	get_image(assistance,'context_help',HelpImg),
	HB->+initialise(Name,HelpImg,->>(HB,help), tooltipMsg := 'Open help page'),
	default(ID,@nil,HId),
	HB->>helpId(HId).
%%

%%
help(HB):->
	%open help if any
	unless
		@nil = HB<<-helpId
	do
		@app->>openHelp(HB?helpId).
%%

:-pce_end_class.