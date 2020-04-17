/*
Definitie subclass van text voor urls
*/


:-pce_begin_class(
		  urlText,
		  text,
		  "A text that displays a link"
		 ).

variable(url,char_array,both,"URL To Open").

%%
initialise(UT,
	Label: label = char_array,
	URL: url = char_array,
	Format: format = [{left,center,right}],
	Font: font = [font]):->

	UT->+initialise(Label,Format,Font),
	UT->>underline(@on),
	UT->>url(URL),
	UT->>recogniser(click_gesture(left, message:= ->>(@receiver,openUrl),
		preview:= ->>(@receiver,inverted,@on), 
		cancel:= ->>(@receiver,inverted,@off)
	)).
%%

%%
openUrl(UT):->
	%geklikt: open de url
	
	UT->>inverted(@off),
	Url = UT<<-url,
	ignore(www_open_url(Url)).
:-pce_end_class.
		  

