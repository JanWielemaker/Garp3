/*
This file contains two classes:
tracer, the implementation of the @tracer global object
traceWindow, the window used by @tracer to show the engine trace
All external calls should use the @tracer interface, not traceWindow

New tracer option: general. Used for general output, usualy to console
This one now works the same as the other options. But when no option given, we use general.
Dialog class to show the engine trace

The most powerfull call is @tracer->>tell, which routes all stdout to the tracewnd.
This is not very save: you need to call @tracer->>told to stop the collecting and
 send the output to the wnd. Just to be on the save side, enginectrl:command calls @tracer->>told after execution.

8-2005 Jelmer Jellema (Spin in het Web)

Part of Garp3 - see copyright notice

gp3 0.3
*/

:-pce_global(@tracer,new(tracer)).

:-pce_begin_class(tracer,object).
variable(wnd,traceWindow*,both).
variable(options,hash_table,both). %maps trace options to bools
variable(tellbuffer,text_buffer*,both). %see tell and told
variable(telloption,name,both). %option type used by tell

%%
initialise(T):->
	T->>options(new(hash_table)),
	%options that are on by default
	T->>option(general,@on).
%%

%%
open(T):->
	%%open the tracewindow
	
	Wnd = T<<-wnd,
	if
		Wnd = @nil
	then
	(
		T->>wnd(new(traceWindow)),
		T?wnd->>open,
		flag(tracerActive, _, true)
	)
	else
		Wnd->>expose,
		flag(tracerActive, _, true).
%%
		
%%
trace_on(T,Option: name ...):->
	%succeed like legacy garp trace_on (when user wants to trace any one of the given options)
	%but fail when tracewindow is closed??

	pl_tracer_checkoption(T,Option,_).
%%

%%
/*
pl_tracer_checkoption(+T,+Options,-FirstFound)
	helper to find the first element in Options that is on
	checks for open window
	
*/
	
/* FL mar 07: commented out:
causes always to fire general comments	
with new tracer that is not good.

pl_tracer_checkoption(T,Options,general):-
	\+ @nil = T<<-wnd,
	member(general,Options),!.
*/	
	
% FL New april 07: warning category should always be displayed in general style
pl_tracer_checkoption(T,Options,general):-
	\+ @nil = T<<-wnd,
	member(warning,Options),!.
	
	
pl_tracer_checkoption(T,Options,FirstFound):-
	\+ @nil = T<<-wnd,
	member(FirstFound,Options),
	@on = T<<-option(FirstFound),
	!.
%%

%%
trace(T,Line: char_array, Option: name ...):->
	%print the line of any one of the given options is on
	%allways succeeds
	
	if
	(
		pl_tracer_checkoption(T,Option,O)
	)
	then
		T?wnd->>trace(Line,O).
%%

%%
tell(T,Option: name ...):->
	%when any one of the given options is on
	%all standard output will be redirected to the tracer
	%this will stop when told is called
	%you cannot nest calls to tell and told, but you can do a new tell without a told
	%the @tracer will output the told lines first (executing ->>told)
	%
	%we close the stream using told, to make sure there is output.
	%fails if none of the given options is on, like trace_on
	
	%check if it should really happen, and get the first option that is @on
	%fails if we should not do anything
	
	pl_tracer_checkoption(T,Option,OnOption),
	
	unless
		@nil = T<<-tellbuffer
	do
	(
		T->>told
		%,trace
	),
	
	%do the work
	TB *= text_buffer, %we need a new buffer, because writing to the tracewindows buffer causes problems (cannot set style beforehand etc). Problem with this solution: cannot do anything about failure.
	T->>tellbuffer(TB),
	T->>telloption(OnOption),
	pce_open(TB,append,TellStream),
	tell(TellStream).
%%

%%
told(T):->
	%call to stop any ->>tell handling
	%if there is a buffer, we will output now
	
	TB = T<<-tellbuffer,
	unless
		TB = @nil
	do
	(
		told,
		T?wnd->>trace(TB?contents,T?telloption),
		T->>tellbuffer(@nil),
		free(TB)
	).

%%

%%
option(T,Option: name,Value: bool):->
	%%write the trace option
	T?options->>append(Option,Value).
%
option(T,Option: name, Value: bool):<-
	%%read the trace option
	%%gives @off if non-existant.
	%% Internal: external calls should use trace_on and tracer
	
	Value = T?options<<-member(Option)
	;
	Value = @off.
%%

%%
traceClosed(T):->
	%message from window that it closed
	%assuming it is our current, we reset our information
	
	T->>told,
	T->>wnd(@nil),
	flag(tracerActive, _, false).
%%

% NEW: FL april 2007: trace does not flush anymore, 
% enginectrl calls finish for a flush after each user command
% (and after a new state or transition (interface_garp3.pl))	
finish(T):->
	unless
	  @nil = T<<-wnd
	do
	(
	  T?wnd->>finish
	).	
	
:-pce_end_class.

:-pce_begin_class(traceWindow,frame).

%%
initialise(F):->
	F->+initialise(label:='Simulate - Engine trace', application:= @app),
	F->>icon(@simulate_icon),
	F->>attribute(noCloseOnModelEvent,@on), %so @app wont close us when a new model is loaded
	Trace *= view(size:=size(80,30)), 
	Trace->>name(trace),
	Trace->>show_label(@off),
	Trace->>editable(@off),
	Trace->>pen(1),
	%option-dependant styles below, per option
	Trace->>background(black),
	F->>append(Trace),
	
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
	Help *= helpButton(help,'Sim_EngineTrace'),
	Assistance->>display(Help),
	
	Assistance->>right(LeftOfAssistance),
	Assistance->>above(Trace), %even before displaying, to get the tiling right
	
	Options *= dialog,
	Options->>name(options),
	Options->>below(Trace),
	Options->>pen(0),
	Options->>border(size(5,5)), %instead of tile border
	Options->>gap(size(0,0)),

	%gp3 0.3.13: reset all tile borders, use dialog borders for spacing
	%F->>resetTileBorders(F?tile),
	F?tile?root->>border(0),
	
	ToggleMsg *= ->>(@tracer,option, @receiver?name, @receiver?value), %tracer keeps track of this
	
		% FL mar 07: changed colours, one to much like white, tried to match buttons.
	%new option voor general
	%0
	Options->>append(
		toggleImgButton(general, ToggleMsg, img:= '00general', inactImg:= @nil, imgCategory:=buttonbars,
				value := ?(@tracer,option,general),
				tt:= when(@arg1?value == @on, 'Hide general reasoning status information', 'Show general reasoning status information'))),
	Trace->>style(general,style(colour:= white)),
	%1
	Options->>append(
			toggleImgButton(specification, ToggleMsg, img:= '01applicable_mfs', inactImg:= @nil, imgCategory:=buttonbars,
				value := ?(@tracer,option,specification),
				tt:= when(@arg1?value == @on, 'Hide search for applicable model fragments', 'Show search for applicable model fragments'))),
	Trace->>style(specification,style(colour:=lightblue)), % aliceblue)), 

	
	%10
	Options->>append(
			toggleImgButton(assumptions, ToggleMsg, img:= '10assuming_mfs', inactImg:= @nil, imgCategory:=buttonbars,value := ?(@tracer,option,assumptions),
				tt:= when(@arg1?value == @on, 'Hide search for assumable model fragments (engine default inequality reasoning)', 'Show search for assumable model fragments (engine default inequality reasoning)')),right),
	Trace->>style(assumptions,style(colour:=lightgreen)), 
	%5
	Options->>append(
			toggleImgButton(respecification, ToggleMsg, img:= '05mf_after_transition', inactImg:= @nil, imgCategory:=buttonbars,value := ?(@tracer,option,respecification),
				tt:= when(@arg1?value == @on, 'Hide search for model fragments still applicable after transition', 'Show search for model fragments still applicable after transition')),right),
	Trace->>style(respecification,style(colour:=mediumturquoise)), %lightblue)),
	%2
	Options->>append(
			toggleImgButton(add_relation, ToggleMsg, img:= '02adding_checking', inactImg:= @nil, imgCategory:=buttonbars,value := ?(@tracer,option,add_relation),
				tt:= when(@arg1?value == @on, 'Hide added dependencies and check on conditional inequalities', 'Show added dependencies and check on conditional inequalities')),right),
	Trace->>style(add_relation,style(colour:=bisque)), 
	%3
	Options->>append(
			toggleImgButton(inequality, ToggleMsg, img:= '03inequality_reasoning', inactImg:= @nil, imgCategory:=buttonbars,value := ?(@tracer,option,inequality),
				tt:= when(@arg1?value == @on, 'Hide inequality reasoning details', 'Show inequality reasoning details')),right),
	Trace->>style(inequality,style(colour:=coral3)),
	%9
	Options->>append(
			toggleImgButton(derivable, ToggleMsg, img:= '09derivable_relations', inactImg:= @nil, imgCategory:=buttonbars,value := ?(@tracer,option,derivable),
				tt:= when(@arg1?value == @on, 'Hide inequality reasoning details: derivable relations', 'Show inequality reasoning details: derivable relations')),right),
	Trace->>style(derivable,style(colour:=indianred)),
	%4
	Options->>append(
			toggleImgButton(resolve, ToggleMsg, img:= '04influence_resolution', inactImg:= @nil, imgCategory:=buttonbars,value := ?(@tracer,option,resolve),
				tt:= when(@arg1?value == @on, 'Hide influence resolution (using influences and proportionalities)', 'Show influence resolution (using influences and proportionalities)')),right),
	Trace->>style(resolve,style(colour:= thistle3)), %darkgoldenrod)),
	%6
	Options->>append(
			toggleImgButton(termination, ToggleMsg, img:= '06terminations', inactImg:= @nil, imgCategory:=buttonbars,value := ?(@tracer,option,termination),
				tt:= when(@arg1?value == @on, 'Hide search for possible terminations', 'Show search for possible terminations')),right),
	Trace->>style(termination,style(colour:=gold)),
	%7
	Options->>append(
			toggleImgButton(ordering, ToggleMsg, img:= '07ordering', inactImg:= @nil, imgCategory:=buttonbars,value := ?(@tracer,option,ordering),
				tt:= when(@arg1?value == @on, 'Hide ordering and removal of possible terminations', 'Show ordering and removal of possible terminations')),right),
	Trace->>style(ordering,style(colour:= darkorange)), % powderblue)),
	%8
	Options->>append(
			toggleImgButton(transition, ToggleMsg, img:= '08successors', inactImg:= @nil, imgCategory:=buttonbars,value := ?(@tracer,option,transition),
				tt:= when(@arg1?value == @on, 'Hide search for successor states (using ordered terminations)', 'Show search for successor states (using ordered terminations)')),right),
	Trace->>style(transition,style(colour:= chocolate3)), %greenyellow)),
	
	
	Options->>append(
			graphical(width:=5, height:=0),right),
	Options->>append(
			imgButton(select_all, ->>(F,setAll,@on), inactImg:= @nil, imgCategory:=buttonbars,
				tt:= 'Select all trace options'),right),
	Options->>append(
			imgButton(select_none, ->>(F,setAll,@off), inactImg:= @nil, imgCategory:=buttonbars,
				tt:= 'Deselect all trace options'),right),
	Options->>append(
			imgButton(reverse, ->>(F,reverseSelection), inactImg:= @nil, imgCategory:=buttonbars,
				tt:= 'Reverse the selection of trace options'),right),
	Options->>append(
			graphical(width:=15, height:=0),right),
	Options->>append(
			imgButton(save, ->>(F,saveTrace), img:= save, inactImg:= @nil,
				imgCategory:=buttonbars,
				tt:= 'Save trace to file'),right),
	Options->>append(
			graphical(width:=5, height:=0),right),
	Options->>append(
			imgButton(clear_all, ->>(F?trace_member,clear), img:= clear, inactImg:= @nil,
				imgCategory:=buttonbars,
				tt:= 'Clear all contents'),right),
	Options->>append(
			graphical(width:=5, height:=0),right),
	Options->>append(
			imgButton(close, ->>(F,destroy), img:= close, inactImg:= @nil,
				imgCategory:=buttonbars,
				tt:= 'Close this window'),right),
	if
		@on = @tracer<<-option(general)
	then
	(
		current_prolog_flag(arch,Arch),
		%F->>trace(string('%s %s trace %s\nEngine running on %s\nUsing libraries from %s',@app?name,@app?version,new(date),Arch,@app?engineArchPath),general)
		F->>trace(string('%s %s trace %s\nEngine running on %s',@app?name,@app?version,new(date),Arch),general)
	).
%%
destroy(F):->
	%send message to tracer that we have gone away
	F->+destroy,
	@tracer->>traceClosed.
%%

%%
setAll(F,Value: bool):->
	%set value of all options and buttons
	
	F?options_member?members->>for_all(
		if(
			->>(@arg1,instance_of,toggleImgButton),
			and(
				->>(@arg1,value,Value),
				->>(@tracer,option,@arg1?name,Value)
			)
		)).
	
%%

%%
reverseSelection(F):->
	%reverse value of all options and buttons
	F?options_member?members->>for_all(
		if(
			->>(@arg1,instance_of,toggleImgButton),
			and(
				->>(@arg1,value,@arg1?value?negate),
				->>(@tracer,option,@arg1?name,@arg1?value?negate)
			)
		)).
%%

%%
saveTrace(F):->
	%save the contents to disk
	if
		FileName = @garp3_finder<<-file(F,'Save trace to file', save,tuple('Text file','txt'))
	then
		F?trace_member->>save(FileName).
%%

%%
trace(F,Line: char_array, Style: name):->
	%called by @tracer to display Line using Style
	%Line could be a multiline, we dont mind

	TM = F<<-trace_member,
	Text = TM<<-text_buffer,
	FR *= fragment(Text,Text?size,0,style:=Style),
	LineString *= string('%s',Line), %make sure its a string
	LineString->>ensure_nl,
	FR->>string(LineString),
	TM->>point_to_bottom_of_file.
	% TM->>flush.
	%TM->>append(dict_item(TM?length,Line,style:=Style)),
	%TM->>scroll_to(TM?length).
%%	

% NEW: FL april 2007: trace does not flush anymore, 
% enginectrl calls finish for a flush after each user command
% 
finish(F):->
	TM = F<<-trace_member,
	TM->>flush.
	
:-pce_end_class.
