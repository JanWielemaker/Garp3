/*
design startup: load all design stuff
Formerly homer, so a lot of dutch comments

PART OF Garp3. SEE COPYRIGHT NOTICE.
Old homer code, except when gp3 is mentioned
 
*/

:-module(design,[]).

:-op(900,fy,not). %for some reason, the one in helpers/prolog_helpers is ignored (module problem?)

:- use_module(library(pce)).

%main, wordt onderaan wel gerund
/*garp3: the old homer code used a application object without a subclass. Called @app. 
We redeclare @app as an instance of garp3, so the definition is gone here.
*/

main:- % JL
	%create an empty model variable and assign an model to it
	% You will delete this one when the program has loaded
	%@model *= garpModel. % works
	%new(@model, garpModel). % works
	new(@model, var),
	new(EmptyModel, garpModel),
	
	% Call the first model @model1 
	gensym(model, NewModelName),
	send(EmptyModel, name_reference, NewModelName),
	get(@pce, object_from_reference, NewModelName, NewModel),

	% Assign @model to @model1
	send(@model, assign, NewModel, global),

	% Indicate the new model is new
	send(@model, modelState, new),

	% Create a copy buffer
	new(@copyBuffer, copyBufferModel).


load_garp(_Class,File):-
	%we used to use pce_autoload when not debugging
	%but I changed that: we load everything (see the app startup.pl)	
	ensure_loaded(File).
	
:- multifile
	file_search_path/2.

user:file_search_path(designcode,		design(code)).
user:file_search_path(sketchcode,		sketch(code)). % AB, feb 2006
user:file_search_path(designhelpers,	designcode(helpers)).
user:file_search_path(designdefinitions,	designcode(definitions)).
user:file_search_path(sketchdefinitions,	sketchcode(definitions)). % AB, feb 2006
user:file_search_path(designelements,	designcode(elements)).
user:file_search_path(designeditors,	designcode(editors)).
user:file_search_path(designchange,	designcode(change)).

:-ensure_loaded('code/globals.pl').
:-ensure_loaded(designhelpers(helpers)).
:-ensure_loaded(designdefinitions(definitions)).
:-ensure_loaded(sketchdefinitions(definitions)). % AB, feb 2006
:-ensure_loaded(designelements(elements)).
:-ensure_loaded(designeditors(editors)).
:-ensure_loaded(designchange(change)).

:-main.
