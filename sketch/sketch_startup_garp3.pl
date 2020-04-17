/*
sketch startup: load all sketch stuff
Based on copy from design startup
Formerly homer, so a lot of dutch comments

PART OF Garp3. SEE COPYRIGHT NOTICE.
Old homer code, except when gp3 is mentioned

*/

:-module(sketch,[]).

:-op(900,fy,not). %for some reason, the one in helpers/prolog_helpers is ignored (module problem?)

:- use_module(library(pce)).

%main, wordt onderaan wel gerund
/*garp3: the old homer code used a application object without a subclass. Called @app. 
We redeclare @app as an instance of garp3, so the definition is gone here.
*/

main:-
	%create an empty model
	% @model *= garpModel.
        true. % do nothing for now...
        % Should the sketches be part of a model, or separate?

load_garp(_Class,File):-
	%we used to use pce_autoload when not debugging
	%but I changed that: we load everything (see the app startup.pl)
	
	ensure_loaded(File).
	
:- multifile
	file_search_path/2.

user:file_search_path(sketchcode,		sketch(code)).
user:file_search_path(sketchhelpers,	sketchcode(helpers)).
user:file_search_path(sketchdefinitions,	sketchcode(definitions)).
user:file_search_path(sketchelements,	sketchcode(elements)).
user:file_search_path(sketcheditors,	sketchcode(editors)).
user:file_search_path(sketchchange,	sketchcode(change)).

%:-ensure_loaded(sketchdefinitions(definitions)). % already done in design load, AB, feb 2006
:-ensure_loaded(sketchelements(elements)).
:-ensure_loaded(sketcheditors(editors)).
%:-ensure_loaded(sketchchange(change)).
%:-ensure_loaded(sketchhelpers(helpers)).

:-main.
