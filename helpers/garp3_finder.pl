/*
Garp3 version of find_file library. Not very many changes, search for %gp3
Problem was: in unix version confirm overwrite is default @on, while windows dialog does not ask for confirmation. We want it off, because we have our own one.



  $Id: find_file.pl,v 1.35 2005/09/12 07:29:57 jan Exp $

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/


%:- module(pce_finder, []). %gp3 0.3.12: changed module:
:- module(garp3_finder, []).
:- use_module(library(pce)).

%gp3 0.3: skipped comments


%	register_file_dialog
%	
%	If the display doesnt implement <-win_file_name we register the
%	autoload class find_file_dialog to do the job ourselves.

register_file_dialog :-
	(   send(@display, has_get_method, win_file_name)
	->  true
	;   pce_autoload(find_file_dialog, library(find_file_dialog)) %gp3 changed this
		%pce_autoload(find_file_dialog, find_file_dialog)
	).

:- register_file_dialog.


		 /*******************************
		 *	     CLASS FINDER	*
		 *******************************/

:- pce_begin_class(garp3_finder, object,
		   "Find files on behalf of applications").

variable(directory,	directory,	both, "Current directory").
variable(label,		[char_array],	both, "Used label").

initialise(F) :->
	send_super(F, initialise),
	send(F, slot, directory, directory('.')).


file(F, Frame: frame, Label: char_array, Exists:exists=[bool|{open,save}], Ext0:extension='[name|chain|tuple]',
     Dir:directory=[directory], Default:default=[name], Confirm_overwrite:confirm_overwrite=[bool], File:name) :<-
 	"Get [existing] file with [extension]"::
 
 	%gp3 0.3.12: added Frame and Label	
 	%gp3 0.3.12: if mode is save, and confirm_overwrite = @on or default, we use our own overwrite
 	%confirmer (for now)
 	

 	F->>label(Label),
 	default(Confirm_overwrite,@on,CO),
 	
 	%gp3 0.4.7: dirty patch to make tooltip disappear, but make sure it appears again 
 	%after we are done (in whatever way)
 	TTA = @tooltip_window<<-active,
 	@tooltip_window->>active(@off),
 	
 	if
		catch(get_file(F, Exists, Ext0, Dir, Default, GivenFile),_,fail)
	then
		OK = true
	else
		OK = false,
	
	%first reset the tooltip
	@tooltip_window->>active(TTA),
	%then fail when needed
	OK == true,
 	
	%sometimes, when user cancels, the call succeeds with GivenFile is a file without a directory
	%gp3 0.3.12 makes it fail in that case:

	\+ '' = file(GivenFile)<<-directory_name,

	Continue = F<<-gp3_checkOverwrite(Frame,CO,Exists,GivenFile), %can we continue?
	if
		Continue = @on
	then
		File = GivenFile
	else
		File = F<<-file(Frame,Label,Exists,Ext0,Dir,Default,Confirm_overwrite). %again
%
gp3_checkOverwrite(_F,Frame: frame, Confirm_overwrite: bool, Exists : [bool|{open,save}],GivenFile: name, Result: bool):<-
	%gp3 0.3.12: check if we should show our own confirm overwrite dialog
	%gives @on if the finder can continue returning the given filename
	%(no dialog, or overwrite ok), or @off if it should ask again
	
	
	mode(Exists,Mode),
	if
	(
		Confirm_overwrite = @on,
		Mode = save,
		file(GivenFile)->>exists
	)
	then
	(
		%show the confirmer
		C *= confirmOverwriteDlg(Frame,'The filename you entered exists. \nAre you sure you want to overwrite it?'), %in main/helper_dialogs
		Result = C<<-confirm_centered,
		C->>destroy
	)
	else
		Result = @on. %no problem
%%

get_file(F, Exists, Ext, Dir, Default, File) :-
	send(@display, has_get_method, win_file_name), !,
	mode(Exists, Mode),
	win_filter(Ext, Filters),
	(   Dir == @default
	->  get(F, directory, DefDir)
	;   DefDir = Dir
	),
	get(F, label, Label),
	get(@display, win_file_name, Mode, Filters,
	    Label, Default, DefDir, File),
	file_directory_name(File, NewDir),
	send(F, slot, directory, NewDir).
	
get_file(F, Exists, Ext, Dir, Default, File) :-
	mode(Exists, Mode),
	get(F, label, Label),
	new(D, find_file_dialog(Mode, Label)),
	send(D, filter, Ext),
	(   Default \== @default
	->  send(D, default_file, Default)
	;   true
	),
	(   Dir == @default
	->  send(D, directory, F?directory)
	;   send(D, directory, Dir)
	),
	send(D, make_transient),
	send(D, message, message(D, return, @arg1)),
	%gp3: no confirm overwrite, thats all we wanted
	send(D,confirm_overwrite,@off),
	
	get(D, confirm, File),
	send(F, slot, directory, D?directory),
	send(D, destroy).

%	mode(+Exists, -Mode)
%	
%	Map old boolean mode to new named mode.

mode(@on,     open).
mode(@off,    save).
mode(default, save).
mode(save,    save).
mode(open,    open).

%	win_filter(+Spec, -Filter)
%	
%	Map abstract filter to a filter for 'display<-win_file_name'.
%	See find_file_dialog for details.

win_filter(@default, @default) :- !.
win_filter(Atom, chain(Tuple)) :-
	atom(Atom), !,
	file_filter(Atom, Tuple).
win_filter(Tuple, chain(Filter)) :-
	send(Tuple, instance_of, tuple), !,
	file_filter(Tuple, Filter).
win_filter(Chain, Filter) :-
	get(Chain, map, ?(@prolog, file_filter, @arg1), Filter).

file_filter(Tuple, tuple(Label, Pattern)) :-
	send(Tuple, instance_of, tuple), !,
	get(Tuple, first, Label),
	get(Tuple, second, ExtList),
	to_pattern(ExtList, Pattern).
file_filter(*, tuple(all_files, *)) :- !.
file_filter(Ext0, Filter) :-
	atom_concat('.', Ext, Ext0), !,
	file_filter(Ext, Filter).
file_filter(Ext, tuple(Name, Pattern)) :-
	file_type(Ext, Name), !,
	atom_concat('*.', Ext, Pattern).
file_filter(Ext, Pattern) :-
	atom_concat('*.', Ext, Pattern).

to_pattern(*, '*.*') :- !.
to_pattern(Ext, Pattern) :-
	atom(Ext), !,
	(   sub_atom(Ext, 0, _, _, '.')
	->  atom_concat('*', Ext, Pattern)
	;   atom_concat('*.', Ext, Pattern)
	).
to_pattern(Chain, Pattern) :-
	chain_list(Chain, List),
	maplist(to_pattern, List, Patterns),
	concat_atom(Patterns, ';', Pattern).
	
%	Allow the user to add rules to this predicate, showing proper
%	names to the user rather than patterns.  The collection here
%	is rather arbitrary ...  Maybe we should read the registery
%	for defined filetypes ...

:- multifile
	file_type/2.

file_type(pl,	'Prolog files').
file_type(c,	'C source files').
file_type(cc,	'C++ source files').
file_type(cpp,	'C++ source files').
file_type(cxx,	'C++ source files').
file_type(h,	'C header files').
file_type(pd,	'PceDraw files').
file_type(ps,	'PostScript files').
file_type(eps,	'Encapsulated PostScript files').
file_type(pdf,	'Portable Document Format files').
file_type(txt,	'Text files').
file_type(jpeg,	'JPEG images').
file_type(jpg,	'JPEG images').
file_type(gif,	'GIF images').
file_type(xpm,	'XPM images').
file_type(ico,	'Icon files').
file_type(cur,	'Cursor files').
file_type(html,	'HTML documents').
file_type(htm,	'HTML documents').
file_type(xml,	'XML documents').
file_type(sgml,	'SGML documents').
file_type(rdf,	'RDF files').
file_type(rdfs,	'RDF schema files').
file_type(tex,	'TeX or LaTeX files').
file_type(*,	'All files').

:- pce_end_class.
