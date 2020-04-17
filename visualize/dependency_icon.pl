/*****************************************************************************************************
Garp3 0.1 Based on main source of visigarp (main.pl). This file is split in parts and edited.
All visualize source based on visigarp 2.05, designed and implemented by Anders Bouwer 18/08/1999
Original copyright: (C) 2004 University of Amsterdam. All rights reserved.

Now:
PART OF Garp3. SEE COPYRIGHT NOTICE.

%all code legacy (visigarp) unless mentioned
*******************************************************************************************************/

%loaded into module (namespace) visualize
%
% Class dependency_icon
%
:- pce_begin_class(dependency_icon(my_tagged_connection, name), non_resizable_device).
%gp3 0.2 did a rewrite for this class, to display gif-images instead of drawn stuff.

variable(connection, my_tagged_connection, both, "The connection associated with this tag").

variable(relation, name, both). %gp3: we save the displayed relation
variable(elementState, name := i, get). %gp3: c, g or i: the display state

% middle
handle(w/2, h/2, in, link).
% middle
handle(w/2, h/2, out, link).
% middle
handle(w/2, h/2, in, link).

initialise(Icon, TC:my_tagged_connection, Rel:name) :->
	"Create from name"::
	send(Icon, send_super, initialise),
	send(Icon, connection, TC),

	%gp3 0.2: no drawing of circle, but loading of bitmap
	%gp3 0.3.13 made this psBitmap: a bitmap with an external .ps file for writing eps
	Icon->>relation(Rel), %save relation
	Bmp *= psBitmap(Icon?relImage), %get the right image
	Bmp->>name(bmp),
	Bmp->>psdef(Icon?relImageName),
	Icon->>display(Bmp),
    % make Icon movable
    send(Icon, recogniser, new(_MG, my_move_gesture(left))).
%%

%%
relImage(Icon, Img: image):<-
	%gp3 0.2 Get the right image
	%0.3.13: get the name seperately (because also used for psdef)

	ImageName = Icon<<-relImageName,
	unless
		get_image(elements,ImageName,Img)
	do
		get_image(elements,missing,Img).
%%

%%
relImageName(Icon,ImageName:name):<-
	%gp3 0.3.13: get the name for image and psdef

	Rel = Icon<<-relation,
	pl_dependency_icon_imagename(Rel,Name),
	ImageName = string('%s_%s',Name,Icon?elementState)<<-value.
%%

pl_dependency_icon_imagename('>',ineq_g).
pl_dependency_icon_imagename('>=',ineq_geq).
pl_dependency_icon_imagename('=',ineq_eq).
pl_dependency_icon_imagename('<=',ineq_leq).
pl_dependency_icon_imagename('<',ineq_l).

pl_dependency_icon_imagename('d>',ineq_dg).
pl_dependency_icon_imagename('d>=',ineq_dgeq).
pl_dependency_icon_imagename('d=',ineq_deq).
pl_dependency_icon_imagename('d<=',ineq_dleq).
pl_dependency_icon_imagename('d<',ineq_dl).

pl_dependency_icon_imagename('Q',corrq).
pl_dependency_icon_imagename('Q^',corrq).
pl_dependency_icon_imagename('mQ',corrq_rev).
pl_dependency_icon_imagename('mQ^',corrq_rev).
pl_dependency_icon_imagename('V',corrv).
pl_dependency_icon_imagename('V^',corrv).

pl_dependency_icon_imagename('dQ',corrdq).
pl_dependency_icon_imagename('dQ^',corrdq).
pl_dependency_icon_imagename('mdQ',corrdq_rev).
pl_dependency_icon_imagename('mdQ^',corrdq_rev).
pl_dependency_icon_imagename('dV', corrdv).
pl_dependency_icon_imagename('dV^', corrdv).

pl_dependency_icon_imagename('F', corrf).
pl_dependency_icon_imagename('F^', corrf).

pl_dependency_icon_imagename('P+',prop_plus).
pl_dependency_icon_imagename('P-',prop_min).
pl_dependency_icon_imagename('P*',prop_mult).
pl_dependency_icon_imagename('P/',prop_diw).
pl_dependency_icon_imagename('I+',inf_plus).
pl_dependency_icon_imagename('I-',inf_min).

pl_dependency_icon_imagename(Rel,Rel). %last guess, just to get info about missing

%%
elementState(Icon, State):->
	%gp3 0.2: save the new elementState (c,g, or i)
	%and redraw the bitmap

	Icon->>slot(elementState,State),
	Icon?bmp_member->>image(Icon?relImage),
	%gp3 0.3.13: also reset the psdef
	Icon?bmp_member->>psdef(Icon?relImageName).
%%

% specialize points
% so connections are redrawn after tag has been moved
%
points(Icon, _XFactor, _YFactor, _Origin) :->
	send(Icon, send_super, points),
        % put the tag on top
	send(Icon, expose),
	send(Icon, redraw),
        send(Icon, hide_connections).



% don't resize quantity nodes, only move them
%
move(_Icon, _XFactor, _YFactor, _Origin) :->
        true.

% put the tag icon on top of all connection lines
%
put_on_top(Icon) :->
	send(Icon, expose),
	send(Icon, redraw),
        send(Icon, hide_connections).


% hide_connections(Icon)
%
% hides connection lines (if there are any) behind the Icon tag
%
hide_connections(Icon) :->
        (get(Icon, connections, Connections)
        ->
          send(Connections, for_all, message(@arg1, hide, Icon))
        ;
          true
        ).

:- pce_end_class.
