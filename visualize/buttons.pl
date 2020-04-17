
:- pce_begin_class(purple_button, button).

class_variable(elevation, elevation,
	       elevation(@nil, 1, purple)).


:- pce_end_class.



:- pce_begin_class(light_blue_button, button).

class_variable(elevation, elevation,
	       elevation(@nil, 1, colour('#00beff'))).
%	       elevation(@nil, 1, light_blue)).



:- pce_end_class.


:- pce_begin_class(blue_button, button).

class_variable(elevation, elevation,
	       elevation(@nil, 1, colour('#0076ff'))).
%	       elevation(@nil, 1, blue)).


:- pce_end_class.


:- pce_begin_class(dark_blue_button, button).

class_variable(elevation, elevation,
	       elevation(@nil, 1, colour('#0000b6'))).
%	       elevation(@nil, 1, dark_blue)).


:- pce_end_class.



:- pce_begin_class(aqua_button, button).

class_variable(elevation, elevation,
	       elevation(@nil, 1, colour('#00d1d1'))).
:- pce_end_class.




:- pce_begin_class(dark_purple_button, button).

class_variable(elevation, elevation,
	       elevation(@nil, 1, colour('#a100a1'))).
:- pce_end_class.




:- pce_begin_class(black_button, button).

class_variable(elevation, elevation,
	       elevation(@nil, 1, colour(black))).
:- pce_end_class.

