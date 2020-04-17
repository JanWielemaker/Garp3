/*
icons.pl: Definitie van helper voor icons
Niet zo veel speciaal. 

Part of Garp3. Used to be homer code.
*/

:-module(icons,[make_icon/3,get_image/3,get_mono/3]).

%some global icons:
:-pce_global(@build_icon,make_build_icon).
:-pce_global(@simulate_icon,make_simulate_icon).

make_icon(Category,Name,Icon):-
	get_image(Category,Name,Image),
	new(Icon,bitmap(Image,@on)).

make_build_icon(B):-
	make_icon(windows,build,B).
%
make_simulate_icon(S):-
	make_icon(windows,simulate,S).
%%

get_image(Category,Name,Image):-
	image_path(Category,Name,Path),
	if
		file(Path)->>exists
	then
		Image *= image(Path)
	else
	(
		!,fail
	).

get_mono(Category,Name,Image):-
	get_image(Category,Name,RealImage),
	get(RealImage,monochrome,Image).

image_path(Category,Name,Path):-
	StPath *= string('%s/icons/%s/%s.gif',@garp3_dir?path,Category,Name),
	Path = StPath<<-value.
