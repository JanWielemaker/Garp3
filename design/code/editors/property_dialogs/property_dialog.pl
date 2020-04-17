/*
definitie propertyDialog klasse.
Abstracte super voor de andere property dialogs

initializer set wat flags, waaronder <<-editor. Openen via --> openDialog (sluit ook na -->return)
*/

:-pce_begin_class(propertyDialog,
		  assistanceDialog, %gp3 0.2: created a new helper class (see helpers)
		  "Super for property dialogs"
		 ).

variable(element,any*,both,"The edited element. @nil means: new one").
variable(garpModel, garpModel, both, 'The model associated to this editor'). % particularly needed for sketch state graph prop editor

%%
initialise(D, Label: name, Editor: editor = frame,HelpId: helpId = [name]*):->
	"Initialise the properties dialog" ::
	D->+initialise(Label,HelpId), %gp3 0.3.13 added the help id
	D->>application(@app),
	D->>transient_for(Editor),
	D->>modal(transient),
	D->>editor(Editor),
	D->>kind(toplevel),
	D->>can_resize(@on),
	D->>scrollbars(none), %not needed (gp3 0.3.13)
	D->>icon(@build_icon).

%%

%%
associateModel(G3EF, GarpModel) :->
    send(G3EF, slot, garpModel, GarpModel).
%%

%%
modelFragment(D,MF: modelFragment):<-
	%geef het bewerkte modelfragment terug, dit is altijd het fragment van de editor

	MF=D?editor<<-fragment.
%%

%%
containerType(D, T: {'MF','SC'}):<-
	%gp3 0.3.15: gives MF or SC for the mf we edit, used in helpids
	%fails with heavy exceptions when used in a dialog that is not for editing mf-elements
	
	if
		D?modelFragment->>isInputSystem
	then
		T = 'SC'
	else
		T = 'MF'.
%%
	
%%
editor(D,E: frame):->
	D->>transient_for(E).
%%

%%
editor(D,E: frame):<-
	E = D<<-transient_for.
%%

%%
label(D,L: name):->
	%zorg ervoor dat het framelabel wordt gezet, niet het window_decorator label
	D?frame->>label(L).
%
label(D, L: name):<-
	L = D?frame<<-label.
%%

%%
openDialog(D):->

	%openen en destroyen
	%maar eerst de hoogte checken

	%hoogte checken
	D->>fit,
	Hoogte = D<<-height,
	DisplayHoogte = @display<<-height,
	if
		Hoogte > (DisplayHoogte - 50)
	then
	(
		D->>height(DisplayHoogte - 50)
	),

	D<<-confirm_centered(D?editor?area?center),
	D->>destroy.
%%


%%
cancel(D):->
	"Cancel pressed" ::

	D->>return.
%%

%%
ok(D):->
	"Ok pressed: save changes" ::
	%1: niets veranders = gewoon sluiten
	(   \+ @nil = D<<-element, %nieuw element altijd opslaan
	    D->>notChanged
	->  true
	;   (   @nil = D<<-element
	    ->  D->>saveNewElement %overschrijven
	    ;   D->>saveChangedElement  %overschrijven
	    )
	),
	D->>return.
%%

%%
notChanged(_D):->
	%moet slagen als er niets gewijzigd is (bij bestaand element)
	%overschrijven dus

	true.
%%

%%
saveNewElement(_D):->
	%moet via CR's de wijzigingen opslaan in het geval van een nieuw element
	%overschrijven dus

	true.
%%

%%
saveChangedElement(_D):->
	%moet via CR's de wijzigingen opslaan in het geval van een bestaand element
	%overschrijven dus

	true.
%%


event(D,E: event):->
	%afvangen escape voor cancel

	'\\e' = E<<-key,
	@on = D?cancel_member<<-active,
	D->>cancel.
%
event(D,E):->
	D->+event(E).
%

:-pce_end_class.
