/*
secondary_id: definitie van de secondaryID klasse.
Deze klasse is er alleen om waar nodig een unieke secundaire referentie aan objecten toe te voegen.
Hierdoor kan dus van 2 verschillende instanties (met verschillende primaire pce referenties) worden aangegeven
dat ze over dezelfde informatie gaan die niet expliciet gerefereerd kan of hoeft te worden.
Andersom kan bij twee keer hetzelfde pce-object een verschillende key worden gegenereerd, wat handig is in bijvoorbeeld list_browsers

Klasse kan op dit moment weinig, wellicht is het ooit handig om dit uit te breiden.
*/

:-module(secondaryID,[]).

:- pce_begin_class(secondaryID, object).
%pce maakt de referentie
:- pce_end_class.

