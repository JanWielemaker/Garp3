/*
Garp3 0.1
Extensions helping object names to create exportable names
Some of this code was part of Homer. There the comments are in dutch.

Part of Garp3. See copyright notice

*/
:-module(design_export_naming,[]).

:-pce_extend_class(char_array).

/*makeGarp: geeft een char_array terug die gestripped is en waar
  dubbele spaties zijn verwijderd en underscores in spaties
  zijn omgezet. De rest wordt gecapitalised,
  dus beginnend met een hoofdletter en de rest klein.*/
  
makeGarp(C,
	 Garped: name):<-
	"Return a Garpified char_array" ::

	NewC *= string('%s',C),
	NewC->>translate('_',' '),
	NewC->>strip,
	
	%en nu alle dubbele spaties eruit
	%hierdoor dus: max 1 spaties tussen woorden, geen ws voor en achter
	%let op: underscores enzo mogen niet hoor! Dat regelt garpModel->validName
	pl_stripDoubleSpaces(NewC),
	Garped *= name(NewC?capitalise).
%
pl_stripDoubleSpaces(C):-
	Re *= regex('\s\s+'),	%2 of meer whitespace
	pl_stripDoubleSpaces(C,Re),
	Re->>done.
%
pl_stripDoubleSpaces(C,Re):-
	Re->>search(C),!, %bevat nog te veel spaties
	C->>delete(Re?register_start,Re?register_size - 1),
	pl_stripDoubleSpaces(C,Re).
%
pl_stripDoubleSpaces(_C,_Re). %klaar
	
%%
/*
exportName: return a string ready for exporting to the engine.
*/

exportName(C,
	ExportName: string):<-
	"Return a string ready for exporting to Garp" ::
	
	/*
	ook gebruikt bij vergelijken
	De naam is al via @model->validName gecheckt en via makeGarp gestipped e.d.
	Hier maken we van elke spatie een underscore en zetten de eerste letter
	in hoofdletter.
	*/
	%gp3 0.2: we change brackets to underscores as well
	ExportName *= string('%s',C?makeGarp?downcase), %ook dubbele spaties weg
	ExportName->>translate(' ','_'),
	ExportName->>translate('(','_'),
	ExportName->>translate(')','_').
%
equalExportName(C, Other: char_array):->
	"Succeed if this string would give the same exported name as the argument" ::
	
	C?exportName->>equal(Other?exportName).
%
makePrologName(C,
	NewC: string):<-
	"Return a string ready to be used as a Prolog var" ::
	
	NewC = C<<-exportName,
	%en nou, zucht, de eerste letter weer groot
	NewC->>character(0,?(NewC?upcase,character,0)).
%
makeRemarks(C,
	Tab: int,
	NewC: string):<-
	"Return the string as prolog remarks" ::
	
	%en houd rekening met het aantal tabs
	Veilig *= string('%s',C),
	Veilig->>translate('*','X'), %sterretjes weg om te zorgen dat het commentaar blijft
	Veilig->>prepend('/* '),
	Veilig->>append(' */'),
	NewC = Veilig<<-tab(Tab).
%
%%

:-pce_end_class.
