/*
lookup_table: de lookupTable klasse
Dit object is qua gebruik vergelijkbaar met hash_table, maar is geen hash_table
Het grootste verschil is dat for_all een vast volgorde oplevert
(bij hash_table kunnen 2 for_all calls de key/values in verschillende volgorden
afleveren en da's niet handig).

Wordt gebruikt door modelFragment exportcode, voor naamgevingstabellen e.d.
Deze moeten elke keer dezelfde volgorde opleveren, anders worden namen als
liquid en liquid2 keer op keer omgedraaid.

De code is pragmatisch: alleen wat nodig is is geimplementeerd
*/

:-module(lookupTable,[]).

:- pce_begin_class(lookupTable, object).
variable(elements,chain,both).	%bevat key-value paren in een tuple

%%
initialise(L):->
	L->+initialise,
	L->>elements(new(chain)).
%%

%%
find_key(L, Test: code, V: any):<-
	%als hash_table<<-find_key, maar dan met vaste volgorde
	
	Result = L?elements<<-find(->>(Test,forward,@arg1?first,@arg1?second)),
	V = Result<<-first.
%%

%%
find_value(L, Test: code, V: any):<-
	%als hash_table<<-find_value, maar dan met vaste volgorde
	
	Result = L?elements<<-find(->>(Test,forward,@arg1?first,@arg1?second)),
	V = Result<<-second.
%%

%%
append(L, Key: any, Value: any):->
	%als hash_table->>append
	
	%bestaat hij?
	ignore(L->>delete(Key)),
	%en achter aansluiten
	L?elements->>append(tuple(Key,Value)).
%%

%%
for_all(L,Action: code, Safe: [bool]):->
	%als hash_table->>for_all, maar dan met vaste volgorde
	
	L?elements->>for_all(
		->>(Action, forward, @arg1?first, @arg1?second),
		Safe
	).
%%
	
%%
member(L, Key: any, Value: any):<-
	%als hash_table<-member
	
	Value = L<<-find_value(@arg1 == Key).
%%

%%
clear(L):->
	%als hash_table->>clear
	
	L?elements->>clear.
%%

%%
delete(L, Key: any):->
	%als hash_table->delete
	
	Element = L?elements<<-find(@arg1?first == Key),
	L?elements->>delete(Element).
%%

%%
size(L, S: int):<-
	%als hash_table<-size
	
	S = L?elements<<-size.
%%

:- pce_end_class.

