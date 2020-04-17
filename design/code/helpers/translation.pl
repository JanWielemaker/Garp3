/*
gp3 1.4.0: Translation
Contains classes for controling translatable data
14-2-2007 Spin in het Web - Jelmer Jellema
*/

%TODO: toevoegen van translator aan model, en spelen met conversiecode van name en string, zodat het omzetten eigenlijk vanzelf gaat

/*
Class translator
A translator is an object that coordinates all translatables that have
registered with it.
The languages chain contains all languages defined (just strings with unique names). Translatables will connect values to these languages. 
It also manages the different languages defined in the model
*/

:- load_files([library(iso_639)], [silent(true)]).

:-pce_begin_class(
		  translator,
		  object
		 ).
		 
variable(languages,chain,both). %list of languages defined in the model (just strings)
variable(currentLanguage,string,both).
variable(iso639Languages,chain:=new(chain),both).
%%

%%
initialise(T):->
	T->+initialise,

	% create a list of all the languages
	forall(
	    iso_639:l2(_, Language),

	    (
		% For synonym names for the same language, use only the first synonym
		concat_atom(LanguageList, ', ', Language) ->
		nth1(1, LanguageList, SubLanguage),
		% Replace the - character, since it is illegal in Garp3
		( string_replace(SubLanguage, '-', ' ', ValidSubLanguage) ; ValidSubLanguage = SubLanguage ),
		send(T?iso639Languages, append, new(string(ValidSubLanguage)))
	    ;
		% Replace the - character, since it is illegal in Garp3
		( string_replace(Language, '-', ' ', ValidLanguage) ; ValidLanguage = Language ),
		send(T?iso639Languages, append, new(string(ValidLanguage)))
	    )
	),
	send(T?iso639Languages, sort),

	% Make the default English/American
	get(T?iso639Languages, find, message(@prolog, ==, @arg1?value, 'English'), English),
	send(T, currentLanguage, English?copy),

	T->>languages(new(chain)),
	T?languages->>append(T?currentLanguage).
%%

%
unlink(T):->
	%we explicitely free the languages
	T?languages->>for_all(->>(@arg1,free)).
%

%
getLanguageKey(_T, Language, Key) :<-
    get(Language, value, LanguageValue),
    iso_639:l2(_, TargetLanguage),
    % Remove the '-' character from the languages so they can be matched to languages in the model
    ( string_replace(TargetLanguage, '-', ' ', TargetLanguageR) ; TargetLanguageR = TargetLanguage ),
    sub_string(TargetLanguageR, _Start, _Length, _After, LanguageValue),
    iso_639:l2(Key, TargetLanguage).
%
getLanguageFromKey(T, Key, Language) :<-
    iso_639:l2(Key, Iso639Language),
    ( string_replace(Iso639Language, '-', ' ', Iso639LanguageR) ; Iso639LanguageR = Iso639Language),
    get(T?iso639Languages, map, @arg1?value, IsoLanguagesC),
    chain_list(IsoLanguagesC, IsoLanguages),
    member(Language, IsoLanguages),
    sub_string(Iso639LanguageR, _Start, _Length, _After, Language).
%

%%
allLanguages(T,Languages: chain):<-
	%return all language-names, sorted
	
	Languages *= chain,
	T?languages->>for_all(
		->>(Languages,append,@arg1?copy)),
	Languages->>sort.
%%

%%
alliso639Languages(T, Languages: chain):<-
    % return all the language names defined is iso 639

    new(Languages, chain),
    send(T?iso639Languages, for_all, message(Languages, append, @arg1?copy)).
%%
	
	
%%
setCurrentLanguage(T, Language: char_array):->
	
	%change language: set CurrentLanguage to the right string
	
	T->>currentLanguage(?(T?languages,find,->>(@arg1,equal,Language,@on))).
%%

%%
addLanguage(T, Language: char_array):->
	%add a new language
	%existing translatables are informed now. They will copy their value from the current language
	\+ T?languages<<-find(->>(@arg1, equal, Language, @on)), 
	\+ Language->>equal(''),
	L *= string('%s',Language),
	T?languages->>append(L),
	T?translatables->>for_all(->>(@arg1,onAddLanguage,L)). 
%%
	
%%
deleteLanguage(T, Language: char_array):->
	%remove a language, informing all translatables
	%gp3 1.4: because the way CR now work, we have to fix this
	%when a deleteLanguage Cr is routed for the current language, the model will add a setcurrentLanguage CR to change the current language. Only, because of strange routing behaviour (for legacy reasons..) in CR, this subchange will only be routed after the deleteLanguage call (should be differt, but this is an old issue about constraints checking). So we cannot fail here when the current language is deleted
	
	if
		T?currentLanguage->>equal(Language,@on)
	then
	(
		%find some other one, fail if not available
		T->>currentLanguage(?(T?languages,find,not(->>(@arg1, equal, Language, @on))))
	),
	L = T?languages<<-find(->>(@arg1, equal, Language, @on)), 
	T?translatables->>for_some(->>(@arg1,onDeleteLanguage,L)), %inform all
	T?languages->>delete(L).
%%

%%
changeLanguageName(T, OldName: char_array, NewName: char_array):->
	%change the name, not the object.
	
	L = T?languages<<-find(->>(@arg1, equal, OldName, @on)), 
	L->>value(NewName).
%%

%%
getTranslatable(T, MB: [{unique,empty}], TR: translatable):<-
	%get a new translatable,registered with this Translator
	
	TR *= translatable(T, MB).
%%

	
%%
translatables(T, Translatables: chain):<-
	%all translatables
	
	Translatables = T<<-all_named_hypered(translatable).
%%
	
:-pce_end_class.

/*
Class translatable
A translatable is used instead of a char_array subclass for textual content
that can be translated. It is itself not a char_array, but it contains
references to its values in different languages.
The translatable is connected to a Translator object (in the model), which
coordinates adding and removal of languages. When created (preferably by
translator<<-getTranslatable), the translatable is registered 
Getting and settings of values is allways done for translator?currentLanguage.
This way this object, which defines some common char_array methods, is transparant
to the interface. Well, sort of anyway.

During development, we introduced the concept of a default value: when a translatable has no value for a language, it would return some default, knowing this was only the default. This gave nice display in the language editor.
This concept was dropped on the sunny aftertoon of 23-4-2007, because of the wish that existing translatable would get a real value when
a language is added.

So now: 
When a language is added, onAddLanguage is called for all translatables, copying the value in the current language to the new one
When a translatable is created, it will have an empty value or a unique value in all existing languages, depending on the setting

Special case: a translatable can be nontranslatable: a nontranslatable cannot be translatable.... 
This is for the case when a datum is normally translatable, but not in a special case.
When nonTranslatable = @on, all values will change when the current value changes
*/


:-pce_begin_class(
		  translatable,
		  object
		 ).
		 
%values are now a hash_table, mapping a languageobject (byref!) to a value, very easy.
%A value is destroyed when the
%translatable is, or when the language it belongs to is destroyed (see deleteLanguage)

variable(values,hash_table,both).
variable(nontranslatable, bool, both).

/* Changing
Value can be changed by just setting ->value.
But the languageEditor uses change requestors  for further checks.
*/


%%
initialise(TR, T: translator, DefaultValue: [{unique,empty}]):->
	TR->+initialise,
	TR->>nontranslatable(@off),
	T->>hyper(TR,translatable,translator), %connect
	V *= string(''),
	unless
		DefaultValue == empty  %default is unique
	do
		V->>format('Please_translate_this_%u',TR?object_reference), %lets just **hope** this is unique
	TR->>values(new(hash_table)),
	T?languages->>for_all(
		->>(TR?values,append,@arg1,V?copy)
	).
%%

%%
copy(TR,NTR):<-
	%return an exact copy of this translatable
	
	NTR *= translatable(TR?translator),
	%values: the new translatable allready has string objects for all languages
	TR?values->>for_all(
		->>(?(NTR?values,member,@arg1),value,@arg2)
	),
	%nontranslatable?
	NTR->>nontranslatable(TR?nontranslatable).
%%

%%
translator(TR,T: translator):<-
	%get the translator for this translatable
	
	T = TR<<-hypered(translator).
%%

%%
setNonTranslatable(TR):->
	%when called, the translatable will no longer be translatable, see comments above
	
	if
		@off = TR<<-nontranslatable
	then
	(
		%copy all values from the current
		TR->>nontranslatable(@on),
		TR->>value(TR?value) %->>value will copy this value to all existing values...
	).
%
setTranslatable(TR):->
	%when called a translatable that used to be nontranslatable, will be reset to translatable

	%just set the slot	
	TR->>nontranslatable(@off).
%

isNonTranslatable(TR):->
	%succeeds if non translatable
	
	@on = TR<<-nontranslatable.
%%

%%
current(TR, Value: string):<-
	%normal case
	%get the value object (string) for this translatable, given the current language
	%value of untranslatable only has meaning when setting a value (see ->value)

	Value = TR?values<<-member(TR?translator?currentLanguage).
%%

onAddLanguage(TR, Language: string):->
	%called by translator when a language is added. We copy the value from our currentvalue

	Value *= string('%s',TR?value),
	TR?values->>append(Language,Value).
%%


%%
onDeleteLanguage(TR, Language: string):->
	%called by translator when a language is destroyed. We remove the value that belongs to that language
	
	ignore(TR?values->>delete(Language)).
%%

	
%%%%%% READING EN WRITING TO THE CURRENT VALUE %%%%%%%
value(TR, V: name):<-
	%see string<<-value
	V = TR?current<<-value. %allways just the current value
%
value(TR,V: char_array):->
	if
		@off = TR<<-nontranslatable
	then
		TR?current->>value(V) %just set the value
	else
		TR?values->>for_all(->>(@arg2,value,V)).
%%

%%%%%%%%%% SPECIALS %%%%%%%%%%%%%%%%%%%
	
%valueForLanguage:
%get the <-value but as if another language was current
%must be the real language object
valueForLanguage(TR, Lang: string, V: name):<-
	V = ?(TR?values,member,Lang)<<-value.
%%

valueForLang(TR, Language, Value:name) :<-
    get(TR?values?keys, find, message(@prolog, ==, @arg1?value, Language), LanguageString),
    get(TR?values, member, LanguageString, ValueString),
    get(ValueString, value, Value).

:-pce_end_class.

