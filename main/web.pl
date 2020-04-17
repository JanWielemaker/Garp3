/*
Garp3 0.2

Weblocations (facts) + some helper code
Part of Garp3, see copyright notice.

*/

:-module(web,[]).

location(help,'http://hcs.science.uva.nl/QRM/help/redirect.php').
location(garp3,'http://hcs.science.uva.nl/QRM/').
location(sihw,'http://spininhetweb.nl').

%open a webpage.
open(Location):-
	location(Location,URL),
	ignore(www_open_url(URL)).
%
open(Location, Context):-
	location(Location,Path),
	Total *= string('%s?%s',Path,Context),
	URL = Total<<-value,
	ignore(www_open_url(URL)).
	
