/*****************************************************************************************************
Garp3 1.4 
Code to find design comments etc in visualize.

PART OF Garp3. SEE COPYRIGHT NOTICE.
*******************************************************************************************************/

%loaded into module (namespace) visualize


%find_active_designobjects(+term,+state,-DesignObjects).
%finds designobjects mapped to lines in active model fragments that correspond to term

find_active_designobjects(Term,N,DesignObjects):-
	%get the active modelfragments
	
	findall(
		DesignObject,
		(
			%first all active system_structures and finally the input_system:
			(
				(
					%system structures
					index_pred_state(N, system_structures, 
	                 system_structures(Structure, _Isa, 
	                 	conditions([
	                 		system_elements(CSE),
	                 		parameters(CPA),
	                 		par_values(CPV),
	                 		par_relations(CPR),
	                 		system_structures(CSS)
	                 	]),
	                 	givens([
	                  	system_elements(GSE),
	                 		parameters(GPA),
	                 		par_values(GPV),
	                 		par_relations(GPR),
	                 		system_structures(GSS)    
	                 	])
	                 )
	                ),     
	       	Structure =.. [Name|_]
	      )
	    ;
	    	(
	    		%scenario 
	    		engine:scenario_state(smd(input_system(Name),
	    							system_elements(GSE),
           					parameters(GPA),
           					par_values(GPV),
           					par_relations(GPR),
           					system_structures(GSS),_)),
	        %no conditions in input_system
	        CSE = [],
	        CPA = [],
	        CPV = [],
	        CPR = [],
	        CSS = []
	      ),! %scenario only once
	    ),
 
      %flatten the full combination
      flatten([CSE,CPA,CPV,CPR,CSS,GSE,GPA,GPV,GPR,GSS],Elements),
			nth1(Number,Elements,Term),

			DesignObject = @app<<-findExportedElement(Name,Number)
		),
		DesignObjects
	).
	
%%relevant_comments(+term,+state,-string) / relevant_comments(+term,+state,+text_when_empty,-string)
%%will find all comments with all relevant design objects for term, and put them in a string
%%will order them. All comments for mf elements are used, and all comments from relevant defintion objects
%%the version without text_when_empty will set the string to '(no comments)' if nothing found
%%the 4 argument version needs a text (could be '')
%%
%%splits lines that are more than X chars

relevant_comments(Term,State,Comments):-
	relevant_comments(Term,State,'(no comments)',Comments).
%
relevant_comments(Term, State, Text_When_Empty, RelevantComments):-

	unless
		find_active_designobjects(Term,State,Objects) %find mf and scenario elements
	do
		Objects = [],
		
	Comments *= string,		
	%first: find all definition objects
	Definitions *= chain, 
	forall(member(O,Objects),
		(
			if 
				D = O<<-definition
			then
				Definitions->>add(D) %add: keep unique
		)
	),

	Definitions->>for_all(
		->>(Comments,ensure_nl,?(@arg1?relevantComments,split_lines,120))),
	
	forall(member(O,Objects),
		(
			Comments->>ensure_nl(?(O?relevantComments,split_lines,120))
		)
	),
	%still empty?
	if 
		0 = Comments<<-size
	then
		RelevantComments *= string(Text_When_Empty)
	else
	(
			%remove \n on the end
		RelevantComments = Comments<<-strip
	).

	