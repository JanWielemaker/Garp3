/*
changeFeedback class

Helper van changeRequestor, zie daar
*/

:-pce_begin_class(changeFeedback,
		  object,
		  "Internal changeRequestor helper objects").
%Een feedbackboom: feedback bij een changeRequestor en de subfeedback bij subchanges

variable(object,object,get).
%Het object dat de feedback geeft (voor afbeelden)

variable(feedback,string,get).
%de feedback zelf

variable(quietWhen,chain*,get).
%Lijst van wijzigingen (in de vorm van changeRequestors met alleen change info)
%Als alle changes uit deze lijst ook werkelijk uitgevoerd gaan worden, dan hoeft deze
%feedback niet gegeven te worden. (en verdwijnt ook de subfeedback). 

variable(impossible,bool,get).
%geeft deze feedback een onmogelijkheid aan?

variable(subFeedback,chain,get).
%De subfeedback bomen

%%
initialise(CF,
	   O : object,
	   F : feedback = string,
	   I : impossible = [bool],
	   QW: [chain]
	   ) :->

	CF ->+ initialise,
	CF ->> slot( object, O ),
	CF ->> slot( feedback, F ),
	CF ->> slot( subFeedback, new( chain )),
	default(I,@off,Imp),
	CF ->> slot( impossible, Imp ),
	(   QW == @default
	->  CF ->> slot( quietWhen, @nil )
	;   ( 
	      CF ->> slot( quietWhen, new( chain )),
	      QW ->> for_all(->>(CF?quietWhen,
				 append,
				 @arg1?copyChangeInfo))
	    )
	).
%%

%%
copy(CF,
     NewCF : changeFeedback
     ):<-
     %maak een copie van het object op het goede nivo

     (   
     @nil = CF <<- quietWhen
     ->	 NewCF *= changeFeedback(CF?object,
				 CF?feedback,
				 CF?impossible)
     ;	 NewCF *= changeFeedback(CF?object,
				 CF?feedback,
				 CF?impossible,
				 CF?quietWhen)
     ),
     %de subs worden op dezelfde manier gekopieerd
     CF?subFeedback ->> for_all(->>(NewCF,
				    appendSubFeedback,
				    @arg1)).
%%

%%
recursiveImpossible(CF
		   ):->
	%slaagt wanneer deze feedback of een subfeedback impossible is (recursief dus)

	@on = CF <<- impossible
	;
	CF?subFeedback ->> find(->>(@arg1,recursiveImpossible)).
%%

%%
appendQuietWhen(CF,
		CR : changeRequestor
		):->
		%voeg een element toe aan de quietwhen lijst als die er nog
		%niet in zit
		
		(   \+ @nil = CF <<- quietWhen
		;   CF ->> slot(quietWhen, new(chain))
		),
		( 
		  CF?quietWhen ->> find(->>(@arg1,equal,CR))
		;   CF?quietWhen ->> append(CR?copyChangeInfo)
		).
%%

%%
checkQuietWhen(CF,
	       Changes: chain
	      ):->
	      %Slaag wanneer we volgens onze quietwhen lijst en de opgegeven changes
	      %niet afgebeeld hoeven te worden

	      \+ @nil = CF <<- quietWhen,
	      CF?quietWhen ->> for_all(->>(CF,checkQuietWhenSub,@arg1,Changes)).
%%

%%
checkQuietWhenSub(_CF,
		  QWChange: changeRequestor,
		  Changes: chain
		 ):->
		 %Slaagt wanneer de opgegeven QWChange in de meegestuurde lijst met changes staat
		 %sub voor checkQuietWhen
		 Changes ->> find(->>( @arg1,equal,QWChange)).
%%

%%
appendSubFeedback(CF,
		  SubF: changeFeedback
		  ):->
	%We kopieren de subfeedback als sub van de onze, als er er nog niet is
	
	NewFeedback = CF <<- appendSubFeedbackInternal(SubF), %find of append
	(   \+ @nil = SubF <<- quietWhen 
	->  SubF?quietWhen ->> for_all(->>(NewFeedback,appendQuietWhen,@arg1))
	;   true
	),
	SubF?subFeedback ->> for_all(->>(NewFeedback,appendSubFeedback,@arg1)).
%%

%%
appendSubFeedbackInternal(CF,
			  SubF: changeFeedback,
			  NewFeedback: changeFeedback
			 ):<-
	%Vind een matchende feedback of maak een nieuwe
	%we maken ons niet druk over subfeedback en quietWhen

	(   NewFeedback = CF?subFeedback <<- find(and(->>(@arg1?object,equal,SubF?object),
						      ->>(@arg1?feedback,equal,SubF?feedback),
						      ->>(@arg1?impossible,equal,SubF?impossible)))
	;   (
	     NewFeedback *= changeFeedback(SubF?object,
					  SubF?feedback,
					  SubF?impossible),
	     CF?subFeedback ->> append(NewFeedback)
	    )
	).
%%
impossibleTree(CF,
	       NewCF: changeFeedback
	      ):<-
	%slaagt als een gedeelte van de boom onmogelijk is
	%geeft dan een kopie van dat gedeelte terug (dus vanaf de top)
	%faalt anders. Gaat wel door onder impossible feedback als er nog meer impossible
	%onder zit

	CF->>recursiveImpossible, %dat moet wel
	NewCF *= changeFeedback(CF?object,CF?feedback,CF?impossible),
	CF?subFeedback->>for_some(->>(NewCF?subFeedback,
				      append,
				      @arg1?impossibleTree)).
%%

%%
feedbackTree(CF,
	     Changes: chain,
	     NewCF: changeFeedback
	    ):<-
	%slaagt als deze feedback of een gedeelte van de boom
	%moet worden afgebeeld na vergelijking van de quietwhen lijsten
	%met de opgegeven changes. Maakt dus een subboom
	%faalt wanneer niets afgebeeld hoeft te worden
	
	\+ CF->>checkQuietWhen(Changes), %deze moet in ieder geval afgebeeld worden
	NewCF *= changeFeedback(CF?object,CF?feedback,CF?impossible),
	CF?subFeedback->>for_some(->>(NewCF?subFeedback,
				      append,
				      ?(@arg1,feedbackTree,Changes))).
%%

%%
fillFeedbackList(CF,
		 List : list_browser,
		 Level : [int]
		):->
	%verzoek van de changeRequestor om in de list_browser de informatie te schrijven
	%dus dat doen we hier. Voor de recursie is het level interessant, dat geeft
	%het aantal inspringingen aan in de lijst

	default(Level,0,L),
	S *= string,
	S ->> insert_character('-',0,L),
	S->>append(CF?feedback),
	Split = S<<-split_lines(List?width),
	D *= dict_item(CF,Split,CF?object),
	(   @on = CF<<-impossible
	->  D->>style(impossible)
	;   true
	),
	List->>append(D),
	%en de recursie
	CF?subFeedback->>for_all(->>(@arg1,
				     fillFeedbackList,
				     List,
				     L + 1)).
%%
:-pce_end_class.

