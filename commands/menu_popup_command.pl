/*
Definitie commandhandler menuPopupCommand

Vervangt: popup (in een menu_bar).
*NB:
Dit is een subclass van popupCommand. Deze gaat ervan uit dat ie in een menu_bar zit en
gebruikt daarom idle time om het label enzo bij te werken. Daardoor dus een infotype meer
en een check meer. Zie verder bij popupCommand.
*/

:-pce_begin_class(menuPopupCommand(name,commandname),
		  popupCommand,
		  "menu-bar popup that uses a command object for messages").

%%
initialise(MPC,
	   C: commandname = name,
	   N: name = [name]
	  ):->
	"Create the menuPopupCommand" ::

	MPC->+initialise(C,N),
	%registeren voor idle time
	MPC->>idle_method(onIdle,1).
%%

%%
onIdle(MPC) :->
	"Handle idle time by updating visual lay-out" ::
	%we zetten het label en we checken of we enabled moeten zijn
	%we vinden het frame op een beetje vreemde manier:
	%Eerste clause is voor als we niet enabled mogen zijn, dan hoeven we veel niet te doen

	Menu = MPC<<-menu_bar, 
	getCommand(Menu?frame,MPC?commandname,Command),
	\+ Command->>canRun(MPC), !, %check functie faalt: disabelen en de rest hoeft niet
	%we komen alleen aan de active state wanneer nodig
	(   @off = MPC<<-active
	;   MPC->>active(@off)
	).
%
onIdle(MPC) :->
	%we mogen actief zijn, dus moeten we ook de boel updaten

	(   @on = MPC<<-active
	;   MPC->>active(@on)
	),
	Menu = MPC<<-menu_bar,
	getCommand(Menu?frame,MPC?commandname,Command),
	Button = MPC<<-button, %slaagt alleen als we in een menu_bar zitten
	Label = Command<<-getInfo('Label',MPC,Button?label),
	%gewijzigd?
	(   Button?label->>equal(Label)
	->  true
	;   ( Button->>label(Label),
	      Menu->>compute
	    )
	).
%%

%%
menu_bar(MPC,Menu: menu_bar):<-
	"Returns the menu bar this popup is connected to. Fails if not connected to a menu_bar" ::
	
	Menu = MPC<<-context,
	Menu->>instance_of(menu_bar).
%%

%%
button(MPC,Button: button):<-
	"Returns the button the popup is connected to in a menu_bar, fails if not in a menu_bar" ::
	
	Menu = MPC<<-menu_bar,
	Index = Menu?members<<-index(MPC),
	Button = Menu?buttons<<-nth1(Index).
%%

:-pce_end_class.

