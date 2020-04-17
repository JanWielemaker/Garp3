/*
Garp3 0.1
The helper menu for exporting to legacy files
PART OF GARP3 SEE COPYRIGHT NOTICE

Allthough this is Garp3, its legacy homer code so most comments are in Dutch.
If you feel like learning a new language, pick dutch, its pretty.

Loaded when needed from application

Code is gp3 only where mentioned.
	
*/

:-pce_begin_class(exportDialog,
		  dialog,
		  "VGARP Export Dialog"
		 ).

variable(exportDir,'directory*',both,"The export directory").
variable(isaFile,'file*',both,"An opened exportfile").
variable(qsFile,'file*',both,"An opened exportfile").
variable(inputSystemFile,'file*',both,"An opened exportfile").
variable(secondaryInputSystemFile,'file*',both,"An opened exportfile").
variable(libraryFile,'file*',both,"An opened exportfile").

%%
initialise(D, MainMenu: mainMenu):-> %gp3: changed class
	"Initialise the editor" ::
	%gp3 0.3 changed the layout, other code still homer
	D->+initialise,
	D->>application(@app),
	D->>transient_for(MainMenu),
	D->>modal(transient),
	%D->>kind(popup), %gp3 0.3 changed this in
	D->>kind(transient),
	D->>label('Export model'),
	
	D->>gap(size(0,0)),
	D->>border(size(6,6)),
	D->>pen(0),
	D->>background(white),
	D->>hyper(MainMenu,mainMenu),

	/*
	%gp3 0.3.13: do we destroy the nice lay out for this help button:
	Help *= helpButton('File_ExportModelToFolderBelow'),
	D->>display(Help,point(0,0)),
	*/
	
	get_image(helpers,'export',Img),
	Logo *= label(logo,Img),
	D->>display(Logo,point(18,18)), %12 points into the box, which will be at 6,6
	Text *= text('Export model to folder below. To change, select a different location',left, font(helvetica,bold,14)),
	Text->>name(lbl),
	Text->>margin(400,wrap),
	D->>display(Text,point(Logo?right_side + 6, Logo?top_side)),

	MaxY *= number(Logo?bottom_side),
	MaxY->>maximum(Text?bottom_side),

	Path *= eventTextItem(path),
	Path->>show_label(@off),
	Path->>onKey(->>(@pce,fail)),
	Path->>background(colour(white)),
	Path->>pen(0),
	D->>display(Path,point(Logo?left_side, MaxY + 12)),
	Path->>right_side(Text?right_side),

	SubDirs *= extendedListBrowser(height := 12),
	SubDirs->>name(subdirs),
	SubDirs->>show_label(@off),
	SubDirs->>background(colour(white)),
	SubDirs->>pen(0),
	SubDirs->>open_message(->>(D,changePath,@arg1)),
	D->>display(SubDirs,point(Logo?left_side,Path?bottom_side + 6 )),
	SubDirs->>right_side(Text?right_side),
	
	%box behind it all	
	Box *= box(Text?right_side - Logo?left_side + 24, SubDirs?bottom_side - Logo?top_side + 24), %spacing
	Box->>pen(0),
	BG = new(colour)<<-convert('#E7EAEA'), %buggy: cannot use the hex value directly
	Box->>fill_pattern(BG),
	D->>display(Box,point(6,6)),
	Box->>hide,
	
	%we use clickImage object, no buttons, like on the mainmenu
	get_image(helpers,'export_legacy',SaveImg),
	Save *= clickImage(go,SaveImg,->>(D,go), tooltipMsg:='Export'),
	get_image(helpers,'cancel',CancelImg),
	Cancel *= clickImage(cancel,CancelImg,->>(D,cancel), tooltipMsg:='Cancel'),
	D->>display(Save,point(Box?left_side,Box?bottom_side + 6)),
	D->>display(Cancel,point(Box?right_side - Cancel?width,Box?bottom_side + 6)),

	%even het pad goed zetten
	D->>setPath(@app?fileDir).
%%

%%
openDialog(D):->
	%openen en destroyen
	D<<-confirm_centered(D?mainMenu?frame?area?center),
	D->>destroy.

%%
mainMenu(D, M: mainMenu):<- %gp3: changed class
	"Return mainmenu" ::
	
	M = D<<-hypered(mainMenu).
%%

%%
setPath(D,Dir: 'directory*'):->
	%zet het pad, vul de lijst met subdirs
	%als directory = @nil, dan is het de topknoop (bij unix gewoon /, bij windows de virtuele knoop boven de logical drives

	L = D<<-subdirs_member,
	L->>clear,
	
	if
		Dir == @nil
	then
	(
		Roots = directory('.')<<-roots,
		if
			1 = Roots<<-size	%geen keus
		then
		(
			NieuwDir *= directory(Roots?head),
			D?path_member->>selection(NieuwDir?path),
			Dirs = NieuwDir<<-directories,
			D?go_member->>active(@on)
		)
		else
		(
			Dirs = Roots,
			NieuwDir = @nil,
			D?go_member->>active(@off),
			D?path_member->>selection('')
		)
	)
	else
	(
		D?path_member->>selection(Dir?path),
		Dirs = Dir<<-directories,
		D?go_member->>active(@on),
		NieuwDir = Dir
	),

	L->>append(dict_item('[..]')), %@nil voor up, want wie weet is er geen parent
	L->>append(dict_item('[~]')),	%en de homedir
	Dirs->>sort(?(@arg1,compare,@arg2,@on)),
	Dirs->>for_all(
		->>(L,append,create(
				dict_item,@arg1)
			)
	),
	Dirs->>done,
	D->>exportDir(NieuwDir).
%
setPath(D,_):->
	%als er ook maar iets faalt: terug
	D->>setPath(D?exportDir).
%%

%%
changePath(D, DI: dict_item):->
	%actie op dubbelklik in lijst
	
	%wat gaan we doen dan?
	
	Current = D<<-exportDir,
	if
		Current == @nil	%de "topknoop"
	then
	(
		if
			DI?label->>equal('[..]')
		then
			NewDir = @nil
		else
		(
			if
				DI?label->>equal('[~]')
			then
				NewDir *= directory('~')
			else
				NewDir *= directory(DI?label)
		)
	)
	else
	(
		if
			DI?label->>equal('[..]')
		then
		(
			NewDir = Current<<-parent
		;
			NewDir = @nil	%als er geen parent is
		)
		else
		(
			if
				DI?label->>equal('[~]')
			then
				NewDir *= directory('~')
			else
				NewDir = Current<<-directory(DI?label)
		)
	),
	D->>setPath(NewDir).
%%

%%
cancel(D):->
	D->>return.
%%

%%
%%Bij GO: ook bij het mainmenu de nieuwe directory goed zetten!
%en natuurlijk goed checken of alle files gemaakt kunnen worden!
go(D):->
	%exporteer het model, kijk of we toestemming hebben voor alle files
	%(en controleer de checksums)

	%check eerst maar eens of we de files kunnen openen en hoe het zit
	%met de checksums
	
	Dir = D<<-exportDir,
	D->>isaFile(?(Dir,file,isa)),
	D?isaFile->>kind(text),
	D->>qsFile(?(Dir,file,quantity_space)),
	D?qsFile->>kind(text),
	D->>inputSystemFile(?(Dir,file,'input_system.gp')),
	D?inputSystemFile->>kind(text),
	D->>secondaryInputSystemFile(?(Dir,file,'input_system.vgp')),
	D?secondaryInputSystemFile->>kind(text),
	D->>libraryFile(?(Dir,file,library)),
	D?libraryFile->>kind(text),

	if
	(
		D->>checkChecksums,
		D->>checkOpenFiles
	)
	then
	(
		if
			@nil = @model<<-filename
		then
			ModelName = '(unsaved model)'
		else
			ModelName = file(@model?filename)<<-base_name,
		@app->>pleaseWait(run),
		@model->>exportIsa(D?isaFile, ModelName),
		@model->>exportQS(D?qsFile, ModelName),
		@model->>exportInputSystem(D?inputSystemFile, ModelName),
		@model->>exportLibrary(D?libraryFile, ModelName,@on), 
			%gp3: LegacyNaming on, see modelFragment->export

		D?isaFile->>close,
		D?qsFile->>close,
		D?inputSystemFile->>close,
		D?libraryFile->>close,
		D->>setChecksums,
		D->>copyInputSystem,
		D?secondaryInputSystemFile->>close,
		@app->>thankYou,
		D->>msgBox(string('Your model has been exported successfully to:\n%s',D?exportDir),notification), %gp3 0.3: changed to new dlg, no label
		D->>return
	).
%%

%%
checkChecksums(D):->
	%check of er files zijn, en zoja, kijk of ze overeenkomen met onze checksums
	
	if
	(
		D->>wrongChecksum(D?isaFile,@model?exportChecksum_isa)
	;
		D->>wrongChecksum(D?qsFile,@model?exportChecksum_qs)
	;
		D->>wrongChecksum(D?inputSystemFile,
				@model?exportChecksum_inputsystem)
	;
		D->>wrongChecksum(D?libraryFile,@model?exportChecksum_library)
	)
	then
	(
		%gp3 0.3: use the new overwrite confirmer
		C *= confirmOverwriteDlg(D,'According to the model information, one or more exportfiles in this folder are changed since the last export.\n\nAre you sure you want to overwrite them?'),
	    R = C<<-confirm_centered(D?frame?area?center),
	    C->>destroy,
		R = @on
	).	
%%

%%
wrongChecksum(D,F: file, Checksum: 'string*'):->
	%slaagt als de checksum niet klopt
	
	F->>exists,	%anders faalt dit meteen, want is er niets aan de hand
	CS = D<<-getChecksum(F),	%faalt ook bij niet-bestaande file
	(
		Checksum == @nil
	;
		\+ CS->>equal(Checksum)
	).
%%

%%
setChecksums(D):->
	%schrijf de nieuwe checksums weg in het model
		@model->>exportChecksum_isa(?(D,getChecksum,D?isaFile)),
		@model->>exportChecksum_qs(?(D,getChecksum,D?qsFile)),
		@model->>exportChecksum_inputsystem(?(D,getChecksum,D?inputSystemFile)),
		@model->>exportChecksum_library(?(D,getChecksum,D?libraryFile)),
		@app->>setModelChanged. %changed, but no conceptual changes.
%
getChecksum(_D,F: file, Checksum: string):<-
	%geeft een nieuwe checksum voor de file
	%faalt als de file niet bestaat
	
	F->>exists,
	Date = F<<-time(modified),
	Checksum *= string('%s%s',F?size,Date?string).
%%

%%
checkOpenFiles(D):->
	%open de 5 files, als dat mag, anders klagen

	if
	(
		\+ D->>checkOpenFile(D?isaFile)
	;
		\+ D->>checkOpenFile(D?qsFile)
	;
		\+ D->>checkOpenFile(D?inputSystemFile)
	;
		\+ D->>checkOpenFile(D?secondaryInputSystemFile)
	;
		\+ D->>checkOpenFile(D?libraryFile)
	)
	then
	(
		D->>msgBox('You don\'t have permission to\nwrite to the export files in this folder!',alarm),
		fail
	).	
%
checkOpenFile(_D, F: file):->
	%open de file voor schrijven, faal als dat niet kan
	
	F->>close,
	if
		F->>exists
	then
		F->>access(write),
	F->>open(write).
%%

%%
copyInputSystem(D):->
	%we kopieren het inputsystem ook nog naar een secondaire file

		SIF = D?secondaryInputSystemFile,
		IF = D?inputSystemFile,
		if
			(\+ IF->>access(read))
		then
			D->>msgBox('Error creating the secondary input system file.\nCheck the permissions.',alarm)
		else
			SIF->>copy(IF).
%%
:-pce_end_class.
