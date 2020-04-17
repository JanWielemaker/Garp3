/*
psBitmap class
part of GARP3

Use this one instead of bitmap, and you can tell which ps file to use as a definition, when postscripted

Set psdef to the name of a definition, and when postscript is needed, 
the bitmap will get the file <psdef>.ps from the icons/ps folder.
The entire content of this file will be used as a procedure definition, for the
procedure names psdef. This will be done only once per postscript file, to keep
the file clean. Then when the bitmap is needed in the ps, it will write out a
call to this procedure definition.

The bitmap and the ps version should have exactly the same dimensions, because we flip the ps version vertically and move them according to the height of the bitmap, to get the place right.

2005, Jelmer Jellema, Spin in het Web

*/

:-pce_global(@psBitmapDefinitions,new(hash_table)). %holders for sort of class-global variables

:-pce_begin_class(psBitmap,
		  bitmap
		 ).


variable(psdef,name*,get). %the name of ps definition.
variable(pspath,name*,both). %the full path, just to make it easier

psdef(BM, Def: name):->
	%gp3 0.3: set ps definition file.
	%we check if it exists
	%if not: we use missing.ps and warn
	
	PSPath = string('%s/icons/ps/%s.ps',@garp3_dir?path,Def)<<-value,
	F *= file(PSPath),
	if
		F->>exists(@on)
	then
	(
		BM->>pspath(PSPath),
		BM->>slot(psdef,Def) %so the original setting can be read
	)
	else
	(
		MissingPath = string('%s/icons/ps/missing.ps',@garp3_dir?path)<<-value,
		BM->>pspath(MissingPath),
		BM->>slot(psdef,'missing')
	).
%%
	

'_draw_post_script'(BM, Pass: {head,body}) :->
	%%gp3 0.3
	%%do we have a defintition?
	
	@nil = BM<<-psdef,!,
	BM->+'_draw_post_script'(Pass).
%
'_draw_post_script'(BM,Pass: {head,body}) :->
	%%When Pass = head: make sure we send the definition when not done before
	
	Pass == head, 
	/*
	It seems to be a bit tricky to figure out whether the definition was
	allready written to *this* postscript output.
	This is because we can only get a stream, no information about
	whether this is the first time the head is asked for this class of objects etc.
	I tried using the number in '$stream'(Number), but prolog reuses its stream numbers.
	So now we use this. PCE first calls all objects with Pass = head,
	then with Pass = body.
	So the first time a head pass is made, we flag this in the
	psBitmapDefinitions global hash_table, which we clear (all informatin = old).
	The next time the head pass is made,  we asume it is still the same postscript
	stream we are writing to.
	The first time the *body* pass is called, we remove the head-flag in the global
	hash. So the next time the head pass is called, we asume it is a new postscript
	export.
	*/
	
	unless
		head = @psBitmapDefinitions<<-member('__currentPassType')
	do
	(
		@psBitmapDefinitions->>clear, %all info was meant for an old stream
		@psBitmapDefinitions->>append('__currentPassType',head)
	),
	
	
	%if our psDef was allready printed to this stream, we are done
	Def = BM<<-psdef,
	unless
		@psBitmapDefinitions<<-member(Def)
	do
	(
		%not printed, do it now
		pce_principal:pce_postscript_stream(Stream),
		Width = BM<<-width, Height = BM<<-height, %make sure its pl-fahig
		format(Stream, '/psBitmap_~w {~n', [Def]),
		format(Stream, '%%Garp3: Icon size: ~w X ~w, flipped:~ngsave 0 ~w translate 1 -1 scale~n',[Width,Height, Height]),
		format(Stream, '%%Garp3 disabled showpage~n/showpage null def~n', []), %disable showpage in eps
		PSPath = BM<<-pspath, %already found in psDef
		open(PSPath, read, In),
		copy_stream_data(In,Stream),
		close(In),
		format(Stream,'~ngrestore~n} def~n~n',[]),
		%and write down this one is done
		@psBitmapDefinitions->>append(Def,done)
	).
%
'_draw_post_script'(BM,Pass: {head,body}) :->
	%%When Pass = body: just call the procedure
	%%we just asume all is done ok in the head Pass
	%except for position: when writing ps ourselves, we have to set the position as well
	
	Pass = body,
	@psBitmapDefinitions->>append('__currentPassType',body), %no longer in head pass, see above
	pce_principal:pce_postscript_stream(Stream),
	Def = BM<<-psdef,
	X = BM<<-x,
	Y = BM<<-y,
	format(Stream,'gsave ~w ~w translate psBitmap_~w grestore~n',[X,Y,Def]).
%%

:-pce_end_class.
