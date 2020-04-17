:-pce_begin_class(
		  garp3EditorFrame(label,application),
		  frame,
		  "A frame for Garp3 design editors. It is a containing frame for dialogs."
		 ).

variable(garpModel, garpModel, both, 'The garpModel associated to the editor.').

initialise(
	   G3EF, 
	   Label:label = [name]
	   ) :->
    send_super(G3EF, initialise, label :=Label, application := @app).

associateModel(G3EF, GarpModel) :->
    send(G3EF, slot, garpModel, GarpModel).

%%
/******************** MULTIPLE MODELS ***********************/ %JL

/* Make the right model active */
input_focus(G3EF, Boolean) :->
	/* Assume the only window in the frame is the editor */
	%get(G3EF?members, tail, Window),
	/* Select the correct tab according to this window */
	send(@app, selectCorrectTab, G3EF), % T is the window object (of the editor)
	send_super(G3EF, input_focus, Boolean).


/********************HELPERS***********************/
%%

   
:-pce_end_class.

