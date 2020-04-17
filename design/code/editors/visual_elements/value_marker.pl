/*
Definitie valueMarker class 
figuurtje om een value bij een qsValueElement of dqsValueElement mee af te beelden

Part of Garp3, see copyright notice
Most code old dutch homer code

2000-2005 Jelmer Jellema, Spin in het Web, spininhetweb.nl
*/


:-pce_begin_class(valueMarker,
		  staticMarker,
		  "Display of conditional or given value"
		 ).
%%
initialise(VM,
	   Fragment : modelFragment,
	   Value: value,
		NextTo: 'qsValueElement | dqsValueElement',
		FragmentState: '[{normal,imported,parent}]',
		Route: [chain] %zie visualElement
	  ):->

	VM->+initialise(Fragment,Value,@nil,FragmentState,Route),

	Bitmap *= psBitmap(VM?stateImage),
	Bitmap->>psdef(VM?imageName),
	Bitmap->>name(bitmap),
	VM->>display(Bitmap),

	%beeld de marker af (updateDisplay wordt niet gebruikt)
	VM->>setMarker(NextTo,left).
	%no updateDisplay here
%%

%%
basename(_VM,N:name):<-
	%gp3 0.2
	N = value.
%%

%%
canHide(_VM):->
	%dit element mag niet hiden, dus een overrule

	!,fail.
%%

:-pce_end_class.
