/*
unlinkNotifier: hulp klasse voor notify van het verwijderen van een object:
gebruikt via object->notifyUnlink, dat hieronder ook is gedefinieerd

*/

:-module(unlinkNotifier,[]).

:- pce_begin_class(unlinkNotifier, hyper).

%%niet om aan te maken in code, zie hieronder object->notifyUnlink
variable(unlinking,code,both).
variable(save,bool,both).
%%
initialise(UN,
	O: object,
	NotifiedObject: object,
	Unlinking: code,
	Safe: [bool]):->
	%wanneer O wordt verwijderd wordt Unlinking gedraaid met @arg1 = O en @arg2 = NotifiedObject
	%wanneer NotifiedObject wordt verwijderd, dan wordt er geen code gedraaid, maar de hyper verwijderd

	UN->+initialise(O,NotifiedObject,unlinkNotifier),
	default(Safe,@off,RSafe),
	UN->>save(RSafe), %@on: niet notifiyen als de notifyto zelf ook unlinkt
	UN->>unlinking(Unlinking).
%%

%%
unlink_from(UN):->

	%1: doe niets als de genen die we op de hoogte moeten stellen zelf ook
	%unlinkt en Save @on is
	
	@on = UN<<-save,
	UN?to->>unlinking,
	!,
	UN->+unlink_from.
%
unlink_from(UN):->
	%2: wel verwijderen

	UN?unlinking->>forward(UN?from, UN?to),
	UN->+unlink_from.
%%

:-pce_end_class.

:-pce_extend_class(object).

%%
notifyUnlink(O,NotifiedObject: notifiedObject = object, C: code = code, Safe: safe = [bool], Replace: replace = [bool]):->
	"Run code when object unlinks, with object as @arg1 and NotifiedObject as @arg2. Does not run code when notifiedObject is unlinking and safe = @on" ::
	%NotifiedObject: wordt meegestuurd als @arg2. Als NotifiedObject is verwijderd, dan wordt
	%de code dus niet gedraaid, maar de notifier verwijderd.
	
	%gp3 0.3 added Replace option: replaces other unlinknotifiers from O to NotifiedObject. Default = @on (old code okay with that)
	
	unless
		Replace = @off
	do
	(
		O->>delete_hypers(condition:=
			and(
				->>(@arg2,instance_of,unlinkNotifier), 
					@arg2?from == O, %also check if O is from, not to
					@arg2?to == NotifiedObject
				))
	),
	_ *= unlinkNotifier(O,NotifiedObject, C,Safe).
%%

%%weer uitschakelen kan nog niet. Misschien de mogelijkheid om de hyper naar een ander
%%object te laten verwijzen zodat je kan selecteren welke je weg wilt hebben?
:- pce_end_class.

