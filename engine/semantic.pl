/*  File:    semantic
    Purpose: Semantic inheritance
    Author:  Martin Reinders & Bert Bredeweg
    Date:    August 1989
    Part-of: GARP (version 1.0)

    Copyright (c) 1989, University of Amsterdam. All rights reserved.

*/

isa_instance(Sub, Sub).
isa_instance(Sub, NewSuper):- 
        isa(Sub, Super), isa_instance(Super, NewSuper).

isa_instance_select(instance(Instance, Generic), L, NL):- 
        common_select(L, instance(Instance, SubGeneric), NL), 
        isa_instance(SubGeneric, Generic).

isa_attribute_select(has_attribute(Instance, Relation, SuperValue), L, NL):-
        common_select(L, has_attribute(Instance, Relation, SubValue), NL), 
        isa_instance(SubValue, SuperValue).


/* 1: conditions
   2: known
   both list of instance(Instance, Generic)
	  or has_attribute(Instance, Relation, InstanceValue)
	  or <relation>(I1, I2)

   a condition must specify the same Generic or a superconcept of Generic
   e.g.
	Condition:  instance(Some, liquid), 
	Known:	    instance(water1, water), 
	given:		isa(water, liquid).		Some = water1

	Condition: has_attribute(water1, relation, value_A)
	Known:	has_attribute(water1, relation, value_AA)
	given:	isa(value_AA, Value_A).		succeeds
*/

% done
super_class_sub_set([], _).

% instance
super_class_sub_set([instance(Instance, Generic)|T], L):-
        isa_instance_select(instance(Instance, Generic), L, RL), 
        super_class_sub_set(T, RL).

% attribute
super_class_sub_set([has_attribute(Instance, Relation, InstanceValue)|T], L):-
        isa_attribute_select(has_attribute(Instance, Relation, InstanceValue), L, RL), 
        super_class_sub_set(T, RL).

% other relations

super_class_sub_set([H|T], L):-
        relation_se(H), 
        common_select(L, H, NL), 
        super_class_sub_set(T, NL).

relation_se(isa(_, _)):- !, fail.
relation_se(has_attribute(_, _, _)):- !, fail.
relation_se(instance(_, _)):- !, fail.
relation_se(_).

