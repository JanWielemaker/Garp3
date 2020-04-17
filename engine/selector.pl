/*  File:    selector
    Purpose: Structure selector
    Author:  Martin Reinders & Bert Bredeweg & Floris Linnebank
    Date:    August 1989
    Part-of:  GARP (version 2.0)
    Modified: 30 July 2004

    Copyright (c) 2004, University of Amsterdam. All rights reserved.

*/

/*
  class input output structure select/change (cio)

  cio structure
  se    1 : 'new' system elements (not used as 'index' before)
  ss    2 : 'new' system structures (idem)
  oe    3 : 'old' system elements (used before)
  os    4 : 'old' system structures (idem)
  hy    5 : System structure hypotheses (system element/structure conditions
        match)
  re    6 : Rejected hypotheses
  as    7 : List of AssumableSS/Assumptions:
        AssumableSS = list of parents-child valid if Assumptions
        are made
  sf    8 : List of valid system structures collected sofar
  p     9 : List of parameters found so far (same as those in IM)
  v     10: List of parameter values (idem), initalised intervals are mapped
             to inequality relations!
  r     11: List of (in) equality parameter relations, (intern representation)
  q     12: List of quantities (value(Instance) | derivative(Instance) |
            multiplication(Quantities));
            item number corresponds to intern representation
  d     13: List of derivable relations (simple: a>x, a>0; intern representation)
  ip    14: List of influences/proportional relations
  c     15: correspondence relations
  m     16: list of calculated multiplications
*/

cio_empty(
        cio(  [],  [],  [],  [],  [],  [],  [],  [], [], [], [], [], [], [], [], []) ).
cio_se(
        cio(  SE,   _,   _,   _,   _,   _,   _,   _,  _,  _,  _,  _,  _,  _, _, _), SE).
cio_ss(
        cio(   _,  SS,   _,   _,   _,   _,   _,   _,  _,  _,  _,  _,  _,  _, _, _), SS).
cio_oe(
        cio(   _,   _,  OE,   _,   _,   _,   _,   _,  _,  _,  _,  _,  _,  _, _, _), OE).
cio_sf(
        cio(   _,   _,  _,   _,   _,   _,   _,   SF,  _,  _,  _,  _,  _,  _, _, _), SF).
cio_p(
        cio(   _,   _,   _,   _,   _,   _,   _,   _,  P,  _,  _,  _,  _,  _, _, _), P).
cio_v(
        cio(   _,   _,   _,   _,   _,   _,   _,   _,  _,  V,  _,  _,  _,  _, _, _), V).
cio_r(
        cio(   _,   _,   _,   _,   _,   _,   _,   _,  _,  _,  R,  _,  _,  _, _, _), R).
cio_q(
        cio(   _,   _,   _,   _,   _,   _,   _,   _,  _,  _,  _,  Q,  _,  _, _, _), Q).
cio_d(
        cio(   _,   _,   _,   _,   _,   _,   _,   _,  _,  _,  _,  _,  D,  _, _, _), D).
cio_c(
        cio(   _,   _,   _,   _,   _,   _,   _,   _,  _,  _,  _,  _,  _,  _, C, _), C).
cio_m(
        cio(   _,   _,   _,   _,   _,   _,   _,   _,  _,  _,  _,  _,  _,  _, _, M), M).
cio_p_np(
        cio(  SE,  SS,  OE,  OS,  HY,  RE,  AS,  SF,  P,  V,  R,  Q,  D, IP, C, M),
        cio(  SE,  SS,  OE,  OS,  HY,  RE,  AS,  SF, NP,  V,  R,  Q,  D, IP, C, M), P, NP).
cio_v_nv(
        cio(  SE,  SS,  OE,  OS,  HY,  RE,  AS,  SF,  P,  V,  R,  Q,  D, IP, C, M),
        cio(  SE,  SS,  OE,  OS,  HY,  RE,  AS,  SF,  P, NV,  R,  Q,  D, IP, C, M), V, NV).
cio_d_nd(
        cio(  SE,  SS,  OE,  OS,  HY,  RE,  AS,  SF,  P,  V,  R,  Q,  D, IP, C, M),
        cio(  SE,  SS,  OE,  OS,  HY,  RE,  AS,  SF,  P,  V,  R,  Q, ND, IP, C, M), D, ND).
cio_c_nc_d(
        cio(  SE,  SS,  OE,  OS,  HY,  RE,  AS,  SF,  P,  V,  R,  Q,  D, IP, C, M),
        cio(  SE,  SS,  OE,  OS,  HY,  RE,  AS,  SF,  P,  V,  R,  Q,  D, IP, NC, M), C, NC, D).
cio_p_np_v_nv(
        cio(  SE,  SS,  OE,  OS,  HY,  RE,  AS,  SF,  P,  V,  R,  Q,  D, IP, C, M),
        cio(  SE,  SS,  OE,  OS,  HY,  RE,  AS,  SF, NP, NV,  R,  Q,  D, IP, C, M), P, NP, V, NV).
cio_se_nse(
        cio(  SE,  SS,  OE,  OS,  HY,  RE,  AS,  SF,  P,  V,  R,  Q,  D, IP, C, M),
        cio(NSE,  SS,  OE,  OS,  HY,  RE,  AS,  SF,  P,  V,  R,  Q,  D, IP, C, M),
        SE, NSE).
cio_ss_nss(
        cio(  SE,  SS,  OE,  OS,  HY,  RE,  AS,  SF,  P,  V,  R,  Q,  D, IP, C, M),
        cio(  SE, NSS,  OE,  OS,  HY,  RE,  AS,  SF,  P,  V,  R,  Q,  D, IP, C, M),
        SS, NSS).
cio_as_nas(
        cio(  SE,  SS,  OE,  OS,  HY,  RE,  AS,  SF,  P,  V,  R,  Q,  D, IP, C, M),
        cio(  SE,  SS,  OE,  OS,  HY,  RE, NAS,  SF,  P,  V,  R,  Q,  D, IP, C, M),
        AS, NAS).
cio_sf_nsf(
        cio(  SE,  SS,  OE,  OS,  HY,  RE,  AS,  SF,  P,  V,  R,  Q,  D, IP, C, M),
        cio(  SE,  SS,  OE,  OS,  HY,  RE,  AS, NSF,  P,  V,  R,  Q,  D, IP, C, M), SF, NSF).
cio_hy_nhy(
        cio(  SE,  SS,  OE,  OS,  HY,  RE,  AS,  SF,  P,  V,  R,  Q,  D, IP, C, M),
        cio(  SE,  SS,  OE,  OS, NHY,  RE,  AS,  SF,  P,  V,  R,  Q,  D, IP, C, M), HY, NHY).
cio_hy_nhy_re_nre(
        cio(  SE,  SS,  OE,  OS,  HY,  RE,  AS,  SF,  P,  V,  R,  Q,  D, IP, C, M),
        cio(  SE,  SS,  OE,  OS, NHY, NRE,  AS,  SF,  P,  V,  R,  Q,  D, IP, C, M), HY, NHY, RE, NRE).
cio_re_nre(
        cio(  SE,  SS,  OE,  OS,  HY,  RE,  AS,  SF,  P,  V,  R,  Q,  D, IP, C, M),
        cio(  SE,  SS,  OE,  OS,  HY, NRE,  AS,  SF,  P,  V,  R,  Q,  D, IP, C, M), RE, NRE).
cio_ss_se_hy_nhy(
        cio(  SE,  SS,  OE,  OS,  HY,  RE,  AS,  SF,  P,  V,  R,  Q,  D, IP, C, M),
        cio(  SE,  SS,  OE,  OS, NHY,  RE,  AS,  SF,  P,  V,  R,  Q,  D, IP, C, M), SS, SE, HY, NHY).
cio_ss_oe_os_hy_nhy_as_re(
        cio(  SE,  SS,  OE,  OS,  HY,  RE,  AS,  SF,  P,  V,  R,  Q,  D, IP, C, M),
        cio(  SE,  SS,  OE,  OS, NHY,  RE,  AS,  SF,  P,  V,  R,  Q,  D, IP, C, M), SS, OE, OS, HY, NHY, AS, RE).
cio_p_v_r(
        cio(   _,   _,   _,   _,   _,   _,   _,   _,  P,  V,  R,  _,  _,  _, _, _), P, V, R).
cio_p_v_q_d(
        cio(   _,   _,   _,   _,   _,   _,   _,   _,  P,  V,  _,  Q,  D,  _, _, _), P, V, Q, D).
cio_q_d(
        cio(   _,   _,   _,   _,   _,   _,   _,   _,  _,  _,  _,  Q,  D,  _, _, _), Q, D).
cio_q_nq(
        cio(  SE,  SS,  OE,  OS,  HY,  RE,  AS,  SF,  P,  V,  R,  Q,  D, IP, C, M),
        cio(  SE,  SS,  OE,  OS,  HY,  RE,  AS,  SF,  P,  V,  R, NQ,  D, IP, C, M), Q, NQ).
cio_r_nr_d_nd_q_nq_c_nc(
        cio(  SE,  SS,  OE,  OS,  HY,  RE,  AS,  SF,  P,  V,  R,  Q,  D, IP, C, M),
        cio(  SE,  SS,  OE,  OS,  HY,  RE,  AS,  SF,  P,  V, NR, NQ, ND, IP, NC, M), R, NR, D, ND, Q, NQ, C, NC).
cio_se_nse_oe_noe(
        cio(  SE,  SS,  OE,  OS,  HY,  RE,  AS,  SF,  P,  V,  R,  Q,  D, IP, C, M),
        cio(NSE,  SS, NOE,  OS,  HY,  RE,  AS,  SF,  P,  V,  R,  Q,  D, IP, C, M), SE, NSE, OE, NOE).
cio_se_nse_ss_oe_noe_os_hy_nhy_as_re(
        cio(  SE,  SS,  OE,  OS,  HY,  RE,  AS,  SF,  P,  V,  R,  Q,  D, IP, C, M),
        cio(NSE,  SS, NOE,  OS, NHY,  RE,  AS,  SF,  P,  V,  R,  Q,  D, IP, C, M), SE, NSE, SS, OE, NOE, OS, HY, NHY, AS, RE).
cio_se_nse_ss_nss(
        cio(  SE,  SS,  OE,  OS,  HY,  RE,  AS,  SF,  P,  V,  R,  Q,  D, IP, C, M),
        cio(NSE, NSS,  OE,  OS,  HY,  RE,  AS,  SF,  P,  V,  R,  Q,  D, IP, C, M), SE, NSE, SS, NSS).
cio_se_ss_hy_as(
        cio(  SE,  SS,   _,   _,  HY,   _,  AS,   _,  _,  _,  _,  _,  _,  _, _, _), SE, SS, HY, AS).
cio_se_ss_nss_oe_os_nos_hy_nhy_as_re(
        cio(  SE,  SS,  OE,  OS,  HY,  RE,  AS,  SF,  P,  V,  R,  Q,  D, IP, C, M),
        cio(  SE, NSS,  OE, NOS, NHY,  RE,  AS,  SF,  P,  V,  R,  Q,  D, IP, C, M), SE, SS, NSS, OE, OS, NOS, HY, NHY, AS, RE).
cio_ss_oe_os_re_hy_nhy(
        cio(  SE,  SS,  OE,  OS,  HY,  RE,  AS,  SF,  P,  V,  R,  Q,  D, IP, C, M),
        cio(  SE,  SS,  OE,  OS, NHY,  RE,  AS,  SF,  P,  V,  R,  Q,  D, IP, C, M), SS, OE, OS, RE, HY, NHY).

cio_ip_nip(
        cio(  SE,  SS,  OE,  OS,  HY,  RE,  AS,  SF,  P,  V,  R,  Q,  D, IP, C, M),
        cio(  SE,  SS,  OE,  OS,  HY,  RE,  AS,  SF,  P,  V,  R,  Q,  D, NIP, C, M), IP, NIP).

cio_c_nc(
        cio(  SE,  SS,  OE,  OS,  HY,  RE,  AS,  SF,  P,  V,  R,  Q,  D, IP, C, M),
        cio(  SE,  SS,  OE,  OS,  HY,  RE,  AS,  SF,  P,  V,  R,  Q,  D, IP, NC, M), C, NC).

cio_r_nr_c_nc(
        cio(  SE,  SS,  OE,  OS,  HY,  RE,  AS,  SF,  P,  V,  R,  Q,  D, IP, C, M),
        cio(  SE,  SS,  OE,  OS,  HY,  RE,  AS,  SF,  P,  V,  NR,  Q,  D, IP, NC, M), R, NR, C, NC).

cio_m_nm(
        cio(  SE,  SS,  OE,  OS,  HY,  RE,  AS,  SF,  P,  V,  R,  Q,  D, IP, C, M),
        cio(  SE,  SS,  OE,  OS,  HY,  RE,  AS,  SF,  P,  V,  R,  Q,  D, IP, C, NM), M, NM).


% ?

inverse_relation(relation(Left, Rel, Right), relation(Right, Inverse, Left) ) :-
        inverse(Rel, Inverse).

% parameter(H, Genetic, SystemElements, Instance, Type)

parameter(H, NH, Genetic, SystemElements, Instance, Type, Space):-
        H =.. [Genetic, SystemElements, Instance, Type],
        NH =.. [Genetic, SystemElements, Instance, Type, Space],
    (Type = continuous; true),
        !.

parameter(H, NH, Genetic, SystemElements, Instance, continuous, Space):-
        H =.. [Genetic, SystemElements, Instance],
        NH =.. [Genetic, SystemElements, Instance, continuous, Space],
        !.

parameter(H, H, Genetic, SystemElements, Instance, Type, Space):-
        H =.. [Genetic, SystemElements, Instance, Type, Space ],
    (Type = continuous; true),
    !.

% value(Value, Instance, _, Interval, Derivative)
value(value(Instance, Q, Interval, Derivative), Instance, Q, Interval, Derivative).

conditions(system_structures(_, _, C, _), C).
givens(system_structures(_, _, _, G), G).

isa_list(isa(L), L).

system_structure_name(system_structures(Name, _, _, _), Name).
system_structure_slots(
        system_structures(
                Name,
                isa(Isa),
                conditions(Conditions),
                givens(Givens)),
        Name, Isa, Conditions, Givens).

system_element_conditions(conditions(Conditions), SEC):-
    memberchk(system_elements(SEC), Conditions), !.
system_element_conditions(_, []).
system_structure_conditions(conditions(Conditions), SSC):-
    memberchk(system_structures(SSC), Conditions), !.
system_structure_conditions(_, []).
parameter_conditions(conditions(Conditions), P):-
    memberchk(parameters(P), Conditions).
par_value_conditions(conditions(Conditions), PV):-
    memberchk(par_values(PV), Conditions).
par_relation_conditions(conditions(Conditions), PR):-
    memberchk(par_relations(PR), Conditions).


/* Conflict using with plcgi, but this one is not used (JW)
error(S, W):- writef(S), write(W), nl, fail.
*/


smd_slots(smd(Name, system_elements(SE), parameters(P), par_values(V),
            par_relations(R), system_structures(SS),
            InputSmd), Name,
            SE, P, V, R, SS, InputSmd).

smd_slots2(smd(Name, SE, P, V, R, SS, IS), Name, SE, P, V, R, SS, IS).


/* FL May 2004: store selector: In the store some often used elements
are stored with a state so that mf fragments do not have to be interpreted
again and again. also the internal mathematical model is stored (which is
present in the cio structure).

NEW FL april 07: store second order derivatives: sod
new FL june 07: store exogenous pos and neg parabola

*/
/*
store(
    0 cd - constant derivatives (parameter names)
    1 cv - constant values      (parameter names)
    2 cr - constant relations   (relations user representation)
    3 ec - exogenous_sinus (former continuous, ambiguous name) (parameter names)
    4 ef - exogenous_free (parameter names)
    5 ei - exogenous_increasing (parameter names)
    6 es - exogenous_steady (parameter names)
    7 ed - exogenous_decreasing (parameter names)
      pp - exogenous_pos_parabola
      np - exogenous_neg_parabola
    8 qt - QuantityTable
    9 con - Continuity relations extern
    10 cio - Cio
    11 lmr - LandMarkRelationsExtern
    12 sod - Second order derivatives (now higher order including third)
    13 amb - Ambiguous Derivatives List
    14 ov - Old Values (from previous state) % these are for post filter
    in third order derivative functionality... (NB NOT in use anymore,
    post filter does not need precursor to previous state info to
    function).

*/


get_store(SMD, Store):-
    smd_slots(SMD, _, _, _, _, _, _, Input),
    smd_slots(Input, _, _, _, _, _, _, Store).


store_empty(store([], [], [], [], [], [], [], [], [], [], [], [], cio, [], [], [], [])).

store_select(store(CD, CV, CR, EC, EF, EI, ES, ED, PP, NP, QT, CON, CIO, LMR, SOD, AMB, OV),
            CD, CV, CR, EC, EF, EI, ES, ED, PP, NP, QT, CON, CIO, LMR, SOD, AMB, OV).

store_cd_ncd(store(CD, CV, CR, EC, EF, EI, ES, ED, PP, NP, QT, CON, CIO, LMR, SOD, AMB, OV),
             store(NCD, CV, CR, EC, EF, EI, ES, ED, PP, NP, QT, CON, CIO, LMR, SOD, AMB, OV), CD, NCD).

store_cv_ncv(store(CD, CV, CR, EC, EF, EI, ES, ED, PP, NP, QT, CON, CIO, LMR, SOD, AMB, OV),
             store(CD, NCV, CR, EC, EF, EI, ES, ED, PP, NP, QT, CON, CIO, LMR, SOD, AMB, OV), CV, NCV).

store_cr_ncr(store(CD, CV, CR, EC, EF, EI, ES, ED, PP, NP, QT, CON, CIO, LMR, SOD, AMB, OV),
             store(CD, CV, NCR, EC, EF, EI, ES, ED, PP, NP, QT, CON, CIO, LMR, SOD, AMB, OV), CR, NCR).

store_ec_nec(store(CD, CV, CR, EC, EF, EI, ES, ED, PP, NP, QT, CON, CIO, LMR, SOD, AMB, OV), % name_format(+QuantityName, -FormattedName)

             store(CD, CV, CR, NEC, EF, EI, ES, ED, PP, NP, QT, CON, CIO, LMR, SOD, AMB, OV), EC, NEC).

store_ef_nef(store(CD, CV, CR, EC, EF, EI, ES, ED, PP, NP, QT, CON, CIO, LMR, SOD, AMB, OV),
             store(CD, CV, CR, EC, NEF, EI, ES, ED, PP, NP, QT, CON, CIO, LMR, SOD, AMB, OV), EF, NEF).

store_ei_nei(store(CD, CV, CR, EC, EF, EI, ES, ED, PP, NP, QT, CON, CIO, LMR, SOD, AMB, OV),
             store(CD, CV, CR, EC, EF, NEI, ES, ED, PP, NP, QT, CON, CIO, LMR, SOD, AMB, OV), EI, NEI).

store_es_nes(store(CD, CV, CR, EC, EF, EI, ES, ED, PP, NP, QT, CON, CIO, LMR, SOD, AMB, OV),
             store(CD, CV, CR, EC, EF, EI, NES, ED, PP, NP, QT, CON, CIO, LMR, SOD, AMB, OV), ES, NES).

store_ed_ned(store(CD, CV, CR, EC, EF, EI, ES, ED, PP, NP, QT, CON, CIO, LMR, SOD, AMB, OV),
             store(CD, CV, CR, EC, EF, EI, ES, NED, PP, NP, QT, CON, CIO, LMR, SOD, AMB, OV), ED, NED).

store_qt_nqt(store(CD, CV, CR, EC, EF, EI, ES, ED, PP, NP, QT, CON, CIO, LMR, SOD, AMB, OV),
             store(CD, CV, CR, EC, EF, EI, ES, ED, PP, NP, NQT, CON, CIO, LMR, SOD, AMB, OV), QT, NQT).

store_con_ncon(store(CD, CV, CR, EC, EF, EI, ES, ED, PP, NP, QT, CON, CIO, LMR, SOD, AMB, OV),
             store(CD, CV, CR, EC, EF, EI, ES, ED, PP, NP, QT, NCON, CIO, LMR, SOD, AMB, OV), CON, NCON).

store_cio_ncio(store(CD, CV, CR, EC, EF, EI, ES, ED, PP, NP, QT, CON, CIO, LMR, SOD, AMB, OV),
               store(CD, CV, CR, EC, EF, EI, ES, ED, PP, NP, QT, CON, NCIO, LMR, SOD, AMB, OV), CIO, NCIO).

store_lmr_nlmr(store(CD, CV, CR, EC, EF, EI, ES, ED, PP, NP, QT, CON, CIO, LMR, SOD, AMB, OV),
               store(CD, CV, CR, EC, EF, EI, ES, ED, PP, NP, QT, CON, CIO, NLMR, SOD, AMB, OV), LMR, NLMR).

store_sod_nsod(store(CD, CV, CR, EC, EF, EI, ES, ED, PP, NP, QT, CON, CIO, LMR, SOD, AMB, OV),
               store(CD, CV, CR, EC, EF, EI, ES, ED, PP, NP, QT, CON, CIO, LMR, NSOD, AMB, OV), SOD, NSOD).

store_amb_namb(store(CD, CV, CR, EC, EF, EI, ES, ED, PP, NP, QT, CON, CIO, LMR, SOD, AMB, OV),
               store(CD, CV, CR, EC, EF, EI, ES, ED, PP, NP, QT, CON, CIO, LMR, SOD, NAMB, OV), AMB, NAMB).

store_ov_nov(store(CD, CV, CR, EC, EF, EI, ES, ED, PP, NP, QT, CON, CIO, LMR, SOD, AMB, OV),
               store(CD, CV, CR, EC, EF, EI, ES, ED, PP, NP, QT, CON, CIO, LMR, SOD, AMB, NOV), OV, NOV).


store_cd(store(CD, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _), CD).

store_cv(store(_, CV, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _), CV).

store_cr(store(_, _, CR, _, _, _, _, _, _, _, _, _, _, _, _, _, _), CR).

store_ec(store(_, _, _, EC, _, _, _, _, _, _, _, _, _, _, _, _, _), EC).

store_ef(store(_, _, _, _, EF, _, _, _, _, _, _, _, _, _, _, _, _), EF).

store_ei(store(_, _, _, _, _, EI, _, _, _, _, _, _, _, _, _, _, _), EI).

store_es(store(_, _, _, _, _, _, ES, _, _, _, _, _, _, _, _, _, _), ES).

store_ed(store(_, _, _, _, _, _, _, ED, _, _, _, _, _, _, _, _, _), ED).

store_pp(store(_, _, _, _, _, _, _, _, PP, _, _, _, _, _, _, _, _), PP).

store_np(store(_, _, _, _, _, _, _, _, _, NP, _, _, _, _, _, _, _), NP).

store_qt(store(_, _, _, _, _, _, _, _, _, _, QT, _, _, _, _, _, _), QT).

store_con(store(_, _, _, _, _, _, _, _, _, _, _, CON, _, _, _, _, _), CON).

store_cio(store(_, _, _, _, _, _, _, _, _, _, _, _, CIO, _, _, _, _), CIO).

store_lmr(store(_, _, _, _, _, _, _, _, _, _, _, _, _, LMR, _, _, _), LMR).

store_sod(store(_, _, _, _, _, _, _, _, _, _, _, _, _, _, SOD, _, _), SOD).

store_amb(store(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, AMB, _), AMB).

store_ov(store(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, OV), OV).

store_new_dvrcfisdpn(store(_, _, _, _, _, _, _, _, _, _, QT, CR, CIO, LMR, SOD, AMB, OV),
                  store(NCD, NCV, NCR, NEC, NEF, NEI, NES, NED, NPP, NNP, QT, CR, CIO, LMR, SOD, AMB, OV),
                  NCD, NCV, NCR, NEC, NEF, NEI, NES, NED, NPP, NNP).
