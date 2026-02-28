% Virusland - CSCK502 January 2026 Programming Assignment

/* =========================================================
   Dynamic declarations (needed for interactive mode)
   ========================================================= */

:- dynamic patient/1.
:- dynamic age/2.
:- dynamic sex/2.
:- dynamic has_symptom/2.
:- dynamic has_comorbidity/2.
:- dynamic exposure/3.

/* =========================================================
   Knowledge Base
   ========================================================= */

% Common symptoms
symptom(common, fever).
symptom(common, dry_cough).
symptom(common, tiredness).

% Less common symptoms
symptom(less_common, aches_pains).
symptom(less_common, sore_throat).
symptom(less_common, diarrhoea).
symptom(less_common, conjunctivitis).
symptom(less_common, headache).
symptom(less_common, anosmia).
symptom(less_common, hyposmia).
symptom(less_common, runny_nose).

% Serious symptoms
symptom(serious, difficulty_breathing).
symptom(serious, chest_pressure_pain).
symptom(serious, loss_speech_movement).

% Comorbidities
comorbidity(diabetes).
comorbidity(hypertension).
comorbidity(cardiovascular_disease).
comorbidity(chronic_respiratory_disease).
comorbidity(cancer).

/* =========================================================
   Sample Patients (static test cases)
   ========================================================= */

% Patients (grouped)
patient(john).
patient(jane).
patient(jack).
patient(jill).
patient(jim).
patient(jess).
patient(joe).
patient(susan).
patient(surf_ok).
patient(surf_no).

% Ages (grouped)
age(john, 45).
age(jane, 70).
age(jack, 30).
age(jill, 25).
age(jim, 60).
age(jess, 50).
age(joe, 80).
age(susan, 40).
age(surf_ok, 35).
age(surf_no, 35).

% Sex (grouped)
sex(john, male).
sex(jane, female).
sex(jack, male).
sex(jill, female).
sex(jim, male).
sex(jess, female).
sex(joe, male).
sex(susan, female).
sex(surf_ok, male).
sex(surf_no, female).

% Symptoms (grouped)
has_symptom(john, fever).
has_symptom(john, dry_cough).
has_symptom(john, dry_cough).
has_symptom(john, tiredness).
has_symptom(john, chest_pressure_pain).

has_symptom(jane, fever).
has_symptom(jane, dry_cough).
has_symptom(jane, tiredness).

has_symptom(jill, anosmia).

has_symptom(jim, fever).
has_symptom(jess, fever).

has_symptom(surf_ok, fever).
has_symptom(surf_no, fever).

% Comorbidities per patient (grouped)
has_comorbidity(susan, diabetes).

% Exposure history (grouped)  exposure(Patient, Type, DaysAgo)
exposure(jane, close_contact, 5).
exposure(jack, close_contact, 15).
exposure(jill, close_contact, 10).
exposure(jim, close_contact, 5).
exposure(jess, close_contact, 15).

% Surface test cases (requirement 2)
exposure(surf_ok, surface_contact, 2).
exposure(surf_no, surface_contact, 4).

/* =========================================================
   Symptom counting
   ========================================================= */

common_symptom_count(P, Count) :-
    findall(S, (has_symptom(P, S), symptom(common, S)), L),
    sort(L, Sorted),
    length(Sorted, Count).

less_common_symptom_count(P, Count) :-
    findall(S, (has_symptom(P, S), symptom(less_common, S)), L),
    sort(L, Sorted),
    length(Sorted, Count).

serious_symptom_count(P, Count) :-
    findall(S, (has_symptom(P, S), symptom(serious, S)), L),
    sort(L, Sorted),
    length(Sorted, Count).

/* =========================================================
   Symptom logic
   ========================================================= */

has_any_common_symptom(P) :-
    has_symptom(P, S),
    symptom(common, S).

has_multiple_common_symptoms(P) :-
    common_symptom_count(P, Count),
    Count > 1.

has_serious_symptoms(P) :-
    serious_symptom_count(P, Count),
    Count > 0.

has_anosmia_or_hyposmia(P) :-
    ( has_symptom(P, anosmia)
    ; has_symptom(P, hyposmia)
    ).

/* =========================================================
   Exposure logic (Option A - typed exposure)
   ========================================================= */

% Close contact: incubation window 1–14 days
valid_exposure(P) :-
    exposure(P, close_contact, D),
    D >= 1,
    D =< 14.

% Surface contact: survives "a few days" (simplified to <= 3)
valid_exposure(P) :-
    exposure(P, surface_contact, D),
    D >= 1,
    D =< 3.

/* =========================================================
   Diagnosis rules
   ========================================================= */

virus_present(P) :-
    has_serious_symptoms(P), !.
virus_present(P) :-
    has_anosmia_or_hyposmia(P), !.
virus_present(P) :-
    has_multiple_common_symptoms(P), !.
virus_present(P) :-
    has_any_common_symptom(P),
    valid_exposure(P).

needs_triage(P) :-
    has_serious_symptoms(P).

/* =========================================================
   Severe risk logic
   ========================================================= */

has_comorbidity_any(P) :-
    comorbidity(C),
    has_comorbidity(P, C).

high_risk_severe(P) :-
    age(P, Age),
    ( Age > 70
    ; has_comorbidity_any(P)
    ).

male_high_risk_note(P) :-
    high_risk_severe(P),
    sex(P, male).

/* =========================================================
   Patient summary (demonstration output)
   ========================================================= */

patient_summary(P) :-
    findall(S, has_symptom(P, S), Symptoms),
    sort(Symptoms, SortedSymptoms),
    findall((T,D), exposure(P, T, D), ExRaw),
    sort(ExRaw, Exposures),

    common_symptom_count(P, C1),
    less_common_symptom_count(P, C2),
    serious_symptom_count(P, C3),

    format('Patient: ~w~n', [P]),
    format('Symptoms: ~w~n', [SortedSymptoms]),
    format('Exposures: ~w~n', [Exposures]),
    ( valid_exposure(P) ->
        format('Valid Exposure: Yes~n', [])
    ;   format('Valid Exposure: No~n', [])
    ),
    format('Common Symptoms: ~w~n', [C1]),
    format('Less Common Symptoms: ~w~n', [C2]),
    format('Serious Symptoms: ~w~n', [C3]),

    ( virus_present(P) ->
        format('Virus Status: Positive~n', [])
    ;   format('Virus Status: Negative~n', [])
    ),

    ( needs_triage(P) ->
        format('Triage Status: Needs Triage~n', [])
    ;   format('Triage Status: No Triage Needed~n', [])
    ),

    ( high_risk_severe(P) ->
        format('Severe Risk: High~n', [])
    ;   format('Severe Risk: Normal~n', [])
    ),

    ( male_high_risk_note(P) ->
        format('Note: Male patients in high-risk groups may have slightly higher risk of severe illness.~n', [])
    ;   true
    ),
    nl.

/* =========================================================
   Interactive mode
   ========================================================= */

run :-
    nl,
    write('--- Virusland Expert System (Interactive Mode) ---'), nl,
    write('Enter patient id (atom, e.g., tom): '), read(P),
    ensure_patient(P),

    write('Enter age: '), read(A),
    retractall(age(P, _)),
    assertz(age(P, A)),

    write('Enter sex (male/female): '), read(S),
    retractall(sex(P, _)),
    assertz(sex(P, S)),

    retractall(has_symptom(P, _)),
    retractall(exposure(P, _, _)),

    nl, write('Answer symptoms (y/n):'), nl,
    ask_all_symptoms(P),

    nl, write('Exposure type (none/close_contact/surface_contact): '),
    read(Type),
    ( Type == none ->
        true
    ; write('Days ago: '), read(D),
      assertz(exposure(P, Type, D))
    ),

    nl,
    patient_summary(P).

ensure_patient(P) :-
    ( patient(P) -> true ; assertz(patient(P)) ).

ask_all_symptoms(P) :-
    symptom(_, S),
    format('~w? (y/n): ', [S]),
    read(Ans),
    ( Ans == y -> assertz(has_symptom(P, S)) ; true ),
    fail.
ask_all_symptoms(_).
