% Virusland - CSCK502 January 2026 Programming Assignment

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

% Patients
patient(john).
patient(jane).
patient(jack).
patient(jill).
patient(jim).
patient(jess).
patient(joe).
patient(susan).

age(john, 45).
age(jane, 70).
age(jack, 30).
age(jill, 25).
age(jim, 60).
age(jess, 50).
age(joe, 80).
age(susan, 40).

sex(john, male).
sex(jane, female).
sex(jack, male).
sex(jill, female).
sex(jim, male).
sex(jess, female).
sex(joe, male).
sex(susan, female).

% John Doe has the following symptoms
has_symptom(john, fever).
has_symptom(john, dry_cough).
has_symptom(john, dry_cough).
has_symptom(john, tiredness).
has_symptom(john, chest_pressure_pain).

% Jane Doe has the following symptoms
has_symptom(jane, fever).
has_symptom(jane, dry_cough).
has_symptom(jane, tiredness).

% Jill Doe has the following symptoms
has_symptom(jill, anosmia).

% Jim Doe has the following symptoms
has_symptom(jim, fever).

% Jess Doe has the following symptoms
has_symptom(jess, fever).

% Susan Doe has the following comorbidities
has_comorbidity(susan, diabetes).

% Exposure history (exposure(Patient, Source, DaysAgo))
exposure(jane, john, 5).
exposure(jack, jane, 15).
exposure(jill, jack, 10).
exposure(jim, jill, 5).
exposure(jess, jim, 15).

% Count the number of common symptoms for a patient
common_symptom_count(P, Count) :-
    findall(S,
            (has_symptom(P, S), symptom(common, S)),
            L),
    sort(L, Sorted),
    length(Sorted, Count).

% Count the number of less common symptoms for a patient
less_common_symptom_count(P, Count) :-
    findall(S,
            (has_symptom(P, S), symptom(less_common, S)),
            L),
    sort(L, Sorted),
    length(Sorted, Count).

% Count the number of serious symptoms for a patient
serious_symptom_count(P, Count) :-
    findall(S,
            (has_symptom(P, S), symptom(serious, S)),
            L),
    sort(L, Sorted),
    length(Sorted, Count).

% Check for any common symptom
has_any_common_symptom(P) :-
    has_symptom(P, S),
    symptom(common, S).

% Check for multiple common symptoms
has_multiple_common_symptoms(P) :-
    common_symptom_count(P, Count),
    Count > 1.

% Check if the patient has any serious symptoms
has_serious_symptoms(P) :-
    serious_symptom_count(P, Count),
    Count > 0.

% Check for anosmia or hyposmia
has_anosmia_or_hyposmia(P) :-
    (
        has_symptom(P, anosmia);
        has_symptom(P, hyposmia)
    ).

% Determine if a patient has the virus based on symptoms and exposure history
virus_present(P) :-
    has_serious_symptoms(P), !.

virus_present(P) :-
    has_anosmia_or_hyposmia(P), !.

virus_present(P) :-
    has_multiple_common_symptoms(P), !.

virus_present(P) :-
    has_any_common_symptom(P),
    at_risk(P).

% Determine if a patient needs triage based on symptoms
needs_triage(P) :-
    has_serious_symptoms(P).

% Determine if a patient is at risk based on the time of the exposure
at_risk(P) :-
    exposure(P, _, DaysAgo),
    (
        DaysAgo >= 1,
        DaysAgo =< 14
    ).

% Determine if a patient has any comorbidities
has_comorbidity(P) :-
    comorbidity(C),
    has_comorbidity(P, C).

% Determine if a patient is at high risk based on age and comorbidities
high_risk_severe(P) :-
    age(P, Age),
    (
        Age > 70;
        has_comorbidity(P)
    ).

% Determine if a male patient is at high risk based on age
male_high_risk_note(P) :-
    high_risk_severe(P),
    sex(P, male).

% Display a summary of the patient's symptoms and risk status
patient_summary(P) :-
    findall(S, has_symptom(P, S), Symptoms),
    sort(Symptoms, SortedSymptoms),
    common_symptom_count(P, CommonCount),
    less_common_symptom_count(P, LessCommonCount),
    serious_symptom_count(P, SeriousCount),
    format('Patient: ~w~n', [P]),
    format('Symptoms: ~w~n', [SortedSymptoms]),
    format('Common Symptoms: ~w~n', [CommonCount]),
    format('Less Common Symptoms: ~w~n', [LessCommonCount]),
    format('Serious Symptoms: ~w~n', [SeriousCount]),
    (virus_present(P) ->
        format('Virus Status: Positive~n', []);
        format('Virus Status: Negative~n', [])),
    (needs_triage(P) ->
        format('Triage Status: Needs Triage~n', []);
        format('Triage Status: No Triage Needed~n', [])),
    (high_risk_severe(P) ->
        format('Severe Risk: High~n', []);
        format('Severe Risk: Normal~n', [])),
    ( male_high_risk_note(P) ->
        format('Note: Male patients in high-risk groups may have slightly higher risk of severe illness.~n', []);
        true),
    nl.
