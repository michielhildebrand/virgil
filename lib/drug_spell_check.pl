:- module(drug_spell_check,
	  [correct_drug_names/0
	  ]).


:- use_module(library(aspell)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(semweb/rdf_litindex)).

user:file_search_path(dict, '../dict').

correct_drug_names :-
	absolute_file_name(dict(.), DictDir),
	atom_concat('--dict-dir=',DictDir,DictOpt),
	create_aspell_process([DictOpt,
			       '--master=drugbank'],
			      PID),
	findall(Lit, rdf(_,aers:drugname,Lit), Drugs0),
	sort(Drugs0, Drugs),
	cleanup_lit(Drugs, Drugs1),
	length(Drugs, UniqueCount),
	length(Drugs1, CleanupCount),
	debug(drugcorrect, '~w unique drug names', [UniqueCount]),
	debug(drugcorrect, '~w after cleanup', [CleanupCount]),
	spell_check(Drugs1, PID, Suggestions),
	length(Suggestions, SuggestCount),
	debug(drugcorrect, '~w corrected', [SuggestCount]),
	maplist(assert_suggestion, Suggestions).

cleanup_lit([], []).
cleanup_lit([Lit|T], [A-Lit|Rest]) :-
	literal_text(Lit, H),
	atom_length(H, Length),
	Length > 2,
	tokenize_atom(H, [A0]),
	downcase_atom(A0, A),
	!,
	cleanup_lit(T, Rest).
cleanup_lit([_|T], Rest) :-
	cleanup_lit(T, Rest).

spell_check([], _, []).
spell_check([A-Lit|T], PID, [Lit-Suggestion|Rest]) :-
	aspell(PID, A, Suggestions),
	Suggestions = [Suggestion|_], % we only keep the first suggestion
	!,
	%debug(drugcorrect, '~w -> ~w', [A,Suggestion]),
	spell_check(T, PID, Rest).
spell_check([_|T], PID, Rest) :-
	spell_check(T, PID, Rest).

assert_suggestion(Lit-Suggestion) :-
	forall(rdf(R,aers:drugname,Lit),
	       rdf_assert(R,aers:drugname_corrected,literal(Suggestion))).



in_drugbank(Q) :-
	tokenize_atom(Q,DL),
	member(Word,DL),
	find_drug_by_name(Word, _).

find_drug_by_name(Q, Drug) :-
	rdf_find_literals(case(Q), Literals),
	member(Lit, Literals),
	drug_name(Lit, Drug).

drug_name(L, Drug) :-
	rdf(Drug,rdfs:label,literal(L)),
	rdf(Drug,rdf:type,drugbank:'drugbank/drugs'),
	!.
drug_name(L, Drug) :-
	rdf(Drug,drugbank:'drugbank/synonym',literal(L)),
	!.
drug_name(L, Drug) :-
	rdf(Drug,drugbank:'drugbank/brandName',literal(L)).
