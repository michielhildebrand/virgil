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
	setof(T, ( member(D,Drugs),
		   drug_token(D, T)
		 ),
	      DrugTokens),
	spell_check(DrugTokens, PID, Suggestions),
	length(Suggestions, SuggestCount),
	debug(drugcorrect, '~w corrected', [SuggestCount]),
	maplist(assert_suggestion, Suggestions).

drug_token(Lit, A) :-
	literal_text(Lit, H),
	tokenize_atom(H, As),
	member(A, As),
	atom_length(A, Length),
	Length > 2.

spell_check([], _, []).
spell_check([H|T], PID, [H-Suggestion|Rest]) :-
	aspell(PID, H, Suggestions),
	member(Suggestion, Suggestions),
	drug_name(Suggestion, _),
	!,
	%debug(drugcorrect, '~w -> ~w', [A,Suggestion]),
	spell_check(T, PID, Rest).
spell_check([_|T], PID, Rest) :-
	spell_check(T, PID, Rest).

assert_suggestion(H-Suggestion) :-
	rdf_find_literals(H, Literals),
	forall((member(Lit, Literals),
	        rdf(R,aers:drugname,literal(Lit))),
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
