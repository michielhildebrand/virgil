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
	findall(T, drug_name_token(T), DrugTokens0),
	sort(DrugTokens0, DrugTokens),
	length(DrugTokens, TokenCount),
	debug(drugcorrect, 'tokens: ~w', [TokenCount]),
	spell_check(DrugTokens, PID, Suggestions),
	length(Suggestions, SuggestCount),
	debug(drugcorrect, '~w corrected', [SuggestCount]),
	maplist(assert_suggestion, Suggestions).

drug_name_token(A) :-
	rdf(_,aers:drugname,Lit),
	literal_text(Lit, H),
	tokenize_atom(H, As),
	member(A, As),
	atom_length(A, Length),
	Length > 3,
	\+ number(A),
	\+ drug_name(A, _).

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
	rdf_find_literals(case(H), Literals),
	forall((member(Lit, Literals),
	        rdf(R,aers:drugname,literal(Lit))),
	       rdf_assert(R,aers:drugname_corrected,literal(Suggestion),corrected_drugnames)).

drug_name(L, Drug) :-
	rdf(Drug,drugbank:'drugbank/genericName',literal(exact(L), _)),
	rdf(Drug,rdf:type,drugbank:'drugbank/drugs'),
	!.
drug_name(L, Drug) :-
	rdf(Drug,drugbank:'drugbank/synonym',literal(exact(L),_)),
	!.
drug_name(L, Drug) :-
	rdf(Drug,drugbank:'drugbank/brandName',literal(exact(L),_)).
