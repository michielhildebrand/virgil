:- module(aers_report,
	  [report/4,
	   report_by_drug/2,
	   report_by_filter/2,
	   reaction/4,
	   drug_reaction_counts/2,
	   reaction_count/2,
	   cc_count/1,
	   flush_virgil_cache/0
	  ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).

:- dynamic
	virgil_cache/2.

flush_virgil_cache :-
	retractall(virgil_cache(_, _)).

%%	report(+DrugMentionList, -ReactionTerms, +ReportFilter, -Report)

report(Drugs, Reactions, Filter, Report) :-
	(   Drugs = [_|_]
	->  report_by_drug(Drugs, Report)
	;   true
	),
	(   Reactions = [_|_]
	->  report_by_reaction(Reactions, Report)
	;   true
	),
	report_by_filter(Filter, Report).

report_by_drug([Drug|_], Report) :-
	rdf(DrugUse, aers:drugname, literal(exact(Drug),_)),
	rdf(Report, aers:drug, DrugUse).
report_by_drug([_|T], Report) :-
	report_by_drug(T, Report).

report_by_reaction([Reaction|_], Report) :-
	rdf(Report, aers:reaction, literal(exact(Reaction),_)).
report_by_reaction([_|T], Report) :-
	report_by_reaction(T, Report).

report_by_filter([], Report) :-
	rdf(Report, rdf:type, aers:'Report').


%%	reaction(+DrugMentionList, +Filter, ?Reaction, -Report)

reaction(Drugs, Filter, Reaction, Report) :-
	(   Drugs = [_|_]
	->  report_by_drug(Drugs, Report)
	;   true
	),
	report_by_filter(Filter, Report),
	rdf(Report, aers:reaction, Reaction_Lit),
	literal_text(Reaction_Lit, Reaction).


%%	drug_reacton_counts(+DrugMentionList, -Pairs:count-reaction)

drug_reaction_counts(Drugs, DRs) :-
	virgil_cache(drug(Drugs), DRs),
	!.
drug_reaction_counts(Drugs, DRs) :-
	findall(Count-Reaction,
		(setof(R, reaction(Drugs, [], Reaction, R), Rs),
		 length(Rs, Count)
		), DRs),
	assert(virgil_cache(drug(Drugs), DRs)).


%%	reaction_count(+Reaction, -ReactionCount)

reaction_count(Reaction, Count) :-
	virgil_cache(reaction(Reaction), Count),
	!.
reaction_count(Reaction, Count) :-
	setof(R-D, (report([], [Reaction], [], R),
		    report_drug_name(R, D)
		   ),
	      Es),
	length(Es, Count),
	assert(virgil_cache(reaction(Reaction), Count)).

%%	cc_count(-Count)

cc_count(Count) :-
	virgil_cache(cc, Count),
	!.
cc_count(Count) :-
	setof(t(R,D,E), report_drug_reaction([], R, D, E), As),
	length(As, Count),
	assert(virgil_cache(cc, Count)).


report_drug_reaction(Filter, Report, DrugName, Reaction) :-
	report_by_filter(Filter, Report),
	report_drug_name(Report, DrugName),
	report_reaction(Report, Reaction).

report_drug_name(Report, Drug) :-
	rdf(Report, aers:drug, DrugUse),
	rdf(DrugUse, aers:drugname, literal(Drug)).

report_reaction(Report, Reaction) :-
	rdf(Report, aers:reaction, literal(Reaction)).

