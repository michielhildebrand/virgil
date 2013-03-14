:- module(aers_report,
	  [report/4,
	   report_by_drug/2,
	   report_by_filter/2
	  ]).

:- use_module(library(semweb/rdf_db)).

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
