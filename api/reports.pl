:- module(reports,
	  []).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(util)).
:- use_module(library(http/http_json)).
:- use_module(library(semweb/rdf_describe)).
:- use_module(library(semweb/rdf_json)).
:- use_module(library(aers_report)).

:- http_handler(cliopatria(aers/api/reports), http_reports, []).


%%	http_reports(+Request)

http_reports(Request) :-
	http_parameters(Request,
			[drug(Drugs,
			      [list(atom)
			      ]),
			 reaction(Reactions,
				  [list(atom)
				  ]),
			 limit(Limit,
			       [default(100)]),
			 offset(Offset,
				[default(0)])
			]),
	findall(R, report(Drugs, Reactions, [], R), Reports0),
	sort(Reports0, Reports),
	length(Reports, Total_Reports),

	list_offset(Reports, Offset, Reports_Offset),
	list_limit(Reports_Offset, Limit, Reports_Limit, _),
	%maplist(report_obj, Reports_Limit, JSON_Reports),
	reply_json(json([totalNumberOfResults=Total_Reports,
			 drug=Drugs,
			 reaction=Reactions,
			 reports=Reports_Limit])).

report_obj(R, JSON) :-
	resource_CBD(rdf, R, Graph),
	graph_json(Graph, JSON).




