:- module(reactions,
	  []).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(util)).
:- use_module(library(http/http_json)).
:- use_module(library(aers_report)).

:- http_handler(cliopatria(aers/api/reactions), http_reactions, []).
:- http_handler(cliopatria(aers/api/reaction/count), http_reaction_count, []).

%%	http_reactions(+Request)

http_reactions(Request) :-
	http_parameters(Request,
			['drugnames[]'(DrugNames,
			      [list(atom)
			      ]),
			 limit(Limit,
			       [default(100)]),
			 offset(Offset,
				[default(0)])
			]),
	drug_reaction_counts(DrugNames, Reactions0),
	keysort(Reactions0, Reactions1),
	reverse(Reactions1, Reactions),
	length(Reactions, Total_Reactions),
	list_offset(Reactions, Offset, Reactions_Offset),
	list_limit(Reactions_Offset, Limit, Reactions_Limit, _),
	maplist(reaction_obj, Reactions_Limit, Reactions_JSON),
	reply_json(json([totalNumberOfResults=Total_Reactions,
			 reactions=Reactions_JSON])).

reaction_obj(C-R, json([reaction=R, count=C])).

%%	http_reaction_count(+Request)

http_reaction_count(Request) :-
	http_parameters(Request,
			[drug(Drugs,
			      [list(atom)
			      ]),
			 reaction(Reactions,
				  [list(atom)
				  ])
			]),
	(   Reactions = []
	->  setof(Report-Reaction,
		  reaction(Drugs, [], Reaction, Report),
		  Rs),
	    length(Rs, Count)
	;   setof(R, report(Drugs, Reactions, [], R), Rs),
	    length(Rs, Count)
	),
	reply_json(json([drugs=Drugs,
			 reactions=Reactions,
			 reaction_count=Count])).
