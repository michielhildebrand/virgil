:- module(measurements,
	  [
	  ]).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(aers_report)).
:- use_module(library(count)).

:- http_handler(cliopatria(aers/api/measurements/prr), http_measurements_prr, []).
:- http_handler(cliopatria(aers/api/measurements/prov), http_measurements_prov, []).



http_measurements_prr(Request) :-
	http_parameters(Request,
			['drugnames[]'(Drugs,
				   [list(atom)]),
			 reaction(Reaction,
				  [atom
				  ])
			]),
	prr(Drugs, Reaction, PRR, Prov),
	reply_json(json([reaction=Reaction,
			 prr=json([value=PRR, prov=Prov])])).

http_measurements_prov(Request) :-
	http_parameters(Request,
			['drugnames[]'(Drugs,
				       [list(atom)]),
			 'reactions[]'(Reactions,
				       [list(atom)
				       ])
			]),
	prr_per_reaction(Reactions, Drugs, JSON),
	reply_json(JSON).



prr_per_reaction([], _, []).
prr_per_reaction([Reaction|Rs], Drugs, [Obj|Os]) :-
	Obj = json([reaction=Reaction, prr=PRR, prov=Prov]),
	prr(Drugs, Reaction, PRR, Prov),
	prr_per_reaction(Rs, Drugs, Os).

prr(Drugs, Reaction, PRR, Prov) :-
	% total drug cooccurrences
	drug_reaction_counts(Drugs, DRs),
	pairs_keys(DRs, DR_Counts),
	sumlist(DR_Counts, D),

	% drug-reaction cooccurrences
        memberchk(DE-Reaction, DRs),

	% reaction cooccurrences
	reaction_count(Reaction, E),

	% total cooccurrences
	cc_count(A),

	PRR is (DE/D)/((E-DE)/(A-D)),

	Prov = json([drug=D,
		     reaction=E,
		     drugreaction=DE,
		     all=A
		    ]).
	%Prov = (DE/D)/((E-DE)/(A-D)).






