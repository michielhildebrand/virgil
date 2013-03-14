:- module(measurements,
	  [flush_virgil_cache/0
	  ]).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(aers_report)).
:- use_module(library(count)).

:- http_handler(cliopatria(aers/api/measurements/prr), http_measurements_prr, []).

:- dynamic
	virgil_cache/2.

http_measurements_prr(Request) :-
	http_parameters(Request,
			['drugnames[]'(Drugs,
				   [list(atom)]),
			 'reactions[]'(Reactions,
				       [list(atom)
				       ])
			]),
	prr_per_reaction(Reactions, Drugs, PRRs),
	reply_json(PRRs).


prr_per_reaction([], _, []).
prr_per_reaction([Reaction|Rs], Drugs, [Obj|Os]) :-
	Obj = json([reaction=Reaction, prr=PRR, prov=Prov]),
	prr(Drugs, Reaction, PRR, Prov),
	prr_per_reaction(Rs, Drugs, Os).

prr(Drugs, Reaction, PRR, Prov) :-
	drug_reactions(Drugs, DRs),
	length(DRs, D),
	proof_count(member(Reaction-_, DRs), DE),
	reaction_count(Reaction, E),
	cc_count(A),

	PRR is (DE/D)/((E-DE)/(A-D)),

	Prov = json([drug=D,
		     reaction=E,
		     drugreaction=DE,
		     all=A
		    ]).
	%Prov = (DE/D)/((E-DE)/(A-D)).


drug_reactions(Drugs, DRs) :-
	virgil_cache(drug(Drugs), DRs),
	!.
drug_reactions(Drugs, DRs) :-
	setof(R-Report, reaction(Drugs, [], R, Report), DRs),
	assert(virgil_cache(drug(Drugs), DRs)).

reaction_count(Reaction, Count) :-
	virgil_cache(reaction(Reaction), Count),
	!.
reaction_count(Reaction, Count) :-
	setof(R, report([], [Reaction], [], R), Es),
	length(Es, Count),
	assert(virgil_cache(reaction(Reaction), Count)).

cc_count(Count) :-
	virgil_cache(cc, Count),
	!.
cc_count(Count) :-
	setof(R, report([], [], [], R), As),
	length(As, Count),
	assert(virgil_cache(cc, Count)).


flush_virgil_cache :-
	retractall(virgil_cache(_, _)).

