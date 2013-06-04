:- module(generate_prov,
	  []).


:- use_module(library(http/http_dispatch)).


prov_triples(Term, Tripples) :-
	prov_triples(Term, Tripples, []).

prov_triples(prr(Value, Drug, DrugNames, Reaction, Counts)) -->
	prr(Value, Drug, DrugNames, Reaction, Counts, _).

prr(Value, Drug, DrugSelection, Reaction, Counts, PRR) -->
	{ rdf_bnode(PRR) },
	prr_compute(Drug, DrugSelection, Reaction, Counts, Process),
	[ rdf(PRR, rdf:type, prov:'Entity'),
	  rdf(PRR, rdf:value, literal(Value)),
	  rdf(PRR, prov:wasGeneratedBy, Process)
	].

prr_compute(Drug, DrugSelection, Reaction, Counts, URI) -->
	{ Counts = counts(D,E,DE,A),
	  format(atom(C), '(~w/~w)/((~w-~w)/(~w-~w))',
		  [DE,D,E,DE,A,D]),
	  http_link_to_id(http_measurements_prr, [reaction(Reaction)], URI)
	},
	all_reports(A, Reports),
	drug_reports(Drug, DrugSelection, D, Reports, Drugs),
	reaction_reports(Reaction, E, Reports, Reactions),
	drug_reaction_reports(Drugs, Reactions, DE, DrugReactions),
	[ rdf(URI, rdf:type, prov:'Activity'),
	  rdf(URI, prov:used, Drugs),
	  rdf(URI, prov:used, Reactions),
	  rdf(URI, prov:used, Reports),
	  rdf(URI, prov:used, DrugReactions),
	  rdf(URI, rdf:value, literal(C))
	].

all_reports(Count, URI) -->
	{ rdf_bnode(URI)
	},
	[ rdf(URI, rdf:type, prov:'Entity'),
	  rdf(URI, rdfs:label, literal('AERS Reports')),
	  rdf(URI, rdf:value, literal(Count))
	].

drug_reports(Drug, DrugSelection, Count, Reports, URI) -->
	{ rdf_bnode(URI)
	},
	drug_report_select(Drug, DrugSelection, Reports, DrugSelect),
	[ rdf(URI, rdf:type, prov:'Entity'),
	  rdf(URI, rdf:value, literal(Count)),
	  rdf(URI, prov:wasGeneratedBy, DrugSelect)
	].

reaction_reports(Reaction, Count, Reports, URI) -->
	{ http_link_to_id(http_reports, [reaction(Reaction)], URI)
	},
	[ rdf(URI, rdf:type, prov:'Entity'),
	  rdf(URI, prov:specializationOf, Reports),
	  rdf(URI, rdf:value, literal(Count))
	].

drug_reaction_reports(Drugs, Reactions, Count, URI) -->
	[ rdf(URI, rdf:type, prov:'Entity'),
	  rdf(URI, prov:specializationOf, Drugs),
	  rdf(URI, prov:specializationOf, Reactions),
	  rdf(URI, rdf:value, literal(Count))
	].


drug_report_select(Drug, DrugSelection, Reports, URI) -->
	{ rdf_bnode(URI)
	},
	drugs(Drug, DrugSelection, Reports),
	[ rdf(URI, rdf:type, prov:'Activity'),
	  rdf(URI, prov:used, DrugSelection),
	  rdf(URI, prov:used, Reports)
	].

drugs(Drug, DrugSelection, Reports) -->
	drug_user_select(Drug, Reports, UserSelect),
	[ rdf(DrugSelection, rdf:type, prov:'Entity'),
	  rdf(DrugSelection, prov:wasGeneratedBy, UserSelect)
	].

drug_user_select(Drug, Reports, URI) -->
	{ rdf_bnode(URI)
	},
	drug_names(names, Drug, Reports, Names),
	drug_names(brands, Drug, Reports, Brands),
	drug_names(synonyms, Drug, Reports, Synonyms),
	[ rdf(URI, rdf:type, prov:'Activity'),
	  rdf(URI, prov:used, Names),
	  rdf(URI, prov:used, Brands),
	  rdf(URI, prov:used, Synonyms)
	].

drug_names(Type, Drug, Reports, URI) -->
	{ rdf_bnode(URI)
	},
	drug_name_select(Type, Drug, Reports, DrugSelect),
	[ rdf(URI, rdf:type, prov:'Entity'),
	  rdf(URI, prov:wasGeneratedBy,	DrugSelect)
	].

drug_name_select(Type, Drug, Reports, URI) -->
	{ drug_name_select_uri(Type, Drug, URI)
	},
	[ rdf(URI, rdf:type, prov:'Activity'),
	  rdf(URI, prov:used, Reports)
	].

drug_name_select_uri(names, Drug, URI) :-
	http_link_to_id(http_drug_mentions, [q(Drug)], URI).
drug_name_select_uri(brands, Drug, URI) :-
	http_link_to_id(http_drug_brand_mentions, [q(Drug)], URI).
drug_name_select_uri(synonyms, Drug, URI) :-
	http_link_to_id(http_drug_synonym_mentions, [drug(Drug)], URI).
