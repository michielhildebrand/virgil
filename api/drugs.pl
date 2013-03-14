:- module(drugs,
	  []).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(http/http_json)).
:- use_module(library(semweb/rdf_litindex)).
:- use_module(library(semweb/rdf_label)).

:- http_handler(cliopatria(aers/api/drug/mentions), http_drug_mentions, []).
:- http_handler(cliopatria(aers/api/drug/brands), http_drug_brands, []).
:- http_handler(cliopatria(aers/api/drug/synonyms), http_drug_synonyms, []).

%%	http_drug_mentions(+Request)

http_drug_mentions(Request) :-
	http_parameters(Request,
			['q[]'(Q,
			       [list(atom)]),
			 method(Method,
			       [default(word),
				one_of([exact,word,stem,sounds,corrected])
			       ])
			]),
	drug_mentions(Q, Method, Mentions),
	maplist(mention_json, Mentions, JSON),
	reply_json(JSON).

mention_json(Count-Name, json([name=Name, reports=Count])).

drug_mentions(Qs, Method, Mentions) :-
	findall(Count-M,
		(   member(Q, Qs),
		    setof(R, drug_mention(Method, Q, M, R), Rs),
		    length(Rs, Count)
		),
		Mentions0),
	keysort(Mentions0, Mentions1),
	reverse(Mentions1, Mentions).

drug_mention(exact, Q, Literal, Report) :-
	rdf(DrugUse, aers:drugname, literal(exact(Q), Literal)),
	rdf(Report, aers:drug, DrugUse).
drug_mention(Method, Q, Lit, Report) :-
	(   Method = word
	->  Query = case(Q)
	;   Method = stem
	->  Query = stem(Q)
	;   Method = sounds
	->  Query = sounds(Q)
	;   Method = corrected
	->  Query = case(Q)
	),
	!,
	rdf_find_literals(Query, Literals),
	member(Lit, Literals),
	(   Method = corrected
	->  rdf(DrugUse, aers:drugname_corrected, literal(Lit))
	;   rdf(DrugUse, aers:drugname, literal(Lit))
	),
	rdf(Report, aers:drug, DrugUse).


%%	http_drug_brands(+Request)

http_drug_brands(Request) :-
	http_parameters(Request,
			[q(Q, []),
			 source(Source,
				[oneof([drugbank,orangebook]),
				 default(drugbank)
				])

			]),
	findall(BN, drug_brand_name(Source, Q, BN), BrandNames0),
	sort(BrandNames0, BrandNames),
	reply_json(BrandNames).

drug_brand_name(drugbank, Q, BrandName) :-
	downcase_atom(Q, DQ),
	(   rdf(Q, rdf:type, drugbank:'drugbank/drugs')
	->  Drug = Q
	;   rdf(Drug, drugbank:'drugbank/genericName', literal(exact(Q),_))
	),
	rdf(Drug, drugbank:'drugbank/brandName', BrandName0),
	literal_text(BrandName0, BrandName),
	downcase_atom(BrandName, DB),
	DQ \== DB.
drug_brand_name(orangebook, Q, BrandName) :-
	downcase_atom(Q, DQ),
	(   rdf(Q, rdf:type, ob:'Ingredient')
	->  Drug = Q
	;   rdf(Drug, ob:'ingredientName', literal(exact(Q),_))
	),
	rdf(Dose, ob:hasIngredient, Drug),
	rdf(Product, ob:hasDose, Dose),
	rdf(Product, ob:tradeName, BrandName0),
	literal_text(BrandName0, BrandName),
	downcase_atom(BrandName, DB),
	DQ \== DB.


%%	http_drug_synonyms(+Request)

http_drug_synonyms(Request) :-
	http_parameters(Request,
			[q(Q, []),
			 source(Source,
				[oneof([drugbank,orangebook]),
				 default(drugbank)
				])
			]),
	findall(BN, drug_synonym(Source, Q, BN), Synonyms0),
	sort(Synonyms0, Synonyms),
	reply_json(Synonyms).

drug_synonym(drugbank, Q, Synonym) :-
	(   rdf(Q, rdf:type, drugbank:'drugbank/drugs')
	->  Drug = Q
	;   rdf(Drug, drugbank:'drugbank/synonym', literal(exact(Q),_))
	),
	rdf(Drug, drugbank:'drugbank/synonym', Synonym0),
	literal_text(Synonym0, Synonym),
	Q \== Synonym.


