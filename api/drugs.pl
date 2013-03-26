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
:- http_handler(cliopatria(aers/api/drug/brand/mentions), http_drug_brand_mentions, []).
:- http_handler(cliopatria(aers/api/drug/synonym/mentions), http_drug_synonym_mentions, []).

%%	http_drug_mentions(+Request)

http_drug_mentions(Request) :-
	http_parameters(Request,
			[q(Q, []),
			 method(Method,
			       [default(word),
				one_of([exact,word,stem,sounds,corrected])
			       ])
			]),
	drug_mentions(Q, Method, Mentions),
	maplist(mention_json, Mentions, JSON),
	reply_json(JSON).

drug_mentions(Q, Method, Mentions) :-
	findall(Count-M,
		(   setof(R, drug_mention(Method, Q, M, R), Rs),
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
	member(L, Literals),
	(   Method = corrected
	->  rdf(DrugUse, aers:drugname_corrected, literal(L)),
	    rdf(DrugUse, aers:drugname, literal(Lit))
	;   rdf(DrugUse, aers:drugname, literal(L)),
	    Lit = L
	),
	rdf(Report, aers:drug, DrugUse).

drug_list_mentions([], _, []).
drug_list_mentions([Q|Qs], Method, [Q-Mentions|Rest]) :-
	drug_mentions(Q, Method, Mentions),
	drug_list_mentions(Qs, Method, Rest).



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

http_drug_brand_mentions(Request) :-
	http_parameters(Request,
			[q(Q,
			   []),
			 source(Source,
				[oneof([drugbank,orangebook]),
				 default(drugbank)
				]),
			 method(Method,
			       [default(word),
				one_of([exact,word,stem,sounds,corrected])
			       ])
			]),
	findall(BN, drug_brand_name(Source, Q, BN), BrandNames0),
	sort(BrandNames0, BrandNames),
	drug_list_mentions(BrandNames, Method, Brand_Mentions),
	maplist(drug_mention_json, Brand_Mentions, JSON),
	reply_json(JSON).


drug_brand_name(drugbank, Q, BrandName) :-
	downcase_atom(Q, DQ),
	drugbank_by_name(Q, Drug),
	rdf(Drug, drugbank:'drugbank/brandName', BrandName0),
	literal_text(BrandName0, BrandName),
	downcase_atom(BrandName, DB),
	DQ \== DB.
drug_brand_name(orangebook, Q, BrandName) :-
	downcase_atom(Q, DQ),
	orange_book_by_name(Q, Drug),
	rdf(Dose, ob:hasIngredient, Drug),
	rdf(Product, ob:hasDose, Dose),
	rdf(Product, ob:tradeName, BrandName0),
	literal_text(BrandName0, BrandName),
	downcase_atom(BrandName, DB),
	DQ \== DB.


drugbank_by_name(Drug, Drug) :-
	rdf(Drug, rdf:type, drugbank:'drugbank/drugs'),
	!.
drugbank_by_name(Q, Drug) :-
	rdf(Drug, drugbank:'drugbank/genericName', literal(exact(Q),_)).

orange_book_by_name(Drug, Drug) :-
	rdf(Drug, rdf:type, ob:'Ingredient'),
	!.
orange_book_by_name(Q, Drug) :-
	rdf(Drug, ob:'ingredientName', literal(exact(Q),_)).

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

http_drug_synonym_mentions(Request) :-
	http_parameters(Request,
			[q(Q,
			   []),
			 source(Source,
				[oneof([drugbank,orangebook]),
				 default(drugbank)
				]),
			 method(Method,
			       [default(word),
				one_of([exact,word,stem,sounds,corrected])
			       ])
			]),
	findall(BN, drug_synonym(Source, Q, BN), Synonyms0),
	sort(Synonyms0, Synonyms),
	drug_list_mentions(Synonyms, Method, Synonym_Mentions),
	maplist(drug_mention_json, Synonym_Mentions, JSON),
	reply_json(JSON).

drug_synonym(drugbank, Q, Synonym) :-
	downcase_atom(Q, DQ),
	drugbank_by_name(Q, Drug),
	rdf(Drug, drugbank:'drugbank/synonym', Synonym0),
	literal_text(Synonym0, Synonym),
	downcase_atom(Synonym, DS),
	DQ \== DS.

		 /*******************************
		 *	      JSON terms	*
		 *******************************/

mention_json(Count-Name, json([name=Name, reports=Count])).

drug_mention_json(Drug-Mentions, json([drug=Drug, mentions=JSON])) :-
	maplist(mention_json, Mentions, JSON).
