:- module(drug_normalise,
	  [drug_normalise/2
	  ]).

drug_normalise(DrugName, Normalised) :-
	tokenize_atom(DrugName, Tokens),
	list_to_set(Tokens, TokenSet0),
	convert(TokenSet0, TokenSet1),
	exclude(punct, TokenSet1, TokenSet),
	atomic_list_concat(TokenSet, ' ', Normalised).

convert([], []).
convert([N0|T], [N|Rest]) :-
	number(N0),
	!,
	N is abs(N0),
	convert(T, Rest).
convert(['MG','/','M2'|T], ['MG'|Rest]) :-
	!,
	convert(T, Rest).
convert([H|T], [H|Rest]) :-
	convert(T, Rest).

punct('!').
punct('.').
punct(',').
punct('-').
punct('_').
punct(')').
punct('(').
