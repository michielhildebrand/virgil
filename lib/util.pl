:- module(virgil_util,
	  [pairs_sort_by_value_count/2,
	   list_limit/4,
	   list_offset/3
	  ]).


%%	pairs_sort_by_value_count(+Pairs, -SortedByValueCount)
%
%

pairs_sort_by_value_count(Pairs, KeysByValueCount) :-
	group_pairs_by_key(Pairs, Groups),
	maplist(value_count, Groups, KeysByValueCount0),
	keysort(KeysByValueCount0, KeysByValueCount).

value_count(K-V, C-K) :-
	is_list(V),
	!,
	length(V, C).

%%	list_offset(+List, +N, -SmallerList)
%
%	SmallerList starts at the nth element of List.

list_offset(L, N, []) :-
	length(L, Length),
	Length < N,
	!.
list_offset(L, N, L1) :-
	list_offset_(L, N, L1).

list_offset_(L, 0, L) :- !.
list_offset_([_|T], N, Rest) :-
	N1 is N-1,
	list_offset_(T, N1, Rest).

%%	list_limit(+List, +N, -SmallerList, -Rest)
%
%	SmallerList ends at the nth element of List.

list_limit(L, N, L, []) :-
	N < 0,
	!.
list_limit(L, N, L, []) :-
	length(L, Length),
	Length < N,
	!.
list_limit(L, N, L1, Rest) :-
	list_limit_(L, N, L1, Rest).

list_limit_(Rest, 0, [], Rest) :- !.
list_limit_([H|T], N, [H|T1], Rest) :-
	N1 is N-1,
	list_limit_(T, N1, T1, Rest).
