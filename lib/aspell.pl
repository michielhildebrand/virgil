:- module(aspell,
	  [create_aspell_process/2,
	   cleanup_aspell_process/1,
	   cleanup_aspell/0,
	   current_aspell_process/2,
	   aspell/3
	  ]).

:- use_module(library(process)).

:- dynamic
	aspell_process/3.

%%	create_aspell_process(+Options, -PID)
%
%	Creates a new aspell process with id PID.
%	A new process is created for each unique option-list.
%
%	If a process already exists for the option-list it simply
%	returns the PID of that process.

create_aspell_process(Options, PID) :-
	current_aspell_process(Options, PID),
	!.
create_aspell_process(Options, PID) :-
	process_create(path(aspell), [pipe|Options],
		  [ stdin(pipe(In)),
		    stdout(pipe(Out)),
		    process(PID)
		  ]),
	assert(aspell_process(PID, in, In)),
	assert(aspell_process(PID, out, Out)),
	assert(aspell_process(PID, options, Options)).

%%	cleanup_aspell_process(+PID)
%
%	Kills the process and removes the administration.

cleanup_aspell_process(PID) :-
	is_process(PID),
	aspell_process(PID, in, In),
	aspell_process(PID, out, Out),
	!,
	retractall(aspell_process(PID,_,_)),
	catch(close(In), _, true),
	catch(close(Out), _, true),
	process_kill(PID).
cleanup_aspell_process(_PID).

cleanup_aspell :-
	forall(aspell_process(PID, in, _),
	       cleanup_aspell_process(PID)).


%%	current_aspell_process(+Options, ?PID)
%
%	True if a process exists with Options and PID.

current_aspell_process(Options, PID) :-
	aspell_process(PID, options, Options).


%%	aspell(+ProcessID, +Word, -SuggestionList)
%
%	SuggestionList contains all candidate corrections provided by
%	aspell.
%
%       Options is an option-list for aspell.

aspell(PID, Word, Suggestions) :-
	is_process(PID),
	aspell_process(PID, in, In),
	aspell_process(PID, out, Out),
	catch(format(In, '~w~n', [Word]), _,
	      (cleanup_aspell_process(PID),
	       fail)),
	catch(flush_output(In), _,
	      (cleanup_aspell_process(PID),
	       fail
	      )),
	!,
	read_suggestions(Out, Suggestions).
aspell(PID, _, _) :-
	existence_error('aspell process', PID).

read_suggestions(Out, Suggestions) :-
	read_line_to_codes(Out, Line),
	read_suggestions(Line, Out, Suggestions).

read_suggestions([], _, []) :- !.
read_suggestions(Line, Out, Suggestions) :-
	phrase(suggestion_line(Suggestions), Line),
	!,
	read_line_to_codes(Out,Next),
        read_to_end(Next,Out).
read_suggestions(_Line,Out, Suggestions) :-
	read_line_to_codes(Out, NextLine),
	read_suggestions(NextLine, Out, Suggestions).

suggestion_line(Suggestions) -->
	"& ",
	!,
	codes_to_suggestions(Suggestions).

codes_to_suggestions(Suggestions) -->
	": ",
	!,
	codes_suggest_list(Suggestions).
codes_to_suggestions(Suggestions) -->
	[_],
	!,
	codes_to_suggestions(Suggestions).
codes_to_suggestions([]) -->
	"".

codes_suggest_list([H|T]) -->
	suggestion_codes(C),
	{ C \== [], !,
	  atom_codes(H, C)
	},
	codes_suggest_list(T).
codes_suggest_list([]) -->
	"".


suggestion_codes([]) -->
	", ",
	!.
suggestion_codes([H|T]) -->
	[H],
	!,
	suggestion_codes(T).
suggestion_codes([]) -->
	[].

read_to_end([], _) :- !.
read_to_end(_, S) :-
	read_line_to_codes(S, L),
	read_to_end(L, S).
