:- module(virgil,
	  []).


:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_server_files)).

:- multifile
	http:location/3.		% Alias, Expansion, Options
:- dynamic
	http:location/3.		% Alias, Expansion, Options

http:location(html, root(html), [ priority(-100) ]).
http:location(img, root(img), [ priority(-100) ]).

:- multifile
	user:file_search_path/2.
:- dynamic
	user:file_search_path/2.

user:file_search_path(img, web(img)).


:- http_handler(html(.), serve_files_in_directory(html),
		[prefix, priority(-100)]).

:- http_handler(img(.), serve_files_in_directory(img),
		[prefix, priority(-100)]).

:- http_handler(root(virgil), virgil, []).

virgil(Request) :-
	http_reply_file(html('virgil.html'), Request, []).
