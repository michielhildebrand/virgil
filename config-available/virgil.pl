:- module(conf_virgil, []).

/** <module> Adverse Event Mining
*/

:- use_module(library(semweb/rdf_db)).

:- rdf_register_ns(aers, 'http://aers.data2semantics.org/vocab/').
:- rdf_register_ns(aers_r, 'http://aers.data2semantics.org/report/').
:- rdf_register_ns(drugbank, 'http://www4.wiwiss.fu-berlin.de/drugbank/resource/').
:- rdf_register_ns(ob, 'http://pashworth.org/orangebook#').

:- load_files([ api(drugs),
		api(reactions),
		api(reports),
		applications(virgil)
	      ],
	      [ silent(true),
		if(not_loaded)
	      ]).
