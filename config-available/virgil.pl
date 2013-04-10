:- module(conf_virgil,
	  [virgil_warmup/0
	  ]).

/** <module> Adverse Event Mining
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_litindex)).

:- rdf_register_ns(aers, 'http://aers.data2semantics.org/vocab/').
:- rdf_register_ns(aers_r, 'http://aers.data2semantics.org/report/').
:- rdf_register_ns(drugbank, 'http://www4.wiwiss.fu-berlin.de/drugbank/resource/').
:- rdf_register_ns(ob, 'http://pashworth.org/orangebook#').

:- load_files([ api(drugs),
		api(reactions),
		api(reports),
		api(measurements),
		library(drug_spell_check),
		library(aers_report),
		applications(virgil)
	      ],
	      [ silent(true),
		if(not_loaded)
	      ]).

virgil_warmup :-
	rdf_warm_indexes,
	rdf_find_literals(a,_),
	set_prolog_stack(global, limit(4 000 000 000)),
	cc_count(_).
