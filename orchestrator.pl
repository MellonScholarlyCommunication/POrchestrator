#!/usr/bin/env swipl

:- use_module(library(semweb/turtle)).
:- use_module(library(process)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(rdf_walk).

:- initialization(main,main).

% prefixes
pfx('pol','https://www.example.org/ns/policy#').
pfx('fno','https://w3id.org/function/ontology#').

% path to eye reaseoner
eye("/usr/local/bin/eye").

string_uri(String,URI) :-
  split_string(String,":","",[P,U]),
  atom_string(PA,P),
  pfx(PA,NS),
  atomic_list_concat([NS,U],URI).

% start reasoning on the input file and capture the output
n3_reasoning(File,Rules,Output) :-
    eye(Path),
    flatten(
      [ '--pass-only-new' , '--nope' , '--quiet' ,
        file(File) , Rules ] ,
      Args
    ),
    concat_atom(['__','#'], AnonBase),
    setup_call_cleanup(
      process_create(
          Path, Args ,
          [ stdout(pipe(Out)) , stderr(null) ]
      ),
      rdf_read_turtle(Out,Output,[anon_prefix(AnonBase)]),
      close(Out)
    ).

% query used in rdf_save_turtle
triple_in(RDF, S,P,O,_G) :-
      member(rdf(S,P,O), RDF).

% split the graph up into policies
split_policy(Graph,Parts) :- 
    string_uri("pol:policy",Policy),
    findall(G,rdf_walk(Graph,rdf(_,Policy,_),G),Parts).

% print a graph to a stream
rdf2turtle(_,[]).
rdf2turtle(Stream,Graph) :-
    rdf_save_turtle(
            Stream,[
              expand(triple_in(Graph)),
              inline_bnodes(true) ,
              silent(true)
            ]).

'http://example.org/appendToLog'(Id,_) :-
    writeln(">>Appending Log"),
    format("..~w~n",[Id]).

'http://example.org/sendEmail'(Id,_) :-
    writeln(">>Sending Mail"),
    format("..~w~n",[Id]).

execute_policy(Graph) :-
    string_uri("fno:executes",Exec),
    string_uri("pol:policy",Pol),
    rdf_match(Graph,rdf(Id,Pol,_)),
    rdf_match(Graph,rdf(_,Exec,Func)),
    call(Func,Id,Graph).

main([]) :-
    writeln(user_error,"usage: orchestrator.pl data/N3 RULES").

main([File|Rules]) :-
    n3_reasoning(File,Rules,Graph),
    split_policy(Graph,Parts),
    % maplist(rdf2turtle(stream(current_output)),Parts),
    maplist(execute_policy,Parts).

   
