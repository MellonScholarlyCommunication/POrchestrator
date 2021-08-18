#!/usr/bin/env swipl

:- use_module(library(semweb/turtle)).
:- use_module(library(process)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(rdf_walk).

:- initialization(main,main).

% setting policy namespace
policy("https://www.example.org/ns/policy#policy").

% path to eye reaseoner
eye("/usr/local/bin/eye").

% directory with rules
rules("rules").

% listing of all Notation3 rules in the rules directory
rules_list(Directory,Result) :-
    directory_files(Directory,Files),
    sort(Files,SortedFiles),
    include(re_match(".*\\.n3$"),SortedFiles,RuleFiles),
    maplist(string_concat(Directory),RuleFiles,Result).

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
    policy(PS),
    atom_string(PA,PS),
    findall(G,rdf_walk(Graph,rdf(_,PA,_),G),Parts).

% print a gragh to a stream
rdf2tutle(_,[]).
rdf2tutle(Stream,Graph) :-
    rdf_save_turtle(
            Stream,[
              expand(triple_in(Graph)),
              inline_bnodes(true) ,
              silent(true)
            ]).

main([]) :-
    writeln(user_error,"usage: orchestrator.pl data/N3 RULES").

main([File|Rules]) :-
    n3_reasoning(File,Rules,Graph),
    split_policy(Graph,Parts),
    maplist(rdf2tutle(stream(current_output)),Parts).

   
