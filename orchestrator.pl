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
          [ stdout(pipe(Out)) ]
      ),
      rdf_read_turtle(Out,Output,[anon_prefix(AnonBase)]),
      close(Out)
    ).

% query used in rdf_save_turtle
triple_in(RDF, S,P,O,_G) :-
      member(rdf(S,P,O), RDF).

policy_partition(Graph,Parts) :- 
    policy(PS),
    atom_string(PA,PS),
    writeln(PA),
    findall(G,rdf_walk(Graph,rdf(_,PA,_),G),Parts).

rdf2tutle(Graph) :-
    rdf_save_turtle(
            stream(current_output),[
              expand(triple_in(Graph)),
              inline_bnodes(true)
            ]).

main([]) :-
    writeln(user_error,"usage: orchestrator.pl N3").

main([File|_]) :-
    rules_list("rules/",Rules),
    n3_reasoning(File,Rules,Graph),
    policy_partition(Graph,Parts),
    maplist(rdf2tutle,Parts).

   
