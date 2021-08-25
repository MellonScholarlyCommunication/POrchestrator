#!/usr/bin/env swipl

:- use_module(library(semweb/turtle)).
:- use_module(library(process)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(rdf_tools).
:- use_module(actions).

:- initialization(main,main).

% path to eye reaseoner
eye("/usr/local/bin/eye").

% start reasoning on the input file and capture the output
n3_reasoning(File,Rules,Output) :-
    eye(Path),
    flatten(
      [ '--pass' , '--nope' , '--quiet' ,
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

% split the graph up into policies
split_policy(Graph,Parts) :- 
    string_uri("pol:policy",Policy),
    findall(rdf(S,Policy,O),member(rdf(S,Policy,O),Graph),Parts).

execute_policy(Graph,rdf(PolicyId,_,Policy)) :-
    string_uri("fno:executes",Exec),

    % find the function name
    rdf_match(Graph,rdf(Policy,Exec,Func)),

    % call the function
    action(Graph,PolicyId,Policy,Func).


main([]) :-
    writeln(user_error,"usage: orchestrator.pl data/N3 RULES").

main([File|Rules]) :-
    print_message(informational,reason_about(File,Rules)),
    n3_reasoning(File,Rules,Graph),

    print_message(informational,graph(Graph)),
    split_policy(Graph,Parts),
   
    maplist(execute_policy(Graph),Parts).

/************
 * Messages */
 ************/

:- multifile prolog:message//1 .

prolog:message(reason_about(File,Rules)) -->
    [ 'orchestrator: reasoning about ~w with ~w'-[File,Rules] , nl ].

prolog:message(graph(Graph)) -->
    { 
      with_output_to(
        string(T),
        rdf2turtle(stream(current_output),Graph)
      )
    } ,
    [ 'graph:' , nl , '~w'-[T]].
