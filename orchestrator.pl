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

execute_policy(Graph) :-
    string_uri("fno:executes",Exec),
    string_uri("pol:policy",Pol),
    
    % find the id
    rdf_match(Graph,rdf(Id,Pol,Policy)),

    % find the function name
    rdf_match(Graph,rdf(Policy,Exec,Func)),
    
    % find the subgraph of arguments
    findall(G,rdf_walk(Graph,rdf(Policy,_,_),G),Parts),
    flatten(Parts,Args),

    % call the function
    action(Func,Id,Args).

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
    print_message(informational,reason_about(File,Rules)),
    n3_reasoning(File,Rules,Graph),

    print_message(informational,graph(Graph)),
    split_policy(Graph,Parts),
   
    maplist(execute_policy,Parts).

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
        rdf2tutle(stream(current_output),Graph)
      )
    } ,
    [ 'graph:' , nl , '~w'-[T]].
