:- module(rdf_tools, [
      rdf_match/2 ,
      rdf_next/3,
      rdf_walk/3,
      string_uri/2,
      rdf2turtle/2,
      rdf2ntriples/2,
      rdf2jsonld/3,
      fix_subject/4
]).

:- use_module(library(semweb/turtle)).
:- use_module(library(lists)).

% prefixes
pfx('pol','https://www.example.org/ns/policy#').
pfx('fno','https://w3id.org/function/ontology#').
pfx('ex','https://www.example.org/').
pfx('as','https://www.w3.org/ns/activitystreams#').
pfx('rdf','http://www.w3.org/1999/02/22-rdf-syntax-ns#').

% string_uri(+ShortUrl,-LongUrl)
% Return a URI for a string
%   E.g. string_uri('as:origin','https://www.w3.org/ns/activitystreams#orgin')
string_uri(String,URI) :-
  split_string(String,":","",[P,U]),
  atom_string(PA,P),
  pfx(PA,NS),
  atomic_list_concat([NS,U],URI).

% query used in rdf_save_turtle
triple_in(RDF,S,P,O,_G) :-
      member(rdf(S,P,O), RDF).

% serialize a graph to JSON
%  - we need a third Id argument to guarantee a COAR compatible serialization
rdf2jsonld(Stream,Graph,Id) :-
    setup_call_cleanup(
      process_create(
          "/usr/local/bin/node", [ "bin/nquads_jsonld.js" , "-" , Id ] ,
          [ stdin(pipe(In)) , stdout(stream(Stream)) ]
      ),
      rdf2ntriples(In,Graph),
      close(In)
    ).

% write a graph to a stream as turtle
rdf2turtle(Stream,Graph) :-
    rdf_save_turtle(
            Stream,[
              expand(triple_in(Graph)),
              inline_bnodes(true) ,
              silent(true)
            ]).

rdf2ntriples(Stream,Graph) :-
    rdf_save_turtle(
            Stream,[
              comment(false),
              encoding(utf8),
              expand(triple_in(Graph)),
              group(false),
              prefixes([]),
              subject_white_lines(0),
              a(false),
              inline_bnodes(false) ,
              abbreviate_literals(false)
            ]).

% rdf_match(+Graph,+Triple)
% Find a matching triple in the graph
rdf_match(Graph,Triple) :-
    member(Triple,Graph).

% rdf_next(+Graph,+Triple,-NextTriple)
% Find a next neighbour triple
rdf_next(Graph,rdf(_,_,Object),rdf(Object,X,Y)) :-
    rdf_match(Graph,rdf(Object,X,Y)).

% rdf_walk(+Graph,+Triple,-NewGraph)
% Start from the Triple and find all matching subgraphs 0,..N
rdf_walk([],_,[],_).

rdf_walk(Graph,Triple,NewGraph) :-
    rdf_match(Graph,Triple),
    rdf_walk_many(Graph,[Triple],[Triple],NewGraph).

rdf_walk_many(_,[],A,A).
    
rdf_walk_many(Graph,[Head|Tail],Acc,NewGraph) :-
    % find all the triples conneced to the head
    findall(X,rdf_next(Graph,Head,X),Next),
    % remove all the triples that were already found
    subtract(Next,Acc,Rest),
    % create a new accumulator with the new triples
    union(Next,Acc,NewAcc),
    % create a new todo list
    append(Rest,Tail,ToDo),
    % and execute the rest
    rdf_walk_many(Graph,ToDo,NewAcc,NewGraph).

fix_subject([],_,_,[]).

fix_subject([rdf(Old,P,O)|Tail],Old,New,[rdf(New,P,O)|Res]) :-
    fix_subject(Tail,Old,New,Res).

fix_subject([rdf(S,P,O)|Tail],Old,New,[rdf(S,P,O)|Res]) :-
    S \= Old ,
    fix_subject(Tail,Old,New,Res).

% basic test
 test0 :-
    rdf_walk([],rdf(a,b,c),[]).

 test1 :-
    rdf_walk([
        rdf(a,b,c)
        ],rdf(a,b,c), [rdf(a,b,c)]).   

% find abc test
test2 :-
    rdf_walk([
        rdf(a,b,c),
        rdf(1,2,3),
        rdf(x,y,z)
        ],rdf(a,b,c),[rdf(a,b,c)]).   

% find _b_ query test
test3 :-
    rdf_walk([
        rdf(a,b,c),
        rdf(1,2,3),
        rdf(x,y,z)
        ],rdf(_,b,_),[rdf(a,b,c)]). 

% cyclic test
test4 :-
    rdf_walk([
       rdf(a,b,c),
       rdf(c,d,a)   
    ],rdf(_,b,_),P)
    , union([rdf(a,b,c),rdf(c,d,a)],P,P).

test5 :-
    rdf_walk([
       rdf(a,b,c),
       rdf(c,d,e),
       rdf(c,f,g),
       rdf(g,h,i),
       rdf(x,y,z)   
    ],rdf(_,b,_),P)
    , union([
        rdf(a,b,c),
        rdf(c,d,e),
        rdf(c,f,g),
        rdf(g,h,i)
        ],P,P).