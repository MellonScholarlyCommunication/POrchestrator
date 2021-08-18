:- module(rdf_walk, [
      rdf_match/2 ,
      rdf_next/3,
      rdf_walk/3
]).

:- use_module(library(lists)).

% Find a matching triple in the graph
rdf_match(Graph,Triple) :-
    member(Triple,Graph).

% Find a next neighbour triple
rdf_next(Graph,rdf(_,_,Object),rdf(Object,X,Y)) :-
    rdf_match(Graph,rdf(Object,X,Y)).

rdf_walk([],_,[]).

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