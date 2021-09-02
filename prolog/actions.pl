:- module(actions, [
      action/3
]).

:- use_module(rdf_tools).
:- use_module(library(uuid)).

% Generate a UUID
gen_id(Id) :-
    uuid(Id).

% Generate a filename based on an Id
gen_file(Func,Id,File) :- 
    cfg(outputDir,Dir),
    swritef(File,'%w/%w/%w',[Dir,Func,Id]).

% Given a Graph and a Policy URI, read the Arg as a subGraph
policy_arg_as_graph(Graph,Policy,Arg,Result) :-
    string_uri(Arg,ArgUri),
    rdf_match(Graph,rdf(Policy,ArgUri,Params)),
    findall(
        G,
        rdf_walk(Graph,rdf(Params,_,_),G),
        Parts),
    flatten(Parts,Result).

% Given a Graph and a Policy URI, read the Arg as a subGraph
policy_arg(Graph,Policy,Arg,Result) :-
    string_uri(Arg,ArgUri),
    rdf_match(Graph,rdf(Policy,ArgUri,Result)).

'http://example.org/appendToLog'(Graph,Policy) :-
    print_message(informational,action('appendToLog',Policy)),

    policy_arg(Graph,Policy,"ex:notification",UriNode),

    policy_arg_as_graph(Graph,Policy,"ex:notification",OutputGraph),
    
    gen_id(Id),
    gen_file('appendToLog',Id,File),

    open(File,write,Stream),
    rdf2jsonld(Stream,OutputGraph,UriNode),
    close(Stream),

    format("http://example.org/appendToLog;~w~n",File).

'http://example.org/sendNotification'(Graph,Policy) :-
    print_message(informational,action('sendNotificAtion',Policy)),

    % Read out the notification   
    policy_arg_as_graph(Graph,Policy,"ex:notification",NewGraph), 

    % Find the current (blank) node of the notification
    string_uri("rdf:type",Type),
    rdf_match(NewGraph,rdf(BlankNode,Type,_)),

    % Create a new URI for the current (blank) node
    gen_id(Id),
    atom_concat('urn:uuid:',Id,UriNode),
    
    % Set the URI as subject of the new graph
    fix_subject(NewGraph,BlankNode,UriNode,OutputGraph),

    gen_file('sendNotification',Id,File),

    open(File,write,Stream),
    rdf2jsonld(Stream,OutputGraph,UriNode),
    close(Stream),

    format("http://example.org/sendNotification;~w~n",File).

% execute the action
action(Graph,Policy,Func) :-
    call(Func,Graph,Policy).

:- multifile prolog:message//1 .

prolog:message(action(Func,Policy)) --> 
    [ 'action: ~w(~w)'-[Func,Policy] , nl ].