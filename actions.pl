:- module(actions, [
      action/3
]).

:- use_module(rdf_tools).
:- use_module(library(uuid)).

cfg(outputDir,"./output").

% Generate a UUID
gen_id(Id) :-
    uuid(Id).

% Generate a filename based on an Id
gen_file(Func,Id,File) :- 
    cfg(outputDir,Dir),
    swritef(File,'%w/%w/%w',[Dir,Func,Id]).

% Read the Arg valid from the policy in the Graph
policy_param(Graph,Policy,Arg,Result) :-
    string_uri(Arg,ArgUri),
    rdf_match(Graph,rdf(Policy,ArgUri,Params)),
    findall(
        G,
        rdf_walk(Graph,rdf(Params,_,_),G),
        Parts),
    flatten(Parts,Result).

'http://example.org/appendToLog'(Graph,Policy) :-
    print_message(informational,action('appendToLog',Policy)),

    policy_param(Graph,Policy,"ex:log",NewGraph),
    
    gen_id(Id),

    gen_file('appendToLog',Id,File),

    open(File,write,Stream),
    rdf2turtle(Stream,NewGraph),
    close(Stream).

'http://example.org/sendNotification'(Graph,Policy) :-
    print_message(informational,action('sendNotificAtion',Policy)),

    % Read out the notification   
    policy_param(Graph,Policy,"ex:notification",NewGraph), 

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
    rdf2turtle(Stream,OutputGraph),
    close(Stream).

% execute the action
action(Graph,Policy,Func) :-
    call(Func,Graph,Policy).

:- multifile prolog:message//1 .

prolog:message(action(Func,Policy)) --> 
    [ 'action: ~w(~w)'-[Func,Policy] , nl ].