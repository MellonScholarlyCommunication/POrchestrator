:- module(actions, [
      action/3
]).

:- use_module(rdf_tools).
:- use_module(library(uuid)).
:- use_module(library(http/json)).

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

% Create a new identifier if the notification contains a blank node
fix_notification_blanks(NodeIRI,Graph,UriNode,NewGraph) :-
    % blank node test
    sub_atom(NodeIRI, 0, _, _, "__"), 

    gen_id(Id),
    
    atom_concat('urn:uuid:',Id,UriNode),
    
    % Set the URI as subject of the new graph
    fix_subject(Graph,NodeIRI,UriNode,NewGraph).

% Don't create a new identifier if the notification contains no blank node
fix_notification_blanks(NodeIRI,Graph,NodeIRI,Graph) :-
    % blank node test
    \+ sub_atom(NodeIRI, 0, _, _, "__").

% SendNotification fills in blank nodes for graph subjects and creates
% a JSON-LD output file
sendNotification(Graph,Policy,Name) :-
    print_message(informational,action(Name,Policy)),

    % ex:notification graph
    policy_arg_as_graph(Graph,Policy,"ex:notification",Notification), 

    % Find the current (blank) node of the notification
    string_uri("rdf:type",Type),
    rdf_match(Notification,rdf(NodeIRI,Type,_)),

    fix_notification_blanks(NodeIRI,Notification,UriNode,OutputGraph) ,

    gen_id(Id),
    gen_file(Name,Id,File),

    open(File,write,Stream),
    rdf2jsonld(Stream,OutputGraph,UriNode),
    close(Stream),

    string_concat("https://www.example.org/",Name,ActionUri),

    json_write(current_output,_{
        id: ActionUri ,
        notification: File
    },[width(0)]) ,
    writeln('').

'https://www.example.org/sendEventLog'(Graph,Policy) :-
    sendNotification(Graph,Policy,"sendEventLog").

'https://www.example.org/sendOutbox'(Graph,Policy) :-
    sendNotification(Graph,Policy,"sendOutbox").

'https://www.example.org/sendInbox'(Graph,Policy) :-
    sendNotification(Graph,Policy,"sendInbox").

% execute the action
action(Graph,Policy,Func) :-
    call(Func,Graph,Policy).

:- multifile prolog:message//1 .

prolog:message(action(Func,Policy)) --> 
    [ 'action: ~w(~w)'-[Func,Policy] , nl ].