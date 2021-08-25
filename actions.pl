:- module(actions, [
      action/4
]).

:- use_module(rdf_tools).
:- use_module(library(uuid)).

cfg(outputDir,"./output").

gen_id(Id) :-
    uuid(Id).

gen_file(Func,Id,File) :- 
    cfg(outputDir,Dir),
    swritef(File,'%w/%w/%w',[Dir,Func,Id]).

'http://example.org/appendToLog'(_,PolicyId,Policy) :-
    print_message(informational,action('appendToLog',PolicyId,Policy)),
    format("..~w~n",[PolicyId]).

'http://example.org/sendNotification'(Graph,PolicyId,Policy) :-
    print_message(informational,action('sendNotificAtion',PolicyId,Policy)),

    % Read out the notification    
    string_uri("ex:notification",Notification),
    rdf_match(Graph,rdf(Policy,Notification,Params)),
    findall(
        G,
        rdf_walk(Graph,rdf(Params,_,_),G),
        Parts),
    flatten(Parts,NewGraph),

    % Find the current (blank) node of the notification
    string_uri("rdf:type",Type),
    rdf_match(NewGraph,rdf(Node,Type,_)),

    % Create a new identifier for the node
    gen_id(Id),
    atom_concat('urn:uuid:',Id,IdUri),
    
    % Set the subject of the new graph
    fix_subject(NewGraph,Node,IdUri,OutputGraph),

    gen_file('sendNotification',Id,File),

    open(File,write,Stream),
    rdf2turtle(Stream,OutputGraph),
    close(Stream).

% execute the action
action(Graph,Id,Policy,Func) :-
    call(Func,Graph,Id,Policy).

:- multifile prolog:message//1 .

prolog:message(action(Func,Id,Policy)) --> 
    [ 'action: ~w(~w,~w)'-[Func,Id,Policy] , nl].