:- module(actions, [
      action/3
]).

'http://example.org/appendToLog'(Id,Args) :-
    print_message(informational,action('appendToLog',Id,Args)),
    format("..~w~n",[Id]).

'http://example.org/sendEmail'(Id,Args) :-
    print_message(informational,action('sendEmail',Id,Args)),
    format("..~w~n",[Id]).

% execute the action
action(Func,Id,Args) :-
    call(Func,Id,Args).

:- multifile prolog:message//1 .

prolog:message(action(Func,Id,Graph)) --> 
    { 
      with_output_to(
        string(T),
        rdf2tutle(stream(current_output),Graph)
      )
    } ,
    [ 'action: ~w(~w) with graph:~n'-[Func,Id] , T].