%% How to run:
%%      swipl --quiet bot_logic.pl

:- use_module(library(random)).
beginChat:-
  welcome_message.

welcome_message:-
  replies_kb(greeting, X), 
  random_n(X,Y), 
  show(me),
  print_sentence(Y),
  flush_output.


%% Prints the little prompt of who is working
%% Me is the Bot
%% You is the user
prompt(me):-
        my_name(X), write(X), write(': '), flush_output.
promp(you):-
        user_name(X), write(X), write(': '), flush_output.
        
%% The prompts that will show in the chat
my_name('Kellogg Assistant').
user_name('You').


%% Receives a list and chooses a random item of it
%% Returns the item of the list
random_n(Res, R):- 
        length(Res, Length),  
        Upper is Length + 1,
        random(1, Upper, Rand),
        get_item(Res, Rand, R).

%% prints a sentence from a list format printing spaces after each Header
print_sentence([]):- nl.
print_sentence([H|T]):- write(H), write(' '), print_sentence(T).

%% get_item(List, pos, item)
%% returns the item in the position pos of a list
get_item([H|_], 1, H).
get_item([_|T], N, X):-
        get_item(T, N1, X),
        N is N1 + 1.


%% The knowlegde base for the random different greetings
replies_kb(greeting, [
        ['Hey there!'], 
        ['Hello!'], 
        ['Hi there!'],
        ['Nice to meet you!'],
        ['It\'s a pleasure to meet you!']
        ]).



%% call to run the chat as soon as the file compiles
?-beginChat.
