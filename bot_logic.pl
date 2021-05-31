%% How to run:
%%      swipl --quiet bot_logic.pl

:- use_module(library(random)).
:- dynamic usr_name/1, information/2, feedback/2, printer/1, loc/1.

beginChat:-
  welcome_message,
  instructions,
  conversate.

conversate:-
        repeat,
        prompt(you),
        readin(M),
        generate_reply(M,R),
        prompt(me),
        print_sentence(R),
        is_quit(M), 
        show_report,!,halt.

%% read_in will process the natural language of the input of the user calling two other functions

%% M is the entered message [hello]
%% L is the list of ASCII values for each of those characters [72,101,108,108,111,10]
read_in(M):-initread(L),words(M,L,[]).
 
%% initread will parse through the entered message list to translate to the ASCII values
initread([K1,K2|U]):-get_code(K1),get_code(K2),readrest(K2,U).


%% readrest checks for the different values of a list of ASCII codes, ignoring question marks and exclamation marks
readrest(63,[]):-!. %% Question mark
readrest(33,[]):-!. %% Exclamation mark
readrest(10,[]):-!. %% Line feed

%% Skip the character if it is a control one
readrest(K,[K1|U]):-K=<32,!,get_code(K1),readrest(K1,U).
%% Else, get its code and add it to the list
readrest(_K1,[K2|U]):-get_code(K2),readrest(K2,U).


%%--------------- DCG Grammar Rules ---------------------
%% will parse through the message to get the word
%% use the syntax for DCG Grammar rules
%% code within {} is prolog regular code
%% [literals]

%% words are built by a word, blank spaces and other words
words([V|U]) --> word(V),!,blanks,words(U).
words([]) --> [].

% a word is built either with alphanum or only digits
word(U1) --> [K],{lc(K,K1)},!,alphanums(U2),{name(U1,[K1|U2])}.
word(nb(N)) --> [K],{digit(K)},!,digits(U),{name(N,[K|U])}.
word(V) --> [K],{name(V,[K])}.

alphanums([K1|U]) --> [K],{alphanum(K,K1)},!,alphanums(U).
alphanums([]) --> [].

%% An alphanum is either _ or a digit
alphanum(95,95) :- !.
alphanum(K,K1):-lc(K,K1).
alphanum(K,K):-digit(K).

%% Digits are built with a digit, and more digits
digits([K|U]) --> [K],{digit(K)},!,digits(U).
digits([]) --> [].

%% A blank space is built with one or more blank spaces
blanks--> [K],{K=<32},!,blanks.
blanks --> [].

%% check if it a number
digit(K):-K>47,K<58.


%% Switch all letters to lower case
lc(K,K1):-K>64,K<91,!,K1 is K+32. 
lc(K,K):-K>96,K<123.

%% filter will examine the words entered in the input stream

% base case, the list is empty
filter([],[]).

% there is a linebreak at the beginning, then ignore it
filter(['\n'|T], R):-  !,
        filter(T, R).


filter([nb(2), X|T], [Rm|R]):- 
        name(X, CharList),
        q_followed_by_nb(CharList),!,
        name(Rm, [50|CharList]),
        filter(T, R).

% if the head of the text is the same as the one in the result
filter([X|T], [X|R]):- 
        filter(T, R).


q_followed_by_nb([113,X|_]):-
        digit(X).


%%------------Generate generate_reply------------------------

%% Check if we the user entered any quit sentence
generate_reply(S, R):-
        is_quit(S), !,
        replies_db(bye, Res), 
        random_n(Res, R).

%% Check if the user entered any thank yu sentence
generate_reply(S, R):-
        is_thanks(S), !,
        replies_db(thanked, Res), 
        random_n(Res, R).

%% Check if the user asked about the bot name
generate_reply(S, R):-
        question(Tree2, S, _Rest), 
        mapping(s2name,Tree1, Tree2), !,
        sentence(Tree1, Rep,[]),
        append(Rep, ['!'], R).
generate_reply(S, R):-
        pattern_name(S, _), !,
        replies_db(my_name, D),
        random_n(D, R).

%% Check if the user ask a HOW question
generate_reply(S, R):-
        question(Tree2, S, _Rest), !, 
        mapping(s2how,Tree1, Tree2),
        sentence(Tree1, Rep,[]), !,
        append(Rep, ['!'], R).

%% Check if the user asked about bot info
generate_reply(S, R):-
        pattern_me(S, _), !,
        replies_db(me, D),
        random_n(D, R).

%% Check if the user asked any WHY questions
generate_reply(S, R):-
        sentence(Tree1, S, _Rest), !, 
        mapping(s2why,Tree1, Tree2),
        question(Tree2, Rep,[]),
        append(Rep, ['?'], R).
generate_reply(S, R):-
        question(Tree2, S, _Rest), !, 
        mapping(s2q,Tree1, Tree2),
        sentence(Tree1, Rep,[]),
        append([yes, ','|Rep], ['!'], R).

%% If we are trying to get information from the user, keep trying and thank him for it
generate_reply(S, R):-
        \+ is_question(S), 
        \+ information(_, _), !,
        get_info(8),
        replies_db(thanks, D),
        random_n(D, R).


generate_reply(S, R):-
        \+ is_question(S), 
        \+ feedback(_, _), !,
        get_feedback(4),
        replies_db(thanks, D),
        random_n(D, R).
generate_reply(S, R):-
        \+ is_question(S), !,
        replies_db(random_q, Res),
        random_n(Res, R).
generate_reply(S, R):- 
        is_question(S), !,
        replies_db(random_s, Res),
        random_n(Res, R).


is_greeting(S):-
        greeting_db(D),
        intersect(S, D, A),
        A \== [].


is_question(S):-
        member('?', S).


is_thanks(S):-
        thanks_db(D),
        intersect(S, D, A),
        A \== [].


is_quit(S):- 
        subset([bye], S).


%%------------GET INFORMATION-------------------
get_info(0).
get_info(N):-
        questions_db(info, D),
        get_item(D, N, Q),
        prompt(me),
        print_sentence(Q),
        prompt(you),
        readin(R),
        assert(information(Q, R)),
        get_info(Q, R),
        M is N - 1,
        get_info(M).
get_info(QL, RL):-
        get_item(QL, 1, Q),
        contains(Q, name), !,
        get_usr_name(Q, RL).
get_info(QL, RL):-
        get_item(QL, 1, Q),
        contains(Q, subjects), !,
        get_printer_info_loop(RL).
get_info(_, _).



get_usr_name(Q):-
        prompt(you),
        readin(S),
        get_usr_name(Q, S).
get_usr_name(_, RL):-
        is_valid_name(RL), !.
get_usr_name(Q, _):-
        replies_db(get_name, D), 
        random_n(D, X), 
        prompt(me),
        print_sentence(X),
        get_usr_name(Q).

get_printer_info_loop:-
        prompt(you),
        readin(S),
        get_printer_info_loop(S).
get_printer_info_loop(S):- 
        is_valid_printer(S), !.
get_printer_info_loop(_):- 
        replies_db(get_printers, D),
        random_n(D, R),
        prompt(me),
        print_sentence(R),
        get_printer_info_loop.

get_feedback(0).
get_feedback(N):-
        questions_db(feedback, D),
        get_item(D, N, R),
        prompt(me),
        print_sentence(R),
        prompt(you),
        readin(S),
        assert(feedback(R, S)),
        M is N - 1,
        get_feedback(M).


is_valid_name(NL):-
        get_item(NL, 1, N),
        nameList(N),
        assert(usr_name(N)).

is_valid_printer(S):- 
        printer_db(D),
        intersect(S, D, A),
        A \== [],
        assert(printer(A)).

%% This function will read and apply a filter to the information introduced by the user
readin(S):- read_in(L), filter(L,S).

welcome_message:-
  replies_db(greeting, X), 
  random_n(X,Y), 
  prompt(me),
  print_sentence(Y),
  flush_output.

instructions:-
  prompt(me),
  write('You can ask me about the processes to add a new device to our global network.' ), nl,
  prompt(me),
  write('How can I help you today?'), nl,
  flush_output.


%% Prints the little prompt of who is working
%% Me is the Bot
%% You is the user
prompt(me):-
        my_name(X), write(X), write(': '), flush_output.
prompt(you):-
        user_name(X), write(X), write(': '), flush_output.
        
%% The prompts that will show in the chat
my_name('Alexa').
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

%%------ HELPER FUNCTIONS -----------------
intersect([], _, []).
intersect([H|T1], L2, [H|T3]):- 
        member(H, L2), !,
        intersect(T1, L2, T3).
intersect([_|T1], L2, L3):-
        intersect(T1, L2, L3).

contains(A, B) :-
  atom(A),
  atom(B),
  name(A, AA),
  name(B, BB),
  contains(AA, BB).
contains(A, B) :-
  atom(A),
  name(A, AA),
  contains(AA, B).
contains(A, B) :-
  sublist(B, A),
  B \= [].

  sublist(S, L) :-
  append(_, L2, L),
  append(S, _, L2).

sentence( s(X,Y, is, Z) ) --> belonging_phrase(X), abstract_noun(Y),  
                              [is],  special_noun(Z).

sentence(s(X, Y, Z)) --> subject_pronoun(X), indicative_verb(Y), 
                         adjective(Z).

sentence(s(X, Y, Z)) --> subject_phrase(X), verb(Y), object_phrase(Z).



sentence(s(X, Y)) --> subject_tobe_verb(X), prepositional_phrase(Y).

sentence(s(X, Y, Z)) --> question(X), object_pronoun(Y), noun(Z).

belonging_phrase(belong(your)) --> [your].
belonging_phrase(belong(my)) --> [my].

abstract_noun(abs_noun(name)) --> [name].

special_noun(sp_noun(melvin)) --> [melvin].
special_noun(sp_noun(tony)) --> [tony].


subject_phrase(sp(X)) --> subject_pronoun(X).
subject_phrase(sp(X)) --> noun_phrase(X).

object_phrase(op(X,Y)) --> noun_phrase(X), adverb(Y).
object_phrase(op(X, Y)) --> object_pronoun(X), adverb(Y).

noun_phrase(np(X, Y)) --> determiner(X), noun(Y).
noun_phrase(np(Y)) --> noun(Y).


preposition(prep(in)) --> [in].
preposition(prep(at)) --> [at].
preposition(prep(from)) --> [from].


subject_pronoun(spn(i)) --> [i].
subject_pronoun(spn(we)) --> [we].
subject_pronoun(spn(you)) --> [you].
subject_pronoun(spn(they)) --> [they].
subject_pronoun(spn(he)) --> [he].
subject_pronoun(spn(she)) --> [she].
subject_pronoun(spn(it)) --> [it].
subject_pronoun(spn(who)) --> [who].

object_pronoun(opn(you))--> [you].
object_pronoun(opn(your))--> [your].
object_pronoun(opn(me))--> [me].
object_pronoun(opn(us))--> [us].
object_pronoun(opn(them))--> [them].
object_pronoun(opn(him))--> [him].
object_pronoun(opn(her))--> [her].
object_pronoun(opn(it))--> [it].

determiner(dtmnr([])) --> [].
determiner(dtmnr([a])) --> [a].
determiner(dtmnr([the])) --> [the].
determiner(dtmnr([my])) --> [my].
determiner(dtmnr([some])) --> [some].
determiner(dtmnr([all])) --> [all].
determiner(dtmnr([that])) --> [that].

noun(noun(uwe)) --> [uwe].
noun(noun(cs_course)) --> [cs_course].
noun(noun(robotics_course)) --> [robotics_course].
noun(noun(robotics_course)) --> [computing_course].
noun(noun(robotics_course)) --> [sd_course].
noun(noun(name)) --> [name].

adverb(ad([very, much])) --> [very, much].
adverb(ad([how])) --> [how].
adverb(ad([])) --> [].

verb(vb(like)) --> [like].
verb(vb(love)) --> [love].
verb(vb(is)) --> [is].

indicative_verb(ivb(are)) --> [are].
indicative_verb(ivb(am)) --> [am].

subject_tobe_verb(s_2b([you, are])) --> [you, are].
subject_tobe_verb(s_2b([i,am])) --> [i, am].
subject_tobe_verb(s_2b([we, are])) --> [we, are].

adjective(adj(great)) --> [great].
adjective(adj(good)) --> [good].
adjective(adj(fine)) --> [fine].

question(q(why,do,S)) --> [why, do], sentence(S).
question(q(do,S)) --> [do], sentence(S).

question(q(X, Y, Z)) --> adverb(X), indicative_verb(Y), subject_pronoun(Z).
question( q( what, is, X, Y ) ) -->  [what, is],  belonging_phrase(X),  
                                     abstract_noun(Y).   

mapping(s2why, 
        s(sp(spn(N1)),vb(V),op(opn(N2),ad(X))),
        q(why,do,s(sp(spn(P1)),vb(V),op(opn(P2),ad(X)))) 
        ) :- 
        mapping_spn(N1, P1), mapping_opn(N2, P2). 
mapping(s2why,
        s(sp(spn(N1)),vb(V),op(np(noun(N2)),ad(X))),
        q(why,do,s(sp(spn(P1)),vb(V),op(np(noun(N2)),ad(X)))) 
        ) :- 
        mapping_spn(N1, P1).


mapping(s2q,
        s(sp(spn(N1)),vb(V),op(opn(N2),ad(X))),
        q(do,s(sp(spn(P1)),vb(V),op(opn(P2),ad(X)))) 
        ) :- 
        mapping_spn(N1, P1), mapping_opn(N2, P2). 
mapping(s2q,
        s(sp(spn(N1)),vb(V),op(np(noun(N2)),ad(X))),
        q(do,s(sp(spn(P1)),vb(V),op(np(noun(N2)),ad(X)))) 
        ) :- 
        mapping_spn(N1, P1).

mapping(s2name,
        s( belong(Y1), abs_noun(X2), is, sp_noun(Y2) ),
        q( what, is, belong(X1), abs_noun(X2) )
        ):-
        mapping_belong(X1, Y1), mapping_noun(X2, Y2).

mapping(s2how,
        s(spn(X1), ivb(Y1), adj(_)),
        q(ad(_), ivb(Y2), spn(Z2))
        ):-
        mapping_spn(X1, Z2), mapping_indicative(Y1, Y2).

mapping_belong(my,your).
mapping_belong(your,my).

mapping_noun(name, tony).
mapping_noun(tony, name).

mapping_indicative(are, am).
mapping_indicative(am, are).

mapping_ad(how, fine).
mapping_ad(fine, how).

mapping_spn(i, you).
mapping_spn(you, i).

mapping_opn(you,me).
mapping_opn(me,you).


%%------ PATTERNS ------------------
pattern_name([what, is, your, name, X |_], X):-!.
pattern_name(['what\'s', your, name, X |_], X):-!.
pattern_name([whats, your, name, X |_], X):-!.
pattern_name([what, are, you, called, X |_], X):-!.
pattern_name([who, are, you, X |_], X):-!.
pattern_name([_|T], X):-
        pattern_name(T, X).



pattern_me([how, are, you, X |_], X):-!.
pattern_me([are, you, ok, X |_], X):-!.
pattern_me([you, ok, X |_], X):-!.
pattern_me([you, okay, X |_], X):-!.
pattern_me([_|T], X):-
        pattern_me(T, X).

nameList('karla').
nameList('cris').
nameList('jucemar').
nameList('alysson').



%% The knowlegde base for the random different greetings
replies_db(greeting, [
        ['Hey there!'], 
        ['Hello!'], 
        ['Hi there!'],
        ['Nice to meet you!'],
        ['It\'s a pleasure to meet you!']
        ]).

replies_db(get_printers, [
        ['Haven\'t heard of that one before!'],
        ['That\'s not a real printer...'],
        ['Are you sure?']
        ]).


replies_db(my_name, [
        ['My name is Tony, nice to meet you.'],
        ['I\'m Tony!'],
        ['My name isn\'t important right now.'],
        ['Tony, at your service, how may I help?']
        ]).

replies_db(thanks, [
        ['Thanks for the info!'],
        ['Thanks, that\'s helpful.'],
        ['Ok, thanks.'],
        ['Cheers for that.'],
        ['Nice one.'],
        ['Great!'],
        ['Awesome']
        ]).


replies_db(get_name, [
        ['Is that your real name?'],
        ['That\'s not your real name...'],
        ['That can\'t be your name.'],
        ['Just tell me your name...'],
        ['I need to know your name!'],
        ['Come on, what are you called?']
        ]).



replies_db(thanked, [
        ['You\'re welcome!'],
        ['Any time.'],
        ['Glad to be of service.'],
        ['No worries.'],
        ['No problem.']
        ]).

replies_db(bye, [
        ['Bye!'], 
        ['Hope to see you again.'], 
        ['Have a nice day!']
        ]).


responses_db(random_q, [
        ['Isn\'t it a nice day?'],
        ['Ok.'],
        ['What do you mean?'],
        ['How rude.'],
        ['You\'re quite rude, aren\'t you?'],
        ['What?'],
        ['Excuse me?']
        ]).

responses_db(random_s, [
        ['I dunno...'],
        ['Sorry, I can\'t answer that one.'],
        ['Not sure!'],
        ['Can I get a different question?'],
        ['Oh, you\'ll have to ask someone else that.'],
        ['Sorry, I\'m only a simple Tony.'],
        ['Sorry, I can\'t remember everything you said...'],
        ['Can you say that again?'],
        ['Now, there\'s a question.'],
        ['Who knows!'],
        ['No. Just no.'],
        ['Yes, I agree.']
        ]).



questions_db(feedback, [
        ['Okay. Did you find any of the talks interesting?'],
        ['Hmm. Do you think that I am a human?'],
        ['Ok, thanks. Have I been helpful?'],
        ['So, what are your thoughts to make me better?']
        ]).

questions_db(info, [
        ['Finally, run the proper tests.'],
        ['To request a printing queue, contact the Basis Team through DC'],
        ['Ask the Security global team to add this IP to the firewalls through Digital Concierge'],
        ['The next step is to ask Adriano to add the translation to the fortigate'],
        ['Okay, please assign a Kellogg Network IP and document it in ...'],
        ['Right now, what\'s the inside IP for the printer?'],
        ['Nice to meet you. What brand and model is the printer?'],
        ['What\'s your name?']
        ]).



greeting_db([
        hello, 
        hi, 
        hey
        ]).

thanks_db([
        thanks,
        thankyou,
        thank,
        cheers
        ]).

printer_db([
        lexmark,
        hP,
        rico,
        riso,
        canon,
        epson,
        xerox
        ]).


show_report:-
        write('\n--- Report of talk ---\n'),
        usr_name(X), 
        print_sentence(['Your name: ', X]),
        retract(usr_name(X)), fail.
show_report:-
        nl, feedback(X, Y), write(X), write(' : '), print_sentence(Y), 
        retract(feedback(X, Y)), fail.
show_report:-
        nl, information(X, Y), write(X), write(' : '), print_sentence(Y), 
        retract(information(X, Y)), fail.
show_report.



%% call to run the chat as soon as the file compiles
?-beginChat.
