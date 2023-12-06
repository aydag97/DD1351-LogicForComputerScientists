  

appendEl(X, [], [X]).
appendEl(X, [H | T], [H | Y]) :- 
  appendEl(X, T, Y).

% For SICStus, uncomment line below: (needed for member/2)
%:- use_module(library(lists)).
% Load model, initial state and formula from file.
verify(Input) :-
    see(Input), read(T), read(L), read(S), read(F), seen,
    check(T, L, S, [], F),!.
    
% check(T, L, S, U, F)
% T - The transitions in form of adjacency lists
% L - The labeling
% S - Current state
% U - Currently recorded states
% F - CTL Formula to check.
%
% Should evaluate to true iff the sequent below is valid. %
% (T,L), S |- F %U

/******************************* check all states **************************************/
% base case
check_all(_,_,[],_,_).
% check all states. the formula has to be true for all of them
check_all(T, L, [S|Rest],U, X) :- check(T,L, S, U,X),
                                        check_all(T, L, Rest, U, X).

/******************************* check existence ***************************************/
% check if such a state exists where the formula is true
check_exist(T, L, [S|Rest], U, X) :- check(T, L, S, U, X) ; check_exist(T, L, Rest, U, X).

/**************************** check atomic propositions ********************************/

check(_, L, S, [], X) :- member([S, Lables], L), member(X, Lables). 
% negation
check(_, L, S, [], neg(X)) :- member([S, Lables], L), \+(member(X, Lables)). 

/****************************** check CTL operators ************************************/
% And
check(T, L, S, [], and(X,Y)) :- check(T, L, S, [], X), check(T, L, S, [], Y).

% Or
check(T, L, S, [], or(X,Y)) :- check(T, L, S, [], X); check(T, L, S, [], Y).

% AX - check all next states that are reachable from the current state S in one step
% why do we have an empty list instead of U? check figure 1 in lab instructions!

check(T, L, S, [], ax(X)) :- member([S, NextStates], T) , check_all(T, L, NextStates, [], X).

% EX
check(T, L, S, [], ex(X)) :- member([S, NextStates], T) , check_exist(T, L, NextStates, [], X).

% AG1
check(_, _, S, U, ag(_)) :- member(S, U).

% AG2
check(T, L, S, U, ag(X)) :- \+member(S, U), check(T, L, S, [], X),
                                            member([S, States], T),
                                            appendEl(S, U, U1),
                                            check_all(T, L, States, U1, ag(X)).


% EG1
check(_, _, S, U, eg(_)) :- member(S, U).

% EG2
check(T, L, S, U, eg(X)) :- \+member(S, U), check(T, L, S, [], X),
                                            member([S, States], T),
                                            appendEl(S, U, U1),
                                            check_exist(T, L, States, U1, eg(X)).

% EF1
check(T, L, S, U, ef(X)) :- \+member(S, U), check(T, L, S, [], X).

% EF2
check(T, L, S, U, ef(X)) :- \+member(S, U),
                                            member([S, States], T),
                                            appendEl(S, U, U1),
                                            check_exist(T, L, States, U1, ef(X)).

% AF1
check(T, L, S, U, af(X)) :- \+member(S, U), check(T, L, S, [], X).

% AF2
check(T, L, S, U, af(X)) :- \+member(S, U),
                                            member([S, States], T),
                                            appendEl(S, U, U1),
                                            check_all(T, L, States, U1, af(X)).


