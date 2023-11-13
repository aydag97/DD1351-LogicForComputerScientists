/*
 * Welcome to the lab 1 in DD1351!
 * This lab gives an introduction into prolog programming.
 * All code provided in assignment 1-4 is written by:
 *
 * Fatima Mohammad Ali & Ayda Ghalkhanbaz
 * Date: 2023-10-05
*/


appends([],L,L).
appends([H|T],L,[H|R]) :- appends(T,L,R).

appendEls(X, [], [X]).
appendEls(X, [H | T], [H | Y]) :-
           appendEls(X, T, Y).

lengths([],0).
lengths([_|T],N) :- lengths(T,N1), N is N1+1.

nths(N,L,E) :- nths(1,N,L,E).
nths(N,N,[H|_],H).
nths(K,N,[_|T],H) :- K1 is K+1, nths(K1,N,T,H).

subsets([], []).
subsets([H|T], [H|R]) :- subsets(T, R).
subsets([_|T], R) :- subsets(T, R).

selects(X,[X|T],T).
selects(X,[Y|T],[Y|R]) :- selects(X,T,R).

members(X,L) :- selects(X,L,_).

memberchks(X,L) :- selects(X,L,_), !.
%%%%%%%%%%%%%%%%%%%%%%%
/* uppgift 1
Betrakta denna fråga till ett Prologsystem:

?- T=f(a,Y,Z), T=f(X,X,b).

Vilka bindningar presenteras som resultat?

Ge en kortfattad förklaring till ditt svar!*/

/* 
svar:
T = f(a, a, b),
Y = X, X = a,
Z = b.
Förklaring: Två termer kan unifieras om det finns en substitution för variablerna i termerna 
så att båda termerna blir syntaktiskt identiska. 
Här unifieras variabeln Y = X och X = a som ger att Z = b, som slutligen ger T =f(a,a,b).
*/
%%%%%%%%%%%%%%%%%%%%%%%%

/* uppgift 2

En lista är en representation av sekvenser där 
den tomma sekvensen representeras av symbolen []
och en sekvens bestående av tre heltal 1 2 3 
representeras av listan [1,2,3] eller i kanonisk syntax 
'.'(1,'.'(2,'.'(3,[]))) eller [1|[2|[3|[]]]]

Den exakta definitionen av en lista är:

list([]).
list([H|T]) :- list(T).


Vi vill definiera ett predikat som givet en lista som 
representerar en sekvens skapar en annan lista som 
innehåller alla element som förekommer i inlistan i 
samma ordning, men 
om ett element har förekommit tidigare i listan skall det 
inte vara med i den resulterande listan.

Till exempel: 

?- remove_duplicates([1,2,3,2,4,1,3,4], E).

skall generera E=[1,2,3,4]

Definiera alltså predikatet remove_duplicates/2!
Förklara varför man kan kalla detta predikat för en
funktion!*/


% basfallet
remove_duplicates([],[]).

remove_duplicates(List,Result):- remove_helper(List,[],Result).

% basfallet till remove_helper 
remove_helper([],Acc,Acc).

% om Head finns i Accumulator så ska vi skippa och fortsätta med resten av listan (fallet där duplicate finns)
remove_helper([Head|Tail],Acc, Result):- members(Head,Acc),!, remove_helper(Tail,Acc,Result).

% om Head inte är med i Acc, då ska vi appenda elementet i Result och fortsätta med resten, tills basfallet nås
remove_helper([Head|Tail],Acc, Result):- appends(Acc,[Head],Newacc), remove_helper(Tail,Newacc,Result).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% uppgift 3
% basfall
%partstring([], 0, []).

%partstring(Y,L,F) :- subsets(Y,F),lengths(F,L).

% om en lista är en delmängd till en annan lista
subsetnew(_,[]).

subsetnew([Head|List], [Head|Result]) :- subsetnew(List, Result).


partstring([],_,[]).

partstring([_|List],L,Result) :- partstring(List,_,Result), lengths(Result,L).

partstring([Head|List],L,[Head|Result]) :- subsetnew(List, Result), lengths([Head|Result],L).



%%%%%%%%%%%%%%%%%
% uppgift 4
node(a). %atomer
node(b).
node(c).
node(d).

edge(a,b). % termer
edge(b,c).
edge(c,d).
edge(d,a).
edge(a,d).
edge(b,a).

path(X, Y, Path) :- path_helper(X, Y, [X], Path). % lägger till första noden

%BASFALL, kommit fram till destination node
path_helper(X, X, Visited, Visited).

path_helper(X, Z, Visited, Path):-  edge(X, W), % hämtar en granne Y till X (enligt definierade kanter)
                                    not(memberchks(W, Visited)), % om vi inte redan passerat noden?! 
                                    appendEls(W,Visited,V2), % lägg till noden i visited
                                    path_helper(W, Z, V2, Path). % rekursivt söker efter en väg från Y till Z
% ändrade rad 129 til memberchks
% rad 106 ändrade basfallet