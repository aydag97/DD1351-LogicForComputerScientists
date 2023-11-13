% for debugging: write('predikatet uppfyllt!')


% reading and dividing a file
verify(FileName):- see(FileName), % reads a file
                    read(Prems),  % first row is unified with the premise
                    read(Goal),   % second row is unified with the goal
                    read(Proof),  % third row is unified with the proof
                    seen,         % close the file
                    valid_proof(Prems,Goal,Proof).  % proof checking

/*************** Valid Proof **************/
valid_proof(Prems,Goal,Proof):- check_goal(Goal,Proof), check_proof(Prems,Proof,[]).

% print yes if proof is valid
valid_proof(Prems,Goal,Proof):- check_goal(Goal,Proof),check_proof(Prems,Proof,[]),
                                write('Yes').

% print no if proof is invalid
valid_proof(Prems,Goal,Proof):- (not(check_goal(Goal,Proof))); (not(check_proof(Prems,Proof,[]))),
                                write('No'),
                                fail.
/******************************************/


/************** Goal checking *************/
                             
% base case
check_goal(Goal,[[_,LastRow,_]|[]]):- Goal = LastRow. 

check_goal(Goal,[_|Tail]) :- check_goal(Goal,Tail). % finding the last row recursively
/******************************************/

check_proof(Prems,Proof,[]).




/************** Rule checking *************/

% premise
check_rule(Prems,[_,Statement,'premise'],_):- member(Statement,Prems).

% andint
check_rule(_,[_,and(X,Y),andint(Row1,Row2)], ValidProofs):- member([Row1,X,_], ValidProofs),
                                                            member([Row2,Y,_], ValidProofs).

% andel 1
check_rule(_,[_,X,andel(RowNumber)], ValidProofs):- member([RowNumber,and(X,_),_], ValidProofs).

%andel 2
check_rule(_,[_,Y,andel(RowNumber)], ValidProofs):- member([RowNumber,and(_,Y),_], ValidProofs).

% orint 1
check_rule(_,[_,or(X,_),orint(RowNumber)], ValidProofs):- member([RowNumber,X,_], ValidProofs).

% orint 2
check_rule(_,[_,or(_,Y),orint(RowNumber)], ValidProofs):- member([RowNumber,Y,_], ValidProofs).

%orel (box)

% impel
check_rule(_,[_,Y,impel(Row1,Row2)], ValidProofs):- member([Row1,X,_], ValidProof), 
                                                    member([Row2,imp(X,Y),_], ValidProofs).

%impint(box)

%negint(box)

% negel
check_rule(_,[_,cont, negel(Row1,Row2)],ValidProofs):- member([Row1, X,_], ValidProofs),
                                                       member([Row2, neg(X),_], ValidProofs).

% copy
check_rule(_,[_,X, copy(RowNumber)], ValidProofs):- member([RowNumber,X,_], ValidProofs).

% negnegel
check_rule(_,[_,X,negnegel(RowNumber)], ValidProofs):- member([RowNumber,neg(neg(X)),_], ValidProofs).

% negnegint
check_rule(_,[_,neg(neg(X)),negnegint(RowNumber)], ValidProofs):- member([RowNumber,X,_], ValidProofs).

% contel
check_rule(_,[_,X, cont(RowNumber)], ValidProofs):- member([RowNumber, 'cont', _], ValidProofs).

% MT
check_rule(_,[_, neg(X), mt(Row1,Row2)], ValidProofs):- member([Row1, imp(X,Y),_], ValidProofs),
                                                        member([Row2, neg(Y),_], ValidProofs).

% PBC(box)

% LEM (it is valid without any conditions)
check_rule(_,[_,or(X,neg(X)), lem], _).