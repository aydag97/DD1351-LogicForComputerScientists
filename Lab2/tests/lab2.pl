
% reading and dividing a file
verify(FileName):- see(FileName), % reads a file
                    read(Prems),  % first row is unified with the premise
                    read(Goal),   % second row is unified with the goal
                    read(Proof),  % third row is unified with the proof
                    seen,         % close the file
                    valid_proof(Prems,Goal,Proof).  % proof checking

/*************** Valid Proof **************/
valid_proof(Prems, Goal, Proof) :- check_proof(Proof, Prems,[]), check_goal(Goal, Proof).

% print yes if proof is valid
/*valid_proof(Prems,Goal,Proof):- check_proof(Proof,Prems,[]),check_goal(Goal,Proof),
                                write("Yes"),!.

% print no if proof is invalid
valid_proof(Prems,Goal,Proof):- ((not(check_proof(Proof,Prems,[]))); (not(check_goal(Goal,Proof)))),
                                write("No"),
                                fail,!.*/
/******************************************/


/************** Proof checking *************/
check_proof([], _, _).
check_proof([Line|RestProof], Prems, ListSoFar) :-
        check_rule(Prems, Line, ListSoFar), 
        append(ListSoFar, [Line], Result), 
        check_proof(RestProof, Prems, Result).
/******************************************/

/************** Goal checking *************/
                             
% base case
check_goal(Goal,[[_,LastRow,_]|[]]):- Goal = LastRow. 

check_goal(Goal,[_|Tail]) :- check_goal(Goal,Tail),!. % finding the last row recursively
/******************************************/


/************** Rule checking *************/

% premise
check_rule(Prems,[_,Premise,premise],_):- member(Premise,Prems),!.

% andint
check_rule(_,[_,and(X,Y),andint(Row1,Row2)], ValidProofs):- member([Row1,X,_], ValidProofs),
                                                            member([Row2,Y,_], ValidProofs),!.

% andel 1
check_rule(_,[_,X,andel1(RowNumber)], ValidProofs):- member([RowNumber,and(X,_),_], ValidProofs),!.

%andel 2
check_rule(_,[_,Y,andel2(RowNumber)], ValidProofs):- member([RowNumber,and(_,Y),_], ValidProofs),!.

% orint 1
check_rule(_,[_,or(X,_),orint1(RowNumber)], ValidProofs):- member([RowNumber,X,_], ValidProofs),!.

% orint 2
check_rule(_,[_,or(_,Y),orint2(RowNumber)], ValidProofs):- member([RowNumber,Y,_], ValidProofs),!.

% impel
check_rule(_,[_,Y,impel(Row1,Row2)], ValidProofs):- member([Row1,X,_], ValidProofs), 
                                                    member([Row2,imp(X,Y),_], ValidProofs),!.
% negel
check_rule(_,[_,cont, negel(Row1,Row2)],ValidProofs):- member([Row1, X,_], ValidProofs),
                                                       member([Row2, neg(X),_], ValidProofs),!.

% copy
check_rule(_,[_,X, copy(RowNumber)], ValidProofs):- member([RowNumber,X,_], ValidProofs),!.

% negnegel
check_rule(_,[_,X,negnegel(RowNumber)], ValidProofs):- member([RowNumber,neg(neg(X)),_], ValidProofs),!.

% negnegint
check_rule(_,[_,neg(neg(X)),negnegint(RowNumber)], ValidProofs):- member([RowNumber,X,_], ValidProofs),!.

% contel
check_rule(_,[_,_, cont(RowNumber)], ValidProofs):- member([RowNumber, cont, _], ValidProofs),!.

% MT
check_rule(_,[_, neg(X), mt(Row1,Row2)], ValidProofs):- member([Row1, imp(X,Y),_], ValidProofs),
                                                        member([Row2, neg(Y),_], ValidProofs),!.

% LEM (it is valid without any conditions)
check_rule(_,[_,or(X,neg(X)), lem], _).


/************** Box checking *************/
check_rule(Prems,[[RowNumber, X, assumption]|Box], ValidProofs):- 
                                    check_proof(Box, Prems, [[RowNumber, X, assumption]|ValidProofs]),!.

/******************************************/

% orel (box)
check_rule(_,[_,Z,orel(Row1,Row2,Row3,Row4,Row5)], ValidProofs):-
    member([Row1,or(X,Y),_], ValidProofs),
    member(BoxOne, ValidProofs), member(BoxTwo, ValidProofs),
        member([Row2,X,assumption], BoxOne),last(BoxOne,Conclusion1),Conclusion1 = [Row3,Z,_],
        member([Row4,Y,assumption], BoxTwo),last(BoxTwo,Conclusion2),Conclusion2 = [Row5,Z,_],!.


% impint(box)
check_rule(_, [_, imp(X, Y), impint(Row1,Row2)], ValidProofs):- member(Box, ValidProofs),
                                                                member([Row1, X, assumption], Box),
                                                                last(Box, Last), Last = [Row2, Y, _],!.

% negint(box)
check_rule(_,[_, neg(X), negint(Row1,Row2)], ValidProofs) :- member(Box, ValidProofs),
                                                            member([Row1,X,assumption], Box),
                                                            last(Box, Last), Last = [Row2, cont, _],!.

% PBC(box)
check_rule(_,[_, X, pbc(Row1,Row2)], ValidProofs) :- member(Box, ValidProofs),
                                                            member([Row1,neg(X),assumption], Box),
                                                            last(Box, Last), Last = [Row2, cont, _],!.
