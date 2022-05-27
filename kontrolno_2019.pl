:-use_module(library(clpfd)).

% зад.1
natural(0). 
natural(N):-natural(N1), N is N1+1.

between(A,B,A):-A =< B.
between(A,B,X):-A < B, A1 is A+1, between(A1,B,X).

pairs(X,Y):-natural(N),between(0,N,X),Y is N-X.

% имаме дадени N,AX,AY

generateLengths(N,L1,L2):-between(0,N,L1),between(0,N,L2), N is 2*L1+2*L2.

q(N,[AX,AY],[BX,BY],[CX,CY],[DX,DY]):-
  pairs(AX,AY),
  generateLengths(N,L1,L2),
  BX is AX+L1,
  CX is BX,
  BY is AY,
  CY is BY+L2,
  DY is CY,
  DX is AX.

generateLengths2(N,L1,L2):-between(0,N,L1),between(0,N,L2), N is L1*L2.

q2(N,[AX,AY],[BX,BY],[CX,CY],[DX,DY]):-
  pairs(AX,AY),
  generateLengths2(N,L1,L2),
  BX is AX+L1,
  CX is BX,
  BY is AY,
  CY is BY+L2,
  DY is CY,
  DX is AX.


% зад. 2
concatenate([],L2,L2).
concatenate([H|T],L2,[H|L]):-concatenate(T,L2,L).

member(X,L):-concatenate(_,[X|_],L).

remove(X,L,R):-concatenate(L1,[X|L2],L), concatenate(L1,L2,R).

permutation([],[]).
permutation(L,[X|T]):-member(X,L), remove(X,L,L1), permutation(L1,T).

% X - списък от естествени числа
% У - списък от списъци, съдържащ всички пермутации на Х

% allPermutations(_,[]).
% allPermutations(L,[P|R]):-allPermutations(L,R),permutation(L,P).

% fact(0, 1).
% fact(N, F):- N>0, N1 is N-1, fact(N1, F1), F is N*F1.

% allP([],[]).
% allP(L,P):-L\=[], allP(L,[],P).
% allP(L,Curr,Curr):-length(Curr,N),length(L,N1), fact(N1,N).
% allP(L,Curr,R):-permutation(L,P),not(member(P,Curr)),allP(L, [P|Curr], R).

allP(L,P):-bagof(P1, permutation(L,P1), P).

lastEl([X],X).
lastEl([_|T],X):-lastEl(T,X).

% eqArr(A1,A2):-not((member(X,A1),not(member(X,A2)))), not((member(Y,A2), not(member(Y,A1)))).
eq([],[]).
eq([H|T],[H|T1]):-eq(T,T1).

numberOfOccurances([],_,0).
numberOfOccurances([X|T],X,N):- numberOfOccurances(T,X,N1), N is N1+1.
numberOfOccurances([H|T],X,N):- not(eq(H,X)), numberOfOccurances(T,X,N).

% не съществува елемент на пермутациите, такъв че броя на срещанията му да не е равен
% на последния му елемент
% eachP(P):-not((member(X,P), lastEl(X,Last), not(numberOfOccurances(X,P,Last)))).

appendNTimes(_,0,L,L).
appendNTimes(El,N,L,[El|R]):-N > 0, N1 is N-1, appendNTimes(El,N1,L,R).

newP([],[]).
newP(L,P):-allP(L,P1),newP(P1,[],P).

newP([],Curr,Curr).
newP([H|Tail],Curr,P):-lastEl(H,Last),appendNTimes(H,Last,Curr,Res),newP(Tail,Res,P).

sumEl([],0).
sumEl([H|T],S):-sumEl(T,S1), S is S1+1.
% вариант 2 - използване на sumEl вместо lastEl