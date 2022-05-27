% зад. 1
% L=[[a,1],[b,2],[c,1],...]
% piece=[a,b]
firstElements([],[]).
firstElements([[A,_]|H],[A|R]):-firstElements(H,R).

subsequence([],[]).
subsequence([H|L],[H|S]):-subsequence(L,S).
subsequence([_|L],S):-subsequence(L,S).

genPiece(L,P):-subsequence(L,P).

sumPiece([],0).
sumPiece([[_,B]|T],S):-sumPiece(T,S1), S is B+S1.

similar(L1,L2):-(member(X,L1), member(X,L2)).
intersection([H|L1],L2,[H|L]):-member(H,L2),intersection(L1,L2,L).
intersection([H|L1],L2,L):-not(member(H,L2)),intersection(L1,L2,L).

indep(L):-
  genPiece(L,P1),
  genPiece(L,P2),
  firstElements(P1,F1),
  firstElements(P2,F2),
  similar(F1,F2),
  intersection(P1,P2,P),
  sumPiece(P,S),
  sumPiece(P1,S1),
  sumPiece(P2,S2),
  S =:= S1*S2.

% зад. 2
natural(0).
natural(N):-natural(N1),N is N1+1.

nthElement([H|_],0,H).
nthElement([_|T],N,X):-N > 0, N1 is N-1, nthElement(T,N1,X).

% съществува ест.число Д, такова че, 
% за всяко ест.число Н, от ... => ...
% Ed Vn (...=>...)
% Ed !En! (...=>...)
% isParaperiodic(L):-
%   natural(D),
%   not((
%     natural(N), 
%     not((
%       N mod 3 =:= 1, 
%       nthElement(L,N,X), 
%       K is N+D, 
%       nthElement(L,K,Y), 
%       X=:=Y 
%     ))
%   )).

% aperiod(X):-
%   natural(N),
%   generateSequence(N,X),
%   isParaperiodic(X).

generateSequence(0,[]).
generateSequence(N,[H|T]):-
  N>0,
  member(H,[0,1]),
  N1 is N-1,
  generateSequence(N1,T).

generateDoubleFromSequence([],[],_).
generateDoubleFromSequence([H|T],[[N,H]|D],N):-
  N1 is N+1,
  generateDoubleFromSequence(T,D,N1).

getSecondElements([],[]).
getSecondElements([[_,B]|L],[B|R]):-
  getSecondElements(L,R).

% Ed Vk e [0,n]  (k mod 3=0 => ak+d = ak)
% Ed !Ek e [0,n] ! (K mod 3 = 0 => ak+d = ak)
isParaperiodic(L):-
  length(L,N),
  between(1,N,D),
  not((
    between(0,N,K),
    not((
      K mod 3 =:= 1,
      K<N,
      K1 is K+D,
      nthElement(L,K,X),
      nthElement(L,K1,Y),
      X=[_,B1],
      Y=[_,B2],
      B1 =:= B2
  )))).


aperiod(X):-
  natural(N),
  generateSequence(N,S),
  generateDoubleFromSequence(S,X,0),
  not(isParaperiodic(X)).