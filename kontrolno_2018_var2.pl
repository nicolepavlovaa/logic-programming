% зад. 2
d([],[],0).
d([H|L1],[H|L2],D):-d(L1,L2,D1), D is D1+1.
d([H|L1],[H1|L2],D):-H\=H1, d(L1,L2,D).

sumD([],_,0).
sumD([H|T],L1,X):-
  sumD(T,L1,X1),
  d(H,L1,X2),
  X is X1+X2.

notEq(L1,L2):-member(X,L1),member(Y,L2), X \= Y.

localPerpiphery(L,L1):-
  member(L1,L),
  sumD(L,L1,D1),
  not((
    member(L2,L),
    notEq(L2,L1),
    sumD(L,L2,D2),
    D2 > D1
    )).

generateBooleanList(0,[]).
generateBooleanList(N,[H|T]):-
  N>0,
  member(H,[0,1]),
  N1 is N-1,
  generateBooleanList(N1,T).

natural(0).
natural(N):-natural(N1),N is N1+1.

% K lists with N elements, L is list of lists
generateLists(0,_,[]).
generateLists(K,N,[L1|L]):-
  generateBooleanList(N,L1),
  K1 is K-1,
  generateLists(K1, N, L).

p(N,X):-
  natural(K),
  generateLists(K,N,L),
  member(L1,L),
  localPerpiphery(L1,L),
  sumD(L,L1,D1),
  not((
    generateBooleanList(N,L2),
    not(member(L2,L)),
    sumD(L,L2,D2),
    D2>D1
    )).
% ---------------------------

remove(X,L,R):-append(L1,[X|L2], L), append(L1,L2,R).

permutation([],[]).
permutation(L,[H|T]):-remove(H,L,L1),permutation(L1,T).

insert(X,L,R):-append(L1,L2,L),append(L1,[X|L2],R).

% permutation2([],[]).
% permutation2([H|T], R):- permutation2(T,T1), insert(H,T1,R).

% pack permutations
% pack(L,[],P) P-съдържа всички пермутации на L
pack([],[],[]).
pack(L,R,R):-not((permutation(L,P),not(member(P,R)))).

pack(L,Curr,Res):-
  permutation(L,P),
  not(member(P,Curr)),
  pack(L,[P|Curr],Res).

% ------------
% append([],L2,L2).
% append([H|L1],L2,[H|L]):-append(L1,L2,L).

% member(X,L):-append(L1,[X|L2],L).
% remove(X,L,R):-append(L1,[X|L2],L),append(L1,L2,R).
% insert(X,L,R):-append(L1,L2,L),append(L1,[X|L2],R).

reversee([],[]).
reversee(L,R):-reversee(L,[],R).

reversee([],Curr,Curr).
reversee([H|T],Curr,Res):-reversee(T,[H|Curr],Res).

naturall(0).
naturall(N):-naturall(N1),N is N1+1.

pairr(N,M):-naturall(Z),between(0,Z,N), M is Z-N.

integerr(0).
integerr(N):-natural(N1),sign(N,N1).
sing(N,N1):-N is (-1)*N1.
sign(N,N1):-N is N1.

int(N):-N is 0.
int(N):-natural(N1), N1 =\= 0, sign(N1,N).
sign(N1,N):-N is N1.
sign(N1,N):-N is (-1)*N1.