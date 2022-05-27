% зад. 1
minEl(X,L):-member(X,L),not((member(Y,L), Y<X)).
maxEl(X,L):-member(X,L),not((member(Y,L), Y>X)).

minElements([],[]).
minElements([H|T],[X|R]):-minEl(X,H), minElements(T,R).

maxElements([],[]).
maxElements([H|T],[X|R]):-maxEl(X,H), maxElements(T,R).

balance(L,B):-maxElements(L,Max), minElements(L,Min), minEl(Max,MinMax), maxEl(Min, MaxMin), B is MinMax-MaxMin.

p(L):-balance(L,B), not((member(X,L), not(member(B,X)))).

% зад. 2
d([],[],0).
d([H|T1],[H|T2],N):-d(T1,T2,N1), N is N1+1.

sumD([],_,0).
sumD([H|T],L1,D):-d(H,L1,D1), sumD(T,L1,DPrev), D is DPrev+D1.

% генерира булев списък с дължина Н
genOneBooleanList(0, []).
genOneBooleanList(N, [H|R]):- N > 0, N1 is N - 1, member(H, [0, 1]), genOneBooleanList(N1, R).

% генерира списък с елементи всички булеви списъци с дължина Н
genLists(N,L):-bagof(L1, genOneBooleanList(N,L1), L).

genLists2(_,0,[]).
genLists2(N,K,[H|T]):-K>0, K1 is K-1, genOneBooleanList(N,H), genLists2(N,K1,T).

center(C,L):-member(C,L),sumD(L,C,N),not((member(C1,L), sumD(L,C1,N1), N1 < N)).

topCenter(C,L):-center(C,L),sumD(L,C,D), length(C,N), not((genOneBooleanList(N,L1), sumD(L,L1,D1), D1<D, not(member(L1,L)))).

natural(0).
natural(N): natural(N1), N is N1+1.

p11(N,L):-natural(M),genLists2(N,M,Lists),topCenter(L,C),length(C,N).
% не е тествано, но на идейно ниво е нещо такова
