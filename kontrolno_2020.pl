% зад. 1
generateList(0,[]).
generateList(Number,[Rem|T]):-
  Number \= 0,
  Next is Number // 8, 
  Rem is Number mod 8, 
  generateList(Next,T).

% indexes([],[]).
% indexes([_],[0]).
% indexes(L,[I|T]):-length(L,N), N1 is N-1, indexes(N1,[],[I|T]).

% indexes(0,[0|Curr],[0|Curr]).
% indexes(N,Curr,R):-N \= 0, N1 is N-1, indexes(N1, [N|Curr], R).

nthElement([H|_],0,H).
nthElement([_,T],N,X):-N1 is N-1, nthElement(T,N1,X).

cond1(L):-
  nthElement(L,U,AU),
  nthElement(L,V,AV),
  V =:= U // 8,
  AU mod 7 =\= 0,
  (AU-AV-1) mod 6 =\= 0.

cond2(L):-
  nthElement(L,U,AU),
  nthElement(L,V,AV),
  V =:= U // 8,
  AU mod 6 =\= 0,
  (AV-AU-1) mod 7 =\= 0.

byteTreeNum(N):-generateList(N,L), (cond1(L); cond2(L)).

% зад. 2
% L is a list of lists
% getNElLists([],_,[]).
% getNElLists([H|T],N,R):-length(H,N1), N1 =\= N, getNElLists(T,N,R).
% getNElLists([H|T],N,[H|R]):-length(H,N), getNElLists(T,N,R).

% Vy member(y,l) x>=y
% maxNumber(L,M):-not((member(X,L), not(X>=M))).

permutation([],[]).
permutation(L,[H|T]):-member(H,L), delete(L,H,L1), permutation(L1,T).

anagrams(L):-not((member(L1,L), member(L2,L), not(permutation(L1,L2)))).

subsequence([],[]).
subsequence([X|T],[X|Seq]):-subsequence(T,Seq).
subsequence([_|T],Seq):-subsequence(T,Seq).

% поредица с мин дължина М съдържаща всички м елементни списъци които са анаграми
% cond1(L,M,Seq):-getNElLists(L,M,L1),subsequence(L1,Seq),anagrams(Seq), length(Seq, N), N>=M.

% maxSubs(L,M,Seq):-cond1(L,M,Seq),not((cond1(L,M1,Seq), M1 > M)).

% може би без да използвам generate
% пиша само нещата, които са като "условия", а генерирането на редицата
% го извършвам в крайния предикат, като казвам че това е редица от списъци, които са елементи 
% на Л, отговарящи на условието и не съществува друга редица с елементи отговарящи на същото
% условие, но с М1 > M

% всички редици са анаграми, броят им е поне М
first([H|_],H).
cond1(S,M):-anagrams(S),length(S,M1), first(S,H), length(H,M), M1>=M.
cond2(S,M):-anagrams(S),length(S,M1), first(S,H), length(H,M), M1>=M-2.

maxAnagrams(L,M):-subsequence(L,S), cond1(S,M), cond2(S,M), not((subsequence(L,S1), cond1(S1,M1), cond2(S1,M1), M1 > M)).