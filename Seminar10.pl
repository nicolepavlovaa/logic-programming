% зад.1
reverse([],[]).
reverse([H,Tail],R):-reverse(Tail,R1),append(R1,[H],R).

% инициализираме стека с [] и използваме помощен предикат с 3 аргумента
% на всяка стъпка взимаме главата на списъка и я слагаме в стека
% списъка така се обръща, защото всеки път слагаме главата на опашката от предния път
% пред последно добавения елемент, т.е. нещо от типа на:
% [1|[2,3]] -> [1]; [2|[3]] -> [2,1]; [3|[]] -> [3,2,1] 
% когато изпразним началния списък копираме стека в резултата, това е дъното на рек-та
reverse1(L,R):-reverse1(L,[],R).
reverse1([],Stack,Stack).
reverse1([H|Tail],Stack,Result):-reverse1(Tail,[H|Stack],Result).

% зад.2
palindrome([]).
palindrome([_]).
palindrome(L):-append([H|Tail],[H],L), palindrome(Tail).

palindrome1(L):- reverse(L,L).

% зад.3
isSorted(L):-not((append(_,[X,Y|_],L),not(X=<Y))).
psort(L,R):-permutation(L,R),isSorted(R).

% minimal Vx(x=<m -> m=x)
% least Vx(m=<x) <-> !Ex(!m=<x)
min1(L,M):-member(M,L),not((member(X,L), not(M=<X))).

min2Arg(A,B,A):-A=<B.
min2Arg(A,B,B):-B=<A.

% тук min2 и min2Arg не могат да се разменят, защото М1 няма да е дефинирано?
min2([X],X).
min2([H|Tail],M):-min2(Tail,M1),min2Arg(H,M1,M).

% помощният предикат горе е за да не се повтаря 2 пъти изчислението като тук:
min21([H|Tail],H):-min21(Tail,M1),H=<M1.
min21([H|Tail],M1):-min21(Tail,M1),not(H=<M1).

% зад.4
% selection sort
remove1(X,L,R):-append(L1,[X|L2],L), append(L1,L2, R).

% мое решение, сортира в намаляващ ред, т.е. reverse-ва и сортира
selectionSort([],[]).
selectionSort(L,R):-selectionSort(L,[],R).
selectionSort([],S,S).
selectionSort(L,S,R):-min2(L,M),remove1(M,L,New),selectionSort(New,[M|S],R).

% тяхно решение, сортира в нарастващ ред
selectionSort2([],[]).
selectionSort2(L,[M|S]):-min2(L,M),remove1(M,L,New),selectionSort2(New,S).

% зад.5
% quick sort

% не работи
% split(_,[],[],[]).
% split(X,[H|Tail],A,B):-X=<H,split(X,Tail,[H|A],B).
% split(X,[H|Tail],A,B):-not(X=<H),split(X,Tail,A,[H|B]).

% работи
split(_,[],[],[]).
split(X,[H|Tail],[H|A],B):-X=<H,split(X,Tail,A,B).
split(X,[H|Tail],A,[H|B]):-not(X=<H),split(X,Tail,A,B).

quickSort([],[]).
quickSort([X|L],S):-split(X,L,A,B),quickSort(A,SA),quickSort(B,SB),append(SA,[X|SB],S).

% зад.6
% merge sort
split2([],[],[]).
split2([X],[X],[]).
split2([X,Y|Tail],[X|A],[Y|B]):-split2(Tail,A,B).

merge(L,[],L).
merge([],R,R).
merge([X|A],[Y|B],[X|S]):-X=<Y,merge(A,[Y|B],S).
merge([X|A],[Y|B],[Y|S]):-not(X=<Y),merge([X|A],B,S).

mergeSort([],[]).
mergeSort([X],[X]).
% може да не сложим и отрицание на [] и [x] на долния ред?
% но тогава безкрайно можем да искаме решения и то ще връща едно и също
mergeSort(L,S):-L\=[],L\=[_],split2(L,A,B),mergeSort(A,SA),mergeSort(B,SB),merge(SA,SB,S).

% зад.7
% двоично наредено дърво, сортиране
makeTree([],empty).
makeTree([H|L],Tree):-makeTree(L,Tree1),add(H,Tree1,Tree).

add(X,empty,tree(empty,X,empty)).
add(X,tree(LT,R,RT),tree(LT1,R,RT)):-X=<R,add(X,LT,LT1).
add(X,tree(LT,R,RT),tree(LT,R,RT1)):-not(X=<R),add(X,RT,RT1).

% обхождаме ляво-корен-дясно, за да вземем елементите, те ще са сортирани
leftRootRight(empty,[]).
leftRootRight(tree(LT,R,RT),L):-leftRootRight(LT,LT1),leftRootRight(RT,RT1),append(LT1,[R|RT1],L).

treeSort(L,S):-makeTree(L,T),leftRootRight(T,S).

% зад.8 - от контролно
% L-списък от списъци, генерираме декартови произведения
% като взимаме всеки един елемент от всеки списък и го слагаме в резултата
% пример: [1,2]x[a,b] -> [1,a],[1,b],[2,a],[2,b]

d([],[]).
d([H|Tail],[A|R]):-member(A,H),d(Tail,R).