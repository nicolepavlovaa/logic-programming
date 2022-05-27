% member, empty, isList, first, last
member(H,[H|_]).
member(X,[_|Tail]):-member(X, Tail).

empty([]).

isList([]).
isList([_|_]).

first([X], X).
first([H|_], H).

last([H],H).
last([_|T], X):-last(T,X).

append([],L2,L2).
append([H|T], L2, [H|T1]):- append(T,L2,T1).

insert(X,L,R):-append(L1,L2,L), append(L1, [X|L2], R).

remove(X,L,R):-append(L1,[X|L2], L), append(L1,L2,R).

remove2(X,[X|T], T).
remove2(X, [H|T], [H|R]):- remove2(X, T, R).

permutation([],[]).
permutation(L,[X|T]):-member(X,L), remove(X,L,L1), permutation(L1, T).

permutation2([],[]).
permutation2([H|T], R):- permutation2(T,T1), insert(H,T1,R).

subsequence([],[]).
subsequence([H|T], [H|T1]):- subsequence(T,T1).
subsequence([_|T], R):- subsequence(T,R).

% m(L,M) да се генерират в М всички списъци, чийто елементи са елементи на L
% [a,b] -> [],[a],[a,a],[a,b,a,b,b,b]
m(_,[]).
m(L, [H|T]):- m(L, T), member(H,L).

reverse(L, R):- reverse(L, [], R).
reverse([], R, R).
reverse([H|T], Curr, R):- reverse(T, [H|Curr], R).

palindrome([]).
palindrome([_]).
palindrome([H|T]):-append(T1,[H],T), palindrome(T1).

isSorted([]).
isSorted([_]).
isSorted([X,Y|T]):- X=<Y, isSorted([Y|T]).

psort(L,R):-permutation(L,R),isSorted(R).

% vy x<=y 
min(X,L):-remove(X,L,L1), not((member(Y,L1), not(X=<Y))).

selectionSort([],[]).
selectionSort(L,[X|R1]):-min(X,L), remove(X,L,L1), selectionSort(L1, R1).

split([],_,[],[]).
split([H|Tail], X, [H|L1], L2):-H=<X, split(Tail, X, L1, L2).
split([H|Tail], X, L1, [H|L2]):-not(H=<X), split(Tail, X, L1, L2).

quickSort([],[]).
quickSort([H|T],R):-split(T,H,L1,L2),quickSort(L1,R1),quickSort(L2,R2), append(R1,[H|R2],R).

split2([],[],[]).
split2([X], [X], []).
split2([X,Y|T],[X|L1],[Y|L2]):- split2(T, L1, L2).

merge([],A,A).
merge(A,[],A).
merge([H1|T1],[H2|T2],[H1|T]):-H1=<H2, merge(T1, [H2|T2], T).
merge([H1|T1],[H2|T2],[H2|T]):-not(H1=<H2), merge([H1|T1], T2, T).

mergeSort([],[]).
mergeSort([X],[X]).
mergeSort(L,R):-L\=[],L\=[_],split2(L,A,B),mergeSort(A,A1),mergeSort(B,B1),merge(A1,B1,R).

% maketree, add, leftrootright, sort

makeTree([],empty).
makeTree([H|L],Tree):-makeTree(L, Tree1), add(H,Tree1, Tree).

add(X, empty, tree(empty,X,empty)).
add(X, tree(LT1,R,RT1), tree(LT2,R,RT1)):- X=<R, add(X, LT1, LT2).
add(X, tree(LT1,R,RT1), tree(LT1,R,RT2)):- not(X=<R), add(X, RT1, RT2).

leftRootRight(tree(LT,R,RT),L):-leftRootRight(LT, L1),leftRootRight(RT,L2), append(L1, [R|L2], L).
treeSort(L,S):-makeTree(L,T), leftRootRight(T,S).

% L-списък от списъци, генерираме декартови произведения
% като взимаме всеки един елемент от всеки списък и го слагаме в резултата
m1([],[]).
m1([H|T],[X|R]):-member(X,H),m1(T,R).

% len,sum,nthEl
len([],0).
len([_|T], N):- len(T, N1), N is N1+1.

sum([],0).
sum([H|T],N):-sum(T,N1), N is N1+H. 

nthEl([H|_],0,H).
nthEl([_|T],N,X):- nthEl(T,N1,X), N is N1+1.

% natural, even
natural(0).
natural(N):-natural(N1), N is N1+1.

% ----------------
% subsequence, prefix, infix, suffix, isSubset

subsequence1([],[]).
subsequence1([H|T],[H|S]):-subsequence1(T,S).
subsequence1([_|T],S):-subsequence1(T,S).

prefix(L,P):-append(P,_,L).
suffix(L,S):-append(_,S,L).

isSubset(L,S):-not((member(X,S), not(member(X,L)))).

% m(L,M) да се генерират в М всички списъци, чийто елементи са елементи на L
% [a,b] -> [],[a],[a,a],[a,b,a,b,b,b]

m11(_,[]).
m11(L,[X|M]):-m11(L,M), member(X,L).

% reverse, palindrome
reverse11([],[]).
reverse11(L,R):-reverse11(L,[],R).

reverse11([],Curr,Curr).
reverse11([H|T],Curr,R):-reverse11(T,[H|Curr],R).

palindrome11([H|T]):-append(T1,[H],T),palindrome(T1).

% L-списък от списъци, генерираме декартови произведения
% като взимаме всеки един елемент от всеки списък и го слагаме в резултата
% пример: [1,2]x[a,b] -> [1,a],[1,b],[2,a],[2,b]
m12([],[]).
m12([H|T],[X|R]):-member(X,H),m12(T,R).

% integer,between,range
int(N):-N is 0.
int(N):-natural(N1), N1 =\= 0, sign(N1,N).
sign(N1,N):-N is N1.
sign(N1,N):-N is (-1)*N1.

between(A,_,A).
between(A,B,X):- A<B, A1 is A+1, between(A1,B,X).

range(A,B,[]):- A>B.
range(A,B,[A|T]):-A=<B, A1 is A+1,range(A1,B,T).

% pairNatural
pair(A,B):-natural(S),between(0,S,A), B is S-A.

% списък от елементи със сума S, т.е. сума на н-орка, сума на N елемента от L трябва да е S
% решаваме у-то x1+x2+...+xn=s
sumL(0,0,[]).
sumL(N,S,[H|T]):-N>0,between(0,S,H), S1 is S-H, N1 is N-1, sumL(N1,S1,T).

% всички н-орки, т.е. Н да може да се мени
norks(S,L):-natural(N),sumL(N,S,L).

finiteNumbers(L):-pair(N,S), sumL(N,S,L).

% fibonacci
fib(X):-fib(X,_).
fib(0,1).
fib(Y,Z):-fib(X,Y), Z is X+Y.

% a0 = a1 = a2 = 1
% a(n+3) = an + a(n+1) {+ 0*a(n+2)}
a(1,1,1).
a(Y,Z,T):-a(X,Y,Z), T is X+Y.
a(X):-a(X,_,_).

% ------------
% flatten
flatten1([],[]).
flatten1([H|Tail], [H|T1]):- not(isList(H)),flatten1(Tail,T1).
flatten1([H|T], R):-isList(H), flatten1(H,R1), flatten1(T,T1), append(R1, T1, R).

% split(L,R) L=[a,b,c] -> R=[[a],[b],[c]], R=[[a,b],[c]], ..., R=[[a,b,c]]
% едно разбиване, при което конкатенирани частите трябва да дават целия списък
split([],[]).
split(L,[X|R]):-append(X,Y,L), split(Y,R).

% sums(N,S)
% sums(3) -> [1,1,1], [3], [2,1], ...
sums(S,L):-between(0,S,N),sums1(S,N,L).

sums1(0,0,[]).
sums1(S,K,[H|T]):-K>0,between(1,S,H), S1 is S-H, K1 is K-1, sums1(S1,K1,T).

% % G=<V,E>
% E=[[a,b],...]]
graph([[a,b,c,d], [[a,b], [b,c], [c,d], [c,a]]]).

edge([_,E],X,Y):-member([X,Y],E);member([Y,X],E).

path(G,A,B,Path):-path(G,A,B,[],Path).

path(_,A,A,Curr,Res):-reverse([A|Curr],Res).
path(G,A,B,Path,Res):-A\=B,edge(G,A,X),not(member(X,Path)), path(G,X,B,[A|Path],Res).

% hasCycle, isConnected
hasCycle1(G, P):-edge(G,A,B), path(G,B,A,P), length(P,N), N>2.

isConnected([V,E]):-not((member(X,V),member(Y,V),not(path([V,E],X,Y,_)))).

spanningTree([V,E],ST):-V=[H|T],spanningTree([V,E],[H],T,ST).

spanningTree(_,_,[],[]).
spanningTree([V,E],Visited,NotVisited,[[X,Y]|ST]):-
  member(X,Visited),
  edge(X,Y,_),
  member(Y,NotVisited),
  remove(Y,NotVisited,NewNotVisited),
  spanningTree([V,E],[Y|Visited],NewNotVisited,ST).

% criticalVertex

% ---------------------------------
% length, between, range, append, perm, fact
:-use_module(library(clpfd)).

lennn([],0).
lennn([_|T],N):-lennn(T,N1), N is N1+1.

lennn1([],N):-N#=0.
lennn1([_|T],N):-N#>0,N#=N1+1,lennn1(T,N1).

betweenn(A,B,A):-A=<B.
betweenn(A,B,X):-A<B, A1 is A+1, betweenn(A1,B,X).

betweenn1(A,B,A):-A#=<B.
betweenn1(A,B,X):-A#<B, A1#=A+1, betweenn1(A1,B,X).

rangee(A,B,[]):-A>B.
rangee(A,B,[A|T]):-A=<B, A1 is A+1, rangee(A1,B,T).

betweenn2(A,B,X):-X in A..B, label([X]).
betweenn3(A,B,X):-X#>=A, X#=<B, label([X]).

appendd([],L2,L2).
appendd([H|T],L2,[H|L]):-appendd(T,L2,L).

memberr(H,[H|_]).
memberr(X,[_|Tail]):-memberr(X, Tail).



% I.1 Редиците на Фарей, Fn , са редици от двойки естествени числа, които
% се дефинират рекурсивно за n ≥ 1 по следния начин:
% • F1 = [[0, 1], [1, 1]];
% • Fn+1 се получава от Fn , като между всеки два последователни члена
% [a, b] и [c, d] на Fn, за които b+d = n+1, се добавя двойката [a+c, n+1].
% Да се дефинира на пролог едноместен предикат farey(F), който при пре-
% удовлетворяване генерира в F всички редици на Фарей. 

farey(L):-f(L,1).

f([[0,1],[1,1]],1).
f(FnPlus1,Nplus1):-Nplus1 #= N+1, f(Fn,N), generateFnPlus1(Fn,FnPlus1,Nplus1).

generateFnPlus1([X],[X],_).
generateFnPlus1([[A,B],[C,D]|Fn],[[A,B],[K,Nplus1]|FnPlus1],Nplus1):-Nplus1 #= B+D, K #= A+C, generateFnPlus1([[C,D]|Fn], FnPlus1, Nplus1).
generateFnPlus1([[A,B],[C,D]|Fn],[[A,B]|FnPlus1],Nplus1):-Nplus1 #\= B+D, generateFnPlus1([[C,D]|Fn], FnPlus1, Nplus1).

% не можем да го питаме кое е числото чийто факториел е F
fact(0, 1).
fact(N, F):- N>0, N1 is N-1, fact(N1, F1), F is N*F1.

fact2(N,F):- N #= 0, F #= 1.
fact2(N,F):- N #> 0, F #= F1 * N, fact2(N-1, F1).

% flatten,member,append,remove,reverse

member(X,[X|L]).
member(X,[_|T]):-member(X,T).

append([],L2,L2).
append([H|T],L2,[H|L]):-append(T,L2,L).

flatten([],[]).
flatten([H|T],[H|R]):-not(isList(H)),flatten(T,R).
flatten([H|T],R):-isList(H),flatten(H,X),flatten(T,T1),append(X,T1,R).

% fact, fib, perm
permutation([],[]).
permutation(L,[H|T]):-remove(H,L,L1), permutation(L1,T).

fib(X):-fib(X,_).
fib(0,1).
fib(Y,Z):-fib(X,Y), Z is X+Y.

% a0 = a1 = a2 = 1
% a(n+3) = an + a(n+1) {+ 0*a(n+2)}
a(X):-a(X,_,_).
a(1,1,1).
a(X,Y,Z):-a(T,X,Y), Z is X + T.
