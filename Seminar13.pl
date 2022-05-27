% only integers - finite domains
:-use_module(library(clpfd)).

% #= #<= #>= #> #<
% ограниченията винаги са в началото

% зад. 1
% стар вариант
% не може да се използва за генератор
len([], 0).
len([_|T], N):- len(T,M), N is M+1.

% нов вариант
% len2([],0). не е същото, 3-3 !=0, а като се използва ограничение, се предизвиква
% аритметика и се пресмята
len2([],N):- N #= 0.
% има проблем с това че не сме ограничили М и N, може да намалява безкрайно
% 0 = -1 + 1 и продължаваме нататък -> нямаме пълна коректност
len2([_|T], N):- N #= M+1, len2(T,M).

% ограничаваме N или M, или и двете
% пазят се като неравенства в паметта
len3([],N):- N #= 0.
len3([_|T], N):- N #>0, N #= M+1, len3(T,M).

len4([], N):- N #= 0.
len4([_|T], N):- N #> 0, len4(T, N-1).
% label([X,Y]) - остойностява,т.е. получаваме конкретни ст-ти, които са решения
% на даденото уравнение, иначе получаваме само допустимите стойности

evenLen([]).
evenLen([_,_|L]):-evenLen(L).

% ясно е че е число _, защото библиотеката е за целите числа
% не се интересуваме как се изчислява, трябва просто да е 2*...
evenLen2(L):-len4(L,2*_).

% зад. 2
between(A,B,A):- A =< B.
between(A,B,C):- A < B, A1 is A+1, between(A1,B,C).

between2(A,B,A):- A #=< B.
% тук навсякъде се извършва пресмятане, защото а1=а+1
between2(A,B,C):- A #< B, A1 #= A+1, between(A1,B,C).

% не може да напишем (C,B,C)
% защото на between3 се подава примерно 1+1 като терм и се проверява, че
% 1+1 <= 3, но не се остойностява, продължава да е терм, т.е. генерира се
% редица от типа 1, 1+1, 1+1+1, въпреки че спира пак когато трябва,
% не се казва никъде стойността на А е 3, тя продължава да е 1+1+1
% ако е сложена ст-та му да е 3, продължава със следващото условие
between3(A,B,C):- A #=< B, C#=A.
between3(A,B,C):- A #< B, between3(A+1, B, C).

% RANGE -> ..
% UNION -> \/
% INF, SUP
% използваме label ако искаме да е генератор
% label и подобни можем да ги използваме наготово
between4(A,B,C):- C in A..B, label([C]).
between5(A,B,C):- C #=< B, C #>= A, label([C]).

% зад. 3
range(A,B,[]):- A #> B.
range(A,B,[A|R]):- A #=< B, A1 #= A+1, range(A1,B,R).

% трябва ни списък с дължина A+B-1, всички да са различни, образуват верига
% която е със <, и върни единственото L, което се получава
range2(A,B,L):- len4(L, B-A+1), all_distinct(L), chain(L, #<), L ins A..B, label(L).

% зад. 4
nthElement([H|_], 0, H).
nthElement([_|T], N, X):- nthElement(T, M, X), N is M+1.
% това не винаги ще работи, защото не винаги n ще има стойност
% ще върне нещо само първия път, после ще се счупи при N и X неизвестни
%  nthElement([_|T], N, X):- M is N-1, nthElement(T, M, X).

nthElement2([H|_], N, H):- N #= 0.
nthElement2([_|T], N, X):- N #>= 0, nthElement2(T, N-1, X).

% зад. 5 
% пермутация
remove(X,L,R):-append(A, [X|B], L), append(A,B,R).

% Гледаме го от гледна точка на пермутацията
% ако махнем елемент от списъка, получаваме списък Q, R e такъв, че
% е някаква пермутация на Q.
perm([],[]).
perm(L, [H|R]):- remove(H,L,Q), perm(Q,R).

perm2(L, R):- perm2(L, [], R).
perm2([], R, R).
perm2(L, Curr, R):-member(X, L), remove(X,L,L1), perm2(L1, [X|Curr], R).

insert(X,L,R):-append(A,B,L), append(A,[X],C), append(C,B,R).

perm3([],[]).
perm3([H|Tail], R):-perm3(Tail, Q), insert(H,Q,R).

% дължината е N, броят на индексите е N и всички индекси са различни
% maplist мапва дадения предикат към елементите на списъка
% nthElement2 ппц приема 3 аргумента, но ние тук му подаваме един, а другите
% си ги взима от I и P
% I е списък с елементи от 0 до N1, където всички са различни
perm4(L,P):-len4(L,N), len4(I, N), N1 is N-1, I ins 0..N1, all_distinct(I), maplist(nthElement2(L), I, P).

% зад. 6
% не можем да го питаме кое е числото чийто факториел е F
fact(0, 1).
fact(N, F):- N>0, N1 is N-1, fact(N1, F1), F is N*F1.

% няма как N*F1 да се подаде директно като аргумент на fact2
fact2(N,F):- N #= 0, F #= 1.
fact2(N,F):- N #> 0, F #= F1 * N, fact2(N-1, F1).

% зад. 7
graph([[a,b,c,d], [[a,b], [b,c], [c,d], [c,a]]]).
edge([_,E], X, Y):-member([X,Y], E); member([Y, X], E).

% в зависимост на това колко арг. има предиката, подаваме толкова + и -
% ако е + ще се пази стойността на аргумента за всяко изпълнение
% ако е - ще се игнорира
:- table path(+,+,+,-).
path(_, Y, Y, [Y]).
path(G, X, Y, [X|P]):- edge(G,X,Z), path(G,Z,Y,P).

% зад. 8
fib(0,1).
fib(Y, Z):-fib(X,Y), Z is X+Y.
fib(X):- fib(X, _).

% dummy fib
n(X):- between(0, inf, X).

:-table fib2(+,-).
fib2(N,0):- N #= 0.
fib2(N,1):- N #= 1.
% N1 и N2 може да не се смятат отвън
fib2(N,Z):- N #> 1, Z #= X+Y, N1 #= N-1, N2 #= N-2, fib2(N1,X), fib2(N2, Y).

fib2(X):- n(N), fib2(N, X).

% зад. 9
% bagof, listof - изчаква да приключи between и събира резултата от изпълнението
% ще гръмне ако е безкрайно
range3(A,B,R):-bagof(X, between(A,B,X), R).

% зад. 10
% I.1 Редиците на Фарей, Fn , са редици от двойки естествени числа, които
% се дефинират рекурсивно за n ≥ 1 по следния начин:
% • F1 = [[0, 1], [1, 1]];
% • Fn+1 се получава от Fn , като между всеки два последователни члена
% [a, b] и [c, d] на Fn, за които b+d = n+1, се добавя двойката [a+c, n+1].
% Да се дефинира на пролог едноместен предикат farey(F), който при пре-
% удовлетворяване генерира в F всички редици на Фарей. 

farey(L):- generateFarey(L, _).

generateFarey([[0, 1], [1, 1]], 1).
generateFarey(FNplus1, Nplus1):- Nplus1 #= N + 1, generateFarey(FN, N), addPairs(FN, Nplus1, FNplus1).

addPairs([X], _, [X]).
addPairs([[A, B], [C, D]|Tail], Nplus1, [[A, B], [AplusC, Nplus1]|R]):- 
        B + D #= Nplus1, AplusC #= A + C, 
        addPairs([[C, D]|Tail], Nplus1, R).
      
addPairs([[A, B], [C, D]|Tail], Nplus1, [[A, B]|R]):- 
        B + D #\= Nplus1, 
        addPairs([[C, D]|Tail], Nplus1, R).

