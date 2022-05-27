% зад. 1 
% flatten(L,R) [[1,[[[[]],2]]]] -> [1,2]
isList([]).
isList([_|_]).

% тук ни трябва помощен метод
flatten1([],[]).
% flatten(a) -> [a] -> не напълно коректно
flatten1([X],X):-not(isList(X)).
flatten1([H|T], R):-flatten1(H,R1), flatten1(T,T1), append(R1, T1, R).
flatten(L,R):-isList(L), flatten1(L,R).

% flatten1([],[]).
% flatten1([H|Tail], [H|T1]):- not(isList(H)),flatten1(Tail,T1).
% isList тук е задължителна проверка, заради преудовлетворяването
% flatten1([H|T], R):-isList(H), flatten1(H,R1), flatten1(T,T1), append(R1, T1, R).

% зад. 2
% split(L,R) L=[a,b,c] -> R=[[a],[b],[c]], R=[[a,b],[c]], ..., R=[[a,b,c]]
% едно разбиване, при което конкатенирани частите трябва да дават целия списък
% т.е. append(X,T) = [H|T] и X!=H !!!, апенд е същата задача за разделяне на 2
% трябва да е != [], за да нямаме безкрайна лява рекурсия, т.е. да не взимаме винаги [] за префикс
split([],[]).
split(L,[X|R]):- append(X,Y,L), X\=[], split(Y,R).

% зад. 3 (контролно)
% sums(N,S)
% симетрично решение на горното
% sums(3) -> [1,1,1], [3], [2,1], ...
sums(0,[]).
sums(N,[H|T]):-between(1,N,H), M is N-H, sums(M,T).

% зад. 4 (контролно)
% []
% [A,B] ako A,B са дървета
% [[],[]]

% грешна дефиниция, защото имаме 2 безкрайни вложени цикъла
% второто ще върви безкрайно, само надясно, затова трябва да балансираме
% аналогия със задачата за наредени двойки
% tree([]).
% tree([A,B]):-tree(A), tree(B).

natural(0).
natural(N):-natural(N1), N is N1 + 1.

% M - броя на върховете в L, K-броя на върховете в R
tree(T):-natural(N), tree(N,T).
tree(0,[]).
tree(N,[L,R]):- N>0, N1 is N-1, between(0,N1,M), K is N1-M, tree(M,L), tree(K,R).

% зад. 5
% графи (ппц ще работим с неориентирани)
% G=<V,E>
% E=[[a,b],...]]

edge([_,E], X, Y):-member([X,Y], E); member([Y, X], E).

% имаме граф, начална A и крайна точка B, търсим път Path
% path([V,E], A, B, Path)
% трябва да внимаваме да не зациклим и ще търсим само простите пътища
% затова ще ни трябва още един помощен параметър, т.е.
% path(G,A,B,V,Path), първоначално е [].
path(G, A, B, Path):-path(G, A, B, [], Path).
% оправяме реда на пътя (защото всеки път сме добавяли като глава на списъка) и добавяме последния ноуд
path(_, B, B, V, R):-reverse([B|V], R).
% все още не сме стигнали целта, местим се на съседен връх на А, който е С, той не трябва да е част от пътя,
% за да не зациклим и оттам търсим път до края, като добавяме А в посетените
path(G, A, B, V, Path):- A\=B, edge(G, A, C),not(member(C,V)), path(G, C, B, [A|V], Path).

% дефинираме си граф, за да не го пишем всеки път
% graph(G), path(G,a,d,Path).
graph([[a,b,c,d], [[a,b], [b,c], [c,d], [c,a]]]).

% зад. 6
% ако имаме директно ребро + път който е с дължина повече от 2, значи има цикъп
% проверява дали има цикъл (и го връща)
hasCycle(G, P):-edge(G,A,B), path(G, B, A, P), length(P, N), N>2.

% зад. 7
% свързаност на граф = има път от всеки до всеки връх
% проверката е от всеки до всеки, т.е. трябва да включим отрицанието
% т.е. VxVy path(X,Y) <-> !Ex!(Vy path(x,y)) <-> !Ex!(!Ey! path(x,y)) <-> !Ex(Ey !path(x,y))
isConnected([V,E]):-not((member(X,V), member(Y,V), X\=Y, not(path([V,E], X, Y, _)))).

% за ориентиран граф ще ни бъде достатъчно да проверим дали от един връх
% можем да стигнем до всички останали
% Vy path(X,Y) <-> !Ey!(path(X,Y))
% не е ясно дали има примки в неориентираните графи ;д
isConnected2([[X|V],E]):-not((member(Y,V), X\=Y, not(path([[X|V], E], X, Y, _)))).

% зад. 8
% покриващо дърво - безтегловен крускал
% spanningTree([V,E], ST).
remove(X, L, R):- append(A, [X|B], L), append(A, B, R).

spanningTree([V,E], ST):- V=[H|T], spanningTree([V,E], [H], T, ST).
spanningTree(_, _, [], []).
spanningTree([V, E], Visited, NotVisited, [[U,W]|ST]):-
  member(U,Visited), 
  edge([V, E], U, W),
  member(W, NotVisited), 
  remove(W, NotVisited, NewNotVisited),
  spanningTree([V, E], [W|Visited], NewNotVisited, ST).

% ------------
spanningTree2([V,E], ST):- V=[H|T], spanningTree2([V,E], [H], T, [], ST).
spanningTree2(_, _, [], Curr, Curr).
spanningTree2([V, E], Visited, NotVisited, Curr, ST):-
  member(U,Visited), member(W, NotVisited),
  edge([V, E], U, W),
  remove(W, NotVisited, NewNotVisited),
  spanningTree2([V, E], [W|Visited], NewNotVisited, [[U,W]|Curr], ST).

isConnected3(G):- spanningTree(G, _).
hasCycleConnectedGraph([V,E]):- spanningTree([V,E], ST), length(E,N), length(ST, M), N > M.

% зад. 9
% генериране на всички критични върхове на графа
% критичен връх е такъв, който след махането му се увеличава броят на свързаните компоненти
% criticalVertex([V, E], X).
% не работи, трябва да се оправи!!!
criticalVertex([V,E], X):- member(A,V), member(B,V),member(X,V), 
  not(member(X, [A,B])), 
  path([V,E],A,B,P), 
  member(X,P),
  not((path([V,E], A, B, P1), not(member(X, P1)))).
