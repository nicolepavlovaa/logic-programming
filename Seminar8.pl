% parent(maria, ivan).
% parent(ivan, hristo).
% parent(tania, hristo).
% parent(tania, petar).

% grandParent(X, Y):- parent(X, T), parent(T, Y).

% ancestor(X, Y):- parent(X,Y).
% ancestor(X, Y):- parent(X, T), ancestor(T, Y).

% isList([]).
% isList([_|_]).

% member1(X, [X|_]).
% member1(X, [_|T]):- member1(X, T).


% INF SUMMER 2021
% зад.1
parent(maria, ivan).
parent(ivan, peter).
parent(ivan, stoyan).

grandparent(X, Y):-parent(X,Z), parent(Z,Y).
sibling(X, Y):-parent(Z,X), parent(Z, Y), X \= Y.

% x е прародител на y
ancestor(X,Y):-parent(X,Z), ancestor(Z,Y).
% искаме да е рефлексивна релация, може да се раздели и на 2 реда, използваме анонимна променлива
% може да разменим 2та реда, но се обръща и редът на отговорите
% ancestor(X,X). не работи => произволна рефлексивност на (1,1) примерно
ancestor(X, X):-parent(X,_); parent(_, X).

% може да изчистим дъното, но разчитаме на parent да не е рефлексивна, ancestor2 също не е рефлексивна
ancestor2(X,Y):-parent(X,Y).
ancestor2(X,Y):-parent(X,Z), ancestor2(Z,Y).

% може да използваме и отрицание
ancestor3(X,X):-parent(_,X); not(parent(_,X)).
ancestor3(X,Y):-parent(X,Z), ancestor3(Z,Y).

% ако нямаме дъно връща false?, защото parent има краен брой решения (sentinel),това е предварително ограничение
% а ancestor винаги апелира към себе си, 
% ако ги разменим 2те неща в условието ще забие, изчерпва крайния брой резултати, отложените почват
% да се преудовлетворяват и са безкрай много и забива

% няма значение дъното дали е преди рекурсията, само решенията излизат в различен ред

% зад.2
% d(X, DX)
d(x, 1).
d(X, 0):-number(X).
d(X+Y,DX+DY):-d(X,DX), d(Y,DY).
d(X*Y,DX*Y+X*DY):-d(X,DX), d(Y,DY).
d(sin(X),cos(X)*DX):-d(X,DX).

% --------------------
% структури- списъци, графи и т.н.
% c е празен списък
empty(c).
add(X, C, f(X, C)).
member(X, f(X,_)).
member(X, f(_, Tail)):-member(X, Tail).

isList([]).
isList([_|_]).

% 3 варианта за first
% може да напишем [X,Y|L], за да отделим само първите n елемента, ако се налага
first(X,L):-L=[X|L].
first(X,L):-L=[X|_].
first(X,[X|_]).

% 2 варианта
second(X, [_,X|_]).
second(X, [_,Tail]):-first(X,Tail).

last(X, [X]).
last(X,[_|Tail]):-last(X,Tail).

member1(X,L):-first(X,L).
member1(X, [_|Tail]):-member1(X,Tail).

% A=[X|A1] L=[X|A1.B]
append1([],B,B).
append1([X|A1],B,[X|Tail]):-append1(A1,B,Tail).

firstApp(X,[H|Tail]):-append1([X], Tail, [H|Tail]).
% firstApp(X,L):-append1([X], _, L).

secondApp(X,L):-append1([_,X], _, L).
% secondApp(X,L):-append1([_], [X,_], L).

lastApp(X,L):-append1(_,[X],L).

% ако не сме дали конкретен Х, то member ще ни изреди всички елементи на списъка
% тоест това е краен генератор
memberApp(X,L):-append1(_, [X|_], L).

% insert(X,L,R). - чрез преудовлетворяване да се инсъртне на всяка възможна позиция
% [A,X]=C [C,B]=R, [A,B]=L => [A,X,B]=R, [A,B]=L
insert(X,L,R):-append1(A,B,L), append1(A,[X],C), append1(C,B,R).

% remove(X,L,R). - премахване на 1 участие на X в L.