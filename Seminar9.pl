% homework
append1([],B,B).
append1([X|A1],B,[X|Tail]):-append1(A1,B,Tail).

insert1(X,L,R):-append1(L1,L2,L), append1(L1, [X|L2], R).

remove1(X,L,R):-append1(L1,[X|L2],L), append1(L1,L2, R).

remove2(X,[X|Tail], Tail).
remove2(X, [H|Tail], [H|New]):-remove2(X,Tail,New).

% we can't use the other remove because of its definition
insert2(X,L,R):-remove2(X,R,L).

% зад.1
% може и без member, защото remove и member правят едно и също, но едното помни повече
permutation1(L,[X|P]):-member(X,L), remove1(X,L,M), permutation1(M, P).
permutation1([],[]).

% сменяме реда, взимаме първия ел. и го местим и т.н. докато горе той седеше
% най-дълго фиксиран
permutation2([],[]).
permutation2([H|Tail], R):-permutation2(Tail, Q), insert1(H,Q,R).

% зад.2 
% <, =<, >, >=
lessOrEqual(X,Y):- X=<Y.

isSorted([]).
isSorted([_]).
isSorted([X,Y|Tail]):-lessOrEqual(X,Y), isSorted([Y|Tail]).

% VxF |=| !Ex!F
% Ако имаме <=2 елемента, ще фейлне при апенда като опитаме да вземем
% 2 елемента и not ще го направи на истина
isSorted2(L):- not((append1(_, [X,Y|_], L), not(lessOrEqual(X,Y)))).

% зад.3
% p1(X,Y) <-> има ел. на Х, който е в ел. на У
% р2(Х,У) <-> има ел. на Х, който е във всеки ел. на У
% р3(Х,У) <-> всеки ел. на Х е в ел. на У.
% р4(Х,У) <-> всеки ел. на Х е във всеки ел. на У.

p1(X,Y):-member(A,X),member(A,B),member(B,Y).

% p2: EaVb member(a,b) <-> Ea!Eb !member(a,b)
p2(X,Y):-member(A,X), not((member(B, Y), not((member(A, B))))).

% p3: VaEb member(a,b) <-> !Ea !Eb member(a,b)
p3(X,Y):-not((member(A,X), not((member(A,B), member(B,Y))))).

% p4: VaVb member(a,b) <-> !Ea!(!(Eb! member(a,b)))  <-> !Ea Eb !member(a,b)
p4(X,Y):- not((member(A,X), member(B, Y), not(member(A,B)))).

% зад. 3
prefix(L,P):-append(P,_,L).
suffix(L,S):-append(_,S,L).

infix1(L,I):-suffix(L,S), prefix(S,I).

% може да зацикли??
infix2(L,I):-prefix(L,P), suffix(P,I).

% зад.4
subsequence([],[]).
subsequence([H|Tail], [H|R]):- subsequence(Tail,R).
subsequence([_|Tail], R):- subsequence(Tail,R).

% зад.5
% (Vx in L1)(x in L2) <-> Ex in L1 not(x in L2)
isSubset(L1,L2):-not((member(X,L1), not(member(X,L2)))).

isSubset1([],_).
isSubset1([H|Tail],L2):-member(H,L2), isSubset1(Tail,L2).

% зад.6
% m(L,M) да се генерират в М всички списъци, чийто елементи са елементи на L
% [a,b] -> [],[a],[a,a],[a,b,a,b,b,b]
% това решение не работи, защото винаги ще е успешно да се вземе 1вия ел. на L
m(L,[]).
m(L,[H|Tail]):-member(H,L),m(L,Tail).

% разменяме местата на генераторите, крайният генератор отива на 2ро място
% така оправяме проблема, ключово е че конюнкцията не е комутативна
% за всеки елемент от безкрайния генератор, ще има краен брой възможности
% генерирани от крайния генератор
% оказва се,че краен генератор следван от безкраен не работи добре
m1(L,[]).
m1(L,[H|Tail]):-m1(L,Tail),member(H,L).

% reverse(L,R).