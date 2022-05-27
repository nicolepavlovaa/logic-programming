:-use_module(library(clpfd)).

% 8
% Eliminate consecutive duplicates of list elements.
eliminate([],[]).
eliminate([X],[X]).
eliminate([A,A|T],R):-eliminate([A|T],R).
eliminate([A,B|T],[A|R]):-A\=B, eliminate([B|T],R).

% 9 Pack consecutive duplicates of list elements into sublists.
pack([],[]).
pack([X],[[X]]).
pack([A,A|T],[[A|T1]|R]):-pack([A|T],[T1|R]).
pack([A,B|T],[[A]|R]):- A\=B, pack([B|T],R).

% P18 (**) Extract a slice from a list. 
slice(L,A,B,R):-N is B-A+1, slice(L,N,R).

slice(_,0,[]).
slice([H|T],N,[H|R]):-N>0, N1 is N-1, slice(T,N1,R).

% P20 (*) Remove the K'th element from a list. 
removeKthElement(X,[X|T],0,T).
removeKthElement(X,[H|T],K,[H|R]):-K>0, K1 is K-1, removeKthElement(X,T,K1,R).

% P23 (**) Extract a given number of randomly selected elements from a list.
rnd_select(_,0,[]).
rnd_select(Xs,N,[X|Zs]) :- N > 0,
    length(Xs,L),
    I is random(L) + 1,
    removeKthElement(X,Xs,I,Ys),
    N1 is N - 1,
    rnd_select(Ys,N1,Zs).

% P24 (*) Lotto: Draw N different random numbers from the set 1..M.

% P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list
% In how many ways can a committee of 3 be chosen from a group of 12 people?

% el(X,L,R) - маха елемента Х от L и го връща в Х
el(X,[X|L],L).
el(X,[_|L],R) :- el(X,L,R).

% ще генерира и [1,2], и [2,1]
comb(_,0,[]).
comb(L,K,[X|R]):-K>0,member(X,L),delete(L,X,L1),K1 is K-1,comb(L1,K1,R).

% това работи напълно правилно
combination(0,_,[]).
combination(K,L,[X|Xs]) :- K > 0, el(X,L,R), K1 is K-1, combination(K1,R,Xs).

% P28 (**) Sorting a list of lists according to length of sublists
% L=[[1,2,3],[1,2],[10]]
isSorted([]).
isSorted([_]).
isSorted([H1,H2|L]):-length(H1,N1),length(H2,N2), N1=<N2, isSorted([H2|L]).

sortL(L,R):-permutation(L,R),isSorted(R).

% P31 (**) Determine whether a given integer number is prime. 
% is_prime(1).
is_prime(2).
is_prime(N):-integer(N), N>2, N mod 2 =\= 0, not(hasFactor(N,3)).

hasFactor(N,L):-N mod L =:= 0, L < N.
hasFactor(N,L):-N*N < L, L1 is L+2, hasFactor(N,L1).

% P32 (**) Determine the greatest common divisor of two positive integer numbers.
gcd(X,0,X):-X > 0.
gcd(A,B,D):-B > 0, C is A mod B, gcd(B,C,D).

% 12 mod 8 = 4 -> 4 != 0 -> continue,
%  8 mod 4 = 0 -> gcd = 4, end.

% The least common multiple (lcm) of a and b is their product divided 
% by their greatest common divisor (gcd) ( i.e. lcm(a, b) = ab/gcd(a,b)). 

