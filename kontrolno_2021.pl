% зад. 1
% R=[[a,b], [c,d], ...]
% S=[[x,y], [z,t], ...]
% дали композицията на релациите представени от R и S
% е релация на еквивалентност

% релация на еквивалентност: рефлексивност, транзитивност, симетричност
% Vx (member([x,_], R) or member([_,x], R)), member([x,x], R) <-> !Ex!((member([x,_], R) or member([_,x], R)), member([x,x], R))
reflexive(R):-not(((member([X,_], R); member([_,X], R)), member([X,X],R))).
% Vx,y,z member([X,Y],R), member([Y,Z],R), member([X,Z],R) <-> 
transitive(R):-not((member([X,Y],R), member([Y,Z],R), not(member([X,Z],R)))).
symmetric(R):-not((member([X,Y],R), not(member([Y,X],R)))).

equivalence(R):-reflexive(R), transitive(R), symmetric(R).

remove(X, [X|T], T).
remove(X, [H|T], [H|T1]):- remove(X, T, T1).

% R*S={<a,c>| Eb <a,b> in S, <b,c> in R}
composition(_,[],[]).
composition(R,[[A,B]|T],[[A,C]|T1]):-
  member([B,C],R),
  remove([B,C],R,R1),
  composition(R1,T,T1).

composition(R,[[_,B]|T],T1):-
  not(member([B,_],R)),
  composition(R,T,T1).

p1(R,S):-composition(R,S,C), equivalence(C).

% зад. 2
% ?- gen_terms([[f,3],[c,0],[r,2]],A).
% A = [c] ;
% A = [r, [c], [c]] ;
% A = [f, [c], [c], [c]] ;
% A = [r, [c], [r, [c], [c]]] ;
:- use_module(library(clpfd)).

build_term([[C,0]|TailSeq],[C],TailSeq).
build_term([[F,N]|Tail],[F|Terms],TailSeq):-N#>0,build_seq_term(Tail,N,Terms,TailSeq).

build_seq_term(Seq,0,[],Seq).
build_seq_term(Seq,N,[T1|Terms],TailSeq):-
  N#>0, N1#=N-1,
  build_term(Seq,T1,TailSeq1),
  build_seq_term(TailSeq1,N1,Terms,TailSeq).

gen_sequence(L,0,[]).
gen_sequence(L,N,[A|Seq]):-N#>0,mem1(A,L),N1#=N-1,gen_sequence(L,N1,Seq).

% mem1(A,L) given L generates in A when resatisfied all the elements
% of L.
mem1(A,[A|_]).
mem1(A,[_|L]):-mem1(A,L).
% Finally, we generate the positive integers. nat(N) generates positive
% integers in N.
nat(1).
nat(N):-nat(N1),N#=N1+1.
% Putting everything together: given L - a list of
% (generalised) functional symbols:
% 1. generate N - a natural number
% 2. generate a sequence, Seq of length N of elements of L
% 3. build a term from the sequence Seq
% 4. Check whether it is valid, by verifying that the sequence has been
% completely parsed.
%
gen_terms(L,Term):-nat(N),gen_sequence(L,N,Seq),build_term(Seq,Term,SeqTail),SeqTail=[].



% и т.н.
% https://github.com/YanaRGeorgieva/Logic-programming/blob/master/%D0%97%D0%B0%D0%B4%D0%B0%D1%87%D0%B8%20%D0%BE%D1%82%20%D0%BC%D0%B8%D0%BD%D0%B0%D0%BB%D0%B8%20%D0%B3%D0%BE%D0%B4%D0%B8%D0%BD%D0%B8/%D0%9A%D0%BE%D0%BD%D1%82%D1%80%D0%BE%D0%BB%D0%BD%D0%B8%20%D0%9A%D0%9D%202020-21/09.01.2021%20exam%20Prolog/terms_solution.pl
