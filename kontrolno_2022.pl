% зад. 1
% isLipschitz(L,C)
% L=[[x=1,f(x)=2],[y=3,f(y)=5],...]
% C - константа
% VxVy x,y e L (abs(f(x)-f(y)) =< c*abs(x-y))
isLipschitz(L,C):-not((member([X,FX],L), member([Y,FY],L), not(abs(FX-FY) =< C*abs(X-Y)))).

% зад. 2
% приемлива подредба = книгите с един и същ автор са само на един от редовете
% дисбаланс = средно аритметично на разликите на броя на книгите, които са на различни рафтове
% order(Books, Shelf1, Shelf2, Shelf3)
% Books = [[title, author], ...]

% Vauthor member([_,author], Shelf1) not((member([_,author], Shelf2), member([_author, Shelf3]))).

split([], [], [], []).
split([Book|T], [Book|Shelf1], Shelf2, Shelf3):-split(T, Shelf1, Shelf2, Shelf3).
split([Book|T], Shelf1, [Book|Shelf2], Shelf3):-split(T, Shelf1, Shelf2, Shelf3). 
split([Book|T], Shelf1, Shelf2, [Book|Shelf3]):- split(T, Shelf1, Shelf2, Shelf3).

% взимаме всички автори на дадените книги
authors([],[]).
authors([[_,Author]|T], [Author|Authors]):-authors(T, Authors).

% "приемлива" подредба за автор е такава че да съществува рафт
% такъв че да не съществува книга от този автор, която да не е член на този рафт
acceptabeForAuthor(Books, Author, Shelves):-
  member(Shelf, Shelves), 
  not((
    member([Title,Author], Books), 
    not(member([Title,Author], Shelf))
    )).

% "приемлива" подредба за всички автори е такава, че да не съществува автор
% за който да не е "приемлива" подредбата
acceptable(Books, Authors, Shelf1, Shelf2, Shelf3):-
  not((
    member(Author, Authors),
    not(acceptabeForAuthor(Books, Author, [Shelf1, Shelf2, Shelf3]))
  )).

% дисбалансът е средно аритметично на разликите на броя книги на различните рафтове
disbalance(Shelf1, Shelf2, Shelf3, D):-
  length(Shelf1,N1),length(Shelf2,N2),length(Shelf3,N3),
  D is (abs(N1-N2)+abs(N2-N3)+abs(N3-N1))/3.

common(L1,L2):-member(X,L1), member(X,L2).

% разделяме книгите на 3 рафта като не трябва да има повтарящи се книги 
% между различните автори
generate(Books,Shelf1,Shelf2,Shelf3):-
  split(Books,Shelf1,Shelf2,Shelf3),
  not(common(Shelf1,Shelf2)),
  not(common(Shelf2, Shelf3)),
  not(common(Shelf1,Shelf3)).

% генерираме всички възможни разбивания на книгите на 3 рафта
% взимаме само тези, които са приемливи
% смятаме дисбаланса и проверяваме дали не съществува друга подредба,
% различна от текущата, т.е. поне един рафт има различни книги в него
% такава че отново да бъде приемлива и да има по-малък дисбаланс
order(Books,Shelf1,Shelf2,Shelf3):-
  authors(Books, Authors),
  generate(Books, Shelf1, Shelf2, Shelf3),
  acceptable(Books, Authors, Shelf1, Shelf2, Shelf3),
  disbalance(Shelf1, Shelf2, Shelf3, D),
  not((
    generate(Books, Shelf11, Shelf22, Shelf33),
    (Shelf1 \= Shelf11; Shelf2 \= Shelf22; Shelf3 \= Shelf33),
    acceptable(Books,Authors,Shelf11,Shelf22,Shelf33),
    disbalance(Shelf11,Shelf22,Shelf33,D1),
    D1 < D
    )).