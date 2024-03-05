m(L,M) :- q(L,M,0).

q([], [], N).
q([A|B], [R|M], N) :- d(A,N,R), q(B,M,h(N)).

d(L, 0, L).
d([H|T], h(N), R) :- d(T,N,R).

