p(s(s(X)),Y).
a(s, swthrhs) :- b(42, 88), test(X).
a(X) :- b(Y).
a(X) :- a(s(X)), b(Y).
a(X) :- a(s(X)), b(s(0)).
b(15).
b(15,15).
b(X, 5).
b(s(1), 45, 67, s).
b(s(s(s(0)))).
b(mike).
son(john, Smith).

male(mike).
male2(swthrhs).
smurfers(X, Y) :- male(X), male2(Y).