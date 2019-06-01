natural(zero).
natural(succ(N)) :- natural(N).

cmp(zero, zero, eq).
cmp(zero, succ(A), lt).
cmp(succ(A), zero, gt).
cmp(succ(A), succ(B), R) :- cmp(A, B, R).

add(A, zero, A).
add(A, succ(B), succ(R)) :- add(A, B, R).

mult(A, zero, zero).
mult(A, succ(B), R) :- mult(A, B, M), add(A, M, R).

% For convenience
short(one,   succ(zero)).
short(two,   succ(succ(zero))).
short(three, succ(succ(succ(zero)))).
short(four,  succ(succ(succ(succ(zero))))).
short(five,  succ(succ(succ(succ(succ(zero)))))).
short(six,   succ(succ(succ(succ(succ(succ(zero))))))).