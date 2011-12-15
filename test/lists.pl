
possible(Anything).

% member(-Elem, +List):
%  Elem is an element in (member of) List.
member(X, [X|Xs]).
member(X, [Y|Xs]) :- member(X, Xs).


% append(+List1, +List2, -List3):
%  List3 is the result of appending List1 and List2.
append([], Ys, Ys).
append([X|Xs], Ys, [X|Zs]) :- append(Xs, Ys, Zs).


% reverse(+List1, -List2):
%  List2 is the reverse of List1.
reverse(Xs, Ys) :- reverse(Xs, [], Ys).

reverse([], Ys, Ys).
reverse([X|Xs], Ys, Zs) :- reverse(Xs, [X|Ys], Zs).
