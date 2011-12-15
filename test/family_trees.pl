
%
% Relationships
%

mother_child(Mother, Child) :- female(Mother), parent_child(Mother, Child).
father_child(Father, Child) :- male(Father),   parent_child(Father, Child).

ancestor_descendent(Ancestor, Descendent) :- parent_child(Ancestor, Descendent).
ancestor_descendent(Ancestor, Descendent) :-
  parent_child(Ancestor, Common),
  ancestor_descendent(Common, Descendent).

siblings(Sibling1, Sibling2) :- parent_child(Parent, Sibling1), parent_child(Parent, Sibling2).


%
% Database
%

parent_child(bill, ted).
parent_child(bill, bob).
parent_child(mary, ted).
parent_child(mary, bob).
parent_child(george, mary).
parent_child(susan,  mary).
parent_child(dave, george).
parent_child(kim,  george).

female(mary).
female(susan).
female(kim).
male(bill).
male(ted).
male(bob).
male(george).
male(dave).
