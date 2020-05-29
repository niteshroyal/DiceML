:- use_module(library(lists)).

count(P,Count) :- findall(1,P,L), length(L,Count).

oneElementOfList([H|_], X) :- X = H.

sample([],L,L).

sample([H|T],Prev,L) :-
    member(Elem,H),
    sample(T,[Elem|Prev],L).

listmax(L, M) :- listmax(L, [], [], M).
listmax([], Seen, MMax, Max) :- MMax=[] -> Max=Seen; listmax(MMax, [], [], Max).
listmax([H|T], Seen, MMax, Max) :- (member(H, Seen) -> listmax(T, Seen, [H|MMax], Max); listmax(T, [H|Seen], MMax, Max)).
maxMod(X, P, Max) :- findall(X,P,L), listmax(L, Max1), oneElementOfList(Max1, Max).

listmin(L, M) :- listmin(L, [], [], M).
listmin([], Seen, MMin, Min) :- MMin=[] -> Min=Seen, !; leftover(Seen, MMin, Min).
listmin([H|T], Seen, Left, Min) :- (member(H, Seen) -> listmin(T, Seen, [H|Left], Min); listmin(T, [H|Seen], Left, Min)).
leftover([], MMin, Min) :- listmin(MMin, [], [], Min).
leftover([H|Seen], MMin, Min) :- (member(H, MMin)) -> leftover(Seen, MMin, Min); Min=[H], !.
minMod(X, P, Min) :- findall(X,P,L), listmin(L, Min1), oneElementOfList(Min1, Min).

max(X, P, Max) :- findall(X,P,L), max_list(L, Max).

min(X, P, Min) :- findall(X,P,L), min_list(L, Min).

listavg(L, C, A) :- C =:= 0 -> A is 0; sum_list(L, Sum), A is Sum / C.
avg(X, P, Avg) :- findall(X,P,L), count(P,Cnt), listavg(L, Cnt, Avg).
