%%% -*- Mode: Prolog; -*-
:- use_module('../../DCRuleLearning/DC/dcpf.pl').
:- use_module('../../DCRuleLearning/DC/random/sampling.pl').
:- use_module('../../DCRuleLearning/DC/distributionalclause.pl').
:- use_module(library(lists)).
:- set_options(default).
:- set_inference(backward(lazy)).
:- initialization(init).

builtin(avg(_,_,_)).
builtin(min_list(_,_)).
builtin(max_list(_,_)).
builtin(lmin(_,_)).
builtin(lmax(_,_)).
builtin(length(_,_)).
builtin(listavg(_,_,_)).
builtin(getMean(_,_,_)).
builtin(logistic(_,_,_)).
builtin(softmax(_,_,_)).

getMean(X,P,Mean) :- dotProd(X,P,Mean).
dotProd([H1|T1], [H2|T2], Prod) :- dotProd(T1, T2, PartProd), Prod is H1*H2 + PartProd.
dotProd([], [H2], Prod) :- Prod is H2.

logistic(P,X,Result) :- dotProd(X,P,Mean), Result is 1/(1 + -1*exp(Mean)).

softmax(P,X,Result) :- calExponent(P,X,Sum,List), normalization(Sum, List, Result).

normalization(Sum, [X|Xs], [Y|Ys]) :- Y is X/Sum, normalization(Sum, Xs, Ys).
normalization(_, [], []).

calExponent([H|T], X, Sum, [Exp|Exps]) :-
    calExponent([H|T], X, 0, Sum, [Exp|Exps]).
calExponent([H|T], X, Acc0, Sum, [Exp|Exps]) :-
    dotProd(X,H,Prod),
    Exp is exp(-1*Prod),
    Acc is Acc0 + Exp,
    calExponent(T, X, Acc, Sum, Exps).
calExponent([], _, Sum, Sum, []).

count(P) ~ val(Count) := findall_forward(1,P,L), length(L,Count).

pickOneElement(List) ~ uniform(List).

lmax(L, M) :- lmax(L, [], [], M).
lmax([], Seen, MMax, Max) :- MMax=[] -> Max=Seen; lmax(MMax, [], [], Max).
lmax([H|T], Seen, MMax, Max) :- (member(H, Seen) -> lmax(T, Seen, [H|MMax], Max); lmax(T, [H|Seen], MMax, Max)).
maxMod(L) ~ val(Max) := lmax(L, Max1), pickOneElement(Max1) ~= Max.

lmin(L, M) :- lmin(L, [], [], M).
lmin([], Seen, MMin, Min) :- MMin=[] -> Min=Seen, !; leftover(Seen, MMin, Min).
lmin([H|T], Seen, Left, Min) :- (member(H, Seen) -> lmin(T, Seen, [H|Left], Min); lmin(T, [H|Seen], Left, Min)).
leftover([], MMin, Min) :- lmin(MMin, [], [], Min).
leftover([H|Seen], MMin, Min) :- (member(H, MMin)) -> leftover(Seen, MMin, Min); Min=[H], !.
minMod(L) ~ val(Min) := lmin(L, Min1), pickOneElement(Min1) ~= Min.

max(L) ~ val(Max) := max_list(L, Max).

min(L) ~ val(Min) := min_list(L, Min).

listavg(L, C, A) :- C =:= 0 -> false; sum_list(L, Sum), A is Sum / C.
avg(L) ~ val(Avg) := length(L,Cnt), listavg(L, Cnt, Avg).


count(P,Count) :- findall(1,P,L), length(L,Count).

oneElementOfList([H|_], X) :- X = H.

maxMod(X, P, Max) :- findall(X,P,L), lmax(L, Max1), oneElementOfList(Max1, Max).

minMod(X, P, Min) :- findall(X,P,L), lmin(L, Min1), oneElementOfList(Min1, Min).
list2set([], []).
list2set([H|T], [H|T1]) :- subtract(T, [H], T2), list2set(T2, T1).

%maxMod(Template,Goal,G) :-
%    findall(X,bagof(Template,Goal,X),Lists),
%    flatten(Lists,G3),
%    list2set(G3,Gset),
%    member(G,Gset).

%minMod(Template,Goal,G) :-
%    findall(X,bagof(Template,Goal,X),Lists),
%    flatten(Lists,G3),
%    list2set(G3,Gset),
%    member(G,Gset).

max(X, P, Max) :- findall(X,P,L), max_list(L, Max).

min(X, P, Min) :- findall(X,P,L), min_list(L, Min).

avg(X, P, Avg) :- findall(X,P,L), count(P,Cnt), listavg(L, Cnt, Avg).


%Prolog Facts
grade(ann, bio, high).
grade(ann, chem, high).
grade(ann, math, med).
grade(ann, geo, low).

person(ann).


%DC Facts
person(bob) := true.
human(ann) ~ val(true).
human(X) ~ val(true) := person(X).

%DC Specific
samplesGenerator(Num, Evidence, Query, Var, SampleList) :- findall(Sample, genSample(Num, Evidence, Query, Var, Sample), SampleList). 
genSample(Num, Evidence, Query, Var, Sample) :- between(1, Num, SID), generate_backward(Evidence, Query, World), storeSamples(Var, Query, World, [], Sample).
storeSamples([H|T], (HQ, TQ), World, Sample, AllSample) :- findall(H, member(HQ, World), S), append(Sample, [S], Sample1), storeSamples(T, TQ, World, Sample1, AllSample).
storeSamples([T], (TQ), World, Sample, AllSample) :- findall(T, member(TQ, World), S), append(Sample, [S], AllSample), !.


test(N) :-
	query([],[],(human(ann)~=true),N,P),
	%query([],[],(\+rating(11)~=high),N,P), 
	%query([],[],(maxMod(R, rating(C)~=R)~=X, X=low),N,P),  
	write('probability: '),writeln(P).
