%%% -*- Mode: Prolog; -*-
:- use_module('../../DCRuleLearning/program/utils.pl').
:- use_module('../../DCRuleLearning/DC/dcpf.pl').
:- use_module('../../DCRuleLearning/DC/random/sampling.pl').
:- use_module('../../DCRuleLearning/DC/distributionalclause.pl').
:- use_module(library(lists)).

:- set_options(default).
:- set_inference(backward(lazy)).
:- initialization(init).

%facts
person(ann) := true.
person(bob) := true.
person(carl) := true.
person(rose) := true.
person(john) := true.

stress(X) ~ finite([0.1:true,0.9:false]) := person(X).

smokes(X) ~ finite([0.3:true,0.7:false]) := stress(X) ~= true.
smokes(X) ~ finite([0.2:true,0.8:false]) := stress(X) ~= false.

grade(ann) ~ val(80.0).
grade(bob) ~ val(90.3) := true.
grade(X) ~ gaussian(68,1) := smokes(X) ~= true.
grade(X) ~ gaussian(88,1) := smokes(X) ~= false.

height(X) ~ val(90.0) := smokes(X) ~= true.
height(X) ~ val(100.0) := smokes(X) ~= false.

samplesGenerator(Num, Evidence, Query, Var, SampleList) :- findall(Sample, genSample(Num, Evidence, Query, Var, Sample), SampleList). 
genSample(Num, Evidence, Query, Var, Sample) :- between(1, Num, SID), generate_backward(Evidence, Query, World), storeSamples(Var, Query, World, [], Sample).
storeSamples([H|T], (HQ, TQ), World, Sample, AllSample) :- findall(H, member(HQ, World), S), append(Sample, [S], Sample1), storeSamples(T, TQ, World, Sample1, AllSample).
storeSamples([T], (TQ), World, Sample, AllSample) :- findall(T, member(TQ, World), S), append(Sample, [S], AllSample), !.

%generate samples
genSample(N) :-
	generate_backward([stress(carl)~=true], (grade(carl)~=X, height(carl)~=Y), Alist),
	findall(X,member(grade(carl)~=X,Alist),List2),
	writeln(List2).
	%writeln(Alist).

%inference
test(N) :-
	init,
	query([],[],(grade(carl)~=X, X>68),N,P),
	write('probability: '),writeln(P).
	%eval_query_distribution(bp(ann),[],[],(bp(ann) ~= B),N,P,Succ_Sum,Sum), 
	%write('distriution: '),writeln(B).

