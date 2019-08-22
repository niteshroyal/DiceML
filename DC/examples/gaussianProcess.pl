%%% -*- Mode: Prolog; -*-
:- use_module('../distributionalclause.pl').
:- use_module('../random/sampling.pl').
:- use_module(library(lists)).
:- set_options(default).
:- set_debug(false).
:- set_inference(backward(lazy)).
:- initialization(init).


%% Prolog Predicates.
conjunct_to_list((A,B), L) :- !, conjunct_to_list(A, L0), conjunct_to_list(B, L1), append(L0, L1, L).
conjunct_to_list(A, [A]).

getMOD([], M1, O1, D1, M, O, D) :- M=M1, O=O1, D=D1. 
getMOD([H|T], M1, O1, D1, M, O, D) :- H=[Obj, Data], append(M1,[0],M2), append(O1,[Obj],O2), append(D1,[Data],D2), getMOD(T, M2, O2, D2, M, O, D).
getMeanObjectsData(X, M, O, D) :- getMOD(X, [], [], [], M, O, D).

constructCovMatrix([], Data, CovTemp, Covariance) :- Covariance=CovTemp.

constructCMLoop2(H, [], CovTemp, Res) :- Res=CovTemp.
constructCMLoop2(H, [H1|T1], CovTemp, Res) :- cubicFunction(H, H1, Cov), append(CovTemp, [Cov], CovTemp2), constructCMLoop2(H, T1, CovTemp2, Res).
constructCovMatrix([H|T], Data, CovTemp, Covariance) :- constructCMLoop2(H, Data, [], CovTemp1), append(CovTemp, CovTemp1, CovTemp2), constructCovMatrix(T, Data, CovTemp2, Covariance).
kernel(L, Covariance, ZeroMean, Objects) :- getMeanObjectsData(L, ZeroMean, Objects, Data), constructCovMatrix(Data, Data, [], Covariance). 

dotProd([H1|T1], [H2|T2], Prod) :- dotProd(T1, T2, PartProd), Prod is H1*H2 + PartProd.
dotProd([], [], Prod) :- Prod is 0.

linearFunction(X1, X2, Res) :- dotProd(X1, X2, Res1), Res is Res1.
cubicFunction(X1, X2, Res) :- dotProd(X1, X2, TempRes1), TempRes is TempRes1+1, Res is TempRes*TempRes*TempRes.

getEstimate(X, [], [], Est) :- fail.
getEstimate(X, [H1|T1], [H2|T2], Est) :- (X=H2 -> Est=H1,! ; getEstimate(X, T1, T2, Est)). 
funcEstimateForThisObject(X, Estimates, Objects, Est) :- conjunct_to_list(Estimates, EstimatesList), getEstimate(X, EstimatesList, Objects, Est).


%% Gaussian Process Regression Model.
builtin(kernel(_,_,_,_)).
builtin(funcEstimateForThisObject(_,_,_,_)).

emission(X) ~ gaussian(6.0, 100).
funcEstimates(Objects) ~ gaussian(ZeroMean, Covariance) := findall_forward([X,[Z]], emission(X)~=Z, L), kernel(L, Covariance, ZeroMean, Objects).
pollution(X) ~ gaussian(Est, 1.0) := funcEstimates(Objects) ~= Estimates, funcEstimateForThisObject(X, Estimates, Objects, Est).


%% Query
test1(N) :-
        init,
        query([emission(1) ~= 0.5, emission(2) ~= 1.2, emission(3) ~= 2.1, emission(4) ~= 3.0, pollution(1) ~= 0.3, pollution(2) ~= 0.7, pollution(3) ~= 1.0],[],(pollution(4) ~= X, X<0),N,P),write('probability: '),writeln(P).


test2(N) :-
        init,
        query([emission(1) ~= 0.5, emission(2) ~= 1.2, emission(3) ~= 2.1, emission(4) ~= 3.0, emission(5) ~= 3.9, emission(6) ~= 4.6, emission(7) ~= 5.1, emission(8) ~= 5.7, emission(9) ~= 6.9, emission(10) ~= 7.4, emission(11) ~= 8.2, emission(12) ~= 9.1, emission(13) ~= 9.5, pollution(1) ~= 0.3, pollution(2) ~= 0.7, pollution(3) ~= 1.0, pollution(4) ~= 1.1, pollution(5) ~= 1.2, pollution(6) ~= 1.4, pollution(7) ~= 2.0, pollution(8) ~= 3.1, pollution(9) ~= 2.5, pollution(10) ~= 1.8, pollution(11) ~= 1.4, pollution(12) ~= 1.8, pollution(13) ~= 2.9, emission(14) ~= 10, pollution(14) ~= 3.9, emission(15) ~= 11, pollution(15) ~= 4.6, emission(16) ~= 11.5, pollution(16) ~= 5.5, emission(17) ~= 12, pollution(17) ~= 7, emission(18) ~= 12.5],[],(pollution(18) ~= X, X<6.5),N,P),write('probability: '),writeln(P).
