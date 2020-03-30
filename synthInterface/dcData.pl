%%% -*- Mode: Prolog; -*-

:- use_module('../DC/dcpf.pl').
:- use_module('../DC/random/sampling.pl').
:- use_module('../DC/distributionalclause.pl').
:- use_module(library(lists)).
:- set_options(default).
:- initialization(init).

builtin(min_list(_,_)).
builtin(max_list(_,_)).
builtin(lmindc(_,_)).
builtin(lmaxdc(_,_)).
builtin(length(_,_)).
builtin(listavgdc(_,_,_)).
builtin(getMean(_,_,_)).
builtin(logistic(_,_,_)).
builtin(softmax(_,_,_)).
builtin(oneElementOfListDC(_,_)).

getMean(X,P,Mean) :- dotProd(X,P,Mean).
dotProd([H1|T1], [H2|T2], Prod) :- dotProd(T1, T2, PartProd), Prod is H1*H2 + PartProd.
dotProd([], [H2], Prod) :- Prod is H2.

logistic(P,X,Result) :- dotProd(X,P,Mean), Result is 1/(1 + exp(-1*Mean)).

softmax(P,X,Result) :- calExponent(P,X,Sum,List), normalization(Sum, List, Result).

normalization(Sum, [X|Xs], [Y|Ys]) :- Y is X/Sum, normalization(Sum, Xs, Ys).
normalization(_, [], []).

calExponent([H|T], X, Sum, [Exp|Exps]) :-
    calExponent([H|T], X, 0, Sum, [Exp|Exps]).
calExponent([H|T], X, Acc0, Sum, [Exp|Exps]) :-
    dotProd(X,H,Prod),
    Exp is exp(Prod),
    Acc is Acc0 + Exp,
    calExponent(T, X, Acc, Sum, Exps).
calExponent([], _, Sum, Sum, []).

cnt(P) ~ val(Count) := length(P,Count).

pickOneElement(List) ~ uniform(List).
%oneElementOfListDC([H|_], X) :- X = H.
%oneElementOfListDC([H|T], X) :- T=[] -> X = H; oneElementOfListDC(T, X).
%pickOneElement(List) ~ val(X) := oneElementOfListDC(List, X).

lmaxdc(L, M) :- lmaxdc(L, [], [], M).
lmaxdc([], Seen, MMax, Max) :- MMax=[] -> Max=Seen; lmaxdc(MMax, [], [], Max).
lmaxdc([H|T], Seen, MMax, Max) :- (member(H, Seen) -> lmaxdc(T, Seen, [H|MMax], Max); lmaxdc(T, [H|Seen], MMax, Max)).
maxMod(L) ~ val(Max) := lmaxdc(L, Max1), pickOneElement(Max1) ~= Max.

lmindc(L, M) :- lmindc(L, [], [], M).
lmindc([], Seen, MMin, Min) :- MMin=[] -> Min=Seen, !; leftoverdc(Seen, MMin, [], Min).
lmindc([H|T], Seen, Left, Min) :- (member(H, Seen) -> lmindc(T, Seen, [H|Left], Min); lmindc(T, [H|Seen], Left, Min)).
leftoverdc([], MMin, TMin, Min) :- TMin=[] -> lmindc(MMin, [], [], Min); Min=TMin, !.
leftoverdc([H|Seen], MMin, TMin, Min) :- (member(H, MMin)) -> leftoverdc(Seen, MMin, TMin, Min); leftoverdc(Seen, MMin, [H|TMin], Min).
minMod(L) ~ val(Min) := lmindc(L, Min1), pickOneElement(Min1) ~= Min.

max(L) ~ val(Max) := max_list(L, Max).

min(L) ~ val(Min) := min_list(L, Min).

listavgdc(L, C, A) :- C =:= 0 -> false; sum_list(L, Sum), A is Sum / C.
avg(L) ~ val(Avg) := length(L,Cnt), listavgdc(L, Cnt, Avg).

samplesGenerator(Num, Evidence, Query, Var, SampleList) :- findall(Var, genSample(Num, Evidence, Query, World), SampleList).
genSample(Num, Evidence, Query, World) :- between(1, Num, SID), generate_backward(Evidence, Query, World).

retractARule(H,B) :-
    clause(H,B),
    B \== true,
    retract(:-(H,B)),
    fail.
retractARule(_,_).

:- dynamic visitedMark/1.
:- dynamic topMark/1.
:- dynamic bottomMark/1.

%Bayes Ball Rules
visit(J,child) :- visited(J), \+evidence(J), markTop(J), parent(J,P), visit(P,child).
visit(J,child) :- visited(J), \+evidence(J), markBottom(J), parent(C,J), visit(C,parent).
visit(J,parent) :- visited(J), evidence(J), markTop(J), parent(J,P), visit(P,child).
visit(J,parent) :- visited(J), \+evidence(J), markBottom(J), parent(C,J), visit(C,parent).
visit(_,_).
visited(J) :- \+visitedMark(J), asserta(visitedMark(J)).
visited(_).
markTop(J) :- \+topMark(J), asserta(topMark(J)).
markBottom(J) :- \+bottomMark(J), asserta(bottomMark(J)).

bayesBall(X,Visited,MarkOnTop,MarkOnBottom,Evidence) :- findall(1,visit(X,child),Z), findall(Y,visitedMark(Y),Visited),
                                                        findall(Y,topMark(Y),MarkOnTop), findall(Y,bottomMark(Y),MarkOnBottom),
                                                        findall(Y,evidence(Y),Evidence), forall(visitedMark(Y),retract(visitedMark(Y))),
                                                        forall(topMark(Y),retract(topMark(Y))), forall(bottomMark(Y),retract(bottomMark(Y))).
retractAndAssert(X,Y) :- retract(evidence(X)), assertz(X), Y=true.
retractAndAssert(X,Y) :- \+retract(evidence(X)), assertz(X), Y=false.
assertAndRetract(X,Y) :- Y=true, asserta(evidence(X)), retract(X).
assertAndRetract(X,Y) :- Y=false, retract(X).

requsite(X,ProbEvidence,Intervention,Z) :- evidence(Z), Intervention=[Z], ProbEvidence=[], !.
requsite(X,ProbEvidence,Intervention,Z) :- \+evidence(Z), retractAndAssert(X,Y),
                                           bayesBall(X,Visited,MarkOnTop,MarkOnBottom,Evidence),
                                           intersection(Evidence,Visited,ProbEvidence1),
                                           intersection(MarkOnTop,ProbEvidence1,ProbEvidence),
                                           subtract(ProbEvidence1, ProbEvidence, Intervention),
                                           assertAndRetract(X,Y).


:- dynamic parent/2.
:- dynamic evidence/1.
:- dynamic client/1.
:- dynamic age/2.
:- dynamic creditScore/2.
:- dynamic hasAcc/3.
:- dynamic account/1.
:- dynamic savings/2.
:- dynamic freq/2.
:- dynamic hasLoan/3.
:- dynamic loan/1.
:- dynamic loanAmt/2.
:- dynamic status/2.

client(X) :- evidence(client(X)).
age(X,Z) :- client(X), evidence(age(X,Z)).
age(X,null) :- client(X), \+evidence(age(X,Z)).
creditScore(X,Z) :- client(X), evidence(creditScore(X,Z)).
creditScore(X,null) :- client(X), \+evidence(creditScore(X,Z)).
hasAcc(X,Y,true) :- evidence(hasAcc(X,Y,true)).
account(X) :- evidence(account(X)).
savings(X,Z) :- account(X), evidence(savings(X,Z)).
savings(X,null) :- account(X), \+evidence(savings(X,Z)).
freq(X,Z) :- account(X), evidence(freq(X,Z)).
freq(X,null) :- account(X), \+evidence(freq(X,Z)).
hasLoan(X,Y,true) :- evidence(hasLoan(X,Y,true)).
loan(X) :- evidence(loan(X)).
loanAmt(X,Z) :- loan(X), evidence(loanAmt(X,Z)).
loanAmt(X,null) :- loan(X), \+evidence(loanAmt(X,Z)).
status(X,Z) :- loan(X), evidence(status(X,Z)).
status(X,null) :- loan(X), \+evidence(status(X,Z)).

%Test data prolog 
evidence(client(ann)).
evidence(age(ann,43)).
evidence(creditScore(ann,600)).
evidence(hasAcc(ann,a_10,true)).
evidence(client(john)).
evidence(creditScore(john,610)).
evidence(hasAcc(john,a_10,true)).
evidence(client(bob)).
evidence(age(bob,45)).
evidence(creditScore(bob,620)).
evidence(hasAcc(bob,a_10,true)).
evidence(client(carl)).
evidence(age(carl,23)).
evidence(creditScore(carl,300)).
evidence(hasAcc(carl,a_11,true)).
evidence(client(rose)).
evidence(age(rose,40)).
evidence(creditScore(rose,700)).
evidence(hasAcc(rose,a_12,true)).
evidence(client(mark)).
evidence(creditScore(mark,350)).
evidence(hasAcc(mark,a_13,true)).
evidence(client(steve)).
evidence(age(steve,54)).
evidence(creditScore(steve,650)).
evidence(hasAcc(steve,a_14,true)).
evidence(client(ritu)).
evidence(age(ritu,57)).
evidence(creditScore(ritu,690)).
evidence(hasAcc(ritu,a_15,true)).
evidence(client(amit)).
evidence(creditScore(amit,720)).
evidence(hasAcc(amit,a_16,true)).
evidence(client(nitesh)).
evidence(age(nitesh,23)).
evidence(creditScore(nitesh,250)).
evidence(hasAcc(nitesh,a_17,true)).
evidence(client(jessa)).
evidence(age(jessa,26)).
evidence(creditScore(jessa,370)).
evidence(hasAcc(jessa,a_18,true)).
evidence(client(mohit)).
evidence(age(mohit,27)).
evidence(hasAcc(mohit,a_18,true)).
evidence(client(robin)).
evidence(age(robin,28)).
evidence(creditScore(robin,300)).
evidence(hasAcc(robin,a_19,true)).
evidence(client(dris)).
evidence(creditScore(dris,301)).
evidence(hasAcc(dris,a_19,true)).
evidence(client(chi)).
evidence(age(chi,19)).
evidence(hasAcc(chi,a_19,true)).
evidence(account(a_10)).
evidence(savings(a_10,2000)).
evidence(freq(a_10,high)).
evidence(hasLoan(a_10,l_20,true)).
evidence(account(a_11)).
evidence(savings(a_11,2502)).
evidence(freq(a_11,high)).
evidence(hasLoan(a_11,l_21,true)).
evidence(account(a_12)).
evidence(savings(a_12,3001)).
evidence(freq(a_12,high)).
evidence(hasLoan(a_12,l_22,true)).
evidence(account(a_13)).
evidence(savings(a_13,3505)).
evidence(freq(a_13,high)).
evidence(hasLoan(a_13,l_23,true)).
evidence(account(a_14)).
evidence(savings(a_14,4009)).
evidence(freq(a_14,high)).
evidence(hasLoan(a_14,l_24,true)).
evidence(account(a_15)).
evidence(savings(a_15,4510)).
evidence(freq(a_15,high)).
evidence(hasLoan(a_15,l_25,true)).
evidence(account(a_16)).
evidence(savings(a_16,5001)).
evidence(freq(a_16,low)).
evidence(hasLoan(a_16,l_26,true)).
evidence(account(a_17)).
evidence(savings(a_17,5503)).
evidence(freq(a_17,low)).
evidence(hasLoan(a_17,l_27,true)).
evidence(account(a_18)).
evidence(savings(a_18,6005)).
evidence(freq(a_18,low)).
evidence(hasLoan(a_18,l_28,true)).
evidence(account(a_19)).
evidence(savings(a_19,6510)).
evidence(freq(a_19,low)).
evidence(hasLoan(a_19,l_29,true)).
evidence(loan(l_20)).
evidence(loanAmt(l_20,20000)).
evidence(status(l_20,appr)).
evidence(loan(l_21)).
evidence(loanAmt(l_21,25000)).
evidence(status(l_21,appr)).
evidence(loan(l_22)).
evidence(loanAmt(l_22,30000)).
evidence(status(l_22,appr)).
evidence(loan(l_23)).
evidence(loanAmt(l_23,35000)).
evidence(status(l_23,pend)).
evidence(loan(l_24)).
evidence(status(l_24,pend)).
evidence(loan(l_25)).
evidence(loanAmt(l_25,45000)).
evidence(status(l_25,decl)).
evidence(loan(l_26)).
evidence(loanAmt(l_26,50000)).
evidence(status(l_26,decl)).
evidence(loan(l_27)).
evidence(loanAmt(l_27,55000)).
evidence(status(l_27,decl)).
evidence(loan(l_28)).
evidence(loanAmt(l_28,60000)).
evidence(status(l_28,decl)).
evidence(loan(l_29)).
evidence(loanAmt(l_29,65000)).
evidence(loan(l_30)).
evidence(loanAmt(l_30,70000)).
evidence(status(l_30,decl)).
evidence(loan(l_31)).
evidence(loanAmt(l_31,40000)).
evidence(status(l_31,pend)).
evidence(loan(l_32)).
evidence(loanAmt(l_32,15000)).
evidence(status(l_32,appr)).
evidence(loan(l_33)).
evidence(loanAmt(l_33,75000)).
evidence(status(l_33,decl)).
evidence(loan(l_34)).
evidence(loanAmt(l_34,80000)).
evidence(status(l_34,decl)).
evidence(loan(l_35)).
evidence(loanAmt(l_35,41000)).
evidence(status(l_35,pend)).
evidence(loan(l_36)).
evidence(loanAmt(l_36,43000)).
evidence(status(l_36,pend)).

%Dependency Structure 
parent(loanAmt(LOAN,X7),freq(ACCOUNT,X5)) :- loanAmt(LOAN,X7),hasLoan(ACCOUNT,LOAN,true),freq(ACCOUNT,X5).
parent(loanAmt(LOAN,X7),savings(ACCOUNT,X4)) :- loanAmt(LOAN,X7),hasLoan(ACCOUNT,LOAN,true),savings(ACCOUNT,X4).
parent(creditScore(CLIENT,X2),age(CLIENT,X1)) :- creditScore(CLIENT,X2),age(CLIENT,X1).
parent(savings(ACCOUNT,X4),age(CLIENT,X1)) :- savings(ACCOUNT,X4),age(CLIENT,X1),hasAcc(CLIENT,ACCOUNT,true).
parent(freq(ACCOUNT,X5),savings(ACCOUNT,X4)) :- freq(ACCOUNT,X5),savings(ACCOUNT,X4).
parent(status(LOAN,X8),savings(ACCOUNT,X4)) :- status(LOAN,X8),hasLoan(ACCOUNT,LOAN,true),savings(ACCOUNT,X4).

%Interventions 
client(ann) := true. 
hasAcc(ann,a_10) ~ val(true).
client(john) := true. 
hasAcc(john,a_10) ~ val(true).
client(bob) := true. 
hasAcc(bob,a_10) ~ val(true).
client(carl) := true. 
hasAcc(carl,a_11) ~ val(true).
client(rose) := true. 
hasAcc(rose,a_12) ~ val(true).
client(mark) := true. 
hasAcc(mark,a_13) ~ val(true).
client(steve) := true. 
hasAcc(steve,a_14) ~ val(true).
client(ritu) := true. 
hasAcc(ritu,a_15) ~ val(true).
client(amit) := true. 
hasAcc(amit,a_16) ~ val(true).
client(nitesh) := true. 
hasAcc(nitesh,a_17) ~ val(true).
client(jessa) := true. 
hasAcc(jessa,a_18) ~ val(true).
client(mohit) := true. 
hasAcc(mohit,a_18) ~ val(true).
client(robin) := true. 
hasAcc(robin,a_19) ~ val(true).
client(dris) := true. 
hasAcc(dris,a_19) ~ val(true).
client(chi) := true. 
hasAcc(chi,a_19) ~ val(true).
account(a_10) := true. 
hasLoan(a_10,l_20) ~ val(true).
account(a_11) := true. 
hasLoan(a_11,l_21) ~ val(true).
account(a_12) := true. 
hasLoan(a_12,l_22) ~ val(true).
account(a_13) := true. 
hasLoan(a_13,l_23) ~ val(true).
account(a_14) := true. 
hasLoan(a_14,l_24) ~ val(true).
account(a_15) := true. 
hasLoan(a_15,l_25) ~ val(true).
account(a_16) := true. 
hasLoan(a_16,l_26) ~ val(true).
account(a_17) := true. 
hasLoan(a_17,l_27) ~ val(true).
account(a_18) := true. 
hasLoan(a_18,l_28) ~ val(true).
account(a_19) := true. 
hasLoan(a_19,l_29) ~ val(true).
loan(l_20) := true. 
loan(l_21) := true. 
loan(l_22) := true. 
loan(l_23) := true. 
loan(l_24) := true. 
loan(l_25) := true. 
loan(l_26) := true. 
loan(l_27) := true. 
loan(l_28) := true. 
loan(l_29) := true. 
loan(l_30) := true. 
loan(l_31) := true. 
loan(l_32) := true. 
loan(l_33) := true. 
loan(l_34) := true. 
loan(l_35) := true. 
loan(l_36) := true. 

%Learned Distributional Clauses 
age(Client) ~ gaussian(35.0,177.2) := true.
creditScore(Client) ~ gaussian(Mean,5942.603266090301) := age(Client)~=X1_M,getMean([X1_M],[13.00672430355427,7.857829010566945],Mean).
creditScore(Client) ~ gaussian(495.25,40836.9166667) := \+age(Client)~=X1_M.
creditScore(Client) ~ gaussian(497.0,33740.3333333) := true.
savings(Account) ~ gaussian(Mean,2429863.4089937364) := findall_forward(X1_M,(hasAcc(Client_M,Account)~=true,age(Client_M)~=X1_M),X_T_1_Temp),avg(X_T_1_Temp)~=X_T_1,getMean([X_T_1],[-42.26936203728013,5792.5480441060645],Mean).
savings(Account) ~ gaussian(4254.6,2297792.71111) := true.
freq(Account) ~ finite([Probability:high,Probability2:low]) := savings(Account)~=X4_M, logistic([0.029202652089567977, -138.90846623285694],[X4_M],Probability2), Probability is 1.0-Probability2.
freq(Account) ~ finite([0.6:high,0.4:low]) := true.
loanAmt(Loan) ~ gaussian(Mean,168.58985572517446) := findall_forward(X4_M,(hasLoan(Account_M,Loan)~=true,savings(Account_M)~=X4_M),X_T_1_Temp),avg(X_T_1_Temp)~=X_T_1,findall_forward(X5_M,(hasLoan(Account_M_1,Loan)~=true,freq(Account_M_1)~=X5_M),X_T_9),minMod(X_T_9)~=high,getMean([X_T_1],[9.960409336974106,86.87358176716589],Mean).
loanAmt(Loan) ~ gaussian(Mean,88.96482311122391) := findall_forward(X4_M,(hasLoan(Account_M,Loan)~=true,savings(Account_M)~=X4_M),X_T_1_Temp),avg(X_T_1_Temp)~=X_T_1,findall_forward(X5_M,(hasLoan(Account_M_1,Loan)~=true,freq(Account_M_1)~=X5_M),X_T_10),minMod(X_T_10)~=low,getMean([X_T_1],[9.9423132316986,284.4729298824823],Mean).
loanAmt(Loan) ~ gaussian(52000.0,558666666.667) := findall_forward(X4_M,(hasLoan(Account_M,Loan)~=true,savings(Account_M)~=X4_M),X_T_2),\+avg(X_T_2)~=_.
loanAmt(Loan) ~ gaussian(46812.5,382829166.667) := true.
status(Loan) ~ finite([Probability1:appr,Probability2:decl,Probability3:pend]) := findall_forward(X4_M,(hasLoan(Account_M,Loan)~=true,savings(Account_M)~=X4_M),X_T_1_Temp),avg(X_T_1_Temp)~=X_T_1, softmax([[-0.059928311835396424,212.7584889008575],[0.05575380542759868,-216.21477724047696],[0.004174506330970806,3.4562883396198543]],[X_T_1],[Probability1,Probability2,Probability3]).
status(Loan) ~ finite([0.42857142857142855:decl,0.42857142857142855:pend,0.14285714285714285:appr]) := findall_forward(X4_M,(hasLoan(Account_M,Loan)~=true,savings(Account_M)~=X4_M),X_T_2),\+avg(X_T_2)~=_.
status(Loan) ~ finite([0.4375:decl,0.3125:pend,0.25:appr]) := true.


