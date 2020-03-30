%%% -*- Mode: Prolog; -*-
%%% This file is used in deterministic case.
 
:- use_module(library(lists)).

cnt(X, P,Count) :- findall(X,P,L), length(L,Count).

oneElementOfList([H|_], X) :- X = H.

lmax(L, M) :- lmax(L, [], [], M).
lmax([], Seen, MMax, Max) :- MMax=[] -> Max=Seen; lmax(MMax, [], [], Max).
lmax([H|T], Seen, MMax, Max) :- (member(H, Seen) -> lmax(T, Seen, [H|MMax], Max); lmax(T, [H|Seen], MMax, Max)).


lmin(L, M) :- lmin(L, [], [], M).
lmin([], Seen, MMin, Min) :- MMin=[] -> Min=Seen, !; leftover(Seen, MMin, [], Min).
lmin([H|T], Seen, Left, Min) :- (member(H, Seen) -> lmin(T, Seen, [H|Left], Min); lmin(T, [H|Seen], Left, Min)).
leftover([], MMin, TMin, Min) :- TMin=[] -> lmin(MMin, [], [], Min); Min=TMin, !.
leftover([H|Seen], MMin, TMin, Min) :- (member(H, MMin)) -> leftover(Seen, MMin, TMin, Min); leftover(Seen, MMin, [H|TMin], Min).


list2set([], []).
list2set([H|T], [H|T1]) :- subtract(T, [H], T2), list2set(T2, T1).

%%%%% Use this only if running in probabilistic mode %%%%%
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%% Use this only if running in deterministic mode %%%%%
maxMod(X, P, Max) :- findall(X,P,L), lmax(L, Max1), 
             oneElementOfList(Max1, Max).
minMod(X, P, Min) :- findall(X,P,L), lmin(L, Min1), 
             oneElementOfList(Min1, Min).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


max(X, P, Max) :- findall(X,P,L), max_list(L, Max).

min(X, P, Min) :- findall(X,P,L), min_list(L, Min).

listavg(L, C, A) :- C =:= 0 -> false; sum_list(L, Sum), A is Sum / C.
avg(X, P, Avg) :- findall(X,P,L), length(L,Cnt), listavg(L, Cnt, Avg).


%%%%%%%%%%% Declarative Bias states here %%%%%%%%%%%%
:- discontiguous client/1.
:- discontiguous age/2.
:- discontiguous creditScore/2.
:- discontiguous hasAcc/3.
:- discontiguous account/1.
:- discontiguous savings/2.
:- discontiguous freq/2.
:- discontiguous hasLoan/3.
:- discontiguous loan/1.
:- discontiguous loanAmt/2.
:- discontiguous status/2.

%Types
base(client(client)).
base(age(client,x1)).
base(creditScore(client,x2)).
base(hasAcc(client,account,x3)).
base(account(account)).
base(savings(account,x4)).
base(freq(account,x5)).
base(hasLoan(account,loan,x6)).
base(loan(loan)).
base(loanAmt(loan,x7)).
base(status(loan,x8)).

%Modes
mode(age, avg, (hasAcc(+,-,true),savings(+,-))).
mode(age, minMod, (hasAcc(+,-,true),freq(+,-))).
mode(age, maxMod, (hasAcc(+,-,true),freq(+,-))).
mode(age, none, creditScore(+,-)).
mode(creditScore, avg, (hasAcc(+,-,true),savings(+,-))).
mode(creditScore, minMod, (hasAcc(+,-,true),freq(+,-))).
mode(creditScore, maxMod, (hasAcc(+,-,true),freq(+,-))).
mode(creditScore, none, age(+,-)).
mode(savings, avg, (hasAcc(-,+,true),age(+,-))).
mode(savings, avg, (hasAcc(-,+,true),creditScore(+,-))).
mode(savings, avg, (hasLoan(+,-,true),loanAmt(+,-))).
mode(savings, minMod, (hasLoan(+,-,true),status(+,-))).
mode(savings, maxMod, (hasLoan(+,-,true),status(+,-))).
mode(savings, none, freq(+,-)).
mode(freq, avg, (hasAcc(-,+,true),age(+,-))).
mode(freq, avg, (hasAcc(-,+,true),creditScore(+,-))).
mode(freq, avg, (hasLoan(+,-,true),loanAmt(+,-))).
mode(freq, minMod, (hasLoan(+,-,true),status(+,-))).
mode(freq, maxMod, (hasLoan(+,-,true),status(+,-))).
mode(freq, none, savings(+,-)).
mode(loanAmt, avg, (hasLoan(-,+,true),savings(+,-))).
mode(loanAmt, minMod, (hasLoan(-,+,true),freq(+,-))).
mode(loanAmt, maxMod, (hasLoan(-,+,true),freq(+,-))).
mode(loanAmt, none, status(+,-)).
mode(status, avg, (hasLoan(-,+,true),savings(+,-))).
mode(status, minMod, (hasLoan(-,+,true),freq(+,-))).
mode(status, maxMod, (hasLoan(-,+,true),freq(+,-))).
mode(status, none, loanAmt(+,-)).

%Declare the type of random variables
thres(age, 2, continuous, []).
thres(creditScore, 2, continuous, []).
thres(hasAcc, 3, continuous, []).
thres(savings, 2, continuous, []).
thres(freq, 2, discrete, [high,low]).
thres(hasLoan, 3, continuous, []).
thres(loanAmt, 2, continuous, []).
thres(status, 2, discrete, [appr,decl,pend]).

%List the name of all random variables
randomVariableNames([age,creditScore,hasAcc,savings,freq,hasLoan,loanAmt,status]).

%Rank declaration
rank([age,creditScore,hasAcc,savings,freq,hasLoan,loanAmt,status]).

%Target
learn(age, 2, 2, continuous).
learn(creditScore, 2, 2, continuous).
learn(savings, 2, 2, continuous).
learn(freq, 2, 2, discrete).
learn(loanAmt, 2, 2, continuous).
learn(status, 2, 2, discrete).
%%%%%%%%%%% Declarative Bias ends here %%%%%%%%%%%%

%Train data
client(ann).
age(ann,43).
creditScore(ann,600).
hasAcc(ann,a_10,true).
client(john).
creditScore(john,610).
hasAcc(john,a_10,true).
client(bob).
age(bob,45).
creditScore(bob,620).
hasAcc(bob,a_10,true).
client(carl).
age(carl,23).
creditScore(carl,300).
hasAcc(carl,a_11,true).
client(rose).
age(rose,40).
creditScore(rose,700).
hasAcc(rose,a_12,true).
client(mark).
creditScore(mark,350).
hasAcc(mark,a_13,true).
client(steve).
age(steve,54).
creditScore(steve,650).
hasAcc(steve,a_14,true).
client(ritu).
age(ritu,57).
creditScore(ritu,690).
hasAcc(ritu,a_15,true).
client(amit).
creditScore(amit,720).
hasAcc(amit,a_16,true).
client(nitesh).
age(nitesh,23).
creditScore(nitesh,250).
hasAcc(nitesh,a_17,true).
client(jessa).
age(jessa,26).
creditScore(jessa,370).
hasAcc(jessa,a_18,true).
client(mohit).
age(mohit,27).
hasAcc(mohit,a_18,true).
client(robin).
age(robin,28).
creditScore(robin,300).
hasAcc(robin,a_19,true).
client(dris).
creditScore(dris,301).
hasAcc(dris,a_19,true).
client(chi).
age(chi,19).
hasAcc(chi,a_19,true).
account(a_10).
savings(a_10,2000).
freq(a_10,high).
hasLoan(a_10,l_20,true).
account(a_11).
savings(a_11,2502).
freq(a_11,high).
hasLoan(a_11,l_21,true).
account(a_12).
savings(a_12,3001).
freq(a_12,high).
hasLoan(a_12,l_22,true).
account(a_13).
savings(a_13,3505).
freq(a_13,high).
hasLoan(a_13,l_23,true).
account(a_14).
savings(a_14,4009).
freq(a_14,high).
hasLoan(a_14,l_24,true).
account(a_15).
savings(a_15,4510).
freq(a_15,high).
hasLoan(a_15,l_25,true).
account(a_16).
savings(a_16,5001).
freq(a_16,low).
hasLoan(a_16,l_26,true).
account(a_17).
savings(a_17,5503).
freq(a_17,low).
hasLoan(a_17,l_27,true).
account(a_18).
savings(a_18,6005).
freq(a_18,low).
hasLoan(a_18,l_28,true).
account(a_19).
savings(a_19,6510).
freq(a_19,low).
hasLoan(a_19,l_29,true).
loan(l_20).
loanAmt(l_20,20000).
status(l_20,appr).
loan(l_21).
loanAmt(l_21,25000).
status(l_21,appr).
loan(l_22).
loanAmt(l_22,30000).
status(l_22,appr).
loan(l_23).
loanAmt(l_23,35000).
status(l_23,pend).
loan(l_24).
status(l_24,pend).
loan(l_25).
loanAmt(l_25,45000).
status(l_25,decl).
loan(l_26).
loanAmt(l_26,50000).
status(l_26,decl).
loan(l_27).
loanAmt(l_27,55000).
status(l_27,decl).
loan(l_28).
loanAmt(l_28,60000).
status(l_28,decl).
loan(l_29).
loanAmt(l_29,65000).
loan(l_30).
loanAmt(l_30,70000).
status(l_30,decl).
loan(l_31).
loanAmt(l_31,40000).
status(l_31,pend).
loan(l_32).
loanAmt(l_32,15000).
status(l_32,appr).
loan(l_33).
loanAmt(l_33,75000).
status(l_33,decl).
loan(l_34).
loanAmt(l_34,80000).
status(l_34,decl).
loan(l_35).
loanAmt(l_35,41000).
status(l_35,pend).
loan(l_36).
loanAmt(l_36,43000).
status(l_36,pend).
