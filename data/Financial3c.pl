%%% -*- Mode: Prolog; -*-
:- use_module(library(lists)).

cnt(X, P,Count) :- findall(X,P,L), length(L,Count).

oneElementOfList([H|_], X) :- X = H.

lmax(L, M) :- lmax(L, [], [], M).
lmax([], Seen, MMax, Max) :- MMax=[] -> Max=Seen; lmax(MMax, [], [], Max).
lmax([H|T], Seen, MMax, Max) :- (member(H, Seen) -> lmax(T, Seen, [H|MMax], Max); lmax(T, [H|Seen], MMax, Max)).
maxMod(X, P, Max) :- findall(X,P,L), lmax(L, Max1), oneElementOfList(Max1, Max).

lmin(L, M) :- lmin(L, [], [], M).
lmin([], Seen, MMin, Min) :- MMin=[] -> Min=Seen, !; leftover(Seen, MMin, [], Min).
lmin([H|T], Seen, Left, Min) :- (member(H, Seen) -> lmin(T, Seen, [H|Left], Min); lmin(T, [H|Seen], Left, Min)).
leftover([], MMin, TMin, Min) :- TMin=[] -> lmin(MMin, [], [], Min); Min=TMin, !.
leftover([H|Seen], MMin, TMin, Min) :- (member(H, MMin)) -> leftover(Seen, MMin, TMin, Min); leftover(Seen, MMin, [H|TMin], Min).
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

listavg(L, C, A) :- C =:= 0 -> false; sum_list(L, Sum), A is Sum / C.
avg(X, P, Avg) :- findall(X,P,L), length(L,Cnt), listavg(L, Cnt, Avg).


:- discontiguous client/1.
:- discontiguous loan/1.
:- discontiguous hasLoan/3.
:- discontiguous clientDistrict/3.
:- discontiguous hasAccount/3.
:- discontiguous district/1.
:- discontiguous account/1.
:- discontiguous loanAmount/2.
:- discontiguous loanStatus/2.
:- discontiguous monthlyPayments/2.
:- discontiguous gender/2.
:- discontiguous clientLoan/3.
:- discontiguous age/2.
:- discontiguous freq/2.
:- discontiguous avgSalary/2.
:- discontiguous ratUrbInhab/2.
:- discontiguous avgNrWith/2.
:- discontiguous avgSumOfInc/2.
:- discontiguous avgSumOfW/2.
:- discontiguous stdMonthInc/2.
:- discontiguous stdMonthW/2.

:- dynamic parent/2.
:- dynamic evidence/1.
:- dynamic client/1.
:- dynamic loan/1.
:- dynamic hasLoan/3.
:- dynamic clientDistrict/3.
:- dynamic hasAccount/3.
:- dynamic district/1.
:- dynamic account/1.
:- dynamic loanAmount/2.
:- dynamic loanStatus/2.
:- dynamic monthlyPayments/2.
:- dynamic gender/2.
:- dynamic clientLoan/3.
:- dynamic age/2.
:- dynamic freq/2.
:- dynamic avgSalary/2.
:- dynamic ratUrbInhab/2.
:- dynamic avgNrWith/2.
:- dynamic avgSumOfInc/2.
:- dynamic avgSumOfW/2.
:- dynamic stdMonthInc/2.
:- dynamic stdMonthW/2.

%Relational Lookahead
districtClientAccount(D,C,A) :- clientDistrict(C,D,X1), X1==true, hasAccount(C,A,X2), X2==true.
districtClientLoan(D,C,L) :- clientDistrict(C,D,X1), X1==true, clientLoan(C,L,X2), X2==true.

%Types
base(client(c)).
base(loan(l)).
base(hasLoan(a,l,x1)).
base(districtClientAccount(d,c,a)).
base(districtClientLoan(d,c,l)).
base(clientDistrict(c,d,x2)).
base(hasAccount(c,a,x3)).
base(district(d)).
base(account(a)).
base(loanAmount(l,la)).
base(loanStatus(l,ls)).
base(monthlyPayments(l,mp)).
base(gender(c,ge)).
base(age(c,ca)).
base(clientLoan(c,l,x4)).
base(freq(a,afr)).
base(avgSalary(d,dav)).
base(ratUrbInhab(d,drat)).
base(avgNrWith(a,aavn)).
base(avgSumOfInc(a,aavsi)).
base(avgSumOfW(a,aavw)).
base(stdMonthInc(a,astdmi)).
base(stdMonthW(a,astdmw)).

randomVariableNames([]).
rank([]).
%Modes
mode(avgSalary, cnt, districtClientAccount(+,-,-)).
%For order% mode(avgSalary, none, ratUrbInhab(+,-)).
%For order% mode(avgSalary, maxMod, (clientDistrict(-,+,true),gender(+,-))).
%For order% mode(avgSalary, avg, (clientDistrict(-,+,true),age(+,-))).
%For order% mode(avgSalary, avg, (districtClientAccount(+,-,-),avgSumOfW(+,-))).
%For order% mode(avgSalary, avg, (districtClientAccount(+,-,-),stdMonthInc(+,-))).
%For order% mode(avgSalary, avg, (districtClientAccount(+,-,-),stdMonthW(+,-))).
%For order% mode(avgSalary, avg, (districtClientAccount(+,-,-),avgNrWith(+,-))).
%For order% mode(avgSalary, avg, (districtClientAccount(+,-,-),avgSumOfInc(+,-))).
%For order% mode(avgSalary, maxMod, (districtClientAccount(+,-,-),freq(+,-))).
%For order% mode(avgSalary, avg, (districtClientLoan(+,-,-),loanAmount(+,-))).
%For order% mode(avgSalary, maxMod, (districtClientLoan(+,-,-),loanStatus(+,-))).
%For order% mode(avgSalary, avg, (districtClientLoan(+,-,-),monthlyPayments(+,-))).

mode(ratUrbInhab, cnt, districtClientAccount(+,-,-)).
mode(ratUrbInhab, none, avgSalary(+,-)).
%For order% mode(ratUrbInhab, maxMod, (clientDistrict(-,+,true),gender(+,-))).
%For order% mode(ratUrbInhab, avg, (clientDistrict(-,+,true),age(+,-))).
%For order% mode(ratUrbInhab, avg, (districtClientAccount(+,-,-),avgSumOfW(+,-))).
%For order% mode(ratUrbInhab, avg, (districtClientAccount(+,-,-),stdMonthInc(+,-))).
%For order% mode(ratUrbInhab, avg, (districtClientAccount(+,-,-),stdMonthW(+,-))).
%For order% mode(ratUrbInhab, avg, (districtClientAccount(+,-,-),avgNrWith(+,-))).
%For order% mode(ratUrbInhab, avg, (districtClientAccount(+,-,-),avgSumOfInc(+,-))).
%For order% mode(ratUrbInhab, maxMod, (districtClientAccount(+,-,-),freq(+,-))).
%For order% mode(ratUrbInhab, avg, (districtClientLoan(+,-,-),loanAmount(+,-))).
%For order% mode(ratUrbInhab, maxMod, (districtClientLoan(+,-,-),loanStatus(+,-))).
%For order% mode(ratUrbInhab, avg, (districtClientLoan(+,-,-),monthlyPayments(+,-))).

mode(gender, cnt, clientDistrict(+,-,true)).
mode(gender, cnt, hasAccount(+,-,true)).
mode(gender, none, age(+,-)).
%For order% mode(gender, avg, (clientDistrict(+,-,true),avgSalary(+,-))).
%For order% mode(gender, avg, (clientDistrict(+,-,true),ratUrbInhab(+,-))).
%For order% mode(gender, avg, (hasAccount(+,-,true),avgSumOfW(+,-))).
%For order% mode(gender, avg, (hasAccount(+,-,true),stdMonthInc(+,-))).
%For order% mode(gender, avg, (hasAccount(+,-,true),stdMonthW(+,-))).
%For order% mode(gender, avg, (hasAccount(+,-,true),avgNrWith(+,-))).
%For order% mode(gender, avg, (hasAccount(+,-,true),avgSumOfInc(+,-))).
%For order% mode(gender, maxMod, (hasAccount(+,-,true),freq(+,-))).
mode(gender, avg, (clientLoan(+,-,true),loanAmount(+,-))).
mode(gender, maxMod, (clientLoan(+,-,true),loanStatus(+,-))).
%mode(gender, avg, (clientLoan(+,-,true),monthlyPayments(+,-))). %Uncommented

mode(age, cnt, clientDistrict(+,-,true)).
mode(age, cnt, hasAccount(+,-,true)).
mode(age, cnt, clientLoan(+,-,true)).
%For order% mode(age, none, gender(+,-)).
mode(age, avg, (clientDistrict(+,-,true),avgSalary(+,-))).
mode(age, avg, (clientDistrict(+,-,true),ratUrbInhab(+,-))).
%For order% mode(age, avg, (hasAccount(+,-,true),avgSumOfW(+,-))).
%For order% mode(age, avg, (hasAccount(+,-,true),stdMonthInc(+,-))).
%For order% mode(age, avg, (hasAccount(+,-,true),stdMonthW(+,-))).
%For order% mode(age, avg, (hasAccount(+,-,true),avgNrWith(+,-))).
%For order% mode(age, avg, (hasAccount(+,-,true),avgSumOfInc(+,-))).
%For order% mode(age, maxMod, (hasAccount(+,-,true),freq(+,-))).
%For order% mode(age, avg, (clientLoan(+,-,true),loanAmount(+,-))).
%For order% mode(age, maxMod, (clientLoan(+,-,true),loanStatus(+,-))).
%For order% mode(age, avg, (clientLoan(+,-,true),monthlyPayments(+,-))).

mode(loanAmount, cnt, hasLoan(-,+,true)).
mode(loanAmount, cnt, districtClientLoan(-,-,+)).
%For order% mode(loanAmount, none, loanStatus(+,-)).
%For order% mode(loanAmount, none, monthlyPayments(+,-)).
%For order% mode(loanAmount, maxMod, (clientLoan(-,+,true),gender(+,-))).
mode(loanAmount, avg, (clientLoan(-,+,true),age(+,-))).
mode(loanAmount, avg, (hasLoan(-,+,true),avgSumOfW(+,-))). %Commented
%For order% mode(loanAmount, avg, (hasLoan(-,+,true),stdMonthInc(+,-))).
%For order% mode(loanAmount, avg, (hasLoan(-,+,true),stdMonthW(+,-))).
mode(loanAmount, avg, (hasLoan(-,+,true),avgNrWith(+,-))). %Commented
%For order% mode(loanAmount, avg, (hasLoan(-,+,true),avgSumOfInc(+,-))).
%For order% mode(loanAmount, maxMod, (hasLoan(-,+,true),freq(+,-))).
%For order% mode(loanAmount, avg, (districtClientLoan(-,-,+),avgSalary(+,-))).
%For order% mode(loanAmount, avg, (districtClientLoan(-,-,+),ratUrbInhab(+,-))).

mode(loanStatus, cnt, hasLoan(-,+,true)).
mode(loanStatus, cnt, districtClientLoan(-,-,+)).
mode(loanStatus, none, loanAmount(+,-)).
%For order% mode(loanStatus, none, monthlyPayments(+,-)).
%For order% mode(loanStatus, maxMod, (clientLoan(-,+,true),gender(+,-))).
mode(loanStatus, avg, (clientLoan(-,+,true),age(+,-))).
%For order% mode(loanStatus, avg, (hasLoan(-,+,true),avgSumOfW(+,-))).
%For order% mode(loanStatus, avg, (hasLoan(-,+,true),stdMonthInc(+,-))).
%For order% mode(loanStatus, avg, (hasLoan(-,+,true),stdMonthW(+,-))).
mode(loanStatus, avg, (hasLoan(-,+,true),avgNrWith(+,-))). %Commented
%For order% mode(loanStatus, avg, (hasLoan(-,+,true),avgSumOfInc(+,-))).
%For order% mode(loanStatus, maxMod, (hasLoan(-,+,true),freq(+,-))).
%For order% mode(loanStatus, avg, (districtClientLoan(-,-,+),avgSalary(+,-))).
%For order% mode(loanStatus, avg, (districtClientLoan(-,-,+),ratUrbInhab(+,-))).

mode(monthlyPayments, cnt, hasLoan(-,+,true)).
mode(monthlyPayments, cnt, districtClientLoan(-,-,+)).
mode(monthlyPayments, none, loanAmount(+,-)).
mode(monthlyPayments, none, loanStatus(+,-)).
mode(monthlyPayments, maxMod, (clientLoan(-,+,true),gender(+,-))). %commented
mode(monthlyPayments, avg, (clientLoan(-,+,true),age(+,-))).
mode(monthlyPayments, avg, (hasLoan(-,+,true),avgSumOfW(+,-))). %commented
mode(monthlyPayments, avg, (hasLoan(-,+,true),stdMonthInc(+,-))). %commented
mode(monthlyPayments, avg, (hasLoan(-,+,true),stdMonthW(+,-))). %commented
mode(monthlyPayments, avg, (hasLoan(-,+,true),avgNrWith(+,-))). %commented
mode(monthlyPayments, avg, (hasLoan(-,+,true),avgSumOfInc(+,-))). %commented
mode(monthlyPayments, maxMod, (hasLoan(-,+,true),freq(+,-))). %commented
mode(monthlyPayments, avg, (districtClientLoan(-,-,+),avgSalary(+,-))). %commented
mode(monthlyPayments, avg, (districtClientLoan(-,-,+),ratUrbInhab(+,-))). %commented

%For order% mode(avgSumOfW, none, stdMonthInc(+,-)).
%For order% mode(avgSumOfW, none, stdMonthW(+,-)).
%For order% mode(avgSumOfW, none, avgNrWith(+,-)).
%For order% mode(avgSumOfW, none, avgSumOfInc(+,-)).
%For order% mode(avgSumOfW, none, freq(+,-)).
mode(avgSumOfW, avg, (districtClientAccount(-,-,+),avgSalary(+,-))).
mode(avgSumOfW, avg, (districtClientAccount(-,-,+),ratUrbInhab(+,-))).
%For order% mode(avgSumOfW, maxMod, (hasAccount(-,+,true),gender(+,-))).
mode(avgSumOfW, avg, (hasAccount(-,+,true),age(+,-))).
%For order% mode(avgSumOfW, avg, (hasLoan(+,-,true),loanAmount(+,-))).
%For order% mode(avgSumOfW, maxMod, (hasLoan(+,-,true),loanStatus(+,-))).
%For order% mode(avgSumOfW, avg, (hasLoan(+,-,true),monthlyPayments(+,-))).

mode(stdMonthInc, none, avgSumOfW(+,-)).
%For order% mode(stdMonthInc, none, stdMonthW(+,-)).
mode(stdMonthInc, none, avgNrWith(+,-)). %Commented
%For order% mode(stdMonthInc, none, avgSumOfInc(+,-)).
%For order% mode(stdMonthInc, none, freq(+,-)).
mode(stdMonthInc, avg, (districtClientAccount(-,-,+),avgSalary(+,-))).
mode(stdMonthInc, avg, (districtClientAccount(-,-,+),ratUrbInhab(+,-))).
%mode(stdMonthInc, maxMod, (hasAccount(-,+,true),gender(+,-))). %uncommented
mode(stdMonthInc, avg, (hasAccount(-,+,true),age(+,-))).
mode(stdMonthInc, avg, (hasLoan(+,-,true),loanAmount(+,-))).
mode(stdMonthInc, maxMod, (hasLoan(+,-,true),loanStatus(+,-))).
%mode(stdMonthInc, avg, (hasLoan(+,-,true),monthlyPayments(+,-))). %Uncommented

mode(stdMonthW, none, avgSumOfW(+,-)).
mode(stdMonthW, none, stdMonthInc(+,-)).
mode(stdMonthW, none, avgNrWith(+,-)).
%mode(stdMonthW, none, avgSumOfInc(+,-)). %Uncommented
mode(stdMonthW, none, freq(+,-)).
mode(stdMonthW, avg, (districtClientAccount(-,-,+),avgSalary(+,-))).
mode(stdMonthW, avg, (districtClientAccount(-,-,+),ratUrbInhab(+,-))).
%mode(stdMonthW, maxMod, (hasAccount(-,+,true),gender(+,-))). %uncommented
mode(stdMonthW, avg, (hasAccount(-,+,true),age(+,-))).
mode(stdMonthW, avg, (hasLoan(+,-,true),loanAmount(+,-))).
mode(stdMonthW, maxMod, (hasLoan(+,-,true),loanStatus(+,-))).
%mode(stdMonthW, avg, (hasLoan(+,-,true),monthlyPayments(+,-))). %Uncommented

mode(avgNrWith, none, avgSumOfW(+,-)).
%For order% mode(avgNrWith, none, stdMonthInc(+,-)).
%For order% mode(avgNrWith, none, stdMonthW(+,-)).
%For order% mode(avgNrWith, none, avgSumOfInc(+,-)).
%For order% mode(avgNrWith, none, freq(+,-)).
mode(avgNrWith, avg, (districtClientAccount(-,-,+),avgSalary(+,-))).
mode(avgNrWith, avg, (districtClientAccount(-,-,+),ratUrbInhab(+,-))).
%For order% mode(avgNrWith, maxMod, (hasAccount(-,+,true),gender(+,-))).
mode(avgNrWith, avg, (hasAccount(-,+,true),age(+,-))).
%For order% mode(avgNrWith, avg, (hasLoan(+,-,true),loanAmount(+,-))).
%For order% mode(avgNrWith, maxMod, (hasLoan(+,-,true),loanStatus(+,-))).
%For order% mode(avgNrWith, avg, (hasLoan(+,-,true),monthlyPayments(+,-))).

mode(avgSumOfInc, none, avgSumOfW(+,-)).
mode(avgSumOfInc, none, stdMonthInc(+,-)).
mode(avgSumOfInc, none, stdMonthW(+,-)).
mode(avgSumOfInc, none, avgNrWith(+,-)).
mode(avgSumOfInc, none, freq(+,-)).
mode(avgSumOfInc, avg, (districtClientAccount(-,-,+),avgSalary(+,-))).
mode(avgSumOfInc, avg, (districtClientAccount(-,-,+),ratUrbInhab(+,-))).
mode(avgSumOfInc, maxMod, (hasAccount(-,+,true),gender(+,-))).
mode(avgSumOfInc, avg, (hasAccount(-,+,true),age(+,-))).
mode(avgSumOfInc, avg, (hasLoan(+,-,true),loanAmount(+,-))).
mode(avgSumOfInc, maxMod, (hasLoan(+,-,true),loanStatus(+,-))).
%mode(avgSumOfInc, avg, (hasLoan(+,-,true),monthlyPayments(+,-))). %Uncommented

mode(freq, none, avgSumOfW(+,-)).
%mode(freq, none, stdMonthInc(+,-)). %uncommented
%mode(freq, none, stdMonthW(+,-)). %uncommented
%For order% mode(freq, none, avgNrWith(+,-)).
%For order% mode(freq, none, avgSumOfInc(+,-)).
mode(freq, avg, (districtClientAccount(-,-,+),avgSalary(+,-))).
mode(freq, avg, (districtClientAccount(-,-,+),ratUrbInhab(+,-))).
%mode(freq, maxMod, (hasAccount(-,+,true),gender(+,-))). %uncommented
mode(freq, avg, (hasAccount(-,+,true),age(+,-))).
mode(freq, avg, (hasLoan(+,-,true),loanAmount(+,-))).
mode(freq, maxMod, (hasLoan(+,-,true),loanStatus(+,-))).
%mode(freq, avg, (hasLoan(+,-,true),monthlyPayments(+,-))). %Uncommented

%For order% mode(clientDistrict, none, age(+,-)).
%For order% mode(clientDistrict, none, gender(+,-)).
%For order% mode(clientDistrict, none, avgSalary(+,-)).
%For order% mode(clientDistrict, none, ratUrbInhab(+,-)).
%For order% mode(clientDistrict, avg, (hasAccount(+,-,true),avgSumOfW(+,-))).
%For order% mode(clientDistrict, avg, (hasAccount(+,-,true),stdMonthInc(+,-))).
%For order% mode(clientDistrict, avg, (hasAccount(+,-,true),stdMonthW(+,-))).
%For order% mode(clientDistrict, avg, (hasAccount(+,-,true),avgNrWith(+,-))).
%For order% mode(clientDistrict, avg, (hasAccount(+,-,true),avgSumOfInc(+,-))).
%For order% mode(clientDistrict, maxMod, (hasAccount(+,-,true),freq(+,-))).
%For order% mode(clientDistrict, avg, (clientLoan(+,-,true),loanAmount(+,-))).
%For order% mode(clientDistrict, maxMod, (clientLoan(+,-,true),loanStatus(+,-))).
%For order% mode(clientDistrict, avg, (clientLoan(+,-,true),monthlyPayments(+,-))).

%For order% mode(hasAccount, none, age(+,-)).
%For order% mode(hasAccount, none, gender(+,-)).
mode(hasAccount, cnt, clientDistrict(+,-,true)).
mode(hasAccount, cnt, clientLoan(+,-,true)).
%For order% mode(hasAccount, avg, (clientDistrict(+,-,true),avgSalary(+,-))).
%For order% mode(hasAccount, avg, (clientDistrict(+,-,true),ratUrbInhab(+,-))).
%For order% mode(hasAccount, none, avgSumOfW(+,-)).
%For order% mode(hasAccount, none, stdMonthInc(+,-)).
%For order% mode(hasAccount, none, stdMonthW(+,-)).
%For order% mode(hasAccount, none, avgNrWith(+,-)).
%For order% mode(hasAccount, none, avgSumOfInc(+,-)).
%For order% mode(hasAccount, none, freq(+,-)).
%For order% mode(hasAccount, avg, (clientLoan(+,-,true),loanAmount(+,-))).
%For order% mode(hasAccount, maxMod, (clientLoan(+,-,true),loanStatus(+,-))).
%For order% mode(hasAccount, avg, (clientLoan(+,-,true),monthlyPayments(+,-))).

mode(clientLoan, cnt, clientDistrict(+,-,true)).
%For order% mode(clientLoan, none, age(+,-)).
%For order% mode(clientLoan, none, gender(+,-)).
%For order% mode(clientLoan, avg, (clientDistrict(+,-,true),avgSalary(+,-))).
%For order% mode(clientLoan, avg, (clientDistrict(+,-,true),ratUrbInhab(+,-))).
%For order% mode(clientLoan, avg, (hasAccount(+,-,true),avgSumOfW(+,-))).
%For order% mode(clientLoan, avg, (hasAccount(+,-,true),stdMonthInc(+,-))).
%For order% mode(clientLoan, avg, (hasAccount(+,-,true),stdMonthW(+,-))).
%For order% mode(clientLoan, avg, (hasAccount(+,-,true),avgNrWith(+,-))).
%For order% mode(clientLoan, avg, (hasAccount(+,-,true),avgSumOfInc(+,-))).
%For order% mode(clientLoan, maxMod, (hasAccount(+,-,true),freq(+,-))).
%For order% mode(clientLoan, none, loanAmount(+,-)).
%For order% mode(clientLoan, none, loanStatus(+,-)).
%For order% mode(clientLoan, none, monthlyPayments(+,-)).

mode(hasLoan, cnt, districtClientAccount(-,-,+)).
mode(hasLoan, cnt, hasAccount(-,+,true)).
%For order% mode(hasLoan, none, avgSumOfInc(+,-)).
%For order% mode(hasLoan, none, avgSumOfW(+,-)).
%For order% mode(hasLoan, none, stdMonthInc(+,-)).
%For order% mode(hasLoan, none, stdMonthW(+,-)).
%For order% mode(hasLoan, none, avgNrWith(+,-)).
%For order% mode(hasLoan, none, freq(+,-)).
%For order% mode(hasLoan, avg, (districtClientAccount(-,-,+),avgSalary(+,-))).
%For order% mode(hasLoan, avg, (districtClientAccount(-,-,+),ratUrbInhab(+,-))).
%For order% mode(hasLoan, maxMod, (hasAccount(-,+,true),gender(+,-))).
%For order% mode(hasLoan, avg, (hasAccount(-,+,true),age(+,-))).
%For order% mode(hasLoan, none, loanAmount(+,-)).
%For order% mode(hasLoan, none, loanStatus(+,-)).
%For order% mode(hasLoan, none, monthlyPayments(+,-)).

%Aggregations
agg(none).
agg(avg).
agg(maxMod).
agg(cnt).
  
%Threshold
thres(hasLoan, 3, continuous, []).
thres(clientDistrict, 3, continuous, []).
thres(districtClientAccount, 3, continuous, []).
thres(districtClientLoan, 3, continuous, []).
thres(hasAccount, 3, continuous, []).
thres(clientLoan, 3, continuous, []).
thres(loanAmount, 2, continuous, []).
thres(loanStatus, 2, discrete, [a,b,c,d]).
thres(monthlyPayments, 2, continuous, []).
thres(gender, 2, discrete, [m,f]).
thres(age, 2, continuous, []).
thres(freq, 2, discrete, [i,w,m]).
thres(avgSalary, 2, continuous, []).
thres(ratUrbInhab, 2, continuous, []).
thres(avgNrWith, 2, continuous, []).
thres(avgSumOfInc, 2, continuous, []).
thres(avgSumOfW, 2, continuous, []).
thres(stdMonthInc, 2, continuous, []).
thres(stdMonthW, 2, continuous, []).

%Target
%learn(hasLoan, 3, 3, discrete).
%learn(clientDistrict, 3, 3, discrete).
%learn(hasAccount, 3, 3, discrete).
%learn(clientLoan, 3, 3, discrete).
learn(loanAmount, 2, 2, continuous).
learn(loanStatus, 2, 2, discrete).
learn(monthlyPayments, 2, 2, continuous).
learn(gender, 2, 2, discrete).
learn(age, 2, 2, continuous).
learn(freq, 2, 2, discrete).
learn(avgSalary, 2, 2, continuous).
learn(ratUrbInhab, 2, 2, continuous).
learn(avgNrWith, 2, 2, continuous).
learn(avgSumOfInc, 2, 2, continuous).
learn(avgSumOfW, 2, 2, continuous).
learn(stdMonthInc, 2, 2, continuous).
learn(stdMonthW, 2, 2, continuous).


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
requsite(X,ProbEvidence,Intervention) :- retractAndAssert(X,Y), bayesBall(X,Visited,MarkOnTop,MarkOnBottom,Evidence),
                                         intersection(Evidence,Visited,ProbEvidence1),
                                         intersection(MarkOnTop,ProbEvidence1,ProbEvidence),
                                         subtract(ProbEvidence1, ProbEvidence, Intervention),assertAndRetract(X,Y).
