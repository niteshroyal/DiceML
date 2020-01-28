'''
Created on Aug 28, 2018

@author: nitesh
'''
import scipy.stats
import logging, time
import statistics
import copy, math
from os import listdir
from os.path import isfile, join
from core.YapPrologInterface import YapPrologInterface
from core.DCLearner import DCLearner
from core.TranslateToDC import TranslateToDC
from sklearn.metrics import confusion_matrix
from sklearn.metrics import accuracy_score
from collections import Counter
import numpy as np
from sklearn.metrics import roc_auc_score

FOLD = 1
DATA_DIRECTORY = '/home/nitesh/eclipse-workspace/DistributionalProgramSynthesis/data/PKDD/pkdd'
PROCESSED_TRAIN_DATA_FILE = '../data/Financial.pl'
PROCESSED_TRAIN_DATA_FILE_DC = '../data/FinancialDC.pl'
DATABASE_NAME = 'financial'

#DATA_DIRECTORY = '/home/nitesh/eclipse-workspace/DistributionalProgramSynthesis/data/University/data800x125x125'
#PROCESSED_TRAIN_DATA_FILE = '../data/University.pl'
#PROCESSED_TRAIN_DATA_FILE_DC = '../data/UniversityDC.pl'
#DATABASE_NAME = 'university'

#DATA_DIRECTORY = '/home/nitesh/eclipse-workspace/DistributionalProgramSynthesis/data/Hepatitis'
#PROCESSED_TRAIN_DATA_FILE = '../data/Hepatitis.pl'
#PROCESSED_TRAIN_DATA_FILE_DC = '../data/HepatitisDC.pl'
#DATABASE_NAME = 'Hepatitis_std'

OUTPUT_ENSEMBLE_OF_DLTS = '../data/EnsembleOfDLTs.pl'

DC_RULES = []
TRAIN_FACTS = []
TEST_FACTS = []

foldString = "Fold" + str(FOLD)

RELATIONAL_PREDICATES = dict(
    financial = ['hasLoan', 'clientDistrict', 'hasAccount', 'clientLoan'],
    university = ['takes', 'friend', 'teaches'],
    Hepatitis_std = ['rel11', 'rel12', 'rel13']
)
RANDOM_VARIABLE_PREDICATE = dict(
    financial = ['hasLoan', 'clientDistrict', 'hasAccount', 'clientLoan', 'loanAmount', 'loanStatus','monthlyPayments', 'gender', 'age', 'freq', 'avgSalary', 'ratUrbInhab', 'avgNrWith', 'avgSumOfInc', 'avgSumOfW', 'stdMonthInc', 'stdMonthW'],
    university = ['nrhours', 'difficulty', 'ability', 'intelligence', 'grade', 'satisfaction', 'takes', 'friend', 'teaches'],
    Hepatitis_std = ['fibros', 'activity', 'sex', 'age', 'type', 'got', 'gpt', 'alb', 'tbil', 'dbil', 'che', 'ttt', 'ztt', 'tcho', 'tp', 'dur']
)
RELATIONAL_CHAIN_OF_PREDICATES = dict(
    financial = {
        'districtClientAccount' : ['clientDistrict', 'hasAccount'],
        'districtClientLoan' : ['clientDistrict', 'clientLoan']
    },
    university = {
        'studentCourseProfessor' : ['takes', 'teaches']
    },
    Hepatitis_std = {
        'bioDispatInf' : ['rel11', 'rel13'],
        'infDispatIndis' : ['rel13', 'rel12'],
        'bioDispatIndis' : ['rel11', 'rel12']
    }
)

RANDOM_VARIABLE_PREDICATE_INITIAL_VAL = dict(
    financial = dict(
        loanAmount = 89880.0,
        monthlyPayments = 3373.0,
        age = 22,
        avgSalary = 12541.0,
        ratUrbInhab = 100.0,
        avgNrWith = 11.41666667,
        avgSumOfInc = 16193.375,
        avgSumOfW = 15071.1666667,
        stdMonthInc = 5526.82090052,
        stdMonthW = 4456.88208549,
        loanStatus = 'c',
        gender = 'm',
        freq = 'm',
        hasLoan = 'true',
        clientDistrict = 'true',
        hasAccount = 'true',
        clientLoan = 'true'
    ), 
)

RANDOM_VARIABLE_PREDICATE_TYPE = dict(
    financial = dict(
        hasLoan = 'discrete',
        clientDistrict = 'discrete',
        hasAccount = 'discrete',
        clientLoan = 'discrete',
        loanAmount = 'continuous',
        loanStatus = 'discrete',
        monthlyPayments = 'continuous',
        gender = 'discrete',
        age = 'continuous',
        freq = 'discrete',
        avgSalary = 'continuous',
        ratUrbInhab = 'continuous',
        avgNrWith = 'continuous',
        avgSumOfInc = 'continuous',
        avgSumOfW = 'continuous',
        stdMonthInc = 'continuous',
        stdMonthW = 'continuous'
    ),
    university = dict(
        nrhours = 'continuous',
        difficulty = 'discrete', 
        ability = 'continuous',
        intelligence = 'continuous', 
        grade = 'discrete',
        satisfaction = 'discrete',
        takes = 'discrete',
        friend = 'discrete',
        teaches = 'discrete'
    ),
    Hepatitis_std = dict(
        fibros = 'continuous',
        activity = 'continuous',
        sex = 'discrete',
        age = 'continuous',
        type = 'discrete',
        got = 'continuous',
        gpt = 'continuous',
        alb = 'discrete',
        tbil = 'discrete',
        dbil = 'discrete',
        che = 'continuous',
        ttt = 'continuous',
        ztt = 'continuous',
        tcho = 'continuous',
        tp = 'continuous',
        dur = 'continuous',
        rel11 = 'discrete',
        rel12 = 'discrete',
        rel13 = 'discrete'
    )
)

RANDOM_VARIABLE_PREDICATE_RANGE = dict(
    financial = dict(
        loanAmount = 585840,
        monthlyPayments = 9606,
        age = 55,
        avgSalary = 4431,
        ratUrbInhab = 66.1,
        avgNrWith = 42.1667,
        avgSumOfInc = 319796.45,
        avgSumOfW = 299106.333,
        stdMonthInc = 92689.04822,
        stdMonthW = 77248.6264725725,
        loanStatus = ['a','b','c','d'],
        gender = ['m', 'f'],
        freq = ['i', 'w', 'm'],
        hasLoan = ['true', 'false'],
        clientDistrict = ['true', 'false'],
        hasAccount = ['true', 'false'],
        clientLoan = ['true', 'false']
    ), 
    university = dict(
        nrhours = 160.0,
        difficulty = ['medium','hard','easy'], 
        ability = 80.0,
        intelligence = 130.0, 
        grade = ['high','low','mid'],
        satisfaction = ['mid','low','high'],
        takes = ['true', 'false'],
        friend = ['true', 'false'],
        teaches = ['true', 'false']
    ),
    Hepatitis_std = dict(
        fibros = 4,
        activity = 4,
        sex = ['male', 'female'],
        age = 6,
        type = ['negative', 'positive'],
        got = 5,
        gpt = 3,
        alb = ['low', 'high'],
        tbil = ['low', 'high'],
        dbil = ['low', 'high'],
        che = 9,
        ttt = 5,
        ztt = 5,
        tcho = 3,
        tp = 3,
        dur = 4
    )
)


INITIAL_VAlUES = dict(
    financial = dict(
        loanAmount = '151845.71',
        monthlyPayments = '4216.92',
        age = '38.83',
        avgSalary = '9033.14',
        ratUrbInhab = '63.03',
        avgNrWith = '11.71',
        avgSumOfInc = '59466.66',
        avgSumOfW = '51974.20',
        stdMonthInc = '21168.75',
        stdMonthW = '15378.94',
        loanStatus = 'c',
        gender = 'm',
        freq = 'm'
    )
)

DECLARATIVE_BIAS_ORDERED = dict(
    financial = '''
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
  
%Facts
''', 

university = '''
:- discontiguous course/1.
:- discontiguous professor/1.
:- discontiguous student/1.
:- discontiguous nrhours/2.
:- discontiguous difficulty/2.
:- discontiguous ability/2.
:- discontiguous intelligence/2.
:- discontiguous grade/3.
:- discontiguous satisfaction/3.
:- discontiguous takes/3.
:- discontiguous friend/3.
:- discontiguous teaches/3.

:- dynamic evidence/1.
:- dynamic course/1.
:- dynamic professor/1.
:- dynamic student/1.
:- dynamic nrhours/2.
:- dynamic difficulty/2.
:- dynamic ability/2.
:- dynamic intelligence/2.
:- dynamic grade/3.
:- dynamic satisfaction/3.
:- dynamic takes/3.
:- dynamic friend/3.
:- dynamic teaches/3.


%Relational Lookahead
studentCourseProfessor(S,C,P) :- takes(S,C,true), teaches(P,C,true).

%Types
base(course(c)).
base(professor(p)).
base(student(s)).
base(studentCourseProfessor(s,c,p)).
base(nrhours(c,nr)).
base(difficulty(c,di)).
base(ability(p,ab)).
base(intelligence(s,in)).
base(grade(s,c,gr)).
base(satisfaction(s,c,sa)).
base(takes(s,c,x1)).
base(friend(s,s,x2)).
base(teaches(p,c,x3)).

%Modes
%mode(nrhours, none, difficulty(+,-)).
%mode(nrhours, avg, (takes(-,+,true), intelligence(+,-))).
%mode(nrhours, maxMod, (takes(-,+,true), grade(+,+,-))).
%mode(nrhours, maxMod, (takes(-,+,true), satisfaction(+,+,-))).
%mode(nrhours, avg, (teaches(-,+,true), ability(+,-))).

mode(difficulty, none, nrhours(+,-)).
%mode(difficulty, avg, (takes(-,+,true), intelligence(+,-))).
%mode(difficulty, maxMod, (takes(-,+,true), grade(+,+,-))).
%mode(difficulty, maxMod, (takes(-,+,true), satisfaction(+,+,-))).
%mode(difficulty, avg, (teaches(-,+,true), ability(+,-))).

mode(ability, avg, (teaches(+,-,true), nrhours(+,-))).
mode(ability, maxMod, (teaches(+,-,true), difficulty(+,-))).

%mode(intelligence, maxMod, grade(+,-,-)).
%mode(intelligence, maxMod, satisfaction(+,-,-)).
mode(intelligence, avg, (takes(+,-,true), nrhours(+,-))).
mode(intelligence, maxMod, (takes(+,-,true), difficulty(+,-))).

mode(grade, none, intelligence(+,-)).
mode(grade, maxMod, satisfaction(+,+,-)).
mode(grade, avg, (takes(+,+,true), nrhours(+,-))).
mode(grade, maxMod, (takes(+,+,true), difficulty(+,-))).

mode(takes, none, intelligence(+,-)).
mode(takes, none, satisfaction(+,+,-)).
mode(takes, none, difficulty(-,+)).
mode(takes, none, nrhours(-,+)).

mode(satisfaction, none, intelligence(+,-)).
%mode(satisfaction, maxMod, grade(+,+,-)).
mode(satisfaction, avg, (takes(+,+,true), nrhours(+,-))).
mode(satisfaction, maxMod, (takes(+,+,true), difficulty(+,-))).

mode(friend, cnt, takes(+,-,true)).
mode(friend, maxMod, grade(+,-,-)).
mode(friend, maxMod, satisfaction(+,-,-)).
mode(friend, none, intelligence(+,-)).

%Aggregations
agg(none).
agg(avg).
agg(maxMod).
agg(cnt).
  
%Threshold
thres(nrhours, 2, continuous, []).
thres(difficulty, 2, discrete, [medium,hard,easy]).
thres(ability, 2, continuous, []).
thres(intelligence, 2, continuous, []).
thres(grade, 3, discrete, [high,low,mid]).
thres(satisfaction, 3, discrete, [mid,low,high]).
thres(friend, 3, discrete, [true,false]).
thres(takes, 3, continuous, []).
thres(teaches, 3, continuous, []).

%Target
learn(nrhours, 2, 2, continuous).
learn(difficulty, 2, 2, discrete).
learn(ability, 2, 2, continuous).
learn(intelligence, 2, 2, continuous).
learn(grade, 3, 3, discrete).
learn(satisfaction, 3, 3, discrete).
learn(friend, 3, 3, discrete).
learn(takes, 3, 3, discrete).
learn(teaches, 3, 3, discrete).

%Facts
''',
Hepatitis_std = '''
:- discontiguous bio/1.
:- discontiguous dispat/1.
:- discontiguous indis/1.
:- discontiguous inf/1.
:- discontiguous rel11/3.
:- discontiguous rel12/3.
:- discontiguous rel13/3.
:- discontiguous fibros/2.
:- discontiguous activity/2.
:- discontiguous sex/2.
:- discontiguous age/2.
:- discontiguous type/2.
:- discontiguous got/2.
:- discontiguous gpt/2.
:- discontiguous alb/2.
:- discontiguous tbil/2.
:- discontiguous dbil/2.
:- discontiguous che/2.
:- discontiguous ttt/2.
:- discontiguous ztt/2.
:- discontiguous tcho/2.
:- discontiguous tp/2.
:- discontiguous dur/2.

:- dynamic evidence/1.
:- dynamic bio/1.
:- dynamic dispat/1.
:- dynamic indis/1.
:- dynamic inf/1.
:- dynamic rel11/3.
:- dynamic rel12/3.
:- dynamic rel13/3.
:- dynamic fibros/2.
:- dynamic activity/2.
:- dynamic sex/2.
:- dynamic age/2.
:- dynamic type/2.
:- dynamic got/2.
:- dynamic gpt/2.
:- dynamic alb/2.
:- dynamic tbil/2.
:- dynamic dbil/2.
:- dynamic che/2.
:- dynamic ttt/2.
:- dynamic ztt/2.
:- dynamic tcho/2.
:- dynamic tp/2.
:- dynamic dur/2.

%Relational Lookahead
bioDispatInf(B,D,J) :- rel11(B,D,true), rel13(J,D,true).
infDispatIndis(J,D,I) :- rel13(J,D,true), rel12(I,D,true).
bioDispatIndis(B,D,I) :- rel11(B,D,true), rel12(I,D,true).

%Types
base(bio(b)).
base(dispat(d)).
base(indis(i)).
base(inf(j)).
base(rel11(b,d,x1)).
base(rel13(j,d,x2)).
base(rel12(i,d,x3)).
base(bioDispatInf(b,d,j)).
base(infDispatIndis(j,d,i)).
base(bioDispatIndis(b,d,i)).
base(fibros(b,fi)).
base(activity(b,ac)).
base(sex(d,se)).
base(age(d,ag)).
base(type(d,ty)).
base(got(i,go)).
base(gpt(i,gp)).
base(alb(i,al)).
base(tbil(i,tb)).
base(dbil(i,db)).
base(che(i,ch)).
base(ttt(i,tt)).
base(ztt(i,zt)).
base(tcho(i,tc)).
base(tp(i,tp)).
base(dur(j,du)).
  
%Modes
mode(fibros, none, activity(+,-)).
%%mode(fibros, maxMod, (rel11(+,-,true), sex(+,-))).
%%mode(fibros, avg, (rel11(+,-,true), age(+,-))).
%%mode(fibros, maxMod, (rel11(+,-,true), type(+,-))).
%mode(fibros, avg, (bioDispatIndis(+,-,-), got(+,-))).
%mode(fibros, avg, (bioDispatIndis(+,-,-), gpt(+,-))).
%mode(fibros, maxMod, (bioDispatIndis(+,-,-), alb(+,-))).
%mode(fibros, maxMod, (bioDispatIndis(+,-,-), tbil(+,-))).
%mode(fibros, maxMod, (bioDispatIndis(+,-,-), dbil(+,-))).
%mode(fibros, avg, (bioDispatIndis(+,-,-), che(+,-))).
%mode(fibros, avg, (bioDispatIndis(+,-,-), ttt(+,-))).
%mode(fibros, avg, (bioDispatIndis(+,-,-), ztt(+,-))).
%mode(fibros, avg, (bioDispatIndis(+,-,-), tcho(+,-))).
%mode(fibros, avg, (bioDispatIndis(+,-,-), tp(+,-))).
%mode(fibros, avg, (bioDispatInf(+,-,-), dur(+,-))).

%%mode(activity, none, fibros(+,-)).
%%mode(activity, maxMod, (rel11(+,-,true), sex(+,-))).
%%mode(activity, avg, (rel11(+,-,true), age(+,-))).
%%mode(activity, maxMod, (rel11(+,-,true), type(+,-))).
%mode(activity, avg, (bioDispatIndis(+,-,-), got(+,-))).
%mode(activity, avg, (bioDispatIndis(+,-,-), gpt(+,-))).
%mode(activity, maxMod, (bioDispatIndis(+,-,-), alb(+,-))).
%mode(activity, maxMod, (bioDispatIndis(+,-,-), tbil(+,-))).
%mode(activity, maxMod, (bioDispatIndis(+,-,-), dbil(+,-))).
%mode(activity, avg, (bioDispatIndis(+,-,-), che(+,-))).
%mode(activity, avg, (bioDispatIndis(+,-,-), ttt(+,-))).
%mode(activity, avg, (bioDispatIndis(+,-,-), ztt(+,-))).
%mode(activity, avg, (bioDispatIndis(+,-,-), tcho(+,-))).
%mode(activity, avg, (bioDispatIndis(+,-,-), tp(+,-))).
%mode(activity, avg, (bioDispatInf(+,-,-), dur(+,-))).

mode(sex, avg, (rel11(-,+,true),fibros(+,-))).
mode(sex, avg, (rel11(-,+,true),activity(+,-))).
%%mode(sex, none, age(+,-)).
%%mode(sex, none, type(+,-)).
mode(sex, avg, (rel12(-,+,true), got(+,-))).
mode(sex, avg, (rel12(-,+,true), gpt(+,-))).
mode(sex, maxMod, (rel12(-,+,true), alb(+,-))).
mode(sex, maxMod, (rel12(-,+,true), tbil(+,-))).
mode(sex, maxMod, (rel12(-,+,true), dbil(+,-))).
mode(sex, avg, (rel12(-,+,true), che(+,-))).
mode(sex, avg, (rel12(-,+,true), ttt(+,-))).
mode(sex, avg, (rel12(-,+,true), ztt(+,-))).
mode(sex, avg, (rel12(-,+,true), tcho(+,-))).
mode(sex, avg, (rel12(-,+,true), tp(+,-))).
mode(sex, avg, (rel13(-,+,true), dur(+,-))).

mode(age, avg, (rel11(-,+,true),fibros(+,-))).
mode(age, avg, (rel11(-,+,true),activity(+,-))).
mode(age, none, sex(+,-)).
%%mode(age, none, type(+,-)).
mode(age, avg, (rel12(-,+,true), got(+,-))).
mode(age, avg, (rel12(-,+,true), gpt(+,-))).
mode(age, maxMod, (rel12(-,+,true), alb(+,-))).
mode(age, maxMod, (rel12(-,+,true), tbil(+,-))).
mode(age, maxMod, (rel12(-,+,true), dbil(+,-))).
mode(age, avg, (rel12(-,+,true), che(+,-))).
mode(age, avg, (rel12(-,+,true), ttt(+,-))).
mode(age, avg, (rel12(-,+,true), ztt(+,-))).
mode(age, avg, (rel12(-,+,true), tcho(+,-))).
mode(age, avg, (rel12(-,+,true), tp(+,-))).
mode(age, avg, (rel13(-,+,true), dur(+,-))).

mode(type, avg, (rel11(-,+,true),fibros(+,-))).
mode(type, avg, (rel11(-,+,true),activity(+,-))).
mode(type, none, sex(+,-)).
mode(type, none, age(+,-)).
mode(type, avg, (rel12(-,+,true), got(+,-))).
mode(type, avg, (rel12(-,+,true), gpt(+,-))).
mode(type, maxMod, (rel12(-,+,true), alb(+,-))).
mode(type, maxMod, (rel12(-,+,true), tbil(+,-))).
mode(type, maxMod, (rel12(-,+,true), dbil(+,-))).
mode(type, avg, (rel12(-,+,true), che(+,-))).
mode(type, avg, (rel12(-,+,true), ttt(+,-))).
mode(type, avg, (rel12(-,+,true), ztt(+,-))).
mode(type, avg, (rel12(-,+,true), tcho(+,-))).
mode(type, avg, (rel12(-,+,true), tp(+,-))).
mode(type, avg, (rel13(-,+,true), dur(+,-))).

%%mode(got, avg, (bioDispatIndis(-,-,+),fibros(+,-))).
%%mode(got, avg, (bioDispatIndis(-,-,+),activity(+,-))).
%%mode(got, maxMod, (rel12(+,-,true), sex(+,-))).
%%mode(got, avg, (rel12(+,-,true), age(+,-))).
%%mode(got, maxMod, (rel12(+,-,true), type(+,-))).
mode(got, none, gpt(+,-)).
mode(got, none, alb(+,-)).
mode(got, none, tbil(+,-)).
mode(got, none, dbil(+,-)).
mode(got, none, che(+,-)).
mode(got, none, ttt(+,-)).
mode(got, none, ztt(+,-)).
mode(got, none, tcho(+,-)).
mode(got, none, tp(+,-)).
%%mode(got, avg, (infDispatIndis(-,-,+), dur(+,-))).

%%mode(gpt, avg, (bioDispatIndis(-,-,+),fibros(+,-))).
%%mode(gpt, avg, (bioDispatIndis(-,-,+),activity(+,-))).
%%mode(gpt, maxMod, (rel12(+,-,true), sex(+,-))).
%%mode(gpt, avg, (rel12(+,-,true), age(+,-))).
%%mode(gpt, maxMod, (rel12(+,-,true), type(+,-))).
%%mode(gpt, none, got(+,-)).
mode(gpt, none, alb(+,-)).
mode(gpt, none, tbil(+,-)).
mode(gpt, none, dbil(+,-)).
mode(gpt, none, che(+,-)).
mode(gpt, none, ttt(+,-)).
mode(gpt, none, ztt(+,-)).
mode(gpt, none, tcho(+,-)).
mode(gpt, none, tp(+,-)).
%%mode(gpt, avg, (infDispatIndis(-,-,+), dur(+,-))).

%%mode(alb, avg, (bioDispatIndis(-,-,+),fibros(+,-))).
%%mode(alb, avg, (bioDispatIndis(-,-,+),activity(+,-))).
%%mode(alb, maxMod, (rel12(+,-,true), sex(+,-))).
%%mode(alb, avg, (rel12(+,-,true), age(+,-))).
%%mode(alb, maxMod, (rel12(+,-,true), type(+,-))).
%%mode(alb, none, got(+,-)).
%%mode(alb, none, gpt(+,-)).
mode(alb, none, tbil(+,-)).
mode(alb, none, dbil(+,-)).
mode(alb, none, che(+,-)).
mode(alb, none, ttt(+,-)).
mode(alb, none, ztt(+,-)).
mode(alb, none, tcho(+,-)).
mode(alb, none, tp(+,-)).
%%mode(alb, avg, (infDispatIndis(-,-,+), dur(+,-))).

%%mode(tbil, avg, (bioDispatIndis(-,-,+),fibros(+,-))).
%%mode(tbil, avg, (bioDispatIndis(-,-,+),activity(+,-))).
%%mode(tbil, maxMod, (rel12(+,-,true), sex(+,-))).
%%mode(tbil, avg, (rel12(+,-,true), age(+,-))).
%%mode(tbil, maxMod, (rel12(+,-,true), type(+,-))).
%%mode(tbil, none, got(+,-)).
%%mode(tbil, none, gpt(+,-)).
%%mode(tbil, none, alb(+,-)).
mode(tbil, none, dbil(+,-)).
mode(tbil, none, che(+,-)).
mode(tbil, none, ttt(+,-)).
mode(tbil, none, ztt(+,-)).
mode(tbil, none, tcho(+,-)).
mode(tbil, none, tp(+,-)).
%%mode(tbil, avg, (infDispatIndis(-,-,+), dur(+,-))).

%%mode(dbil, avg, (bioDispatIndis(-,-,+),fibros(+,-))).
%%mode(dbil, avg, (bioDispatIndis(-,-,+),activity(+,-))).
%%mode(dbil, maxMod, (rel12(+,-,true), sex(+,-))).
%%mode(dbil, avg, (rel12(+,-,true), age(+,-))).
%%mode(dbil, maxMod, (rel12(+,-,true), type(+,-))).
%%mode(dbil, none, got(+,-)).
%%mode(dbil, none, gpt(+,-)).
%%mode(dbil, none, alb(+,-)).
%%mode(dbil, none, tbil(+,-)).
mode(dbil, none, che(+,-)).
mode(dbil, none, ttt(+,-)).
mode(dbil, none, ztt(+,-)).
mode(dbil, none, tcho(+,-)).
mode(dbil, none, tp(+,-)).
%%mode(dbil, avg, (infDispatIndis(-,-,+), dur(+,-))).

%%mode(che, avg, (bioDispatIndis(-,-,+),fibros(+,-))).
%%mode(che, avg, (bioDispatIndis(-,-,+),activity(+,-))).
%%mode(che, maxMod, (rel12(+,-,true), sex(+,-))).
%%mode(che, avg, (rel12(+,-,true), age(+,-))).
%%mode(che, maxMod, (rel12(+,-,true), type(+,-))).
%%mode(che, none, got(+,-)).
%%mode(che, none, gpt(+,-)).
%%mode(che, none, alb(+,-)).
%%mode(che, none, tbil(+,-)).
%%mode(che, none, dbil(+,-)).
mode(che, none, ttt(+,-)).
mode(che, none, ztt(+,-)).
mode(che, none, tcho(+,-)).
mode(che, none, tp(+,-)).
%%mode(che, avg, (infDispatIndis(-,-,+), dur(+,-))).

%%mode(ttt, avg, (bioDispatIndis(-,-,+),fibros(+,-))).
%%mode(ttt, avg, (bioDispatIndis(-,-,+),activity(+,-))).
%%mode(ttt, maxMod, (rel12(+,-,true), sex(+,-))).
%%mode(ttt, avg, (rel12(+,-,true), age(+,-))).
%%mode(ttt, maxMod, (rel12(+,-,true), type(+,-))).
%%mode(ttt, none, got(+,-)).
%%mode(ttt, none, gpt(+,-)).
%%mode(ttt, none, alb(+,-)).
%%mode(ttt, none, tbil(+,-)).
%%mode(ttt, none, dbil(+,-)).
%%mode(ttt, none, che(+,-)).
mode(ttt, none, ztt(+,-)).
mode(ttt, none, tcho(+,-)).
mode(ttt, none, tp(+,-)).
%%mode(ttt, avg, (infDispatIndis(-,-,+), dur(+,-))).

%%mode(ztt, avg, (bioDispatIndis(-,-,+),fibros(+,-))).
%%mode(ztt, avg, (bioDispatIndis(-,-,+),activity(+,-))).
%%mode(ztt, maxMod, (rel12(+,-,true), sex(+,-))).
%%mode(ztt, avg, (rel12(+,-,true), age(+,-))).
%%mode(ztt, maxMod, (rel12(+,-,true), type(+,-))).
%%mode(ztt, none, got(+,-)).
%%mode(ztt, none, gpt(+,-)).
%%mode(ztt, none, alb(+,-)).
%%mode(ztt, none, tbil(+,-)).
%%mode(ztt, none, dbil(+,-)).
%%mode(ztt, none, che(+,-)).
%%mode(ztt, none, ttt(+,-)).
mode(ztt, none, tcho(+,-)).
mode(ztt, none, tp(+,-)).
%%mode(ztt, avg, (infDispatIndis(-,-,+), dur(+,-))).

%%mode(tcho, avg, (bioDispatIndis(-,-,+),fibros(+,-))).
%%mode(tcho, avg, (bioDispatIndis(-,-,+),activity(+,-))).
%%mode(tcho, maxMod, (rel12(+,-,true), sex(+,-))).
%%mode(tcho, avg, (rel12(+,-,true), age(+,-))).
%%mode(tcho, maxMod, (rel12(+,-,true), type(+,-))).
%%mode(tcho, none, got(+,-)).
%%mode(tcho, none, gpt(+,-)).
%%mode(tcho, none, alb(+,-)).
%%mode(tcho, none, tbil(+,-)).
%%mode(tcho, none, dbil(+,-)).
%%mode(tcho, none, che(+,-)).
%%mode(tcho, none, ttt(+,-)).
%%mode(tcho, none, ztt(+,-)).
mode(tcho, none, tp(+,-)).
%%mode(tcho, avg, (infDispatIndis(-,-,+), dur(+,-))).

%%mode(tp, avg, (bioDispatIndis(-,-,+),fibros(+,-))).
%%mode(tp, avg, (bioDispatIndis(-,-,+),activity(+,-))).
%%mode(tp, maxMod, (rel12(+,-,true), sex(+,-))).
%%mode(tp, avg, (rel12(+,-,true), age(+,-))).
%%mode(tp, maxMod, (rel12(+,-,true), type(+,-))).
%%mode(tp, none, got(+,-)).
%%mode(tp, none, gpt(+,-)).
%%mode(tp, none, alb(+,-)).
%%mode(tp, none, tbil(+,-)).
%%mode(tp, none, dbil(+,-)).
%%mode(tp, none, che(+,-)).
%%mode(tp, none, ttt(+,-)).
%%mode(tp, none, ztt(+,-)).
%%mode(tp, none, tcho(+,-)).
%%mode(tp, avg, (infDispatIndis(-,-,+), dur(+,-))).

%%mode(dur, avg, (bioDispatInf(-,-,+),fibros(+,-))).
%%mode(dur, avg, (bioDispatInf(-,-,+),activity(+,-))).
%%mode(dur, maxMod, (rel13(+,-,true), sex(+,-))).
%%mode(dur, avg, (rel13(+,-,true), age(+,-))).
%%mode(dur, maxMod, (rel13(+,-,true), type(+,-))).
%mode(dur, avg, (infDispatIndis(+,-,-), got(+,-))).
%mode(dur, avg, (infDispatIndis(+,-,-), gpt(+,-))).
%mode(dur, maxMod, (infDispatIndis(+,-,-), alb(+,-))).
%mode(dur, maxMod, (infDispatIndis(+,-,-), tbil(+,-))).
%mode(dur, maxMod, (infDispatIndis(+,-,-), dbil(+,-))).
%mode(dur, avg, (infDispatIndis(+,-,-), che(+,-))).
%mode(dur, avg, (infDispatIndis(+,-,-), ttt(+,-))).
%mode(dur, avg, (infDispatIndis(+,-,-), ztt(+,-))).
%mode(dur, avg, (infDispatIndis(+,-,-), tcho(+,-))).
%mode(dur, avg, (infDispatIndis(+,-,-), tp(+,-))).

%Aggregations
agg(none).
agg(avg).
agg(maxMod).
agg(cnt).
  
%Threshold
thres(rel11, 3, continuous, []).
thres(rel12, 3, continuous, []).
thres(rel13, 3, continuous, []).
thres(bioDispatInf, 3, continuous, []).
thres(infDispatIndis, 3, continuous, []).
thres(bioDispatIndis, 3, continuous, []).
thres(fibros, 2, continuous, []).
thres(activity, 2, continuous, []).
thres(sex, 2, discrete, [male, female]).
thres(age, 2, continuous, []).
thres(type, 2, discrete, [negative, positive]).
thres(got, 2, continuous, []).
thres(gpt, 2, continuous, []).
thres(alb, 2, discrete, [low, high]).
thres(tbil, 2, discrete, [low, high]).
thres(dbil, 2, discrete, [low, high]).
thres(che, 2, continuous, []).
thres(ttt, 2, continuous, []).
thres(ztt, 2, continuous, []).
thres(tcho, 2, continuous, []).
thres(tp, 2, continuous, []).
thres(dur, 2, continuous, []).

%Target
learn(rel11, 3, 3, discrete).
learn(rel12, 3, 3, discrete).
learn(rel13, 3, 3, discrete).
learn(fibros, 2, 2, continuous).
learn(activity, 2, 2, continuous).
learn(sex, 2, 2, discrete).
learn(age, 2, 2, continuous).
learn(type, 2, 2, discrete).
learn(got, 2, 2, continuous).
learn(gpt, 2, 2, continuous).
learn(alb, 2, 2, discrete).
learn(tbil, 2, 2, discrete).
learn(dbil, 2, 2, discrete).
learn(che, 2, 2, continuous).
learn(ttt, 2, 2, continuous).
learn(ztt, 2, 2, continuous).
learn(tcho, 2, 2, continuous).
learn(tp, 2, 2, continuous).
learn(dur, 2, 2, continuous).
  
%Facts
'''
)

DECLARATIVE_BIAS = dict(
    financial = '''
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
districtClientAccount(D,C,A) :- clientDistrict(C,D,true), hasAccount(C,A,true).
districtClientLoan(D,C,L) :- clientDistrict(C,D,true), clientLoan(C,L,true).

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
mode(avgSalary, none, ratUrbInhab(+,-)).
mode(avgSalary, maxMod, (clientDistrict(-,+,true),gender(+,-))).
mode(avgSalary, avg, (clientDistrict(-,+,true),age(+,-))).
mode(avgSalary, avg, (districtClientAccount(+,-,-),avgSumOfW(+,-))).
mode(avgSalary, avg, (districtClientAccount(+,-,-),stdMonthInc(+,-))).
mode(avgSalary, avg, (districtClientAccount(+,-,-),stdMonthW(+,-))).
mode(avgSalary, avg, (districtClientAccount(+,-,-),avgNrWith(+,-))).
mode(avgSalary, avg, (districtClientAccount(+,-,-),avgSumOfInc(+,-))).
mode(avgSalary, maxMod, (districtClientAccount(+,-,-),freq(+,-))).
mode(avgSalary, avg, (districtClientLoan(+,-,-),loanAmount(+,-))).
mode(avgSalary, maxMod, (districtClientLoan(+,-,-),loanStatus(+,-))).
mode(avgSalary, avg, (districtClientLoan(+,-,-),monthlyPayments(+,-))).

mode(ratUrbInhab, cnt, districtClientAccount(+,-,-)).
mode(ratUrbInhab, none, avgSalary(+,-)).
mode(ratUrbInhab, maxMod, (clientDistrict(-,+,true),gender(+,-))).
mode(ratUrbInhab, avg, (clientDistrict(-,+,true),age(+,-))).
mode(ratUrbInhab, avg, (districtClientAccount(+,-,-),avgSumOfW(+,-))).
mode(ratUrbInhab, avg, (districtClientAccount(+,-,-),stdMonthInc(+,-))).
mode(ratUrbInhab, avg, (districtClientAccount(+,-,-),stdMonthW(+,-))).
mode(ratUrbInhab, avg, (districtClientAccount(+,-,-),avgNrWith(+,-))).
mode(ratUrbInhab, avg, (districtClientAccount(+,-,-),avgSumOfInc(+,-))).
mode(ratUrbInhab, maxMod, (districtClientAccount(+,-,-),freq(+,-))).
mode(ratUrbInhab, avg, (districtClientLoan(+,-,-),loanAmount(+,-))).
mode(ratUrbInhab, maxMod, (districtClientLoan(+,-,-),loanStatus(+,-))).
mode(ratUrbInhab, avg, (districtClientLoan(+,-,-),monthlyPayments(+,-))).

mode(gender, cnt, clientDistrict(+,-,true)).
mode(gender, cnt, hasAccount(+,-,true)).
mode(gender, none, age(+,-)).
mode(gender, avg, (clientDistrict(+,-,true),avgSalary(+,-))).
mode(gender, avg, (clientDistrict(+,-,true),ratUrbInhab(+,-))).
mode(gender, avg, (hasAccount(+,-,true),avgSumOfW(+,-))).
mode(gender, avg, (hasAccount(+,-,true),stdMonthInc(+,-))).
mode(gender, avg, (hasAccount(+,-,true),stdMonthW(+,-))).
mode(gender, avg, (hasAccount(+,-,true),avgNrWith(+,-))).
mode(gender, avg, (hasAccount(+,-,true),avgSumOfInc(+,-))).
mode(gender, maxMod, (hasAccount(+,-,true),freq(+,-))).
mode(gender, avg, (clientLoan(+,-,true),loanAmount(+,-))).
mode(gender, maxMod, (clientLoan(+,-,true),loanStatus(+,-))).
mode(gender, avg, (clientLoan(+,-,true),monthlyPayments(+,-))).

mode(age, cnt, clientDistrict(+,-,true)).
mode(age, cnt, hasAccount(+,-,true)).
mode(age, cnt, clientLoan(+,-,true)).
mode(age, none, gender(+,-)).
mode(age, avg, (clientDistrict(+,-,true),avgSalary(+,-))).
mode(age, avg, (clientDistrict(+,-,true),ratUrbInhab(+,-))).
mode(age, avg, (hasAccount(+,-,true),avgSumOfW(+,-))).
mode(age, avg, (hasAccount(+,-,true),stdMonthInc(+,-))).
mode(age, avg, (hasAccount(+,-,true),stdMonthW(+,-))).
mode(age, avg, (hasAccount(+,-,true),avgNrWith(+,-))).
mode(age, avg, (hasAccount(+,-,true),avgSumOfInc(+,-))).
mode(age, maxMod, (hasAccount(+,-,true),freq(+,-))).
mode(age, avg, (clientLoan(+,-,true),loanAmount(+,-))).
mode(age, maxMod, (clientLoan(+,-,true),loanStatus(+,-))).
mode(age, avg, (clientLoan(+,-,true),monthlyPayments(+,-))).

mode(loanAmount, cnt, hasLoan(-,+,true)).
mode(loanAmount, cnt, districtClientLoan(-,-,+)).
mode(loanAmount, none, loanStatus(+,-)).
mode(loanAmount, none, monthlyPayments(+,-)).
mode(loanAmount, maxMod, (clientLoan(-,+,true),gender(+,-))).
mode(loanAmount, avg, (clientLoan(-,+,true),age(+,-))).
mode(loanAmount, avg, (hasLoan(-,+,true),avgSumOfW(+,-))).
mode(loanAmount, avg, (hasLoan(-,+,true),stdMonthInc(+,-))).
mode(loanAmount, avg, (hasLoan(-,+,true),stdMonthW(+,-))).
mode(loanAmount, avg, (hasLoan(-,+,true),avgNrWith(+,-))).
mode(loanAmount, avg, (hasLoan(-,+,true),avgSumOfInc(+,-))).
mode(loanAmount, maxMod, (hasLoan(-,+,true),freq(+,-))).
mode(loanAmount, avg, (districtClientLoan(-,-,+),avgSalary(+,-))).
mode(loanAmount, avg, (districtClientLoan(-,-,+),ratUrbInhab(+,-))).

mode(loanStatus, cnt, hasLoan(-,+,true)).
mode(loanStatus, cnt, districtClientLoan(-,-,+)).
mode(loanStatus, none, loanAmount(+,-)).
mode(loanStatus, none, monthlyPayments(+,-)).
mode(loanStatus, maxMod, (clientLoan(-,+,true),gender(+,-))).
mode(loanStatus, avg, (clientLoan(-,+,true),age(+,-))).
mode(loanStatus, avg, (hasLoan(-,+,true),avgSumOfW(+,-))).
mode(loanStatus, avg, (hasLoan(-,+,true),stdMonthInc(+,-))).
mode(loanStatus, avg, (hasLoan(-,+,true),stdMonthW(+,-))).
mode(loanStatus, avg, (hasLoan(-,+,true),avgNrWith(+,-))).
mode(loanStatus, avg, (hasLoan(-,+,true),avgSumOfInc(+,-))).
mode(loanStatus, maxMod, (hasLoan(-,+,true),freq(+,-))).
mode(loanStatus, avg, (districtClientLoan(-,-,+),avgSalary(+,-))).
mode(loanStatus, avg, (districtClientLoan(-,-,+),ratUrbInhab(+,-))).

mode(monthlyPayments, cnt, hasLoan(-,+,true)).
mode(monthlyPayments, cnt, districtClientLoan(-,-,+)).
mode(monthlyPayments, none, loanAmount(+,-)).
mode(monthlyPayments, none, loanStatus(+,-)).
mode(monthlyPayments, maxMod, (clientLoan(-,+,true),gender(+,-))).
mode(monthlyPayments, avg, (clientLoan(-,+,true),age(+,-))).
mode(monthlyPayments, avg, (hasLoan(-,+,true),avgSumOfW(+,-))).
mode(monthlyPayments, avg, (hasLoan(-,+,true),stdMonthInc(+,-))).
mode(monthlyPayments, avg, (hasLoan(-,+,true),stdMonthW(+,-))).
mode(monthlyPayments, avg, (hasLoan(-,+,true),avgNrWith(+,-))).
mode(monthlyPayments, avg, (hasLoan(-,+,true),avgSumOfInc(+,-))).
mode(monthlyPayments, maxMod, (hasLoan(-,+,true),freq(+,-))).
mode(monthlyPayments, avg, (districtClientLoan(-,-,+),avgSalary(+,-))).
mode(monthlyPayments, avg, (districtClientLoan(-,-,+),ratUrbInhab(+,-))).

mode(avgSumOfW, none, stdMonthInc(+,-)).
mode(avgSumOfW, none, stdMonthW(+,-)).
mode(avgSumOfW, none, avgNrWith(+,-)).
mode(avgSumOfW, none, avgSumOfInc(+,-)).
mode(avgSumOfW, none, freq(+,-)).
mode(avgSumOfW, avg, (districtClientAccount(-,-,+),avgSalary(+,-))).
mode(avgSumOfW, avg, (districtClientAccount(-,-,+),ratUrbInhab(+,-))).
mode(avgSumOfW, maxMod, (hasAccount(-,+,true),gender(+,-))).
mode(avgSumOfW, avg, (hasAccount(-,+,true),age(+,-))).
mode(avgSumOfW, avg, (hasLoan(+,-,true),loanAmount(+,-))).
mode(avgSumOfW, maxMod, (hasLoan(+,-,true),loanStatus(+,-))).
mode(avgSumOfW, avg, (hasLoan(+,-,true),monthlyPayments(+,-))).

mode(stdMonthInc, none, avgSumOfW(+,-)).
mode(stdMonthInc, none, stdMonthW(+,-)).
mode(stdMonthInc, none, avgNrWith(+,-)).
mode(stdMonthInc, none, avgSumOfInc(+,-)).
mode(stdMonthInc, none, freq(+,-)).
mode(stdMonthInc, avg, (districtClientAccount(-,-,+),avgSalary(+,-))).
mode(stdMonthInc, avg, (districtClientAccount(-,-,+),ratUrbInhab(+,-))).
mode(stdMonthInc, maxMod, (hasAccount(-,+,true),gender(+,-))).
mode(stdMonthInc, avg, (hasAccount(-,+,true),age(+,-))).
mode(stdMonthInc, avg, (hasLoan(+,-,true),loanAmount(+,-))).
mode(stdMonthInc, maxMod, (hasLoan(+,-,true),loanStatus(+,-))).
mode(stdMonthInc, avg, (hasLoan(+,-,true),monthlyPayments(+,-))).

mode(stdMonthW, none, avgSumOfW(+,-)).
mode(stdMonthW, none, stdMonthInc(+,-)).
mode(stdMonthW, none, avgNrWith(+,-)).
mode(stdMonthW, none, avgSumOfInc(+,-)).
mode(stdMonthW, none, freq(+,-)).
mode(stdMonthW, avg, (districtClientAccount(-,-,+),avgSalary(+,-))).
mode(stdMonthW, avg, (districtClientAccount(-,-,+),ratUrbInhab(+,-))).
mode(stdMonthW, maxMod, (hasAccount(-,+,true),gender(+,-))).
mode(stdMonthW, avg, (hasAccount(-,+,true),age(+,-))).
mode(stdMonthW, avg, (hasLoan(+,-,true),loanAmount(+,-))).
mode(stdMonthW, maxMod, (hasLoan(+,-,true),loanStatus(+,-))).
mode(stdMonthW, avg, (hasLoan(+,-,true),monthlyPayments(+,-))).

mode(avgNrWith, none, avgSumOfW(+,-)).
mode(avgNrWith, none, stdMonthInc(+,-)).
mode(avgNrWith, none, stdMonthW(+,-)).
mode(avgNrWith, none, avgSumOfInc(+,-)).
mode(avgNrWith, none, freq(+,-)).
mode(avgNrWith, avg, (districtClientAccount(-,-,+),avgSalary(+,-))).
mode(avgNrWith, avg, (districtClientAccount(-,-,+),ratUrbInhab(+,-))).
mode(avgNrWith, maxMod, (hasAccount(-,+,true),gender(+,-))).
mode(avgNrWith, avg, (hasAccount(-,+,true),age(+,-))).
mode(avgNrWith, avg, (hasLoan(+,-,true),loanAmount(+,-))).
mode(avgNrWith, maxMod, (hasLoan(+,-,true),loanStatus(+,-))).
mode(avgNrWith, avg, (hasLoan(+,-,true),monthlyPayments(+,-))).

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
mode(avgSumOfInc, avg, (hasLoan(+,-,true),monthlyPayments(+,-))).

mode(freq, none, avgSumOfW(+,-)).
mode(freq, none, stdMonthInc(+,-)).
mode(freq, none, stdMonthW(+,-)).
mode(freq, none, avgNrWith(+,-)).
mode(freq, none, avgSumOfInc(+,-)).
mode(freq, avg, (districtClientAccount(-,-,+),avgSalary(+,-))).
mode(freq, avg, (districtClientAccount(-,-,+),ratUrbInhab(+,-))).
mode(freq, maxMod, (hasAccount(-,+,true),gender(+,-))).
mode(freq, avg, (hasAccount(-,+,true),age(+,-))).
mode(freq, avg, (hasLoan(+,-,true),loanAmount(+,-))).
mode(freq, maxMod, (hasLoan(+,-,true),loanStatus(+,-))).
mode(freq, avg, (hasLoan(+,-,true),monthlyPayments(+,-))).

mode(clientDistrict, none, age(+,-)).
mode(clientDistrict, none, gender(+,-)).
mode(clientDistrict, none, avgSalary(+,-)).
mode(clientDistrict, none, ratUrbInhab(+,-)).
mode(clientDistrict, avg, (hasAccount(+,-,true),avgSumOfW(+,-))).
mode(clientDistrict, avg, (hasAccount(+,-,true),stdMonthInc(+,-))).
mode(clientDistrict, avg, (hasAccount(+,-,true),stdMonthW(+,-))).
mode(clientDistrict, avg, (hasAccount(+,-,true),avgNrWith(+,-))).
mode(clientDistrict, avg, (hasAccount(+,-,true),avgSumOfInc(+,-))).
mode(clientDistrict, maxMod, (hasAccount(+,-,true),freq(+,-))).
mode(clientDistrict, avg, (clientLoan(+,-,true),loanAmount(+,-))).
mode(clientDistrict, maxMod, (clientLoan(+,-,true),loanStatus(+,-))).
mode(clientDistrict, avg, (clientLoan(+,-,true),monthlyPayments(+,-))).

mode(hasAccount, none, age(+,-)).
mode(hasAccount, none, gender(+,-)).
mode(hasAccount, cnt, clientDistrict(+,-,true)).
mode(hasAccount, cnt, clientLoan(+,-,true)).
mode(hasAccount, avg, (clientDistrict(+,-,true),avgSalary(+,-))).
mode(hasAccount, avg, (clientDistrict(+,-,true),ratUrbInhab(+,-))).
mode(hasAccount, none, avgSumOfW(+,-)).
mode(hasAccount, none, stdMonthInc(+,-)).
mode(hasAccount, none, stdMonthW(+,-)).
mode(hasAccount, none, avgNrWith(+,-)).
mode(hasAccount, none, avgSumOfInc(+,-)).
mode(hasAccount, none, freq(+,-)).
mode(hasAccount, avg, (clientLoan(+,-,true),loanAmount(+,-))).
mode(hasAccount, maxMod, (clientLoan(+,-,true),loanStatus(+,-))).
mode(hasAccount, avg, (clientLoan(+,-,true),monthlyPayments(+,-))).

mode(clientLoan, cnt, clientDistrict(+,-,true)).
mode(clientLoan, none, age(+,-)).
mode(clientLoan, none, gender(+,-)).
mode(clientLoan, avg, (clientDistrict(+,-,true),avgSalary(+,-))).
mode(clientLoan, avg, (clientDistrict(+,-,true),ratUrbInhab(+,-))).
mode(clientLoan, avg, (hasAccount(+,-,true),avgSumOfW(+,-))).
mode(clientLoan, avg, (hasAccount(+,-,true),stdMonthInc(+,-))).
mode(clientLoan, avg, (hasAccount(+,-,true),stdMonthW(+,-))).
mode(clientLoan, avg, (hasAccount(+,-,true),avgNrWith(+,-))).
mode(clientLoan, avg, (hasAccount(+,-,true),avgSumOfInc(+,-))).
mode(clientLoan, maxMod, (hasAccount(+,-,true),freq(+,-))).
mode(clientLoan, none, loanAmount(+,-)).
mode(clientLoan, none, loanStatus(+,-)).
mode(clientLoan, none, monthlyPayments(+,-)).

mode(hasLoan, cnt, districtClientAccount(-,-,+)).
mode(hasLoan, cnt, hasAccount(-,+,true)).
mode(hasLoan, none, avgSumOfInc(+,-)).
mode(hasLoan, none, avgSumOfW(+,-)).
mode(hasLoan, none, stdMonthInc(+,-)).
mode(hasLoan, none, stdMonthW(+,-)).
mode(hasLoan, none, avgNrWith(+,-)).
mode(hasLoan, none, freq(+,-)).
mode(hasLoan, avg, (districtClientAccount(-,-,+),avgSalary(+,-))).
mode(hasLoan, avg, (districtClientAccount(-,-,+),ratUrbInhab(+,-))).
mode(hasLoan, maxMod, (hasAccount(-,+,true),gender(+,-))).
mode(hasLoan, avg, (hasAccount(-,+,true),age(+,-))).
mode(hasLoan, none, loanAmount(+,-)).
mode(hasLoan, none, loanStatus(+,-)).
mode(hasLoan, none, monthlyPayments(+,-)).

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
learn(hasLoan, 3, 3, discrete).
learn(clientDistrict, 3, 3, discrete).
learn(hasAccount, 3, 3, discrete).
learn(clientLoan, 3, 3, discrete).
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
  
%Facts
''',

university = '''
:- discontiguous course/1.
:- discontiguous professor/1.
:- discontiguous student/1.
:- discontiguous nrhours/2.
:- discontiguous difficulty/2.
:- discontiguous ability/2.
:- discontiguous intelligence/2.
:- discontiguous grade/3.
:- discontiguous satisfaction/3.
:- discontiguous takes/3.
:- discontiguous friend/3.
:- discontiguous teaches/3.

:- dynamic course/1.
:- dynamic professor/1.
:- dynamic student/1.
:- dynamic nrhours/2.
:- dynamic difficulty/2.
:- dynamic ability/2.
:- dynamic intelligence/2.
:- dynamic grade/3.
:- dynamic satisfaction/3.
:- dynamic takes/3.
:- dynamic friend/3.
:- dynamic teaches/3.


%Relational Lookahead
studentCourseProfessor(S,C,P) :- takes(S,C,true), teaches(P,C,true).

%Types
base(course(c)).
base(professor(p)).
base(student(s)).
base(studentCourseProfessor(s,c,p)).
base(nrhours(c,nr)).
base(difficulty(c,di)).
base(ability(p,ab)).
base(intelligence(s,in)).
base(grade(s,c,gr)).
base(satisfaction(s,c,sa)).
base(takes(s,c,x1)).
base(friend(s,s,x2)).
base(teaches(p,c,x3)).

%Modes
mode(nrhours, none, difficulty(+,-)).
mode(nrhours, avg, (takes(-,+,true), intelligence(+,-))).
mode(nrhours, maxMod, (takes(-,+,true), grade(+,+,-))).
mode(nrhours, maxMod, (takes(-,+,true), satisfaction(+,+,-))).
mode(nrhours, avg, (teaches(-,+,true), ability(+,-))).

mode(difficulty, none, nrhours(+,-)).
mode(difficulty, avg, (takes(-,+,true), intelligence(+,-))).
mode(difficulty, maxMod, (takes(-,+,true), grade(+,+,-))).
mode(difficulty, maxMod, (takes(-,+,true), satisfaction(+,+,-))).
mode(difficulty, avg, (teaches(-,+,true), ability(+,-))).

mode(ability, avg, (teaches(+,-,true), nrhours(+,-))).
mode(ability, maxMod, (teaches(+,-,true), difficulty(+,-))).
%mode(ability, avg, (studentCourseProfessor(-,-,+), intelligence(+,-))).
%mode(ability, maxMod, (studentCourseProfessor(-,-,+), grade(+,+,-))).
%mode(ability, maxMod, (studentCourseProfessor(-,-,+), satisfaction(+,+,-))).

mode(intelligence, maxMod, grade(+,-,-)).
mode(intelligence, maxMod, satisfaction(+,-,-)).
mode(intelligence, avg, (takes(+,-,true), nrhours(+,-))).
mode(intelligence, maxMod, (takes(+,-,true), difficulty(+,-))).
%mode(intelligence, avg, (studentCourseProfessor(+,-,-), ability(+,-))).
%mode(intelligence, maxMod, (friend(+,-,true), grade(+,-,-))).
%mode(intelligence, maxMod, (friend(+,-,true), satisfaction(+,-,-))).
%mode(intelligence, maxMod, (friend(+,-,true), intelligence(+,-))).

mode(grade, none, intelligence(+,-)).
mode(grade, maxMod, satisfaction(+,+,-)).
mode(grade, avg, (takes(+,+,true), nrhours(+,-))).
mode(grade, maxMod, (takes(+,+,true), difficulty(+,-))).
%mode(grade, avg, (studentCourseProfessor(+,+,-), ability(+,-))).
%mode(grade, maxMod, (friend(+,-,true), grade(+,+,-))).
%mode(grade, maxMod, (friend(+,-,true), satisfaction(+,+,-))).
%mode(grade, maxMod, (friend(+,-,true), intelligence(+,-))).

mode(takes, none, intelligence(+,-)).
mode(takes, none, satisfaction(+,+,-)).
mode(takes, none, difficulty(-,+)).
mode(takes, none, nrhours(-,+)).

mode(satisfaction, none, intelligence(+,-)).
mode(satisfaction, maxMod, grade(+,+,-)).
mode(satisfaction, avg, (takes(+,+,true), nrhours(+,-))).
mode(satisfaction, maxMod, (takes(+,+,true), difficulty(+,-))).
%mode(satisfaction, avg, (studentCourseProfessor(+,+,-), ability(+,-))).
%mode(satisfaction, maxMod, (friend(+,-,true), grade(+,+,-))).
%mode(satisfaction, maxMod, (friend(+,-,true), satisfaction(+,+,-))).
%mode(satisfaction, maxMod, (friend(+,-,true), intelligence(+,-))).

mode(friend, cnt, takes(+,-,true)).
mode(friend, maxMod, grade(+,-,-)).
mode(friend, maxMod, satisfaction(+,-,-)).
mode(friend, none, intelligence(+,-)).

%Aggregations
agg(none).
agg(avg).
agg(maxMod).
agg(cnt).
  
%Threshold
thres(nrhours, 2, continuous, []).
thres(difficulty, 2, discrete, [medium,hard,easy]).
thres(ability, 2, continuous, []).
thres(intelligence, 2, continuous, []).
thres(grade, 3, discrete, [high,low,mid]).
thres(satisfaction, 3, discrete, [mid,low,high]).
thres(friend, 3, discrete, [true,false]).
thres(takes, 3, continuous, []).
thres(teaches, 3, continuous, []).

%Target
learn(nrhours, 2, 2, continuous).
learn(difficulty, 2, 2, discrete).
learn(ability, 2, 2, continuous).
learn(intelligence, 2, 2, continuous).
learn(grade, 3, 3, discrete).
learn(satisfaction, 3, 3, discrete).
learn(friend, 3, 3, discrete).
learn(takes, 3, 3, discrete).
learn(teaches, 3, 3, discrete).

%Facts
''',
Hepatitis_std = '''
:- discontiguous bio/1.
:- discontiguous dispat/1.
:- discontiguous indis/1.
:- discontiguous inf/1.
:- discontiguous rel11/3.
:- discontiguous rel12/3.
:- discontiguous rel13/3.
:- discontiguous fibros/2.
:- discontiguous activity/2.
:- discontiguous sex/2.
:- discontiguous age/2.
:- discontiguous type/2.
:- discontiguous got/2.
:- discontiguous gpt/2.
:- discontiguous alb/2.
:- discontiguous tbil/2.
:- discontiguous dbil/2.
:- discontiguous che/2.
:- discontiguous ttt/2.
:- discontiguous ztt/2.
:- discontiguous tcho/2.
:- discontiguous tp/2.
:- discontiguous dur/2.

:- dynamic bio/1.
:- dynamic dispat/1.
:- dynamic indis/1.
:- dynamic inf/1.
:- dynamic rel11/3.
:- dynamic rel12/3.
:- dynamic rel13/3.
:- dynamic fibros/2.
:- dynamic activity/2.
:- dynamic sex/2.
:- dynamic age/2.
:- dynamic type/2.
:- dynamic got/2.
:- dynamic gpt/2.
:- dynamic alb/2.
:- dynamic tbil/2.
:- dynamic dbil/2.
:- dynamic che/2.
:- dynamic ttt/2.
:- dynamic ztt/2.
:- dynamic tcho/2.
:- dynamic tp/2.
:- dynamic dur/2.

%Relational Lookahead
bioDispatInf(B,D,J) :- rel11(B,D,true), rel13(J,D,true).
infDispatIndis(J,D,I) :- rel13(J,D,true), rel12(I,D,true).
bioDispatIndis(B,D,I) :- rel11(B,D,true), rel12(I,D,true).

%Types
base(bio(b)).
base(dispat(d)).
base(indis(i)).
base(inf(j)).
base(rel11(b,d,x1)).
base(rel13(j,d,x2)).
base(rel12(i,d,x3)).
base(bioDispatInf(b,d,j)).
base(infDispatIndis(j,d,i)).
base(bioDispatIndis(b,d,i)).
base(fibros(b,fi)).
base(activity(b,ac)).
base(sex(d,se)).
base(age(d,ag)).
base(type(d,ty)).
base(got(i,go)).
base(gpt(i,gp)).
base(alb(i,al)).
base(tbil(i,tb)).
base(dbil(i,db)).
base(che(i,ch)).
base(ttt(i,tt)).
base(ztt(i,zt)).
base(tcho(i,tc)).
base(tp(i,tp)).
base(dur(j,du)).
  
%Modes
mode(fibros, none, activity(+,-)).
mode(fibros, maxMod, (rel11(+,-,true), sex(+,-))).
mode(fibros, avg, (rel11(+,-,true), age(+,-))).
mode(fibros, maxMod, (rel11(+,-,true), type(+,-))).
mode(fibros, avg, (bioDispatIndis(+,-,-), got(+,-))).
mode(fibros, avg, (bioDispatIndis(+,-,-), gpt(+,-))).
mode(fibros, maxMod, (bioDispatIndis(+,-,-), alb(+,-))).
mode(fibros, maxMod, (bioDispatIndis(+,-,-), tbil(+,-))).
mode(fibros, maxMod, (bioDispatIndis(+,-,-), dbil(+,-))).
mode(fibros, avg, (bioDispatIndis(+,-,-), che(+,-))).
mode(fibros, avg, (bioDispatIndis(+,-,-), ttt(+,-))).
mode(fibros, avg, (bioDispatIndis(+,-,-), ztt(+,-))).
mode(fibros, avg, (bioDispatIndis(+,-,-), tcho(+,-))).
mode(fibros, avg, (bioDispatIndis(+,-,-), tp(+,-))).
mode(fibros, avg, (bioDispatInf(+,-,-), dur(+,-))).

mode(activity, none, fibros(+,-)).
mode(activity, maxMod, (rel11(+,-,true), sex(+,-))).
mode(activity, avg, (rel11(+,-,true), age(+,-))).
mode(activity, maxMod, (rel11(+,-,true), type(+,-))).
mode(activity, avg, (bioDispatIndis(+,-,-), got(+,-))).
mode(activity, avg, (bioDispatIndis(+,-,-), gpt(+,-))).
mode(activity, maxMod, (bioDispatIndis(+,-,-), alb(+,-))).
mode(activity, maxMod, (bioDispatIndis(+,-,-), tbil(+,-))).
mode(activity, maxMod, (bioDispatIndis(+,-,-), dbil(+,-))).
mode(activity, avg, (bioDispatIndis(+,-,-), che(+,-))).
mode(activity, avg, (bioDispatIndis(+,-,-), ttt(+,-))).
mode(activity, avg, (bioDispatIndis(+,-,-), ztt(+,-))).
mode(activity, avg, (bioDispatIndis(+,-,-), tcho(+,-))).
mode(activity, avg, (bioDispatIndis(+,-,-), tp(+,-))).
mode(activity, avg, (bioDispatInf(+,-,-), dur(+,-))).

mode(sex, avg, (rel11(-,+,true),fibros(+,-))).
mode(sex, avg, (rel11(-,+,true),activity(+,-))).
mode(sex, none, age(+,-)).
mode(sex, none, type(+,-)).
mode(sex, avg, (rel12(-,+,true), got(+,-))).
mode(sex, avg, (rel12(-,+,true), gpt(+,-))).
mode(sex, maxMod, (rel12(-,+,true), alb(+,-))).
mode(sex, maxMod, (rel12(-,+,true), tbil(+,-))).
mode(sex, maxMod, (rel12(-,+,true), dbil(+,-))).
mode(sex, avg, (rel12(-,+,true), che(+,-))).
mode(sex, avg, (rel12(-,+,true), ttt(+,-))).
mode(sex, avg, (rel12(-,+,true), ztt(+,-))).
mode(sex, avg, (rel12(-,+,true), tcho(+,-))).
mode(sex, avg, (rel12(-,+,true), tp(+,-))).
mode(sex, avg, (rel13(-,+,true), dur(+,-))).

mode(age, avg, (rel11(-,+,true),fibros(+,-))).
mode(age, avg, (rel11(-,+,true),activity(+,-))).
mode(age, none, sex(+,-)).
mode(age, none, type(+,-)).
mode(age, avg, (rel12(-,+,true), got(+,-))).
mode(age, avg, (rel12(-,+,true), gpt(+,-))).
mode(age, maxMod, (rel12(-,+,true), alb(+,-))).
mode(age, maxMod, (rel12(-,+,true), tbil(+,-))).
mode(age, maxMod, (rel12(-,+,true), dbil(+,-))).
mode(age, avg, (rel12(-,+,true), che(+,-))).
mode(age, avg, (rel12(-,+,true), ttt(+,-))).
mode(age, avg, (rel12(-,+,true), ztt(+,-))).
mode(age, avg, (rel12(-,+,true), tcho(+,-))).
mode(age, avg, (rel12(-,+,true), tp(+,-))).
mode(age, avg, (rel13(-,+,true), dur(+,-))).

mode(type, avg, (rel11(-,+,true),fibros(+,-))).
mode(type, avg, (rel11(-,+,true),activity(+,-))).
mode(type, none, sex(+,-)).
mode(type, none, age(+,-)).
mode(type, avg, (rel12(-,+,true), got(+,-))).
mode(type, avg, (rel12(-,+,true), gpt(+,-))).
mode(type, maxMod, (rel12(-,+,true), alb(+,-))).
mode(type, maxMod, (rel12(-,+,true), tbil(+,-))).
mode(type, maxMod, (rel12(-,+,true), dbil(+,-))).
mode(type, avg, (rel12(-,+,true), che(+,-))).
mode(type, avg, (rel12(-,+,true), ttt(+,-))).
mode(type, avg, (rel12(-,+,true), ztt(+,-))).
mode(type, avg, (rel12(-,+,true), tcho(+,-))).
mode(type, avg, (rel12(-,+,true), tp(+,-))).
mode(type, avg, (rel13(-,+,true), dur(+,-))).

mode(got, avg, (bioDispatIndis(-,-,+),fibros(+,-))).
mode(got, avg, (bioDispatIndis(-,-,+),activity(+,-))).
mode(got, maxMod, (rel12(+,-,true), sex(+,-))).
mode(got, avg, (rel12(+,-,true), age(+,-))).
mode(got, maxMod, (rel12(+,-,true), type(+,-))).
mode(got, none, gpt(+,-)).
mode(got, none, alb(+,-)).
mode(got, none, tbil(+,-)).
mode(got, none, dbil(+,-)).
mode(got, none, che(+,-)).
mode(got, none, ttt(+,-)).
mode(got, none, ztt(+,-)).
mode(got, none, tcho(+,-)).
mode(got, none, tp(+,-)).
mode(got, avg, (infDispatIndis(-,-,+), dur(+,-))).

mode(gpt, avg, (bioDispatIndis(-,-,+),fibros(+,-))).
mode(gpt, avg, (bioDispatIndis(-,-,+),activity(+,-))).
mode(gpt, maxMod, (rel12(+,-,true), sex(+,-))).
mode(gpt, avg, (rel12(+,-,true), age(+,-))).
mode(gpt, maxMod, (rel12(+,-,true), type(+,-))).
mode(gpt, none, got(+,-)).
mode(gpt, none, alb(+,-)).
mode(gpt, none, tbil(+,-)).
mode(gpt, none, dbil(+,-)).
mode(gpt, none, che(+,-)).
mode(gpt, none, ttt(+,-)).
mode(gpt, none, ztt(+,-)).
mode(gpt, none, tcho(+,-)).
mode(gpt, none, tp(+,-)).
mode(gpt, avg, (infDispatIndis(-,-,+), dur(+,-))).

mode(alb, avg, (bioDispatIndis(-,-,+),fibros(+,-))).
mode(alb, avg, (bioDispatIndis(-,-,+),activity(+,-))).
mode(alb, maxMod, (rel12(+,-,true), sex(+,-))).
mode(alb, avg, (rel12(+,-,true), age(+,-))).
mode(alb, maxMod, (rel12(+,-,true), type(+,-))).
mode(alb, none, got(+,-)).
mode(alb, none, gpt(+,-)).
mode(alb, none, tbil(+,-)).
mode(alb, none, dbil(+,-)).
mode(alb, none, che(+,-)).
mode(alb, none, ttt(+,-)).
mode(alb, none, ztt(+,-)).
mode(alb, none, tcho(+,-)).
mode(alb, none, tp(+,-)).
mode(alb, avg, (infDispatIndis(-,-,+), dur(+,-))).

mode(tbil, avg, (bioDispatIndis(-,-,+),fibros(+,-))).
mode(tbil, avg, (bioDispatIndis(-,-,+),activity(+,-))).
mode(tbil, maxMod, (rel12(+,-,true), sex(+,-))).
mode(tbil, avg, (rel12(+,-,true), age(+,-))).
mode(tbil, maxMod, (rel12(+,-,true), type(+,-))).
mode(tbil, none, got(+,-)).
mode(tbil, none, gpt(+,-)).
mode(tbil, none, alb(+,-)).
mode(tbil, none, dbil(+,-)).
mode(tbil, none, che(+,-)).
mode(tbil, none, ttt(+,-)).
mode(tbil, none, ztt(+,-)).
mode(tbil, none, tcho(+,-)).
mode(tbil, none, tp(+,-)).
mode(tbil, avg, (infDispatIndis(-,-,+), dur(+,-))).

mode(dbil, avg, (bioDispatIndis(-,-,+),fibros(+,-))).
mode(dbil, avg, (bioDispatIndis(-,-,+),activity(+,-))).
mode(dbil, maxMod, (rel12(+,-,true), sex(+,-))).
mode(dbil, avg, (rel12(+,-,true), age(+,-))).
mode(dbil, maxMod, (rel12(+,-,true), type(+,-))).
mode(dbil, none, got(+,-)).
mode(dbil, none, gpt(+,-)).
mode(dbil, none, alb(+,-)).
mode(dbil, none, tbil(+,-)).
mode(dbil, none, che(+,-)).
mode(dbil, none, ttt(+,-)).
mode(dbil, none, ztt(+,-)).
mode(dbil, none, tcho(+,-)).
mode(dbil, none, tp(+,-)).
mode(dbil, avg, (infDispatIndis(-,-,+), dur(+,-))).

mode(che, avg, (bioDispatIndis(-,-,+),fibros(+,-))).
mode(che, avg, (bioDispatIndis(-,-,+),activity(+,-))).
mode(che, maxMod, (rel12(+,-,true), sex(+,-))).
mode(che, avg, (rel12(+,-,true), age(+,-))).
mode(che, maxMod, (rel12(+,-,true), type(+,-))).
mode(che, none, got(+,-)).
mode(che, none, gpt(+,-)).
mode(che, none, alb(+,-)).
mode(che, none, tbil(+,-)).
mode(che, none, dbil(+,-)).
mode(che, none, ttt(+,-)).
mode(che, none, ztt(+,-)).
mode(che, none, tcho(+,-)).
mode(che, none, tp(+,-)).
mode(che, avg, (infDispatIndis(-,-,+), dur(+,-))).

mode(ttt, avg, (bioDispatIndis(-,-,+),fibros(+,-))).
mode(ttt, avg, (bioDispatIndis(-,-,+),activity(+,-))).
mode(ttt, maxMod, (rel12(+,-,true), sex(+,-))).
mode(ttt, avg, (rel12(+,-,true), age(+,-))).
mode(ttt, maxMod, (rel12(+,-,true), type(+,-))).
mode(ttt, none, got(+,-)).
mode(ttt, none, gpt(+,-)).
mode(ttt, none, alb(+,-)).
mode(ttt, none, tbil(+,-)).
mode(ttt, none, dbil(+,-)).
mode(ttt, none, che(+,-)).
mode(ttt, none, ztt(+,-)).
mode(ttt, none, tcho(+,-)).
mode(ttt, none, tp(+,-)).
mode(ttt, avg, (infDispatIndis(-,-,+), dur(+,-))).

mode(ztt, avg, (bioDispatIndis(-,-,+),fibros(+,-))).
mode(ztt, avg, (bioDispatIndis(-,-,+),activity(+,-))).
mode(ztt, maxMod, (rel12(+,-,true), sex(+,-))).
mode(ztt, avg, (rel12(+,-,true), age(+,-))).
mode(ztt, maxMod, (rel12(+,-,true), type(+,-))).
mode(ztt, none, got(+,-)).
mode(ztt, none, gpt(+,-)).
mode(ztt, none, alb(+,-)).
mode(ztt, none, tbil(+,-)).
mode(ztt, none, dbil(+,-)).
mode(ztt, none, che(+,-)).
mode(ztt, none, ttt(+,-)).
mode(ztt, none, tcho(+,-)).
mode(ztt, none, tp(+,-)).
mode(ztt, avg, (infDispatIndis(-,-,+), dur(+,-))).

mode(tcho, avg, (bioDispatIndis(-,-,+),fibros(+,-))).
mode(tcho, avg, (bioDispatIndis(-,-,+),activity(+,-))).
mode(tcho, maxMod, (rel12(+,-,true), sex(+,-))).
mode(tcho, avg, (rel12(+,-,true), age(+,-))).
mode(tcho, maxMod, (rel12(+,-,true), type(+,-))).
mode(tcho, none, got(+,-)).
mode(tcho, none, gpt(+,-)).
mode(tcho, none, alb(+,-)).
mode(tcho, none, tbil(+,-)).
mode(tcho, none, dbil(+,-)).
mode(tcho, none, che(+,-)).
mode(tcho, none, ttt(+,-)).
mode(tcho, none, ztt(+,-)).
mode(tcho, none, tp(+,-)).
mode(tcho, avg, (infDispatIndis(-,-,+), dur(+,-))).

mode(tp, avg, (bioDispatIndis(-,-,+),fibros(+,-))).
mode(tp, avg, (bioDispatIndis(-,-,+),activity(+,-))).
mode(tp, maxMod, (rel12(+,-,true), sex(+,-))).
mode(tp, avg, (rel12(+,-,true), age(+,-))).
mode(tp, maxMod, (rel12(+,-,true), type(+,-))).
mode(tp, none, got(+,-)).
mode(tp, none, gpt(+,-)).
mode(tp, none, alb(+,-)).
mode(tp, none, tbil(+,-)).
mode(tp, none, dbil(+,-)).
mode(tp, none, che(+,-)).
mode(tp, none, ttt(+,-)).
mode(tp, none, ztt(+,-)).
mode(tp, none, tcho(+,-)).
mode(tp, avg, (infDispatIndis(-,-,+), dur(+,-))).

mode(dur, avg, (bioDispatInf(-,-,+),fibros(+,-))).
mode(dur, avg, (bioDispatInf(-,-,+),activity(+,-))).
mode(dur, maxMod, (rel13(+,-,true), sex(+,-))).
mode(dur, avg, (rel13(+,-,true), age(+,-))).
mode(dur, maxMod, (rel13(+,-,true), type(+,-))).
mode(dur, avg, (infDispatIndis(+,-,-), got(+,-))).
mode(dur, avg, (infDispatIndis(+,-,-), gpt(+,-))).
mode(dur, maxMod, (infDispatIndis(+,-,-), alb(+,-))).
mode(dur, maxMod, (infDispatIndis(+,-,-), tbil(+,-))).
mode(dur, maxMod, (infDispatIndis(+,-,-), dbil(+,-))).
mode(dur, avg, (infDispatIndis(+,-,-), che(+,-))).
mode(dur, avg, (infDispatIndis(+,-,-), ttt(+,-))).
mode(dur, avg, (infDispatIndis(+,-,-), ztt(+,-))).
mode(dur, avg, (infDispatIndis(+,-,-), tcho(+,-))).
mode(dur, avg, (infDispatIndis(+,-,-), tp(+,-))).

%Aggregations
agg(none).
agg(avg).
agg(maxMod).
agg(cnt).
  
%Threshold
thres(rel11, 3, continuous, []).
thres(rel12, 3, continuous, []).
thres(rel13, 3, continuous, []).
thres(bioDispatInf, 3, continuous, []).
thres(infDispatIndis, 3, continuous, []).
thres(bioDispatIndis, 3, continuous, []).
thres(fibros, 2, continuous, []).
thres(activity, 2, continuous, []).
thres(sex, 2, discrete, [male, female]).
thres(age, 2, continuous, []).
thres(type, 2, discrete, [negative, positive]).
thres(got, 2, continuous, []).
thres(gpt, 2, continuous, []).
thres(alb, 2, discrete, [low, high]).
thres(tbil, 2, discrete, [low, high]).
thres(dbil, 2, discrete, [low, high]).
thres(che, 2, continuous, []).
thres(ttt, 2, continuous, []).
thres(ztt, 2, continuous, []).
thres(tcho, 2, continuous, []).
thres(tp, 2, continuous, []).
thres(dur, 2, continuous, []).

%Target
learn(rel11, 3, 3, discrete).
learn(rel12, 3, 3, discrete).
learn(rel13, 3, 3, discrete).
learn(fibros, 2, 2, continuous).
learn(activity, 2, 2, continuous).
learn(sex, 2, 2, discrete).
learn(age, 2, 2, continuous).
learn(type, 2, 2, discrete).
learn(got, 2, 2, continuous).
learn(gpt, 2, 2, continuous).
learn(alb, 2, 2, discrete).
learn(tbil, 2, 2, discrete).
learn(dbil, 2, 2, discrete).
learn(che, 2, 2, continuous).
learn(ttt, 2, 2, continuous).
learn(ztt, 2, 2, continuous).
learn(tcho, 2, 2, continuous).
learn(tp, 2, 2, continuous).
learn(dur, 2, 2, continuous).
  
%Facts
'''
)


DECLARATIVE_BIAS_ORDERED_RELATION_PRED = dict(
    financial = '''
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
  
%Modes
%mode(avgSalary, cnt, districtClientAccount(+,-,-)).
%mode(avgSalary, none, ratUrbInhab(+,-)).
%mode(avgSalary, maxMod, (clientDistrict(-,+,true),gender(+,-))).
%mode(avgSalary, avg, (clientDistrict(-,+,true),age(+,-))).
%mode(avgSalary, avg, (districtClientAccount(+,-,-),avgSumOfW(+,-))).
%mode(avgSalary, avg, (districtClientAccount(+,-,-),stdMonthInc(+,-))).
%mode(avgSalary, avg, (districtClientAccount(+,-,-),stdMonthW(+,-))).
%mode(avgSalary, avg, (districtClientAccount(+,-,-),avgNrWith(+,-))).
%mode(avgSalary, avg, (districtClientAccount(+,-,-),avgSumOfInc(+,-))).
%mode(avgSalary, maxMod, (districtClientAccount(+,-,-),freq(+,-))).
%mode(avgSalary, avg, (districtClientLoan(+,-,-),loanAmount(+,-))).
%mode(avgSalary, maxMod, (districtClientLoan(+,-,-),loanStatus(+,-))).
%mode(avgSalary, avg, (districtClientLoan(+,-,-),monthlyPayments(+,-))).

%mode(ratUrbInhab, cnt, districtClientAccount(+,-,-)).
mode(ratUrbInhab, none, avgSalary(+,-)).
%mode(ratUrbInhab, maxMod, (clientDistrict(-,+,true),gender(+,-))).
%mode(ratUrbInhab, avg, (clientDistrict(-,+,true),age(+,-))).
%mode(ratUrbInhab, avg, (districtClientAccount(+,-,-),avgSumOfW(+,-))).
%mode(ratUrbInhab, avg, (districtClientAccount(+,-,-),stdMonthInc(+,-))).
%mode(ratUrbInhab, avg, (districtClientAccount(+,-,-),stdMonthW(+,-))).
%mode(ratUrbInhab, avg, (districtClientAccount(+,-,-),avgNrWith(+,-))).
%mode(ratUrbInhab, avg, (districtClientAccount(+,-,-),avgSumOfInc(+,-))).
%mode(ratUrbInhab, maxMod, (districtClientAccount(+,-,-),freq(+,-))).
%mode(ratUrbInhab, avg, (districtClientLoan(+,-,-),loanAmount(+,-))).
%mode(ratUrbInhab, maxMod, (districtClientLoan(+,-,-),loanStatus(+,-))).
%mode(ratUrbInhab, avg, (districtClientLoan(+,-,-),monthlyPayments(+,-))).

%mode(gender, cnt, clientDistrict(+,-,true)).
%mode(gender, cnt, hasAccount(+,-,true)).
mode(gender, none, age(+,-)).
mode(gender, avg, (clientDistrict(+,-,true),avgSalary(+,-))).
mode(gender, avg, (clientDistrict(+,-,true),ratUrbInhab(+,-))).
%mode(gender, avg, (hasAccount(+,-,true),avgSumOfW(+,-))).
%mode(gender, avg, (hasAccount(+,-,true),stdMonthInc(+,-))).
%mode(gender, avg, (hasAccount(+,-,true),stdMonthW(+,-))).
%mode(gender, avg, (hasAccount(+,-,true),avgNrWith(+,-))).
%mode(gender, avg, (hasAccount(+,-,true),avgSumOfInc(+,-))).
%mode(gender, maxMod, (hasAccount(+,-,true),freq(+,-))).
%mode(gender, avg, (clientLoan(+,-,true),loanAmount(+,-))).
%mode(gender, maxMod, (clientLoan(+,-,true),loanStatus(+,-))).
%mode(gender, avg, (clientLoan(+,-,true),monthlyPayments(+,-))).

%mode(age, cnt, clientDistrict(+,-,true)).
%mode(age, cnt, hasAccount(+,-,true)).
%mode(age, cnt, clientLoan(+,-,true)).
%mode(age, none, gender(+,-)).
mode(age, avg, (clientDistrict(+,-,true),avgSalary(+,-))).
mode(age, avg, (clientDistrict(+,-,true),ratUrbInhab(+,-))).
%mode(age, avg, (hasAccount(+,-,true),avgSumOfW(+,-))).
%mode(age, avg, (hasAccount(+,-,true),stdMonthInc(+,-))).
%mode(age, avg, (hasAccount(+,-,true),stdMonthW(+,-))).
%mode(age, avg, (hasAccount(+,-,true),avgNrWith(+,-))).
%mode(age, avg, (hasAccount(+,-,true),avgSumOfInc(+,-))).
%mode(age, maxMod, (hasAccount(+,-,true),freq(+,-))).
%mode(age, avg, (clientLoan(+,-,true),loanAmount(+,-))).
%mode(age, maxMod, (clientLoan(+,-,true),loanStatus(+,-))).
%mode(age, avg, (clientLoan(+,-,true),monthlyPayments(+,-))).

%mode(loanAmount, cnt, hasLoan(-,+,true)).
%mode(loanAmount, cnt, districtClientLoan(-,-,+)).
%mode(loanAmount, none, loanStatus(+,-)).
%mode(loanAmount, none, monthlyPayments(+,-)).
mode(loanAmount, maxMod, (clientLoan(-,+,true),gender(+,-))).
mode(loanAmount, avg, (clientLoan(-,+,true),age(+,-))).
mode(loanAmount, avg, (hasLoan(-,+,true),avgSumOfW(+,-))).
mode(loanAmount, avg, (hasLoan(-,+,true),stdMonthInc(+,-))).
mode(loanAmount, avg, (hasLoan(-,+,true),stdMonthW(+,-))).
mode(loanAmount, avg, (hasLoan(-,+,true),avgNrWith(+,-))).
mode(loanAmount, avg, (hasLoan(-,+,true),avgSumOfInc(+,-))).
mode(loanAmount, maxMod, (hasLoan(-,+,true),freq(+,-))).
mode(loanAmount, avg, (districtClientLoan(-,-,+),avgSalary(+,-))).
mode(loanAmount, avg, (districtClientLoan(-,-,+),ratUrbInhab(+,-))).

%mode(loanStatus, cnt, hasLoan(-,+,true)).
%mode(loanStatus, cnt, districtClientLoan(-,-,+)).
mode(loanStatus, none, loanAmount(+,-)).
%mode(loanStatus, none, monthlyPayments(+,-)).
mode(loanStatus, maxMod, (clientLoan(-,+,true),gender(+,-))).
mode(loanStatus, avg, (clientLoan(-,+,true),age(+,-))).
mode(loanStatus, avg, (hasLoan(-,+,true),avgSumOfW(+,-))).
mode(loanStatus, avg, (hasLoan(-,+,true),stdMonthInc(+,-))).
mode(loanStatus, avg, (hasLoan(-,+,true),stdMonthW(+,-))).
mode(loanStatus, avg, (hasLoan(-,+,true),avgNrWith(+,-))).
mode(loanStatus, avg, (hasLoan(-,+,true),avgSumOfInc(+,-))).
mode(loanStatus, maxMod, (hasLoan(-,+,true),freq(+,-))).
mode(loanStatus, avg, (districtClientLoan(-,-,+),avgSalary(+,-))).
mode(loanStatus, avg, (districtClientLoan(-,-,+),ratUrbInhab(+,-))).

%mode(monthlyPayments, cnt, hasLoan(-,+,true)).
%mode(monthlyPayments, cnt, districtClientLoan(-,-,+)).
mode(monthlyPayments, none, loanAmount(+,-)).
mode(monthlyPayments, none, loanStatus(+,-)).
mode(monthlyPayments, maxMod, (clientLoan(-,+,true),gender(+,-))).
mode(monthlyPayments, avg, (clientLoan(-,+,true),age(+,-))).
mode(monthlyPayments, avg, (hasLoan(-,+,true),avgSumOfW(+,-))).
mode(monthlyPayments, avg, (hasLoan(-,+,true),stdMonthInc(+,-))).
mode(monthlyPayments, avg, (hasLoan(-,+,true),stdMonthW(+,-))).
mode(monthlyPayments, avg, (hasLoan(-,+,true),avgNrWith(+,-))).
mode(monthlyPayments, avg, (hasLoan(-,+,true),avgSumOfInc(+,-))).
mode(monthlyPayments, maxMod, (hasLoan(-,+,true),freq(+,-))).
mode(monthlyPayments, avg, (districtClientLoan(-,-,+),avgSalary(+,-))).
mode(monthlyPayments, avg, (districtClientLoan(-,-,+),ratUrbInhab(+,-))).

%mode(avgSumOfW, none, stdMonthInc(+,-)).
%mode(avgSumOfW, none, stdMonthW(+,-)).
%mode(avgSumOfW, none, avgNrWith(+,-)).
%mode(avgSumOfW, none, avgSumOfInc(+,-)).
%mode(avgSumOfW, none, freq(+,-)).
mode(avgSumOfW, avg, (districtClientAccount(-,-,+),avgSalary(+,-))).
mode(avgSumOfW, avg, (districtClientAccount(-,-,+),ratUrbInhab(+,-))).
mode(avgSumOfW, maxMod, (hasAccount(-,+,true),gender(+,-))).
mode(avgSumOfW, avg, (hasAccount(-,+,true),age(+,-))).
%mode(avgSumOfW, avg, (hasLoan(+,-,true),loanAmount(+,-))).
%mode(avgSumOfW, maxMod, (hasLoan(+,-,true),loanStatus(+,-))).
%mode(avgSumOfW, avg, (hasLoan(+,-,true),monthlyPayments(+,-))).

mode(stdMonthInc, none, avgSumOfW(+,-)).
%mode(stdMonthInc, none, stdMonthW(+,-)).
mode(stdMonthInc, none, avgNrWith(+,-)).
%mode(stdMonthInc, none, avgSumOfInc(+,-)).
mode(stdMonthInc, none, freq(+,-)).
mode(stdMonthInc, avg, (districtClientAccount(-,-,+),avgSalary(+,-))).
mode(stdMonthInc, avg, (districtClientAccount(-,-,+),ratUrbInhab(+,-))).
mode(stdMonthInc, maxMod, (hasAccount(-,+,true),gender(+,-))).
mode(stdMonthInc, avg, (hasAccount(-,+,true),age(+,-))).
%mode(stdMonthInc, avg, (hasLoan(+,-,true),loanAmount(+,-))).
%mode(stdMonthInc, maxMod, (hasLoan(+,-,true),loanStatus(+,-))).
%mode(stdMonthInc, avg, (hasLoan(+,-,true),monthlyPayments(+,-))).

mode(stdMonthW, none, avgSumOfW(+,-)).
mode(stdMonthW, none, stdMonthInc(+,-)).
mode(stdMonthW, none, avgNrWith(+,-)).
%mode(stdMonthW, none, avgSumOfInc(+,-)).
mode(stdMonthW, none, freq(+,-)).
mode(stdMonthW, avg, (districtClientAccount(-,-,+),avgSalary(+,-))).
mode(stdMonthW, avg, (districtClientAccount(-,-,+),ratUrbInhab(+,-))).
mode(stdMonthW, maxMod, (hasAccount(-,+,true),gender(+,-))).
mode(stdMonthW, avg, (hasAccount(-,+,true),age(+,-))).
%mode(stdMonthW, avg, (hasLoan(+,-,true),loanAmount(+,-))).
%mode(stdMonthW, maxMod, (hasLoan(+,-,true),loanStatus(+,-))).
%mode(stdMonthW, avg, (hasLoan(+,-,true),monthlyPayments(+,-))).

mode(avgNrWith, none, avgSumOfW(+,-)).
%mode(avgNrWith, none, stdMonthInc(+,-)).
%mode(avgNrWith, none, stdMonthW(+,-)).
%mode(avgNrWith, none, avgSumOfInc(+,-)).
%mode(avgNrWith, none, freq(+,-)).
mode(avgNrWith, avg, (districtClientAccount(-,-,+),avgSalary(+,-))).
mode(avgNrWith, avg, (districtClientAccount(-,-,+),ratUrbInhab(+,-))).
mode(avgNrWith, maxMod, (hasAccount(-,+,true),gender(+,-))).
mode(avgNrWith, avg, (hasAccount(-,+,true),age(+,-))).
%mode(avgNrWith, avg, (hasLoan(+,-,true),loanAmount(+,-))).
%mode(avgNrWith, maxMod, (hasLoan(+,-,true),loanStatus(+,-))).
%mode(avgNrWith, avg, (hasLoan(+,-,true),monthlyPayments(+,-))).

mode(avgSumOfInc, none, avgSumOfW(+,-)).
mode(avgSumOfInc, none, stdMonthInc(+,-)).
mode(avgSumOfInc, none, stdMonthW(+,-)).
mode(avgSumOfInc, none, avgNrWith(+,-)).
mode(avgSumOfInc, none, freq(+,-)).
mode(avgSumOfInc, avg, (districtClientAccount(-,-,+),avgSalary(+,-))).
mode(avgSumOfInc, avg, (districtClientAccount(-,-,+),ratUrbInhab(+,-))).
mode(avgSumOfInc, maxMod, (hasAccount(-,+,true),gender(+,-))).
mode(avgSumOfInc, avg, (hasAccount(-,+,true),age(+,-))).
%mode(avgSumOfInc, avg, (hasLoan(+,-,true),loanAmount(+,-))).
%mode(avgSumOfInc, maxMod, (hasLoan(+,-,true),loanStatus(+,-))).
%mode(avgSumOfInc, avg, (hasLoan(+,-,true),monthlyPayments(+,-))).

mode(freq, none, avgSumOfW(+,-)).
%mode(freq, none, stdMonthInc(+,-)).
%mode(freq, none, stdMonthW(+,-)).
mode(freq, none, avgNrWith(+,-)).
%mode(freq, none, avgSumOfInc(+,-)).
mode(freq, avg, (districtClientAccount(-,-,+),avgSalary(+,-))).
mode(freq, avg, (districtClientAccount(-,-,+),ratUrbInhab(+,-))).
mode(freq, maxMod, (hasAccount(-,+,true),gender(+,-))).
mode(freq, avg, (hasAccount(-,+,true),age(+,-))).
%mode(freq, avg, (hasLoan(+,-,true),loanAmount(+,-))).
%mode(freq, maxMod, (hasLoan(+,-,true),loanStatus(+,-))).
%mode(freq, avg, (hasLoan(+,-,true),monthlyPayments(+,-))).

%mode(clientDistrict, none, age(+,-)).
%mode(clientDistrict, none, gender(+,-)).
%mode(clientDistrict, none, avgSalary(+,-)).
%mode(clientDistrict, none, ratUrbInhab(+,-)).
%mode(clientDistrict, avg, (hasAccount(+,-,true),avgSumOfW(+,-))).
%mode(clientDistrict, avg, (hasAccount(+,-,true),stdMonthInc(+,-))).
%mode(clientDistrict, avg, (hasAccount(+,-,true),stdMonthW(+,-))).
%mode(clientDistrict, avg, (hasAccount(+,-,true),avgNrWith(+,-))).
%mode(clientDistrict, avg, (hasAccount(+,-,true),avgSumOfInc(+,-))).
%mode(clientDistrict, maxMod, (hasAccount(+,-,true),freq(+,-))).
%mode(clientDistrict, avg, (clientLoan(+,-,true),loanAmount(+,-))).
%mode(clientDistrict, maxMod, (clientLoan(+,-,true),loanStatus(+,-))).
%mode(clientDistrict, avg, (clientLoan(+,-,true),monthlyPayments(+,-))).

%mode(hasAccount, none, age(+,-)).
%mode(hasAccount, none, gender(+,-)).
%mode(hasAccount, cnt, clientDistrict(+,-,true)).
%mode(hasAccount, cnt, clientLoan(+,-,true)).
%mode(hasAccount, avg, (clientDistrict(+,-,true),avgSalary(+,-))).
%mode(hasAccount, avg, (clientDistrict(+,-,true),ratUrbInhab(+,-))).
%mode(hasAccount, none, avgSumOfW(+,-)).
%mode(hasAccount, none, stdMonthInc(+,-)).
%mode(hasAccount, none, stdMonthW(+,-)).
%mode(hasAccount, none, avgNrWith(+,-)).
%mode(hasAccount, none, avgSumOfInc(+,-)).
%mode(hasAccount, none, freq(+,-)).
%mode(hasAccount, avg, (clientLoan(+,-,true),loanAmount(+,-))).
%mode(hasAccount, maxMod, (clientLoan(+,-,true),loanStatus(+,-))).
%mode(hasAccount, avg, (clientLoan(+,-,true),monthlyPayments(+,-))).

%mode(clientLoan, cnt, clientDistrict(+,-,true)).
%mode(clientLoan, none, age(+,-)).
%mode(clientLoan, none, gender(+,-)).
%mode(clientLoan, avg, (clientDistrict(+,-,true),avgSalary(+,-))).
%mode(clientLoan, avg, (clientDistrict(+,-,true),ratUrbInhab(+,-))).
%mode(clientLoan, avg, (hasAccount(+,-,true),avgSumOfW(+,-))).
%mode(clientLoan, avg, (hasAccount(+,-,true),stdMonthInc(+,-))).
%mode(clientLoan, avg, (hasAccount(+,-,true),stdMonthW(+,-))).
%mode(clientLoan, avg, (hasAccount(+,-,true),avgNrWith(+,-))).
%mode(clientLoan, avg, (hasAccount(+,-,true),avgSumOfInc(+,-))).
%mode(clientLoan, maxMod, (hasAccount(+,-,true),freq(+,-))).
%mode(clientLoan, none, loanAmount(+,-)).
%mode(clientLoan, none, loanStatus(+,-)).
%mode(clientLoan, none, monthlyPayments(+,-)).

%mode(hasLoan, cnt, districtClientAccount(-,-,+)).
%mode(hasLoan, cnt, hasAccount(-,+,true)).
%mode(hasLoan, none, avgSumOfInc(+,-)).
%mode(hasLoan, none, avgSumOfW(+,-)).
%mode(hasLoan, none, stdMonthInc(+,-)).
%mode(hasLoan, none, stdMonthW(+,-)).
%mode(hasLoan, none, avgNrWith(+,-)).
%mode(hasLoan, none, freq(+,-)).
%mode(hasLoan, avg, (districtClientAccount(-,-,+),avgSalary(+,-))).
%mode(hasLoan, avg, (districtClientAccount(-,-,+),ratUrbInhab(+,-))).
%mode(hasLoan, maxMod, (hasAccount(-,+,true),gender(+,-))).
%mode(hasLoan, avg, (hasAccount(-,+,true),age(+,-))).
%mode(hasLoan, none, loanAmount(+,-)).
%mode(hasLoan, none, loanStatus(+,-)).
%mode(hasLoan, none, monthlyPayments(+,-)).

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
  
%Facts
''')

MISSING_ATTRIBUTES_FILLER_RELATION_PRED = dict(
    financial = '''

loan(L) :- evidence(loan(L)).
client(C) :- evidence(client(C)).
account(A) :- evidence(account(A)).
district(D) :- evidence(district(D)).
hasLoan(A,L,X) :- account(A), loan(L), evidence(hasLoan(A,L,X)).
hasLoan(A,L,null) :- account(A), loan(L), \+evidence(hasLoan(A,L,X)).
clientDistrict(C,D,X) :- client(C), district(D), evidence(clientDistrict(C,D,X)).
clientDistrict(C,D,null) :- client(C), district(D), \+evidence(clientDistrict(C,D,X)).
hasAccount(C,A,X) :- client(C), account(A), evidence(hasAccount(C,A,X)).
hasAccount(C,A,null) :- client(C), account(A), \+evidence(hasAccount(C,A,X)).
clientLoan(C,L,X) :- client(C), loan(L), evidence(clientLoan(C,L,X)).
clientLoan(C,L,null) :- client(C), loan(L), \+evidence(clientLoan(C,L,X)).
loanAmount(L, X) :- loan(L), evidence(loanAmount(L, X)).
loanAmount(L, null) :- loan(L), \+evidence(loanAmount(L, X)).
loanStatus(L, X) :- loan(L), evidence(loanStatus(L, X)).
loanStatus(L, null) :- loan(L), \+evidence(loanStatus(L, X)).
monthlyPayments(L, X) :- loan(L), evidence(monthlyPayments(L, X)).
monthlyPayments(L, null) :- loan(L), \+evidence(monthlyPayments(L, X)).
gender(C, X) :- client(C), evidence(gender(C, X)).
gender(C, null) :- client(C), \+evidence(gender(C, X)).
age(C, X) :- client(C), evidence(age(C, X)).
age(C, null) :- client(C), \+evidence(age(C, X)).
freq(A, X) :- account(A), evidence(freq(A, X)). 
freq(A, null) :- account(A), \+evidence(freq(A, X)).
avgNrWith(A, X) :- account(A), evidence(avgNrWith(A, X)).
avgNrWith(A, null) :- account(A), \+evidence(avgNrWith(A, X)).
avgSumOfInc(A, X) :- account(A), evidence(avgSumOfInc(A, X)).
avgSumOfInc(A, null) :- account(A), \+evidence(avgSumOfInc(A, X)).
avgSumOfW(A, X) :- account(A), evidence(avgSumOfW(A, X)).
avgSumOfW(A, null) :- account(A), \+evidence(avgSumOfW(A, X)).
stdMonthInc(A, X) :- account(A), evidence(stdMonthInc(A, X)).
stdMonthInc(A, null) :- account(A), \+evidence(stdMonthInc(A, X)).
stdMonthW(A, X) :- account(A), evidence(stdMonthW(A, X)).
stdMonthW(A, null) :- account(A), \+evidence(stdMonthW(A, X)).
avgSalary(D, X) :- district(D), evidence(avgSalary(D, X)). 
avgSalary(D, null) :- district(D), \+evidence(avgSalary(D, X)).
ratUrbInhab(D, X) :- district(D), evidence(ratUrbInhab(D, X)). 
ratUrbInhab(D, null) :- district(D), \+evidence(ratUrbInhab(D, X)).
''')

DC_PROGRAM_DATA_DEPENDENT_RELATION = dict(
    university = '''%Relational Lookahead
builtin(studentCourseProfessor(_,_,_)).

studentCourseProfessor(S,C,P) :- takes(S,C,true), teaches(P,C,true).

''',
    financial = '''%Relational Lookahead
builtin(districtClientAccount(_,_,_)).
builtin(districtClientLoan(_,_,_)).

districtClientAccount(D,C,A) :- clientDistrict(C,D,true), hasAccount(C,A,true).
districtClientLoan(D,C,L) :- clientDistrict(C,D,true), clientLoan(C,L,true).

''',
    Hepatitis_std = '''%Relational Lookahead
builtin(bioDispatInf(_,_,_)).
builtin(infDispatIndis(_,_,_)).
builtin(bioDispatIndis(_,_,_)).

bioDispatInf(B,D,J) :- rel11(B,D,true), rel13(J,D,true).
infDispatIndis(J,D,I) :- rel13(J,D,true), rel12(I,D,true).
bioDispatIndis(B,D,I) :- rel11(B,D,true), rel12(I,D,true).

'''
)

DC_PROGRAM_DATA_DEPENDENT_RELATION_PROBABILISTIC = dict(
    university = '''%Relational Lookahead
builtin(studentCourseProfessor(_,_,_)).

%Facts

''',
    financial = '''%Relational Lookahead
builtin(districtClientAccount(_,_,_)).
builtin(districtClientLoan(_,_,_)).

%Facts

''',
    Hepatitis_std = '''%Relational Lookahead
builtin(bioDispatInf(_,_,_)).
builtin(infDispatIndis(_,_,_)).
builtin(bioDispatIndis(_,_,_)).

%Facts
 
'''
)

PROLOG_PROGRAM_HEADER = '''%%% -*- Mode: Prolog; -*-
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

'''

PROLOG_PROGRAM_HEADER_PROBABILISTIC = '''%%% -*- Mode: Prolog; -*-
:- use_module(library(lists)).
:- dynamic maxMod/3.
:- dynamic minMod/3.

cnt(X, P,Count) :- findall(X,P,L), length(L,Count).

oneElementOfList([H|_], X) :- X = H.

lmax(L, M) :- lmax(L, [], [], M).
lmax([], Seen, MMax, Max) :- MMax=[] -> Max=Seen; lmax(MMax, [], [], Max).
lmax([H|T], Seen, MMax, Max) :- (member(H, Seen) -> lmax(T, Seen, [H|MMax], Max); lmax(T, [H|Seen], MMax, Max)).
%maxMod(X, P, Max) :- findall(X,P,L), lmax(L, Max1), oneElementOfList(Max1, Max).

lmin(L, M) :- lmin(L, [], [], M).
lmin([], Seen, MMin, Min) :- MMin=[] -> Min=Seen, !; leftover(Seen, MMin, [], Min).
lmin([H|T], Seen, Left, Min) :- (member(H, Seen) -> lmin(T, Seen, [H|Left], Min); lmin(T, [H|Seen], Left, Min)).
leftover([], MMin, TMin, Min) :- TMin=[] -> lmin(MMin, [], [], Min); Min=TMin, !.
leftover([H|Seen], MMin, TMin, Min) :- (member(H, MMin)) -> leftover(Seen, MMin, TMin, Min); leftover(Seen, MMin, [H|TMin], Min).
%minMod(X, P, Min) :- findall(X,P,L), lmin(L, Min1), oneElementOfList(Min1, Min).

list2set([], []).
list2set([H|T], [H|T1]) :- subtract(T, [H], T2), list2set(T2, T1).

maxMod(Template,Goal,G) :-
    findall(X,bagof(Template,Goal,X),Lists),
    flatten(Lists,G3),
    list2set(G3,Gset),
    member(G,Gset).

minMod(Template,Goal,G) :-
    findall(X,bagof(Template,Goal,X),Lists),
    flatten(Lists,G3),
    list2set(G3,Gset),
    member(G,Gset).

max(X, P, Max) :- findall(X,P,L), max_list(L, Max).

min(X, P, Min) :- findall(X,P,L), min_list(L, Min).

listavg(L, C, A) :- C =:= 0 -> false; sum_list(L, Sum), A is Sum / C.
avg(X, P, Avg) :- findall(X,P,L), length(L,Cnt), listavg(L, Cnt, Avg).

'''

PROLOG_MARKOV_BLANKET_INTERVENTIONS = '''

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
'''

PROLOG_MARKOV_BLANKET_INTERVENTIONS2 = '''

retractARule(H,B) :-
    clause(H,B),
    B \== true,
    retract(:-(H,B)),
    fail.
retractARule(_,_).

%Bayes Ball Rules
visit(J,child) :- remove(J), \+evidence(J), markTop(J), parent(J,P), schedule(P), visit(P,child).
visit(J,child) :- remove(J), \+evidence(J), markBottom(J), parent(C,J), schedule(C), visit(C,parent).
visit(J,parent) :- remove(J), evidence(J), markTop(J), parent(J,P), schedule(P), visit(P,child).
visit(J,parent) :- remove(J), \+evidence(J), markBottom(J), parent(C,J), schedule(C), visit(C,parent).
visit(_,_) :- empty.
empty :- nb_getval(schedule, S), S=[].
remove(J) :- visited(J), nb_getval(schedule,S), member(J,S), delete(S,J,S1), nb_setval(schedule,S1).
remove(_).
schedule(J) :- nb_getval(schedule,S), \+member(J,S), append(S,[J],S1), nb_setval(schedule,S1).
schedule(_).
visited(J) :- markEvidence(J), nb_getval(visited,V), \+member(J,V), append(V,[J],V1), nb_setval(visited,V1).
visited(_).
markEvidence(J) :- evidence(J), nb_getval(evidence,E), \+member(J,E), append(E,[J],E1), nb_setval(evidence,E1).
markEvidence(_).
markTop(J) :- nb_getval(top,T), \+member(J,T), append(T,[J],T1), nb_setval(top,T1).
markBottom(J) :- nb_getval(bottom,B), \+member(J,B), append(B,[J],B1), nb_setval(bottom,B1).
initial :- nb_setval(evidence,[]), nb_setval(schedule,[]),nb_setval(visited,[]), nb_setval(top,[]), nb_setval(bottom,[]).
deleteGlobal :- nb_delete(evidence), nb_delete(schedule), nb_delete(visited), nb_delete(top), nb_delete(bottom).
bayesBall(X,Visited,MarkOnTop,MarkOnBottom,Evidence) :- initial, findall(1, visit(X,child),Z), nb_getval(visited,Visited),
                                                        nb_getval(top,MarkOnTop), nb_getval(bottom, MarkOnBottom), nb_getval(evidence,Evidence),
                                                        initial,deleteGlobal.
retractAndAssert(X,Y) :- retract(evidence(X)), assertz(X), Y=true.
retractAndAssert(X,Y) :- \+retract(evidence(X)), assertz(X), Y=false.
assertAndRetract(X,Y) :- Y=true, asserta(evidence(X)), retract(X).
assertAndRetract(X,Y) :- Y=false, retract(X).
requsite(X,ProbEvidence,Intervention) :- retractAndAssert(X,Y), bayesBall(X,Visited,MarkOnTop,MarkOnBottom,Evidence),
                                         intersection(Evidence,Visited,ProbEvidence1),
                                         intersection(MarkOnTop,ProbEvidence1,ProbEvidence),
                                         subtract(ProbEvidence1, ProbEvidence, Intervention),assertAndRetract(X,Y).
'''

MISSING_ATTRIBUTES_FILLER = dict(
    financial = '''

loan(L) :- evidence(loan(L)).
client(C) :- evidence(client(C)).
account(A) :- evidence(account(A)).
district(D) :- evidence(district(D)).
hasLoan(A,L,X) :- account(A), loan(L), evidence(hasLoan(A,L,X)).
clientDistrict(C,D,X) :- client(C), district(D), evidence(clientDistrict(C,D,X)).
hasAccount(C,A,X) :- client(C), account(A), evidence(hasAccount(C,A,X)).
clientLoan(C,L,X) :- client(C), loan(L), evidence(clientLoan(C,L,X)).
loanAmount(L, X) :- loan(L), evidence(loanAmount(L, X)).
loanAmount(L, null) :- loan(L), \+evidence(loanAmount(L, X)).
loanStatus(L, X) :- loan(L), evidence(loanStatus(L, X)).
loanStatus(L, null) :- loan(L), \+evidence(loanStatus(L, X)).
monthlyPayments(L, X) :- loan(L), evidence(monthlyPayments(L, X)).
monthlyPayments(L, null) :- loan(L), \+evidence(monthlyPayments(L, X)).
gender(C, X) :- client(C), evidence(gender(C, X)).
gender(C, null) :- client(C), \+evidence(gender(C, X)).
age(C, X) :- client(C), evidence(age(C, X)).
age(C, null) :- client(C), \+evidence(age(C, X)).
freq(A, X) :- account(A), evidence(freq(A, X)). 
freq(A, null) :- account(A), \+evidence(freq(A, X)).
avgNrWith(A, X) :- account(A), evidence(avgNrWith(A, X)).
avgNrWith(A, null) :- account(A), \+evidence(avgNrWith(A, X)).
avgSumOfInc(A, X) :- account(A), evidence(avgSumOfInc(A, X)).
avgSumOfInc(A, null) :- account(A), \+evidence(avgSumOfInc(A, X)).
avgSumOfW(A, X) :- account(A), evidence(avgSumOfW(A, X)).
avgSumOfW(A, null) :- account(A), \+evidence(avgSumOfW(A, X)).
stdMonthInc(A, X) :- account(A), evidence(stdMonthInc(A, X)).
stdMonthInc(A, null) :- account(A), \+evidence(stdMonthInc(A, X)).
stdMonthW(A, X) :- account(A), evidence(stdMonthW(A, X)).
stdMonthW(A, null) :- account(A), \+evidence(stdMonthW(A, X)).
avgSalary(D, X) :- district(D), evidence(avgSalary(D, X)). 
avgSalary(D, null) :- district(D), \+evidence(avgSalary(D, X)).
ratUrbInhab(D, X) :- district(D), evidence(ratUrbInhab(D, X)). 
ratUrbInhab(D, null) :- district(D), \+evidence(ratUrbInhab(D, X)).
''', university = '''

course(C) :- evidence(course(C)).
professor(P) :- evidence(professor(P)).
student(S) :- evidence(student(S)).
takes(S,C,X) :- student(S), course(C), evidence(takes(S,C,X)).
teaches(P,C,X) :- professor(P), course(C), evidence(teaches(P,C,X)).
friend(S,S1,X) :- student(S), student(S1), evidence(friend(S,S1,X)).
nrhours(C, X) :- course(C), evidence(nrhours(C, X)).
nrhours(C, null) :- course(C), \+evidence(nrhours(C, X)).
difficulty(C, X) :- course(C), evidence(difficulty(C, X)).
difficulty(C, null) :- course(C), \+evidence(difficulty(C, X)).
ability(P, X) :- professor(P), evidence(ability(P, X)).
ability(P, null) :- professor(P), \+evidence(ability(P, X)).
intelligence(S, X) :- student(S), evidence(intelligence(S, X)).
intelligence(S, null) :- student(S), \+evidence(intelligence(S, X)).
grade(S, C, X) :- student(S), course(C), evidence(grade(S, C, X)).
grade(S, C, null) :- student(S), course(C), \+evidence(grade(S, C, X)).
satisfaction(S, C, X) :- student(S), course(C), evidence(satisfaction(S, C, X)).
satisfaction(S, C, null) :- student(S), course(C), \+evidence(satisfaction(S, C, X)).

''', Hepatitis_std = '''

bio(B) :- evidence(bio(B)).
dispat(D) :- evidence(dispat(D)).
indis(I) :- evidence(indis(I)).
inf(J) :- evidence(inf(J)).
rel11(B,D,X) :- bio(B), dispat(D), evidence(rel11(B,D,X)).
rel12(I,D,X) :- indis(I), dispat(D), evidence(rel12(I,D,X)).
rel13(J,D,X) :- inf(J), dispat(D), evidence(rel13(J,D,X)).
fibros(B, X) :- bio(B), evidence(fibros(B, X)).
fibros(B, null) :- bio(B), \+evidence(fibros(B, X)).
activity(B, X) :- bio(B), evidence(activity(B, X)).
activity(B, null) :- bio(B), \+evidence(activity(B, X)).
sex(D, X) :- dispat(D), evidence(sex(D, X)).
sex(D, null) :- dispat(D), \+evidence(sex(D, X)).
age(D, X) :- dispat(D), evidence(age(D, X)).
age(D, null) :- dispat(D), \+evidence(age(D, X)).
type(D, X) :- dispat(D), evidence(type(D, X)).
type(D, null) :- dispat(D), \+evidence(type(D, X)).
got(I, X) :- indis(I), evidence(got(I, X)).
got(I, null) :- indis(I), \+evidence(got(I, X)).
gpt(I, X) :- indis(I), evidence(gpt(I, X)).
gpt(I, null) :- indis(I), \+evidence(gpt(I, X)).
alb(I, X) :- indis(I), evidence(alb(I, X)).
alb(I, null) :- indis(I), \+evidence(alb(I, X)).
tbil(I, X) :- indis(I), evidence(tbil(I, X)).
tbil(I, null) :- indis(I), \+evidence(tbil(I, X)).
dbil(I, X) :- indis(I), evidence(dbil(I, X)).
dbil(I, null) :- indis(I), \+evidence(dbil(I, X)).
che(I, X) :- indis(I), evidence(che(I, X)).
che(I, null) :- indis(I), \+evidence(che(I, X)).
ttt(I, X) :- indis(I), evidence(ttt(I, X)).
ttt(I, null) :- indis(I), \+evidence(ttt(I, X)).
ztt(I, X) :- indis(I), evidence(ztt(I, X)).
ztt(I, null) :- indis(I), \+evidence(ztt(I, X)).
tcho(I, X) :- indis(I), evidence(tcho(I, X)).
tcho(I, null) :- indis(I), \+evidence(tcho(I, X)).
tp(I, X) :- indis(I), evidence(tp(I, X)).
tp(I, null) :- indis(I), \+evidence(tp(I, X)).
dur(J, X) :- inf(J), evidence(dur(J, X)).
dur(J, null) :- inf(J), \+evidence(dur(J, X)).
'''
)

DYNAMIC = dict(
    financial = '''

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

    ''',
    university = '''

:- dynamic parent/2.
:- dynamic evidence/1.
:- dynamic course/1.
:- dynamic professor/1.
:- dynamic student/1.
:- dynamic nrhours/2.
:- dynamic difficulty/2.
:- dynamic ability/2.
:- dynamic intelligence/2.
:- dynamic grade/3.
:- dynamic satisfaction/3.
:- dynamic takes/3.
:- dynamic friend/3.
:- dynamic teaches/3.

    ''',
    Hepatitis_std = '''

:- dynamic parent/2.
:- dynamic evidence/1.
:- dynamic bio/1.
:- dynamic dispat/1.
:- dynamic indis/1.
:- dynamic inf/1.
:- dynamic rel11/3.
:- dynamic rel12/3.
:- dynamic rel13/3.
:- dynamic fibros/2.
:- dynamic activity/2.
:- dynamic sex/2.
:- dynamic age/2.
:- dynamic type/2.
:- dynamic got/2.
:- dynamic gpt/2.
:- dynamic alb/2.
:- dynamic tbil/2.
:- dynamic dbil/2.
:- dynamic che/2.
:- dynamic ttt/2.
:- dynamic ztt/2.
:- dynamic tcho/2.
:- dynamic tp/2.
:- dynamic dur/2.

    '''
)

DC_PROGRAM_HEADER = '''%%% -*- Mode: Prolog; -*-

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

'''

AGGREGATION_IN_PROBABILISTIC_CASE = '''
maxMod(Template,Goal,G) :- findall(X,bagof(Template,Goal,X),Lists), flatten(Lists,G3), list2set(G3,Gset), member(G,Gset).
minMod(Template,Goal,G) :- findall(X,bagof(Template,Goal,X),Lists), flatten(Lists,G3), list2set(G3,Gset), member(G,Gset).
'''
AGGREGATION_IN_DETERMINISTIC_CASE = '''
maxMod(X, P, Max) :- findall(X,P,L), lmax(L, Max1), oneElementOfList(Max1, Max).
minMod(X, P, Min) :- findall(X,P,L), lmin(L, Min1), oneElementOfList(Min1, Min).
'''
ERROR = dict()
YPRED = dict()
YTRUE = dict()
YPREDROC = dict()
YTRUEROC = dict()
WPLL = dict()
NUM_OF_ELEMENTS_IN_TEST = dict()
WRMSE= dict()
AUCROC = dict()
WPLL_TOTAL = dict()

logging.basicConfig(level=logging.INFO, filename="Test", filemode="w", format="%(asctime)-15s %(levelname)-8s %(message)s")


class Test():  

    def __init__(self):
        pass

    def getPredicateName(self, feat):
        feat = feat.replace(' ', '')
        featName = ''
        for i in feat:
            if i == '(':
                break
            else:
                featName+=i
        return featName

    def getListOfPredicatesInFolder(self, folderString):
        listOfFacts = []
        onlyfiles = [f for f in listdir(folderString) if isfile(join(folderString, f))]
        for f in onlyfiles:
            fSplit = f.split('.')
            filePath = join(folderString, f)
            filePointer = open(filePath, 'r')
            data = filePointer.read()
            filePointer.close()
            data = data.split('\n')
            for dat in data:
                if dat == '':
                    continue
                else:
                    dat = dat.replace('\r', '')
                    dat = dat.replace(' ', '')
                    datList = []
                    predicateName = self.getPredicateName(dat)
                    if predicateName in RELATIONAL_PREDICATES[DATABASE_NAME]:
                        if fSplit[1] == 'neg':
                            dat = dat.replace(').', ',false).')
                        else:
                            dat = dat.replace(').', ',true).')
                        datList.append(dat)
                    elif predicateName == 'clientAge':
                        tempDat = dat
                        tempDat = tempDat.split(',')
                        tempDat[0] = tempDat[0].replace('clientAge', 'clientLoan')
                        tempDat1 = tempDat[0] + ',' + tempDat[1] + ',true).'
                        tempDat[0] = tempDat[0].replace('clientLoan', 'age')
                        tempDat2 = tempDat[0] + ',' + tempDat[2]
                        datList = []
                        datList.append(tempDat1)
                        datList.append(tempDat2)
                    else:
                        datList.append(dat)
                    listOfFacts = listOfFacts + datList
        return listOfFacts

    def generatePrologFile(self, filename, trainingData, targetVariable=''):
        declarativeBias = DECLARATIVE_BIAS[DATABASE_NAME]
        if targetVariable == '':
            pass
        else:
            declarativeBias = ''
            targetVariableString = 'learn(' + targetVariable
            declarativeBiasTemp = DECLARATIVE_BIAS[DATABASE_NAME].split('\n')
            learnDefinitionStarts = False
            for line in declarativeBiasTemp:
                if '%Target' in line and not learnDefinitionStarts:
                    declarativeBias = declarativeBias + line + '\n'
                    learnDefinitionStarts = True
                elif learnDefinitionStarts:
                    if targetVariableString in line:
                        declarativeBias = declarativeBias + line + '\n'
                    else:
                        pass
                else:
                    declarativeBias = declarativeBias + line + '\n'
        f = open(filename, 'w')
        f.write(PROLOG_PROGRAM_HEADER)
        f.write(declarativeBias)
        for i in trainingData:
            f.write(i)
            f.write('\n')
        f.close()

    def findListOfDCFacts(self, factList):
        factListDC = []
        for feat in factList:
            feat = feat.replace(' ', '')
            featName = ''
            for i in feat:
                if i == '(':
                    break
                else:
                    featName+=i
            if featName in RANDOM_VARIABLE_PREDICATE[DATABASE_NAME]:
                feat = feat.split(',')
                value = feat[-1][0:-2]
                feat = ','.join(feat[0:-1])
                feat += ')'
                feat = feat + ' ~ ' + 'val(' + value + ').'
                factListDC.append(feat)
            else:
                feat = feat[0:-1]
                feat = feat + " := true."
                factListDC.append(feat)
        return factListDC

    def findListOfPrologFactsFromDCFacts(self, factList):
        factListProlog = []
        for feat in factList:
            feat = feat.replace(' ', '')
            feat = feat.split('~')
            feat[1] = feat[1].replace('val(', '')
            feat = ','.join(feat)
            factListProlog.append(feat)
        return factListProlog
            
    def generateDCFile(self, filename, trainingData, testData, dcRules):
        f = open(filename, 'w')
        f.write(DC_PROGRAM_HEADER)
        f.write(DC_PROGRAM_DATA_DEPENDENT_RELATION[DATABASE_NAME])
        trainingDataDC = self.findListOfDCFacts(trainingData)
        testDataDC = self.findListOfDCFacts(testData)
        f.write("\n\n%Train data prolog\n")
        for item in trainingData:
            f.write(item + '\n')
        f.write("\n\n%Test data prolog\n")
        for item in testData:
            f.write(item + '\n')
        #f.write("\n\n%Train data\n")
        #for item in trainingDataDC:
        #    f.write(item + '\n')
        f.write("\n\n%Test data\n")
        for item in testDataDC:
            f.write(item + '\n')
        f.write("\n\n%Background Theory\n")
        for item in dcRules:
            f.write(item + '\n')
        f.close()

    def flushPredictionDataStructures(self):
        global ERROR 
        global YPRED 
        global YTRUE
        global YPREDROC
        global YTRUEROC
        global NUM_OF_ELEMENTS_IN_TEST
        ERROR = dict()
        YPRED = dict()
        YTRUE = dict()
        YPREDROC = dict()
        YTRUEROC = dict()
        NUM_OF_ELEMENTS_IN_TEST = dict()

    def updateWPLLList(self, samples, featureName, originalValue, ty):
        if ty == 'continuous':
            empty = False
            for i in range(0, len(samples)):
                if samples[i] == '':
                    empty =True
                    break
                else:
                    samples[i] = float(samples[i])
            if empty:
                pass
            else:
                mn = statistics.mean(samples)
                std = statistics.stdev(samples)
                WPLL[featureName].append(np.log(scipy.stats.norm(mn, std).pdf(float(originalValue))))
        else:
            if samples[0] == '':
                pass
            else:
                lenSamples = len(samples)
                counts = Counter(samples)
                predList = []
                predProb = []
                for ele in RANDOM_VARIABLE_PREDICATE_RANGE[DATABASE_NAME][featureName]:
                    if ele == originalValue:
                        predList.append(1)
                    else:
                        predList.append(0)
                    if counts.has_key(ele):
                        predProb.append(counts[ele]/float(lenSamples))
                    else:
                        predProb.append(0.000001)
                probability = predProb[RANDOM_VARIABLE_PREDICATE_RANGE[DATABASE_NAME][featureName].index(originalValue)]
                WPLL[featureName].append(np.log(probability))
    
    def updateAccuracyList(self, samples, featureName, originalValue, ty):
        if ty == 'continuous':
            empty = False
            for i in range(0, len(samples)):
                if samples[i] == '':
                    empty =True
                    break
                else:
                    samples[i] = float(samples[i])
            if empty:
                pass
            else:
                average = sum(samples)/float(len(samples))
                ERROR[featureName].append((average - float(originalValue))*(average - float(originalValue)))
                NUM_OF_ELEMENTS_IN_TEST[featureName] = NUM_OF_ELEMENTS_IN_TEST[featureName] + 1
        else:
            if samples[0] == '':
                pass
            else:
                lenSamples = len(samples)
                counts = Counter(samples)
                predList = []
                predProb = []
                for ele in RANDOM_VARIABLE_PREDICATE_RANGE[DATABASE_NAME][featureName]:
                    if ele == originalValue:
                        predList.append(1)
                    else:
                        predList.append(0)
                    if counts.has_key(ele):
                        predProb.append(counts[ele]/float(lenSamples))
                    else:
                        predProb.append(0.0)
                YTRUEROC[featureName].append(predList)
                YPREDROC[featureName].append(predProb)
                yPred = max(set(samples), key=samples.count)
                YPRED[featureName].append(yPred)
                YTRUE[featureName].append(originalValue)
                NUM_OF_ELEMENTS_IN_TEST[featureName] = NUM_OF_ELEMENTS_IN_TEST[featureName] + 1
                
    def findWPLL(self, numOfSamples, tempDcFileName, testFacts, listOfTargetPredicates, trainFacts, dcRules):
        for predicate in listOfTargetPredicates:
            WPLL[predicate] = []
        for featName in listOfTargetPredicates:
            restTestFacts = []
            tempTestFacts = []
            for tFacts in testFacts:
                if featName not in tFacts:
                    restTestFacts.append(tFacts)
                else:
                    tempTestFacts.append(tFacts)
            ty = RANDOM_VARIABLE_PREDICATE_TYPE[DATABASE_NAME][featName]
            self.generateDCFile(tempDcFileName, trainFacts, restTestFacts, dcRules)
            yapObj = YapPrologInterface(RANDOM_VARIABLE_PREDICATE[DATABASE_NAME])
            yapObj.consultWithOneFile(tempDcFileName)
            for i in range(0, len(tempTestFacts)): 
                feat = tempTestFacts[i]
                feat = self.findListOfDCFacts([feat])[0]
                feat = feat.split('~')
                feat[1] = feat[1].replace('val(', '')
                feat[1] = feat[1].replace(').', '')
                feat[1] = feat[1].replace(' ', '')
                evidence = '[]'
                variable = '[X]'
                query = '(' + feat[0] + ' ~= X)'
                samples = yapObj.sample(numOfSamples, query, evidence, variable)
                samples = samples.split(',')
                for i in range(0, len(samples)):
                    samples[i] = samples[i].replace('[', '').replace(']', '')
                self.updateWPLLList(samples, featName, feat[1], ty)
                print feat, samples[0:10]
            del yapObj
        for predicate in listOfTargetPredicates:
            ty = RANDOM_VARIABLE_PREDICATE_TYPE[DATABASE_NAME][predicate] 
            logging.info('\n \n \n' + '$$$$$$$$$$ Results for %s $$$$$$$$$$' % (predicate))
            wpll = WPLL[predicate]
            numElements = len(wpll)
            if numElements == 0:
                wpllScore = float("-inf")
            else:
                wpllScore = sum(wpll)/float(numElements)
            logging.info('\nWPLL = %f, Number of tests consider = %d' % (wpllScore, numElements))
            if WPLL_TOTAL.has_key(predicate):
                tempList = WPLL_TOTAL[predicate]
                tempList.append(wpllScore)
            else:
                WPLL_TOTAL[predicate] = [wpllScore]
        

    def findTestAccuracy(self, numOfSamples, tempDcFileName, testFacts, listOfTargetPredicates, trainFacts, dcRules):
        for predicate in listOfTargetPredicates:
            ERROR[predicate] = []
            YPRED[predicate] = []
            YTRUE[predicate] = []
            YTRUEROC[predicate] = []
            YPREDROC[predicate] = []
            NUM_OF_ELEMENTS_IN_TEST[predicate] = 0
        for featName in listOfTargetPredicates:
            restTestFacts = []
            tempTestFacts = []
            for tFacts in testFacts:
                if featName not in tFacts:
                    restTestFacts.append(tFacts)
                else:
                    tempTestFacts.append(tFacts)
            ty = RANDOM_VARIABLE_PREDICATE_TYPE[DATABASE_NAME][featName]
            self.generateDCFile(tempDcFileName, trainFacts, restTestFacts, dcRules)
            yapObj = YapPrologInterface(RANDOM_VARIABLE_PREDICATE[DATABASE_NAME])
            yapObj.consultWithOneFile(tempDcFileName)
            for i in range(0, len(tempTestFacts)): 
                feat = tempTestFacts[i]
                feat = self.findListOfDCFacts([feat])[0]
                feat = feat.split('~')
                feat[1] = feat[1].replace('val(', '')
                feat[1] = feat[1].replace(').', '')
                feat[1] = feat[1].replace(' ', '')
                evidence = '[]'
                variable = '[X]'
                query = '(' + feat[0] + ' ~= X)'
                samples = yapObj.sample(numOfSamples, query, evidence, variable)
                samples = samples.split(',')
                for i in range(0, len(samples)):
                    samples[i] = samples[i].replace('[', '').replace(']', '')
                self.updateAccuracyList(samples, featName, feat[1], ty)
                print feat, samples[0:10]
            yapObj.terminate()
            del yapObj
        for predicate in listOfTargetPredicates:
            ty = RANDOM_VARIABLE_PREDICATE_TYPE[DATABASE_NAME][predicate] 
            logging.info('\n \n \n' + '$$$$$$$$$$ Results for %s $$$$$$$$$$' % (predicate))
            if ty == 'continuous':
                acc = float('nan')
                if NUM_OF_ELEMENTS_IN_TEST[predicate] == 0:
                    logging.info('WRMSE = %f, Number of tests consider = %d' % (acc, NUM_OF_ELEMENTS_IN_TEST[predicate]))
                else:
                    wrmse = math.sqrt(sum(ERROR[predicate])/float(NUM_OF_ELEMENTS_IN_TEST[predicate]))/float(RANDOM_VARIABLE_PREDICATE_RANGE[DATABASE_NAME][predicate])
                    if WRMSE.has_key(predicate):
                        tempList = WRMSE[predicate]
                        tempList.append(wrmse)
                    else:
                        WRMSE[predicate] = [wrmse]
                    logging.info('WRMSE = %f, Number of tests consider = %d' % (wrmse, NUM_OF_ELEMENTS_IN_TEST[predicate]))
            else:
                uniqueLabel = list(set(YPRED[predicate] + YTRUE[predicate]))
                logging.info ('Confusion Matrix:')
                logging.info(confusion_matrix(YTRUE[predicate], YPRED[predicate], labels=uniqueLabel))
                logging.info('Labels: '  + str(uniqueLabel))
                if NUM_OF_ELEMENTS_IN_TEST[predicate] == 0:
                    logging.info('Accuracy: nan')
                    logging.info('Area Under ROC: prediction list is empty')
                else:
                    logging.info('Accuracy: %f'  % accuracy_score(YTRUE[predicate], YPRED[predicate]))
                    try:
                        auROC = roc_auc_score(np.array(YTRUEROC[predicate]), np.array(YPREDROC[predicate]), average='weighted')
                        if AUCROC.has_key(predicate):
                            tempList = AUCROC[predicate]
                            tempList.append(auROC)
                        else:
                            AUCROC[predicate] = [auROC]
                        logging.info('Area Under ROC: %f' % auROC)
                    except ValueError:
                        logging.info('Only one class present in y_true, Area Under ROC: %f' % 1)

    def generateEnsembleOfDLTs(self, fileName, trainFacts, outputFile):
        self.generatePrologFile(fileName, trainFacts)
        f = open(outputFile, 'w')
        obj = DCLearner(fileName, '', '', RANDOM_VARIABLE_PREDICATE[DATABASE_NAME])
        obj.learnRules()
        obj1 = TranslateToDC()
        dcRules = []
        obj1.setRandomVariablePredicates(RANDOM_VARIABLE_PREDICATE[DATABASE_NAME])
        for rule in obj.rules:
            rule = obj1.translate(rule)
            dcRules.append(rule)
            f.write(rule + '\n')
        f.close()
        return dcRules

    def experiment1_financial(self, prologFileName, dcFileName, treeOutputFile):
        for randVariable in ['avgSalary', 'ratUrbInhab']: ## Put all target random variable here.
            for fold in range(1,11):
                foldString = "Fold" + str(fold)
                trainFacts = self.getListOfPredicatesInFolder(DATA_DIRECTORY + '/' + foldString + '/' + 'train')
                validateFacts = self.getListOfPredicatesInFolder(DATA_DIRECTORY + '/' + foldString + '/' + 'validate')
                testFacts = self.getListOfPredicatesInFolder(DATA_DIRECTORY + '/' + foldString + '/' + 'test')
                trainFacts = trainFacts + validateFacts
                self.flushPredictionDataStructures()
                
                self.generatePrologFile(prologFileName, trainFacts, targetVariable=randVariable)
                f = open(treeOutputFile, 'w')
                obj = DCLearner(prologFileName, '', '', RANDOM_VARIABLE_PREDICATE[DATABASE_NAME])
                obj.learnRules()
                obj1 = TranslateToDC()
                dcRules = []
                obj1.setRandomVariablePredicates(RANDOM_VARIABLE_PREDICATE[DATABASE_NAME])
                for rule in obj.rules:
                    rule = obj1.translate(rule)
                    dcRules.append(rule)
                    f.write(rule + '\n')
                f.close()
                self.findTestAccuracy(1000, dcFileName, testFacts, [randVariable], trainFacts, dcRules)
            for key in WRMSE:
                a = WRMSE[key]
                logging.info('\nPredicate = %s, WRMSE = %f +- %f' % (key, statistics.mean(a), statistics.stdev(a)))
            for key in AUCROC:
                a = AUCROC[key]
                logging.info('\nPredicate = %s, AUCROC = %f +- %f' % (key, statistics.mean(a), statistics.stdev(a)))
            print "Too much work. Going to take nap for 10 secs."
            time.sleep(10)
            print "I am feeling fresh now :)"
        
    def experiment1_university(self, prologFileName, dcFileName, treeOutputFile):
        startFold = 1
        endFold = 11
        for randVariable in ['nrhours']: ## Put all target random variable here.
            for fold in range(startFold,endFold):
                foldString = "data800x125x125_" + str(fold) + '/data800x125x125'
                trainFacts = self.getListOfPredicatesInFolder(DATA_DIRECTORY + '/' + foldString + '/' + 'train')
                #validateFacts = getListOfPredicatesInFolder(DATA_DIRECTORY + '/' + foldString + '/' + 'validate')
                testFacts = self.getListOfPredicatesInFolder(DATA_DIRECTORY + '/' + foldString + '/' + 'test')
                trainFacts = trainFacts
                self.flushPredictionDataStructures()
                
                self.generatePrologFile(prologFileName, trainFacts, targetVariable=randVariable)
                f = open(treeOutputFile, 'w')
                obj = DCLearner(prologFileName, '', '', RANDOM_VARIABLE_PREDICATE[DATABASE_NAME])
                obj.learnRules()
                obj1 = TranslateToDC()
                dcRules = []
                obj1.setRandomVariablePredicates(RANDOM_VARIABLE_PREDICATE[DATABASE_NAME])
                for rule in obj.rules:
                    rule = obj1.translate(rule)
                    dcRules.append(rule)
                    f.write(rule + '\n')
                f.close()
                self.findWPLL(1000, dcFileName, testFacts, [randVariable], [], dcRules)
            if (endFold - startFold) > 1:
                for key in WPLL_TOTAL:
                    a = WPLL_TOTAL[key]
                    logging.info('\nPredicate = %s, WPLL = %f +- %f' % (key, statistics.mean(a), statistics.stdev(a)))
            print "Too much work. Going to take nap for 10 secs."
            time.sleep(10)
            print "I am feeling fresh now :)"
        
if __name__ == '__main__':        
    obj = Test()
    obj.experiment1_financial(PROCESSED_TRAIN_DATA_FILE, PROCESSED_TRAIN_DATA_FILE_DC, OUTPUT_ENSEMBLE_OF_DLTS)
    
    #obj.experiment1_university(PROCESSED_TRAIN_DATA_FILE, PROCESSED_TRAIN_DATA_FILE_DC, OUTPUT_ENSEMBLE_OF_DLTS)
    
    #TRAIN_FACTS = getListOfPredicatesInFolder(DATA_DIRECTORY + '/' + foldString + '/' + 'train')
    #validateFacts = getListOfPredicatesInFolder(DATA_DIRECTORY + '/' + foldString + '/' + 'validate')
    #TEST_FACTS = getListOfPredicatesInFolder(DATA_DIRECTORY + '/' + foldString + '/' + 'test')
    #TRAIN_FACTS = TRAIN_FACTS + validateFacts
    #DC_RULES = generateEnsembleOfDLTs(PROCESSED_TRAIN_DATA_FILE, TRAIN_FACTS, OUTPUT_ENSEMBLE_OF_DLTS)
    #findTestAccuracy(1000, PROCESSED_TRAIN_DATA_FILE_DC, TEST_FACTS, ['loanStatus'], TRAIN_FACTS, DC_RULES)
    #generatePrologFile(PROCESSED_TRAIN_DATA_FILE+'x', TEST_FACTS)


