'''
Created on Jan 30, 2020

@author: nitesh
'''

import subprocess, pickle
from .InterfaceDCQuery  import InterfaceDCQuery

PROLOG_ADDITIONAL = '''%%% -*- Mode: Prolog; -*-
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

'''

DC_ADDITIONAL = '''%%% -*- Mode: Prolog; -*-

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

'''


class Functor(object):
    name = None
    
    """
    Can be of type 'attribute', 'relation' or 'entity'
    """
    type = None
    tableName = None
    relateTo = None
    variableType = None
    domain = None


class DreaML(object):
    def __init__(self):
        self.prologFileName = 'prologData.pl'
        self.dcFileName = 'dcData.pl'
        self.learnedDCRules = 'learnedDCRules.pl'
        self.tempDependencyFile = 'tempDependency.pkl'
        self.dcLearner = 'InvokeDCLearner.py'
        self.prologFacts = []
        self.dcRelationalStructure = []
        self.currentNumber = 0
        self.allFunctors = []
        self.python2env = None
        self.dcFactsStore = None
        self.RELATIONAL_CHAIN_OF_PREDICATES = dict()
        self.RANDOM_VARIABLE_PREDICATE = None
        self.RELATIONAL_PREDICATES = None
        self.dcInterface = None
        self.schema = None
        self.database = None
        self.schemaPrimaryKey = None
        self.interventions = None
    
    def setPython2Path(self, env):
        self.python2env = env
    
    def initialize(self, schema, database, foreignKeys):
        self.schema = schema
        self.database = database
        self.processDatabase(schema, database, foreignKeys)
        self.setRandomVariablePredicates()
        f = open(self.prologFileName, 'w')
        f.write(PROLOG_ADDITIONAL)
        f.write('\n')
        f.write('%%%%%%%%%%% Declarative Bias states here %%%%%%%%%%%%' + '\n')
        for item in self.generateDiscontinuous():
            f.write(item)
            f.write('\n')
        f.write('\n')
        f.write('%Types' + '\n')
        for item in self.generateTypeDeclaration():
            f.write(item)
            f.write('\n')
        f.write('\n')
        f.write('%Modes' +'\n')
        for item in self.generateModeDeclaration():
            f.write(item)
            f.write('\n')
        f.write('\n')
        f.write('%Declare the type of random variables' + '\n')
        for item in self.generateThresDeclaration():
            f.write(item)
            f.write('\n')
        f.write('\n')
        
        f.write('%List the name of all random variables' + '\n')
        f.write(self.generateVariablesName() + '\n\n') 
        
        f.write('%Rank declaration' + '\n')
        f.write(self.generateRankDeclaration() + '\n\n')
        
        f.write('%Target' + '\n')
        for item in self.generateLearnDeclaration():
            f.write(item)
            f.write('\n')
        f.write('%%%%%%%%%%% Declarative Bias ends here %%%%%%%%%%%%' + '\n\n')
        f.write('%Train data' + '\n')
        [prologFacts, dcFacts, interventions] = self.generatePrologFacts(schema, database)
        self.interventions = interventions
        self.dcFactsStore = dcFacts
        for item in prologFacts:
            f.write(item)
            f.write('\n')
        f.close()
    
    def learn(self):
        try:
            path = self.python2env['lib'] + ':../'
            python2_env = {"PYTHONPATH": path}
            command = [self.python2env['bin'], self.dcLearner, self.prologFileName, self.learnedDCRules, self.tempDependencyFile]
            process = subprocess.run(command, env=python2_env, check=True, stdout=subprocess.PIPE, universal_newlines=True)
            output = process.stdout
            print(output)
        except:
            raise
    
    def loadDCProgram(self, numOfSamples):
        self.generateDCFile()
        self.dcInterface = InterfaceDCQuery(self.dcFileName, numOfSamples, self.allFunctors)

    def query(self, inputEvidence, query, inputInterventionList):
        result = []
        for qy in query:
            result.append(self.dcInterface.query(inputEvidence, qy, inputInterventionList))
        return result
    
    def queryDC(self, query, predictUncertainty, additionalQuery, additionalConditions, additionalInterventions):
        queryList = []
        for qy in query:
            (table, row, col) = qy
            attributeName = self.schema[table][col]['header']
            primaryCol = self.schemaPrimaryKey[table]
            primaryKey = self.database[table][row][primaryCol]
            q = attributeName + '(' + primaryKey + ',null)'
            queryList.append(q)
        queryList = queryList + additionalQuery
        self.dcInterface.setpredictUncertainty(predictUncertainty)
        return self.query(additionalConditions, queryList, additionalInterventions)

    def generateDCFile(self):
        f = open(self.dcFileName, 'w')
        f.write(DC_ADDITIONAL)
        f.write('\n')
        for item in self.generateDynamic():
            f.write(item)
            f.write('\n')
        f.write('\n')
        for item in self.generateMissingValuesFiller():
            f.write(item)
            f.write('\n')
        f.write('\n')
        
        f.write('%Test data prolog \n')
        for item in self.dcFactsStore:
            f.write(item)
            f.write('\n')
        f.write('\n')
        
        f.write('%Dependency Structure \n')
        for item in self.getDependencyStructureList():
            f.write(item)
        f.write('\n')
        
        f.write('%Interventions \n')
        for item in self.interventions:
            f.write(item)
            f.write('\n')
        f.write('\n')
        
        f.write('%Learned Distributional Clauses \n')
        for item in self.readLearnedDCRules():
            f.write(item)
            f.write('\n')
        f.write('\n')
        f.close()
    
    def readLearnedDCRules(self):
        f = open(self.learnedDCRules, 'r')
        rules = f.read()
        f.close()
        ruleList = rules.split('\n')
        return ruleList    
    
    def getDependencyStructureList(self):
        with open(self.tempDependencyFile, 'rb') as inp:
            prologDependencyStructureList = pickle.load(inp)
            basePredicates = pickle.load(inp)
        prologDependencyStructure = []
        for dependency in prologDependencyStructureList:
            for head in dependency.keys():
                if dependency[head] == []:
                    pass
                else:
                    pds = set()
                    uniqueAtoms = set()
                    for feature in dependency[head]:
                        for atom in feature:
                            uniqueAtoms.add(atom)
                    chainOfRelations = self.RELATIONAL_CHAIN_OF_PREDICATES.keys()
                    for rel in chainOfRelations:
                        if rel in uniqueAtoms:
                            chain = self.RELATIONAL_CHAIN_OF_PREDICATES[rel]
                            atoms = []
                            for c in chain:
                                atoms.append(basePredicates[c])
                            body = ' :- ' + basePredicates[head] + ',' + ','.join(atoms) + '.\n'
                            for c in chain:
                                #pds.add('parent(' + basePredicates[head] + ',' + basePredicates[c] + ')' + body)
                                pass
                    for rel in self.relationRelatedPredicates():
                        if rel in uniqueAtoms:
                            body = ' :- ' + basePredicates[head] + ',' + basePredicates[rel] + '.\n'
                            #pds.add('parent(' + basePredicates[head] + ',' + basePredicates[rel] + ')' + body)
                            pass
                    for feature in dependency[head]:
                        atoms = []
                        for atom in feature:
                            if atom in self.relationRelatedPredicates():
                                temp = basePredicates[atom]
                                temp = temp.split(',')
                                temp[-1] = 'true)'
                                atoms.append(','.join(temp))
                            else:
                                atoms.append(basePredicates[atom])
                        body = ' :- ' + basePredicates[head] + ',' + ','.join(atoms) + '.\n'
                        for atom in feature:
                            if atom in self.attributeRelatedPredicates():
                                pds.add('parent(' + basePredicates[head] + ',' + basePredicates[atom] + ')' + body)
                    for item in pds:
                        prologDependencyStructure += [item]
        return prologDependencyStructure
    
    def createJMP(self):
        f = open(self.learnedDCRules, 'r')
        rules = f.read()
        rules = rules.split('\n')
        f.close()
        f.write('\n')
        f.write('%Joint Model Program')
        f = open(self.dcFileName, 'a')
        for rule in rules:
            f.write(rule)
            f.write('\n')
        f.close()
    
    def generateModeDeclaration(self):
        modeDeclaration = []
        for functor in self.allFunctors:
            if functor.type == 'attribute':
                for key in functor.relateTo:
                    if key == 'none':
                        relatedFunctors = functor.relateTo[key]['functors']
                        for rFunc in relatedFunctors:
                            if rFunc.type == 'attribute' and functor.name != rFunc.name:
                                fact = 'mode(' + functor.name + ', ' + 'none, ' +  rFunc.name + '(+,-)).'
                                modeDeclaration.append(fact)
                            else:
                                pass
                    elif key == 'tableNameWithPrimaryKey':
                        pass
                    else:
                        relatedFunctors = functor.relateTo[key]['functors']
                        for rFunc in relatedFunctors:
                            if rFunc.type == 'attribute' and rFunc.variableType == 'continuous' and functor.relateTo[key]['direction'] == 'plus':
                                fact = 'mode(' + functor.name + ', ' + 'avg, (' + key + '(+,-,true),' + rFunc.name + '(+,-))).'
                                modeDeclaration.append(fact)
                            elif rFunc.type == 'attribute' and rFunc.variableType == 'continuous' and functor.relateTo[key]['direction'] == 'minus':
                                fact = 'mode(' + functor.name + ', ' + 'avg, (' + key + '(-,+,true),' + rFunc.name + '(+,-))).'
                                modeDeclaration.append(fact)
                            elif rFunc.type == 'attribute' and rFunc.variableType == 'discrete' and functor.relateTo[key]['direction'] == 'plus':
                                fact = 'mode(' + functor.name + ', ' + 'minMod, (' + key + '(+,-,true),' + rFunc.name + '(+,-))).'
                                modeDeclaration.append(fact)
                                fact = 'mode(' + functor.name + ', ' + 'maxMod, (' + key + '(+,-,true),' + rFunc.name + '(+,-))).'
                                modeDeclaration.append(fact)
                            elif rFunc.type == 'attribute' and rFunc.variableType == 'discrete' and functor.relateTo[key]['direction'] == 'minus':
                                fact = 'mode(' + functor.name + ', ' + 'minMod, (' + key + '(-,+,true),' + rFunc.name + '(+,-))).'
                                modeDeclaration.append(fact)
                                fact = 'mode(' + functor.name + ', ' + 'maxMod, (' + key + '(-,+,true),' + rFunc.name + '(+,-))).'
                                modeDeclaration.append(fact)
                            else:
                                pass
        return modeDeclaration
    
    def generatePrologFacts(self, schema, database):
        prologFacts = []
        dcFacts = []
        interventions = []
        self.schemaPrimaryKey = dict()
        for key in database.keys():
            numOfPrimaryKey = 0
            for i in range(0, len(schema[key])):
                if schema[key][i]['type'] == 'primaryKey':
                    primaryCol = i
                    numOfPrimaryKey += 1
            if numOfPrimaryKey > 1:
                raise Exception('currently only one column can be primary key!')
            
            self.schemaPrimaryKey[key] = primaryCol
        for key in database.keys():
            table = database[key]
            tableSchema = schema[key]
            
            for row in table:
                for i in range(0, len(row)):
                    if tableSchema[i]['type'] == 'primaryKey':
                        if row[i] == '':
                            raise Exception('primary key can not be missing!')
                        fact = key + '(' + row[i] + ')'
                        eFact = 'evidence(' + fact + ').'
                        iFact = fact + ' := true. '
                        fact = fact + '.'
                        dcFacts.append(eFact)
                        prologFacts.append(fact)
                        interventions.append(iFact)
                    elif row[i] == '':
                        pass
                    
                    elif tableSchema[i]['type'] == 'foreignKey':
                        fact = tableSchema[i]['header'] + '(' + row[self.schemaPrimaryKey[key]] + ',' + row[i] + ',true)'
                        eFact = 'evidence(' + fact + ').'
                        fact = fact + '.'
                        dcFacts.append(eFact)
                        prologFacts.append(fact)
                        iFact = tableSchema[i]['header'] + '(' + row[self.schemaPrimaryKey[key]] + ',' + row[i] + ') ~ val(true).'
                        interventions.append(iFact)
                    else:
                        fact = tableSchema[i]['header'] + '(' + row[self.schemaPrimaryKey[key]] + ',' + row[i] + ')'
                        eFact = 'evidence(' + fact + ').'
                        fact = fact + '.'
                        dcFacts.append(eFact)
                        prologFacts.append(fact)
        return [prologFacts, dcFacts, interventions]
    
    def generateTypeDeclaration(self):
        typeDeclaration = []
        for functor in self.allFunctors:
            if functor.type == 'entity':
                fact = 'base(' + functor.tableName + '(' + functor.tableName + ')).'
                typeDeclaration.append(fact)
            elif functor.type == 'attribute':
                self.currentNumber = self.currentNumber + 1
                varName = 'x' + str(self.currentNumber) 
                fact = 'base(' + functor.name + '(' + functor.tableName + ',' + varName + ')).'
                typeDeclaration.append(fact)
            elif functor.type == 'relation':
                self.currentNumber = self.currentNumber + 1
                varName = 'x' + str(self.currentNumber)
                relateToTableName = functor.relateTo['tableNameWithPrimaryKey']
                fact = 'base(' + functor.name + '(' + functor.tableName + ',' + relateToTableName + ',' + varName + ')).'
                typeDeclaration.append(fact)
            else:
                pass
        return typeDeclaration
    
    def generateThresDeclaration(self):
        thresDeclaration = []
        for functor in self.allFunctors:
            if functor.type == 'attribute':
                if functor.variableType == 'continuous':
                    fact = 'thres(' + functor.name + ', 2, ' + 'continuous, ' + functor.domain + ').'
                    thresDeclaration.append(fact)
                elif functor.variableType == 'discrete':
                    fact = 'thres(' + functor.name + ', 2, ' + 'discrete, ' + functor.domain + ').'
                    thresDeclaration.append(fact)
                else:
                    pass
            elif functor.type == 'relation':
                fact = 'thres(' + functor.name + ', 3, ' + 'continuous, ' + functor.domain + ').'
                thresDeclaration.append(fact)
            else:
                pass
        return thresDeclaration
        
    def generateLearnDeclaration(self):
        learnDeclaration = []
        for functor in self.allFunctors:
            if functor.type == 'attribute':
                if functor.variableType == 'continuous':
                    fact = 'learn(' + functor.name + ', 2, 2, ' + 'continuous).'
                    learnDeclaration.append(fact)
                elif functor.variableType == 'discrete':
                    fact = 'learn(' + functor.name + ', 2, 2, ' + 'discrete).'
                    learnDeclaration.append(fact)
                else:
                    pass
            else:
                pass
        return learnDeclaration
    
    def generateVariablesName(self):
        fact = 'randomVariableNames(['
        first = True
        for functor in self.allFunctors:
            if functor.type == 'attribute':
                if first:
                    fact = fact + functor.name
                else:
                    fact = fact + ',' + functor.name
                first = False
            elif functor.type == 'relation':
                if first:
                    fact = fact + functor.name
                else:
                    fact = fact + ',' + functor.name
                first = False
            else:
                pass
        fact = fact + ']).'
        return fact
    
    def setRandomVariablePredicates(self):
        self.RANDOM_VARIABLE_PREDICATE = []
        self.RELATIONAL_PREDICATES = []
        for functor in self.allFunctors:
            if functor.type == 'attribute':
                self.RANDOM_VARIABLE_PREDICATE.append(functor.name)
            elif functor.type == 'relation':
                self.RANDOM_VARIABLE_PREDICATE.append(functor.name)
                self.RELATIONAL_PREDICATES.append(functor.name)
            else:
                pass
            
    def attributeRelatedPredicates(self):
        attributeRelatedPredicates = self.RANDOM_VARIABLE_PREDICATE
        relationRelatedPredicates = self.RELATIONAL_PREDICATES
        attributeRelatedPredicates = list(set(attributeRelatedPredicates)-set(relationRelatedPredicates))
        return attributeRelatedPredicates

    def relationRelatedPredicates(self):
        return self.RELATIONAL_PREDICATES
            
    def generateRankDeclaration(self):
        fact = 'rank(['
        first = True
        for functor in self.allFunctors:
            if functor.type == 'attribute':
                if first:
                    fact = fact + functor.name
                else:
                    fact = fact + ',' + functor.name
                first = False
            elif functor.type == 'relation':
                if first:
                    fact = fact + functor.name
                else:
                    fact = fact + ',' + functor.name
                first = False
            else:
                pass
        fact = fact + ']).'
        return fact
    
    def generateDiscontinuous(self):
        discontinuous = []
        for functor in self.allFunctors:
            if functor.type == 'entity':
                fact = ':- discontiguous ' + functor.tableName + '/1.'
                discontinuous.append(fact)
            elif functor.type == 'attribute':
                fact = ':- discontiguous ' + functor.name + '/2.'
                discontinuous.append(fact)
            elif functor.type == 'relation':
                fact = ':- discontiguous ' + functor.name + '/3.'
                discontinuous.append(fact)
            else:
                pass
        return discontinuous
    
    def generateDynamic(self):
        dynamic = [':- dynamic parent/2.', ':- dynamic evidence/1.']
        for functor in self.allFunctors:
            if functor.type == 'entity':
                fact = ':- dynamic ' + functor.tableName + '/1.'
                dynamic.append(fact)
            elif functor.type == 'attribute':
                fact = ':- dynamic ' + functor.name + '/2.'
                dynamic.append(fact)
            elif functor.type == 'relation':
                fact = ':- dynamic ' + functor.name + '/3.'
                dynamic.append(fact)
            else:
                pass
        return dynamic
    
    def generateMissingValuesFiller(self):
        missingValueFiller = []
        for functor in self.allFunctors:
            if functor.type == 'entity':
                tableName = functor.tableName
                rule1 = tableName + '(X) :- ' + 'evidence(' + tableName + '(X)).'
                missingValueFiller.append(rule1)
            elif functor.type == 'attribute':
                functorName = functor.name
                tableName = functor.tableName
                rule1 = functorName + '(X,Z) :- ' + tableName + '(X), ' + 'evidence(' +     functorName + '(X,Z)).'
                missingValueFiller.append(rule1)
                rule2 = functorName + '(X,null) :- ' + tableName + '(X), ' + '\+evidence(' +     functorName + '(X,Z)).'
                missingValueFiller.append(rule2)
            elif functor.type == 'relation':
                functorName = functor.name
                rule1 = functorName + '(X,Y,true) :- ' + 'evidence(' + functorName + '(X,Y,true)).'
                missingValueFiller.append(rule1)
            else:
                pass
        return missingValueFiller
    
    def processDatabase(self, schema, database, foreignKeys):
        for key in schema.keys():
            functors = []
            for i in range(0, len(schema[key])):
                column = schema[key][i]
                if column['type'] == 'primaryKey':
                    entityFunctor = Functor()
                    entityFunctor.name = column['header']
                    entityFunctor.type = 'entity'
                    entityFunctor.tableName = key
                elif column['type'] == 'foreignKey':
                    entityFunctor = Functor()
                    entityFunctor.name = column['header']
                    entityFunctor.type = 'relation'
                    entityFunctor.tableName = key
                    entityFunctor.domain = '[]'
                elif column['type'] == 'discrete' or column['type'] == 'continuous':
                    entityFunctor = Functor()
                    entityFunctor.name = column['header']
                    entityFunctor.type = 'attribute'
                    entityFunctor.tableName = key
                    if column['type'] == 'discrete':
                        entityFunctor.variableType = 'discrete'
                        colRecord = []
                        for rec in database[key]:
                            colRecord.append(rec[i])
                        colRecordSet = set(colRecord)
                        if '' in colRecordSet:
                            colRecordSet.remove('')
                        domStr = '[' 
                        for item in colRecordSet:
                            domStr = domStr + item + ','
                        domStr = domStr[0:-1] + ']'
                        entityFunctor.domain = domStr
                        #entityFunctor.domain = str(list(colRecordSet))
                    elif column['type'] == 'continuous':
                        entityFunctor.variableType = 'continuous'
                        entityFunctor.domain = '[]'
                    else:
                        pass
                else:
                    pass
                functors.append(entityFunctor)
            for functor in functors:
                aDict = dict()
                aDict['direction'] = None
                aDict['functors'] = functors
                functor.relateTo = dict()
                functor.relateTo['none'] = aDict
                self.allFunctors.append(functor)
        for fKey in foreignKeys:
            lTableName, foreignK = fKey[0]
            rTableName, primaryK = fKey[1]
            lFunctors = []
            for functor in self.allFunctors:
                if functor.tableName == lTableName:
                    lFunctors.append(functor)
            rFunctors = []
            for functor in self.allFunctors:
                if functor.tableName == rTableName:
                    rFunctors.append(functor)
            for functor in self.allFunctors:
                lDict = dict()
                lDict['direction'] = 'plus'
                lDict['functors'] = rFunctors
                rDict = dict()
                rDict['direction'] = 'minus'
                rDict['functors'] = lFunctors
                if functor.tableName == lTableName:
                    functor.relateTo[foreignK] = lDict
                    functor.relateTo['tableNameWithPrimaryKey'] = rTableName
                elif functor.tableName == rTableName:
                    functor.relateTo[foreignK] = rDict
                else:
                    pass
    
if __name__ == '__main__':
    pass

    
    
    
    