'''
Created on Oct 4, 2018

@author: nitesh
'''
import numpy as np
import time, math, sys
from core.TranslateToDC import TranslateToDC
from sklearn.metrics import confusion_matrix
from sklearn.metrics import accuracy_score
from sklearn.metrics import roc_auc_score
from core.DCLearner import DCLearner
#import core.DCLearner.DCLearner as DCLearner

from Experiment1 import Experiment1b
from Settings import RANDOM_VARIABLE_PREDICATE_TYPE, RANDOM_VARIABLE_PREDICATE, RANDOM_VARIABLE_PREDICATE_RANGE,\
    DECLARATIVE_BIAS, DECLARATIVE_BIAS_ORDERED, PROLOG_PROGRAM_HEADER, DC_PROGRAM_HEADER, \
    RELATIONAL_CHAIN_OF_PREDICATES, RELATIONAL_PREDICATES, PROLOG_MARKOV_BLANKET_INTERVENTIONS,\
    DC_PROGRAM_DATA_DEPENDENT_RELATION_PROBABILISTIC, MISSING_ATTRIBUTES_FILLER, RANDOM_VARIABLE_PREDICATE_INITIAL_VAL

class StochasticEM():  
    def __init__(self, databaseName, numSamples, numEMIterations):
        self.DATABASE_NAME = databaseName
        self.numOfSamples = numSamples
        self.numOfEMIterations = numEMIterations
        self.ERROR = dict()
        self.YPRED = dict()
        self.YTRUE = dict()
        self.YPREDROC = dict()
        self.YTRUEROC = dict()
        self.WPLL = dict()
        self.NUM_OF_ELEMENTS_IN_TEST = dict()
        self.WRMSE= dict()
        self.AUCROC = dict()
        self.WPLL_TOTAL = dict()
        self.samplesOfMissingFields = []
        self.evidenceListString = ''
        self.queryListString = ''
        self.variableNameListString = ''
        self.exp1Obj = Experiment1b()
        self.exp1Obj.databaseName = databaseName

    def assertDCTrainFacts(self, facts, yapObj):
        for item in facts:
            item = self.exp1Obj.findListOfDCFacts([item])[0]
            item = item[0:-1]
            [head, distribution, body] = self.splitDCToHeadDistributionBody(item)
            if distribution == '':
                query = 'asserta(user:hardclause(' + head + ',' + '(' + body + ')' + ',0)).'
            else:
                query = 'asserta(user:distributionalclause(' + head + ',' + distribution + ',' + '(' + body + ')' + ',0)).'
            success = yapObj.executeQuery(query)
            if success == False:
                raise Exception('Yap execute failed!')
            
    def retractDCTrainFacts(self, facts, yapObj):
        for item in facts:
            item = self.exp1Obj.findListOfDCFacts([item])[0]
            item = item[0:-1]
            [head, distribution, body] = self.splitDCToHeadDistributionBody(item)
            if distribution == '':
                query = 'retract(user:hardclause(' + head + ',' + '(' + body + ')' + ',0)).'
            else:
                query = 'retract(user:distributionalclause(' + head + ',' + distribution + ',' + '(' + body + ')' + ',0)).'
            success = yapObj.executeQuery(query)
            if success == False:
                raise Exception('Yap execute failed!')

    def assertPrologFactsWithEvidencePredicate(self, facts, yapObj):
        for fact in facts:
            fact = fact[0:-1]
            query = 'asserta(evidence(' + fact + '))'
            success = yapObj.executeQuery(query)
            if success == False:
                raise Exception('Yap execute failed!')
                
    def retractPrologFactsWithEvidencePredicate(self, facts, yapObj):
        for fact in facts:
            fact = fact[0:-1]
            query = 'retract(evidence(' + fact + '))'
            success = yapObj.executeQuery(query)
            if success == False:
                raise Exception('Yap execute failed!')

    def assertRulesOfMissingValue(self, yapObj):
        rules = MISSING_ATTRIBUTES_FILLER[self.DATABASE_NAME]
        rules = rules.split('\n')
        for rule in rules:
            if rule == '':
                pass
            else:
                rule = rule.replace(' ', '')
                rule = rule[0:-1]
                self.assertRule(yapObj, rule)

    def retractRulesOfMissingValue(self, yapObj):
        rules = MISSING_ATTRIBUTES_FILLER[self.DATABASE_NAME]
        rules = rules.split('\n')
        for rule in rules:
            if rule == '':
                pass
            else:
                rule = rule.replace(' ', '')
                rule = rule[0:-1]
                self.retractRule(yapObj, rule)

    def assertRule(self, yapObj, rule):
        query = 'asserta((' + rule + ')).'
        success = yapObj.executeQuery(query)
        if success == False:
            raise Exception('Yap execute failed!')
    
    def retractRule(self, yapObj, rule):
        rule = rule.split(':-')
        query = 'retractARule(' + rule[0] + ',' + '(' + rule[1] + ')' + ').'
        success = yapObj.executeQuery(query)
        if success == False:
            raise Exception('Yap execute failed!')
    
        
    def assertPrologTrainFacts(self, facts, yapObj):
        for fact in facts:
            fact = fact[0:-1]
            query = 'asserta(' + fact + ')'
            success = yapObj.executeQuery(query)
            if success == False:
                raise Exception('Yap execute failed!')

    def assertPrologTrainFactsAtEnd(self, facts, yapObj):
        for fact in facts:
            fact = fact[0:-1]
            query = 'assertz(' + fact + ')'
            success = yapObj.executeQuery(query)
            if success == False:
                raise Exception('Yap execute failed!')
    
    def retractPrologTrainFacts(self, facts, yapObj):
        for fact in facts:
            fact = fact[0:-1]
            query = 'retract(' + fact + ')'
            success = yapObj.executeQuery(query)
            if success == False:
                raise Exception('Yap execute failed!')
    
    # If flag is true then DC only samples and does not find weights of the samples
    def setDCToOnlySample(self, yapObj, flag):
        query = 'set_onlySample(' + flag + ')'
        success = yapObj.executeQuery(query)
        if success == False:
            raise Exception('Yap execute failed!')

    def attributeRelatedPredicates(self):
        attributeRelatedPredicates = RANDOM_VARIABLE_PREDICATE[self.DATABASE_NAME]
        relationRelatedPredicates = RELATIONAL_PREDICATES[self.DATABASE_NAME]
        attributeRelatedPredicates = list(set(attributeRelatedPredicates)-set(relationRelatedPredicates))
        return attributeRelatedPredicates

    def relationRelatedPredicates(self):
        return RELATIONAL_PREDICATES[self.DATABASE_NAME]
    
    def splitDCToHeadDistributionBody(self, dcString):
        rule = dcString.split(':=')
        if ':=' in dcString:
            body = rule[1]
        else:
            body = 'true'
        if '~' in rule[0]:
            rule[0] = rule[0].split('~')
            head = rule[0][0]
            distribution = rule[0][1]
        else:
            head = rule[0]
            distribution = ''
        return [head, distribution, body]
    
    def updateEvaluationMericList(self, samples, featureName, originalValue, ty):
        if ty == 'continuous':
            if len(samples) == 0:
                pass
            else:
                average = 0
                for sample in samples:
                    average += sample[0][0]*sample[3]
                self.ERROR[featureName].append((average - float(originalValue))*(average - float(originalValue)))
                self.NUM_OF_ELEMENTS_IN_TEST[featureName] = self.NUM_OF_ELEMENTS_IN_TEST[featureName] + 1
                return average
        else:
            if len(samples) == 0:
                pass
            else:
                counts = {}
                for sample in samples:
                    if counts.has_key(sample[0][0]):
                        counts[sample[0][0]] += sample[3]
                    else:
                        counts[sample[0][0]] = sample[3]
                sumOfAllCounts = 0
                for key in counts.keys():
                    sumOfAllCounts += counts[key]
                yPred = counts.keys()[0]
                yVal = counts[yPred]/float(sumOfAllCounts)
                for key in counts.keys():
                    if counts[key]/float(sumOfAllCounts) > yVal:
                        yPred = key
                        yVal = counts[key]/float(sumOfAllCounts)
                
                predList = []
                predProb = []
                for ele in RANDOM_VARIABLE_PREDICATE_RANGE[self.DATABASE_NAME][featureName]:
                    if ele == originalValue:
                        predList.append(1)
                    else:
                        predList.append(0)
                    if counts.has_key(ele):
                        predProb.append(counts[ele]/sumOfAllCounts)
                    else:
                        predProb.append(0.0)
                self.YTRUEROC[featureName].append(predList)
                self.YPREDROC[featureName].append(predProb)
                self.YPRED[featureName].append(yPred)
                self.YTRUE[featureName].append(originalValue)
                self.NUM_OF_ELEMENTS_IN_TEST[featureName] = self.NUM_OF_ELEMENTS_IN_TEST[featureName] + 1
                return yPred
             
    def flushPredictionDataStructures(self):
        self.ERROR = dict()
        self.YPRED = dict()
        self.YTRUE = dict()
        self.YPREDROC = dict()
        self.YTRUEROC = dict()
        self.NUM_OF_ELEMENTS_IN_TEST = dict()
        
    def findTestAccuracy(self, yapObj, testFacts, listOfTargetPredicates, dcRules, prologDependencyStructure):
        attributeRelatedPredicates = self.attributeRelatedPredicates()
        for predicate in listOfTargetPredicates:
            self.ERROR[predicate] = []
            self.YPRED[predicate] = []
            self.YTRUE[predicate] = []
            self.YTRUEROC[predicate] = []
            self.YPREDROC[predicate] = []
            self.NUM_OF_ELEMENTS_IN_TEST[predicate] = 0
    
        testRelationalFacts = [] 
        for item in testFacts:
            tName = self.exp1Obj.getPredicateName(item)
            if tName in attributeRelatedPredicates:
                pass
            else:
                testRelationalFacts.append(item)
        self.assertDCTrainFacts(testRelationalFacts, yapObj)
        self.assertPrologFactsWithEvidencePredicate(testFacts, yapObj)
        self.assertRulesOfMissingValue(yapObj)
        for bt in dcRules:
            bt = bt[0:-1]
            [head, distribution, body] = self.splitDCToHeadDistributionBody(bt)
            query = 'assertz(user:distributionalclause(' + head + ',' + distribution + ',' + '(' + body + ')' + ',0)).'
            success = yapObj.executeQuery(query)
            if success == False:
                raise Exception('Yap execute failed!')
        for dependency in prologDependencyStructure:
            dependency = dependency[0:-1]
            dependency = dependency[0:-1]
            self.assertRule(yapObj, dependency)
        
        for featName in listOfTargetPredicates:
            tempTestFacts = []
            for tFacts in testFacts:
                if featName not in tFacts:
                    pass
                else:
                    tempTestFacts.append(tFacts)
                    
            ty = RANDOM_VARIABLE_PREDICATE_TYPE[self.DATABASE_NAME][featName]
            for i in range(0, len(tempTestFacts)): 
                feat = tempTestFacts[i]
                feat = self.exp1Obj.dcPredicateAndValue(feat)
                variable = '[X]'
                actualValue = feat[1]
                ## Query
                query = '(' + feat[0] + ' ~= X)'
                ## Evidence
                evidenceTest = ''
                tempTestFact = tempTestFacts[i][0:-1]
                res = yapObj.requisiteEvidence(tempTestFact)
                for ele in res['ProbEvidence']:
                    evidence = ele + '.'
                    tName = self.exp1Obj.getPredicateName(evidence)
                    if tName in attributeRelatedPredicates:
                        feat = self.exp1Obj.dcPredicateAndValue(evidence)
                        if evidenceTest == '':
                            evidenceTest = evidenceTest + feat[0] + '~=' + feat[1]
                        else:
                            evidenceTest = evidenceTest + ',' + feat[0] + '~=' + feat[1]
                for item in res['Intervention']:
                    tName = self.exp1Obj.getPredicateName(item)
                    item = item + '.'
                    item = self.exp1Obj.findListOfDCFacts([item])[0]
                    item = item[0:-1]
                    if tName in attributeRelatedPredicates:
                        self.exp1Obj.interventionAssert(item, yapObj)
                result = yapObj.queryWithSamples(self.numOfSamples, query, evidenceTest, variable)
                samples = result['samples']
                samplesWithProb = self.exp1Obj.generateSamplesWithProb(samples, yapObj)
                predictedValue = self.updateEvaluationMericList(samplesWithProb, featName, actualValue, ty)
                print query, actualValue, predictedValue, res['Intervention'], res['ProbEvidence']
                for item in res['Intervention']:
                    tName = self.exp1Obj.getPredicateName(item)
                    item = item + '.'
                    item = self.exp1Obj.findListOfDCFacts([item])[0]
                    item = item[0:-1]
                    if tName in attributeRelatedPredicates:
                        self.exp1Obj.interventionRetract(item, yapObj)
        for predicate in listOfTargetPredicates:
            ty = RANDOM_VARIABLE_PREDICATE_TYPE[self.DATABASE_NAME][predicate] 
            #logging.info('\n \n \n' + '$$$$$$$$$$ Results for %s $$$$$$$$$$' % (predicate))
            print('\n \n \n' + '$$$$$$$$$$ Results for %s $$$$$$$$$$' % (predicate))
            if ty == 'continuous':
                acc = float('nan')
                if self.NUM_OF_ELEMENTS_IN_TEST[predicate] == 0:
                    #logging.info('WRMSE = %f, Number of tests consider = %d' % (acc, NUM_OF_ELEMENTS_IN_TEST[predicate]))
                    print('WRMSE = %f, Number of tests consider = %d' % (acc, self.NUM_OF_ELEMENTS_IN_TEST[predicate]))
                else:
                    wrmse = math.sqrt(sum(self.ERROR[predicate])/float(self.NUM_OF_ELEMENTS_IN_TEST[predicate]))/float(RANDOM_VARIABLE_PREDICATE_RANGE[self.DATABASE_NAME][predicate])
                    if self.WRMSE.has_key(predicate):
                        tempList = self.WRMSE[predicate]
                        tempList.append(wrmse)
                    else:
                        self.WRMSE[predicate] = [wrmse]
                    #logging.info('WRMSE = %f, Number of tests consider = %d' % (wrmse, NUM_OF_ELEMENTS_IN_TEST[predicate]))
                    print('WRMSE = %f, Number of tests consider = %d' % (wrmse, self.NUM_OF_ELEMENTS_IN_TEST[predicate]))
            else:
                uniqueLabel = list(set(self.YPRED[predicate] + self.YTRUE[predicate]))
                #logging.info ('Confusion Matrix:')
                print('Confusion Matrix:')
                #logging.info(confusion_matrix(YTRUE[predicate], YPRED[predicate], labels=uniqueLabel))
                print(confusion_matrix(self.YTRUE[predicate], self.YPRED[predicate], labels=uniqueLabel))
                #logging.info('Labels: '  + str(uniqueLabel))
                print('Labels: '  + str(uniqueLabel))
                if self.NUM_OF_ELEMENTS_IN_TEST[predicate] == 0:
                    #logging.info('Accuracy: nan')
                    print('Accuracy: nan')
                    #logging.info('Area Under ROC: prediction list is empty')
                    print('Area Under ROC: prediction list is empty')
                else:
                    #logging.info('Accuracy: %f'  % accuracy_score(YTRUE[predicate], YPRED[predicate]))
                    print('Accuracy: %f'  % accuracy_score(self.YTRUE[predicate], self.YPRED[predicate]))
                    try:
                        auROC = roc_auc_score(np.array(self.YTRUEROC[predicate]), np.array(self.YPREDROC[predicate]), average='weighted')
                        if self.AUCROC.has_key(predicate):
                            tempList = self.AUCROC[predicate]
                            tempList.append(auROC)
                        else:
                            self.AUCROC[predicate] = [auROC]
                        #logging.info('Area Under ROC: %f' % auROC)
                        print('Area Under ROC: %f' % auROC)
                    except ValueError:
                        #logging.info('Only one class present in y_true, Area Under ROC: %f' % 1)
                        print('Only one class present in y_true, Area Under ROC: %f' % 1)
        self.flushPredictionDataStructures()
        self.retractDCTrainFacts(testRelationalFacts, yapObj)
        self.retractPrologFactsWithEvidencePredicate(testFacts, yapObj)
        self.retractRulesOfMissingValue(yapObj)
        for bt in dcRules:
            bt = bt[0:-1]
            [head, distribution, body] = self.splitDCToHeadDistributionBody(bt)
            query = 'retract(user:distributionalclause(' + head + ',' + distribution + ',' + '(' + body + ')' + ',0)).'
            success = yapObj.executeQuery(query)
            if success == False:
                raise Exception('Yap execute failed!')
        for dependency in prologDependencyStructure:
            dependency = dependency[0:-1]
            dependency = dependency[0:-1]
            self.retractRule(yapObj, dependency)

                
    def getDependencyStructureList(self, obj):
        prologDependencyStructureList = []
        prologDependencyStructureList.append(obj.generateDependencyStructureOfRules(obj.rulesFragmented))
        basePredicates = obj.generateBasePredicates()
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
                    chainOfRelations = RELATIONAL_CHAIN_OF_PREDICATES[self.DATABASE_NAME].keys()
                    for rel in chainOfRelations:
                        if rel in uniqueAtoms:
                            chain = RELATIONAL_CHAIN_OF_PREDICATES[self.DATABASE_NAME][rel]
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

    def generateSamplesOfMissingFields(self, yapObj, missingFacts):
        samples = []
        var = '[' + self.variableNameListString + ']'
        query = '(' + self.queryListString + ')'
        evidence = self.evidenceListString
        sample = '[]'
        while sample == '[]':
            print 'Sampling a joint state'
            result = yapObj.queryWithSamples(1, query, evidence, var)
            print 'Successfully sampled a joint state'
            sample = result['samples']
            time.sleep(10)
        sampleList = self.exp1Obj.getSampleVal(result, yapObj)
        for i in range(0, len(missingFacts)):
            fact = missingFacts[i]
            val = sampleList[i]
            try:
                fValue = float(val)
                val = str(round(fValue,8))
            except ValueError:
                pass
            fact = fact[0:-1] + ',' + str(val) + ').'
            samples.append(fact)
        return samples
    
    def generateInitialMissingFields(self, missingFacts):
        initializedMissingFields = []
        for item in missingFacts:
            featName = self.exp1Obj.getPredicateName(item)
            initValRange = RANDOM_VARIABLE_PREDICATE_INITIAL_VAL[self.DATABASE_NAME][featName]
            initValRange = str(initValRange)
            fact = item[0:-1] + ',' + str(initValRange) + ').'
            initializedMissingFields.append(fact)
        return initializedMissingFields

    def em(self, treeLearnerObject, translateRuleObject, missingFacts, trainRelationalFacts, trainRandVarFacts, treeOutputFileTraining):
        oldBackgroundTheory = []
        for j in range(0, self.numOfEMIterations):
            backgroundTheory = {'theory': [], 'dependency': []}
            treeLearnerObject.rules = []
            treeLearnerObject.rulesFragmented = []
            if oldBackgroundTheory != []:
                for bt in oldBackgroundTheory['theory']:
                    bt = bt[0:-1]
                    [head, distribution, body] = self.splitDCToHeadDistributionBody(bt)
                    query = 'assertz(user:distributionalclause(' + head + ',' + distribution + ',' + '(' + body + ')' + ',0)).'
                    success = treeLearnerObject.interface.executeQuery(query)
                    if success == False:
                        raise Exception('Yap execute failed!')
                global samplesOfMissingFields
                samplesOfMissingFields = self.generateSamplesOfMissingFields(treeLearnerObject.interface, missingFacts)
                for bt in oldBackgroundTheory['theory']:
                    bt = bt[0:-1]
                    [head, distribution, body] = self.splitDCToHeadDistributionBody(bt)
                    query = 'retract(user:distributionalclause(' + head + ',' + distribution + ',' + '(' + body + ')' + ',0)).'
                    success = treeLearnerObject.interface.executeQuery(query)
                    if success == False:
                        raise Exception('Yap execute failed!')
                self.assertPrologTrainFactsAtEnd(samplesOfMissingFields, treeLearnerObject.interface)
                treeLearnerObject.runProbMode = False
                treeLearnerObject.learnRules()
                print 'Complete Log-Likelihood for EM case = ' + str(treeLearnerObject.completeteLikelihood) + '\n'
                self.retractPrologTrainFacts(samplesOfMissingFields, treeLearnerObject.interface)
            else:
                treeLearnerObject.runProbMode = False
                self.assertPrologTrainFactsAtEnd(trainRelationalFacts, treeLearnerObject.interface)
                self.assertPrologTrainFactsAtEnd(trainRandVarFacts, treeLearnerObject.interface)
                initialMissingFields = self.generateInitialMissingFields(missingFacts)
                self.assertPrologTrainFactsAtEnd(initialMissingFields, treeLearnerObject.interface)
                treeLearnerObject.learnRules()
                print 'Complete Log-Likelihood for EM case = ' + str(treeLearnerObject.completeteLikelihood) + '\n'
                self.retractPrologTrainFacts(initialMissingFields, treeLearnerObject.interface)
                self.assertDCTrainFacts(trainRelationalFacts, treeLearnerObject.interface)
                evidenceTest = ''
                
                for item in trainRandVarFacts:
                    feat = self.exp1Obj.dcPredicateAndValue(item)
                    if evidenceTest == '':
                        evidenceTest = evidenceTest + feat[0] + ' ~= ' + feat[1]
                    else:
                        evidenceTest = evidenceTest + ',' + feat[0] + ' ~= ' + feat[1]
                baseVar = 'X'
                variables = ''
                queryTest = ''
                for i in range(0, len(missingFacts)):
                    newVar = baseVar + str(i+1)
                    if variables == '':
                        variables = variables + newVar
                    else:
                        variables = variables + ',' + newVar
                    if queryTest == '':
                        queryTest = queryTest + missingFacts[i] + ' ~= ' + newVar
                    else:
                        queryTest = queryTest + ',' + missingFacts[i] + ' ~= ' + newVar
                self.evidenceListString = evidenceTest
                self.queryListString = queryTest
                self.variableNameListString = variables
            for rule in treeLearnerObject.rules:
                rule = translateRuleObject.translate(rule)
                backgroundTheory['theory'].append(rule)
            dependencyStructureList = self.getDependencyStructureList(treeLearnerObject)
            backgroundTheory['dependency'] = dependencyStructureList
            
            oldBackgroundTheory = backgroundTheory
            f = open(treeOutputFileTraining + str(j), 'w')
            for rule in oldBackgroundTheory['theory']:
                f.write(rule + '\n')
            f.write('\n\n')
            for rule in oldBackgroundTheory['dependency']:
                f.write(rule)
            f.close()
        self.retractPrologTrainFacts(trainRelationalFacts, treeLearnerObject.interface)
        self.retractPrologTrainFacts(trainRandVarFacts, treeLearnerObject.interface)
        self.retractDCTrainFacts(trainRelationalFacts, treeLearnerObject.interface)
        return oldBackgroundTheory

    
    def generateDCPrologFile(self, prologTrainingFileName, dcTrainingFile, mode, targetVariable=[]):
        if mode == 'em':
            declarativeBias = DECLARATIVE_BIAS_ORDERED[self.DATABASE_NAME]
        else:
            declarativeBias = DECLARATIVE_BIAS[self.DATABASE_NAME]
        if targetVariable == []:
            pass
        else:
            declarativeBias = ''
            targetVariableStringList = []
            for i in targetVariable:
                targetVariableStringList.append('learn(' + i)
            if mode == 'em':
                declarativeBiasTemp = DECLARATIVE_BIAS_ORDERED[self.DATABASE_NAME].split('\n')
            else:
                declarativeBiasTemp = DECLARATIVE_BIAS[self.DATABASE_NAME].split('\n')
            
            learnDefinitionStarts = False
            for line in declarativeBiasTemp:
                if '%Target' in line and not learnDefinitionStarts:
                    declarativeBias = declarativeBias + line + '\n'
                    learnDefinitionStarts = True
                elif learnDefinitionStarts:
                    for i in targetVariableStringList:
                        if i in line:
                            declarativeBias = declarativeBias + line + '\n'
                            break
                        else:
                            pass
                else:
                    declarativeBias = declarativeBias + line + '\n'
        f = open(prologTrainingFileName, 'w')
        f.write(PROLOG_PROGRAM_HEADER)
        f.write(declarativeBias)
        f.write(PROLOG_MARKOV_BLANKET_INTERVENTIONS)
        f.close()
        f = open(dcTrainingFile, 'w')
        f.write(DC_PROGRAM_HEADER)
        f.write(DC_PROGRAM_DATA_DEPENDENT_RELATION_PROBABILISTIC[self.DATABASE_NAME])
        f.close()

    def experimentRunner(self, mode, trainingFolder, validateFoldString, testFolder, percentageMissing, prologTrainingFileName, dcTrainingFile, treeOutputFileTraining, testPredicate, listOfMissingPredicate):
        countsPerPredicate = dict()
        missingFactsDict = dict()
        intialNum = 0
        for i in listOfMissingPredicate:
            countsPerPredicate[i] = intialNum
            missingFactsDict[i] = []
            intialNum += 1
        trainFacts = self.exp1Obj.getListOfPredicatesInFolder(trainingFolder)
        trainFacts = trainFacts + self.exp1Obj.getListOfPredicatesInFolder(validateFoldString)
        testFacts = self.exp1Obj.getListOfPredicatesInFolder(testFolder)
        trainFacts = list(dict.fromkeys(trainFacts))
        testFacts = list(dict.fromkeys(testFacts))
        #experiment3c_logger.info('\nPercentage Missing = %d' % (percentageMissing))
        print('\nPercentage Missing = %d' % (percentageMissing))
        missingFactsTotal = []
        for missingPredicate in listOfMissingPredicate:
            newTrainFacts = []
            missingFacts = []
            oNum = int((100 - percentageMissing)/float(10))
            for i in range(0, len(trainFacts)):
                missingPredicateName = ''
                for ele in [missingPredicate]:
                    if ele+'(' in trainFacts[i]:
                        missingPredicateName = ele
                if missingPredicateName != '':
                    num = countsPerPredicate[missingPredicateName]
                    if num%10 >= oNum:
                        missingFacts.append(trainFacts[i])
                        missingFactsTotal.append(trainFacts[i])
                    else:
                        newTrainFacts.append(trainFacts[i])
                    countsPerPredicate[missingPredicateName] = num+1
                else:
                    newTrainFacts.append(trainFacts[i])
            missingFactsDict[missingPredicate] = missingFacts
        newTrainFacts = []
        for i in range(0, len(trainFacts)):
            if trainFacts[i] in missingFactsTotal:
                pass
            else:
                newTrainFacts.append(trainFacts[i])
        
        missingFactsTotalWithoutVal = []
        for item in missingFactsTotal:
            feat = self.exp1Obj.dcPredicateAndValue(item)
            missingFactsTotalWithoutVal.append(feat[0])
        
        trainRelationalFacts = [] 
        trainRandVarFacts = []
        
        for item in newTrainFacts:
            tName = self.exp1Obj.getPredicateName(item)
            if tName in self.attributeRelatedPredicates():
                trainRandVarFacts.append(item)
            else:
                trainRelationalFacts.append(item)

        if mode == 'em':
            self.generateDCPrologFile(prologTrainingFileName, dcTrainingFile, mode, targetVariable=RANDOM_VARIABLE_PREDICATE[self.DATABASE_NAME])
            treeLearnerObject = DCLearner(prologTrainingFileName, dcTrainingFile, '', RANDOM_VARIABLE_PREDICATE[self.DATABASE_NAME])
            translateRuleObject = TranslateToDC()
            translateRuleObject.setRandomVariablePredicates(RANDOM_VARIABLE_PREDICATE[self.DATABASE_NAME])
            self.setDCToOnlySample(treeLearnerObject.interface, 'true')
            newBackgroundTheory = self.em(treeLearnerObject, translateRuleObject, missingFactsTotalWithoutVal, trainRelationalFacts, trainRandVarFacts, treeOutputFileTraining)
            self.setDCToOnlySample(treeLearnerObject.interface, 'false')
            print('\n\n\nRuns over EM of missing values')
            self.findTestAccuracy(treeLearnerObject.interface, testFacts, [testPredicate], newBackgroundTheory['theory'], newBackgroundTheory['dependency'])
        elif mode == 'partial':
            self.generateDCPrologFile(prologTrainingFileName, dcTrainingFile, mode, targetVariable=[testPredicate])
            obj = DCLearner(prologTrainingFileName, dcTrainingFile, '', RANDOM_VARIABLE_PREDICATE[self.DATABASE_NAME])
            self.setDCToOnlySample(obj.interface, 'false')
            self.assertPrologTrainFactsAtEnd(newTrainFacts, obj.interface)
            obj.runProbMode = False
            obj.learnRules()
            print 'Complete Log-Likelihood for partial case = ' + str(obj.completeteLikelihood) + '\n'
            self.retractPrologTrainFacts(newTrainFacts, obj.interface)
            obj1 = TranslateToDC()
            obj1.setRandomVariablePredicates(RANDOM_VARIABLE_PREDICATE[self.DATABASE_NAME])
            theory = {'theory': [], 'dependency': []}   
            f = open(treeOutputFileTraining, 'w')
            for rule in obj.rules:
                rule = obj1.translate(rule)
                theory['theory'].append(rule)
                f.write(rule + '\n')
            f.write('\n\n')
            dependencyStructureList = self.getDependencyStructureList(obj)
            for dep in dependencyStructureList:
                f.write(dep)
            f.close()
            theory['dependency'] = dependencyStructureList
            print('\n\n\nRuns over missing values')
            self.findTestAccuracy(obj.interface, testFacts, [testPredicate], theory['theory'], theory['dependency'])
        elif mode == 'complete':
            self.generateDCPrologFile(prologTrainingFileName, dcTrainingFile, mode, targetVariable=[testPredicate])
            obj = DCLearner(prologTrainingFileName, dcTrainingFile, '', RANDOM_VARIABLE_PREDICATE[self.DATABASE_NAME])
            self.setDCToOnlySample(obj.interface, 'false')
            self.assertPrologTrainFactsAtEnd(trainFacts, obj.interface)
            obj.runProbMode = False
            obj.learnRules()
            print 'Complete Log-Likelihood for complete case = ' + str(obj.completeteLikelihood) + '\n'
            self.retractPrologTrainFacts(trainFacts, obj.interface)
            obj1 = TranslateToDC()
            obj1.setRandomVariablePredicates(RANDOM_VARIABLE_PREDICATE[self.DATABASE_NAME])
            theory = {'theory': [], 'dependency': []}  
            f = open(treeOutputFileTraining, 'w') 
            for rule in obj.rules:
                rule = obj1.translate(rule)
                theory['theory'].append(rule)
                f.write(rule + '\n')
            f.write('\n\n')
            dependencyStructureList = self.getDependencyStructureList(obj)
            for dep in dependencyStructureList:
                f.write(dep)
            f.close()
            theory['dependency'] = dependencyStructureList
            print('\n\n\nRuns over complete values')
            self.findTestAccuracy(obj.interface, testFacts, [testPredicate], theory['theory'], theory['dependency'])
        else:
            pass


if __name__ == "__main__":
    
    print sys.argv
    #databaseName = sys.argv[1]
    databaseName = 'financial'
    
    #numSamples = int(sys.argv[2])
    numSamples = 1000
    
    #numEMIterations = int(sys.argv[3])
    numEMIterations = 1
    
    #mode = sys.argv[4]
    mode = 'em' ## 'em', 'partial', 'complete'
    
    #trainFoldString = sys.argv[5]
    trainFoldString = '/home/nitesh/eclipse-workspace/DistributionalProgramSynthesis/data/PKDD/pkdd/Fold2/train'
    
    #validateFoldString = sys.argv[6]
    validateFoldString = '/home/nitesh/eclipse-workspace/DistributionalProgramSynthesis/data/PKDD/pkdd/Fold2/validate'
    
    #testFolderString = sys.argv[7]
    testFolderString = '/home/nitesh/eclipse-workspace/DistributionalProgramSynthesis/data/PKDD/pkdd/Fold2/test'
    
    #percentageMissing = int(sys.argv[8])
    percentageMissing = 10
    
    #prologTrainingFileName = sys.argv[9]
    prologTrainingFileName = '../data/Financial3c.pl'
    
    #dcTrainingFile = sys.argv[10]
    dcTrainingFile = '../data/FinancialDC3c.pl'
    
    #treeOutputFileTraining = sys.argv[11]
    treeOutputFileTraining = '../data/EnsembleOfDLTs3c.pl'
    
    #testPredicate = sys.argv[12]
    testPredicate = 'monthlyPayments'
    
    #listOfMissingPredicate = ['avgSumOfInc', 'loanStatus', 'loanAmount', 'gender', 'age', 'avgNrWith', 'monthlyPayments', 'avgSumOfW', 'freq', 'stdMonthInc', 'stdMonthW', 'avgSalary', 'ratUrbInhab']
    #listOfMissingPredicate = ['avgSumOfInc', 'loanStatus', 'loanAmount', 'gender', 'age', 'avgNrWith', 'monthlyPayments', 'avgSumOfW', 'freq', 'stdMonthInc', 'stdMonthW']
    listOfMissingPredicate = ['avgSumOfInc', 'loanStatus', 'loanAmount', 'gender', 'age', 'avgNrWith', 'monthlyPayments', 'avgSumOfW', 'freq', 'stdMonthInc', 'stdMonthW', 'avgSalary', 'ratUrbInhab']
    
    stochasticEMObj = StochasticEM(databaseName, numSamples, numEMIterations)
    stochasticEMObj.experimentRunner(mode, trainFoldString, validateFoldString, testFolderString, percentageMissing, prologTrainingFileName, dcTrainingFile, treeOutputFileTraining, testPredicate, listOfMissingPredicate)

    
    



