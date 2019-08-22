'''
This experiment compares the joint model program learned with HRDNs and individual relational trees.

@author: nitesh
'''
import scipy.stats
import logging, time
import statistics
import math
from os import listdir
from os.path import isfile, join
from core.YapPrologInterface import YapPrologInterface
from core.DCLearner import DCLearner
from core.TranslateToDC import TranslateToDC
from sklearn.metrics import confusion_matrix
from sklearn.metrics import accuracy_score
import numpy as np
from sklearn.metrics import roc_auc_score

from Settings import RELATIONAL_PREDICATES, DECLARATIVE_BIAS_ORDERED, PROLOG_PROGRAM_HEADER, RANDOM_VARIABLE_PREDICATE, DC_PROGRAM_HEADER, DC_PROGRAM_DATA_DEPENDENT_RELATION, RANDOM_VARIABLE_PREDICATE_RANGE, RANDOM_VARIABLE_PREDICATE_TYPE, RELATIONAL_CHAIN_OF_PREDICATES, PROLOG_MARKOV_BLANKET_INTERVENTIONS, MISSING_ATTRIBUTES_FILLER ,\
    DYNAMIC

FOLD = 1
DATA_DIRECTORY = '/home/nitesh/eclipse-workspace/DistributionalProgramSynthesis/data/PKDD/pkdd'
PROCESSED_TRAIN_DATA_FILE = '../data/Financial.pl'
PROCESSED_TRAIN_DATA_FILE_DC = '../data/FinancialDC.pl'
DATABASE_NAME = 'financial'

#DATA_DIRECTORY = '/home/nitesh/eclipse-workspace/DistributionalProgramSynthesis/data/University/data800x125x125'
#PROCESSED_TRAIN_DATA_FILE = '../data/University.pl'
#PROCESSED_TRAIN_DATA_FILE_DC = '../data/UniversityDC.pl'
#DATABASE_NAME = 'university'

OUTPUT_ENSEMBLE_OF_DLTS = '../data/EnsembleOfDLTs.pl'


DC_RULES = []
TRAIN_FACTS = []
TEST_FACTS = []

foldString = "Fold" + str(FOLD)


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

logging.basicConfig(level=logging.INFO, filename="Experiment1", filemode="w", format="%(asctime)-15s %(levelname)-8s %(message)s")


class Experiment1b():  

    def __init__(self):
        self.databaseName = DATABASE_NAME
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
                    if predicateName in RELATIONAL_PREDICATES[self.databaseName]:
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
                        tempDat = dat
                        tempDat = tempDat.split(',')
                        value = tempDat[-1]
                        value = value.replace(').', '')
                        try:
                            fValue = float(value)
                            value = str(round(fValue,8))
                        except ValueError:
                            pass
                        tempDat[-1] = value + ').'
                        dat = ','.join(tempDat)
                        datList.append(dat)
                    listOfFacts = listOfFacts + datList
        return listOfFacts

    def generatePrologFile(self, filename, trainingData, targetVariable=[]):
        declarativeBias = DECLARATIVE_BIAS_ORDERED[self.databaseName]
        if targetVariable == []:
            pass
        else:
            declarativeBias = ''
            targetVariableStringList = []
            for i in targetVariable:
                targetVariableStringList.append('learn(' + i)
            declarativeBiasTemp = DECLARATIVE_BIAS_ORDERED[self.databaseName].split('\n')
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
            if featName in RANDOM_VARIABLE_PREDICATE[self.databaseName]:
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
            
    def generateDCFile(self, filename, trainingData, testData, interventions, dcRules, prologDependencyStructure):
        f = open(filename, 'w')
        f.write(DC_PROGRAM_HEADER)
        f.write(DYNAMIC[self.databaseName])
        f.write(PROLOG_MARKOV_BLANKET_INTERVENTIONS)
        f.write(DC_PROGRAM_DATA_DEPENDENT_RELATION[self.databaseName])
        f.write(MISSING_ATTRIBUTES_FILLER[self.databaseName])
        #trainingDataDC = self.findListOfDCFacts(trainingData)
        #testDataDC = self.findListOfDCFacts(testData)
        #f.write("\n\n%Train data prolog\n")
        #for item in trainingData:
        #    f.write(item + '\n')
        f.write("\n\n%Test data prolog\n")
        for item in testData:
            tempItem = item[0:-1]
            f.write('evidence(' + tempItem +').' + '\n')
        
        f.write("\n\n%Dependency Structure\n")
        f.write(prologDependencyStructure)
        #f.write("\n\n%Train data\n")
        #for item in trainingDataDC:
        #    f.write(item + '\n')
        #f.write("\n\n%Test data\n")
        #for item in testDataDC:
        #    f.write(item + '\n')
        f.write("\n\n%Interventions\n")
        for item in interventions:
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
            if len(samples) < 2:
                pass
            else:
                average = 0
                for sample in samples:
                    average += sample[0][0]*sample[3]
                nSamples = len(samples)
                square = 0
                for sample in samples:
                    square += sample[3]*(sample[0][0] - average)*(sample[0][0] - average)
                weight = 0
                for sample in samples:
                    weight += sample[3]
                numOfNonZeroWeights = 0
                for sample in samples:
                    if sample[3] != 0:
                        numOfNonZeroWeights+=1
                square = square/((float(numOfNonZeroWeights - 1)*weight)/float(numOfNonZeroWeights))
                std = math.sqrt(square)
                WPLL[featureName].append(np.log(scipy.stats.norm(average, std).pdf(float(originalValue))))
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
                for ele in RANDOM_VARIABLE_PREDICATE_RANGE[self.databaseName][featureName]:
                    if ele == originalValue:
                        predList.append(1)
                    else:
                        predList.append(0)
                    if counts.has_key(ele):
                        predProb.append(counts[ele]/sumOfAllCounts)
                    else:
                        predProb.append(1e-100)
                probability = predProb[RANDOM_VARIABLE_PREDICATE_RANGE[self.databaseName][featureName].index(originalValue)]
                WPLL[featureName].append(np.log(probability))
                return yPred
    
    def updateAccuracyList(self, samples, featureName, originalValue, ty):
        if ty == 'continuous':
            if len(samples) == 0:
                pass
            else:
                average = 0
                for sample in samples:
                    average += sample[0][0]*sample[3]
                ERROR[featureName].append((average - float(originalValue))*(average - float(originalValue)))
                NUM_OF_ELEMENTS_IN_TEST[featureName] = NUM_OF_ELEMENTS_IN_TEST[featureName] + 1
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
                for ele in RANDOM_VARIABLE_PREDICATE_RANGE[self.databaseName][featureName]:
                    if ele == originalValue:
                        predList.append(1)
                    else:
                        predList.append(0)
                    if counts.has_key(ele):
                        predProb.append(counts[ele]/sumOfAllCounts)
                    else:
                        predProb.append(0.0)
                YTRUEROC[featureName].append(predList)
                YPREDROC[featureName].append(predProb)
                YPRED[featureName].append(yPred)
                YTRUE[featureName].append(originalValue)
                NUM_OF_ELEMENTS_IN_TEST[featureName] = NUM_OF_ELEMENTS_IN_TEST[featureName] + 1
                return yPred
        
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

    def getLikelihoodWeight(self, secondResult):
        weight = secondResult['probability']
        return weight
        
    def getSampleVal(self, result, yapObj):
        samples = result['samples']
        listSamples = yapObj.convertStringReprOfListToList(samples)
        return listSamples[0][0]
            
    def getSampleList(self, result, yapObj):
        samples = result['samples']
        if result['probability']  != 1.0:
            return None
        else:
            listSamples = yapObj.convertStringReprOfListToList(samples)
            return listSamples[0][0][0]

    def generateSamplesWithProb(self, samples, yapObj):
        listSamples = yapObj.convertStringReprOfListToList(samples)
        acceptedSamples = []
        sumW1 = 0
        for i in range(0, len(listSamples)):
            if listSamples[i][2] == 0:
                pass
            else:
                acceptedSamples.append(listSamples[i])
            sumW1 += listSamples[i][1]
        for i in range(0, len(acceptedSamples)):
            p = (acceptedSamples[i][1] * acceptedSamples[i][2])/float(sumW1)
            acceptedSamples[i].append(p)
        return acceptedSamples
        
    def findWPLL(self, numOfSamples, tempDcFileName, testFacts, listOfTargetPredicates, trainFacts, dcRules, prologDependencyStructure, attributeRelatedPredicates, relationRelatedPredicates):
        for predicate in listOfTargetPredicates:
            WPLL[predicate] = []
        for featName in listOfTargetPredicates:
            tempTestFacts = []
            for tFacts in testFacts:
                if featName not in tFacts:
                    pass
                else:
                    tempTestFacts.append(tFacts)
            ## Interventions 
            interventions = []
            for tFacts in testFacts:
                tName = self.getPredicateName(tFacts)
                if tName in relationRelatedPredicates:
                    feat = self.findListOfDCFacts([tFacts])[0]
                    interventions.append(feat)
            ty = RANDOM_VARIABLE_PREDICATE_TYPE[self.databaseName][featName]
            self.generateDCFile(tempDcFileName, trainFacts, testFacts, interventions, dcRules, prologDependencyStructure)
            yapObj = YapPrologInterface(RANDOM_VARIABLE_PREDICATE[self.databaseName])
            yapObj.consultWithOneFile(tempDcFileName)
            for i in range(0, len(tempTestFacts)): 
                feat = tempTestFacts[i]
                feat = self.dcPredicateAndValue(feat)
                variable = '[X]'
                actualValue = feat[1]
                ## Query
                query = '(' + feat[0] + ' ~= X)'
                ## Evidence
                evidenceTest = ''
                tempTestFact = tempTestFacts[i][0:-1]
                res = yapObj.listQueryPatch('requsite('+tempTestFact+',ProbEvidence,Intervention)',['ProbEvidence', 'Intervention'])
                for ele in res['ProbEvidence']:
                    evidence = ele + '.'
                    tName = self.getPredicateName(evidence)
                    if tName in attributeRelatedPredicates:
                        feat = self.dcPredicateAndValue(evidence)
                        if evidenceTest == '':
                            evidenceTest = evidenceTest + feat[0] + '~=' + feat[1]
                        else:
                            evidenceTest = evidenceTest + ',' + feat[0] + '~=' + feat[1]
                for item in res['Intervention']:
                    tName = self.getPredicateName(item)
                    item = item + '.'
                    item = self.findListOfDCFacts([item])[0]
                    item = item[0:-1]
                    if tName in attributeRelatedPredicates:
                        self.interventionAssert(item, yapObj)
                result = yapObj.queryWithSamples(numOfSamples, query, evidenceTest, variable)
                samples = result['samples']
                samplesWithProb = self.generateSamplesWithProb(samples, yapObj)
                predictedValue = self.updateWPLLList(samplesWithProb, featName, actualValue, ty)
                print query, actualValue, predictedValue, evidenceTest
                for item in res['Intervention']:
                    tName = self.getPredicateName(item)
                    item = item + '.'
                    item = self.findListOfDCFacts([item])[0]
                    item = item[0:-1]
                    if tName in attributeRelatedPredicates:
                        self.interventionRetract(item, yapObj)
            yapObj.terminate()
            del yapObj
        for predicate in listOfTargetPredicates:
            ty = RANDOM_VARIABLE_PREDICATE_TYPE[self.databaseName][predicate] 
            #logging.info('\n \n \n' + '$$$$$$$$$$ Results for %s $$$$$$$$$$' % (predicate))
            print('\n \n \n' + '$$$$$$$$$$ Results for %s $$$$$$$$$$' % (predicate))
            wpll = WPLL[predicate]
            numElements = len(wpll)
            if numElements == 0:
                wpllScore = float("-inf")
            else:
                wpllScore = sum(wpll)/float(numElements)
            #logging.info('\nWPLL = %f, Number of tests consider = %d' % (wpllScore, numElements))
            print('\nWPLL = %f, Number of tests consider = %d' % (wpllScore, numElements))
            if WPLL_TOTAL.has_key(predicate):
                tempList = WPLL_TOTAL[predicate]
                tempList.append(wpllScore)
            else:
                WPLL_TOTAL[predicate] = [wpllScore]

    def interventionAssert(self, item, yapObj):
        [head, distribution, body] = self.splitDCToHeadDistributionBody(item)
        if distribution == '':
            query = 'asserta(user:hardclause(' + head + ',' + '(' + body + ')' + ',0)).'
        else:
            query = 'asserta(user:distributionalclause(' + head + ',' + distribution + ',' + '(' + body + ')' + ',0)).'
        yapObj.executeQuery(query)
        pass
        
    def interventionRetract(self, item, yapObj):
        [head, distribution, body] = self.splitDCToHeadDistributionBody(item)
        if distribution == '':
            query = 'retract(user:hardclause(' + head + ',' + '(' + body + ')' + ',0)).'
        else:
            query = 'retract(user:distributionalclause(' + head + ',' + distribution + ',' + '(' + body + ')' + ',0)).'
        yapObj.executeQuery(query)
        pass
    
    def dcPredicateAndValue(self, item):
        feat = self.findListOfDCFacts([item])[0]
        if '~' in feat:
            feat = feat.split('~')
            feat[1] = feat[1].replace('val(', '')
            feat[1] = feat[1].replace(').', '')
            feat[1] = feat[1].replace(' ', '')
            feat[0] = feat[0].replace(' ', '')
            return feat
        else:
            return None
    
    def findTestAccuracy(self, numOfSamples, tempDcFileName, testFacts, listOfTargetPredicates, trainFacts, dcRules, prologDependencyStructure, attributeRelatedPredicates, relationRelatedPredicates):
        for predicate in listOfTargetPredicates:
            ERROR[predicate] = []
            YPRED[predicate] = []
            YTRUE[predicate] = []
            YTRUEROC[predicate] = []
            YPREDROC[predicate] = []
            NUM_OF_ELEMENTS_IN_TEST[predicate] = 0
        for featName in listOfTargetPredicates:
            tempTestFacts = []
            for tFacts in testFacts:
                if featName not in tFacts:
                    pass
                else:
                    tempTestFacts.append(tFacts)
            ## relation intervention
            interventions = []
            for tFacts in testFacts:
                tName = self.getPredicateName(tFacts)
                if tName in relationRelatedPredicates:
                    feat = self.findListOfDCFacts([tFacts])[0]
                    interventions.append(feat)
            ty = RANDOM_VARIABLE_PREDICATE_TYPE[self.databaseName][featName]
            self.generateDCFile(tempDcFileName, trainFacts, testFacts, interventions, dcRules, prologDependencyStructure)
            yapObj = YapPrologInterface(RANDOM_VARIABLE_PREDICATE[self.databaseName])
            yapObj.consultWithOneFile(tempDcFileName)
            for i in range(0, len(tempTestFacts)): 
                feat = tempTestFacts[i]
                feat = self.dcPredicateAndValue(feat)
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
                    tName = self.getPredicateName(evidence)
                    if tName in attributeRelatedPredicates:
                        feat = self.dcPredicateAndValue(evidence)
                        if evidenceTest == '':
                            evidenceTest = evidenceTest + feat[0] + '~=' + feat[1]
                        else:
                            evidenceTest = evidenceTest + ',' + feat[0] + '~=' + feat[1]
                for item in res['Intervention']:
                    tName = self.getPredicateName(item)
                    item = item + '.'
                    item = self.findListOfDCFacts([item])[0]
                    item = item[0:-1]
                    if tName in attributeRelatedPredicates:
                        self.interventionAssert(item, yapObj)
                result = yapObj.queryWithSamples(numOfSamples, query, evidenceTest, variable)
                samples = result['samples']
                samplesWithProb = self.generateSamplesWithProb(samples, yapObj)
                predictedValue = self.updateAccuracyList(samplesWithProb, featName, actualValue, ty)
                print query, actualValue, predictedValue, res['Intervention'], res['ProbEvidence']
                for item in res['Intervention']:
                    tName = self.getPredicateName(item)
                    item = item + '.'
                    item = self.findListOfDCFacts([item])[0]
                    item = item[0:-1]
                    if tName in attributeRelatedPredicates:
                        self.interventionRetract(item, yapObj)
            yapObj.terminate()
            del yapObj
        for predicate in listOfTargetPredicates:
            ty = RANDOM_VARIABLE_PREDICATE_TYPE[self.databaseName][predicate] 
            #logging.info('\n \n \n' + '$$$$$$$$$$ Results for %s $$$$$$$$$$' % (predicate))
            print('\n \n \n' + '$$$$$$$$$$ Results for %s $$$$$$$$$$' % (predicate))
            if ty == 'continuous':
                acc = float('nan')
                if NUM_OF_ELEMENTS_IN_TEST[predicate] == 0:
                    #logging.info('WRMSE = %f, Number of tests consider = %d' % (acc, NUM_OF_ELEMENTS_IN_TEST[predicate]))
                    print('WRMSE = %f, Number of tests consider = %d' % (acc, NUM_OF_ELEMENTS_IN_TEST[predicate]))
                else:
                    wrmse = math.sqrt(sum(ERROR[predicate])/float(NUM_OF_ELEMENTS_IN_TEST[predicate]))/float(RANDOM_VARIABLE_PREDICATE_RANGE[self.databaseName][predicate])
                    if WRMSE.has_key(predicate):
                        tempList = WRMSE[predicate]
                        tempList.append(wrmse)
                    else:
                        WRMSE[predicate] = [wrmse]
                    #logging.info('WRMSE = %f, Number of tests consider = %d' % (wrmse, NUM_OF_ELEMENTS_IN_TEST[predicate]))
                    print('WRMSE = %f, Number of tests consider = %d' % (wrmse, NUM_OF_ELEMENTS_IN_TEST[predicate]))
            else:
                uniqueLabel = list(set(YPRED[predicate] + YTRUE[predicate]))
                #logging.info ('Confusion Matrix:')
                print('Confusion Matrix:')
                #logging.info(confusion_matrix(YTRUE[predicate], YPRED[predicate], labels=uniqueLabel))
                print(confusion_matrix(YTRUE[predicate], YPRED[predicate], labels=uniqueLabel))
                #logging.info('Labels: '  + str(uniqueLabel))
                print('Labels: '  + str(uniqueLabel))
                if NUM_OF_ELEMENTS_IN_TEST[predicate] == 0:
                    #logging.info('Accuracy: nan')
                    print('Accuracy: nan')
                    #logging.info('Area Under ROC: prediction list is empty')
                    print('Area Under ROC: prediction list is empty')
                else:
                    #logging.info('Accuracy: %f'  % accuracy_score(YTRUE[predicate], YPRED[predicate]))
                    print('Accuracy: %f'  % accuracy_score(YTRUE[predicate], YPRED[predicate]))
                    try:
                        auROC = roc_auc_score(np.array(YTRUEROC[predicate]), np.array(YPREDROC[predicate]), average='weighted')
                        if AUCROC.has_key(predicate):
                            tempList = AUCROC[predicate]
                            tempList.append(auROC)
                        else:
                            AUCROC[predicate] = [auROC]
                        #logging.info('Area Under ROC: %f' % auROC)
                        print('Area Under ROC: %f' % auROC)
                    except ValueError:
                        #logging.info('Only one class present in y_true, Area Under ROC: %f' % 1)
                        print('Only one class present in y_true, Area Under ROC: %f' % 1)

    def generateEnsembleOfDLTs(self, fileName, trainFacts, outputFile):
        self.generatePrologFile(fileName, trainFacts)
        f = open(outputFile, 'w')
        obj = DCLearner(fileName, '', '', RANDOM_VARIABLE_PREDICATE[self.databaseName])
        obj.learnRules()
        obj1 = TranslateToDC()
        dcRules = []
        obj1.setRandomVariablePredicates(RANDOM_VARIABLE_PREDICATE[self.databaseName])
        for rule in obj.rules:
            rule = obj1.translate(rule)
            dcRules.append(rule)
            f.write(rule + '\n')
        f.close()
        return dcRules
    
    
    def experiment1_financial(self, prologFileName, dcFileName, treeOutputFile):
        attributeRelatedPredicates = RANDOM_VARIABLE_PREDICATE['financial']
        relationRelatedPredicates = RELATIONAL_PREDICATES['financial']
        attributeRelatedPredicates = list(set(attributeRelatedPredicates)-set(relationRelatedPredicates))
        for randVariable in ['avgSumOfW']: ## Put all target random variable here.
            for fold in range(1,11):
                foldString = "Fold" + str(fold)
                trainFacts = self.getListOfPredicatesInFolder(DATA_DIRECTORY + '/' + foldString + '/' + 'train')
                validateFacts = self.getListOfPredicatesInFolder(DATA_DIRECTORY + '/' + foldString + '/' + 'validate')
                testFacts = self.getListOfPredicatesInFolder(DATA_DIRECTORY + '/' + foldString + '/' + 'test')
                trainFacts = trainFacts + validateFacts
                self.flushPredictionDataStructures()
                
                prologRules = []
                prologDependencyStructureList = []
                self.generatePrologFile(prologFileName, trainFacts, targetVariable=attributeRelatedPredicates)
                obj = DCLearner(prologFileName, '', '', RANDOM_VARIABLE_PREDICATE[self.databaseName])
                obj.learnRules()
                prologRules = prologRules + obj.rules
                prologDependencyStructureList.append(obj.generateDependencyStructureOfRules(obj.rulesFragmented)) 
                
                basePredicates = obj.generateBasePredicates()
                prologDependencyStructure = ''
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
                            chainOfRelations = RELATIONAL_CHAIN_OF_PREDICATES[self.databaseName].keys()
                            for rel in chainOfRelations:
                                if rel in uniqueAtoms:
                                    chain = RELATIONAL_CHAIN_OF_PREDICATES[self.databaseName][rel]
                                    atoms = []
                                    for c in chain:
                                        atoms.append(basePredicates[c])
                                    body = ' :- ' + basePredicates[head] + ',' + ','.join(atoms) + '.\n'
                                    for c in chain:
                                        #pds.add('parent(' + basePredicates[head] + ',' + basePredicates[c] + ')' + body)
                                        pass
                            
                            for rel in relationRelatedPredicates:
                                if rel in uniqueAtoms:
                                    body = ' :- ' + basePredicates[head] + ',' + basePredicates[rel] + '.\n'
                                    #pds.add('parent(' + basePredicates[head] + ',' + basePredicates[rel] + ')' + body)
                                    pass
                                    
                            for feature in dependency[head]:
                                atoms = []
                                for atom in feature:
                                    if atom in relationRelatedPredicates:
                                        temp = basePredicates[atom]
                                        temp = temp.split(',')
                                        temp[-1] = 'true)'
                                        atoms.append(','.join(temp))
                                    else:
                                        atoms.append(basePredicates[atom])
                                body = ' :- ' + basePredicates[head] + ',' + ','.join(atoms) + '.\n'
                                for atom in feature:
                                    if atom in attributeRelatedPredicates:
                                        pds.add('parent(' + basePredicates[head] + ',' + basePredicates[atom] + ')' + body)
                            for item in pds:
                                prologDependencyStructure += item
                obj1 = TranslateToDC()
                dcRules = []
                obj1.setRandomVariablePredicates(RANDOM_VARIABLE_PREDICATE[self.databaseName])
                f = open(treeOutputFile, 'w')
                for rule in prologRules:
                    rule = obj1.translate(rule)
                    dcRules.append(rule)
                    f.write(rule + '\n')
                f.close()
                self.findTestAccuracy(1000, dcFileName, testFacts, [randVariable], trainFacts, dcRules, prologDependencyStructure, attributeRelatedPredicates, relationRelatedPredicates)
            for key in WRMSE:
                a = WRMSE[key]
                #logging.info('\nPredicate = %s, WRMSE = %f +- %f' % (key, statistics.mean(a), statistics.stdev(a)))
                print('\nPredicate = %s, WRMSE = %f +- %f' % (key, statistics.mean(a), statistics.stdev(a)))
            for key in AUCROC:
                a = AUCROC[key]
                #logging.info('\nPredicate = %s, AUCROC = %f +- %f' % (key, statistics.mean(a), statistics.stdev(a)))
                print('\nPredicate = %s, AUCROC = %f +- %f' % (key, statistics.mean(a), statistics.stdev(a)))
            print "Too much work. Going to take nap for 10 secs."
            time.sleep(10)
            print "I am feeling fresh now :)"
        

    def experiment1_university(self, prologFileName, dcFileName, treeOutputFile):
        attributeRelatedPredicates = RANDOM_VARIABLE_PREDICATE['university']
        relationRelatedPredicates = RELATIONAL_PREDICATES['university']
        attributeRelatedPredicates = list(set(attributeRelatedPredicates)-set(relationRelatedPredicates))
        for randVariable in ['nrhours', 'difficulty', 'ability', 'intelligence', 'grade', 'satisfaction']: ## Put all target random variable here.
            for fold in range(1,2):
                foldString = "data800x125x125_" + str(fold) + '/data800x125x125'
                trainFacts = self.getListOfPredicatesInFolder(DATA_DIRECTORY + '/' + foldString + '/' + 'train')
                #validateFacts = getListOfPredicatesInFolder(DATA_DIRECTORY + '/' + foldString + '/' + 'validate')
                testFacts = self.getListOfPredicatesInFolder(DATA_DIRECTORY + '/' + foldString + '/' + 'test')
                trainFacts = trainFacts
                self.flushPredictionDataStructures()
                
                prologRules = []
                prologDependencyStructureList = []
                self.generatePrologFile(prologFileName, trainFacts, targetVariable=attributeRelatedPredicates)
                obj = DCLearner(prologFileName, '', '', RANDOM_VARIABLE_PREDICATE[self.databaseName])
                obj.learnRules()
                prologRules = prologRules + obj.rules
                prologDependencyStructureList.append(obj.generateDependencyStructureOfRules(obj.rulesFragmented)) 
                
                basePredicates = obj.generateBasePredicates()
                prologDependencyStructure = ''
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
                            chainOfRelations = RELATIONAL_CHAIN_OF_PREDICATES[self.databaseName].keys()
                            for rel in chainOfRelations:
                                if rel in uniqueAtoms:
                                    chain = RELATIONAL_CHAIN_OF_PREDICATES[self.databaseName][rel]
                                    atoms = []
                                    for c in chain:
                                        atoms.append(basePredicates[c])
                                    body = ' :- ' + basePredicates[head] + ',' + ','.join(atoms) + '.\n'
                                    for c in chain:
                                        #pds.add('parent(' + basePredicates[head] + ',' + basePredicates[c] + ')' + body)
                                        pass
                            
                            for rel in relationRelatedPredicates:
                                if rel in uniqueAtoms:
                                    body = ' :- ' + basePredicates[head] + ',' + basePredicates[rel] + '.\n'
                                    #pds.add('parent(' + basePredicates[head] + ',' + basePredicates[rel] + ')' + body)
                                    pass
                                    
                            for feature in dependency[head]:
                                atoms = []
                                for atom in feature:
                                    if atom in relationRelatedPredicates:
                                        temp = basePredicates[atom]
                                        temp = temp.split(',')
                                        temp[-1] = 'true)'
                                        atoms.append(','.join(temp))
                                    else:
                                        atoms.append(basePredicates[atom])
                                body = ' :- ' + basePredicates[head] + ',' + ','.join(atoms) + '.\n'
                                for atom in feature:
                                    if atom in attributeRelatedPredicates:
                                        pds.add('parent(' + basePredicates[head] + ',' + basePredicates[atom] + ')' + body)
                            for item in pds:
                                prologDependencyStructure += item
                obj1 = TranslateToDC()
                dcRules = []
                obj1.setRandomVariablePredicates(RANDOM_VARIABLE_PREDICATE[self.databaseName])
                f = open(treeOutputFile, 'w')
                for rule in prologRules:
                    rule = obj1.translate(rule)
                    dcRules.append(rule)
                    f.write(rule + '\n')
                f.close()
                self.findWPLL(1000, dcFileName, testFacts, [randVariable], trainFacts, dcRules, prologDependencyStructure, attributeRelatedPredicates, relationRelatedPredicates)
            for key in WPLL_TOTAL:
                a = WPLL_TOTAL[key]
                #logging.info('\nPredicate = %s, WPLL = %f +- %f' % (key, statistics.mean(a), statistics.stdev(a)))
                if len(a) >2:
                    print('\nPredicate = %s, WPLL = %f +- %f' % (key, statistics.mean(a), statistics.stdev(a)))
            print "Too much work. Going to take nap for 10 secs."
            time.sleep(10)
            print "I am feeling fresh now :)"
        
if __name__ == '__main__':        
    obj = Experiment1b()
    obj.experiment1_financial(PROCESSED_TRAIN_DATA_FILE, PROCESSED_TRAIN_DATA_FILE_DC, OUTPUT_ENSEMBLE_OF_DLTS)
    
    #obj.experiment1_university(PROCESSED_TRAIN_DATA_FILE, PROCESSED_TRAIN_DATA_FILE_DC, OUTPUT_ENSEMBLE_OF_DLTS)
    
    #TRAIN_FACTS = getListOfPredicatesInFolder(DATA_DIRECTORY + '/' + foldString + '/' + 'train')
    #validateFacts = getListOfPredicatesInFolder(DATA_DIRECTORY + '/' + foldString + '/' + 'validate')
    #TEST_FACTS = getListOfPredicatesInFolder(DATA_DIRECTORY + '/' + foldString + '/' + 'test')
    #TRAIN_FACTS = TRAIN_FACTS + validateFacts
    #DC_RULES = generateEnsembleOfDLTs(PROCESSED_TRAIN_DATA_FILE, TRAIN_FACTS, OUTPUT_ENSEMBLE_OF_DLTS)
    #findTestAccuracy(1000, PROCESSED_TRAIN_DATA_FILE_DC, TEST_FACTS, ['loanStatus'], TRAIN_FACTS, DC_RULES)
    #generatePrologFile(PROCESSED_TRAIN_DATA_FILE+'x', TEST_FACTS)


