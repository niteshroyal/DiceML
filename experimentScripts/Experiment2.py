'''
This experiment evaluates the joint model program learned from data and background knowledge.

@author: nitesh
'''
import logging, math
import statistics, time
import numpy as np
from collections import Counter
from core.YapPrologInterface import YapPrologInterface
from core.TranslateToDC import TranslateToDC
from sklearn.metrics import confusion_matrix
from sklearn.metrics import accuracy_score
from sklearn.metrics import roc_auc_score
from core.DCLearner import DCLearner

from Settings import Test
from Settings import RANDOM_VARIABLE_PREDICATE_TYPE, RANDOM_VARIABLE_PREDICATE, DATABASE_NAME, RANDOM_VARIABLE_PREDICATE_RANGE, DECLARATIVE_BIAS_ORDERED, PROLOG_PROGRAM_HEADER, DC_PROGRAM_HEADER, DC_PROGRAM_DATA_DEPENDENT_RELATION
from Settings import PROLOG_PROGRAM_HEADER_PROBABILISTIC,\
    DC_PROGRAM_DATA_DEPENDENT_RELATION_PROBABILISTIC

#listOfMissingPredicate = ['loanStatus', 'loanAmount', 'freq', 'avgSumOfW', 'stdMonthInc', 'stdMonthW', 'avgNrWith', 'avgSumOfInc', 'avgSalary', 'ratUrbInhab', 'gender', 'age']
#listOfTestPredicate = ['monthlyPayments']
logging.basicConfig(level=logging.INFO, filename="Experiment2b", filemode="w", format="%(asctime)-15s %(levelname)-8s %(message)s")

experiment2b_logger = logging.getLogger('Experiment 2b Logger :')
experiment2b_logger_handler = logging.FileHandler("Experiment2b", mode="w")
experiment2b_logger_handler.setFormatter(logging.Formatter("%(asctime)-15s %(levelname)-8s %(message)s"))
experiment2b_logger_handler.setLevel(logging.INFO)
experiment2b_logger.addHandler(experiment2b_logger_handler)

WRMSE= dict()
AUCROC = dict()
WPLL_TOTAL = dict()
exp1Obj = Test()

def findDistributionOfList(samples, featureName, ty):
    distribution = None
    mode = None
    enumeration = []
    actualSamples = []
    for i in range(0, len(samples)):
        if samples[i] == '' or '_' in samples[i]:
            continue
        else:
            actualSamples.append(samples[i])
    if ty == 'continuous':
        for i in range(0, len(actualSamples)):
            actualSamples[i] = float(actualSamples[i])
        if len(actualSamples) > 1:
            m = statistics.mean(actualSamples)
            s = statistics.stdev(actualSamples)
            distribution = 'gaussian(' + str(m) + ',' + str(s) + ').'
            enumeration.append(',' + str(-1) + ').')
            mode = ',' + str(m) + ').'
        elif len(actualSamples) == 1:
            m = statistics.mean(actualSamples)
            distribution = 'val(' + str(m) + ').'
            enumeration.append(',' + str(-1) + ').')
            mode = ',' + str(m) + ').'
        else:
            distribution = None
            enumeration = []
            mode = None
    else:
        lenSamples = len(actualSamples)
        counts = Counter(actualSamples)
        predProb = []
        elementPresent = False
        for ele in RANDOM_VARIABLE_PREDICATE_RANGE[DATABASE_NAME][featureName]:
            if counts.has_key(ele):
                predProb.append(str(counts[ele]/float(lenSamples)) + ':' + ele)
                elementPresent = True
            else:
                predProb.append(str(0.0) + ':' + ele)
            enumeration.append(',' + ele + ').')
        if not elementPresent:
            distribution = None
            enumeration = []
            mode = None
        else:
            distribution = 'finite(['
            for i in range(0, len(predProb)):
                if i == len(predProb)-1:
                    distribution = distribution + predProb[i]
                else:
                    distribution = distribution + predProb[i] + ','
            distribution = distribution + ']).'
            mode = max(set(samples), key=samples.count)
            mode = ',' + str(mode) + ').'
    return [distribution, mode, enumeration]


def generatePrologFile(filename, trainingData, targetVariable=[]):
    declarativeBias = DECLARATIVE_BIAS_ORDERED[DATABASE_NAME]
    if targetVariable == []:
        pass
    else:
        declarativeBias = ''
        targetVariableStringList = []
        for i in targetVariable:
            targetVariableStringList.append('learn(' + i)
        declarativeBiasTemp = DECLARATIVE_BIAS_ORDERED[DATABASE_NAME].split('\n')
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
                
def generateDLTsFromValidation(numOfSamples, validationFolder, prologValidationFile, newTrainFacts, missingFacts, treeOutputFile, tempDcFileName, missingPredicate):
    listOfDistributinalFacts = []
    listOfModeOfDistribtion = []
    listOfActualValue = []
    listOfEnumeration = []
    validateFacts = exp1Obj.getListOfPredicatesInFolder(validationFolder)
    generatePrologFile(prologValidationFile, validateFacts, targetVariable=missingPredicate)
    obj = DCLearner(prologValidationFile, '', '', RANDOM_VARIABLE_PREDICATE[DATABASE_NAME])
    obj.learnRules()
    obj1 = TranslateToDC()
    dcRules = []
    obj1.setRandomVariablePredicates(RANDOM_VARIABLE_PREDICATE[DATABASE_NAME])
    with open(treeOutputFile, 'w') as f:
        for rule in obj.rules:
            rule = obj1.translate(rule)
            dcRules.append(rule)
            f.write(rule + '\n')
    obj.interface.terminate()
    del obj.interface
    tempNewTrainFacts = []
    for i in newTrainFacts:
        tempNewTrainFacts.append(i)
    for i in validateFacts:
        tempNewTrainFacts.append(i)

    global FILE_COUNTER
    FILE_COUNTER += 1
    tempDcFileName = tempDcFileName + str(FILE_COUNTER) + '.pl'
    
    print "Going to sleep for 2 secs 1 ."
    time.sleep(2)
    print "Restarting again :)"
    
    generateDCFile(tempDcFileName, tempNewTrainFacts, [], [], [], [], dcRules)
    
    print "Going to sleep for 2 secs 1 ."
    time.sleep(2)
    print "Restarting again :)"

    yapObj = YapPrologInterface(RANDOM_VARIABLE_PREDICATE[DATABASE_NAME])
    
    print "Going to sleep for 2 secs 1 ."
    time.sleep(2)
    print "Restarting again :)"
    
    yapObj.consultWithOneFile(tempDcFileName)    
    
    for randVariable in missingPredicate:
        restMissingFacts = []
        tempMissingFacts = []
        for tFacts in missingFacts:
            if randVariable not in tFacts:
                restMissingFacts.append(tFacts)
            else:
                tempMissingFacts.append(tFacts)
                
        ty = RANDOM_VARIABLE_PREDICATE_TYPE[DATABASE_NAME][randVariable]
        #generateDCFile(tempDcFileName, restValidateFacts, [], [], [], [], dcRules)   #Change restValidateFacts to tempNewTrainFacts
        
        for i in range(0, len(tempMissingFacts)): 
            feat = tempMissingFacts[i]
            feat = exp1Obj.findListOfDCFacts([feat])[0]
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
            [distribution, mode, enumeration] = findDistributionOfList(samples, randVariable, ty)
            if distribution == None:
                pass
            else:
                tempFeature = feat[0].replace(' ', '')
                tempFeature = tempFeature[0:-1]
                listOfDistributinalFacts.append(feat[0] + '~' + distribution)
                #listOfDistributinalFacts.append(feat[0] + '~' + 'val(' + feat[1] + ').')
                listOfModeOfDistribtion.append(tempFeature + mode)
                listOfActualValue.append(tempFeature + ',' + feat[1] + ').')
                for j in enumeration:
                    listOfEnumeration.append(tempFeature + j)
                #listOfEnumeration.append(tempFeature + ',' + feat[1] + ').')
            print feat, distribution
    #yapObj.executeQuery("unload_file('" + tempDcFileName + "').")
    yapObj.terminate()
    del yapObj
    return [listOfDistributinalFacts, listOfModeOfDistribtion, listOfEnumeration, listOfActualValue]

ERROR = dict()
YPRED = dict()
YTRUE = dict()
YPREDROC = dict()
YTRUEROC = dict()
WPLL = dict()
NUM_OF_ELEMENTS_IN_TEST = dict()

def flushPredictionDataStructures():
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

def updateAccuracyList(samples, featureName, originalValue, ty):
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
            err = (average - float(originalValue))*(average - float(originalValue))
            thresErr = RANDOM_VARIABLE_PREDICATE_RANGE[DATABASE_NAME][featureName] * RANDOM_VARIABLE_PREDICATE_RANGE[DATABASE_NAME][featureName]
            if err > thresErr:
                print 'Error > Threshold Error '
            else:
                ERROR[featureName].append(err)
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

def generateDCFile(filename, trainingData, listOfDistributionalFacts, listOfEnumeration, listOfModeOfDistribtion, testData, dcRules):
    f = open(filename, 'w')
    f.write(DC_PROGRAM_HEADER)
    f.write(DC_PROGRAM_DATA_DEPENDENT_RELATION[DATABASE_NAME])
    trainingDataDC = exp1Obj.findListOfDCFacts(trainingData)
    testDataDC = exp1Obj.findListOfDCFacts(testData)
    listOfModeOfDistribtionDC = exp1Obj.findListOfDCFacts(listOfModeOfDistribtion)
    f.write("\n\n%Train data prolog\n")
    for item in trainingData:
        f.write(item + '\n')
    f.write("\n\n%Mode of distribution\n")
    for item in listOfModeOfDistribtion:
        f.write(item + '\n')
    f.write("\n\n%List of enumeration\n")
    for item in listOfEnumeration:
        f.write(item + '\n')
    f.write("\n\n%Test data prolog\n")
    for item in testData:
        f.write(item + '\n')
    f.write("\n\n%Train data\n")
    for item in trainingDataDC:
        f.write(item + '\n')
    f.write("\n\n%Mode of distribution in DC\n")
    for item in listOfModeOfDistribtionDC:
        f.write(item + '\n')
    f.write("\n\n%Test data\n")
    for item in testDataDC:
        f.write(item + '\n')
    f.write("\n\n%Missing data distribution\n")
    for item in listOfDistributionalFacts:
        f.write(item + '\n')
    f.write("\n\n%Background Theory\n")
    for item in dcRules:
        f.write(item + '\n')
    f.close()
    print "Going to sleep for 2 secs."
    time.sleep(2)
    print "Restarting again :)"
        
def findTestAccuracy(numOfSamples, tempDcFileName, testFacts, listOfTargetPredicates, trainFacts, listOfDistributionalFacts, listOfEnumeration, listOfModeOfDistribtion, dcRules, mode):
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
        
        generateDCFile(tempDcFileName, trainFacts, listOfDistributionalFacts, listOfEnumeration, listOfModeOfDistribtion, restTestFacts, dcRules)
        print "Going to sleep for 2 secs 1 ."
        time.sleep(2)
        print "Restarting again :)"
        yapObj = YapPrologInterface(RANDOM_VARIABLE_PREDICATE[DATABASE_NAME])
        print "Going to sleep for 2 secs 2"
        time.sleep(2)
        print "Restarting again :)"
        yapObj.consultWithOneFile(tempDcFileName)
        for i in range(0, len(tempTestFacts)): 
            feat = tempTestFacts[i]
            feat = exp1Obj.findListOfDCFacts([feat])[0]
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
            updateAccuracyList(samples, featName, feat[1], ty)
            print feat, samples[0:10]
        #del yapObj
    for predicate in listOfTargetPredicates:
        ty = RANDOM_VARIABLE_PREDICATE_TYPE[DATABASE_NAME][predicate] 
        if mode == 'mode':
            experiment2b_logger.info('\n\n\nRuns over mode of distribution of missing values')
        elif mode == 'probabilistic':
            experiment2b_logger.info('\n\n\nRuns over probability distribution of missing values')
        elif mode == 'missing':
            experiment2b_logger.info('\n\n\nRuns over missing values')
        elif mode == 'complete':
            experiment2b_logger.info('\n\n\nRuns over complete data')
        else:
            pass
        experiment2b_logger.info('\n \n \n' + '$$$$$$$$$$ Results for %s $$$$$$$$$$' % (predicate))
        if ty == 'continuous':
            acc = float('nan')
            if NUM_OF_ELEMENTS_IN_TEST[predicate] == 0:
                experiment2b_logger.info('WRMSE = %f, Number of tests consider = %d' % (acc, NUM_OF_ELEMENTS_IN_TEST[predicate]))
            else:
                wrmse = math.sqrt(sum(ERROR[predicate])/float(NUM_OF_ELEMENTS_IN_TEST[predicate]))/float(RANDOM_VARIABLE_PREDICATE_RANGE[DATABASE_NAME][predicate])
                if WRMSE.has_key(predicate):
                    tempList = WRMSE[predicate]
                    tempList.append(wrmse)
                else:
                    WRMSE[predicate] = [wrmse]
                experiment2b_logger.info('WRMSE = %f, Number of tests consider = %d' % (wrmse, NUM_OF_ELEMENTS_IN_TEST[predicate]))
        else:
            uniqueLabel = list(set(YPRED[predicate] + YTRUE[predicate]))
            experiment2b_logger.info ('Confusion Matrix:')
            experiment2b_logger.info(confusion_matrix(YTRUE[predicate], YPRED[predicate], labels=uniqueLabel))
            experiment2b_logger.info('Labels: '  + str(uniqueLabel))
            if NUM_OF_ELEMENTS_IN_TEST[predicate] == 0:
                experiment2b_logger.info('Accuracy: nan')
                experiment2b_logger.info('Area Under ROC: prediction list is empty')
            else:
                experiment2b_logger.info('Accuracy: %f'  % accuracy_score(YTRUE[predicate], YPRED[predicate]))
                try:
                    auROC = roc_auc_score(np.array(YTRUEROC[predicate]), np.array(YPREDROC[predicate]), average='weighted')
                    if AUCROC.has_key(predicate):
                        tempList = AUCROC[predicate]
                        tempList.append(auROC)
                    else:
                        AUCROC[predicate] = [auROC]
                    experiment2b_logger.info('Area Under ROC: %f' % auROC)
                except ValueError:
                    experiment2b_logger.info('Only one class present in y_true, Area Under ROC: %f' % 1)

def generateDCPrologFile(prologTrainingFileName, dcTrainingFile, newTrainFacts, listOfDistributionalFacts, listOfEnumeration, listOfModeOfDistribtion, dcRules, mode, targetVariable=''):
    declarativeBias = DECLARATIVE_BIAS_ORDERED[DATABASE_NAME]
    if targetVariable == '':
        pass
    else:
        declarativeBias = ''
        targetVariableString = 'learn(' + targetVariable
        declarativeBiasTemp = DECLARATIVE_BIAS_ORDERED[DATABASE_NAME].split('\n')
        learnDefinitionStarts = False
        for line in declarativeBiasTemp:
            if '%Target' in line and not learnDefinitionStarts:
                declarativeBias = declarativeBias + line + '\n'
                learnDefinitionStarts = True
            elif learnDefinitionStarts:
                if targetVariableString in line:
                    declarativeBias = declarativeBias + line + '\n'
            else:
                declarativeBias = declarativeBias + line + '\n'
    f = open(prologTrainingFileName, 'w')
    if mode == 'probabilistic':
        f.write(PROLOG_PROGRAM_HEADER_PROBABILISTIC)
    else:
        f.write(PROLOG_PROGRAM_HEADER)
    f.write(declarativeBias)
    f.write("\n\n%Train data\n")
    for i in newTrainFacts:
        f.write(i)
        f.write('\n')
    f.write("\n\n%Enumeration data\n")
    for i in listOfEnumeration:
        f.write(i)
        f.write('\n')
    f.write("\n\n%Mode of distribution\n")
    for i in listOfModeOfDistribtion:
        f.write(i)
        f.write('\n')
    f.close()
    f = open(dcTrainingFile, 'w')
    f.write(DC_PROGRAM_HEADER)
    if mode == 'probabilistic':
        f.write(DC_PROGRAM_DATA_DEPENDENT_RELATION_PROBABILISTIC[DATABASE_NAME])
    else:
        f.write(DC_PROGRAM_DATA_DEPENDENT_RELATION[DATABASE_NAME])
    trainingDataDC = exp1Obj.findListOfDCFacts(newTrainFacts)
    listOfModeOfDistribtionDC = exp1Obj.findListOfDCFacts(listOfModeOfDistribtion)
#     f.write("\n\n%Train data prolog\n")
#     for item in newTrainFacts:
#         f.write(item + '\n')
#     f.write("\n\n%Missing data enumeration\n")
#     for item in listOfEnumeration:
#         f.write(item + '\n')
#     f.write("\n\n%Missing data mode\n")
#     for item in listOfModeOfDistribtion:
#         f.write(item + '\n')
    f.write("\n\n%Train data\n")
    for item in trainingDataDC:
        f.write(item + '\n')
    f.write("\n\n%Missing data distribution\n")
    for item in listOfDistributionalFacts:
        f.write(item + '\n')
    f.write("\n\n%Missing data mode of distribution\n")
    for item in listOfModeOfDistribtionDC:
        f.write(item + '\n')
    f.write("\n\n%Background Theory\n")
    for item in dcRules:
        f.write(item + '\n')
    f.close()
    print "Going to sleep for 2 secs."
    time.sleep(2)
    print "Restarting again :)"

     
def experiment2b(numOfSamples, trainingFolder, validationFolder, testFolder, prologTrainingFileName, dcTrainingFile, tempDcFileName, testDcFileName, percentageMissing, prologValidationFile, treeOutputFileTraining, treeOutputFileValidation):
    countsPerPredicate = dict()
    for i in listOfMissingPredicate:
        countsPerPredicate[i] = 0
    trainFacts = exp1Obj.getListOfPredicatesInFolder(trainingFolder)
    testFacts = exp1Obj.getListOfPredicatesInFolder(testFolder)
    
    listOfDistributionalFacts = []
    listOfModeOfDistribtion = []
    listOfEnumeration = []
    listOfActualValue = []
    experiment2b_logger.info('\nPercentage Missing = %d' % (percentageMissing))
    missingFactsTotal = []
    for missingPredicate in listOfMissingPredicate:
        newTrainFacts = []
        missingFacts = []
        oNum = int((100 - percentageMissing)/10)
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
                countsPerPredicate[missingPredicateName] = num +1
            else:
                newTrainFacts.append(trainFacts[i])

    newTrainFacts = []
    for i in range(0, len(trainFacts)):
        if trainFacts[i] in missingFactsTotal:
            pass
        else:
            newTrainFacts.append(trainFacts[i])
    [listOfD, listOfM, listOfE, listOfA] = generateDLTsFromValidation(numOfSamples, validationFolder, prologValidationFile, newTrainFacts, missingFactsTotal, treeOutputFileValidation, tempDcFileName, listOfMissingPredicate)
    listOfModeOfDistribtion = listOfModeOfDistribtion + listOfM
    listOfDistributionalFacts = listOfDistributionalFacts + listOfD
    listOfEnumeration = listOfEnumeration + listOfE
    listOfActualValue = listOfActualValue + listOfA
            
    #modeList = ['missing','complete']
    
    modeList = ['probabilistic', 'mode', 'missing', 'complete']
    
    for mode in modeList:
        for randVariable in listOfTestPredicate:
            background = []
            if mode == 'mode':
                generateDCPrologFile(prologTrainingFileName, dcTrainingFile, newTrainFacts, [], [], listOfModeOfDistribtion, background, mode, targetVariable=randVariable)
                f = open(treeOutputFileTraining, 'w')
                obj = DCLearner(prologTrainingFileName, '', '', RANDOM_VARIABLE_PREDICATE[DATABASE_NAME])
                obj.learnRules()
                obj1 = TranslateToDC()
                dcRules = []
                obj1.setRandomVariablePredicates(RANDOM_VARIABLE_PREDICATE[DATABASE_NAME])
                for rule in obj.rules:
                    rule = obj1.translate(rule)
                    dcRules.append(rule)
                    f.write(rule + '\n')
                f.close()
                obj.interface.terminate()
                del obj.interface
                findTestAccuracy(numOfSamples, testDcFileName, testFacts, [randVariable], newTrainFacts, [], [], listOfModeOfDistribtion, dcRules, mode)
            elif mode == 'probabilistic':
                generateDCPrologFile(prologTrainingFileName, dcTrainingFile, newTrainFacts, listOfDistributionalFacts, listOfEnumeration, [], background, mode, targetVariable=randVariable)
                f = open(treeOutputFileTraining, 'w')
                obj = DCLearner(prologTrainingFileName, dcTrainingFile, '', RANDOM_VARIABLE_PREDICATE[DATABASE_NAME])
                obj.learnRules()
                obj1 = TranslateToDC()
                dcRules = []
                obj1.setRandomVariablePredicates(RANDOM_VARIABLE_PREDICATE[DATABASE_NAME])
                for rule in obj.rules:
                    rule = obj1.translate(rule)
                    dcRules.append(rule)
                    f.write(rule + '\n')
                f.close()
                obj.interface.terminate()
                del obj.interface
                findTestAccuracy(numOfSamples, testDcFileName, testFacts, [randVariable], newTrainFacts, listOfDistributionalFacts, listOfEnumeration, [], dcRules, mode)
            elif mode == 'missing':
                #generateDCPrologFile(prologTrainingFileName, dcTrainingFile, newTrainFacts, [], [], listOfActualValue, background, targetVariable=randVariable)
                
                generateDCPrologFile(prologTrainingFileName, dcTrainingFile, newTrainFacts, [], [], [], background, mode, targetVariable=randVariable)

                f = open(treeOutputFileTraining, 'w')
                obj = DCLearner(prologTrainingFileName, '', '', RANDOM_VARIABLE_PREDICATE[DATABASE_NAME])
                obj.learnRules()
                obj1 = TranslateToDC()
                dcRules = []
                obj1.setRandomVariablePredicates(RANDOM_VARIABLE_PREDICATE[DATABASE_NAME])
                for rule in obj.rules:
                    rule = obj1.translate(rule)
                    dcRules.append(rule)
                    f.write(rule + '\n')
                f.close()
                obj.interface.terminate()
                del obj.interface
                #findTestAccuracy(numOfSamples, testDcFileName, testFacts, [randVariable], newTrainFacts, [], [], listOfActualValue, dcRules, mode)
                
                findTestAccuracy(numOfSamples, testDcFileName, testFacts, [randVariable], newTrainFacts, [], [], [], dcRules, mode)
            elif mode == 'complete':
                generateDCPrologFile(prologTrainingFileName, dcTrainingFile, trainFacts, [], [], [], background, mode, targetVariable=randVariable)
                f = open(treeOutputFileTraining, 'w')
                obj = DCLearner(prologTrainingFileName, '', '', RANDOM_VARIABLE_PREDICATE[DATABASE_NAME])
                obj.learnRules()
                obj1 = TranslateToDC()
                dcRules = []
                obj1.setRandomVariablePredicates(RANDOM_VARIABLE_PREDICATE[DATABASE_NAME])
                for rule in obj.rules:
                    rule = obj1.translate(rule)
                    dcRules.append(rule)
                    f.write(rule + '\n')
                f.close()
                obj.interface.terminate()
                del obj.interface
                findTestAccuracy(numOfSamples, testDcFileName, testFacts, [randVariable], trainFacts, [], [], [], dcRules, mode)            
            else:
                pass
        
FILE_COUNTER = 0
numOfSamples = 1000
prologTrainingFileName = '../data/Financial.pl'
prologValidationFile = '../data/FinancialValidation.pl'
dcTrainingFile = '../data/FinancialDC.pl'
tempDcFileName = '../temp/FinancialDCTemp'
testDcFileName = '../data/FinancialDCTest.pl'
#percentageMissing = 80
treeOutputFileTraining = '../data/EnsembleOfDLTs.pl'
treeOutputFileValidation = '../data/EnsembleOfDLTsValidation.pl'
#mode = 'complete'



#percentageMissingList = [90, 80, 70, 60, 50, 40, 30, 20]
percentageMissingList = [20]
#percentageMissingList = [80]
#missPred = [['ratUrbInhab', 'gender', 'age'], ['ratUrbInhab', 'gender', 'age']]
#testPred = [['monthlyPayments']]

missPred = [['loanStatus', 'loanAmount', 'freq', 'stdMonthInc', 'stdMonthW', 'avgSumOfInc', 'gender'], ['monthlyPayments', 'loanAmount', 'freq', 'avgSumOfW', 'stdMonthInc', 'stdMonthW', 'avgNrWith', 'avgSumOfInc', 'avgSalary', 'ratUrbInhab', 'gender']]

#missPred = [['loanStatus', 'loanAmount', 'freq', 'avgSumOfW', 'stdMonthInc', 'stdMonthW', 'avgNrWith', 'avgSumOfInc', 'avgSalary', 'ratUrbInhab', 'gender'], ['monthlyPayments', 'loanAmount', 'freq', 'avgSumOfW', 'stdMonthInc', 'stdMonthW', 'avgNrWith', 'avgSumOfInc', 'avgSalary', 'ratUrbInhab', 'gender']]

#missPred = [['loanStatus', 'loanAmount'], ['monthlyPayments', 'loanAmount']]

testPred = [['monthlyPayments'],['loanStatus']]

#modeList = ['probabilistic']
#percentageMissingList = [80]
#missPred = [['loanStatus', 'loanAmount', 'freq', 'avgSumOfW', 'stdMonthInc', 'stdMonthW', 'avgNrWith', 'avgSumOfInc', 'avgSalary', 'ratUrbInhab', 'gender', 'age']]
#testPred = [['monthlyPayments']]

folder = '/home/nitesh/eclipse-workspace/DistributionalProgramSynthesis/data/PKDD/pkdd/'
for j in range(1,11):
    trainFoldString = folder + "Fold" + str(j) + '/validate1'
    validationFoldString = folder + "Fold" + str(j) + '/train1'
    testFolderString = folder + "Fold" + str(j) + '/test'
    experiment2b_logger.info('\n ********* Fold Number : %d' % j)
    for i in range(0,1):
        listOfMissingPredicate = missPred[i]
        listOfTestPredicate = testPred[i]
        for precent in percentageMissingList:
            percentageMissing = precent
            try:
                experiment2b(numOfSamples, trainFoldString, validationFoldString, testFolderString, prologTrainingFileName, dcTrainingFile, tempDcFileName, testDcFileName, percentageMissing, prologValidationFile, treeOutputFileTraining, treeOutputFileValidation)
            except RuntimeError:
                precent = precent - 1
                print "Going to sleep for 10 secs."
                time.sleep(10)
                print "Restarting again :)"
            except OSError:
                precent = precent - 1
                print "Going to sleep for 10 secs."
                time.sleep(100)
                print "Restarting again :)"







