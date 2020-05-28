'''
Created on Mar 25, 2020

@author: nitesh
'''

import math, ast

from yapWrapper import PyDC


BIGNUM = 10000000
FLAG = 0

class YapInterface(object):
    
    def __init__(self):
        self.numOfSamples = 0
        self.yapObject = None
        self.yapFileName = []
    
    def consultWithOneFile(self, yapFileName):
        self.yapObject = PyDC(yapFileName)
        self.yapFileName.append(yapFileName)
        
    def setNumOfSamples(self, num):
        self.numOfSamples = num
        
    def executeQuery(self, queryStr):
        success = self.yapObject.execute(queryStr)
        return success
    
    def queryWithSamples(self, numOfSamples, query, evidence, variable):
        self.yapObject.queryWithSamples(numOfSamples, query, evidence, variable, FLAG, BIGNUM)
        result = dict()
        result['probability'] = self.yapObject.prob
        result['samples'] = self.yapObject.samples
        return result

    def requisiteEvidence(self, query_null, query_X):
        query = 'requsite(' + query_null + ',ProbEvidence,Intervention,' + query_X + ').'
        self.yapObject.requisiteEvidence(query, BIGNUM)
        result = dict()
        result['ProbEvidence'] = self.convertStringReprOfListToList2(self.yapObject.probEvidence)
        result['Intervention'] = self.convertStringReprOfListToList2(self.yapObject.intervention)
        return result
    
    def convertStringReprOfListToList(self, inp):
        openBrackets = ['[', '(', '{']
        closeBrackets = [']', ')', '}']
        insideBracketCounter = 0
        inp = inp.replace(' ', '')
        inp = list(inp)
        for i in range(0, len(inp)):
            if inp[i] in openBrackets:
                insideBracketCounter += 1
            elif inp[i] in closeBrackets:
                insideBracketCounter -= 1
            else:
                pass
            if inp[i] == ',' and insideBracketCounter == 1:
                inp[i] = ';'
            elif inp[i] == ',' and insideBracketCounter == 2:
                inp[i] = ':'
            else:
                pass
        inp = ''.join(inp)
        inp = inp[1:]
        inp = inp[:-1]
        out = []
        if inp == '':
            return out
        inp = inp.split(';')
        for i in range(0, len(inp)):
            inp[i] = inp[i][1:]
            inp[i] = inp[i][:-1]
            inp[i] = inp[i].split(':')
        for i in range(0, len(inp)):
            tempOutput = []
            for j in range(0, len(inp[i])):
                if ('[' in inp[i][j]):
                    tempLiteralList = []
                    tempLiteral = inp[i][j]
                    tempLiteral = tempLiteral[1:]
                    tempLiteral = tempLiteral[:-1]
                    tempLiteral = tempLiteral.split(',')
                    for k in range(0, len(tempLiteral)):
                        st = self.returnLiteral(tempLiteral[k])
                        if st == '':
                            pass
                        else:
                            tempLiteralList.append(st)
                    tempLiteral = tempLiteralList
                    tempOutput.append(tempLiteral)
                else:
                    tempLiteral = self.returnLiteral(inp[i][j])
                    tempOutput.append(tempLiteral)
            out.append(tempOutput)
        return out
    
    def returnLiteral(self, inp):
        out = inp
        try:
            out = ast.literal_eval(inp)
        except Exception:
            pass
        return out
    
    def convertStringReprOfListToList2(self, inp):
        openBrackets = ['[', '(', '{']
        closeBrackets = [']', ')', '}']
        insideBracketCounter = 0
        inp = inp.replace(' ', '')
        inp = list(inp)
        for i in range(0, len(inp)):
            if inp[i] in openBrackets:
                insideBracketCounter += 1
            elif inp[i] in closeBrackets:
                insideBracketCounter -= 1
            else:
                pass
            if inp[i] == ',' and insideBracketCounter == 1:
                inp[i] = ';'
            elif inp[i] == ',' and insideBracketCounter == 2:
                pass
            else:
                pass
        inp = ''.join(inp)
        inp = inp[1:]
        inp = inp[:-1]
        out = []
        if inp == '':
            return out
        inp = inp.split(';')
        out = inp
        return out

class InterfaceDCQuery(object):
    
    def __init__(self, dcFileName, numOfSamples, allFunctors):
        self.yapObj = YapInterface()
        self.yapObj.consultWithOneFile(dcFileName)
        self.RANDOM_VARIABLE_PREDICATE = None
        self.RANDOM_VARIABLE_PREDICATE_RANGE = None
        self.RANDOM_VARIABLE_PREDICATE_TYPE = None
        self.analyzeFunctors(allFunctors)
        self.predictUncertainty = None
        self.numOfSamples = numOfSamples
    
    def setpredictUncertainty(self, value):
        self.predictUncertainty = value
        
    def analyzeFunctors(self, allFunctors):
        self.RANDOM_VARIABLE_PREDICATE_RANGE = dict()
        self.RANDOM_VARIABLE_PREDICATE = []
        self.RANDOM_VARIABLE_PREDICATE_TYPE = dict()
        for func in allFunctors:
            if func.type == 'attribute' or func.type == 'relation':
                self.RANDOM_VARIABLE_PREDICATE.append(func.name)
            
            if func.type == 'relation':
                self.RANDOM_VARIABLE_PREDICATE_TYPE[func.name] = 'discrete'
            elif func.type == 'attribute':
                if func.variableType == 'continuous':
                    self.RANDOM_VARIABLE_PREDICATE_TYPE[func.name] = 'continuous'
                elif func.variableType == 'discrete':
                    self.RANDOM_VARIABLE_PREDICATE_TYPE[func.name] = 'discrete'
                else:
                    pass
            if func.type == 'relation':
                self.RANDOM_VARIABLE_PREDICATE_RANGE[func.name] = None
            elif func.type == 'attribute':
                if func.variableType == 'continuous':
                    self.RANDOM_VARIABLE_PREDICATE_RANGE[func.name] = None
                elif func.variableType == 'discrete':
                    dom = func.domain 
                    dom = dom.strip('][').split(',') 
                    self.RANDOM_VARIABLE_PREDICATE_RANGE[func.name] = dom
                else:
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
            if featName in self.RANDOM_VARIABLE_PREDICATE:
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

    def assertPrologFactsWithEvidencePredicate(self, facts, yapObj):
        for fact in facts:
            query = 'asserta(evidence(' + fact + '))'
            success = yapObj.executeQuery(query)
            if success == False:
                raise Exception('Yap execute failed!')
                
    def retractPrologFactsWithEvidencePredicate(self, facts, yapObj):
        for fact in facts:
            query = 'retract(evidence(' + fact + '))'
            success = yapObj.executeQuery(query)
            if success == False:
                raise Exception('Yap execute failed!')

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
    

    # qy of the form 'savings(a_11, null).'
    # evidenceList should be of the form: ['savings(a_11, 1000)', 'age(c_20,30)']
    # inputInterventionList should be of the form:  ['hasLoan(a_11,l_20)', 'account(a_10)']
    def query(self, inputEvidence, qy, inputInterventionList):        
        qy += '.'
        self.assertPrologFactsWithEvidencePredicate(inputEvidence, self.yapObj)
        self.assertPrologFactsWithEvidencePredicate(inputInterventionList, self.yapObj) 
        for item in inputInterventionList:
            item = item + '.'
            item = self.findListOfDCFacts([item])[0]
            item = item[0:-1]
            self.interventionAssert(item, self.yapObj)
        feat = qy
        feat = self.dcPredicateAndValue(feat)
        variable = '[X]'
        query = '(' + feat[0] + ' ~= X)'
        evidenceTest = ''
        tempTestFact_null = feat[0][0:-1] + ',null)'
        tempTestFact_X = feat[0][0:-1] + ',X)'
        res = self.yapObj.requisiteEvidence(tempTestFact_null, tempTestFact_X)        
        for ele in res['ProbEvidence']:
            evidence = ele + '.'
            feat = self.dcPredicateAndValue(evidence)
            if evidenceTest == '':
                evidenceTest = evidenceTest + feat[0] + '~=' + feat[1]
            else:
                evidenceTest = evidenceTest + ',' + feat[0] + '~=' + feat[1]
        for item in res['Intervention']:
            item = item + '.'
            item = self.findListOfDCFacts([item])[0]
            item = item[0:-1]
            self.interventionAssert(item, self.yapObj)
        result = self.yapObj.queryWithSamples(self.numOfSamples, query, evidenceTest, variable)
        
        samples = result['samples']
        samplesWithProb = self.generateSamplesWithProb(samples, self.yapObj)
        prediction = self.generateDistribution(samplesWithProb, qy)
        for item in res['Intervention']:
            item = item + '.'
            item = self.findListOfDCFacts([item])[0]
            item = item[0:-1]
            self.interventionRetract(item, self.yapObj)
        self.retractPrologFactsWithEvidencePredicate(inputEvidence, self.yapObj)
        self.retractPrologFactsWithEvidencePredicate(inputInterventionList, self.yapObj)        
        for item in inputInterventionList:
            item = item + '.'
            item = self.findListOfDCFacts([item])[0]
            item = item[0:-1]
            self.interventionRetract(item, self.yapObj)
        return prediction
    
    def generateDistribution(self, samples, query):
        featureName = self.getPredicateName(query)
        ty = self.RANDOM_VARIABLE_PREDICATE_TYPE[featureName]
        prediction = None
        if ty == 'continuous':
            if len(samples) < 2:
                pass
            else:
                average = 0
                for sample in samples:
                    average += sample[0][0]*sample[3]
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
                
                if self.predictUncertainty:
                    prediction = 'gaussian(' + str(average) + ',' + str(std) + ')'
                else:
                    prediction = str(average)
        elif ty == 'discrete':
            if len(samples) == 0:
                pass
            else:
                counts = {}
                for sample in samples:
                    if sample[0][0] in counts:
                        counts[sample[0][0]] += sample[3]
                    else:
                        counts[sample[0][0]] = sample[3]
                sumOfAllCounts = 0
                for key in counts.keys():
                    sumOfAllCounts += counts[key]
                yPred = list(counts)[0]
                yVal = counts[yPred]/float(sumOfAllCounts)
                for key in counts.keys():
                    if counts[key]/float(sumOfAllCounts) > yVal:
                        yPred = key
                        yVal = counts[key]/float(sumOfAllCounts)
                distribution = 'finite(['
                for ele in self.RANDOM_VARIABLE_PREDICATE_RANGE[featureName]:
                    if ele in counts: 
                        prob = counts[ele]/sumOfAllCounts
                        distribution += str(prob) + ':' + ele
                        if ele == self.RANDOM_VARIABLE_PREDICATE_RANGE[featureName][-1]:
                            pass
                        else:
                            distribution += ','
                    else:
                        distribution += str(0.0) + ':' + ele
                        if ele == self.RANDOM_VARIABLE_PREDICATE_RANGE[featureName][-1]:
                            pass
                        else:
                            distribution += ','
                distribution += '])'            
                if self.predictUncertainty:
                    prediction = distribution
                else:
                    prediction = str(yPred)
        else:
            pass
        return prediction
        
    
    
    
    
    
    
    
    