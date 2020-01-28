'''
Created on Aug 29, 2017

@author: niteshkumar
'''
import ast, re
from TranslateToDC import TranslateToDC
import logging

## Build yapWrapper.so using : $ python setup.py build_ext --inplace
## Place yapWrapper.so in the current folder
from yapWrapper import PyDC

BIGNUM = 1000000
VARIABLE_NUMBER = 3
FLAG = 0

class YapPrologInterface(object):
    
    def __init__(self, randomVariableName = []):
        logging.basicConfig(level=logging.INFO, filename="dcProbLogs", filemode="w", format="%(asctime)-15s %(levelname)-8s %(message)s")
        self.yapPrologInterface_logger = logging.getLogger('YapPrologInterface Logger :')
        yapPrologInterface_logger_handler = logging.FileHandler("dcProbLogs", mode="w")
        yapPrologInterface_logger_handler.setFormatter(logging.Formatter("%(asctime)-15s %(levelname)-8s %(message)s"))
        yapPrologInterface_logger_handler.setLevel(logging.INFO)
        self.yapPrologInterface_logger.addHandler(yapPrologInterface_logger_handler)
        self.translator = TranslateToDC()
        self.numOfSamples = 0
        self.yapObject = None
        self.yapFileName = []
        if randomVariableName != []:
            self.setRandomVariablePredicates(randomVariableName)
    
    def setRandomVariablePredicates(self, name):
        self.translator.setRandomVariablePredicates(name)
        
    
    def consultWithOneFile(self, yapFileName):
        self.yapObject = PyDC(yapFileName)
        self.yapFileName.append(yapFileName)
        
    def consultWithTwoFiles(self, yapFileName1, yapFileName2, numOfSamples):
        self.yapObject = PyDC(yapFileName1, yapFileName2)
        self.yapFileName.append(yapFileName1)
        self.yapFileName.append(yapFileName2)
        self.numOfSamples = numOfSamples
    
    def setNumOfSamples(self, num):
        self.numOfSamples = num
    
    def query(self, numOfSamples, query, evidence):
        probability = self.yapObject.query(numOfSamples, query, evidence)
        return probability
    
    def terminate(self):
        self.yapObject.terminate()
    
    def executeQuery(self, queryStr):
        success = self.yapObject.execute(queryStr)
        return success
    
    def sample(self, numOfSamples, query, evidence, variable):
        return self.yapObject.sample(numOfSamples, query, evidence, variable, BIGNUM)
    
    def prologQuery(self, query):
        st = self.yapObject.prologQuery(VARIABLE_NUMBER, query, BIGNUM)
        return st
    
    def queryWithSamples(self, numOfSamples, query, evidence, variable):
        self.yapObject.queryWithSamples(numOfSamples, query, evidence, variable, FLAG, BIGNUM)
        result = dict()
        result['probability'] = self.yapObject.prob
        result['samples'] = self.yapObject.samples
        return result

    def requisiteEvidence(self, query):
        query = 'requsite(' + query + ',ProbEvidence,Intervention).'
        self.yapObject.requisiteEvidence(query, BIGNUM)
        result = dict()
        result['ProbEvidence'] = self.convertStringReprOfListToList2(self.yapObject.probEvidence)
        result['Intervention'] = self.convertStringReprOfListToList2(self.yapObject.intervention)
        return result
    
    def simpleQuery(self, query):
        query = query.replace(' ', '')
        listOfVars = re.findall('\((.*)\)', query)[0]
        listOfVars = listOfVars.split(',')
        result = self.listQuery(query, listOfVars)
        keys = result.keys()
        akey = keys[0]
        avalue = result[akey]
        res = []
        for i in range(0, len(avalue)):
            tempres = {}
            for key in keys:
                tempres[key] = result[key][i]
            res.append(tempres)
        return res
    
    ## Merge this with listQuery function
    def listQueryPatch(self, query, listOfVars):
        result = {}
        for var in listOfVars:
            result[var] = []
        stringOfVars = '['
        lenOfListOfVars = len(listOfVars)
        for i in range(0, lenOfListOfVars):
            if i == lenOfListOfVars-1:
                stringOfVars += listOfVars[i] + ']'
            else:
                stringOfVars += listOfVars[i] + ','
        if query[-1] == '.':
            query = query[:-1]
        query = 'findall(' + stringOfVars + ',' + '(' + query + '),FINDALL).'
        response = self.prologQuery(query)
        response = response[1:-1]
        response = self.convertStringReprOfListToList(response)

        for i in range(0, len(listOfVars)):
            result[listOfVars[i]] = response[i]
        s = 'Query: ', query, '   Response ' + str(result)
        logging.info(s)
        return result
    
    def listQuery(self, query, listOfVars):
        result = {}
        for var in listOfVars:
            result[var] = []
        stringOfVars = '['
        lenOfListOfVars = len(listOfVars)
        for i in range(0, lenOfListOfVars):
            if i == lenOfListOfVars-1:
                stringOfVars += listOfVars[i] + ']'
            else:
                stringOfVars += listOfVars[i] + ','
        if query[-1] == '.':
            query = query[:-1]
        query = 'findall(' + stringOfVars + ',' + '(' + query + '),FINDALL).'
        response = self.prologQuery(query)
        response = self.convertStringReprOfListToList(response)
        for ele in response:
            for i in range(0, len(listOfVars)):
                a = result[listOfVars[i]]
                a.append(ele[i])
        s = 'Query: ', query, '   Response ' + str(result)
        logging.info(s)
        return result

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
        
    def getProbabilityOfQuery(self, query, body, listOfSubstitutionVars):
        result = self.listQuery(query, listOfSubstitutionVars)
        keys = result.keys()
        akey = keys[0]
        avalue = result[akey]
        response = []
        for i in range(0, len(avalue)):
            tempres = {}
            for key in keys:
                tempres[key] = result[key][i]
            response.append(tempres)
        prob = []
        for ele in response:
            subs = {}
            for var in listOfSubstitutionVars:
                if var in ele.keys():
                    subs[var] = ele[var]
            dcQuery = self.generateDCQuery(body, subs)
            p = self.query(self.numOfSamples, "(" + dcQuery + ")", "")
            s = str(p) + ' ' + dcQuery
            self.yapPrologInterface_logger.info(s)
            prob.append(p)   
        return prob
    
    def getSamplesAndProbabilityOfQuery(self, query, body, listOfSubstitutionVars, listOfContinuousVars):
        stringOfContVars = '['
        lenOfListOfContVars = len(listOfContinuousVars)
        for i in range(0, lenOfListOfContVars):
            if i == lenOfListOfContVars-1:
                stringOfContVars += listOfContinuousVars[i] + ']'
            else:
                stringOfContVars += listOfContinuousVars[i] + ','
        result = self.listQuery(query, listOfSubstitutionVars)
        keys = result.keys()
        akey = keys[0]
        avalue = result[akey]
        response = []
        for i in range(0, len(avalue)):
            tempres = {}
            for key in keys:
                tempres[key] = result[key][i]
            response.append(tempres)
        prob = []
        for ele in response:
            subs = {}
            for var in listOfSubstitutionVars:
                if var in ele.keys():
                    subs[var] = ele[var]
            dcQuery = self.generateDCQuery(body, subs)
            probAndSamples = self.queryWithSamples(self.numOfSamples, "(" + dcQuery + ")", "", stringOfContVars)
            samples = probAndSamples['samples']
            probability = probAndSamples['probability']
            listSamples = self.convertStringReprOfListToList(samples)
            acceptedSamples = []
            sumW1 = 0
            for i in range(0, len(listSamples)):
                if listSamples[i][2] == 0:
                    pass
                else:
                    acceptedSamples.append(listSamples[i])
                sumW1 += listSamples[i][1]
            for i in range(0, len(acceptedSamples)):
                p = (acceptedSamples[i][1] * acceptedSamples[i][2])/sumW1
                acceptedSamples[i].append(p)
            s = str(probability) + ' ' + dcQuery + ' ' + str(acceptedSamples)
            self.yapPrologInterface_logger.info(s)
            prob.append(acceptedSamples)   
        return prob
    
    def generateDCQuery(self, query, subs):
        query = query.replace('\\\\+', '\\+')
        query = query.replace(' ', '')
        for key in subs.keys():
            keyStr = '(' + key + ')'
            valStr = '(' + str(subs[key]) + ')'
            query = query.replace(keyStr, valStr)
            keyStr = ',' + key + ')'
            valStr = ',' + str(subs[key]) + ')'
            query = query.replace(keyStr, valStr)
            keyStr = ',' + key + ','
            valStr = ',' + str(subs[key]) + ','
            query = query.replace(keyStr, valStr)
            keyStr = '(' + key + ','
            valStr = '(' + str(subs[key]) + ','
            query = query.replace(keyStr, valStr)
        query = self.translator.convertDCPredicates(query, 'body')
        return query
    
    def getProlog(self):
        return self.yapObject
    
if __name__ == '__main__':
    #obj = YapPrologInterface()
    #obj.consultWithOneFile('../program/testSampling.pl')
    #obj.setNumOfSamples(10)
    #body = 'stress(S, Y), rating(S, Z)'
    #query = 'smokes(S, X),stress(S, Y), rating(S, Z)'
    #res = obj.getSamplesAndProbabilityOfQuery(query, body, ['S','X'], ['Z','Y'])
    #print res
    
    obj = YapPrologInterface()
    obj.consultWithTwoFiles('../data/FinancialData_Enumerated.pl', '../data/FinancialDataDC.pl', 10)
    query = '(findall_forward(Ca_M,(hasAccount(C_M,a_10630)~=true,age(C_M)~=Ca_M),X_T_12_Temp),avg(X_T_12_Temp)~=X_T_12)'
    res = obj.queryWithSamples(2, query, '', '[X_T_12]')
    print res
    
    
    