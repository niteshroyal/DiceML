'''
Created on Aug 18, 2017

@author: niteshkumar
'''
import itertools, copy
from Predicate import Predicate

class TypesModesAggregationLanguage(object):
    def __init__(self, prologInterface):
        self.dictOfPredicates = {}
        self.target = []
        self.prologInterface = prologInterface
        self.typeConstants = {}
        self.freeVariableCount = 1
        self.readTypes()
        self.readModes()
        self.readThresholds()
        self.readTarget()
        self.fillTypeConstants()
        self.targetPredicateName = ''
        self.randomVariableNames = ''
        self.readRandomVariableNames()
        self.prologInterface.setRandomVariablePredicates(self.randomVariableNames)
        self.rankDeclarationPresent = False
        self.readRank()

        
    def setTargetPredicateName(self, name):
        self.targetPredicateName = name
    
    def setFreeVariable(self):
        self.freeVariableCount = 1
        
    def fillTypeConstants(self):
        for key in self.dictOfPredicates.keys():
            predicate = self.dictOfPredicates[key]
            types = predicate.getTypes()
            name = predicate.getName()
            arity = predicate.getArity()
            query = name + '('
            variables = []
            idx = 0
            for ty in types:
                variables.append(self.generateNewVariable(ty, 0, ''))
                if idx == arity - 1:
                    query += variables[idx]
                    idx += 1
                else:
                    query += variables[idx] + ', '
                    idx += 1
            query += ').'
            results = list(self.prologInterface.simpleQuery(query))
            for m in results:
                idx = 0
                for v in variables:
                    if(self.typeConstants.has_key(types[idx])):
                        st = self.typeConstants[types[idx]]
                        st.add(m[v])
                    else:
                        self.typeConstants[types[idx]] = {m[v]}
                    idx += 1
    def generateTargetPredicateVariables(self, predicate):
        types = predicate.getTypes()
        variables = []
        dictOfVariablesTemp = dict()
        for ty in types:
            if dictOfVariablesTemp.has_key(ty):
                varPresentAlready = dictOfVariablesTemp[ty]
                idx = len(varPresentAlready)-1
                tempV = self.generateNewVariable(ty, 0, '') + str(idx+1)
                varPresentAlready.append(tempV)
                variables.append(tempV)
            else:
                tempV = self.generateNewVariable(ty, 0, '')
                dictOfVariablesTemp[ty] = [tempV]
                variables.append(tempV)
        predicate.addVariables(variables)
        return predicate
    
    def generateNewVariable(self, ty, count, code):
        tempTy = list(ty)
        tempTy[0] = tempTy[0].upper()
        ty = ''.join(tempTy)
        if code == '':
            if count == 0:
                pass
            else:
                ty += '_' + str(count)
        elif code == 'T':
            ty += '_' + code + '_' + str(self.freeVariableCount)
            self.freeVariableCount += 1
        else:
            if count == 0:
                ty += '_' + code
            else:
                ty += '_' + code + '_' + str(count)
        return ty
    
    def generateTypeModeConformPredicates(self, currentPredicateList, targetPredicate, isLastPredicateConditionInAggr):
        dictTypes = {}
        alreadyUsedVariables = {}
        for predicate in currentPredicateList:
            types = predicate.getTypes()
            currentAggr = predicate.getCurrentAggr()
            modes = predicate.getModes(self.targetPredicateName, currentAggr)
            negated = predicate.getNegation()
            aggrApplied = False
            if(currentAggr != 'none'):
                aggrApplied = True
            variables = predicate.getVariables()
            idx = 0
            for ty in types:
                if ((not aggrApplied) or (modes[idx] == '+')) and (not negated):
                    if dictTypes.has_key(ty):
                        t = dictTypes[ty]
                        t.add(variables[idx])
                    else:
                        dictTypes[ty] = {variables[idx]}
                else:
                    pass
                if alreadyUsedVariables.has_key(ty):
                    t = alreadyUsedVariables[ty]
                    t.add(variables[idx])
                else:
                    alreadyUsedVariables[ty] = {variables[idx]}
                idx += 1
        dictTypesSpecialCase = {}
        if isLastPredicateConditionInAggr:
            predicate = currentPredicateList[-1]
            types = predicate.getTypes()
            variables = predicate.getVariables()
            idx = 0
            for ty in types:
                if dictTypesSpecialCase.has_key(ty):
                    t = dictTypesSpecialCase[ty]
                    t.add(variables[idx])
                else:
                    dictTypesSpecialCase[ty] = {variables[idx]}
                idx += 1
        targetTypes = targetPredicate.getTypes()
        currentTargetAggr = targetPredicate.getCurrentAggr()
        targetModes = targetPredicate.getModes(self.targetPredicateName, currentTargetAggr)
        arity = targetPredicate.getArity()
        variables = []
        for i in range(0, arity):
            variables.append(None)
        idx = 0
        inputVarSet = set()
        for mode in targetModes:
            if mode == '-':
                ty = targetTypes[idx]
                if alreadyUsedVariables.has_key(ty):
                    temp = alreadyUsedVariables[ty]
                    for ele in temp:
                        inputVarSet.add(ele)
                else:
                    pass
                x = 0
                newVarGenerated = self.generateNewVariable(ty, x, 'M')
                while newVarGenerated in inputVarSet:
                    x += 1
                    newVarGenerated = self.generateNewVariable(ty, x, 'M')
                variables[idx] = {newVarGenerated}
                inputVarSet.add(newVarGenerated)
            elif mode == '+':
                ty = targetTypes[idx]
                if dictTypes.has_key(ty):
                    v = dictTypes[ty]
                    variables[idx] = v
                else:
                    variables[idx] = None
                if isLastPredicateConditionInAggr:
                    if dictTypesSpecialCase.has_key(ty):
                        variables[idx] = dictTypesSpecialCase[ty]
                    else:
                        pass
                else:
                    pass
            elif mode == 'c':
                ty = targetTypes[idx]
                if self.typeConstants.has_key(ty):
                    v = self.typeConstants[ty]
                    variables[idx] = v
                else:
                    variables[idx] = None
            elif mode == 'true':
                variables[idx] = {'true'}
            else:
                pass
            idx += 1
        for v in variables:
            if v == None:
                return []
        variablesList = list(itertools.product(*variables))
        targetPredicateList = []
        for v in variablesList:
            newTPredicate = copy.copy(targetPredicate)
            newTPredicate.addVariables(list(v))
            targetPredicateList.append(newTPredicate)
        return targetPredicateList
    
    def readTypes(self):
        types = list(self.prologInterface.simpleQuery('base(X).'))
        for t in types:
            t = t['X']
            predicate = None
            typ = []
            predicateName = ''
            predicateType = ''
            addPredicateName = True
            addPredicateType = False
            for st in t:
                if(st == ' ' or st == ''):
                    continue
                else:
                    if(st == '('):
                        predicate = Predicate()
                        predicate.setName(predicateName)
                        addPredicateType = True
                        addPredicateName = False
                        continue
                    elif(st == ',' or st == ')'):
                        typ.append(predicateType)
                        predicateType = ''
                        addPredicateType = True
                        continue
                    else:
                        pass
                    if(addPredicateName):
                        predicateName += st
                    elif(addPredicateType):
                        predicateType += st
                    else:
                        pass
            predicate.addTypes(typ)
            predicate.addArity(len(typ))
            self.dictOfPredicates[predicateName] = predicate
    
    def readRank(self):
        rank = list(self.prologInterface.simpleQuery('rank(X).'))
        for r in rank:
            rankList = r['X']
            for i in range(0,len(rankList)):
                if self.dictOfPredicates.has_key(rankList[i]):
                    predicate = self.dictOfPredicates[rankList[i]]
                    predicate.setRank(i)
            if rankList != []:
                self.rankDeclarationPresent = True
            else:
                pass
            break

    def readRandomVariableNames(self):
        nameList = list(self.prologInterface.simpleQuery('randomVariableNames(X).'))
        for n in nameList:
            self.randomVariableNames = n['X']
            break
        
    
    def readModes(self):
        modes = list(self.prologInterface.simpleQuery('mode(Z,X,Y).'))
        for m in modes:
            m1 = m
            m = m['Y']
            if m[0] == ',':
                m = m[2:len(m)-1]
            elif m[0] == '(':
                m = m[1:len(m)-1]
            else:
                pass
            predicate = None
            conditionalPredicates = []
            mod = []
            predicateName = ''
            predicateType = ''
            addPredicateName = True
            addPredicateType = False
            insideBracket = False
            for st in m:
                if(st == ' ' or st == ''):
                    continue
                else:
                    if(st == '('):
                        predicate = Predicate()
                        predicate.setName(predicateName)
                        p = self.dictOfPredicates[predicateName]
                        predicate.addTypes(p.getTypes())
                        addPredicateType = True
                        insideBracket = True
                        addPredicateName = False
                        continue
                    elif(st == ',' and insideBracket):
                        mod.append(predicateType)
                        predicateType = ''
                        addPredicateType = True
                        continue
                    elif(st == ')' and insideBracket):
                        mod.append(predicateType)
                        predicateType = ''
                        addPredicateType = True
                        insideBracket = False
                        continue
                    elif(st == ',' and not insideBracket):
                        predicate.addModes(m1['Z'], 'none', mod, [])
                        predicate.addArity(len(mod))
                        conditionalPredicates.append(predicate)
                        mod = []
                        predicateName = ''
                        predicateType = ''
                        addPredicateName = True
                        continue
                    else:
                        pass
                    if(addPredicateName):
                        predicateName += st
                    elif(addPredicateType):
                        predicateType += st
                    else:
                        pass
            predicate.addModes(m1['Z'], m1['X'], mod, conditionalPredicates)
            predicate.addArity(len(mod))
            if(self.dictOfPredicates.has_key(predicateName)):
                predicate1 = self.dictOfPredicates[predicateName]
                predicate1.addModes(m1['Z'], m1['X'], mod, conditionalPredicates)
                predicate1.appendAggregators(m1['Z'], m1['X'])
            else:
                predicate.appendAggregators(m1['Z'], m1['X'])
                self.dictOfPredicates[predicateName] = predicate
    
    def readThresholds(self):
        modes = list(self.prologInterface.simpleQuery('thres(W,X,Y,Z).'))
        for m in modes:
            predicate = self.dictOfPredicates[m['W']]
            predicate.appendThreshold(m['X'], m['Z'])
            predicate.appendRandomVariableType(m['X'], m['Y'])
    
    def readTarget(self):
        modes = list(self.prologInterface.simpleQuery('learn(W,X,Y,Z).'))
        for m in modes:
            name = m['W']
            arity = m['X']
            randomValLocation = m['Y']
            randomValType = m['Z']
            self.target.append({'target': name, 'arity': arity, 'targetLocation': randomValLocation, 'randomValType': randomValType})
