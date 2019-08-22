'''
Created on Aug 18, 2017

@author: niteshkumar
'''
import copy

class Feature(object):
    
    def __init__(self):
        self.variables = None
        self.randomVariableType = ''
        self.featureLocation = -1
        self.predicateName = ''
        self.aggregator = ''
        self.thresholds = None
        self.arity = -1
        self.isContinuos = False
        self.negationApplied = False
        self.isCondInsideAggr = False
        self.condInsideAggr = None
        self.isAggrAndContinuous = False
        self.aggrAndContRandomVariable = None
        self.rank = 0
    
    def setCondInsideAggr(self, pred):
        self.condInsideAggr = pred
        
    def getCondInsideAggr(self):
        return self.condInsideAggr
    
    def setNegation(self, flag):
        self.negationApplied = flag
        
    def getNegation(self):
        return self.negationApplied
    
    def setArity(self, a):
        self.arity = a
    
    def getArity(self):
        return self.arity
    
    def setThresholds(self, t):
        self.thresholds = t
    
    def getThresholds(self):
        l = []
        for thres in self.thresholds:
            if type(thres) is 'Atom': #Should be: if type(thres) is Atom:
                l.append(thres.get_value())
            else:
                l.append(thres)
        return l
    
    def setAggregator(self, a):
        self.aggregator = a
        
    def getAggregator(self):
        return self.aggregator
    
    def setRank(self, n):
        self.rank = n
        
    def getRank(self):
        return self.rank
        
    def setPredicateName(self, n):
        self.predicateName = n
        
    def getPredicateName(self):
        return self.predicateName
            
    def setVariables(self, l):
        self.variables = l
        
    def getVariables(self):
        return self.variables
    
    def setRandomVariableType(self, t):
        self.randomVariableType = t
        
    def getRandomVariableType(self):
        return self.randomVariableType
    
    def setFeatureLocation(self, loc):
        self.featureLocation = loc
        
    def getFeatureLocation(self):
        return self.featureLocation
    
    def getContVariables(self):
        if self.isContinuos and not self.isAggrAndContinuous:
            return [self.variables[self.featureLocation-1]]
        elif self.isContinuos and self.isAggrAndContinuous:
            return self.aggrAndContRandomVariable
        else:
            return []
        
    def featureAsListOfString(self, language):
        features = []
        if self.aggregator == 'none':
            if self.randomVariableType == 'continuous' and len(self.thresholds)<1:
                stFeature = ''
                stFeature += self.predicateName + '('
                j = 0
                lenj = len(self.variables)-1
                for i in range(0, len(self.variables)):
                    if(j==lenj):
                        stFeature += self.variables[i] + ')'
                        j += 1
                    else:
                        stFeature += self.variables[i] + ', '
                        j += 1
                features.append(stFeature)
                stFeature = '\+'
                stFeature += self.predicateName + '('
                j = 0
                lenj = len(self.variables)-1
                for i in range(0, len(self.variables)):
                    if(j==lenj):
                        stFeature += self.variables[i] + ')'
                        j += 1
                    else:
                        stFeature += self.variables[i] + ', '
                        j += 1
                features.append(stFeature)
                self.isContinuos = True
            elif self.randomVariableType == 'continuous' and len(self.thresholds)>=1:
                thresholds = self.getThresholds()
                k = 1
                while k<=len(thresholds)-1:
                    stFeature = ''
                    stFeature += self.predicateName + '('
                    j = 0
                    lenj = len(self.variables)-1
                    for i in range(0, len(self.variables)):
                        if(j==lenj):
                            stFeature += self.variables[i] + ')'
                            j += 1
                        else:
                            stFeature += self.variables[i] + ', '
                            j += 1
                    if(thresholds[k-1] == '-'):
                        stFeature += ', ' + self.variables[self.featureLocation - 1] + ' < ' + str(thresholds[k])
                    elif(thresholds[k] == '+'):
                        stFeature += ', ' + self.variables[self.featureLocation - 1] + ' > ' + str(thresholds[k-1])
                    else:
                        stFeature += ', ' + self.variables[self.featureLocation - 1] + ' >= ' + str(thresholds[k-1]) + ', ' + self.variables[self.featureLocation - 1] + ' <' + str(thresholds[k])
                    features.append(stFeature)
                    k += 1
                stFeature = '\+'
                stFeature += self.predicateName + '('
                j = 0
                lenj = len(self.variables)-1
                for i in range(0, len(self.variables)):
                    if(j==lenj):
                        stFeature += self.variables[i] + ')'
                        j += 1
                    else:
                        stFeature += self.variables[i] + ', '
                        j += 1
                features.append(stFeature)
            elif self.randomVariableType == 'discrete':
                thresholds = self.getThresholds()
                for thres in thresholds:
                    stFeature = ''
                    stFeature += self.predicateName + '('
                    j = 0
                    lenj = len(self.variables)-1
                    for i in range(0, len(self.variables)):
                        if(j==lenj):
                            stFeature += self.variables[i] + ')'
                            j += 1
                        else:
                            stFeature += self.variables[i] + ', '
                            j += 1
                    stFeature += ', ' + self.variables[self.featureLocation - 1] + ' == ' + str(thres)
                    features.append(stFeature)
                stFeature = '\+'
                stFeature += self.predicateName + '('
                j = 0
                lenj = len(self.variables)-1
                for i in range(0, len(self.variables)):
                    if(j==lenj):
                        stFeature += self.variables[i] + ')'
                        j += 1
                    else:
                        stFeature += self.variables[i] + ', '
                        j += 1
                features.append(stFeature)
            elif self.randomVariableType == '':
                stFeature = ''
                stFeature += self.predicateName + '('
                j = 0
                lenj = len(self.variables)-1
                for i in range(0, len(self.variables)):
                    if(j==lenj):
                        stFeature += self.variables[i] + ')'
                        j += 1
                    else:
                        stFeature += self.variables[i] + ', '
                        j += 1
                features.append(stFeature)
                stFeature = '\+'
                stFeature += self.predicateName + '('
                j = 0
                lenj = len(self.variables)-1
                for i in range(0, len(self.variables)):
                    if(j==lenj):
                        stFeature += self.variables[i] + ')'
                        j += 1
                    else:
                        stFeature += self.variables[i] + ', '
                        j += 1
                features.append(stFeature)
            else:
                pass
        else:
            if self.randomVariableType == 'continuous' and len(self.thresholds)<1:
                stFeature = '' + self.aggregator + '(' + self.variables[self.featureLocation - 1] + ', '
                if self.isCondInsideAggr:
                    stFeature += '(' + self.condInsideAggr.toString() + ', '
                stFeature += self.predicateName + '('
                j = 0
                lenj = len(self.variables)-1
                for i in range(0, len(self.variables)):
                    if(j==lenj):
                        stFeature += self.variables[i] + ')'
                        j += 1
                    else:
                        stFeature += self.variables[i] + ', '
                        j += 1
                if self.isCondInsideAggr:
                    stFeature += ')'
                xVar = language.generateNewVariable('x', -1, 'T')
                stFeature += ', ' + xVar + ')'
                features.append(stFeature)
                self.aggrAndContRandomVariable = [xVar]
                stFeature = '\+' + self.aggregator + '(' + self.variables[self.featureLocation - 1] + ', '
                if self.isCondInsideAggr:
                    stFeature += '(' + self.condInsideAggr.toString() + ', '
                stFeature += self.predicateName + '('
                j = 0
                lenj = len(self.variables)-1
                for i in range(0, len(self.variables)):
                    if(j==lenj):
                        stFeature += self.variables[i] + ')'
                        j += 1
                    else:
                        stFeature += self.variables[i] + ', '
                        j += 1
                if self.isCondInsideAggr:
                    stFeature += ')'
                xVar = language.generateNewVariable('x', -1, 'T')
                stFeature += ', ' + xVar + ')'
                features.append(stFeature)
                self.isContinuos = True
                self.isAggrAndContinuous = True
                
            elif self.randomVariableType == 'continuous' and len(self.thresholds)>=1:
                k = 1
                thresholds = self.thresholds
                while k<=len(thresholds)-1:
                    stFeature = '' + self.aggregator + '(' + self.variables[self.featureLocation - 1] + ', '
                    if self.isCondInsideAggr:
                        stFeature += '(' + self.condInsideAggr.toString() + ', '
                    stFeature += self.predicateName + '('
                    j = 0
                    lenj = len(self.variables)-1
                    for i in range(0, len(self.variables)):
                        if(j==lenj):
                            stFeature += self.variables[i] + ')'
                            j += 1
                        else:
                            stFeature += self.variables[i] + ', '
                            j += 1
                    if self.isCondInsideAggr:
                        stFeature += ')'
                    xVar = language.generateNewVariable('x', -1, 'T')
                    stFeature += ', ' + xVar + ')'
                    if(thresholds[k-1] == '-'):
                        stFeature += ', ' + xVar + ' < ' + thresholds[k]
                    elif(thresholds[k] == '+'):
                        stFeature += ', ' + xVar + ' > ' + thresholds[k-1]
                    else:
                        stFeature += ', ' + xVar + ' >= ' + thresholds[k-1] + ', ' + xVar + ' <' + thresholds[k-1]
                    features.append(stFeature)
                    k += 1
                stFeature = '\+' + self.aggregator + '(' + self.variables[self.featureLocation - 1] + ', '
                if self.isCondInsideAggr:
                    stFeature += '(' + self.condInsideAggr.toString() + ', '
                stFeature += self.predicateName + '('
                j = 0
                lenj = len(self.variables)-1
                for i in range(0, len(self.variables)):
                    if(j==lenj):
                        stFeature += self.variables[i] + ')'
                        j += 1
                    else:
                        stFeature += self.variables[i] + ', '
                        j += 1
                if self.isCondInsideAggr:
                    stFeature += ')'
                xVar = language.generateNewVariable('x', -1, 'T')
                stFeature += ', ' + xVar + ')'
                features.append(stFeature)
            elif self.randomVariableType == 'discrete':
                for thres in self.getThresholds():
                    stFeature = '' + self.aggregator + '(' + self.variables[self.featureLocation - 1] + ', '
                    if self.isCondInsideAggr:
                        stFeature += '(' + self.condInsideAggr.toString() + ', '
                    stFeature += self.predicateName + '('
                    j = 0
                    lenj = len(self.variables)-1
                    for i in range(0, len(self.variables)):
                        if(j==lenj):
                            stFeature += self.variables[i] + ')'
                            j += 1
                        else:
                            stFeature += self.variables[i] + ', '
                            j += 1
                    if self.isCondInsideAggr:
                        stFeature += ')'
                    xVar = language.generateNewVariable('x', -1, 'T')
                    stFeature += ', ' + xVar + ')'
                    stFeature += ', ' + xVar + ' == ' + thres
                    features.append(stFeature)
                stFeature = '\+' + self.aggregator + '(' + self.variables[self.featureLocation - 1] + ', '
                if self.isCondInsideAggr:
                    stFeature += '(' + self.condInsideAggr.toString() + ', '
                stFeature += self.predicateName + '('
                j = 0
                lenj = len(self.variables)-1
                for i in range(0, len(self.variables)):
                    if(j==lenj):
                        stFeature += self.variables[i] + ')'
                        j += 1
                    else:
                        stFeature += self.variables[i] + ', '
                        j += 1
                if self.isCondInsideAggr:
                    stFeature += ')'
                xVar = language.generateNewVariable('x', -1, 'T')
                stFeature += ', ' + xVar + ')'
                features.append(stFeature)
            else:
                pass
        return features

class FeatureCreator(object):
    def __init__(self, language, name):
        self.featureList = []
        self.targetPredicateName = name
        self.language = language
        pass
    
    def fillFeatureList(self):
        dictOfPredicates = self.language.dictOfPredicates
        for key in dictOfPredicates.keys():
            predicate = dictOfPredicates[key]
            variables = []
            for var in range(0, predicate.getArity()):
                variables.append('_')
            for aggregator in predicate.getAggregators():
                tempAggregator = aggregator.split(':::')
                if self.targetPredicateName != tempAggregator[0]:
                    continue
                randomValType = predicate.getRandomVariableType()
                thres = predicate.getThreshold()
                if(len(randomValType) == 0):
                    feature = Feature()
                    feature.setVariables(variables)
                    feature.setRandomVariableType('')
                    feature.setPredicateName(predicate.getName())
                    feature.setRank(predicate.getRank())
                    feature.setArity(predicate.getArity())
                    feature.setAggregator(tempAggregator[1])
                    feature.setFeatureLocation(-1)
                    feature.setThresholds([])
                    self.featureList.append(feature)
                else:
                    for k in randomValType.keys():
                        if randomValType[k] == '':
                            continue
                        else :
                            feature = Feature()
                            feature.setVariables(variables)
                            feature.setRandomVariableType(randomValType[k])
                            feature.setPredicateName(predicate.getName())
                            feature.setRank(predicate.getRank())
                            feature.setArity(predicate.getArity())
                            feature.setAggregator(tempAggregator[1])
                            feature.setFeatureLocation(k)
                            feature.setThresholds(thres[k])
                            self.featureList.append(feature)
        return self.featureList
    
    def getTypeModeConformFeatures(self, currentFeature, previousFeatureList, targetPredicate):
        previousPredicateList = [targetPredicate]
        for feature in previousFeatureList:
            predicate = self.language.dictOfPredicates[feature.getPredicateName()]
            predicate = copy.copy(predicate)
            predicate.addVariables(feature.getVariables())
            predicate.addCurrentAggr(feature.getAggregator())
            predicate.setNegation(feature.getNegation())
            previousPredicateList.append(predicate)
        currentPredicate = self.language.dictOfPredicates[currentFeature.getPredicateName()]
        currentPredicate = copy.copy(currentPredicate)
        currentPredicate.addCurrentAggr(currentFeature.getAggregator())
        if currentPredicate.hasModeConditions(self.targetPredicateName, currentFeature.getAggregator()):
            condPredicates = currentPredicate.getModeConditions(self.targetPredicateName, currentFeature.getAggregator())
            condPredCopy = copy.copy(condPredicates[0]) ##TODO Handle multiple relational conditions, Only one supported right now
            condPredCopy.addCurrentAggr('none')
            tmcCondPredicates = self.language.generateTypeModeConformPredicates(previousPredicateList, condPredCopy, False)
            typeModeConformPredicatesMerge = []
            for tmcPred in tmcCondPredicates:
                    previousPredicateListCopy = copy.copy(previousPredicateList)
                    previousPredicateListCopy.append(tmcPred)
                    typeModeConformPredicates = self.language.generateTypeModeConformPredicates(previousPredicateListCopy, currentPredicate, True)
                    for pred in typeModeConformPredicates:
                        thisCondPred = pred.getModeConditions(self.targetPredicateName, pred.getCurrentAggr())[0] ## TODO Handle multiple relational conditions, Only one supported right now
                        thisCondPred.addVariables(tmcPred.getVariables())
                        typeModeConformPredicatesMerge.append(pred)
            typeModeConformFeatures = []
            for pred in typeModeConformPredicatesMerge:
                feature = copy.copy(currentFeature)
                feature.setVariables(pred.getVariables())
                feature.isCondInsideAggr = True
                feature.setCondInsideAggr(pred.getModeConditions(self.targetPredicateName, pred.getCurrentAggr())[0]) ## TODO Handle multiple relational conditions, Only one supported right now
                typeModeConformFeatures.append(feature)
        else:
            typeModeConformPredicates = self.language.generateTypeModeConformPredicates(previousPredicateList, currentPredicate, False)
            typeModeConformFeatures = []
            for pred in typeModeConformPredicates:
                feature = copy.copy(currentFeature)
                feature.setVariables(pred.getVariables())
                typeModeConformFeatures.append(feature)
        return typeModeConformFeatures
    