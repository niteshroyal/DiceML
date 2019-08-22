'''
Created on Aug 18, 2017

@author: niteshkumar
'''

class Predicate(object):
    def __init__(self):
        self.name = ''
        self.arity = -1
        self.type = []
        self.mode = {}
        self.modeConditions = {}
        self.aggregators = []
        self.thresholds = {}
        self.typeOfRandomVariable = {}
        self.variables = []
        self.currentAggr = ''
        self.negationApplied = False
        self.rank = 0
    
    def getBasePredicate(self):
        basePredicate = self.name + '('
        for i in range(0,len(self.type)):
            if i == len(self.type)-1:
                basePredicate += self.type[i].upper() + ')'
            else:
                basePredicate += self.type[i].upper() + ','
        return basePredicate
    
    def setRank(self, r):
        self.rank = r
        
    def getRank(self):
        return self.rank
                
    def setNegation(self, flag):
        self.negationApplied = flag
        
    def getNegation(self):
        return self.negationApplied
        
    def addCurrentAggr(self, aggr):
        self.currentAggr = aggr
    
    def getCurrentAggr(self):
        return self.currentAggr
        
    def addVariables(self, v):
        self.variables = v
    
    def getVariables(self):
        return self.variables
    
    def addTypes(self, t):
        self.type = t
    
    def getTypes(self):
        return self.type
    
    def addModes(self, targetPredicateName, aggr, m, conds):
        self.mode[targetPredicateName + ':::' + aggr] = m
        self.modeConditions[targetPredicateName + ':::' + aggr] = conds
    
    def hasModeConditions(self, targetPredicateName, aggr):
        conds = self.getModeConditions(targetPredicateName, aggr)
        if conds == []:
            return False
        else:
            return True
    
    def getModes(self, targetPredicateName, aggr):
        if(self.mode.has_key(targetPredicateName + ':::' + aggr)):
            return self.mode[targetPredicateName + ':::' + aggr]
        else:
            return None
    
    def getModeConditions(self, targetPredicateName, aggr):
        if(self.modeConditions.has_key(targetPredicateName + ':::' + aggr)):
            return self.modeConditions[targetPredicateName + ':::' + aggr]
        else:
            return None
    
    def addArity(self, a):
        self.arity = a
        for i in range(0, a):
            self.variables.append('_')
    
    def getArity(self):
        return self.arity
    
    def setName(self, n):
        self.name = n
    
    def getName(self):
        return self.name
    
    def appendAggregators(self, targetPredicateName, agg):
        self.aggregators.append(targetPredicateName + ':::' + agg)
        
    def getAggregators(self):
        return self.aggregators
    
    def appendThreshold(self, key, val):
        self.thresholds[key] = val
    
    def getThreshold(self):
        return self.thresholds
        
    def appendRandomVariableType(self, key, val):
        self.typeOfRandomVariable[key] = val
    
    def getRandomVariableType(self):
        return self.typeOfRandomVariable
    
    def isObjectModesSame(self, obj):
        if(len(self.mode) != len(obj.mode)):
            return False
        else:
            m1 = self.mode
            m2 = obj.mode
            for i in m1.keys():
                if(not m2.has_key(i)):
                    return False
                elif(m1[i] != m2[i]):
                    return False
        if(self.getArity() != self.getArity()):
            return False
        else:
            return True
    
    def toString(self):
        st = self.name + '('
        for i in range(0, self.arity):
            if i == self.arity - 1:
                st += self.variables[i] + ')'
            else:
                st += self.variables[i] + ','
        return st