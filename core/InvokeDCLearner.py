'''
Created on Mar 26, 2020

@author: nitesh
'''
import sys, pickle
from DCLearner import DCLearner


if __name__ == '__main__':
    outputFile = sys.argv[2]
    f = open(outputFile, 'w')
    obj = DCLearner(sys.argv[1], '', '')
    obj.learnRules()
    translateObj = obj.interface.translator
    for rule in obj.rules:
        rule = translateObj.translate(rule)
        f.write(rule + '\n')
    f.close()
    prologDependencyStructureList = []
    prologDependencyStructureList.append(obj.generateDependencyStructureOfRules(obj.rulesFragmented))
    
    basePredicates = obj.generateBasePredicates()
    
    
    with open(sys.argv[3], 'wb') as output:
        pickle.dump(prologDependencyStructureList, output, pickle.HIGHEST_PROTOCOL)
        
        pickle.dump(basePredicates, output, pickle.HIGHEST_PROTOCOL)
