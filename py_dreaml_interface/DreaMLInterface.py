'''
Created on Jan 29, 2020

@author: nitesh
'''
from .DreaML import DreaML


class DreaMLInterface(object):
    
    def __init__(self):
        self.dreaml = DreaML()
    
    def initialize(self, schema, database, foreignKeys):
        self.dreaml.initialize(schema, database, foreignKeys)
    
    def setPython2Path(self, env):
        self.dreaml.setPython2Path(env)
    
    def learn(self):
        self.dreaml.learn()
    
    def loadDCProgram(self, numOfSamples):
        self.dreaml.loadDCProgram(numOfSamples)
    
    def queryDC(self, query, predictUncertainty, additionalQuery, additionalConditions, additionalInterventions):
        result = self.dreaml.queryDC(query, predictUncertainty, additionalQuery, additionalConditions, additionalInterventions)
        return result


my_interface = None


def dreaml_learn(my_schema, my_tables, my_foreign_keys):
    global my_interface

    my_interface = DreaMLInterface()

    my_interface.initialize(my_schema, my_tables, my_foreign_keys)
    my_interface.learn()

    # load the learned DC program and provide the number of samples to use for inference.
    my_interface.loadDCProgram(1000)

    return 0


def dreaml_query(my_query, my_predictUncertainty, my_additionalQuery, my_additionalConditions, my_additionalInterventions):
    my_result = my_interface.queryDC(my_query, my_predictUncertainty, my_additionalQuery, my_additionalConditions, my_additionalInterventions)
    return my_result