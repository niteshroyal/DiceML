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

    
if __name__ == '__main__':    
    ## Relational schema
    ## {    'table_name': [{'header': 'column_name', 'type': 'type of random variable'}, ... ]
    ##        .
    ##        .
    ## }
    ## Remember
    ##     1) Each table should have only one 'primaryKey'. (Note: this is just a limitation of this synthInterface. DC learner can support multiple primary keys)
    ##     2) Columns of type 'continuous' or 'discrete' can be predicted
    ##     3) If a column contains foreign key then put 'foreignKey' as type
    ##     4) If a column contains strings, text etc. then put 'unsupported' as type. (Note: keys and strings are not predicted currently)
    schema = {
        'account': [{'header': 'acc_id', 'type': 'primaryKey'}, {'header': 'savings', 'type': 'continuous'}, {'header': 'freq', 'type': 'discrete'}, {'header': 'hasLoan', 'type': 'foreignKey'}],
        'loan': [{'header': 'loan_id', 'type': 'primaryKey'}, {'header': 'loanAmt', 'type': 'continuous'}, {'header': 'status', 'type': 'discrete'}],
        'client': [{'header': 'cl_id', 'type': 'primaryKey'}, {'header': 'age', 'type': 'continuous'}, {'header': 'creditScore', 'type': 'continuous'}, {'header': 'hasAcc', 'type': 'foreignKey'}]
    }
    
    
    ## Relational database
    ## {'table_name': [[row1], ..., [rowN]], ...}
    tables = {
        'account': [['a_10', '2000', 'high', 'l_20'], 
                    ['a_11', '2502', 'high', 'l_21'], 
                    ['a_12', '3001', 'high', 'l_22'], 
                    ['a_13', '3505', 'high', 'l_23'], 
                    ['a_14', '4009', 'high', 'l_24'],
                    ['a_15', '4510', 'high', 'l_25'],
                    ['a_16', '5001', 'low', 'l_26'],
                    ['a_17', '5503', 'low', 'l_27'],
                    ['a_18', '6005', 'low', 'l_28'],
                    ['a_19', '6510', 'low', 'l_29']],
        
        'client': [['ann', '43', '600', 'a_10'],
                   ['john', '', '610', 'a_10'],
                   ['bob', '45', '620', 'a_10'],
                   ['carl', '23', '300', 'a_11'],
                   ['rose', '40', '700', 'a_12'],
                   ['mark', '', '350', 'a_13'],
                   ['steve', '54', '650', 'a_14'],
                   ['ritu', '57', '690', 'a_15'],
                   ['amit', '', '720', 'a_16'],
                   ['nitesh', '23', '250', 'a_17'],
                   ['jessa', '26', '370', 'a_18'],
                   ['mohit', '27', '', 'a_18'], 
                   ['robin', '28', '300', 'a_19'], 
                   ['dris', '', '301', 'a_19'], 
                   ['chi', '19', '', 'a_19']],
        
        'loan': [['l_20', '20000', 'appr'],
                 ['l_21', '25000', 'appr'], 
                 ['l_22', '30000', 'appr'], 
                 ['l_23', '35000', 'pend'],
                 ['l_24', '', 'pend'],
                 ['l_25', '45000', 'decl'],
                 ['l_26', '50000', 'decl'],
                 ['l_27', '55000', 'decl'],
                 ['l_28', '60000', 'decl'],
                 ['l_29', '65000', ''],
                 ['l_30', '70000', 'decl'],
                 ['l_31', '40000', 'pend'],
                 ['l_32', '15000', 'appr'],
                 ['l_33', '75000', 'decl'],
                 ['l_34', '80000', 'decl'],
                 ['l_35', '41000', 'pend'],
                 ['l_36', '43000', 'pend']]    
    }
    
    ## Foreign keys
    ## [[('table_name1', 'foreign key column'), ('table_name2', 'primary key column')], ...]
    foreignKeys = [[('account', 'hasLoan'), ('loan', 'loan_id')], [('client', 'hasAcc'), ('account', 'acc_id')]]
    
    interface = DreaMLInterface()
    
    ## Declare Python 2 environment. (Note: DC Learner is written in Python 2.7)
    python2_env = {'bin': '/usr/bin/python', 'lib': '/usr/lib/python2.7'}
    interface.setPython2Path(python2_env)
        
    interface.initialize(schema, tables, foreignKeys)
    interface.learn()
    
    ## load the learned DC program and provide the number of samples to use for inference.
    interface.loadDCProgram(1000)
    
    ## [('table_name1', row_number, column_number), ...]
    ## Remember that primary or foreign keys are not predicted currently.
    ## Note: row_number and column_number starts with 0
    query = [('account', 3, 1), ('loan', 3, 2)]
    
    ## if predictUncertainty == False then result = ['3000', 'appr'] 
    ## else if predictUncertainty == True then result = ['gaussian(3000, 10,2)', 'discrete([0.6:appr, 0.3:pend, 0.1:decl])'] 
    predictUncertainty = True
    
    ## These are additional queries about attributes of instances that are not present in the data.
    ## Note: the learned model can also be used to predict attributes of instances that were not shown during training. 
    ## Possibly empty OR of the form: additionalQuery = ['freq(a_101, null)', 'loanAmt(l_120, null)']
    ## Notice that account 'a_101' and loan 'l_120' are not present in the training data. 
    additionalQuery = []
    
    ## These are additional observed values (Note: Training data has already been observed.) 
    ## Possibly empty OR of the form: conditions = ['savings(a_101, 1000)', 'age(c_210,30)']
    additionalConditions = []
    
    ## These are additional interventions (Note: The input training data has already observed values)
    ## Interventions define the relational structure, i.e., relations and entities.
    ## Possibly empty OR of the form: interventions = ['hasLoan(a_101,l_120)', 'account(a_101)', 'loan(l_120)', 'client(c_210)']
    additionalInterventions = []
    
    result = interface.queryDC(query, predictUncertainty, additionalQuery, additionalConditions, additionalInterventions)
    print(result)
    
    predictUncertainty = False
    query = [('loan', 4, 1)]
    result = interface.queryDC(query, predictUncertainty, additionalQuery, additionalConditions, additionalInterventions)
    print(result)
    
    predictUncertainty = True
    query = [('loan', 9, 2)]
    result = interface.queryDC(query, predictUncertainty, additionalQuery, additionalConditions, additionalInterventions)
    print(result)
    
    query = []
    additionalQuery = ['loanAmt(l_137, null)']
    additionalConditions = ['status(l_137, decl)', 'savings(a_120,6300)', 'freq(a_120,low)', 'savings(a_121,6400)', 'freq(a_121,low)']
    additionalInterventions = ['loan(l_137)', 'account(a_120)', 'account(a_121)', 'hasLoan(a_120,l_137,true)', 'hasLoan(a_121,l_137,true)']
    result = interface.queryDC(query, predictUncertainty, additionalQuery, additionalConditions, additionalInterventions)
    print(result)
    


    
    
    