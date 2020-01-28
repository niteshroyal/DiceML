'''
This experiment evaluates the Stochastic EM algorithm for learning the joint model program in the presence of missing data.

@author: nitesh
'''
import subprocess, time

databaseName = 'financial'

numSamples = 1000

numEMIterations = 2

#prologTrainingFileName = '../data/Hepatitis3c.pl'
prologTrainingFileName = '../data/Financial3c.pl'

dcTrainingFile = '../data/FinancialDC3c.pl'
#dcTrainingFile = '../data/HepatitisDC3c.pl'

treeOutputFileTraining = '../data/EnsembleOfDLTs3c.pl'
#treeOutputFileTraining = '../data/EnsembleOfDLTs3c.pl'

percentageMissingList = [50]
#percentageMissingList = [10, 20, 30, 40, 50]

folder = '/home/nitesh/eclipse-workspace/DistributionalProgramSynthesis/data/PKDD/pkdd/'
#folder = '/home/nitesh/eclipse-workspace/DistributionalProgramSynthesis/data/Hepatitis/'

#missPred = [['avgSumOfInc', 'loanStatus', 'loanAmount', 'gender', 'age', 'avgNrWith', 'monthlyPayments', 'avgSumOfW', 'freq', 'stdMonthInc', 'stdMonthW', 'avgSalary', 'ratUrbInhab'], ['avgSumOfInc', 'loanStatus', 'loanAmount', 'gender', 'age', 'ratUrbInhab', 'avgNrWith', 'monthlyPayments', 'avgSalary', 'avgSumOfW', 'freq', 'stdMonthInc', 'stdMonthW']]
#testPred = [['monthlyPayments'],['loanStatus']]

#missPred = [['avgSumOfInc', 'loanStatus', 'loanAmount', 'gender', 'age', 'avgNrWith', 'monthlyPayments', 'avgSumOfW', 'freq', 'stdMonthInc', 'stdMonthW', 'avgSalary', 'ratUrbInhab'], ['avgSumOfInc', 'loanStatus', 'loanAmount', 'gender', 'age', 'ratUrbInhab', 'avgNrWith', 'monthlyPayments', 'avgSalary', 'avgSumOfW', 'freq', 'stdMonthInc', 'stdMonthW']]
#missPred = [['avgSumOfInc', 'loanStatus', 'loanAmount', 'gender', 'age', 'avgNrWith', 'monthlyPayments', 'avgSumOfW', 'freq', 'stdMonthInc', 'stdMonthW'], ['avgSumOfInc', 'loanStatus', 'loanAmount', 'gender', 'age', 'ratUrbInhab', 'avgNrWith', 'monthlyPayments', 'avgSalary', 'avgSumOfW', 'freq', 'stdMonthInc', 'stdMonthW']]
testPred = ['monthlyPayments', 'loanStatus']
#missPred = [['fibros', 'activity', 'sex', 'age', 'got', 'gpt', 'alb', 'tbil', 'dbil', 'che', 'ttt', 'ztt', 'tcho', 'tp', 'dur']]
#missPred = [['got']]
#missPred = [['got', 'gpt', 'alb', 'tbil', 'dbil', 'che', 'ttt', 'ztt', 'tcho', 'tp']]
#missPred = [['tcho']]

modes = ['em']

#modes = ['em', 'partial', 'complete']

for j in range(1,2):
    trainFoldString = folder + "Fold" + str(j) + '/train'
    validateFoldString = folder + "Fold" + str(j) + '/validate'
    testFolderString = folder + "Fold" + str(j) + '/test'
    print('\n ********* Fold Number : %d' % j)
    for i in range(0,1):
        testPredicate = testPred[i]
        for precent in percentageMissingList:
            percentageMissing = precent
            for mode in modes:
                try:
                    command = ['python', 'StochasticEM.py', databaseName, str(numSamples), str(numEMIterations), mode, trainFoldString, validateFoldString, testFolderString, str(percentageMissing), prologTrainingFileName, dcTrainingFile, treeOutputFileTraining, testPredicate]
                    subprocess.call(command)
                    time.sleep(30)
                except RuntimeError as e:
                    print e
                    time.sleep(10)
                except OSError as e:
                    print e
                    time.sleep(10)

