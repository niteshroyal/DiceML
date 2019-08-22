# DistributionalProgramSynthesis
Learning Distributional Programs for Relational Auto-completion

Installation
============
```
1. Install Yap Prolog following steps here: https://github.com/davidenitti/DC

2. Install python packages: Numpy, Sklearn, Cython

3. Build PyDC executable file and copy to the 'core' folder
   $ cd yapInterface
   $ python setup.py build_ext --inplace
   $ mv yapWrapper.so ../core/
```
   	
Execution 
=========

1. Learning a distributional program from deterministic data. An example of declarative bias in shown in file '../data/FinancialData.pl'

```python
 ## Output DC program
 outputFile = '../data/MyDCRules.pl'
 f = open(outputFile, 'w')

 ## Input Prolog program '../data/FinancialData.pl' contains example of 
 ## declarative bias needed for the deterministic case 
 obj = TreeLearnerProbabilistic('../data/FinancialData.pl', '', '')

 obj.learnRules()
 translateObj = obj.interface.translator
 for rule in obj.rules:
     rule = translateObj.translate(rule)
     f.write(rule + '\n')
 f.close()
```

2. Learning a distributional program from deterministic/probabilistic data as well as background theory. Two files are needed for this case.

```python
 ## Output DC program
 outputFile = '../data/MyDCRules.pl'
 f = open(outputFile, 'w')

 ## Input DC program and a helper Prolog program
 obj = TreeLearnerProbabilistic('../data/FinancialData_Enumerated.pl','../data/FinancialDataDC.pl','')

 obj.learnRules()
 translateObj = obj.interface.translator
 for rule in obj.rules:
     rule = translateObj.translate(rule)
     f.write(rule + '\n')
 f.close()
```
