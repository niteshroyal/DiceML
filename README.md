# Distributional Program Synthesis
* Learns a joint probabilistic relational model in the form of the Distributional Program for a relational database.
* It can make use of background knowledge (if available) written in DC language.
* Performs Stochastic EM if some fields are missing in the relational database. 

The code is in beta, if you need help or find a bug please write an [issue](https://github.com/niteshroyal/DistributionalProgramSynthesis/issues)
or contact me at nitesh.kr369 (_DOT_) 30 (AT) gmail (_DOT_) com
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

2. Learning a distributional program from deterministic/probabilistic data as well as background theory. Two input files are needed for this case.

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
