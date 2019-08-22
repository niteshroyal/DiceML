from yapWrapper import PyDC

## After executing from command line : $ python setup.py build_ext --inplace

BIGNUM = 1000000
obj = PyDC('/testHepatitis.pl')
query = 'age(447,5.0)'
query = 'requsite(' + query + ',ProbEvidence,Intervention).'
obj.requisiteEvidence(query, BIGNUM)

print obj.probEvidence
print obj.intervention

obj.terminate()
