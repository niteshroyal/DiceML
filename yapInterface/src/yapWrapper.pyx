from libcpp.string cimport string
from libc.stdlib cimport free
from libcpp cimport bool
from libc.string cimport strcpy
    
cdef extern from "dc.h":
    ctypedef struct probAndSamples:
        double prob
        char* samples
    ctypedef struct probEvdInt:
        char* probEvidence
        char* intervention
    cdef cppclass dc:
        dc() except +
        dc(string file) except +
        dc(string file1, string file2) except +
        void terminate()
        bool execute(string q)
        double query(int n, string query,string evidence)
        char* sample(int n, string query, string evidence, string variable, unsigned int size)
        probAndSamples* queryWithSamples(int n, string query, string evidence, string vars, int flag, unsigned int size)
        probEvdInt* requisiteEvidence(string query, unsigned int size)
        char* prologQuery(int n, string query, unsigned int size)

cdef class PyDC:
    cdef dc c_dc      
    probSamplesDict = dict()
    evdIntDict = dict()
    
    def __cinit__(self, *file):
        if len(file) == 1:
            self.c_dc = dc(file[0])
        else:
            self.c_dc = dc(file[0], file[1])    
    
    def query(self, n, query, evidence):
        return self.c_dc.query(n, query, evidence)
    
    def terminate(self):
        self.c_dc.terminate()
        
    def execute(self, execQuery):
        success = self.c_dc.execute(execQuery)
        return success
        
    def sample(self, n, query, evidence, variable, size):
        samplePython = None
        cdef char* samples
        try:
            samples = self.c_dc.sample(n, query, evidence, variable, size)
            samplePython = samples
        except Exception as e:
            print e 
        finally:
            free(samples)
        return samplePython
    
    def prologQuery(self, n, query, size):
        prologQuery = None
        cdef char* results
        try:
            results = self.c_dc.prologQuery(n, query, size)
            prologQuery = results
        except Exception as e:
            print e 
        finally:
            free(results)
        return prologQuery
        
    def queryWithSamples(self, n, query, evidence, vars, flag, size):
        cdef probAndSamples* probSamples
        cdef char* samples
        try:
            probSamples = self.c_dc.queryWithSamples(n, query, evidence, vars, flag, size)
            samples = probSamples.samples
            self.probSamplesDict['prob'] = probSamples.prob
            self.probSamplesDict['samples'] = samples 
        except Exception as e:
            print e 
        finally:
            free(samples)
            free(probSamples)
            
            
    def requisiteEvidence(self, query, size):
        cdef probEvdInt* requsite
        cdef char* probEvd
        cdef char* intEvd
        try:
            requsite = self.c_dc.requisiteEvidence(query, size)
            probEvd = requsite.probEvidence
            intEvd = requsite.intervention
            self.evdIntDict['probEvidence'] = probEvd
            self.evdIntDict['intervention'] = intEvd
        except Exception as e:
            print e 
        finally:
            free(probEvd)
            free(intEvd)
            free(requsite)

    @property
    def prob(self):
        return self.probSamplesDict['prob']
   
    @property
    def samples(self):
        return self.probSamplesDict['samples']
    
    @property
    def probEvidence(self):
        return self.evdIntDict['probEvidence']
    
    @property
    def intervention(self):
        return self.evdIntDict['intervention']
    
