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
            self.c_dc = dc(file[0].encode("UTF-8"))
        else:
            self.c_dc = dc(file[0].encode("UTF-8"), file[1].encode("UTF-8"))    
    
    def query(self, n, query, evidence):
        return self.c_dc.query(n, query.encode("UTF-8"), evidence.encode("UTF-8"))
    
    def terminate(self):
        self.c_dc.terminate()
        
    def execute(self, execQuery):
        success = self.c_dc.execute(execQuery.encode("UTF-8"))
        return success
        
    def sample(self, n, query, evidence, variable, size):
        samplePython = None
        cdef char* samples
        try:
            samples = self.c_dc.sample(n, query.encode("UTF-8"), evidence.encode("UTF-8"), variable.encode("UTF-8"), size)
            samplePython = samples
        except Exception as e:
            print e 
        finally:
            pass
            #free(samples)
        return samplePython
    
    def prologQuery(self, n, query, size):
        prologQuery = None
        cdef char* results
        try:
            results = self.c_dc.prologQuery(n, query.encode("UTF-8"), size)
            prologQuery = results
        except Exception as e:
            print e 
        finally:
            pass
            #free(results)
        return prologQuery
        
    def queryWithSamples(self, n, query, evidence, vars, flag, size):
        cdef probAndSamples* probSamples
        cdef char* samples
        try:
            probSamples = self.c_dc.queryWithSamples(n, query.encode("UTF-8"), evidence.encode("UTF-8"), vars.encode("UTF-8"), flag, size)
            samples = probSamples.samples
            self.probSamplesDict['prob'] = probSamples.prob
            self.probSamplesDict['samples'] = samples.decode("UTF-8")
        except Exception as e:
            print e 
        finally:
            pass
            #free(samples)
            #free(probSamples)
            
            
    def requisiteEvidence(self, query, size):
        cdef probEvdInt* requsite
        cdef char* probEvd
        cdef char* intEvd
        try:
            requsite = self.c_dc.requisiteEvidence(query.encode("UTF-8"), size)
            probEvd = requsite.probEvidence
            intEvd = requsite.intervention
            self.evdIntDict['probEvidence'] = probEvd.decode("UTF-8")
            self.evdIntDict['intervention'] = intEvd.decode("UTF-8")
        except Exception as e:
            print e 
        finally:
            pass
            #free(probEvd)
            #free(intEvd)
            #free(requsite)

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
    
