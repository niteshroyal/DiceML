#include <iostream>
#include <exception>
#include <string>
#include <Yap/YapInterface.h>
#include <Yap/c_interface.h>
#include <stdio.h>


using namespace std;

#ifndef DC_H
#define DC_H

class initerrordc: public std::exception
{
	virtual const char* what() const throw()
	{
	return "Initialization error: file not found or particle initialization failed";
	}
};

typedef struct probAndSamples {
  double prob;
  char* samples;
} probAndSamples;

typedef struct probEvdInt {
  char* probEvidence;
  char* intervention;
} probEvdInt;

class dc
{
	private:
		bool yaploaded=false;
		bool load(string model);
		bool load(string file1, string file2);
		bool load();

	protected:
		YAP_Term emptylist,yap_false,yap_true;
	public:
		dc(string file);
		dc(string file1, string file2);
		dc();
		~dc();
		void terminate(void);
		void free(char* ptr);
		double query(int n, string query,string evidence);
		probAndSamples* queryWithSamples(int n, string query, string evidence, string vars, int flag, unsigned int size);
		probEvdInt* requisiteEvidence(string query, unsigned int size);
		char* sample(int n, string query,string evidence, string variable, unsigned int size);
		char* prologQuery(int n, string query, unsigned int size);
		bool execute(string q);
		static void print(YAP_Term t) { YAP_Write(t,0,YAP_WRITE_HANDLE_VARS); };
		static void printnl(YAP_Term t) { YAP_Write(t,0,YAP_WRITE_HANDLE_VARS);cout<<endl; };
		bool	runGoalOnce(YAP_Term tmp,int argOutput, int &out);
		bool	runGoalOnce(YAP_Term tmp,int argOutput, double &out);
		bool runGoalOnce(YAP_Term tmp,int argOutput1, int argOutput2, double &out1, YAP_Term &out2);
		bool runGoalOnce(YAP_Term tmp,int argOutput1, int argOutput2, YAP_Term &out1, YAP_Term &out2);
		bool runGoalOnce(YAP_Term tmp,int argOutput, YAP_Term &out);
		bool runGoal(YAP_Term tmp,int argOutput, YAP_Term &out);
		YAP_Term runGoalOnce(YAP_Term tmp,int argOutput);
};
#endif
