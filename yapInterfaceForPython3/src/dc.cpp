#include "dc.h"
#include <cstdio>
#include <cstring>
#include <iostream>
#include <string>
#include <exception>

initerrordc exceptiondc;



dc::dc()
{
	if(load()!=1)
		throw exceptiondc;
	YAP_Term error;
	emptylist=YAP_ReadBuffer("[]",&error);
	yap_false=YAP_ReadBuffer("false",&error);
	yap_true=YAP_ReadBuffer("true",&error);
}

dc::dc(string file)
{
	if(load(file)!=1)
		throw exceptiondc;
	YAP_Term error;
	emptylist=YAP_ReadBuffer("[]",&error);
	yap_false=YAP_ReadBuffer("false",&error);
	yap_true=YAP_ReadBuffer("true",&error);
}

dc::dc(string file1, string file2)
{
	if(load(file1, file2)!=1)
		throw exceptiondc;
	YAP_Term error;
	emptylist=YAP_ReadBuffer("[]",&error);
	yap_false=YAP_ReadBuffer("false",&error);
	yap_true=YAP_ReadBuffer("true",&error);
}

dc::~dc(){}

bool dc::load(){
	if (YAP_FastInit(NULL) == YAP_BOOT_ERROR)
		return false;
	else
		yaploaded=true;
	execute("yap_flag(informational_messages,off)");
	return 1;
}

bool dc::load(string file){
	if (YAP_FastInit(NULL) == YAP_BOOT_ERROR)
		return false;
	else
		yaploaded=true;
	execute("yap_flag(informational_messages,off)");
	string goal="consult('"+file+"')";
	return execute(goal); // 1 ok
}

bool dc::load(string file1, string file2){
	if (YAP_FastInit(NULL) == YAP_BOOT_ERROR)
		return false;
	else
		yaploaded=true;
	execute("yap_flag(informational_messages,off)");
	string goal="consult(['"+file1 + "', '" + file2 + "'])";
	return execute(goal); // 1 ok
}




// evaluate probability of a query: query([evidence],[],query,n,P,_,_)
double dc::query(int n, string query,string evidence){
	YAP_Term error;
/*	YAP_Atom q=YAP_LookupAtom("query");
	cout<<YAP_AtomName(q);
*/
	string goal="query([" + evidence + "],[],"+query + "," + std::to_string(n) + ",ProbabilityQuery,_,_)";
	YAP_Term tmp = YAP_ReadBuffer(goal.c_str(),&error);
	double prob;
	if(!runGoalOnce(tmp,5,prob)) return -1;
	return prob;
}

// evaluate probability of a query: query([evidence],[],query,n,P,_,_)
probAndSamples* dc::queryWithSamples(int n, string query, string evidence, string vars, int flag, unsigned int size){

	try {

		YAP_Term error;
		string goal="query_for_structure_learning([" + evidence + "],[],"+query + "," + std::to_string(n) + ",ProbabilityQuery," + vars + "," + "StructLearnObj," + std::to_string(flag)+ ")";
		YAP_Term tmp = YAP_ReadBuffer(goal.c_str(),&error);
		YAP_Term out;
		double prob;
		bool res = runGoalOnce(tmp,5,7,prob,out);
		probAndSamples* obj = new probAndSamples();
		char* str1 = new char[size]();
		char* str2 = (char *)"no";
		if(!res) {
			obj->prob = -1;
			obj->samples = &str2[0];
			return obj;
		} else {
			obj->prob = prob;
			obj->samples = &str1[0];
			YAP_WriteBuffer(out, str1, size, YAP_WRITE_HANDLE_VARS);
			return obj;
		}

	} catch (exception& e) {
		cout << "Exception from queryWithSamples in dc.cpp: " << e.what() << '\n';
	}
	return 0;
}


// evaluate requisite evidence for a query: requsite(query_null,ProbEvidence,Intervention,query_X)
probEvdInt* dc::requisiteEvidence(string query, unsigned int size){
	try {
		YAP_Term error;
		string goal=query;
		YAP_Term tmp = YAP_ReadBuffer(goal.c_str(),&error);
		YAP_Term probEvidence;
		YAP_Term interverntion;

		bool res = runGoalOnce(tmp,2,3,probEvidence,interverntion);
		probEvdInt* obj = new probEvdInt();
		char* str1 = new char[size]();
		char* str2 = (char *)"no";
		char* str3 = new char[size]();
		char* str4 = (char *)"no";
		if(!res) {
			obj->probEvidence = &str4[0];
			obj->intervention = &str2[0];
			return obj;
		} else {
			obj->probEvidence = &str3[0];
			obj->intervention = &str1[0];
			YAP_WriteBuffer(probEvidence, str3, size, YAP_WRITE_HANDLE_VARS);
			YAP_WriteBuffer(interverntion, str1, size, YAP_WRITE_HANDLE_VARS);
			return obj;
		}

	} catch (exception& e) {
		cout << "Exception from queryWithSamples in dc.cpp: " << e.what() << '\n';
	}
	return 0;
}

char* dc::sample(int n, string query,string evidence, string variable, unsigned int size){
	YAP_Term error;
	string goal="samplesGenerator(" + std::to_string(n) + "," + evidence + "," + query + "," + variable + ",SampleList)";
	YAP_Term tmp = YAP_ReadBuffer(goal.c_str(),&error);
	YAP_Term out;
	bool res = runGoalOnce(tmp,5,out);
	char* str1 = new char[size]();
	char* str2 = (char *)"";
	char* sol;
	if(!res) {
		sol = &str2[0];
		return sol;
	} else {
		sol = &str1[0];
		YAP_WriteBuffer(out, str1, size, YAP_WRITE_HANDLE_VARS);
		return sol;
	}
}


// evaluate probability of a query: query([evidence],[],query,n,P,_,_)
char* dc::prologQuery(int n, string query, unsigned int size){
	YAP_Term error;
	YAP_Term tmp = YAP_ReadBuffer(query.c_str(),&error);
	YAP_Term out;
	bool res = runGoalOnce(tmp,n,out);
	char* str1 = new char[size]();
	char* str2 = (char *)"";
	char* sol;
	if(!res) {
		sol = &str2[0];
		return sol;
	} else {
		sol = &str1[0];
		YAP_WriteBuffer(out, str1, size, YAP_WRITE_HANDLE_VARS);
		return sol;
	}
}

// execute an arbitrary prolog goal (query)
bool dc::execute(string q){

	try {
		YAP_Term error;
		int res = YAP_RunGoal(YAP_ReadBuffer(q.c_str(),&error));
		return res;
	} catch (exception& e) {
		cout << "Exception from execute in dc.cpp: " << e.what() << '\n';
	}
	return 0;
}

void dc::terminate(void) {
	try {
		//YAP_Exit(1);
		YAP_EndConsult();
	} catch (exception& e) {
		cout << "Exception from terminate in dc.cpp: " << e.what() << '\n';
	}
}

bool dc::runGoalOnce(YAP_Term tmp,int argOutput, int &out){

	long safe_t = YAP_InitSlot(tmp); // have a safe pointer to term
	int res = YAP_RunGoal(tmp);
	if (res==false)
		return false;
	out = YAP_IntOfTerm(YAP_ArgOfTerm(argOutput,YAP_GetFromSlot(safe_t)));
	YAP_RecoverSlots(1); // safe copy not needed anymore
	//res = YAP_ShutdownGoal(TRUE);
	return res;

}


bool dc::runGoalOnce(YAP_Term tmp,int argOutput, double &out){
	long safe_t = YAP_InitSlot(tmp); // have a safe pointer to term
	int res = YAP_RunGoal(tmp);
	if (res==false)
		return false;
	out = YAP_FloatOfTerm(YAP_ArgOfTerm(argOutput,YAP_GetFromSlot(safe_t)));
	YAP_RecoverSlots(1); // safe copy not needed anymore
	//res = YAP_ShutdownGoal(TRUE);
	return res;
}

bool dc::runGoalOnce(YAP_Term tmp,int argOutput1, int argOutput2, double &out1, YAP_Term &out2){
	long safe_t = YAP_InitSlot(tmp); // have a safe pointer to term
	int res = YAP_RunGoal(tmp);
	if (res==false)
		return false;
	out1 = YAP_FloatOfTerm(YAP_ArgOfTerm(argOutput1,YAP_GetFromSlot(safe_t)));
	out2 = YAP_ArgOfTerm(argOutput2,YAP_GetFromSlot(safe_t));
	YAP_RecoverSlots(1); // safe copy not needed anymore
	//res = YAP_ShutdownGoal(TRUE);
	return res;
}

bool dc::runGoal(YAP_Term tmp,int argOutput, YAP_Term &out){
	long safe_t = YAP_InitSlot(tmp); // have a safe pointer to term
	int res = YAP_RunGoal(tmp);
	if (res==false)
		return false;
	out = YAP_ArgOfTerm(argOutput,YAP_GetFromSlot(safe_t));
	YAP_RecoverSlots(1); // safe copy not needed anymore
	//res = YAP_ShutdownGoal(TRUE);
	return res;
}

bool dc::runGoalOnce(YAP_Term tmp,int argOutput, YAP_Term &out){
	long safe_t = YAP_InitSlot(tmp); // have a safe pointer to term
	int res = YAP_RunGoal(tmp);
	if (res==false)
		return false;
	out = YAP_ArgOfTerm(argOutput,YAP_GetFromSlot(safe_t));
	YAP_RecoverSlots(1); // safe copy not needed anymore
	//res = YAP_ShutdownGoal(TRUE);
	return res;
}

bool dc::runGoalOnce(YAP_Term tmp,int argOutput1, int argOutput2, YAP_Term &out1, YAP_Term &out2){
	long safe_t = YAP_InitSlot(tmp); // have a safe pointer to term
	int res = YAP_RunGoal(tmp);
	if (res==false)
		return false;
	out1 = YAP_ArgOfTerm(argOutput1,YAP_GetFromSlot(safe_t));
	out2 = YAP_ArgOfTerm(argOutput2,YAP_GetFromSlot(safe_t));
	YAP_RecoverSlots(1); // safe copy not needed anymore
	//res = YAP_ShutdownGoal(TRUE);
	return res;
}

void dc::free(char* ptr) {
	free(ptr);
}

int main() {
/*

	string query = "(smokes(carl)~=true)";
	string evidence = "";
	int n = 1000;
	dc a = dc("data/cancer.pl");
	double b = a.query(n, query, evidence);
	cout << b << endl;
	char* sol = a.sample(10, "(grade(carl)~=X, height(carl)~=Y)", "[stress(carl)~=true]", "[X,Y]", 65535);
	cout << sol <<endl;
	return 0;

	int n = 10;
	string evidence = "";
	string query = "(smokes(ann)~=false,rating(ann)~=X)";
	string vars = "[X]";
	dc *obj = new dc("../DCRuleLearningTestBed/program/testSampling.pl");
	probAndSamples* sol = obj->queryWithSamples(n, query, evidence, vars, 1, 65535);
	cout << sol->prob << endl;
	cout << sol->samples << endl;
	obj->terminate();
	//obj->execute("halt(1).");
	cout << "I am here. ";
	sol = obj->queryWithSamples(n, query, evidence, vars, 1, 65535);
	cout << sol->samples << endl;

	string query1 = "findall([X],(base(X)),FINDALL).";
	int n = 3;
	dc *obj = new dc("../DCRuleLearningTestBed/data/hepatitis.pl", "../DCRuleLearningTestBed/data/dcHepatitis.pl");
	char* sol1 = obj->prologQuery(n, query1, 65535);
	cout << sol1 <<endl;
	char* sol = obj->sample(10, "(findall_forward(Tc_M,(rel12(In_M,2),tcho(In_M)~=Tc_M),X_T_1),minMod(X_T_1)~=tcho0)", "[]", "[X_T_1]", 1000000);
	cout << sol << endl;


	delete obj;
	return 0;
*/

	string query1 = "requsite(fibros(10,2.0),ProbEvidence,Intervention).";
	dc *obj = new dc("../DCRuleLearningTestBed/data/testHepatitis.pl");
	probEvdInt* sol1 = obj->requisiteEvidence(query1, 65535);
	cout << sol1->intervention <<endl;
	cout << sol1->probEvidence <<endl;
	obj->terminate();
	return 0;



}
