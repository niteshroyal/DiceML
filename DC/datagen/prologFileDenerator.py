NUM_OF_PROP_OF_OBJECT = 5
st = ''':- ['../program/aggregationFunctions'].
:- discontiguous current_posX/3.
:- discontiguous current_posY/3.
:- discontiguous current_posZ/3.
:- discontiguous next_posX/3.
:- discontiguous next_posY/3.
:- discontiguous next_posZ/3.
:- discontiguous move_behind_of/3.
:- discontiguous move_infront_of/3.
:- discontiguous move_left_of/3.
:- discontiguous move_right_of/3.
:- discontiguous behind_of/3.
:- discontiguous infront_of/3.
:- discontiguous left_of/3.
:- discontiguous right_of/3.

%Types
base(current_posX(s, o, x)).
base(current_posY(s, o, y)).
base(current_posZ(s, o, z)).
base(next_posX(s, o, x)).
base(next_posY(s, o, y)).
base(next_posZ(s, o, z)).
base(move_behind_of(s, o, o)).
base(move_infront_of(s, o, o)).
base(move_left_of(s, o, o)).
base(move_right_of(s, o, o)).
base(behind_of(s, o, o)).
base(infront_of(s, o, o)).
base(left_of(s, o, o)).
base(right_of(s, o, o)).

%Modes
mode(none, current_posX(+, +, -)).
mode(none, current_posY(+, +, -)).
mode(none, current_posZ(+, +, -)).
mode(none, move_behind_of(+, -, +)).
mode(none, move_infront_of(+, -, +)).
mode(none, move_left_of(+, -, +)).
mode(none, move_right_of(+, -, +)).
mode(none, behind_of(+, +, -)).
mode(none, infront_of(+, +, -)).
mode(none, left_of(+, +, -)).
mode(none, right_of(+, +, -)).

%Aggregations
agg(none).

%Threshold
thres(current_posX, 3, continuous, []).
thres(current_posY, 3, continuous, []).
thres(current_posZ, 3, continuous, []).

%Target
learn(next_posX, 3, 3, continuous).
%learn(next_posY, 3, 3, continuous).
%learn(next_posZ, 3, 3, continuous).

%Facts
'''

def samplePositionWrite(f, record, current):
    record = record.replace('[', '').replace('(', '').replace(']', '').replace(')', '')
    record = record.split(',')
    sampleId = record[0]
    del record[0]
    numOfObjects = len(record)/NUM_OF_PROP_OF_OBJECT
    if current:
        for i in range(0, numOfObjects):
            f.write('current_posX(sample' + sampleId + ', obj' + record[i*NUM_OF_PROP_OF_OBJECT] + ', ' + record[i*NUM_OF_PROP_OF_OBJECT+1] + ').\n')
            f.write('current_posY(sample' + sampleId + ', obj' + record[i*NUM_OF_PROP_OF_OBJECT] + ', ' + record[i*NUM_OF_PROP_OF_OBJECT+2] + ').\n')
            f.write('current_posZ(sample' + sampleId + ', obj' + record[i*NUM_OF_PROP_OF_OBJECT] + ', ' + record[i*NUM_OF_PROP_OF_OBJECT+3] + ').\n')
    else:
        for i in range(0, numOfObjects):
            f.write('next_posX(sample' + sampleId + ', obj' + record[i*NUM_OF_PROP_OF_OBJECT] + ', ' + record[i*NUM_OF_PROP_OF_OBJECT+1] + ').\n')
            f.write('next_posY(sample' + sampleId + ', obj' + record[i*NUM_OF_PROP_OF_OBJECT] + ', ' + record[i*NUM_OF_PROP_OF_OBJECT+2] + ').\n')
            f.write('next_posZ(sample' + sampleId + ', obj' + record[i*NUM_OF_PROP_OF_OBJECT] + ', ' + record[i*NUM_OF_PROP_OF_OBJECT+3] + ').\n')
    return int(sampleId)

def samplePredicateWrite(f, record, sampleId):
    record = record.replace('[', '').replace(']', '')
    record = record.split(',')
    f.write(record[0] + '(sample' + str(sampleId) + ', obj' + record[2] + ', obj' + record[1] + ').\n')
    f.write(record[3] + '(sample' + str(sampleId) + ', obj' + record[2] + ', obj' + record[4] + ').\n')

f = open('data.txt', 'r')
data = f.read()
f.close()

data = data.split('\n')
sampleId = -1
current = True
f = open('dataWithNeuralPred.pl', 'w')
f.write(st)
for rec in data:
    if rec == '':
        continue
    else:
        if rec[1].isalpha(): 
            samplePredicateWrite(f, rec, sampleId)
        else:
            sampleId = samplePositionWrite(f, rec, current)
            current = not current
f.close()


