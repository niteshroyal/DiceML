'''
Created on Jun 19, 2018

@author: nitesh
'''
import re

class TranslateToDC(object):
    
    def __init__(self):
        pass
    
    def setRandomVariablePredicates(self, randomVariablesName):
        self.random_variable_predicate = randomVariablesName
        
    def find_all(self, a_str, sub):
        start = 0
        while True:
            start = a_str.find(sub, start)
            if start == -1: return
            yield start
            start += len(sub)
        
    def convertDCPredicates(self, body, ty):
        for pattern in self.random_variable_predicate:
            pattern = pattern + '('
            positions = list(self.find_all(body, pattern))
            body1 = body
            for position in positions:
                if position == -1:
                    pass
                else:   
                    stripString = ''
                    for i in range(position, len(body)):
                        if body[i] == ')':
                            stripString += body[i]
                            break
                        else:
                            stripString += body[i]
                    splitString = stripString.split(',')
                    variable = splitString[-1][0:-1]
                    newString = ''
                    length = len(splitString)
                    for j in range(0, length-1):
                        if j == length-2:
                            newString += splitString[j]
                        else:
                            newString += splitString[j] + ','
                    if ty == 'body':
                        newString += ')~='
                        newString += variable
                    else:
                        newString += ')'
                    body1 = body1.replace(stripString, newString)
            body = body1
        body = self.convertAggToDC(body)        
        return body
    
    def convertAggToDC(self, body):
        positions1 = list(self.find_all(body, 'maxMod('))
        positions2 = list(self.find_all(body, 'minMod('))
        positions = positions1 + positions2
        bodyTemp = body
        for pos in positions:
            if pos >= 2 and body[pos-1] == '+':
                continue
            stripString = ''
            endStripString = False
            for i in range(pos, len(body)):
                if body[i] == '=':
                    if i+1 <= len(body):
                        if body[i+1] == '=':
                            endStripString = True
                    stripString += body[i]
                elif body[i] == ',' and endStripString:
                    break
                else:
                    stripString += body[i]
            if pos in positions1:
                searchResults = re.findall('maxMod\(.*,(\w+)\),\w+==(\w+)', stripString)
                modeName = 'maxMod'
            else:
                searchResults = re.findall('minMod\(.*,(\w+)\),\w+==(\w+)', stripString)
                modeName = 'minMod'
            searchResult = searchResults[0]
            variable = ',' + searchResult[0] + '),' + searchResult[0] + '=='
            replaceVariable = ',' + searchResult[0] + '),' + modeName + '(' + searchResult[0] + ')~='
            stripStringNew = stripString.replace(modeName + '(', 'findall_forward(')
            stripStringNew = stripStringNew.replace(variable, replaceVariable)
            bodyTemp = bodyTemp.replace(stripString, stripStringNew)
        body = bodyTemp
        positions1 = list(self.find_all(body, '\\+maxMod('))
        positions2 = list(self.find_all(body, '\\+minMod('))
        positions3 = list(self.find_all(body, 'max('))
        positions4 = list(self.find_all(body, 'min('))
        positions5 = list(self.find_all(body, 'avg('))
        positions6 = list(self.find_all(body, 'cnt('))
        positions = positions1 + positions2 + positions3 + positions4 + positions5 + positions6
        bodyTemp = body
        for pos in positions:
            stripString = ''
            openStriping = 0
            for i in range(pos, len(body)):
                if body[i] == '(':
                    stripString += body[i]
                    openStriping += 1
                elif body[i] == ')':
                    stripString += body[i]
                    openStriping -= 1
                    if openStriping == 0:
                        break
                else:
                    stripString += body[i]
            searchResults = re.findall('.*\(.*,(\w+)\)', stripString)
            searchResult = searchResults[0]
            variable = ',' + searchResult + ')'
            if (pos in positions1) or (pos in positions2) or (pos >= 2 and body[pos-1] == '+' and body[pos-2] == '\\'):
                if pos in positions1:
                    modeName = '\\+maxMod'
                elif pos in positions2:
                    modeName = '\\+minMod'
                elif pos in positions3:
                    modeName = '\\+max'
                elif pos in positions4:
                    modeName = '\\+min'
                elif pos in positions5:
                    modeName = '\\+avg'
                elif pos in positions6:
                    modeName = '\\+cnt'
                else:
                    st = 'Something went wrong during translation to DC. (Message 1)'
                    raise Exception(st)
                replaceVariable = ',' + searchResult + '),' + modeName + '('+ searchResult + ')~=_'
                #replaceVariable = ')~=_)'
                if pos >= 2 and body[pos-1] == '+':
                    stripString = '\\+' + stripString
                else:
                    pass
                stripStringNew = stripString.replace(modeName + '(', 'findall_forward(')
                stripStringNew = stripStringNew.replace(variable, replaceVariable)
                bodyTemp = bodyTemp.replace(stripString, stripStringNew)
            else:
                if pos in positions3:
                    modeName = 'max'
                elif pos in positions4:
                    modeName = 'min'
                elif pos in positions5:
                    modeName = 'avg'
                elif pos in positions6:
                    modeName = 'cnt'
                else:
                    st = 'Something went wrong during translation to DC. (Message 2)'
                    raise Exception(st)
                searchResultTemp = searchResult + '_Temp'
                replaceVariable = ',' + searchResultTemp + '),' + modeName + '('+ searchResultTemp + ')~=' + searchResult
                stripStringNew = stripString.replace(modeName + '(', 'findall_forward(')
                stripStringNew = stripStringNew.replace(variable, replaceVariable)
                bodyTemp = bodyTemp.replace(stripString, stripStringNew)
        body = bodyTemp
        #body = body.replace('\\+', '\\+(')
        body = body.replace('\\+', '\\+')
        return body

    def split_distrib(self, distrib):
        name, args = distrib.split('(')
        arglist = list()
        inarg = False
        ininarg = False
        for c in args:
            if c == '[':
                if inarg == True:
                    ininarg = True
                    arglist[-1][-1] += c
                else:
                    inarg = True
                    arglist.append([''])
            elif c == ']':
                if ininarg == True:
                    ininarg = False
                    arglist[-1][-1] += c
                else:
                    inarg = False
            else:
                if inarg:
                    if c == ',':
                        arglist[-1].append('')
                    else:
                        arglist[-1][-1] += c
        return name,arglist

    def translate(self, rule):
        rule = rule.replace('\\\\+', '\\+')
        rule = rule.replace(' ', '')
        rule = rule.split(':-')
        body = rule[1]
        rule[0] = rule[0].split('~')
        head = rule[0][0]
        distribution = rule[0][1]
        body = self.convertDCPredicates(body, 'body')
        head = self.convertDCPredicates(head, 'head')
        mean = ''
        distrib = distribution.strip()
        name,args = self.split_distrib(distrib)
            
        if 'Gaussian' in distribution:
            distribution = distribution.replace('Gaussian', 'gaussian')
            strip = False
            containsVariable = False
            for j in range(0, len(distribution)):
                c = distribution[j]
                if c == '(':
                    strip = True
                    continue
                if (c == ',') and (distribution[j-1] == ']') and (distribution[j+1] != '['):
                    strip = False
                if strip:
                    mean += c
                if c == ']' or c == '[':
                    containsVariable = True
            if (mean == '') or (not containsVariable):
                pass
            else:
                distribution = distribution.replace(mean, 'Mean')
                mean = ',getMean(' + mean + ',Mean).'
                body = body.replace('.', mean)
        elif 'Finite' in distribution:
            probs = ['{}:{}'.format(args[1][i],args[0][i]) for i in range(len(args[0]))]
            distribution = 'finite([{}])'.format(','.join(probs))
        elif 'LogisticRegression' in distribution:
            distribution = 'finite([Probability:{},Probability2:{}])'.format(args[1][0],args[1][1])
            tempBody = ', logistic({},{},Probability2), Probability is 1.0-Probability2.'.format([float(x) for x in args[2]],'[{}]'.format(', '.join([x.strip() for x in args[0]])))
            body = body.replace('.', tempBody)
        elif 'SoftmaxRegression' in distribution:
            d = 'finite(['
            p = '['
            argsLen = len(args[1])
            for i in range(0, argsLen):
                if i == argsLen-1:
                    d += 'Probability' + str(i+1) + ':' + args[1][i] + '])'
                    p += 'Probability' + str(i+1) + ']).'
                else:
                    d += 'Probability' + str(i+1) + ':' + args[1][i] + ','
                    p += 'Probability' + str(i+1) + ','
            distribution = d
            tempBody = ', softmax(' +  '[' + ','.join(args[2]) + ']' + ',[' + ','.join(args[0]) + '],' + p
            body = body.replace('.', tempBody)
        else:
            pass
        rule = head + ' ~ ' + distribution + ' := ' + body
        return rule


if __name__ == '__main__':
    pass
#     outputFile = '../data/MyDCRules.pl'
#     f = open(outputFile, 'w')
#     obj = TreeLearner('../data/prologUniv.pl', '../data/interp1.pl', 'SWI-prolog', '')
#     obj.learnRules()
#     obj1 = TranslateToDC()
#     for rule in obj.rules:
#         rule = obj1.translate(rule)
#         f.write(rule + '\n')
#     f.close()
    
