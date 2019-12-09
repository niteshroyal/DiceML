'''
Created on Aug 28, 2017

@author: niteshkumar
'''
import numpy as np
from sklearn.linear_model import LogisticRegression
from sklearn.linear_model import LinearRegression

class ScoreFinderProbabilistic(object):
    def __init__(self):
        numIteration = 1000
        # For softmax regression
        regularization = 1
        self.softmax = LogisticRegression(solver='lbfgs', multi_class='multinomial', C=regularization, penalty='l2', fit_intercept=True, max_iter=numIteration)
        
        # For linear regression
        self.linear = LinearRegression(fit_intercept=True)
    
    def fitNoDistribution(self):
        result = {'score' : np.nan, 'coefficient' : np.nan, 'variance' : np.nan, 'distribution': 'NoDistribution'}
        return result
    
    def fitGaussainDistribution(self, bigY, bigYCross, bigP, numOfExamples):
        mean = 0
        numRows = len(bigY)
        sumProb = 0
        for i in range(0, numRows):
            mean += bigY[i]*bigP[i]
            sumProb += bigP[i]
        mean = mean/float(sumProb)
        variance = 0.0
        for i in range(0, numRows):
            variance += bigP[i]*(bigY[i] - mean)*(bigY[i] - mean)
        if numOfExamples < 2:
            variance = 0.0
        else:
            variance = variance/float(numOfExamples-1)
        score = 0
        bigP1 = np.log(bigP)
        pScore = sum(bigP1)
        pScore = 2*pScore
        if variance == 0.0:
            pass
        else:
            cons1 = -0.91893853320467267 - 0.5*np.log(variance)
            cons2 = -0.5*(1/variance)
            for i in range(0,numRows):
                a = bigY[i] - mean
                a *= a
                a *= cons2*bigP[i]
                a += cons1*bigP[i]
                score += a
        distribution = 'Gaussian(' + str(mean) + ', ' + str(variance) + ')'
        bscore = 2*score
        result = {'score' : bscore, 'coefficient' : mean, 'variance' : variance, 'distribution': distribution, 'likelihood': score}
        return result
    
    def fitMultinomialDistribution(self, bigY, bigYCross, bigP):
        classes = {}
        classLogProbs = {}
        classProbs = []
        sumProb = sum(bigP)
        for i in range(0, len(bigY)):
            if classes.has_key(bigY[i]):
                cnt = classes[bigY[i]]
                classes[bigY[i]] = cnt + bigP[i]
            else:
                classes[bigY[i]] = bigP[i]
        leni = len(classes)-1
        i = 0
        classKeys = '['
        for key in classes.keys():
            classLogProbs[key] = np.log(classes[key]/float(sumProb))
            classProbs.append(classes[key]/float(sumProb))
            if i == leni:
                classKeys += str(key) + ']'
            else:
                classKeys += str(key) + ', '
            i += 1
        score = 0
        for i in range(0, len(bigYCross)):
            score += classLogProbs[bigYCross[i]]*bigP[i]
        distribution = 'Finite(' + classKeys + ', ' + str(classProbs) + ')' 
        bscore = 2*score
        result = {'score': bscore, 'coefficient': classProbs, 'distribution': distribution, 'likelihood': score}
        return result
        
    def fitLinearRegressionModel(self, bigX, bigY, bigXCross, bigYCross, xVar, bigP, numOfExamples):
        colSize = len(bigX)
        rowSize = len(bigX[0])
        data = []
        for i in range(0, rowSize):
            aRow = [] 
            for j in range(0, colSize):
                aRow.append(bigX[j][i])
            data.append(aRow)
        model = self.linear.fit(data, bigY, sample_weight=bigP)
        coef = model.coef_
        intercept = model.intercept_
        param = list(coef)
        param.append(intercept)
        probs = []
        colSize = len(bigXCross)
        rowSize = len(bigXCross[0])
        data = []
        for i in range(0, rowSize):
            aRow = [] 
            for j in range(0, colSize):
                aRow.append(bigXCross[j][i])
            data.append(aRow)
        numCols = len(data[0])
        numRows = len(data)
        numCoef = len(param)
        for rec in data:
            p = 0
            for i in range(0,numCols):
                p += coef[i]*rec[i]
            p += intercept
            probs.append(p)
        score = 0
        variance = 0
        for i in range(0, numRows):
            variance += bigP[i]*(bigYCross[i] - probs[i])*(bigYCross[i] - probs[i])
        if numOfExamples < 2:
            variance = 0
        else:
            variance = variance/float(numOfExamples-1)
        if variance == 0.0:
            pass
        else:
            cons1 = -0.91893853320467267 - 0.5*np.log(variance)
            cons2 = -0.5*(1/variance)
            for i in range(0,numRows):
                a = bigYCross[i] - probs[i]
                a *= a
                a *= cons2*bigP[i]
                a += cons1*bigP[i]
                score += a
        distribution = 'Gaussian(['
        leni = len(xVar)-1
        i = 0
        for var in xVar:
            if i == leni:
                distribution += var + '], '
            else:
                distribution += var + ', '
            i += 1
        distribution += str(param) + ', ' + str(variance) + ')'
        bscore = 2*score-(numCoef*np.log(numOfExamples))
        result = {'score' : bscore, 'coefficient' : param, 'variance' : variance, 'distribution': distribution, 'likelihood': score}
        return result
    
    def fitMultinomialClassificationModel(self, bigX, bigY, bigXCross, bigYCross, xVar, bigP, numOfExamples):
        colSize = len(bigX)
        rowSize = len(bigX[0])
        data = []
        for i in range(0, rowSize):
            aRow = [] 
            for j in range(0, colSize):
                aRow.append(bigX[j][i])
            data.append(aRow)
        model = self.softmax.fit(data, bigY, sample_weight=bigP)
        colSize = len(bigXCross)
        rowSize = len(bigXCross[0])
        data = []
        for i in range(0, rowSize):
            aRow = [] 
            for j in range(0, colSize):
                aRow.append(bigXCross[j][i])
            data.append(aRow)
        logEstimate = model.predict_log_proba(data)
        classes = model.classes_.tolist()
        classString = '['
        leni = len(classes)-1
        for i in range(0,len(classes)):
            if i == leni:
                classString += str(classes[i]) + ']'
            else:
                classString += str(classes[i]) + ', '
        coef = model.coef_
        intercept = model.intercept_
        score = 0
        for i in range(0,len(logEstimate)):
            idx = classes.index(bigYCross[i])
            score += logEstimate[i][idx]*bigP[i]
        coef = np.concatenate((coef, np.array([list(intercept)]).T), axis=1)
        param = {}
        parameter = []
        idx = 0
        for arr in coef:
            param[classes[idx]] = list(arr)
            parameter.append(list(arr))
            idx += 1
        
        numCoef = 0
        if(len(parameter) == 1):
            numCoef = colSize+1
            distribution = 'LogisticRegression(['
            leni = len(xVar)-1
            i = 0
            for var in xVar:
                if i == leni:
                    distribution += var + '], '
                else:
                    distribution += var + ', '
                i += 1
            distribution += classString + ', ' + str(parameter[0]) + ')'
        else:
            numCoef = len(classes) * (colSize+1)
            distribution = 'SoftmaxRegression(['
            leni = len(xVar)-1
            i = 0
            for var in xVar:
                if i == leni:
                    distribution += var + '], '
                else:
                    distribution += var + ', '
                i += 1
            distribution += classString + ', ' + str(parameter) + ')'
        bscore = 2*score - numCoef*np.log(numOfExamples)
        result = {'score' : bscore, 'coefficient': param, 'distribution': distribution, 'likelihood': score}
        return result

    def getScore(self, bigX, bigY, targetType, xVar, bigP, numOfExamples, numSamples):
        bigX1 = []
        bigY1 = []
        bigP1 = []
        for i in range(0, len(bigP)):
            if bigP[i] == 0.0:
                pass
            else:
                bigP1.append(bigP[i])
                bigY1.append(bigY[i])
        for j in range(0, len(bigX)):
            a = []
            for i in range(0, len(bigP)):
                if bigP[i] == 0.0:
                    pass
                else:
                    a.append(bigX[j][i])
            bigX1.append(a)
        bigX = bigX1
        bigXCross = bigX1
        bigY = bigY1
        bigYCross = bigY1
        bigP = bigP1
        scoreAndDistribution = None
        bigXEmpty = False
        if(len(bigX) == 0):
            bigXEmpty = True
        elif(len(bigX[0]) == 0):
            bigXEmpty = True
        else:
            bigXEmpty = False
        if(bigXEmpty):
            if(bigY == []):
                scoreAndDistribution = self.fitNoDistribution()
            elif(targetType == 'discrete'):
                scoreAndDistribution = self.fitMultinomialDistribution(bigY, bigYCross, bigP)
            elif(targetType == 'continuous'):
                scoreAndDistribution = self.fitGaussainDistribution(bigY, bigYCross, bigP, numOfExamples)
            else:
                pass
        elif(targetType == 'continuous'):
            scoreAndDistribution = self.fitLinearRegressionModel(bigX, bigY, bigXCross, bigYCross, xVar, bigP, numOfExamples)
        elif(targetType == 'discrete'):
            if(len(set(bigY)) == 1):
                scoreAndDistribution = self.fitMultinomialDistribution(bigY, bigYCross, bigP)
            else:
                scoreAndDistribution = self.fitMultinomialClassificationModel(bigX, bigY, bigXCross, bigYCross, xVar, bigP, numOfExamples)
        else:
            print "Target variable type undefined"
        return scoreAndDistribution
            
if __name__ == '__main__':
    X = [[0.2, 4.1, 4.9, 0.1, 0.2, 0.3], [0.3, 0.2, 4.1, 4.2, 4.0, 3.9]]
    Y = [1, 1, 1, 2, 1, 2]
    P = [0.05, 0.05, 0.06, 0.06, 0.01, 0.06]
    sf = ScoreFinderProbabilistic()
    a = sf.getScore(X, Y, 'discrete', ['A', 'B'], P)
    print a 
    b = sf.getScore([], Y, 'discrete', ['A', 'B'], P)
    print b
