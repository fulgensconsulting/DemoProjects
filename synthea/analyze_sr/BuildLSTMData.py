''' Prep patient records for modeling time-series with LSTMs.

    Author: Seth Rhoades
'''

import sys, re
import pandas as pd
import numpy as np
import setup_PhenotypePredict as util

def main(patientRecords, phenotype, nsteps):

    patientRecords.columns = ['PATIENT', 'GENDER', 'RACE', 'ZIP', 'Age',
            'PROCEDURE_CODE', 'OBSERVATION_DESCRIPTION', 'OBSERVATION_VALUE',
            'CONDITION_CODE', 'CONDITION_DESCRIPTION']
    
    (raceDict, genderDict, zipDict, smokerDict, conditionDict, procedureDict, 
        orderedRecs) = util.buildDictsSortHistories(patientRecords)

    xtrain, xtest, ytrain, ytest = util.buildRNNData(orderedRecs, nsteps, phenotype,
        zipDict, genderDict, raceDict, smokerDict, conditionDict, procedureDict, 'no')

    return xtrain, xtest, ytrain, ytest

if __name__ == '__main__':
        
    try:
        patientFile = sys.argv[1]
    except IndexError:
        patientFile = 'AggregatePatientData.csv'

    pheno = 'Myocardial Infarction'
    nSteps = 5
    
    records = pd.read_csv(patientFile, header = None)
    
    xTrain, xTest, yTrain, yTest = main(records, pheno, nSteps)

    util.write3DArray(xTrain, 'xTrain.txt')
    util.write3DArray(xTest, 'xTest.txt')
    np.savetxt('yTrain.txt', yTrain)
    np.savetxt('yTest.txt', yTest)

