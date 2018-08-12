
import sys, re, random
import pandas as pd
import numpy as np
from itertools import chain
from sklearn.model_selection import train_test_split

def buildDictsSortHistories(patientRecs):
    """ Build dictionaries of covariates for post-modeling analysis. Also order
        each patient's records by their age

    Args:
        patientRecs: Patient medical history

    Returns:
        raceDict: Dictionary of unique races, numerically coded
        genderDict: Dictionary of M/F, 0/1 coded
        zipDict: Dictionary of unique zip codes represented in the data
        smokerDict: Dictionary of unique smoking status (should be 3)
        conditionDict: Dictionary of unique conditions
        procedureDict: Dictionary of unique procedures
        sortedHistory: History of patient records, ordered by age
    """
    raceDict = dict()
    for i, j in reversed(list(enumerate(set(patientRecs.RACE)))):
        raceDict.update({j:i})

    genderDict = dict()
    for i, j in reversed(list(enumerate(set(patientRecs.GENDER)))):
        genderDict.update({j:i})

    patientRecs.ZIP = ['0' + re.sub('\.0', '', str(x)) for x in patientRecs.ZIP]
    zips = set(patientRecs.ZIP)
    zipDict = dict()
    for i, j in enumerate(zips):
        zipDict.update({j:i})

    smokerDict = dict()
    for i, j in reversed(list(enumerate(set(patientRecs.OBSERVATION_VALUE[patientRecs.
        OBSERVATION_DESCRIPTION=='Tobacco smoking status NHIS'])))):
        smokerDict.update({j:i})

    conditionDict = dict()
    for i, j in reversed(list(enumerate(set(patientRecs.CONDITION_DESCRIPTION)))):
        conditionDict.update({j:i})

    procedureDict = dict()
    for i, j in reversed(list(enumerate(set(patientRecs.PROCEDURE_CODE)))):
        procedureDict.update({j:i})

    sortHistory = [(x[0], x[1].sort_values('Age')) for x in patientRecs.groupby('PATIENT')]

    return(raceDict, genderDict, zipDict, smokerDict, conditionDict, procedureDict, 
        sortHistory)


def conditionVars(singleRecord, conditionDict):
    """ Create a 0/1 vector of the unique conditions for a given individual's record

    Args:
        singleRecord: One age-year of data for one person
        conditionDict: Dictionary of conditions, with values as unique numbers
    Returns:
        0/1 vector based on which conditions an individual received in a given age-year
    """
    conditionVec = np.zeros(len(conditionDict))
    changeLocation = []
    for condition in list(set(singleRecord.CONDITION_DESCRIPTION.values)):
        if condition != 'nan':
            locationAdd = conditionDict[condition]
            changeLocation.append(locationAdd)

    for loc in changeLocation:
        conditionVec[loc] = 1.
    return(list(conditionVec))


def procedureVars(singleRecord, procedureDict):
    """ Create a 0/1 vector of the unique procedures for a given individual's record

    Args:
        singleRecord: One age-year of data for one person
        conditionDict: Dictionary of procedures, with values as unique numbers
    Returns:
        0/1 vector based on which procedures an individual received in a given age-year
    """
    procedureVec = np.zeros(len(procedureDict))
    changeLocation = []
    for procedure in list(set(singleRecord.PROCEDURE_CODE.values)):
        if procedure != 'nan':
            locationAdd = procedureDict[procedure]
            changeLocation.append(locationAdd)

    for loc in changeLocation:
        procedureVec[loc] = 1.
    return(list(procedureVec))

def extractVarsOneStep(singleRecord, singleZip, singleGender, singleRace,
    zipDict, genderDict, raceDict, smokerDict, conditionDict, procedureDict, age):
    """ Create the values for a one-step entry in an RNN-style dataset

    Args:
        singleRecord: One age-year of data for one person
        singleZip: Individual zip code, already-calcuated to take out of loops
        singleGender: Individual gender value, already-calcuated to take out of loops
        singleRace: Individual race value, already-calcuated to take out of loops
        conditionDict: Dictionary of procedures, with values as unique numbers
        raceDict: Dictionary of unique races, numerically coded
        genderDict: Dictionary of M/F, 0/1 coded
        zipDict: Dictionary of unique zip codes represented in the data
        smokerDict: Dictionary of unique smoking status (should be 3)
        conditionDict: Dictionary of unique conditions
        procedureDict: Dictionary of unique procedures
        age: Year-age
    Returns:
        Concatenated vector of values for a single age-year individual's record
    """
    #If values exist, take it, if not, set a missingness indicator variable
    if 'Tobacco smoking status NHIS' in singleRecord.OBSERVATION_DESCRIPTION.values:
        smokerStatus = [smokerDict[(singleRecord.OBSERVATION_VALUE[singleRecord.
            OBSERVATION_DESCRIPTION=='Tobacco smoking status NHIS'].values[0])], 0.]
    else:
        smokerStatus = [0., 1.]

    if 'Diastolic Blood Pressure' in singleRecord.OBSERVATION_DESCRIPTION.values:
        diaStatus =  [(singleRecord.OBSERVATION_VALUE[singleRecord.
            OBSERVATION_DESCRIPTION=='Diastolic Blood Pressure'].astype(float).values[0]), 0.]
    else:
        diaStatus = [0., 1.]

    if 'Systolic Blood Pressure' in singleRecord.OBSERVATION_DESCRIPTION.values:
        sysStatus = [(singleRecord.OBSERVATION_VALUE[singleRecord.
            OBSERVATION_DESCRIPTION=='Systolic Blood Pressure'].astype(float).values[0]), 0.]
    else:
        sysStatus = [0., 1.]

    if 'Body Mass Index' in singleRecord.OBSERVATION_DESCRIPTION.values:
        bmiStatus = [(singleRecord.OBSERVATION_VALUE[singleRecord.
            OBSERVATION_DESCRIPTION=='Body Mass Index'].astype(float).values[0]), 0.]
    else:
        bmiStatus = [0., 1.]

    singleVisit = ([zipDict[singleZip]], [genderDict[singleGender]],
        [raceDict[singleRace]], 
        conditionVars(singleRecord, conditionDict),
        procedureVars(singleRecord, procedureDict),
        smokerStatus, diaStatus, sysStatus, bmiStatus, [age])
    
    return(list(chain.from_iterable(singleVisit)))

def balanceSamples(xData, yData, ratio):
    """Create a ratio:1 sample for 0s to 1s in an unbalanced dataset
    """
    zeros = [i for i,j in enumerate(yData) if j==0.]
    ones = [i for i,j in enumerate(yData) if j==1.]
    random.Random(10).shuffle(zeros)
    zeros = zeros[0:len(ones)*ratio]
    allLocs = zeros + ones
    xData, yData = xData[allLocs], yData[allLocs]

    return(xData, yData)

def buildRNNData(patientRecs, nSteps, phenotype, zipDict, genderDict, raceDict,
    smokerDict, conditionDict, procedureDict, posControl = 'no'):
    """ Build x and y datasets for the phenotype of interest (e.g. Diabetes). See
        other functions for more details. The objective to is to take windows of nSteps
        in a patient's record with and without the phenotype of interest. Note that
        for those who have the phenotype of interest, we only take records up until
        they receive that phenotype, with the goal of a sequence classification of
        whether or not someone may get the diagnosis in the future.
    
    Args:
        (see above for most of these arguments)
        nSteps: Number of timesteps to consider
        phenotype: The condition are we trying to predict

    Returns:
        xTrain, xTest, yTrain, yTest data, with y being 0/1 indicator of the phenotype
        occuring some point in the patient's future.
    """
    xData = []
    yData = []
    for patient in patientRecs:
        oneZip = patient[1].ZIP.values[0]
        oneGender = patient[1].GENDER.values[0]
        oneRace = patient[1].RACE.values[0]

        if phenotype in patient[1].CONDITION_DESCRIPTION.values:
            firstAge = min(patient[1].Age[patient[1].CONDITION_DESCRIPTION==phenotype])
            if len(set(patient[1].Age[patient[1].Age < firstAge])) >= nSteps:
                subRec = patient[1][patient[1].Age < firstAge]
                uniAges = list(set(subRec.Age.values))
                for i,_ in enumerate(uniAges):
                    if i+nSteps <= len(uniAges):
                        matchAges = uniAges[i:i+nSteps]
                        subsubRec = subRec[subRec.Age.isin(matchAges)]
                        timeSeries = [] #fill nSteps into here
                        for subAge in matchAges:
                            oneAge = subsubRec[subsubRec.Age==subAge]
                            oneAgeData = extractVarsOneStep(oneAge, oneZip, oneGender, 
                            oneRace, zipDict, genderDict, raceDict, smokerDict,
                                conditionDict, procedureDict, subAge)
                            timeSeries.append(oneAgeData)
                        xData.append(timeSeries)
                        yData.append(1.)

        else:        
            subRec = patient[1]
            uniAges = list(set(patient[1].Age.values))
            for i,_ in enumerate(uniAges):
                if i+nSteps <= len(uniAges):
                    matchAges = uniAges[i:i+nSteps]
                    subsubRec = subRec[subRec.Age.isin(matchAges)]
                    timeSeries = [] #fill nSteps into here
                    for subAge in matchAges:
                        oneAge = subsubRec[subsubRec.Age==subAge]
                        oneAgeData = extractVarsOneStep(oneAge, oneZip, oneGender, 
                            oneRace, zipDict, genderDict, raceDict, smokerDict,
                            conditionDict, procedureDict, subAge)
                        timeSeries.append(oneAgeData)
                    xData.append(timeSeries)
                    yData.append(0.)
    
    xData, yData = np.array(xData), np.array(yData)

    xData, yData = balanceSamples(xData, yData, 1)

    #Make a positive control for the model
    if posControl == 'yes':
        xData = makePosControl(xData, yData)

    xTrain, xTest, yTrain, yTest = train_test_split(xData, 
        yData, test_size = 0.25, random_state = 10)

    return(xTrain, xTest, yTrain, yTest)

def makePosControl(xData, yData):

    addMe = np.zeros((xData.shape[0], xData.shape[1], 1))
    xData = np.dstack((xData, addMe))
    xData[:, :, -1] += yData.reshape(yData.shape[0], 1)*100

    return(xData)

def write3DArray(data, fileName):

    with open(fileName, 'w') as outfile:
        outfile.write('# Array shape: {0}\n'.format(data.shape))
        for data_slice in data: 
            np.savetxt(outfile, data_slice, fmt='%-7.2f')
            outfile.write('# New slice\n')

