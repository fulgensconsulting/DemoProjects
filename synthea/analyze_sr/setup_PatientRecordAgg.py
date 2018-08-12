
import sys, re
import pandas as pd
import numpy as np

def combineDatasets(patientFile, procFile, obsFile, conditionFile):
    """ Combine the datasets for each individual and caculate their medical costs
        by age
    
    Args:
        patientFile: Base patient ID information
        procFile: Procedures history
        obsFile: History of observations (BMI, BP, etc..)
        conditionFile: History of conditions and phenotypes
    
    Returns:
        patientCostRecord: A patient's history of medical costs
        patientIDs: IDs of patients who are still alive
    """

    patients = pd.read_csv(patientFile)
    
    procedures = pd.read_csv(procFile)[['DATE', 'PATIENT', 'CODE']]
    procedures.columns = ['DATE', 'PATIENT', 'PROCEDURE_CODE']

    observations = pd.read_csv(obsFile)[['DATE', 'PATIENT', 'DESCRIPTION', 'VALUE']]
    observations.columns = ['DATE', 'PATIENT', 'OBSERVATION_DESCRIPTION', 'OBSERVATION_VALUE']

    conditions = pd.read_csv(conditionFile)[['START', 'PATIENT', 'CODE', 'DESCRIPTION']]
    conditions.START = [re.sub('T.*', '', str(x)) for x in conditions.START]
    conditions.columns = ['DATE', 'PATIENT', 'CONDITION_CODE', 'CONDITION_DESCRIPTION']

    #trim observations to BP, BMI, smoking first
    obsKeep = ['Tobacco smoking status NHIS', 'Body Mass Index', 
        'Diastolic Blood Pressure', 'Systolic Blood Pressure']
    observations = observations[observations.OBSERVATION_DESCRIPTION.isin(obsKeep)]

    patientProcRecs = patients.merge(procedures, how = 'inner', left_on = 'ID', 
        right_on = 'PATIENT')

    patientProcRecs['Age'] = ((pd.to_datetime(patientProcRecs.DATE) - 
        pd.to_datetime(patientProcRecs.BIRTHDATE)).astype('<m8[Y]'))
    
    patientObsRecs = patients.merge(observations, how = 'inner', left_on = 'ID', 
        right_on = 'PATIENT')

    patientObsRecs['Age'] = ((pd.to_datetime(patientObsRecs.DATE) - 
        pd.to_datetime(patientObsRecs.BIRTHDATE)).astype('<m8[Y]'))

    patientCondRecs = patients.merge(conditions, how = 'inner', left_on = 'ID', 
        right_on = 'PATIENT')

    patientCondRecs['Age'] = ((pd.to_datetime(patientCondRecs.DATE) - 
        pd.to_datetime(patientCondRecs.BIRTHDATE)).astype('<m8[Y]'))

    patientRecs = patientProcRecs.merge(patientObsRecs, how = 'left', on = None)
    patientRecs = patientRecs.merge(patientCondRecs, how = 'left', on = None)

    patientRecs = patientRecs[(patientRecs.DEATHDATE.notnull() & 
        patientRecs.Age.notnull())]
    aliveIDs = set(patientRecs.ID)

    patientRecs = patientRecs[['PATIENT', 'GENDER', 'RACE', 'ZIP', 'Age', 
        'PROCEDURE_CODE', 'OBSERVATION_DESCRIPTION', 'OBSERVATION_VALUE', 
        'CONDITION_CODE', 'CONDITION_DESCRIPTION']]
   
    return(patientRecs, aliveIDs)

def AggregateQuantValues(patientRecs, patientIDs):
    """ Aggregate values for BP and BMI within the same age year

    Args:
        patientRecs: Patient records
        patientIDs: Unique patients in the records

    Returns:
        patientRecs: Patient records, with values for BP and BMI averaged by age-year
    """
    pd.options.mode.chained_assignment = None

    #Make BP and BMI rounded floats for averaging
    patientRecs.OBSERVATION_VALUE[patientRecs.OBSERVATION_DESCRIPTION=='Diastolic Blood Pressure'] = (patientRecs.
        OBSERVATION_VALUE[patientRecs.OBSERVATION_DESCRIPTION=='Diastolic Blood Pressure'].
            astype(float).round())

    patientRecs.OBSERVATION_VALUE[patientRecs.OBSERVATION_DESCRIPTION=='Systolic Blood Pressure'] = (patientRecs.
        OBSERVATION_VALUE[patientRecs.OBSERVATION_DESCRIPTION=='Systolic Blood Pressure'].
            astype(float).round())

    patientRecs.OBSERVATION_VALUE[patientRecs.OBSERVATION_DESCRIPTION=='Body Mass Index'] = (patientRecs.
        OBSERVATION_VALUE[patientRecs.OBSERVATION_DESCRIPTION=='Body Mass Index'].
            astype(float).round())
     
    storeData = []
    for patient in patientIDs:
        patientStore = []
        for age in patientRecs.Age[patientRecs.PATIENT==patient].unique():
            diastole = patientRecs.OBSERVATION_VALUE[(patientRecs.PATIENT==patient) & 
                (patientRecs.Age==age) & 
                (patientRecs.OBSERVATION_DESCRIPTION=='Diastolic Blood Pressure')].mean()
            patientRecs.OBSERVATION_VALUE[(patientRecs.PATIENT==patient) & 
                (patientRecs.Age==age) & 
                (patientRecs.OBSERVATION_DESCRIPTION=='Diastolic Blood Pressure')] = diastole

            systole = patientRecs.OBSERVATION_VALUE[(patientRecs.PATIENT==patient) & 
                (patientRecs.Age==age) & 
                (patientRecs.OBSERVATION_DESCRIPTION=='Systolic Blood Pressure')].mean()
            patientRecs.OBSERVATION_VALUE[(patientRecs.PATIENT==patient) & 
                (patientRecs.Age==age) & 
                (patientRecs.OBSERVATION_DESCRIPTION=='Systolic Blood Pressure')] = systole
            
            bmi = patientRecs.OBSERVATION_VALUE[(patientRecs.PATIENT==patient) & 
                (patientRecs.Age==age) & 
                (patientRecs.OBSERVATION_DESCRIPTION=='Body Mass Index')].mean()
            patientRecs.OBSERVATION_VALUE[(patientRecs.PATIENT==patient) & 
                (patientRecs.Age==age) & 
                (patientRecs.OBSERVATION_DESCRIPTION=='Body Mass Index')] = bmi
            
            aggAge = patientRecs[(patientRecs.PATIENT==patient) & 
                (patientRecs.Age==age)].drop_duplicates()
            
            patientStore.append(aggAge)
        
        storeData.append(pd.concat(patientStore))
    
    returnRecs = pd.concat(storeData)
    return(returnRecs)
