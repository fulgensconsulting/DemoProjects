''' Aggregate patient data across patient IDs, procedure, observation, condition
    histories. In preparation for future analysis, such as sequence classifiction
    via LSTMs.

    Author: Seth Rhoades
'''

import sys, re
import pandas as pd
import numpy as np
import setup_PatientRecordAgg as util
from pathos.helpers import mp
from itertools import repeat

try:
    patientFile = sys.argv[1]
    procedureFile = sys.argv[2]
    observationFile = sys.argv[3]
    conditionFile = sys.argv[4]
    coreNum = sys.argv[5]
except IndexError:
    patientFile = 'output/csv/patients.csv'
    procedureFile = 'output/csv/procedures.csv'
    observationFile = 'output/csv/observations.csv'
    conditionFile = 'output/csv/conditions.csv'
    coreNum = 10

def main(patientFile, procedureFile, observationFile, conditionFile, coreNum):
        
    patientRecs, IDs = util.combineDatasets(patientFile, procedureFile, observationFile,
        conditionFile)

    patientRecCopy = list(repeat(patientRecs, coreNum))
    splitIDs = np.array_split(list(IDs), coreNum)

    pooler = mp.Pool(coreNum)

    with open('AggregatePatientData.csv', 'a') as fout:
        for result in pooler.starmap(util.AggregateQuantValues, zip(patientRecCopy, 
            splitIDs)):
            result.to_csv(fout, index=False, header=False)

if __name__ == '__main__':
    main(patientFile, procedureFile, observationFile, conditionFile, coreNum)
