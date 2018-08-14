#Taking quantiative values, dimension-reduction,  then
# PCA of the quantiative values (lab work and blood pressure mainly). What
#factors dominate patient clustering?
#
#Plots in /plots
#
#Author: Seth Rhoades

library(dplyr)
library(yaml)
library(readr)
library(ggplot2)
library(cowplot)

filePath = ('.') 

cmdArgs = commandArgs(TRUE)
if(length(cmdArgs) == 0){
  paramDir = 'params_sr'
  paramFile = 'params_QuantitativeDimReduction.yaml'
} else {
  paramDir = dirname(cmdArgs[1])
  paramFile = basename(cmdArgs[1])
}


if (sys.nframe() == 0){
        
    params = read_yaml(file.path(filePath, paramDir, paramFile))
    source(file.path(params$functionSource))

    observations = read_csv(file.path(params$observations))
    conditions = read_csv(file.path(params$conditions))

    patientObs = read_csv(file.path(params$patients)) %>%
        left_join(observations, by = c('ID' = 'PATIENT')) %>% select(ID, BIRTHDATE, 
        RACE, ETHNICITY, GENDER, DATE, ENCOUNTER, CODE, DESCRIPTION, VALUE, UNITS, TYPE) %>%
        as.data.frame()
        
    patientObsAge = calcAgeAtEncounter(patientObs)
    patientTestTrimChol = trimByTests(patientObsAge, minPopTestNum = 200000)
    patientTestTrimNoChol = trimByTests(patientObsAge, minPopTestNum = 250000) 
    #these PopTestNums are a division between including cholesterol or not
    
    patientTestTrimChol = sortByTestVals(patientTestTrimChol)
    patientTestTrimNoChol = sortByTestVals(patientTestTrimNoChol)

    patientPlotChol = plotPCARmOutliersAdultsBodyCompositionDiabetes(patientTestTrimChol, 
        outlierSDRemove = 3, conditions, 'Cholesterol Included', nPoints = 7500)
    patientPlotNoChol = plotPCARmOutliersAdultsBodyCompositionDiabetes(patientTestTrimNoChol, 
        outlierSDRemove = 3, conditions, 'Cholesterol Excluded', nPoints = 7500)
    
    combinedPlot = plot_grid(patientPlotNoChol, patientPlotChol, nrow = 2)
    ggsave(filename = 'LabValueAllPCA.pdf', plot = combinedPlot, 
        path = file.path(params$plotDir), height = 6, width = 8)
    ggsave(filename = 'LabValueAllPCA.jpeg', plot = combinedPlot, 
        path = file.path(params$plotDir), height = 6, width = 8, dpi = 600)

}
