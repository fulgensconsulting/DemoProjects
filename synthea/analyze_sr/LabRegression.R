#Modeling quatitative values over time. Given cholesterol and creatinine
#are important variables in population-level patient groupings, regression
#modeling strategies may be useful to discern if these differences play out over
#age and racial background.
#
#Author: Seth Rhoades

library(dplyr)
library(yaml)
library(readr)
library(ggplot2)
library(cowplot)

filePath = ('.') #within explore right now

cmdArgs = commandArgs(TRUE)
if(length(cmdArgs) == 0){
  paramDir = 'params_sr'
  paramFile = 'params_LabRegression.yaml'
} else {
  paramDir = dirname(cmdArgs[1])
  paramFile = basename(cmdArgs[1])
}

if(sys.nframe() == 0){

  params = read_yaml(file.path(filePath, paramDir, paramFile))
  source(file.path(params$functionSource))

  observations = read_csv(file.path(params$observations))

  patientObs = read_csv(file.path(params$patients)) %>%
      left_join(observations, by = c('ID' = 'PATIENT')) %>% select(ID, BIRTHDATE, 
      RACE, ETHNICITY, GENDER, DATE, ENCOUNTER, CODE, DESCRIPTION, VALUE, UNITS, TYPE) %>%
      as.data.frame()

  patientObsAge = calcAgeAtEncounter(patientObs)
  patientObsLDL = prepForModel(patientObsAge, "Low Density Lipoprotein Cholesterol")
  LDLPreds = buildOrdinalModel(patientObsLDL)
  LDLPlot = createLabGroupPlot(LDLPreds, 'LDL (mg/dL)', NULL, c(80, 120))

  patientObsHDL = prepForModel(patientObsAge, "High Density Lipoprotein Cholesterol")
  HDLPreds = buildOrdinalModel(patientObsHDL)
  HDLPlot = createLabGroupPlot(HDLPreds, 'HDL (mg/dL)', NULL, c(50, 70))

  patientObsCreatinine = prepForModel(patientObsAge, "Creatinine")
  CreatininePreds = buildOrdinalModel(patientObsCreatinine)
  CreatininePlot = createLabGroupPlot(CreatininePreds, 'Creatinine (mg/dL)', 'Age', c(0, 4))

  LabPlots = plot_grid(LDLPlot, HDLPlot, CreatininePlot, nrow = 3)
  ggsave(filename = 'LabRegressions.pdf', path = file.path(params$plotDir), 
      width = 6.5, height = 7)
}
