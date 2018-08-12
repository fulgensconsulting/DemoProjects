# Cursory analysis of syntheas synthetic EHR data. Tracking allergy prevalence
# over time, and if people have more than one allergy.
#
# Plots in /plots.
#
# Author: Seth Rhoades

library(readr)
library(dplyr)
library(ggplot2)
library(cowplot)
library(yaml)

filePath = ('.') #within explore right now

cmdArgs = commandArgs(TRUE)
if(length(cmdArgs) == 0){
  paramDir = 'params_sr'
  paramFile = 'params_AllergyHistory.yaml'
} else {
  paramDir = dirname(cmdArgs[1])
  paramFile = basename(cmdArgs[1])
}

if (sys.nframe() == 0){

    params = read_yaml(file.path(filePath, paramDir, paramFile))
    source(file.path(params$functionSource)) #function source

    allergies = read_csv(file.path(params$allergies), 
        col_types = 'DDcccc') %>% rename(Allergy = DESCRIPTION, DOB = START) %>% 
            as.data.frame()

    patients_allergies = read_csv(file.path(params$patients)) %>%
        left_join(allergies, by = c('ID' = 'PATIENT'))
        
    allergiesYOB = calcYOB(patients_allergies)
    yearSeq = extractYearSeq(allergiesYOB, 5)
    allergiesAddNorm = normalizeAllergyFrequencies(allergiesYOB, yearSeq)

    singleAllergyPlot = plotSingleAllergyFrequencies(allergiesAddNorm, 1950, 2005)

    allergyNums = histoAllergyNum(allergiesAddNorm, yearSeq)
    allergyNumsPlot = allergyNumPlot(allergyNums, 1950, 2005)

    outPlot = plot_grid(allergyNumsPlot, singleAllergyPlot, nrow = 2, align = 'v', 
        rel_heights = c(0.9, 1))
    ggsave(filename = 'AllergyFrequencies.png', plot = outPlot, 
        path = file.path(params$plotDir), height = 7, width = 6)

}
