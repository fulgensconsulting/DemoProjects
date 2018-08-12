# Plot ROC and PR for myocardial infarction with LSTMs (from LSTMFit.py)
#
# Plots in /plots.
#
# Author: Seth Rhoades

library(readr)
library(ggplot2)
library(cowplot)
library(precrec)
library(yaml)

filePath = ('.') #within explore right now

cmdArgs = commandArgs(TRUE)
if(length(cmdArgs) == 0){
  paramDir = 'params_sr'
  paramFile = 'params_LSTMPredictionsROC.yaml'
} else {
  paramDir = dirname(cmdArgs[1])
  paramFile = basename(cmdArgs[1])
}

if (sys.nframe() == 0){

    params = read_yaml(file.path(filePath, paramDir, paramFile))
    source(file.path(params$functionSource)) #function source

    predictions = read_csv(file.path(filePath, params$preds), col_types = 'idd')
    predictions$yPred = predictions$yPredScores
    predictions$yPred = round(predictions$yPred)

    predAcc = (sum(predictions$yTest==predictions$yPred)/nrow(predictions))*100
    print(paste0('Accuracy of y predictions: ', round(predAcc, 2), '%'))

    precPrep = evalmod(scores = predictions$yPredScores, labels = predictions$yTest)
    #Given the big inconvenience of extracting the numerical values, I will add them manually
    rocAUC = c(0.836)
    prAUC = c(0.816)

    plotData = as.data.frame(precPrep)
    roc = plotROC(plotData, rocAUC)
    pr = plotPR(plotData, prAUC)
    curveCow = plot_grid(roc, pr)
    ggsave(filename = 'MI_PRROC.png', plot = curveCow, path = file.path(params$plotDir), 
        height = 3, width = 6)

}
