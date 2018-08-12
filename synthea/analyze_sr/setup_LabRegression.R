#Modeling quatitative values over time. Using BRMS for Bayesian modeling

#Author: Seth Rhoades

library(dplyr)
library(rms)
library(mgcv)
library(yaml)
library(readr)
library(ggplot2)
library(cowplot)

#Pick common description (e.g. test)
calcAgeAtEncounter = function(patientdata){
    patientdata$AgeAtEncounter = trunc((patientdata$DATE - patientdata$BIRTHDATE)/365)
    return(patientdata)
}

prepForModel = function(patientdata, vital){
    patientdata = patientdata %>% filter(DESCRIPTION==vital) %>% select(RACE, 
        GENDER, AgeAtEncounter, VALUE)
    patientdata$VALUE = as.numeric(patientdata$VALUE)
    patientdata$RACE = as.factor(patientdata$RACE)
    patientdata$GENDER = as.factor(patientdata$GENDER)
    patientdata$AgeAtEncounter = as.numeric(patientdata$AgeAtEncounter)

    return(patientdata)
}

buildOrdinalModel = function(patientdata){

    patientdata$VALUE = round(patientdata$VALUE) #more suitable for ordinal
    patientdata = patientdata %>% filter(AgeAtEncounter >= 18 & RACE!='native') %>% 
        rename(Value = VALUE, Gender = GENDER, Race = RACE, Age = AgeAtEncounter) %>%
        mutate_at('Gender', as.character) %>% mutate_at('Race', as.character)

    patientdata$Gender[patientdata$Gender=='F'] = 'Female'
    patientdata$Gender[patientdata$Gender=='M'] = 'Male'
    patientdata$Race[patientdata$Race=='black'] = 'Black'
    patientdata$Race[patientdata$Race=='hispanic'] = 'Hispanic'
    patientdata$Race[patientdata$Race=='white'] = 'White'
    patientdata$Race[patientdata$Race=='asian'] = 'Asian'

    labModel = orm(Value ~ Race + Gender + rcs(Age), 
        data = patientdata, x = TRUE, y = TRUE)

    meanVals = Mean(labModel)
    modelPreds = Predict(labModel, Age = c(seq(min(patientdata$Age), 
        max(patientdata$Age), 1)), Gender = c('Male', 'Female'), 
        Race = c('Black', 'Hispanic', 'White', 'Asian'), fun = meanVals)
    return(modelPreds)
}

createLabGroupPlot = function(modelPreds, yLabel, xLabel, yLims){

    predsRace = aggregate(. ~ Age + Race, data = modelPreds, FUN = 'mean')
    plotRace = ggplot(predsRace, aes(x = Age, y = yhat, colour = Race))+
        geom_line(size = 1.2)+
        geom_ribbon(aes(ymin = lower, ymax = upper, fill = Race), colour = NA, alpha = 0.15)+
        theme_bw()+
        ylab(yLabel)+
        ylim(yLims)+
        xlab(xLabel)+
        theme(axis.text = element_text(size = 7), 
            axis.title = element_text(size = 8, face = 'bold'),
            legend.title = element_text(size = 8, face = 'bold'), 
            legend.text = element_text(size = 7),
            plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, unit = 'in'))
        
    predsGender = aggregate(. ~ Age + Gender, data = modelPreds, FUN = 'mean')
    plotGender = ggplot(predsGender, aes(x = Age, y = yhat, colour = Gender))+
        geom_line(size = 1.2)+
        geom_ribbon(aes(ymin = lower, ymax = upper, fill = Gender), colour = NA, alpha = 0.15)+
        theme_bw()+
        ylab(NULL)+
        ylim(yLims)+
        xlab(xLabel)+
        theme(axis.text = element_text(size = 7), 
            axis.title = element_text(size = 8, face = 'bold'),
            legend.title = element_text(size = 8, face = 'bold'), 
            legend.text = element_text(size = 7),
            plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, unit = 'in'))
    
    plotOut = plot_grid(plotRace, plotGender,  
        rel_widths = c(1, 0.95), hjust = 0, vjust = 1.2, nrow = 1)
    return(plotOut)

}
