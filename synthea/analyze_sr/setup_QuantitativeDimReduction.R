
library(dplyr)
library(yaml)
library(readr)
library(reshape2)
library(ggplot2)
library(ggrepel)
library(cowplot)

#Need age at encounter
calcAgeAtEncounter = function(patientdata){
    patientdata$AgeAtEncounter = as.numeric(trunc((patientdata$DATE - patientdata$BIRTHDATE)/365))
    patientdata$Age = NA
    patientdata$Age[patientdata$AgeAtEncounter<18] = '<18'
    patientdata$Age[(patientdata$AgeAtEncounter>=18) & 
        (patientdata$AgeAtEncounter<40)] = '18-39'
    patientdata$Age[(patientdata$AgeAtEncounter>=40) & 
        (patientdata$AgeAtEncounter<60)] = '40-59'
    patientdata$Age[(patientdata$AgeAtEncounter>=60) & 
        (patientdata$AgeAtEncounter<80)] = '60-79'
    patientdata$Age[(patientdata$AgeAtEncounter>=80)] = '80+'

    return(patientdata)
}

trimByTests = function(patientdata, minPopTestNum){
    KeepTests = c()
    for(Test in unique(patientdata$DESCRIPTION)){
        if(nrow(patientdata[patientdata$DESCRIPTION==Test, ])>minPopTestNum){
            KeepTests = c(KeepTests, Test)
        }
    }
    patientdata = patientdata %>% filter(DESCRIPTION %in% KeepTests)
    patientdata$VALUE = as.numeric(patientdata$VALUE)
    return(patientdata)
}

sortByTestVals = function(patientdata){
    patientdata = dcast(patientdata, ID + ENCOUNTER + GENDER + RACE + AgeAtEncounter
     + Age ~ DESCRIPTION, value.var = c('VALUE'))
    return(patientdata)
}

plotPCARmOutliersAdultsBodyCompositionDiabetes = function(patientdata, outlierSDRemove, 
    conditiondata, valsIncludedTitle, nPoints){
    #This function takes the patient data and their condition histories and performs
    #   PCA on the quantiative lab values (and BP). outlierSDRemove refers to the
    #   number of standard deviations to limit the PCA outlier removal check (i.e. 
    #       how big of a Hotellings). Returns a scores and loadings plot with labels
    #   by diabetes diagnosis (since post-hoc analysis demonstrates the influence
    #       of A1C and Creatinine levels) and colors by age, as age drives a lot
    #   of variance as well, even with values such as height and weight removed. 
    #   Note only adults are analyzed here.

    #Find mention of diabetes - note the grep will exclude a common "Prediabetes" term 
    #   in the condition dataset
    T2D = unique(conditiondata$PATIENT[grep('Diabetes', 
        conditiondata$DESCRIPTION)])
    patientdata$Condition = 'No history of diabetes'
    patientdata$Condition[(patientdata$ID %in% T2D)] = 'Diabetic'
    patientdata$Condition[!(patientdata$ID %in% T2D)] = 'Non-diabetic'
    patientdata = patientdata[, colSums(is.na(patientdata)) < nrow(patientTestTrimChol)]

    #Adults only
    pcaTest = patientdata[complete.cases(patientdata), ]
    keepInfoAdult = pcaTest %>% filter(Age!='<18') %>% select(GENDER, RACE, AgeAtEncounter, 
        Age, Condition) %>% rename(Gender = GENDER, Race = RACE)
    dataOnlyAdult = pcaTest %>%  filter(Age!='<18') %>% select(-ID, -ENCOUNTER, -GENDER, -Condition,
        -RACE, -AgeAtEncounter, -Age, -'Body Mass Index', -'Body Height', -'Body Weight') %>% 
        mutate_all(as.numeric)

    #PCA
    pcaFitInitAdult = prcomp(dataOnlyAdult, center = TRUE, scale = TRUE)
    pcaFitInitAdultDF = as.data.frame(pcaFitInitAdult$x)

    #Remove outliers
    dataOnlyAdultRm = dataOnlyAdult[(-which(abs(pcaFitInitAdultDF$PC1) > 
        outlierSDRemove*pcaFitInitAdult$sdev[1] | 
        abs(pcaFitInitAdultDF$PC2) > outlierSDRemove*pcaFitInitAdult$sdev[2])), ]
    keepInfoAdultRm = keepInfoAdult[(-which(abs(pcaFitInitAdultDF$PC1) > 
        outlierSDRemove*pcaFitInitAdult$sdev[1] | 
        abs(pcaFitInitAdultDF$PC2) > outlierSDRemove*pcaFitInitAdult$sdev[2])), ]

    #Re-fit PCA
    pcaFitAdult = prcomp(dataOnlyAdultRm, center = TRUE, scale = TRUE)
    pcaFitAdultDF = as.data.frame(pcaFitAdult$x)

    plotPCAAdult = cbind(keepInfoAdultRm, pcaFitAdultDF[, c('PC1' ,'PC2')])
    plotPCAAdult$Age = as.factor(plotPCAAdult$Age)

    VarEx1 = round(summary(pcaFitAdult)$importance[2,1] * 100, 2) #Var explained
    VarEx2 = round(summary(pcaFitAdult)$importance[2,2] * 100, 2) #Var explained

    scoreGG = buildScoresPlot(plotPCAAdult, VarEx1, VarEx2, valsIncludedTitle, nPoints)
    loadingGG = buildLoadingPlot(pcaFitAdult, valsIncludedTitle)

    pcaCow = plot_grid(scoreGG, loadingGG, rel_widths = c(1, 0.8), align = 'h')

    return(pcaCow)
}


buildScoresPlot = function(pcaDF, varPC1, varPC2, valsIncludedTitle, nPoints){
    
    colorScale = scales::seq_gradient_pal("orange", "blue", "Lab")(seq(0, 1 , 
        length.out = length(unique(pcaDF$Age))))

    #Only plot 10k points
    set.seed(4)
    pcaDF = sample_n(pcaDF, nPoints)

    scoreGG = ggplot(pcaDF, aes(x = PC1, y = PC2, colour = Age, shape = Condition))+
        geom_point(size = 0.9)+
        theme_bw()+
        ylab(sprintf('PC2 (%s%s variance explained)', varPC2, '%')) + 
        xlab(sprintf('PC1 (%s%s variance explained)', varPC1, '%')) + 
        ggtitle(paste('Scores - ', valsIncludedTitle)) + 
        scale_colour_manual(values = colorScale)+
        geom_hline(yintercept = 0, alpha = 0.5)+
        geom_vline(xintercept = 0, alpha = 0.5)+
        theme(axis.text = element_blank(), axis.title = element_text(size = 9, face = "bold"), 
            legend.title = element_text(size = 7), legend.text = element_text(size = 8), 
            title = element_text(size = 8, face = "bold"),
            panel.grid.major = element_line(colour = 'grey98'), 
            panel.grid.minor = element_line(colour = 'grey98'),
            legend.box.margin = margin(-10, -10, -10, -10),
            plot.title = element_text(hjust = 0.5))
    
    return(scoreGG)

}

buildLoadingPlot = function(pcaDFObject, valsIncludedTitle){

    loadingPlot = as.data.frame(cbind(rownames(pcaDFObject$rotation), 
            pcaDFObject$rotation[, c('PC1', 'PC2')])) %>% rename(Metric = V1)
    loadingPlot$PC1 = as.numeric(as.character(unlist(loadingPlot$PC1)))
    loadingPlot$PC2 = as.numeric(as.character(unlist(loadingPlot$PC2)))

    loadingGG = ggplot(loadingPlot, aes(x = PC1, y = PC2))+
        geom_point(size = 1)+
        theme_bw()+
        ylab(NULL) + 
        xlab(NULL) + 
        ggtitle(paste('Loadings -', valsIncludedTitle))+
        geom_text_repel(aes(label = Metric), size = 2.5, max.iter = 10000)+
        geom_hline(yintercept = 0, alpha = 0.5)+
        geom_vline(xintercept = 0, alpha = 0.5)+
        theme(axis.text = element_blank(), 
            title = element_text(size = 8, face = "bold"),
            panel.grid.major = element_line(colour = 'grey98'), 
            panel.grid.minor = element_line(colour = 'grey98'),
            plot.title = element_text(hjust = 0.5))
    return(loadingGG)

}
