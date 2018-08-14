
library(readr)
library(dplyr)
library(reshape2)
library(stringr)
library(ggplot2)
library(cowplot)

calcYOB = function(df){
    df$YOB = as.integer(substr(df$DOB, start = 1, stop = 4))
    return(df)
}

extractYearSeq = function(df, yearWindow){
    if(is.numeric(yearWindow)){
        minYear = min(df$YOB[!is.na(df$YOB)])
        maxYear = max(df$YOB[!is.na(df$YOB)])
        minYearAdj = minYear - (minYear %% yearWindow)
        maxYearAdj = maxYear - (maxYear %% yearWindow) + yearWindow
        yearSequence = seq(minYearAdj, maxYearAdj, by = yearWindow)
        return(yearSequence)
    } else {
        print('Invalid years, enter a number') 
    }
}

normalizeAllergyFrequencies = function(df, yearSequence){
    #This function calculates the frequency of reported allergies from all individauls
    #   in the records within the 5 year time spans I selected.
    df$AllergyFreq = 0
    for(t in 1:(length(yearSequence)-1)){
        for(allergy in unique(df$Allergy)){
            filterDF1 = df %>% filter(YOB > yearSequence[t] & YOB < yearSequence[t+1])
            filterDF2 = df %>% filter(YOB > yearSequence[t] & YOB < yearSequence[t+1] & 
                Allergy==allergy)
            #Unique individuals
            val = length(unique(filterDF2$ID))/length(unique(filterDF1$ID))
            if(is.na(val)){
                val = 0
            } else if (val > 1) {
                print('invalid probability')
                stop
            }
        df$AllergyFreq[(df$YOB > yearSequence[t] & df$YOB < yearSequence[t+1] & 
            df$Allergy==allergy)] = val
        }
    }
    return(df)
}

plotSingleAllergyFrequencies = function(df, yearCutoffmin, yearCutoffmax){
    allergyNormGG = ggplot(df, aes(x = YOB, y = AllergyFreq, colour = Allergy))+
    theme_bw()+
    xlab('Year of Birth')+
    ylab('Period prevalence')+
    ggtitle(paste0('Prevalence of individual allergies'))+
    geom_smooth(data = df[(df$YOB > yearCutoffmin & df$YOB < yearCutoffmax & 
        !is.na(df$AllergyFreq)), ],
        method = 'gam', formula = y ~ s(x, k = 4, bs = 'cs'), se = FALSE)+
    theme(axis.text = element_text(size = 8), 
            axis.title = element_text(size = 9, face = "bold"), 
            legend.title = element_text(size = 8, face = 'bold'), 
            legend.text = element_text(size = 7),
            legend.box.margin = margin(-1, -1, -1, -1),
            legend.key = element_rect(size = 2), legend.key.size = unit(1, 'lines'),
            title = element_text(size = 8, face = "bold"), 
            plot.title = element_text(hjust = 0.5))
  
    return(allergyNormGG)
}

histoAllergyNum = function(df, yearSequence){
    allergyProb = as.data.frame(cbind(yearSequence, 0, 0, 0, 0))
    colnames(allergyProb) = c('Year', '1', '2', '3', '>3')
    
    for(t in 1:(length(yearSequence)-1)){
        tempDF = df[(df$YOB > yearSequence[t] & df$YOB < yearSequence[t+1]), ]
        for(person in unique(tempDF$ID)){
            allergiesPerson = tempDF$Allergy[tempDF$ID==person]
            allergyNum = length(unique(allergiesPerson[!is.na(allergiesPerson)]))
            if(allergyNum==1){
                allergyProb$'1'[allergyProb$Year==yearSequence[t]] = 
                    allergyProb$'1'[allergyProb$Year==yearSequence[t]] + 1
            } else if(allergyNum==2){
                allergyProb$'2'[allergyProb$Year==yearSequence[t]] = 
                    allergyProb$'2'[allergyProb$Year==yearSequence[t]] + 1
            } else if(allergyNum==3){
                allergyProb$'3'[allergyProb$Year==yearSequence[t]] = 
                    allergyProb$'3'[allergyProb$Year==yearSequence[t]] + 1
            } else if(allergyNum > 3){
                allergyProb$'>3'[allergyProb$Year==yearSequence[t]] = 
                    allergyProb$'>3'[allergyProb$Year==yearSequence[t]] + 1
            }
        }
        allergyProb$'1'[allergyProb$Year==yearSequence[t]] = 
            allergyProb$'1'[allergyProb$Year==yearSequence[t]]/length(unique(tempDF$ID))
        allergyProb$'2'[allergyProb$Year==yearSequence[t]] = 
            allergyProb$'2'[allergyProb$Year==yearSequence[t]]/length(unique(tempDF$ID))
        allergyProb$'3'[allergyProb$Year==yearSequence[t]] = 
            allergyProb$'3'[allergyProb$Year==yearSequence[t]]/length(unique(tempDF$ID))
        allergyProb$'>3'[allergyProb$Year==yearSequence[t]] = 
            allergyProb$'>3'[allergyProb$Year==yearSequence[t]]/length(unique(tempDF$ID))

    }
    return(allergyProb)
}

allergyNumPlot = function(df, yearCutoffmin, yearCutoffmax){
    dfGG = melt(df, id.var = 'Year')
    colnames(dfGG) = c('Year', 'AllergyNumber', 'Frequency')
    allergyNormGG = ggplot(dfGG, aes(x = Year, y = Frequency, colour = AllergyNumber))+
    theme_bw()+
    xlab('Year of Birth')+
    ylab(paste0('Period proportion of individuals', '\n', 'with at least one allergy'))+
    labs(colour= "Number of Allergies")+
    ggtitle(paste0('Number of unique allergies per person'))+
    geom_smooth(data = dfGG[(dfGG$Year > yearCutoffmin & dfGG$Year < yearCutoffmax), ], 
        se = FALSE)+
    theme(axis.text = element_text(size = 8), 
        axis.title = element_text(size = 9, face = "bold"),
        legend.title = element_text(size = 8, face = 'bold'), 
        legend.text = element_text(size = 8),
        legend.box.margin = margin(-1, -1, -1, -1),
        legend.key = element_rect(size = 2), legend.key.size = unit(1, 'lines'),
        title = element_text(size = 8, face = "bold"), 
        plot.title = element_text(hjust = 0.5))
        
    return(allergyNormGG)
}
