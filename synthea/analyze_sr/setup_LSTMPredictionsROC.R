
library(ggplot2)
library(cowplot)

plotROC = function(precData, AUC){

    aucData = precData[precData$type=='ROC', ]

    ROC = ggplot(aucData, aes(x = x, y = y))+
    scale_x_continuous(expand = c(0.0075, 0.0075))+
    scale_y_continuous(expand = c(0.005, 0.005))+
    geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), lty = 2, color = 'black')+
    geom_line(size = 0.75, color = 'red')+
    theme_bw()+
    ggtitle('Myocardial infarction ROC')+
    xlab('1 - Specificity')+
    ylab('Sensitivity')+
    theme(axis.text = element_text(size=6), 
    axis.title = element_text(size = 7, face = 'bold'),
    legend.position = 'none',
    title = element_text(size = 7, face = 'bold'), 
    plot.title = element_text(hjust = 0.5))

    ROC = ROC + annotate('text', x = 0.85, y = 0.05, 
        label = paste('AUC = ', AUC), size = 2.5)
    return(ROC)

}

plotPR = function(precData, AUC){
    
    prData = precData[precData$type=='PRC', ]

    PR = ggplot(prData, aes(x = x, y = y))+
        scale_x_continuous(expand = c(0.0075, 0.0075))+
        scale_y_continuous(expand = c(0.005, 0.005))+
        geom_segment(aes(x = 0, xend = 1, y = 1, yend = 0), lty = 2, color = 'black')+
        geom_line(size = 0.75, color = 'red')+
        theme_bw()+
        ggtitle('Myocardial infarction PR')+
        xlab('Recall')+
        ylab('Precision')+
        theme(axis.text = element_text(size=6), 
        axis.title = element_text(size = 7, face = 'bold'),
        legend.position = 'none',
        title = element_text(size = 7, face = 'bold'), 
        plot.title = element_text(hjust = 0.5))

    PR = PR + annotate('text', x = 0.15, y = 0.05, 
        label = paste('AUC = ', AUC), size = 2.5)
    return(PR)

}
