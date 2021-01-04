library(ggplot2)
library(caret)

#plotting categorical variable distribution
PlotCatDist <- function(data, xcol){
    return(ggplot(data, aes(as.factor(xcol))) +
        geom_bar(fill = "mediumpurple1") +
        theme_minimal())
}

#creating dataframe with dummy vars
DummyVarDf <- function(data, indexnum){
    #make dummy variables
    dmy <- dummyVars(" ~ .", data = data[, -indexnum])
    dummy_df <- data.frame(predict(dmy, newdata = data[, -indexnum]))
    
    #adding outcome variable
    dummy_df <- cbind(dummy_df, data[indexnum])
    return(dummy_df)
}