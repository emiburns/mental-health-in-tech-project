#loading libraries
library(tidyverse)
library(caret)
library(ggplot2)
source("~/Documents/Data_Science/projects/medium/code/script/functions/funs_do_feature_selection.R")
source("~/Documents/Data_Science/projects/medium/code/script/functions/funs_do_caret_modeling.R")

#loading data
clean_df <- read.csv("~/Documents/Data_Science/projects/medium/data/processed_data/mental-health-in-tech-2016-modeling.csv")

dim(clean_df)
head(clean_df)

#train/test split-------------------------------------------------------------------------------------
trainIndex <- createDataPartition(clean_df$mh_discussion_negative, p = .75, 
                                  list = FALSE, 
                                  times = 1)
trainData <- clean_df[ trainIndex,]
testData <- clean_df[-trainIndex,]


#class imbalance-------------------------------------------------------------------------------------
PlotCatDist(trainData, trainData$mh_discussion_negative)

trainSmote <- SMOTE(mh_discussion_negative ~ ., data  = trainData)                         
table(trainSmote$mh_discussion_negative)


#linear svm classification--------------------------------------------------------------------------------
svmGrid <-  expand.grid(C = c(.001, .01, .1, 0.5, 1.0))

#fitting training data w/ sbf feature selection
ft_selected <- SelectFeatures(svmGrid, "svmLinear", trainSmote)
svmMod <- FitTrainingModel(svmGrid, "svmLinear", trainSmote[,c(ft_selected,"mh_discussion_negative")])
svmMod

#plotting training results
trellis.par.set(caretTheme())
ggplot(svmMod)  
densityplot(svmMod, pch = "|")

#fitting test data
svm_test_results <- FitTestModel(svmMod, testData)
svm_test_results


#linear svm classification--------------------------------------------------------------------------------
rfGrid <-  expand.grid(mtry = c(2, 3, 4, 5),
                       splitrule = c("gini", "extratrees"),
                       min.node.size = c(1, 3, 5))

#fitting training data w/ sbf feature selection
rf_ft_selected <- SelectFeatures(svmGrid, "ranger", trainSmote)
rfMod <- FitTrainingModel(rfGrid, "ranger", trainSmote[,c(rf_ft_selected,"mh_discussion_negative")])
rfMod

#plotting training results
trellis.par.set(caretTheme())
ggplot(rfMod)  
densityplot(rfMod, pch = "|")

#fitting test data
rf_test_results <- FitTestModel(rfMod, testData)
rf_test_results

#variable importance per model
plot(varImp(svmMod), top=20)
plot(varImp(rfMod), top=20)

#direct model performance comparison
resamps <- resamples(list(LinearSVM = svmMod,
                          RF = rfMod))
resamps
summary(resamps)



difValues <- diff(resamps)
difValues
summary(difValues)

trellis.par.set(caretTheme())
bwplot(difValues, layout = c(3, 1))