#import libraries
library(caret)


#feature selection
SelectFeatures <- function(grid, method, data){
    
    fitControl <- trainControl(method = "repeatedcv",
                               number = 10,
                               repeats = 10,
                               classProbs = TRUE)
    
    sbf_fit <- sbf(
        form = mh_discussion_negative ~ .,
        data = data, 
        method = method, 
        tuneGrid=grid,
        trControl = fitControl,
        sbfControl = sbfControl(method = "LGOCV",
                                number = 5,
                                p = .8,
                                functions = rfSBF,
                                saveDetails = TRUE))
    
    prop_included = rowMeans(sapply(sbf_fit$variables,function(i)sbf_fit$coefnames %in% i))
    selected = sbf_fit$coefnames[prop_included > 0.7]
    
    return(selected)

}


#fitting model to training data
FitTrainingModel <- function(mod_grid, alg_method, selected_data, mod_importance){
    
    fitControl_mod <- trainControl(method = "repeatedcv",
                               number = 10,
                               repeats = 10,
                               classProbs = TRUE)
    
    
    set.seed(101)
    fitted_mod <- train(form = mh_discussion_negative ~ ., 
                      data = selected_data, 
                      method = alg_method,
                      trControl = fitControl_mod, 
                      verbose = FALSE,
                      tuneGrid = mod_grid,
                      importance = mod_importance)
    
    return(fitted_mod)
}


#fitting model to test data
FitTestModel <- function(model, test_data){
    model_prediction <- predict(model, test_data)
    results <- confusionMatrix(model_prediction, test_data$mh_discussion_negative)
    
    return(results)
}
