#loading libraries
library(tidymodels)
library(tidyverse)
library(workflows)
library(tune)
library(caret)
library(vip)
source("~/Documents/Data_Science/projects/medium/code/script/functions/funs_do_tidy_modeling.R")

#loading data
clean_df <- read.csv("~/Documents/Data_Science/projects/medium/data/processed_data/mental-health-in-tech-2016-clean.csv")

#using features selected by chi-squared test in 02_do_feature_selection.R
tidy_df <- SelectSigFeatures(clean_df)


#train/test split-------------------------------------------------------------------------------------

set.seed(101)
data_split <- initial_split(tidy_df, prop = 3/4)

train_data <- training(data_split)
test_data  <- testing(data_split)


#model prep-------------------------------------------------------------------------------------

#initiate recipe
tidy_rec <- 
    recipe(mh_discussion_negative ~ ., data = train_data) %>% 
    step_dummy(all_nominal(), -all_outcomes()) %>% 
    step_zv(all_predictors())

#tuning grid
rf_grid <- expand.grid(mtry = c(2, 3, 4, 5),
                        min_n = c(1, 3, 5))

#resampling with cross-validation
folds <- vfold_cv(train_data, v = 10, repeats = 10)


#train model-------------------------------------------------------------------------------------

#initiating random forest model
rf_mod <- 
    rand_forest() %>% 
    set_args(mtry = tune(), min_n = tune()) %>%
    set_engine("ranger", importance = "impurity") %>% 
    set_mode("classification")

#building model workflow
rf_wflow <- 
    workflow() %>%
    add_model(rf_mod) %>% 
    add_recipe(tidy_rec)

rf_wflow

#adding tuning and resampling to workflow
rf_tune_results <- 
    rf_wflow %>% 
    tune_grid(
        resamples = folds,
        grid = rf_grid,
        metrics = metric_set(accuracy, roc_auc),
        control = control_grid(save_pred = TRUE)
    )

#selecting best model-------------------------------------------------------------------------------------

#returning all metrics
rf_tune_results %>%
    collect_metrics()

#best metric
rf_tune_results %>%
    show_best(metric = "roc_auc")

#plotting results
autoplot(rf_tune_results)

#returning best result
best_forest <- rf_tune_results %>%
    select_best("roc_auc")
best_forest

#finalizing workflow 
rf_wflow <- 
    rf_wflow %>% 
    finalize_workflow(best_forest)



#predict-------------------------------------------------------------------------------------

#fitting final tuned model 
rf_fit <- 
    rf_wflow %>%
    last_fit(data_split)
rf_fit

#evaluating performance of model on test data
test_performance <- 
    rf_fit %>% collect_metrics()
test_performance

test_predictions <- 
    rf_fit %>% collect_predictions()
test_predictions

test_conf_mat <- 
    test_predictions %>% 
    conf_mat(truth = mh_discussion_negative, estimate = .pred_class)
test_conf_mat

#plotting variable importance
rf_fit %>% 
    pluck(".workflow", 1) %>%   
    pull_workflow_fit() %>% 
    vip(num_features = 20)

#plotting outcome ROC's 
rf_fit %>% 
    collect_predictions() %>% 
    roc_curve(mh_discussion_negative, c(.pred_Maybe, .pred_No, .pred_Yes)) %>% 
    autoplot()