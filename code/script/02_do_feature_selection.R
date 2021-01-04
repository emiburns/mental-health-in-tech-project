#loading libraries
library(stats)
library(tidyverse)
library(magrittr)
library(ggplot2)
library(caret)
source("~/Documents/Data_Science/projects/medium/code/script/functions/funs_do_feature_selection.R")


#loading data------------------------------------------------------------------------------------------

clean_df <- read.csv("~/Documents/Data_Science/projects/medium/data/processed_data/mental-health-in-tech-2016-clean.csv")


#building dataframe specific for ml modeling---------------------------------------------------------------

#dropping unrelated variables
clean_df <- clean_df[, -c(28,30,39,44,47,49, 52:83)]

#engineering work and residential countries
table(clean_df$work_country)
clean_df$work_country <- ifelse(clean_df$work_country == "United States of America", "USA", 
                                ifelse(clean_df$work_country == "United Kingdom", "UK", "Other"))

table(clean_df$resident_country)
clean_df$resident_country <- ifelse(clean_df$resident_country == "United States of America", "USA", 
                                    ifelse(clean_df$resident_country == "United Kingdom", "UK", "Other"))

table(clean_df$resident_country, clean_df$work_country)

#creating dummy variables
dummy_df <- DummyVarDf(clean_df[, 1:45], 9)

#adding back outcome and continuous variables
dummy_df <- cbind(dummy_df, clean_df[, c(46:47)])
dim(dummy_df)

#near-zero-variance predictor identification
nzv <- nearZeroVar(dummy_df, saveMetrics= TRUE)
nzv[nzv$nzv,] 
nzv <- nearZeroVar(dummy_df)
dummy_df <- dummy_df[, -nzv]



#feature importance------------------------------------------------------------------------------------------

#initial look at potentially significant features with chi squared
weights <- chi.squared(mh_discussion_negative ~., dummy_df[, -c(156:157)]) 
print(weights)
subset<- cutoff.k(weights, 20)
f <- as.simple.formula(subset, "Class")
print(f) 

####################################################################################################
#in descending order of importance: 
#mh_discussion_supervisor.Yes + mh_discussion_supervisor.No + ph_discussion_negative.No + 
#mh_ph_serious.Yes + mh_id_coworkers_impact.Yes..I.think.they.would + mh_discussion_coworkers.No + 
#mh_medical_leave.Very.easy + ph_discussion_negative.Maybe + mh_id_career_impact.Yes..I.think.it.would + 
#mh_ph_serious.No + mh_medical_leave.Very.difficult + mh_id_coworkers_impact.No..I.don.t.think.they.would + 
#mh_discussion_coworkers.Yes + mh_id_career_impact.No..I.don.t.think.it.would + 
#mh_interview.No + previous_employers_mh_discuss_impact.Yes..all.of.them + 
#mh_id_career_impact.Maybe + mh_other_impact.Yes + mh_medical_leave.Somewhat.difficult + mh_unsupportive_workplace.No
####################################################################################################



#univariate analysis------------------------------------------------------------------------------------------

#looking at distribution of outcome variable
PlotCatDist(clean_df, clean_df$mh_discussion_negative) 

#looking at distribution of potential predictor variables
PlotCatDist(clean_df, clean_df$mh_discussion_supervisor) 
PlotCatDist(clean_df, clean_df$ph_discussion_negative) 
PlotCatDist(clean_df, clean_df$mh_id_coworkers_impact) 
PlotCatDist(clean_df, clean_df$mh_ph_serious)
PlotCatDist(clean_df, clean_df$mh_discussion_coworkers) 
PlotCatDist(clean_df, clean_df$mh_medical_leave)
PlotCatDist(clean_df, clean_df$mh_id_career_impact)
PlotCatDist(clean_df, clean_df$mh_interview) 
PlotCatDist(clean_df, clean_df$previous_employers_mh_discuss_impact)
PlotCatDist(clean_df, clean_df$mh_other_impact)
PlotCatDist(clean_df, clean_df$mh_unsupportive_workplace)

#looking at correlation between potential categorical predictor variables and outcome variable
chisq.test(clean_df$mh_discussion_negative, clean_df$mh_discussion_supervisor) #sig
chisq.test(clean_df$mh_discussion_negative, clean_df$ph_discussion_negative) #sig
chisq.test(clean_df$mh_discussion_negative, clean_df$mh_id_coworkers_impact) #sig
chisq.test(clean_df$mh_discussion_negative, clean_df$mh_ph_serious) #sig
chisq.test(clean_df$mh_discussion_negative, clean_df$mh_discussion_coworkers) #sig
chisq.test(clean_df$mh_discussion_negative, clean_df$mh_medical_leave) #sig
chisq.test(clean_df$mh_discussion_negative, clean_df$mh_id_career_impact) #sig
chisq.test(clean_df$mh_discussion_negative, clean_df$mh_interview) #sig
chisq.test(clean_df$mh_discussion_negative, clean_df$previous_employers_mh_discuss_impact) #sig
chisq.test(clean_df$mh_discussion_negative, clean_df$mh_other_impact) #sig
chisq.test(clean_df$mh_discussion_negative, clean_df$mh_unsupportive_workplace) #sig

#looking at correlation between potential continuous predictor variables and outcome variable
kruskal.test(clean_df$mh_discussion_negative ~ clean_df$total_dx, data = clean_df) #non sig 
kruskal.test(clean_df$mh_discussion_negative ~ clean_df$total_dx_belief, data = clean_df) #non sig


#writing modeling df to directory--------------------------------------------------------------

write_csv(dummy_df, path = "~/Documents/Data_Science/projects/medium/data/processed_data/mental-health-in-tech-2016-modeling.csv")
